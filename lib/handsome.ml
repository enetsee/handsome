open StdLabels

module Width = struct
  module type S = sig
    type t

    val zero : t
    val add : t -> t -> t
    val compare : t -> t -> int

    (* [measure] must report at least the display width, must be additive over
     concatenation, and must measure the empty string at [zero]. [add] must be
     associative and commutative. A document's width is the sum of its text
     nodes', computed at construction and held from then on. *)
    val measure : string -> t
  end
end

module Ascii_width = struct
  type t = int

  let zero = 0
  let add = ( + )
  let compare = Int.compare
  let measure = String.length
end

let unicode_version = Unicode_tables.unicode_version

module Utf8_width = struct
  type t = int

  let zero = 0
  let add = ( + )
  let compare = Int.compare

  (* The least code point either table can match. The tables are ascending and
     disjoint and [zero] starts below [wide], so nothing below this bound is in
     either one and every such code point takes a column. Derived from the table
     from the table itself, so it stays true across a regeneration;
     [width-utf8/7] pins the ordering it rests on.

     Hoisted out of the call, which is worth about 4%.
     The bound takes ASCII off the two binary searches, and ASCII is almost all of
     the input: 4.0x on an ASCII corpus, 1.0x on an all-CJK one. *)
  let zero_min = Unicode_tables.zero.(0)

  (* The two categories overlap: U+302A..U+302F are General_Category Mn and
     East_Asian_Width Wide. Zero is tested first, since a combining mark is drawn
     over the character it follows. *)
  let column u =
    if u < zero_min
    then 1
    else if Unicode_tables.mem Unicode_tables.zero u
    then 0
    else if Unicode_tables.mem Unicode_tables.wide u
    then 2
    else 1
  ;;

  (* [Uchar.utf_decode_uchar] yields U+FFFD on a malformed decode and
     [utf_decode_length] the number of bytes to step over, which is at least one.
     The loop therefore advances on every input and terminates. U+FFFD is
     East_Asian_Width Ambiguous, so it measures one column. *)
  let measure s =
    let n = String.length s in
    let w = ref 0
    and i = ref 0 in
    while !i < n do
      (* A byte below 0x80 is a one-byte code point below [zero_min], so it takes
         a column and neither the decoder nor [column] can say otherwise. Skipping
         both is a further 4x on ASCII, 16x with the bound above it, and costs a
         few percent on text with no ASCII in it at all.

         [String.get] keeps the bound check, which is 13% of the fast path. The
         library uses no other unsafe primitive. *)
      if String.get s !i < '\x80'
      then (
        incr w;
        incr i)
      else (
        let d = String.get_utf_8_uchar s !i in
        w := !w + column (Uchar.to_int (Uchar.utf_decode_uchar d));
        i := !i + Uchar.utf_decode_length d)
    done;
    !w
  ;;
end

module type S = sig
  type width
  type 'a t

  type error =
    | Newline_in_text of
        { text : string
        ; index : int
        }
    | Alt_outside_frame of int

  val text : string -> 'a t
  val empty : 'a t
  val ( ^^ ) : 'a t -> 'a t -> 'a t
  val concat : 'a t list -> 'a t
  val flat_alt : 'a t -> 'a t -> 'a t
  val line : 'a t
  val softline : 'a t
  val hardline : 'a t
  val blank : 'a t
  val group : 'a t -> 'a t
  val nest : int -> 'a t -> 'a t
  val align : 'a t -> 'a t
  val framed : (('a t -> 'a t -> 'a t) -> 'a t) -> 'a t
  val annotate : 'a -> 'a t -> 'a t
  val reannotate : ('a -> 'b) -> 'a t -> 'b t
  val unannotate : 'a t -> unit t
  val check : 'a t -> (unit, error list) result
  val pp : Format.formatter -> 'a t -> unit
  val pp_error : Format.formatter -> error -> unit

  type 'a stream =
    | S_empty
    | S_text of width * string * 'a stream
    | S_line of int * 'a stream
    | S_ann_push of 'a * 'a stream
    | S_ann_pop of 'a stream

  type resolutions = { declined : (int * width) list }

  val render : width:width -> 'a t -> 'a stream * resolutions
  val to_string : 'a stream -> string
  val lines : 'a stream -> width array
end

module Make (W : Width.S) = struct
  type width = W.t

  (* -- documents --------------------------------------------------------------

     Every composite node caches the width it occupies laid out flat, together
     with a flag saying whether a flat layout exists at all: a hardline in flat
     position rules one out. The fit decision at a group is then a comparison
     against a value already present, and rendering is linear in the document.
     A frame and each of its conditionals add a lookup in the set of frames laid
     out flat, logarithmic in its size.

     The two are fields of an inline record, a [bool] and a [W.t], with the
     [W.t] a placeholder where the [bool] is false. The alternative encoding is
     [W.t option].

     [W.t] is abstract, so it holds no value that can stand for infinity, and
     [val infinity : t] in WIDTH would oblige every implementation to reserve
     one and [measure] to keep clear of it. That leaves the flag.

     The option costs an allocation per composite node during construction.
     [Cat (W.t option, _, _)] is a four-word block plus a two-word [Some]; the
     inline record is a five-word block. Measured on a 200k-group document,
     non-flambda, against the option encoding in one process: 17.0 Mwords
     allocated falls to 14.2, and construction is 1.15x faster. Rendering and
     [to_string] hold steady, the cached width being read during the fit test
     and built at construction.

     Storing a sentinel in the width field itself would cost 12.2 Mwords, and
     needs a concrete [W.t]. That word is the whole runtime cost of the functor:
     with W bound to a concrete structure so the compiler can see through it,
     construction, rendering and [to_string] fall within 0.95-1.05x, which is
     noise in both directions.

     [flattenable] and the fit test in [step] are the only readers of these
     fields, and both consult the flag first.
     ------------------------------------------------------------------------ *)

  (* A frame's name. [framed] takes a fresh one for each frame and hands the
     caller its conditional, so no caller names a tag directly. *)
  type tag = int

  module Tag_set = Set.Make (Int)

  (* -- frames -------------------------------------------------------------------

     A group decides whether it fits only when nothing around it is flat: a
     flat group or frame puts everything inside it in flat mode, where no
     decision is made. So whenever a group decides, every frame around it has
     already broken, and a conditional whose frame lies outside the deciding
     group takes its broken branch. One whose frame lies inside takes its flat
     branch if the group goes flat, because the frame goes flat with it.

     Every node therefore caches its width with each free conditional -- one
     whose frame is further out -- at its broken branch, and that width is the
     one the node prints. The frame is the one node that has to see the other
     branch, and a [Free] node carries what it needs: it sits over a region
     holding free conditionals, and records per frame what their flat and
     broken branches weigh. [frame] reads its own frame's entry, measures those
     conditionals at their flat branch, and passes the rest further out. A
     conditional with no frame at all takes its broken branch, which is what it
     was measured at.

     One case is measured conservatively. Inside a branch of a conditional, a
     conditional free in that branch counts at the wider of its two branches.
     Whether it is printed depends on the frame of the conditional holding it
     as well as on its own, and measuring it exactly means carrying it through
     every frame between it and the root under each combination of the two.
     Done directly that is quadratic in the depth of the nesting, and
     exponential where one subdocument sits in both branches of nested
     conditionals.

     The entries are kept in a balanced tree keyed by tag, whose nodes hold the
     sums over their subtrees. [W] has no subtraction, so a frame takes "every
     frame's conditionals but mine" by removing its own entry and reading the
     root. A frame then costs O(log k) for k frames free in its body, a
     conditional costs O(1), and [( ^^ )] adds the smaller side's entries to the
     larger's, so a document of n nodes, counted as a tree, is built in
     O(n log^2 n) at most.

     Summing by frame adds widths out of document order, which is why [add]
     has to be commutative.

     [Free] never wraps a [Free]. A [Free] can also sit where its entries go no
     further up: in the broken branch of a [flat_alt], which no group around it
     measures, and in a branch of a conditional, which is measured at its
     widest. A document built without [framed] holds none, so documents without
     frames are built exactly as before.
     ------------------------------------------------------------------------ *)

  (* What one frame's free conditionals weigh in a region: at their flat
     branches, at their broken branches, and each at whichever of its two is
     wider. The [_ok] flags say whether those branches can lie flat. *)
  type free =
    { flat_w : W.t
    ; flat_ok : bool
    ; broken_w : W.t
    ; broken_ok : bool
    ; wider_w : W.t
    }

  (* An AVL tree of [free]s keyed by tag. Each node also holds its subtree's
     size, for choosing the smaller side of a union, and the sums over its
     subtree at the broken branch and at the wider one. *)
  type frees =
    | F_leaf
    | F_node of
        { l : frees
        ; tag : tag
        ; e : free
        ; r : frees
        ; height : int
        ; size : int
        ; all_broken_w : W.t
        ; all_broken_ok : bool
        ; all_wider_w : W.t
        ; all_wider_ok : bool
        }

  let f_height = function
    | F_leaf -> 0
    | F_node n -> n.height
  ;;

  let f_size = function
    | F_leaf -> 0
    | F_node n -> n.size
  ;;

  let broken_w_of = function
    | F_leaf -> W.zero
    | F_node n -> n.all_broken_w
  ;;

  let broken_ok_of = function
    | F_leaf -> true
    | F_node n -> n.all_broken_ok
  ;;

  let wider_w_of = function
    | F_leaf -> W.zero
    | F_node n -> n.all_wider_w
  ;;

  let wider_ok_of = function
    | F_leaf -> true
    | F_node n -> n.all_wider_ok
  ;;

  let f_node l tag e r =
    F_node
      { l
      ; tag
      ; e
      ; r
      ; height = 1 + Int.max (f_height l) (f_height r)
      ; size = f_size l + 1 + f_size r
      ; all_broken_w = W.add (W.add (broken_w_of l) e.broken_w) (broken_w_of r)
      ; all_broken_ok = broken_ok_of l && e.broken_ok && broken_ok_of r
      ; all_wider_w = W.add (W.add (wider_w_of l) e.wider_w) (wider_w_of r)
      ; all_wider_ok = wider_ok_of l && e.flat_ok && e.broken_ok && wider_ok_of r
      }
  ;;

  (* Rebalances a node whose subtrees differ in height by at most three, as
     [Stdlib.Map] does. A subtree two taller than its sibling is a node, so the
     [F_leaf] cases are unreachable. *)
  let f_balance l tag e r =
    let hl = f_height l
    and hr = f_height r in
    if hl > hr + 2
    then (
      match l with
      | F_leaf -> assert false
      | F_node ln ->
        if f_height ln.l >= f_height ln.r
        then f_node ln.l ln.tag ln.e (f_node ln.r tag e r)
        else (
          match ln.r with
          | F_leaf -> assert false
          | F_node lr ->
            f_node (f_node ln.l ln.tag ln.e lr.l) lr.tag lr.e (f_node lr.r tag e r)))
    else if hr > hl + 2
    then (
      match r with
      | F_leaf -> assert false
      | F_node rn ->
        if f_height rn.r >= f_height rn.l
        then f_node (f_node l tag e rn.l) rn.tag rn.e rn.r
        else (
          match rn.l with
          | F_leaf -> assert false
          | F_node rl ->
            f_node (f_node l tag e rl.l) rl.tag rl.e (f_node rl.r rn.tag rn.e rn.r)))
    else f_node l tag e r
  ;;

  let combine x y =
    { flat_w = W.add x.flat_w y.flat_w
    ; flat_ok = x.flat_ok && y.flat_ok
    ; broken_w = W.add x.broken_w y.broken_w
    ; broken_ok = x.broken_ok && y.broken_ok
    ; wider_w = W.add x.wider_w y.wider_w
    }
  ;;

  (* Adds [e] under [tag], summing it into the entry already there. *)
  let rec f_add tag e = function
    | F_leaf -> f_node F_leaf tag e F_leaf
    | F_node n ->
      let c = Int.compare tag n.tag in
      if c = 0
      then f_node n.l tag (combine n.e e) n.r
      else if c < 0
      then f_balance (f_add tag e n.l) n.tag n.e n.r
      else f_balance n.l n.tag n.e (f_add tag e n.r)
  ;;

  let rec f_find tag = function
    | F_leaf -> None
    | F_node n ->
      let c = Int.compare tag n.tag in
      if c = 0 then Some n.e else f_find tag (if c < 0 then n.l else n.r)
  ;;

  let rec f_remove_min = function
    | F_leaf -> assert false
    | F_node { l = F_leaf; tag; e; r; _ } -> tag, e, r
    | F_node ({ l = F_node _; _ } as n) ->
      let tag, e, l = f_remove_min n.l in
      tag, e, f_balance l n.tag n.e n.r
  ;;

  let f_join l r =
    match l, r with
    | F_leaf, t | t, F_leaf -> t
    | F_node _, F_node _ ->
      let tag, e, r = f_remove_min r in
      f_balance l tag e r
  ;;

  let rec f_remove tag = function
    | F_leaf -> F_leaf
    | F_node n ->
      let c = Int.compare tag n.tag in
      if c = 0
      then f_join n.l n.r
      else if c < 0
      then f_balance (f_remove tag n.l) n.tag n.e n.r
      else f_balance n.l n.tag n.e (f_remove tag n.r)
  ;;

  let rec f_add_all t into =
    match t with
    | F_leaf -> into
    | F_node n -> f_add_all n.r (f_add n.tag n.e (f_add_all n.l into))
  ;;

  (* The smaller side's entries added to the larger's. *)
  let f_union x y = if f_size x <= f_size y then f_add_all x y else f_add_all y x

  type 'a t =
    | Empty
    | Text of W.t * string
    | Hard
    | Cat of
        { flattenable : bool
        ; req : W.t
        ; l : 'a t
        ; r : 'a t
        }
    | Alt of
        { flattenable : bool
        ; req : W.t
        ; f : 'a t
        ; b : 'a t
        }
    (* [f] is taken when the enclosing group is flat, [b] when it is broken; the
       requirement is [f]'s. *)
    | Group of
        { flattenable : bool
        ; req : W.t
        ; d : 'a t
        }
    | Nest of
        { flattenable : bool
        ; req : W.t
        ; j : int
        ; d : 'a t
        }
    | Align of
        { flattenable : bool
        ; req : W.t
        ; d : 'a t
        }
    | Annot of
        { flattenable : bool
        ; req : W.t
        ; a : 'a
        ; d : 'a t
        }
    (* A group that records how it resolved under [tag], and a conditional that
       reads that record where [Alt] reads the group directly enclosing it.
       [Frame]'s requirement has its own conditionals at their flat branch, and
       [Falt]'s is its broken branch's; see above. *)
    | Frame of
        { flattenable : bool
        ; req : W.t
        ; tag : tag
        ; d : 'a t
        }
    | Falt of
        { flattenable : bool
        ; req : W.t
        ; tag : tag
        ; f : 'a t
        ; b : 'a t
        }
    | Free of 'a region

  (* [fixed] and [fixed_ok] measure what the free conditionals leave alone.
     [all_w] and [all_ok] measure the whole, the free conditionals at their
     broken branch, and are copied from [inside]'s own fields, so that reading a
     region's measure is one field away. [inside] is never a [Free]. *)
  and 'a region =
    { fixed : W.t
    ; fixed_ok : bool
    ; frees : frees
    ; inside : 'a t
    ; all_w : W.t
    ; all_ok : bool
    }

  type error =
    | Newline_in_text of
        { text : string
        ; index : int
        }
    | Alt_outside_frame of int

  (* [flat_width] carries a width where [flattenable] is true, and a placeholder
     on [Hard] and on anything holding it in flat position. *)

  let flattenable = function
    | Empty | Text _ -> true
    | Hard -> false
    | Cat r -> r.flattenable
    | Alt r -> r.flattenable
    | Group r -> r.flattenable
    | Nest r -> r.flattenable
    | Align r -> r.flattenable
    | Annot r -> r.flattenable
    | Frame r -> r.flattenable
    | Falt r -> r.flattenable
    | Free r -> r.all_ok
  ;;

  let flat_width = function
    | Empty | Hard -> W.zero
    | Text (w, _) -> w
    | Cat r -> r.req
    | Alt r -> r.req
    | Group r -> r.req
    | Nest r -> r.req
    | Align r -> r.req
    | Annot r -> r.req
    | Frame r -> r.req
    | Falt r -> r.req
    | Free r -> r.all_w
  ;;

  (* -- constructors -------------------------------------------------------- *)

  let empty = Empty
  let text s = if String.length s = 0 then Empty else Text (W.measure s, s)

  (* [Empty] is the only empty document; the constructors below maintain that.
     One exhaustive match here, so that adding a constructor forces a decision
     in a single place. *)
  let is_empty = function
    | Empty -> true
    | Text _
    | Hard
    | Cat _
    | Alt _
    | Group _
    | Nest _
    | Align _
    | Annot _
    | Frame _
    | Falt _
    | Free _ -> false
  ;;

  let is_free = function
    | Free _ -> true
    | Empty
    | Text _
    | Hard
    | Cat _
    | Alt _
    | Group _
    | Nest _
    | Align _
    | Annot _
    | Frame _
    | Falt _ -> false
  ;;

  (* [d] seen as a region: its own, or, where nothing in it is free, one that
     is all fixed. The second allocates, so the constructors ask [is_free]
     before they ask for this. *)
  let view d =
    match d with
    | Free r -> r
    | Empty
    | Text _
    | Hard
    | Cat _
    | Alt _
    | Group _
    | Nest _
    | Align _
    | Annot _
    | Frame _
    | Falt _ ->
      let w = flat_width d
      and ok = flattenable d in
      { fixed = w; fixed_ok = ok; frees = F_leaf; inside = d; all_w = w; all_ok = ok }
  ;;

  (* [d], which is never a [Free], as a region with these parts, or [d] itself
     where nothing is free. *)
  let region fixed fixed_ok frees d =
    match frees with
    | F_leaf -> d
    | F_node _ ->
      Free
        { fixed
        ; fixed_ok
        ; frees
        ; inside = d
        ; all_w = flat_width d
        ; all_ok = flattenable d
        }
  ;;

  (* The region [r] around a new node built over its [inside]. *)
  let rewrap r d =
    Free { r with inside = d; all_w = flat_width d; all_ok = flattenable d }
  ;;

  let wider x y = if W.compare x y >= 0 then x else y

  (* [d] with every free conditional at its wider branch, and whether all of
     their branches lie flat. This is how a conditional measures its branches;
     see the comment above [free]. *)
  let widest d =
    if is_free d
    then (
      let r = view d in
      W.add r.fixed (wider_w_of r.frees), r.fixed_ok && wider_ok_of r.frees)
    else flat_width d, flattenable d
  ;;

  let[@inline] cat x y =
    let flattenable = flattenable x && flattenable y in
    let req = if flattenable then W.add (flat_width x) (flat_width y) else W.zero in
    Cat { flattenable; req; l = x; r = y }
  ;;

  let ( ^^ ) x y =
    if is_empty x
    then y
    else if is_empty y
    then x
    else if is_free x || is_free y
    then (
      let rx = view x
      and ry = view y in
      region
        (W.add rx.fixed ry.fixed)
        (rx.fixed_ok && ry.fixed_ok)
        (f_union rx.frees ry.frees)
        (cat rx.inside ry.inside))
    else cat x y
  ;;

  (* Reversing first and folding left builds the same right-leaning tree as
     [List.fold_right], and stays tail-recursive on a long list. *)
  let concat ds = List.fold_left ~f:(fun acc d -> d ^^ acc) ~init:Empty (List.rev ds)

  (* The cached width of [flat_alt a b] is that of [a], the branch taken when
     laid out flat, which is why a hardline in the flat branch leaves every
     enclosing group broken. For the same reason only [a]'s free conditionals
     reach the region above: [b] is printed only where the group around the
     [flat_alt] broke, and no group around it measures [b]. *)
  let alt a b = Alt { flattenable = flattenable a; req = flat_width a; f = a; b }

  let flat_alt a b =
    if is_free a
    then (
      let r = view a in
      rewrap r (alt r.inside b))
    else alt a b
  ;;

  let hardline = Hard
  let line = flat_alt (text " ") hardline
  let softline = flat_alt empty hardline
  let blank = flat_alt (text " ") empty
  let group_node d = Group { flattenable = true; req = flat_width d; d }

  (* A group holding a hardline in flat position is dropped: it would always be
     laid out broken. The output is identical either way, and [pp] and the
     document keep to nodes that carry a decision. *)
  let group d =
    if is_empty d || not (flattenable d)
    then d
    else if is_free d
    then (
      let r = view d in
      rewrap r (group_node r.inside))
    else group_node d
  ;;

  (* Generated, so no two frames share a tag and a document carries its own
     scoping. Atomic, so two domains building documents at once never take the
     same one. *)
  let next_tag = Atomic.make 0
  let new_tag () = Atomic.fetch_and_add next_tag 1

  (* A frame measures its own conditionals at their flat branch, which is the
     branch they take whenever it is laid out flat, and leaves the rest free.

     Unlike [group], this keeps its node when [d] holds a hardline. A frame
     laid out broken binds nothing, so its conditionals take their broken
     branch, as a conditional with no frame does, and the output would be the
     same without the node. [check] would then report the conditionals inside
     as outside their frame. *)
  let frame tag d =
    if is_empty d
    then d
    else (
      let r = view d in
      let fixed, fixed_ok =
        match f_find tag r.frees with
        | Some own -> W.add r.fixed own.flat_w, r.fixed_ok && own.flat_ok
        | None -> r.fixed, r.fixed_ok
      in
      let rest = f_remove tag r.frees in
      let flattenable = fixed_ok && broken_ok_of rest in
      let req = if flattenable then W.add fixed (broken_w_of rest) else W.zero in
      region fixed fixed_ok rest (Frame { flattenable; req; tag; d = r.inside }))
  ;;

  (* A conditional is free until its frame, so it is measured at its broken
     branch, and its region records both branches for the frame. Each branch is
     measured at its widest; see the comment above [free]. *)
  let frame_alt tag a b =
    let flat_w, flat_ok = widest a
    and broken_w, broken_ok = widest b in
    let e = { flat_w; flat_ok; broken_w; broken_ok; wider_w = wider flat_w broken_w } in
    let req = if broken_ok then broken_w else W.zero in
    Free
      { fixed = W.zero
      ; fixed_ok = true
      ; frees = f_node F_leaf tag e F_leaf
      ; inside = Falt { flattenable = broken_ok; req; tag; f = a; b }
      ; all_w = req
      ; all_ok = broken_ok
      }
  ;;

  (* The tag is taken before the body is built, because the conditionals that
     read it are inside. The callback is what keeps them inside: [alt] exists
     only while the frame's body is being built. *)
  let framed body =
    let tag = new_tag () in
    frame tag (body (frame_alt tag))
  ;;

  let nest_node j d = Nest { flattenable = flattenable d; req = flat_width d; j; d }

  let nest j d =
    if is_empty d || j = 0
    then d
    else if is_free d
    then (
      let r = view d in
      rewrap r (nest_node j r.inside))
    else nest_node j d
  ;;

  let align_node d = Align { flattenable = flattenable d; req = flat_width d; d }

  let align d =
    if is_empty d
    then d
    else if is_free d
    then (
      let r = view d in
      rewrap r (align_node r.inside))
    else align_node d
  ;;

  (* Kept for [Empty] as well. An annotated empty region is a position in the
     stream, which a source map can use. *)
  let annotate_node a d = Annot { flattenable = flattenable d; req = flat_width d; a; d }

  let annotate a d =
    if is_free d
    then (
      let r = view d in
      rewrap r (annotate_node a r.inside))
    else annotate_node a d
  ;;

  (* Both rebuild through the smart constructors. [unannotate] can turn a
     non-empty region into an empty one, and the constructors restore the
     invariant that [Empty] is the only empty document, on which [group] and
     [( ^^ )] rely to normalise.

     Rebuilding through them makes this a post-order walk, and the walk is
     explicit for the same reason [check]'s worklist is. Written as
     structural recursion, neither call in [Cat] sits in tail position -- both
     are arguments to [( ^^ )] -- so depth cost stack, and a right-leaning
     [concat] chain is as deep as it is long. At an 8 MB stack, roughly what the
     declared 4.14 floor gets, [reannotate] overflowed at 200k nodes and
     [unannotate] at 2M, inside the range [bench/bench.ml] builds. OCaml 5
     grows the main fibre's stack on demand and hides it, which is why
     [test/test_depth.ml] runs under a cap.

     The continuation is defunctionalised, one entry per node on the way down and
     one step back up per entry. An entry holds the sibling it is still waiting
     for, and once that sibling is finished it holds the finished term instead:
     [R_cat_todo] carries a child still to visit, [R_cat_done] the child already
     rebuilt. Because the finished term lives in the entry, every match here is
     exhaustive: an entry and its operands always agree, so there is no case to
     rule out.

     [reannotate]'s ['a -> 'b] is why an entry's two halves have different
     types. [unannotate] shares the entries and pushes no [R_annot]: it drops
     the annotation, so its child's result is the node's result.

     A rebuilt [Frame] or [Falt] keeps its tag. A fresh one would leave the
     conditionals inside looking for a group that is gone. A [Free] node is
     passed through: the constructors on the way back up put it back. *)
  type ('a, 'b) rebuild =
    | R_done
    | R_cat_todo of 'a t * ('a, 'b) rebuild
    | R_cat_done of 'b t * ('a, 'b) rebuild
    | R_alt_todo of 'a t * ('a, 'b) rebuild
    | R_alt_done of 'b t * ('a, 'b) rebuild
    | R_group of ('a, 'b) rebuild
    | R_nest of int * ('a, 'b) rebuild
    | R_align of ('a, 'b) rebuild
    | R_annot of 'b * ('a, 'b) rebuild
    | R_frame of tag * ('a, 'b) rebuild
    | R_falt_todo of tag * 'a t * ('a, 'b) rebuild
    | R_falt_done of tag * 'b t * ('a, 'b) rebuild

  let reannotate fn d =
    let rec down d k =
      match d with
      | Empty -> up Empty k
      | Text (w, s) -> up (Text (w, s)) k
      | Hard -> up Hard k
      | Cat r -> down r.l (R_cat_todo (r.r, k))
      | Alt r -> down r.f (R_alt_todo (r.b, k))
      | Group r -> down r.d (R_group k)
      | Nest r -> down r.d (R_nest (r.j, k))
      | Align r -> down r.d (R_align k)
      | Annot r -> down r.d (R_annot (fn r.a, k))
      | Frame r -> down r.d (R_frame (r.tag, k))
      | Falt r -> down r.f (R_falt_todo (r.tag, r.b, k))
      | Free r -> down r.inside k
    and up v k =
      match k with
      | R_done -> v
      | R_cat_todo (r, k) -> down r (R_cat_done (v, k))
      | R_cat_done (l, k) -> up (l ^^ v) k
      | R_alt_todo (b, k) -> down b (R_alt_done (v, k))
      | R_alt_done (f, k) -> up (flat_alt f v) k
      | R_group k -> up (group v) k
      | R_nest (j, k) -> up (nest j v) k
      | R_align k -> up (align v) k
      | R_annot (a, k) -> up (annotate a v) k
      | R_frame (tag, k) -> up (frame tag v) k
      | R_falt_todo (tag, b, k) -> down b (R_falt_done (tag, v, k))
      | R_falt_done (tag, f, k) -> up (frame_alt tag f v) k
    in
    down d R_done
  ;;

  let unannotate : 'a. 'a t -> unit t =
    fun d ->
    let rec down d k =
      match d with
      | Empty -> up Empty k
      | Text (w, s) -> up (Text (w, s)) k
      | Hard -> up Hard k
      | Cat r -> down r.l (R_cat_todo (r.r, k))
      | Alt r -> down r.f (R_alt_todo (r.b, k))
      | Group r -> down r.d (R_group k)
      | Nest r -> down r.d (R_nest (r.j, k))
      | Align r -> down r.d (R_align k)
      | Annot r -> down r.d k
      | Frame r -> down r.d (R_frame (r.tag, k))
      | Falt r -> down r.f (R_falt_todo (r.tag, r.b, k))
      | Free r -> down r.inside k
    and up v k =
      match k with
      | R_done -> v
      | R_cat_todo (r, k) -> down r (R_cat_done (v, k))
      | R_cat_done (l, k) -> up (l ^^ v) k
      | R_alt_todo (b, k) -> down b (R_alt_done (v, k))
      | R_alt_done (f, k) -> up (flat_alt f v) k
      | R_group k -> up (group v) k
      | R_nest (j, k) -> up (nest j v) k
      | R_align k -> up (align v) k
      | R_annot ((), k) -> up v k
      | R_frame (tag, k) -> up (frame tag v) k
      | R_falt_todo (tag, b, k) -> down b (R_falt_done (tag, v, k))
      | R_falt_done (tag, f, k) -> up (frame_alt tag f v) k
    in
    down d R_done
  ;;

  (* -- checking ------------------------------------------------------------ *)

  (* Numbers tags from 0 in the order they are first met. [check] and [pp] walk a
     document in the same order, so they give every tag the same number, and a
     conditional [check] reports can be found in [pp]'s output. *)
  let numbering () =
    let names = Hashtbl.create 8 in
    fun tag ->
      match Hashtbl.find_opt names tag with
      | Some n -> n
      | None ->
        let n = Hashtbl.length names in
        Hashtbl.add names tag n;
        n
  ;;

  (* The worklist closes each frame after its body, putting back the scope it
     found, so [scope] holds the frames open at the node in hand. *)
  type 'a check_step =
    | Check of 'a t
    | Close of Tag_set.t

  let check d =
    let errs = ref [] in
    let number = numbering () in
    let scope = ref Tag_set.empty in
    let rec go = function
      | [] -> ()
      | Close outer :: rest ->
        scope := outer;
        go rest
      | Check d :: rest ->
        (match d with
         | Empty | Hard -> go rest
         | Text (_, s) ->
           (match String.index_opt s '\n' with
            | Some i -> errs := Newline_in_text { text = s; index = i } :: !errs
            | None -> ());
           go rest
         | Cat r -> go (Check r.l :: Check r.r :: rest)
         | Alt r -> go (Check r.f :: Check r.b :: rest)
         | Group r -> go (Check r.d :: rest)
         | Nest r -> go (Check r.d :: rest)
         | Align r -> go (Check r.d :: rest)
         | Annot r -> go (Check r.d :: rest)
         | Frame r ->
           ignore (number r.tag : int);
           let outer = !scope in
           scope := Tag_set.add r.tag outer;
           go (Check r.d :: Close outer :: rest)
         | Falt r ->
           let n = number r.tag in
           if not (Tag_set.mem r.tag !scope) then errs := Alt_outside_frame n :: !errs;
           go (Check r.f :: Check r.b :: rest)
         | Free r -> go (Check r.inside :: rest))
    in
    go [ Check d ];
    match !errs with
    | [] -> Ok ()
    | es -> Error (List.rev es)
  ;;

  let pp_error ppf = function
    | Newline_in_text { text; index } ->
      Format.fprintf ppf "newline at byte %d of text node %S" index text
    | Alt_outside_frame n ->
      Format.fprintf ppf "(frame-alt %d ...) outside any (frame %d ...)" n n
  ;;

  (* -- printing the document structure ------------------------------------- *)

  (* Iterative for the same reason as [reannotate]: [%a] holds its argument in a
     recursive call, so a deep document was a deep stack. The boxes are opened
     and closed by hand, since a node's box has to stay open across the children
     that follow it on the worklist, and [@[<hov 1>] and [@]] inside one
     [fprintf] would close it too early. A leaf still goes through [fprintf]
     whole, which keeps [%S] and [%d] rendering exactly as they did.

     The output is byte-identical to the recursive form, checked over the
     generated corpus at the time of the change and guarded since by [doc/8],
     which round-trips it through the reader.

     One box per node is still open at the deepest point, so depth costs heap
     inside [Format] as well as here. The renderer makes the same trade. The heap
     grows on demand where the stack has a fixed limit.

     A tag prints as a number local to the printout, counted from 0 in order of
     first appearance. The value [new_tag] handed out depends on how many tags
     the process took before it, so printing that would make the same document
     print differently from one run to the next. *)
  type 'a pp_step =
    | Pp_doc of 'a t
    | Pp_text of string
    | Pp_space
    | Pp_close

  let pp ppf d =
    let enter name =
      Format.pp_open_hovbox ppf 1;
      Format.pp_print_string ppf name
    in
    let number = numbering () in
    let print_tag tag =
      Format.pp_print_space ppf ();
      Format.pp_print_int ppf (number tag)
    in
    let rec go = function
      | [] -> ()
      | Pp_text t :: k ->
        Format.pp_print_string ppf t;
        go k
      | Pp_space :: k ->
        Format.pp_print_space ppf ();
        go k
      | Pp_close :: k ->
        Format.pp_close_box ppf ();
        go k
      | Pp_doc d :: k ->
        (match d with
         | Empty ->
           Format.pp_print_string ppf "empty";
           go k
         | Hard ->
           Format.pp_print_string ppf "hardline";
           go k
         | Text (_, s) ->
           Format.fprintf ppf "@[<hov 1>(text@ %S)@]" s;
           go k
         | Cat r ->
           enter "(cat";
           go
             (Pp_space
              :: Pp_doc r.l
              :: Pp_space
              :: Pp_doc r.r
              :: Pp_text ")"
              :: Pp_close
              :: k)
         | Alt r ->
           enter "(flat-alt";
           go
             (Pp_space
              :: Pp_doc r.f
              :: Pp_space
              :: Pp_doc r.b
              :: Pp_text ")"
              :: Pp_close
              :: k)
         | Group r ->
           enter "(group";
           go (Pp_space :: Pp_doc r.d :: Pp_text ")" :: Pp_close :: k)
         | Nest r ->
           enter "(nest";
           Format.pp_print_space ppf ();
           Format.pp_print_int ppf r.j;
           go (Pp_space :: Pp_doc r.d :: Pp_text ")" :: Pp_close :: k)
         | Align r ->
           enter "(align";
           go (Pp_space :: Pp_doc r.d :: Pp_text ")" :: Pp_close :: k)
         | Annot r ->
           enter "(annotate";
           go (Pp_space :: Pp_doc r.d :: Pp_text ")" :: Pp_close :: k)
         | Frame r ->
           enter "(frame";
           print_tag r.tag;
           go (Pp_space :: Pp_doc r.d :: Pp_text ")" :: Pp_close :: k)
         | Falt r ->
           enter "(frame-alt";
           print_tag r.tag;
           go
             (Pp_space
              :: Pp_doc r.f
              :: Pp_space
              :: Pp_doc r.b
              :: Pp_text ")"
              :: Pp_close
              :: k)
         | Free r -> go (Pp_doc r.inside :: k))
    in
    go [ Pp_doc d ]
  ;;

  (* -- the rendered stream ------------------------------------------------- *)

  type 'a stream =
    | S_empty
    | S_text of width * string * 'a stream
    | S_line of int * 'a stream
    | S_ann_push of 'a * 'a stream
    | S_ann_pop of 'a stream

  type resolutions = { declined : (int * width) list }

  let spaces_cache = Array.init 65 ~f:(fun i -> String.make i ' ')
  let iw_cache = Array.map ~f:W.measure spaces_cache

  (* [iw_pow.(k)] is the width of [2^k] spaces, so an arbitrary indentation is a
     fold over the set bits of [n]: O(log n) additions, allocation-free.
     Measuring a freshly built string of spaces is O(n) in both, which makes a
     deeply indented document quadratic in [render] as well as in its output.

     This relies on additivity of [measure] over concatenation, as the cached
     document width does. *)
  let iw_pow =
    (* One entry per bit an [int] can hold below the sign, so [indent_width] is
       defined for every non-negative [int]. A table sized to a plausible
       indentation leaves [indent_width] indexing past the end, and [render]
       raising, once a [nest] is large enough. *)
    let a = Array.make (Sys.int_size - 1) iw_cache.(1) in
    for k = 1 to Array.length a - 1 do
      a.(k) <- W.add a.(k - 1) a.(k - 1)
    done;
    a
  ;;

  let indent_width n =
    if n <= 0
    then iw_cache.(0)
    else if n < 65
    then iw_cache.(n)
    else (
      let w = ref iw_cache.(0)
      and n = ref n
      and k = ref 0 in
      while !n > 0 do
        if !n land 1 = 1 then w := W.add !w iw_pow.(!k);
        n := !n lsr 1;
        incr k
      done;
      !w)
  ;;

  (* The largest number of spaces whose width falls within [c], the inverse of
     [indent_width]. [align] sets the indentation to the current column, and the
     column is a [W.t] where indentation is a count of spaces.

     Binary search, since the signature leaves the width of a space open. Where a
     space measures zero, every count of spaces fits and the answer is taken to
     be zero. *)
  let spaces_for c =
    if W.compare c iw_cache.(1) < 0 || W.compare iw_cache.(1) iw_cache.(0) <= 0
    then 0
    else (
      let hi = ref 2 in
      while !hi < max_int / 2 && W.compare (indent_width !hi) c <= 0 do
        hi := !hi * 2
      done;
      let lo = ref (!hi / 2)
      and hi = ref !hi in
      (* invariant: indent_width !lo <= c < indent_width !hi, where the loop
         above left by the width test.

         Where it left on the [max_int / 2] guard instead, the upper half does
         not hold: [indent_width !hi <= c] still does, and the search converges
         to [!hi - 1], a count it never established a bound for. The answer is
         still usable: every count of spaces fits, and 2^61 - 1 is a reasonable
         way to say so. It is a different claim from the line above, so the
         comment says which exit each
         one describes. Reaching that exit
         needs a [W.t] whose [indent_width] never passes [c]. A saturating
         measure does it, and is a legal instance.

         The midpoint does not overflow. [hi] starts at 2 and only doubles, so it
         is a power of two, and the guard caps it at 2^61: [lo + hi] is then at
         most 3 * 2^60 against [max_int] = 2^62 - 1, and the search only narrows
         from there. On 32-bit it is 3 * 2^28 against 2^30 - 1. *)
      while !hi - !lo > 1 do
        let m = (!lo + !hi) / 2 in
        if W.compare (indent_width m) c <= 0 then lo := m else hi := m
      done;
      !lo)
  ;;

  (* Indentation added from [spaces_cache] a chunk at a time. [String.make]
     allocated a string per line past the 64 the cache covers, so the garbage
     scaled with the number of lines: 200k lines at indentation 65 cost 2.00
     Mwords, and at 100, 2.80.

     The bound stays checked here. It came for free from [String.make] and
     {!to_string} documents it, so an explicit comparison keeps it now that
     nothing is allocated; without one a Buffer past its own limit would raise
     something else, much later. A count at or below zero adds nothing, since
     [nest] takes a negative adjustment and the renderer does not clamp it. *)
  let add_indent b n =
    if n > Sys.max_string_length then invalid_arg "Handsome.to_string: indentation";
    let n = ref n in
    while !n > 64 do
      Buffer.add_string b spaces_cache.(64);
      n := !n - 64
    done;
    if !n > 0 then Buffer.add_string b spaces_cache.(!n)
  ;;

  let to_string s =
    let b = Buffer.create 256 in
    let rec go = function
      | S_empty -> ()
      | S_text (_, t, k) ->
        Buffer.add_string b t;
        go k
      | S_line (n, k) ->
        Buffer.add_char b '\n';
        add_indent b n;
        go k
      | S_ann_push (_, k) | S_ann_pop k -> go k
    in
    go s;
    Buffer.contents b
  ;;

  (* Two passes over the stream and nothing allocated but the result. Counting
     the breaks first gives the length directly, where accumulating a list cost
     the list, a [List.length] walk of it, and a [List.iteri] filling the array
     back to front. Both passes are tail-recursive, as everything that walks a
     document or a stream here is. *)
  let lines s =
    let breaks = ref 0 in
    let rec count = function
      | S_empty -> ()
      | S_text (_, _, k) -> count k
      | S_line (_, k) ->
        incr breaks;
        count k
      | S_ann_push (_, k) | S_ann_pop k -> count k
    in
    count s;
    (* [lines] returns one entry more than there are breaks. *)
    let a = Array.make (!breaks + 1) W.zero in
    let i = ref 0
    and cur = ref W.zero in
    let rec fill = function
      | S_empty -> ()
      | S_text (w, _, k) ->
        cur := W.add !cur w;
        fill k
      | S_line (n, k) ->
        a.(!i) <- !cur;
        incr i;
        cur := indent_width n;
        fill k
      | S_ann_push (_, k) | S_ann_pop k -> fill k
    in
    fill s;
    a.(!i) <- !cur;
    a
  ;;

  (* -- the renderer -----------------------------------------------------------

     State is one mutable record, saved and restored around [nest], [align],
     [group] and [framed] by entries on an explicit continuation. The
     continuation is held in the record as well, so the exception handler in
     [drive] can see where the failure occurred. The engine is tail-recursive
     throughout, so document depth costs heap and leaves the stack flat. That holds of every traversal in the
     library: [check] walks a worklist, and [reannotate], [unannotate] and [pp]
     defunctionalise their continuations, for the reason given above
     [reannotate]. [test/test_depth.ml] tests all five.
     ------------------------------------------------------------------------ *)

  (* A growable array, so [group] can truncate the output back to a saved
     length. *)
  type 'a onode =
    | O_text of W.t * string
    | O_line of int ref
    | O_push of 'a
    | O_pop

  type snap =
    { s_indent : int
    ; s_flat : bool
    ; s_column : W.t
    ; s_line : int
    ; s_len : int
    ; s_pending : int ref option
    ; s_pending_v : int
    ; s_declined : (int * W.t) list
    }

  type 'a kont =
    | KNil
    | KDoc of 'a t * 'a kont
    | KRestore of int * bool * 'a kont
    | KPop of 'a kont
    | KGroup of snap * 'a t * 'a kont
    (* The frames laid out flat that were open before a [Frame] laid out flat,
       put back on the way out. The set is persistent, so capturing it is a
       pointer. *)
    | KFrames of Tag_set.t * 'a kont

  type 'a state =
    { ruler : W.t
    ; mutable indent : int
    ; mutable flat : bool
    ; mutable column : W.t
    ; mutable line : int
    ; (* The most recent line break, holding the indentation it will emit once
         text lands on that line. A line that stays empty has this set to zero,
         so the indentation the engine emits is always followed by something.
         A space at the end of a line comes from a text node. *)
      mutable pending : int ref option
    ; mutable buf : 'a onode array
    ; mutable len : int
    ; (* Accumulated in reverse and reversed once at the end. Named apart from
         [resolutions.declined] so that neither record's fields are resolved by
         type-directed disambiguation. *)
      mutable declined_rev : (int * W.t) list
    ; (* The frames open at this point that were laid out flat. A [Falt] reads
         its tag here where an [Alt] reads [flat]. A frame laid out broken is
         left out, and its conditionals, finding no binding, take their broken
         branch. *)
      mutable flat_frames : Tag_set.t
    ; mutable k : 'a kont
    }

  (* Raised by a hardline reached in flat mode, and caught in [drive], which
     undoes the group that chose flat and lays it out broken. The cached widths
     keep flat mode clear of hardlines, so this stays unraised. *)
  exception Flat_violation

  let push st n =
    if st.len = Array.length st.buf
    then (
      let bigger = Array.make (2 * Array.length st.buf) O_pop in
      Array.blit ~src:st.buf ~src_pos:0 ~dst:bigger ~dst_pos:0 ~len:st.len;
      st.buf <- bigger);
    st.buf.(st.len) <- n;
    st.len <- st.len + 1
  ;;

  let emit_text st w s =
    push st (O_text (w, s));
    st.pending <- None;
    st.column <- W.add st.column w
  ;;

  let emit_break st =
    let ind = if st.indent < 0 then 0 else st.indent in
    (match st.pending with
     | Some r -> r := 0
     | None -> ());
    let r = ref ind in
    push st (O_line r);
    st.pending <- Some r;
    st.line <- st.line + 1;
    st.column <- indent_width ind
  ;;

  let snapshot st =
    { s_indent = st.indent
    ; s_flat = st.flat
    ; s_column = st.column
    ; s_line = st.line
    ; s_len = st.len
    ; s_pending = st.pending
    ; s_pending_v =
        (match st.pending with
         | Some r -> !r
         | None -> 0)
    ; s_declined = st.declined_rev
    }
  ;;

  let restore st s =
    st.indent <- s.s_indent;
    st.flat <- s.s_flat;
    st.column <- s.s_column;
    st.line <- s.s_line;
    st.len <- s.s_len;
    st.pending <- s.s_pending;
    (match s.s_pending with
     | Some r -> r := s.s_pending_v
     | None -> ());
    st.declined_rev <- s.s_declined
  ;;

  let rec run st =
    match st.k with
    | KNil -> ()
    | KDoc (d, k) ->
      st.k <- k;
      step st d
    | KRestore (i, f, k) ->
      st.indent <- i;
      st.flat <- f;
      st.k <- k;
      run st
    | KPop k ->
      push st O_pop;
      st.k <- k;
      run st
    | KGroup (s, _, k) ->
      st.indent <- s.s_indent;
      st.flat <- s.s_flat;
      st.k <- k;
      run st
    | KFrames (fs, k) ->
      st.flat_frames <- fs;
      st.k <- k;
      run st

  and step st d =
    match d with
    | Empty -> run st
    | Text (w, s) ->
      emit_text st w s;
      run st
    | Hard ->
      if st.flat
      then raise Flat_violation
      else (
        emit_break st;
        run st)
    | Cat r ->
      st.k <- KDoc (r.r, st.k);
      step st r.l
    | Alt r ->
      if st.flat
      then (
        (* An elective break resolved flat, recorded at the position it stood
           at. *)
        st.declined_rev <- (st.line, st.column) :: st.declined_rev;
        step st r.f)
      else step st r.b
    | Nest r ->
      st.k <- KRestore (st.indent, st.flat, st.k);
      st.indent <- st.indent + r.j;
      step st r.d
    | Align r ->
      st.k <- KRestore (st.indent, st.flat, st.k);
      st.indent <- spaces_for st.column;
      step st r.d
    | Annot r ->
      push st (O_push r.a);
      st.k <- KPop st.k;
      step st r.d
    | Group r ->
      if st.flat
      then
        (* An enclosing group has already committed to flat, so the layout here
           follows from that and the state stands. *)
        step st r.d
      else (
        (* [r.flattenable] is left unread. [group] returns its argument
           unchanged when the flag is clear, so every [Group] node that exists
           carries it true, and ['a t] is abstract, so none can be built
           elsewhere. Worth spelling out here because the field is in scope:
           reading it would imply it could be false. *)
        let fits = W.compare (W.add st.column r.req) st.ruler <= 0 in
        if not fits
        then step st r.d
        else (
          let s = snapshot st in
          st.flat <- true;
          st.k <- KGroup (s, r.d, st.k);
          step st r.d))
    | Frame r ->
      if st.flat
      then (
        (* Inside a group or frame laid out flat, so this one is flat too. *)
        st.k <- KFrames (st.flat_frames, st.k);
        st.flat_frames <- Tag_set.add r.tag st.flat_frames;
        step st r.d)
      else if not r.flattenable
      then (* A hardline inside it, so it is laid out broken. *)
        step st r.d
      else (
        let fits = W.compare (W.add st.column r.req) st.ruler <= 0 in
        if not fits
        then step st r.d
        else (
          (* [KFrames] goes above [KGroup], so that the rewind in [drive] finds
             the frames that were flat before this one. *)
          let s = snapshot st in
          st.flat <- true;
          st.k <- KGroup (s, r.d, st.k);
          st.k <- KFrames (st.flat_frames, st.k);
          st.flat_frames <- Tag_set.add r.tag st.flat_frames;
          step st r.d))
    | Falt r ->
      (* A conditional takes its flat branch where its frame is open and was
         laid out flat, and its broken branch anywhere else, which is the branch
         every group around it measured. One resolved flat is recorded as a
         [flat_alt] is: it is a choice the engine made flat, and its broken
         branch can hold a break. *)
      if Tag_set.mem r.tag st.flat_frames
      then (
        st.declined_rev <- (st.line, st.column) :: st.declined_rev;
        step st r.f)
      else step st r.b
    | Free r -> step st r.inside
  ;;

  (* The [KGroup] that entered flat mode, and the frames that were flat when it
     did. A frame opened since then and still open left the set it found on a
     [KFrames]; the one nearest the [KGroup] was the first opened, and found
     the set the snapshot saw. A frame opened and closed since then put the set
     back. So where no [KFrames] lies above the [KGroup], the set in hand is
     the one the snapshot saw. The snapshot does not carry it, which saves a
     word on every group that fits. *)
  let rec unwind before = function
    | KNil -> None
    | KGroup (s, d, k) -> Some (s, d, k, before)
    | KFrames (fs, k) -> unwind (Some fs) k
    | KDoc (_, k) | KRestore (_, _, k) | KPop k -> unwind before k
  ;;

  (* Recovery from a hardline reached in flat mode. Flat mode is entered where
     the cached width is finite, and a hardline makes it infinite, so the raise
     stays unreached; this branch makes [render] total by construction as well as
     by that argument.

     The recovery is local. It unwinds to the group that chose flat, undoes
     everything that group emitted, and lays it out broken; frames above the
     group are left as they were. The frames laid out flat since go with the
     rest: [unwind] finds the set from before them. Where the group that chose
     flat is itself a frame, its conditionals then have no binding and take
     their broken branch, which is what a frame laid out broken gives them.

     Making [Hard] flattenable brings flat mode within reach of a hardline and
     fires this on most of the test corpus. Every document still renders as the
     reference says, apart from the one case measured conservatively: a
     conditional in another's branch, free there, with a hardline in one of its
     branches. The rule lays that out broken, and the edit lets it lay out flat
     and take the other branch, never reaching here. So the recovery produces
     the correct layout when it runs.

     TODO: decide whether this stays; the question is still open. The branch
     cannot run, so its cost buys nothing today: [snapshot] allocates nine words
     on every group that fits, and the ordinary exit at [KGroup] reads two of
     the eight fields it captured. The other six are there for the rewind below.

     The numbers, so the next reader can skip measuring them. Substituting
     [KRestore] for [KGroup] and dropping [snapshot] came out at -27% allocation
     on the "every atom in its own group" family, 1.17x at n = 10^5 and 1.06x at
     n = 10^6, with output byte-identical at widths 0, 1, 3, 7, 20 and 60.

     Removing it takes [snap], [snapshot], [restore], [unwind], [Flat_violation]
     and this arm with it, and collapses [drive] into [run]. It would then want
     the benchmark numbers re-measured, the allocation figures on the document
     type revisited, and a fresh look at what [hardline-is-flattenable] reddens
     once there is nothing left to recover.

     The unreachability is checked. [st.flat] is set true in two places, at a
     [Group] and at a [Frame]. A [Group] node is built in one place and only
     behind [flattenable], a [Frame] enters flat mode only behind its own flag,
     and ['a t] is abstract, so no caller can build one another way. A frame's
     conditional counts as flattenable where the branch that flat mode gives it
     is: the broken one for a group between it and its frame, the flat one for
     the frame and the groups around it, and both inside another conditional's
     branch. That keeps the argument true of it. Recomputing the flag from
     the structure alone, trusting no cached field, agreed with it at 2.1M
     subterms of 200k generated documents built to break it, and nothing arrived
     here across 2.2M renders; both figures predate [framed].
     [flat-violation-unreachable] reddens nothing in the suite. The gap in all
     that is a later constructor whose [flattenable] does not propagate, which is
     the case this branch is for. *)
  let rec drive st =
    match
      try
        run st;
        None
      with
      | Flat_violation -> Some ()
    with
    | None -> ()
    | Some () ->
      (* [unwind] stops at a [KGroup] entry. Only a group or a frame enters flat
         mode, and each pushes a [KGroup] as it does, so a hardline that raises
         in flat mode always has one below it and the search always finds it.
         [unwind] lists the other kinds of entry, so that adding one forces a
         decision there. *)
      (match unwind None st.k with
       | Some (s, d, k, before) ->
         restore st s;
         Option.iter (fun fs -> st.flat_frames <- fs) before;
         st.flat <- false;
         st.k <- KDoc (d, KRestore (s.s_indent, s.s_flat, k));
         drive st
       | None -> ())
  ;;

  let render ~width d =
    let st =
      { ruler = width
      ; indent = 0
      ; flat = false
      ; column = W.zero
      ; line = 0
      ; pending = None
      ; buf = Array.make 32 O_pop
      ; len = 0
      ; declined_rev = []
      ; flat_frames = Tag_set.empty
      ; k = KDoc (d, KNil)
      }
    in
    drive st;
    (* The last line's indentation is trailing whitespace. *)
    (match st.pending with
     | Some r -> r := 0
     | None -> ());
    let s = ref S_empty in
    for i = st.len - 1 downto 0 do
      s
      := match st.buf.(i) with
         | O_text (w, t) -> S_text (w, t, !s)
         | O_line r -> S_line (!r, !s)
         | O_pop -> S_ann_pop !s
         | O_push a -> S_ann_push (a, !s)
    done;
    !s, { declined = List.rev st.declined_rev }
  ;;
end

module Ascii = Make (Ascii_width)
module Utf8 = Make (Utf8_width)
