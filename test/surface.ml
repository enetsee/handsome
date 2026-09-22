(* A surface syntax for documents.

   Tests generate values of this type, so the same generated case can be
   interpreted three ways: into a handsome document, into a PPrint document for
   the differential, and into the offences [check] should report, which is its
   ground truth. It also prints and shrinks.

   [Framed] is [framed], and [Frame_alt] one of the conditionals it hands out,
   written as a reference to an enclosing [Framed]: [Frame_alt (0, _, _)] reads
   the innermost [Framed] around it, [1] the next out, and an index past the last
   is a conditional outside its frame. *)

open StdLabels

type t =
  | Text of string
  | Empty
  | Cat of t * t
  | Concat of t list
  | Flat_alt of t * t
  | Line
  | Softline
  | Hardline
  | Blank
  | Group of t
  | Nest of int * t
  | Align of t
  | Annot of int * t
  | Framed of t
  | Frame_alt of int * t * t

(* -- interpretation into handsome ------------------------------------------ *)

module type DOC = Handsome.S with type width = int

(* A functor, because the same generated case has to be built under more than
   one width instance: the laws are re-run under Utf8 as well as Ascii, and
   several properties compare the two renderings of one document. *)
module Doc (H : DOC) = struct
  (* A conditional whose frame has already closed. Letting [alt] escape its
     callback is the only way to place one outside its frame. *)
  let stray () =
    let got = ref None in
    ignore
      (H.framed (fun alt ->
         got := Some alt;
         H.empty)
       : int H.t);
    match !got with
    | Some alt -> alt
    | None -> assert false
  ;;

  (* [alts] holds the conditionals of the enclosing [Framed]s, innermost first,
     and an index past the end takes a stray one. *)
  let to_doc : t -> int H.t =
    let rec go alts = function
      | Text s -> H.text s
      | Empty -> H.empty
      | Cat (a, b) -> H.(go alts a ^^ go alts b)
      | Concat ds -> H.concat (List.map ~f:(go alts) ds)
      | Flat_alt (a, b) -> H.flat_alt (go alts a) (go alts b)
      | Line -> H.line
      | Softline -> H.softline
      | Hardline -> H.hardline
      | Blank -> H.blank
      | Group d -> H.group (go alts d)
      | Nest (j, d) -> H.nest j (go alts d)
      | Align d -> H.align (go alts d)
      | Annot (a, d) -> H.annotate a (go alts d)
      | Framed d -> H.framed (fun alt -> go (alt :: alts) d)
      | Frame_alt (i, a, b) ->
        let alt =
          match List.nth_opt alts i with
          | Some alt -> alt
          | None -> stray ()
        in
        alt (go alts a) (go alts b)
    in
    go []
  ;;
end

module Ascii_doc = Doc (Handsome.Ascii)
module Utf8_doc = Doc (Handsome.Utf8)

(* The Ascii instance, used by the suites that fix the width. *)
let to_doc = Ascii_doc.to_doc

(* -- interpretation into PPrint -----------------------------------------------

   [line], [softline] and [blank] are spelled with [ifflat] and
   [string]/[empty]. PPrint's [blank] is a suppressible space: the renderer
   drops blanks that end up at the end of a line. handsome's [blank] is
   [flat_alt (text " ") empty], a space that vanishes when the group breaks,
   which is a property of the document. Spelling the mapping with [string]
   spaces keeps the comparison to the two layout algorithms.

   [to_pprint_idiomatic] below is the mapping a PPrint user would write, and the
   difference between the two is measured.

   PPrint has no conditional on an outer group, so [Frame_alt] has no
   counterpart and both mappings reject it. [Framed] with nothing reading its tag
   is a group, and maps to one.
   -------------------------------------------------------------------------- *)

let no_counterpart () =
  invalid_arg "Surface.to_pprint: a frame's conditional has no PPrint counterpart"
;;

let ( ^^ ) = PPrint.( ^^ )

let rec to_pprint : t -> PPrint.document = function
  | Text s -> PPrint.string s
  | Empty -> PPrint.empty
  | Cat (a, b) -> to_pprint a ^^ to_pprint b
  | Concat ds ->
    List.fold_right ~f:(fun d acc -> to_pprint d ^^ acc) ds ~init:PPrint.empty
  | Flat_alt (a, b) -> PPrint.ifflat (to_pprint a) (to_pprint b)
  | Line -> PPrint.ifflat (PPrint.string " ") PPrint.hardline
  | Softline -> PPrint.ifflat PPrint.empty PPrint.hardline
  | Hardline -> PPrint.hardline
  | Blank -> PPrint.ifflat (PPrint.string " ") PPrint.empty
  | Group d -> PPrint.group (to_pprint d)
  | Nest (j, d) -> PPrint.nest j (to_pprint d)
  | Align d -> PPrint.align (to_pprint d)
  | Annot (_, d) -> to_pprint d
  | Framed d -> PPrint.group (to_pprint d)
  | Frame_alt _ -> no_counterpart ()
;;

let rec to_pprint_idiomatic : t -> PPrint.document = function
  | Text s -> PPrint.string s
  | Empty -> PPrint.empty
  | Cat (a, b) -> to_pprint_idiomatic a ^^ to_pprint_idiomatic b
  | Concat ds ->
    List.fold_right ~f:(fun d acc -> to_pprint_idiomatic d ^^ acc) ds ~init:PPrint.empty
  | Flat_alt (a, b) -> PPrint.ifflat (to_pprint_idiomatic a) (to_pprint_idiomatic b)
  | Line -> PPrint.break 1
  | Softline -> PPrint.break 0
  | Hardline -> PPrint.hardline
  (* [PPrint.blank 1] is a space in both modes, where handsome's [blank] is a
     space when flat alone. PPrint has this branch alone as its counterpart, so
     it is kept exact and the comparison isolates [break]. *)
  | Blank -> PPrint.ifflat (PPrint.string " ") PPrint.empty
  | Group d -> PPrint.group (to_pprint_idiomatic d)
  | Nest (j, d) -> PPrint.nest j (to_pprint_idiomatic d)
  | Align d -> PPrint.align (to_pprint_idiomatic d)
  | Annot (_, d) -> to_pprint_idiomatic d
  | Framed d -> PPrint.group (to_pprint_idiomatic d)
  | Frame_alt _ -> no_counterpart ()
;;

let pprint_to_string ?(f = to_pprint) ~width d =
  let b = Buffer.create 256 in
  PPrint.ToBuffer.pretty 1.0 width b (f d);
  Buffer.contents b
;;

(* -- ground truth for [check] ---------------------------------------------- *)

type offence =
  | Newline of string * int (** the text node, and the byte its first newline is at *)
  | Outside of int (** the number [check] and [pp] give its frame *)

(* Whether the smart constructors reduce [d] to [empty]. A [Framed] over such a
   body builds no frame, so it takes no number below. *)
let rec is_empty = function
  | Text s -> String.length s = 0
  | Empty -> true
  | Cat (a, b) -> is_empty a && is_empty b
  | Concat ds -> List.for_all ~f:is_empty ds
  | Group d | Nest (_, d) | Align d | Framed d -> is_empty d
  | Annot _ | Flat_alt _ | Line | Softline | Hardline | Blank | Frame_alt _ -> false
;;

(* The offending nodes, in the document order [check] reports them in. Frames
   are numbered from 0 as [check] first meets them: where one opens, and, for a
   conditional outside its frame, where the conditional stands. *)
let offenders d =
  let acc = ref [] in
  let next = ref 0 in
  let fresh () =
    let n = !next in
    incr next;
    n
  in
  let rec go scope = function
    | Text s ->
      (match String.index_opt s '\n' with
       | Some i -> acc := Newline (s, i) :: !acc
       | None -> ())
    | Empty | Line | Softline | Hardline | Blank -> ()
    | Cat (a, b) | Flat_alt (a, b) ->
      go scope a;
      go scope b
    | Concat ds -> List.iter ~f:(go scope) ds
    | Group d | Nest (_, d) | Align d | Annot (_, d) -> go scope d
    | Framed d -> if not (is_empty d) then go (fresh () :: scope) d
    | Frame_alt (i, a, b) ->
      if i >= List.length scope then acc := Outside (fresh ()) :: !acc;
      go scope a;
      go scope b
  in
  go [] d;
  List.rev !acc
;;

(* -- the case measured conservatively ---------------------------------------- *)

(* Whether [d] holds a conditional whose [Framed] is outside it. *)
let rec has_free depth = function
  | Frame_alt (i, a, b) -> i >= depth || has_free depth a || has_free depth b
  | Framed d -> has_free (depth + 1) d
  | Cat (a, b) | Flat_alt (a, b) -> has_free depth a || has_free depth b
  | Concat ds -> List.exists ~f:(has_free depth) ds
  | Group d | Nest (_, d) | Align d | Annot (_, d) -> has_free depth d
  | Text _ | Empty | Line | Softline | Hardline | Blank -> false
;;

(* Whether a conditional's branch holds a conditional free in that branch,
   which the interface measures at its wider branch. *)
let rec nested_free = function
  | Frame_alt (_, a, b) -> has_free 0 a || has_free 0 b || nested_free a || nested_free b
  | Framed d | Group d | Nest (_, d) | Align d | Annot (_, d) -> nested_free d
  | Cat (a, b) | Flat_alt (a, b) -> nested_free a || nested_free b
  | Concat ds -> List.exists ~f:nested_free ds
  | Text _ | Empty | Line | Softline | Hardline | Blank -> false
;;

(* -- printing, for failure output ------------------------------------------ *)

let rec show = function
  | Text s -> Printf.sprintf "(text %S)" s
  | Empty -> "empty"
  | Cat (a, b) -> Printf.sprintf "(cat %s %s)" (show a) (show b)
  | Concat ds -> "(concat " ^ String.concat ~sep:" " (List.map ~f:show ds) ^ ")"
  | Flat_alt (a, b) -> Printf.sprintf "(flat_alt %s %s)" (show a) (show b)
  | Line -> "line"
  | Softline -> "softline"
  | Hardline -> "hardline"
  | Blank -> "blank"
  | Group d -> Printf.sprintf "(group %s)" (show d)
  | Nest (j, d) -> Printf.sprintf "(nest %d %s)" j (show d)
  | Align d -> Printf.sprintf "(align %s)" (show d)
  | Annot (a, d) -> Printf.sprintf "(annotate %d %s)" a (show d)
  | Framed d -> Printf.sprintf "(framed %s)" (show d)
  | Frame_alt (i, a, b) -> Printf.sprintf "(frame_alt %d %s %s)" i (show a) (show b)
;;

(* -- generation ------------------------------------------------------------ *)

open QCheck2

type flavour =
  { newlines : bool (** text nodes may contain '\n' *)
  ; annotations : bool
  ; general_flat_alt : bool (** [flat_alt] with arbitrary branches *)
  ; neg_nest : bool
    (** [nest] with a negative argument. handsome allows it and clamps the
          emitted indentation at zero; PPrint asserts against it, so the
          differential corpora leave it off. *)
  ; unicode : bool
    (** text nodes may be multi-byte. Safe in the differential too: PPrint
          measures [String.length] and so does {!Handsome.Ascii_width}, so the
          two still agree byte for byte on text neither of them understands. *)
  ; malformed : bool (** text nodes may hold bytes outside valid UTF-8 *)
  ; aligns : bool
    (** [align] is one of exactly two ways the measure reaches the output --
          it turns a column into an indentation -- so a property about the other
          way, the fit decision, has to be able to switch it off. *)
  ; frames : bool
    (** [framed] and its conditionals. PPrint has no conditional on an outer
          group, so the differential corpora leave these off, and [framed] is
          compared with no independently written printer. That loss is
          permanent. {!Reference} is the second implementation it is checked
          against, and was written from the same specification. *)
  }

let plain =
  { newlines = false
  ; annotations = false
  ; general_flat_alt = false
  ; neg_nest = false
  ; unicode = false
  ; malformed = false
  ; aligns = true
  ; frames = false
  }
;;

let rich = { plain with annotations = true; general_flat_alt = true; unicode = true }

(* Well-formed UTF-8, exercising dedent and frames, which PPrint cannot
   express. *)
let wild = { rich with neg_nest = true; frames = true }
let no_align = { wild with aligns = false }

(* [wild] restricted to the derived breaks, whose two branches differ in
   whitespace alone. A [Frame_alt] there has branches of the same kind. *)
let breaks_only = { wild with general_flat_alt = false }

(* Everything the library accepts, including what [check] rejects and bytes
   outside any text encoding. *)
let dirty = { wild with newlines = true; malformed = true }
let words = [ "a"; "bb"; "ccc"; "let"; "()"; "->"; "x1"; "hello"; "," ]

(* One per case the Utf8 measure distinguishes, so that a corpus with these in
   it actually reaches all of them. Widths are under Utf8_width. *)
let unicode_words =
  [ "\xe4\xb8\xad" (* U+4E2D, wide: 3 bytes, 2 columns *)
  ; "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e" (* three wide *)
  ; "\xc3\xa9" (* U+00E9, precomposed e-acute: 2 bytes, 1 column *)
  ; "e\xcc\x81" (* e + U+0301 combining acute: 3 bytes, 1 column *)
  ; "\xef\xbc\xa1" (* U+FF21 fullwidth A: 2 columns *)
  ; "\xef\xbd\xb1" (* U+FF71 halfwidth katakana: 1 column *)
  ; "\xe2\x86\x92" (* U+2192, ambiguous width: 1 column *)
  ; "\xf0\x9f\x91\x8d" (* U+1F44D, wide: 4 bytes, 2 columns *)
  ; "\xe3\x80\xaa" (* U+302A, Mn and wide at once: 0 columns *)
  ]
;;

(* Outside valid UTF-8 in any position: a lone continuation byte, a truncated
   sequence, an invalid lead byte. *)
let malformed_words = [ "\x80"; "\xe4\xb8"; "\xff"; "a\xffb" ]
let dirty_words = [ "a\nb"; "\n"; "x\n"; "\ny"; "p\nq\nr" ]

let gen_text flavour =
  let choices =
    [ 6, Gen.oneof_list words; 1, Gen.pure "" ]
    @ (if flavour.unicode then [ 4, Gen.oneof_list unicode_words ] else [])
    @ (if flavour.newlines then [ 1, Gen.oneof_list dirty_words ] else [])
    @ if flavour.malformed then [ 1, Gen.oneof_list malformed_words ] else []
  in
  Gen.oneof_weighted choices
;;

(* Mostly the innermost frame, sometimes one further out, and sometimes a frame
   that is not there. *)
let gen_index = Gen.oneof_weighted [ 5, Gen.pure 0; 2, Gen.pure 1; 1, Gen.pure 2 ]

(* The shapes a caller writes. The trailing separator is the one the primitive is
   for; the other two mirror [blank] and [softline], and differ in whitespace
   alone. *)
let gen_frame_alt_leaf flavour =
  let shapes =
    [ 2, (Text " ", Empty); 1, (Empty, Hardline) ]
    @ if flavour.general_flat_alt then [ 3, (Empty, Text ",") ] else []
  in
  Gen.map2
    (fun i (a, b) -> Frame_alt (i, a, b))
    gen_index
    (Gen.oneof_weighted (List.map ~f:(fun (w, ab) -> w, Gen.pure ab) shapes))
;;

let gen flavour =
  let leaf =
    Gen.oneof_weighted
      ([ 6, Gen.map (fun s -> Text s) (gen_text flavour)
       ; 1, Gen.pure Empty
       ; 3, Gen.pure Line
       ; 3, Gen.pure Softline
       ; 1, Gen.pure Hardline
       ; 2, Gen.pure Blank
       ]
       @ if flavour.frames then [ 2, gen_frame_alt_leaf flavour ] else [])
  in
  let rec node n =
    if n <= 1
    then leaf
    else (
      let half = Gen.sized_size (Gen.pure (n / 2)) node in
      Gen.oneof_weighted
        ([ 2, leaf
         ; 5, Gen.map2 (fun a b -> Cat (a, b)) half half
         ; ( 2
           , Gen.map
               (fun ds -> Concat ds)
               (Gen.list_size
                  (Gen.int_range 0 3)
                  (Gen.sized_size (Gen.pure (n / 3)) node)) )
         ; 5, Gen.map (fun d -> Group d) (Gen.sized_size (Gen.pure (n - 1)) node)
         ; ( 3
           , Gen.map2
               (fun j d -> Nest (j, d))
               (if flavour.neg_nest then Gen.int_range (-3) 6 else Gen.int_range 0 6)
               (Gen.sized_size (Gen.pure (n - 1)) node) )
         ]
         @ (if flavour.aligns
            then
              [ 2, Gen.map (fun d -> Align d) (Gen.sized_size (Gen.pure (n - 1)) node) ]
            else [])
         @ (if flavour.general_flat_alt
            then [ 2, Gen.map2 (fun a b -> Flat_alt (a, b)) half half ]
            else [])
         @ (if flavour.annotations
            then
              [ ( 2
                , Gen.map2
                    (fun a d -> Annot (a, d))
                    (Gen.int_range 0 3)
                    (Gen.sized_size (Gen.pure (n - 1)) node) )
              ]
            else [])
         @ (if flavour.frames
            then
              [ 3, Gen.map (fun d -> Framed d) (Gen.sized_size (Gen.pure (n - 1)) node)
                (* The shape lingo builds: the last element in a group of its
                   own, holding a conditional on the frame around both. A
                   conditional only follows a frame when one encloses it, and
                   only differs from [flat_alt] when a group sits between the
                   two, which the node above reaches by chance and this one
                   reaches every time. *)
              ; ( 2
                , Gen.map3
                    (fun d e alt -> Framed (Cat (d, Group (Cat (e, alt)))))
                    half
                    half
                    (gen_frame_alt_leaf flavour) )
              ]
            else [])
         @
         if flavour.frames && flavour.general_flat_alt
         then [ 2, Gen.map3 (fun i a b -> Frame_alt (i, a, b)) gen_index half half ]
         else []))
  in
  Gen.sized_size (Gen.int_range 1 22) node
;;

(* Frames nested up to 40 deep, with the conditionals of many of them in one
   region: at the innermost point, the way a Lisp printer closes its brackets,
   and here and there on the way in, with some outside every frame. The corpus
   above seldom has more than two frames free in one region, so the tree a frame
   keeps them in would otherwise go untried. *)
let gen_deep =
  let open Gen in
  int_range 1 40
  >>= fun depth ->
  let cond k = map (fun i -> Frame_alt (i, Empty, Text ")")) (int_range 0 (k + 1)) in
  let rec level k =
    if k = depth
    then
      map
        (fun cs -> Group (Concat (Text "x" :: cs)))
        (list_size (int_range 0 (2 * depth)) (cond depth))
    else
      map3
        (fun pre inner post -> Framed (Concat [ pre; Softline; inner; post ]))
        (oneof_list [ Text "("; Text "(("; Empty ])
        (level (k + 1))
        (oneof [ pure Empty; cond (k + 1); map (fun c -> Group c) (cond (k + 1)) ])
  in
  level 0
;;

(* Every property that varies the width runs at all of these, so a case which
   misbehaves only at a narrow ruler is reached on every run. *)
let widths = [ 0; 1; 2; 3; 5; 8; 13; 20; 40; 80 ]
