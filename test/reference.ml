(* A second, deliberately naive implementation of the layout algorithm, written
   against the surface syntax and producing a string directly.

   It gives "the rendered bytes" an independent meaning, which the stream
   fidelity law needs: [to_string] as the sole authority on the bytes leaves that
   law comparing a fold with itself. This renderer reaches the same bytes by a
   second route, walking the document to recompute each group's flat width and
   emitting straight to a buffer.

   [measure] is a parameter for the reason it is one in the library: the laws
   run under Utf8 as well as Ascii, and both need a reference.

   Tags are indices here, the way [Surface] writes them: a list of the enclosing
   [Framed]s' resolutions, innermost first, stands in for the engine's table.
   An index past the end of it is a conditional with no frame, and takes its
   broken branch. *)

open StdLabels
open Surface

let add a b =
  match a, b with
  | Some x, Some y -> Some (x + y)
  | _ -> None
;;

(* The width a group measures [d] at when deciding whether it fits; [None]
   where it holds a hardline that counts.

   [depth] is how many [Framed]s lie between [d]'s root and the node in hand, so
   a [Frame_alt] whose index falls below it has its frame inside [d]. A group
   only decides while every frame around it is broken, so such a conditional
   counts at its flat branch, and any other at its broken branch. Inside a
   branch of a [Frame_alt], one that is free in that branch counts at the
   wider of its two, which is what [wider] says. Branches are measured from
   their own root, so that "free in that branch" is decided relative to it. *)
let rec measure_at ~measure ~wider depth : Surface.t -> int option = function
  | Text s -> Some (measure s)
  | Empty -> Some 0
  | Cat (a, b) ->
    add (measure_at ~measure ~wider depth a) (measure_at ~measure ~wider depth b)
  | Concat ds ->
    List.fold_left
      ~f:(fun acc d -> add acc (measure_at ~measure ~wider depth d))
      ~init:(Some 0)
      ds
  | Flat_alt (a, _) -> measure_at ~measure ~wider depth a
  | Line -> Some (measure " ")
  | Softline -> Some 0
  | Hardline -> None
  | Blank -> Some (measure " ")
  | Group d | Nest (_, d) | Align d | Annot (_, d) -> measure_at ~measure ~wider depth d
  | Framed d -> measure_at ~measure ~wider (depth + 1) d
  | Frame_alt (i, a, b) ->
    let branch x = measure_at ~measure ~wider:true 0 x in
    if i < depth
    then branch a
    else if wider
    then (
      match branch a, branch b with
      | Some x, Some y -> Some (max x y)
      | _ -> None)
    else branch b
;;

let flat_width ~measure d = measure_at ~measure ~wider:false 0 d

(* What [d] puts on the line it starts on when laid out broken, and whether a
   break ends that line inside it: the share of the line a pending document
   takes, which is what the [Line] rule adds to a group's own width.

   Everything pending when a group decides is laid out broken, because a group
   only decides while every group and frame around it is broken. So a
   conditional counts at its broken branch here. A group or frame inside counts
   at its flat width, since it decides for itself when it is reached; that is
   the assumption Lindig's [fits] makes. One that cannot be laid out flat is laid
   out broken, and counts as its content does. *)
let rec lead ~measure : Surface.t -> int * bool = function
  | Text s -> measure s, false
  | Empty | Blank -> 0, false
  | Line | Softline | Hardline -> 0, true
  | Cat (a, b) -> lead_seq ~measure [ a; b ]
  | Concat ds -> lead_seq ~measure ds
  | Flat_alt (_, b) | Frame_alt (_, _, b) -> lead ~measure b
  | Nest (_, d) | Align d | Annot (_, d) -> lead ~measure d
  | (Group d | Framed d) as g ->
    (match flat_width ~measure g with
     | Some w -> w, false
     | None -> lead ~measure d)

and lead_seq ~measure = function
  | [] -> 0, false
  | d :: ds ->
    let w, b = lead ~measure d in
    if b
    then w, true
    else (
      let w', b' = lead_seq ~measure ds in
      w + w', b')
;;

type rendered =
  { bytes : string
  ; declined : (int * int) list
    (** the flat_alts and frames' conditionals resolved flat, in order *)
  }

(* The flat rendering: every elective break resolved flat, so the result is a
   single line. [None] where no group around the document could lay it out flat,
   which is where [flat_width] says so. Written directly, so that a property
   relating this to [render] at a wide ruler compares two implementations.

   Every frame is flat here, so a conditional takes its flat branch where its
   frame is in the document and its broken branch where it has none. *)
let flat d =
  let b = Buffer.create 64 in
  let rec go depth = function
    | Text s -> Buffer.add_string b s
    | Empty -> ()
    | Cat (a, c) ->
      go depth a;
      go depth c
    | Concat ds -> List.iter ~f:(go depth) ds
    | Flat_alt (a, _) -> go depth a
    | Line | Blank -> Buffer.add_char b ' '
    | Softline -> ()
    | Hardline -> assert false (* ruled out by [flat_width] *)
    | Group d | Nest (_, d) | Align d | Annot (_, d) -> go depth d
    | Framed d -> go (depth + 1) d
    | Frame_alt (i, a, c) -> go depth (if i < depth then a else c)
  in
  match flat_width ~measure:String.length d with
  | None -> None
  | Some _ ->
    go 0 d;
    Some (Buffer.contents b)
;;

(* [fit] chooses what a group measures when it decides: under [Content] its own
   flat width, and under [Line] that plus [lead] of everything pending after it,
   walked afresh at each decision. [k] is that pending work, nearest first.
   [Surface] has a [Line] of its own, so the rule's constructors are written
   qualified. *)
let render ?(fit = Handsome.Content) ~measure ~width d =
  let b = Buffer.create 256 in
  let line = ref 0 in
  let declined = ref [] in
  (* Indentation owed to the current line, or [-1] if none is owed. It is paid
     only when something lands on the line, so a line that stays empty ends
     with no whitespace, and neither does the last line. *)
  let pending = ref (-1) in
  let column = ref 0 in
  let emit s =
    if String.length s > 0
    then (
      if !pending >= 0
      then (
        Buffer.add_string b (String.make !pending ' ');
        pending := -1);
      Buffer.add_string b s;
      column := !column + measure s)
  in
  let brk indent =
    let i = if indent < 0 then 0 else indent in
    Buffer.add_char b '\n';
    pending := i;
    column := i;
    incr line
  in
  let tail k =
    match fit with
    | Handsome.Content -> 0
    | Handsome.Line -> fst (lead_seq ~measure k)
  in
  let fits k x =
    match flat_width ~measure x with
    | Some w -> !column + w + tail k <= width
    | None -> false
  in
  let rec go indent flat frames k d =
    match d with
    | Text s -> emit s
    | Empty -> ()
    | Cat (a, c) ->
      go indent flat frames (c :: k) a;
      go indent flat frames k c
    | Concat [] -> ()
    | Concat (x :: xs) ->
      go indent flat frames (Concat xs :: k) x;
      go indent flat frames k (Concat xs)
    | Flat_alt (a, c) ->
      if flat
      then (
        declined := (!line, !column) :: !declined;
        go indent flat frames k a)
      else go indent flat frames k c
    | Line -> go indent flat frames k (Flat_alt (Text " ", Hardline))
    | Softline -> go indent flat frames k (Flat_alt (Empty, Hardline))
    | Blank -> go indent flat frames k (Flat_alt (Text " ", Empty))
    | Hardline -> brk indent
    | Group x -> go indent (flat || fits k x) frames k x
    | Nest (j, x) -> go (indent + j) flat frames k x
    | Align x -> go !column flat frames k x
    | Annot (_, x) -> go indent flat frames k x
    | Framed x ->
      let resolved = flat || fits k d in
      go indent resolved (resolved :: frames) k x
    (* Recorded in [declined] where it takes its flat branch, as a [Flat_alt]
       is. *)
    | Frame_alt (i, a, c) ->
      let take_flat =
        match List.nth_opt frames i with
        | Some resolved -> resolved
        | None -> false
      in
      if take_flat
      then (
        declined := (!line, !column) :: !declined;
        go indent flat frames k a)
      else go indent flat frames k c
  in
  go 0 false [] [] d;
  { bytes = Buffer.contents b; declined = List.rev !declined }
;;
