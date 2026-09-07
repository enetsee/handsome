(* A second, deliberately naive implementation of the layout algorithm, written
   against the surface syntax and producing a string directly.

   It gives "the rendered bytes" an independent meaning, which the stream
   fidelity law needs: [to_string] as the sole authority on the bytes leaves that
   law comparing a fold with itself. This renderer reaches the same bytes by a
   second route, walking the document to recompute each group's flat width and
   emitting straight to a buffer.

   [measure] is a parameter for the reason it is one in the library: the laws
   run under Utf8 as well as Ascii, and both need a reference. *)

open StdLabels
open Surface

let add a b =
  match a, b with
  | Some x, Some y -> Some (x + y)
  | _ -> None
;;

(* The width of a document laid out flat; [None] where it holds a hardline. *)
let rec flat_width ~measure : Surface.t -> int option = function
  | Text s -> Some (measure s)
  | Empty -> Some 0
  | Cat (a, b) -> add (flat_width ~measure a) (flat_width ~measure b)
  | Concat ds ->
    List.fold_left ~f:(fun acc d -> add acc (flat_width ~measure d)) ~init:(Some 0) ds
  | Flat_alt (a, _) -> flat_width ~measure a
  | Line -> Some (measure " ")
  | Softline -> Some 0
  | Hardline -> None
  | Blank -> Some (measure " ")
  | Group d | Nest (_, d) | Align d | Annot (_, d) -> flat_width ~measure d
;;

type rendered =
  { bytes : string
  ; declined : (int * int) list (** the flat_alts resolved flat, in order *)
  }

(* The flat rendering: every elective break resolved flat, so the result is a
   single line. [None] where the document holds a hardline. Written directly, so
   that a property relating this to [render] at a wide ruler compares two
   implementations. *)
let flat d =
  let b = Buffer.create 64 in
  let exception Unflattenable in
  let rec go = function
    | Text s -> Buffer.add_string b s
    | Empty -> ()
    | Cat (a, c) ->
      go a;
      go c
    | Concat ds -> List.iter ~f:go ds
    | Flat_alt (a, _) -> go a
    | Line | Blank -> Buffer.add_char b ' '
    | Softline -> ()
    | Hardline -> raise Unflattenable
    | Group d | Nest (_, d) | Align d | Annot (_, d) -> go d
  in
  match go d with
  | () -> Some (Buffer.contents b)
  | exception Unflattenable -> None
;;

let render ~measure ~width d =
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
  let rec go indent flat d =
    match d with
    | Text s -> emit s
    | Empty -> ()
    | Cat (a, c) ->
      go indent flat a;
      go indent flat c
    | Concat ds -> List.iter ~f:(go indent flat) ds
    | Flat_alt (a, c) ->
      if flat
      then (
        declined := (!line, !column) :: !declined;
        go indent flat a)
      else go indent flat c
    | Line -> go indent flat (Flat_alt (Text " ", Hardline))
    | Softline -> go indent flat (Flat_alt (Empty, Hardline))
    | Blank -> go indent flat (Flat_alt (Text " ", Empty))
    | Hardline -> brk indent
    | Group x ->
      let fits =
        match flat_width ~measure x with
        | Some w -> !column + w <= width
        | None -> false
      in
      go indent (flat || fits) x
    | Nest (j, x) -> go (indent + j) flat x
    | Align x -> go !column flat x
    | Annot (_, x) -> go indent flat x
  in
  go 0 false d;
  { bytes = Buffer.contents b; declined = List.rev !declined }
;;
