(* A reader for the s-expressions that [Handsome.pp] emits.

   It exists so that [pp] can be held to a round-trip. Reading the output back
   and printing it again is the cheapest way to establish that [pp] preserves
   the structure a caller would want to report on.

   Annotation payloads are outside what a generic printer can render, so [pp]
   emits [(annotate d)] and this rebuilds [annotate () d]. The round-trip is
   therefore exact on [unit t], and exact up to annotation payloads elsewhere. *)

open StdLabels
module H = Handsome.Ascii

exception Parse_error of string

type token =
  | Lpar
  | Rpar
  | Atom of string
  | Str of string

let tokenise (s : string) : token list =
  let n = String.length s in
  let out = ref [] in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c = ' ' || c = '\n' || c = '\t' || c = '\r'
    then incr i
    else if c = '('
    then (
      out := Lpar :: !out;
      incr i)
    else if c = ')'
    then (
      out := Rpar :: !out;
      incr i)
    else if c = '"'
    then (
      (* Find the closing quote, honouring backslash escapes, then let Scanf
         decode the literal with OCaml's own lexical convention -- the same one
         [%S] printed it with. *)
      let j = ref (!i + 1) in
      let fin = ref (-1) in
      while !fin < 0 do
        if !j >= n
        then raise (Parse_error "unterminated string")
        else if s.[!j] = '\\'
        then j := !j + 2
        else if s.[!j] = '"'
        then fin := !j
        else incr j
      done;
      let lit = String.sub s ~pos:!i ~len:(!fin - !i + 1) in
      out := Str (Scanf.sscanf lit "%S" (fun x -> x)) :: !out;
      i := !fin + 1)
    else (
      let j = ref !i in
      while
        !j < n
        &&
        let c = s.[!j] in
        c <> ' ' && c <> '\n' && c <> '\t' && c <> '\r' && c <> '(' && c <> ')'
      do
        incr j
      done;
      out := Atom (String.sub s ~pos:!i ~len:(!j - !i)) :: !out;
      i := !j)
  done;
  List.rev !out
;;

let read (s : string) : unit H.t =
  let toks = ref (tokenise s) in
  let pop () =
    match !toks with
    | [] -> raise (Parse_error "unexpected end of input")
    | t :: rest ->
      toks := rest;
      t
  in
  let expect_rpar () =
    match pop () with
    | Rpar -> ()
    | Lpar | Atom _ | Str _ -> raise (Parse_error "expected )")
  in
  let rec doc () =
    match pop () with
    | Atom "empty" -> H.empty
    | Atom "hardline" -> H.hardline
    | Atom a -> raise (Parse_error ("unexpected atom " ^ a))
    | Str _ -> raise (Parse_error "unexpected string")
    | Rpar -> raise (Parse_error "unexpected )")
    | Lpar ->
      (match pop () with
       | Atom "text" ->
         let s =
           match pop () with
           | Str s -> s
           | _ -> raise (Parse_error "text expects a string")
         in
         expect_rpar ();
         H.text s
       | Atom "cat" ->
         let a = doc () in
         let b = doc () in
         expect_rpar ();
         H.( ^^ ) a b
       | Atom "flat-alt" ->
         let a = doc () in
         let b = doc () in
         expect_rpar ();
         H.flat_alt a b
       | Atom "group" ->
         let d = doc () in
         expect_rpar ();
         H.group d
       | Atom "nest" ->
         let j =
           match pop () with
           | Atom a -> int_of_string a
           | _ -> raise (Parse_error "nest expects an integer")
         in
         let d = doc () in
         expect_rpar ();
         H.nest j d
       | Atom "align" ->
         let d = doc () in
         expect_rpar ();
         H.align d
       | Atom "annotate" ->
         let d = doc () in
         expect_rpar ();
         H.annotate () d
       | Lpar | Rpar | Atom _ | Str _ -> raise (Parse_error "expected a head symbol"))
  in
  let d = doc () in
  (match !toks with
   | [] -> ()
   | _ -> raise (Parse_error "trailing input"));
  d
;;

let print_doc (d : 'a H.t) : string =
  let b = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer b in
  (* A margin narrow enough that [pp]'s own boxes actually break, so the reader
     is exercised on multi-line output too. *)
  Format.pp_set_margin ppf 60;
  Format.fprintf ppf "%a@?" H.pp d;
  Buffer.contents b
;;
