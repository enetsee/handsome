(* A surface syntax for documents.

   Tests generate values of this type, so the same generated case can be
   interpreted three ways: into a handsome document, into a PPrint document for
   the differential, and into the list of text nodes carrying a newline, which is
   the ground truth for [check]. It also prints and shrinks. *)

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

(* -- interpretation into handsome ------------------------------------------ *)

module type DOC = Handsome.S with type width = int

(* A functor, because the same generated case has to be built under more than
   one width instance: the laws are re-run under Utf8 as well as Ascii, and
   several properties compare the two renderings of one document. *)
module Doc (H : DOC) = struct
  let rec to_doc : t -> int H.t = function
    | Text s -> H.text s
    | Empty -> H.empty
    | Cat (a, b) -> H.(to_doc a ^^ to_doc b)
    | Concat ds -> H.concat (List.map ~f:to_doc ds)
    | Flat_alt (a, b) -> H.flat_alt (to_doc a) (to_doc b)
    | Line -> H.line
    | Softline -> H.softline
    | Hardline -> H.hardline
    | Blank -> H.blank
    | Group d -> H.group (to_doc d)
    | Nest (j, d) -> H.nest j (to_doc d)
    | Align d -> H.align (to_doc d)
    | Annot (a, d) -> H.annotate a (to_doc d)
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
   -------------------------------------------------------------------------- *)

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
;;

let pprint_to_string ?(f = to_pprint) ~width d =
  let b = Buffer.create 256 in
  PPrint.ToBuffer.pretty 1.0 width b (f d);
  Buffer.contents b
;;

(* -- ground truth for [check] ---------------------------------------------- *)

(* The offending text nodes, in the document order [check] reports them in. *)
let offenders d =
  let acc = ref [] in
  let rec go = function
    | Text s ->
      (match String.index_opt s '\n' with
       | Some i -> acc := (s, i) :: !acc
       | None -> ())
    | Empty | Line | Softline | Hardline | Blank -> ()
    | Cat (a, b) | Flat_alt (a, b) ->
      go a;
      go b
    | Concat ds -> List.iter ~f:go ds
    | Group d | Nest (_, d) | Align d | Annot (_, d) -> go d
  in
  go d;
  List.rev !acc
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
  }

let plain =
  { newlines = false
  ; annotations = false
  ; general_flat_alt = false
  ; neg_nest = false
  ; unicode = false
  ; malformed = false
  ; aligns = true
  }
;;

let rich = { plain with annotations = true; general_flat_alt = true; unicode = true }

(* Well-formed UTF-8, exercising dedent. *)
let wild = { rich with neg_nest = true }
let no_align = { wild with aligns = false }

(* [wild] restricted to the derived breaks, whose two branches differ in
   whitespace alone. *)
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

let gen flavour =
  let leaf =
    Gen.oneof_weighted
      [ 6, Gen.map (fun s -> Text s) (gen_text flavour)
      ; 1, Gen.pure Empty
      ; 3, Gen.pure Line
      ; 3, Gen.pure Softline
      ; 1, Gen.pure Hardline
      ; 2, Gen.pure Blank
      ]
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
         @
         if flavour.annotations
         then
           [ ( 2
             , Gen.map2
                 (fun a d -> Annot (a, d))
                 (Gen.int_range 0 3)
                 (Gen.sized_size (Gen.pure (n - 1)) node) )
           ]
         else []))
  in
  Gen.sized_size (Gen.int_range 1 22) node
;;

(* Every property that varies the width runs at all of these, so a case which
   misbehaves only at a narrow ruler is reached on every run. *)
let widths = [ 0; 1; 2; 3; 5; 8; 13; 20; 40; 80 ]
