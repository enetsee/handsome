(* The [Line] fit rule: a group measures what follows it on the line as well as
   its own content.

   PPrint decides by content alone, so the differential covers [Line] only where
   the two rules agree, on documents where every group is followed directly by
   a break. Everywhere else [Line] is checked against [Reference], which walks
   the pending work afresh at each decision where the library carries a running
   total. The two share the rule and nothing of its implementation. *)

open StdLabels
open QCheck2

module type INSTANCE = sig
  val name : string
  val measure : string -> int

  module H : Handsome.S with type width = int
end

module Make (P : INSTANCE) = struct
  module H = P.H
  module D = Surface.Doc (P.H)

  let render ~width s = H.render ~fit:Line ~width (D.to_doc s)
  let ref_render ~width s = Reference.render ~fit:Line ~measure:P.measure ~width s
  let at_all_widths p s = List.for_all ~f:(fun w -> p w s) Surface.widths

  let test_on ?(count = 1000) name gen prop =
    QCheck_alcotest.to_alcotest
      ~speed_level:`Quick
      (Test.make ~count ~name ~print:Surface.show gen prop)
  ;;

  let test ?count name flavour prop = test_on ?count name (Surface.gen flavour) prop

  let stream_fidelity =
    test
      "under Line, the bytes are the reference's"
      Surface.wild
      (at_all_widths (fun width s ->
         String.equal
           (H.to_string (fst (render ~width s)))
           (ref_render ~width s).Reference.bytes))
  ;;

  let declined_complete =
    test
      "under Line, declined is the reference's"
      Surface.wild
      (at_all_widths (fun width s ->
         (snd (render ~width s)).H.declined = (ref_render ~width s).Reference.declined))
  ;;

  let deep_frames =
    test_on
      ~count:500
      "under Line, many frames free in one region lay out as the reference says"
      Surface.gen_deep
      (at_all_widths (fun width s ->
         let stream, r = render ~width s in
         let want = ref_render ~width s in
         String.equal (H.to_string stream) want.Reference.bytes
         && r.H.declined = want.Reference.declined))
  ;;

  (* The guarantee [Line] exists for. A group laid out flat was measured with
     everything up to the next break, so the line its declined breaks sit on
     stays within the ruler.

     A group further along that line is counted at its flat width, and it does
     lay out flat: it measures the same things from a column the first group
     already counted, so it fits by the same sum. Everything else up to the
     break is counted as it prints, at its broken branch. So this holds for any
     document without a newline in its text, general [flat_alt] and frames
     included. *)
  let line_within_ruler =
    test
      "under Line, a line holding a declined break stays within the ruler"
      Surface.wild
      (at_all_widths (fun width s ->
         let stream, r = render ~width s in
         let ls = H.lines stream in
         List.for_all ~f:(fun (l, _) -> ls.(l) <= width) r.H.declined))
  ;;

  (* MUTATION line within ruler:
       in [reach], replace
           | Line -> W.add (W.add st.column req) st.tail
       with
           | Line -> W.add st.column req
     -- [Line] decides as [Content] does, and a group fits with what follows it
     running past the ruler. *)

  let suite =
    ( "fit-" ^ P.name
    , [ stream_fidelity; declined_complete; deep_frames; line_within_ruler ] )
  ;;
end

module Ascii = Make (struct
    let name = "ascii"
    let measure = String.length

    module H = Handsome.Ascii
  end)

module Utf8 = Make (struct
    let name = "utf8"
    let measure = Handsome.Utf8_width.measure

    module H = Handsome.Utf8
  end)

(* -- examples --------------------------------------------------------------- *)

module H = Handsome.Ascii

let ( ^^ ) = H.( ^^ )
let show ?fit width d = H.to_string (fst (H.render ?fit ~width d))
let case name f = name, `Quick, f

let continuation_measured =
  case "a group measures what follows it on the line" (fun () ->
    let d = H.group (H.text "ab" ^^ H.line ^^ H.text "cd") ^^ H.text "efgh" in
    Alcotest.(check string) "Content" "ab cdefgh" (show 6 d);
    Alcotest.(check string) "Line" "ab\ncdefgh" (show ~fit:Line 6 d))
;;

let continuation_ends_at_a_break =
  (* Everything pending when a group decides is laid out broken, so the [line]
     after each argument is a newline and the measure stops there. Measured
     with its flat branch, the tail would run to the closing bracket and every
     argument but the last would break. *)
  case "the measure stops at the next break" (fun () ->
    let arg s = H.group (H.text s ^^ H.line ^^ H.text s) in
    let d =
      H.group
        (H.text "f("
         ^^ H.nest
              2
              (H.line
               ^^ arg "aaa"
               ^^ H.text ","
               ^^ H.line
               ^^ arg "bbb"
               ^^ H.text ","
               ^^ H.line
               ^^ arg "ccc")
         ^^ H.text ")")
    in
    let want = "f(\n  aaa aaa,\n  bbb bbb,\n  ccc ccc)" in
    Alcotest.(check string) "Content" want (show 12 d);
    Alcotest.(check string) "Line" want (show ~fit:Line 12 d))
;;

let counted width d =
  let stream, _ = H.render ~fit:Line ~width d in
  Array.length (H.lines stream), H.to_string stream
;;

let monotone_where_content_is_not =
  (* The counterexample pinned in [test_laws.ml]. Under [Content], widening the
     ruler from 2 to 3 flattens the first group, which spends the columns the
     second group needed. Under [Line] the first group counts the second at its
     flat width, so it flattens only where both fit. *)
  case "the counterexample to monotonicity under Content is monotone" (fun () ->
    let d = H.text "bb" ^^ H.group H.line ^^ H.group (H.line ^^ H.line) in
    let at = counted in
    Alcotest.(check (pair int string)) "width 2" (2, "bb\n  ") (at 2 d);
    Alcotest.(check (pair int string)) "width 3" (2, "bb\n  ") (at 3 d);
    Alcotest.(check (pair int string)) "width 5" (1, "bb   ") (at 5 d))
;;

let align_breaks_monotonicity =
  (* Found by generating. Widening the ruler from 2 to 3 flattens [group line],
     which moves the column to 3. The [line] inside [align] then breaks to that
     column, so the frame starts at 3, where it no longer fits. The frame is
     incidental: [group (text "a" ^^ softline ^^ softline)] in its place does
     the same. The first group's decision looks as far as the next break, and
     the cost lands after it, so both rules give the same output here. *)
  case "widening the ruler can still add lines, through align" (fun () ->
    let d =
      H.text "aa"
      ^^ H.group H.line
      ^^ H.align H.line
      ^^ H.framed (fun alt -> H.text "a" ^^ H.group (H.line ^^ alt H.empty H.hardline))
    in
    Alcotest.(check (pair int string)) "width 2" (3, "aa\n\na ") (counted 2 d);
    Alcotest.(check (pair int string)) "width 3" (4, "aa \n   a\n\n") (counted 3 d))
;;

(* -- the differential, where the rules agree ---------------------------------

   Where every group is followed directly by a [line], the tail at each
   decision is zero: the [line] is pending, and pending work is laid out
   broken. [Line] then decides as [Content] does, and so as PPrint does. This
   covers the tail's bookkeeping and [breaks]; a tail of any width is left to
   the reference. *)

let rec seal : Surface.t -> Surface.t = function
  | Group d -> Cat (Group (seal d), Line)
  | Cat (a, b) -> Cat (seal a, seal b)
  | Concat ds -> Concat (List.map ~f:seal ds)
  | Flat_alt (a, b) -> Flat_alt (seal a, seal b)
  | Nest (j, d) -> Nest (j, seal d)
  | Align d -> Align (seal d)
  | Annot (a, d) -> Annot (a, seal d)
  | Framed d -> Framed (seal d)
  | Frame_alt (i, a, b) -> Frame_alt (i, seal a, seal b)
  | (Text _ | Empty | Line | Softline | Hardline | Blank) as d -> d
;;

let sealed_differential =
  QCheck_alcotest.to_alcotest
    ~speed_level:`Quick
    (Test.make
       ~count:3000
       ~name:"under Line, agrees with PPrint where each group is followed by a line"
       ~print:Surface.show
       (Gen.map seal (Surface.gen Surface.rich))
       (fun s ->
          List.for_all
            ~f:(fun width ->
              String.equal
                (show ~fit:Line width (Surface.to_doc s))
                (Surface.pprint_to_string ~f:Surface.to_pprint ~width s))
            Surface.widths))
;;

let suites =
  [ Ascii.suite
  ; Utf8.suite
  ; ( "fit"
    , [ continuation_measured
      ; continuation_ends_at_a_break
      ; monotone_where_content_is_not
      ; align_breaks_monotonicity
      ; sealed_differential
      ] )
  ]
;;
