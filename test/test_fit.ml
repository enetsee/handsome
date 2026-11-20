(* The [Line] fit rule: a group measures what follows it on the line as well as
   its own content.

   PPrint decides by content alone, so the differential covers [Line] only where
   the two rules agree, on documents where every group is followed directly by
   a break. Everywhere else [Line] is checked against [Reference], which walks
   the pending work afresh at each decision where the library carries a running
   total. The two share the rule and nothing of its implementation.

   [group ~yields:true] and [framed ~yields:true] change what [Line] counts, so
   they are tested here. The corpus generates them, so every law below covers
   them, and the laws from [annotations_keep_the_layout] on are about them. *)

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

  let ( ^^ ) = H.( ^^ )
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

  (* -- yielding groups ------------------------------------------------------

     The four laws above run over a corpus with yielding groups and frames in
     it, so the reference already holds the engine to the rule, and
     [line_within_ruler] to the guarantee. A group that lies flat counted a
     yielding group ahead of it as far as its first break. If the yielding group
     then breaks, the line holds what was counted. If it lies flat, it measured
     the line to its end itself, from a column the first group had counted.
     ---------------------------------------------------------------------- *)

  let same_layout (sa, ra) (sb, rb) =
    String.equal (H.to_string sa) (H.to_string sb)
    && H.lines sa = H.lines sb
    && ra.H.declined = rb.H.declined
  ;;

  let annotations_keep_the_layout =
    (* Annotation erasure in [test_laws.ml] renders under [Content], which
       leaves [yields] unread, so a rebuild that dropped the flag would pass
       it. *)
    test
      "under Line, unannotate and reannotate leave the layout as it was"
      Surface.wild
      (at_all_widths (fun width s ->
         let d = D.to_doc s in
         let want = H.render ~fit:Line ~width d in
         same_layout want (H.render ~fit:Line ~width (H.unannotate d))
         && same_layout want (H.render ~fit:Line ~width (H.reannotate succ d))))
  ;;

  (* MUTATION yields survives a rebuild:
       in [reannotate] and in [unannotate], replace
           | Yielding r -> down r.d (R_group (true, k))
       with
           | Yielding r -> down r.d (R_group (false, k))
     -- the rebuilt group is a plain one, and the groups before it count it at
     its flat width.

   MUTATION yields survives a rebuild, frames:
       in [reannotate] and in [unannotate], replace
           | Frame r -> down r.d (R_frame (r.tag, yields_bit r.flags, k))
       with
           | Frame r -> down r.d (R_frame (r.tag, false, k)) *)

  let content_leaves_yields_unread =
    test
      "under Content, a document renders the same with ~yields:true and without"
      Surface.wild
      (at_all_widths (fun width s ->
         same_layout
           (H.render ~width (D.to_doc s))
           (H.render ~width (D.to_doc (Surface.unyielding s)))))
  ;;

  (* MUTATION Content leaves yields unread:
       in [reach], replace
           | Content -> W.add st.column req
       with
           | Content -> W.add (W.add st.column req) st.tail
     -- the default rule measures the continuation, which is where a yielding
     group counts for less than a plain one. *)

  (* [text p ^^ group (a ^^ probe) ^^ b ^^ g], indented by [j], where [g] is a
     group of [c] and the probe prints F where the first group lay flat. No word
     in the corpus holds an F. *)
  let before_a_group ~yields (a, b, c, p, j) =
    H.nest
      j
      (H.text p
       ^^ H.group (D.to_doc a ^^ H.flat_alt (H.text "F") (H.text "B"))
       ^^ D.to_doc b
       ^^ H.group ~yields (D.to_doc c))
  ;;

  let probe_gen =
    Gen.map3
      (fun (a, b, c) p j -> a, b, c, p, j)
      (Gen.triple
         (Surface.gen Surface.wild)
         (Surface.gen Surface.wild)
         (* Half the time a block that is wider up to its first break than it
            is flat, which is the case the law turns on. The other half is
            whatever the corpus gives. *)
         (Gen.oneof
            [ Surface.gen Surface.wild
            ; Gen.map2
                (fun w d ->
                   Surface.Concat
                     [ Text "{"; Flat_alt (Empty, Text w); Line; d; Line; Text "}" ])
                (Gen.oneof_list Surface.words)
                (Surface.gen Surface.wild)
            ]))
      (Gen.oneof_list (Surface.words @ Surface.unicode_words))
      (Gen.int_range 0 6)
  ;;

  let show_probe_case (a, b, c, p, j) =
    Printf.sprintf
      "a=%s b=%s c=%s p=%S j=%d"
      (Surface.show a)
      (Surface.show b)
      (Surface.show c)
      p
      j
  ;;

  let lies_flat ~width d =
    String.contains (H.to_string (fst (H.render ~fit:Line ~width d))) 'F'
  ;;

  let yielding_counts_for_no_more =
    (* A yielding group counts for at most what the same group counts made
       without [yields], so the group before it fits wherever it did. *)
    QCheck_alcotest.to_alcotest
      ~speed_level:`Quick
      (Test.make
         ~count:2000
         ~name:"under Line, a group that fits before a group fits before it yielding"
         ~print:show_probe_case
         probe_gen
         (fun case ->
            List.for_all
              ~f:(fun width ->
                (not (lies_flat ~width (before_a_group ~yields:false case)))
                || lies_flat ~width (before_a_group ~yields:true case))
              Surface.widths))
  ;;

  (* MUTATION a yielding group counts for no more:
       in [yielded], replace
           if W.compare l req <= 0 then l, breaks d else req, false
       with
           l, breaks d
     -- a yielding group is counted as far as its first break even where that
     is wider than the whole group flat, and a document that fits on one line
     breaks before it. *)

  let yielding_decides_as_a_group =
    (* [yields] is read by the groups before a group on its line. With none
       there, the document renders as it does without the flag: the yielding
       group's own test is the plain one. *)
    QCheck_alcotest.to_alcotest
      ~speed_level:`Quick
      (Test.make
         ~count:2000
         ~name:
           "under Line, a yielding group with no group before it decides as a plain one"
         ~print:show_probe_case
         probe_gen
         (fun (a, b, _, p, j) ->
            let d yields =
              H.nest j (H.text p ^^ H.group ~yields (D.to_doc a)) ^^ D.to_doc b
            in
            List.for_all
              ~f:(fun width ->
                same_layout
                  (H.render ~fit:Line ~width (d true))
                  (H.render ~fit:Line ~width (d false)))
              Surface.widths))
  ;;

  (* MUTATION a yielding group decides as a group:
       in [step], the [Yielding] case, replace
           if st.flat || W.compare (reach st y.req) st.ruler > 0
       with
           if st.flat || W.compare (reach st y.req) st.ruler >= 0
     -- a yielding group exactly as wide as the room left is laid out broken,
     where a plain group lies flat. The [Yielding] case repeats the test the
     [Group] case makes, and this law is what holds the two together.

     Or in [yielding_node], replace
           Yielding { flags = flags ~flattenable:true ~breaks; req; lead; d }
       with
           Yielding { flags = flags ~flattenable:true ~breaks; req = lead; lead; d }
     -- a yielding group decides by the width it is counted at, and lies flat
     wherever its opening line fits. *)

  let suite =
    ( "fit-" ^ P.name
    , [ stream_fidelity
      ; declined_complete
      ; deep_frames
      ; line_within_ruler
      ; annotations_keep_the_layout
      ; content_leaves_yields_unread
      ; yielding_counts_for_no_more
      ; yielding_decides_as_a_group
      ] )
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

(* -- yielding groups, by example ---------------------------------------------- *)

(* A header whose parameter list is a group, then a block on the same line. The
   whole is 34 wide and the header as far as the bracket is 22. *)
let def_ ?(params = [ "sig,"; "ps" ]) body =
  let params =
    H.group
      (H.text "("
       ^^ H.nest
            2
            (H.softline
             ^^ H.concat
                  (List.mapi params ~f:(fun i p ->
                     if i = 0 then H.text p else H.line ^^ H.text p)))
       ^^ H.softline
       ^^ H.text ")")
  in
  H.text "def f" ^^ params ^^ H.text ": Bool " ^^ body
;;

let block_body =
  H.text "{"
  ^^ H.nest 2 (H.line ^^ H.text "aaaa" ^^ H.line ^^ H.text "bbbb")
  ^^ H.line
  ^^ H.text "}"
;;

let yielding_block =
  (* The example in the interface. Without [yields] the parameter list counts
     the block flat, 34 columns in all, and opens. With it the list counts as
     far as the bracket, 22 columns, and the block takes the break. *)
  case "a yielding block lets the group before it stay flat" (fun () ->
    let plain = def_ (H.group block_body)
    and yielding = def_ (H.group ~yields:true block_body) in
    let one_line = "def f(sig, ps): Bool { aaaa bbbb }" in
    Alcotest.(check string) "plain, 34" one_line (show ~fit:Line 34 plain);
    Alcotest.(check string) "yielding, 34" one_line (show ~fit:Line 34 yielding);
    Alcotest.(check string)
      "plain, 26"
      "def f(\n  sig,\n  ps\n): Bool { aaaa bbbb }"
      (show ~fit:Line 26 plain);
    let opened = "def f(sig, ps): Bool {\n  aaaa\n  bbbb\n}" in
    Alcotest.(check string) "yielding, 33" opened (show ~fit:Line 33 yielding);
    Alcotest.(check string) "yielding, 22" opened (show ~fit:Line 22 yielding);
    (* One column short of the header, the list opens as well. The block is
       then on a line of its own making, and fits. *)
    Alcotest.(check string)
      "yielding, 21"
      "def f(\n  sig,\n  ps\n): Bool { aaaa bbbb }"
      (show ~fit:Line 21 yielding);
    (* [Content] leaves the flag unread. *)
    List.iter [ 34; 26; 22; 21; 12 ] ~f:(fun w ->
      Alcotest.(check string)
        (Printf.sprintf "Content, %d" w)
        (show w plain)
        (show w yielding)))
;;

let yielding_frame =
  (* The same block as a frame, whose last element prints a separator where the
     block breaks. The list counts the frame as far as its bracket. *)
  case "a yielding frame is counted as far as its first break" (fun () ->
    let body yields =
      H.framed ~yields (fun alt ->
        H.text "{"
        ^^ H.nest
             2
             (H.line
              ^^ H.text "aaaa;"
              ^^ H.line
              ^^ H.group (H.text "bbbb" ^^ alt H.empty (H.text ";")))
        ^^ H.line
        ^^ H.text "}")
    in
    Alcotest.(check string)
      "plain, 26"
      "def f(\n  sig,\n  ps\n): Bool { aaaa; bbbb }"
      (show ~fit:Line 26 (def_ (body false)));
    Alcotest.(check string)
      "yielding, 26"
      "def f(sig, ps): Bool {\n  aaaa;\n  bbbb;\n}"
      (show ~fit:Line 26 (def_ (body true)));
    Alcotest.(check string)
      "yielding, 35"
      "def f(sig, ps): Bool { aaaa; bbbb }"
      (show ~fit:Line 35 (def_ (body true))))
;;

let yielding_by_position =
  (* What is counted is the group laid out broken as far as its first break,
     and a group met on the way counts at its flat width. So the flag does
     nothing on a group whose body is a plain group, or inside a later plain
     group. *)
  case "a plain group around or inside a yielding one is counted flat" (fun () ->
    let opens_the_list = "def f(\n  sig,\n  ps\n): Bool { aaaa bbbb }" in
    Alcotest.(check string)
      "yielding over a plain group"
      opens_the_list
      (show ~fit:Line 26 (def_ (H.group ~yields:true (H.group block_body))));
    Alcotest.(check string)
      "yielding inside a later plain group"
      opens_the_list
      (show ~fit:Line 26 (def_ (H.group (H.group ~yields:true block_body)))))
;;

let yielding_through_a_wrapper =
  (* The nesting lingo builds for a production whose last child is a block: a
     yielding group over a plain group, some text, and the block, itself
     yielding, under [from_line]. Annotations, [nest] and [from_line] pass a
     lead through, and [from_line] takes the block's indentation from the line
     it opens on, whatever the [nest] around it says. The wrapper counts as the
     plain group flat, the text, and the block as far as its bracket:
     "(x, y) => {" is 11 columns after "def f(sig, ps): Bool ", 21, which makes
     32. *)
  case "a yielding group counts the yielding group its body ends in" (fun () ->
    let pattern =
      H.group (H.text "(" ^^ H.text "x," ^^ H.line ^^ H.text "y" ^^ H.text ")")
    in
    let arm ~outer ~inner =
      H.group
        ~yields:outer
        (H.annotate
           0
           (pattern
            ^^ H.text " => "
            ^^ H.nest 2 (H.from_line (H.annotate 1 (H.group ~yields:inner block_body)))))
    in
    let hugged = "def f(sig, ps): Bool (x, y) => {\n  aaaa\n  bbbb\n}" in
    let d = def_ (arm ~outer:true ~inner:true) in
    Alcotest.(check string) "32" hugged (show ~fit:Line 32 d);
    Alcotest.(check string)
      "31"
      "def f(\n  sig,\n  ps\n): Bool (x, y) => { aaaa bbbb }"
      (show ~fit:Line 31 d);
    (* With either of the two plain, the list counts the whole arm flat. *)
    let opens_the_list = "def f(\n  sig,\n  ps\n): Bool (x, y) => { aaaa bbbb }" in
    Alcotest.(check string)
      "plain wrapper, 32"
      opens_the_list
      (show ~fit:Line 32 (def_ (arm ~outer:false ~inner:true)));
    Alcotest.(check string)
      "plain block, 32"
      opens_the_list
      (show ~fit:Line 32 (def_ (arm ~outer:true ~inner:false))))
;;

let yielding_counts_at_most_flat =
  (* Broken, this block is 14 columns up to its first break, and flat it is 5.
     Counted as far as its first break it would cost the group before it its
     line, on a document that is 10 columns wide. It counts at 5. *)
  case "a yielding group wider to its first break than flat counts flat" (fun () ->
    let block yields =
      H.group
        ~yields
        (H.text "{"
         ^^ H.flat_alt H.empty (H.text " /* opened */")
         ^^ H.line
         ^^ H.text "x }")
    in
    let d yields =
      H.text "h" ^^ H.group (H.line ^^ H.text "aa") ^^ H.text " " ^^ block yields
    in
    Alcotest.(check string) "plain, 12" "h aa { x }" (show ~fit:Line 12 (d false));
    Alcotest.(check string) "yielding, 12" "h aa { x }" (show ~fit:Line 12 (d true)))
;;

let last_yielding_breaks_first =
  (* Each block counts the one after it as far as its bracket, so the first
     fits while "if (c) { aaaa } else {" does, 22 columns, and the last takes
     the break. Without [yields] the first counts the second flat, and
     breaks. *)
  case "of several yielding groups on a line, the last breaks first" (fun () ->
    let block yields s =
      H.group ~yields (H.text "{" ^^ H.nest 2 (H.line ^^ H.text s) ^^ H.line ^^ H.text "}")
    in
    let d yields =
      H.text "if (c) " ^^ block yields "aaaa" ^^ H.text " else " ^^ block yields "bbbb"
    in
    Alcotest.(check string)
      "plain, 22"
      "if (c) {\n  aaaa\n} else { bbbb }"
      (show ~fit:Line 22 (d false));
    Alcotest.(check string)
      "yielding, 22"
      "if (c) { aaaa } else {\n  bbbb\n}"
      (show ~fit:Line 22 (d true));
    Alcotest.(check string)
      "yielding, 21"
      "if (c) {\n  aaaa\n} else { bbbb }"
      (show ~fit:Line 21 (d true)))
;;

let yielding_from_a_break =
  (* [yields] is for a group that puts text before its first break. This one
     starts with a break, so the group before it counts nothing for it, and
     the counterexample to monotonicity that [Line] removed is back: at width 3
     the first group lies flat and the second breaks at once. *)
  case "a yielding group that starts with a break gives nothing to count" (fun () ->
    let d = H.text "bb" ^^ H.group H.line ^^ H.group ~yields:true (H.line ^^ H.line) in
    Alcotest.(check (pair int string)) "width 2" (2, "bb\n  ") (counted 2 d);
    Alcotest.(check (pair int string)) "width 3" (3, "bb \n\n") (counted 3 d);
    Alcotest.(check (pair int string)) "width 5" (1, "bb   ") (counted 5 d))
;;

(* -- the differential, where the rules agree ---------------------------------

   Where every group is followed directly by a [line], the tail at each
   decision is zero: the [line] is pending, and pending work is laid out
   broken. [Line] then decides as [Content] does, and so as PPrint does. This
   covers the tail's bookkeeping and [breaks]; a tail of any width is left to
   the reference. *)

let rec seal : Surface.t -> Surface.t = function
  | Group d -> Cat (Group (seal d), Line)
  | Yielding d -> Cat (Yielding (seal d), Line)
  | Cat (a, b) -> Cat (seal a, seal b)
  | Concat ds -> Concat (List.map ~f:seal ds)
  | Flat_alt (a, b) -> Flat_alt (seal a, seal b)
  | Nest (j, d) -> Nest (j, seal d)
  | Align d -> Align (seal d)
  | From_line d -> From_line (seal d)
  | Annot (a, d) -> Annot (a, seal d)
  | Framed d -> Framed (seal d)
  | Yielding_framed d -> Yielding_framed (seal d)
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
      ; yielding_block
      ; yielding_frame
      ; yielding_by_position
      ; yielding_through_a_wrapper
      ; yielding_counts_at_most_flat
      ; last_yielding_breaks_first
      ; yielding_from_a_break
      ] )
  ]
;;
