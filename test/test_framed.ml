(* [framed] on small documents with literal answers.

   The laws over generated documents are in [test_laws.ml]. These pin the cases
   those laws generalise, one construction each, so that a failure names the
   case. Each document is one line to read and each answer one line of output.

   MUTATIONS, each written against [lib/handsome.ml] and named as
   [scratch/mutate.py] names it:
     falt-branches-swapped              reddens framed/0 and framed/1
     falt-reads-any-flat-frame          reddens framed/3
     new-tag-repeats                    reddens framed/3
     frame-drops-a-broken-body          reddens framed/5, through [check]
     frame-measures-its-own-at-broken   reddens framed/6
     rebuild-takes-a-fresh-tag          reddens framed/7
     frames-outlive-the-frame           reddens framed/9
     pp-loses-the-tag                   reddens framed/10
     widest-measures-free-at-broken     reddens framed/13
     check-misses-an-alt-outside        reddens framed/14
     check-leaves-frames-open           reddens framed/14 *)

open StdLabels
module H = Handsome.Ascii

let ( ^^ ) = H.( ^^ )
let str ~width d = H.to_string (fst (H.render ~width d))
let eq name want got = Alcotest.(check string) name want got
let f_or_b alt = alt (H.text "F") (H.text "B")

(* A conditional whose frame has already closed. *)
let stray () =
  let got = ref None in
  ignore
    (H.framed (fun alt ->
       got := Some alt;
       H.empty)
     : unit H.t);
  match !got with
  | Some alt -> alt
  | None -> assert false
;;

(* A one-element list with a trailing comma. It measures three columns, which
   is what it prints when flat. *)
let list_of_a () =
  H.framed (fun alt ->
    H.text "["
    ^^ H.nest 2 (H.softline ^^ H.text "a" ^^ alt H.empty (H.text ","))
    ^^ H.softline
    ^^ H.text "]")
;;

let unit_cases =
  [ ( "a frame laid out flat takes the flat branch"
    , `Quick
    , fun () -> eq "width 3" "[a]" (str ~width:3 (list_of_a ())) )
  ; ( "a frame laid out broken takes the broken branch"
    , `Quick
    , fun () -> eq "width 2" "[\n  a,\n]" (str ~width:2 (list_of_a ())) )
  ; ( "the frame decides, where flat_alt reads the nearest group"
    , `Quick
    , fun () ->
        (* The frame is seven columns and breaks at width 3. The inner group is
           two and fits. The conditional sits inside the inner group, so the two
           primitives disagree, and the pair is the reason the primitive
           exists. *)
        let framed =
          H.framed (fun alt ->
            H.text "aaaa" ^^ H.line ^^ H.group (H.text "b" ^^ f_or_b alt))
        in
        let plain =
          H.group
            (H.text "aaaa"
             ^^ H.line
             ^^ H.group (H.text "b" ^^ H.flat_alt (H.text "F") (H.text "B")))
        in
        eq "framed follows the frame" "aaaa\nbB" (str ~width:3 framed);
        eq "flat_alt follows the inner group" "aaaa\nbF" (str ~width:3 plain) )
  ; ( "a conditional on an outer frame, inside an inner frame, follows the outer"
    , `Quick
    , fun () ->
        (* The outer frame breaks and the inner one fits, which is the one way
           round they can differ. The first conditional is the outer frame's and
           the second the inner's. The shape is [[1, 2], [3, 4]]'s, one list
           inside another. *)
        let d =
          H.framed (fun outer ->
            H.text "aaaa"
            ^^ H.line
            ^^ H.framed (fun inner -> H.text "b" ^^ f_or_b outer ^^ f_or_b inner))
        in
        eq "width 3" "aaaa\nbBF" (str ~width:3 d) )
  ; ( "a conditional outside its frame takes its broken branch"
    , `Quick
    , fun () ->
        (* Whether the group around it is flat or broken. *)
        let alt = stray () in
        let outside = H.group (H.text "ab" ^^ H.line ^^ f_or_b alt) in
        let with_b = H.group (H.text "ab" ^^ H.line ^^ H.text "B") in
        for width = 0 to 8 do
          eq (Printf.sprintf "width %d" width) (str ~width with_b) (str ~width outside)
        done;
        eq "at the top level" "B" (str ~width:80 (f_or_b alt)) )
  ; ( "a frame holding a hardline keeps its node"
    , `Quick
    , fun () ->
        (* The hardline breaks the frame, so the conditional takes [B]. Without
           its node the conditional would take [B] as well, having no frame, and
           [check] would report it outside. *)
        let d = H.framed (fun alt -> H.text "a" ^^ H.hardline ^^ H.group (f_or_b alt)) in
        eq "width 80" "a\nB" (str ~width:80 d);
        Alcotest.(check bool) "check finds it inside" true (H.check d = Ok ()) )
  ; ( "a frame measures its trailing separator as it prints it"
    , `Quick
    , fun () ->
        (* Flat, the frame prints no comma and measures none, so it fits where
           [flat_alt] does. *)
        let framed = H.framed (fun alt -> H.text "xxxx" ^^ alt H.empty (H.text ",")) in
        let plain = H.group (H.text "xxxx" ^^ H.flat_alt H.empty (H.text ",")) in
        eq "framed, width 4" "xxxx" (str ~width:4 framed);
        eq "flat_alt, width 4" "xxxx" (str ~width:4 plain);
        eq "framed, width 3" "xxxx," (str ~width:3 framed) )
  ; ( "unannotate keeps each conditional on its frame"
    , `Quick
    , fun () ->
        (* The rebuild goes through the smart constructors. A frame rebuilt under
           a new tag would leave its conditional outside it. *)
        let d =
          H.framed (fun alt ->
            H.annotate 1 (H.text "aaaa") ^^ H.line ^^ H.group (H.text "b" ^^ f_or_b alt))
        in
        let rebuilt = H.unannotate (H.reannotate succ d) in
        eq "width 3" "aaaa\nbB" (str ~width:3 rebuilt);
        Alcotest.(check bool) "check finds it inside" true (H.check rebuilt = Ok ()) )
  ; ( "a flat branch wider than the broken one is measured at its own width"
    , `Quick
    , fun () ->
        (* Flat, this is six columns. Measuring the broken branch alone would
           call it three and lay it out flat at width 4, three columns past the
           ruler. *)
        let d =
          H.framed (fun alt ->
            H.text "ab" ^^ H.softline ^^ alt (H.text "cdef") (H.text "c"))
        in
        eq "width 4" "ab\nc" (str ~width:4 d);
        eq "width 6" "abcdef" (str ~width:6 d) )
  ; ( "a frame's binding ends with the frame"
    , `Quick
    , fun () ->
        (* The frame is laid out flat. A conditional that escaped it and is used
           after it takes its broken branch. *)
        let escaped = ref None in
        let frame =
          H.framed (fun alt ->
            escaped := Some alt;
            H.text "ab")
        in
        let alt = Option.get !escaped in
        eq "width 80" "abB" (str ~width:80 (frame ^^ f_or_b alt)) )
  ]
;;

(* -- goldens ------------------------------------------------------------------

   A printed structure and a rendering over a spread of widths, each compared as
   one string so that a change reads as a diff of the whole.
   -------------------------------------------------------------------------- *)

let pp_golden =
  ( "pp prints frames local to the printout"
  , `Quick
  , fun () ->
      (* Frames made beforehand, so the numbers printed are shown to be counted
         from this document. The last conditional is outside its frame. *)
      let _ : unit H.t -> unit H.t -> unit H.t = stray () in
      let _ : unit H.t -> unit H.t -> unit H.t = stray () in
      let outside = stray () in
      let d =
        H.framed (fun outer ->
          H.text "["
          ^^ H.framed (fun _ -> H.text "a" ^^ outer H.empty (H.text ","))
          ^^ outside (H.text "x") H.empty)
      in
      eq
        ""
        {|(frame 0
 (cat (text "[")
  (cat
   (frame 1
    (cat (text "a") (frame-alt 0 empty (text ","))))
   (frame-alt 2 (text "x") empty))))|}
        (Reader.print_doc d) )
;;

(* The shape the primitive is for: a list whose last element sits in a group of
   its own with the list's trailing comma. *)
let list elems =
  H.framed (fun alt ->
    let rec items = function
      | [] -> H.empty
      | [ x ] -> H.group (x ^^ alt H.empty (H.text ","))
      | x :: rest -> H.group (x ^^ H.text ",") ^^ H.line ^^ items rest
    in
    H.text "[" ^^ H.nest 2 (H.softline ^^ items elems) ^^ H.softline ^^ H.text "]")
;;

let ints ns = list (List.map ~f:(fun n -> H.text (string_of_int n)) ns)

let rendering_golden =
  ( "a nested list with trailing commas, across widths"
  , `Quick
  , fun () ->
      (* Flat, the outer list prints and measures 20 columns. On a line of its
         own, an inner list and the comma after it take 7 columns from column
         2, so they fit at 9, and the inner list breaks at 7. *)
      let d = list [ ints [ 1; 2 ]; ints [ 3; 4 ]; list [] ] in
      let at width = Printf.sprintf "-- width %d\n%s\n" width (str ~width d) in
      eq
        ""
        {|-- width 20
[[1, 2], [3, 4], []]
-- width 19
[
  [1, 2],
  [3, 4],
  [],
]
-- width 9
[
  [1, 2],
  [3, 4],
  [],
]
-- width 7
[
  [
    1,
    2,
  ],
  [
    3,
    4,
  ],
  [],
]
|}
        (String.concat ~sep:"" (List.map ~f:at [ 20; 19; 9; 7 ])) )
;;

(* -- appended, so that no id above moves ------------------------------------- *)

let later_cases =
  [ ( "a hardline in the broken branch breaks the groups inside the frame alone"
    , `Quick
    , fun () ->
        (* A newline where the frame broke and nothing where it was flat. The
           group around the conditional is broken whenever it has to decide, so
           it can hold the hardline; the frame, which prints [empty] when flat,
           can still lay out flat. *)
        let d =
          H.framed (fun alt ->
            H.text "[" ^^ H.group (H.text "a" ^^ alt H.empty H.hardline) ^^ H.text "]")
        in
        eq "width 3" "[a]" (str ~width:3 d);
        eq "width 2" "[a\n]" (str ~width:2 d) )
  ; ( "a conditional nested in another's branch counts at its wider branch"
    , `Quick
    , fun () ->
        (* Inside a branch of the inner frame's conditional, the outer frame's is
           free, so it counts at the wider of its two branches either way round,
           and both frames measure five. With [wide] as the broken branch they
           print two, which is the conservative case; with [wide] as the flat
           branch they print five, which a count of the broken branch would
           miss. *)
        let outer_first flat broken =
          H.framed (fun outer ->
            H.framed (fun inner ->
              H.text "a" ^^ inner (outer (H.text flat) (H.text broken)) H.empty))
        in
        eq "n flat, width 4" "a" (str ~width:4 (outer_first "n" "wide"));
        eq "n flat, width 5" "an" (str ~width:5 (outer_first "n" "wide"));
        eq "wide flat, width 4" "a" (str ~width:4 (outer_first "wide" "n"));
        eq "wide flat, width 5" "awide" (str ~width:5 (outer_first "wide" "n"));
        (* The other nesting: the nested conditional's frame lies inside the
           frame of the conditional holding it, and is outside the branch all the
           same. *)
        let d =
          H.framed (fun outer ->
            H.framed (fun inner ->
              H.text "a" ^^ outer (inner (H.text "n") (H.text "wide")) H.empty))
        in
        eq "inner's frame inside, width 4" "a" (str ~width:4 d);
        eq "inner's frame inside, width 5" "an" (str ~width:5 d) )
  ; ( "check reports a conditional outside its frame by the number pp prints"
    , `Quick
    , fun () ->
        (* The first conditional escaped the frame before it, numbered 0, and is
           used after it; the second belongs to a frame the document never
           holds, numbered 1. *)
        let escaped = ref None in
        let frame =
          H.framed (fun alt ->
            escaped := Some alt;
            H.text "ab")
        in
        let d = frame ^^ f_or_b (Option.get !escaped) ^^ f_or_b (stray ()) in
        Alcotest.(check bool)
          "two outside"
          true
          (H.check d = Error [ H.Alt_outside_frame 0; H.Alt_outside_frame 1 ]);
        eq
          "pp"
          {|(cat (frame 0 (text "ab"))
 (cat (frame-alt 0 (text "F") (text "B"))
  (frame-alt 1 (text "F") (text "B"))))|}
          (Reader.print_doc d) )
  ]
;;

let suite = "framed", unit_cases @ [ pp_golden; rendering_golden ] @ later_cases
