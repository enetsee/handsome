(* [render] and the folds over its stream. The bytes match what an independent
   implementation produces, [lines] agrees with [to_string], annotation markers
   bracket the region they were given, and every line of the engine's own output
   ends in something other than whitespace it emitted. *)

open StdLabels
module H = Handsome.Ascii
open QCheck2

let ( ^^ ) = H.( ^^ )

let test ?(count = 500) name flavour prop =
  QCheck_alcotest.to_alcotest
    ~speed_level:`Quick
    (Test.make ~count ~name ~print:Surface.show (Surface.gen flavour) prop)
;;

(* Every property below is checked at every width in [Surface.widths] for each
   generated document, so that a case which misbehaves only at a narrow ruler is
   reached on every run. *)
let at_all_widths f s = List.for_all ~f:(fun w -> f w s) Surface.widths

(* -- properties ------------------------------------------------------------ *)

let fidelity =
  test
    "to_string reproduces the rendered bytes"
    Surface.rich
    (at_all_widths (fun width s ->
       let d = Surface.to_doc s in
       let stream, _ = H.render ~width d in
       String.equal
         (H.to_string stream)
         (Reference.render ~measure:String.length ~width s).Reference.bytes))
;;

let totality =
  test
    "render returns for every document, checked or not"
    Surface.dirty
    (at_all_widths (fun width s ->
       let d = Surface.to_doc s in
       match H.render ~width d with
       | _stream, _res -> true
       | exception _ -> false))
;;

let lines_agree =
  test
    "lines is the width of each line of to_string"
    Surface.rich
    (at_all_widths (fun width s ->
       let stream, _ = H.render ~width (Surface.to_doc s) in
       let bytes = H.to_string stream in
       let expected =
         Array.of_list (List.map ~f:String.length (String.split_on_char ~sep:'\n' bytes))
       in
       H.lines stream = expected))
;;

let declined_indexes_a_line =
  test
    "every declined resolution names a line that exists"
    Surface.rich
    (at_all_widths (fun width s ->
       let stream, r = H.render ~width (Surface.to_doc s) in
       let n = Array.length (H.lines stream) in
       List.for_all ~f:(fun (l, _) -> 0 <= l && l < n) r.H.declined))
;;

let annotations_balanced =
  test
    "annotation pushes and pops are balanced and nested"
    Surface.rich
    (at_all_widths (fun width s ->
       let stream, _ = H.render ~width (Surface.to_doc s) in
       let rec go depth = function
         | H.S_empty -> depth = 0
         | H.S_text (_, _, k) | H.S_line (_, k) -> go depth k
         | H.S_ann_push (_, k) -> go (depth + 1) k
         | H.S_ann_pop k -> depth > 0 && go (depth - 1) k
       in
       go 0 stream))
;;

let no_trailing_whitespace =
  (* The engine emits indentation once something lands on the line, so no line
     ends in indentation it emitted. A space at the end of a line comes from a
     text node: a flat group ending in [line] or [blank], with the break arriving
     from outside the group, leaves one there. *)
  test
    "the engine never emits trailing indentation"
    Surface.rich
    (at_all_widths (fun width s ->
       let stream, _ = H.render ~width (Surface.to_doc s) in
       let rec go = function
         | H.S_empty -> true
         | H.S_line (n, k) ->
           let rec next = function
             | H.S_empty -> n = 0
             | H.S_line (_, _) -> n = 0
             | H.S_text (_, t, _) -> String.length t > 0
             | H.S_ann_push (_, k) | H.S_ann_pop k -> next k
           in
           next k && go k
         | H.S_text (_, _, k) | H.S_ann_push (_, k) | H.S_ann_pop k -> go k
       in
       go stream))
;;

(* -- worked examples ------------------------------------------------------- *)

let unit_cases =
  let d = H.group (H.text "let" ^^ H.line ^^ H.text "x" ^^ H.line ^^ H.text "=") in
  [ ( "a group that fits is flat"
    , `Quick
    , fun () ->
        Alcotest.(check string) "" "let x =" (H.to_string (fst (H.render ~width:80 d))) )
  ; ( "a group that does not fit is broken"
    , `Quick
    , fun () ->
        Alcotest.(check string) "" "let\nx\n=" (H.to_string (fst (H.render ~width:3 d)))
    )
  ; ( "declined records the breaks resolved flat"
    , `Quick
    , fun () ->
        let _, r = H.render ~width:80 d in
        Alcotest.(check (list (pair int int))) "" [ 0, 3; 0, 5 ] r.H.declined )
  ; ( "declined is empty when everything broke"
    , `Quick
    , fun () ->
        let _, r = H.render ~width:3 d in
        Alcotest.(check (list (pair int int))) "" [] r.H.declined )
  ; ( "nest indents after a break"
    , `Quick
    , fun () ->
        Alcotest.(check string)
          ""
          "a\n  b"
          (H.to_string
             (fst (H.render ~width:1 (H.nest 2 (H.text "a" ^^ H.line ^^ H.text "b"))))) )
  ; ( "align indents to the current column"
    , `Quick
    , fun () ->
        Alcotest.(check string)
          ""
          "ab\n  c"
          (H.to_string
             (fst (H.render ~width:1 (H.text "ab" ^^ H.align (H.line ^^ H.text "c"))))) )
  ; ( "an empty line carries no indentation"
    , `Quick
    , fun () ->
        Alcotest.(check string)
          ""
          "a\n\n  b"
          (H.to_string
             (fst
                (H.render
                   ~width:1
                   (H.nest 2 (H.text "a" ^^ H.hardline ^^ H.hardline ^^ H.text "b"))))) )
  ; ( "the stream brackets the annotated region"
    , `Quick
    , fun () ->
        let stream, _ =
          H.render ~width:80 (H.text "a" ^^ H.annotate `Kw (H.text "let") ^^ H.text "b")
        in
        let rec shape = function
          | H.S_empty -> []
          | H.S_text (_, t, k) -> ("t:" ^ t) :: shape k
          | H.S_line (n, k) -> Printf.sprintf "l:%d" n :: shape k
          | H.S_ann_push (_, k) -> "push" :: shape k
          | H.S_ann_pop k -> "pop" :: shape k
        in
        Alcotest.(check (list string))
          ""
          [ "t:a"; "push"; "t:let"; "pop"; "t:b" ]
          (shape stream) )
  ]
;;

(* [to_string] adds indentation from a 65-entry cache a chunk at a time, where it
   used to build a string per line past 64 spaces. The chunking is meant to be
   invisible in the bytes, so it needs pinning here: nothing
   else in the suite compares output either side of the boundary, and [laws-*/16]
   stops deliberately short of [to_string], since the indentations it renders
   would each exceed the length of a string.

   Appended after the properties, so the render/N ids the mutation report names
   stay put. *)
let indentation_across_the_cache_boundary =
  ( "indentation is the same either side of the cache boundary"
  , `Quick
  , fun () ->
      List.iter
        ~f:(fun n ->
          let d = H.nest n (H.text "a" ^^ H.hardline ^^ H.text "b") in
          Alcotest.(check string)
            (Printf.sprintf "one line indented %d" n)
            ("a\n" ^ String.make n ' ' ^ "b")
            (H.to_string (fst (H.render ~width:1 d))))
        [ 0; 1; 63; 64; 65; 66; 127; 128; 129; 200 ];
      (* Two indented lines, so a chunk loop that leaks per line shows up as a
         difference between them. A shift in both would be a different fault. *)
      List.iter
        ~f:(fun n ->
          let d =
            H.nest n (H.text "a" ^^ H.hardline ^^ H.text "b" ^^ H.hardline ^^ H.text "c")
          in
          let pad = String.make n ' ' in
          Alcotest.(check string)
            (Printf.sprintf "two lines indented %d" n)
            ("a\n" ^ pad ^ "b\n" ^ pad ^ "c")
            (H.to_string (fst (H.render ~width:1 d))))
        [ 65; 200 ];
      (* The raise the interface documents. It used to come for free from
         [String.make]. Nothing allocates now, so it is an explicit comparison,
         and this case keeps it there. [render] and [lines] accept the
         same document, which is the other half of the documented contract. *)
      let d =
        H.nest (Sys.max_string_length + 1) (H.text "a" ^^ H.hardline ^^ H.text "b")
      in
      let stream, _ = H.render ~width:80 d in
      Alcotest.(check int) "render accepts it" 2 (Array.length (H.lines stream));
      Alcotest.check_raises
        "to_string rejects it"
        (Invalid_argument "Handsome.to_string: indentation")
        (fun () -> ignore (H.to_string stream)) )
;;

(* -- the four stream invariants, together and under Utf8 ----------------------

   render/10 to render/13 each check one of these, one at a time, under [Ascii],
   each on its own draw from the generator. This checks all four of the same
   stream, under [Utf8], so that a document satisfying one and breaking another
   is reached -- and at 200k documents against the 500 the individual properties
   draw, which is two million renders and costs about 0.8s.

   The measure is the reason to repeat them under a second instance:
   indentation is a count of spaces but a line's width is a [W.t], and under
   [Utf8] those stop being the same number as soon as a wide character reaches a
   column that [align] then reads.
   -------------------------------------------------------------------------- *)

module U = Handsome.Utf8
module Du = Surface.Utf8_doc

let stream_invariants =
  QCheck_alcotest.to_alcotest
    ~speed_level:`Quick
    (Test.make
       ~count:200_000
       ~name:"the four stream invariants hold of the same stream, under Utf8"
       ~print:Surface.show
       (Surface.gen Surface.rich)
       (fun s ->
          let d = Du.to_doc s in
          List.for_all
            ~f:(fun width ->
              let stream, r = U.render ~width d in
              (* 1. No line break carries indentation that nothing lands on. *)
              let rec no_dangling_indent = function
                | U.S_empty -> true
                | U.S_line (n, k) ->
                  let rec lands = function
                    | U.S_empty -> n = 0
                    | U.S_line (_, _) -> n = 0
                    | U.S_text (_, t, _) -> String.length t > 0
                    | U.S_ann_push (_, k) | U.S_ann_pop k -> lands k
                  in
                  lands k && no_dangling_indent k
                | U.S_text (_, _, k) | U.S_ann_push (_, k) | U.S_ann_pop k ->
                  no_dangling_indent k
              in
              (* 2. Annotation pushes and pops balance, and the depth never goes
                 negative on the way. *)
              let rec balanced depth = function
                | U.S_empty -> depth = 0
                | U.S_text (_, _, k) | U.S_line (_, k) -> balanced depth k
                | U.S_ann_push (_, k) -> balanced (depth + 1) k
                | U.S_ann_pop k -> depth > 0 && balanced (depth - 1) k
              in
              (* 3. [lines] has one entry per newline-separated piece of the
                 bytes. *)
              let pieces =
                List.length (String.split_on_char ~sep:'\n' (U.to_string stream))
              in
              (* 4. Every declined resolution names a line that exists and sits
                 at a column the ruler still had room at. *)
              let declined_in_range =
                List.for_all
                  ~f:(fun (l, col) ->
                    l >= 0
                    && l < Array.length (U.lines stream)
                    && Handsome.Utf8_width.compare col width <= 0)
                  r.U.declined
              in
              no_dangling_indent stream
              && balanced 0 stream
              && Array.length (U.lines stream) = pieces
              && declined_in_range)
            Surface.widths))
;;

(* MUTATIONS: eleven redden this. Running the four properties together earns
   that -- each one has at least one mutation of its own.
     trailing-indentation           a break carrying indentation nothing lands
                                    on                             (property 1)
     annotation-markers-unbalanced  pushes and pops that do not nest
                                                                   (property 2)
     text-truncated                 bytes dropped, so [lines] and [to_string]
                                    stop agreeing                  (property 3)
     declined-completeness          a declined entry naming the wrong line
                                                                   (property 4) *)

let suite =
  ( "render"
  , unit_cases
    @ [ fidelity
      ; totality
      ; lines_agree
      ; declined_indexes_a_line
      ; annotations_balanced
      ; no_trailing_whitespace
      ; indentation_across_the_cache_boundary
      ; stream_invariants
      ] )
;;
