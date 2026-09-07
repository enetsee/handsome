(* The laws of the layout algorithm, one test each.

   Every test below names a mutation of lib/handsome.ml that reddens it, written
   as the exact text to replace so it can be reproduced by hand. A law that stays
   green under every mutation is untested, and where that happens it is recorded
   here as a coverage statement.

   The laws are a functor over the width instance, and each runs under both
   {!Handsome.Ascii} and {!Handsome.Utf8}. That matters most for width
   soundness, which is the law an under-reporting measure breaks: under Ascii
   alone it runs where byte length guarantees the answer. The differential at
   the bottom of this file is Ascii-only, for the reason given there. *)

open StdLabels
open QCheck2

module type INSTANCE = sig
  val name : string

  (* The instance's [measure], which the reference implementation needs in order
     to say what the right answer is under this width. *)
  val measure : string -> int

  module H : Handsome.S with type width = int
end

module Make (P : INSTANCE) = struct
  module H = P.H
  module D = Surface.Doc (P.H)

  let to_doc = D.to_doc
  let ref_render = Reference.render ~measure:P.measure
  let ref_flat_width = Reference.flat_width ~measure:P.measure
  let ( ^^ ) = H.( ^^ )
  let at_all_widths p s = List.for_all ~f:(fun w -> p w s) Surface.widths

  let test ?(count = 1000) name flavour prop =
    QCheck_alcotest.to_alcotest
      ~speed_level:`Quick
      (Test.make ~count ~name ~print:Surface.show (Surface.gen flavour) prop)
  ;;

  let render ~width s = H.render ~width (to_doc s)
  let bytes ~width s = H.to_string (fst (render ~width s))

  (* -- determinism --------------------------------------------------------- *)

  let determinism =
    (* Rendering the same document twice, and rendering a freshly built but
       structurally equal document, must give the same stream and the same
       resolutions. The second half catches state cached on a node. *)
    test
      "determinism: render depends on the document and the width alone"
      Surface.wild
      (at_all_widths (fun width s ->
         let d = to_doc s in
         let a = H.render ~width d in
         (* interleave an unrelated render, so leftover state would show *)
         let _ = H.render ~width:7 (H.text "unrelated" ^^ H.line) in
         let b = H.render ~width d in
         let c = H.render ~width (to_doc s) in
         a = b && a = c))
  ;;

  (* MUTATION determinism:
       in [render], carry the line counter across calls.
       Add before [let render ~width d =]:
           let leaked = ref 0
       and in the state initialiser replace
           line = 0;
       with
           line = !leaked;
       and after [drive st;] insert
           leaked := st.line;
     This is the "reuse the record and save the allocation" optimisation, done
     wrong. It moves the line numbers in [declined] and leaves the bytes alone,
     which is why the law compares resolutions as well as bytes. *)

  (* -- annotation transparency --------------------------------------------- *)

  let annotation_transparency =
    test
      "annotation transparency: unannotate (reannotate f d) = unannotate d"
      Surface.rich
      (fun s ->
         let d = to_doc s in
         let f x = x * 2 in
         H.unannotate (H.reannotate f d) = H.unannotate d)
  ;;

  (* MUTATION annotation transparency:
       in [reannotate], replace
           | Cat r -> reannotate fn r.l ^^ reannotate fn r.r
       with
           | Cat r -> reannotate fn r.r ^^ reannotate fn r.l

     A weaker mutation leaves it green: dropping the annotation node outright
     ([| Annot (_, _, x) -> reannotate f x]) keeps the law true, since
     [unannotate] removes the node on both sides. This law constrains
     [reannotate]'s treatment of the structure around annotations, and annotation
     erasure below constrains the annotations themselves. *)

  (* -- annotation erasure -------------------------------------------------- *)

  let annotation_erasure =
    test
      "annotation erasure: annotations do not change the rendering"
      Surface.rich
      (at_all_widths (fun width s ->
         let d = to_doc s in
         let sa, ra = H.render ~width d in
         let sb, rb = H.render ~width (H.unannotate d) in
         String.equal (H.to_string sa) (H.to_string sb)
         && H.lines sa = H.lines sb
         && ra.H.declined = rb.H.declined))
  ;;

  (* MUTATION annotation erasure:
       in [annotate], replace
           req = flat_width d;
       with
           req = W.add (flat_width d) (W.measure " ");
     -- an annotated region measures one column wider than it renders, and
     annotations are meant to be transparent to measurement. *)

  (* -- group idempotence --------------------------------------------------- *)

  let group_idempotence =
    test
      "group idempotence: group (group d) = group d"
      Surface.wild
      (at_all_widths (fun width s ->
         let d = to_doc s in
         H.render ~width (H.group (H.group d)) = H.render ~width (H.group d)))
  ;;

  (* MUTATION group idempotence:
       in [step], the [Group] case, replace
           if st.flat then
             (* Already committed to flat by an enclosing group: no decision here,
                and no state to save. *)
             step st r.d
       with
           if st.flat then begin
             st.declined <- (st.line, st.column) :: st.declined;
             step st r.d
           end
     -- treating "a group laid out flat" as a declined break. It is a plausible
     reading of what a resolution is, and it is wrong: the elective break is the
     flat_alt, and the
     group, and counting groups makes the record depend on how many times the
     document was grouped. *)

  (* -- flat_alt, left and right -----------------------------------------------

     Both are stated in a context that moves the column and the indentation, so
     that "fits" varies across the corpus: the document is
     [nest j (text p ^^ group X)], and the group fits exactly when
     [measure p + flat_width a <= width]. Where it fits, [flat_alt a b] renders
     as [a] does under the same group. Where it exceeds the ruler, the group is
     left broken and [flat_alt a b] renders as [b] does at that indentation with
     the group removed.

     [flat_width] comes from the reference implementation, so the premise of each
     law is decided independently of the engine under test.
     ------------------------------------------------------------------------ *)

  let ctx j p x = H.nest j (H.text p ^^ x)

  let flat_alt_gen =
    Gen.map4
      (fun a b p j -> a, b, p, j)
      (Surface.gen Surface.rich)
      (Surface.gen Surface.rich)
      (Gen.oneof_list (Surface.words @ Surface.unicode_words))
      (Gen.int_range 0 6)
  ;;

  let show_case (a, b, p, j) =
    Printf.sprintf "a=%s b=%s p=%S j=%d" (Surface.show a) (Surface.show b) p j
  ;;

  let fits ~width p a =
    match ref_flat_width a with
    | Some w -> P.measure p + w <= width
    | None -> false
  ;;

  let flat_alt_left =
    QCheck_alcotest.to_alcotest
      ~speed_level:`Quick
      (Test.make
         ~count:2000
         ~name:"flat_alt left: inside a group that fits, flat_alt a b renders as a"
         ~print:show_case
         flat_alt_gen
         (fun (a, b, p, j) ->
            List.for_all
              ~f:(fun width ->
                (not (fits ~width p a))
                ||
                let da = to_doc a
                and db = to_doc b in
                let got = ctx j p (H.group (H.flat_alt da db)) in
                let want = ctx j p (H.group da) in
                String.equal
                  (H.to_string (fst (H.render ~width got)))
                  (H.to_string (fst (H.render ~width want))))
              Surface.widths))
  ;;

  (* MUTATION flat_alt left:
       in [step], the [Alt] case, replace
           step st r.f
       (the branch guarded by [if st.flat then]) with
           step st r.b
     -- the flat branch of a flat_alt taken broken. *)

  let flat_alt_right =
    QCheck_alcotest.to_alcotest
      ~speed_level:`Quick
      (Test.make
         ~count:2000
         ~name:
           "flat_alt right: inside a group that does not fit, flat_alt a b renders as b"
         ~print:show_case
         flat_alt_gen
         (fun (a, b, p, j) ->
            List.for_all
              ~f:(fun width ->
                fits ~width p a
                ||
                let da = to_doc a
                and db = to_doc b in
                let got = ctx j p (H.group (H.flat_alt da db)) in
                let want = ctx j p db in
                String.equal
                  (H.to_string (fst (H.render ~width got)))
                  (H.to_string (fst (H.render ~width want))))
              Surface.widths))
  ;;

  (* MUTATION flat_alt right:
       in [step], the [Alt] case, replace
           else step st r.b
       with
           else step st r.f *)

  (* -- no newline in text -------------------------------------------------- *)

  let no_newline_in_text =
    test
      ~count:2000
      "no newline in text: check rejects exactly the documents containing one"
      Surface.dirty
      (fun s ->
         let expected = Surface.offenders s in
         match H.check (to_doc s), expected with
         | Ok (), [] -> true
         | Error es, _ :: _ ->
           List.length es = List.length expected
           && List.for_all2
                ~f:(fun (e : H.error) (text, index) -> e.text = text && e.index = index)
                es
                expected
         | Ok (), _ :: _ | Error _, [] -> false)
  ;;

  (* MUTATION no newline in text:
       in [check], replace
           (match String.index_opt s '\n' with
       with
           (match (ignore s; None) with
     -- [check] accepts everything. The corpus [Surface.dirty] mixes text nodes
     carrying newlines with clean ones, so a mutation in either direction
     reddens: [Some 0] in place of [None], making [check] reject everything,
     reddens it too. *)

  (* -- totality ------------------------------------------------------------ *)

  let totality =
    test
      ~count:2000
      "totality: render returns for every document"
      Surface.dirty
      (at_all_widths (fun width s ->
         match H.render ~width (to_doc s) with
         | _ -> true
         | exception _ -> false))
  ;;

  (* MUTATION totality:
       treat [nest] as non-negative -- two edits, the clamp being defensive at
       more than one level. In [emit_break] replace
           let ind = if st.indent < 0 then 0 else st.indent in
       with
           let ind = st.indent in
       and replace
           let indent_width n =
             if n <= 0 then iw_cache.(0)
             else if n < 65 then iw_cache.(n)
       with
           let indent_width n =
             if n < 65 then iw_cache.(n)
     -- [render] then indexes the cache out of bounds. The corpus for this law
     ([Surface.dirty]) generates negative nesting for that reason: [nest] accepts
     it, the emitted indentation is clamped at zero, and the arithmetic in
     between has to survive it.

     RECORDED, UNTESTED. The second route to a partial [render] is the
     [Flat_violation] path, a hardline reached in flat mode. Replacing
         if st.flat then raise Flat_violation
     with
         if st.flat then assert false
     stays green at any corpus size, and the corpus is adequate: flat mode is
     entered where the cached width is finite, [Hard] is unflattenable, and [Alt]
     is flattenable exactly when its flat branch is, so flat mode keeps clear of
     hardlines. The recovery in [drive] makes totality structural as well as
     argued.

     The recovery can be brought within reach and judged. The mutation
         | Hard -> false   ~>   | Hard -> true
     in [flattenable] measures every group containing a hardline as flattenable,
     firing the recovery on most of the corpus, with the whole suite still
     passing and the PPrint differential included. So it produces the correct
     layout when it runs. Combining that with a flat-mode hardline that emits
     nothing reddens [doc/3]. The cached width protects that invariant. *)

  (* -- stream fidelity ----------------------------------------------------- *)

  let stream_fidelity =
    test
      "stream fidelity: to_string reproduces the rendered bytes"
      Surface.wild
      (at_all_widths (fun width s ->
         String.equal (bytes ~width s) (ref_render ~width s).Reference.bytes))
  ;;

  (* MUTATION stream fidelity:
       in [to_string], replace
           | S_line (n, k) ->
               Buffer.add_char b '\n';
               Buffer.add_string b (spaces n);
       with
           | S_line (_, k) ->
               Buffer.add_char b '\n';
     -- the fold drops the indentation the stream recorded. *)

  (* -- width soundness --------------------------------------------------------

     The law as usually stated -- "no line exceeds the width unless it contains
     an unbreakable unit that is itself wider than the space remaining" -- is a
     tautology, and is tested as such below for completeness. The column advances
     by emitting an atom, so the atom the column was crossing the ruler on began
     at some column c <= width and ended past it, making it wider than
     [width - c] by arithmetic. It stays green under every mutation of the
     engine, which is recorded here.

     What the sentence is trying to say is that every position where the
     printer had a break available and resolved it flat lay within the ruler:
     [declined_within_ruler] below. That is a theorem about the engine, a flat
     group being entered at column c only where c plus its width fits, so every
     column inside its flat extent fits. It carries weight because [declined] is
     itself checked complete against an implementation that computes it
     separately.
     ------------------------------------------------------------------------ *)

  let declined_complete =
    test
      "declined records exactly the flat_alts resolved flat"
      Surface.wild
      (at_all_widths (fun width s ->
         let _, r = render ~width s in
         r.H.declined = (ref_render ~width s).Reference.declined))
  ;;

  (* MUTATION declined completeness:
       in [step], the [Alt] case, replace
           st.declined <- (st.line, st.column) :: st.declined;
       with
           st.declined <- (st.line + 1, st.column) :: st.declined; *)

  let declined_within_ruler =
    test
      "width soundness: no break is declined from beyond the ruler"
      Surface.wild
      (at_all_widths (fun width s ->
         let _, r = render ~width s in
         List.for_all ~f:(fun (_, c) -> c <= width) r.H.declined))
  ;;

  (* MUTATION width soundness:
       in [step], the [Group] case, replace
           r.flattenable && W.compare (W.add st.column r.req) st.ruler <= 0
       with
           r.flattenable && W.compare st.column st.ruler <= 0
     -- the fit test forgets to add the group's requirement, so a group is laid
     out flat whenever the current column is inside the ruler however wide the
     group is, and breaks get declined from columns past it. *)

  let width_soundness_as_written =
    (* The sentence above, verbatim, with "unbreakable unit" read as "run of output
       between two consecutive positions where the printer could have broken":
       the start of the line, each declined break on it, and its end. *)
    test
      "width soundness as written (tautological; see the comment above)"
      Surface.wild
      (at_all_widths (fun width s ->
         let stream, r = render ~width s in
         let ls = H.lines stream in
         Array.to_list ls
         |> List.mapi ~f:(fun i w -> i, w)
         |> List.for_all ~f:(fun (l, lw) ->
           lw <= width
           ||
           let breaks =
             0
             :: List.filter_map
                  ~f:(fun (l', c) -> if l' = l then Some c else None)
                  r.H.declined
           in
           let bounds = breaks @ [ lw ] in
           let rec any = function
             | c0 :: (c1 :: _ as rest) -> c1 - c0 > width - c0 || any rest
             | _ -> false
           in
           any bounds)))
  ;;

  (* -- width monotonicity -----------------------------------------------------

     The strong form -- widening the ruler leaves the line count at or below
     what it was --

         w <= w'  =>  line_count (render ~width:w' d) <= line_count (render ~width:w d)

     is FALSE. Widening the ruler can flatten an early group, saving that group's
     lines and spending horizontal room a later group needed; the later group
     then breaks, at a cost above what the first one saved. The minimal
     counterexample -- exhaustively minimal at nine nodes over
     {text, line, softline, hardline, group, ^^} -- is pinned as a unit test
     below, so that a change to it shows up as an edit here.

     The two ends hold. The shortest rendering is at unbounded width and the
     longest at zero, so the line count moves between two fixed endpoints.
     ------------------------------------------------------------------------ *)

  let line_count ~width s = Array.length (H.lines (fst (H.render ~width (to_doc s))))

  let flat_is_shortest =
    (* At an unbounded ruler every group fits and is laid out flat, contributing
       zero breaks: a hardline inside one would have made the group
       unflattenable. The breaks that remain lie outside every group, and are
       taken at every width. The line count at an unbounded ruler is therefore
       the minimum over all widths. *)
    test
      "flat is shortest: no width renders in fewer lines than max_int does"
      Surface.wild
      (at_all_widths (fun width s -> line_count ~width s >= line_count ~width:max_int s))
  ;;

  let narrowest_is_longest =
    (* The other end, by the same argument run backwards: a group laid out flat
       contributes no breaks, so replacing flat by broken can only add them, and
       a ruler of zero flattens the least. *)
    test
      "narrowest is longest: no width renders in more lines than 0 does"
      Surface.wild
      (at_all_widths (fun width s -> line_count ~width:0 s >= line_count ~width s))
  ;;

  let flat_threshold =
    (* The monotone statement that does hold: a group within the ruler is laid
       out flat, on one line, and that line is the flat rendering. Compared
       against an independent flat interpreter, so this relates two
       implementations. *)
    test
      "a group at or above its flat width renders flat, on one line"
      Surface.wild
      (fun s ->
         match Reference.flat s with
         | None -> true (* contains a hardline; nothing to say *)
         | Some flat ->
           let fw = Option.get (ref_flat_width s) in
           let d = H.group (to_doc s) in
           List.for_all
             ~f:(fun width ->
               width < fw
               ||
               let stream, _ = H.render ~width d in
               Array.length (H.lines stream) = 1 && String.equal (H.to_string stream) flat)
             (max_int :: fw :: (fw + 1) :: Surface.widths))
  ;;

  (* MUTATION flat is shortest / narrowest is longest / flat threshold:
       in [step], the [Group] case, replace
           if not fits then step st r.d
       with
           if fits then step st r.d
     -- the fit decision inverted, so a group flattens exactly where it exceeds
     the ruler. Reddens all three, and much else.

     For [flat_threshold] on its own,
       in [step], the [Group] case, replace
           r.flattenable && W.compare (W.add st.column r.req) st.ruler <= 0
       with
           r.flattenable && W.compare (W.add st.column r.req) st.ruler < 0
     -- a group exactly as wide as the ruler is laid out broken, which is the
     threshold this law names. *)

  let monotonicity_counterexample =
    (* Pinned. Widening the ruler from 2 to 3 flattens the first group, which
       saves one line and costs three columns, and those three columns are what
       the second group needed. *)
    ( "width monotonicity fails: the minimal counterexample"
    , `Quick
    , fun () ->
        let d = H.text "bb" ^^ H.group H.line ^^ H.group (H.line ^^ H.line) in
        let at width =
          let stream, _ = H.render ~width d in
          Array.length (H.lines stream), H.to_string stream
        in
        Alcotest.(check (pair int string)) "width 2" (2, "bb\n  ") (at 2);
        Alcotest.(check (pair int string)) "width 3" (3, "bb \n\n") (at 3);
        (* Wrapping the whole document in one group leaves monotonicity broken.
           At widths 1 and 2 the outer group exceeds the ruler, so the inner pair
           behaves as above; at 3 the outer group takes over. *)
        let nested =
          H.group (H.group (H.text "a" ^^ H.line) ^^ H.group (H.softline ^^ H.line))
        in
        let at width =
          let stream, _ = H.render ~width nested in
          Array.length (H.lines stream), H.to_string stream
        in
        Alcotest.(check (pair int string)) "nested, width 1" (2, "a\n ") (at 1);
        Alcotest.(check (pair int string)) "nested, width 2" (3, "a \n\n") (at 2);
        Alcotest.(check (pair int string)) "nested, width 3" (1, "a  ") (at 3) )
  ;;

  let extreme_nesting =
    (* [render] is total for absurd documents as much as sensible ones. The
       generated corpus nests by single digits and reaches the arithmetic on
       small values alone; an indentation with a high bit set exercises the rest.

       [render] and [lines] are the claims here. [to_string] materialises the
       indentation, and 2^48 spaces exceed the length of a string. *)
    ( "render is total at absurd nesting depths"
    , `Quick
    , fun () ->
        List.iter
          ~f:(fun k ->
            let j = 1 lsl k in
            let d = H.nest j (H.text "a" ^^ H.hardline ^^ H.text "b") in
            match H.render ~width:80 d with
            | stream, _ ->
              Alcotest.(check int)
                (Printf.sprintf "nest (1 lsl %d): two lines" k)
                2
                (Array.length (H.lines stream))
            | exception e ->
              Alcotest.failf "nest (1 lsl %d) raised %s" k (Printexc.to_string e))
          [ 8; 30; 47; 48; 49; 60; Sys.int_size - 2 ] )
  ;;

  let suite =
    ( "laws-" ^ P.name
    , [ determinism
      ; annotation_transparency
      ; annotation_erasure
      ; group_idempotence
      ; flat_alt_left
      ; flat_alt_right
      ; no_newline_in_text
      ; totality
      ; stream_fidelity
      ; declined_complete
      ; declined_within_ruler
      ; width_soundness_as_written
      ; flat_is_shortest
      ; narrowest_is_longest
      ; flat_threshold
      ; monotonicity_counterexample
      ; extreme_nesting
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

(* -- the differential against PPrint ------------------------------------------

   A small corpus here, so that the law table is complete in one file; the
   exhaustive corpus is a suite of its own.

   Ascii only. PPrint measures a string as [String.length], which is
   [Ascii_width], so the comparison is defined under that instance alone;
   agreement with a column-counting engine would require a different printer.
   The Utf8 counterpart is measure-independence, checked in its own suite: the
   choice of measure moves whitespace, and the two instances put the same text in
   the same order, differing in where they break.
   -------------------------------------------------------------------------- *)

module H = Handsome.Ascii

let differential =
  QCheck_alcotest.to_alcotest
    ~speed_level:`Quick
    (Test.make
       ~count:2000
       ~name:"differential: agrees with PPrint byte for byte"
       ~print:Surface.show
       (Surface.gen Surface.rich)
       (fun s ->
          List.for_all
            ~f:(fun width ->
              String.equal
                (H.to_string (fst (H.render ~width (Surface.to_doc s))))
                (Surface.pprint_to_string ~f:Surface.to_pprint ~width s))
            Surface.widths))
;;

(* MUTATION differential:
     in [step], the [Group] case, replace
         r.flattenable && W.compare (W.add st.column r.req) st.ruler <= 0
     with
         r.flattenable && W.compare (W.add st.column r.req) st.ruler < 0
   -- an off-by-one ruler. It reddens stream fidelity as well, the reference
   renderer implementing the same specification. The differential carries weight
   because PPrint was written independently. *)

let suites = [ Ascii.suite; Utf8.suite; "laws-differential", [ differential ] ]
