(* The document type. The derived breaks are the flat_alts they are defined to
   be, [check] rejects exactly the documents with a newline in a text node, and
   [pp] preserves enough structure to be read back. *)

open StdLabels
module H = Handsome.Ascii
open QCheck2

let ( ^^ ) = H.( ^^ )

let test ?(count = 1000) name flavour prop =
  QCheck_alcotest.to_alcotest
    ~speed_level:`Quick
    (Test.make ~count ~name ~print:Surface.show (Surface.gen flavour) prop)
;;

(* -- [check] --------------------------------------------------------------- *)

let check_accepts_clean =
  test "check accepts every document without a newline in text" Surface.rich (fun s ->
    match H.check (Surface.to_doc s) with
    | Ok () -> true
    | Error _ -> false)
;;

let check_agrees_with_ground_truth =
  (* The [dirty] corpus mixes text nodes carrying newlines with clean ones, so
     this test carries both halves of the law: the documents rejected are exactly
     those holding one, and the nodes named are exactly those that do. *)
  test
    "check rejects a document iff a text node contains a newline"
    Surface.dirty
    (fun s ->
       let expected = Surface.offenders s in
       match H.check (Surface.to_doc s), expected with
       | Ok (), [] -> true
       | Error es, _ :: _ ->
         List.length es = List.length expected
         && List.for_all2
              ~f:(fun (e : H.error) (text, index) -> e.text = text && e.index = index)
              es
              expected
       | Ok (), _ :: _ | Error _, [] -> false)
;;

(* -- [pp] ------------------------------------------------------------------ *)

let pp_round_trips =
  test "pp round-trips through the reader" Surface.dirty (fun s ->
    let d = Surface.to_doc s in
    let printed = Reader.print_doc d in
    let printed' = Reader.print_doc (Reader.read printed) in
    String.equal printed printed')
;;

let pp_round_trip_is_exact_on_unit =
  (* [unit t] has a single annotation payload, so the round-trip is the identity
     on the document itself as well as on its printed form. *)
  test "read (pp d) = d structurally, on unit documents" Surface.rich (fun s ->
    let d = H.unannotate (Surface.to_doc s) in
    Reader.read (Reader.print_doc d) = d)
;;

(* -- the derived breaks -------------------------------------------------------

   Each is the flat_alt it is defined to be, and the square they occupy is
   complete:

                       broken = hardline   broken = empty
     flat = text " "   line                blank
     flat = empty      softline            empty
   -------------------------------------------------------------------------- *)

let unit_cases =
  let str ~width d = H.to_string (fst (H.render ~width d)) in
  let eq name a b = Alcotest.(check string) name a b in
  [ ( "line = flat_alt (text \" \") hardline"
    , `Quick
    , fun () ->
        eq "flat" (str ~width:80 (H.group H.line)) " ";
        eq "broken" (str ~width:0 (H.group H.line)) "\n" )
  ; ( "softline = flat_alt empty hardline"
    , `Quick
    , fun () ->
        eq "flat" (str ~width:80 (H.group H.softline)) "";
        eq "broken" (str ~width:0 (H.group (H.text "ab" ^^ H.softline))) "ab\n" )
  ; ( "blank = flat_alt (text \" \") empty"
    , `Quick
    , fun () ->
        eq "flat" (str ~width:80 (H.group (H.text "a" ^^ H.blank ^^ H.text "b"))) "a b";
        eq
          "broken"
          (str ~width:1 (H.group (H.text "a" ^^ H.blank ^^ H.softline ^^ H.text "b")))
          "a\nb" )
  ; ( "hardline dissolves its group"
    , `Quick
    , fun () -> eq "" (str ~width:80 (H.group (H.text "a" ^^ H.hardline))) "a\n" )
  ; ( "outside a group a document is broken"
    , `Quick
    , fun () -> eq "" (str ~width:80 (H.text "a" ^^ H.line ^^ H.text "b")) "a\nb" )
  ; ( "check reports the offending node"
    , `Quick
    , fun () ->
        match H.check (H.text "a" ^^ H.text "b\nc") with
        | Ok () -> Alcotest.fail "expected a rejection"
        | Error [ e ] ->
          eq "text" e.H.text "b\nc";
          Alcotest.(check int) "index" 1 e.H.index
        | Error es -> Alcotest.failf "expected one error, got %d" (List.length es) )
  ]
;;

let suite =
  ( "doc"
  , unit_cases
    @ [ check_accepts_clean
      ; check_agrees_with_ground_truth
      ; pp_round_trips
      ; pp_round_trip_is_exact_on_unit
      ] )
;;
