(* The stack guarantee, under the only configuration that can see it.

   [reannotate], [unannotate] and [pp] were ordinary structural recursion, which
   costs stack in the depth of the document: in [Cat] neither call sits in tail
   position, so a right-leaning [concat] chain is as deep as it is long. At an
   8 MB stack [reannotate] and [pp] failed at 200k nodes and [unannotate] at 2M,
   while [render] and [check] passed both.

   OCaml 5 grows the main fibre's stack on demand, so at the default settings the
   recursive forms passed 4M nodes and the suite saw nothing. This runs under
   [OCAMLRUNPARAM=l=1000000] -- 1M words, about what the declared 4.14 floor gets
   from the OS. It gets its own executable because the cap would otherwise apply
   to the whole of [test_handsome], and the rest of the suite is written on the
   assumption of a stack it can grow.

   Not Alcotest, to keep the process to the library and the stdlib under a
   deliberately hostile runtime. It prints what Alcotest prints for the two
   things the mutation harness reads: [list] gives an id and a name per case, and
   a failure gives a [[FAIL] depth <i>] line. Exit status is the verdict.

   MUTATIONS:
     reannotate-recursive  reannotate restored to structural recursion.
     unannotate-recursive  the same for unannotate.
     pp-recursive          the same for pp.
     check-recursive       check's worklist replaced by recursion, which only
                           the left-leaning corpus can catch.

   Each must redden its own case. If one does not, suspect the cap before the
   test: check that the recursive form still overflows at [n] by running this
   executable with the mutation applied and the environment variable set. *)

open StdLabels
module H = Handsome.Ascii

(* Two corpora, because the leaning of a [concat] chain decides which traversals
   go deep.

   [concat] folds right, so [right] leans right: every [Cat] node's second child
   is the whole of the rest. [left] is the mirror image, and what a caller
   writing [List.fold_left ( ^^ )]
   builds.

   The distinction matters. A [Cat] case written [go r.l; go r.r]
   recurses on the left child and tail-calls the right, so it is flat on a
   right-leaning chain and as deep as the document on a left-leaning one -- that
   is [check]'s shape, and it is why a right-leaning corpus alone reported
   nothing when [check]'s worklist was replaced by recursion. Running
   both leanings costs a second pass and covers the [Cat] cases that lean the
   other way.

   The size is set by the last traversal to break. Under
   this cap the recursive [pp] failed at 150k, [reannotate] at 200k and
   [unannotate] only at 300k: [unannotate] drops the annotation in tail position
   where [reannotate] rebuilds it, so it spends one frame per node where the
   other spends two, and needs twice the document to run out. 600k is twice the
   largest of those, which leaves room for a frame layout that differs across the
   three compilers CI builds on. It is also inside the range [bench/bench.ml]
   builds. *)
let n = 600_000
let leaf i = H.annotate i (H.text "x")
let right () = H.concat (List.init ~len:n ~f:leaf)

let left () =
  let d = ref H.empty in
  for i = 0 to n - 1 do
    d := H.( ^^ ) !d (leaf i)
  done;
  !d
;;

(* Output is not the subject; the shape of the traversal is. *)
let sink = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let ops =
  [ ("render", fun d -> ignore (H.render ~width:60 d))
  ; ("check", fun d -> ignore (H.check d))
  ; ("unannotate", fun d -> ignore (H.unannotate d))
  ; ("reannotate", fun d -> ignore (H.reannotate succ d))
  ; ( "pp"
    , fun d ->
        H.pp sink d;
        Format.pp_print_flush sink () )
  ]
;;

let corpora = [ "a right-leaning chain", right; "a left-leaning chain", left ]
let name op shape = Printf.sprintf "%s leaves the stack flat on %s" op shape

let () =
  if Array.length Sys.argv > 1 && String.equal Sys.argv.(1) "list"
  then
    List.iteri
      ~f:(fun ci (shape, _) ->
        List.iteri
          ~f:(fun oi (op, _) ->
            Printf.printf "depth %d %s\n" ((ci * List.length ops) + oi) (name op shape))
          ops)
      corpora
  else (
    Printf.printf
      "depth: %d nodes, OCAMLRUNPARAM=%s\n"
      n
      (try Sys.getenv "OCAMLRUNPARAM" with
       | Not_found -> "(unset)");
    let failed = ref false in
    List.iteri
      ~f:(fun ci (shape, build) ->
        let d = build () in
        List.iteri
          ~f:(fun oi (op, f) ->
            let i = (ci * List.length ops) + oi in
            match f d with
            | () -> Printf.printf "[OK] depth %d %s\n" i (name op shape)
            | exception e ->
              failed := true;
              Printf.printf
                "[FAIL] depth %d %s: %s\n"
                i
                (name op shape)
                (Printexc.to_string e))
          ops;
        (* Returned before the next corpus is built, so the peak holds one
           corpus and one result. *)
        Gc.compact ())
      corpora;
    if !failed
    then (
      print_endline "depth: FAILED";
      exit 1)
    else Printf.printf "depth: %d cases passed\n" (List.length corpora * List.length ops))
;;
