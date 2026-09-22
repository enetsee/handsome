(* Rendering is linear in the document, because each node caches the width it
   would occupy laid out flat and a group's decision is a comparison against a
   number that is already there.

   The second family is the one that separates the two designs: every atom
   inside its own group, which costs O(nw) in an engine that lays each group out
   flat and re-runs it broken on failure. The third is deep group nesting, which
   costs stack in an engine that recurses. The fourth is n frames nested inside
   one another with every frame's conditional at the innermost point, the way a
   Lisp printer closes its brackets: each frame then has every frame outside it
   free in its body, which is quadratic in n for a frame that looks at each.

   A frame's body is built inside its callback, so building that family recurses
   n deep, and OCaml 5 scans the whole stack at each minor collection. Past
   n = 10^5 the growth in building time is that scan: with OCAMLRUNPARAM=s=8M,
   which makes minor collections rarer, building stays near a microsecond a
   node to n = 8 x 10^5.

   Run with: dune exec bench/bench.exe *)

open StdLabels
module H = Handsome.Ascii

let ( ^^ ) = H.( ^^ )

let time name f =
  let t = Sys.time () in
  let r = f () in
  Printf.printf "  %-34s %6.3fs\n%!" name (Sys.time () -. t);
  r
;;

let sizes = [ 10_000; 100_000; 1_000_000 ]

let () =
  print_endline "flat concatenation (n text nodes and breaks)";
  List.iter
    ~f:(fun n ->
      let d =
        H.concat
          (List.init ~len:n ~f:(fun i -> H.text (string_of_int (i mod 10)) ^^ H.line))
      in
      let s =
        time (Printf.sprintf "render n=%d" n) (fun () -> fst (H.render ~width:60 d))
      in
      Printf.printf
        "     %d lines, %d bytes\n%!"
        (Array.length (H.lines s))
        (String.length (H.to_string s)))
    sizes;
  print_endline "every atom in its own group (the case measuring first avoids)";
  List.iter
    ~f:(fun n ->
      let d = H.concat (List.init ~len:n ~f:(fun _ -> H.group (H.text "x" ^^ H.line))) in
      ignore
        (time (Printf.sprintf "render n=%d" n) (fun () -> fst (H.render ~width:60 d))))
    sizes;
  print_endline "n nested groups, each adding a level of indentation";
  List.iter
    ~f:(fun n ->
      let d = ref (H.text "x") in
      for _ = 1 to n do
        d := H.group (H.nest 1 (!d ^^ H.line))
      done;
      ignore
        (time (Printf.sprintf "render n=%d" n) (fun () -> fst (H.render ~width:60 !d))))
    [ 10_000; 100_000; 1_000_000 ];
  print_endline "n nested frames, every conditional at the innermost point";
  List.iter
    ~f:(fun n ->
      let d =
        time (Printf.sprintf "build n=%d" n) (fun () ->
          let rec nest alts i =
            if i = n
            then
              H.text "x"
              ^^ H.concat (List.map ~f:(fun alt -> alt H.empty (H.text ")")) alts)
            else H.framed (fun alt -> H.text "(" ^^ nest (alt :: alts) (i + 1))
          in
          nest [] 0)
      in
      ignore (time (Printf.sprintf "check n=%d" n) (fun () -> H.check d));
      ignore
        (time (Printf.sprintf "render n=%d" n) (fun () -> fst (H.render ~width:60 d))))
    [ 1_000; 10_000; 100_000 ]
;;
