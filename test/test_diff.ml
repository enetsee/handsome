(* The differential against PPrint.

   PPrint (Pottier) implements the same Wadler-Leijen algorithm, with widths
   cached at construction, arrived at independently. Agreement with it over
   documents both can express is the evidence that this engine lays out
   correctly.

   The mapping is in [surface.ml]. Every handsome construct in the corpus has an
   exact PPrint counterpart:

     text s     -> string s          group      -> group
     empty      -> empty             nest j     -> nest j
     ( ^^ )     -> ( ^^ )            align      -> align
     hardline   -> hardline          annotate   -> (erased: transparent)
     flat_alt   -> ifflat
     line       -> ifflat (string " ") hardline
     softline   -> ifflat empty hardline
     blank      -> ifflat (string " ") empty

   The ribbon fraction is 1.0, which makes PPrint's ribbon constraint
   ([column <= last_indent + ribbon]) implied by its width constraint and so a
   no-op; handsome has no ribbon.

   PPrint's suppressible blanks ([blank], [space], [break]) lie outside the
   shared fragment. They are a property of PPrint's renderer, where every
   handsome construct is a property of the document.
   [idiomatic_up_to_blanks] below measures that gap. *)

open StdLabels
module H = Handsome.Ascii
open QCheck2

let test ?(count = 2000) name flavour prop =
  QCheck_alcotest.to_alcotest
    ~speed_level:`Quick
    (Test.make ~count ~name ~print:Surface.show (Surface.gen flavour) prop)
;;

let handsome ~width s = H.to_string (fst (H.render ~width (Surface.to_doc s)))

let agrees ~f width s =
  String.equal (handsome ~width s) (Surface.pprint_to_string ~f ~width s)
;;

let at_all_widths p s = List.for_all ~f:(fun w -> p w s) Surface.widths

(* -- the gate -------------------------------------------------------------- *)

let plain_fragment =
  test
    ~count:3000
    "agrees with PPrint byte for byte (plain fragment)"
    Surface.plain
    (at_all_widths (fun width s -> agrees ~f:Surface.to_pprint width s))
;;

let rich_fragment =
  (* Both engines express annotations and general [flat_alt]: the first is
     transparent to layout, and the second is [ifflat]. Both therefore belong in
     the differential. *)
  test
    ~count:3000
    "agrees with PPrint byte for byte (annotations, flat_alt)"
    Surface.rich
    (at_all_widths (fun width s -> agrees ~f:Surface.to_pprint width s))
;;

let wide_widths =
  (* The corpus above tops out at width 80 with short words; this one pushes the
     ruler through the range where documents are near the boundary. *)
  QCheck_alcotest.to_alcotest
    ~speed_level:`Quick
    (Test.make
       ~count:3000
       ~name:"agrees with PPrint at every width from 0 to 60"
       ~print:(fun (s, w) -> Printf.sprintf "%s @ width %d" (Surface.show s) w)
       (Gen.pair (Surface.gen Surface.rich) (Gen.int_range 0 60))
       (fun (s, width) -> agrees ~f:Surface.to_pprint width s))
;;

(* -- where the two engines differ ------------------------------------------ *)

let strip_trailing_spaces s =
  String.concat
    ~sep:"\n"
    (List.map
       ~f:(fun line ->
         let n = ref (String.length line) in
         while !n > 0 && line.[!n - 1] = ' ' do
           decr n
         done;
         String.sub line ~pos:0 ~len:!n)
       (String.split_on_char ~sep:'\n' s))
;;

let idiomatic_up_to_blanks =
  (* Spelling [line] and [softline] as PPrint's [break] -- what a PPrint user
     would write -- separates the two, PPrint's [break] emitting a suppressible
     blank where handsome's [line] emits a space. The gap is trailing whitespace
     and nothing else, and it appears in one shape: a flat group ending in the
     separator, with the break arriving from outside the group. *)
  test
    "matches idiomatic PPrint (break) up to trailing whitespace"
    Surface.rich
    (at_all_widths (fun width s ->
       String.equal
         (strip_trailing_spaces (handsome ~width s))
         (strip_trailing_spaces
            (Surface.pprint_to_string ~f:Surface.to_pprint_idiomatic ~width s))))
;;

let suite =
  "differential", [ plain_fragment; rich_fragment; wide_widths; idiomatic_up_to_blanks ]
;;
