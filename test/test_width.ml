(* The WIDTH instances: the obligations the signature places on any of them,
   what distinguishes the two supplied, and what both leave alone.

   The obligations are a functor instantiated for each instance, since they are
   properties of the signature. A third instance can be dropped in and held to
   the same terms.

   Every property names a mutation that reddens it, in the format the rest of
   the suite uses. *)

open StdLabels
open QCheck2
module A = Handsome.Ascii_width
module U = Handsome.Utf8_width

let quick ?(count = 1000) name print gen prop =
  QCheck_alcotest.to_alcotest ~speed_level:`Quick (Test.make ~count ~name ~print gen prop)
;;

(* -- corpora --------------------------------------------------------------- *)

let pieces = Surface.words @ Surface.unicode_words

(* Always valid UTF-8, and reaching every case the Utf8 measure distinguishes:
   wide, zero-width, halfwidth, ambiguous, astral, and the code point that is
   both combining and wide. *)
let gen_valid =
  Gen.map
    (String.concat ~sep:"")
    (Gen.list_size (Gen.int_range 0 5) (Gen.oneof_list pieces))
;;

(* Arbitrary bytes, mostly outside valid UTF-8. *)
let gen_bytes =
  Gen.string_size (Gen.int_range 0 12) ~gen:(Gen.map Char.chr (Gen.int_range 0 255))
;;

let gen_ascii =
  Gen.map
    (String.concat ~sep:"")
    (Gen.list_size (Gen.int_range 0 5) (Gen.oneof_list Surface.words))
;;

let is_valid_utf8 s =
  let n = String.length s in
  let rec go i =
    if i >= n
    then true
    else (
      let d = String.get_utf_8_uchar s i in
      Uchar.utf_decode_is_valid d && go (i + Uchar.utf_decode_length d))
  in
  go 0
;;

(* -- the signature's obligations ------------------------------------------- *)

module Obligations
    (W : Handsome.Width.S with type t = int)
    (P : sig
       val name : string

       (* The strings this instance is asked to be additive over. Additivity is a
     statement about text: splitting a multi-byte sequence down the middle gives
     two strings whose displayed widths sum to something else, and the library
     measures and renders a text node whole. The corpus here is therefore
     well-formed input, and [total_on_garbage] below covers the rest. *)
       val gen : string Gen.t
     end) =
struct
  let t name = quick (Printf.sprintf "%s: %s" P.name name)

  let empty_is_zero =
    ( Printf.sprintf "%s: measure \"\" = zero" P.name
    , `Quick
    , fun () -> Alcotest.(check int) "" W.zero (W.measure "") )
  ;;

  let additive =
    t
      "measure (a ^ b) = add (measure a) (measure b)"
      (fun (a, b) -> Printf.sprintf "%S ^ %S" a b)
      (Gen.pair P.gen P.gen)
      (fun (a, b) -> W.measure (a ^ b) = W.add (W.measure a) (W.measure b))
  ;;

  let non_negative =
    t
      "measure s >= zero"
      (fun s -> Printf.sprintf "%S" s)
      gen_bytes
      (fun s -> W.compare (W.measure s) W.zero >= 0)
  ;;

  (* [compare] a total order, [add] associative, commutative and monotone in
     both arguments. The engine needs all four: it compares a sum against the
     ruler, it sums a document's text nodes in whatever order the concatenation
     tree happens to associate, and a frame sums its conditionals by frame, out
     of document order. *)
  let algebra =
    t
      "compare is a total order; add is associative, commutative and monotone"
      (fun (a, b, c) -> Printf.sprintf "%d %d %d" a b c)
      (Gen.triple
         (Gen.map W.measure P.gen)
         (Gen.map W.measure P.gen)
         (Gen.map W.measure P.gen))
      (fun (a, b, c) ->
         let sign x = compare x 0 in
         W.compare a a = 0
         && sign (W.compare a b) = -sign (W.compare b a)
         && ((not (W.compare a b <= 0 && W.compare b c <= 0)) || W.compare a c <= 0)
         && W.add (W.add a b) c = W.add a (W.add b c)
         && W.add a b = W.add b a
         && ((not (W.compare a b <= 0))
             || (W.compare (W.add a c) (W.add b c) <= 0
                 && W.compare (W.add c a) (W.add c b) <= 0)))
  ;;

  let total_on_garbage =
    t
      "measure never raises, on any bytes"
      (fun s -> Printf.sprintf "%S" s)
      gen_bytes
      (fun s ->
         match W.measure s with
         | _ -> true
         | exception _ -> false)
  ;;

  let cases = [ empty_is_zero; additive; non_negative; algebra; total_on_garbage ]
end

module O_ascii =
  Obligations
    (Handsome.Ascii_width)
    (struct
      let name = "ascii"
      let gen = gen_bytes
    end)

module O_utf8 =
  Obligations
    (Handsome.Utf8_width)
    (struct
      let name = "utf8"
      let gen = gen_valid
    end)

(* MUTATIONS for the obligations. Each edits one instance, and reddens only that
   instance's half of the pairs above, which shows the functor is instantiated
   twice over distinct modules.

     ascii-zero-is-one           Ascii_width: let zero = 0  ~>  let zero = 1
     ascii-measure-off-by-one    Ascii_width: let measure = String.length  ~>
                                   let measure s =
                                     if s = "" then 0 else String.length s + 1
     ascii-add-is-minus          Ascii_width: let add = ( + )  ~>
                                   let add a b = a - b
     ascii-raises-on-high-bytes  Ascii_width.measure raises on a byte above 127
     utf8-zero-is-one            Utf8_width: let zero = 0  ~>  let zero = 1
     utf8-add-is-minus           Utf8_width: let add = ( + )  ~>
                                   let add a b = a - b
     utf8-not-additive           Utf8_width.measure: !w  ~>
                                   if n = 0 then 0 else !w + 1
     utf8-raises-on-invalid      Utf8_width.measure raises where it would
                                   substitute U+FFFD

   [ascii-raises-on-high-bytes] makes [String.length] partial deliberately.
   Making it partial is the only way to redden the totality property, and a
   property that stays green under every mutation is untested. *)

(* -- the two instances against each other ---------------------------------- *)

let ascii_never_below_utf8 =
  (* The soundness argument for Ascii_width, made concrete: byte length reports
     at least the display width, since every UTF-8 encoding is at least as long
     in bytes as it is wide in columns. A one-column code point takes at least
     one byte; a two-column one is East Asian and takes at least three. *)
  quick
    "ascii >= utf8 on every valid UTF-8 string"
    (fun s -> Printf.sprintf "%S" s)
    gen_valid
    (fun s -> A.measure s >= U.measure s)
;;

let agree_on_ascii =
  quick
    "ascii = utf8 = String.length on ASCII-only input"
    (fun s -> Printf.sprintf "%S" s)
    gen_ascii
    (fun s -> A.measure s = String.length s && U.measure s = String.length s)
;;

(* MUTATIONS:
     utf8-wide-is-four    Utf8_width.column: wide -> 2  ~>  wide -> 4
     utf8-default-is-two  Utf8_width.column: else 1  ~>  else 2

   The first carries the weight. A wide code point claiming more columns than
   its encoding has bytes breaks Ascii >= Utf8, and that inequality lets a
   caller move from one instance to the other knowing lines can only shorten. *)

(* -- the Utf8 measure ------------------------------------------------------ *)

let utf8_cases =
  let eq name expect s = Alcotest.(check int) name expect (U.measure s) in
  [ ( "combining marks measure 0"
    , `Quick
    , fun () ->
        eq "U+0301" 0 "\xcc\x81";
        eq "U+302A (Mn and Wide at once: zero wins)" 0 "\xe3\x80\xaa";
        eq "U+200D zero width joiner (Cf)" 0 "\xe2\x80\x8d" )
  ; ( "precomposed and decomposed agree"
    , `Quick
    , fun () ->
        eq "U+00E9" 1 "\xc3\xa9";
        eq "e + U+0301" 1 "e\xcc\x81" )
  ; ( "wide characters measure 2"
    , `Quick
    , fun () ->
        eq "U+4E2D" 2 "\xe4\xb8\xad";
        eq "U+FF21 fullwidth A" 2 "\xef\xbc\xa1";
        eq "U+1F44D" 2 "\xf0\x9f\x91\x8d" )
  ; ( "halfwidth forms measure 1"
    , `Quick
    , fun () ->
        eq "U+FF71 halfwidth katakana" 1 "\xef\xbd\xb1";
        eq "U+2192, East Asian Ambiguous" 1 "\xe2\x86\x92" )
  ; ( "malformed bytes measure as U+FFFD"
    , `Quick
    , fun () ->
        eq "lone continuation byte" 1 "\x80";
        eq "invalid lead byte" 1 "\xff";
        eq "truncated three-byte sequence" 1 "\xe4\xb8";
        eq "valid text either side of a bad byte" 3 "a\xffb" )
  ; ( "a ZWJ sequence is counted component by component"
    , `Quick
    , fun () ->
        (* Pinned. Utf8_width measures per code point, so the man-woman-girl
           family is three wide code points and two joiners: 2 + 0 + 2 + 0 + 2.
           A terminal following the emoji recommendations draws it in 2 columns,
           which requires UAX #29 grapheme clustering.

           The number is pinned so that a change to it shows up as an edit here.
           The direction is over-reporting, which the obligation permits, so
           width soundness holds at either value. *)
        eq
          "U+1F468 ZWJ U+1F469 ZWJ U+1F467"
          6
          "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x91\xa7" )
  ; ( "the tables say which Unicode release they came from"
    , `Quick
    , fun () ->
        Alcotest.(check bool) "non-empty" true (String.length Handsome.unicode_version > 0)
    )
  ; ( "the fast path agrees with the tables, which stay in the order it assumes"
    , `Quick
    , fun () ->
        (* [Utf8_width.column] sends every code point below [zero.(0)] straight
           to one column, on the ground that neither table can match there, and
           [measure] sends every byte below 0x80 straight past the decoder. Two
           things hold that up.

           The order. A regeneration that put a wide range below the first zero
           range would leave the fast path claiming one column for a code point
           worth two, and no test written against [measure] could see it, because
           [measure] would be the thing at fault. Reached through the mangled
           name: the tables are internal, and their order is the one fact outside
           the library that depends on them.

           The agreement. Over every code point, what [measure] answers must be
           what the tables say, with [mem] consulted directly as the oracle the
           fast path is an optimisation of. Checking every code point shows the
           fast path changed no answer at all. A corpus would only show that it
           changed none of the answers that corpus happened to ask for. *)
        let module T = Handsome__Unicode_tables in
        Alcotest.(check bool) "zero.(0) <= wide.(0)" true (T.zero.(0) <= T.wide.(0));
        let expect u = if T.mem T.zero u then 0 else if T.mem T.wide u then 2 else 1 in
        let b = Buffer.create 4 in
        for u = 0 to 0x10FFFF do
          (* Surrogates are not scalar values and [Uchar.of_int] rejects them. *)
          if u < 0xD800 || u > 0xDFFF
          then (
            Buffer.clear b;
            Buffer.add_utf_8_uchar b (Uchar.of_int u);
            let got = U.measure (Buffer.contents b) in
            if got <> expect u
            then
              Alcotest.failf "U+%04X: measure says %d, the tables say %d" u got (expect u))
        done )
  ]
;;

(* MUTATIONS:
     utf8-fast-path-bound-too-high
                             [column]: the fast-path bound raised from
                             [zero_min] to 0x1100, the first wide range, so
                             every combining mark below it measures one column
                             where it should measure zero.
     utf8-zero-before-wide   the two table lookups in [column] swapped. U+302A
                             is both Mn and Wide, so this claims two columns for
                             a mark drawn over the character it follows.
     utf8-combining-is-one   [column]: zero -> 0  ~>  zero -> 1
     utf8-wide-is-one        [column]: wide -> 2  ~>  wide -> 1
     utf8-byte-at-a-time     [measure]: i := !i + Uchar.utf_decode_length d  ~>
                             i := !i + 1, counting each byte of a multi-byte
                             sequence separately. The guard [!i < n] keeps it
                             terminating.
     unicode-version-lost    let unicode_version = Unicode_tables.unicode_version
                             ~>  let unicode_version = "" *)

(* -- what the measure leaves alone --------------------------------------------

   Choosing a width instance decides where lines break. Which bytes are printed,
   the order they appear in, and the integrity of each text node all hold
   constant across the choice.
   -------------------------------------------------------------------------- *)

module Da = Surface.Ascii_doc
module Du = Surface.Utf8_doc

let payloads_ascii s =
  let rec go acc = function
    | Handsome.Ascii.S_empty -> List.rev acc
    | Handsome.Ascii.S_text (_, t, k) -> go (t :: acc) k
    | Handsome.Ascii.S_line (_, k) -> go acc k
    | Handsome.Ascii.S_ann_push (_, k) | Handsome.Ascii.S_ann_pop k -> go acc k
  in
  go [] s
;;

let payloads_utf8 s =
  let rec go acc = function
    | Handsome.Utf8.S_empty -> List.rev acc
    | Handsome.Utf8.S_text (_, t, k) -> go (t :: acc) k
    | Handsome.Utf8.S_line (_, k) -> go acc k
    | Handsome.Utf8.S_ann_push (_, k) | Handsome.Utf8.S_ann_pop k -> go acc k
  in
  go [] s
;;

let doc_test ?(count = 500) name flavour prop =
  quick ~count name Surface.show (Surface.gen flavour) prop
;;

(* An elective break may change whitespace; anything else it changes is content
   the measure decided to move. *)
let strip_whitespace s =
  String.concat
    ~sep:""
    (List.filter_map
       ~f:(fun c -> if c = ' ' || c = '\n' then None else Some (String.make 1 c))
       (List.init ~len:(String.length s) ~f:(String.get s)))
;;

let same_text =
  (* Corpus restricted to the derived breaks. The general form of this property
     -- "the same document prints the same bytes under both instances" -- holds
     only where the branches of a [flat_alt] agree on their text:

         flat_alt (text "a") (text "bbbb")

     is a choice between two documents, and the width decides which one is
     taken. The interface asks callers to keep the branches semantically
     equivalent, and leaves that to them. Over [line], [softline], [blank] and
     [hardline], whose branches differ by a space or a newline, the property
     holds and states what it was meant to: the measure moves whitespace and
     leaves the rest in place. *)
  doc_test
    "the measure changes whitespace and no other byte"
    Surface.breaks_only
    (fun s ->
       List.for_all
         ~f:(fun width ->
           let a = fst (Handsome.Ascii.render ~width (Da.to_doc s)) in
           let u = fst (Handsome.Utf8.render ~width (Du.to_doc s)) in
           String.equal
             (strip_whitespace (String.concat ~sep:"" (payloads_ascii a)))
             (strip_whitespace (String.concat ~sep:"" (payloads_utf8 u))))
         Surface.widths)
;;

let same_at_infinite_width =
  (* The measure reaches the output through two channels: the fit decision at a
     group, and [align], which turns a column into an indentation. A ruler of
     [max_int] closes the first, every group fitting and every elective break
     resolving flat, so both instances take the same branch throughout. This
     corpus closes the second by omitting [align]. The bytes that remain are
     identical.

     Retaining [align] separates them, by the amount [align] is defined to
     differ:

         align (text "\xe4\xb8\xad" ^^ text "a" ^^ hardline ^^ text "b")

     indents the second line by 4 under Utf8 and by 5 under Ascii, each being
     the column under that measure. *)
  doc_test
    "at width = max_int, and with no align, the two agree exactly"
    Surface.no_align
    (fun s ->
       let width = max_int in
       String.equal
         (Handsome.Ascii.to_string (fst (Handsome.Ascii.render ~width (Da.to_doc s))))
         (Handsome.Utf8.to_string (fst (Handsome.Utf8.render ~width (Du.to_doc s)))))
;;

let cached_width_agrees =
  doc_test
    "every S_text carries exactly W.measure of its own bytes"
    Surface.dirty
    (fun s ->
       List.for_all
         ~f:(fun width ->
           let rec ok_a = function
             | Handsome.Ascii.S_empty -> true
             | Handsome.Ascii.S_text (w, t, k) -> w = A.measure t && ok_a k
             | Handsome.Ascii.S_line (_, k)
             | Handsome.Ascii.S_ann_push (_, k)
             | Handsome.Ascii.S_ann_pop k -> ok_a k
           in
           let rec ok_u = function
             | Handsome.Utf8.S_empty -> true
             | Handsome.Utf8.S_text (w, t, k) -> w = U.measure t && ok_u k
             | Handsome.Utf8.S_line (_, k)
             | Handsome.Utf8.S_ann_push (_, k)
             | Handsome.Utf8.S_ann_pop k -> ok_u k
           in
           ok_a (fst (Handsome.Ascii.render ~width (Da.to_doc s)))
           && ok_u (fst (Handsome.Utf8.render ~width (Du.to_doc s))))
         Surface.widths)
;;

let text_nodes_stay_whole =
  (* A text node is atomic: the engine breaks between nodes, so a document built
     from valid UTF-8 renders to valid UTF-8 payloads. A failure here indicates
     a multi-byte character cut in half. *)
  doc_test
    "a valid-UTF-8 document yields only valid-UTF-8 payloads"
    Surface.wild
    (fun s ->
       List.for_all
         ~f:(fun width ->
           List.for_all
             ~f:is_valid_utf8
             (payloads_utf8 (fst (Handsome.Utf8.render ~width (Du.to_doc s)))))
         Surface.widths)
;;

(* MUTATIONS:
     text-dropped-past-the-ruler  [step], the [Text] case:
                                    emit_text st w s
                                  ~>
                                    if W.compare st.column st.ruler <= 0 then
                                      emit_text st w s
                                  -- truncating a line at the ruler. It is the
                                  one mutation that lets the measure decide
                                  which bytes are printed. These properties
                                  are what pin that down.
     nest-reads-the-column        [step], the [Nest] case:
                                    st.indent <- st.indent + r.j
                                  ~>
                                    st.indent <- spaces_for st.column + r.j
                                  -- [nest] behaving like [align], opening a
                                  second route from the measure to the
                                  indentation.
     stream-width-zeroed          [emit_text]: O_text (w, s)  ~>  O_text (W.zero, s)
     text-truncated               [step], the [Text] case: emit half of every
                                  text node, which cuts multi-byte characters in
                                  half.

   [text-truncated] leaves "the measure changes whitespace and no other byte"
   green, truncating by the same rule under both instances so that the two print
   the same bytes. The property speaks about the measure, and other suites cover
   the printer's correctness. *)

(* -- instances whose space is not one column wide -----------------------------

   [Ascii_width] and [Utf8_width] both measure a space at 1, which makes
   [spaces_for] the identity: the largest [n] with [n <= c] is [c], however the
   search is written. Two things go untested as a result.

   [spaces_for]'s zero-space guard is [W.compare iw_cache.(1) iw_cache.(0) <= 0],
   which is [1 <= 0] under either shipped instance. No test in the suite has ever
   taken that branch. And [indent_width]'s bit fold, which covers indentations
   past the 64 the table holds, only ever has to agree with the identity.

   These two instances close both. Each is a legal [Width.S]: additive over
   concatenation, [zero] on the empty string, never below [zero], and ordered by
   a total order.
   -------------------------------------------------------------------------- *)

module Wide_width = struct
  type t = int

  let zero = 0
  let add = ( + )
  let compare = Int.compare

  (* Every byte two columns, so a space is two and [spaces_for] has to halve
     the column to answer. *)
  let measure s = 2 * String.length s
end

module Zero_space_width = struct
  type t = int

  let zero = 0
  let add = ( + )
  let compare = Int.compare

  (* A space costs nothing, so every count of spaces fits every column, and
     [spaces_for] is documented to answer 0 straight away. *)
  let measure s =
    let n = ref 0 in
    String.iter ~f:(fun c -> if not (Char.equal c ' ') then incr n) s;
    !n
  ;;
end

(* Held to the same terms as the shipped two. The file's premise is that the
   obligations are properties of the signature and a new instance can be dropped
   in under them; an instance used as a fixture and exempted from them would make
   the cases below tests against an instance that might not be legal, and a
   failure there would say nothing. The zero-space corpus is mostly spaces, since
   that is the character whose width it makes unusual. *)
module O_wide =
  Obligations
    (Wide_width)
    (struct
      let name = "wide"
      let gen = gen_bytes
    end)

module O_zero_space =
  Obligations
    (Zero_space_width)
    (struct
      let name = "zero-space"

      let gen =
        Gen.string_size
          (Gen.int_range 0 12)
          ~gen:(Gen.oneof (List.map ~f:Gen.return [ ' '; ' '; 'a'; 'z' ]))
      ;;
    end)

module Wide = Handsome.Make (Wide_width)
module Zero = Handsome.Make (Zero_space_width)

let exotic_cases =
  [ ( "a space of two columns halves the alignment"
    , `Quick
    , fun () ->
        (* [text "abcdef"] leaves the column at 12, and six spaces are exactly 12
           columns: the largest count whose width falls within the column, and
           [align] is defined to
           set exactly that. Under a one-column space this
           document also indents by six -- but only because six is the column
           too. This instance tells the two apart. *)
        let d = Wide.(text "abcdef" ^^ align (hardline ^^ text "x")) in
        Alcotest.(check string)
          "six spaces, twelve columns"
          "abcdef\n      x"
          (Wide.to_string (fst (Wide.render ~width:100 d))) )
  ; ( "a space of no columns aligns to zero"
    , `Quick
    , fun () ->
        (* The guard no shipped instance reaches. Every count of spaces fits, so
           there is no largest one, and [spaces_for] answers 0. The search
           below it would be looking for a bound that does not exist. *)
        let d = Zero.(text "abcdef" ^^ align (hardline ^^ text "x")) in
        Alcotest.(check string)
          "no indentation"
          "abcdef\nx"
          (Zero.to_string (fst (Zero.render ~width:100 d))) )
  ; ( "lines agrees with the bytes past the indentation table"
    , `Quick
    , fun () ->
        (* [indent_width] reads a 65-entry table below 65 and folds over the set
           bits of [n] above it. Under a one-column space the fold only has to
           agree with the identity; here it has to double. Every level from 0 to
           400 crosses the boundary, so both paths are covered, and both are
           checked against the rendered bytes. *)
        for j = 0 to 400 do
          let d = Wide.(nest j (text "a" ^^ hardline ^^ text "b")) in
          let stream, _ = Wide.render ~width:max_int d in
          let bytes = Wide.to_string stream in
          let expect =
            Array.of_list
              (List.map
                 ~f:(fun l -> 2 * String.length l)
                 (String.split_on_char ~sep:'\n' bytes))
          in
          if Wide.lines stream <> expect
          then Alcotest.failf "nest %d: lines disagrees with the rendered bytes" j
        done )
  ]
;;

(* MUTATIONS:
     spaces-for-zero-space-guard  [spaces_for]: the zero-space guard weakened
                                  from [<= 0] to [< 0], so a zero-width space
                                  falls into a search that no count of spaces can
                                  bound. Invisible to both shipped instances.
     spaces-for-search-low        [spaces_for]: the search keeps the largest [n]
                                  with [indent_width n < c], where it should
                                  keep the largest with [<= c].
     iw-pow-not-doubling          [iw_pow]: each entry copied from the one below
                                  where it should double, so [indent_width]
                                  past 64 counts an indentation's set bits and
                                  gives them all the same weight.
                                  Invisible to both shipped instances, whose
                                  only test of that path checks totality. *)

let suites =
  [ ( "width-obligations"
      (* The two shipped instances first, so their ids stay where the mutation
         report names them. *)
    , O_ascii.cases @ O_utf8.cases @ O_wide.cases @ O_zero_space.cases )
  ; "width-instances", [ ascii_never_below_utf8; agree_on_ascii ]
  ; "width-utf8", utf8_cases
  ; ( "width-independence"
    , [ same_text; same_at_infinite_width; cached_width_agrees; text_nodes_stay_whole ] )
    (* Last, so that no id above it moves. *)
  ; "width-exotic", exotic_cases
  ]
;;
