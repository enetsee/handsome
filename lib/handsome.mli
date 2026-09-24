(** Wadler–Leijen pretty-printing.

    - {!module-type-S.flat_alt} is the primitive conditional.
      {!module-type-S.line}, {!module-type-S.softline} and
      {!module-type-S.blank} are defined in terms of it.
    - {!module-type-S.framed} makes a group and hands its body a conditional
      that follows it from any depth. A trailing separator is the case it is
      for: the whole list decides whether it appears, and the last element's
      line prints it.
    - {!module-type-S.annotate} applies to a region, and annotations appear in
      the rendered {!module-type-S.stream}. Plain text, HTML and terminal colour
      are folds over that stream.
    - {!module-type-S.render} also returns the elective breaks it resolved flat.
      Those leave no trace in the output, so the record is the only account of
      them.
    - {!module-type-S.check} rejects a newline inside a text node. The enclosing
      group measures such a document as though it could be laid out flat, and
      indentation applies only to breaks the engine emits.
    - Width is a functor parameter. {!Ascii_width} and {!Utf8_width} are the
      instances supplied.
    - Document depth costs heap and leaves the stack flat. No traversal here
      recurses on the structure of a document, so the heap bounds a deep one.
      [concat] folds right, which makes a long document a deep one. *)

(** {1 Width} *)

module Width : sig
  module type S = sig
    type t

    val zero : t
    val add : t -> t -> t
    val compare : t -> t -> int

    (** [measure s] is the width of [s] laid out on one line. In a document that
      [check] accepts, [s] is free of newlines.

      [measure] must report at least the display width. A group is laid out flat
      when its measured width added to the current column falls within the
      ruler, so a measure below what the terminal draws yields a line past the
      ruler that the renderer records as fitting. Reporting more than the
      display width is permitted, and costs a line break earlier than the ruler
      requires.

      Counting code points reports less than the display width: it is exact for
      accented Latin and half the drawn width for East Asian text, so ["中文"]
      would measure 2 against a drawn width of 4.

      The engine requires the rest of the signature as well:

      - [measure "" = zero], and [measure s >= zero];
      - [measure (a ^ b) = add (measure a) (measure b)]. A document's width is
        the sum of its text nodes', computed at construction and held from then
        on, so an additive measure keeps the cached width correct;
      - [add] is associative and commutative, and monotone in both arguments
        under [compare]. A group made with [framed] adds up the widths of the
        conditionals inside it by frame, out of document order;
      - [compare] is a total order. *)
    val measure : string -> t
  end
end

(** Width in bytes. Exact for ASCII.

    Every UTF-8 encoding is at least as long in bytes as it is wide in columns,
    so byte length reports at least the display width for any input, including
    input outside UTF-8. It reports more for non-ASCII text: ["中"] measures 3
    against a drawn width of 2, and ["é"] measures 2 against 1, so lines of such
    text break earlier than the ruler requires.

    For text that may be non-ASCII, use {!Utf8_width}. *)
module Ascii_width : Width.S with type t = int

(** Width in terminal columns, for UTF-8 encoded text.

    Decodes with [String.get_utf_8_uchar] and sums over code points:

    - East_Asian_Width Wide or Fullwidth: 2
    - General_Category Mn, Me or Cf: 0
    - everything else: 1

    A byte outside valid UTF-8 decodes to U+FFFD and measures 1, so [measure]
    accepts every string.

    The two categories overlap: the CJK tone marks at U+302A are both Mn and
    Wide. Zero takes precedence, since a combining mark is drawn over the
    character it follows.

    Additivity holds over well-formed UTF-8. Splitting a multi-byte sequence
    across two strings and measuring each separately gives a different result
    from measuring the whole; a text node is measured and rendered as a unit, so
    the engine keeps sequences intact.

    Measurement is per code point. A sequence joined by U+200D ZERO WIDTH JOINER
    is measured as its components: the family emoji U+1F468 U+200D U+1F469 U+200D
    U+1F467 measures 6, against 2 drawn by a terminal implementing the emoji
    recommendations. Reaching 2 requires UAX #29 grapheme clustering, and
    terminals differ over the answer. The direction of the error is
    over-reporting, which the signature permits.

    The tables are generated from the Unicode Character Database by
    [tools/gen_unicode_tables.py]. {!unicode_version} gives the release. *)
module Utf8_width : Width.S with type t = int

(** The Unicode release {!Utf8_width}'s tables were generated from. *)
val unicode_version : string

(** {1 Documents} *)

(** How a group decides whether to lay out flat. Under either rule it compares
    a width against the ruler, from the column the group starts at.

    - [Content] measures the group's own content laid out flat. It is the rule
      of Wadler's printer and of PPrint, and the default. What follows the group
      on the same line takes no part in the decision, so a group can fit where
      the line it ends up on does not.
    - [Line] adds what follows the group up to the next break, as Lindig's
      strict printer does. That is measured as it will print: a conditional at
      its broken branch, since a group only decides while the groups around it
      are broken, and a group further along at its flat width, which it then
      takes.

    {[
      group (text "ab" ^^ line ^^ text "cd") ^^ text "efgh"
      (* at width 6, under Content:  "ab cdefgh"  *)
      (* at width 6, under Line:     "ab\ncdefgh" *)
    ]}

    Under [Line], every line holding a break the engine resolved flat stays
    within the ruler; see {!module-type-S.resolutions}. Rendering is linear in
    the document under both. *)
type fit =
  | Content
  | Line

module type S = sig
  type width

  (** The rule a group decides by; see {!Handsome.fit}. *)
  type nonrec fit = fit =
    | Content
    | Line

  (** A document whose annotations have type ['a]. *)
  type 'a t

  (** A defect found by {!check}. *)
  type error =
    | Newline_in_text of
        { text : string (** the offending text node *)
        ; index : int (** byte offset of its first newline *)
        }
    | Alt_outside_frame of int
    (** A conditional from {!framed} used outside the body it was handed to.
        The number is the one {!pp} prints for its frame. *)

  (** {2 Atoms and concatenation} *)

  (** [text s] is [s] laid out on one line. A newline in [s] is an error;
      {!check} reports it and {!render} emits it verbatim. *)
  val text : string -> 'a t

  val empty : 'a t
  val ( ^^ ) : 'a t -> 'a t -> 'a t
  val concat : 'a t list -> 'a t

  (** {2 Line breaks}

      [flat_alt] is the primitive. The three non-trivial points of the {i flat} ×
      {i broken} square are named:

      {v
                        broken = hardline   broken = empty
      flat = text " "   line                blank
      flat = empty      softline            empty
      v} *)

  (** [flat_alt a b] is [a] when the enclosing group is flat and [b] when it is
      broken. At the top level a document is broken, so it is [b] there.

      The engine chooses between the branches, so the caller must accept either:
      the two should be semantically equivalent.

      [a] alone fixes the measured width, so a [hardline] in [a] leaves every
      enclosing group broken. *)
  val flat_alt : 'a t -> 'a t -> 'a t

  (** [flat_alt (text " ") hardline] — a space when flat, a newline when broken.
  *)
  val line : 'a t

  (** [flat_alt empty hardline] — nothing when flat, a newline when broken. *)
  val softline : 'a t

  (** An unconditional newline. A group containing one is laid out broken. *)
  val hardline : 'a t

  (** [flat_alt (text " ") empty] — a space when flat, nothing when broken. A
      separator that clears itself when the group breaks: in
      [group (a ^^ blank ^^ softline ^^ b)] the space is present on one line and
      absent on two.

      This covers the case where the break follows the separator inside the same
      group. A flat group ending in a space, with the break arriving from
      outside it, leaves that space at the end of the line:

      {[
        group (text "a" ^^ line) ^^ hardline ^^ text "b"    (* "a \nb" *)
        group (text "a" ^^ line ^^ text "b")                (* "a b" / "a\nb" *)
      ]}

      [blank] behaves the same way there, the group being flat. Moving the break
      inside the group, as on the second line, puts the separator and the break
      under one decision. *)
  val blank : 'a t

  (** {2 Grouping and indentation} *)

  (** [group d] lays [d] out flat if it fits in the ruler from the current
      column, and broken otherwise. The decision is made by measuring [d] before
      rendering it. *)
  val group : 'a t -> 'a t

  (** [nest j d] renders [d] with the indentation level increased by [j], which
      is the number of spaces emitted after every line break inside [d]. [j] may
      be negative; the emitted indentation is clamped at zero. *)
  val nest : int -> 'a t -> 'a t

  (** [align d] sets the indentation level to the current column, so [d] is
      rendered in a box whose top-left corner is where the printer stands.

      Indentation is a count of spaces and the column is a {!type-width}, so the
      level becomes the largest number of spaces whose width falls within the
      column. Under {!Ascii_width} that is the byte count; under {!Utf8_width} it
      is the display column. *)
  val align : 'a t -> 'a t

  (** {2 Conditionals on an outer group}

      {!flat_alt} resolves against the group directly enclosing it. Some choices
      belong to a group further out. Whether a list prints a trailing separator
      depends on whether the whole list broke, and the separator is printed on
      the last element's line, which may be a group of its own.

      {!framed} makes a group and hands its body a conditional that follows
      it, so the separator stays where it is printed and follows the list's
      decision:

      {[
        framed (fun alt ->
          text "["
          ^^ nest 2 (softline ^^ elements ^^ alt empty (text ","))
          ^^ softline
          ^^ text "]")
      ]} *)

  (** [framed body] is [body alt] laid out as {!group} lays it out, where
      [alt a b] is [a] if this group was laid out flat and [b] if it broke.
      [alt] can be passed to the code that builds part of the body, and used at
      any depth inside it, including inside other groups and other [framed]
      groups.

      Used outside the body it was handed to, [alt a b] is [b], and {!check}
      reports it. A body that cannot lie flat, because a flat layout would reach
      a hardline, is laid out broken, as under {!group}, so every [alt] in it
      takes its broken branch. {!reannotate} and {!unannotate} keep each
      conditional following its frame.

      The body is built inside the callback, so frames nested [n] deep are built
      by [n] nested calls.

      Measurement is exact but for one case, below. A group decides whether it
      fits only while every group around it is broken, so a group between the
      conditional and its frame measures [alt a b] at [b], and the frame and
      every group around it measure it at [a]. Each measures what it would
      print:

      {[
        framed (fun alt -> text "abc" ^^ alt empty (text ","))
        (* measures 3, and prints "abc" when flat *)
      ]}

      A hardline follows the same rule. One in [b] leaves broken the groups
      between the conditional and its frame, since those decide only once the
      frame has broken, and one in [a] leaves the frame and every group around
      it broken. So [alt empty hardline] is a newline where the frame broke and
      nothing where it was flat.

      The one case measured conservatively is a conditional inside a branch of
      another frame's conditional, whose own frame lies outside that branch. It
      counts at the wider of its two branches, and a hardline in either counts.
      Whether it is printed then turns on two frames at once. Measuring that
      exactly within the costs below would take a much heavier structure than
      the rest of the design needs, for a case that seldom arises.

      Building a document of [n] nodes, counted as a tree, costs
      [O(n log^2 n)] at most, and a frame [O(log k)] for [k] frames whose
      conditionals pass through it. Rendering and {!check} look each frame and
      conditional up in the set of frames around it, at [O(log k)] each. *)
  val framed : (('a t -> 'a t -> 'a t) -> 'a t) -> 'a t

  (** {2 Annotations}

      An annotation applies to a region, and is transparent to measurement:
      layout is decided as though it were absent. Formatting applied to an
      annotated region afterwards, which changes its rendered width, falls
      outside what the layout accounted for. *)

  val annotate : 'a -> 'a t -> 'a t
  val reannotate : ('a -> 'b) -> 'a t -> 'b t

  (** Removes the annotation regions. The layout is unchanged. *)
  val unannotate : 'a t -> unit t

  (** {2 Checking and printing} *)

  (** [check d] is [Ok ()] when every text node in [d] is free of newlines and
      every conditional from {!framed} lies inside its frame, and [Error es]
      otherwise, with one entry per offending node in document order.

      A newline the engine emits carries the current indentation and leaves the
      column at it. One inside a text node leaves the column wrong for
      everything after it, and the enclosing group measures the document as
      though it could be laid out flat.

      A conditional outside its frame takes its broken branch, which is well
      defined and seldom what was meant. Check the finished document: a part
      built inside a frame's body and checked on its own reports its
      conditionals outside. *)
  val check : 'a t -> (unit, error list) result

  (** Prints the structure of a document as an s-expression. Annotation payloads
      are outside what a generic printer can render, so [annotate a d] prints as
      [(annotate d)].

      [framed] prints as [(frame n d)] and each of its conditionals as
      [(frame-alt n a b)], where [n] numbers the frames from 0 in the order the
      printout first meets them. The same document prints the same way however
      many frames the process made before it. *)
  val pp : Format.formatter -> 'a t -> unit

  val pp_error : Format.formatter -> error -> unit

  (** {2 Rendering} *)

  type 'a stream =
    | S_empty
    | S_text of width * string * 'a stream (** measured width, bytes *)
    | S_line of int * 'a stream (** newline, then this many spaces *)
    | S_ann_push of 'a * 'a stream
    | S_ann_pop of 'a stream
    (** The rendered output, as a fold source. Plain text, HTML and terminal
            colour are folds over it.

            [S_line] carries the indentation emitted. Indentation is emitted
            once something follows it on the line, so a line that ends up empty
            carries [0]. A space at the end of a line comes from a text node;
            see {!blank}. *)

  (** [declined] holds one [(line, column)] entry per {!flat_alt}, and per
      conditional from {!framed}, that the renderer resolved flat, in document
      order, recorded at the position the alternative stood at.

      A [flat_alt] resolved flat leaves no mark in the output, so this record is
      the only account of it. It distinguishes a line over the ruler that the
      printer had an opportunity to break from one where every break was taken.

      A caller can use it to check its own documents:

      {[
        for every (line, col) in r.declined:  (lines s).(line) <= width
      ]}

      Under {!Line} that check holds for every document without a newline in a
      text node, because each group measures its line up to the next break.

      Under {!Content} it holds where the caller accounts for everything it
      places on a line. The engine's guarantee covers the flat region it
      committed to; content appended to the same line afterwards falls outside
      it.

      {[
        group (text "ab" ^^ line ^^ text "cd") ^^ text "eeeeeeeeee"
        (* at width 10, under Content:  "ab cdeeeeeeeeee",   declined [(0, 2)] *)
        (* at width 10, under Line:     "ab\ncdeeeeeeeeee", declined []       *)
      ]}

      A failure under [Content] means the caller placed bytes on a line that
      some fit decision omitted. The engine's guarantee under either rule
      includes the local one: every declined break sits at a column within the
      ruler. *)
  type resolutions = { declined : (int * width) list }

  (** Returns for every document, including one {!check} rejects. [fit] is the
      rule each group decides by, {!Content} unless given. *)
  val render : ?fit:fit -> width:width -> 'a t -> 'a stream * resolutions

  (** The rendered bytes. Requires each line's indentation to fit within
      [Sys.max_string_length], and raises where it exceeds that; {!render} and
      {!lines} accept those documents. *)
  val to_string : 'a stream -> string

  (** The measured width of each line; its length is one more than the number of
      [S_line] nodes. *)
  val lines : 'a stream -> width array
end

module Make (W : Width.S) : S with type width = W.t

(** [Make (Ascii_width)], pre-applied. *)
module Ascii : S with type width = int

(** [Make (Utf8_width)], pre-applied. *)
module Utf8 : S with type width = int
