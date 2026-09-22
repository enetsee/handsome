## Unreleased

First release. Wadler–Leijen pretty-printing with three things the usual
libraries leave out:

- Annotations cover a whole *region* and survive into the rendered stream, so
  one document serves plain text, HTML with spans and terminal colour as three
  folds over the same output.
- `render` returns a stream of text, line breaks and annotation markers, plus
  the elective breaks it resolved flat. A break resolved flat leaves no sign in
  the bytes, so the record is the only way to tell a long line caused by a poor
  choice from
  one caused by text with nowhere to break in it.
- `check` reports a newline inside a text node as an error, because it makes the
  column model lie for everything after it.

Also:

- `flat_alt` is the primitive; `line`, `softline` and `blank` derive from it.
- `framed`: a group whose body is handed a conditional that follows it from any
  depth. `flat_alt` follows the group directly around it, which is wrong for a
  trailing separator: the whole list decides whether it appears, and the last
  element's line prints it. In `framed (fun alt -> ...)`, `alt a b` is `a` where
  the frame was laid out flat and `b` where it broke. Measurement stays exact,
  so a list with a trailing comma fits at exactly the width it prints at; the
  one exception, a conditional inside another frame's conditional, is measured
  at its wider branch. `check` reports a conditional used outside its frame.
- `add` must be commutative as well as associative, since a frame adds up its
  conditionals' widths out of document order.
- Width is a functor parameter with a documented obligation — `measure` must
  never *under*-report display width — and two instances: `Ascii_width`
  (bytes) and `Utf8_width` (terminal columns, from tables generated out of the
  Unicode Character Database, no dependency).
- Measure-first grouping, so rendering is linear in the document even when
  every atom sits inside its own group.
- Every traversal iterative, so document depth costs heap and leaves the stack
  flat. `render`, `check`, `reannotate`, `unannotate` and `pp` all hold at
  document sizes that overflow an 8 MB stack under structural recursion.
