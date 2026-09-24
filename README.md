# handsome

[![CI](https://github.com/enetsee/handsome/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/enetsee/handsome/actions/workflows/ci.yml)
[![Docs](https://github.com/enetsee/handsome/actions/workflows/docs.yml/badge.svg?branch=main)](https://github.com/enetsee/handsome/actions/workflows/docs.yml)

A pretty-printer that tells you what it decided.

Wadler–Leijen layout for OCaml, with three additions.

**Annotations cover a region, and survive rendering.** `annotate` takes a whole
sub-document, and the annotation comes back in the rendered output. One document
then serves plain text, HTML with spans, and terminal colour: each of those is a
fold over the same stream.

**Rendering produces a stream.**

```ocaml
type 'a stream =
  | S_empty
  | S_text     of int * string * 'a stream   (* width, bytes *)
  | S_line     of int * 'a stream            (* indentation *)
  | S_ann_push of 'a * 'a stream
  | S_ann_pop  of 'a stream
```

**The renderer reports the breaks it decided to skip.** When a group fits on one
line, the optional breaks inside it are laid out flat: a `line` becomes a space,
a `softline` disappears altogether. The output keeps no sign that a break was
available there. `render` hands back the line and column of each one, so you can
see where the printer had a choice and which way it went.

This has two consequences. 

1) You can tell a long line caused by a bad choice from one
caused by a run of text with nowhere to break in it. And,
2) since the answer is a return value, you can write a test over it: every byte on a line was counted by the decision that put it there.

**A group can measure the rest of its line.** By default a group decides by its
own content, as Wadler's printer and PPrint do, so
`group (text "ab" ^^ line ^^ text "cd") ^^ text "efgh"` prints `ab cdefgh` at
width 6: nine columns on a ruler of six. `render ~fit:Line` has each group
measure what follows it up to the next break as well, as Lindig's strict printer
does, and prints `ab` and `cdefgh` on two lines. Under that rule the test above
holds for every document `check` accepts: each line holding a declined break is
within the ruler.

Two smaller decisions come from the same place: 
1) the layout model has to match
what gets printed. `check` reports a newline inside a text node as an error, because a newline the engine did not emit leaves the column wrong for everything after it. And,
2) width is a functor parameter with an explicit obligation: `measure` must never
*under*-report display width. Two instances ship:

```ocaml
module Ascii : S with type width = int   (* Make (Ascii_width) — bytes *)
module Utf8  : S with type width = int   (* Make (Utf8_width)  — columns *)
```

`Ascii_width` counts bytes, which is exact for ASCII and pessimistic for
everything else. `Utf8_width` counts terminal columns. It decodes with
`String.get_utf_8_uchar` and consults East_Asian_Width and General_Category
tables generated from the Unicode Character Database by
`tools/gen_unicode_tables.py`. A byte below `0x80` skips both steps: it is a
code point below the first range of either table, so it takes one column and
the engine skips the decoder and both table searches. That makes measuring
ASCII about 16x faster, at a cost of a few percent on text with no ASCII in it.
The tables are 490 ranges of inlined source and the decoder is in the standard
library, so the package has no dependencies. `dune build @unicode` checks the
tables against the Unicode release they name; it has its own alias and stays out
of default builds, because it reads the UCD over the network.

```ocaml
val flat_alt : 'a t -> 'a t -> 'a t   (* the primitive; line and softline derive from it *)
val render   : ?fit:fit -> width:width -> 'a t -> 'a stream * resolutions
val check    : 'a t -> (unit, error list) result
```

The design is fairly unoriginal. The annotation scheme is Haskell
[`prettyprinter`](https://hackage.haskell.org/package/prettyprinter)'s, whose lineage runs Wadler → Leijen → Bolingbroke → Luposchainsky. What handsome adds is the break-resolution record, treating a newline in a text node as an error.

[API documentation](https://enetsee.github.io/handsome/handsome/Handsome/index.html)

## Install

```sh
opam install handsome
```

The development version:

```sh
opam pin add handsome https://github.com/enetsee/handsome.git
```

No dependencies beyond the standard library.

## Performance

The renderer measures first. Each node caches the width it would occupy laid out
flat, so a group's decision is a comparison against a number that is already
there. Rendering is linear in the document, including the case where every atom
sits inside its own group:

```
                                     n=10⁴    n=10⁵    n=10⁶
flat concatenation                   0.001s   0.011s   0.113s
every atom in its own group          0.002s   0.014s   0.165s
n nested groups, one indent each     0.001s   0.012s   0.142s
```

(`dune exec bench/bench.exe`.)

A functor also raises the question of cost.
With `W` bound to a concrete structure, so the compiler can see through it,
build, render and
`to_string` come out at 0.95–1.05x. The abstraction is free, even without
flambda. It costs one word per node: an abstract `W.t` has no spare value to
stand for infinity, so a node needs a separate flag to say it has no flat
layout.

The `Line` rule costs one more word per node, under either rule: the width each
node puts on its first line when laid out broken. Documents are about an eighth larger for it, and rendering is up to a tenth slower.
