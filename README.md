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

Two things follow. You can tell a long line caused by a bad choice from one
caused by a run of text with nowhere to break in it. And since the answer is a
return value, you can write a test over it: every byte on a line was counted by
the decision that put it there.

Two smaller decisions come from the same place: the layout model has to match
what gets printed. `check` reports a newline inside a text node as an error,
because a newline the engine did not emit leaves the column wrong for everything
after it. And width is a
functor parameter with an explicit obligation: `measure` must never
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
val render   : width:width -> 'a t -> 'a stream * resolutions
val check    : 'a t -> (unit, error list) result
```

The design is borrowed. The annotation scheme is Haskell
[`prettyprinter`](https://hackage.haskell.org/package/prettyprinter)'s, whose
lineage runs Wadler → Leijen → Bolingbroke → Luposchainsky. What handsome adds
is the break-resolution record, treating a newline in a text node as an error,
and being in OCaml.

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

## What the tests establish

- **A differential against [PPrint](https://github.com/fpottier/pprint).** Every
  construct in the library has an exact PPrint counterpart, so the generated
  documents cover all of it. `to_string (render d)` agrees with PPrint byte for
  byte over 9000 documents at ten widths each, and 3000 more at every width from
  0 to 60, with zero disagreements. The two libraries differ in one place:
  PPrint has *suppressible blanks*, which belong to its renderer, where
  handsome's blanks belong to the document. Against idiomatic PPrint (`break`)
  the two agree exactly, up to trailing whitespace.
- **A negative result.** Widening the ruler does not monotonically shorten the
  output: flattening an early group spends horizontal room that a later group
  needed. The counterexample is exhaustively minimal at nine nodes, and a test
  pins it. Two endpoints do hold: no width renders in fewer lines than an
  unbounded one, and none in more than zero.
- **The laws**, one property test each, in `test/test_laws.ml`, run under both
  width instances. That matters most for width soundness, which is the law that
  fails when a measure under-reports. Running it only under the measure that
  always over-reports would put it where it has nothing to catch.
- **The width signature's obligations** as properties over a functor, in
  `test/test_width.ml`, with four instances: the two the library ships, and two
  synthetic ones where a space measures something other than one column. One
  measures every byte at two columns; the other measures a space at zero. Under
  the shipped instances a space is one column, which makes `spaces_for` the
  identity function, so a wrong answer from it would still look right. The
  synthetic pair forces a real answer. The same file checks what separates the
  two shipped instances (`Ascii.measure s >= Utf8.measure s` on every valid
  UTF-8 string) and what both leave alone (the measure moves whitespace and
  every other byte stays put).
- **Mutation coverage.** Each of a set of named mutations goes into a clean copy
  of the library on its own, and the tests that go red are recorded. Almost
  every test is reddened by at least one. The handful that survive everything
  are listed with the reason: the `render` and `check` depth cases, because no
  small edit makes the engine recurse on the document, and the obligations on
  the two synthetic width instances, which live in the test file where a
  mutation of the library cannot reach them. Two mutations redden nothing at
  all, and those are recorded as findings.

### Two decisions worth spelling out

`blank` is `flat_alt (text " ") empty`. It fills the fourth corner of the table
the derived breaks form:

|                   | broken = `hardline` | broken = `empty` |
|-------------------|---------------------|------------------|
| flat = `text " "` | `line`              | `blank`          |
| flat = `empty`    | `softline`          | `empty`          |

It clears itself when the group breaks: in `group (a ^^ blank ^^ softline ^^ b)`
the space is there on one line and gone on two. That covers the case where the
break follows the separator inside the same group.

A flat group ending in a space, with the break arriving from outside it, leaves
the space at the end of the line. `group (text "a" ^^ line) ^^ hardline` gives
`"a \n"`. PPrint suppresses that. Its blanks belong to the renderer, where
handsome's belong to the document, and moving the break inside the group puts
both under one decision. That accounts for the "up to trailing whitespace"
qualification above.

Indentation is emitted once something lands on the line. `S_line` carries `0`
for a line that stays empty, so a line always ends in something the engine
printed. Emitting indentation eagerly would break the width soundness law, since
a blank line indented past the ruler exceeds the width while holding nothing
that could be broken. PPrint does the same.
