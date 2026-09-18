(lang dune 3.20)

(name handsome)

(generate_opam_files true)

(source
 (github enetsee/handsome))

(authors "Michael Thomas <mthomas180@gmail.com>")

(maintainers "Michael Thomas <mthomas180@gmail.com>")

(license MIT)

(documentation https://enetsee.github.io/handsome/)

(package
 (name handsome)

 (synopsis "A pretty-printer that tells you what it decided")
 (description
  "Wadler-Leijen pretty-printing with three things the usual libraries leave
out. Documents carry annotations over a whole region, and those
annotations survive into the output, so one renderer serves plain text, HTML
with spans, and terminal colour. Rendering produces a stream of text, line
breaks and annotation markers, so a caller folds it into
whatever it needed. And the renderer reports which elective breaks it resolved
flat: when a group fits on one line the breaks inside it come out as a space or
as nothing at all, so the output keeps no sign of them. The record gives their
positions, so a caller can check that every byte printed on a line was counted
by the decision that put it there. check reports a newline inside a text node
as an error, because it makes the width model lie. Width is a functor
parameter with a documented obligation -- measure must never under-report
display width -- and two instances: bytes, and terminal columns for UTF-8 from
tables generated out of the Unicode Character Database. No dependencies.")
 (depends
  (ocaml
   (>= 4.14))
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)
  (pprint :with-test))
 (tags
  ("pretty-printer" "formatting" "layout" "wadler" "annotations")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
