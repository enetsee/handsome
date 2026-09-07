#!/usr/bin/env python3
"""Generate lib/unicode_tables.ml from the Unicode Character Database.

The tables are the two facts Utf8_width needs about a code point:

  wide  East_Asian_Width is Wide or Fullwidth      -> two columns
  zero  General_Category is Mn, Me or Cf           -> no columns

Both come out as a few hundred inclusive ranges, because the code points
concerned sit in contiguous blocks. Emitted as flat int arrays, they are a few
KB of source and no dependency: the library decodes UTF-8 with
String.get_utf_8_uchar, which has been in the stdlib since 4.14.

Regenerating against a new Unicode release:

    python3 tools/gen_unicode_tables.py --version 16.0.0 -o lib/unicode_tables.ml
    dune build @all && dune test

or, offline, with EastAsianWidth.txt and UnicodeData.txt in a directory:

    python3 tools/gen_unicode_tables.py --ucd path/to/ucd -o lib/unicode_tables.ml

`dune build @unicode` checks the checked-in tables against the release they
name, by running this script with --check. It is its own alias, held out of the
default build because it reads the UCD over the network.

A change to the checked-in tables comes from regenerating them; the pinning
tests in test/test_width.ml show what moved. They are listed in
.ocamlformat-ignore, so that a regeneration reproduces this script's output
exactly.
"""

import argparse, difflib, io, os, sys, urllib.request

UCD_URL = "https://www.unicode.org/Public/%s/ucd/%s"

# EastAsianWidth.txt assigns "N" to every code point it leaves unlisted, and its
# header carves out five ranges whose *unassigned* code points default to "W".
# Those are included here: omitting them would give an unassigned CJK code point
# one column against two drawn by a terminal, which is an under-report. Quoting
# the file:
#
#   - The unassigned code points in the following blocks default to "W":
#          CJK Unified Ideographs Extension A: U+3400..U+4DBF
#          CJK Unified Ideographs:             U+4E00..U+9FFF
#          CJK Compatibility Ideographs:       U+F900..U+FAFF
#   - All undesignated code points in Planes 2 and 3, whether inside or
#       outside of allocated blocks, default to "W":
#          Plane 2:                            U+20000..U+2FFFD
#          Plane 3:                            U+30000..U+3FFFD
DEFAULT_WIDE = [
    (0x3400, 0x4DBF),
    (0x4E00, 0x9FFF),
    (0xF900, 0xFAFF),
    (0x20000, 0x2FFFD),
    (0x30000, 0x3FFFD),
]

ZERO_CATEGORIES = {"Mn", "Me", "Cf"}


def fetch(args, name):
    if args.ucd:
        with open(os.path.join(args.ucd, name), encoding="utf-8") as f:
            return f.read()
    url = UCD_URL % (args.version, name)
    print("fetching %s" % url, file=sys.stderr)
    with urllib.request.urlopen(url, timeout=120) as r:
        return r.read().decode("utf-8")


def strip(line):
    return line.split("#", 1)[0].strip()


def east_asian_wide(text):
    """Inclusive ranges whose East_Asian_Width is W or F."""
    out = list(DEFAULT_WIDE)
    for line in text.splitlines():
        line = strip(line)
        if not line:
            continue
        field, value = (p.strip() for p in line.split(";")[:2])
        if value not in ("W", "F"):
            continue
        if ".." in field:
            lo, hi = (int(p, 16) for p in field.split(".."))
        else:
            lo = hi = int(field, 16)
        out.append((lo, hi))
    return out


def zero_width(text):
    """Inclusive ranges whose General_Category is Mn, Me or Cf.

    UnicodeData.txt spells large blocks as a First/Last pair of lines, so those
    have to be stitched back together.
    """
    out = []
    pending = None
    for line in text.splitlines():
        fields = line.split(";")
        if len(fields) < 3:
            continue
        cp, name, gc = int(fields[0], 16), fields[1], fields[2]
        if name.endswith(", First>"):
            pending = (cp, gc)
            continue
        if name.endswith(", Last>"):
            if pending is None or pending[1] != gc:
                sys.exit("gen_unicode_tables: unpaired range at U+%04X" % cp)
            if gc in ZERO_CATEGORIES:
                out.append((pending[0], cp))
            pending = None
            continue
        if gc in ZERO_CATEGORIES:
            out.append((cp, cp))
    return out


def merge(ranges):
    """Sort and coalesce, so membership is one binary search over disjoint,
    ascending ranges."""
    out = []
    for lo, hi in sorted(ranges):
        if out and lo <= out[-1][1] + 1:
            out[-1] = (out[-1][0], max(out[-1][1], hi))
        else:
            out.append((lo, hi))
    return out


def emit(name, doc, ranges, w):
    w("(* %s\n\n   %d ranges. *)\n" % (doc, len(ranges)))
    w("let %s =\n  [|" % name)
    for i, (lo, hi) in enumerate(ranges):
        w(("\n    " if i % 4 == 0 else " ") + "0x%04X; 0x%04X;" % (lo, hi))
    w("\n  |]\n\n")


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--version", default="16.0.0")
    p.add_argument("--ucd", help="directory holding the UCD .txt files")
    p.add_argument("-o", "--output", default="-")
    p.add_argument("--check", metavar="FILE",
                   help="compare FILE against freshly generated tables and exit "
                        "non-zero if they differ, writing nothing")
    args = p.parse_args()

    wide = merge(east_asian_wide(fetch(args, "EastAsianWidth.txt")))
    zero = merge(zero_width(fetch(args, "UnicodeData.txt")))

    buf = io.StringIO()
    w = buf.write
    w("(* GENERATED from Unicode %s by tools/gen_unicode_tables.py.\n"
      "   Changes to this file come from regenerating it; that script carries\n"
      "   the command. *)\n\nopen StdLabels\n\n" % args.version)
    w("let unicode_version = \"%s\"\n\n" % args.version)
    emit("wide",
         "Code points two columns wide: East_Asian_Width Wide or Fullwidth,\n"
         "   plus the ranges whose unassigned code points default to Wide.",
         wide, w)
    emit("zero",
         "Code points that occupy no columns: General_Category Mn, Me or Cf.",
         zero, w)
    w("""(* Flattened inclusive ranges, ascending and disjoint, so membership is a
   binary search: [t.(2*i)] and [t.(2*i+1)] are the bounds of range [i]. *)
let mem (t : int array) (u : int) =
  let lo = ref 0 and hi = ref ((Array.length t / 2) - 1) in
  let found = ref false in
  while (not !found) && !lo <= !hi do
    let m = (!lo + !hi) / 2 in
    if u < t.(2 * m) then hi := m - 1
    else if u > t.((2 * m) + 1) then lo := m + 1
    else found := true
  done;
  !found
""")
    generated = buf.getvalue()
    print("wide: %d ranges, zero: %d ranges" % (len(wide), len(zero)),
          file=sys.stderr)

    if args.check:
        current = open(args.check).read()
        if current == generated:
            print("lib/unicode_tables.ml is up to date with Unicode %s"
                  % args.version, file=sys.stderr)
            return
        sys.stderr.writelines(
            difflib.unified_diff(
                current.splitlines(keepends=True),
                generated.splitlines(keepends=True),
                fromfile="lib/unicode_tables.ml", tofile="generated", n=1))
        sys.exit(
            "\nlib/unicode_tables.ml differs from what this script produces for "
            "Unicode %s.\nRegenerate it from the project root:\n"
            "    python3 tools/gen_unicode_tables.py --version %s "
            "-o lib/unicode_tables.ml\n" % (args.version, args.version))

    if args.output == "-":
        sys.stdout.write(generated)
    else:
        with open(args.output, "w") as f:
            f.write(generated)


main()
