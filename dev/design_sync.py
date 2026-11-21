#!/usr/bin/env python3
"""Keep src/design in step with the Limen site's design system.

limen-book is the source of truth: tools/tokens.css (the tokens), tools/home.css
(the components) and assets/ (the mark and the fonts). This copies what the
compiler's outputs need into src/design:

- limen.css: the tokens without their @font-face rules (generated pages use the
  system stacks the tokens already list), the components named in SECTIONS, then
  the hand-written part for Limen's own outputs, which is kept as it is;
- logo.svg and fonts/*.woff2 with their licences, for `limen new --web`.

    python3 dev/design_sync.py [LIMEN_BOOK]          rewrite src/design
    python3 dev/design_sync.py --check [LIMEN_BOOK]  fail if src/design is out of step
"""
import os
import re
import sys

# home.css sections (by the comment that opens them) the outputs use
SECTIONS = ["base", "nav", "buttons", "hero", "copy box: a command and a copy button", "benchmark table", "sections", "windows: code and terminal",
            "features", "footer", "the logo tile keeps its shape on dark pages"]
FILES = ["logo.svg", "fonts/Inter.woff2", "fonts/Inter-LICENSE.txt", "fonts/JetBrainsMono-Regular.woff2",
         "fonts/JetBrainsMono-Medium.woff2", "fonts/JetBrainsMono-LICENSE.txt"]
END = "/* ==== end of the part copied from limen-book: edit below this line ==== */\n"
HEADER = """/* Limen's shared stylesheet: the design of limen-lang.org, used by what the
   compiler writes (`limen doc --html`, `limen new --web`) and, trimmed, by the
   standard library's error pages (std/http.lmn).

   Everything above the END line is copied from limen-book by
   dev/design_sync.py: do not edit it here. Change limen-book, then run
   `python3 dev/design_sync.py`; selfhost/check.sh fails while the two differ. */

"""


def sections(css):
    """home.css split at the comments that start a line: {first words: text}."""
    out, name, cur = {}, None, []
    for line in css.splitlines(keepends=True):
        m = re.match(r"/\* ([^*\n]*?)(?: \*/|$)", line)
        if m and not line.startswith(" "):
            if name is not None:
                out[name] = "".join(cur)
            name, cur = m.group(1).split(":")[0] if m.group(1).startswith("features") else m.group(1), []
        cur.append(line)
    if name is not None:
        out[name] = "".join(cur)
    return out


def tokens(css):
    """tokens.css without the @font-face rules and the comment above them."""
    css = re.sub(r"/\* The fonts are served by this site.*?\*/\n", "", css, flags=re.S)
    css = re.sub(r"@font-face \{[^}]*\}\n", "", css)
    return re.sub(r"\n{3,}", "\n\n", css)


def generate(book, own):
    home = sections(read(os.path.join(book, "tools/home.css")))
    missing = [s for s in SECTIONS if s not in home]
    if missing:
        sys.exit("design_sync: home.css has no section " + ", ".join(missing))
    parts = [HEADER, "/* ---- tokens: limen-book tools/tokens.css ---- */\n\n",
             tokens(read(os.path.join(book, "tools/tokens.css"))).rstrip("\n") + "\n\n",
             "/* ---- components: limen-book tools/home.css ---- */\n\n"]
    parts += [home[s].rstrip("\n") + "\n\n" for s in SECTIONS]
    return "".join(parts) + END + own


def read(path, mode="r"):
    with open(path, mode) as f:
        return f.read()


def main(argv):
    check = "--check" in argv
    args = [a for a in argv if a != "--check"]
    book = args[0] if args else os.environ.get("LIMEN_BOOK", os.path.join(os.path.dirname(__file__), "..", "..", "limen-book"))
    design = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "src", "design")
    css_path = os.path.join(design, "limen.css")
    current = read(css_path) if os.path.exists(css_path) else ""
    own = current.split(END, 1)[1] if END in current else ""
    want = {"limen.css": generate(book, own).encode()}
    for f in FILES:
        want[f] = read(os.path.join(book, "assets", f), "rb")
    stale = [f for f, data in want.items()
             if not os.path.exists(os.path.join(design, f)) or read(os.path.join(design, f), "rb") != data]
    if check:
        if stale:
            sys.exit("src/design is out of step with limen-book (" + ", ".join(stale) +
                     "): run python3 dev/design_sync.py")
        return
    for f in stale:
        path = os.path.join(design, f)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "wb") as out:
            out.write(want[f])
        print("wrote src/design/" + f)


if __name__ == "__main__":
    main(sys.argv[1:])
