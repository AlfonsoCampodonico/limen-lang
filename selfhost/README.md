# Tessera in Tessera

The start of a Tessera compiler written in Tessera. The plan is the usual
bootstrap: the OCaml compiler in `src/` builds this one, this one builds
itself, and when two rounds of that produce the same C, the compiler hosts
itself. The OCaml compiler stays as the way to build from scratch.

So far: the lexer and the parser.

- `lexer.tsr` turns source text into tokens, with the same rules, positions
  and errors as `src/lexer.ml`.
- `ast.tsr` is the syntax tree, the shape of `src/ast.ml` without the slots
  the checker fills in.
- `parser.tsr` turns the tokens into that tree, with the same structure,
  positions, errors and messages as `src/parser.ml`. `floats.tsr` writes
  Floats the way OCaml's `%h` and `%g` do, for the tree dumps and for
  messages.
- `tokens.tsr` prints a file's tokens one per line; `tokdump/` prints the
  OCaml lexer's in the same format.
- `parse.tsr` prints a file's tree (with `ast_print.tsr`): the compiler's
  position-free format from `src/astdump.ml`, then every span in the tree,
  or the error. `astdump/` prints the OCaml parser's in the same format.
- `check.sh` builds both programs and compares the lexers and the parsers on
  every `.tsr` file in the repository, the parsers also on the edge cases in
  `tests/` (constructs that are easy to get wrong, and one file for each
  error the parser can report) and on the book's examples when
  `../tessera-book` (or `$TESSERA_BOOK`) is there:

```sh
sh selfhost/check.sh
```

Next: the checker, lowering and C generation, compared the same way
against the OCaml compiler at each step.
