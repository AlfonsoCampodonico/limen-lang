# Tessera in Tessera

The start of a Tessera compiler written in Tessera. The plan is the usual
bootstrap: the OCaml compiler in `src/` builds this one, this one builds
itself, and when two rounds of that produce the same C, the compiler hosts
itself. The OCaml compiler stays as the way to build from scratch.

So far: the lexer, the parser and the module loader.

- `lexer.tsr` turns source text into tokens, with the same rules, positions
  and errors as `src/lexer.ml`.
- `ast.tsr` is the syntax tree, the shape of `src/ast.ml` without the slots
  the checker fills in. A Tessera value cannot be changed from elsewhere,
  so the checker will keep what it learns about an expression (its type,
  the type arguments of a generic function's use, which function a call
  reaches) in maps keyed by the expression's `id`, and every span names its
  file by number.
- `ids.tsr` numbers the expressions of a tree: the parser numbers each file
  from a first id the loader gives it, so ids are unique in a program.
- `parser.tsr` turns the tokens into that tree, with the same structure,
  positions, errors and messages as `src/parser.ml`. `floats.tsr` writes
  Floats the way OCaml's `%h` and `%g` do, for the tree dumps and for
  messages.
- `loader.tsr` is `src/modules.ml`: it finds the modules a file uses (next
  to it, in a tessera.toml dependency, in the standard library), with
  `package.tsr` reading tessera.toml, and makes one program of them.
  `rename.tsr` gives every item its qualified name and rewrites the
  references, expands type aliases, pipes, tuple unpacking and tuple
  assignment; `embeds.tsr` reads the files `embed` names (the loader reads
  only through the Fs capabilities it is handed: one for the program, one
  for the standard library's sources); `forwarders.tsr` adds the methods
  struct embedding promotes. The diagnostics are the OCaml loader's, in
  the same order.
- `tokens.tsr` prints a file's tokens one per line; `tokdump/` prints the
  OCaml lexer's in the same format.
- `parse.tsr` prints a file's tree (with `ast_print.tsr`): the compiler's
  position-free format from `src/astdump.ml`, then every span in the tree,
  or the error. `astdump/` prints the OCaml parser's in the same format.
- `load.tsr` prints the loaded program (its items in the same format, then
  the C declarations, the embedded files and every span with its file), or
  the loader's diagnostics in order. `loaddump/` prints the OCaml loader's.
- `check.sh` builds the programs and compares the lexers, the parsers and
  the loaders on every `.tsr` file in the repository, the parsers also on
  the edge cases in `tests/` (constructs that are easy to get wrong, and
  one file for each error the parser can report), the loaders on those in
  `tests/load/` (one directory each, for each diagnostic the loader can
  report and each expansion), and both on the book's examples when
  `../tessera-book` (or `$TESSERA_BOOK`) is there:

```sh
sh selfhost/check.sh
```

The loader does not fetch git dependencies (the OCaml one runs git and
keeps tessera.lock), and it sees only what its Fs grants: a dependency or
an embedded file outside the granted directory does not exist for it.

Next: the checker, lowering and C generation, compared the same way
against the OCaml compiler at each step.
