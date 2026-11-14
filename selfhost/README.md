# Tessera in Tessera

The start of a Tessera compiler written in Tessera. The plan is the usual
bootstrap: the OCaml compiler in `src/` builds this one, this one builds
itself, and when two rounds of that produce the same C, the compiler hosts
itself. The OCaml compiler stays as the way to build from scratch.

So far: the lexer.

- `lexer.tsr` turns source text into tokens, with the same rules, positions
  and errors as `src/lexer.ml`.
- `tokens.tsr` prints a file's tokens one per line; `tokdump/` prints the
  OCaml lexer's in the same format.
- `check.sh` builds `tokens.tsr` and compares the two lexers on every `.tsr`
  file in the repository:

```sh
sh selfhost/check.sh
```

Next: the parser, then the checker, lowering and C generation, compared the
same way against the OCaml compiler at each step.
