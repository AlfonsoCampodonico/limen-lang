# Tessera in Tessera

The start of a Tessera compiler written in Tessera. The plan is the usual
bootstrap: the OCaml compiler in `src/` builds this one, this one builds
itself, and when two rounds of that produce the same C, the compiler hosts
itself. The OCaml compiler stays as the way to build from scratch.

So far: the whole compiler for `check`, `caps` and `build`: the lexer,
the parser, the module loader, the type checker, lowering to the IR and
the passes on it, C generation, and the build (the C compiler, the build
cache). It gives the OCaml compiler's C byte for byte.

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
- The type checker is `src/check.ml` and `src/types.ml`. `types.tsr` has
  the types and unification: OCaml's type variables
  are mutable cells, so here a variable is a number and what it stands for
  is in a substitution the checker threads through everything, next to
  what it learns (`env.tsr`'s `State`: each expression's type and type
  arguments by id, the problems, and the new form of each expression it
  rewrites, `x.m()` into `T::m(x)`, named arguments into parameter order,
  method values, promoted fields, `#go`, `map_err`, and a value boxed as
  `dyn I`, which becomes `EDyn`). `typecheck.tsr` checks the declarations
  and then the bodies, and reports what each function can do (its
  capabilities, whether it runs callbacks: what `tessera caps` prints);
  `checker.tsr` the expressions, statements and patterns, with whether a
  type satisfies an interface (`satisfies`, which boxing, `?` into
  `dyn Error` and the bounds of generic calls and types all ask);
  `exhaust.tsr` whether a `match` covers every value; `ffi.tsr` what a
  foreign function's declaration means at the boundary to C (`src/ffi.ml`);
  `builtins.tsr` has the built-in methods, and `prelude.tsr` the items
  every program gets. Suggestions ("did you mean") pick between equally
  close names in the order OCaml's hash tables list them, which
  `ocamlhash.tsr` reproduces.
- `caps.tsr` is what `src/package.ml` checks after the checker: each
  dependency (and each standard library module) takes only the
  capabilities its tessera.toml line grants (E0320), and a dependency
  trusted with C calls only the libraries tessera.lock recorded (E0324).
  It reads tessera.lock through the Fs and never writes it; the OCaml
  compiler records new libraries there.
- The backend is `src/lower.ml`, `src/ir.ml`, `src/pure.ml`,
  `src/own.ml`, `src/hoist.ml` and `src/free.ml`, in the order
  `src/native.ml` runs them before C generation. `ir.tsr` is the IR:
  A-normal form, every type concrete, every variable owning its value. `lower.tsr`
  lowers the checked program (its prelude and the built-in methods it
  uses included) to it, one instance per generic function and type
  arguments, closures lifted, list pipelines fused into loops (`free.tsr`
  says which closures can be inlined). The OCaml lowering keeps a mutable
  context per function; here it is one value, threaded through, with the
  enclosing functions of a closure in a list, and a nested block is
  `open` then `close` around its statements. Types come from the
  checker's State through its final substitution and `types.zonk`.
  `pure.tsr` finds the pure functions, reuses repeated pure calls (CSE)
  and plans which heavy calls to keep across a loop; `own.tsr` places the
  retains and releases (liveness in `intset.tsr`'s sorted lists);
  `hoist.tsr` finds the loops C generation makes faster (copy-on-write
  checks made once, fill loops, versioned loops). `ffi.tsr` gives C
  generation the C signature, the results and the libraries of foreign
  functions.
- `cgen.tsr` is `src/cgen.ml`: the IR as C. The OCaml generator names
  types (`T0`, `T1`, ...), string literals, callback wrappers and C
  structs in the order it first meets them, in a mutable context; here
  the context is one value, `Cg`, threaded through, and the order is
  OCaml's order of evaluation, which takes a call's arguments from right
  to left. The loops that keep pure calls in slots are found by their
  number in the function, as `pure.plan` numbers them.
- `native.tsr` is the rest of `src/native.ml`: the TLS core pasted in
  when the program may use TLS, the C compiler's command line, and the
  build cache, keyed as the OCaml compiler keys it (`md5.tsr` is MD5), so
  the two compilers share it. What the Fs cannot do (a file's size,
  modification time and inode for the key, an executable copy, touching
  a cache entry) it asks `stat`, `readlink`, `cp`, `mv` and `touch` to do
  through the Exec; OpenSSL is found through `TESSERA_OPENSSL` or
  `pkg-config`.
- `tessera.tsr` is the compiler: `check FILE`, `caps FILE` and `build FILE
  [-o OUT] [--emit-c]` as `bin/main.ml` has them, plus `build --tests` for
  the test binary of `tessera test --native` (`cdump/` writes the OCaml
  compiler's C of it). It reads the standard library and the C runtime
  from `TESSERA_SRC` (the repository's `src/`), which the OCaml compiler
  has built in: a program can embed only files of its own package. A
  Tessera program cannot write to stderr, so what the OCaml compiler
  writes there goes to stdout. Its grants, in full:

```sh
dune build && _build/default/bin/main.exe build selfhost/tessera.tsr -o tsc
TESSERA_SRC=$PWD/src ./tsc --grant fs=/ \
  --grant env=PWD --grant env=TESSERA_SRC --grant env=CC --grant env=PATH --grant env=HOME \
  --grant env=TESSERA_CACHE --grant env=XDG_CACHE_HOME --grant env=DEVELOPER_DIR --grant env=TESSERA_OPENSSL \
  --grant env=LIBRARY_PATH --grant env=CPATH --grant env=C_INCLUDE_PATH --grant env=SDKROOT --grant env=MACOSX_DEPLOYMENT_TARGET \
  --grant exec=cc --grant exec=stat --grant exec=readlink --grant exec=cp --grant exec=mv --grant exec=touch --grant exec=pkg-config \
  -- build examples/hello.tsr -o hello
```

  The Fs must hold the program, `TESSERA_SRC`, the output and (to use
  it) the build cache; `fs=/` is the simple choice.
- `tokens.tsr` prints a file's tokens one per line; `tokdump/` prints the
  OCaml lexer's in the same format.
- `parse.tsr` prints a file's tree (with `ast_print.tsr`): the compiler's
  position-free format from `src/astdump.ml`, then every span in the tree,
  or the error. `astdump/` prints the OCaml parser's in the same format.
- `load.tsr` prints the loaded program (its items in the same format, then
  the C declarations, the embedded files and every span with its file), or
  the loader's diagnostics in order. `loaddump/` prints the OCaml loader's.
- `check.tsr` prints what the checker makes of a program: its problems as
  `tessera check` reports them (with notes and fixes, the capability
  checks included), what `tessera caps` reports of each function, then
  each item as rewritten, with every expression's type and type arguments
  in the order `walk.tsr` visits them. `checkdump/` prints the OCaml
  checker's.
- `lowered.tsr` prints the IR a program lowers to after every pass, as
  C generation gets it, with what C generation decides about it (the
  pure functions, the calls kept across loops, the loops it hoists,
  fills or versions), in `ir_print.tsr`'s format: as `tessera build`
  compiles it, and as `tessera test` does when it has tests. `irdump/`
  prints the OCaml backend's.
- `check.sh` builds the programs and compares the lexers, the parsers,
  the loaders, the checkers and the backends on every `.tsr` file in the repository, the
  parsers also on the edge cases in `tests/` (constructs that are easy to
  get wrong, and one file for each error the parser can report), the
  loaders on those in `tests/load/` (one directory each, for each
  diagnostic the loader can report and each expansion), the checkers on
  those in `tests/check/` (every problem the checker and the capability
  checks can report, and every rewrite), the backends on those in
  `tests/ir/` (each construct the lowering and the passes treat on their
  own), and all of them on the book's examples when `../tessera-book`
  (or `$TESSERA_BOOK`) is there. Then the compilers: the C of `build
  --emit-c` (and of the test binary) on every program, the edge cases in
  `tests/c/` and the book's examples, with the output and exit status
  where there is no C; and a set of the book's programs (concurrency,
  HTTP, TLS, JSON, SQL, C calls, embedded files, tests, ...) built by
  both compilers and run:

```sh
sh selfhost/check.sh
```

The loader does not fetch git dependencies (the OCaml one runs git and
keeps tessera.lock), and it sees only what its Fs grants: a dependency or
an embedded file outside the granted directory does not exist for it.

Next: the bootstrap. `tessera.tsr` compiled by itself, then that
compiler compiling it again, should give the same C.
