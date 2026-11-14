# Tessera in Tessera

The Tessera compiler, written in Tessera. Tessera is self-hosting: the
OCaml compiler in `src/` builds this one (stage 1), stage 1 builds itself
(stage 2), stage 2 builds itself again (stage 3), and stages 2 and 3 emit
the same C, which is also exactly what the OCaml compiler emits for it
(about 382,000 lines, the embedded standard library and runtime included). `sh selfhost/bootstrap.sh` runs all three stages and
checks that; it takes about three minutes, nearly all of it the C compiler.
The OCaml compiler stays as the way to build from scratch, and as the
reference: `sh selfhost/check.sh` requires both compilers to agree on every
program, step by step.

`tessera.tsr` is the compiler program: `check`, `caps` and `build` (with
`--emit-c` and `--tests`), with the same output and exit codes as the OCaml
CLI. It holds only the authority it is granted: files, a fixed list of
environment variables, and the programs a build runs (the C compiler and a
few file tools); `tessera caps selfhost/tessera.tsr` shows that checking,
lowering and C generation are pure. Like the OCaml compiler, it has the
standard library and the C runtime built in: `selfhost/tessera.toml`
declares `src/std` and `src/runtime` embed roots, and `sources.tsr` embeds
them, so the binary reads nothing of its own when it runs. The interpreter, formatter, language
server, `doc` and MCP server are still OCaml only.

What each file does, in pipeline order:

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
  assignment; `embeds.tsr` reads the files `embed` names, in the package
  or under the embed roots its tessera.toml declares (the loader reads
  only through the Fs it is handed, for the program; the standard
  library's sources come built in, from `sources.tsr`); `forwarders.tsr` adds the methods
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
  Checking reads tessera.lock through the Fs and never writes it; a
  build then records the libraries it has no line for (`caps.record`),
  as the OCaml compiler's build does.
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
  build cache, keyed as the OCaml compiler keys it (MD5, from the standard
  library's `hash` module), so
  the two compilers share it. The Fs gives the C compiler's size and
  modification time for the key (`Fs.info`) and moves a finished copy
  into place (`Fs.rename`); what it cannot do (read the link xcode-select
  keeps, copy a binary keeping it executable, touch a cache entry) it
  asks `readlink`, `cp` and `touch` to do through the Exec; OpenSSL is
  found through `TESSERA_OPENSSL` or `pkg-config`. A `Builder` holds the
  Exec and the Fs for the steps that need both (the cache key, the cache);
  the others take only the one they use.
- `sources.tsr` is the standard library's sources and the C runtime,
  embedded from `src/std` and `src/runtime` when the compiler is built, as
  `src/dune` builds them into the OCaml compiler.
- `tessera.tsr` is the compiler: `check FILE`, `caps FILE` and `build FILE
  [-o OUT] [--emit-c]` as `bin/main.ml` has them, plus `build --tests` for
  the test binary of `tessera test --native` (`cdump/` writes the OCaml
  compiler's C of it). The standard library and the C runtime are built
  into it (`sources.tsr`), as into the OCaml compiler. What
  the OCaml compiler writes to stderr (the diagnostics and their count,
  the usage) it writes to stderr too, and a build records the C libraries
  of trusted dependencies in tessera.lock as the OCaml one does (`check`
  and `caps` only read it). Its grants, in full:

```sh
dune build && _build/default/bin/main.exe build selfhost/tessera.tsr -o tsc
./tsc --grant fs=/ \
  --grant env=PWD --grant env=CC --grant env=PATH --grant env=HOME \
  --grant env=TESSERA_CACHE --grant env=XDG_CACHE_HOME --grant env=DEVELOPER_DIR --grant env=TESSERA_OPENSSL \
  --grant env=LIBRARY_PATH --grant env=CPATH --grant env=C_INCLUDE_PATH --grant env=SDKROOT --grant env=MACOSX_DEPLOYMENT_TARGET \
  --grant exec=cc --grant exec=readlink --grant exec=cp --grant exec=touch --grant exec=pkg-config \
  -- build examples/hello.tsr -o hello
```

  The Fs must hold the program, the output and (to use it) the build
  cache, and the C compiler for the cache key to match the OCaml
  compiler's; `fs=/` is the simple choice.
- `tokens.tsr` prints a file's tokens one per line; `tokdump/` prints the
  OCaml lexer's in the same format.
- `parse.tsr` prints a file's tree (with `ast_print.tsr`): the compiler's
  position-free format from `src/astdump.ml`, then every span in the tree,
  or the error. `astdump/` prints the OCaml parser's in the same format.
- `load.tsr` prints the loaded program (its items in the same format, then
  the C declarations, the embed roots, the embedded files and every span
  with its file), or
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
  `tests/c/` and the book's examples, with the output, the diagnostics
  on stderr (compared apart from the output) and the exit status where
  there is no C; and a set of the book's programs (concurrency,
  HTTP, TLS, JSON, SQL, C calls, embedded files, tests, ...) built by
  both compilers and run. The edge cases are programs of their own:
  `tests/tessera.toml` keeps them out of the compiler's package (and its
  embed roots), and the ones that embed files have a tessera.toml in
  their own directory, so their package is that directory:

```sh
sh selfhost/check.sh
```

The loader does not fetch git dependencies (the OCaml one runs git and
keeps tessera.lock), and it sees only what its Fs grants: a dependency or
an embedded file outside the granted directory does not exist for it.

Next: the bootstrap. `tessera.tsr` compiled by itself, then that
compiler compiling it again, should give the same C.
