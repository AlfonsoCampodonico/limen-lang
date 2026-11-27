# A playground in the browser: design

Status: agreed in conversation on 2026-11-26, not yet planned or built.

## Intent

A page beside the book where someone who has never installed Limen can type
a program, see it checked, see the capabilities it asks for, and run it. The
point is that people can try the language. The playground is itself coded in
Limen: the self-hosted compiler checks the program, and a runner written in
Limen runs it, both built to WebAssembly. Everything happens in the visitor's
browser; the server only hands out static files.

ROADMAP 0.9 lists "A playground and short videos beside the book"; this is the
playground half.

## Goals

- `limen-lang.org/play` (served by the book's Limen server): an editor, the
  diagnostics and the capability list as you type, a Run button, the output.
- Checking is the real compiler: the self-hosted compiler
  (`selfhost/limen.lmn` and the modules it uses), compiled through its C to
  WebAssembly, so the playground's errors are exactly `limen check`'s.
- Running covers pure programs that use Stdout, Stderr, Stdin
  (interactively), Clock and Rand, which is what the book's early chapters
  need, chapter 2's guessing game included.
- Examples taken from the book, and Share links that carry the code in the
  URL fragment (never sent to the server).
- Each Limen release rebuilds the playground; the page shows the version.

## Non-goals

- Running code on a server. Strangers' programs never run on the host of the
  book: the Linux sandbox is marked untested and the outside security review
  is still open (ROADMAP 0.9).
- Net, Fs, Listen, Env, Exec and Foreign in the playground.
- Tasks (`spawn`, `group`, channels, `select`) in the first version.
- Packages and `limen.toml`: a playground program is one file.
- A WebAssembly backend (the "After 1.0" item); replacing `limen run` or the
  OCaml interpreter.

## Anti-goals

- A program that the playground runs and that behaves differently from the
  native build. Whatever the runner cannot run exactly, it refuses with a
  clear message ("tasks do not run in the playground yet"); it never
  approximates.
- A playground that needs a server to work, or sends code anywhere.

## Constraints

- The self-hosted compiler emits about 446k lines of C (after 0.8) and links
  the C runtime: `src/runtime/limen_rt.c`, `limen_rt.h`, `limen_core.h`,
  `limen_sandbox.h`, plus `limen_tls.h` and `limen_sql.h`.
- What in that runtime has no WebAssembly equivalent: the fiber context
  switch is hand-written arm64 and x86-64 assembly (`limen_rt.c` around line
  1212); a pthread pool (over 200 `pthread_` uses); the epoll/kqueue poller;
  processes (`spawn.h`), sockets, signals, `rlimit`; the OS sandbox; OpenSSL.
  Panics unwind with setjmp/longjmp.
- What helps: memory is reference-counted (`w_rc_dec`), so there is no
  collector scanning the stack, which is what usually makes such ports hard.
- The self-hosted compiler reaches files only through its Fs grant and runs
  `cc` through Exec for `build`. `check` and `caps` need no Exec.
- Repo rules: `dune build`, `dune exec ./test/test_limen.exe`,
  `sh selfhost/check.sh` and `sh selfhost/bootstrap.sh` pass before any
  change; British spelling, no em dashes, no emojis.
- Every language change goes into both compilers. The IR evaluator is a
  deliberate exception (see below).
- The book server (`limen-book/server.lmn`) serves an allow-list of paths
  with `content_type`; it needs a `/play` route, `.wasm` as
  `application/wasm`, and the playground's files on the list. The book is
  pushed only for a release or with the user's say-so.

## Approaches considered

1. **All in the browser (chosen).** The self-hosted compiler built to
   WebAssembly for checking; a runner written in Limen, built the same way,
   for running. No server, really coded in Limen. Costs: a WebAssembly mode
   of the runtime, the runner, a download of a few MB.
2. **Run on the server.** A Limen HTTP endpoint compiles with `cc` and runs
   the program in the sandbox with no grants. Cheapest and runs everything,
   but puts strangers' code on the book's host before the sandbox is
   reviewed. Rejected.
3. **Hybrid: check in the browser, run on the server.** Same risk as 2 for
   the run half. Rejected.

For the runner, two choices:

- **A playground-only IR evaluator (chosen).** It interprets the
  self-hosted compiler's IR (`selfhost/ir.lmn`: structured, A-normal form,
  every type concrete, ownership already decided), sitting beside
  `selfhost/cgen.lmn` as a second way out of the compiler. Because the front
  end and lowering are shared, it changes only when the IR changes, as cgen
  does; most language changes cost it nothing.
- **A Limen port of the OCaml interpreter (`src/interp.ml`, 3.4k lines)**
  aimed at replacing `limen run`. Would settle the 0.9 question, but must
  match the whole suite (2342 tests) including tasks, Net, Fs and Exec,
  which delays the playground behind work a browser cannot use. Not chosen;
  if 0.9 decides the interpreter moves to Limen, the evaluator is a starting
  point.

## The design

Three parts, each usable on its own.

### 1. A WebAssembly build of the self-hosted compiler (check and caps)

- Built with clang `--target=wasm32-wasi` and the WASI sysroot (wasi-sdk or
  Homebrew LLVM plus wasi-libc), not Emscripten: lighter, and WASI's file
  calls match how the compiler already reads through its Fs.
- A WebAssembly mode of the runtime, selected by a define (for example
  `LIMEN_WASM`): one thread, no fibers, no thread pool, no poller, no
  processes, sockets, signals or sandbox, no OpenSSL. Calls into those parts
  fail with a clear panic; the playground's programs never reach them
  because the checks refuse them first. Panics keep setjmp/longjmp through
  WebAssembly's exception handling (`-mllvm -wasm-enable-sjlj`, supported by
  current browsers).
- A small JavaScript shim provides WASI: an in-memory file system holding
  `main.lmn`, stdout and stderr captured as text. A browser WASI shim
  library or a hand-written one of a few hundred lines; builder's call.
- The entry point is the command line (`check FILE --json`,
  `caps FILE --json`), so the compiler needs no playground-specific code.
  The JSON output drives the editor's markers and the capability list.
  The self-hosted compiler does not have `--json` yet
  (`selfhost/limen.lmn` prints "--json is not in this compiler yet"); the
  first piece of work ports both JSON formats from the OCaml CLI
  (`bin/main.ml`, `Diag.list_to_json`) byte for byte and adds them to
  `selfhost/check.sh`. (My call: porting beats parsing text in JavaScript,
  and closes a gap the 0.9 "self-hosted compiler as default" item needs
  closed anyway.)
- A size budget is set from the first measurement; the build uses `-Oz` and
  `wasm-opt` if it helps, and the files are served compressed.

### 2. The IR evaluator

- A new module of the self-hosted compiler (for example
  `selfhost/eval.lmn`) that takes the lowered, ownership-checked IR and runs
  it. Values are a Limen enum; built-in methods (Str, List, Map, and the
  rest in `limen_core.h`) map onto Limen's own operations on those values.
  Reference counts are not modelled: the evaluator's own values are counted
  by the runtime it runs on; VDup and Drop are no-ops apart from any
  observable effect (none today; confirm while building).
- Panics, integer overflow, index and key errors, exit codes and the
  messages they print match the native build byte for byte.
- Resource limits are not semantics. Native `main` has a 1 GB stack; the
  evaluator in wasm32 will run out of stack or memory much sooner. Running
  out of stack, memory, time or output ends the run with a message naming
  the playground limit ("ran out of stack in the playground"), not a
  different result, so a plain recursive evaluator is fine; it need not
  reach native depths. (My call: the anti-goal is about programs that
  finish differently, and the time limit already differs from native.)
- Stdout, Stderr, Clock and Rand are supported (Clock from the browser's
  time, Rand seeded from the browser unless the page passes a seed). Book
  programs take `fn main(root: Root)`; the evaluator accepts `Root` and
  judges a program by the fields it uses, as `limen caps` does.
  `root.args()` is empty in the playground. Stderr shows in the output
  pane, marked as such.
- Stdin is supported and interactive (decided by the user): a program
  that reads a line waits while the visitor types into the output pane,
  as in a terminal, so chapter 2's guessing game plays as it does
  natively. The evaluator runs to completion inside the worker, so a read
  blocks the worker with `Atomics.wait` on a `SharedArrayBuffer` that the
  page fills when the visitor presses Enter (end of input: a button or
  Ctrl-D). `SharedArrayBuffer` needs cross-origin isolation, so the server
  sends `Cross-Origin-Opener-Policy: same-origin` and
  `Cross-Origin-Embedder-Policy: require-corp` on `/play` and the
  playground's own files only, not on the rest of the book. Everything the
  page loads is same-origin (the site's own fonts and scripts), so nothing
  else changes. Stop and the time limit still terminate the worker; the
  time limit does not count time spent waiting for input. Anything else the
  evaluator cannot run exactly, such as tasks, is refused before the program
  starts, naming the construct and its line.
- Reached through a new command of the self-hosted compiler, for example
  `limen run FILE --eval`, so it is usable and testable from the command
  line too, not only in the browser.
- Tests: every test in the suite whose program needs only Stdout, Stderr,
  Stdin (fed from a file), Clock and Rand (with a fixed seed), and every
  book example that does, runs through the evaluator and its output and
  exit code are compared with the native build. The comparison joins
  `selfhost/check.sh` so a later IR change that breaks the evaluator fails
  the gate.
- The both-compilers exception: the evaluator exists only in the
  self-hosted compiler. SPEC and ROADMAP say so, and the design doc records
  it as a decision.

### 3. The page

```
+------------------------------------------------------------+
| Limen playground   0.8.0      [Examples v]  [Share]  [Run] |
+------------------------------+-----------------------------+
| fn main(out: Stdout) {       | Capabilities: Stdout        |
|     out.print("hi")          | --------------------------- |
| }                            | hi                          |
|                              |                             |
+------------------------------+-----------------------------+
```

- Lives in limen-book (it is part of the site), at
  `book.limen-lang.org/play`, in the book's design (its tokens, fonts and
  light/dark themes). `limen-lang.org/play` already reaches it through the
  landing host's existing redirect to the book host; the landing page and
  the book link to it. (My call: the landing host serves only `home/`.)
- The compiler's `.wasm` ships gzipped inside the server, like the fonts:
  `dist/play/limen.wasm.gz` is embedded by `embed_dir("dist")` into the
  committed `build/server.c`, and the server serves it at
  `/play/limen.wasm` with `Content-Type: application/wasm` and
  `Content-Encoding: gzip`, its URL versioned (`?v=0.8.0`) so it caches for
  good. (My call, revised while planning: downloading it at deploy and
  serving it from disk does not work, because Cloud's Rust image copies
  only the executables in `target/release` into the running container.
  The cost is a larger `server.c`, a few MB of git history per release
  that changes the compiler; measured in the plan's size task.)
- The compiler runs in a Web Worker. Checking runs after a short pause in
  typing; Run checks, then evaluates. Stop terminates the worker; a time
  limit (a few seconds) and an output cap do the same automatically, with a
  message saying which.
- Diagnostics are shown under the code and as markers in the editor; the
  capability list shows what the program asks for, and a program asking for
  Fs, Net or Exec gets a plain explanation of why the playground does not
  grant it, linking to the book's chapter on capabilities.
- Examples: a dropdown of book programs that run in the playground.
- Share: the code compressed into the URL fragment.
- The editor: a textarea over code rendered by Pierre (`@pierre/diffs`)
  with Shiki and the Limen TextMate grammar the VS Code extension uses,
  both vendored into limen-book and served by the book's own server
  (decided by the user on 2026-11-27; the plan's Task 7 has the details).
  The book's pages keep their own highlighting.

## Left to the builder

- The exact names (`LIMEN_WASM`, `eval.lmn`, `--eval`).
- wasi-sdk or Homebrew LLVM; which WASI shim.
- How the WebAssembly build is scripted (a `make` target or `dev/` script)
  and where it runs in the release checklist.
- The time limit, output cap and size budget, from measurements.
- Which book examples go in the dropdown (those the evaluator runs).
