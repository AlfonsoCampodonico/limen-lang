# Playground Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A page at `book.limen-lang.org/play` where a visitor writes Limen, sees `limen check` and `limen caps` as they type, and runs the program, all in their browser, through the self-hosted compiler built to WebAssembly and a new IR evaluator written in Limen.

**Architecture:** The self-hosted compiler's C (`limen build selfhost/limen.lmn --emit-c`) is compiled with clang for `wasm32-wasip1` under a new `LIMEN_WASM` mode of the C runtime (one thread, no fibers, no OS services beyond WASI). A new `eval` command of the self-hosted compiler runs a program by interpreting its IR after the same passes C generation runs. One JavaScript WASI shim (`dev/wasm/wasi.mjs`) runs that `.wasm` in node (tests) and in a browser Web Worker (the page), where stdin blocks on `Atomics.wait`.

**Tech Stack:** Limen (selfhost), C (runtime), clang + wasi-libc (Homebrew `llvm`, `wasi-libc`, `wasi-runtimes`) or wasi-sdk, node 22+ for tests, plain JavaScript ES modules in the page (no framework, no bundler); Pierre's `@pierre/diffs` with Shiki for the editor's highlighting and the diagnostics view, vendored into limen-book (Task 7).

**Spec:** `docs/superpowers/specs/2026-11-26-playground-design.md`

## Global Constraints

- Before every commit in the limen repo: `dune build`, `dune exec ./test/test_limen.exe`, `sh selfhost/check.sh`, `sh selfhost/bootstrap.sh` all pass. Run long ones in the background; never pipe a step whose exit code matters.
- British spelling, no em dashes, no emojis, in code comments, docs and page copy.
- No Claude attribution in commits. Commit per task; never push either repo (the book is pushed only for a release or with the user's say-so).
- `eval` exists only in the self-hosted compiler: the one deliberate exception to "every language change goes into both compilers". Everything else (`--json`) matches the OCaml compiler byte for byte.
- The playground never sends code anywhere; nothing runs on the server.
- Anti-goal: a program that `eval` runs and that behaves differently from the native build. Whatever `eval` cannot run exactly, it refuses before running.
- Runs: Stdout, Stderr, Stdin (interactive in the page), Clock, Rand, `root.args()` (empty in the page). Refused: Net, Fs, Listen, Env, Exec, Foreign, tasks, groups, channels, select, C calls.
- `COOP: same-origin` and `COEP: require-corp` only on `/play` and the playground's own files.
- Work in a worktree of each repo (`superpowers:using-git-worktrees`), branch `playground`.

## Review Focus

1. **Input ends.** A visitor presses Ctrl-D (or End input) while the program waits on `read_line`: the program sees end of input (`None`) exactly as native does on EOF, and later reads also get `None`. Test in Task 7.
2. **A program that never stops.** An endless loop with no output, and one printing forever: the time limit (10 s, not counting time waiting for input) or the output cap (1 MiB) stops it with its message, and the next Run works. Test in Task 7.
3. **Deep recursion.** In `eval`, a recursion past the depth limit ends with the playground's stack message, never a wasm trap or a hang; and a deeply nested source (`selfhost/tests/deep-*.lmn`) checked by the wasm compiler either checks as natively or ends with the page's trap message. Tests in Tasks 2 and 5.
4. **A valid program the playground cannot run** (book chapter 14's files example): it checks clean, the caps pane explains, and Run refuses with the line and the capability, without starting. Tests in Tasks 3 and 7.
5. **Non-ASCII text** in source, output and typed input (`é`, CJK, an emoji in a string literal): byte-exact through the shim, including a multi-byte character split across two `fd_write` calls. Tests in Tasks 2 and 7.

---

### Task 1: `check --json` and `caps --json` in the self-hosted compiler

**Files:**
- Modify: `selfhost/diag.lmn` (JSON of a diagnostic)
- Modify: `selfhost/limen.lmn:96-102` (replace the "--json is not in this compiler yet" arm), `selfhost/limen.lmn` `check` and `caps_of`
- Modify: `selfhost/check.sh` (compare both formats)
- Test: `selfhost/tests/` new cases `json-warning.lmn`, `json-fix.lmn`, `json-unicode.lmn`

**Interfaces:**
- Produces: `diag.to_json(d: Diag, files: List[Source]) -> Str`, `diag.list_to_json(ds: List[Diag], files: List[Source]) -> Str`, `diag.jstr(s: Str) -> Str`; `limen check FILE --json` and `limen caps FILE --json` (either argument order), output and exit status identical to `bin/main.ml` `cmd_check` / `cmd_caps` (`src/diag.ml:110-146` and `bin/main.ml:424-460` are the reference, including warnings kept under `--json` and the `embeds` part of `caps`).

- [ ] **Step 1:** Add the three cases: a warning-only program, a diagnostic with a fix (edits and replacement), and a message quoting a non-ASCII identifier or string. Add to `check.sh` a `compare_json` that runs `check FILE --json` and `caps FILE --json` with both compilers on every file the C comparison already covers and on `selfhost/tests/*.lmn`, comparing stdout and exit status.
- [ ] **Step 2:** Run `sh selfhost/check.sh`. Expected: the new JSON comparisons fail (the Limen compiler prints the "not in this compiler yet" message, exit 2).
- [ ] **Step 3:** Implement the JSON writers and the two command arms. `jstr` must escape exactly as `Diag.jstr` does (check `src/diag.ml` for which control characters become `\uXXXX`).
- [ ] **Step 4:** Run `sh selfhost/check.sh`. Expected: all comparisons pass, the JSON count printed among them.
- [ ] **Step 5:** Full gate, then commit `selfhost: check --json and caps --json`.

### Task 2: The runtime's WebAssembly mode and the wasm build of the compiler

**Files:**
- Modify: `src/runtime/limen_rt.h`, `src/runtime/limen_rt.c`, `src/runtime/limen_sandbox.h` (every OS-specific part under `#ifdef LIMEN_WASM`)
- Create: `dev/wasm/build.sh` (C from the OCaml compiler's `build --emit-c selfhost/limen.lmn`, then clang to `_build/wasm/limen.wasm`)
- Create: `dev/wasm/wasi.mjs` (the WASI shim, an ES module that runs in node and in a browser Worker)
- Create: `dev/wasm/run.mjs` (node CLI: `node dev/wasm/run.mjs [--stdin-file F] -- ARGS...`, maps the current directory into the shim's file system, exits with the program's status)
- Create: `dev/wasm/check.sh` (compares the wasm compiler with the native self-hosted one)
- Modify: `selfhost/check.sh` (runs `dev/wasm/check.sh` when `dev/wasm/build.sh --probe` finds a toolchain; prints `wasm: skipped, no wasm32-wasip1 toolchain` otherwise)

**Interfaces:**
- Consumes: Task 1's `--json`.
- Produces: `_build/wasm/limen.wasm`; `wasi.mjs` exporting `createWasi({ args: string[], files: Map<string, Uint8Array>, stdout: (bytes: Uint8Array) => void, stderr: (bytes: Uint8Array) => void, readStdin: (max: number) => Uint8Array | null, now: () => bigint, random: (buf: Uint8Array) => void }) -> { imports: WebAssembly.Imports, start(instance: WebAssembly.Instance): number }`. `readStdin` returning `null` means end of input; an empty array is never returned. Page and node both use this one file.

The `LIMEN_WASM` mode: `w_init` skips `SIGPIPE`, `rlimit` and sandbox setup, and accepts `--grant fs=/` (the shim preopens `/`) and `--seed`. `w_run` calls the entry directly on the wasm stack: no fiber, no worker, no poller. Stdin reads are a plain blocking `read(0, ...)`. Every task, group, channel, select, poller, socket, process, sandbox, TLS and SQL entry point panics with `panic: not in the WebAssembly build: NAME` (NAME the runtime function). The compiler itself uses none of them. Panics keep setjmp/longjmp through wasm exception handling. Starting flags for `build.sh` (adjust only with a measured reason, recorded in a comment): `--target=wasm32-wasip1 -O2 -DLIMEN_WASM -fwrapv -ffp-contract=off -mexception-handling -mllvm -wasm-enable-sjlj -D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_PROCESS_CLOCKS -Wl,-z,stack-size=67108864 -Wl,--max-memory=4294967296 -lsetjmp -lwasi-emulated-signal -lwasi-emulated-process-clocks -lm`, then `wasm-opt -O2` when installed. `build.sh` prints the `.wasm` size raw and gzipped.

- [ ] **Step 1:** Write `dev/wasm/check.sh`: for every file `check.sh` already compares, run the native self-hosted compiler and `node dev/wasm/run.mjs -- check FILE --json` and `-- caps FILE --json` in the file's directory: same stdout, same exit status. Include `selfhost/tests/deep-*.lmn` and a `selfhost/tests/unicode-output.lmn` (non-ASCII identifiers in an error). Add a shim unit test `dev/wasm/wasi.test.mjs` (`node --test`): stdout bytes for `"é"` written in two `fd_write` calls (`0xC3`, then `0xA9`) arrive intact; `readStdin` returning `null` makes `fd_read` return 0 bytes.
- [ ] **Step 2:** Run `sh dev/wasm/build.sh`. Expected: fails (the runtime does not compile for wasm32).
- [ ] **Step 3:** Implement the `LIMEN_WASM` mode, `build.sh`, `wasi.mjs` and `run.mjs`.
- [ ] **Step 4:** Run `sh dev/wasm/build.sh && sh dev/wasm/check.sh && node --test dev/wasm/`. Expected: every comparison matches. A deep-nesting case that the native compiler handles but the 64 MB wasm stack does not: raise the stack size if the gzipped size is unaffected, otherwise record the case in `dev/wasm/check.sh` as a known limit with the page's trap message (Task 7). Record the sizes in the commit message.
- [ ] **Step 5:** Full gate (native builds unchanged: the bootstrap fixed point must still hold), then commit `runtime: a WebAssembly mode; the compiler built to wasm32-wasip1`.

### Task 3: The evaluator's core and the `eval` command

**Files:**
- Create: `selfhost/eval.lmn`
- Modify: `selfhost/limen.lmn` (the `eval` command, its usage line)
- Create: `selfhost/tests/eval/` cases, each `NAME.lmn` with optional `NAME.in` (stdin)
- Modify: `selfhost/check.sh` (`compare_eval`)

**Interfaces:**
- Consumes: the IR after `own.program(pure.cse(lower.lower_program(c.prelude, c.items, c.cx, c.state, false, true, false)))`, as `c_source` in `selfhost/limen.lmn:433` builds it but with `lines` true, so `Line(span)` statements give positions.
- Produces: `eval.run(prog: ir.Program, ro: lower.Ro, files: List[diag.Source], io: eval.Io) -> Int` (the exit status); `pub struct Io { out: Stdout, err: Stderr, input: Stdin, clock: Clock, rand: Rand, args: List[Str] }`; `eval.refusal(prog: ir.Program, files: List[diag.Source]) -> Option[Str]`. Command: `limen eval FILE [-- ARGS...]` (checks first, prints diagnostics and exits 1 as `check` does when the program has errors).

Design pinned here:
- Values: `pub enum Value` with one case per runtime shape (Int, Float, Bool, Unit, Str, Bytes, List, Map, struct and enum as tag plus fields, closure as function name plus captures, dyn as vtable key plus value). Mutation through `Mutate` and `ByPlace` changes the place in the evaluator's environment; reference counts are not modelled, so `VDup` reads like `V` and `Drop` does nothing.
- Control flow: statements evaluate to `enum Flow { Next, Brk, Cont, Ret(Value), Panicked(Str) }`; a Limen-level panic is `Panicked`, never a panic of the host, so `catch` and `defer` (Task 5) can see it.
- Refusal runs before anything else, on the whole IR: any `GroupNew`, `Spawn`, `GroupJoin`, `ChanNew`, `ChanSend`, `Select`, `SelGot`, `CallC`, `Scope`, and any `RootField` other than `stdout`, `stderr`, `stdin`, `clock`, `rand`, `args`. Message on stderr, exit status 2, position from the nearest preceding `Line`: `limen eval: FILE:LINE:COL: not available in the playground: WHAT`, WHAT one of `tasks`, `channels`, `select`, `calls into C`, `the Fs capability` (and likewise `Net`, `Listen`, `Env`, `Exec`, `Foreign`). Only the first refusal is reported.

- [ ] **Step 1:** Add `compare_eval DIR FILE`: native binary from the OCaml compiler run with `--seed 1` and the `.in` file (if any) on stdin, versus `tsc --seed 1 --grant fs=DIR:ro -- eval FILE`: same stdout, stderr and exit status. Add cases: `arith.lmn` (Int/Float/Bool operators, comparison, integer overflow panic), `control.lmn` (if, loops, break, continue, early return), `data.lmn` (structs, enums, match with nested patterns, lists and maps read and mutated, `mut` parameters on a list element and a struct field), `closures.lmn` (captures, closures passed and returned), `format.lmn` (interpolation of every value shape, the quoted form inside containers), `panic.lmn` (`panic`, index out of range, missing key, each exit status). Add the refusal cases with fixed expected output (no native comparison): `refuse-fs.lmn` (expected `limen eval: refuse-fs.lmn:LINE:COL: not available in the playground: the Fs capability`, status 2, LINE and COL those of the `root.fs` use as the case is written) and `refuse-spawn.lmn` (`...: not available in the playground: tasks`).
- [ ] **Step 2:** Run `sh selfhost/check.sh`. Expected: `eval` cases fail (unknown command).
- [ ] **Step 3:** Implement `eval.lmn` and the command. Built-ins (`Prim`) needed by these cases only; the rest is Task 4.
- [ ] **Step 4:** Run `sh selfhost/check.sh`. Expected: all `eval` cases pass.
- [ ] **Step 5:** Full gate, commit `selfhost: limen eval, an IR evaluator for the playground`.

### Task 4: Every built-in method in the evaluator, and the whole corpus

**Files:**
- Create: `selfhost/eval_prims.lmn` (one function per receiver type, dispatched on the `Prim` name as `cgen.prim` in `selfhost/cgen.lmn:1588` does)
- Modify: `selfhost/eval.lmn` (dispatch to it)
- Modify: `selfhost/check.sh` (the corpus)
- Test: `selfhost/tests/eval/prims-*.lmn`, one per receiver type

**Interfaces:**
- Consumes: Task 3's `Value`, `Flow`, `Io`.
- Produces: `eval_prims.call(name: Str, recv_ty: types.Ty, args: List[Value], io: Io, at: ast.Span) -> Result[Value, Str]` (`Err` is the panic message, exactly native's).

Standard library modules written in Limen need nothing here: they are ordinary IR. Every `Prim` name that `cgen.prim` handles is implemented, except those reachable only through a refused capability or task construct. Each built-in maps onto the host Limen operation on the same value (`xs.sort()` on the host list, `s.split(t)` on the host string), so results, float formatting and panic messages come from the same runtime code as native. Clock and Rand prims call `io.clock` and `io.rand`, so `--seed N` on the host gives the native sequence.

- [ ] **Step 1:** Add `prims-str.lmn`, `prims-list.lmn`, `prims-map.lmn`, `prims-int-float.lmn`, `prims-bytes.lmn`, `prims-option-result.lmn`, each calling every method `selfhost/builtins.lmn` lists for that type, including the panicking edge (empty list `first`, bad slice range). Extend `check.sh`: every `.lmn` in the repo and every book example whose `commands` file runs `limen run main.lmn` with nothing but `--seed N` and `< input.txt`, and which `eval` does not refuse, goes through `compare_eval`. Print `eval: N programs, M refused`.
- [ ] **Step 2:** Run `sh selfhost/check.sh`. Expected: the new cases and much of the corpus fail with an unknown built-in.
- [ ] **Step 3:** Implement `eval_prims.lmn`. Add a check that every `Prim` name in `cgen.prim` is either handled or on an explicit refused list, so a built-in added later fails `check.sh` until the evaluator has it.
- [ ] **Step 4:** Run `sh selfhost/check.sh`. Expected: every case and every corpus program passes or is refused; none differs.
- [ ] **Step 5:** Full gate, commit `selfhost: eval runs every built-in method`.

### Task 5: catch, defer, dyn, Root I/O, exit status and the depth limit

**Files:**
- Modify: `selfhost/eval.lmn`
- Test: `selfhost/tests/eval/` cases `catch.lmn`, `defer.lmn` (defer running on normal exit, on early return and on panic), `dyn.lmn`, `stdin.lmn` + `stdin.in` (lines, then end of input, then a read after end of input), `stderr.lmn`, `main-result.lmn` (main returning `Result` with an error: native's `error: ...` line and status), `main-int.lmn` (main returning an Int status), `args.lmn` (run with `-- a b`), `rand.lmn` (with `--seed 1`), `deep.lmn` (recursion to depth 1000 succeeds), `too-deep.lmn` (unbounded recursion)

**Interfaces:**
- Produces: `eval.max_depth() -> Int`, the call depth at which `eval` stops with `limen eval: the program ran out of stack in the playground (more than N nested calls)` on stderr, status 1. N starts at 10000; set it below what the wasm build's stack survives (measure with `too-deep.lmn` through `dev/wasm/run.mjs`) and above 1000.

- [ ] **Step 1:** Add the cases; `too-deep.lmn` has a fixed expected output (the message above with N), the rest go through `compare_eval`. Add `too-deep.lmn` and `deep.lmn` to `dev/wasm/check.sh` running `eval` through the wasm build: `deep` must match native, `too-deep` must print the message, never trap.
- [ ] **Step 2:** Run both checks. Expected: the new cases fail.
- [ ] **Step 3:** Implement `Catch`, `Guard`, `GuardEnd`, `MkDyn`, `CallDyn`, the Root fields, main's result and status as `cgen` writes them, and the depth counter.
- [ ] **Step 4:** Run `sh selfhost/check.sh` and `sh dev/wasm/build.sh && sh dev/wasm/check.sh`. Expected: all pass.
- [ ] **Step 5:** Full gate, commit `selfhost: eval handles catch, defer, dyn, Root and deep recursion`.

### Task 6: Release wiring and documentation (limen repo)

**Files:**
- Create: `dev/release/wasm.sh` (`sh dev/release/wasm.sh X.Y.Z` writes `dist-release/limen-X.Y.Z.wasm.gz` and `dist-release/wasi-X.Y.Z.mjs`, adds both to `SHA256SUMS`)
- Modify: `SPEC.md` (the `eval` command: self-hosted only, what it runs and refuses, its messages), `ROADMAP.md` (0.9 playground item: what is built), `selfhost/README.md` (eval, the wasm build), `README.md` (one line on the playground)

- [ ] **Step 1:** Write `wasm.sh` on top of `dev/wasm/build.sh`; run it with the current version. Expected: both files and their sums in `dist-release/`.
- [ ] **Step 2:** Update the docs. State the both-compilers exception in SPEC and ROADMAP in one plain sentence each.
- [ ] **Step 3:** Full gate, commit `release: the playground's wasm; docs for eval`.
- [ ] **Step 4:** Update the design doc (Claude Docs project `UqAJ6SrQyDNhe4KjicrS5X`): decisions (browser only, IR evaluator, both-compilers exception, interactive stdin, wasm embedded in the server), glance table, roadmap, gaps (tasks in the playground).

### Task 7: The playground page and its workers (limen-book)

**Files:**
- Create: `tools/play.html` (page, in the book's layout and tokens), `tools/play.css`, `tools/play.js` (editor, panes, share, examples), `tools/play-worker.js` (one compiler instance per worker; protocol below), `tools/play-highlight.js` (Pierre and Shiki set up for Limen: the grammar, the theme, the diagnostics as line annotations)
- Create: `tools/vendor.sh` (fetches pinned versions of `@pierre/diffs`, `shiki` and the packages they import from the npm registry, checks each tarball against a SHA-256 recorded in the script, and writes them as ES modules to `tools/vendor/`, with each package's licence), and `tools/vendor/` (the result, committed)
- Modify: `tools/make_book.py` (writes `dist/play/index.html`, `play.css`, `play.js`, `play-worker.js`, `play-highlight.js`, `examples.json`, `vendor/` (the vendored modules) and `limen.tmLanguage.json` (copied from `$LIMEN_REPO/editors/vscode/syntaxes/`); copies `wasi.mjs` and `limen.wasm.gz` from `$LIMEN_WASM_DIR`, default `$LIMEN_REPO/_build/wasm`, gzipping the `.wasm`)
- Modify: `Makefile` (`play` target: `sh $(LIMEN_REPO)/dev/wasm/build.sh`, then the book; `book` depends on it)
- Test: `tools/play.test.mjs` (`node --test`, the worker protocol against the real `.wasm`), `tools/play-highlight.test.mjs` (`node --test`, the highlighting in node against the vendored modules)

Highlighting, decided by the user on 2026-11-27, and as built: Pierre (`@pierre/diffs`) is the editor, highlighted by Shiki, both vendored, nothing loaded from a CDN (COEP `require-corp` would refuse it, and the page must work with only the book's server).
- Pierre has an editor of its own (`File` plus `Editor` from `@pierre/diffs/edit`), so there is no textarea over rendered code; diagnostics from `check --json` are its markers, and listed under the editor.
- Bundled, not served as the packages' own modules: they import dozens of packages by bare name. `tools/vendor.sh` installs the versions `tools/vendor/package-lock.json` pins (npm checks each by hash) and bundles `tools/play-editor.js` with esbuild (`tools/vendor/build.mjs`) into `tools/vendor/editor/`, committed: a 355 KB entry (105 KB gzipped) and chunks loaded when needed. Shiki's own languages and themes are left out; only the Limen grammar (`editors/vscode/syntaxes/limen.tmLanguage.json`) and one theme in the book's code colours are registered. The colours are hex: the editor's live colouring cannot read CSS variables.
- The book's own pages keep `highlight_limen`.
- Examples: the book's single-file examples that `eval` runs (make_book.py asks the wasm compiler through `dev/wasm/run.mjs --batch`), after the playground's own in `tools/play-examples/` (Conway's Game of Life).

Behaviour pinned:
- Two workers: a checker (check then caps, 400 ms after typing stops, the newer request wins) and a runner (Run: `eval`). Checking keeps working while a program waits for input.
- Run: output pane clears, stdout plain, stderr marked. A line typed into the output pane's input field and Enter sends it with its newline; Ctrl-D or the End input button sends end of input.
- Stop terminates the runner and starts a new one from the cached `WebAssembly.Module`: message `Stopped.` Time limit 10 s, not counting time in `waiting`: `Stopped: the program ran for more than 10 seconds.` Output cap 1 MiB: `Stopped: the program printed more than 1 MB.` A trap: `Stopped: the playground ran out of memory or stack.`
- Caps pane: the capability list from `caps --json` for `main`; for Fs, Net, Listen, Env, Exec or Foreign, one sentence that the playground grants none of these, linking `ch12-capabilities.html`.
- Share: `#code=` plus base64url of `CompressionStream("deflate-raw")` of the UTF-8 source; loading a page with it fills the editor.
- Examples: `examples.json` is `[{ "name": "ch02-03-game", "title": "...", "code": "..." }]`, the book examples that Task 4's corpus marks as runnable (make_book.py runs `eval` through `dev/wasm/run.mjs`, or reads a list the limen repo's check writes; builder's call, one source of truth).
- The header shows the version (`?v=` on the wasm URL is the same version).

- [ ] **Step 1:** Write `play-highlight.test.mjs` (the book examples' tokens, as above, and a diagnostic rendered as an annotation on its line) and `play.test.mjs` driving `play-worker.js` in a node `worker_threads` Worker: check of a broken program returns its diagnostics JSON; eval of `ch01-01-hello` streams `Hello, world!`; eval of the guessing game with stdin lines fed through the buffer matches `examples/ch02-03-game/expected.txt`'s run part; end of input on `read_line` gives `None`; an endless loop is stopped by the 10 s limit while a program blocked 15 s on stdin is not; printing forever is stopped at 1 MiB; output containing `é`, CJK and an emoji is exact; the Fs example from chapter 14 gets the refusal.
- [ ] **Step 2:** Run `node --test tools/`. Expected: fails (no worker).
- [ ] **Step 3:** Run `sh tools/vendor.sh`; implement the worker, page, the Pierre and Shiki highlighting and the `make_book.py` changes.
- [ ] **Step 4:** Run `node --test tools/` and `make book`. Expected: pass; `dist/play/` holds the page, scripts, `vendor/`, `limen.tmLanguage.json`, `examples.json`, `wasi.mjs`, `limen.wasm.gz`.
- [ ] **Step 5:** Commit `play: the playground page` in limen-book (with `dist/`).

### Task 8: Serving `/play` (limen-book)

**Files:**
- Modify: `server.lmn` (`book` routing, `file`, `content_type`, tests at the end of the file)
- Modify: `tools/page.html` and `tools/home.html` (a Playground link in the book's navigation and on the landing page)
- Regenerate: `build/server.c` (`make c`)

**Interfaces:**
- Consumes: `dist/play/*` from Task 7.

Routing on the book host: `/play` and `/play/` serve `play/index.html`; `/play/limen.wasm` serves the embedded `play/limen.wasm.gz` with `Content-Type: application/wasm` and `Content-Encoding: gzip`; `.mjs` and the vendored `.js` are `text/javascript; charset=utf-8`, `.json` is `application/json`; `/play/vendor/` serves the vendored modules from `dist/play/vendor/`. Every response under `/play` also carries `Cross-Origin-Opener-Policy: same-origin`, `Cross-Origin-Embedder-Policy: require-corp`, `Cross-Origin-Resource-Policy: same-origin`; no other path does. The page and the shared assets it loads from outside `/play` (`tokens.css`, `book.css`, `fonts/`) carry `Cross-Origin-Resource-Policy: same-origin` too, so COEP accepts them. `limen-lang.org/play` reaches it through the landing host's existing redirect.

- [ ] **Step 1:** Add server tests: `/play` is 200 with the three headers; `/play/limen.wasm` has the wasm type and gzip encoding and its body is the embedded bytes; `/ch01-getting-started.html` has no COOP/COEP; `limen-lang.org/play` is a 301 to `book.limen-lang.org/play`; the ETag/304 path still works for the wasm.
- [ ] **Step 2:** Run `make test`. Expected: the new tests fail.
- [ ] **Step 3:** Implement the routes and headers; add the links; `make c`.
- [ ] **Step 4:** Run `make test`, then `make serve` and open `http://localhost:8080/play` in the in-app browser: check a program, run the guessing game typing guesses, Stop an endless loop, open a Share link, switch light and dark. Record the `server.c` size before and after, and the gzipped wasm size, in the commit message.
- [ ] **Step 5:** Commit `play: serve the playground` in limen-book (with `build/server.c`). Do not push.
