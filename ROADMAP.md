# Roadmap

Where Limen goes from 0.3.3 to 1.0, one release at a time. Each release has
one goal and a test for when it's done; the items under it are what stands
between here and that test. There are no dates: this is a one-person
project, and the order matters more than the calendar.

The short version: make a Limen server safe to run on the internet, then
make the language less tedious to write, then make it pleasant to debug,
then harden it, then promise not to break it.

This file is the plan. The design doc keeps the reasons and the history of
what was done; its older lists of what was missing point here.

Updated 2026-11-27, after 0.8.1.

## 0.4: a server you can leave running (done, 0.4.0)

A Limen web service, exposed to the internet, that stays up and keeps its
memory flat. `examples/todo_api.lmn`, run natively: 500 slowloris clients
each got 408 at 10 s while normal requests answered in under 8 ms, and 11.5
hours of load (136 million requests, no errors) held the server at 4 MB. The
run was meant to be 24 hours; the machine restarted first. Deployed on Cloud
it served the API, and Cloud's proxy dropped slow clients at 15 s without
slowing anyone else.

What made it: HTTP server time limits (`http.server(clock)`), each SQL
statement as one blocking call, finished tasks freed at once so
`serve_forever` stays flat, `json.Int`, fuzzers for the HTTP server and JSON,
and CI on x86-64 and arm64 Linux and on macOS.

## 0.5: less to write (done, 0.5.0)

The things newcomers hit in their first hour.

- Converting a struct to and from JSON and SQL rows without writing every
  field by hand: `json.encode(x)` and `json.decode(s)` (shipped early in
  0.4.0), and `db.all(text, params)`, `db.one(text, params)`,
  `sql.decode(row)` and `sql.values(x)`. The compiler writes the
  conversion for each type they are used with.
- Closure parameter types inferred inside a list or tuple literal, so
  `[("GET /", |r, p| ...)]` checks where the type is known.
- Hex and binary literals (`0xff`, `0b1010`) and a `\u{...}` string escape.
- Default parameter values, constants only: `fn join(xs: List[Str], sep:
  Str = ", ")`. The standard library uses them where callers passed the
  same thing every time (`db.exec("CREATE TABLE ...")`).
- Struct patterns in `let` and `for`: `for Todo { id, title, .. } in todos`.

`todo_api.lmn` went from 394 lines in 0.4.0 to 351, with no JSON or row
code written by hand.

## 0.6: debugging

Build speed is done. The runtime is compiled once and cached, with its
declarations precompiled; a program's C is cut into pieces that compile in
parallel; development builds (`limen build --dev`, `limen run --native`,
`limen test --native`) skip optimisation. Hello world builds in 0.05 s and
`todo_api.lmn` in 0.65 s optimised (it took 4 s), 0.2 s with `--dev`.

- `#line` directives in the generated C, so lldb, gdb, the sanitizers and
  crash traces point at Limen lines. Done: development builds mark every
  Limen statement and compile with `-g`; under lldb a crash in C called
  from Limen shows the Limen calls that led to it. Variables are named
  after their Limen names in the C (`total_15`), so a debugger shows them.
- Incremental checking for the language server. Done in part: diagnostics
  go out before the reference index is built (on demand now), of edits
  that arrive faster than a check only the newest is checked, files whose
  text didn't change aren't parsed again, and a change rechecks the other
  open files that use it. A change still checks the whole program: 68 ms
  for the self-hosted compiler (85 ms before the parse cache), 10 ms for
  `todo_api.lmn`. Checking only the functions an edit touches is left, for
  when a program is large enough to need it.
- A profiler and test coverage. Done: `limen build --profile` (own CPU
  time and calls per function, sampled) and `limen test --coverage`.

Done when a crash in a native build shows the Limen line that caused it
(it does, in a development build).

## 0.7: streams and the libraries people ask for

- Lazy sequences, so a large file goes through a pipeline line by line
  instead of being read whole. Decided: folds, as `http.request_fold`
  already does, since Limen has neither mutable iterators nor generators
  and a fold needs neither. Done: `fs.read_at` and the `stream` module.
- Streaming decompression. Done: `compress.fold_gunzip` and
  `stream.fold_gzip_lines`.
- A Postgres client. Done early, with Redis: `postgres` and `redis` in
  the standard library, written in Limen, with pools and TLS (Postgres
  through `Conn.start_tls`, verified by default).
- WebSockets. Done: the `websocket` module, server and client.
- Encryption and public-key signatures beyond TLS. Done: the `crypto`
  module, ChaCha20-Poly1305, Ed25519 and X25519 on OpenSSL, with no
  algorithm to choose.
- Integer types besides `Int` outside FFI (bytes, `u32`) and a character
  type with Unicode beyond `chars()` (case folding, normalisation). Done:
  `UInt8` to `UInt64`, wrapping, never mixed with `Int`. Decided: no
  character type; a character stays a one-character `Str` and its number
  an `Int`, and the `unicode` module (Unicode 17.0) folds case, maps it in
  full and normalises to NFC and NFD. A long `match` on integer literals
  now compiles to a C `switch`, which its tables needed.
- C callbacks that C can keep and call later, for libraries that call back
  on their own schedule. Done: `Kept[fn(...)]`, whose argument is a
  channel. Each call from C, from any thread, sends its arguments on it
  without waiting, so no Limen code runs on C's threads.

Done when a program can read a gzipped log larger than memory, line by
line, and write rows to Postgres.

## 0.8: hardening, and the breaking decisions (done, 0.8.0, without the outside review)

The items marked done below shipped early, in 0.7.3 and 0.7.4.

Everything that would break programs has to be decided before 1.0, so it
happens here.

- The design questions, decided: Limen never gets references that can be
  stored; functions in fields (done, shipped early in 0.7.3; E0304 is no longer
  reported); a group
  keeps its list of results, so a group that runs for ever uses tasks that
  return nothing and a channel (documented).
- One definition of each built-in. Each is written twice, in the
  interpreter and the C runtime, and only the tests keep them in step: the
  NUL-path difference came from this (done: the built-ins whose answer
  depends on bytes, Unicode, parsing, float formatting, scalar order, random
  numbers or a panic message are written once, in `src/runtime/limen_core.h`,
  and called by both backends; the rest are checked against each other on
  every test run by generated programs, `LIMEN_CROSSCHECK_SEED` for more
  inputs. Behaviour changes for the release notes: natively `"\r".lines()` is
  `[]` and `"a\n\r".lines()` is `["a"]`, as SPEC.md says; in the interpreter
  `UInt64.to_float()` rounds once, so 2^63 + 1025 gives
  9223372036854777856).
- Bound authority through bounds of bounds, not just an interface's own
  methods (done: the `dyn` interfaces a bound's methods take or return are
  followed to any depth, outermost first, each interface entered once
  whatever its arguments, and `limen caps` and E0320 name the path,
  `Outer.get, Inner.give`).
- `share`, `==`, printing and dropping `dyn` values without recursion, so a
  deep value can't overflow a task's stack (done: each recurses a few dozen
  levels and keeps the rest on a work stack, natively; the interpreter's
  printing no longer takes quadratic time on a deep value).
- The same for the walks still left recursive: ordering (`<`, `sort`) and
  hashing a deep map key, natively and in the interpreter, and a check of
  `json.encode` on a deep value (done: natively ordering and hashing keep the
  rest on a work stack as `==` does, with the same answers and hashes; the
  interpreter's walks already fit OCaml's growing stack; `json.stringify`
  writes with a list rather than recursion, and no longer takes quadratic
  time on a deep value; the conversions `json.encode` and `json.decode`
  write for a type that holds itself encode in pieces left for later and
  decode a step at a time, so they no longer recurse once per level).
- The JSON edges: a lone `\ud800`, duplicate keys, a nesting limit, numbers
  over 400 characters (done: each is a parse error, and the parser reads
  nesting without recursion, so it can't overflow a task's stack).
- Fuzz the lexer, parser, checker and the rest of the runtime (done:
  `fuzz/front` runs mutated programs through both compilers' front ends,
  and `fuzz/formats.lmn`, `regexes.lmn`, `inflate.lmn` and `ws/frames.lmn`
  the readers of URLs, CSV, base64, hex, UUIDs, times, Unicode, regexes,
  gzip, TZif and WebSocket frames; what they found is fixed, and
  expressions, types and patterns now nest at most 256 deep, E0010, and
  a chain of operators, calls or `else if`s has at most 50,000 links,
  E0011).
- `full_path` hands SQLite a path string that another process with write
  access to the granted directory could race with a symlink swap after the
  check. The OS sandbox (Landlock, the macOS profile) refuses what such a
  swap would reach (decided: SQLite already refuses links in place of its
  side files and a moved database; the one case left needs write access
  outside the grant, which the OS sandbox refuses, so no VFS).
- An outside security review of the sandbox and the capability checks.
  Two reviews in two days found ten holes, so there are more. Not done: 0.8.0
  shipped without it, and it moves to 0.9.

Done when the review's findings are fixed and nothing on the list of
breaking changes is still open.

## `mut` parameters (done, in 0.8)

A function changes its caller's value through a `mut` parameter, without a
stored reference: `fn roll(mut seed: Int) -> Int` is called `roll(mut seed)`,
and a `mut self` method is called `buf.add(1)`, as `push` is. The argument is
a place whose root is `mut`, of exactly the parameter's type, and a `mut`
parameter takes no default. Three new errors: E0213 when an argument's `mut`
does not match its parameter (with a fix), E0214 when the `mut` places of one
call overlap or another argument changes one of them, and E0215 when a
function with a `mut` parameter is used as a value. Both compilers check it,
and both backends run it: the interpreter passes the caller's variable, or
a cell it writes back to a field or element however the call ends, and the C
backend a pointer to the caller's place, so a value held once is still
changed in place. Limen still never gets references that can be stored:
storing, returning, capturing or sending a `mut` parameter takes a copy. Not
yet: `mut` parameters in interface methods, foreign functions and function
types (SPEC.md section 20).

The self-hosted compiler threads its state through `mut` parameters, the
proof that they are enough: the statements that take a result and a state
back out of a tuple, `let (x, st) = f(st, ...)`, went from 738 to 62 across
the checker, the lowering, renaming, numbering, ownership, C generation and
the loader. The parser and lexer keep threading their state by design.

## Kubernetes without a proxy (not started)

`examples/site_operator.lmn` is a working operator, but it reaches the API
server through `kubectl proxy`, which holds the credentials. An operator
should run in a pod with a grant for the API server and nothing else, and
`limen caps` should show that it can reach nothing more.

- A `yaml` module, reading and writing the YAML that manifests are written
  in, with `yaml.encode` and `yaml.decode` written by the compiler as
  `json`'s are.
- In-cluster authentication: the pod's service-account token and CA
  certificate, and the kubeconfig outside a cluster, read through a granted
  `Fs`; TLS with a custom CA and client certificates.
- Typed Kubernetes objects (Deployment, Service, ConfigMap and the rest) as
  a package, generated from the API's OpenAPI description, so manifests are
  checked instead of built as JSON by hand.
- gRPC and protobuf, for the APIs that speak them.

Done when `site_operator.lmn` runs as a Deployment, with one grant for the
API server, and no proxy.

## 0.9: installing and sharing

- The outside security review of the sandbox and the capability checks,
  carried over from 0.8.
- A signed apt repository, so `apt upgrade` updates Limen.
- Homebrew core, and Intel Macs.
- One release script: build all three, write `SHA256SUMS`, upload, bump the
  formula. Today it is three scripts and two steps by hand.
- A hosted package index as the default (it exists, but is empty and you
  point `LIMEN_INDEX` at one).
- The self-hosted compiler as the default for `check`, `caps` and `build`,
  shipped as one generated C file, so building Limen needs only a C
  compiler. Then decide whether the interpreter, formatter, language
  server, `doc` and the MCP server move to Limen or stay in OCaml.
- A playground and short videos beside the book. The playground is done
  (shipped early, in 0.8.1, at book.limen-lang.org/play): the self-hosted compiler builds to WebAssembly
  (`dev/wasm/build.sh`, about 1.6 MB gzipped) and runs in node and in a
  browser through one WASI shim (`dev/wasm/wasi.mjs`), with the native
  compiler's `check --json` and `caps --json`; `limen eval` runs a program
  by interpreting its IR, with the native build's output, panics and
  exit status, or refuses it (tasks, channels, select, C, and the Net,
  Fs, Listen, Env, Exec and Foreign capabilities); `dev/release/wasm.sh`
  writes the two files a release adds. `eval` is only in the self-hosted
  compiler: the one deliberate exception to every change going into both
  compilers. The book's `/play` page runs it in a worker. Left: the
  short videos.

Done when someone can install Limen, add a package and publish their own
without reading anything but the book.

## 1.0

A stability promise: code that builds on 1.0 builds on every 1.x, what
counts as breaking, and how editions or deprecations work after it. The
book and SPEC.md describe 1.0 exactly.

## After 1.0

- Windows. Fibers, the poller and the sandbox are macOS and Linux only.
- Cross-compilation and a WebAssembly target.
- MySQL.
- Lower-level code: integer types of a fixed size (`Int8` to `Int64`
  beside the `UInt` types) outside C calls, and byte buffers that change in
  place. Limen stays a reference-counted language without raw pointers:
  calling C, under the `Foreign` capability, stays the way to go lower.

## Known and not planned

- Reference cycles through `Shared` or channels leak (SPEC.md section 8).
  They are rare and can only be built on purpose; a cycle collector would
  cost every program for them.
- Stack overflow can't be caught, and a task's buffered output is lost when
  it happens.

## Smaller, whenever

- The interpreter starts a thread per blocking C call instead of using a
  pool (done: blocking calls share a pool of up to
  `LIMEN_BLOCKING_THREADS` threads, as they do natively).
- A program cut into pieces calls its own functions across pieces as
  ordinary external calls, so the C compiler can't change how they are
  called. One piece keeps everything static; a large program could keep
  static what no other piece calls (done: a function no other piece, the
  header or the variables name stays `static` in its piece).
- No cancellation checkpoints inside a single built-in operation (a sort) or
  in the fast path of fill loops (done: a cancelled sort leaves the list as
  it was, in both backends).
- `Fs` can't set the executable bit or file times, or read a link, so the
  build cache still runs `cp`, `touch` and `readlink` through `Exec` (done:
  `Fs.copy`, `touch`, `set_executable` and `read_link`, and the build cache
  no longer runs `cp` or `touch`; the self-hosted compiler still runs
  `readlink` for Xcode's link, which is outside its `Fs` grant).

## Before any change

`dune build`, `dune exec ./test/test_limen.exe`, `sh selfhost/check.sh` and
`sh selfhost/bootstrap.sh` must pass, and every change goes into both
compilers (`src/` and `selfhost/`).
