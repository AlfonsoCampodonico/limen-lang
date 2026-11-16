# Roadmap

Where Limen goes from 0.3.2 to 1.0, one release at a time. Each release has
one goal and a test for when it's done; the items under it are what stands
between here and that test. There are no dates: this is a one-person
project, and the order matters more than the calendar.

The short version: make a Limen server safe to run on the internet, then
make the language less tedious to write, then make it pleasant to debug,
then harden it, then promise not to break it.

Updated 2026-11-16, after 0.3.2.

## 0.4: a server you can leave running

A Limen web service, exposed to the internet, that stays up and keeps its
memory flat.

- HTTP server timeouts: for the request head, the body, idle keep-alive time
  and the whole connection, through `Clock`, with defaults and a way to set
  them. Today one slow client holds a task for as long as it likes.
- `serve_forever` keeps a record per finished connection, because a group's
  value is the list of every task's result. A long-running group needs a way
  not to collect them.
- `json.Int(Int)`, so ids and counts above 2^53 survive the trip out.
- SQLite back to its old speed: a scan of 102k rows went from 11 ms to about
  65 ms when the calls moved off the worker threads. One pool job per query
  instead of one per row (work in progress in `limen-sqlite-wip/`).
- `full_path` hands C code (SQLite) a path string that can still race a
  directory swap after the check.
- Fuzz the HTTP parser and JSON, the two things a server reads from
  strangers.
- CI on real x86-64 Linux and macOS. Only arm64 Linux runs today, in Docker.

Done when `examples/todo_api.lmn`, deployed, survives a slowloris client and
a day of load with flat memory.

## 0.5: less to write

The things newcomers hit in their first hour.

- Converting a struct to and from JSON (and SQL rows) without writing every
  field by hand. Today that is a third of `todo_api.lmn`.
- Closure parameter types inferred inside a tuple in a list literal, so
  `[("GET /", |r, p| ...)]` checks.
- Hex and binary literals (`0xff`, `0b1010`) and a `\u{...}` string escape.
- Default parameter values.
- Struct patterns in `let` and `for`.

Done when `todo_api.lmn` is noticeably shorter with nothing hand-written
that the compiler could have written.

## 0.6: debugging

Build speed is done. The runtime is compiled once and cached, with its
declarations precompiled; a program's C is cut into pieces that compile in
parallel; development builds (`limen build --dev`, `limen run --native`,
`limen test --native`) skip optimisation. Hello world builds in 0.05 s and
`todo_api.lmn` in 0.65 s optimised (it took 4 s), 0.2 s with `--dev`.

- `#line` directives in the generated C, so lldb, gdb, the sanitizers and
  crash traces point at Limen lines.
- Incremental checking for the language server.
- A profiler and test coverage.

Done when a crash in a native build shows the Limen line that caused it.

## 0.7: streams and the libraries people ask for

- Lazy sequences, so a large file goes through a pipeline line by line
  instead of being read whole. Decide between an iterator interface the
  pipelines fuse over and generators.
- Streaming decompression.
- A Postgres client.
- WebSockets.
- Encryption and public-key signatures beyond TLS.
- Integer types besides `Int` outside FFI (bytes, `u32`) and a character
  type with Unicode beyond `chars()`.

Done when a program can read a gzipped log larger than memory, line by
line, and write rows to Postgres.

## 0.8: hardening, and the breaking decisions

Everything that would break programs has to be decided before 1.0, so it
happens here.

- The design questions: shared mutable references (stateful code threads
  state by hand today, as the self-hosted compiler shows); E0304, which rules
  out callback tables while generics allow them; what a group returns.
- One definition of each built-in. Each is written twice, in the
  interpreter and the C runtime, and only the tests keep them in step: the
  NUL-path difference came from this.
- Bound authority through bounds of bounds, not just an interface's own
  methods.
- `share`, `==`, printing and dropping `dyn` values without recursion, so a
  deep value can't overflow a task's stack.
- The JSON edges: a lone `\ud800`, duplicate keys, a nesting limit, numbers
  over 400 characters.
- Fuzz the lexer, parser, checker and the rest of the runtime.
- An outside security review of the sandbox and the capability checks.
  Two reviews in two days found ten holes, so there are more.

Done when the review's findings are fixed and nothing on the list of
breaking changes is still open.

## 0.9: installing and sharing

- A signed apt repository, so `apt upgrade` updates Limen.
- Homebrew core, and Intel Macs.
- One release script: build all three, write `SHA256SUMS`, upload, bump the
  formula. Today it is three scripts and two steps by hand.
- A hosted package index as the default (it exists, but is empty and you
  point `LIMEN_INDEX` at one).

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

## Known and not planned

- Reference cycles through `Shared` or channels leak (SPEC.md section 8).
  They are rare and can only be built on purpose; a cycle collector would
  cost every program for them.
- Stack overflow can't be caught, and a task's buffered output is lost when
  it happens.

## Smaller, whenever

- The interpreter starts a thread per blocking C call instead of using a
  pool.
- A program cut into pieces calls its own functions across pieces as
  ordinary external calls, so the C compiler can't change how they are
  called. One piece keeps everything static; a large program could keep
  static what no other piece calls.
- No cancellation checkpoints inside a single built-in operation (a sort) or
  in the fast path of fill loops.

## Before any change

`dune build`, `dune exec ./test/test_limen.exe`, `sh selfhost/check.sh` and
`sh selfhost/bootstrap.sh` must pass, and every change goes into both
compilers (`src/` and `selfhost/`).
