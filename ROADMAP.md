# Roadmap

What Limen still needs to be a full language, in the order I'd do it. Written
on 2026-11-15, after 0.2.0. The design doc has the history and the decisions;
this is the list to pick up from.

Before starting any of it: `dune build`, `dune exec ./test/test_limen.exe`,
`sh selfhost/check.sh` and `sh selfhost/bootstrap.sh` must pass, and every
change goes into both compilers (`src/` and `selfhost/`).

## 1. HTTP server timeouts

The most serious gap for anyone running a Limen server. Today an idle
keep-alive connection holds its task until the client closes it, and
`serve_forever` keeps a record per finished connection until it returns
(SPEC, `http` section). Add a read timeout for the request head and body, an
idle timeout between keep-alive requests, and a total cap per connection,
through `Clock`, with defaults and a way to set them.

## 2. What newcomers hit first

- Hex and binary literals: `0xff` and `0b1010` are syntax errors today. The
  hash constants in `src/std/hash.lmn` are written in decimal because of it.
- Default parameter values.
- Lazy sequences: lists are always built in full, so a large file can't be
  streamed line by line through a pipeline. Decide between an iterator
  interface the pipelines fuse over and generators.
- Smaller: integer types besides `Int` outside FFI (bytes, `u32`), a
  character type, Unicode beyond `chars()`, struct patterns in `let` and
  `for`, a `\u{...}` string escape.

## 3. Debugging and build speed

- `#line` directives in the generated C, so lldb, gdb, the sanitizers and
  crash traces point at Limen lines.
- Separate compilation: build the runtime and the standard library once and
  link them, with `-O0` for development builds. A cold build of an HTTP
  program is 2 to 3 s today, nearly all of it the C compiler.
- A profiler and test coverage.

## 4. Open source, licence, CI, fuzzing

- Pick a licence. Without one the published binaries are all rights
  reserved.
- Decide on opening the source (repo `limen-lang` is private). It also
  decides homebrew-core and Debian or Ubuntu packaging, which need public
  source.
- CI on real x86-64 Linux and macOS. `dev/linux/run.sh` covers arm64 Linux
  in Docker; amd64 runs only emulated, where ThreadSanitizer can't start.
- Fuzz the lexer, parser, checker and runtime (HTTP parsing, compression,
  JSON). Then an outside security review: five sandbox findings in a row on
  2026-11-15 say there are more.

## 5. Platforms

- Windows: fibers, the poller and the sandbox are macOS and Linux only.
- Intel Macs: no build yet.
- Cross-compilation and a WebAssembly target.

## Open from 2026-11-15

- SQLite got slower when its calls moved off the worker threads: a full scan
  of 102k rows went from 11 ms to about 65 ms. The fix is one pool job per
  query instead of one per row. Work in progress is in
  `~/Github/Personal/limen-sqlite-wip/` (a patch and `limen_sql.h`).
- The interpreter starts a thread per blocking C call instead of using a pool.
- `limen caps` prints `pure` for a function that takes an `Atomic`, a `Shared`
  or a channel, though such a function is not pure.
- No checkpoints in a single built-in operation (a sort) or in the fast path
  of fill loops, so those can't be cancelled midway.

## Distribution

- A signed apt repository, so `sudo apt install limen` and `apt upgrade`
  work. Needs a GPG key that stays on the maintainer's machine.
- One release script that builds macOS and both Linux architectures, writes
  `SHA256SUMS`, uploads to the release and bumps the Homebrew formula.
  Today it is three steps: `dune build --profile release` on the Mac,
  `dev/release/linux.sh VERSION`, then `gh release` by hand.
- The package index exists but is empty and has no hosted default.
- A stability promise: what 1.0 means and how versions change after it.

## Standard library

- Postgres and MySQL clients (only SQLite today).
- WebSockets.
- Encryption and public-key signatures beyond TLS.
- Streaming decompression.
