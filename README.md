# Tessera

A small, statically typed language where code can only touch the outside world through capabilities it was handed. There is no global `print`, `open`, `http.get` or `time.now`; `main` receives a `Root` value and passes narrower capabilities to the functions that need them.

The full language reference is [SPEC.md](SPEC.md). The compiler is written in OCaml with no dependencies beyond the standard distribution. It has a type checker with JSON diagnostics, a tree-walking interpreter, a native backend that compiles to C, and structured concurrency on a thread pool.

```
fn main(root: Root) -> Result[(), IoError] {
    let gh = root.net.only("api.example.com")   // narrower capability
    let names = fetch_repos(gh, "rust-lang")?
    root.stdout.print("{names.len()} repos")
    Ok(())
}

fn fetch_repos(net: Net, org: Str) -> Result[List[Str], IoError] {
    let resp = net.get("http://api.example.com/orgs/{org}/repos")?
    Ok(parse_names(resp.body))
}

// No capability parameters: this function cannot do I/O.
fn parse_names(body: Str) -> List[Str] { ... }
```

## Build and run

Needs OCaml 5 and dune (`brew install ocaml dune`), plus a C compiler (`cc`, or set `CC`) for the native backend, and OpenSSL for TLS (`brew install openssl@3` on macOS, `libssl-dev` or `openssl-devel` on Linux; `TESSERA_OPENSSL=PREFIX` points at another install). Without OpenSSL everything but TLS still works. `Net.get` speaks HTTP/1.1 itself, http and https.

```bash
dune build
```

```bash
dune test
```

`dune test` runs every program in the interpreter and natively under AddressSanitizer (and ThreadSanitizer for the concurrent ones). The native builds and runs go in parallel, one per core (`TESSERA_JOBS=N` to change that), and compiled test binaries are cached in `~/.cache/tessera/test-native` (or `$TESSERA_CACHE/test-native`), keyed by the compiler, its flags and the C source, so a run that changes no generated C compiles nothing; `TESSERA_TEST_CACHE=0` turns the cache off. `TESSERA_TEST_ONLY=prefix,...` runs only the matching program tests, `TESSERA_SKIP_NATIVE=1` skips the native half, and `TESSERA_TEST_SLOW=1` lists the slowest native jobs.

```bash
dune exec tessera -- run examples/repos.tsr --grant net=api.github.com
```

```bash
dune exec tessera -- build examples/repos.tsr -o repos && ./repos --grant net=api.github.com
```

## Linux

`dev/linux/run.sh` builds Tessera in a Debian container (OCaml 5.3, gcc, OpenSSL, SQLite) and runs the full test suite and grammar check there; `dev/linux/run.sh amd64` does the same for x86-64. Linux on arm64 passes everything. On an arm64 Mac, amd64 runs under emulation, where the native programs, fibers and epoll work but ThreadSanitizer cannot start and a missing program run through `Exec` exits 127 instead of failing; those checks need real x86-64 hardware.

## Try it

Put `tessera` on your PATH once (`dune install` copies it into your opam switch), or alias it to the build:

```bash
alias tessera="$PWD/_build/default/bin/main.exe"
```

Then work through the examples. Every one runs the same in the interpreter and natively (add `--native`):

| Example | Shows | Run |
| --- | --- | --- |
| `tour.tsr` | the newest features in one file: `dyn` branches, ranges, method values, struct map keys, `Bytes`, `defer`, `dyn Error` and `?` with context, groups with deadlines, `select` timeouts, exit codes, tests, table tests, benchmarks | `tessera run examples/tour.tsr; echo $?`, `tessera test examples/tour.tsr` and `tessera test examples/tour.tsr --bench` |
| `hello.tsr`, `wordcount.tsr` | capabilities: nothing touches the outside world without a grant | `tessera run examples/wordcount.tsr --grant fs=examples:ro` |
| `methods.tsr`, `interfaces.tsr`, `dyn.tsr`, `shapes.tsr` | methods, structural interfaces, `dyn` values | `tessera run examples/dyn.tsr` |
| `tuples.tsr`, `maps.tsr` | tuples, destructuring, maps | `tessera run examples/maps.tsr` |
| `parallel.tsr`, `select.tsr` | tasks, groups, channels, `select` | `tessera run examples/select.tsr` |
| `stdlib.tsr`, `report.tsr`, `logscan.tsr` | `json`, `format`, `time`, `regex` | `tessera run examples/logscan.tsr` |
| `http_demo.tsr` | an HTTP server and client in one program | `tessera run examples/http_demo.tsr --grant listen=127.0.0.1:0 --grant net=127.0.0.1` |
| `repos.tsr` | HTTPS to a real API | `tessera run examples/repos.tsr --grant net=api.github.com` |
| `notes.tsr` | SQLite through `sql` | see the comment at the top of the file |
| `todo_api.tsr` | a CRUD API: todos over HTTP and JSON, stored in SQLite, with tests that call the router directly | `tessera run examples/todo_api.tsr --grant listen=127.0.0.1:0 --grant net=127.0.0.1`, or `tessera test examples/todo_api.tsr` |
| `site_operator.tsr` | a Kubernetes operator: a StaticSite custom resource becomes a ConfigMap, an nginx Deployment and a Service, kept in place by server-side apply, with status written back; it watches the sites and its objects (`http.request_lines`), so it acts on a change as it happens, and can reach only `kubectl proxy` | see the file's header (needs a cluster, e.g. kind), or `tessera test examples/site_operator.tsr` |
| `ffi.tsr` | calling C | `tessera run examples/ffi.tsr --grant foreign=c` |

To write your own: `tessera check` gives errors with fixes, `tessera explain E0302` explains a code, `tessera fmt --write` formats, `tessera caps` lists what each function can touch, and `tessera doc --std --html -o std.html` documents the standard library. For an editor, point it at `tessera lsp`.

## Commands

| Command | What it does |
| --- | --- |
| `tessera run FILE [--native] [--grant net=HOST[:PORT]\|*]... [--grant fs=DIR[:ro]] [--grant listen=HOST[:PORT]]... [--grant env=NAME\|*]... [--grant exec=PROGRAM\|*]... [--grant foreign=LIB\|*]... [--seed N] [--no-sandbox] [-- ARGS...]` | Type-check, then run `main` in the interpreter, or compiled with `--native`. Stdout, Stdin, Clock and Rand are always granted; Net, Fs, Listen, Env, Exec and Foreign only when granted, by flags or by `[grants]` in tessera.toml. `--seed` makes `Rand` repeatable; arguments after `--` are `root.args()`. The exit status is 0, 1 after a panic or an `Err` from `main`, or the `Int` that `main` returns. |
| `tessera build FILE [-o OUT] [--emit-c]` | Compile to a native binary through C. The binary takes the same `--grant` flags. `--emit-c` writes the C instead. Binaries are cached by the exact C, compiler flags and compiler (in `~/Library/Caches/tessera`, or `$XDG_CACHE_HOME/tessera` or `~/.cache/tessera`; the 200 most recently used), so rebuilding an unchanged program, or `tessera run --native` on one, takes milliseconds. `TESSERA_CACHE=DIR` puts the cache elsewhere, `TESSERA_CACHE=off` compiles every time. |
| `tessera check FILE [--json]` | Type-check only, writing nothing (tessera.lock included). Errors and warnings close with their count (`2 errors, 1 warning`). `--json` emits diagnostics with codes, spans, notes and machine-applicable fixes. |
| `tessera caps FILE [--json]` | List each function's authority: the capabilities in its signature, or `pure`; then the files it embeds. |
| `tessera explain CODE` | Explain an error code, for example `tessera explain E0302`. |
| `tessera test FILE [--native] [--run TEXT] [--json]` | Run the file's `test "name" { ... }` blocks, each isolated, with their times. A test that declares `(fs: Fs)` gets a fresh temporary directory; one that declares `(clock: virtual Clock)` runs on virtual time. `--run` picks tests by name; `--json` reports for tools and agents. |
| `tessera test FILE --bench [--bench-time 1s\|100x]` | Also run the `bench "name" { ... }` blocks, natively, and report ns/op and allocations/op. |
| `tessera fmt FILE... [--write\|--check]` | Print the canonical formatting, rewrite files, or check them. |
| `tessera doc FILE\|MODULE... [--std] [--all] [--html] [-o OUT]` | Documentation from doc comments (the `//` lines directly above an item) and signatures, as Markdown or one static HTML page. `tessera doc --std --html -o std.html` documents the standard library. |
| `tessera lsp` | Language server on stdin/stdout. |
| `tessera mcp` | MCP server for coding agents on stdin/stdout. |
| `tessera spec` | Print the language specification. |
| `tessera search [WORDS...]` | Search the package index named by `TESSERA_INDEX`: each matching package's description, latest version and capabilities. |
| `tessera add NAME[@VERSION]` | Add a package from the index to tessera.toml as a git dependency, with `caps` set to what the index lists; refuses a moved tag or code that takes more. |
| `tessera index entry [DIR]` | Print the index entry for the package's current tag, with the capabilities computed from its code. |

## Tooling for editors and models

- **Language server (`tessera lsp`):**
  - diagnostics as you type
  - hover with types, for example `let total: Int` or `print(Str) -> ()`, and for a function, method or type its declaration and doc comment
  - go to definition for functions, methods, locals, parameters, fields, types and variants, into other modules too (a standard library module opens as a temporary copy)
  - find references across the modules the file uses
  - rename, refused with a reason when it is unsafe: the new name must be a valid name of the right case, the symbol must be defined in the program (not built in or in the standard library), interface methods and methods an interface may rely on are left alone, and the renamed program must check with no new errors and with the new name referring to exactly the renamed places (so a rename never captures or is captured by another name). `S { name }` shorthand is expanded where needed.
  - completion: members after `.` (fields, methods, capabilities on `root`, and after a module name such as `json.` its public functions, types and variants), otherwise locals, functions, types and keywords
  - the checker's auto-fixes as quick fixes
  - an outline of the file

  Positions count bytes, which is exact for ASCII source.
- **Grammar ([grammar/tessera.gbnf](grammar/tessera.gbnf)):** in llama.cpp's GBNF format, for constrained decoding. It accepts every example, benchmark and test-suite program, and rejects the hand-written invalid cases. It is slightly more permissive than the parser: keywords aren't excluded from identifiers. It requires a space after keywords (`if x {`), which is the canonical style.
- **Formatter (`tessera fmt`):** keeps comments where they were written, including inside an expression (a comment in a one-line argument list puts it one argument per line; one after a binary operator breaks the line there), number and string spellings, and the author's line breaks in lists and argument lists. The test suite formats every program and checks that the tree is unchanged, a second pass changes nothing, and no comment is lost.
- **Tests (`tessera test`):** `test "name" { assert_eq(...) }` blocks. They get no `root`; a test can declare `(fs: Fs, clock: Clock)` to get a fresh temporary directory, removed afterwards, and the real clock, and nothing else. `clock: virtual Clock` gives a clock on virtual time instead: it starts at 2026-01-01T00:00:00Z and jumps to the next timer whenever every task is waiting, so sleeps, `select` timeouts and group deadlines take no real time and timeout logic gives the same result every run, in both backends. Each runs isolated in the interpreter, or as its own process with `--native`. `--run TEXT` runs only the tests whose name contains it, and `--json` prints the results as one JSON object.
- **Table tests:** `test "add" for (a, b, want) in [(1, 2, 3), (2, 2, 4)] { assert_eq(a + b, want) }` runs the body once per case, each case its own isolated test named by its text (`add (2, 2, 4)`), so a failure says which case. The cases are written out in place, which is what lets every case be named before anything runs.
- **Benchmarks:** `bench "name" { ... }` blocks, skipped by `tessera test` and run natively by `tessera test --bench`. As in Go, the iteration count grows until a round fills the time budget (`--bench-time 1s` by default, or `100x` for a fixed count); the report gives ns/op and allocations/op from the runtime's allocation counter. The body's value goes to `keep(x)`, which the C compiler cannot see through, so the work is not optimized away.
- **MCP server (`tessera mcp`):** tools to check, run, test, format, read a type, find references, rename (with the language server's safety checks), list capabilities, explain an error, and read the spec. `tessera_run` gives the program no grants, so code an agent just wrote can print but cannot touch files or the network, and it stops at a time limit.
- **Checking the grammar:** `python3 grammar/gbnf_check.py` checks files against it with an Earley recognizer that follows llama.cpp's grammar syntax and acceptance rule. `grammar/check-suite.sh` runs it over every program in the test suite. To check with llama.cpp's own GBNF parser and matcher (the code that does the constrained decoding), run `sh grammar/llama-check.sh`: it fetches llama.cpp at a pinned release into `~/.cache/tessera` (or `$TESSERA_CACHE`), builds only its library, and runs the accept and reject cases, the examples, the benchmarks and every test-suite program through [grammar/llama_check.cpp](grammar/llama_check.cpp). It needs git, cmake and a C++ compiler, and is not part of `dune test`. Homebrew's llama.cpp cannot be used for this: it ships no grammar validator and not the internal header one needs.

Neovim:

```
vim.lsp.start({ name = "tessera", cmd = { "tessera", "lsp" }, root_dir = vim.fn.getcwd() })
```

VS Code: [editors/vscode](editors/vscode) is a local extension with highlighting and the language server. Install it with `editors/vscode/install.sh` (needs Node and the `code` command).

## Capability rules

- `Root` holds `net`, `fs`, `listen`, `env`, `exec`, `foreign`, `stdout`, `stdin`, `clock` and `rand`. Only the runtime creates capabilities.
- Attenuation only narrows: `net.only(host)`, `fs.under(dir)`, `fs.read_only()`. Asking for more than you hold gives an empty capability.
- Capabilities and function types cannot be stored in struct or enum fields (E0301, E0304), so authority always shows up in signatures. Generic instantiations like `Box[Net]` are allowed because the capability is visible in the type.
- A function with no capability or function-typed parameters is pure.
- Runtime enforcement: `Net` is an http/https host allowlist, redirects are not followed, and other schemes are refused. TLS certificates are always verified against the system's trusted certificates (`SSL_CERT_FILE` replaces them, for tests). `listen.on_tls(addr, cert, key)` serves TLS from certificate and key text the program read through its own `Fs` (or `Env`), so no new grant is involved. `Fs` resolves paths under its root and refuses absolute paths, `..` escapes and symlinks that lead outside.
- Files a program ships with need no `Fs`: `embed("page.html")`, `embed_bytes("logo.png")` and `embed_dir("public")` (a `Map[Str, Bytes]` of every file under it) read them when the program is compiled. The path must stay inside the package that names it (its tessera.toml's directory, or a dependency's own), so a dependency cannot bake your files into its code (E0405). `tessera caps` lists every embedded file.

## Calling C

A `foreign` block declares C functions, and calling one takes the `Foreign` capability, which exists only with `--grant foreign=LIB` (per library, or `*`):

```
foreign "c" {
    fn strtol(s: Str, end: Out[Handle], base: Int32) -> Int
}

fn main(root: Root) {
    let (n, _) = strtol(root.foreign, "42 apples", 10)    // --grant foreign=c
    root.stdout.print("{n}")
}
```

`Int`, `Int32`, `Float`, `Bool`, `Str` and `Bytes` (a pointer lent for the call), `Handle` (an opaque `void *`), `Out[T]` out-parameters, C structs declared in the foreign block (by value) and callbacks into top-level Tessera functions cross the boundary, and a variadic function (`printf`) is declared with the argument types it passes after `...`. Natively the library is linked with `-l<lib>`; the interpreter loads it with `dlopen`, so both backends run the same programs. C code runs with the process's full authority, so the foreign grant is treated as full trust: `tessera run` warns when it is given, a dependency may call C only if tessera.toml names it in `[trust] foreign = [...]` (recorded in tessera.lock, so an update that starts calling new C libraries stops the build), and `tessera check` lists every part of the program that calls C. The right shape is a small module that wraps the C calls in a safe API, like the `sql` module, which the standard library trusts with SQLite so programs using it grant only an `Fs`. SPEC.md section 9 has the type table, the ownership rules (callbacks included) and the limits.

## The sandbox

Every program also runs in an operating-system sandbox made from its grants, in both backends: files only under the `Fs` directory (plus system locations for reading), no network without `Net` or `Listen`, no other programs without `Exec`. That keeps C code reached through `foreign`, and any runtime bug, inside the grants too. Host and program allowlists stay in Tessera; the kernel enforces the coarse lines. macOS uses a sandbox profile and Linux uses Landlock plus seccomp (run on arm64; x86-64 is compiled but not run). `--no-sandbox` turns it off for debugging. SPEC.md section 9 has the details.

## Modules, packages and the standard library

`use json` loads `json.tsr` from next to the file, from a dependency, or from the standard library; its `pub` items are then `json.parse(...)`, `json.Value` and so on. Modules are merged into one program before checking, so both backends handle them unchanged.

A `tessera.toml` lists dependencies, by local path or by git URL at a tag or commit, and the capabilities each may use:

```
[dependencies]
fetch = { path = "../fetch", caps = ["net"] }
color = { git = "https://example.com/color.git", tag = "v1.2.0" }
```

Git dependencies are fetched with `git` into a local cache (`~/.cache/tessera`, or `$TESSERA_CACHE`). `tessera.lock` records the exact commit and a hash of the files of each one, and every build checks the cache against it, so a moved tag or a changed copy cannot slip in. `build`, `run` and `test` write the lock; `check` and `caps` only read it.

A package index, named by `TESSERA_INDEX` (a git URL or a directory; there is no default), lists packages by git URL and tag with the capabilities each needs, and hosts nothing. `tessera search WORDS` finds packages; `tessera add NAME[@VERSION]` writes the dependency into tessera.toml with `caps` set to what the index lists, prints them, and refuses if the tag moved since it was indexed or the code takes more than the index says. It never grants `foreign`: a package that calls C needs your own `[trust]` line. `tessera index entry`, run in a package's repository, prints the entry for its current tag, capabilities computed from the code; publishing is adding that entry to the index. SPEC.md section 18 has the format.

Since capabilities only arrive as parameters, a dependency's signatures are a complete list of what it can do. If one asks for a capability it was not granted, the program does not compile (E0320).

The same file can hold the program's own grants, so they are not retyped on every `tessera run` (flags still add to them). A dependency's own tessera.toml never grants anything:

```
[grants]
fs = "data:ro"
net = ["api.github.com"]
```

The standard library is written in Tessera: `json` (parse, stringify, field access), `http` (an HTTP/1.1 client over `Net` and a server over `Listener`: one task per connection, keep-alive, a serve-forever loop and streamed bodies), `list` (`sort_by` and friends), `text`, `format` (thousands separators, zero padding, hex and other bases, aligned columns; fixed decimals are `x.to_fixed(2)`), `time` (dates from epoch milliseconds, ISO 8601 formatting and parsing, strftime-style layouts, named zones such as Europe/Madrid read from the system's tz database through an `Fs`, and `within`, a deadline for any call that waits), `regex` (a Pike VM: no catastrophic backtracking; match, find, find all, captures, replace, split) and `sql` (SQLite through the system's libsqlite3: parameters bound by SQLite, typed row access, transactions; see [examples/notes.tsr](examples/notes.tsr)). The standard library is checked like a dependency: `src/std/tessera.toml` declares what each module may take (`http` takes `net` and `listen`, `sql` takes `fs` and is trusted to call SQLite, and `time` takes `clock` for `within` and `fs` for `load_zone`; the others take none), and a std function that takes more is a compile error. The capability methods themselves (`Net.get`, `Fs.read`, ...) stay trusted, since they are where grants are enforced. See [examples/http_demo.tsr](examples/http_demo.tsr), [examples/report.tsr](examples/report.tsr), [examples/logscan.tsr](examples/logscan.tsr) and SPEC.md sections 18 and 19.

## Concurrency

Tasks are fibers: small stacks scheduled by worker threads, one per core, with work stealing. A task waiting on a group, a channel, a socket, stdin or `clock.sleep(ms)` parks without blocking a thread. If every task is waiting and nothing can wake one, the program panics with a deadlock message instead of hanging.

A `group` block starts tasks with `g.go(closure)` and evaluates to the list of their results, in the order they were started. Tasks that return `()` run for their effect and add no item, so a group can mix them with tasks that compute a value. Every task has finished when the block ends, so nothing outlives it.

```
let counts = group g {
    for row in rows { g.go(|| count_row(row)) }
}
```

- Tasks share data by value. A value handed to a task is switched to atomic reference counting, together with everything it references. Values that stay on one thread keep plain counts, so single-threaded code pays nothing.
- A task can only touch the world through capabilities it captured. Spawning is not a capability, because it has no effect outside the program.
- The handle `g` can only be used as `g.go(...)` in the block's own body (E0310), and the body cannot `return` or use `?` to leave early (E0311).
- **Output order:** what tasks print does not depend on scheduling. A group's output reads as if its body ran first and then each task in start order: the first unfinished task prints directly and the later ones are held until it finishes (at most 4 MiB held in all). `group g interleaved { ... }` lets tasks that run until stopped, like a server's connections, print as they go; the `http` servers use it.
- **Channels:** `channel(n)` makes a `Chan[T]` with `send`, `recv` (returning `None` once closed and drained) and `close`; `channel(0)` is unbuffered, so a send waits for its receiver. Values sent are shared, not copied, across threads.
- **Sockets:** `root.listen.on("host:port")` (granted with `--grant listen=...`) gives a `Listener`; `accept` and `Net.connect` give a `Conn` with `read`, `write` and `close`. Addresses may be IPv6 (`[::1]:8080`, `http://[::1]/`); `connect` tries each address a name resolves to. `time.within(clock, ms, || net.get(url))` gives any call that waits a deadline. The native poller is kqueue on macOS and epoll on Linux (Linux untested).
- **Echo server:** [examples/echo.tsr](examples/echo.tsr) serves 10,000 concurrent connections, each kept open until all are connected, with every echo correct. Natively that takes about 2 s and 335 MB peak. Reproduce with `bench/echo_load.py`.
- **Interpreter:** runs tasks as coroutines on OCaml 5 effect handlers, so the same programs behave the same way.

## Native backend

`tessera build` lowers the typed program to a small IR, instantiates generics per concrete type, inserts reference counting, and emits C compiled with `cc -O3 -fwrapv -ffp-contract=off`.

- **Memory:** Perceus-style reference counting without a GC. A use that is a variable's last use becomes a move instead of a retain, so counts usually stay at 1 and `xs.push(v)` or `xs[i] = v` mutate in place. Shared values are copied on write, which keeps value semantics.
- **Layout:** Int, Float, Bool, structs and non-recursive enums are plain C values. Strings, lists, closures, capabilities and recursive enums are reference-counted pointers.
- **Fusion:** `xs.filter(p).map(g).fold(z, h)` becomes one loop with the closures inlined, but only when the earlier stages are pure closures (capturing no capability and no function value). Capabilities are what make purity provable here. A fused pipeline can only differ from the unfused one in which panic happens first.
- **Floats:** no fused multiply-add, so results match the interpreter bit for bit.
- **Loops:**
  - A list that a loop mutates but never shares is made unique once before the loop, so writes inside skip the copy-on-write check.
  - `for` loops read elements without bounds checks, since the loop's own `i < len` test already proves them.
  - A `while m < n` loop indexed by `m` is versioned: one range check before the loop decides whether the body can run without bounds checks.
  - A counted loop that only pushes a constant becomes one bulk fill.

The test suite runs every program in both the interpreter and the native binary, built with AddressSanitizer, and checks that outputs match and that no object is still alive at exit. Programs that use `group` also run under ThreadSanitizer with 8 threads.

## Performance

Best of 5 on an Apple Silicon Mac with 12 cores, against C compiled with `-O2 -fwrapv -ffp-contract=off`; `tessera build` compiles its C at `-O3`, as Rust's release builds do. Every row produces identical output. Reproduce with `python3 bench/run.py`, and against Rust and Go with `python3 bench/compare.py`.

Single-threaded:

| Benchmark | C | Tessera | Tessera / C |
| --- | --- | --- | --- |
| n-body, 15M steps, `Array[Body, 5]` | 0.507 s | 0.328 s | 0.65x |
| fib(40), recursion | 0.194 s | 0.180 s | 0.93x |
| filter/map/fold pipeline, 30 x 3M | 0.040 s | 0.040 s | 0.99x |
| sieve to 100M, list mutated by index | 0.345 s | 0.348 s | 1.01x |
| n-body, 15M steps, `List[Body]` | 0.505 s | 0.561 s | 1.11x |

With `Array[Body, 5]` the body count is part of the type, so at `-O3` the C compiler unrolls the pair loop completely; the C program, at `-O2`, keeps its loops. With a `List`, the length is only known at run time.

Parallel, with `group` (Tessera on one thread uses `TESSERA_THREADS=1`):

| Benchmark | C sequential | C pthreads | Tessera, 1 thread | Tessera, all cores | Tessera / C sequential |
| --- | --- | --- | --- | --- | --- |
| Mandelbrot 4000x4000 | 6.719 s | 0.791 s | 6.718 s | 0.778 s | 0.12x |
| longest Collatz chain below 5M | 0.586 s | 0.074 s | 0.597 s | 0.076 s | 0.13x |

On one thread Tessera is level with C on these workloads. With `group` it runs about 8x faster than the sequential C program and matches C that was parallelised by hand with pthreads.

## Language

Types: `Int`, `Float`, `Bool`, `Str`, `Bytes` (binary data), `()`, `List[T]`, fixed-size `Array[T, N]` (a value type), structs, enums with payloads, generics on functions, structs and enums, and closures. `Option`, `Result` and `IoError` come from the prelude. Control flow: `if`/`else`, exhaustive `match`, `while`, `for x in list`, `for i in 0..<n` (counts without building a list), `break`, `continue`, `return`, `?`, and `defer` (runs when its block is left, by any exit including a panic; last written runs first). Text is `Str`; `s.chars()` gives its characters as one-character strings. Statements end at a newline; a line starting with `.` continues a method chain. String interpolation takes any expression: `"{xs.join(", ")}"`.

Calls can name their arguments, `connect(host, retries: 3, port: 443)`: positional ones first, then named ones in any order, checked against the parameter names. Every parameter is still passed (there are no default values), and the arguments run in the order they are written. A closure can follow the call as a block, `xs.map { |x| x * 2 }`, `catch { risky() }`, `g.go { work() }`, except in the condition of `if`, `while`, `for` and `match`, where `{` starts the body.

Methods: a function whose first parameter is `self: T` is called as `value.name(...)`, a file can add its own methods to built-in types (`fn shout(self: Str) -> Str`), and `value.name` without the call is a function bound to that value: `xs.map(origin.add)`, `let say = out.print` (see [examples/methods.tsr](examples/methods.tsr)).

Collections: lists have `map`, `filter`, `fold`, `sort` and the rest, plus `any`, `all`, `count`, `find`, `sum_by`, `min`/`max`, `min_by`/`max_by`, `flat_map`, `take_while`/`skip_while`, `with_index`, `zip`, `distinct`/`distinct_by`, `sort_by(key)`, `partition`, `chunks(n)`, `windows(n)`, `chunk_by`, and `group_by`, `count_by` and `tally`, which build insertion-ordered maps. Maps add `map_values`, `filter` and `merge`. `words.tally().filter(|w, n| n > 1)` keeps the repeated words. The newer list and map methods are written in Tessera (`src/std/methods.tsr`), so the native backend compiles them like your own code.

Interfaces: `interface Shape { fn area(self) -> Float }` bounds a generic parameter, `fn total[T: Shape](xs: List[T])`, and any type with those methods satisfies it, built-in capabilities included, so tests can pass a fake `Clock` (see [examples/interfaces.tsr](examples/interfaces.tsr)). Two bounds are built in: `Ordered` (the type has `<`) and `Key` (it can be a map key), as in `fn index[K: Key, V](xs: List[V], key: fn(V) -> K) -> Map[K, V]`.

`dyn Shape` is a value of any type that satisfies `Shape`, so `List[dyn Shape]` can mix types, and `let s: dyn Shape = if big { Circle { r: 1.0 } } else { Square { s: 2.0 } }` picks one (see [examples/dyn.tsr](examples/dyn.tsr)).

Embedding: `struct Admin { User, level: Int }` embeds a `User`, so `admin.name` and `admin.greet()` reach the `User`'s field and method, and `Admin` satisfies the interfaces `User` does (as bounds and as `dyn`). It is composition, not inheritance: the embedded value is a plain field, `admin.User`, set in a literal as `Admin { User: u, level: 3 }`; the outer struct's own names come first, a name two embedded structs share is an error only where it is used, and there is no overriding or `super` (see SPEC.md, "Embedding").

Ordering is structural: `<` and `sort()` work on numbers, strings, `Bool`, and on lists, tuples, structs and enums of them, item by item, so `[(2, "b"), (1, "z")].sort()` sorts by the first item, then the second.

Tuples `(Int, Str)` and maps `Map[Str, Int]` are built in: `let (q, r) = divmod(17, 5)`, `counts[w] = counts.get(w) ?? 0 + 1`. Keys can be tuples, structs and enums (`grid[(x, y)]`), and entries change in place: `stats[name].hits = stats[name].hits + 1`. Maps iterate in insertion order, the same in both backends (see [examples/tuples.tsr](examples/tuples.tsr) and [examples/maps.tsr](examples/maps.tsr)).

`group g until clock.after(2000) { ... }` gives a group a deadline: tasks still running then are cancelled (freeing what they own) and the group evaluates to a `List[Option[T]]` with `None` for each of them.

`select { v = jobs.recv() if open => ..., out.send(x) => ..., clock.after(500) => ... }` waits on several channels, with guards to turn arms off and an optional timeout (or `_` to not wait at all; see [examples/select.tsr](examples/select.tsr)). `clock.sleep(ms)` parks a task without blocking a thread.

Errors: any type with `fn message(self: T) -> Str` satisfies the prelude's `Error` interface (as do `Str` and `IoError`), and in a function returning `Result[T, dyn Error]`, `?` converts each of them, so one function can mix file, parse and its own errors. A string after `?` adds context: `fs.read(path)? "reading config"` fails with `reading config: <the error>`, keeping the original as its `cause()`; `opt? "no port set"` turns a missing value into an error. The `errors` module walks the chain (`errors.report(e)` prints one cause per line), and `main` returning a `dyn Error` prints its message. `.map_error(f)` converts an error by hand (the old `map_err` still works, with warning W0001 and a fix that renames it).

`user?.address?.city ?? "unknown"` navigates through `Option`s: `None` anywhere short-circuits the rest of the chain, and the result is a single `Option`.

`match` arms take guards, as `select` arms do: `Some(n) if n > 10 => ...`. A guarded arm does not count toward exhaustiveness. Patterns have alternatives (`Circle(r) | Ring(r, _) => ...`, `Some(1 | 2)`), `..` for the parts not written (`SLet(name, ..)`, `(first, ..)`), and list patterns (`[]`, `[x]`, `[first, ..rest]`, `[.., last]`); exhaustiveness understands all three, and an arm that can never match is a warning (W0002).

`(x, p) = parse(p)?` assigns a tuple's items to existing `mut` variables (or fields, elements, `_`, nested tuples), and `xs + ys` concatenates lists.

`{ p with x: 3, y: 4 }` copies a struct with some fields replaced; natively, when `p` is not used again its storage is reused in place.

`x |> f(a)` is `f(x, a)`: a pipeline reads in the order things happen, one step per line if you like (`lines |> parse_all |> list.sort_by(less) |> top(10)`). It is the loosest operator, so `a + b |> f` is `f(a + b)`.

A panic ends only the task it happens in, and `catch(|| ...)` turns one into `Err(message)`, so `http.serve` answers 500 when a handler panics and keeps serving.

Not yet: nothing big; see the design doc's Known gaps. `Int` is 64-bit and wraps on overflow, identically in the interpreter and in native code.
