# Writ

A small, statically typed language where code can only touch the outside world through capabilities it was handed. There is no global `print`, `open`, `http.get` or `time.now`; `main` receives a `Root` value and passes narrower capabilities to the functions that need them.

The full language reference is [SPEC.md](SPEC.md). The compiler is written in OCaml with no dependencies beyond the standard distribution. It has a type checker with JSON diagnostics, a tree-walking interpreter, a native backend that compiles to C, and structured concurrency on a thread pool.

```
fn main(root: Root) -> Result[(), IoError] {
    let gh = root.net.only("api.github.com:443")   // narrower capability
    let names = fetch_repos(gh, "rust-lang")?
    root.stdout.print("{names.len()} repos")
    Ok(())
}

fn fetch_repos(net: Net, org: Str) -> Result[List[Str], IoError] {
    let body = net.get("https://api.github.com/orgs/{org}/repos")?
    Ok(parse_names(body))
}

// No capability parameters: this function cannot do I/O.
fn parse_names(body: Str) -> List[Str] { ... }
```

## Build and run

Needs OCaml 5 and dune (`brew install ocaml dune`), plus a C compiler (`cc`, or set `CC`) for the native backend. `curl` is used for `Net.get`.

```bash
dune build
```

```bash
dune test
```

```bash
dune exec writ -- run examples/repos.wr --grant net=api.github.com
```

```bash
dune exec writ -- build examples/repos.wr -o repos && ./repos --grant net=api.github.com
```

## Commands

| Command | What it does |
| --- | --- |
| `writ run FILE [--native] [--grant net=HOST[:PORT]\|*]... [--grant fs=DIR[:ro]] [--grant listen=HOST[:PORT]]...` | Type-check, then run `main` in the interpreter, or compiled with `--native`. Stdout and Clock are always granted; Net, Fs and Listen only when granted. |
| `writ build FILE [-o OUT] [--emit-c]` | Compile to a native binary through C. The binary takes the same `--grant` flags. `--emit-c` writes the C instead. |
| `writ check FILE [--json]` | Type-check only. `--json` emits diagnostics with codes, spans, notes and machine-applicable fixes. |
| `writ caps FILE [--json]` | List each function's authority: the capabilities in its signature, or `pure`. |
| `writ explain CODE` | Explain an error code, for example `writ explain E0302`. |
| `writ test FILE [--native]` | Run the file's `test "name" { ... }` blocks, each isolated. |
| `writ fmt FILE... [--write\|--check]` | Print the canonical formatting, rewrite files, or check them. |
| `writ lsp` | Language server on stdin/stdout. |
| `writ mcp` | MCP server for coding agents on stdin/stdout. |
| `writ spec` | Print the language specification. |

## Tooling for editors and models

- **Language server (`writ lsp`):**
  - diagnostics as you type
  - hover with types, for example `let total: Int` or `print(Str) -> ()`
  - go to definition for functions, locals, parameters, fields, types and variants
  - completion: members after `.` (fields, methods, capabilities on `root`), otherwise locals, functions, types and keywords
  - the checker's auto-fixes as quick fixes
  - an outline of the file

  Positions count bytes, which is exact for ASCII source.
- **Grammar ([grammar/writ.gbnf](grammar/writ.gbnf)):** in llama.cpp's GBNF format, for constrained decoding. It accepts every example, benchmark and test-suite program, and rejects the hand-written invalid cases. It is slightly more permissive than the parser: keywords aren't excluded from identifiers. It requires a space after keywords (`if x {`), which is the canonical style.
- **Formatter (`writ fmt`):** keeps comments, number and string spellings, and the author's line breaks in lists and argument lists. The test suite formats every program and checks that the tree is unchanged, a second pass changes nothing, and no comment is lost.
- **Tests (`writ test`):** `test "name" { assert_eq(...) }` blocks. They get no `root`, so they cannot do I/O. Each runs isolated in the interpreter, or as its own process with `--native`.
- **MCP server (`writ mcp`):** tools to check, run, test, format, read a type, list capabilities, explain an error, and read the spec. `writ_run` gives the program no grants, so code an agent just wrote can print but cannot touch files or the network, and it stops at a time limit.
- **Checking the grammar:** `python3 grammar/gbnf_check.py` checks files against it with an Earley recognizer that follows llama.cpp's grammar syntax and acceptance rule. `grammar/check-suite.sh` runs it over every program in the test suite.

Neovim:

```
vim.lsp.start({ name = "writ", cmd = { "writ", "lsp" }, root_dir = vim.fn.getcwd() })
```

With VS Code, any generic LSP client extension works: point it at `writ lsp` for `*.wr` files.

## Capability rules

- `Root` holds `net`, `fs`, `stdout` and `clock`. Only the runtime creates capabilities.
- Attenuation only narrows: `net.only(host)`, `fs.under(dir)`, `fs.read_only()`. Asking for more than you hold gives an empty capability.
- Capabilities and function types cannot be stored in struct or enum fields (E0301, E0304), so authority always shows up in signatures. Generic instantiations like `Box[Net]` are allowed because the capability is visible in the type.
- A function with no capability or function-typed parameters is pure.
- Runtime enforcement: `Net` is an http/https host allowlist, redirects are not followed, and other schemes are refused. `Fs` resolves paths under its root and refuses absolute paths, `..` escapes and symlinks that lead outside.

## Concurrency

Tasks are fibers: small stacks scheduled by worker threads, one per core, with work stealing. A task waiting on a group, a channel or a socket parks without blocking a thread. If every task is waiting and nothing can wake one, the program panics with a deadlock message instead of hanging.

A `group` block starts tasks with `g.go(closure)` and evaluates to the list of their results, in the order they were started. Every task has finished when the block ends, so nothing outlives it.

```
let counts = group g {
    for row in rows { g.go(|| count_row(row)) }
}
```

- Tasks share data by value. A value handed to a task is switched to atomic reference counting, together with everything it references. Values that stay on one thread keep plain counts, so single-threaded code pays nothing.
- A task can only touch the world through capabilities it captured. Spawning is not a capability, because it has no effect outside the program.
- The handle `g` can only be used as `g.go(...)` in the block's own body (E0310), and the body cannot `return` or use `?` to leave early (E0311).
- **Channels:** `channel(n)` makes a `Chan[T]` with `send`, `recv` (returning `None` once closed and drained) and `close`. Values sent are shared, not copied, across threads.
- **Sockets:** `root.listen.on("host:port")` (granted with `--grant listen=...`) gives a `Listener`; `accept` and `Net.connect` give a `Conn` with `read`, `write` and `close`. The native poller is kqueue on macOS and epoll on Linux (Linux untested).
- **Echo server:** [examples/echo.wr](examples/echo.wr) serves 10,000 concurrent connections, each kept open until all are connected, with every echo correct. Natively that takes about 2 s and 335 MB peak. Reproduce with `bench/echo_load.py`.
- **Interpreter:** runs tasks as coroutines on OCaml 5 effect handlers, so the same programs behave the same way.

## Native backend

`writ build` lowers the typed program to a small IR, instantiates generics per concrete type, inserts reference counting, and emits C compiled with `cc -O2 -fwrapv -ffp-contract=off`.

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

Best of 5 on an Apple Silicon Mac with 12 cores, against C compiled with `-O2 -fwrapv -ffp-contract=off`. Every row produces identical output. Reproduce with `python3 bench/run.py`.

Single-threaded:

| Benchmark | C | Writ | Writ / C |
| --- | --- | --- | --- |
| filter/map/fold pipeline, 30 x 3M | 0.037 s | 0.034 s | 0.93x |
| fib(40), recursion | 0.198 s | 0.185 s | 0.94x |
| n-body, 15M steps, `Array[Body, 5]` | 0.509 s | 0.505 s | 0.99x |
| sieve to 100M, list mutated by index | 0.325 s | 0.327 s | 1.01x |
| n-body, 15M steps, `List[Body]` | 0.508 s | 0.558 s | 1.10x |

The C n-body passes the body count as the literal `5`, so clang unrolls its loops at compile time. Writ matches it when the program states the size too, with `Array[Body, 5]`. With a `List`, the length is only known at runtime.

Parallel, with `group` (Writ on one thread uses `WRIT_THREADS=1`):

| Benchmark | C sequential | C pthreads | Writ, 1 thread | Writ, all cores | Writ / C sequential |
| --- | --- | --- | --- | --- | --- |
| Mandelbrot 4000x4000 | 6.719 s | 0.791 s | 6.718 s | 0.778 s | 0.12x |
| longest Collatz chain below 5M | 0.586 s | 0.074 s | 0.597 s | 0.076 s | 0.13x |

On one thread Writ is level with C on these workloads. With `group` it runs about 8x faster than the sequential C program and matches C that was parallelised by hand with pthreads.

## Language

Types: `Int`, `Float`, `Bool`, `Str`, `()`, `List[T]`, fixed-size `Array[T, N]` (a value type), structs, enums with payloads, generics on functions, structs and enums, and closures. `Option`, `Result` and `IoError` come from the prelude. Control flow: `if`/`else`, exhaustive `match`, `while`, `for x in list`, `break`, `continue`, `return` and `?`. Statements end at a newline; a line starting with `.` continues a method chain. String interpolation takes any expression: `"{xs.join(", ")}"`.

Not yet: methods on user types, modules, tuples and maps, and `select` over channels. `Int` is 64-bit in native code but 63-bit in the interpreter, because OCaml reserves one bit of its native int; the two only differ on overflow.
