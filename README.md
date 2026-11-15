# Limen

Limen is a small compiled language where code can only do what it was handed.

There is no global `print`, `open` or `http.get`. A program starts with one
value, `root`, which holds everything it was granted, and passes narrower
pieces of it to the functions that need them. So a function's signature tells
you everything it can do, and the compiler checks it.

```limen
fn main(root: Root) -> Result[(), IoError] {
    let notes = root.fs.under("notes").read_only()
    let text = load(notes)?
    report(root.stdout, count_words(text))
    Ok(())
}

// Can read files under notes/, and nothing else.
fn load(fs: Fs) -> Result[Str, IoError] {
    fs.read("today.txt")
}

// Can only compute: no file, network, clock or output.
fn count_words(text: Str) -> Int {
    text.split(" ").filter(|w| w != "").len()
}

// Can print, and nothing else.
fn report(out: Stdout, n: Int) {
    out.print("{n} words")
}
```

```console
$ limen run main.lmn --grant fs=.
8 words
$ limen caps main.lmn
main         Root
load         Fs
count_words  pure
report       Stdout
```

## Install

```bash
curl -fsSL https://limen-lang.org/install | sh
```

That's macOS on Apple silicon (through Homebrew when you have it) and Linux on
x86-64 and arm64 (a `.deb` on Debian 12+ and Ubuntu 22.04+). Limen compiles
programs through C, so you also need a C compiler.

## Learn it

- [The Limen Book](https://book.limen-lang.org): the language, one chapter at a
  time, every example run by the real compiler.
- [SPEC.md](SPEC.md): the full reference, written so that a person
  or a model can learn the whole language from it.

## What's in the box

- **Capabilities all the way down.** Narrow them before you pass them on
  (`fs.under("data").read_only()`, `net.only("api.example.com")`), revoke them,
  and list what every function can reach with `limen caps`.
- **Dependencies you can trust.** Each one in `limen.toml` lists the
  capabilities it may receive, and the compiler checks every function it
  exports against that list.
- **Native code through C.** Single-threaded it runs level with C on the
  benchmarks in `bench/` (fib 0.93x, a filter/map/fold pipeline 0.99x, a
  sieve 1.01x, measured on an Apple silicon Mac). An interpreter runs the same
  programs for quick edits.
- **Structured concurrency.** Tasks live inside a `group` and never outlive it.
  Deadlines cancel what's left, panics come back to the caller, and channels,
  atomics and shared values are safe by construction.
- **A sandbox underneath.** Every program also runs in an operating-system
  sandbox built from the same grants, so C code stays inside them too.
- **Written in itself.** `selfhost/` is a Limen compiler in Limen. It builds
  itself to the same C as the original compiler, byte for byte.

## Build from source

You need OCaml 5 with dune, a C compiler and OpenSSL.

```bash
dune build
dune exec ./test/test_limen.exe
```

The tests run every program in the interpreter and natively under
AddressSanitizer, and the concurrent ones under ThreadSanitizer too.
`sh selfhost/check.sh` compares the two compilers at every stage, and
`sh selfhost/bootstrap.sh` builds the self-hosted compiler with itself.
`dev/linux/run.sh` runs the whole suite in a Linux container.

## Layout

| Path | What's there |
| --- | --- |
| `src/` | the compiler in OCaml: lexer, parser, checker, interpreter, C backend, runtime |
| `src/std/` | the standard library, written in Limen |
| `selfhost/` | the compiler written in Limen |
| `editors/vscode/` | highlighting and the language server for VS Code |
| `examples/`, `bench/` | programs to read, and the benchmarks |

What's still missing is in [ROADMAP.md](ROADMAP.md).

Limen is Latin for threshold: the line you cross only if you're let in. It was
called Writ, then Vela, then Tessera, before it found its name.
