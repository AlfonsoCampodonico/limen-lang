# One definition of each built-in

Every built-in that can disagree between the interpreter and native code is
written once, in C, and called by both. The trivial ones stay written twice,
and a test that runs every built-in through both backends on edge-case and
generated inputs catches drift in what is left. It is the "one definition of
each built-in" item of 0.8 in ROADMAP.md.

## Why

The interpreter (`src/interp.ml`) and the C runtime (`src/runtime/limen_rt.c`
and its headers) each implement the built-ins, and only the tests keep them
in step. They drift: the NUL-path difference came from this, and two more
differences were found while planning this, neither caught by the about 270
tests that already run both backends:

| | Interpreter | Native |
|---|---|---|
| `"\r".lines()` | `[]` | `[""]` |
| `"a\n\r".lines()` | `["a"]` | `["a", ""]` |
| `UInt64` 2^63 + 1025, `.to_float()` | 9223372036854775808 | 9223372036854777856 |

SPEC.md (section 11, `lines()`) agrees with the interpreter on `lines`; IEEE
rounding agrees with native on `to_float`. A program should give the same
answer whichever way it runs, since `limen run` and `limen test` interpret
while `limen build` ships the C.

## Goals

- Each built-in whose result depends on bytes, Unicode, parsing, float
  formatting, integer edge cases, ordering of scalars, random numbers or a
  panic message has one C definition, used by both backends.
- Every built-in, shared or not, is exercised under both backends by a
  generated test, and a difference fails the build.
- The two differences above are fixed, each pinned by a test.

## Non-goals

- Dropping or replacing the interpreter (the 0.9 question).
- Sharing the walks over whole values: printing, `==` and ordering of
  structs, enums, lists and maps stay in each backend. Their leaves (floats,
  strings, escaping, scalar comparison) are what gets shared.
- Changing any built-in's documented behaviour. Where the two backends
  disagree, the fix moves the wrong one to SPEC.md, not SPEC.md to it
  (where SPEC.md is silent, see "When the backends disagree").
- The standard library written in Limen (`json`, `regex`, `time` and the
  rest of `src/std/`): it is already one definition.

## Anti-goals

It is a failure, even if it works, if:

- `limen run` or `limen test` gets noticeably slower on string-heavy
  programs (a test suite that takes 10 % longer is noticed);
- a shared C function can be reached from a Limen program other than
  through the built-in it implements;
- the cross-check is so slow or flaky that it gets skipped.

## Constraints

- Every change goes into both compilers (`src/` and `selfhost/`). The
  self-hosted compiler emits C against the same runtime, but it embeds the
  runtime's text itself (`selfhost/sources.lmn`), so it must embed
  `limen_core.h` in the same place `src/dune` does.
- `dune build`, `dune exec ./test/test_limen.exe`, `sh selfhost/check.sh`
  and `sh selfhost/bootstrap.sh` pass.
- The runtime still needs nothing but a C compiler and libc: a native
  program's C stays self-contained.
- macOS and Linux, x86-64 and arm64.

## Approaches

1. **Shared C, cross-checked** (picked by Alfonso). The risky built-ins move
   into C functions both backends call, the way `fs` already works; the
   trivial ones stay written twice; a generated cross-check covers all of
   them.
2. *Drop the interpreter's built-ins* by having `limen run` and `limen test`
   compile natively. Truly one definition, but it is the 0.9 decision about
   the interpreter brought forward, and `--coverage` and the interpreted
   paths depend on it. Rejected for now.
3. *Everything in C*, the interpreter a thin shim. Walking boxed OCaml lists
   and maps from C costs more than it saves, and the interpreter would slow
   down. Rejected.

## The design

### The shared header

- A new header, `src/runtime/limen_core.h`, holds `static` C functions for
  the shared built-ins. It is used the way `limen_sandbox.h` is today: put
  in front of `limen_rt.c` in the runtime text a native program gets
  (`src/dune`, the "Embed the C runtime" rule, and `selfhost/sources.lmn`),
  and included by a C stub file compiled into the OCaml library through
  `foreign_stubs`. It goes in the runtime object only, not the header that
  each piece of a split build includes: a piece calls the runtime's
  `Str_*` and `Float_*` functions, which wrap the core ones, never the core
  ones directly (my call).
- The functions work on plain data only: `(const char *, size_t)` for
  strings and bytes, `int64_t`, `uint64_t` and `double` for numbers, and a
  caller-supplied or `malloc`'d buffer for results. They know nothing of
  `WStr` or OCaml values, so both sides wrap them thinly.
- The native runtime's `Str_*`, `Float_*`, `w_*` functions become wrappers
  over them; the OCaml side gets `external` declarations in a small module
  (`src/core.ml`) that `interp.ml` calls instead of its own code.
- A result that is a list of strings (`split`, `lines`, `chars`) comes back
  to OCaml as one buffer of length-prefixed pieces (a NUL separator, as the
  `fs` list uses, does not work: strings may hold NUL).

### What is shared

From the inventory, the built-ins rated high or medium risk:

- Str: `split`, `lines`, `trim`, `replace`, `chars` and UTF-8 length,
  `parse_int`, `parse_float`, escaping for `show`.
- Numbers: float formatting (`fmt_float`, used by `to_str` and
  interpolation), `to_fixed`, `Float.to_int`, `UInt*.to_float`.
- Not integer `/`, `%`, shifts and rotation (my call). Natively they are
  `static inline` and emitted in place; in the interpreter they are its
  hottest path, and a C call per operation would break the speed budget.
  They stay written twice, and the cross-check covers them with every edge
  case (division by -1 and 0, shifts by the width and more, the smallest
  `Int`).
- Scalar ordering: `Float` comparison (NaN, -0.0) and `Str` comparison.
- `Rand`: splitmix64 and `int`, `float`, `bytes` built on it. The state
  stays where each backend keeps it today and is passed in and out; C
  holds no state of its own (my call).
- Panic messages: index out of range, missing map key, byte range. Only
  the message text is shared; each backend raises the panic and prints
  the location as it does now (my call).
- `os_random` and the stat-to-milliseconds code, today copied between
  `ffi_stubs.c` and `limen_rt.c` rather than shared.

What stays twice: `len`, `is_empty`, `contains`/`index_of`/
`last_index_of`, `starts_with`/`ends_with`, ASCII case mapping, `slice`,
`bytes`, concatenation, list and map access, `join`, `sum`, ranges, the
handle table, the byte readers. They are each a few lines, and the
cross-check covers them.

### The cross-check

- A generator in the test suite writes, for each built-in, a Limen program
  that applies it to a fixed list of edge cases (empty, one byte, NUL, a
  lone `\r`, invalid UTF-8, the largest and smallest of each integer type,
  -0.0, NaN, infinities, subnormals, 2^53 and 2^63 neighbours, very long
  inputs) and to inputs from a seeded random generator, and prints each
  result.
- It runs through the existing `out` path, which already runs the
  interpreter and the native build and compares their output, so a
  difference fails `dune exec ./test/test_limen.exe` like any other test.
- The seed is fixed in CI; an environment variable sets another, to look
  for more, and the failing input is printed.
- It is in the normal test run, so it has to stay fast: one program per
  area, not per built-in, under the binary cache.

### Speed and memory (my calls)

- The budget is one number: the interpreter's time on the existing test
  suite and on `selfhost/check.sh` grows by less than 5 %. If moving a
  built-in breaks it, that built-in stays written twice and the
  cross-check covers it; the shared list shrinks, the budget doesn't.
- A C function writes into a buffer the stub owns (on the stack, or
  `malloc`'d and freed by the stub after copying into an OCaml string).
  Nothing allocated in C outlives the call.

### When the backends disagree (my call)

- SPEC.md decides. Where it is silent, the answer that is correct by the
  standard it rests on wins (IEEE 754 for floats, Unicode for text), and
  where neither decides, native, since that is what ships. Each such
  answer is written into SPEC.md in the same commit, so the next
  disagreement has a rule to go by. A program that relied on the losing
  side's behaviour changes; each case is listed in the release notes.

### The two fixes

- `Str.lines`: native moves to SPEC.md's rule (strip a trailing `\r` per
  line, then drop one final empty line). With `lines` shared this is the
  shared definition.
- `UInt64.to_float`: the interpreter moves to the correctly rounded
  conversion. Shared, this is the C cast.

## Proof

- The cross-check passes, and fails when either backend's copy of a
  not-shared built-in is changed by hand (checked once, by hand).
- Both differences above have tests.
- The interpreter's run time on the existing test suite and on
  `selfhost/check.sh` changes by less than 5 %.

## Left to the builder

- Function names and the exact split of `limen_core.h` (one header or a few).
- Whether to put the new stubs in `ffi_stubs.c` or a new `core_stubs.c`.
- How many generated programs, and the edge-case lists beyond those above.
- The order in which built-ins move; each move is a commit that keeps the
  tests green.
