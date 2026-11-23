# One definition of each built-in Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Write each risky built-in once, in C, called by both the interpreter and native code; cross-check every built-in under both backends on every test run; fix the two known differences (`Str.lines` on a lone `\r`, `UInt64.to_float` past 2^63).

**Architecture:** A new header of `static` C functions on plain data (`src/runtime/limen_core.h`) is pasted into the runtime text a native program gets, just ahead of `limen_rt.c`, and included by a new OCaml stub file (`src/core_stubs.c`) declared to OCaml in `src/core.ml`. The runtime's `Str_*`, `Float_*` and `w_*` functions and the interpreter's helpers become thin wrappers over it. Two conversions that native code inlines into every split-build piece live in a second, tiny header (`limen_core_inline.h`) that goes ahead of `limen_rt.h` in both the runtime and the per-piece header. A generator in the test suite writes one Limen program per area, runs it through both backends and fails on the first line that differs.

**Tech Stack:** OCaml (dune, C stubs), the C runtime, the self-hosted compiler in Limen (`selfhost/`, only its runtime embedding changes).

**Spec:** `docs/superpowers/specs/2026-11-23-one-definition-of-builtins-design.md`

## Global Constraints

- Every change goes into both compilers (`src/` and `selfhost/`) and both backends; the self-hosted compiler stays at the bootstrap fixed point. Here the only self-hosted change is `selfhost/sources.lmn`, which must paste the runtime files in exactly the order `src/dune` does.
- Before every commit: `dune build`, `dune exec ./test/test_limen.exe`, `sh selfhost/check.sh` and `sh selfhost/bootstrap.sh` all pass. Never rebuild while check.sh runs.
- Commit messages: no `Co-Authored-By`, no "Generated with", no Claude attribution of any kind. No emojis. British spelling, no em dashes, in messages, comments and docs. Commit locally per task; never push.
- RTK may rewrite shell commands; `rtk proxy <cmd>` runs one raw (use it for `git` plumbing whose output you parse, and for `nm`).
- The runtime still needs nothing but a C compiler and libc; macOS and Linux, x86-64 and arm64.
- Core functions take plain data only: `(const char *, size_t)`, `int64_t`, `uint64_t`, `double`, caller-supplied buffers. They know nothing of `WStr` or OCaml `value`s, hold no state, and nothing they allocate outlives the call. All are declared `LC_FN` (`#define LC_FN static inline`, so an unused one never warns) with names starting `lc_`.
- Stubs in `core_stubs.c`: never keep a `char *` into an OCaml string across an allocation (the minor GC moves small strings). Keep offsets and re-read `String_val` after every `caml_alloc*`. Lists of strings come back as `string array`, built in two passes (count, then `caml_alloc(n, 0)` and fill with `caml_alloc_initialized_string`); this replaces the spec's length-prefixed buffer, which needs a decode on the OCaml side.
- Numeric stubs with no allocation are `[@@noalloc]` with `[@unboxed]`/`[@untagged]` arguments and a boxed `_byte` variant for bytecode, e.g. `external f2i : (float [@unboxed]) -> (int64 [@unboxed]) = "limen_core_f2i_byte" "limen_core_f2i" [@@noalloc]`.
- Where the backends disagree: SPEC.md decides; where it is silent, IEEE 754 for floats and Unicode for text; where neither decides, native. A rule written down this way goes into SPEC.md in the same commit.
- Speed budget: the interpreter's time on the test suite (`LIMEN_SKIP_NATIVE=1`) and on `selfhost/check.sh` grows by less than 5 %. A built-in that breaks it goes back to being written twice; the budget does not move.
- Integer `/`, `%`, shifts and rotation stay written twice (native inlines them); the cross-check covers them.

## Review Focus

1. A small string with many pieces (`"x\n"` repeated 1000 times, under the minor heap's size) split by `lines()`, `split` or `chars()` while each piece allocation can trigger a minor GC: every piece must be right, every time. Test `str-lines-gc` in Task 1.
2. NUL bytes inside strings passed to C: `"1\0".parse_int()` is `None`, `"\0".trim()` keeps its byte, `"a\0b".split("\0")` is `["a", "b"]`, `"1\0".parse_float()` is `None`. C must use lengths, never `strlen`. Tests `str-nul-builtins` in Task 4 and `float-parse-nul` in Task 5.
3. `parse_float` on inputs either side of the 408-byte stack buffer (407, 408, 409 bytes) and on 10 000 digits: the same answer as a short spelling of the number. Test `float-parse-long` in Task 5.
4. `to_fixed(30)` of the largest and smallest finite floats: 340 and 341 bytes, never cut off by a fixed buffer. Test `float-to-fixed-huge` in Task 5.
5. Seeded `Rand.int` over a span past 2^63 (`int(-9223372036854775808, 9223372036854775807)`) and `bytes(n)` for n not a multiple of 8: the same stream in both backends. Test `rand-wide-span` in Task 7.

---

### Task 1: `limen_core.h`, its wiring, and `Str.lines` defined once

**Files:**
- Create: `src/runtime/limen_core.h`, `src/core_stubs.c`, `src/core.ml`
- Modify: `src/dune` (the `ffi_stubs` `foreign_stubs` stanza; the "Embed the C runtime" rule and its comment)
- Modify: `selfhost/sources.lmn:18-22` (`runtime()` and its comment)
- Modify: `src/runtime/limen_rt.c:527-538` (`Str_lines`)
- Modify: `src/interp.ml:450-453` (`lines`), `src/interp.ml:2976`
- Modify: comments naming the runtime files: `src/runtime/limen_rt.h:1-20`, `selfhost/cgen.lmn:3327`
- Test: `test/test_limen.ml` (beside `out` at line 627)

**Interfaces:**
- Produces, in `limen_core.h` (include guard `LIMEN_CORE_H`; starts with the `_GNU_SOURCE` guard that `limen_rt.h:24` uses, then its own system includes, so `core_stubs.c` can include it first):
  - `#define LC_FN static inline`
  - `LC_FN bool lc_lines_next(const char *s, size_t n, size_t *pos, size_t *start, size_t *len)`: the next line of `s[0..n)` by SPEC.md's rule. `*pos` starts at 0; each call yields one piece in `*start`/`*len` and returns true, or returns false when there are no more. Algorithm: if `*pos > n` return false; the piece runs from `*pos` to the next `\n` or `n`; drop one trailing `\r`; if the piece ended at `n` and is now empty, return false; else `*pos` = end + 1 and return true.
- Produces, in `src/core.ml`: `external lines : string -> string array = "limen_core_lines"`.
- Produces: runtime text order `limen_sandbox.h, limen_rt.h, limen_sql.h, limen_core.h, limen_rt.c` in both `src/dune` and `selfhost/sources.lmn`. `limen_core.h` is not in `Runtime_text.header` / `runtime_header()`: a split-build piece calls `Str_*`, never `lc_*`.

- [ ] **Step 1: Write the failing tests** in `test/test_limen.ml` after the test at line 627:

```ocaml
out "str-lines-cr"
  (main_wrap {|    out.print("{"\r".lines()} {"a\n\r".lines()} {"a\r\nb\r\n".lines()} {"".lines()} {"\n".lines()} {"a\n\n".lines()} {"\r\n\r".lines()}")|})
  "[] [\"a\"] [\"a\", \"b\"] [] [\"\"] [\"a\", \"\"] [\"\"]\n";
out "str-lines-gc"
  (main_wrap {|    let mut parts: List[Str] = []
    for i in 0..<1000 {
        parts.push("x{i}")
    }
    let s = parts.join("\n")
    let mut ok = 0
    for i in 0..<300 {
        if s.lines().join("\n") == s && s.lines().len() == 1000 {
            ok = ok + 1
        }
    }
    out.print("{ok}")|})
  "300\n";
```

- [ ] **Step 2: Run them to see the first fail natively only**

Run: `LIMEN_TEST_ONLY=str-lines dune exec ./test/test_limen.exe`
Expected: `FAIL str-lines-cr/native: stdout "[\"\"] [\"a\", \"\"] ...`; the interpreter halves and `str-lines-gc` pass.

- [ ] **Step 3: Create `limen_core.h` with `lc_lines_next`, and wire it in**

- `src/dune`: in the `ffi_stubs` stanza, `(names ffi_stubs core_stubs)` and add `runtime/limen_core.h` to `extra_deps`; in the "Embed the C runtime" rule add `runtime/limen_core.h` to `deps` and `(cat runtime/limen_core.h)` between `limen_sql.h` and `limen_rt.c`; update the rule's comment ("the sandbox, the declarations, the SQL core, the shared built-ins, the definitions").
- `selfhost/sources.lmn` `runtime()`: `... + embed("../src/runtime/limen_sql.h") + embed("../src/runtime/limen_core.h") + embed("../src/runtime/limen_rt.c")`, comment to match.
- Update the file lists in the comments at `limen_rt.h:1-20` and `selfhost/cgen.lmn:3327`, and give `limen_core.h` a header comment saying what belongs in it (a built-in whose answer depends on bytes, Unicode, parsing, float formatting, integer edge cases, scalar order, random numbers or a panic message) and the plain-data rule.

- [ ] **Step 4: `src/core_stubs.c` and `src/core.ml`**

`core_stubs.c` starts with `#define _GNU_SOURCE`, includes `runtime/limen_core.h`, then the `caml/` headers. `value limen_core_lines(value s)`: two passes over `lc_lines_next` as in Global Constraints. `src/core.ml` holds the `external`, with a header comment pointing at `limen_core.h`.

- [ ] **Step 5: Make both backends call it**

`Str_lines` (`limen_rt.c:527`) loops `lc_lines_next` and pushes `Str_from(s->data + start, len)`. In `interp.ml` delete `lines` (450-453) and make line 2976 `VList (Array.map (fun l -> VStr l) (Core.lines s))`.

- [ ] **Step 6: Run the tests**

Run: `LIMEN_TEST_ONLY=str-lines dune exec ./test/test_limen.exe`
Expected: all pass, `N/N passed`.

- [ ] **Step 7: Check no `lc_` symbol can be reached from outside**

Run: `rtk proxy nm -g _build/default/bin/main.exe | grep lc_`
Expected: no output (the core functions are static; only the `limen_core_*` stubs are exported, as the existing `limen_fs_op` is).

- [ ] **Step 8: Full verification** (`dune build`, `dune exec ./test/test_limen.exe`, `sh selfhost/check.sh`, `sh selfhost/bootstrap.sh`); all pass.

- [ ] **Step 9: Commit**

```bash
git add src/runtime/limen_core.h src/core_stubs.c src/core.ml src/dune selfhost/sources.lmn src/runtime/limen_rt.c src/runtime/limen_rt.h selfhost/cgen.lmn src/interp.ml test/test_limen.ml
git commit -m "runtime: limen_core.h, built-ins written once in C; Str.lines first

Native lines() kept an empty last line left by a stripped \r (\"\\r\".lines()
gave [\"\"]); both backends now follow SPEC.md's rule through lc_lines_next."
```

---

### Task 2: `UInt*.to_float` and `Float.to_int` defined once

**Files:**
- Create: `src/runtime/limen_core_inline.h`
- Modify: `src/dune` (runtime rule: `text` and `header`), `selfhost/sources.lmn` (`runtime_header()`)
- Modify: `src/runtime/limen_rt.h:388-394` (`w_u2f`, `w_f2i`)
- Modify: `src/interp.ml:505-510` (`float_to_int`), `src/interp.ml:3017-3019` (`UInt to_float`)
- Modify: `src/core_stubs.c`, `src/core.ml`
- Modify: `SPEC.md:1249` (UInt row)
- Test: `test/test_limen.ml` (beside the test at line 981)

**Interfaces:**
- Consumes: Task 1's `LC_FN`, wiring and stub file.
- Produces, in `limen_core_inline.h` (own include guard and `_GNU_SOURCE` guard; includes only `<stdint.h>` and `<math.h>`): `static inline int64_t lc_f2i(double x)` (NaN 0, `>= 9.2e18` max, `<= -9.2e18` min, else truncate: the body now in `w_f2i`) and `static inline double lc_u2f(uint64_t x)` (`(double)x`, IEEE round to nearest even).
- Produces: `limen_rt.h`'s `w_f2i`/`w_u2f` return `lc_f2i(x)`/`lc_u2f(x)`. Runtime text order becomes `sandbox, core_inline, rt.h, sql, core, rt.c`; the header becomes `core_inline, rt.h`; in `sources.lmn`, `runtime_header()` returns `embed(core_inline) + embed(rt.h)` and `runtime()` already uses it.
- Produces, in `core.ml`: `external f2i : (float [@unboxed]) -> (int64 [@unboxed]) = "limen_core_f2i_byte" "limen_core_f2i" [@@noalloc]` and `external u2f : (int64 [@unboxed]) -> (float [@unboxed]) = "limen_core_u2f_byte" "limen_core_u2f" [@@noalloc]` (the int64 is the UInt's 64 bits).

- [ ] **Step 1: Write the failing test** after line 981:

```ocaml
out "uint-to-float-rounding"
  (main_wrap {|    let x = (-9223372036854775807 - 1).to_uint64() + 1025.to_uint64()
    out.print("{x} {x.to_float().to_fixed(0)} {(-1).to_uint64().to_float().to_fixed(0)} {9007199254740993.to_uint64().to_float().to_fixed(0)} {255.to_uint8().to_float()}")|})
  "9223372036854776833 9223372036854777856 18446744073709551616 9007199254740992 255.0\n";
```

- [ ] **Step 2: Run it**

Run: `LIMEN_TEST_ONLY=uint-to-float dune exec ./test/test_limen.exe`
Expected: `FAIL uint-to-float-rounding: got "9223372036854776833 9223372036854775808 ..."` (interpreter); native passes.

- [ ] **Step 3: Create `limen_core_inline.h`, wire it into `text` and `header` in `src/dune` and `runtime_header()` in `sources.lmn`, make `w_f2i`/`w_u2f` wrappers, add the two stubs and externals.** `core_stubs.c` includes `limen_core_inline.h` before `limen_core.h`.

- [ ] **Step 4: Interpreter calls them**: `float_to_int` becomes `Core.f2i`; the `VUInt (_, n), "to_float"` arm becomes `VFloat (Core.u2f n)` and its comment goes.

- [ ] **Step 5: SPEC.md:1249**: after `to_float()` add "(the nearest `Float`, ties to even, as IEEE 754 rounds: `(2^63 + 1025).to_float()` is `9223372036854777856.0`)". The interpreter's answer changes; SPEC.md was silent, IEEE decides.

- [ ] **Step 6: Run** `LIMEN_TEST_ONLY=uint-to-float,int64 dune exec ./test/test_limen.exe`; expected all pass.

- [ ] **Step 7: Full verification**, all four commands pass.

- [ ] **Step 8: Commit**

```bash
git add src/runtime/limen_core_inline.h src/dune selfhost/sources.lmn src/runtime/limen_rt.h src/interp.ml src/core_stubs.c src/core.ml SPEC.md test/test_limen.ml
git commit -m "runtime: UInt to_float and Float to_int written once

The interpreter rounded UInt64 values past 2^63 twice (2^63 + 1025 gave
2^63); it now uses the C conversion, which rounds once as IEEE 754 does."
```

---

### Task 3: the cross-check

**Files:**
- Create: `test/crosscheck.ml`
- Modify: `test/test_limen.ml` (a `same` helper after `out` at line 383; the calls at the end of the main `let () =`, before `drain ()`)

**Interfaces:**
- Produces, in `test/crosscheck.ml` (part of the test executable: `test/dune` has no `modules` field):
  - `val seed : int`: `LIMEN_CROSSCHECK_SEED` when set to an integer, else `20261123`.
  - `val programs : seed:int -> (string * string * int option) list`: (test name, Limen source, `--seed` for `Rand` or `None`). Names start `xc-`.
- Produces, in `test_limen.ml`: `val same : string -> ?grants:Interp.grants -> string -> unit`. Runs the interpreter, then the native build (through `run_native`, so ASan/UBSan and the leak check apply), and compares stdout. On a difference it reports `line N: interpreter "...", native "..."` for the first differing line, plus `(seed S)`; native must also be clean as in `check_native` (exit 0, stderr `"limen-leakcheck: 0 live\n"`). Honours `selected`, `native_for` and the binary cache like `out`.

- [ ] **Step 1: Write the generator.** One program per area, each line `"{area} {k} {show of the input} -> {result}"`, so a differing line names its input. Strings are written as `bytes([..]).to_str()` (so any byte, NUL or invalid UTF-8 included, survives the source); UInt values as `(<int64 bit pattern>).to_uint<w>()`; floats as `%.17g` literals, with NaN, the infinities and -0.0 as `0.0 / 0.0`, `1.0 / 0.0`, `-1.0 / 0.0`, `-0.0`. Random inputs come from `Random.State.make [| seed |]`.

| Name | Built-ins | Edge inputs (plus 200 random ones each, from the seed) |
|---|---|---|
| `xc-str` | every Str method in SPEC.md section 11 (`len`, `is_empty`, `lines`, `split` with seps `""`, `","`, `"\n"`, `"aa"`, `"\r\n"`, `chars`, `trim`, `contains`, `starts_with`, `ends_with`, `index_of`, `last_index_of`, `to_upper`, `to_lower`, `replace` with `("", "x")`, `("a", "")`, `("aa", "b")`, `parse_int`, `parse_float`, `slice` with -1, 0, 1, len, len + 1, Int min and max, `bytes`), `+`, `Int.to_char`, a string printed inside a list | empty, one byte, `\0`, lone `\r`, `\r\n`, `\n\r`, `a\n\r`, `\xff`, overlong `\xc0\x80`, surrogate `\xed\xa0\x80`, `\xf4\x90\x80\x80`, truncated `\xe2\x82`, `café`, an emoji, `" \t\x0c a \r\n"`, `a,,b`, `aaa`, `+7`, `-0`, `9223372036854775807`, `9223372036854775808`, `-9223372036854775808`, `1_000`, `0x10`, `1e5`, `.5`, `5.`, `1e`, `e5`, `-`, `nan`, `inf`, digit strings of 407, 408 and 409 bytes; code points -1, 0, 0x7f, 0x80, 0x7ff, 0x800, 0xd800, 0xdfff, 0xffff, 0x10000, 0x10ffff, 0x110000, Int max. Random: 0 to 40 bytes drawn from `a`, `,`, space, `\n`, `\r`, `\t`, `\0`, `\xc3`, `\xa9`, `\xf0`, `\x9f`, digits, `.`, `e`, `-`, `+`, any byte |
| `xc-long` | `lines`, `split("\r")`, `chars`, `replace("\r\n", "\n")`, `trim`, printed as lengths | `"a\r\n"` 20 000 times, built with `push` and `join` |
| `xc-int` | `+ - * / % << >> & \| ^`, `shift_right_unsigned`, `abs`, `min`, `max`, `to_float`, `to_str`, `to_uint8` to `to_uint64`, comparisons | 0, 1, -1, 2, -2, 63, 64, 65, -64, 2^31, 2^32, 2^53 - 1, 2^53 + 1, Int max and min, in all pairs (divisors and shift counts that panic go to `xc-panic`) |
| `xc-uint` | for each width 8, 16, 32, 64: `+ - * / % << >> & \| ^`, `rotate_left`/`rotate_right` by -1, 0, 1, w, w + 1, `to_int`, `to_str`, `to_float().to_fixed(0)`, `min`, `max`, comparisons | 0, 1, max, max - 1, 2^(w-1), and for 64: 2^63 ± 1, 2^63 + 1023, + 1024, + 1025, 2^64 - 1025 |
| `xc-float` | `to_str`, interpolation, `to_int`, `to_fixed` with -1, 0, 1, 2, 15, 30, 31, `abs`, `sqrt`, `+ - * /`, `< <= > >= ==` | 0.0, -0.0, NaN, ±inf, 5e-324, 2.2250738585072014e-308, 0.1, 1e-7, 1e15, 1e16 - 2, 1e16, 9.2e18, -9.2e18, 9.3e18, 2^53, 2^53 + 2, 123456789012345.6, 1e21, 1e22, ±1.7976931348623157e308. Random: `Int64.float_of_bits` of random 64 bits, finite ones only |
| `xc-order` | `sort()` and `<` on lists of floats, strings, bytes, tuples `(Float, Str)` | the float and string edge lists above, shuffled by the seed |
| `xc-rand` (`--seed` = the cross-check seed) | `int` with (0, 1), (0, 2), (-5, 5), (0, 2^62 + 1), (Int min, Int max), `float`, `bytes` with 0, 1, 7, 8, 9, 17 | 50 draws each |
| `xc-panic` | each through `catch(|| ...)`, printing the `Result`: list index -1, len, Int max; bytes index; missing map key with Str (holding `"`, `\n`, `\0`) and Int keys; `bytes([256])`, `bytes([-1])`; `/` and `%` by 0; `<<`, `>>` and `shift_right_unsigned` by -1; UInt `/` by 0; `rand.int(3, 3)`; `rand.bytes(-1)` | as listed |

Keep each program's output under about 200 KB; `programs` is cheap to call.

- [ ] **Step 2: Add `same` and call it**: at the end of the main `let () =`, before `drain ()`:

```ocaml
List.iter
  (fun (name, src, rseed) ->
    let grants = { Interp.no_grants with seed = rseed } in
    same name ~grants src)
  (Crosscheck.programs ~seed:Crosscheck.seed);
```

- [ ] **Step 3: Run it**

Run: `LIMEN_TEST_ONLY=xc- dune exec ./test/test_limen.exe`
Expected: all `xc-*` pass. Then `LIMEN_CROSSCHECK_SEED=1 LIMEN_TEST_ONLY=xc- dune exec ./test/test_limen.exe` and with `=2`: pass. A difference found here is a real bug: settle it by the disagreement rule (Global Constraints) in this task, pin it with a named `out` test, and write the rule into SPEC.md if it was silent there.

- [ ] **Step 4: Check that it catches drift, by hand, once.** Make the interpreter's `to_upper` also map `é`'s first byte (`\xc3`) to `\xe3`; run `LIMEN_TEST_ONLY=xc-str dune exec ./test/test_limen.exe`. Expected: `FAIL xc-str/...: line N: interpreter ..., native ...`. Revert. Do the same to `Str_map_case` in `limen_rt.c`; expected FAIL; revert. Nothing of this is committed.

- [ ] **Step 5: Time it**: `LIMEN_TEST_ONLY=xc- dune exec ./test/test_limen.exe` with a warm cache and with `LIMEN_TEST_CACHE=0`. Expected: under 15 s cold on an 8-core machine; if not, cut the random counts.

- [ ] **Step 6: Full verification**, all four commands pass.

- [ ] **Step 7: Commit**

```bash
git add test/crosscheck.ml test/test_limen.ml
git commit -m "test: cross-check every built-in under both backends

One generated program per area runs edge cases and seeded random inputs
through the interpreter and the native build; the first line that differs
fails the run. LIMEN_CROSSCHECK_SEED picks another seed."
```

---

### Task 4: strings defined once

**Files:**
- Modify: `src/runtime/limen_core.h`, `src/core_stubs.c`, `src/core.ml`
- Modify: `src/runtime/limen_rt.c` `Str_split` (516), `Str_trim` (540), `Str_parse_int` (566), `w_utf8_len` (634), `Str_chars` (649), `Int_to_char` (668); remove `is_ws` (539) once nothing uses it (Task 5 removes its last user)
- Modify: `src/interp.ml` `split_on` (438), `utf8_of_code` (464), `utf8_len` (472), `chars` (488), `parse_decimal` (492), and the arms at 2977, 2978, 2985, 2994, 3007
- Test: `test/test_limen.ml`

**Interfaces:**
- Produces, in `limen_core.h`:
  - `LC_FN bool lc_split_next(const char *s, size_t n, const char *sep, size_t m, size_t *pos, size_t *start, size_t *len)`: as `lc_lines_next`; with `m == 0` one piece per byte (none for an empty `s`), else the pieces between non-overlapping occurrences left to right, always at least one. `*pos` is set to `SIZE_MAX` after the last piece.
  - `LC_FN void lc_trim(const char *s, size_t n, size_t *a, size_t *b)`: bounds without leading and trailing space, `\t`, `\n`, `\r`, `\f`.
  - `LC_FN size_t lc_utf8_len(const unsigned char *s, size_t i, size_t n)`: the body of `w_utf8_len`.
  - `LC_FN size_t lc_utf8_encode(int64_t cp, char out[4])`: U+FFFD for a negative value, a surrogate or past 0x10FFFF.
  - `LC_FN bool lc_parse_int(const char *s, size_t n, int64_t *out)`: the body of `Str_parse_int`.
- Produces, in `core.ml`: `split : string -> string -> string array`, `trim : string -> string`, `chars : string -> string array`, `of_code : int64 -> string`, `parse_int : string -> int64 option` (stubs `limen_core_split`, `limen_core_trim`, `limen_core_chars`, `limen_core_of_code`, `limen_core_parse_int`).
- `Str_replace` and the interpreter's `replace_all` stay built on split and join, so `replace` follows `split`.

- [ ] **Step 1: Write the pinned test**

```ocaml
out "str-nul-builtins"
  (main_wrap {|    let z = bytes([0]).to_str()
    out.print("{("1" + z).parse_int()} {z.trim().len()} {("a" + z + "b").split(z)} {(z + "x" + z).chars().len()} {(" 7" + z).trim().len()}")|})
  "None 1 [\"a\", \"b\"] 3 2\n";
```

- [ ] **Step 2: Run it**: `LIMEN_TEST_ONLY=str-nul,xc-str dune exec ./test/test_limen.exe`; expected PASS before the move (both backends already agree; the test pins it across the move).

- [ ] **Step 3: Add the five core functions, stubs and externals; make the runtime functions listed above wrappers and the interpreter helpers calls to `Core`** (`split_on s sep = Array.to_list (Core.split s sep)`; delete `utf8_len`, `chars`, `utf8_of_code`, `parse_decimal` if nothing else uses them).

- [ ] **Step 4: Run** `LIMEN_TEST_ONLY=str-,xc- dune exec ./test/test_limen.exe`; expected all pass.

- [ ] **Step 5: Full verification**, all four commands pass.

- [ ] **Step 6: Commit** `runtime: split, trim, chars, to_char and parse_int written once`.

---

### Task 5: float formatting and parsing defined once

**Files:**
- Modify: `src/runtime/limen_core.h`, `src/core_stubs.c`, `src/core.ml`
- Modify: `src/runtime/limen_rt.c` `fmt_float` (406), `Float_to_fixed` (680), `Str_parse_float` (694), then remove `is_ws` (539) if unused
- Modify: `src/interp.ml` `fmt_float` (285), `parse_float` (457), the `to_fixed` arm (3035-3041)
- Test: `test/test_limen.ml`

**Interfaces:**
- Produces, in `limen_core.h`:
  - `#define LC_FLOAT_MAX 32` and `LC_FN size_t lc_fmt_float(char *out, size_t cap, double x)`: `%.1f` when `x == floor(x) && fabs(x) < 1e16`, else `%.15g`; returns the length.
  - `#define LC_FIXED_MAX 352` and `LC_FN size_t lc_to_fixed(char *out, size_t cap, double x, int64_t d)`: `nan`, `inf`, `-inf`; `d` clamped to 0..30; `%.*f`. 352 covers `-DBL_MAX` with 30 decimals (341 bytes).
  - `LC_FN bool lc_parse_float(const char *s, size_t n, double *out)`: the body of `Str_parse_float`, its 408-byte stack copy, `malloc` and `free` past that.
- Produces, in `core.ml`: `fmt_float : float -> string`, `to_fixed : float -> int64 -> string`, `parse_float : string -> float option`. The runtime keeps `fmt_float(char *, size_t, double)` as a wrapper, since `Float_show` and the interpolation code call it.

- [ ] **Step 1: Write the pinned tests**

```ocaml
out "float-parse-long"
  (main_wrap {|    let mut zs: List[Str] = []
    for i in 0..<10000 {
        zs.push("0")
    }
    let z = zs.join("")
    let a = ("1." + z.slice(0, 405)).parse_float()
    let b = ("1." + z.slice(0, 406)).parse_float()
    let c = ("1." + z.slice(0, 407)).parse_float()
    out.print("{a} {b} {c} {(z + "2.5").parse_float()} {(z + "1e").parse_float()}")|})
  "Some(1.0) Some(1.0) Some(1.0) Some(2.5) None\n";
out "float-parse-nul" (main_wrap {|    out.print("{("1" + bytes([0]).to_str()).parse_float()}")|}) "None\n";
out "float-to-fixed-huge"
  (main_wrap {|    out.print("{1.7976931348623157e308.to_fixed(30).len()} {(-1.7976931348623157e308).to_fixed(30).len()} {(-1.7976931348623157e308).to_fixed(30).slice(0, 4)}")|})
  "340 341 -179\n";
```

- [ ] **Step 2: Run them**: `LIMEN_TEST_ONLY=float- dune exec ./test/test_limen.exe`; expected PASS (pinned across the move). If one fails, it is a real difference: settle it by the disagreement rule in this task.

- [ ] **Step 3: Add the three core functions, stubs and externals; make the runtime functions wrappers; the interpreter's `fmt_float` becomes `Core.fmt_float`, `parse_float` becomes `Core.parse_float`, the `to_fixed` arm `VStr (Core.to_fixed x d)`.**

- [ ] **Step 4: Run** `LIMEN_TEST_ONLY=float-,xc- dune exec ./test/test_limen.exe`; expected all pass.

- [ ] **Step 5: Full verification**, all four commands pass.

- [ ] **Step 6: Commit** `runtime: float printing, to_fixed and parse_float written once`.

---

### Task 6: scalar ordering and escaping defined once

**Files:**
- Modify: `src/runtime/limen_core.h`, `src/core_stubs.c`, `src/core.ml`
- Modify: `src/runtime/limen_rt.c` `Str_cmp` (386), `buf_escaped` (412), `Float_cmp` (433)
- Modify: `src/interp.ml` `show` (345), `compare_v` (402-403)
- Test: `test/test_limen.ml`

**Interfaces:**
- Produces, in `limen_core.h`: `LC_FN int lc_float_cmp(double a, double b)` (NaN first and equal to itself, -0.0 equal to 0.0; -1, 0 or 1), `LC_FN int lc_bytes_cmp(const char *a, size_t an, const char *b, size_t bn)` (memcmp, then length; -1, 0 or 1), `LC_FN size_t lc_escaped_len(const char *s, size_t n)` and `LC_FN void lc_escape(const char *s, size_t n, char *out)` (SPEC.md section 13's escaping, quotes included).
- Produces, in `core.ml`: `float_cmp : (float [@unboxed]) -> (float [@unboxed]) -> (int [@untagged])` and `str_cmp : string -> string -> int`, both `[@@noalloc]`; `escaped : string -> string` (quoted).
- `buf_escaped` writes through a 512-byte stack buffer, or `w_raw` and `free` past it.

- [ ] **Step 1: Write a sort-heavy timing program** at `_build/bench-builtins.lmn` (not committed; Task 9 reuses it): build 200 000 strings `"k{(i * 7919) % 200000}"` and floats `((i * 7919) % 200000).to_float() / 7.0`, `sort()` each, print the first and last of each, print the string list's first 1000 inside a list (escaping). Time it before the change: `time _build/default/bin/main.exe run _build/bench-builtins.lmn > /dev/null`, best of 3.

- [ ] **Step 2: Add the core functions, stubs and externals; make the runtime functions wrappers; `compare_v` uses `Core.float_cmp` and `Core.str_cmp` (for `VStr` and `VBytes`), `show` writes `Core.escaped x` for a nested `VStr`.**

- [ ] **Step 3: Run** `LIMEN_TEST_ONLY=xc-,sort dune exec ./test/test_limen.exe`; expected all pass.

- [ ] **Step 4: Time the program again**, best of 3. Expected: within 5 % of Step 1. If over, keep `lc_*` for native, put `compare_v`'s old `compare` back for the offender, and say so in the commit message.

- [ ] **Step 5: Full verification**, all four commands pass.

- [ ] **Step 6: Commit** `runtime: Float and Str order and string escaping written once`.

---

### Task 7: `Rand`, the OS generator and file times defined once

**Files:**
- Modify: `src/runtime/limen_core.h`, `src/core_stubs.c`, `src/core.ml`
- Modify: `src/runtime/limen_rt.c` `w_rand_next` (988), `w_os_random` (995), `Cap_rand_int` (1032), `Cap_rand_float` (1045), `Cap_rand_bytes` (1050), `Cap_fs_info` (1335-1339)
- Modify: `src/ffi_stubs.c` `limen_mtime_ms` (687-697), `limen_os_random` (703-740), the stat in `limen_fs_stat` (881-885); include `runtime/limen_core_inline.h` and `runtime/limen_core.h` next to `limen_sandbox.h` (670); drop the duplicate `#include <fcntl.h>`
- Modify: `src/interp.ml` `rand_next` (203), `rand_int` (241), `rand_bytes` (256), the `CRand, "float"` arm (3157)
- Test: `test/test_limen.ml`

**Interfaces:**
- Produces, in `limen_core.h`: `#define LC_GOLDEN 0x9E3779B97F4A7C15ULL`; `LC_FN uint64_t lc_mix64(uint64_t z)` (splitmix64's output from an advanced state); `LC_FN uint64_t lc_rand_skip(uint64_t span)` (`-span % span`); `LC_FN double lc_rand_unit(uint64_t z)` (`(z >> 11) * 0x1.0p-53`); `LC_FN void lc_rand_le(uint64_t z, unsigned char *p, size_t k)` (the low `k <= 8` bytes, low first); `LC_FN int lc_os_random(unsigned char *p, size_t n)` (0, or -1 if the generators gave fewer bytes); `LC_FN int64_t lc_mtime_ms(const struct stat *st)`.
- State stays with each backend: native keeps its atomic add on `g_rand_state` and calls `lc_mix64` on the result; the interpreter adds `LC_GOLDEN` to `rand_state` and calls `Core.mix64`.
- Produces, in `core.ml`: `mix64`, `rand_skip : (int64 [@unboxed]) -> (int64 [@unboxed])`, `rand_unit : (int64 [@unboxed]) -> (float [@unboxed])`, all `[@@noalloc]`; `rand_le : bytes -> int -> int -> int64 -> unit` (buffer, offset, count, z) `[@@noalloc]`. `Interp.os_random` keeps its stub name `limen_os_random`.

- [ ] **Step 1: Write the pinned test** (no fixed answer: the backends must agree):

```ocaml
same "rand-wide-span" ~grants:{ Interp.no_grants with seed = Some 7 }
  (main_wrap {|    let r = root.rand
    for i in 0..<20 {
        out.print("{r.int(-9223372036854775807 - 1, 9223372036854775807)} {r.int(0, 3)} {r.float()} {r.bytes(i)}")
    }|});
```

- [ ] **Step 2: Run it** with `LIMEN_TEST_ONLY=rand,xc-rand,fs-info`; expected PASS before the move.

- [ ] **Step 3: Add the core functions, stubs and externals; make the native, `ffi_stubs.c` and interpreter code above call them.**

- [ ] **Step 4: Run** `LIMEN_TEST_ONLY=rand,xc-,fs,std-uuid dune exec ./test/test_limen.exe`; expected all pass.

- [ ] **Step 5: Linux**: `dev/linux/run.sh arm64 dune exec ./test/test_limen.exe` (exercises `getrandom` and `st_mtim`); expected all pass.

- [ ] **Step 6: Full verification**, all four commands pass.

- [ ] **Step 7: Commit** `runtime: Rand, the system generator and file times written once`.

---

### Task 8: panic messages defined once

**Files:**
- Modify: `src/runtime/limen_core.h`, `src/core_stubs.c`, `src/core.ml`
- Modify: `src/runtime/limen_rt.c` `w_panic_index` (446), `w_panic_key` (488), `Bytes_from_list` (616), `Bytes_at` (627)
- Modify: `src/interp.ml` lines 2383, 2391, 2394, 2398, 2674, 2820, 2828
- Test: `test/test_limen.ml` (`xc-panic` covers them; no new test)

**Interfaces:**
- Produces, in `limen_core.h`, each `snprintf`-style (writes at most `cap`, returns the full length): `LC_FN size_t lc_msg_list_index(char *out, size_t cap, int64_t i, int64_t len)` ("index %lld is out of range for a list of length %lld"), `lc_msg_bytes_index` (same arguments, "... for bytes of length %lld"), `LC_FN size_t lc_msg_byte_range(char *out, size_t cap, int64_t v)` ("byte %lld is out of range 0 to 255"), `LC_FN size_t lc_msg_key(char *out, size_t cap, const char *key, size_t kn)` ("key " + key + " is not in the map"; the key's text stays each backend's own printing).
- Produces, in `core.ml`: `msg_list_index : int64 -> int -> string`, `msg_bytes_index : int64 -> int -> string`, `msg_byte_range : int64 -> string`, `msg_key : string -> string`.
- Each backend still raises the panic and adds the location itself.

- [ ] **Step 1: Run the panic tests before**: `LIMEN_TEST_ONLY=xc-panic,index,bytes,map dune exec ./test/test_limen.exe`; expected PASS.

- [ ] **Step 2: Add the four core functions, stubs and externals; switch the native and interpreter sites above to them.** `w_panic_key` sizes its buffer from `lc_msg_key`'s return.

- [ ] **Step 3: Run the same command**; expected PASS.

- [ ] **Step 4: Full verification**, all four commands pass.

- [ ] **Step 5: Commit** `runtime: panic messages for indexes, bytes and map keys written once`.

---

### Task 9: the speed budget, SPEC.md and ROADMAP.md

**Files:**
- Modify: `ROADMAP.md:118-120`
- Modify: `SPEC.md` only if Tasks 3 to 8 settled a difference not yet written there

**Interfaces:**
- Consumes: everything above; `_build/bench-builtins.lmn` from Task 6.

- [ ] **Step 1: Build the base.** `BASE=$(rtk proxy git log --format=%H -1 --grep='limen_core.h, built-ins written once')^`; `git worktree add ../limen-base "$BASE"`; `(cd ../limen-base && dune build)`.

- [ ] **Step 2: Measure both trees**, best of 3 each, same machine, nothing else running:
  - `time (LIMEN_SKIP_NATIVE=1 dune exec ./test/test_limen.exe)` (the interpreter's time on the suite)
  - `time sh selfhost/check.sh`
  - `time _build/default/bin/main.exe run _build/bench-builtins.lmn > /dev/null`

  Expected: each within 5 % of the base. If one is over, find the built-in responsible (`perf` on Linux, Instruments' time profiler or `sample` on macOS, against the interpreter on the slow input), put its interpreter side back to OCaml, keep its cross-check, rerun, and record what went back in the ROADMAP entry.

- [ ] **Step 3: Linux, both architectures**: `dev/linux/run.sh arm64 dune exec ./test/test_limen.exe` and `dev/linux/run.sh amd64 dune exec ./test/test_limen.exe`; expected all pass.

- [ ] **Step 4: ROADMAP.md:118-120**: keep the item's first sentence and add "(done: the built-ins whose answer depends on bytes, Unicode, parsing, float formatting, scalar order, random numbers or a panic message are written once, in `src/runtime/limen_core.h`, and called by both backends; the rest are checked against each other on every test run by generated programs, `LIMEN_CROSSCHECK_SEED` for more inputs. Behaviour changes for the release notes: natively `"\r".lines()` is `[]` and `"a\n\r".lines()` is `["a"]`, as SPEC.md says; in the interpreter `UInt64.to_float()` rounds once, so 2^63 + 1025 gives 9223372036854777856)" plus any difference Tasks 3 to 8 settled and any built-in put back in Step 2.

- [ ] **Step 5: Full verification**, all four commands pass. `git worktree remove ../limen-base`.

- [ ] **Step 6: Commit** `ROADMAP: one definition of each built-in, done`.
