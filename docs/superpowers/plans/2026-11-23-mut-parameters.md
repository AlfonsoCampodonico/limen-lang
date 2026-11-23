# `mut` parameters Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A function can declare `mut` parameters (including `mut self`) that change the caller's place for the length of the call, in both compilers and both backends, with the formatter, language server, grammars and docs, and the self-hosted compiler rewritten to use them while staying at the bootstrap fixed point.

**Architecture:** The parser records `mut` on a parameter (`prm_mut` in OCaml, `mutable` in Limen, as `env.Var` names it) and wraps a `mut place` argument in a new expression node `EMut`. The checker validates the place (E0205, E0209), the marker (new E0213), overlap (new E0214) and use as a value (new E0215), and rewrites the receiver of a `mut self` call to `EMut recv` so both backends see one form. The interpreter binds a `mut` parameter to the caller's own variable cell (a path place gets a cell written back on every exit). Lowering emits a new IR call `CallMut` whose place arguments are a variable and steps; IR variables gain `by_ref`, C passes a pointer to the caller's slot, and Own, Pure and Hoist treat a place argument as borrowed and changed, never moved, kept or reused.

**Tech Stack:** OCaml compiler (`src/`, dune), self-hosted compiler in Limen (`selfhost/`), C runtime (`src/runtime/`), GBNF and TextMate grammars, the book (`../limen-book`, Python build).

**Spec:** `docs/superpowers/specs/2026-11-19-0.8-design-questions-design.md`, section "1. `mut` parameters", "Order and releases" items 4 and 5, and "Left to the builder". Sections 2 and 3 (E0304, groups) are done and out of scope.

## Global Constraints

- Every change goes into both compilers (`src/` and `selfhost/`) and both backends (interpreter and C), and the self-hosted compiler stays at the bootstrap fixed point after every task.
- Before any commit: `dune build`, `dune exec ./test/test_limen.exe`, `sh selfhost/check.sh` and `sh selfhost/bootstrap.sh` all pass (with the book checked out at `../limen-book`, check.sh also checks its examples). Never rebuild while check.sh runs.
- Commit messages: no `Co-Authored-By`, no "Generated with", no Claude attribution of any kind. No emojis. British spelling, no em dashes. Commit locally; never push (the book included).
- No Rust-isms in names or syntax. `mut` is already a keyword; no new keyword.
- None of this breaks a program that builds today: every new error is reachable only from code that writes `mut` on a parameter or an argument.
- New error codes: E0213 (a `mut` marker that does not match the parameter), E0214 (overlapping `mut` places in one call), E0215 (a function with a `mut` parameter used as a value). Messages say what is wrong, then what to write, and are identical in both compilers (check.sh compares them).
- RTK may rewrite shell commands; `rtk proxy <cmd>` runs one raw.

## Review Focus

1. A plain argument naming the list a `mut` element comes from, `g(out, mut xs[0], xs)`: the plain one is a snapshot taken before the call, so natively it must be retained before the list is made unique for the element pointer; `g` prints `[1, 2]` and the caller then prints `[5, 2]`. Tests in Tasks 4 and 5.
2. The last use of a `mut` parameter inside the callee (`let ys = xs`, `return xs`, a capture) must copy, never move out of the caller's slot: the caller still holds its value and nothing leaks or double-frees under ASan. Tests in Tasks 4 and 5.
3. A panic or cancellation halfway through the callee: a `defer` in the caller (natively after the longjmp) and one in the callee see what was written so far, for a variable root and for a path place `mut s.items`. Tests in Tasks 4 and 5.
4. Section 14: a call with a `mut` argument inside a loop runs on every iteration, and a pure call after `grow(mut xs)` does not reuse a result computed before it (`len_of(xs)` before and after differ). Tests in Tasks 4 and 5.
5. Copy-on-write hoisting: a list passed as `mut` inside a loop to a callee that keeps a copy (`keep.push(xs)` then `xs.push(1)`) must not have its unique check hoisted, or the kept copy changes. Test in Task 5.

---

### Task 1: Syntax: AST, parsers, formatter, dumps

**Files:**
- Modify: `src/ast.ml:128-147` (`ENamed` neighbour, `param`)
- Modify: `src/parser.ml:376-383` (`parse_arg`), `930-950` (`parse_param`), `969-978` (`test_param`), `1013` (fn params), `1066-1082` (interface params), `1141-1156` (foreign params), `180-190` (function types), closure parameters
- Modify: `src/fmt.ml:331` (arguments), `758` (parameters); `src/astdump.ml:50,127`
- Modify every exhaustive walk over `expr_desc` that `dune build` flags: `src/free.ml:28,87`, `src/coverage.ml:46`, `src/derive.ml:520`, `src/modules.ml:468`, `src/refs.ml:176`, `src/lsp.ml:105`, `src/interp.ml:637`, `src/interp.ml:2298`, `src/lower.ml:305`; `src/check.ml:961` (pass-through only)
- Modify: `selfhost/ast.lmn:126-165,257-264`; `selfhost/parser.lmn:536-545` (`parse_arg`), `1309-1330` (`parse_param`, `parse_fn_param`), `1425-1441` (`test_param`), `1560-1580` (interface), `1700-1712` (foreign), `257-305` (function types), closure parameters; `selfhost/ast_print.lmn:130,199,336`; `selfhost/free.lmn:49,149`, `selfhost/ids.lmn:147`, `selfhost/rename.lmn:438`, `selfhost/walk.lmn:83`, `selfhost/typecheck.lmn:1383`, `selfhost/lower.lmn:610`, `selfhost/checker.lmn:151` (pass-through)
- Test: `test/test_limen.ml` (new section `(* ---------- mut parameters ---------- *)` before the final `drain ()`), `selfhost/tests/mut-syntax.lmn`, `selfhost/tests/err-mut-*.lmn`

**Interfaces:**
- Produces (OCaml): `param = { prm_name; prm_span; prm_ty; prm_default; prm_mut : bool }`; `expr_desc` gains `EMut of expr` (a `mut place` call argument; its span runs from `mut` to the end of the place). `prm_span` stays the name's span.
- Produces (Limen): `ast.Param` gains `mutable: Bool`; `ExprDesc` gains `EMut(Expr)`.
- Produces: astdump and ast_print write a `mut` parameter as `mut name:Type` and `EMut x` as `mut ` followed by `x`'s dump.
- Produces: until Task 2, both checkers type `EMut x` as `x` and ignore `prm_mut`.

- [ ] **Step 1: Write the failing tests**

```ocaml
let mut_src = {|struct Buf {
    items: List[Int],
}

fn add(mut self: Buf, x: Int) {
    self.items.push(x)
}

fn bump(mut n: Int, by: Int) {
    n = n + by
}

fn main(root: Root) {
    let mut b = Buf { items: [] }
    b.add(1)
    let mut k = 0
    bump(mut k, 2)
    bump(by: 3, n: mut k)
}
|} in
ok "mut-fmt" (Fmt.format ~file:"mut-fmt.lmn" mut_src = mut_src) (Fmt.format ~file:"mut-fmt.lmn" mut_src);
ok "mut-fmt-spacing" (Fmt.format ~file:"mut-fmt2.lmn" "fn f(mut   n: Int) {}\nfn main(root: Root) { let mut k = 0\n f(mut    k) }\n"
  = "fn f(mut n: Int) {}\n\nfn main(root: Root) {\n    let mut k = 0\n    f(mut k)\n}\n") "spacing";
errs "mut-parses" mut_src [];
says "mut-default" "fn f(mut n: Int = 0) {}\nfn main(root: Root) {}" "E0123" "`n` is `mut`, so it cannot have a default";
says "mut-interface" "interface I {\n    fn f(self, mut n: Int)\n}\nfn main(root: Root) {}" "E0003" "an interface method cannot take a `mut` parameter";
says "mut-foreign" "foreign \"c\" {\n    fn abs(mut n: Int) -> Int\n}\nfn main(root: Root) {}" "E0003" "a foreign function cannot take a `mut` parameter";
says "mut-fn-type" "fn f(g: fn(mut Int)) {}\nfn main(root: Root) {}" "E0003" "a function type cannot have a `mut` parameter";
says "mut-closure" (main_wrap "    let f = |mut x: Int| x") "E0003" "a closure cannot take a `mut` parameter";
says "mut-test-param" "test \"t\" (mut fs: Fs) {}\n" "E0003" "a test's parameters cannot be `mut`";
errs "mut-outside-call" (main_wrap "    let mut x = 1\n    let y = mut x") [ "E0003" ];
```

(`says` already parses and runs the formatter round trip through `compile`.) Add `selfhost/tests/mut-syntax.lmn` (the declarations and calls of `mut_src`, without `main`, so check.sh only parses, loads and checks it until Task 5) and one `selfhost/tests/err-mut-N.lmn` per parse error above.

- [ ] **Step 2: Run them to see them fail**

Run: `dune build 2>&1 | head; dune exec ./test/test_limen.exe 2>&1 | grep -E "mut-|passed"`
Expected: FAIL (`mut` is an unexpected token).

- [ ] **Step 3: Implement the syntax in `src/`**

`parse_param ~mut_ok st` reads an optional `KW "mut"` before the name: only a `fn` item's parameters pass `~mut_ok:true`; the others raise the E0003 messages above. `mut` with `= default` raises E0123 "`n` is `mut`, so it cannot have a default: the caller always passes the place". `parse_arg` accepts `mut` after an optional label: `mk (EMut place) (merge mut_span place.espan)`, where `place` is `parse_expr`. A `mut` inside `fn(...)` types and closure parameters raises its E0003. The formatter prints `mut ` before a marked parameter and before `EMut`'s expression. Walks recurse into `EMut`; `Interp.eval` and `Lower.lower` fail with `"internal: a mut argument outside a call"` for a bare `EMut`; `check_expr` types `EMut x` as `x` for now.

- [ ] **Step 4: Mirror it in `selfhost/`** with the same messages and dump formats.

- [ ] **Step 5: Run everything**

Run: `dune build && dune exec ./test/test_limen.exe && sh selfhost/check.sh && sh selfhost/bootstrap.sh`
Expected: all tests pass, check.sh exit 0, "fixed point".

- [ ] **Step 6: Commit**

```bash
git add src selfhost test
git commit -m "Parse mut parameters and mut arguments in both compilers"
```

---

### Task 2: Checker: declaring and calling

**Files:**
- Modify: `src/check.ml`: tables at `625-631`; `check_var` `1204-1263`; `order_args`/`keep_order` `1274-1385`; `check_call` `1398-1560`; `field_of` `1588-1628`; `method_arity` `1631-1640`; `check_method_value` `1703-1940` (user-method branch and built-in methods); `check_place` `1944-1967`; `SAssign` `2817-2828`; `check_main` `2885-2896`; `check_fn` `3050-3076`; registration `3462`
- Modify: `src/embed.ml:138-150` (forwarders); `src/diag.ml:56-69` (explanations, E0205 text)
- Modify: `selfhost/env.lmn:62,106` (`Cx.param_muts`); `selfhost/typecheck.lmn:591,881` (registration), `1019-1040` (`check_main`), `1112` (binding); `selfhost/checker.lmn`: `check_var` 517, `keep_order`/`pure` 837-887, `check_call` 1012, `field_of` 1211, `method_arity` 1268, `check_method_value` 1535, `builtin_method_call` 1710, `check_place` 1968, `SAssign` in `check_stmt` 2891; `selfhost/forwarders.lmn:217`
- Test: `test/test_limen.ml`, `selfhost/tests/check/mut-calls.lmn`, `selfhost/tests/check/err-mut-N.lmn`

**Interfaces:**
- Consumes: `prm_mut`, `EMut` (Task 1).
- Produces (OCaml): `param_muts : (string, bool list) Hashtbl.t`, keyed like `param_names` (function name, or `Type::method` with `self` first), filled only for functions with at least one `mut` parameter. `has_mut key = List.mem true (Hashtbl.find_opt param_muts key |> Option.value ~default:[])`.
- Produces (Limen): `Cx.param_muts: Map[Str, List[Bool]]`, same keys and rule.
- Produces: after checking, the receiver of a call to a `mut self` method (user methods, forwarders and methods a file adds to built-in types) is rewritten to `EMut recv` (span of `recv`), in place in OCaml and as a rewrite in `st.rewrites` in Limen. Every `mut` argument in a checked tree is an `EMut` whose parameter is `mut`; backends rely on this.

- [ ] **Step 1: Write the failing tests**

```ocaml
let mut_decls = "struct Buf {\n    items: List[Int],\n}\n\nfn add(mut self: Buf, x: Int) {\n    self.items.push(x)\n}\n\nfn bump(mut n: Int) {\n    n = n + 1\n}\n\nfn plain(n: Int) -> Int {\n    n\n}\n\n" in
errs "mut-call-ok" (mut_decls ^ main_wrap "    let mut k = 0\n    bump(mut k)\n    let mut b = Buf { items: [1] }\n    b.add(1)\n    let mut bs = [b]\n    bs[0].add(2)\n    bump(mut bs[0].items[0])\n    let mut m = map([(\"a\", b)])\n    m[\"a\"].add(3)\n    bump(mut m[\"a\"].items[0])") [];
errs "mut-param-assign" "fn f(mut xs: List[Int]) {\n    xs = [1]\n    xs[0] = 2\n    xs.push(3)\n}\nfn main(root: Root) {}" [];
errs "mut-root-let" (mut_decls ^ main_wrap "    let k = 0\n    bump(mut k)") [ "E0205" ];
errs "mut-root-param" (mut_decls ^ "fn g(n: Int) {\n    bump(mut n)\n}\nfn main(root: Root) {}") [ "E0205" ];
errs "mut-in-closure" (mut_decls ^ main_wrap "    let mut k = 0\n    let f = || bump(mut k)") [ "E0205" ];
errs "mut-self-not-mut" (mut_decls ^ main_wrap "    let b = Buf { items: [] }\n    b.add(1)") [ "E0205" ];
errs "mut-not-place" (mut_decls ^ main_wrap "    bump(mut plain(1))") [ "E0209" ];
errs "mut-bytes" (mut_decls ^ main_wrap "    let mut bs = bytes([1])\n    bump(mut bs[0])") [ "E0209" ];
says "mut-missing" (mut_decls ^ main_wrap "    let mut k = 0\n    bump(k)") "E0213" "`bump` changes its parameter `n`, so the argument must be written with `mut`";
says "mut-extra" (mut_decls ^ main_wrap "    let mut k = 0\n    let j = plain(mut k)") "E0213" "`plain` does not change its parameter `n`, so the argument is written without `mut`";
says "mut-to-closure" (main_wrap "    let mut k = 0\n    let f = |n: Int| n\n    let j = f(mut k)") "E0213" "`mut` is only for an argument whose parameter is declared `mut`";
errs "mut-to-builtin" (main_wrap "    let mut k = 1\n    let ys = [1, 2].take(mut k)") [ "E0213" ];
errs "mut-piped" (mut_decls ^ main_wrap "    let mut k = 0\n    k |> bump") [ "E0213" ];
says "mut-exact-type" "interface Named {\n    fn name(self) -> Str\n}\nstruct P {\n    x: Int,\n}\nfn name(self: P) -> Str {\n    \"p\"\n}\nfn rename(mut n: dyn Named) {}\nfn main(root: Root) {\n    let mut p = P { x: 1 }\n    rename(mut p)\n}\n" "E0201" "a `mut` argument must have exactly the parameter's type";
says "mut-fn-value" (mut_decls ^ main_wrap "    let g = bump") "E0215" "`bump` has a `mut` parameter, so it can only be called, not used as a value";
says "mut-method-value" (mut_decls ^ main_wrap "    let mut b = Buf { items: [] }\n    let g = b.add") "E0215" "`add` has a `mut` parameter, so it can only be called, not used as a value";
errs "mut-main" "fn main(mut root: Root) {}" [ "E0303" ];
errs "mut-forwarder" "struct Inner {\n    n: Int,\n}\nfn bump(mut self: Inner) {\n    self.n = self.n + 1\n}\nstruct Outer {\n    Inner,\n}\nfn main(root: Root) {\n    let mut o = Outer { Inner: Inner { n: 0 } }\n    o.bump()\n}\n" [];
errs "mut-builtin-type-method" "fn bump(mut self: Int) {\n    self = self + 1\n}\nfn main(root: Root) {\n    let mut k = 0\n    k.bump()\n}\n" [];
(* the fixes: missing marker added, extra one removed, and the result checks clean *)
List.iter (fun (name, body) ->
    let src = mut_decls ^ main_wrap body in
    let _, ds, _ = compile name src in
    let fixed = apply_fixes src ds in
    let _, ds2, _ = compile (name ^ "-fixed") fixed in
    ok name (codes ds = [ "E0213" ] && Diag.errors ds2 = []) fixed)
  [ ("mut-fix-missing", "    let mut k = 0\n    bump(k)"); ("mut-fix-extra", "    let mut k = 0\n    let j = plain(mut k)") ];
ok "mut-no-fix-piped" (let _, ds, _ = compile "mut-piped2" (mut_decls ^ main_wrap "    let mut k = 0\n    k |> bump") in List.for_all (fun (d : Diag.t) -> d.fix = None) ds) "a piped value got a fix";
ok "mut-explained" (List.for_all (fun c -> List.mem_assoc c Diag.explanations) [ "E0213"; "E0214"; "E0215" ]) "no explanation";
```

Copy each erroring case into `selfhost/tests/check/err-mut-N.lmn` and the clean ones into `selfhost/tests/check/mut-calls.lmn`.

- [ ] **Step 2: Run them to see them fail**

Run: `dune exec ./test/test_limen.exe 2>&1 | grep -E "mut-|passed"`
Expected: FAIL (no E0213/E0215; `mut-root-let` passes vacuously or not at all).

- [ ] **Step 3: Implement the rules in `src/check.ml`**

- Binding: `check_fn` binds a `mut` parameter with `vmut = true` (still `KParam`). `check_main` refuses any `mut` parameter with its existing E0303.
- A call to a function or user method with `param_muts`, after `order_args` (arguments in parameter order): for each argument and its flag, `EMut p` with a `mut` parameter runs `check_place` on `p` (E0205, E0209, E0310), the `Bytes` refusal that `SAssign` makes (factor it into `check_target ctx f lhs`, used by both), then checks `p` and unifies it with the parameter type without `expect`'s coercions; a mismatch is E0201 "a `mut` argument must have exactly the parameter's type: expected `T`, found `U`". `EMut p` for a plain parameter is E0213 "`f` does not change its parameter `x`, so the argument is written without `mut`", fix "remove `mut`" (deletes from the `EMut` span's start to `p`'s start), then `p` is checked as a plain argument. A plain argument for a `mut` parameter is E0213 "`f` changes its parameter `x`, so the argument must be written with `mut`", with fix "add `mut`" (inserts `"mut "` at the argument's start) only when the argument is an `EVar`, `EField` or `EIndex` that starts after the callee (a piped value starts before it: no fix, note "a value piped with `|>` cannot be passed as `mut`; call `f(mut x)`"). For methods the message names the method, as `add`.
- Any other callee (closure, function value, enum variant, foreign function, `assert` and the other built-in functions, built-in methods): each `EMut` argument is E0213 "`mut` is only for an argument whose parameter is declared `mut`, and this callee has none", fix "remove `mut`", then checked plainly.
- `mut self`: where `check_method_value` resolves a user method (or a file's method on a built-in type) whose key has `true` first in `param_muts`, it runs `check_place` on the receiver and rewrites the receiver to `EMut recv`.
- `keep_order` treats an `EMut` argument as staying in the call; any index or key expression inside its place that `pure` rejects is moved to a `$argN` temporary in written order like the other arguments, and the place rewritten to read the temporary.
- `check_var` reports E0215 for a name whose `param_muts` holds `true` and that is not being called (`!calling` false), with note "a `mut` argument is a place, which only a direct call can pass: call `f(...)` by name". `method_arity` returns `None` for a method with a `mut` parameter, and `field_of` reports E0215 for it instead of E0103.
- `Embed.forwarder` copies `prm_mut` onto the forwarder's parameters (`self` takes the method's own `self` flag) and wraps each `mut` parameter's argument in `EMut`.
- `diag.ml`: explanations for E0213, E0214 and E0215 (each with a two-line example), and the E0205 text gains "A `mut` parameter is a `mut` variable for the length of the call."

- [ ] **Step 4: Mirror it in `selfhost/`** with identical codes, messages, notes, fixes and rewrites.

- [ ] **Step 5: Run everything** (same command and expectation as Task 1 Step 5).

- [ ] **Step 6: Commit** `git commit -m "Check mut parameters and arguments: E0213 and E0215"`

---

### Task 3: Checker: overlap, nested calls, purity

**Files:**
- Modify: `src/check.ml` (end of `check_call` and of the user-method and `push`/`remove` paths of `check_method_value`; `cb_params` in `check_fn` at `3063`)
- Modify: `selfhost/checker.lmn` (same places), `selfhost/typecheck.lmn:1086-1112` (callback parameters)
- Test: `test/test_limen.ml`, `selfhost/tests/check/err-mut-overlap-N.lmn`, `selfhost/tests/check/mut-overlap.lmn`

**Interfaces:**
- Consumes: checked arguments with `EMut` and the rewritten receivers (Task 2).
- Produces (both compilers): `place_path e : (string * pstep list) option` with `pstep = PName of string | PAny` (a field or tuple item by name; any list, array or map index as `PAny`), and `overlaps a b`: same root, and walking both step lists together never meets two different `PName`s before one list ends.

- [ ] **Step 1: Write the failing tests**

```ocaml
let two = "struct P {\n    x: Int,\n    y: Int,\n}\n\nfn two(mut a: Int, mut b: Int) {}\n\nfn pt(mut p: P, mut v: Int) {}\n\nfn next(mut n: Int) -> Int {\n    n\n}\n\nfn take(mut xs: List[Int]) -> Int {\n    0\n}\n\nfn both(mut xs: List[Int], ys: List[Int]) -> Int {\n    0\n}\n\nstruct Buf {\n    items: List[Int],\n}\n\nfn grab(mut self: Buf, mut more: List[Int]) {}\n\n" in
let m body = two ^ main_wrap ("    let mut k = 0\n    let mut j = 0\n    let mut p = P { x: 0, y: 0 }\n    let mut xs = [1, 2]\n    let mut mm = map([(\"a\", 1)])\n    let mut b = Buf { items: [] }\n" ^ body) in
says "mut-overlap-same" (m "    two(mut k, mut k)") "E0214" "this `mut` argument overlaps another one of the same call";
errs "mut-overlap-prefix" (m "    pt(mut p, mut p.x)") [ "E0214" ];
errs "mut-overlap-receiver" (m "    b.grab(mut b.items)") [ "E0214" ];
errs "mut-overlap-elements" (m "    two(mut xs[0], mut xs[1])") [ "E0214" ];
errs "mut-overlap-entries" (m "    two(mut mm[\"a\"], mut mm[\"b\"])") [ "E0214" ];
errs "mut-distinct-fields" (m "    two(mut p.x, mut p.y)") [];
errs "mut-and-plain" (m "    let n = both(mut xs, xs)") [];
says "mut-nested" (m "    two(mut k, next(mut k))") "E0214" "this changes a value that the enclosing call also takes as `mut`";
errs "mut-nested-push" (m "    xs.push(take(mut xs))") [ "E0214" ];
errs "mut-nested-other" (m "    two(mut k, next(mut j))") [];
(let _, ds, infos = compile "mut-pure" (two ^ "fn bump(mut n: Int) {\n    n = n + 1\n}\n\nfn swap_cb(mut f: fn() -> Int) {\n    f = || 1\n}\n\nfn main(root: Root) {}\n") in
 let text n = Check.pure_text (List.find (fun (i : Check.fn_info) -> i.name = n) infos) in
 ok "mut-pure" (ds = [] && text "bump" = "pure" && text "swap_cb" = "pure itself, runs callbacks") (text "bump" ^ "; " ^ text "swap_cb"));
```

- [ ] **Step 2: Run them to see them fail** (`grep -E "mut-overlap|mut-nested|mut-pure"`; expected FAIL).

- [ ] **Step 3: Implement** after a call's arguments are checked (so promoted fields are already rewritten to their full path): collect the call's `mut` places (its `EMut` arguments, an `EMut` receiver, and the receiver of `push` and `remove`) in written order; for each place that overlaps an earlier one, E0214 at the later place with note "a call changes each value through one `mut` argument: `mut p.x, mut p.y` are separate, but two elements of one list or map are not (take them out and put them back)". Then collect the places taken as `mut` by calls nested anywhere inside the call's arguments and receiver (not inside closures), and report each that overlaps one of the call's own places: E0214 at the nested place, "this changes a value that the enclosing call also takes as `mut`", note "make that call first, on its own line, and pass its result". A function-typed `mut` parameter is left out of `cb_params`, so a function taking one is "pure itself, runs callbacks"; nothing else about purity changes.

- [ ] **Step 4: Mirror it in `selfhost/`.**

- [ ] **Step 5: Run everything** (as Task 1 Step 5).

- [ ] **Step 6: Commit** `git commit -m "Refuse overlapping mut arguments: E0214"`

---

### Task 4: Interpreter

**Files:**
- Modify: `src/interp.ml`: `eval` `ECall` cases `2299-2320`, `EMethod` cases `2321-2360`; `call_fn` `2699-2711`; `assign` `2807-2833`; `body_refs` near `637`; the `keeping` path at `2302-2315`
- Test: `test/test_limen.ml`

**Interfaces:**
- Consumes: checked trees where every `mut` argument and `mut self` receiver is `EMut` (Task 2).
- Produces: `arg_cell rt env (a : expr) : value ref * (unit -> unit)`: for `EMut { edesc = EVar n }` the variable's own cell and a no-op; for `EMut p` with a path, `p`'s index and key expressions evaluated once, left to right, the current value read (panicking as `m[k].push(x)` does: `key "b" is not in the map`, or out of range), a fresh cell holding it, and a write-back that stores `settle cell` into the place through the evaluated indexes; for any other argument `ref (eval a)` and a no-op. `call_fn_cells rt d (cells : value ref list)` binds each parameter to its cell instead of a new one.

- [ ] **Step 1: Write the failing tests**, with two helpers that run only the interpreter (Task 5 replaces them with `out` and `panics`):

```ocaml
let interp_out name src want = if selected name then (match run name src with Ok (got, _) -> ok name (got = want) (sprintf "got %S, want %S" got want) | Error e -> ok name false e) in
let interp_panics name src want = if selected name then (match run name src with Error e -> ok name (e = "panic: " ^ want) e | Ok (o, _) -> ok name false ("no panic, output " ^ o)) in
let decls = "struct Buf {\n    items: List[Int],\n}\n\nfn add(mut self: Buf, x: Int) {\n    self.items.push(x)\n}\n\nfn bump(mut n: Int) {\n    n = n + 1\n}\n\nfn keep(mut xs: List[Int]) -> List[Int] {\n    let ys = xs\n    xs.push(9)\n    ys\n}\n\nfn g(out: Stdout, mut x: Int, xs: List[Int]) {\n    x = 5\n    out.print(\"{xs}\")\n}\n\nfn fill(out: Stdout, mut xs: List[Int]) {\n    defer out.print(\"callee {xs}\")\n    xs.push(1)\n    xs.push(2)\n    panic(\"stop\")\n}\n\nfn countdown(mut n: Int, mut log: List[Int]) {\n    if n > 0 {\n        log.push(n)\n        n = n - 1\n        countdown(mut n, mut log)\n    }\n}\n\nfn len_of(xs: List[Int]) -> Int {\n    xs.len()\n}\n\nfn grow(mut xs: List[Int]) {\n    xs.push(0)\n}\n\nfn side(out: Stdout, s: Str, v: Int) -> Int {\n    out.print(s)\n    v\n}\n\nfn setp(mut x: Int, v: Int) {\n    x = v\n}\n\n" in
interp_out "mut-run-basic" (decls ^ main_wrap "    let mut k = 0\n    bump(mut k)\n    bump(mut k)\n    let mut b = Buf { items: [] }\n    b.add(1)\n    b.add(2)\n    let mut bs = [b, b]\n    bump(mut bs[1].items[0])\n    let mut m = map([(\"a\", 1)])\n    bump(mut m[\"a\"])\n    out.print(\"{k} {b.items} {bs[1].items} {m}\")") "2 [1, 2] [2, 2] {\"a\": 2}\n";
interp_out "mut-run-copies" (decls ^ main_wrap "    let mut xs = [1]\n    let ys = keep(mut xs)\n    out.print(\"{xs} {ys}\")") "[1, 9] [1]\n";
interp_out "mut-run-snapshot" (decls ^ main_wrap "    let mut xs = [1, 2]\n    g(out, mut xs[0], xs)\n    out.print(\"{xs}\")") "[1, 2]\n[5, 2]\n";
interp_out "mut-run-panic" (decls ^ main_wrap "    let r = catch(|| {\n        let mut ys: List[Int] = []\n        defer out.print(\"caller {ys}\")\n        fill(out, mut ys)\n        0\n    })\n    let s = catch(|| {\n        let mut b = Buf { items: [] }\n        defer out.print(\"path {b.items}\")\n        fill(out, mut b.items)\n        0\n    })") "callee [1, 2]\ncaller [1, 2]\ncallee [1, 2]\npath [1, 2]\n";
interp_out "mut-run-recursion" (decls ^ main_wrap "    let mut n = 3\n    let mut log: List[Int] = []\n    countdown(mut n, mut log)\n    out.print(\"{n} {log}\")") "0 [3, 2, 1]\n";
interp_out "mut-run-loop" (decls ^ main_wrap "    let mut k = 0\n    for _ in 0..<3 {\n        bump(mut k)\n    }\n    let mut xs = [1]\n    let a = len_of(xs)\n    grow(mut xs)\n    let b = len_of(xs)\n    out.print(\"{k} {a} {b}\")") "3 1 2\n";
interp_out "mut-run-labels" (decls ^ main_wrap "    let mut xs = [0]\n    setp(v: side(out, \"v\", 7), x: mut xs[side(out, \"i\", 0)])\n    out.print(\"{xs}\")") "v\ni\n[7]\n";
interp_out "mut-run-forwarder" "struct Inner {\n    n: Int,\n}\nfn bump(mut self: Inner) {\n    self.n = self.n + 1\n}\nstruct Outer {\n    Inner,\n}\nfn main(root: Root) {\n    let mut o = Outer { Inner: Inner { n: 0 } }\n    o.bump()\n    root.stdout.print(\"{o.n}\")\n}\n" "1\n";
interp_panics "mut-run-missing-key" (decls ^ main_wrap "    let mut m = map([(\"a\", 1)])\n    bump(mut m[\"b\"])") "key \"b\" is not in the map";
interp_out "mut-run-cancel" (decls ^ "fn slow(clock: Clock, mut xs: List[Int]) {\n    xs.push(1)\n    clock.sleep(1000)\n    xs.push(2)\n}\n\n" ^ main_wrap "    let clock = root.clock\n    let r = group g until clock.after(10) {\n        g.go(|| {\n            let mut xs: List[Int] = []\n            defer out.print(\"{xs}\")\n            slow(clock, mut xs)\n        })\n    }") "[1]\n";
```

- [ ] **Step 2: Run them to see them fail** (`grep -E "mut-run"`; expected FAIL with "internal: a mut argument outside a call").

- [ ] **Step 3: Implement** in `eval`: a call to a known function or a user method (including a file's method on a built-in type) whose arguments or receiver contain an `EMut` evaluates every argument with `arg_cell` in order (receiver first), then runs `call_fn_cells` under `Fun.protect ~finally` that runs each write-back, so a return, a panic and a cancellation all leave the place holding what the callee wrote (a variable root needs no write-back: the callee changes the caller's own cell, so `push` stays in place on its `VBuf`). Such a call never goes through `keeping`. The method is looked up from the receiver cell's current value as `call_method` does. `body_refs` walks `EMut`.

- [ ] **Step 4: Run the tests** (expected: the `mut-run` cases pass), then the full suite, check.sh and bootstrap.

- [ ] **Step 5: Commit** `git commit -m "Interpreter: pass mut arguments by place"`

---

### Task 5: Native backend, both compilers

**Files:**
- Modify: `src/ir.ml:6` (`var`), `26-71` (`rhs`), `113-150` (`consumed`, `borrowed`); `src/lower.ml:43-46` (`fresh_var`), `498-501` (`lower_call` for functions), `542-558` (`lower_place`), `870-905` and the later `Call` branches of `lower_method`, `1249-1262` (`lower_fn`); `src/own.ml:100-115` (`rewrite_rhs`), `212-214` (`Return`), `230-236` (`fn`); `src/pure.ml:74-80` (`stmt_atoms`), `103-185` (`analyse`), `186-243` (`pure_rhs`, `assigned`, `key_of`), `312-340` (`same_each_time`, `plan`); `src/hoist.ml:23-36` (`escapes`), `80-120` (`versionable`); `src/cgen.ml:163-166` (`var`), `540` (`rhs_into`), `1332-1378` (`Mutate`), `1523-1526` (`signature`), `1588-1593` (`fn_body`); `selfhost/irdump/irdump.ml:81,136,203`
- Modify: `selfhost/ir.lmn:15-19,62-121,216,233`; `selfhost/lower.lmn:141-145` (`fresh_var`), `845` (`lower_call`), `1022` (`lower_place`), `1506-1549` (`lower_method`), `2043` (`lower_fn`); `selfhost/own.lmn:46,158,297,327`; `selfhost/pure.lmn:94,158,281,307,343,464`; `selfhost/hoist.lmn:64,154`; `selfhost/cgen.lmn:365` (`var`), `1038` (`rhs_into`), `2259` (`mutate`), `2399` (`signature`), `2486` (`fn_body`); `selfhost/ir_print.lmn:129,203,290`
- Test: `test/test_limen.ml`; `selfhost/tests/ir/mut-params.lmn`, `selfhost/tests/c/mut-params.lmn`; move `selfhost/tests/mut-syntax.lmn`'s calls under a `main`

**Interfaces:**
- Consumes: `EMut` arguments and receivers (Task 2), `prm_mut`.
- Produces (OCaml): `type var = { id; name; ty; by_ref : bool }` (true only for a `mut` parameter's variable in its own function; captures and temporaries are false); `type arg = ByValue of atom | ByPlace of var * step list`; `rhs` gains `CallMut of string * arg list` (values consumed, places borrowed and changed). `consumed (CallMut (_, xs))` is the `ByValue` atoms; `borrowed` is `V v` and the step atoms of each `ByPlace`.
- Produces (Limen): `ir.Var` gains `by_ref: Bool`; `pub enum Arg { ByValue(Atom), ByPlace(Var, List[Step]) }`; `Rhs` gains `CallMut(Str, List[Arg])`.
- Produces: irdump and ir_print write `CallMut` as `call f(a#1, mut x#2.0[i#3])` (a place is `mut ` + the variable + its steps as `mutate` writes them) and a by-ref parameter in a function header as `mut x#2: T`.
- Produces (C, identical from both compilers): a by-ref parameter is declared `T* name_N` and every use of it prints `(*name_N)`; it gets no cleanup cell. A `CallMut` is emitted as a block: first `T a{k}_ = <atom>;` for each `ByValue` argument `k`, in order (so retains happen before any copy-on-write), then the pointer chain of each `ByPlace` argument `k` with prefix `r{k}_` from `place_ptr c line prefix v steps : string * ty` (the chain `Mutate` builds today, extracted; `Mutate` calls it with prefix `p` so its C is unchanged), then the call with `a{k}_` and the final `r{k}_{n}_` pointers in argument order.

- [ ] **Step 1: Turn the Task 4 tests into both-backend tests and add the native ones.** Replace every `interp_out` with `out` and `interp_panics` with `panics`, delete the two helpers, and add:

```ocaml
out "mut-native-in-place" (decls ^ main_wrap "    let mut xs: List[Int] = []\n    let mut i = 0\n    while i < 100000 {\n        grow(mut xs)\n        i = i + 1\n    }\n    out.print(\"{xs.len()}\")") "100000\n";
check_native "mut-native-in-place-path" (decls ^ main_wrap "    let mut b = Buf { items: [] }\n    let mut i = 0\n    while i < 100000 {\n        grow(mut b.items)\n        i = i + 1\n    }\n    out.print(\"{b.items.len()}\")") ~want_out:"100000\n" ~want_err_prefix:None;
out "mut-native-move" (decls ^ "fn last(mut xs: List[Int]) -> List[Int] {\n    xs.push(3)\n    xs\n}\n\n" ^ main_wrap "    let mut xs = [1]\n    let ys = last(mut xs)\n    out.print(\"{xs} {ys}\")") "[1, 3] [1, 3]\n";
out "mut-native-hoist" (decls ^ "fn stash(mut xs: List[Int], mut kept: List[List[Int]]) {\n    kept.push(xs)\n    xs.push(1)\n}\n\n" ^ main_wrap "    let mut xs: List[Int] = []\n    let mut kept: List[List[Int]] = []\n    for i in 0..<3 {\n        xs.push(i)\n        stash(mut xs, mut kept)\n    }\n    out.print(\"{kept}\")") "[[0], [0, 1, 1], [0, 1, 1, 1, 2]]\n";
out "mut-native-cse" (decls ^ main_wrap "    let mut xs = [1]\n    let mut total = 0\n    for _ in 0..<3 {\n        total = total + len_of(xs)\n        grow(mut xs)\n    }\n    out.print(\"{total}\")") "6\n";
```

Add `selfhost/tests/ir/mut-params.lmn` and `selfhost/tests/c/mut-params.lmn` with the `decls` and the bodies of `mut-run-basic`, `mut-run-snapshot`, `mut-run-panic` and `mut-native-hoist`, so check.sh compares the IR and the C of both compilers on them.

- [ ] **Step 2: Run them to see them fail** (`grep -E "mut-"`; expected: the `/native` halves FAIL with "internal: a mut argument outside a call").

- [ ] **Step 3: Implement in `src/`**
  - Lowering: `lower_fn` makes a `mut` parameter's variable with `by_ref = true`. A call (function or user method) with an `EMut` argument or receiver lowers its arguments in order, each `EMut p` through `lower_place` to `ByPlace`, the others to `ByValue`, and emits `CallMut`.
  - Own: a by-ref parameter is owned at entry and pinned live everywhere (pass the by-ref ids in `~pin` of the outer `live_block`), so a consuming use always retains it and it is never dropped; `Return` drops everything owned except by-ref parameters. `rewrite_rhs` rebuilds `CallMut`, consuming the `ByValue` atoms with every `ByPlace` root (and step atom) in `~borrowed`.
  - Pure: `key_of (CallMut _) = None` (never reused in a block or kept across a loop); every place that treats `Mutate (v, ...)` as changing `v` (`assigned`, `stmt_atoms`, the loop-invariance check) treats each `ByPlace (v, _)` the same; for deciding whether the enclosing function is pure, `CallMut (f, _)` counts as `Call (f, _)`.
  - Hoist: `escapes v` is true when `v` is a `ByPlace` root; `versionable` counts such a statement as an assignment of `v`.
  - Cgen: as in Interfaces. A by-ref parameter is never declared `volatile`: its pointer never changes, and writes through it reach memory before any call that can panic.
  - irdump: as in Interfaces.

- [ ] **Step 4: Mirror it in `selfhost/`** (IR, lowering, own, pure, hoist, cgen, ir_print), giving byte-identical C.

- [ ] **Step 5: Run everything** (as Task 1 Step 5). Expected also: the `mut-native-*` cases finish in well under a second each (a copying `push` would make them quadratic).

- [ ] **Step 6: Commit** `git commit -m "Native backend: pass mut arguments as pointers to the caller's slot"`

---

### Task 6: Language server, `limen doc`, grammars

**Files:**
- Modify: `src/lsp.ml:155-160` (`fn_signature`), `531-560` and `615-622` (semantic tokens); `src/doc.ml:92,103`
- Modify: `grammar/limen.gbnf:41-42` (`fn`, `dparam`), `95` (`arg`); `editors/vscode/test/grammar.test.js:100-115`
- Test: `test/test_limen.ml` (the LSP block near `3425-3545`), `editors/vscode/test/grammar.test.js`

**Interfaces:**
- Consumes: E0213 fixes (Task 2), `prm_mut`.

- [ ] **Step 1: Write the failing tests** in the LSP block: open a document with `fn bump(mut n: Int) {\n    n = n + 1\n}` and `let mut k = 0\n    bump(k)`; assert the hover on `bump(k)` is ```` "```limen\nfn bump(mut n: Int)\n```" ````; the code action at `k)` has `newText = "mut "` (as `lsp-quickfix` does); the semantic token for `n` in the parameter list carries `mutable`, not `readonly`. In `grammar.test.js`, a line `fn f(mut n: Int) { g(mut n) }` whose two `mut`s scope as `storage.modifier.limen`. Doc: `limen doc` on that file shows `fn bump(mut n: Int)`.

- [ ] **Step 2: Run them to see them fail** (`dune exec ./test/test_limen.exe 2>&1 | grep lsp-`, `cd editors/vscode && npm test`).

- [ ] **Step 3: Implement**: `fn_signature` and `Doc` print `mut ` before a marked parameter; semantic tokens mark a `mut` parameter `mutable`. GBNF: `dparam ::= "mut" sp1 param | param (sp "=" ws dvalue)?` and `arg ::= (ident sp ":" ws)? ("mut" sp1)? expr`. The TextMate grammar already scopes `mut` anywhere; only its test changes.

- [ ] **Step 4: Run** the tests, `sh grammar/check-suite.sh` (every program the suite parses is accepted, every one it rejects is rejected), then the four checks of Task 1 Step 5.

- [ ] **Step 5: Commit** `git commit -m "Language server, doc and grammars: mut parameters"`

---

### Task 7: SPEC.md, the book, ROADMAP.md, the design doc

**Files:**
- Modify: `SPEC.md`: section 4 (a bullet after the defaults bullet at `161`; `Methods` at `210` and the method-value line after it), section 5 `Calls` (`549-568`), section 6 (`597`), section 8 (`707-719`), section 9 rule 4 (`741`), section 11 `List[T]` `push` row (`1258`), section 14 (`1552-1561`), section 15 table (after E0212 at `1617`), section 16 grammar (`fn`, `param`, `arg`), section 20 (`2466-2475`)
- Modify: `ROADMAP.md:163-169`
- Modify (book, `~/Github/Personal/limen-book`, commit there, do not push): `tools/chapters/ch04-values.md` (a section after "Changing several variables at once", line 41-61), `tools/chapters/ch05-structs.md` (`Methods`, line 19), `tools/chapters/appendix-a-keywords-and-operators.md` (the `mut` row at 12, error codes at 83), `tools/chapters/appendix-d-not-yet.md` (`Language`, 31); Create: `examples/ch04-08-mut-params/{main.lmn,commands,expected.txt}`, `examples/ch04-09-mut-marker/{main.lmn,commands,expected.txt}`

- [ ] **Step 1: SPEC.md.** Say, in the spec's own register: a parameter may be `mut` (`self` too); it is a `mut` variable for the length of the call; the argument is written `mut place` (what an assignment can have on its left, root `mut`, E0205), a `mut self` receiver takes no marker; the marker must match the parameter (E0213, with a fix); `mut` places of one call may not overlap, distinct fields do not, elements and entries of one list or map do (E0214), and neither may a nested call's (E0214); a function with one is only called, never a value (E0215), like `push` and `remove`; no defaults, not on `main`; when the callee returns, panics or is cancelled, the place holds what it wrote, and a `defer` sees that; the argument's type is the parameter's exactly; storing, returning, capturing or sending the parameter takes a copy, as for any variable. Section 6's "Parameters ... are immutable" gains "unless declared `mut`". Section 9: a `mut` parameter does not make a function impure. Section 11: `push` is a built-in `mut self` method. Section 14: a call with a `mut` argument is never reused or kept. Section 15: rows E0213, E0214, E0215. Section 16: `param = [ "mut" ] IDENT ":" type` for `fn` items and `arg = [ IDENT ":" ] [ "mut" ] expr`. Section 20: `mut` parameters in interface methods (and so on `dyn`), foreign functions and function types are not there yet.

- [ ] **Step 2: ROADMAP.md.** Replace the "0.8.x or 0.9: `mut` parameters" section with what shipped (the syntax, the three new codes, both backends) and that the self-hosted compiler uses them (filled in after Task 12); keep the promise that Limen never gets stored references.

- [ ] **Step 3: The book.** In ch04, "Changing a value in place": the `(die, seed) = roll(seed)` example rewritten with `fn roll(mut seed: Int) -> Int`, then `mut self` (`buf.add(1)`), then the marker error (`ch04-09-mut-marker`, showing E0213 and its fix in `expected.txt`). ch05 `Methods`: one paragraph on `mut self`. Appendix A: `mut` row "declare a variable or parameter; make it changeable; mark a `mut` argument", and E0213 to E0215. Appendix D: the "not yet" line. Then `make` in the book (never piped), expecting every example to check and match its output.

- [ ] **Step 4: The design doc** (Claude Docs project `UqAJ6SrQyDNhe4KjicrS5X`, node `259b3653-1283`), through the docs connector if this session has it, otherwise list the change for Alfonso: section 1 marked shipped, with the error codes and the two decisions this plan made (exact argument type, no defaults).

- [ ] **Step 5: Run** the four checks of Task 1 Step 5 (check.sh also runs the book's examples).

- [ ] **Step 6: Commit** in both repositories: `git commit -m "SPEC and ROADMAP: mut parameters"`; in the book `git commit -m "Values: changing a value through a mut parameter"`.

---

### Proof batches (Tasks 8 to 12): the procedure

Each batch converts one part of the self-hosted compiler's threaded state to `mut` parameters and changes no behaviour, so check.sh (which compares the self-hosted compiler's output with the OCaml one's on every file) and the fixed point are its tests. For each:

- [ ] **Step 1: Baseline.** Run `grep -cE '^\s*(let )?\([a-z_, ]+\) = [a-z_.]+\(' FILES` and `sh selfhost/bootstrap.sh`, and note the count and the stage 2 and 3 times.
- [ ] **Step 2: Convert.** A function `fn f(st: S, ...) -> (T, S)` becomes `fn f(mut st: S, ...) -> T` (one returning only `S` returns `()`); callers `let (t, st) = f(st, ...)` become `let t = f(mut st, ...)`, and `st = st.m(...)` becomes `st.m(...)`. The caller's variable must be a `mut` variable or parameter. A closure cannot change a captured variable (E0205), so a `fold` or `map` that threads state becomes a `for` loop. Two `mut` uses in one call are E0214: `st.unify_at(sp, st.fresh(), t)` becomes `let t0 = st.fresh()` then `st.unify_at(sp, t0, t)`.
- [ ] **Step 3: Verify.** `dune build && dune exec ./test/test_limen.exe && sh selfhost/check.sh && sh selfhost/bootstrap.sh`. Expected: everything passes, "fixed point", and stage times no worse than the baseline by more than noise (a slowdown means a `mut` path that copies: check that the value is held once).
- [ ] **Step 4: Commit** with the before and after counts in the message, for example `git commit -m "selfhost: thread checker State through mut parameters (117 tuple assignments to 4)"`.

The parser (`selfhost/parser.lmn`) and lexer keep threading their `Parser`: a parse function returns `Err` without one, and callers that try one form and fall back rely on still holding the earlier `p`.

### Task 8: `State` methods

**Files:** `selfhost/env.lmn:260-475` (`fresh`, `fresh_list`, `unify`, `unify_at`, `set_type`, `set_inst`, `rewrite`, `node`, `report`, `did_you_mean`, `param_kind`, `add_type_bounds` and the other `self: State` methods returning `State`) become `mut self` methods; every caller in `selfhost/checker.lmn`, `selfhost/typecheck.lmn`, `selfhost/derive.lmn`, `selfhost/exhaust.lmn`. Follow the procedure.

### Task 9: Declaration passes in `typecheck.lmn`

**Files:** `selfhost/typecheck.lmn`: `register_types` 450, `resolve_bounds` 519, `take_pending` 539, `type_bounds` 548, `iface_methods` 571, `fields_and_variants` 619, `no_self_containing` 710, `signatures` 821, `check_self` 933, `unique_tests` 972, `test_params` 992, `check_main` 1019, `check_one` 1044, `check_fn` 1086, `check_pending` 1190, `refuse_compared_params` 243, `authority` 383, and `check_program` 86-146 (so `(cx, st, ds) = fields_and_variants(cx, st, ds, all)` becomes `fields_and_variants(mut cx, mut st, mut ds, all)`). Follow the procedure.

### Task 10: Expression checking in `checker.lmn`

**Files:** `selfhost/checker.lmn`: every function returning `(..., env.State)`, starting from `check_expr` 24 and `inner` 34 (`fn check_expr(mut st: env.State, cx: env.Cx, fx: env.Fx, e: ast.Expr) -> types.Ty`), and their callers in `typecheck.lmn` and `derive.lmn`. Follow the procedure.

### Task 11: Lowering in `lower.lmn`

**Files:** `selfhost/lower.lmn`: the `Lx` methods returning `(..., Lx)` (`lower` 517, `lower_call` 845, `access` 982, `lower_place` 1022, `lower_method` 1506, `lower_fn` 2043, `fresh_var` 141, `tmp`, `emit`, `in_block` and the rest) take `mut self: Lx`. Follow the procedure.

### Task 12: Renaming, numbering, ownership, C generation

**Files:** `selfhost/rename.lmn` (133 tuple assignments), `selfhost/ids.lmn` (70), `selfhost/own.lmn` (26), `selfhost/cgen.lmn` (the `Cg` methods: `stmt` 2120, `mutate` 2259, `fn_body` 2486, `rhs_into` 1038 and the chained `c.line(...)` helpers become `mut self` statements). Follow the procedure, one commit per file. Then fill in ROADMAP.md's line from Task 7 Step 2 with the total before and after, and commit `git commit -m "ROADMAP: the self-hosted compiler uses mut parameters"`.
