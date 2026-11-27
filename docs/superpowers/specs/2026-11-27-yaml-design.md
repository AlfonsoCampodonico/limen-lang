# A `yaml` module: design

## Intent

Kubernetes manifests, kubeconfigs, CI files, Helm values and compose files
are YAML. Limen reads and writes JSON but not YAML, so an operator can't
read a kubeconfig and a tool can't read a manifest. The user asked for all
of YAML 1.2, not a Kubernetes subset, so the module reads whatever YAML a
program is handed. It is also the base for kubeconfig support in `kube`.

## Goals

- `yaml.parse` reads any YAML 1.2 stream that the yaml-test-suite says is
  valid, and refuses every stream it says is an error.
- `yaml.stringify`, and `yaml.encode` and `yaml.decode` written by the
  compiler for a type, as `json`'s are.
- Safe on untrusted input: bounded nesting, bounded alias expansion,
  bounded size of what a stream expands to, never a crash or a stack
  overflow, in either backend and in the playground.

## Non-goals

- YAML 1.1 behaviour (`yes`, `no`, `on`, `off` as booleans; sexagesimal
  numbers; `<<` merge keys as a rule of the language). The core schema of
  1.2 only.
- Keeping comments, key order and styles on a round trip (an editing
  library). `parse` then `stringify` gives equivalent YAML, not the same
  text.
- Custom tags turned into Limen types. A tag is kept on the node and the
  program decides.

## Anti-goals

- Hand-written C parsing outside input (approach C below).
- Exponential expansion: a stream whose aliases expand past the budget is
  an error, not a hang.

## Approaches

The user picked A.

- **A. The parser in Limen, `src/std/yaml.lmn`, beside `json` (chosen).**
  Memory-safe on untrusted input, the same in both backends and the
  playground, conformance measured by the yaml-test-suite, fuzzed with
  the existing fuzzers. The most code, and slower than C, which is fine
  for configuration.
- B. libyaml's C source vendored into the runtime. Fast and proven, but
  YAML 1.1, so a 1.2 layer goes on top, and it adds about 10,000 lines of
  outside C to a runtime that has kept to libc and OpenSSL.
- C. Our own parser in C, shared like `limen_core.h`. Fast and written
  once, but hand-written C on untrusted input is where memory bugs live.

## Design

### Values

```limen
pub enum Node {
    Null,
    Bool(Bool),
    Int(Int),
    Float(Float),
    Str(Str),
    Seq(List[Node]),
    Map(List[Pair]),          // in document order; keys may be any node
    Tagged(Str, Node),        // a tag other than the core schema's
}
pub struct Pair { key: Node, value: Node }
```

- Scalars resolve by the 1.2 core schema: `null`, `~` and empty are
  `Null`; `true`/`false` (and their capitalised forms) are `Bool`;
  decimal, `0o` and `0x` integers are `Int` (out of `Int`'s range is an
  error); decimal floats, `.inf`, `-.inf` and `.nan` are `Float`; the rest
  is `Str`. Quoted scalars are always `Str`. The standard tags `!!null`,
  `!!bool`, `!!int`, `!!float`, `!!str`, `!!seq`, `!!map` apply; any other
  tag gives `Tagged`.
- Anchors and aliases are resolved while parsing: an alias is a copy of
  the node it names (Limen values are values, so there are no shared
  nodes to keep). A map key that repeats is an error, as the spec says.
- Accessors in `json`'s style: `get(key)` on a map with a string key,
  `at(i)`, `as_str`, `as_int`, `as_float`, `as_bool`, `as_seq`, `pairs`.
- `yaml.to_json(node) -> Result[json.Value, Str]` and
  `yaml.from_json(json.Value) -> Node`, for code that already works on
  JSON (a map whose keys aren't all strings, and `Tagged`, don't convert).

### Functions

| | |
|---|---|
| `parse(s) -> Result[Node, Str]` | exactly one document; more than one is an error saying to use `parse_all` |
| `parse_all(s) -> Result[List[Node], Str]` | every document of a stream (`---` and `...`) |
| `stringify(node) -> Str` | block style, two-space indent, strings quoted when plain style would read back as something else |
| `encode(x) -> Str`, `decode(s) -> Result[T, Str]` | written by the compiler per type, with the same rules, errors and E0212 as `json.encode`/`decode` (SPEC.md section on json) |

Errors name the line and column: "line 4, column 3: mapping values are
not allowed here".

### Limits

- Nesting: at most 512 levels (as `json.parse`), without recursion in the
  parser, so a deep document can't overflow a task's stack.
- Aliases: the nodes produced by expanding aliases count towards a budget
  of 1,000,000 nodes per stream (and 100 levels of an alias inside an
  alias); past it, `Err("... aliases expand past the limit")`.
- Each limit is a named constant in the module, documented in SPEC.md.

### Conformance and tests

- The yaml-test-suite's data release (MIT) is vendored as test data under
  `test/yaml-suite/`, downloaded once with the user's approval. A runner in
  the test suite parses each case's `in.yaml` and compares with its
  `in.json` (or its error flag), in both backends. The target is every
  case except the few that need features JSON can't express (non-string
  keys are compared through the event list instead); the count passing is
  printed, and a case that starts failing fails the gate.
- Cases written for Limen: the core schema's resolution table, the limits
  (a billion-laughs stream, 513 levels), error positions, `encode` and
  `decode` round trips on structs and enums, and Kubernetes files
  (`examples/k8s/*.yaml`, a kubeconfig).
- `fuzz/yaml.lmn` in the style of `fuzz/json.lmn`.
- The cross-check and `selfhost/check.sh` cover it like any standard
  library module, and `eval` runs it, so the playground reads YAML.

## Constraints

- The module is ordinary Limen, so both compilers and both backends get
  it unchanged; `encode` and `decode` need the compiler to write the
  conversions, in both compilers, as it does for `json`.
- SPEC.md describes the module in the same commit; the book gains a few
  paragraphs beside `json` in chapter 14.
- The plan's commit rules: plain messages, no attribution, British
  spelling, no em dashes.

## Left to the builder

- The parser's structure (a scanner and an event-driven composer is the
  usual shape).
- Exact error texts, in the style of `json`'s.
- How `encode`/`decode` share the compiler's JSON code path.
