# Writ language specification

This document describes Writ as implemented by this repository's compiler: interpreter (`writ run`) and native backend (`writ build`, `writ run --native`). It is meant to be read whole, by people and by models writing Writ. Where the two backends differ, the difference is stated.

## 1. The idea

Writ code can only touch the outside world through **capabilities**: values that grant authority. There is no global `print`, `open`, `http.get` or clock. The program starts with one capability, `root: Root`, and passes narrower ones to the functions that need them. A function's parameter types therefore say everything it can do.

```
fn main(root: Root) -> Result[(), IoError] {
    let gh = root.net.only("api.github.com")   // narrower capability
    let names = fetch_repos(gh, "rust-lang")?
    root.stdout.print("{names.len()} repos")
    Ok(())
}

fn fetch_repos(net: Net, org: Str) -> Result[List[Str], IoError] {
    let body = net.get("https://api.github.com/orgs/{org}/repos")?
    Ok(parse_names(body))
}

fn parse_names(body: Str) -> List[Str] {   // no capabilities: cannot do I/O
    body.split("\"full_name\":").skip(1).map(|c| c.trim().split("\"").get(1).unwrap_or("?"))
}
```

## 2. Lexical structure

- **Comments:** `//` to the end of the line. There are no block comments.
- **Identifiers:** `[A-Za-z_][A-Za-z0-9_]*`. By convention types and enum variants start with an uppercase letter, everything else lowercase. Patterns rely on this (section 7).
- **Keywords:** `fn let mut if else match for in while return struct enum true false group break continue`.
- **Integers:** decimal digits, `_` allowed as a separator (`1_000_000`). A literal must fit in 63 bits.
- **Floats:** digits with a fraction (`1.5`), an exponent (`1e9`, `2.5E-3`), or both. `_` is allowed. There is no leading-dot form (`.5`).
- **Strings:** `"..."`, may span lines. Escapes: `\n \t \r \\ \" \{ \} \0`. Any other escape is error E0006.
- **Interpolation:** `{expr}` inside a string evaluates any expression, including ones with their own strings: `"{xs.join(", ")}"`. An empty `{}` is error E0007; write `\{` for a literal brace.
- **Symbols:** `( ) { } [ ] , : ; . | + - * / % ! ? = < > -> => == != <= >= && ||`.

### Newlines

Statements are separated by a newline or `;`. Two statements on one line without `;` is error E0004. An expression continues onto the next line only in these cases:

- A line starting with `.` continues a method chain:
  ```
  let n = xs
      .map(|x| x * 2)
      .sum()
  ```
- A binary operator at the **end** of a line continues the expression; one at the start of a line does not. So `a\n-1` is two statements.
- `(`, `[` and `?` at the start of a line do not apply to the previous line: `f\n(x)` is two statements.
- Inside `(...)`, `[...]`, struct literals, parameter lists and match arms, a newline can replace a `,`.

## 3. Types

| Type | Values | Notes |
| --- | --- | --- |
| `Int` | 64-bit signed integers | Arithmetic wraps on overflow. The interpreter uses 63 bits; the backends only differ on overflow. |
| `Float` | IEEE 754 double | No fused multiply-add, so both backends give identical results. |
| `Bool` | `true`, `false` | |
| `Str` | immutable byte strings | `len()` counts bytes. |
| `()` | the unit value `()` | Type of statements and of functions without `->`. |
| `List[T]` | growable sequence | A value: assigning or passing it never shares mutation (section 8). |
| `Array[T, N]` | exactly `N` elements | A value type stored inline, like a struct. `N` is an integer literal. |
| `Option[T]` | `Some(T)`, `None` | From the prelude. |
| `Result[T, E]` | `Ok(T)`, `Err(E)` | From the prelude. |
| `IoError` | `Denied(Str)`, `Failed(Str)` | Error type of every capability operation. |
| `fn(A, B) -> R` | functions and closures | |
| `Root`, `Net`, `Fs`, `Stdout`, `Clock` | capabilities | Cannot be created by programs (section 9). |
| user `struct` and `enum` | | Optionally generic (section 4). |

There is no null and no implicit conversion between types: `Int` and `Float` never mix without `to_float()` or `to_int()`.

Inference is local. Function parameters always carry types, and a missing parameter type is error E0008. Types inside a body are inferred, with annotations where needed: `let xs: List[Int] = []`. A value whose type cannot be determined where it is used is error E0207.

## 4. Declarations

A file is a sequence of `fn`, `struct` and `enum` declarations, in any order.

```
struct Pair[A, B] {
    first: A,
    second: B,
}

enum Shape {
    Circle(Float),
    Rect(Float, Float),
    Point,
}

fn swap[A, B](p: Pair[A, B]) -> Pair[B, A] {
    Pair { first: p.second, second: p.first }
}
```

- A function without `-> T` returns `()`.
- The body's last expression, without `;`, is the return value. `return e` returns early.
- Generic parameters go in `[...]` after the name. They are instantiated at each use and are never written at call sites: `swap(p)`.
- Struct fields and enum payloads cannot hold a capability (E0301) or a function type (E0304). That keeps authority visible in signatures. A generic instantiation like `Pair[Net, Int]` is allowed, because the capability is visible in the type.
- Defining the same type, function, field or variant twice is error E0106.
- User types have no methods. Write functions that take the value.

### main

`main` must be one of:

```
fn main(root: Root)
fn main(root: Root) -> Result[(), E]
fn main()
```

If `main` returns `Err(e)`, the program prints `error: <e>` to stderr and exits with status 1.

## 5. Expressions

### Precedence

From loosest to tightest. All binary operators are left-associative.

| Level | Operators |
| --- | --- |
| 1 | `\|\|` |
| 2 | `&&` |
| 3 | `==` `!=` |
| 4 | `<` `<=` `>` `>=` |
| 5 | `+` `-` |
| 6 | `*` `/` `%` |
| 7 | unary `-` `!` |
| 8 | call `f(x)`, index `a[i]`, field `p.x`, method `s.len()`, `?` |

### Operators

| Operator | Operands | Result |
| --- | --- | --- |
| `+` | `Int`, `Float`, `Str` (concatenation) | same type |
| `-` `*` `/` | `Int`, `Float` | same type |
| `%` | `Int` | `Int` |
| `<` `<=` `>` `>=` | `Int`, `Float`, `Str` (byte order) | `Bool` |
| `==` `!=` | any type except functions and capabilities; structural equality | `Bool` |
| `&&` `\|\|` | `Bool`, short-circuit | `Bool` |
| `-x` | `Int`, `Float` | same type |
| `!x` | `Bool` | `Bool` |

Both operands must have the same type. On `Int`, `/` truncates toward zero, `%` takes the sign of the dividend, and division or remainder by zero panics. On `Float`, comparisons follow IEEE, so NaN is unequal to everything, and division by zero gives infinity.

### Literals and constructors

- `[1, 2, 3]` is a `List[Int]`. Where an `Array[T, N]` is expected (an annotated `let`, an argument, a field), a list literal of exactly `N` elements builds the array. A wrong count is error E0201.
- Struct literal: `Pair { first: 1, second: "a" }`. Every field must be set (E0206). `Pair { first, second }` is shorthand for `first: first`. A struct literal cannot appear directly as the condition of `if`, `while`, `for` or `match`; wrap it in parentheses.
- Enum variants are built by name: `Some(3)`, `None`, `Circle(1.0)`, `Point`. If several enums define the same variant name, qualify it: `Shape.Point` (E0104).

### Blocks, if and match

- A block `{ ... }` is an expression; its value is the last expression without `;`.
- `if c { a } else { b }` is an expression, and both branches must have the same type. `if` without `else` has type `()`, so its block must also be `()` (E0203). `else if` chains are allowed, and `else` may start a new line.
- `match` (section 7) is an expression.

### Closures

`|x, y: Int| expr` or `|| expr`. Parameter types are optional when the context gives them (`xs.map(|x| x + 1)`). A closure captures the variables it uses **by value**, when it is created, and it cannot assign to them (E0205). Inside a closure, `return` and `?` leave the closure, not the enclosing function.

### The `?` operator

On a `Result[T, E]`, `e?` evaluates to the `Ok` value or returns `Err(e)` from the enclosing function, which must return `Result[_, E]` with the same `E` (E0202). Convert errors with `.map_err(|e| ...)`. On an `Option[T]`, `?` returns `None` from a function returning `Option`.

### Calls

`f(a, b)` calls a function or closure. The number of arguments must match (E0107). `panic(msg: Str)` aborts the program with a message and source location; it can be used where any type is expected.

Evaluation order is left to right: the callee, then the arguments in order. For assignment, the right-hand side is evaluated before the target's indexes.

## 6. Statements

```
let x = 1                 // immutable
let mut xs: List[Int] = []
x = 2                     // error E0205: not mut
xs = [1]
xs[0] = 5                 // through index and field paths, if the root variable is mut
p.first = 3
xs.push(4)                // also needs mut
return value
while cond { ... }
for x in list_or_array { ... }
break
continue
expr
```

- Parameters and pattern bindings are immutable. Use `let mut y = y` to get a mutable copy.
- `for` iterates over a snapshot: changing the list inside the loop does not affect the iteration.
- `break` and `continue` only work inside loops, not across closures (E0208).
- Only variables, fields and list or array elements can be assigned (E0209).

## 7. Patterns

Used in `match` arms:

| Pattern | Matches |
| --- | --- |
| `_` | anything |
| `name` (lowercase) | anything, binding it to `name` |
| `42`, `-1`, `"text"`, `true` | that literal |
| `Variant` (uppercase) | a payload-less variant |
| `Variant(p1, p2)` | a variant, matching its payload against sub-patterns |
| `Enum.Variant(...)` | a variant, qualified |

```
match shape {
    Circle(r) => 3.14159 * r * r,
    Rect(w, h) => w * h,
    Point => 0.0,
}
```

Arms are separated by `,` or a newline. A `match` must be exhaustive (E0204); the error comes with an auto-fix that adds the missing arms. Matching on `Int` or `Str` needs a `_` or binding arm.

## 8. Value semantics

Every value behaves as if it were copied on assignment, on passing and on capture. No two variables ever observe each other's mutations:

```
let mut a = [1]
let b = a
a.push(2)      // a is [1, 2], b is still [1]
```

The native backend implements this with reference counting and copy-on-write. A value is copied only if it is shared at the moment it is mutated. A value's last use moves it rather than retaining it, so in-place updates are the normal case.

## 9. Capabilities

`Root` has four fields, each a capability:

| Field | Type | Granted |
| --- | --- | --- |
| `root.stdout` | `Stdout` | always |
| `root.clock` | `Clock` | always |
| `root.net` | `Net` | only with `--grant net=HOST[:PORT]` (repeatable) or `--grant net=*` |
| `root.fs` | `Fs` | only with `--grant fs=DIR` or `--grant fs=DIR:ro` |

An ungranted `Net` or `Fs` still exists, but every operation on it returns `Err(Denied(...))`.

The rules:

1. Programs cannot create capabilities. They come only from `root`.
2. Attenuation only narrows. `net.only(host)` returns a `Net` limited to that host, and it is empty if you didn't hold that host. `fs.under(dir)` limits to a subdirectory; `fs.read_only()` removes writing.
3. There is no ambient I/O. Using `print`, `open`, `fetch` and similar names is error E0302, which explains the capability to use instead.
4. A function whose parameters contain no capability and no function type is **pure**: it can only compute (and panic). `writ caps FILE` lists each function's authority.
5. Enforcement at runtime:
   - `Net`: http and https only, host allowlist, redirects not followed.
   - `Fs`: paths are relative to the granted directory; absolute paths, `..` escapes and symlinks pointing outside are refused.

## 10. Concurrency

```
let results = group g {
    for row in rows {
        g.go(|| count_row(row))
    }
}
```

- `group g { body }` runs `body`, in which `g.go(f)` starts `f: fn() -> T` as a task. The group evaluates to a `List[T]` of the task results **in the order the tasks were started**.
- When the block ends, every task has finished. Nothing outlives the group.
- `g` can only be used as `g.go(...)` directly in the body. It cannot be stored, passed, returned, or used inside a closure (E0310).
- The body cannot leave early with `return` or `?` (E0311), and `break` and `continue` cannot target loops outside the group.
- Tasks share data by value, so there are no data races. A task reaches the world only through capabilities its closure captured. Spawning is not a capability.
- Native code runs tasks on one worker per core (`WRIT_THREADS` overrides this). The interpreter runs them one after another in start order. Output printed by tasks may interleave differently between the two.

## 11. Built-in methods

Methods take the receiver by value and never modify it, except `push`, which needs a `mut` place.

### Str

| Method | Result |
| --- | --- |
| `len()` | `Int`, bytes |
| `is_empty()` | `Bool` |
| `lines()` | `List[Str]`, split on `\n`, stripping a trailing `\r` and dropping one final empty line |
| `split(sep)` | `List[Str]`; `split("")` splits into single bytes |
| `trim()` | `Str` without leading and trailing spaces, tabs, newlines, `\r` and form feeds |
| `contains(s)`, `starts_with(s)`, `ends_with(s)` | `Bool` |
| `to_upper()`, `to_lower()` | `Str`, ASCII only |
| `replace(a, b)` | `Str`, every occurrence |
| `parse_int()` | `Option[Int]`, decimal with an optional sign, surrounding whitespace allowed |

### Int, Float, Bool

| Type | Methods |
| --- | --- |
| `Int` | `to_str()`, `to_float()`, `abs()` |
| `Float` | `to_str()`, `to_int()` (truncates; NaN gives 0), `abs()`, `sqrt()` |
| `Bool` | `to_str()` |

### List[T]

| Method | Result |
| --- | --- |
| `len()`, `is_empty()` | `Int`, `Bool` |
| `get(i)`, `first()`, `last()` | `Option[T]` |
| `push(x)` | `()`, appends in place (needs `mut`) |
| `contains(x)` | `Bool` |
| `reverse()`, `take(n)`, `skip(n)` | `List[T]`; `take` and `skip` clamp out-of-range counts |
| `sort()` | `List[T]`, stable; only for `Int`, `Float` and `Str` elements |
| `map(f)`, `filter(p)` | `List[U]`, `List[T]` |
| `fold(init, f)` | `U`, with `f: fn(U, T) -> U` |
| `join(sep)` | `Str`; `List[Str]` only |
| `sum()` | `Int`; `List[Int]` only |

`xs[i]` panics when `i` is out of range; `xs.get(i)` returns `None` instead.

### Array[T, N]

`len()` (always `N`), `get(i)`, `contains(x)`, `to_list()`, indexing with `a[i]`, and `for x in a`.

### Option[T] and Result[T, E]

| Method | Result |
| --- | --- |
| `is_some()`, `is_none()` / `is_ok()`, `is_err()` | `Bool` |
| `unwrap_or(default)` | `T` |
| `map(f)` (Option) | `Option[U]` |
| `map_err(f)` (Result) | `Result[T, F]` |

### Capabilities

| Capability | Method | Result |
| --- | --- | --- |
| `Stdout` | `print(s)` | `()`, writes `s` and a newline |
| `Stdout` | `write(s)` | `()`, writes `s` |
| `Clock` | `now_ms()` | `Int`, milliseconds since the Unix epoch |
| `Net` | `get(url)` | `Result[Str, IoError]`, the response body; a non-2xx status is `Failed` |
| `Net` | `only(host)` | `Net`; `host` is `name`, `name:port` or `*` |
| `Fs` | `read(path)` | `Result[Str, IoError]` |
| `Fs` | `write(path, text)` | `Result[(), IoError]` |
| `Fs` | `exists(path)` | `Bool` |
| `Fs` | `under(dir)`, `read_only()` | `Fs` |

## 12. How values print

String interpolation and `to_str()` use these rules. At the top level a `Str` prints raw; inside a container it prints quoted, with `"`, `\`, newline, tab, `\r` and `\b` escaped and other non-printable bytes as `\ddd`.

| Value | Prints as |
| --- | --- |
| `Int` | decimal |
| `Float` | `2.0` if integral and below 10^16 in magnitude, else 15 significant digits (`0.333333333333333`) |
| `Bool` | `true` / `false` |
| `()` | `()` |
| `List`, `Array` | `[1, 2, 3]` |
| struct | `Pair { first: 1, second: "a" }` |
| enum | `None`, `Some(3)`, `Rect(2.0, 3.0)` |
| function | `<fn>` |
| capability | `<Net>` |

## 13. Optimizations that affect observable behavior

The native backend may run a chain like `xs.filter(p).map(f).fold(z, g)` as one fused loop, interleaving the stages per element. It does this only when the earlier stages are pure closure literals, which capture no capability and no function. The only observable difference is **which panic happens first** when more than one stage would panic. Everything else (results, output order, values) is the same.

## 14. Errors

Every diagnostic has a stable code. `writ explain CODE` prints its explanation, and `writ check --json` emits code, span, notes and an auto-fix when there is one.

| Code | Meaning |
| --- | --- |
| E0001-E0008 | lexing and parsing: bad character, unclosed string, unexpected token, missing newline, integer too large, bad escape, empty interpolation, untyped parameter |
| E0101 | unknown name (with a suggestion) |
| E0102 | unknown type |
| E0103 | unknown field |
| E0104 | unknown or ambiguous variant |
| E0105 | unknown method |
| E0106 | defined twice |
| E0107 | wrong number of arguments |
| E0108 | not callable |
| E0109 | wrong number of type arguments |
| E0201 | type mismatch |
| E0202 | misuse of `?` |
| E0203 | `if` without `else` produces a value |
| E0204 | non-exhaustive `match` |
| E0205 | assignment to something immutable |
| E0206 | missing struct field |
| E0207 | type unknown here |
| E0208 | `break`/`continue` outside a loop |
| E0209 | invalid assignment target |
| E0301 | capability stored in a field |
| E0302 | ambient I/O name such as `print` |
| E0303 | bad `main` signature |
| E0304 | function stored in a field |
| E0310 | misuse of a group handle |
| E0311 | `return` or `?` leaving a group |

A **panic** (division by zero, index out of range, `panic(...)`, no matching arm) prints `panic: message` and the source location to stderr, then exits with status 1.

## 15. Grammar

EBNF. `NL` is a newline, as described in section 2. A character-level version for constrained decoding, with the newline rules spelled out, is in [grammar/writ.gbnf](grammar/writ.gbnf). `{ x }` means zero or more, and `[ x ]` means optional.

```
program    = { item } ;
item       = fn | struct | enum ;
fn         = "fn" IDENT [ generics ] "(" [ params ] ")" [ "->" type ] block ;
generics   = "[" IDENT { "," IDENT } "]" ;
params     = param { sep param } [ "," ] ;
param      = IDENT ":" type ;
struct     = "struct" IDENT [ generics ] "{" [ field { sep field } [ "," ] ] "}" ;
field      = IDENT ":" type ;
enum       = "enum" IDENT [ generics ] "{" [ variant { sep variant } [ "," ] ] "}" ;
variant    = IDENT [ "(" type { "," type } ")" ] ;
type       = IDENT [ "[" typearg { "," typearg } "]" ]
           | "(" ")"
           | "fn" "(" [ type { "," type } ] ")" "->" type ;
typearg    = type | INT ;                       (* INT only as Array's size *)
sep        = "," | NL ;

block      = "{" { stmt ( NL | ";" ) } [ expr ] "}" ;
stmt       = "let" [ "mut" ] IDENT [ ":" type ] "=" expr
           | "return" [ expr ]
           | "while" expr_ns block
           | "for" IDENT "in" expr_ns block
           | "break" | "continue"
           | expr [ "=" expr ] ;

expr       = unary { BINOP unary } ;            (* precedence: section 5 *)
expr_ns    = expr ;                             (* no struct literal at the top level *)
unary      = ( "-" | "!" ) unary | postfix ;
postfix    = primary { "(" [ args ] ")" | "[" expr "]" | "." IDENT [ "(" [ args ] ")" ] | "?" } ;
args       = expr { sep expr } [ "," ] ;
primary    = INT | FLOAT | STRING | "true" | "false"
           | IDENT [ "{" [ init { sep init } [ "," ] ] "}" ]      (* struct literal *)
           | "(" ")" | "(" expr ")"
           | "[" [ args ] "]"
           | block
           | "if" expr_ns block [ "else" ( block | if ) ]
           | "match" expr_ns "{" { pattern "=>" expr ( "," | NL ) } "}"
           | "|" [ cparam { "," cparam } ] "|" expr | "||" expr
           | "group" IDENT block ;
init       = IDENT [ ":" expr ] ;
cparam     = IDENT [ ":" type ] ;
pattern    = "_" | IDENT | [ "-" ] INT | STRING | "true" | "false"
           | IDENT [ "." IDENT ] [ "(" pattern { "," pattern } ")" ] ;
```

## 16. Not yet

Channels, methods on user types, modules and imports, and a capability for listening on the network. `group` exists; channels are planned.
