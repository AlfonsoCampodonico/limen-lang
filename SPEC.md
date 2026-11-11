# Vela language specification

This document describes Vela as implemented by this repository's compiler: interpreter (`vela run`) and native backend (`vela build`, `vela run --native`). It is meant to be read whole, by people and by models writing Vela. Where the two backends differ, the difference is stated.

## 1. The idea

Vela code can only touch the outside world through **capabilities**: values that grant authority. There is no global `print`, `open`, `http.get` or clock. The program starts with one capability, `root: Root`, and passes narrower ones to the functions that need them. A function's parameter types therefore say everything it can do.

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

fn parse_names(body: Str) -> List[Str] {   // no capabilities: cannot do I/O
    body.split("\"full_name\":").skip(1).map(|c| c.trim().split("\"").get(1) ?? "?")
}
```

## 2. Lexical structure

- **Comments:** `//` to the end of the line. There are no block comments.
- **Doc comments:** the `//` lines directly above an item, a struct field, an enum variant or an interface method, with no blank line in between, document it. The comment block at the very top of a file, followed by a blank line, documents the module. `vela doc` reads them; there is no special marker.
- **Identifiers:** `[A-Za-z_][A-Za-z0-9_]*`. By convention types and enum variants start with an uppercase letter, everything else lowercase. Patterns rely on this (section 7).
- **Keywords:** `fn let mut if else match for in while return struct enum true false group break continue pub use interface dyn select defer`.
- **Integers:** decimal digits, `_` allowed as a separator (`1_000_000`). A literal must fit in 64 bits: up to `9223372036854775807` (E0005).
- **Floats:** digits with a fraction (`1.5`), an exponent (`1e9`, `2.5E-3`), or both. `_` is allowed. There is no leading-dot form (`.5`).
- **Strings:** `"..."`, may span lines. Escapes: `\n \t \r \\ \" \{ \} \0`. Any other escape is error E0006.
- **Interpolation:** `{expr}` inside a string evaluates any expression, including ones with their own strings: `"{xs.join(", ")}"`. An empty `{}` is error E0007; write `\{` for a literal brace.
- **Symbols:** `( ) { } [ ] , : ; . | + - * / % ! ? = < > -> => == != <= >= && || ..<`.

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
| `Int` | 64-bit signed integers | Arithmetic wraps on overflow, the same in both backends. `x / -1` and `x % -1` wrap too instead of trapping, `abs()` of the smallest Int is itself, and `Float.to_int()` saturates (NaN gives 0). |
| `Float` | IEEE 754 double | No fused multiply-add, so both backends give identical results. |
| `Bool` | `true`, `false` | |
| `Str` | immutable byte strings | `len()` counts bytes. |
| `Bytes` | immutable binary data | Built with `bytes([104, 105])` or `s.to_bytes()`; prints as `bytes([104, 105])`. |
| `()` | the unit value `()` | Type of statements and of functions without `->`. |
| `List[T]` | growable sequence | A value: assigning or passing it never shares mutation (section 8). |
| `Array[T, N]` | exactly `N` elements | A value type stored inline, like a struct. `N` is an integer literal. |
| `(A, B)`, `(A, B, C)`, ... | tuples of 2 to 8 values | Written `(1, "a")`, read with `.0`, `.1`, unpacked with patterns. |
| `Map[K, V]` | keys to values | Keys are `Int`, `Str`, `Bytes`, `Bool`, or tuples, structs and enums made of them (E0113). A value, like `List`; iterates in insertion order. |
| `Option[T]` | `Some(T)`, `None` | From the prelude. |
| `Result[T, E]` | `Ok(T)`, `Err(E)` | From the prelude. |
| `IoError` | `Denied(Str)`, `Failed(Str)` | Error type of every capability operation. |
| `HttpResponse` | `{ status: Int, headers: Map[Str, Str], body: Str }` | From the prelude; returned by `Net.get`. |
| `ProcessOutput` | `{ code: Int, stdout: Str, stderr: Str }` | From the prelude; returned by `Exec.run`. |
| `fn(A, B) -> R` | functions and closures | |
| `Chan[T]` | a channel between tasks | Created with `channel(capacity)` (section 10). |
| `Handle` | `{ addr: Int }` | From the prelude: an opaque C pointer, for foreign functions (section 9). |
| `Root`, `Net`, `Fs`, `Stdout`, `Clock`, `Listen`, `Listener`, `Conn`, `Rand`, `Stdin`, `Env`, `Exec`, `Foreign` | capabilities | Cannot be created by programs (section 9). |
| user `struct` and `enum` | | Optionally generic (section 4). |

There is no separate character type: a character is a `Str` holding one UTF-8 code point. `s.chars()` gives them as a `List[Str]`, `for c in s.chars() { ... }` visits them, and `s.bytes()` gives the bytes (section 11). One type for text keeps the API small; `len()` still counts bytes.

`lo..<hi` is the `List[Int]` from `lo` up to but not including `hi`, empty when `hi <= lo`. It counts in loops, `for i in 0..<n { ... }`, without building the list, and is a list everywhere else: `(1..<4).map(|x| x * x)`. There is one form, exclusive, spelled `..<` as in Swift and Kotlin so it cannot be misread; for an inclusive end write `0..<n + 1`.

### Tuples

```
fn divmod(a: Int, b: Int) -> (Int, Int) {
    (a / b, a % b)
}

let (q, r) = divmod(17, 5)        // unpack into names; `_` skips one
let t = (1, "one", 1.5)
t.0 + 1                           // fields by position
for (n, s) in [(1, "a"), (2, "b")] { ... }
```

Tuples compare and print like structs: `(1, "a") == (1, "a")`, and `(1, "a")` prints as `(1, "a")`. `let` and `for` can only unpack into names and `_`; `match` takes any patterns, such as `(true, 0)`.

### Maps

```
let mut counts = map()                    // or map([("a", 1), ("b", 2)])
for w in words {
    counts[w] = counts.get(w) ?? 0 + 1
}
counts["the"]                             // panics if the key is missing
for (w, n) in counts.entries() { ... }    // in insertion order
```

`m[k] = v` inserts or replaces and needs a `mut` place, like `push`. An entry can also be changed in place through its key: `m[k].hits = m[k].hits + 1`, `m[k].names.push(n)`, `m[a][b] = v` for a map of maps. Those need the key to be there and panic like `m[k]` when it is not; only `m[k] = v` adds a key.

A key is an `Int`, `Str`, `Bytes` or `Bool`, or a tuple, struct or enum made of them, compared by value: `grid[(x, y)]`, `seen[Pos { x, y }]`. `Float` (NaN is not equal to itself), lists and maps cannot be keys (E0113). Iteration (`keys`, `values`, `entries`) and printing follow insertion order, the same in both backends; replacing a value keeps its key's position, and removing then re-inserting moves it to the end. Two maps are equal when they hold the same entries, in any order.

There is no null and no implicit conversion between types: `Int` and `Float` never mix without `to_float()` or `to_int()`.

Inference is local. Function parameters always carry types, and a missing parameter type is error E0008. Types inside a body are inferred, with annotations where needed: `let xs: List[Int] = []`. A value whose type cannot be determined where it is used is error E0207.

## 4. Declarations

A file is a sequence of `use` lines and `fn`, `struct` and `enum` declarations, in any order (put `use` first by convention). Section 18 covers modules.

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
- Struct fields and enum payloads cannot hold a capability (E0301), a channel (E0305) or a function type (E0304). That keeps authority visible in signatures. A generic instantiation like `Pair[Net, Int]` is allowed, because the capability is visible in the type.
- Defining the same type, function, field or variant twice is error E0106.
- Types can be recursive through an enum, a `List` or a `Map`, which hold their contents indirectly: `struct Node { value: Int, next: Option[Node] }`, `enum Tree { Leaf, Node(Tree, Tree) }`, `struct Dir { files: List[Dir] }`. A struct stores its fields inline, so one that contains itself through structs, tuples or arrays alone would be infinitely large (E0114). There is no `box`: those types are the indirection.

### Methods

A function whose first parameter is named `self` is a method of that parameter's type, and is called with a dot:

```
struct Point {
    x: Float,
    y: Float,
}

fn add(self: Point, o: Point) -> Point {
    Point { x: self.x + o.x, y: self.y + o.y }
}

fn swap[A, B](self: Pair[A, B]) -> Pair[B, A] {
    Pair { first: self.second, second: self.first }
}

let p = a.add(b).add(c)
```

- `self` must be the first parameter, and its type a struct or enum declared in the program, generic or not, or one of the built-in types below (E0110).
- A file can add methods to `Int`, `Float`, `Bool`, `Str`, `Bytes`, `List[T]`, `Map[K, V]`, `Option[T]`, `Result[T, E]` and `IoError`: `fn shout(self: Str) -> Str { self.to_upper() + "!" }` makes `"hi".shout()` work **in that file only**. Built-in types belong to no module, so this keeps two modules' helpers from clashing; such a method cannot be `pub` (E0110), and it cannot reuse the name of a built-in method (E0106). Export a plain function to share it. Methods added this way do not count toward interfaces, and capabilities, channels and arrays cannot get methods.
- A method is only called with a dot: `p.add(q)`, never `add(p, q)`. Its name does not clash with a plain function or with methods of other types, so `fn area(self: Circle)` and `fn area(self: Rect)` can both exist.
- Methods take `self` by value like any parameter; to "change" a value, return the new one.
- A method without the call is a value: `p.add` is a function that calls `add` on `p`, the same as `|q| p.add(q)`. Like a closure, it holds a copy of `p` taken when `p.add` is evaluated, so `[a, b].map(origin.add)` works and later changes to `p` do not reach it. This works for every method that can be called with a dot: user methods, `dyn` and interface-bounded methods, and built-in ones (`let say = out.print`), except `push` and `remove`, which change their receiver. (It is `p.add`, as in Go and Swift, and not `Point.add`: the value already knows its type, and `Name.x` is taken by enum variants and modules.)
- In a module, only `pub fn` methods can be called from other modules (E0402).

### Interfaces

An interface lists methods. Any type that has them satisfies it; there is no declaration that it does. Interfaces are used as bounds on a function's type parameters:

```
interface Shape {
    fn area(self) -> Float
    fn name(self) -> Str
}

fn total[T: Shape](xs: List[T]) -> Float {
    xs.map(|s| s.area()).fold(0.0, |a, b| a + b)
}

total([Circle { r: 1.0 }, Circle { r: 2.0 }])   // Circle has `area` and `name`
```

- Inside the function, a value of type `T` has exactly the methods its bounds declare. Several bounds are joined with `+`: `[T: Shape + Named]`.
- At each call the compiler checks that the type used for `T` has every method with the same parameter and result types (E0111). Built-in types count with their built-in methods, so `Clock` satisfies `interface Ticker { fn now_ms(self) -> Int }`.
- Code is generated separately for each type used, so a call through an interface costs the same as a direct call.
- An interface is not a type: `fn f(s: Shape)` is E0102; write `fn f[T: Shape](s: T)`. A list holds one type, so `List[T]` cannot mix `Circle` and `Square` values.
- Bounds go only on functions, not on structs or enums.
- Because built-in capabilities satisfy interfaces too, tests can pass a fake:

```
interface Ticker {
    fn now_ms(self) -> Int
}

struct FakeClock {
    t: Int,
}

fn now_ms(self: FakeClock) -> Int {
    self.t
}

fn elapsed[C: Ticker](c: C, since: Int) -> Int {
    c.now_ms() - since
}

test "elapsed" {
    assert_eq(elapsed(FakeClock { t: 1500 }, 1000), 500)
}
// in main: elapsed(root.clock, start)
```

`vela caps` reports a function with bounded type parameters like one that takes callbacks: what it can do depends on the type it is called with.

### Interface values: `dyn`

`dyn Shape` is a type: a value of any type that satisfies `Shape`, called through the interface's methods. A list of them can mix types:

```
let shapes: List[dyn Shape] = [Circle { r: 1.0 }, Square { s: 2.0 }]
for s in shapes {
    out.print("{s.name()}: {s.area()}")
}
```

- A struct or enum value becomes `dyn Shape` where one is expected: an annotated `let`, a function argument, a list literal of that type, a struct field, a `return`, the last expression of a function returning `dyn Shape`, or `push`. The expected type reaches into each branch of an `if`, a `match` and a block, so the branches can hold different types. Its type must satisfy the interface (E0111).
- Only struct and enum values can become `dyn`, and only if they hold no capability, function or channel (E0112), so a `dyn` value never hides authority. To work with capabilities through an interface, use a bounded type parameter instead.
- Only the interface's methods can be called on a `dyn` value. It prints like the value inside. `==` is not allowed.
- To choose between types, say where the `dyn` is expected: `let s: dyn Shape = if big { Circle { r: 1.0 } } else { Square { s: 2.0 } }`. Without an expected type, the branches of an `if` must have the same type.
- A `dyn` value is boxed and its calls go through a table of methods; a bounded type parameter (`[T: Shape]`) has no such cost.

### main

`main` must be one of:

```
fn main(root: Root)
fn main(root: Root) -> Int
fn main(root: Root) -> Result[(), E]
fn main(root: Root) -> Result[Int, E]
fn main()
```

`root` may be left out of any of them. When `main` returns, the program exits with status 0, or with the `Int` it returned (directly or as `Ok(n)`). The operating system keeps only the low 8 bits, so use 0 to 255: `-> Int { 256 + 7 }` exits with 7. If `main` returns `Err(e)`, the program prints `error: <e>` to stderr and exits with status 1.

```
fn main(root: Root) -> Int {
    let bad = check(root.stdin.read_all())
    if bad > 0 {
        root.stdout.print("{bad} problems")
        return 2
    }
    0
}
```

There is no `exit` function: the exit code is what `main` returns, so groups finish, everything is freed, and no function deep in the program can end it.

## 5. Expressions

### Precedence

From loosest to tightest. All binary operators are left-associative.

| Level | Operators |
| --- | --- |
| 1 | `\|\|` |
| 2 | `&&` |
| 3 | `==` `!=` |
| 4 | `<` `<=` `>` `>=` |
| 5 | `..<` |
| 6 | `+` `-` |
| 7 | `*` `/` `%` |
| 8 | `??` |
| 9 | unary `-` `!` |
| 10 | call `f(x)`, index `a[i]`, field `p.x`, method `s.len()`, `?` |

### Operators

| Operator | Operands | Result |
| --- | --- | --- |
| `+` | `Int`, `Float`, `Str`, `Bytes` (concatenation) | same type |
| `-` `*` `/` | `Int`, `Float` | same type |
| `%` | `Int` | `Int` |
| `<` `<=` `>` `>=` | any ordered type (below) | `Bool` |
| `==` `!=` | any type except functions and capabilities; structural equality | `Bool` |
| `&&` `\|\|` | `Bool`, short-circuit | `Bool` |
| `..<` | `Int` | `List[Int]`: `lo..<hi` counts up from `lo`, stopping before `hi` |
| `-x` | `Int`, `Float` | same type |
| `!x` | `Bool` | `Bool` |

Both operands must have the same type. The **ordered** types are `Int`, `Float`, `Str` and `Bytes` (byte order), `Bool` (`false` first), `()`, and lists, arrays, tuples, structs and enums whose items are ordered. Those compare item by item: lists and arrays lexicographically (a prefix comes first), tuples and structs field by field in declaration order, enums by variant in declaration order and then by payload, so `None` sorts after every `Some`. Maps, functions, capabilities, channels, `dyn` values and type parameters have no order (E0201). On `Int`, `/` truncates toward zero, `%` takes the sign of the dividend, and division or remainder by zero panics. On `Float`, comparisons follow IEEE, so NaN is unequal to everything, and division by zero gives infinity. Inside a list, tuple, struct or enum, and in `sort()`, floats use a total order with NaN first, so sorting never misbehaves.

### Literals and constructors

- `[1, 2, 3]` is a `List[Int]`. Where an `Array[T, N]` is expected (an annotated `let`, an argument, a field), a list literal of exactly `N` elements builds the array. A wrong count is error E0201.
- Struct literal: `Pair { first: 1, second: "a" }`. Every field must be set (E0206). `Pair { first, second }` is shorthand for `first: first`. A struct literal cannot appear directly as the condition of `if`, `while`, `for` or `match`; wrap it in parentheses.
- Enum variants are built by name: `Some(3)`, `None`, `Circle(1.0)`, `Point`. A bare name means a variant of the file's own enums first, then of the prelude (`Some`, `None`, `Ok`, `Err`, `Denied`, `Failed`), then of other modules' enums, so a module's `Num` never clashes with `json.Num`, and a file that defines its own `Some` shadows the prelude's. If two enums at the nearest of those levels define the name, qualify it: `Shape.Point` or `json.Num(1.0)` (E0104). In patterns the type being matched decides, so qualifying is rarely needed there.

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
for i in 0..<n { ... }    // 0, 1, ..., n - 1
let (a, b) = pair         // unpack a tuple
for (k, v) in m.entries() { ... }
m[key] = value            // insert into a map (needs mut)
break
continue
defer f.close()           // runs when the enclosing block is left
defer { ... }
expr
```

- Parameters and pattern bindings are immutable. Use `let mut y = y` to get a mutable copy.
- `for` iterates over a snapshot: changing the list inside the loop does not affect the iteration. Over a range, both bounds are evaluated once, before the first iteration.
- `break` and `continue` only work inside loops, not across closures (E0208).
- Only variables, fields, list or array elements and map entries, and paths through them such as `m[k].x` or `xs[i].tags[j]`, can be assigned (E0209).

### defer

`defer` takes a block, or one statement other than `let` (`defer f.close()`, `defer count = count + 1`), and runs it when the enclosing block is left, however it is left:

```
fn copy(fs: Fs, from: Str, to: Str) -> Result[(), IoError] {
    let log = Logger { lines: [] }
    defer log.flush()                       // runs last
    let data = fs.read(from)?               // `?` leaving early runs it too
    defer {
        log.add("copied {data.len()} bytes")  // runs first
    }
    fs.write(to, data)
}
```

- It is scoped to its block, like Swift's and unlike Go's (which waits for the function): a `defer` inside a loop body runs at the end of every iteration, and one inside an `if` branch when that branch ends.
- It runs on every way out: the end of the block, `return`, `?`, a `break` or `continue` that leaves the block, a panic, and a task's cancellation. After a panic or a cancellation the deferred body runs and the panic or cancellation then carries on, so `catch` still sees the panic and cancelled tasks still end.
- Several defers in one block run in reverse order: the last one written runs first.
- The body sees the names in scope at the `defer`, with their values at the time it runs: a variable assigned after the `defer` is seen with its new value, but a `let` after the `defer` that reuses a name does not change what the body refers to.
- The body cannot leave: no `return`, no `?`, and no `break` or `continue` for a loop outside it (E0115). Its value is discarded.
- A panic inside a deferred body replaces the one that was leaving the block, if any.
- In a native build, the variables a deferred body reads, and those declared before the `defer` that the code after it changes, are kept in memory rather than registers, so after a panic the body sees their latest values. Everything else compiles as usual; a cleanup `defer` costs nothing measurable in a hot loop.

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
| `(p1, p2)` | a tuple, matching each item |

```
match shape {
    Circle(r) => 3.14159 * r * r,
    Rect(w, h) => w * h,
    Point => 0.0,
}
```

Arms are separated by `,` or a newline. An arm's body is an expression, so a statement such as an assignment needs a block: `None => { more = false },`. Patterns nest: `Err(Failed(msg))`, `Some(Some(n))`. A `match` must be exhaustive (E0204); the error comes with an auto-fix that adds the missing arms. Matching on `Int` or `Str` needs a `_` or binding arm. Tuples are checked column by column, so `(true, _)` and `(false, _)` together are exhaustive.

## 8. Value semantics

Every value behaves as if it were copied on assignment, on passing and on capture. No two variables ever observe each other's mutations:

```
let mut a = [1]
let b = a
a.push(2)      // a is [1, 2], b is still [1]
```

The native backend implements this with reference counting and copy-on-write. A value is copied only if it is shared at the moment it is mutated. A value's last use moves it rather than retaining it, so in-place updates are the normal case.

## 9. Capabilities

`Root` has ten fields, each a capability, and one method:

| Field | Type | Granted |
| --- | --- | --- |
| `root.stdout` | `Stdout` | always |
| `root.stdin` | `Stdin` | always |
| `root.clock` | `Clock` | always |
| `root.rand` | `Rand` | always; `--seed N` makes the numbers repeatable, the same in both backends |
| `root.env` | `Env` | only with `--grant env=NAME` (repeatable) or `--grant env=*`; other variables read as `None` |
| `root.net` | `Net` | only with `--grant net=HOST[:PORT]` (repeatable) or `--grant net=*` |
| `root.fs` | `Fs` | only with `--grant fs=DIR` or `--grant fs=DIR:ro` |
| `root.listen` | `Listen` | only with `--grant listen=HOST[:PORT]` (repeatable) |
| `root.exec` | `Exec` | only with `--grant exec=PROGRAM` (repeatable) or `--grant exec=*` |
| `root.foreign` | `Foreign` | only with `--grant foreign=LIB` (repeatable) or `--grant foreign=*`: calls into C libraries |

`root.args()` is the `List[Str]` of command-line arguments after `--`: `vela run app.vela -- a b`.

Grants can also be written once in the `[grants]` table of the program's vela.toml (section 18), so they are not retyped every run. `vela run` applies them first and the `--grant` flags add to them; a `--grant fs=...` flag replaces the fs grant, since there is only one. A binary from `vela build` reads only its flags.

An ungranted `Net`, `Fs`, `Listen` or `Exec` still exists, but every operation on it returns `Err(Denied(...))`. An ungranted `Foreign` exists too; calling a foreign function through it panics (`foreign.allows(lib)` checks first). `Listener` and `Conn` are capabilities too: you get them from `Listen.on`, `Listener.accept` and `Net.connect`.

The rules:

1. Programs cannot create capabilities. They come only from `root`.
2. Attenuation only narrows. `net.only(host)` returns a `Net` limited to that host, and it is empty if you didn't hold that host. `fs.under(dir)` limits to a subdirectory; `fs.read_only()` removes writing. `exec.only(program)` limits `Exec` to one program; `foreign.only(lib)` limits `Foreign` to one library.
3. There is no ambient I/O. Using `print`, `open`, `fetch` and similar names is error E0302, which explains the capability to use instead.
4. A function whose parameters contain no capability and no function type is **pure**: it can only compute (and panic). `vela caps FILE` lists each function's authority.
5. Enforcement at runtime:
   - `Net`: http and https URLs only, host allowlist, redirects not followed. `Net.connect` and `Net.connect_tls` use the same allowlist. TLS certificates are always verified (section 10).
   - `Exec`: the program name must match a grant exactly (`git` and `/usr/bin/git` are different grants), or the grant is `*`. Arguments are passed as a list, never through a shell. The child gets `/dev/null` as stdin, the working directory, `PATH`, and only the environment variables granted with `--grant env=`. A program name without `/` is looked up in `PATH`, as a shell would. Granting a program grants everything that program can do with any arguments, so `exec=sh` is as good as every other grant.
   - `Listen`: binding is allowed only for granted `host` or `host:port` entries, with or without TLS.
   - `Fs`: paths are relative to the granted directory; absolute paths, `..` escapes and symlinks pointing outside are refused.
   - `Foreign`: a call checks that the function's library is on the `Foreign` value's list (or the list holds `*`). C code runs with the whole process's authority, so granting a library grants everything that library can do; see below.

### Foreign functions (C)

A `foreign` block declares functions of a C library. Each has a signature and no body:

```
foreign "sqlite3" {
    fn sqlite3_libversion() -> Str
    fn sqlite3_open_v2(path: Str, db: Out[Handle], flags: Int32, vfs: Int) -> Int32
    pub fn sqlite3_close_v2(db: Handle) -> Int32
}
```

Calling one takes a `Foreign` capability first, then the other arguments: `sqlite3_libversion(root.foreign)`. Each call checks that the `Foreign` value grants the library (`--grant foreign=sqlite3`), and panics if not. So C is reachable only from code that was handed `Foreign`, `vela caps` lists such functions, and a package dependency can use a foreign block only if the program names it in `[trust] foreign = [...]` in vela.toml (section 18). The library name is the `-l` name: `foreign "sqlite3"` links `-lsqlite3` natively and loads `libsqlite3.dylib` (macOS) or `libsqlite3.so` in the interpreter; `foreign "c"` is the C library itself, which is always there. The function name is the C symbol. Declarations are one per line; `pub fn` exports one from a module.

The types that cross the boundary:

| Vela | C | Notes |
| --- | --- | --- |
| `Int` | `int64_t` | |
| `Int32` | `int32_t` (C `int`) | an `Int` in Vela; an argument outside the 32-bit range panics |
| `Float` | `double` | |
| `Bool` | `int` | 0 is `false`; any other value is `true` |
| `Str` | `const char *` | the string's bytes followed by a NUL; pass the length yourself if C wants it (`s.len()`) |
| `Bytes` | `const void *` | the bytes; pass the length yourself |
| `Handle` | `void *` | an opaque pointer, `Handle { addr: Int }` in Vela; `addr == 0` is NULL |
| `Out[T]` | `T *` | an out-parameter of `Int`, `Int32`, `Float` or `Handle`: C writes it, Vela returns it |

The result is one of the same types (not `Bytes`: C gives no length) or nothing. A `Str` result is copied from a NUL-terminated C string at once (NULL gives `""`); C keeps ownership of the original. `Out` parameters are not passed by the caller: they come back after the result, so `sqlite3_open_v2` above has the Vela type `fn(Foreign, Str, Int, Int) -> (Int, Handle)`, called as `let (rc, db) = sqlite3_open_v2(f, path, flags, 0)`. A function with no result and one `Out` returns that value alone.

Ownership at the boundary:

- Arguments are lent to C for the duration of the call, never moved. A `Str` or `Bytes` pointer is valid only until the call returns: C must copy what it keeps (SQLite's `SQLITE_TRANSIENT`, a destructor argument of `-1`, does this). A `Handle` that C returns pointing into a Vela string is dangling once that string is freed.
- Memory C allocates belongs to C: free it with the library's own function (`sqlite3_finalize`, `free`). Vela never frees a `Handle`, and the leak check does not see C memory.
- `foreign.copy(p, n)` copies `n` bytes at `p` into new `Bytes`, for results that are a pointer and a length (`sqlite3_column_blob` and `sqlite3_column_bytes`). It reads raw memory, so it needs a `Foreign` that grants at least one library.

The foreign grant is full trust, and Vela treats it that way:

- Only the program grants it: `--grant foreign=LIB` or `[grants] foreign` in the program's own vela.toml. `vela run` prints a one-line warning whenever it is given, since nothing in Vela bounds what C code does; only the operating-system sandbox (below) still does.
- It does not pass along: a dependency's `caps` cannot include `foreign`. A dependency may declare foreign blocks only when the program names it in `[trust]`, and vela.lock records the C libraries each trusted dependency uses (section 18).
- `vela check` lists every part of the program that calls C, one line each: the program's own files, trusted dependencies, and trusted standard library modules.
- The standard library calls C only from modules that its manifest, src/std/vela.toml, trusts in the same way (`sql`). Their foreign functions are private to the module and take no `Foreign`, so a program using `sql` grants only an `Fs`; the module is what keeps SQLite inside that grant.

What the capability lets code do: anything the C functions it declares can do, with the program's full authority, outside every other capability's checks. Vela checks the declared types and the library grant; it cannot check that a declaration matches the C prototype, and a wrong one is undefined behaviour. Keep foreign blocks small, private to a module that wraps them in a safe Vela API (as the `sql` module does), and grant `foreign` only to programs and dependencies you trust.

Limits, the same in both backends: at most 8 integer-like (everything but `Float`, `Out` included) and 8 `Float` parameters; no variadic functions (`printf`), no structs by value, no callbacks from C into Vela. A foreign function can only be called, not used as a value (E0331): wrap it in a closure. A C call blocks its worker thread until it returns, like a busy loop, and C libraries decide their own thread safety.

The backends differ in how they reach the code, not in what a program sees: natively each function is called through its exact prototype and linked with `-l<lib>` (a library outside the C compiler's default paths is found through `LIBRARY_PATH`); the interpreter loads the library with `dlopen` and calls through a generic stub, resolving the declarations of granted libraries before `main` starts. A missing library or symbol is therefore an error before the program runs in both: a link error natively, `foreign: cannot load library ...` or `foreign: library ... has no function ...` in the interpreter. One difference follows from linking: a native binary loads every library it declares when it starts (running the library's initialisers, as any linked library does), granted or not, while the interpreter never loads a library the grants do not cover. In both, a call into an ungranted library panics before it reaches the library.

### The operating-system sandbox

Vela's checks cover Vela code. To cover everything else in the process (C called through `foreign`, and any bug in the runtime), every program also runs in an operating-system sandbox built from its grants. It is on by default, in both backends: `vela run` enters it once the program is loaded (and its granted C libraries with it), and a native binary enters it at startup, before any thread starts. `--no-sandbox` (on `vela run`, or on a built binary) turns it off; it is for debugging, not for production.

What the kernel enforces:

| | Allowed |
| --- | --- |
| Writing files | under the `Fs` directory, unless it is read-only; the temporary directory if `Exec` is granted (Exec keeps a child's stderr there); `/dev/null` and the terminal |
| Reading files | under the `Fs` directory; system locations (`/usr`, `/System`, `/Library`, `/etc`, `/dev`, Homebrew's prefix, ...: libraries, time zones, TLS certificates); the temporary directory with `Exec`. Not home directories, other users' files or anything else |
| Network | nothing unless `Net` or `Listen` is granted; then any host |
| Starting programs | only if `Exec` is granted. A started program inherits the sandbox: it can read system locations and the `Fs` directory, and list (not read) the directories above the current one, which shells need |

What stays in Vela's own checks, not the kernel's: which hosts and ports `Net` and `Listen` allow (the kernel allows all networking once either is granted), which programs `Exec` may run, which variables `Env` shows, `Fs`'s refusal of `..` and absolute paths (the kernel sees only the resolved file), and everything inside the process: C code with the foreign grant can still read and corrupt the program's own memory, and through it every capability the program holds.

How:

- macOS: a sandbox profile (`sandbox_init`), allowing by default and denying the above. It also refuses Apple events and LaunchServices opens, so the program cannot have another application act for it. Mach services stay reachable, as for every process.
- Linux: Landlock (kernel 5.13 or later) for files, and on kernels with Landlock ABI 4 or later for TCP too, plus a seccomp filter that, without `Net` or `Listen`, allows only Unix sockets and refuses io_uring, and without `Exec` refuses `execve`; it also refuses x32 system calls. It has been run on arm64 Linux (kernel 7.0); on x86-64 it compiles but has not been run on a real kernel.
- A system without a sandbox (another OS, a Linux kernel without Landlock or seccomp filters, an emulator) runs the program without one, silently, as before. A sandbox that exists but cannot be entered stops the program with a message naming `--no-sandbox`.
- `vela test` runs native tests in the sandbox too (a test that takes `fs: Fs` gets its temporary directory); interpreted tests run inside the `vela` process, unsandboxed.

## 10. Concurrency

```
let results = group g {
    for row in rows {
        g.go(|| count_row(row))
    }
}
```

- `group g { body }` runs `body`, in which `g.go(f)` starts `f: fn() -> T` as a task. The group evaluates to a `List[T]` of the task results **in the order the tasks were started**.
- A task whose closure returns `()` runs for its effect and adds nothing to the list, so one group can mix them with tasks that return a value: the list holds the results of the value tasks, which must all have one type. A group of only `()` tasks evaluates to an empty list. To drop a value a task would return, end its body with a statement: `g.go(|| { log(x); })`. The rule is decided by the closure's type as written, so in generic code a task returning `T` always adds an item, even when `T` is `()`.
- When the block ends, every task has finished. Nothing outlives the group.
- `g` can only be used as `g.go(...)` directly in the body. It cannot be stored, passed, returned, or used inside a closure (E0310).
- The body cannot leave early with `return` or `?` (E0311), and `break` and `continue` cannot target loops outside the group.
- Tasks share data by value, so there are no data races. A task reaches the world only through capabilities its closure captured. Spawning is not a capability.
- Tasks are lightweight: natively each is a fiber with a small stack, scheduled on one worker thread per core (`VELA_THREADS` overrides this), and a task waiting on a group, a channel, a socket, stdin or `clock.sleep(ms)` parks without blocking a thread. The interpreter runs tasks as coroutines. Output printed by tasks may interleave differently between the two.
- If every task is waiting and nothing can wake one (no socket and no timer can), the program panics with `all tasks are blocked (deadlock)` instead of hanging.
- A panic in a task ends only that task. The group still waits for the others, then passes the first panic on (section 15).

### Channels

```
let c: Chan[Int] = channel(8)        // holds up to 8 values; channel(0) holds none
let total = group g {
    g.go(|| {                         // returns (): adds no item
        for i in [1, 2, 3] { c.send(i) }
        c.close()
    })
    g.go(|| {
        let mut sum = 0
        let mut more = true
        while more {
            match c.recv() {
                Some(v) => { sum = sum + v },
                None => { more = false },
            }
        }
        sum
    })
}                                    // [6]
```

- `channel(n)` creates a `Chan[T]` holding up to `n` values; `T` comes from the annotation or from use. A negative capacity panics.
- `channel(0)` is unbuffered: `send` waits until a receiver takes the value, so sender and receiver meet. In a `select`, an unbuffered send arm is ready only while a receiver is waiting, and a receive arm only while a sender is waiting (or the channel is closed).
- `c.send(v)` waits while the channel is full; sending on a closed channel panics, and so does a send that is waiting when the channel is closed.
- `c.recv()` waits while the channel is empty and returns `Some(v)`; once the channel is closed and drained it returns `None`.
- `c.close()` closes it; closing twice panics.
- A channel is shared, not copied: tasks that capture the same channel talk through it.

### select

`select` waits on several channel operations and runs the arm of the one that happens:

```
while nums_open || words_open {
    select {
        n = nums.recv() if nums_open => match n {
            Some(x) => { total = total + x },
            None => { nums_open = false },      // nums is closed and empty
        },
        w = words.recv() if words_open => match w {
            Some(s) => { letters = letters + s },
            None => { words_open = false },
        },
        results.send(total) => (),
        _ => (),                                 // optional: do not wait
    }
}
```

- An arm is `name = ch.recv()` (the name gets `Option[T]`; `None` means closed and empty), `ch.send(value)`, a timeout `clock.after(ms)`, or `_`. Like `match`, `select` is an expression and every arm has the same type.
- All channels, values and guards are evaluated first, in order. Then the first ready operation in the order written runs, so list the channel you want served first first. With no ready operation, `_` runs if there is one; otherwise `select` waits until one is ready.
- A closed channel is always ready to receive (`None`), so turn its arm off with a guard: `if cond` after the operation skips the arm when `cond` is false. If every arm is off and there is no `_`, `select` panics instead of waiting forever.
- A send arm that is not chosen sends nothing. A send arm on a closed channel panics, as `send` does.
- `clock.after(ms)` is ready once `ms` milliseconds have passed since the `select` started (after its operands were evaluated); `ms <= 0` is ready at once. It takes its place in the order like any arm, so a channel arm written before it still wins when both are ready. A `select` has at most one timeout (E0106), and not together with `_`, which never waits (E0003). `clock` must be the `Clock` capability itself; `after` exists only as a `select` arm.

```
let reply = select {
    r = replies.recv() => r ?? "closed",
    clock.after(500) => "timed out",
}
```

To wait for something until a deadline across a loop, compute the remaining time each round: `clock.after(deadline - clock.now_ms())`.

### Time

```
clock.sleep(250)                      // wait a quarter of a second
```

- `clock.sleep(ms)` waits at least `ms` milliseconds; other tasks keep running, and natively no thread waits. Time is measured on a monotonic clock, so changing the system time does not shorten or stretch a sleep.
- Waiting needs the `Clock` capability like reading the time does, so a pure function cannot depend on timing. For deterministic tests, take the clock through an interface (`interface Sleeper { fn sleep(self, ms: Int) }`) and pass a fake that does not wait; `Clock` satisfies such interfaces (section 4).

### Deadlines and cancellation

```
let pages = group g until clock.after(2000) {
    for u in urls {
        g.go(|| fetch(net, u))
    }
}
// pages: List[Option[Str]], None for each fetch still running at the deadline
```

- `group g until clock.after(ms) { body }` gives the group a deadline `ms` milliseconds after it starts. Its value is a `List[Option[T]]`, in start order: `Some(v)` for a task that finished, `None` for a task the deadline cancelled. As in any group, tasks that return `()` are left out of the list.
- At the deadline every task still running is cancelled. A cancelled task stops at a wait: if it is waiting (on a channel, a `select`, `clock.sleep`, a socket, stdin or a group) it stops at once; otherwise it stops when it next has to wait. Code that does not wait cannot be stopped, and a task that finishes without waiting again keeps its result. A task started after the deadline never runs, and `ms <= 0` cancels every task before it starts.
- A cancelled task unwinds as a panic does: everything it owns is freed and sockets it holds are closed. A group the task is running cancels its own tasks too and waits for them. `catch` inside the task does not stop a cancellation, and a cancellation is not a panic: the group passes nothing on for it. A panic in another task of the group is still passed on once every task has stopped.
- The deadline cancels the tasks, not the body: keep the body to starting tasks and do blocking work, like `accept`, inside a task.

### Sockets

```
fn main(root: Root) -> Result[(), IoError] {
    let l = root.listen.on("127.0.0.1:9000")?     // --grant listen=127.0.0.1:9000
    let served = group g {
        let mut i = 0
        while i < 100 {
            match l.accept() {
                Ok(c) => g.go(|| echo(c)),
                Err(e) => root.stdout.print("{e}"),
            }
            i = i + 1
        }
    }
    Ok(())
}
```

- `Listen.on("host:port")` binds; port `0` picks a free port, which `Listener.port()` reports.
- `Listener.accept()` waits for a connection. `Net.connect("host:port")` opens one.
- `Conn.read()` waits for data and returns what arrived; `""` means the peer closed. `Conn.write(s)` sends all of `s`. Both return `Result`.
- `close()` on a `Listener` or `Conn` makes any task waiting on it get `Err(Failed("... is closed"))`.
- One task at a time reads a connection or accepts on a listener, and one writes a connection. A `read` or `accept` that has to wait while another task is already waiting for the same thing, or a `write` that starts while another task's write on that connection is still going, returns `Err(Failed("another task is already reading this connection"))` (or `accepting on this listener`, `writing this connection`) instead of mixing their data; a read or accept that can go on at once is not refused, and a refused write sends nothing. Both backends behave alike.

### TLS

```
// client: --grant net=api.example.com
let c = root.net.connect_tls("api.example.com:443")?
c.write("GET / HTTP/1.1\r\nHost: api.example.com\r\nConnection: close\r\n\r\n")?

// server: --grant listen=0.0.0.0:8443 --grant fs=/etc/myapp:ro
let cert = root.fs.read("cert.pem")?
let key = root.fs.read("key.pem")?
let l = root.listen.on_tls("0.0.0.0:8443", cert, key)?
```

- `Net.connect_tls("host:port")` connects like `connect`, then does the TLS handshake and returns a `Conn`. Everything that works on a `Conn` works on it unchanged: `read`, `write`, `close`, the one-reader-one-writer rule, and the `http` module.
- The server's certificate is always verified: its chain against the trusted certificates, and its name (or IP address, when `host` is one) against `host`. There is no switch to turn this off. A failure is `Err(Failed("tls: certificate verify failed: ..."))`, for example `self-signed certificate` or `hostname mismatch`.
- The trusted certificates are the system's (see "TLS library" below). Whoever starts the program can replace them with the standard `SSL_CERT_FILE` (a PEM file) or `SSL_CERT_DIR` environment variables, as for curl, Python or Go; this is how tests trust a locally made certificate. The program itself cannot change what is trusted.
- `Listen.on_tls("host:port", cert, key)` binds like `on`; `accept()` then returns TLS connections. `cert` is the PEM text of the certificate, optionally followed by the intermediate certificates of its chain, and `key` the PEM text of its unencrypted private key. A certificate that cannot be parsed or a key that does not match it is `Err(Failed("tls: ..."))`, and nothing stays bound.
- How this fits the capabilities: the certificate and key are plain text, so the program reads them with whatever capability holds them (usually `Fs`, or `Env` for secrets passed in the environment) and passes the text on. Serving TLS needs only the `Listen` grant for the address; there is no separate grant and no path the runtime opens behind the program's back.
- A server connection does its handshake during its first `read` or `write`, so `accept()` never waits for a slow client, and a failed handshake is that `read`'s error. A client connection has done its handshake when `connect_tls` returns.
- Handshakes, reads and writes park the task like plain socket operations; no worker thread blocks. Cancellation (a group deadline) works the same.
- TLS 1.2 or later. Clients do not send certificates and servers do not ask for them. `close()` sends the TLS close message. A peer that closes without one may have been cut off by someone in between, so that `read` is `Err(Failed("tls: the connection was closed without a TLS close message, so the data may be cut short"))` rather than `""`. `Net.get` and the `http` client still accept such a response when its `Content-Length` or chunked body shows it is complete, and refuse one whose body runs until the close.
- Errors from TLS start with `tls: `. Both backends use the same TLS code, so they verify and fail alike.

#### TLS library

TLS is OpenSSL (1.1.1 or later), in both backends: the interpreter is linked with it, and native programs that use `Net.get`, `connect_tls` or `on_tls` are linked with it. It is found when vela is built: `$VELA_OPENSSL` (an install prefix) if set, else `pkg-config openssl` (looking in Homebrew's `openssl@3` first), else the system headers. On macOS that is Homebrew's `openssl@3` (`brew install openssl@3`), whose trusted certificates are Homebrew's `cert.pem` bundle; on Linux it is the distribution's libssl (`libssl-dev` or `openssl-devel` to build), trusting the distribution's certificate store. Native programs link OpenSSL's shared libraries, so a binary from `vela build` that uses TLS needs them where it runs. If OpenSSL is not found, vela still builds and runs everything else, and every TLS operation fails with `Failed("tls: ... built without TLS ...")`.

### Running programs

```
fn main(root: Root) -> Result[Int, IoError] {
    let r = root.exec.run("git", ["status", "--short"])?    // --grant exec=git
    root.stdout.write(r.stdout)
    Ok(r.code)
}
```

- `Exec.run(program, args)` runs `program` with the list `args`, waits for it to finish, and returns `Ok(ProcessOutput { code, stdout, stderr })` with its exit status and everything it wrote. A child killed by signal N has code 128 + N.
- It is `Err(Denied(...))` if the program is not granted, and `Err(Failed(...))` if it cannot be started (for example, not found). A non-zero exit status is not an error; check `code`.
- Waiting for the child parks only the calling task, so a group can run several programs at once.

### HTTP

```
let resp = net.get("http://127.0.0.1:8080/status")?
if resp.status == 200 {
    out.print(resp.headers.get("content-type") ?? "no type")
    out.print(resp.body)
}
```

- `Net.get(url)` sends one HTTP/1.1 `GET` (with `Host`, `User-Agent: vela`, `Accept: */*` and `Connection: close`), reads until the server closes, and returns `HttpResponse { status, headers, body }`.
- Every status is `Ok`, including 404 and 500; redirects are returned, not followed. `Err(Failed(...))` means the server could not be reached or the response was not valid HTTP; responses over 64 MiB, with more than 64 KiB of headers or with more than 1000 header lines are refused.
- `headers` is a `Map[Str, Str]` with lowercased names, in the order they arrived; a repeated header's values are joined with `", "`. The body is decoded from chunked transfer encoding or cut to `Content-Length`.
- `https://` URLs go over `Net.connect_tls` (port 443 unless given), with the certificate checks described under TLS. There is no timeout yet. For other methods, request headers or bodies, use `http.request` from the `http` module, which works over `Net.connect` and `Net.connect_tls`.

## 11. Built-in methods

Methods take the receiver by value and never modify it, except `push`, which needs a `mut` place.

### Str

| Method | Result |
| --- | --- |
| `len()` | `Int`, bytes |
| `is_empty()` | `Bool` |
| `lines()` | `List[Str]`, split on `\n`, stripping a trailing `\r` and dropping one final empty line |
| `split(sep)` | `List[Str]`, keeping empty pieces (`"a,,b".split(",")` is `["a", "", "b"]`); `split("")` splits into single bytes |
| `chars()` | `List[Str]`, one string per UTF-8 code point; a byte that does not start a well-formed one is a piece of its own, so `s.chars().join("") == s` always |
| `trim()` | `Str` without leading and trailing spaces, tabs, newlines, `\r` and form feeds |
| `contains(s)`, `starts_with(s)`, `ends_with(s)` | `Bool` |
| `to_upper()`, `to_lower()` | `Str`, ASCII only |
| `replace(a, b)` | `Str`, every occurrence |
| `parse_int()` | `Option[Int]`, decimal with an optional sign, surrounding whitespace allowed |
| `parse_float()` | `Option[Float]`, decimal with optional sign, point and exponent (`"2.5e3"`); no hex, `nan` or `inf` |
| `slice(lo, hi)` | `Str`, bytes `lo` up to `hi`; out-of-range bounds are clamped, so it never panics |
| `index_of(s)` | `Option[Int]`, byte offset of the first occurrence |
| `bytes()` | `List[Int]`, each byte as 0-255; index it to scan a string fast |

### Bytes

Binary data, kept apart from text so a byte buffer is never printed or concatenated as a string by mistake. Natively it is stored like a `Str`, so conversions cost one reference.

| Method | Result |
| --- | --- |
| `bytes(list)` (function) | `Bytes` from a `List[Int]`; an item outside 0 to 255 panics |
| `s.to_bytes()` (on `Str`) | the string's bytes |
| `len()`, `is_empty()` | `Int`, `Bool` |
| `b[i]`, `get(i)` | `Int` (0 to 255; panics out of range), `Option[Int]` |
| `slice(lo, hi)` | `Bytes`, clamped like `Str.slice` |
| `to_list()` | `List[Int]` |
| `to_str()` | `Str` with the same bytes (no check: a `Str` may hold any bytes) |

`+` concatenates, `==` and `<` compare byte by byte, and `Bytes` can be a map key. It cannot be changed in place (`b[i] = x` is E0209). Capabilities still read and write `Str`; convert with `to_bytes()` and `to_str()`.

### Int, Float, Bool

| Type | Methods |
| --- | --- |
| `Int` | `to_str()`, `to_float()`, `abs()`, `min(n)`, `max(n)`, `to_char()` (the UTF-8 text of a code point, `65.to_char()` is `"A"`; invalid code points give U+FFFD) |
| `Float` | `to_str()`, `to_int()` (truncates; NaN gives 0), `abs()`, `sqrt()`, `to_fixed(d)` (exactly `d` decimals, 0 to 30, rounded as C's printf rounds: `3.14159.to_fixed(2)` is `"3.14"`; NaN and infinities give `nan`, `inf`, `-inf`) |
| `Bool` | `to_str()` |

### List[T]

| Method | Result |
| --- | --- |
| `len()`, `is_empty()` | `Int`, `Bool` |
| `get(i)`, `first()`, `last()` | `Option[T]` |
| `push(x)` | `()`, appends in place (needs `mut`) |
| `contains(x)` | `Bool` |
| `reverse()`, `take(n)`, `skip(n)` | `List[T]`; `take` and `skip` clamp out-of-range counts |
| `sort()` | `List[T]`, stable, in the order of `<`; `T` must be ordered (section 5) |
| `map(f)`, `filter(p)` | `List[U]`, `List[T]` |
| `fold(init, f)` | `U`, with `f: fn(U, T) -> U` |
| `join(sep)` | `Str`; `List[Str]` only |
| `sum()` | `Int`; `List[Int]` only |

`xs[i]` panics when `i` is out of range; `xs.get(i)` returns `None` instead. Writes go through index and field paths: `groups[i].words.push(w)` and `items[i].qty = 3` both work on a `mut` list. `sort()` uses the order of `<`, so it sorts tuples by their first item, then the second, and structs by their fields in declaration order; for another order and for inserting or removing at a position, use the `list` module (`sort_by`, `insert_at`, `remove_at`, section 19).

Strings compare byte by byte, so `c >= "a" && c <= "z"` tests a one-byte string, such as a character from `chars()`. `slice`, `index_of` and `bytes()` give positions and byte codes.

### Map[K, V]

| Method | Result |
| --- | --- |
| `len()`, `is_empty()` | `Int`, `Bool` |
| `m[k]` | `V`; panics if `k` is missing |
| `get(k)` | `Option[V]` |
| `contains(k)` | `Bool` |
| `m[k] = v` | inserts or replaces (needs `mut`) |
| `m[k].field = v`, `m[k].push(x)`, ... | changes the entry in place (needs `mut`); panics if `k` is missing |
| `remove(k)` | `()`, removes the entry if present (needs `mut`) |
| `keys()`, `values()` | `List[K]`, `List[V]`, in insertion order |
| `entries()` | `List[(K, V)]`, in insertion order |

### Array[T, N]

`len()` (always `N`), `get(i)`, `contains(x)`, `to_list()`, indexing with `a[i]`, and `for x in a`.

### Option[T] and Result[T, E]

`x ?? default` is the value inside `Some` or `Ok`, or `default` otherwise; `default` is evaluated only when it is needed. `??` binds tighter than arithmetic, so `counts.get(w) ?? 0 + 1` adds 1 to the count. There is no `unwrap_or`; using it is error E0105 with a fix that rewrites it.

| Method | Result |
| --- | --- |
| `is_some()`, `is_none()` / `is_ok()`, `is_err()` | `Bool` |
| `map(f)` (Option) | `Option[U]` |
| `map_err(f)` (Result) | `Result[T, F]` |

### Chan[T]

| Method | Result |
| --- | --- |
| `send(v)` | `()` |
| `recv()` | `Option[T]` |
| `close()` | `()` |

### Built-in functions

| Function | Meaning |
| --- | --- |
| `panic(msg)` | stop with a message and source location (section 15) |
| `catch(f)` | `Result[T, Str]`: run `f: fn() -> T`; `Err(message)` if it panicked (section 15) |
| `channel(n)` | a new `Chan[T]` with capacity `n` |
| `map()`, `map(pairs)` | a new `Map[K, V]`, empty or from a `List[(K, V)]` |
| `bytes(list)` | a new `Bytes` from a `List[Int]` of values 0 to 255 |
| `assert(cond)` | panic with `assertion failed` unless `cond` is true |
| `assert_eq(a, b)` | panic with `assertion failed: <a> != <b>` unless `a == b` |

### Capabilities

| Capability | Method | Result |
| --- | --- | --- |
| `Stdout` | `print(s)` | `()`, writes `s` and a newline |
| `Stdout` | `write(s)` | `()`, writes `s` |
| `Clock` | `now_ms()` | `Int`, milliseconds since the Unix epoch |
| `Clock` | `sleep(ms)` | `()`, waits `ms` milliseconds (section 10); returns at once if `ms <= 0` |
| `Rand` | `int(lo, hi)` | `Int` in `lo` up to but not including `hi`; panics if `hi <= lo` |
| `Rand` | `float()` | `Float` in [0, 1) |
| `Stdin` | `read_line()` | `Option[Str]`, without the newline (or `\r\n`); `None` at the end of input. Waiting for input parks the task; one task reads at a time, so tasks reading together each get whole lines |
| `Stdin` | `read_all()` | `Str`, the rest of the input |
| `Env` | `get(name)` | `Option[Str]`; `None` if unset or not granted |
| `Net` | `get(url)` | `Result[HttpResponse, IoError]`: `status`, `headers` (`Map[Str, Str]`, lowercased names), `body`; http and https (section 10) |
| `Net` | `only(host)` | `Net`; `host` is `name`, `name:port` or `*` |
| `Net` | `connect(addr)` | `Result[Conn, IoError]`; `addr` is `host:port` |
| `Net` | `connect_tls(addr)` | `Result[Conn, IoError]`, a TLS connection whose certificate was verified for `host` |
| `Exec` | `run(program, args)` | `Result[ProcessOutput, IoError]`: `code`, `stdout`, `stderr` (section 10) |
| `Exec` | `only(program)` | `Exec`; `program` is a name or `*` |
| `Foreign` | `only(lib)` | `Foreign`; `lib` is a library name or `*` |
| `Foreign` | `allows(lib)` | `Bool`: whether calls into `lib` are granted |
| `Foreign` | `copy(p, n)` | `Bytes`: `n` bytes of C memory at `Handle` `p` (section 9) |
| `Listen` | `on(addr)` | `Result[Listener, IoError]` |
| `Listen` | `on_tls(addr, cert, key)` | `Result[Listener, IoError]` whose connections speak TLS; `cert` and `key` are PEM text |
| `Listener` | `accept()` | `Result[Conn, IoError]` |
| `Listener` | `port()` | `Int` |
| `Listener`, `Conn` | `close()` | `()` |
| `Conn` | `read()` | `Result[Str, IoError]`; `""` when the peer closed |
| `Conn` | `write(s)` | `Result[(), IoError]` |
| `Fs` | `read(path)` | `Result[Str, IoError]` |
| `Fs` | `write(path, text)` | `Result[(), IoError]` |
| `Fs` | `append(path, text)` | `Result[(), IoError]`, creating the file if needed |
| `Fs` | `list(dir)` | `Result[List[Str], IoError]`, names sorted, without `.` and `..`; `""` is the granted directory |
| `Fs` | `make_dir(path)` | `Result[(), IoError]`; the parent must exist |
| `Fs` | `remove(path)` | `Result[(), IoError]`, a file or an empty directory, never the granted directory itself |
| `Fs` | `exists(path)`, `is_dir(path)` | `Bool` |
| `Fs` | `under(dir)`, `read_only()` | `Fs` |
| `Fs` | `full_path(path)` | `Result[Str, IoError]`: the absolute path, checked as for writing (the file need not exist, its directory must), with the directory part resolved to its real path, for handing to C or another program |
| `Fs` | `is_read_only()` | `Bool`; an ungranted `Fs` is read-only |

## 12. Tests

```
fn add(a: Int, b: Int) -> Int { a + b }

test "adds small numbers" {
    assert_eq(add(2, 3), 5)
    assert(add(-1, 1) == 0)
}
```

- `test "name" { ... }` is a top-level item. Tests get no `root`, so by default they cannot do I/O; they can use groups and channels, and fakes through interfaces.
- A test can ask the runner for two real capabilities, by declaring them like parameters: `fs: Fs` is a fresh, empty, writable temporary directory, removed when the test ends, and `clock: Clock` is the real clock. Each at most once; anything else is E0306.

```
test "saves the config" (fs: Fs) {
    assert(save(fs, "app.conf", 3).is_ok())
    assert_eq(fs.read("app.conf"), Ok("3"))
}
```
- `vela test FILE` runs every test, each isolated, and prints `ok` or `FAIL` with the panic message and location; it exits with status 1 if any test fails. `--native` runs them as native code.
- Test names must be unique (E0106). Tests are type-checked by `vela check` and ignored by `vela run` and `vela build`.

## 13. How values print

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

## 14. Optimizations that affect observable behavior

The native backend may run a chain like `xs.filter(p).map(f).fold(z, g)` as one fused loop, interleaving the stages per element. It does this only when the earlier stages are pure closure literals, which capture no capability and no function. The only observable difference is **which panic happens first** when more than one stage would panic. Everything else (results, output order, values) is the same.

## 15. Errors

Every diagnostic has a stable code. `vela explain CODE` prints its explanation, and `vela check --json` emits code, span, notes and an auto-fix when there is one.

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
| E0110 | misplaced `self`, a method on a type that cannot have them, or a `pub` method on a built-in type |
| E0111 | a type does not satisfy an interface |
| E0112 | a value that cannot become `dyn` |
| E0113 | a map key type that is not `Int`, `Str`, `Bytes`, `Bool`, or a tuple, struct or enum of them |
| E0114 | a struct that contains itself by value |
| E0115 | `return`, `?`, `break` or `continue` that would leave a `defer` body |
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
| E0305 | channel stored in a field |
| E0306 | a test parameter that is not `fs: Fs` or `clock: Clock` |
| E0310 | misuse of a group handle |
| E0311 | `return` or `?` leaving a group |
| E0320 | a dependency takes a capability vela.toml does not grant it |
| E0321 | malformed vela.toml |
| E0322 | a git dependency could not be fetched |
| E0323 | vela.lock does not match: a malformed line, or a dependency whose files changed |
| E0324 | a trusted dependency calls a C library that vela.lock did not record |
| E0330 | a foreign function signature with a type that cannot cross to C, too many parameters, or a bad library name |
| E0331 | a foreign function used as a value instead of called |
| E0401 | module not found, or a qualified name without `use` |
| E0402 | item is not `pub` |
| E0403 | module has no such item |
| E0404 | module name used as a value |

A **panic** (division by zero, index out of range, a missing map key, `panic(...)`, no matching arm) stops the code that hit it:

- Inside `catch(|| ...)`, the panic ends the closure and `catch` returns `Err(message)`. Otherwise it returns `Ok(value)`.
- Inside a task, the panic ends only that task; the other tasks of its group run to completion. When the group finishes, it passes the first panic (in the order the tasks were started) on to the code that ran the group, where a `catch` can handle it.
- Anywhere else, it prints `panic: message` and the source location to stderr, and the program exits with status 1. A deadlock always ends the program.

```
let r = catch(|| parse_config(text))      // Result[Config, Str]
match r {
    Ok(cfg) => run(cfg),
    Err(msg) => out.print("bad config: {msg}"),
}
```

A panic unwinds the stack up to the boundary: everything the abandoned code owned is freed, including values made before the boundary and moved into it, so a loop that catches a panic on every iteration does not grow. Even so, `catch` is for keeping a program alive through bugs, such as a server answering 500 for one request, not for ordinary control flow; use `Result` and `?` for errors you expect.

## 16. Grammar

EBNF. `NL` is a newline, as described in section 2. A character-level version for constrained decoding, with the newline rules spelled out, is in [grammar/vela.gbnf](grammar/vela.gbnf). `{ x }` means zero or more, and `[ x ]` means optional.

```
program    = { "use" IDENT | [ "pub" ] ( fn | struct | enum | interface ) | test | foreign } ;
foreign    = "foreign" STRING "{" { [ "pub" ] "fn" IDENT "(" [ params ] ")" [ "->" type ] ( NL | ";" ) } "}" ;
interface  = "interface" IDENT "{" { "fn" IDENT "(" "self" { "," param } ")" [ "->" type ] ( "," | NL ) } "}" ;
test       = "test" STRING block ;
fn         = "fn" IDENT [ bgenerics ] "(" [ params ] ")" [ "->" type ] block ;
bgenerics  = "[" IDENT [ ":" bound { "+" bound } ] { "," IDENT [ ":" bound { "+" bound } ] } "]" ;
bound      = IDENT [ "." IDENT ] ;                (* an interface *)
generics   = "[" IDENT { "," IDENT } "]" ;
params     = param { sep param } [ "," ] ;
param      = IDENT ":" type ;
struct     = "struct" IDENT [ generics ] "{" [ field { sep field } [ "," ] ] "}" ;
field      = IDENT ":" type ;
enum       = "enum" IDENT [ generics ] "{" [ variant { sep variant } [ "," ] ] "}" ;
variant    = IDENT [ "(" type { "," type } ")" ] ;
type       = "dyn" IDENT [ "." IDENT ]               (* an interface value *)
           | "(" type "," type { "," type } ")"     (* a tuple *)
           | IDENT [ "." IDENT ] [ "[" typearg { "," typearg } "]" ]
           | "(" ")"
           | "fn" "(" [ type { "," type } ] ")" "->" type ;
typearg    = type | INT ;                       (* INT only as Array's size *)
sep        = "," | NL ;

block      = "{" { stmt ( NL | ";" ) } [ expr ] "}" ;
stmt       = "let" [ "mut" ] IDENT [ ":" type ] "=" expr
           | "let" tuplepat "=" expr
           | "return" [ expr ]
           | "while" expr_ns block
           | "for" ( IDENT | tuplepat ) "in" expr_ns block
           | "break" | "continue"
           | "defer" ( block | stmt )             (* not a `let` *)
           | expr [ "=" expr ] ;

expr       = unary { BINOP unary } ;            (* precedence: section 5 *)
expr_ns    = expr ;                             (* no struct literal at the top level *)
unary      = ( "-" | "!" ) unary | postfix ;
postfix    = primary { "(" [ args ] ")" | "[" expr "]" | "." IDENT [ "(" [ args ] ")" ] | "." INT | "?" } ;
args       = expr { sep expr } [ "," ] ;
primary    = INT | FLOAT | STRING | "true" | "false"
           | [ IDENT "." ] IDENT [ "{" [ init { sep init } [ "," ] ] "}" ]   (* struct literal *)
           | "(" ")" | "(" expr ")" | "(" expr "," expr { "," expr } ")"   (* tuple *)
           | "[" [ args ] "]"
           | block
           | "if" expr_ns block [ "else" ( block | if ) ]
           | "match" expr_ns "{" { pattern "=>" expr ( "," | NL ) } "}"
           | "select" "{" { selarm ( "," | NL ) } "}"
           | "|" [ cparam { "," cparam } ] "|" expr | "||" expr
           | "group" IDENT [ "until" expr ] block ;   (* until clock.after(ms) *)
selarm     = ( IDENT "=" expr | expr | "_" ) [ "if" expr ] "=>" expr ;
                                                (* ch.recv(), ch.send(v) or clock.after(ms) *)
init       = IDENT [ ":" expr ] ;
cparam     = IDENT [ ":" type ] ;
pattern    = "_" | IDENT | [ "-" ] INT | STRING | "true" | "false"
           | IDENT [ "." IDENT ] [ "(" pattern { "," pattern } ")" ]
           | tuplepat ;
tuplepat   = "(" pattern "," pattern { "," pattern } ")" ;
```

## 17. Tools

- `vela fmt FILE` prints the canonical formatting (`--write` rewrites, `--check` verifies).
- `vela lsp` is a language server (diagnostics, hover, definition, references, rename, completion, quick fixes, outline); `vela mcp` is an MCP server exposing check, run, test, format, caps, type-at, references, rename and the spec to coding agents. Rename is refused when it is unsafe: the renamed program must check with no new errors, and the new name must refer to exactly the renamed places.
- `vela caps FILE` lists each function's authority; `vela explain CODE` explains an error.
- `vela doc FILE|MODULE... [--std] [--all] [--html] [-o OUT]` prints documentation built from doc comments (section 2) and signatures, as Markdown, or with `--html` as one static page. An argument that is not a file names a standard library module; `--std` documents all of them. It lists `pub` items (everything with `--all`); methods appear under their type, and methods on a type the module does not declare (such as `Str`) get their own section. Files are parsed, not type-checked.

```vela
// Geometry helpers.

// A point on the plane.
pub struct Point {
    // Distance from the left edge.
    x: Float,
    y: Float,
}

// How far `self` is from the origin.
pub fn norm(self: Point) -> Float {
    (self.x * self.x + self.y * self.y).sqrt()
}
```

## 18. Modules and packages

### Modules

```
use json                  // json.vela next to this file, a dependency, or the standard library

pub fn load(text: Str) -> Result[json.Value, Str] {
    json.parse(text)
}
```

- `use NAME` makes module `NAME` available in this file. It looks for `NAME.vela` next to the file, then a dependency named `NAME` in vela.toml, then the standard library. A file never imports itself.
- Refer to a module's items as `NAME.item`: functions `json.parse(s)`, types `json.Value`, struct literals `json.Field { key, value }`, variants `json.Num(1.0)` and patterns `json.Num(x)`. A function can be used as a value: `let f = json.parse`.
- Only items marked `pub` (`pub fn`, `pub struct`, `pub enum`) can be used from another module (E0402). Struct fields and enum variants of a `pub` type are all visible.
- Inside a module, its own items are used unqualified. A local variable shadows a module name.
- Variants are matched by name, so `match v { Num(x) => ... }` works without the module prefix when the type is known.
- `test` blocks in a used module are not run; `vela test` runs the tests of the file you give it.
- Modules can use each other, including in cycles. Type names print with their module: `json.Value`.

### Packages and capabilities

A vela.toml next to the program (or in a parent directory) lists the program's own grants, its dependencies, and what each may do:

```
[package]
name = "app"

[grants]                                           # what `vela run` grants the program
fs = "data:ro"
net = ["api.github.com"]
env = ["HOME", "LANG"]

[dependencies]
json2 = { path = "vendor/json2" }                  # pure: no capabilities
fetch = { path = "../fetch", caps = ["net"] }
color = { git = "https://example.com/color.git", tag = "v1.2.0" }
yaml = { git = "git@example.com:me/yaml.git", commit = "3f9c2e1d7a" }
```

- `[grants]` takes `net`, `fs`, `listen`, `env`, `exec` and `foreign`, each a string or a list of strings written exactly like the text after `--grant KIND=`. `fs` takes one directory, relative to vela.toml, with an optional `:ro`. Stdout, Stdin, Clock and Rand are always granted, so listing them is an error. Flags on the command line add to these grants.
- Grants belong to the program being run: only the vela.toml found for the main file is read for them. A dependency's own vela.toml never grants anything, so adding or updating a dependency cannot widen what the program may do. A dependency gets capabilities in exactly one way: your code passes them to it, and only of the kinds its `caps` allow. `[grants]` decides what reaches `main`; `caps` decides which of those may cross into each dependency.
- A dependency is a directory holding `NAME.vela`, plus any modules that file uses from the same directory.
- `caps` lists what the dependency may use: `net` (Net and Conn), `listen` (Listen, Listener and Conn), `fs`, `stdout`, `stdin`, `clock`, `rand`, `env`, `exec`. The default is none. No dependency can take `Root`, and `caps` cannot give `foreign` (E0321).
- `[trust] foreign = ["name", ...]` names the dependencies that may call C: declare foreign blocks and take `Foreign`. Calling C is full trust, since C code is outside every capability check, so it is a separate, explicit list rather than one more cap, and only the program's vela.toml can hold it. An untrusted dependency that calls C is E0320. vela.lock gets a line per trusted dependency that calls C, `foreign NAME LIB,LIB`, written on the first build; when an update makes the dependency declare a library the line does not have, the build stops (E0324) until you review it and delete the line.
- Vela has no ambient authority, so a function can only use a capability that reaches it as a parameter. The checker therefore knows everything a dependency can ever do from its signatures, and rejects the program (E0320) if a dependency's function takes a capability it was not granted. Granting is still only an upper bound: at run time the dependency gets exactly the capabilities you pass it, which can be narrowed with `only` and `under`.
- A dependency is a local directory (`path`) or a git repository (`git`) at a `tag` or a `commit` (lowercase hex, at least 7 characters). The URL is `https://`, `http://`, `ssh://`, `git://`, `file://` or `user@host:path`. There is no central registry: the URL is the name of the source.
- A git dependency is fetched with the `git` command into a cache, one directory per commit: `$VELA_CACHE`, else `$XDG_CACHE_HOME/vela`, else `~/.cache/vela`. Fetching needs no grant because it happens in the compiler, before the program exists; `vela lsp` and `vela mcp` never fetch, and report a dependency that is not cached yet (E0322).
- `vela.lock`, next to vela.toml, is written on the first build and should be committed. It holds one line per git dependency: name, URL, `tag:NAME` or `commit:HASH` as written in vela.toml, the full commit, and a BLAKE2b hash of the dependency's files:

  ```
  color https://example.com/color.git tag:v1.2.0 8d022128f5f177019f1e6aed46f75c93c90815f1 blake2b:5fea136b...
  ```

  Every build uses the locked commit, even if the tag has moved since, and checks the cached files against the hash (E0323). To update a dependency, change its `tag` or `commit`, or delete its line; vela fetches it and rewrites the line.
- Each dependency must be listed in the program's vela.toml, including dependencies of dependencies: a dependency's own vela.toml is not read.

## 19. Standard library

Written in Vela and built into the compiler; `use` any of them. They take capabilities only as parameters, like any other code.

The standard library is checked like a dependency. `src/std/vela.toml` lists the capabilities each module may take, and every program that uses a module is checked against it (E0320), with or without a vela.toml of its own:

| Module | May take |
| --- | --- |
| `json`, `list`, `text`, `format`, `time`, `regex` | nothing: pure |
| `http` | `net` (Net, Conn) and `listen` (Listener, Conn) |
| `sql` | `fs` (the database file's directory); trusted with C (`[trust] foreign = ["sql"]`) to call SQLite |

A module missing from the list may take nothing. `vela caps FILE` shows what each std function takes. What stays trusted, and why:

- The built-in methods of the capabilities themselves (`Net.get`, `Fs.read`, `Listen.on`, ...), written in OCaml for the interpreter and C for the native runtime. They are where the grants are enforced (host allowlists, the fs root, read-only), so there is nothing below them to check them against; they are covered by the tests of both backends.
- The built-in methods of plain values (`Str`, `List`, `Map`, channels, ...). They take no capability, so they cannot do I/O.
- The std modules' source. It is embedded in the compiler, not fetched, so it is as trustworthy as the compiler binary; the manifest check makes sure that what it can do stays what it declares.
- The C libraries that std modules trusted in the manifest's `[trust]` declare in `foreign` blocks (SQLite for `sql`). Their calls need no `Foreign`, and the declarations are private to the module; the module wraps every call and keeps SQLite inside the `Fs` grant (see `sql` below). A standard library module that is not trusted cannot call C. Trusted modules may also declare the runtime's own helper library, `foreign "vela"`, which no other code may: `vela_copy(p, n)` copies C memory into `Bytes`, and a handle table (`vela_handle_new`, `_acquire`, `_release`, `_close`) keeps C pointers out of Vela values. A trusted module's values hold a random id from that table instead of a `Handle`, since any code can write `Handle { addr: ... }` but no code can turn an id into a pointer; an unknown or closed id is an error, and a pointer is freed only once no use of it is in progress.
- Callbacks. `http.serve(l, n, handler)` calls `handler` inside std code; the handler carries only the capabilities its closure captured from your code, which appear in your signatures, not std's.

### json

| Item | Meaning |
| --- | --- |
| `enum Value { Null, Bool(Bool), Num(Float), Str(Str), Arr(List[Value]), Obj(List[Field]) }` | a JSON value; objects keep their fields in order |
| `struct Field { key: Str, value: Value }`, `field(key, value)` | an object member |
| `parse(s) -> Result[Value, Str]` | the error names what was expected and the byte offset |
| `stringify(v) -> Str` | compact JSON; integral numbers print without `.0` |
| `quote(s) -> Str` | a JSON string literal |
| `v.get(key)`, `v.at(i)` | methods: `Option[Value]`, an object member or an array item |
| `v.as_str()`, `as_num()`, `as_int()`, `as_bool()`, `as_list()` | methods: `Option` of the payload, `None` for another kind |

### http

HTTP/1.1, plain or over TLS. The server keeps connections alive between requests and reads pipelined requests in order; bodies can be streamed in pieces both ways. The client uses TLS for `https://` URLs; the server uses TLS when its listener came from `Listen.on_tls`, and nothing else changes.

| Item | Meaning |
| --- | --- |
| `struct Header { name, value }`, `req.header(name)`, `resp.header(name)`, `find_header(headers, name)` | headers; lookup ignores case |
| `struct Request { method, path, version, headers, body }` | `version` is `"HTTP/1.1"` or `"HTTP/1.0"` |
| `struct Response { status: Int, headers, body }`, `text(status, body)`, `json(status, body)`, `reason(status)` | |
| `get(net, url)`, `post(net, url, content_type, body)`, `request(net, method, url, headers, body)` | `Result[Response, IoError]`; `http://host[:port]/path` and `https://host[:port]/path` URLs, chunked responses decoded, one connection per request |
| `request_fold(net, method, url, headers, body, init, f)` | `Result[(Response, T), IoError]`: the body is passed to `f: fn(T, Str) -> T` piece by piece as it arrives instead of being held in memory; the response comes back with an empty body |
| `serve_forever(listener, handler) -> IoError` | serve every connection in its own task until the listener is closed (by another task holding it) or fails; returns that error |
| `serve(listener, count, handler) -> Int` | the same for `count` connections; returns how many requests were answered |
| `serve_conn(conn, handler) -> Int` | answer requests on one connection until the client closes it or asks to (`Connection: close`, or HTTP/1.0 without keep-alive); 400 and close on a malformed request, 500 if the handler panics, headers only for HEAD |
| `handle(conn, handler) -> Bool` | answer one request, then close |
| `accept_forever(listener, f) -> IoError` | run `f: fn(Conn) -> Int` on each accepted connection in its own task, until accepting fails |
| `serve_stream(conn, handler) -> Int` | like `serve_conn` for a handler `fn(Request, Conn) -> Result[(), IoError]` that writes its own response; an error or panic closes the connection |
| `write_head(conn, status, headers)`, `write_chunk(conn, piece)`, `end_body(conn)` | write a response whose body follows in pieces (`Transfer-Encoding: chunked`) |
| `read_request(conn)`, `write_response(conn, r)` | one message on a connection; `write_response` says the connection will close |

Limits: request heads up to 64 KiB, request bodies up to 8 MiB (read whole, chunked or not), 1000 requests per connection. There are no timeouts yet, so an idle keep-alive connection holds its task until the client closes it, and `serve_forever` keeps a small record per finished connection until it returns.

```
use http

fn hello(r: http.Request) -> http.Response {
    http.text(200, "hello from {r.path}")
}

fn main(root: Root) -> Result[(), IoError] {
    let l = root.listen.on("127.0.0.1:8080")?
    Err(http.serve_forever(l, hello))
}
```

The handler is a plain function value, so it gets no capability unless a closure captures one. A streamed response:

```
fn numbers(r: http.Request, c: Conn) -> Result[(), IoError] {
    http.write_head(c, 200, [http.Header { name: "Content-Type", value: "text/plain" }])?
    for n in [1, 2, 3] {
        http.write_chunk(c, "{n}\n")?
    }
    http.end_body(c)
}

// in main: http.accept_forever(l, |c| http.serve_stream(c, numbers))
```

### list

`sort_by(xs, less)` (stable merge sort, `less(a, b)` says whether `a` goes first), `index_of(xs, x)`, `find_index(xs, pred)`, `range(lo, hi)`, `remove_at(xs, i)`, `insert_at(xs, i, x)`. All return new lists.

### format

Number formatting. Fixed decimals are the Float method `x.to_fixed(d)`; this module adds the rest. Pure functions.

| Item | Meaning |
| --- | --- |
| `thousands(n)` | `"1,234,567"` |
| `grouped(number, sep)` | `sep` between groups of three digits in the integer part of a number already written out: `grouped(x.to_fixed(2), " ")` is `"1 234 567.89"` |
| `zero_pad(n, width)` | zeros after the sign up to `width` characters: `zero_pad(-7, 4)` is `"-007"` |
| `percent(x, decimals)` | `percent(0.4567, 1)` is `"45.7%"` |
| `hex(n)`, `base(n, radix)` | lowercase digits in base 16 or 2 to 36, with `-` for negatives: `hex(255)` is `"ff"`; another radix panics |
| `parse_base(s, radix)` | `Option[Int]`: digits in that base (either case) with an optional sign; `None` on anything else or overflow |
| `align_left(s, w)`, `align_right(s, w)`, `align_center(s, w)` | `s` padded with spaces to `w` characters for table columns; counts code points, not bytes |
| `width(s)` | the number of code points in `s` |

### regex

Regular expressions that never backtrack: a pattern compiles to a program for a Pike VM, which follows every way the pattern can match at once, so a search takes time proportional to the pattern size times the text length, whatever the pattern. `(a+)+$` on a long run of `a`s is as fast as any other pattern. Written in Vela.

| Item | Meaning |
| --- | --- |
| `compile(pattern) -> Result[Regex, Str]`, `must(pattern) -> Regex` | `must` panics on a bad pattern; use it for patterns written in the source |
| `r.matches(s)` | whether the whole of `s` matches |
| `r.find(s)`, `r.find_from(s, start)` | `Option[Match]`, the leftmost match (from byte `start`) |
| `r.find_all(s)` | `List[Match]`, left to right without overlaps; an empty match right after a match is skipped |
| `r.replace(s, template)`, `r.replace_first(s, template)` | `$1` or `${1}` is a group, `${name}` a named group, `$0` the whole match, `$$` a dollar sign |
| `r.replace_with(s, f)` | every match replaced by `f(match)` |
| `r.split(s)` | the pieces between matches; an empty pattern splits into characters |
| `escape(s)` | `s` with metacharacters escaped, to match it literally |
| `struct Match { text, start, end, groups, names }`, `m.capture(i)`, `m.named(name)` | `start` and `end` are byte offsets; `capture(0)` is the whole match; a group that did not take part is `None` |

The syntax is a subset of RE2's: literals; `.` (any character but newline); classes `[abc]`, `[^a-z]`, `[\d_]`; `\d \w \s` and `\D \W \S` (ASCII); `\b \B` word edges; `^ $` (text start and end) and `\A \z`; `* + ? {n} {n,} {n,m}` (counts up to 1000), each lazy with a trailing `?`; `( )` captures, `(?: )` groups without capturing, `(?P<name> )` or `(?<name> )` name a capture; `|`; escapes `\n \t \r \f \v \0 \xHH` and `\` before any punctuation. Flags `(?i)` (ASCII case-insensitive), `(?m)` (`^` and `$` match at lines) and `(?s)` (`.` matches newline) apply from where they appear to the end of the pattern. Text is UTF-8: `.` and negated classes match whole characters, and a non-ASCII character is one unit for a quantifier. Classes may list non-ASCII characters but not ranges of them, and a negated class cannot list them. Backreferences and lookaround are not supported, since they need backtracking. A Vela string needs `\{` for a literal brace, so `\d{4}` is written `"\\d\{4\}"`.

```
use regex

fn main(root: Root) {
    let date = regex.must("(?P<y>\\d\{4\})-(?P<m>\\d\{2\})-(?P<d>\\d\{2\})")
    match date.find("due 2026-12-01") {
        Some(m) => root.stdout.print("{m.named("d") ?? ""}/{m.named("m") ?? ""}"),
        None => root.stdout.print("no date"),
    }
    root.stdout.print(date.replace("2026-12-01", "$3.$2.$1"))
}
```

### time

Calendar dates and times as pure functions of epoch milliseconds, the number `Clock.now_ms()` returns. Everything is UTC on the proleptic Gregorian calendar, without leap seconds. Fixed offsets from UTC (in minutes, `60` for +01:00) are supported where a function says so; named time zones with daylight saving rules are not, as they need the system's zone database.

| Item | Meaning |
| --- | --- |
| `struct DateTime { year, month, day, hour, minute, second, ms, weekday, yday }` | `month` 1-12, `weekday` 1 (Monday) to 7 (Sunday), `yday` 1-366 |
| `parts(ms)`, `parts_at(ms, offset_minutes)` | the fields of a moment, in UTC or at an offset |
| `date(year, month, day)`, `from_parts(p)` | epoch milliseconds; out-of-range months, days and times roll over (`date(2024, 13, 1)` is 2025-01-01) |
| `iso(ms)` | `"2026-11-11T18:22:05.123Z"`; years outside 0-9999 get a sign and six digits |
| `iso_at(ms, offset_minutes)`, `iso_date(ms)` | `"2026-11-11T19:22:05.123+01:00"`, `"2026-11-11"` |
| `parse_iso(s) -> Result[Int, Str]` | a date (midnight UTC) or date and time joined by `T` or a space, with optional seconds and fraction, then `Z`, an offset (`+01:00`, `+0100`, `+01`) or nothing (UTC); the error names the byte |
| `format(ms, layout)`, `format_at(ms, offset_minutes, layout)` | strftime-style: `%Y %m %d %H %M %S`, `%L` milliseconds, `%j` day of year, `%y`, `%I`, `%p`, `%u`, `%a %A %b %B` (English names), `%s` epoch seconds, `%z` (`+0100`), `%Z` (`UTC` or `+01:00`), `%%` |
| `seconds(n)`, `minutes(n)`, `hours(n)`, `days(n)` | durations in milliseconds: `now + time.days(3)` |
| `add_months(ms, n)` | same time of day `n` months later, the day clamped to the month's end |
| `is_leap_year(y)`, `days_in_month(y, m)`, `month_name(m)`, `weekday_name(d)` | |

```
use time

fn main(root: Root) {
    let now = root.clock.now_ms()
    root.stdout.print(time.format(now, "%A %d %B %Y, %H:%M UTC"))
    let due = time.parse_iso("2026-12-01T09:00+01:00") ?? 0
    root.stdout.print("{(due - now) / time.days(1)} days left")
}
```

### text

`repeat(s, n)`, `pad_left(s, width, fill)`, `pad_right(s, width, fill)`, `count(s, sub)`, `words(s)`, and byte tests `is_digit(b)`, `is_alpha(b)`, `is_space(b)` for use with `bytes()`.

### sql

SQL databases through SQLite, built on foreign functions (section 9) and the system's `libsqlite3` (on macOS it is part of the OS; on Linux install the SQLite development package). The module is trusted to call SQLite itself, so a program needs no foreign grant: `--grant fs=DIR` for a database file, and nothing for `sql.memory()`.

```
use sql

fn main(root: Root) -> Result[(), IoError] {
    let db = sql.open(root.fs, "app.db")?          // --grant fs=DIR
    db.exec("CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT NOT NULL, age INTEGER)", [])?
    db.exec("INSERT INTO users (name, age) VALUES (?, ?)", [sql.Text("Ada"), sql.Int(36)])?
    for row in db.query("SELECT id, name, age FROM users WHERE age > ?", [sql.Int(30)])? {
        root.stdout.print("{row.int("id") ?? 0} {row.text("name") ?? ""}")
    }
    db.close()
}
```

- `sql.open(fs, name)` opens or creates the database file `name` under the directory `fs` grants (`fs.full_path`), read-only when `fs` is read-only (the file must then exist). `sql.memory()` is a fresh in-memory database. Both return `Result[Db, IoError]`: `Denied` when the path is outside the grant, `Failed` with SQLite's message otherwise.
- `Db` is a plain value naming the connection, so `db.exec(text, params)` and `db.query(text, params)` take nothing else. It holds a random id in the runtime's handle table, not the connection's pointer, so a `Db` cannot be forged; code that holds one can change that database and nothing else. It can be passed to tasks, which share the one connection safely (SQLite serialises it). `db.close()` closes it once operations running in other tasks finish; after that every operation on it, and a second close, is `Err(Failed("sql: the database is closed"))`. One that is never closed stays open until the program exits.
- Values go in as parameters, never into the SQL text: each `?` takes the next item of `params`, a `List[sql.Value]`, which is `sql.Null`, `sql.Int(n)`, `sql.Float(x)`, `sql.Text(s)` or `sql.Blob(bytes)`. The number of `?`s must match the list.
- `exec` returns the number of rows changed. With no parameters its text may hold several statements separated by `;` (a schema); with parameters it must be exactly one.
- `query` runs one statement and returns every row as a `List[sql.Row]`; `query_one` returns the first, as an `Option`. A row holds `columns` (names) and `values`. `row.get(name)` is the `Value`; `row.int(name)`, `row.float(name)` (an integer reads as a float too), `row.text(name)` and `row.blob(name)` are `Option`s, `None` for NULL; `row.is_null(name)`. Asking for a column the row does not have, or reading one as the wrong type, panics: that is a bug in the query or the code, not a condition to handle.
- `db.transaction(|| { ...; Ok(v) })` runs the closure between `BEGIN` and `COMMIT`. If it returns `Err` or panics, the transaction is rolled back and the error (or the panic) goes on. Transactions do not nest: starting one inside another is an `Err`.
- `db.last_id()` is the rowid of the last row inserted through the connection, as a `Result[Int, IoError]`.
- SQLite stays inside the grants. `ATTACH` is turned off, and with it `VACUUM` (which attaches a temporary database; `VACUUM INTO` would write anywhere), so SQL cannot open other files. The database file is not opened through a symlink, and `open` refuses a database whose `-journal`, `-wal` or `-shm` file is a symlink out of the granted directory (or a broken one), since SQLite follows those. URI filenames and extensions are off (SQLite's defaults, which the module never changes), and temporary tables and sorts stay in memory. What remains is a race no Vela code can cause: another process that can write to the directory swapping in such a symlink while the database is open. SQLite writes its journal next to the database, inside the granted directory. A busy database is waited on for up to 5 seconds; that wait, like every SQLite call, blocks the worker thread.

## 20. Not yet

A package registry and a Cranelift backend. Foreign functions have no callbacks from C, no variadic functions and no structs by value. See the design doc's Known gaps for the full list.
