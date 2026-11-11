# Vela language specification

This document describes Vela as implemented by this repository's compiler: interpreter (`vela run`) and native backend (`vela build`, `vela run --native`). It is meant to be read whole, by people and by models writing Vela. Where the two backends differ, the difference is stated.

## 1. The idea

Vela code can only touch the outside world through **capabilities**: values that grant authority. There is no global `print`, `open`, `http.get` or clock. The program starts with one capability, `root: Root`, and passes narrower ones to the functions that need them. A function's parameter types therefore say everything it can do.

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
    body.split("\"full_name\":").skip(1).map(|c| c.trim().split("\"").get(1) ?? "?")
}
```

## 2. Lexical structure

- **Comments:** `//` to the end of the line. There are no block comments.
- **Identifiers:** `[A-Za-z_][A-Za-z0-9_]*`. By convention types and enum variants start with an uppercase letter, everything else lowercase. Patterns rely on this (section 7).
- **Keywords:** `fn let mut if else match for in while return struct enum true false group break continue`.
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
| `Map[K, V]` | keys to values | Keys are `Int`, `Str`, `Bool`, or tuples, structs and enums made of them (E0113). A value, like `List`; iterates in insertion order. |
| `Option[T]` | `Some(T)`, `None` | From the prelude. |
| `Result[T, E]` | `Ok(T)`, `Err(E)` | From the prelude. |
| `IoError` | `Denied(Str)`, `Failed(Str)` | Error type of every capability operation. |
| `fn(A, B) -> R` | functions and closures | |
| `Chan[T]` | a channel between tasks | Created with `channel(capacity)` (section 10). |
| `Root`, `Net`, `Fs`, `Stdout`, `Clock`, `Listen`, `Listener`, `Conn` | capabilities | Cannot be created by programs (section 9). |
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
expr
```

- Parameters and pattern bindings are immutable. Use `let mut y = y` to get a mutable copy.
- `for` iterates over a snapshot: changing the list inside the loop does not affect the iteration. Over a range, both bounds are evaluated once, before the first iteration.
- `break` and `continue` only work inside loops, not across closures (E0208).
- Only variables, fields, list or array elements and map entries, and paths through them such as `m[k].x` or `xs[i].tags[j]`, can be assigned (E0209).

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

`Root` has eight fields, each a capability, and one method:

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

`root.args()` is the `List[Str]` of command-line arguments after `--`: `vela run app.vela -- a b`.

An ungranted `Net`, `Fs` or `Listen` still exists, but every operation on it returns `Err(Denied(...))`. `Listener` and `Conn` are capabilities too: you get them from `Listen.on`, `Listener.accept` and `Net.connect`.

The rules:

1. Programs cannot create capabilities. They come only from `root`.
2. Attenuation only narrows. `net.only(host)` returns a `Net` limited to that host, and it is empty if you didn't hold that host. `fs.under(dir)` limits to a subdirectory; `fs.read_only()` removes writing.
3. There is no ambient I/O. Using `print`, `open`, `fetch` and similar names is error E0302, which explains the capability to use instead.
4. A function whose parameters contain no capability and no function type is **pure**: it can only compute (and panic). `vela caps FILE` lists each function's authority.
5. Enforcement at runtime:
   - `Net`: http and https only, host allowlist, redirects not followed. `Net.connect` uses the same allowlist.
   - `Listen`: binding is allowed only for granted `host` or `host:port` entries.
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
- A task whose closure returns `()` runs for its effect and adds nothing to the list, so one group can mix them with tasks that return a value: the list holds the results of the value tasks, which must all have one type. A group of only `()` tasks evaluates to an empty list. To drop a value a task would return, end its body with a statement: `g.go(|| { log(x); })`. The rule is decided by the closure's type as written, so in generic code a task returning `T` always adds an item, even when `T` is `()`.
- When the block ends, every task has finished. Nothing outlives the group.
- `g` can only be used as `g.go(...)` directly in the body. It cannot be stored, passed, returned, or used inside a closure (E0310).
- The body cannot leave early with `return` or `?` (E0311), and `break` and `continue` cannot target loops outside the group.
- Tasks share data by value, so there are no data races. A task reaches the world only through capabilities its closure captured. Spawning is not a capability.
- Tasks are lightweight: natively each is a fiber with a small stack, scheduled on one worker thread per core (`VELA_THREADS` overrides this), and a task waiting on a group, a channel or a socket parks without blocking a thread. The interpreter runs tasks as coroutines. Output printed by tasks may interleave differently between the two.
- If every task is waiting and nothing can wake one, the program panics with `all tasks are blocked (deadlock)` instead of hanging.
- A panic in a task ends only that task. The group still waits for the others, then passes the first panic on (section 15).

### Channels

```
let c: Chan[Int] = channel(8)        // capacity at least 1
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

- `channel(n)` creates a `Chan[T]` holding up to `n` values; `T` comes from the annotation or from use. A capacity below 1 panics.
- `c.send(v)` waits while the channel is full; sending on a closed channel panics.
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

- An arm is `name = ch.recv()` (the name gets `Option[T]`; `None` means closed and empty), `ch.send(value)`, or `_`. Like `match`, `select` is an expression and every arm has the same type.
- All channels, values and guards are evaluated first, in order. Then the first ready operation in the order written runs, so list the channel you want served first first. With no ready operation, `_` runs if there is one; otherwise `select` waits until one is ready.
- A closed channel is always ready to receive (`None`), so turn its arm off with a guard: `if cond` after the operation skips the arm when `cond` is false. If every arm is off and there is no `_`, `select` panics instead of waiting forever.
- A send arm that is not chosen sends nothing. A send arm on a closed channel panics, as `send` does.
- There are no timeouts yet.

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
- Use one task at a time to read a given connection (and one to write it).

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
| `Rand` | `int(lo, hi)` | `Int` in `lo` up to but not including `hi`; panics if `hi <= lo` |
| `Rand` | `float()` | `Float` in [0, 1) |
| `Stdin` | `read_line()` | `Option[Str]`, without the newline; `None` at the end of input |
| `Stdin` | `read_all()` | `Str`, the rest of the input |
| `Env` | `get(name)` | `Option[Str]`; `None` if unset or not granted |
| `Net` | `get(url)` | `Result[Str, IoError]`, the response body; a non-2xx status is `Failed` |
| `Net` | `only(host)` | `Net`; `host` is `name`, `name:port` or `*` |
| `Net` | `connect(addr)` | `Result[Conn, IoError]`; `addr` is `host:port` |
| `Listen` | `on(addr)` | `Result[Listener, IoError]` |
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

## 12. Tests

```
fn add(a: Int, b: Int) -> Int { a + b }

test "adds small numbers" {
    assert_eq(add(2, 3), 5)
    assert(add(-1, 1) == 0)
}
```

- `test "name" { ... }` is a top-level item. Tests take no arguments and get no `root`, so they cannot do I/O; they can use groups and channels.
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
| E0113 | a map key type that is not `Int`, `Str`, `Bool`, or a tuple, struct or enum of them |
| E0114 | a struct that contains itself by value |
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
| E0310 | misuse of a group handle |
| E0311 | `return` or `?` leaving a group |
| E0320 | a dependency takes a capability vela.toml does not grant it |
| E0321 | malformed vela.toml |
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
program    = { "use" IDENT | [ "pub" ] ( fn | struct | enum | interface ) | test } ;
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
           | "|" [ cparam { "," cparam } ] "|" expr | "||" expr
           | "group" IDENT block ;
init       = IDENT [ ":" expr ] ;
cparam     = IDENT [ ":" type ] ;
pattern    = "_" | IDENT | [ "-" ] INT | STRING | "true" | "false"
           | IDENT [ "." IDENT ] [ "(" pattern { "," pattern } ")" ]
           | tuplepat ;
tuplepat   = "(" pattern "," pattern { "," pattern } ")" ;
```

## 17. Tools

- `vela fmt FILE` prints the canonical formatting (`--write` rewrites, `--check` verifies).
- `vela lsp` is a language server; `vela mcp` is an MCP server exposing check, run, test, format, caps, type-at and the spec to coding agents.
- `vela caps FILE` lists each function's authority; `vela explain CODE` explains an error.

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

A vela.toml next to the program (or in a parent directory) lists its dependencies, and what each may do:

```
[package]
name = "app"

[dependencies]
json2 = { path = "vendor/json2" }                  # pure: no capabilities
fetch = { path = "../fetch", caps = ["net"] }
```

- A dependency is a directory holding `NAME.vela`, plus any modules that file uses from the same directory.
- `caps` lists what the dependency may use: `net` (Net and Conn), `listen` (Listen, Listener and Conn), `fs`, `stdout`, `stdin`, `clock`, `rand`, `env`. The default is none. No dependency can take `Root`.
- Vela has no ambient authority, so a function can only use a capability that reaches it as a parameter. The checker therefore knows everything a dependency can ever do from its signatures, and rejects the program (E0320) if a dependency's function takes a capability it was not granted. Granting is still only an upper bound: at run time the dependency gets exactly the capabilities you pass it, which can be narrowed with `only` and `under`.
- Only local paths are supported; there is no registry, versioning or lock file yet.

## 19. Standard library

Written in Vela and built into the compiler; `use` any of them. They take capabilities only as parameters, like any other code.

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

HTTP/1.1 over plain TCP (no TLS yet). Each message is read whole; bodies up to 8 MiB.

| Item | Meaning |
| --- | --- |
| `struct Header { name, value }`, `req.header(name)`, `resp.header(name)`, `find_header(headers, name)` | headers; lookup ignores case |
| `struct Request { method, path, headers, body }` | |
| `struct Response { status: Int, headers, body }`, `text(status, body)`, `json(status, body)`, `reason(status)` | |
| `get(net, url)`, `post(net, url, content_type, body)`, `request(net, method, url, headers, body)` | `Result[Response, IoError]`; `http://host[:port]/path` URLs, chunked responses decoded |
| `read_request(conn)`, `write_response(conn, r)` | one message on a connection |
| `handle(conn, handler) -> Bool` | read, answer with `handler` (400 if malformed, 500 if the handler panics), close |
| `serve(listener, count, handler) -> Int` | accept `count` connections, each handled in its own task; returns how many were answered |

```
use http

fn hello(r: http.Request) -> http.Response {
    http.text(200, "hello from {r.path}")
}

fn main(root: Root) -> Result[(), IoError] {
    let l = root.listen.on("127.0.0.1:8080")?
    http.serve(l, 100, hello)
    Ok(())
}
```

The handler is a plain function value, so it gets no capability unless a closure captures one.

### list

`sort_by(xs, less)` (stable merge sort, `less(a, b)` says whether `a` goes first), `index_of(xs, x)`, `find_index(xs, pred)`, `range(lo, hi)`, `remove_at(xs, i)`, `insert_at(xs, i, x)`. All return new lists.

### text

`repeat(s, n)`, `pad_left(s, width, fill)`, `pad_right(s, width, fill)`, `count(s, sub)`, `words(s)`, and byte tests `is_digit(b)`, `is_alpha(b)`, `is_space(b)` for use with `bytes()`.

## 20. Not yet

Unbuffered (capacity 0) channels, timeouts in `select`, TLS, SQL, a package registry, and a Cranelift backend. See the design doc's Known gaps for the full list.
