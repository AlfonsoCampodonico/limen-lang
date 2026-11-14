# Tessera language specification

This document describes Tessera as implemented by this repository's compiler: interpreter (`tessera run`) and native backend (`tessera build`, `tessera run --native`). It is meant to be read whole, by people and by models writing Tessera. Where the two backends differ, the difference is stated.

## 1. The idea

Tessera code can only touch the outside world through **capabilities**: values that grant authority. There is no global `print`, `open`, `http.get` or clock. The program starts with one capability, `root: Root`, and passes narrower ones to the functions that need them. A function's parameter types therefore say everything it can do.

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
- **Doc comments:** the `//` lines directly above an item, a struct field, an enum variant or an interface method, with no blank line in between, document it. The comment block at the very top of a file, followed by a blank line, documents the module. `tessera doc` reads them; there is no special marker.
- **Identifiers:** `[A-Za-z_][A-Za-z0-9_]*`. By convention types and enum variants start with an uppercase letter, everything else lowercase. Patterns rely on this (section 7).
- **Keywords:** `fn let mut if else match for in while return struct enum true false group break continue pub use interface dyn select defer`. `type`, `test`, `bench`, `foreign`, `until`, `interleaved` and `with` are words only where they start their construct, so they stay usable as names: a field can be called `type`.
- **Integers:** decimal digits, `_` allowed as a separator (`1_000_000`). A literal must fit in 64 bits: up to `9223372036854775807` (E0005).
- **Floats:** digits with a fraction (`1.5`), an exponent (`1e9`, `2.5E-3`), or both. `_` is allowed. There is no leading-dot form (`.5`).
- **Strings:** `"..."`, may span lines. Escapes: `\n \t \r \\ \" \{ \} \0`. Any other escape is error E0006.
- **Interpolation:** `{expr}` inside a string evaluates any expression, including ones with their own strings: `"{xs.join(", ")}"`. An empty `{}` is error E0007; write `\{` for a literal brace.
- **Symbols:** `( ) { } [ ] , : ; . | + - * / % ! ? = < > & ^ ~ -> => == != <= >= && || << >> ..< |> ?.`. `?.` is one symbol (safe navigation), so `e?.name` is never `?` followed by `.name`; write `(e?).name` for that.

### Newlines

Statements are separated by a newline or `;`. Two statements on one line without `;` is error E0004. An expression continues onto the next line only in these cases:

- A line starting with `.` (or `?.`) continues a method chain:
  ```
  let n = xs
      .map(|x| x * 2)
      .sum()
  ```
- A line starting with `|>` continues a pipeline (section 5):
  ```
  let line = name
      |> text.pad_right(12, ".")
      |> with_price(price)
  ```
- Any other binary operator continues the expression only at the **end** of a line; one at the start of a line does not. So `a\n-1` is two statements.
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
| `Array[T, N]` | exactly `N` elements | A value type stored inline, like a struct. `N` is an integer literal, or a type parameter that stands for one (section 4). |
| `(A, B)`, `(A, B, C)`, ... | tuples of 2 to 8 values | Written `(1, "a")`, read with `.0`, `.1`, unpacked with patterns. |
| `Map[K, V]` | keys to values | Keys are `Int`, `Str`, `Bytes`, `Bool`, or tuples, structs and enums made of them (E0113). A value, like `List`; iterates in insertion order. |
| `Option[T]` | `Some(T)`, `None` | From the prelude. |
| `Result[T, E]` | `Ok(T)`, `Err(E)` | From the prelude. |
| `IoError` | `Denied(Str)`, `Failed(Str)` | Error type of every capability operation. |
| `HttpResponse` | `{ status: Int, headers: Map[Str, Str], body: Str }` | From the prelude; returned by `Net.get`. |
| `ProcessOutput` | `{ code: Int, stdout: Str, stderr: Str }` | From the prelude; returned by `Exec.run`. |
| `fn(A, B) -> R` | functions and closures | `fn(A, B)` without `-> R` returns `()`, as a function declared without `->` does. |
| `Chan[T]` | a channel between tasks | Created with `channel(capacity)` (section 10). |
| `Handle` | `{ addr: Int }` | From the prelude: an opaque C pointer, for foreign functions (section 9). |
| `FileInfo` | `{ size: Int, modified_ms: Int, is_file: Bool, is_dir: Bool }` | From the prelude; returned by `Fs.info`. |
| `Root`, `Net`, `Fs`, `Stdout`, `Stderr`, `Clock`, `Listen`, `Listener`, `Conn`, `Rand`, `Stdin`, `Env`, `Exec`, `Foreign` | capabilities | Cannot be created by programs (section 9). |
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

A file is a sequence of `use` lines and `fn`, `struct`, `enum`, `interface` and `type` declarations, in any order (put `use` first by convention). Section 18 covers modules.

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
- A type parameter written as an array's size stands for a length, inferred at each call: `fn sum[N](xs: Array[Int, N]) -> Int` takes an array of any length, and `fn first[T, N](xs: Array[T, N]) -> T` any element type too. Inside, `xs.len()` is that length. The function is compiled once per length used, like the rest of generics. A parameter cannot be both a length and a type (E0102), and it is not a value: use `xs.len()`.
- Struct fields and enum payloads cannot hold a capability (E0301), a channel (E0305) or a function type (E0304). That keeps authority visible in signatures. A generic instantiation like `Pair[Net, Int]` is allowed, because the capability is visible in the type.
- Defining the same type, function, field or variant twice is error E0106.
- Types can be recursive through an enum, a `List` or a `Map`, which hold their contents indirectly: `struct Node { value: Int, next: Option[Node] }`, `enum Tree { Leaf, Node(Tree, Tree) }`, `struct Dir { files: List[Dir] }`. A struct stores its fields inline, so one that contains itself through structs, tuples or arrays alone would be infinitely large (E0114). There is no `box`: those types are the indirection.

### Type aliases

`type Name = Type` gives a type another name. It is the same type, not a new one: a value of either is a value of both, and messages show the type it stands for.

```
type Counts = Map[Str, Int]
type Pair[T] = (T, T)
type Handler = fn(Request) -> Response
pub type Parsed[T] = Result[T, Str]

fn swap[T](p: Pair[T]) -> Pair[T] {
    (p.1, p.0)
}
```

- An alias can take type parameters, and must be given exactly that many (E0109). `pub type` exports it; other modules write `shapes.Path`.
- It cannot contain itself, directly or through other aliases (E0116): `type Tree = List[Tree]` would never end; declare an enum for that.
- It cannot reuse the name of another type or item in its module, or of a built-in type (E0106).
- A method declared on an alias is a method on the type it stands for: `fn total(self: Counts)` is a method on `Map[Str, Int]`, private to its file like any method on a built-in type.

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
- Two bounds are built in and have no methods. `Ordered`: the type has `<` (section 5), so `fn lowest[T: Ordered](xs: List[T])` can compare items and call `xs.min()` or `xs.sort()`. `Key`: the type can be a map key (`Int`, `Str`, `Bytes`, `Bool`, and tuples, structs and enums of them), so `fn index[K: Key, V](xs: List[V], key: fn(V) -> K) -> Map[K, V]` can build a map keyed by `K`; keys are ordered too, so `Key` includes `Ordered`. A type that is not ordered or not a key is E0111 at the call. They cannot be `dyn`, and no interface can take their names.
- Code is generated separately for each type used, so a call through an interface costs the same as a direct call.
- An interface is not a type: `fn f(s: Shape)` is E0102; write `fn f[T: Shape](s: T)`. A list holds one type, so `List[T]` cannot mix `Circle` and `Square` values.
- Bounds go on the type parameters of functions, structs and enums (below), not of interfaces or aliases.
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

`tessera caps` reports a function with bounded type parameters like one that takes callbacks: what it can do depends on the type it is called with.

#### Generic interfaces

An interface can take type parameters, used in its methods. A bound names its arguments:

```
interface Container[T] {
    fn get(self, i: Int) -> Option[T]
    fn len(self) -> Int
}

fn count[C: Container[Int]](c: C) -> Int {
    c.len()
}

fn first[C: Container[T], T](c: C) -> Option[T] {
    c.get(0)
}

first(["a", "b"])     // Some("a"): `List[Str]` has `get` and `len`, so T is Str
count([1, 2, 3])      // 3
```

- An argument can be a type parameter of the function, as `T` in `first`. At each call it is inferred from the methods of the type used, here `List[Str]`'s `get`, so `first(xs)` has type `Option[Str]`. Inside, `c.get(0)` has type `Option[T]`.
- A generic interface is always written with its arguments: `[C: Container]` or `Container[Int, Str]` is E0109, with the right form in the note. A type that has the methods with other types, such as a `get` returning `Option[Str]` for `Container[Int]`, is E0111.
- Interfaces from other modules and aliases work as anywhere else: `[C: coll.Container[Int]]`, and `type Ints = Container[Int]` makes `[C: Ints]` the same bound.
- There are no associated types: a type an interface's methods depend on is a parameter of the interface, inferred as above. `fn first[C: Container[T], T](c: C) -> Option[T]` says what an associated `Item` type would, with nothing new to learn.

#### Bounds on structs and enums

A struct's or enum's type parameters can have bounds, written as on functions:

```
struct Sorted[T: Ordered] {
    items: List[T],
}

enum Tree[K: Key, V] {
    Leaf,
    Node(K, V, Tree[K, V], Tree[K, V]),
}

fn insert[T](self: Sorted[T], x: T) -> Sorted[T] {
    let mut items = self.items
    items.push(x)
    Sorted { items: items.sort() }
}
```

- Every place the type gets its arguments is checked against the bounds (E0111): a struct literal, a variant, and every type written with it, such as `let s: Sorted[Foo]`, a parameter, a field or a result type.
- A function whose parameter has the type with a type parameter as an argument gets the bounds for free: in `insert`, `T` is `Ordered` because `self` is a `Sorted[T]`, so `items.sort()` (and `<` on two `T`s) works without writing `[T: Ordered]`. That holds only for parameters; a function that makes a `Sorted[T]` from a plain `T`, or returns one, declares the bound itself, and so does a struct with a `Sorted[T]` field.

### Interface values: `dyn`

`dyn Shape` is a type: a value of any type that satisfies `Shape`, called through the interface's methods. A list of them can mix types:

```
let shapes: List[dyn Shape] = [Circle { r: 1.0 }, Square { s: 2.0 }]
for s in shapes {
    out.print("{s.name()}: {s.area()}")
}
```

- A struct or enum value becomes `dyn Shape` where one is expected: an annotated `let`, a function argument, a list literal of that type, a struct field, a `return`, the last expression of a function returning `dyn Shape`, `push`, or the argument of a constructor whose type is expected, as `Some(x)` where an `Option[dyn Shape]` is expected and `Err(x)` in a function returning `Result[T, dyn Error]`. The expected type reaches into each branch of an `if`, a `match` and a block, so the branches can hold different types. Its type must satisfy the interface (E0111).
- Only struct and enum values can become `dyn`, and only if they hold no capability, function or channel (E0112), so a `dyn` value never hides authority. To work with capabilities through an interface, use a bounded type parameter instead. The one exception is `dyn Error` (below), which `Str` and `IoError` values can become too.
- Only the interface's methods can be called on a `dyn` value. It prints like the value inside. `==` is not allowed.
- To choose between types, say where the `dyn` is expected: `let s: dyn Shape = if big { Circle { r: 1.0 } } else { Square { s: 2.0 } }`. Without an expected type, the branches of an `if` must have the same type.
- A `dyn` value is boxed and its calls go through a table of methods; a bounded type parameter (`[T: Shape]`) has no such cost.
- A generic interface is `dyn` with its arguments: `let bags: List[dyn Container[Str]] = [a, b]`. `dyn Container[Int]` and `dyn Container[Str]` are different types.

### Embedding

A struct can embed another struct by writing its type without a field name. The embedded struct's fields and methods can then be used on the outer struct directly:

```
struct User {
    name: Str,
}

fn greet(self: User) -> Str {
    "hi, {self.name}"
}

struct Admin {
    User,
    level: Int,
}

let a = Admin { User: User { name: "ana" }, level: 3 }
a.name           // "ana", the same as a.User.name
a.greet()        // "hi, ana", the same as a.User.greet()
```

- An embedded struct is an ordinary field named after its type: `User` (and `auth.User` from a module, or `Box[Int]`, are both named by the last part, `User` and `Box`). Reach it explicitly as `a.User`; set it in a literal as `Admin { User: u, level: 3 }`. A literal sets the embedded struct as a whole, never its fields one by one (`Admin { name: "x" }` is E0103). It is stored inline, copied, compared, ordered and printed like any field: `Admin { User: User { name: "ana" }, level: 3 }`.
- Only a struct can be embedded, written with a capital first letter (`User`, `auth.User`, `Box[T]`); an `Int`, an enum or a type parameter is E0121. Two embedded structs with the same name, or an embedded struct and a field with its name, are defined twice (E0106).
- **Lookup.** `a.x` and `a.x()` look at the outer struct's own fields and methods first. Otherwise they look through the embedded structs one depth at a time: first the structs `Admin` embeds, then the ones those embed, and so on, with no limit on depth. The first depth that has `x` wins, so a name nearer the top hides a deeper one. A field access looks for fields and methods; a call looks for methods.
- **Ambiguity.** If two structs at that nearest depth both have `x`, then `a.x` is ambiguous (E0120). That is an error only where `a.x` is used; name the one you mean, as in `a.Left.x`. A struct may embed two structs that share names as long as nothing uses them unqualified.
- **Interfaces.** Promoted methods count toward interfaces, both as bounds (`[T: Greeter]`) and as `dyn Greeter` values, so `Admin` satisfies any interface `User` does, unless it has a method of its own with that name (which must then have the right type), or the name is ambiguous.
- **Composition, not inheritance.** An outer method with the same name as an embedded one (`fn greet(self: Admin)`) hides it on `Admin` values, but does not replace it: `a.User.greet()` still calls `User`'s, and a method of `User` that calls `self.greet()` always calls `User`'s, because its `self` is a `User`. There is no overriding, no `super`, and an `Admin` is not a `User`: pass `a.User` where a `User` is needed.
- Assignment goes through promotion: `a.name = "eva"` changes `a.User.name` (`a` must be `mut`).
- Generic structs can embed and be embedded: `struct Labeled[T] { Box[T], label: Str }` promotes `Box`'s fields and its generic methods. A method written for one instantiation only (`fn f(self: Box[Int])`) is promoted for direct calls, but counts toward interfaces only when the outer struct embeds exactly that type.
- Privacy is unchanged: a promoted method that is private to its module is still private (E0402).
- Under the hood the checker rewrites `a.name` to `a.User.name`, and for interfaces adds a method `Admin::greet` that calls `self.User.greet()`; both backends see only ordinary fields and methods.

### Errors: the `Error` interface

Every program can use this interface without declaring it:

```
interface Error {
    fn message(self) -> Str
    fn cause(self) -> Option[dyn Error]
}
```

- `cause` is optional: a type with a `message` method and no `cause` satisfies `Error`, and its `cause()` is `None`. Define `cause` to point at the error underneath, as in `fn cause(self: ConfigError) -> Option[dyn Error] { Some(self.why) }`.
- `Str` (the error of `catch`, and of the `json`, `time` and `regex` modules) and `IoError` (the error of every capability and of `http`) satisfy it. `IoError`'s message is `denied: <text>` for `Denied(text)` and `<text>` for `Failed(text)`.
- `Result[T, dyn Error]` holds any error. In a function that returns it, `?` takes a `Result[_, E]` for any `E` that satisfies `Error` and converts the error to `dyn Error` (section 5). A function can mix `IoError`, `Str` and its own error types without converting them by hand.
- `main` returning `Result[_, dyn Error]` reports an error by its message: `error: reading config: denied: fs: not granted`.

```
struct ParseError {
    line: Int,
}

fn message(self: ParseError) -> Str {
    "bad line {self.line}"
}

fn load(fs: Fs) -> Result[Config, dyn Error] {
    let text = fs.read("app.toml")?       // IoError
    let cfg = parse(text)?                 // ParseError
    Ok(cfg)
}
```

### main

`main` must be one of:

```
fn main(root: Root)
fn main(root: Root) -> Int
fn main(root: Root) -> Result[(), E]
fn main(root: Root) -> Result[Int, E]
fn main()
```

`root` may be left out of any of them. When `main` returns, the program exits with status 0, or with the `Int` it returned (directly or as `Ok(n)`). The operating system keeps only the low 8 bits, so use 0 to 255: `-> Int { 256 + 7 }` exits with 7. If `main` returns `Err(e)`, the program prints `error: <e>` to stderr and exits with status 1; a `dyn Error` prints its `message()`.

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
| 1 | `\|>` |
| 2 | `\|\|` |
| 3 | `&&` |
| 4 | `==` `!=` |
| 5 | `<` `<=` `>` `>=` |
| 6 | `..<` |
| 7 | `+` `-` `\|` `^` |
| 8 | `*` `/` `%` `&` `<<` `>>` |
| 9 | `??` |
| 10 | unary `-` `!` `~` |
| 11 | call `f(x)`, index `a[i]`, field `p.x`, method `s.len()`, safe navigation `a?.b`, `?` |

### Operators

| Operator | Operands | Result |
| --- | --- | --- |
| `+` | `Int`, `Float`, `Str`, `Bytes`, `List[T]` (concatenation) | same type |
| `-` `*` `/` | `Int`, `Float` | same type |
| `%` | `Int` | `Int` |
| `<` `<=` `>` `>=` | any ordered type (below) | `Bool` |
| `==` `!=` | any type except functions and capabilities; structural equality | `Bool` |
| `&&` `\|\|` | `Bool`, short-circuit | `Bool` |
| `..<` | `Int` | `List[Int]`: `lo..<hi` counts up from `lo`, stopping before `hi` |
| `-x` | `Int`, `Float` | same type |
| `!x` | `Bool` | `Bool` |
| `&` `\|` `^` | `Int`: bitwise and, or, exclusive or | `Int` |
| `<<` `>>` | `Int`: shift left, shift right keeping the sign | `Int` |
| `~x` | `Int`: every bit flipped (`-x - 1`) | `Int` |

The bitwise operators bind as in Go: `|` and `^` with `+`, `&` and the shifts with `*`, so `x & mask == 0` is `(x & mask) == 0` and `a + b << 2` is `a + (b << 2)`. `|` between two expressions is always this operator; a closure's `|x|` and an or-pattern's `A | B` are where an expression cannot be. A shift count of 64 or more shifts every bit out: `x << 64` is `0` and `x >> 64` is `0` or `-1` by the sign of `x`; a negative count panics (`negative shift count`), the same in both backends. `>>` is arithmetic; `x.shift_right_unsigned(n)` shifts in zeros instead (section 11).

Both operands must have the same type. The **ordered** types are `Int`, `Float`, `Str` and `Bytes` (byte order), `Bool` (`false` first), `()`, and lists, arrays, tuples, structs and enums whose items are ordered. Those compare item by item: lists and arrays lexicographically (a prefix comes first), tuples and structs field by field in declaration order, enums by variant in declaration order and then by payload, so `None` sorts after every `Some`. Maps, functions, capabilities, channels, `dyn` values and type parameters have no order (E0201), unless the type parameter has the bound `Ordered` or `Key` (section 4). On `Int`, `/` truncates toward zero, `%` takes the sign of the dividend, and division or remainder by zero panics. On `Float`, comparisons follow IEEE, so NaN is unequal to everything, and division by zero gives infinity. Inside a list, tuple, struct or enum, and in `sort()`, floats use a total order with NaN first, so sorting never misbehaves.

### Literals and constructors

- `[1, 2, 3]` is a `List[Int]`. Where an `Array[T, N]` is expected (an annotated `let`, an argument, a field), a list literal of exactly `N` elements builds the array. A wrong count is error E0201.
- Struct literal: `Pair { first: 1, second: "a" }`. Every field must be set (E0206). `Pair { first, second }` is shorthand for `first: first`. A struct literal cannot appear directly as the condition of `if`, `while`, `for` or `match`; wrap it in parentheses.
- Copy with changes: `{ p with x: 3, y: 4 }` is a new struct value equal to `p` except for the fields named, as in F# and C#'s `with`. The base is any expression of a struct type (E0201 otherwise); each field must exist (E0103), appear once (E0106) and have the field's type, and the shorthand `{ p with name }` works as in a literal. The fields not named are kept, so nothing is "missing". `with` must follow the base on the same line; the fields may go on their own lines. The base is evaluated first, then the new values in order, except that a base that is just a variable is read after them, which only matters if a field expression assigns to that variable. Natively, when the base is not used afterwards (`p = { p with x: p.x + 1 }`, or a parameter returned changed), its storage is reused: the replaced fields are overwritten in place and the others are not touched; otherwise the struct is copied first.
- Enum variants are built by name: `Some(3)`, `None`, `Circle(1.0)`, `Point`. A bare name means a variant of the file's own enums first, then of the prelude (`Some`, `None`, `Ok`, `Err`, `Denied`, `Failed`), then of other modules' enums, so a module's `Num` never clashes with `json.Num`, and a file that defines its own `Some` shadows the prelude's. If two enums at the nearest of those levels define the name, qualify it: `Shape.Point` or `json.Num(1.0)` (E0104). In patterns the type being matched decides, so qualifying is rarely needed there.

### Blocks, if and match

- A block `{ ... }` is an expression; its value is the last expression without `;`.
- `if c { a } else { b }` is an expression, and both branches must have the same type. `if` without `else` has type `()`, so its block must also be `()` (E0203). `else if` chains are allowed, and `else` may start a new line.
- `match` (section 7) is an expression.

### Closures

`|x, y: Int| expr` or `|| expr`. Parameter types are optional when the context gives them (`xs.map(|x| x + 1)`). A closure captures the variables it uses **by value**, when it is created, and it cannot assign to them (E0205). Inside a closure, `return` and `?` leave the closure, not the enclosing function.

**Trailing closures.** A block written right after a call's parentheses, on the same line, is a closure passed as the last argument. Its parameters, if any, come first inside the braces:

```
let big = xs.filter { |x| x > 10 }        // xs.filter(|x| x > 10)
each(rows) { |row|                        // each(rows, |row| { ... })
    out.print(row.name)
}
let r = catch { risky() }                 // catch(|| { risky() })
group g {
    g.go { fetch(net, url) }              // g.go(|| { fetch(net, url) })
}
```

- With no other arguments the parentheses can go: `catch { ... }`, `xs.map { |x| x * 2 }`. After a plain name, only a lowercase one: `Point { x: 1 }` is still a struct literal.
- The closure is always the last argument, after named ones too: `retry(times: 3) { |n| attempt(n) }` (a closure written last after named arguments fills the last parameter).
- **Not in a condition.** In the head of `if`, `while`, `for` and `match` (and a group's `until`), `{` starts the body, as with struct literals, so a trailing closure there is error E0003. Pass the closure in the parentheses, `if xs.filter(|x| x > 1).len() > 0 {`, or wrap the whole condition in parentheses.
- It is the same closure as `|x| { ... }`: same types, same captures. `tessera fmt` keeps whichever form was written (and drops the empty `()` in `f() { ... }`).
- There is one trailing closure per call; `f(a) { ... } { ... }` is not two.

### The `?` operator

On a `Result[T, E]`, `e?` evaluates to the `Ok` value or returns `Err(e)` from the enclosing function, which must return `Result[_, E]` with the same `E`, or `Result[_, dyn Error]` when `E` satisfies `Error` (section 4); then the error converts to `dyn Error`. Any other error type is E0202, which offers to change the function's error type to `dyn Error` when both types satisfy `Error`. Convert errors by hand with `.map_error(|e| ...)`. On an `Option[T]`, `?` returns `None` from a function returning `Option`.

A string after `?` on the same line adds context: `x? "reading config"` returns early with an error whose message is `reading config: <the error's message>` and whose `cause()` is the original error. The function must return `Result[_, dyn Error]` (E0202, with a fix that changes its error type), and the error must satisfy `Error`. The string can interpolate (`"reading {path}"`) and is built only when there is an error. On an `Option`, `opt? "no port set"` returns an error with that message and no cause, so a missing value becomes an error in one step. The wrapper is the prelude struct `WrappedError { context: Str, inner: dyn Error }`; the `errors` module (section 19) walks and prints the chain.

```
fn start(fs: Fs, all: Map[Str, Str]) -> Result[Server, dyn Error] {
    let text = fs.read("app.toml")? "reading config"
    let port = all.get("port")? "no port set"
    let cfg = parse(text)? "parsing config"
    ...
}
```

The context is a string after `?`, rather than a method such as `.context("...")?`, so the common case stays one short step that reads like the comment it replaces, and the string is evaluated only on the error path.

### Safe navigation: `?.`

On an `Option`, `a?.b` reaches into the value if there is one: it is `None` when `a` is `None`, and otherwise `Some` of `.b` on the value inside. It works with fields, methods and tuple fields (`a?.len()`, `t?.0`), and `None` short-circuits **the whole chain** to its right, as in C# and TypeScript: in `user?.name.len()`, `.len()` runs only when there is a user, and the result is an `Option[Int]`.

```
let city = user?.address?.city ?? "unknown"
```

- The result is an `Option` once, never nested: if the chain's own result is already an `Option` (here `address` is an `Option[Address]`, and `xs?.get(0)` returns one), it is the result as is, as in Swift and Kotlin. So `user?.address?.city` is an `Option[Str]`.
- The left side must be an `Option` (E0201 otherwise; a value that is always there takes a plain `.`). On a `Result`, `e?.name` is E0201 too: `?.` is one symbol, so to return early on `Err` and then use the value write `(e?).name`.
- The chain is the postfix operations after `?.`: fields, method calls, calls, indexes and further `?.`. It ends at a binary operator, so `??` (which binds tighter than arithmetic) applies to the whole chain, and it ends at a postfix `?`, which applies to the whole chain too: `find(id)?.address?.city?` returns `None` from the function when any step is missing.
- `?.` can start a line, like `.`. It cannot be assigned to (E0209).

### Pipes

`x |> f(a, b)` is `f(x, a, b)`: the value on the left goes in as the **first** argument of the call on the right, as in F# and Elixir. A pipeline reads in the order things happen:

```
let report = read_orders(text)
    |> list.sort_by(|a, b| a.total > b.total)
    |> top(10)
    |> summarize
```

- The right side is a call `f(a)`, a method or module call `r.m(a)` or `list.sort_by(less)` (the value goes in after the receiver: `x |> r.m(a)` is `r.m(x, a)`), a bare function or closure `f`, `list.sum`, `(|n| n * 2)`, which is called with the value alone, or one of those followed by `?`: `text |> parse()?` is `parse(text)?`. Anything else is error E0003.
- `|>` is the loosest operator, below `||`, and left-associative: `a + b |> f` is `f(a + b)`, and `x |> f |> g` is `g(f(x))`. To use a pipeline's result in a larger expression, put it in parentheses or a `let`: `(xs |> total) + 1`, `(user |> find) ?? guest`.
- A line may start with `|>`, so a pipeline can be written one step per line, like a method chain.
- A pipe is exactly the call it stands for, so it type-checks and evaluates as that call: `x |> r.m(a)` evaluates `r`, then `x`, then `a`.

### Calls

`f(a, b)` calls a function or closure. The number of arguments must match (E0107). `panic(msg: Str)` aborts the program with a message and source location; it can be used where any type is expected.

Evaluation order is left to right: the callee (or a method's receiver), then the arguments in the order they are written. For assignment, the right-hand side is evaluated before the target's indexes.

**Named arguments.** An argument can name the parameter it is for, which reads better than a row of bare numbers and booleans:

```
fn connect(host: Str, port: Int, retries: Int) -> Conn { ... }

connect(host, retries: 3, port: 443)
server.describe(prefix: "to", upper: true)
```

- Positional arguments come first, then named ones in any order. A named argument is `name: expr`, with the parameter's name as declared.
- Every parameter is passed exactly once. Labels only say which is which: Tessera has no default values, so leaving one out is still E0107.
- They work for functions and methods declared with `fn`, generic ones included, for methods called through an interface bound or a `dyn` value (the names are the interface's), and for functions of other modules. They do not work where the callee has no parameter names: closures and other function values (a type like `fn(Int) -> Int` has none), enum variants, built-in methods and functions, and foreign functions. A label there is E0122, with a fix that removes it.
- An unknown name, a parameter passed twice, and a positional argument after a named one are E0122 (except a closure written last, which fills the last parameter: that is how a trailing closure combines with labels). When the right name is clear (one unknown label and one parameter left over, or a positional argument where one parameter is missing) the error carries a fix.
- The arguments run in the order they are written, not in the parameter order, so `connect(retries: next(), port: other())` calls `next` first. Renaming a parameter changes the labels callers can use: it is part of the function's interface, like its types.

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
(x, p) = step(p)?         // assign a tuple's items to existing variables
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

### Tuple assignment

`(a, b) = value` assigns the items of a tuple to places that already exist, the way `let (a, b) = value` makes new names:

```
let mut x = 0
let mut p = start
(x, p) = parse_number(p)?     // value and new position, both kept
(a, b) = (b, a)               // swap
((lo, hi), _) = bounds()      // nested; `_` ignores an item
(best.score, scores[i]) = (s, s * 2)
```

- The right side is evaluated first, completely, then each target is assigned from left to right. A target's own index or path is evaluated when that target is assigned, so in `(i, xs[i]) = (1, 5)` the element written is `xs[1]`.
- A target is anything an assignment can have on its left (section 6 above: a `mut` variable, a field, a list element, a map entry, a path through them), `_`, or a nested tuple of targets. Each one is checked against its item's type as a plain assignment is: E0205 when it is not `mut`, E0201 when the type differs.
- The right side must be a tuple with one item per target (E0211).

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
| `[]`, `[p1, p2]` | a list of exactly that length, matching each item |
| `[p1, ..]`, `[.., pn]`, `[p1, ..rest]` | a list at least that long; `..rest` binds the items it skips, as a list |
| `p1 \| p2` | either pattern: an or-pattern |
| `Point { x, y: 0 }`, `Point { x, .. }` | a struct, matching the fields it names; `geo.Point { ... }` for another module's |
| `p as name` | what `p` matches, binding the whole value to `name` as well |

```
match shape {
    Circle(r) => 3.14159 * r * r,
    Rect(w, h) => w * h,
    Point => 0.0,
}
```

**Or-patterns.** `p1 | p2 | ...` matches when any alternative does, tried from left to right. It can stand for a whole arm or for a part of a pattern: `Keyword("fn") | Keyword("let") => ...`, `Some(1 | 2)`, `(Red | Green, n)`. Every alternative must bind the same names with the same types, so the arm's body can use them whichever one matched (E0210): `Circle(r) | Ring(r, _) => r`.

**The rest: `..`.** In a variant or tuple pattern, `..` stands for all the items not written out: `SLet(name, ..)` matches an `SLet` whatever its other fields, and `(first, ..)` any tuple, taking its first item. It may come anywhere among the items, at most once per pattern: `(.., last)`, `Rect(w, .., h)` are fine. The type being matched says how many items it stands for, possibly none; writing more items than the value has is E0107. In a variant or tuple `..` cannot be named.

**List patterns.** `[p1, p2]` matches a list of exactly two items. With a `..` it matches any list at least as long as the items written: `[first, ..]` a list with at least one item, `[.., last]` likewise from the end, `[first, .., last]` one with at least two (for one item, `first` and `last` would be the same item, so that list does not match). In a list, the rest can be named: `[first, ..rest]` binds `rest` to a new `List` of the items after `first`, `[..init, last]` to those before `last`. One `..` per list.

```
match tokens {
    [] => "empty",
    [only] => "just {only}",
    [first, ..rest] => "{first}, then {rest.len()} more",
}
```

**Struct patterns.** `Point { x: 0, y }` matches a `Point` whose `x` is `0`, binding its `y`. Each field is `name: pattern`, or `name` alone for `name: name`, which binds the field to a variable of its own name. Every field must be named (E0206), unless the pattern ends with `..`, which matches any value of the fields left out: `Point { x: 0, .. }`. `..` comes last, once. A field named twice is E0106, one the struct does not have E0103 (only the struct's own fields: one promoted from an embedded struct is matched through the embedded field, `Admin { User: User { name, .. }, .. }`), and an enum's or an unknown name E0102. They nest like the other patterns, `Line { from: Point { x, .. }, to }`, and a struct of another module is named as in a type, `geo.Point { x, .. }`. A struct has one shape, so for exhaustiveness its fields are checked like a tuple's items: `Point { x: 0, .. }` and `Point { x: _, .. }` together cover every `Point`.

**As-patterns.** `p as name` matches what `p` matches and binds the whole value to `name` too, next to the names `p` binds: `Some(Point { x, .. } as p) => p.y + x`, `[first, ..] as all => all.len()`. `as` takes in everything before it: `A | B as x` is `(A | B) as x`, binding `x` whichever alternative matched; to name one alternative, use parentheses, `(A as x) | B`, and then every alternative must bind `x` (E0210). `as _` is E0003; leave it out. The name is written after the pattern, as in OCaml, F# and Python, rather than with a sign before it.

`let`, `for` and table tests unpack only names, `_` and tuples of those (`let (a, b) = pair`); the other patterns need a `match`. `let [a, b] = xs` says so (E0003): a list can have any length, so it is taken apart with `match`.

An arm can have a guard, `if cond` after the pattern, as in a `select`: the arm applies only when its pattern matches **and** the guard is true, otherwise matching goes on with the next arm. The guard sees the pattern's bindings and must be a `Bool` (E0201). Guards run in order, only until an arm applies.

```
match order {
    Some(o) if o.total > 1000 => "large",
    Some(o) if o.paid => "paid",
    Some(_) => "open",
    None => "none",
}
```

A guarded arm counts for nothing when checking that a `match` is exhaustive, since its guard may be false: above, `Some(_)` is still needed.

Arms are separated by `,` or a newline. An arm's body is an expression, so a statement such as an assignment needs a block: `None => { more = false },`. Patterns nest: `Err(Failed(msg))`, `Some(Some(n))`. A `match` must be exhaustive (E0204); the error comes with an auto-fix that adds the missing arms. Matching on `Int` or `Str` needs a `_` or binding arm. Tuples are checked column by column, so `(true, _)` and `(false, _)` together are exhaustive; an or-pattern covers what its alternatives cover; lists are checked by length, so `[]` and `[_, ..]` together are exhaustive, and so are `[]`, `[x]` and `[x, y, ..rest]`; a struct pattern is checked field by field, and `p as name` covers what `p` covers.

An arm that can never match, because the unguarded arms before it already match everything it would, is reported with a warning (W0002), and so is an alternative of an or-pattern that the arms and alternatives before it cover: in `A | B => 1, B => 2` the second arm never runs.

## 8. Value semantics

Every value behaves as if it were copied on assignment, on passing and on capture. No two variables ever observe each other's mutations:

```
let mut a = [1]
let b = a
a.push(2)      // a is [1, 2], b is still [1]
```

The native backend implements this with reference counting and copy-on-write. A value is copied only if it is shared at the moment it is mutated. A value's last use moves it rather than retaining it, so in-place updates are the normal case.

## 9. Capabilities

`Root` has eleven fields, each a capability, and one method:

| Field | Type | Granted |
| --- | --- | --- |
| `root.stdout` | `Stdout` | always |
| `root.stderr` | `Stderr` | always |
| `root.stdin` | `Stdin` | always |
| `root.clock` | `Clock` | always |
| `root.rand` | `Rand` | always; `--seed N` makes the numbers repeatable, the same in both backends |
| `root.env` | `Env` | only with `--grant env=NAME` (repeatable) or `--grant env=*`; other variables read as `None` |
| `root.net` | `Net` | only with `--grant net=HOST[:PORT]` (repeatable) or `--grant net=*` |
| `root.fs` | `Fs` | only with `--grant fs=DIR` or `--grant fs=DIR:ro` |
| `root.listen` | `Listen` | only with `--grant listen=HOST[:PORT]` (repeatable) |
| `root.exec` | `Exec` | only with `--grant exec=PROGRAM` (repeatable) or `--grant exec=*` |
| `root.foreign` | `Foreign` | only with `--grant foreign=LIB` (repeatable) or `--grant foreign=*`: calls into C libraries |

`root.args()` is the `List[Str]` of command-line arguments after `--`: `tessera run app.tsr -- a b`.

Grants can also be written once in the `[grants]` table of the program's tessera.toml (section 18), so they are not retyped every run. `tessera run` applies them first and the `--grant` flags add to them; a `--grant fs=...` flag replaces the fs grant, since there is only one. A binary from `tessera build` reads only its flags.

An ungranted `Net`, `Fs`, `Listen` or `Exec` still exists, but every operation on it returns `Err(Denied(...))`. An ungranted `Foreign` exists too; calling a foreign function through it panics (`foreign.allows(lib)` checks first). `Listener` and `Conn` are capabilities too: you get them from `Listen.on`, `Listener.accept` and `Net.connect`.

The rules:

1. Programs cannot create capabilities. They come only from `root`.
2. Attenuation only narrows. `net.only(host)` returns a `Net` limited to that host, and it is empty if you didn't hold that host. `fs.under(dir)` limits to a subdirectory; `fs.read_only()` removes writing. `exec.only(program)` limits `Exec` to one program; `foreign.only(lib)` limits `Foreign` to one library.
3. There is no ambient I/O. Using `print`, `open`, `fetch` and similar names is error E0302, which explains the capability to use instead.
4. A function whose parameters contain no capability and no function type is **pure**: it can only compute (and panic). `tessera caps FILE` lists each function's authority. Section 14 says what the compiler does with pure calls. It never runs them in parallel on its own: concurrency stays visible and cancellable in a `group`, where you write it.
5. Enforcement at runtime:
   - `Net`: http and https URLs only, host allowlist, redirects not followed. `Net.connect` and `Net.connect_tls` use the same allowlist. TLS certificates are always verified (section 10).
   - `Exec`: the program name must match a grant exactly (`git` and `/usr/bin/git` are different grants), or the grant is `*`. Arguments are passed as a list, never through a shell. The child gets `/dev/null` as stdin, the working directory, `PATH`, and only the environment variables granted with `--grant env=`. A program name without `/` is looked up in `PATH`, as a shell would. Granting a program grants everything that program can do with any arguments, so `exec=sh` is as good as every other grant.
   - `Listen`: binding is allowed only for granted `host` or `host:port` entries, with or without TLS.
   - `Fs`: paths are relative to the granted directory; absolute paths, `..` escapes and symlinks pointing outside are refused. That holds for both ends of a `rename`, and a read-only `Fs` refuses it, as it refuses every write.
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

Calling one takes a `Foreign` capability first, then the other arguments: `sqlite3_libversion(root.foreign)`. Each call checks that the `Foreign` value grants the library (`--grant foreign=sqlite3`), and panics if not. So C is reachable only from code that was handed `Foreign`, `tessera caps` lists such functions, and a package dependency can use a foreign block only if the program names it in `[trust] foreign = [...]` in tessera.toml (section 18). The library name is the `-l` name: `foreign "sqlite3"` links `-lsqlite3` natively and loads `libsqlite3.dylib` (macOS) or `libsqlite3.so` in the interpreter; `foreign "c"` is the C library itself, which is always there. The function name is the C symbol, unless the declaration names another after its signature: `fn print_int(format: Str, ..., n: Int) -> Int32 = "printf"`. Declarations are one per line; `pub fn` exports one from a module.

The types that cross the boundary:

| Tessera | C | Notes |
| --- | --- | --- |
| `Int` | `int64_t` | |
| `Int32` | `int32_t` (C `int`) | an `Int` in Tessera; an argument outside the 32-bit range panics |
| `Float` | `double` | |
| `Bool` | `int` | 0 is `false`; any other value is `true` |
| `Str` | `const char *` | the string's bytes followed by a NUL; pass the length yourself if C wants it (`s.len()`) |
| `Bytes` | `const void *` | the bytes; pass the length yourself |
| `Handle` | `void *` | an opaque pointer, `Handle { addr: Int }` in Tessera; `addr == 0` is NULL |
| `Out[T]` | `T *` | an out-parameter of `Int`, `Int32`, `Float` or `Handle`: C writes it, Tessera returns it |
| a C struct | `struct { ... }` | declared in the foreign block, passed and returned by value (below) |
| `fn(A, B) -> R` | a pointer to a function | a callback: a top-level Tessera function C calls during the call (below) |

The result is one of the same types (not `Bytes`: C gives no length) or nothing. A `Str` result is copied from a NUL-terminated C string at once (NULL gives `""`); C keeps ownership of the original. `Out` parameters are not passed by the caller: they come back after the result, so `sqlite3_open_v2` above has the Tessera type `fn(Foreign, Str, Int, Int) -> (Int, Handle)`, called as `let (rc, db) = sqlite3_open_v2(f, path, flags, 0)`. A function with no result and one `Out` returns that value alone.

**Variadic functions.** C cannot tell what a variadic function was passed, so a declaration fixes it: the parameters before `...` are the C function's own, and the ones after it are the variadic arguments this declaration passes. A C function called with several shapes gets one declaration per shape, each with its own Tessera name:

```
foreign "c" {
    fn snprintf(buf: Handle, size: Int, format: Str, ..., n: Int32, x: Float) -> Int32
    fn format_int(buf: Handle, size: Int, format: Str, ..., n: Int) -> Int32 = "snprintf"
}
```

C's default promotions apply to the arguments after `...`: `Int32` and `Bool` go as `int`, `Float` as `double`, `Int` as `int64_t` (`%lld`, not `%d`). Before `...` come 1 to 6 integer-like parameters (no `Float` or struct), after it at most 8 of `Int`, `Int32`, `Float`, `Bool`, `Str`, `Bytes` and `Handle`; a variadic function does not return a struct.

**Structs by value.** A `struct` in a foreign block is a C struct, with fields of `Int`, `Int32`, `Float`, `Bool` and `Handle` (C's `int64_t`, `int32_t`, `double`, `int` and `void *`) laid out as C lays them out, 1 to 8 of them. To Tessera it is an ordinary struct of the module, with `Int32` fields as `Int`s: built with `Div { quot: 1, rem: 0 }`, printed, compared and passed around like any other, and exported with `pub struct`. A foreign function takes and returns it by value:

```
foreign "c" {
    struct Div { quot: Int32, rem: Int32 }
    fn div(a: Int32, b: Int32) -> Div
}
```

Passing one checks each `Int32` field's range, as for an `Int32` argument. Only structs declared in a foreign block cross to C; a struct holds no `Str`, `Bytes` or other struct.

**Callbacks.** A parameter of a function type, made of boundary types, is a pointer to a function that C calls:

```
foreign "c" {
    fn qsort(base: Handle, n: Int, size: Int, compare: fn(Handle, Handle) -> Int32)
}

fn by_byte(f: Foreign, a: Handle, b: Handle) -> Int {
    f.copy(a, 1)[0] - f.copy(b, 1)[0]
}

qsort(f, buf, n, 1, by_byte)
```

- The argument is a top-level function, named (E0332): C calls it with its arguments alone, so it has nowhere to keep a closure's captured values. A callback may take a `Foreign` first, before what C passes: it gets the Foreign value the foreign call was given, so it can read C memory and call C. It has no other capability.
- Its parameters are `Int`, `Int32`, `Float`, `Bool`, `Str` and `Handle` (at most 8 integer-like and 8 `Float`); a `Str` is copied from the NUL-terminated C string. It returns one of those but `Str`, or `()`; an `Int32` result out of range is a panic.
- C may call it only during the foreign call that was given it, and on the same thread. If C keeps the pointer and calls it after the call returns, or from another thread, the behaviour is undefined (both backends stop the program where they can tell).
- Nothing unwinds through C's frames. A panic in a callback ends the program with `panic: in a callback from C: ...`, past every `catch` and task boundary, and so does waiting in one (on a group, a channel, a timer or input), which would leave C's frames on a task that moves.
- A callback can make foreign calls with callbacks of their own. The interpreter has room for 32 callbacks in use at once (each foreign call takes its own until it returns); more is a panic there.

Ownership at the boundary:

- Arguments are lent to C for the duration of the call, never moved. A `Str` or `Bytes` pointer is valid only until the call returns: C must copy what it keeps (SQLite's `SQLITE_TRANSIENT`, a destructor argument of `-1`, does this). A `Handle` that C returns pointing into a Tessera string is dangling once that string is freed.
- Memory C allocates belongs to C: free it with the library's own function (`sqlite3_finalize`, `free`). Tessera never frees a `Handle`, and the leak check does not see C memory.
- `foreign.copy(p, n)` copies `n` bytes at `p` into new `Bytes`, for results that are a pointer and a length (`sqlite3_column_blob` and `sqlite3_column_bytes`). It reads raw memory, so it needs a `Foreign` that grants at least one library.

The foreign grant is full trust, and Tessera treats it that way:

- Only the program grants it: `--grant foreign=LIB` or `[grants] foreign` in the program's own tessera.toml. `tessera run` prints a one-line warning whenever it is given, since nothing in Tessera bounds what C code does; only the operating-system sandbox (below) still does.
- It does not pass along: a dependency's `caps` cannot include `foreign`. A dependency may declare foreign blocks only when the program names it in `[trust]`, and tessera.lock records the C libraries each trusted dependency uses (section 18).
- `tessera check` lists every part of the program that calls C, one line each: the program's own files, trusted dependencies, and trusted standard library modules.
- The standard library calls C only from modules that its manifest, src/std/tessera.toml, trusts in the same way (`sql`). Their foreign functions are private to the module and take no `Foreign`, so a program using `sql` grants only an `Fs`; the module is what keeps SQLite inside that grant.

What the capability lets code do: anything the C functions it declares can do, with the program's full authority, outside every other capability's checks. Tessera checks the declared types and the library grant; it cannot check that a declaration matches the C prototype, and a wrong one is undefined behaviour. Keep foreign blocks small, private to a module that wraps them in a safe Tessera API (as the `sql` module does), and grant `foreign` only to programs and dependencies you trust.

Limits, the same in both backends: at most 8 integer-like (everything but `Float`, `Out` and callbacks included) and 8 `Float` values, each field of a struct by value counting as one; the variadic and callback limits above; structs of 1 to 8 scalar fields, not nested. A foreign function can only be called, not used as a value (E0331): wrap it in a closure. A C call blocks its worker thread until it returns, like a busy loop, and C libraries decide their own thread safety.

The backends differ in how they reach the code, not in what a program sees: natively each function is called through its exact prototype (a variadic one through its variadic prototype, a struct through its C declaration, a callback through a generated C function that calls the compiled Tessera function) and linked with `-l<lib>` (a library outside the C compiler's default paths is found through `LIBRARY_PATH`); the interpreter loads the library with `dlopen` and calls through a generic stub that lays out the arguments as the platform's calling convention does (x86-64 System V, arm64, and Apple's arm64 variant: registers, stack, how a struct is split or passed by pointer), and C calls callbacks through a fixed set of trampolines, resolving the declarations of granted libraries before `main` starts. A missing library or symbol is therefore an error before the program runs in both: a link error natively, `foreign: cannot load library ...` or `foreign: library ... has no function ...` in the interpreter. One difference follows from linking: a native binary loads every library it declares when it starts (running the library's initialisers, as any linked library does), granted or not, while the interpreter never loads a library the grants do not cover. In both, a call into an ungranted library panics before it reaches the library.

### The operating-system sandbox

Tessera's checks cover Tessera code. To cover everything else in the process (C called through `foreign`, and any bug in the runtime), every program also runs in an operating-system sandbox built from its grants. It is on by default, in both backends: `tessera run` enters it once the program is loaded (and its granted C libraries with it), and a native binary enters it at startup, before any thread starts. `--no-sandbox` (on `tessera run`, or on a built binary) turns it off; it is for debugging, not for production.

What the kernel enforces:

| | Allowed |
| --- | --- |
| Writing files | under the `Fs` directory, unless it is read-only; the temporary directory if `Exec` is granted (Exec keeps a child's stderr there); `/dev/null` and the terminal |
| Reading files | under the `Fs` directory; system locations (`/usr`, `/System`, `/Library`, `/etc`, `/dev`, Homebrew's prefix, ...: libraries, time zones, TLS certificates); the temporary directory with `Exec`. Not home directories, other users' files or anything else |
| Network | nothing unless `Net` or `Listen` is granted; then any host |
| Starting programs | only if `Exec` is granted. A started program inherits the sandbox: it can read system locations and the `Fs` directory, and list (not read) the directories above the current one, which shells need |

What stays in Tessera's own checks, not the kernel's: which hosts and ports `Net` and `Listen` allow (the kernel allows all networking once either is granted), which programs `Exec` may run, which variables `Env` shows, `Fs`'s refusal of `..` and absolute paths (the kernel sees only the resolved file), and everything inside the process: C code with the foreign grant can still read and corrupt the program's own memory, and through it every capability the program holds.

How:

- macOS: a sandbox profile (`sandbox_init`), allowing by default and denying the above. It also refuses Apple events and LaunchServices opens, so the program cannot have another application act for it. Mach services stay reachable, as for every process.
- Linux: Landlock (kernel 5.13 or later) for files, and on kernels with Landlock ABI 4 or later for TCP too, plus a seccomp filter that, without `Net` or `Listen`, allows only Unix sockets and refuses io_uring, and without `Exec` refuses `execve`; it also refuses x32 system calls. It has been run on arm64 Linux (kernel 7.0); on x86-64 it compiles but has not been run on a real kernel.
- A system without a sandbox (another OS, a Linux kernel without Landlock or seccomp filters, an emulator) runs the program without one, silently, as before. A sandbox that exists but cannot be entered stops the program with a message naming `--no-sandbox`.
- `tessera test` runs native tests in the sandbox too (a test that takes `fs: Fs` gets its temporary directory); interpreted tests run inside the `tessera` process, unsandboxed.

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
- Tasks are lightweight: natively each is a fiber with a small stack, scheduled on one worker thread per core (`TESSERA_THREADS` overrides this), and a task waiting on a group, a channel, a socket, stdin or `clock.sleep(ms)` parks without blocking a thread. The interpreter runs tasks as coroutines. What tasks print comes out in the same order in both (below).
- If every task is waiting and nothing can wake one (no socket and no timer can), the program panics with `all tasks are blocked (deadlock)` instead of hanging.
- A panic in a task ends only that task. The group still waits for the others, then passes the first panic on (section 15).

### Output order

What a group's tasks print does not depend on how they are scheduled: the group's output reads as if its body ran first and then each task, one after another, in the order they were started.

```
let _ = group g {
    for (name, ms) in [("slow", 50), ("fast", 0)] {
        g.go(|| {
            clock.sleep(ms)
            out.print("{name} done")
        })
    }
    out.print("started")
}
// started
// slow done
// fast done
```

- The body prints directly, like code outside any group. A task's output is held until the body has ended and every task started before it has finished; then what it holds is written, and from then on it prints directly. So the first unfinished task streams while the later ones wait their turn.
- Groups nest: a group inside a task is ordered within that task's output.
- A task stopped by a panic or by its group's deadline still has its output written in its turn, before the group passes the panic on. A program that ends with an uncaught panic or a deadlock first writes everything still held, innermost groups first, then the message on stderr. Nothing printed is lost.
- `group g interleaved { body }` (with a deadline, `group g interleaved until clock.after(ms) { body }`) turns this off: its tasks print directly, as the body does, so their output interleaves in the order it happens. Use it for tasks that run until stopped or log as they go (a server's connections, watchers), where waiting for earlier tasks would hold their output back for as long as those run. The `http` module's servers use it.
- Held output is limited to 4 MiB for the whole program. A print that would go past it makes its group give up order: what the group holds is written at once, in start order, and its tasks print directly from then on, as if it were `interleaved`. Reading `Stdin` does the same for every group holding the reading task's output, so a prompt shows before the program waits for the answer.
- In every case each `print` and `write` reaches stdout whole, never mixed with another task's. Stderr, where panics and `error:` go, and where `root.stderr` writes, is never held: a line written there comes out at once, after whatever stdout output has been written out so far.

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
- `clock.after(ms)` is ready once `ms` milliseconds have passed since the `select` started (after its operands were evaluated); `ms <= 0` is ready at once. It takes its place in the order like any arm, so a channel arm written before it still wins when both are ready. A `select` may have several timeouts: the earliest is ready first, and of two equal ones the first written wins. A timeout cannot go together with `_`, which never waits (E0003). `clock` must be the `Clock` capability itself; `after` exists only as a `select` arm.

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
- Waiting needs the `Clock` capability like reading the time does, so a pure function cannot depend on timing. For deterministic tests, ask for `clock: virtual Clock` (section 12): sleeps, `select` timeouts and group deadlines then run on virtual time, which jumps ahead whenever every task is waiting, so timeout logic is tested instantly and gives the same result every run. A fake through an interface (`interface Sleeper { fn sleep(self, ms: Int) }`) works too; `Clock` satisfies such interfaces (section 4).

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
- The deadline stops the body too, the same way: if the body is waiting (on a channel, a `select`, `clock.sleep`, a socket or an inner group) it stops there, its `defer`s run, groups it opened are cancelled and waited for, and the tasks it did not get to start are not in the list. The group then waits for its tasks and returns as usual, so the code after it runs normally. Only the group's own deadline is caught there: when the task running the body is cancelled, or an outer group's deadline passes, the body stops and the cancellation goes on outwards. A panic in the body is passed on as before.

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
- An address is `name:port`, `ipv4:port` or `[ipv6]:port`, and grants are written the same way (`--grant net=[::1]`). Listening on `[::]` accepts IPv6 and IPv4. `connect` tries every address a name resolves to, IPv6 and IPv4, in the order the system's resolver gives, until one connects; the error is the last one's. Natively a name lookup runs on a thread of its own, so a cancelled task stops waiting for it.
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

TLS is OpenSSL (1.1.1 or later), in both backends: the interpreter is linked with it, and native programs that use `Net.get`, `connect_tls` or `on_tls` are linked with it. It is found when tessera is built: `$TESSERA_OPENSSL` (an install prefix) if set, else `pkg-config openssl` (looking in Homebrew's `openssl@3` first), else the system headers. On macOS that is Homebrew's `openssl@3` (`brew install openssl@3`), whose trusted certificates are Homebrew's `cert.pem` bundle; on Linux it is the distribution's libssl (`libssl-dev` or `openssl-devel` to build), trusting the distribution's certificate store. Native programs link OpenSSL's shared libraries, so a binary from `tessera build` that uses TLS needs them where it runs. If OpenSSL is not found, tessera still builds and runs everything else, and every TLS operation fails with `Failed("tls: ... built without TLS ...")`.

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

- `Net.get(url)` sends one HTTP/1.1 `GET` (with `Host`, `User-Agent: tessera`, `Accept: */*` and `Connection: close`), reads until the server closes, and returns `HttpResponse { status, headers, body }`.
- Every status is `Ok`, including 404 and 500; redirects are returned, not followed. `Err(Failed(...))` means the server could not be reached or the response was not valid HTTP; responses over 64 MiB, with more than 64 KiB of headers or with more than 1000 header lines are refused.
- `headers` is a `Map[Str, Str]` with lowercased names, in the order they arrived; a repeated header's values are joined with `", "`. The body is decoded from chunked transfer encoding or cut to `Content-Length`.
- `https://` URLs go over `Net.connect_tls` (port 443 unless given), with the certificate checks described under TLS. URLs may name an IPv6 address: `http://[::1]:8080/`. A server that never answers keeps `get` waiting; give it a deadline with `time.within(clock, ms, || net.get(url))`. For other methods, request headers or bodies, use `http.request` from the `http` module, which works over `Net.connect` and `Net.connect_tls`.

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
| `last_index_of(s)` | `Option[Int]`, byte offset of the last occurrence (`"a/b/c".last_index_of("/")` is `Some(3)`); an empty `s` is found at the end, `Some(len())` |
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
| `Int` | `to_str()`, `to_float()`, `abs()`, `min(n)`, `max(n)`, `to_char()` (the UTF-8 text of a code point, `65.to_char()` is `"A"`; invalid code points give U+FFFD), `shift_right_unsigned(n)` (`>>` shifting in zeros instead of the sign: `(-1).shift_right_unsigned(60)` is `15`; a count of 64 or more gives `0`, a negative one panics) |
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
| `join(sep)` | `Str`; `List[Str]` only (for other items, `xs.map(\|x\| "{x}").join(sep)`) |
| `sum()` | `Int`; `List[Int]` only |
| `any(p)`, `all(p)` | `Bool`: `p` holds for some item, for every item (`true` on an empty list) |
| `count(p)` | `Int`, the items for which `p` holds |
| `find(p)` | `Option[T]`, the first item for which `p` holds |
| `sum_by(f)` | `Int`, the sum of `f(x)` with `f: fn(T) -> Int` |
| `min()`, `max()` | `Option[T]`, `None` when empty; `T` must be ordered |
| `min_by(key)`, `max_by(key)` | `Option[T]`, the first item with the smallest or largest `key(x)`; the key must be ordered |
| `flat_map(f)` | `List[U]`, the lists `f(x)` joined, with `f: fn(T) -> List[U]` |
| `take_while(p)`, `skip_while(p)` | `List[T]`, the items before the first one for which `p` fails, and the rest |
| `with_index()` | `List[(Int, T)]`, each item with its position: `for p in xs.with_index() { ... p.0 ... p.1 }` |
| `zip(ys)` | `List[(T, U)]`, pairs up to the shorter list's length |
| `distinct()`, `distinct_by(key)` | `List[T]`, the first item of each value (or key), in order; the value or key must be a map key type |
| `sort_by(key)` | `List[T]`, stable, in the order of `key(x)`, which is computed once per item; the key must be ordered |
| `partition(p)` | `(List[T], List[T])`, the items for which `p` holds and the others, each in order |
| `chunks(n)` | `List[List[T]]`, consecutive pieces of `n` items, the last one shorter; `[]` when `n` is below 1 |
| `windows(n)` | `List[List[T]]`, every run of `n` neighbors, in order: `[1, 2, 3].windows(2)` is `[[1, 2], [2, 3]]`; `[]` when `n` is below 1 or above the length |
| `chunk_by(key)` | `List[List[T]]`, runs of neighbors with equal `key(x)`: `[1, 1, 2, 1].chunk_by(\|x\| x)` is `[[1, 1], [2], [1]]` |
| `group_by(key)` | `Map[K, List[T]]`, the items by `key(x)`, keys in order of first appearance; the key must be a map key type |
| `count_by(key)` | `Map[K, Int]`, how many items have each key |
| `tally()` | `Map[T, Int]`, how many times each item occurs: `"hello".chars().tally()` is `{"h": 1, "e": 1, "l": 2, "o": 1}` |

`xs + ys` is a new list: the items of `xs`, then those of `ys`. Neither operand changes, as with strings.

None of these panic, except when a function passed to them does. The methods from `any` on take functions and are written in Tessera; they work the same as the others (a file cannot define a method with one of their names, E0106), and natively they compile like your own code, so a call through a closure literal costs a function call per item. A pipeline of `map` and `filter` before one of them still runs as one loop.

`xs[i]` panics when `i` is out of range; `xs.get(i)` returns `None` instead. Writes go through index and field paths: `groups[i].words.push(w)` and `items[i].qty = 3` both work on a `mut` list. `sort()` uses the order of `<`, so it sorts tuples by their first item, then the second, and structs by their fields in declaration order; `sort_by(key)` sorts by a key; for another order and for inserting or removing at a position, use the `list` module (`list.sort_by(xs, less)`, `insert_at`, `remove_at`, section 19).

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
| `map_values(f)` | `Map[K, U]`, the same keys with `f(v)`, with `f: fn(V) -> U` |
| `filter(p)` | `Map[K, V]`, the entries for which `p(k, v)` holds, with `p: fn(K, V) -> Bool` |
| `merge(other)` | `Map[K, V]`, this map's entries, then `other`'s: a key in both takes `other`'s value and keeps its place here |

For a missing key with a default, write `m.get(k) ?? default`.

### Array[T, N]

`len()` (always `N`), `get(i)`, `contains(x)`, `to_list()`, indexing with `a[i]`, and `for x in a`.

### Option[T] and Result[T, E]

`x ?? default` is the value inside `Some` or `Ok`, or `default` otherwise; `default` is evaluated only when it is needed. `??` binds tighter than arithmetic, so `counts.get(w) ?? 0 + 1` adds 1 to the count. There is no `unwrap_or`; using it is error E0105 with a fix that rewrites it. `and_then` is called `flat_map`, as on lists; `and_then` is E0105 with a fix that renames it.

| Method | Result |
| --- | --- |
| `is_some()`, `is_none()` / `is_ok()`, `is_err()` | `Bool` |
| `map(f)` | `Option[U]` / `Result[U, E]`: the value inside passed through `f`; `None` or an `Err` unchanged |
| `flat_map(f)` | the same, with `f` itself returning `Option[U]` / `Result[U, E]`: `parse(s).flat_map(check)` keeps the first error |
| `map_error(f)` (Result) | `Result[T, F]`: the error passed through `f`, an `Ok` unchanged |

`map_error` was called `map_err`. The old name still works, with warning W0001 and a fix that renames it; it will be removed.

`Str` and `IoError` also have `message()`, from the `Error` interface (section 4): the string itself, and the text of the `IoError`.

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
| `keep(x)` | `()`; `x` counts as used, so the work that made it is not optimized away (for benchmarks, section 12) |
| `embed("path")` | `Str`: the file's text, read when the program is compiled (below) |
| `embed_bytes("path")` | `Bytes`: the file's contents, read when the program is compiled |
| `embed_dir("path")` | `Map[Str, Bytes]`: every file under a directory, by its path inside it (`"css/site.css"`), read when the program is compiled |

**Embedding files.** `embed`, `embed_bytes` and `embed_dir` read files at compile time and put its contents into the program as a literal, so a program that serves its own page or assets needs no `Fs`:

```
fn main(root: Root) {
    let page = embed("dist/index.html")
    let logo = embed_bytes("logo.png")
    let assets = embed_dir("public")       // assets["css/site.css"]
    ...
}
```

- The path must be a string literal, relative to the file that names it.
- It must stay inside that file's package: the directory of the program's tessera.toml (or of the main file when there is none), or a dependency's own directory. An absolute path, a `..` that leaves the package or a symlink that points outside it is error E0405. Without this rule a dependency could put `../../.ssh/id_rsa` into the binary, a read that no grant allows.
- `embed` requires UTF-8 text (E0405 otherwise); `embed_bytes` takes any file. The standard library embeds nothing.
- `embed_dir` takes every file under the directory and its subdirectories, keyed by its path inside it with `/` between parts, in sorted order. Names starting with `.` (`.git`, `.env`, `.DS_Store`) are left out, at any depth. Every file and subdirectory, links included, must stay inside the package (E0405), and a directory with nothing to embed is E0405 too. Values are `Bytes`; `to_str()` costs nothing for text.
- The text is taken as it is: braces in the file do not start interpolation.
- Changing an embedded file changes the program, and the build cache sees it, because the contents are part of the generated C. `tessera caps` lists every embedded file with its size after the functions (`"embeds"` in `--json`).
- A function or variable named `embed` in scope is called as usual.

### Capabilities

| Capability | Method | Result |
| --- | --- | --- |
| `Stdout` | `print(s)` | `()`, writes `s` and a newline |
| `Stdout` | `write(s)` | `()`, writes `s` |
| `Stderr` | `print(s)`, `write(s)` | `()`, as `Stdout` does, to standard error: at once, never held in a group's order (section 10), after what stdout has buffered so far |
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
| `Fs` | `rename(from, to)` | `Result[(), IoError]`: moves a file or directory in one step, replacing what is at `to` (a file, or an empty directory); never the granted directory itself |
| `Fs` | `info(path)` | `Result[FileInfo, IoError]`, following links: `size` in bytes, `modified_ms` (milliseconds since the Unix epoch), `is_file`, `is_dir` |
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
- A test can ask the runner for two real capabilities, by declaring them like parameters: `fs: Fs` is a fresh, empty, writable temporary directory, removed when the test ends, and `clock: Clock` is the real clock. `clock: virtual Clock` asks for a clock on virtual time instead (below). Each at most once; anything else is E0306.

```
test "saves the config" (fs: Fs) {
    assert(save(fs, "app.conf", 3).is_ok())
    assert_eq(fs.read("app.conf"), Ok("3"))
}
```

### Virtual time

```
test "gives up after five seconds" (clock: virtual Clock) {
    let replies: Chan[Str] = channel(1)
    let got = group g {
        g.go(|| {
            clock.sleep(60000)                // a reply that comes a minute later
            replies.send("late")
        })
        g.go(|| {
            select {
                r = replies.recv() => r ?? "closed",
                clock.after(5000) => "timed out",
            }
        })
    }
    assert_eq(got, ["timed out"])
}
```

- The clock is still a `Clock`, so the test passes it to the code under test as usual. `virtual` changes only where its time comes from, and is written only there, in a test's parameters. A `bench` is timed on the real clock and cannot take one (E0306).
- The clock starts at 2026-01-01T00:00:00Z: `clock.now_ms()` is `1767225600000` when the test starts, every run.
- It does not move while any task of the test can go on. When every task is waiting (on `clock.sleep`, a `clock.after` arm, a group's deadline, a channel or a group), the clock jumps to the earliest timer and that timer rings: its sleep returns, its `select` arm is chosen or its group's deadline passes. Then the woken tasks run until every task is waiting again, and the next timer rings. So a test that sleeps for an hour finishes at once, and `now_ms` afterwards is exactly an hour later.
- Timers ring one at a time, in a fixed order: the earliest first, and of two set for the same instant, the one set first (a `sleep(5000)` started before a `select` with `clock.after(5000)` returns first, so a message it then sends wins). The next timer, even one due at the same instant, rings only once every task is waiting again. Natively, the tasks of such a test take turns on one thread in the order they become ready, as in the interpreter, so the same test does the same thing every run, in both.
- Work does not take time: while a task computes, writes files or calls C, the clock stands still. The same holds for a task waiting on real I/O (a socket, stdin): no timer rings until it has its data, since only the outside world can wake it. A test cannot take `Net`, `Listen` or `Stdin`, so in practice its tasks only ever wait on each other and on timers.
- When every task is waiting and no timer is set, nothing can ever wake one: the test fails with `all tasks are blocked (deadlock)`, as a program does.
- Only `tessera test` (with or without `--native`) runs code on virtual time, and only in tests that ask for it; `tessera run` and `tessera build` always use the real clock, as do tests that take `clock: Clock`.
- `tessera test FILE` runs every test, each isolated, and prints `ok` (with the time it took) or `FAIL` with the panic message and location; it exits with status 1 if any test fails. `--native` runs them as native code.
- `--run TEXT` runs only the tests (and benchmarks) whose name contains `TEXT`. `--json` prints one JSON object instead, for tools and agents: `{"tests": [{"name", "ok", "message"?, "where"?, "seconds"}], "benches": [...], "passed", "failed", "seconds"}`; a test's own output is left out of it.
- Test names must be unique (E0106). Tests are type-checked by `tessera check` and ignored by `tessera run` and `tessera build`.

### Table tests

One test body can run over a list of cases, written out in place after `for`:

```
test "add" for (a, b, want) in [(1, 2, 3), (2, 2, 4), (-1, 1, 0)] {
    assert_eq(add(a, b), want)
}
```

- Each case is a test of its own: it runs isolated, and it is named by the test's name and the case's text, as in `add (2, 2, 4)`. A failure names the case, and `--run "add (2"` picks cases by their text.
- The pattern after `for` unpacks each case like `let` does: a name, `_`, or a tuple of those, nested as needed (`for (name, (input, want)) in [...]`).
- The cases must be a list literal with at least one case (E0003), so every case is known, and named, before anything runs. To share cases between tests, write the list twice or loop inside one test.
- Capabilities go before `for`: `test "save" (fs: Fs) for name in ["a.txt", "b.txt"] { ... }`. Each case gets its own fresh directory.
- The body is type-checked for every case. A problem shows once; when only some cases have it, a note names the case. Two cases with the same text are E0106.

### Benchmarks

```
bench "sum to 100" {
    let mut t = 0
    for i in 0..<100 {
        t = t + i
    }
    t
}
```

- `bench "name" { ... }` is a top-level item like `test`, and can take `fs: Fs` and `clock: Clock` (never `virtual`) the same way (one directory for the whole run). Its body is one operation.
- `tessera test FILE` skips benchmarks. `tessera test FILE --bench` runs the tests and then every benchmark, always as native code: each benchmark runs in its own process, first once, then with more and more iterations (at most 100 times more per round), until one round takes at least the time budget, one second by default. `--bench-time 200ms` (or `2s`, `1m`) sets the budget, and `--bench-time 100x` runs exactly 100 iterations.
- It reports the iterations of the last round, the time per operation, and the runtime's allocations per operation (strings, lists, maps, closures and other objects made, whether or not they were freed). A panic fails the benchmark like a test.
- The value of the body's last expression is passed to `keep`, so the C compiler cannot drop the work that made it. Inside a loop, call `keep(x)` on values that are otherwise unused. A constant result (`1 + 2`) is still computed at compile time, as in any program, and a pure call whose arguments do not change in a loop is made once (section 14): vary the arguments to time each call.
- Benchmark names must be unique among benchmarks (E0106); a test and a benchmark may share a name.

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

Other uses of purity change nothing a program can observe. A pure call (to a pure function, or a built-in method such as `split` or `to_upper`) gives the same result for the same arguments, so:

- **In a loop**, a pure call whose arguments do not change in the loop is made the first time it is reached, and later iterations reuse its result. A loop that runs zero times never makes the call, a call under an `if` runs only once the condition holds, and a call that panics or never returns does so at the same point as before.
- **In a block**, a second call with the same arguments, where nothing they name was reassigned or changed in between, reuses the first call's result.

The interpreter reuses a pure call's result in a loop when the same function is called again with the same values. Pure calls on constants are not evaluated at compile time, beyond what the C compiler does itself, and results are never remembered across calls: write a cache yourself, as a `Map` you pass along, where one is worth it.

## 15. Errors

Every diagnostic has a stable code. `tessera explain CODE` prints its explanation, and `tessera check --json` emits code, span, notes and an auto-fix when there is one. Codes starting with W are warnings: they are printed, but the program still builds and runs.

| Code | Meaning |
| --- | --- |
| W0001 | a renamed built-in method (`map_err`, now `map_error`) |
| W0002 | a `match` arm, or an alternative of an or-pattern, that can never match |
| E0001-E0008 | lexing and parsing: bad character, unclosed string, unexpected token, missing newline, integer too large, bad escape, empty interpolation, untyped parameter |
| E0101 | unknown name (with a suggestion) |
| E0102 | unknown type, or a struct pattern naming something that is not a struct |
| E0103 | unknown field |
| E0104 | unknown or ambiguous variant |
| E0105 | unknown method |
| E0106 | defined twice, a type named like a built-in type or capability, or a field set or matched twice |
| E0107 | wrong number of arguments |
| E0108 | not callable |
| E0109 | wrong number of type arguments |
| E0110 | misplaced `self`, a method on a type that cannot have them, or a `pub` method on a built-in type |
| E0111 | a type does not satisfy an interface |
| E0112 | a value that cannot become `dyn` |
| E0113 | a map key type that is not `Int`, `Str`, `Bytes`, `Bool`, or a tuple, struct or enum of them |
| E0114 | a struct that contains itself by value |
| E0115 | `return`, `?`, `break` or `continue` that would leave a `defer` body |
| E0120 | an ambiguous name promoted from two embedded structs at the same depth |
| E0121 | embedding something that is not a struct |
| E0122 | a named argument that names no parameter, names one twice, comes before a positional argument, or is given to a callee without parameter names |
| E0201 | type mismatch |
| E0202 | misuse of `?` |
| E0203 | `if` without `else` produces a value |
| E0204 | non-exhaustive `match` |
| E0205 | assignment to something immutable |
| E0206 | missing struct field, in a literal or in a struct pattern without `..` |
| E0207 | type unknown here |
| E0208 | `break`/`continue` outside a loop |
| E0209 | invalid assignment target |
| E0210 | the alternatives of an or-pattern bind different names, or a name with different types |
| E0211 | a tuple assignment whose right side is not a tuple of as many items |
| E0301 | capability stored in a field |
| E0302 | ambient I/O name such as `print` |
| E0303 | bad `main` signature |
| E0304 | function stored in a field |
| E0305 | channel stored in a field |
| E0306 | a test parameter that is not `fs: Fs`, `clock: Clock` or `clock: virtual Clock`, or a bench that takes `virtual Clock` |
| E0310 | misuse of a group handle |
| E0311 | `return` or `?` leaving a group |
| E0320 | a dependency takes a capability tessera.toml does not grant it |
| E0321 | malformed tessera.toml |
| E0322 | a git dependency could not be fetched |
| E0323 | tessera.lock does not match: a malformed line, or a dependency whose files changed |
| E0324 | a trusted dependency calls a C library that tessera.lock did not record |
| E0330 | a foreign function signature with a type that cannot cross to C, too many parameters, or a bad library name |
| E0331 | a foreign function used as a value instead of called |
| E0401 | module not found, or a qualified name without `use` |
| E0402 | item is not `pub` |
| E0403 | module has no such item |
| E0404 | module name used as a value |

A **panic** (division by zero, a negative shift count, index out of range, a missing map key, `panic(...)`, no matching arm) stops the code that hit it:

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

EBNF. `NL` is a newline, as described in section 2. `UPPER` is an `IDENT` that starts with a capital letter. A character-level version for constrained decoding, with the newline rules spelled out, is in [grammar/tessera.gbnf](grammar/tessera.gbnf). `{ x }` means zero or more, and `[ x ]` means optional.

```
program    = { "use" IDENT | [ "pub" ] ( fn | struct | enum | interface ) | test | bench | foreign } ;
foreign    = "foreign" STRING "{" { [ "pub" ] "fn" IDENT "(" [ params ] ")" [ "->" type ] ( NL | ";" ) } "}" ;
interface  = "interface" IDENT [ generics ] "{" { "fn" IDENT "(" "self" { "," param } ")" [ "->" type ] ( "," | NL ) } "}" ;
test       = "test" STRING [ "(" [ tparams ] ")" ] [ "for" ( IDENT | tuplepat ) "in" "[" expr { "," expr } [ "," ] "]" ] block ;
bench      = "bench" STRING [ "(" [ params ] ")" ] block ;
fn         = "fn" IDENT [ bgenerics ] "(" [ params ] ")" [ "->" type ] block ;
bgenerics  = "[" IDENT [ ":" bound { "+" bound } ] { "," IDENT [ ":" bound { "+" bound } ] } "]" ;
bound      = IDENT [ "." IDENT ] [ "[" typearg { "," typearg } "]" ] ;   (* an interface, with its arguments *)
generics   = "[" IDENT { "," IDENT } "]" ;
params     = param { sep param } [ "," ] ;
param      = IDENT ":" type ;
tparams    = tparam { sep tparam } [ "," ] ;
tparam     = param | IDENT ":" "virtual" "Clock" ;             (* a clock on virtual time *)
struct     = "struct" IDENT [ bgenerics ] "{" [ field { sep field } [ "," ] ] "}" ;
field      = IDENT ":" type
           | [ IDENT "." ] UPPER [ "[" typearg { "," typearg } "]" ] ;   (* an embedded struct *)
enum       = "enum" IDENT [ bgenerics ] "{" [ variant { sep variant } [ "," ] ] "}" ;
variant    = IDENT [ "(" type { "," type } ")" ] ;
type       = "dyn" bound                          (* an interface value *)
           | "(" type "," type { "," type } ")"     (* a tuple *)
           | IDENT [ "." IDENT ] [ "[" typearg { "," typearg } "]" ]
           | "(" ")"
           | "fn" "(" [ type { "," type } ] ")" [ "->" type ] ;   (* no "->": returns () *)
typearg    = type | INT ;                       (* INT only as Array's size, which can also be a type parameter *)
sep        = "," | NL ;

block      = "{" { stmt ( NL | ";" ) } [ expr ] "}" ;
stmt       = "let" [ "mut" ] IDENT [ ":" type ] "=" expr
           | "let" tuplepat "=" expr
           | "return" [ expr ]
           | "while" expr_ns block
           | "for" ( IDENT | tuplepat ) "in" expr_ns block
           | "break" | "continue"
           | "defer" ( block | stmt )             (* not a `let` *)
           | expr [ "=" expr ] ;                 (* a tuple of targets on the left: `(a, b) = e` *)

expr       = unary { BINOP unary | "|>" unary } ; (* precedence: section 5; a line may start with "|>" *)
expr_ns    = expr ;                             (* no struct literal at the top level *)
unary      = ( "-" | "!" | "~" ) unary | postfix ;
postfix    = primary { "(" [ args ] ")" [ trailing ] | trailing | "[" expr "]" | ( "." | "?." ) IDENT [ "(" [ args ] ")" ] [ trailing ] | ( "." | "?." ) INT | "?" [ STRING ] } ;
trailing   = "{" [ "|" [ cparam { "," cparam } ] "|" ] { stmt ( NL | ";" ) } [ expr ] "}" ;
                                                (* same line; not in expr_ns; bare only after a lowercase name or method *)
args       = arg { sep arg } [ "," ] ;
arg        = [ IDENT ":" ] expr ;                 (* a named argument *)
primary    = INT | FLOAT | STRING | "true" | "false"
           | [ IDENT "." ] IDENT [ "{" [ init { sep init } [ "," ] ] "}" ]   (* struct literal *)
           | "(" ")" | "(" expr ")" | "(" expr "," expr { "," expr } ")"   (* tuple *)
           | "[" [ expr { sep expr } [ "," ] ] "]"
           | "{" expr "with" init { sep init } [ "," ] "}"            (* copy with changes *)
           | block
           | "if" expr_ns block [ "else" ( block | if ) ]
           | "match" expr_ns "{" { pattern [ "if" expr ] "=>" expr ( "," | NL ) } "}"
           | "select" "{" { selarm ( "," | NL ) } "}"
           | "|" [ cparam { "," cparam } ] "|" expr | "||" expr
           | "group" IDENT [ "interleaved" ] [ "until" expr ] block ;   (* until clock.after(ms) *)
selarm     = ( IDENT "=" expr | expr | "_" ) [ "if" expr ] "=>" expr ;
                                                (* ch.recv(), ch.send(v) or clock.after(ms) *)
init       = IDENT [ ":" expr ] ;
cparam     = IDENT [ ":" type ] ;
pattern    = alt { "|" alt } [ "as" IDENT ] ;  (* an or-pattern, named; not in `let`, `for` or a table test *)
alt        = "_" | IDENT | [ "-" ] INT | STRING | "true" | "false"
           | IDENT [ "." IDENT ] [ "(" items ")" ]
           | [ IDENT "." ] UPPER "{" [ fpat { sep fpat } [ sep ".." ] [ "," ] | ".." [ "," ] ] "}"   (* a struct *)
           | "(" items ")"                       (* a tuple, or one pattern in parentheses *)
           | "[" [ items ] "]" ;                (* a list *)
items      = item { sep item } [ "," ] ;        (* at most one ".." *)
item       = pattern | ".." [ IDENT ] ;         (* `..name` only in a list *)
fpat       = IDENT [ ":" pattern ] ;            (* `x` alone is `x: x` *)
tuplepat   = "(" upat "," upat { "," upat } ")" ;   (* unpacking in `let`, `for` and table tests *)
upat       = "_" | IDENT | tuplepat ;
```

## 17. Tools

- `tessera fmt FILE` prints the canonical formatting (`--write` rewrites, `--check` verifies).
- `tessera lsp` is a language server (diagnostics, hover, definition, references, rename, completion, quick fixes, outline); `tessera mcp` is an MCP server exposing check, run, test, format, caps, type-at, references, rename and the spec to coding agents. Rename is refused when it is unsafe: the renamed program must check with no new errors, and the new name must refer to exactly the renamed places.
- `tessera caps FILE` lists each function's authority; `tessera explain CODE` explains an error.
- `tessera search`, `tessera add` and `tessera index entry` use the package index (section 18).
- `tessera doc FILE|MODULE... [--std] [--all] [--html] [-o OUT]` prints documentation built from doc comments (section 2) and signatures, as Markdown, or with `--html` as one static page. An argument that is not a file names a standard library module; `--std` documents all of them. It lists `pub` items (everything with `--all`); methods appear under their type, and methods on a type the module does not declare (such as `Str`) get their own section. Files are parsed, not type-checked.

```tessera
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
use json                  // json.tsr next to this file, a dependency, or the standard library

pub fn load(text: Str) -> Result[json.Value, Str] {
    json.parse(text)
}
```

- `use NAME` makes module `NAME` available in this file. It looks for `NAME.tsr` next to the file, then a dependency named `NAME` in tessera.toml, then the standard library. A file never imports itself.
- Refer to a module's items as `NAME.item`: functions `json.parse(s)`, types `json.Value`, struct literals `json.Field { key, value }`, variants `json.Num(1.0)` and patterns `json.Num(x)`. A function can be used as a value: `let f = json.parse`.
- Only items marked `pub` (`pub fn`, `pub struct`, `pub enum`) can be used from another module (E0402). Struct fields and enum variants of a `pub` type are all visible.
- Inside a module, its own items are used unqualified. A local variable shadows a module name.
- Variants are matched by name, so `match v { Num(x) => ... }` works without the module prefix when the type is known.
- `test` blocks in a used module are not run; `tessera test` runs the tests of the file you give it.
- Modules can use each other, including in cycles. Type names print with their module: `json.Value`.

### Packages and capabilities

A tessera.toml next to the program (or in a parent directory) lists the program's own grants, its dependencies, and what each may do:

```
[package]
name = "app"

[grants]                                           # what `tessera run` grants the program
fs = "data:ro"
net = ["api.github.com"]
env = ["HOME", "LANG"]

[dependencies]
json2 = { path = "vendor/json2" }                  # pure: no capabilities
fetch = { path = "../fetch", caps = ["net"] }
color = { git = "https://example.com/color.git", tag = "v1.2.0" }
yaml = { git = "git@example.com:me/yaml.git", commit = "3f9c2e1d7a" }
```

- `[grants]` takes `net`, `fs`, `listen`, `env`, `exec` and `foreign`, each a string or a list of strings written exactly like the text after `--grant KIND=`. `fs` takes one directory, relative to tessera.toml, with an optional `:ro`. Stdout, Stderr, Stdin, Clock and Rand are always granted, so listing them is an error. Flags on the command line add to these grants.
- Grants belong to the program being run: only the tessera.toml found for the main file is read for them. A dependency's own tessera.toml never grants anything, so adding or updating a dependency cannot widen what the program may do. A dependency gets capabilities in exactly one way: your code passes them to it, and only of the kinds its `caps` allow. `[grants]` decides what reaches `main`; `caps` decides which of those may cross into each dependency.
- A dependency is a directory holding `NAME.tsr`, plus any modules that file uses from the same directory.
- `caps` lists what the dependency may use: `net` (Net and Conn), `listen` (Listen, Listener and Conn), `fs`, `stdout`, `stderr`, `stdin`, `clock`, `rand`, `env`, `exec`. The default is none. No dependency can take `Root`, and `caps` cannot give `foreign` (E0321).
- `[trust] foreign = ["name", ...]` names the dependencies that may call C: declare foreign blocks and take `Foreign`. Calling C is full trust, since C code is outside every capability check, so it is a separate, explicit list rather than one more cap, and only the program's tessera.toml can hold it. An untrusted dependency that calls C is E0320. tessera.lock gets a line per trusted dependency that calls C, `foreign NAME LIB,LIB`, written on the first build; when an update makes the dependency declare a library the line does not have, the build stops (E0324) until you review it and delete the line.
- Tessera has no ambient authority, so a function can only use a capability that reaches it as a parameter. The checker therefore knows everything a dependency can ever do from its signatures, and rejects the program (E0320) if a dependency's function takes a capability it was not granted. Granting is still only an upper bound: at run time the dependency gets exactly the capabilities you pass it, which can be narrowed with `only` and `under`.
- A dependency is a local directory (`path`) or a git repository (`git`) at a `tag` or a `commit` (lowercase hex, at least 7 characters). The URL is `https://`, `http://`, `ssh://`, `git://`, `file://` or `user@host:path`. No registry holds the code: the URL is the name of the source, and the package index (below) only points at URLs.
- A git dependency is fetched with the `git` command into a cache, one directory per commit: `$TESSERA_CACHE`, else `$XDG_CACHE_HOME/tessera`, else `~/.cache/tessera`. Fetching needs no grant because it happens in the compiler, before the program exists; `tessera lsp` and `tessera mcp` never fetch, and report a dependency that is not cached yet (E0322).
- `tessera.lock`, next to tessera.toml, is written by the commands that produce something, `tessera build`, `tessera run` and `tessera test` (and `tessera add`), the first time they meet a new git dependency or a new trusted one that calls C, and should be committed. `tessera check`, `tessera caps` and the editor and agent servers read it and never write it: a check of a project whose lock is missing a line passes or fails as the build would, and leaves the file as it is. It holds one line per git dependency: name, URL, `tag:NAME` or `commit:HASH` as written in tessera.toml, the full commit, and a BLAKE2b hash of the dependency's files:

  ```
  color https://example.com/color.git tag:v1.2.0 8d022128f5f177019f1e6aed46f75c93c90815f1 blake2b:5fea136b...
  ```

  Every build uses the locked commit, even if the tag has moved since, and checks the cached files against the hash (E0323). To update a dependency, change its `tag` or `commit`, or delete its line; tessera fetches it and rewrites the line.
- Each dependency must be listed in the program's tessera.toml, including dependencies of dependencies: a dependency's own tessera.toml is not read.

### The package index

A package index is a list of packages to search and add, not a place that stores them: each entry names a git repository and a tag, with the capabilities the package needs. There is no service and no account. The index is a git repository (or a local directory) of `.toml` files, one `[[package]]` table per version:

```
[[package]]
name = "color"
git = "https://example.com/color.git"
tag = "v1.2.0"
commit = "8d022128f5f177019f1e6aed46f75c93c90815f1"
description = "Colors for terminal output"
caps = ["stdout"]
```

- `commit` is the commit the tag pointed at when the entry was written. `caps` are written as in tessera.toml, plus `foreign` for a package that calls C. `description` and `caps` may be left out (no description, no capabilities).
- `TESSERA_INDEX` names the index: a git URL (cloned each time it is used), a directory (every `.toml` file at its top) or one file. It has no default; without it, the commands below say how to set it. An index that cannot be reached is an error, never an empty result.
- `tessera search [WORDS...]` lists the packages whose name or description contains every word (ignoring case), each with its description, its latest version and its capabilities. Versions compare as numbers: a leading `v` is dropped, so `v1.10.0` is newer than `v1.2.0`.
- `tessera add NAME[@VERSION]` adds the latest version, or the one named (`color@1.2.0` or `color@v1.2.0`), to the nearest tessera.toml (a new one in the current directory if there is none) as `NAME = { git = "URL", tag = "TAG", caps = [...] }`, with `caps` exactly what the index lists, and prints those capabilities: they are what you are granting. Before writing anything it fetches the tag and refuses when the tag no longer points at the indexed commit (it was moved), or when the package's code takes a capability the index does not list (the same check as E0320, with `caps` set to the index's list). It writes the checked commit to tessera.lock. It never grants `foreign`: for a package that calls C it says so, and the program does not build until you add `[trust] foreign = ["NAME"]` yourself.
- `tessera index entry [DIR]`, run at the top of a package's git repository, prints the entry for the tag on the current commit: the name (tessera.toml's `[package] name`, else the directory's), the URL of the `origin` remote, the tag and commit, tessera.toml's `[package] description`, and the capabilities its functions take, computed from the code. It refuses uncommitted changes and an untagged commit. Publishing a version is adding that entry to the index, for example in a pull request.

## 19. Standard library

Written in Tessera and built into the compiler; `use` any of them. They take capabilities only as parameters, like any other code.

The standard library is checked like a dependency. `src/std/tessera.toml` lists the capabilities each module may take, and every program that uses a module is checked against it (E0320), with or without a tessera.toml of its own:

| Module | May take |
| --- | --- |
| `json`, `list`, `text`, `format`, `time`, `regex`, `errors` | nothing: pure |
| `http` | `net` (Net, Conn) and `listen` (Listener, Conn) |
| `sql` | `fs` (the database file's directory); trusted with C (`[trust] foreign = ["sql"]`) to call SQLite |

A module missing from the list may take nothing. `tessera caps FILE` shows what each std function takes. What stays trusted, and why:

- The built-in methods of the capabilities themselves (`Net.get`, `Fs.read`, `Listen.on`, ...), written in OCaml for the interpreter and C for the native runtime. They are where the grants are enforced (host allowlists, the fs root, read-only), so there is nothing below them to check them against; they are covered by the tests of both backends.
- The built-in methods of plain values (`Str`, `List`, `Map`, channels, ...). They take no capability, so they cannot do I/O.
- The std modules' source. It is embedded in the compiler, not fetched, so it is as trustworthy as the compiler binary; the manifest check makes sure that what it can do stays what it declares.
- The C libraries that std modules trusted in the manifest's `[trust]` declare in `foreign` blocks (SQLite for `sql`). Their calls need no `Foreign`, and the declarations are private to the module; the module wraps every call and keeps SQLite inside the `Fs` grant (see `sql` below). A standard library module that is not trusted cannot call C. Trusted modules may also declare the runtime's own helper library, `foreign "tessera"`, which no other code may: `tessera_copy(p, n)` copies C memory into `Bytes`, and a handle table (`tessera_handle_new`, `_acquire`, `_release`, `_close`) keeps C pointers out of Tessera values. A trusted module's values hold a random id from that table instead of a `Handle`, since any code can write `Handle { addr: ... }` but no code can turn an id into a pointer; an unknown or closed id is an error, and a pointer is freed only once no use of it is in progress.
- Callbacks. `http.serve(l, n, handler)` calls `handler` inside std code; the handler carries only the capabilities its closure captured from your code, which appear in your signatures, not std's.

### errors

Walking and printing the chain of causes of a `dyn Error` (section 4).

| Function | Result |
| --- | --- |
| `chain(e)` | `List[dyn Error]`: `e`, its cause, that one's cause, ... |
| `root(e)` | `dyn Error`: the innermost cause, the error that started it |
| `wrap(e, context)` | `dyn Error`: `e` with context, as `x? "context"` makes it |
| `own_message(e)` | `Str`: what this link adds, its message without its cause's message at the end |
| `report(e)` | `Str`: the chain, one link per line, each after the first marked `caused by:` |

```
use errors

match start(root.fs) {
    Ok(s) => run(s),
    Err(e) => root.stdout.print(errors.report(e)),
}
// starting
// caused by: reading config
// caused by: denied: fs: not granted (run with --grant fs=DIR)
```

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
| `request_lines(net, method, url, headers, body, f)` | `Result[Response, IoError]`: for a stream that may never end (a Kubernetes watch, server-sent events, a log follow), each line of a 2xx body is passed to `f: fn(Str) -> Bool` as soon as it has arrived, without its `\n` or `\r\n`; `f` returns `false` to stop, and the connection is closed. Any other status is returned with its whole body, and `f` is not called |
| `serve_forever(listener, handler) -> IoError` | serve every connection in its own task until the listener is closed (by another task holding it) or fails; returns that error. The tasks are an `interleaved` group (section 10): what a handler prints goes out at once |
| `serve(listener, count, handler) -> Int` | the same for `count` connections; returns how many requests were answered |
| `serve_conn(conn, handler) -> Int` | answer requests on one connection until the client closes it or asks to (`Connection: close`, or HTTP/1.0 without keep-alive); 400 and close on a malformed request, 500 if the handler panics, headers only for HEAD |
| `handle(conn, handler) -> Bool` | answer one request, then close |
| `accept_forever(listener, f) -> IoError` | run `f: fn(Conn) -> Int` on each accepted connection in its own task, until accepting fails; an `interleaved` group too |
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

`sort_by(xs, less)` (stable merge sort, `less(a, b)` says whether `a` goes first; to sort by a key, use the method `xs.sort_by(key)`), `index_of(xs, x)`, `find_index(xs, pred)`, `range(lo, hi)`, `remove_at(xs, i)`, `insert_at(xs, i, x)`. All return new lists.

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

Regular expressions that never backtrack: a pattern compiles to a program for a Pike VM, which follows every way the pattern can match at once, so a search takes time proportional to the pattern size times the text length, whatever the pattern. `(a+)+$` on a long run of `a`s is as fast as any other pattern. Written in Tessera.

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

The syntax is a subset of RE2's: literals; `.` (any character but newline); classes `[abc]`, `[^a-z]`, `[\d_]`; `\d \w \s` and `\D \W \S` (ASCII); `\b \B` word edges; `^ $` (text start and end) and `\A \z`; `* + ? {n} {n,} {n,m}` (counts up to 1000), each lazy with a trailing `?`; `( )` captures, `(?: )` groups without capturing, `(?P<name> )` or `(?<name> )` name a capture; `|`; escapes `\n \t \r \f \v \0 \xHH` and `\` before any punctuation. Flags `(?i)` (ASCII case-insensitive), `(?m)` (`^` and `$` match at lines) and `(?s)` (`.` matches newline) apply from where they appear to the end of the pattern. Text is UTF-8: `.` and negated classes match whole characters, and a non-ASCII character is one unit for a quantifier. Classes may list non-ASCII characters but not ranges of them, and a negated class cannot list them. Backreferences and lookaround are not supported, since they need backtracking. A Tessera string needs `\{` for a literal brace, so `\d{4}` is written `"\\d\{4\}"`.

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

Calendar dates and times as pure functions of epoch milliseconds (and `within`, a deadline for anything that waits), the number `Clock.now_ms()` returns. Everything is UTC on the proleptic Gregorian calendar, without leap seconds. Fixed offsets from UTC (in minutes, `60` for +01:00) are supported where a function says so. Named time zones with daylight saving rules come from the system's tz database, read through an `Fs` granted to it; after loading, a `Zone` is plain data and the functions using it are pure.

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
| `struct Zone { name, ... }` | a named zone: its offsets over history and the POSIX rule for times after the last listed change |
| `load_zone(fs, name) -> Result[Zone, IoError]` | reads `name` (`"Europe/Madrid"`; letters, digits, `/ _ - +`, no `..`) from the directory `fs` is granted: `--grant fs=/usr/share/zoneinfo:ro` |
| `zone_from_tzif(name, data: Bytes) -> Result[Zone, Str]` | a zone from a TZif file's bytes (RFC 8536, versions 1 to 4), for zones shipped with a program (`embed_bytes`) |
| `offset_in(ms, z)`, `abbr_in(ms, z)`, `is_dst_in(ms, z)` | the offset in minutes (rounded toward zero; only local mean times before about 1900 have seconds), `"CEST"`, `true` |
| `parts_in(ms, z)`, `iso_in(ms, z)`, `format_in(ms, z, layout)` | as `parts_at`, `iso_at` and `format_at` at the zone's offset then; `format_in`'s `%Z` is the abbreviation |
| `from_parts_in(p, z)` | the moment local fields name in a zone. A time that happens twice when clocks go back gives the earlier moment; a skipped one (02:30 when clocks jump from 02:00 to 03:00) is read with the offset before the change, so it lands after it (03:30) |
| `within(clock, ms, f) -> Result[T, IoError]` | runs `f: fn() -> Result[T, IoError]` with a deadline: its result, or `Err(Failed("timed out after {ms} ms"))`. `f` runs as a task of a group with that deadline (section 10), so at the deadline it stops at its next wait, its defers run and its sockets are closed. With `load_zone`, one of the two functions here that take a capability |

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
- SQLite stays inside the grants. `ATTACH` is turned off, and with it `VACUUM` (which attaches a temporary database; `VACUUM INTO` would write anywhere), so SQL cannot open other files. The database file is not opened through a symlink, and `open` refuses a database whose `-journal`, `-wal` or `-shm` file is a symlink out of the granted directory (or a broken one), since SQLite follows those. URI filenames and extensions are off (SQLite's defaults, which the module never changes), and temporary tables and sorts stay in memory. What remains is a race no Tessera code can cause: another process that can write to the directory swapping in such a symlink while the database is open. SQLite writes its journal next to the database, inside the granted directory. A busy database is waited on for up to 5 seconds; that wait, like every SQLite call, blocks the worker thread.

## 20. Not yet

The core language is close to complete; what is missing is mostly around it.

- Platforms: no Windows (fibers, the poller and the sandbox are macOS and Linux only); x86-64 has not run on real hardware; no cross-compilation or WebAssembly target.
- Standard library: no hashing or crypto (SHA-256, HMAC, secure random numbers), base64 or hex, URL parsing, CSV, compression, UUIDs, command-line flags, logging, path helpers or directory walking, and Unicode support stops at `chars()`.
- Tooling: no debugger mapping (native code is C without `#line`), profiler or test coverage; cold builds spend about 3 s in the C compiler for a 600-line program; no incremental checking.
- Distribution: no prebuilt releases or Homebrew formula; the package index has no hosted default; no stability promise yet.
- Language: no default parameter values, no integer types besides `Int` outside FFI, no code generation for repetitive cases such as JSON for a struct, and C callbacks cannot be kept for later.
- A native backend other than C: Cranelift was decided against, since native code is C compiled by the system's compiler.

See the design doc's "Toward a full language" and Known gaps for the details.
