# Limen language specification

This document describes Limen as implemented by this repository's compiler: interpreter (`limen run`) and native backend (`limen build`, `limen run --native`). It is meant to be read whole, by people and by models writing Limen. Where the two backends differ, the difference is stated.

## 1. The idea

Limen code can only touch the outside world through **capabilities**: values that grant authority. There is no global `print`, `open`, `http.get` or clock. The program starts with one capability, `root: Root`, and passes narrower ones to the functions that need them. A function's parameter types therefore say everything it can do.

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
- **Doc comments:** the `//` lines directly above an item, a struct field, an enum variant or an interface method, with no blank line in between, document it. The comment block at the very top of a file, followed by a blank line, documents the module. `limen doc` reads them; there is no special marker.
- **Identifiers:** `[A-Za-z_][A-Za-z0-9_]*`. By convention types and enum variants start with an uppercase letter, everything else lowercase. Patterns rely on this (section 7).
- **Keywords:** `fn let mut if else match for in while return struct enum true false group break continue pub use interface dyn select defer`. `type`, `test`, `bench`, `foreign`, `until`, `interleaved` and `with` are words only where they start their construct, so they stay usable as names: a field can be called `type`.
- **Integers:** decimal digits, `_` allowed as a separator (`1_000_000`); hex after `0x` (`0xff`, `0xFF_00`) and binary after `0b` (`0b1010`), with `_` too. A literal must fit in an `Int`: up to `9223372036854775807`, or `0x7fff_ffff_ffff_ffff` (E0005). A hex or binary literal with no digits, or a digit its base does not have (`0b102`), is E0005 too.
- **Floats:** digits with a fraction (`1.5`), an exponent (`1e9`, `2.5E-3`), or both. `_` is allowed. There is no leading-dot form (`.5`).
- **Strings:** `"..."`, may span lines. Escapes: `\n \t \r \\ \" \{ \} \0`, and `\u{...}` with 1 to 6 hex digits for a Unicode character, written as its UTF-8 bytes (`"caf\u{e9}"` is `"café"`). A code point past `10ffff`, or from `d800` to `dfff` (reserved for UTF-16), is not a character. Any other escape is error E0006.
- **Interpolation:** `{expr}` inside a string evaluates any expression, including ones with their own strings: `"{xs.join(", ")}"`. An empty `{}` is error E0007; write `\{` for a literal brace.
- **Multi-line strings:** `"""` at the end of a line opens one, its text is on the lines that follow, and `"""` on a line of its own closes it; code may follow the closing `"""` on that line. The closing line's indentation (its spaces and tabs, byte for byte) is taken off every line, and the line breaks right after the opening `"""` and right before the closing one are not part of the string:
  ```
  c.write("""
      HTTP/1.1 200 OK
      Content-Type: {kind}

      {body}
      """)
  ```
  is `"HTTP/1.1 200 OK\nContent-Type: {kind}\n\n{body}"`. Escapes and interpolation work as in `"..."`, and `"` needs no escape (write `\"""` for a line that starts with three quotes). A line ending in `\` goes on with the next line, with no line break, and the next line's indentation is still taken off. A line indented less than the closing `"""` is error E0009, unless it holds only spaces and tabs (it is then an empty line); so is text after the opening `"""`, and a `\` ending the last line. A line break written `\r\n` in the file is `\n` in the string. For a protocol that wants `\r\n` line ends, replace them: `"""...""".replace("\n", "\r\n")`. An interpolation's code is code, not text: it may span lines, and those lines keep their indentation.
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
| `UInt8`, `UInt16`, `UInt32`, `UInt64` | unsigned integers of 8, 16, 32 and 64 bits | Arithmetic wraps modulo 2^bits; a shift by the width or more gives 0. An integer literal takes the type where one is expected (`let b: UInt8 = 255`, `[1, 2]` as a `List[UInt16]`, a match arm) and must fit (E0201); there is no unary minus. A UInt does not mix with `Int` (E0201): convert with `n.to_uint8()` and the like, which wrap, and `u.to_int()`. For bytes, hashes and C's formats. |
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
| `Atomic` | an `Int` tasks share | Created with `atomic(n)`; changed in place, atomically (section 10). |
| `Shared[T]` | a value tasks share, behind a lock | Created with `shared(v)`; read as snapshots, changed by `set` and `update` (section 10). |
| `Handle` | `{ addr: Int }` | From the prelude: an opaque C pointer, for foreign functions (section 9). |
| `FileInfo` | `{ size: Int, modified_ms: Int, is_file: Bool, is_dir: Bool }` | From the prelude; returned by `Fs.info`. |
| `Root`, `Net`, `Fs`, `Stdout`, `Stderr`, `Clock`, `Listen`, `Listener`, `Conn`, `Rand`, `Stdin`, `Env`, `Exec`, `Foreign` | capabilities | Cannot be created by programs; can be passed, returned and stored in struct fields, enum payloads and collections (section 9). |
| user `struct` and `enum` | | Optionally generic (section 4). |

There is no separate character type: a character is a `Str` holding one UTF-8 code point, and its number an `Int`. `s.chars()` gives them as a `List[Str]`, `for c in s.chars() { ... }` visits them, `s.bytes()` gives the bytes (section 11), and the `unicode` module the code points, full case mapping, case folding and the normal forms NFC and NFD (section 19). One type for text keeps the API small; `len()` still counts bytes.

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

Tuples compare and print like structs: `(1, "a") == (1, "a")`, and `(1, "a")` prints as `(1, "a")`. `let` and `for` can only unpack into names and `_` (in tuples and struct patterns); `match` takes any patterns, such as `(true, 0)`.

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

A key is an `Int`, `Str`, `Bytes` or `Bool`, or a tuple, struct or enum made of them, compared by value: `grid[(x, y)]`, `seen[Pos { x, y }]`. `Float` (NaN is not equal to itself), lists, maps and capabilities cannot be keys, nor can a struct or enum that holds one (E0113). Iteration (`keys`, `values`, `entries`) and printing follow insertion order, the same in both backends; replacing a value keeps its key's position, and removing then re-inserting moves it to the end. Two maps are equal when they hold the same entries, in any order.

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
- A parameter can have a default value, used when a call leaves the argument out: `fn join(xs: List[Str], sep: Str = ", ")` is called `join(xs)` or `join(xs, " | ")`. A default is a constant (a number, a plain string, `true`, `false`, `None` or `[]`), anything else is E0123. It is checked once, where the function is declared, so the parameter's type cannot use the function's type parameters (E0123). Parameters with defaults can come anywhere: the ones before a later required parameter are filled by name, `f(b: 2)`. Methods take defaults too; foreign functions and interface methods do not. A function used as a value (`let g = join`) has its full type, `fn(List[Str], Str) -> Str`, and every argument is passed.
- A parameter can be `mut`, `self` included: `fn roll(mut seed: Int) -> Int`. Inside the function it is a `mut` variable for the length of the call, and it is the caller's place: each change the function makes to it lands there as it is made. The call writes the argument `mut place`, `roll(mut seed)`, where the place is anything an assignment can have on its left (section 6: a variable, a field, a list or array element, a map entry, or a path through them) and its root is `mut` (E0205). The place has exactly the parameter's type, with no conversion (E0201). The receiver of a `mut self` method is the place and takes no marker (Methods, below). The marker matches the parameter: `mut` on an argument whose parameter is not `mut`, or whose callee has no `mut` parameters (a closure, a function value, a variant, a built-in function or method), and a missing `mut` where the parameter is `mut`, are E0213, with a fix that removes or adds it; a value piped with `|>` cannot be passed as `mut`. When the callee returns, panics or is cancelled, the place holds what it has written so far, and a `defer` in the caller or the callee sees that. Storing the parameter, returning it, capturing it in a closure or sending it on a channel takes a copy, as for any variable (section 8), and a closure cannot assign it (E0205). A `mut` parameter cannot have a default (E0123), and `main` cannot have one (E0303). Only functions and methods declared with `fn` take `mut` parameters: not closures, tests, interface methods or foreign functions (E0003, section 20).
- The `mut` places of one call may not overlap (E0214): `swap(mut a, mut a)` and `fill(mut s, mut s.items)` are refused, and so is `s.add(mut s.items)` for a `mut self` method. Distinct fields of one value do not overlap, `swap(mut p.x, mut p.y)`; two elements of one list or entries of one map count as overlapping, since their indexes are not known when checking: take them out, call, and put them back. The other arguments of the call, outside closures, may not change one of its `mut` places either, through a `mut` argument or `push` of their own (`f(mut a, g(mut a))`) or by assigning it (`f(mut a, { a = 1; 2 })`), and an index of a `mut` place may not assign a variable that another index of the same place reads; make that call or assignment first, on its own line. A plain argument may name a `mut` place: `f(mut xs, xs)` passes the plain one as it is before the call.
- A function or method with a `mut` parameter can only be called by name, never used as a value (E0215), like `push` and `remove`: a `mut` argument is a place, which only a direct call can pass. `let g = roll`, `xs.map(roll)` and `let f = buf.add` are refused; wrap the call in a function that takes and returns the value.
- The body's last expression, without `;`, is the return value. `return e` returns early.
- Generic parameters go in `[...]` after the name. They are instantiated at each use and are never written at call sites: `swap(p)`.
- A type parameter written as an array's size stands for a length, inferred at each call: `fn sum[N](xs: Array[Int, N]) -> Int` takes an array of any length, and `fn first[T, N](xs: Array[T, N]) -> T` any element type too. Inside, `xs.len()` is that length. The function is compiled once per length used, like the rest of generics. A parameter cannot be both a length and a type (E0102), and it is not a value: use `xs.len()`.
- Struct fields and enum payloads can hold capabilities: `struct Client { net: Net, base: Str }`. The authority a type holds through its fields is tracked wherever the type goes (section 9). They can hold functions too: `struct Handlers { on_get: fn(Str) -> Str }`. A function stored in a field is called with brackets, `(h.on_get)("/")`, because `h.on_get("/")` is a method call (E0105). A struct may have a field and a method of the same name: `(h.f)(x)` calls the field, `h.f(x)` the method, and `h.f` as a value is the field. A type holding a function, directly or through its fields, follows the rules for functions: it cannot become `dyn`, be a map key or be compared with `==`, and a function taking one runs callbacks (section 9). They cannot hold a channel, `Atomic` or `Shared` (E0305), which could carry shared state out of sight, however deep: not in a `List[Chan[Int]]` field either. A type argument of a generic struct or enum is what its fields hold, so the same rule applies to it wherever the type is written (`fn f(b: Box[Atomic])`), made (`Box { item: atomic(0) }`, a variant constructor) or returned by a generic function with this use's type arguments (`wrap(atomic(0))` where `fn wrap[T](x: T) -> Box[T]`). `Option`, `Result`, tuples, lists and maps hold values, so `Some(atomic(0))` and `[|| 1]` are fine.
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
- Methods take `self` by value like any parameter; to "change" a value, return the new one, or declare it `mut self`. `fn add(mut self: Buf, x: Int) { self.items.push(x) }` changes the value it is called on, which must be a `mut` place, as for `push`: `buf.add(1)`, with no marker on the receiver. The receiver counts as a `mut` argument of the call (E0214). Such a method cannot satisfy an interface's method or a bound (E0111), since a bound or `dyn` value calls it as a value, with a plain receiver.
- A method without the call is a value: `p.add` is a function that calls `add` on `p`, the same as `|q| p.add(q)`. Like a closure, it holds a copy of `p` taken when `p.add` is evaluated, so `[a, b].map(origin.add)` works and later changes to `p` do not reach it. This works for every method that can be called with a dot: user methods, `dyn` and interface-bounded methods, and built-in ones (`let say = out.print`), except `push` and `remove`, which change their receiver, and methods with a `mut` parameter, `mut self` included (E0215). (It is `p.add`, as in Go and Swift, and not `Point.add`: the value already knows its type, and `Name.x` is taken by enum variants and modules.)
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

`limen caps` reports a function with bounded type parameters like one that takes callbacks: what it can do depends on the type it is called with.

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
- Only struct and enum values can become `dyn`, and only if they hold no capability, function or channel (E0112), directly or through the fields of the structs and enums inside them, so a `dyn` value never hides authority. To work with such values through an interface, use a bounded type parameter instead. The one exception is `dyn Error` (below), which `Str` and `IoError` values can become too.
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
| `==` `!=` | any type except functions, channels, `Atomic`s, `Shared`s, `dyn` values and capabilities, and values holding any of them however deep, such as `List[Atomic]` or `Option[fn() -> Int]` (E0201, as for the built-ins that compare, such as `contains`); structural equality | `Bool` |
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

`|x, y: Int| expr` or `|| expr`. Parameter types are optional when the context gives them (`xs.map(|x| x + 1)`). The context reaches into list and tuple literals, so a closure inside one gets its types from the type expected there: `let ops: List[(Str, fn(Int, Int) -> Int)] = [("add", |a, b| a + b)]`, or a function returning `List[http.Route]` whose body is `[("GET /", |r, p| ...)]`. A closure captures the variables it uses **by value**, when it is created, and it cannot assign to them (E0205). Inside a closure, `return` and `?` leave the closure, not the enclosing function.

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
- It is the same closure as `|x| { ... }`: same types, same captures. `limen fmt` keeps whichever form was written (and drops the empty `()` in `f() { ... }`).
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

Evaluation order is left to right: the callee (or a method's receiver), then the arguments in the order they are written. For assignment, the right-hand side is evaluated first, then the target's indexes and keys left to right, each exactly once; a variable used as an index is read where it is written, and the root variable is written as it is after them.

**`mut` arguments.** `f(mut place)` passes a place to a `mut` parameter (section 4), and the callee changes that place. The place's indexes are evaluated where the argument is written, left to right, as written; a plain argument holds the value it had where it is written. A named argument can be `mut` too: `bump(n: mut k)`.

**Named arguments.** An argument can name the parameter it is for, which reads better than a row of bare numbers and booleans:

```
fn connect(host: Str, port: Int, retries: Int) -> Conn { ... }

connect(host, retries: 3, port: 443)
server.describe(prefix: "to", upper: true)
```

- Positional arguments come first, then named ones in any order. A named argument is `name: expr`, with the parameter's name as declared.
- Every parameter is passed exactly once, except one with a default value (section 4), which may be left out; leaving out any other is E0107. A named argument skips the defaults before it: `connect(host, port, retries: 1)` leaves the others between at their defaults.
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
let Point { x, y } = p    // unpack a struct (`..` for the fields left out)
(x, p) = step(p)?         // assign a tuple's items to existing variables
for (k, v) in m.entries() { ... }
for Todo { id, title, .. } in todos { ... }
m[key] = value            // insert into a map (needs mut)
break
continue
defer f.close()           // runs when the enclosing block is left
defer { ... }
expr
```

- Parameters, unless declared `mut` (section 4), and pattern bindings are immutable. Use `let mut y = y` to get a mutable copy.
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
| `42`, `-1`, `"text"`, `true` | that literal; an integer literal matches a `UInt` value too, and must fit in it |
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

`let` and `for` unpack names, `_`, and tuples and struct patterns of those, as deep as they go (`let (a, b) = pair`, `let Line { from: Point { x, .. }, .. } = l`, `for Todo { id, title, .. } in todos`); table tests unpack tuples only. A struct pattern there is checked as in a `match`: the value must be that struct (E0201), each field must exist (E0103), and every field is named unless the pattern ends in `..` (E0206). The other patterns, which can fail to match, need a `match`. `let [a, b] = xs` says so (E0003): a list can have any length, so it is taken apart with `match`.

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

A `mut` parameter (section 4) does not share a value. It lets one call change one place that the caller names with `mut`, `roll(mut seed)`, and nothing else can reach that place during the call: closures and tasks hold copies. Storing the parameter, returning it, capturing it or sending it takes a copy, as for any variable, so Limen has no stored references. Natively the callee writes to the caller's place directly, so a value held once, such as a list pushed to through a `mut` parameter, is changed in place without a copy.

The native backend implements this with reference counting and copy-on-write. A value is copied only if it is shared at the moment it is mutated. A value's last use moves it rather than retaining it, so in-place updates are the normal case.

Memory is managed by reference counting, natively, with no garbage collector: a value is freed as soon as the last reference to it goes, and freeing a long chain (a list of a million `Option[Node]`s, lists nested as deep, a chain of `dyn` values) takes constant stack. So do the other walks over a whole value: printing it, comparing it with `==`, ordering it (`<`, `sort`, `min`, `max`), hashing it as a map key and sharing it with another task (sending it on a channel, putting it in a `Shared`, capturing it in a task). Natively each recurses only a few dozen levels and keeps the rest on a list of its own, on the heap, so a value as deep as it is long, such as an enum list built a million `Cons`es deep, cannot overflow a task's stack; the output, the answers and the hashes are the same as a recursive walk's, and `==` and `<` still stop at the first difference. Because values are copied, not shared, a value cannot contain itself, so reference counting frees everything, with one exception: the only things that are shared rather than copied are a `Shared` and a channel (section 10), and through them a cycle can be built: a `Shared[List[fn() -> Int]]` holding a closure that captured that same `Shared`, or a channel whose buffer holds a value that captured the channel. Nothing collects such a cycle, so it stays allocated until the program exits. To avoid one, do not put into a `Shared` or a channel anything that holds that same `Shared` or channel; pass the `Shared` or channel to the code that needs it as a parameter instead of capturing it in a value stored inside it. The interpreter has a garbage collector and frees cycles too; that difference is invisible to a program, except in how much memory it uses.

## 9. Capabilities

`Root` has eleven fields, each a capability, and one method:

| Field | Type | Granted |
| --- | --- | --- |
| `root.stdout` | `Stdout` | always |
| `root.stderr` | `Stderr` | always |
| `root.stdin` | `Stdin` | always |
| `root.clock` | `Clock` | always |
| `root.rand` | `Rand` | always: the operating system's secure generator; `--seed N` makes the numbers repeatable instead, the same in both backends (`bytes` included) |
| `root.env` | `Env` | only with `--grant env=NAME` (repeatable) or `--grant env=*`; other variables read as `None` |
| `root.net` | `Net` | only with `--grant net=HOST[:PORT]` (repeatable) or `--grant net=*` |
| `root.fs` | `Fs` | only with `--grant fs=DIR` or `--grant fs=DIR:ro` |
| `root.listen` | `Listen` | only with `--grant listen=HOST[:PORT]` (repeatable) |
| `root.exec` | `Exec` | only with `--grant exec=PROGRAM` (repeatable), `--grant 'exec=PROGRAM ARGS\|ARGS...'` (that program with arguments starting with one of those, below) or `--grant exec=*` |
| `root.foreign` | `Foreign` | only with `--grant foreign=LIB` (repeatable, or several at once: `foreign=z,sqlite3`) or `--grant foreign=*`: calls into C libraries |

`root.args()` is the `List[Str]` of command-line arguments after `--`: `limen run app.lmn -- a b`.

Grants can also be written once in the `[grants]` table of the program's limen.toml (section 18), so they are not retyped every run. `limen run` applies them first and the `--grant` flags add to them; a `--grant fs=...` flag replaces the fs grant, since there is only one. A binary from `limen build` reads only its flags.

An ungranted `Net`, `Fs`, `Listen` or `Exec` still exists, but every operation on it returns `Err(Denied(...))`. An ungranted `Foreign` exists too; calling a foreign function through it panics (`foreign.allows(lib)` checks first). `Listener` and `Conn` are capabilities too: you get them from `Listen.on`, `Listener.accept` and `Net.connect`.

The rules:

1. Programs cannot create capabilities. They come only from `root`.
2. Attenuation only narrows. `net.only(host)` returns a `Net` limited to that host, and it is empty if you didn't hold that host. `fs.under(dir)` limits to a subdirectory; `fs.read_only()` removes writing. `exec.only(program)` limits `Exec` to one program (keeping any argument prefixes it was limited to), and `exec.only_args(program, prefixes)` to that program with arguments starting with one of `prefixes` (below); `foreign.only(lib)` limits `Foreign` to one library.
3. There is no ambient I/O. Using `print`, `open`, `fetch` and similar names is error E0302, which explains the capability to use instead.
4. A function whose parameters hold no capability (not even through a struct or enum field, below), no function type (also not through a field) and no channel, `Atomic` or `Shared` is **pure**: it can only compute (and panic). What a channel carries counts like what a list holds: a function taking `Chan[Stdout]` takes `Stdout`. `limen caps FILE` lists each function's authority, and says `shares state` (`shares state, runs callbacks`; `"shares": true` in `--json`) for one that holds no capability but takes a channel, `Atomic` or `Shared`, even in a list, an option or a tuple. Section 14 says what the compiler does with pure calls. It never runs them in parallel on its own: concurrency stays visible and cancellable in a `group`, where you write it. A `mut` parameter (section 4) does not by itself make a function impure: what it changes is the place the caller marked `mut`, as visible at the call as a returned value is.

   A function that would be pure but for parameters of function type is **pure if they are**, when all it does with them is call them or pass them on, as they are, to a function that is itself pure if they are: `fn my_map(xs: List[Int], f: fn(Int) -> Int) -> List[Int]` that only calls `f(x)`. A call of it is then a pure call when the functions it is given are pure: `my_map(xs, |x| x * 2)` is, `my_map(xs, |x| { out.print("{x}"); x })` is not. Storing such a parameter (in a list, a struct, a map), returning it, capturing it in a closure, sending it on a channel or running it as a task makes the function only "pure itself": its calls are never treated as pure. `limen caps` says `pure if f is` (`pure if f and g are`; `"pure_if": ["f", "g"]` in `--json`), and `pure itself, runs callbacks` for the others. So does a function with a `mut` parameter of function type, which may replace the caller's function. A parameter whose type holds a function through its fields, such as `Handlers { on_get: fn(Str) -> Str }`, is not a function parameter in this sense: a function taking one is only pure itself, and `limen caps` names the field, `pure itself, runs callbacks (via Handlers.on_get)`. A bound on a type parameter (`[T: Named]`) still counts as running callbacks, and the authority its methods reach counts as the function's own: in `fn f[T: Giver](x: T)`, where `interface Giver { fn give(self) -> Stdout }`, `x.give()` is a `Stdout`, so `f` takes `Stdout` (`limen caps` shows `Stdout (via Giver.give)`), and so does a capability a bound method passes to a callback, `fn take(self, f: fn(Fs))`. This goes through bounds of bounds: the interface of a `dyn` type written in a bound method's signature, anywhere in its parameter or result types (`fn get(self) -> dyn Inner`, `fn make(self) -> fn() -> dyn Taker`), bounds the value inside, so its methods count too, to any depth, and `limen caps` names each method on the way, `Stdout (via Outer.get, Inner.give)`. The outermost `dyn` is followed first, so in `fn get(self) -> dyn Box[dyn Leaf]` the path goes through `Box`'s methods, `Outer.get, Box.open, Leaf.dial`. Each interface's methods are walked once, whatever its arguments, so an interface that mentions itself, `interface Node { fn next(self) -> dyn Node }`, or grows its own arguments, `dyn Grow[Option[T]]`, does not make the walk endless; a capability in the arguments of a later mention (`dyn Grow[Stdout]`) still counts, through the method that mentions it. Only types written in a bound method's signature are followed: a `dyn` field of a struct such a method returns is not.
5. Enforcement at runtime:
   - `Net`: http and https URLs only, host allowlist, redirects not followed. `Net.connect` and `Net.connect_tls` use the same allowlist. TLS certificates are always verified (section 10).
   - `Exec`: the program name must match a grant exactly (`git` and `/usr/bin/git` are different grants), or the grant is `*`. Arguments are passed as a list, never through a shell. The child gets `/dev/null` as stdin, the working directory, `PATH`, and only the environment variables granted with `--grant env=`. A program name without `/` is looked up in `PATH`, as a shell would. Granting a program grants everything that program can do with any arguments, so `exec=sh` is as good as every other grant, unless the grant limits the arguments.
   - `Exec` argument prefixes: `--grant 'exec=git status|log|remote -v'` (or `exec.only_args("git", ["status", "log", "remote -v"])`) allows `git` only when its arguments start with one of the prefixes, compared word for word from the first argument: `git status --short` and `git remote -v` run, `git config core.sshCommand ...`, `git -c x=y status`, `git --upload-pack=... status`, `git statusx` and `git` alone are `Err(Denied("exec: `git` is not granted with these arguments"))`. A prefix is one or more words separated by spaces (so a word with a space in it cannot be part of one); no patterns, no shell. What follows a prefix is not checked, so grant the narrowest prefix that does the job: `git fetch` still takes `--upload-pack`. Narrowing keeps only what is held: `only_args` keeps the prefixes that start with one already granted (`"status --short"` under `status`), `only(program)` keeps the program's prefixes, and a grant of `*` cannot have prefixes. Exec has no knobs for the child's environment or directory beyond these, so there is nothing else to narrow.
   - `Listen`: binding is allowed only for granted `host` or `host:port` entries, with or without TLS.
   - `Fs`: paths are relative to the granted directory; absolute paths, `..` escapes and symlinks pointing outside are refused. That holds for both ends of a `rename`, and a read-only `Fs` refuses it, as it refuses every write. A symbolic link that stays inside is followed (`remove` and `rename` act on the link itself, `make_dir` does not follow it). Each operation walks its path one name at a time from the directory the `Fs` holds, through directory descriptors, reading each link itself rather than letting the operating system follow it, so a directory swapped for a link while the operation runs (by another task's `rename`, say) cannot lead it outside; there is no gap between a check and the use of the file. A path containing a NUL byte is `Err(Failed("fs: the path contains a NUL byte"))` (`exists` and `is_dir` are `false`, and `under` gives an `Fs` with nothing granted), and reading a directory is an error, in both backends.
   - `Foreign`: a call checks that the function's library is on the `Foreign` value's list (or the list holds `*`). C code runs with the whole process's authority, so granting a library grants everything that library can do; see below.

### Revocation

Authority can be lent and taken back. `x.revocable()`, on a `Net`, `Fs`, `Listen`, `Exec`, `Env` or `Foreign`, returns a copy of `x` and a `Revoker`, the switch that withdraws it:

```
let (fs2, off) = root.fs.revocable()
plugin.run(fs2.under("plugin"))     // the plugin may keep it, in a task too
off.revoke()                        // from here on, fs2 and everything made from it are dead
```

- `off.revoke()` withdraws the copy at once, and everything narrowed from it (`under`, `read_only`, `only`, `only_args`), in every task that holds it. Revoking twice does nothing; `off.is_revoked()` says whether it is revoked, its own switch or one it was made under.
- After that, an operation with a `Result` gives `Err(Denied("fs: revoked"))` (`net:`, `listen:`, `exec:`); `Fs.exists` and `is_dir` give `false`, `is_read_only` gives `true`; `Env.get` gives `None`, as for a variable that is not granted; `Foreign.allows` gives `false`, and a foreign call or `foreign.copy` through it panics with `foreign: revoked`, as a call into an ungranted library does. Nothing a revoked capability did before is undone.
- The `Listener`s and `Conn`s obtained through it (`on`, `accept`, `connect`, `connect_tls`) are closed when it is revoked, as `close()` closes them: a task waiting in `accept` or `read` wakes with `Err(Failed("listener is closed"))` or `"connection is closed"`, and so does every later use.
- A revocable capability can be made revocable again: revoking the outer switch withdraws the inner copy too, not the other way round.
- `Revoker` is a capability: a function taking one is not pure, and it has no equality, cannot be a map key or `dyn`, and prints as `<Revoker>`. No grant gives one and none is needed: only `revocable()` makes one, and it can only take authority away, so a dependency may take a `Revoker` whatever its `caps` say. `Stdout`, `Stderr`, `Stdin`, `Clock` and `Rand` are not revocable: they are always granted and their operations have no way to fail.

The same in both backends: natively the switch is shared by the threads whose tasks hold the copy, and an operation checks it as it starts, so one already in progress when the switch goes off finishes (a socket's is cut short by the close).

### Capabilities in structs and enums

A struct field or an enum payload can be a capability, so authority that travels together can be bundled:

```
struct Client {
    net: Net,
    base: Str,
}

fn fetch(c: Client, path: Str) -> Result[Str, IoError] {
    Ok(c.net.get(c.base + path)?.body)
}
```

A type **holds** a capability when it contains one: written in it (`Net`, `List[Net]`, `Box[Net]`), or reached through the fields and payloads of the structs and enums in it, however deeply and through lists, maps, options and type arguments (`struct App { client: Client }` holds `Net`). The compiler works this out once per type, recursive types included, and every rule about authority looks through it:

- A function taking `c: Client` takes `Net`. `limen caps` says where it comes from, `Net (via Client.net)` (a path through fields, with an enum payload named by its variant, `Sink.ToOut`), and the `--json` output has a `"via"` object mapping each such capability to its path. A capability also written in a parameter's type has no `via`.
- Such a function is never pure, so the compiler never reuses or keeps its calls (section 14), and a closure that captures such a value is not fused into a pipeline.
- A dependency taking a `Client` is checked against its `caps` like one taking a `Net` (E0320): authority cannot cross into a dependency inside a struct, nor on a channel (`Chan[Stdout]`), nor through the methods of a type parameter's bound, or of the `dyn` interfaces those methods mention (rule 4; the note names each method, `through Giver.give` or `through Outer.get, Inner.give`).
- A value holding a capability cannot become `dyn` (E0112), cannot be a map key (E0113) and cannot be compared with `==` or `!=` (E0201): capabilities have no equality, and neither does a value holding one. The built-ins that compare values refuse it the same way (E0201, naming the built-in and the path): `contains` on lists and arrays, `sort`, `chunk_by`'s keys, `list.index_of` and `assert_eq`; those bounded by `Ordered` or `Key` (`min`, `max`, `min_by`, `max_by`, `sort_by`, `distinct`, `distinct_by`, `group_by`, `count_by`, `tally`) refuse it as a type without order or a key (E0111). Compare something it holds that is not a capability instead, `xs.any(|x| x.name == name)`. Use a bounded type parameter, `[T: Named]`, to reach its methods through an interface.
- Generic functions are held to the same rule where they are used. The compiler finds which type parameters each generic function compares: with `==` or `!=`, with the built-ins above, or by passing them on to another generic function that compares them (`fn same[U](a: U, b: U) -> Bool { eq([a], [b]) }` compares `U`). A use that gives such a parameter a type without equality, one holding a capability, a function, a channel or a `dyn` value, is E0201, naming the function, the parameter and the path: "`eq` compares values of `T`, and `Logger` holds capability `Stdout` through `Logger.out`". A parameter bounded by `Ordered` or `Key` is refused by its bound instead (E0111).
- It is an ordinary value otherwise: built with a literal, updated with `with`, kept in lists and maps, matched, passed to tasks (each task gets its own copy; capabilities are shared safely between threads). It prints with its capabilities as `<Net>`, which cannot be turned back into one.

Rule 1 still holds: a struct cannot make a capability, only carry one it was given.

### Foreign functions (C)

A `foreign` block declares functions of a C library. Each has a signature and no body:

```
foreign "sqlite3" {
    fn sqlite3_libversion() -> Str
    fn sqlite3_open_v2(path: Str, db: Out[Handle], flags: Int32, vfs: Int) -> Int32
    pub fn sqlite3_close_v2(db: Handle) -> Int32
}
```

Calling one takes a `Foreign` capability first, then the other arguments: `sqlite3_libversion(root.foreign)`. Each call checks that the `Foreign` value grants the library (`--grant foreign=sqlite3`), and panics if not. So C is reachable only from code that was handed `Foreign`, `limen caps` lists such functions, and a package dependency can use a foreign block only if the program names it in `[trust] foreign = [...]` in limen.toml (section 18). The library name is the `-l` name: `foreign "sqlite3"` links `-lsqlite3` natively and loads `libsqlite3.dylib` (macOS) or `libsqlite3.so` in the interpreter; `foreign "c"` is the C library itself, which is always there. The function name is the C symbol, unless the declaration names another after its signature: `fn print_int(format: Str, ..., n: Int) -> Int32 = "printf"`. Declarations are one per line; `pub fn` exports one from a module.

**Per library.** A `Foreign` grants a list of libraries, and narrows like the other capabilities: `root.foreign.only("z")` grants only `z` (and nothing if it did not hold `z`), so a function handed it can call the functions of `foreign "z"` blocks and of no other library. A foreign function belongs to its block's library, so where the checker can see what a `Foreign` was narrowed to, a call of another library's function is refused when compiling (E0333): `let z = root.foreign.only("z")` and then `sqlite3_open_v2(z, ...)`, or the same with `root.foreign.only("z")` written in the call. A variable bound with `let` (not `let mut`) keeps what it was narrowed to; a parameter, a field or a reassigned variable is checked at the call, as before. `limen caps` lists the libraries each function reaches with its `Foreign`, `Foreign (z, sqlite3)` (`"libs"` in `--json`): its own library for a foreign function, and for any other the libraries of the functions it uses that take a `Foreign`, however deep. A function that only copies memory (`foreign.copy`) shows `Foreign` alone.

The types that cross the boundary:

| Limen | C | Notes |
| --- | --- | --- |
| `Int` | `int64_t` | |
| `Int32` | `int32_t` (C `int`) | an `Int` in Limen; an argument outside the 32-bit range panics |
| `Float` | `double` | |
| `Bool` | `int` | 0 is `false`; any other value is `true` |
| `Str` | `const char *` | the string's bytes followed by a NUL; pass the length yourself if C wants it (`s.len()`) |
| `Bytes` | `const void *` | the bytes; pass the length yourself |
| `Handle` | `void *` | an opaque pointer, `Handle { addr: Int }` in Limen; `addr == 0` is NULL |
| `Out[T]` | `T *` | an out-parameter of `Int`, `Int32`, `Float` or `Handle`: C writes it, Limen returns it |
| a C struct | `struct { ... }` | declared in the foreign block, passed and returned by value (below) |
| `fn(A, B) -> R` | a pointer to a function | a callback: a top-level Limen function C calls during the call (below) |

The result is one of the same types (not `Bytes`: C gives no length) or nothing. A `Str` result is copied from a NUL-terminated C string at once (NULL gives `""`); C keeps ownership of the original. `Out` parameters are not passed by the caller: they come back after the result, so `sqlite3_open_v2` above has the Limen type `fn(Foreign, Str, Int, Int) -> (Int, Handle)`, called as `let (rc, db) = sqlite3_open_v2(f, path, flags, 0)`. A function with no result and one `Out` returns that value alone.

**Variadic functions.** C cannot tell what a variadic function was passed, so a declaration fixes it: the parameters before `...` are the C function's own, and the ones after it are the variadic arguments this declaration passes. A C function called with several shapes gets one declaration per shape, each with its own Limen name:

```
foreign "c" {
    fn snprintf(buf: Handle, size: Int, format: Str, ..., n: Int32, x: Float) -> Int32
    fn format_int(buf: Handle, size: Int, format: Str, ..., n: Int) -> Int32 = "snprintf"
}
```

C's default promotions apply to the arguments after `...`: `Int32` and `Bool` go as `int`, `Float` as `double`, `Int` as `int64_t` (`%lld`, not `%d`). Before `...` come 1 to 6 integer-like parameters (no `Float` or struct), after it at most 8 of `Int`, `Int32`, `Float`, `Bool`, `Str`, `Bytes` and `Handle`; a variadic function does not return a struct.

**Structs by value.** A `struct` in a foreign block is a C struct, with fields of `Int`, `Int32`, `Float`, `Bool` and `Handle` (C's `int64_t`, `int32_t`, `double`, `int` and `void *`) laid out as C lays them out, 1 to 8 of them. To Limen it is an ordinary struct of the module, with `Int32` fields as `Int`s: built with `Div { quot: 1, rem: 0 }`, printed, compared and passed around like any other, and exported with `pub struct`. A foreign function takes and returns it by value:

```
foreign "c" {
    struct Div { quot: Int32, rem: Int32 }
    fn div(a: Int32, b: Int32) -> Div
}
```

Passing one checks each `Int32` field's range, as for an `Int32` argument. Only structs declared in a foreign block cross to C; a struct holds no `Str`, `Bytes` or other struct.

**Blocking calls.** A C call runs on the thread of the task that makes it, so a call that takes long (a query, a file, a lock, a sleep) holds that thread up: natively it is one of the worker threads that run every task. A declaration marked `blocking` runs elsewhere:

```
foreign "sqlite3" {
    blocking fn sqlite3_step(stmt: Handle) -> Int32
    fn sqlite3_column_int64(stmt: Handle, i: Int32) -> Int
}
```

- Natively a blocking call runs on a thread of the blocking-call pool while its task waits, parked like a task waiting on a socket, so other tasks keep running. The pool starts a thread whenever a call finds none free, up to `LIMEN_BLOCKING_THREADS` (64 by default); past that, calls wait their turn. In the interpreter each blocking call runs on a thread of its own while the other tasks go on.
- A call that returns quickly still crosses to another thread and back: about half a microsecond natively (both sides watch for each other for a few tens of microseconds before sleeping), against a nanosecond or two for an ordinary call. So `blocking` is for calls that can take long, and is not the default: most C functions (`strlen`, `abs`, a getter) are short, and run on the task's thread.
- Cancellation (section 10): a task cancelled while its blocking call is in C stops waiting and unwinds at once. The call goes on in the background and its results are dropped when it returns; the strings and bytes it was lent stay alive until then (C never sees freed memory), and an out-parameter is written into memory that is freed afterwards. A `Handle` the abandoned call returns is lost, as is any C memory nobody frees. A blocking call made while a cancellation is unwinding the task (in a `defer`) is waited for to the end, like the rest of that deferred code.
- A blocking function cannot take a callback (E0330): C may call a callback only during the call, on the calling task's own thread. Inside a callback from C, a blocking call runs on the spot, since the task cannot park there.
- Everything else is as for any foreign function: the grant check, the types, the ownership rules, and C's own thread-safety rules, since the call may run on a different thread each time (most libraries allow that; one whose functions must all run on one thread cannot be declared blocking).

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
- C may call it only during the foreign call that was given it, and on the same thread. If C keeps the pointer and calls it after the call returns, or from another thread, the behaviour is undefined (both backends stop the program where they can tell). For a callback C keeps, use `Kept` (below).
- Nothing unwinds through C's frames. A panic in a callback ends the program with `panic: in a callback from C: ...`, past every `catch` and task boundary, and so does waiting in one (on a group, a channel, a timer or input), which would leave C's frames on a task that moves.
- A callback can make foreign calls with callbacks of their own. The interpreter has room for 32 callbacks in use at once (each foreign call takes its own until it returns); more is a panic there.

**Kept callbacks.** A library that calls back on its own schedule (a timer, an event loop, a thread it starts) keeps the pointer and calls it later, perhaps from a thread of its own, where no Limen code can run. A parameter `Kept[fn(A, B)]` is such a pointer, and its argument is a channel: each call from C sends its arguments on it, and a task receives them as any other message.

```
foreign "c" {
    fn pthread_create(thread: Out[Handle], attr: Handle, start: Kept[fn(Handle)], arg: Handle) -> Int32
}

let started: Chan[Handle] = channel(0)
let (rc, t) = pthread_create(f, Handle { addr: 0 }, started, Handle { addr: 1 })
let arg = started.recv()                        // Some(Handle { addr: 1 }), from the new thread
```

- The parameters are those of a callback, and it returns nothing (`Kept[fn(...) -> T]` is error E0330): C gets no answer from a channel. The channel carries the one parameter, a tuple of several (`Chan[(Handle, Int)]` for `Kept[fn(Handle, Int32)]`), or `()` for none; an `Int32` arrives as an `Int`, a `Str` is copied when C calls.
- A call from C never waits: a task waiting to receive gets the value at once; otherwise it is queued, past the channel's capacity if need be. On a closed channel it is dropped.
- The pointer stays valid for the rest of the program, since C may hold it: the same channel always gives the same pointer, and a program can give C 32 channels at most (more is a panic). While C holds one, a task waiting on it is not a deadlock.

Ownership at the boundary:

- Arguments are lent to C for the duration of the call, never moved. A `Str` or `Bytes` pointer is valid only until the call returns: C must copy what it keeps (SQLite's `SQLITE_TRANSIENT`, a destructor argument of `-1`, does this). A `Handle` that C returns pointing into a Limen string is dangling once that string is freed.
- Memory C allocates belongs to C: free it with the library's own function (`sqlite3_finalize`, `free`). Limen never frees a `Handle`, and the leak check does not see C memory.
- `foreign.copy(p, n)` copies `n` bytes at `p` into new `Bytes`, for results that are a pointer and a length (`sqlite3_column_blob` and `sqlite3_column_bytes`). It reads raw memory, so it needs a `Foreign` that grants at least one library.

The foreign grant is full trust, and Limen treats it that way:

- Only the program grants it: `--grant foreign=LIB` or `[grants] foreign` in the program's own limen.toml. `limen run` prints a one-line warning whenever it is given, since nothing in Limen bounds what C code does; only the operating-system sandbox (below) still does.
- It does not pass along: a dependency's `caps` cannot include `foreign`. A dependency may declare foreign blocks only when the program names it in `[trust]`, and limen.lock records the C libraries each trusted dependency uses (section 18). Its caps can also name the libraries it may reach, `caps = ["foreign=z"]`: then a foreign block of another library, or a function that reaches one through another dependency's function taking a `Foreign`, is E0320, naming the function and the library.
- `limen check` lists every part of the program that calls C, one line each: the program's own files, trusted dependencies, and trusted standard library modules.
- The standard library calls C only from modules that its manifest, src/std/limen.toml, trusts in the same way (`sql`, whose caps name its one library, `foreign=sqlite3`). Their foreign functions are private to the module and take no `Foreign`, so a program using `sql` grants only an `Fs`; the module is what keeps SQLite inside that grant.

What the capability lets code do: anything the C functions it declares can do, with the program's full authority, outside every other capability's checks. Narrowing a `Foreign` to a library limits which library's declared functions Limen code may call; it does not limit what those functions do once called. C runs in the program's process: a C function of a granted library can read and change any of the program's memory, other libraries' and the runtime's included, call anything linked into the process, and so reach every capability the program holds. Only the operating-system sandbox (below) bounds it, and only to the program's grants as a whole. Limen checks the declared types and the library grant; it cannot check that a declaration matches the C prototype, and a wrong one is undefined behaviour. Keep foreign blocks small, private to a module that wraps them in a safe Limen API (as the `sql` module does), and grant `foreign` only to programs and dependencies you trust.

Limits, the same in both backends: at most 8 integer-like (everything but `Float`, `Out` and callbacks included) and 8 `Float` values, each field of a struct by value counting as one; the variadic and callback limits above; structs of 1 to 8 scalar fields, not nested. A foreign function can only be called, not used as a value (E0331): wrap it in a closure. A C call not declared `blocking` holds its worker thread until it returns, like a busy loop; C libraries decide their own thread safety.

The backends differ in how they reach the code, not in what a program sees: natively each function is called through its exact prototype (a variadic one through its variadic prototype, a struct through its C declaration, a callback through a generated C function that calls the compiled Limen function) and linked with `-l<lib>` (a library outside the C compiler's default paths is found through `LIBRARY_PATH`); the interpreter loads the library with `dlopen` and calls through a generic stub that lays out the arguments as the platform's calling convention does (x86-64 System V, arm64, and Apple's arm64 variant: registers, stack, how a struct is split or passed by pointer), and C calls callbacks through a fixed set of trampolines, resolving the declarations of granted libraries before `main` starts. A missing library or symbol is therefore an error before the program runs in both: a link error natively, `foreign: cannot load library ...` or `foreign: library ... has no function ...` in the interpreter. One difference follows from linking: a native binary loads every library it declares when it starts (running the library's initialisers, as any linked library does), granted or not, while the interpreter never loads a library the grants do not cover. In both, a call into an ungranted library panics before it reaches the library.

### The operating-system sandbox

Limen's checks cover Limen code. To cover everything else in the process (C called through `foreign`, and any bug in the runtime), every program also runs in an operating-system sandbox built from its grants. It is on by default, in both backends: `limen run` enters it once the program is loaded (and its granted C libraries with it), and a native binary enters it at startup, before any thread starts. `--no-sandbox` (on `limen run`, or on a built binary) turns it off; it is for debugging, not for production.

What the kernel enforces:

| | Allowed |
| --- | --- |
| Writing files | under the `Fs` directory, unless it is read-only; the temporary directory if `Exec` is granted (Exec keeps a child's stderr there); `/dev/null` and the terminal |
| Reading files | under the `Fs` directory; system locations (`/usr`, `/System`, `/Library`, `/etc`, `/dev`, Homebrew's prefix, ...: libraries, time zones, TLS certificates); the temporary directory with `Exec`. Not home directories, other users' files or anything else |
| Network | nothing unless `Net` or `Listen` is granted; then any host |
| Starting programs | only if `Exec` is granted, and then (unless it is granted `*`) only the granted programs' files, resolved through `PATH` when the process starts, and programs in system locations (`/usr`, `/bin`, `/sbin`, `/System`, `/Library`, Homebrew's prefix, Xcode; `/usr`, `/bin`, `/sbin`, `/lib`, `/opt` on Linux), which the granted programs may start in turn. Never a file under a directory the program can write (the `Fs` directory unless it is read-only, and the temporary directory), even when that directory lies inside a system location such as Homebrew's prefix, `/usr/local` or `/opt`, unless a grant names that file: on macOS nothing under those directories starts but the granted programs' own files; on Linux, where rules only add rights, a system location that holds one of those directories (or lies inside one) loses execute as a whole, so only the granted programs' files start from it. When a writable `Fs` directory holds system locations (`fs=/`, as the self-hosted compiler is granted), the OS's own program directories still start programs if only root can change them (`/bin`, `/sbin`, `/usr/bin`, `/usr/sbin`, `/usr/lib`, `/usr/libexec` and the Command Line Tools' `usr/bin` and `usr/libexec` on macOS; `/usr/bin`, `/usr/sbin`, `/usr/lib*`, `/usr/libexec`, `/bin`, `/sbin`, `/lib*` on Linux): the program is not running as root, nor on Linux with a capability that gets past file permissions (`CAP_CHOWN`, `CAP_DAC_OVERRIDE`, `CAP_FOWNER`, `CAP_SYS_ADMIN`); the directory and every directory above it, on its path as named (where a symbolic link must be root's too) and on its real path, are owned by root, writable by neither group nor others nor (through an ACL) the program; no writable directory lies inside it; and when a writable directory holds it, everything inside it is root's and writable by no one else, or it lies on a file system mounted read-only (macOS's sealed system volume). Whatever cannot be checked (an unreadable directory, another file system inside it, more than 50,000 entries) leaves it unsealed. So nothing an admin can rename (Xcode in `/Applications`) starts programs there. A started program inherits the sandbox: it can read system locations and the `Fs` directory, and list (not read) the directories above the current one, which shells need |

What stays in Limen's own checks, not the kernel's: which hosts and ports `Net` and `Listen` allow (the kernel allows all networking once either is granted), which of the programs in system locations `Exec` may run and with which arguments, which variables `Env` shows, `Fs`'s refusal of `..` and absolute paths (the kernel sees only the resolved file), and everything inside the process: C code with the foreign grant can still read and corrupt the program's own memory, and through it every capability the program holds.

How:

- macOS: a sandbox profile (`sandbox_init`), allowing by default and denying the above. It also refuses Apple events and LaunchServices opens, so the program cannot have another application act for it. Mach services stay reachable, as for every process.
- Linux: Landlock (kernel 5.13 or later) for files, and on kernels with Landlock ABI 4 or later for TCP too, plus a seccomp filter that, without `Net` or `Listen`, allows only Unix sockets and refuses io_uring, and without `Exec` refuses `execve`; it also refuses x32 system calls. It has been run on arm64 Linux (kernel 7.0); on x86-64 it compiles but has not been run on a real kernel.
- A system without a sandbox (another OS, a Linux kernel without Landlock or seccomp filters, an emulator) runs the program without one, silently, as before. A sandbox that exists but cannot be entered stops the program with a message naming `--no-sandbox`.
- `limen test` runs native tests in the sandbox too (a test that takes `fs: Fs` gets its temporary directory); interpreted tests run inside the `limen` process, unsandboxed.

## 10. Concurrency

```
let results = group g {
    for row in rows {
        g.go(|| count_row(row))
    }
}
```

- `group g { body }` runs `body`, in which `g.go(f)` starts `f: fn() -> T` as a task. The group evaluates to a `List[T]` of the task results **in the order the tasks were started**.
- A task whose closure returns `()` runs for its effect and adds nothing to the list, so one group can mix them with tasks that return a value: the list holds the results of the value tasks, which must all have one type. A group of only `()` tasks evaluates to an empty list. To drop a value a task would return, end its body with a statement: `g.go(|| { log(x); })`. The rule is decided by the closure's type as written, so in generic code a task returning `T` always adds an item, even when `T` is `()`. A `()` task that finished without a panic is freed at once, natively and in the interpreter, unless its group still holds its output for start order (section 10, below): so a group that runs for ever, such as a server accepting connections, stays the same size.
- Value tasks are for fork-join: each is kept until the group ends. A group that runs for ever uses `()` tasks and sends what it produces on a channel.
- When the block ends, every task has finished. Nothing outlives the group.
- `g` can only be used as `g.go(...)` directly in the body. It cannot be stored, passed, returned, or used inside a closure (E0310).
- The body cannot leave early with `return` or `?` (E0311), and `break` and `continue` cannot target loops outside the group.
- Tasks share data by value, so there are no data races. What they can share and change is only what is made for it: channels, and `Atomic` and `Shared` values (below). A task reaches the world only through capabilities its closure captured. Spawning is not a capability.
- Tasks are lightweight: natively each is a fiber with a small stack (1 MB, reserved but only used as needed; `LIMEN_STACK_KB` sets it; `main` gets 1 GB), scheduled on one worker thread per core (`LIMEN_THREADS` overrides this), and a task waiting on a group, a channel, a socket, stdin or `clock.sleep(ms)` parks without blocking a thread. The interpreter runs tasks as coroutines on one thread: a task that computes for more than a few milliseconds without waiting steps aside at a checkpoint (below) so the others, and deadlines, get their turn. What tasks print comes out in the same order in both (below).
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
- Arm by arm, in the order written, each arm's guard is evaluated first, then its channel and the value it sends (or the clock and the milliseconds); this holds for an arm whose guard is false too. Then the first ready operation in the order written runs, so list the channel you want served first first. With no ready operation, `_` runs if there is one; otherwise `select` waits until one is ready.
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

### Shared state: Atomic and Shared

```
let hits = atomic(0)                              // an Int tasks share
let seen: Shared[Map[Str, Int]] = shared(map())   // a value tasks share
let _ = group g {
    for page in pages {
        g.go(|| {
            hits.add(1)
            seen.update(|m| {
                let mut m2 = m
                m2[page] = (m.get(page) ?? 0) + 1
                m2
            })
        })
    }
}
// hits.get() is pages.len(), and seen counts every page, whatever the order
```

Besides channels, two built-in types hold state that tasks share and change. Like a channel, each is shared, not copied: tasks that capture the same one see each other's changes, and so does code that passes it on.

- `atomic(n)` makes an `Atomic` holding the `Int` `n`. `a.get()` reads it; `a.set(n)` replaces it; `a.add(n)` adds `n` and returns the **new** value, so `let id = next.add(1)` hands out ids; `a.swap(n)` replaces it and returns the **old** value; `a.compare_and_set(old, new)` replaces it only if it still holds `old`, returning whether it did. Each is one atomic step of the processor, never torn and never lost (`add` wraps on overflow, as Int arithmetic does).
- `shared(v)` makes a `Shared[T]` holding `v`. `s.get()` returns the value as it is now: a snapshot, which later changes do not touch (values are copy-on-write, so this copies nothing). `s.set(v)` replaces it. `s.update(f)` applies `f: fn(T) -> T` to the value and stores the result, which it also returns, as one step: updates from many tasks apply one at a time, each to the result of the one before, so none is lost. `f` gets its own copy of the value, as any function does, so changing a large list or map in it copies it first; for heavy contention on large values, split the state into several `Shared`s or count with an `Atomic`.
- If `f` panics, or its task is cancelled while it runs, the value stays as it was and the panic or the cancellation goes on.
- **`f` must not wait.** It runs holding the value's lock, so it may only compute: no channel operation, `select`, `group`, `clock.sleep`, socket, stdin, program, blocking C call (section 9), and no use of a `Shared` (that one would wait for itself, another could wait for a task that waits for this one). When `f` is a closure literal the compiler checks what is written in it (E0312); what happens in functions it calls is checked as it runs: a wait there panics with `update must not wait` (at the `update`), before it could deadlock. Natively a task waiting for the lock spins briefly and then parks, so it never holds up a worker thread; since the holder never waits, the lock is always about to be let go, and a task waiting for it does not stop for a cancellation until it has it.
- The rules of channels: a function that takes an `Atomic` or a `Shared` (even in a list or an option) is not pure (section 9); a value holding one cannot become `dyn` (E0112), be a map key (E0113), be compared or interpolated (E0201), and cannot be stored in a struct or enum field (E0305), so everything a function can change in place is visible in its parameters and its captures. They print as `<atomic>` and `<shared>`.
- Both backends behave the same; natively an `Atomic` is a 64-bit atomic and a `Shared` a reference-counted value behind a lock that parks tasks, the interpreter's tasks take turns on one thread.

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
- At the deadline every task still running is cancelled. Timers ring in the order they are due, also when the program was too busy to notice them in time: a deadline that passed no later than a task's `clock.sleep` or `select` timeout was due stops that task there, even if both were already due when it next got to run, and a sleep due before the deadline returns. A cancelled task stops at its next wait or checkpoint: if it is waiting (on a channel, a `select`, `clock.sleep`, a socket, stdin or a group) it stops at once; otherwise it stops when it next has to wait or reaches a checkpoint. Every loop round (`while`, `for`, and the loops the compiler makes of list pipelines) and every function call is a checkpoint, so a task that only computes, in a loop or by recursion, is stopped too. What has no checkpoint cannot be stopped: a C call (section 9) runs until it returns (a task waiting for a `blocking` one stops waiting, and the call ends in the background), as does a single built-in operation (sorting a list, filling one). A task that finishes without reaching a wait or a checkpoint after the deadline keeps its result. A task started after the deadline never runs, and `ms <= 0` cancels every task before it starts.
- A cancelled task unwinds as a panic does: everything it owns is freed and sockets it holds are closed. A group the task is running cancels its own tasks too and waits for them. `catch` inside the task does not stop a cancellation, and a cancellation is not a panic: the group passes nothing on for it. A panic in another task of the group is still passed on once every task has stopped.
- The `defer`s a cancellation runs on the way out are not stopped at checkpoints: their loops and calls run to the end. They still stop at a wait, as the cancelled task would (so a deferred body cannot hang on a channel or a socket once its task is cancelled).
- A checkpoint is not an effect: a pure function stays pure (section 9), and its calls are reused and kept as before (section 14). It costs one load of a shared flag and a branch while nothing is being cancelled; a callback from C (section 9) has none, since nothing unwinds through C's frames: it stops at the first checkpoint after the foreign call returns.
- The deadline stops the body too, the same way: if the body is waiting (on a channel, a `select`, `clock.sleep`, a socket or an inner group) it stops there, and otherwise at its next wait or checkpoint; its `defer`s run, groups it opened are cancelled and waited for, and the tasks it did not get to start are not in the list. The group then waits for its tasks and returns as usual, so the code after it runs normally. Only the group's own deadline is caught there: when the task running the body is cancelled, or an outer group's deadline passes, the body stops and the cancellation goes on outwards. A panic in the body is passed on as before.

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
- `conn.start_tls(host)` starts TLS on a connection that is already open, for protocols that ask for it inside plain TCP (Postgres's SSLRequest, SMTP's STARTTLS): the client handshake runs on the same socket, and the server's certificate is checked against `host` as in `connect_tls`. It returns `Result[(), IoError]`; afterwards `read` and `write` go through TLS. It holds both the reading and the writing side while it runs (another task using the connection meanwhile is an error), a connection already speaking TLS is an error, and a failed handshake closes the connection.
- A server connection does its handshake during its first `read` or `write`, so `accept()` never waits for a slow client, and a failed handshake is that `read`'s error. A client connection has done its handshake when `connect_tls` returns.
- Handshakes, reads and writes park the task like plain socket operations; no worker thread blocks. Cancellation (a group deadline) works the same.
- TLS 1.2 or later. Clients do not send certificates and servers do not ask for them. `close()` sends the TLS close message. A peer that closes without one may have been cut off by someone in between, so that `read` is `Err(Failed("tls: the connection was closed without a TLS close message, so the data may be cut short"))` rather than `""`. `Net.get` and the `http` client still accept such a response when its `Content-Length` or chunked body shows it is complete, and refuse one whose body runs until the close.
- Errors from TLS start with `tls: `. Both backends use the same TLS code, so they verify and fail alike.

#### TLS library

TLS is OpenSSL (1.1.1 or later), in both backends: the interpreter is linked with it, and native programs that use `Net.get`, `connect_tls` or `on_tls` are linked with it. It is found when limen is built: `$LIMEN_OPENSSL` (an install prefix) if set, else `pkg-config openssl` (looking in Homebrew's `openssl@3` first), else the system headers. On macOS that is Homebrew's `openssl@3` (`brew install openssl@3`), whose trusted certificates are Homebrew's `cert.pem` bundle; on Linux it is the distribution's libssl (`libssl-dev` or `openssl-devel` to build), trusting the distribution's certificate store. Native programs link OpenSSL's shared libraries, so a binary from `limen build` that uses TLS needs them where it runs. If OpenSSL is not found, limen still builds and runs everything else, and every TLS operation fails with `Failed("tls: ... built without TLS ...")`. The macOS release binary has OpenSSL linked in (`LIMEN_OPENSSL_STATIC=1` with `LIMEN_OPENSSL`), so the interpreter's TLS works without Homebrew; when the trust store OpenSSL was built with isn't on the machine and neither `SSL_CERT_FILE` nor `SSL_CERT_DIR` is set, clients trust the system's bundle (`/etc/ssl/cert.pem` on macOS). A native program that uses TLS still needs OpenSSL's headers and libraries where it is built, and the build says so when they're missing.

### Running programs

```
fn main(root: Root) -> Result[Int, IoError] {
    let r = root.exec.run("git", ["status", "--short"])?    // --grant exec=git
    root.stdout.write(r.stdout)
    Ok(r.code)
}
```

- `Exec.run(program, args)` runs `program` with the list `args`, waits for it to finish, and returns `Ok(ProcessOutput { code, stdout, stderr })` with its exit status and everything it wrote. A child killed by signal N has code 128 + N.
- It is `Err(Denied(...))` if the program is not granted, or not with these arguments (argument prefixes, section 9), and `Err(Failed(...))` if it cannot be started (for example, not found). A non-zero exit status is not an error; check `code`.
- Waiting for the child parks only the calling task, so a group can run several programs at once.

### HTTP

```
let resp = net.get("http://127.0.0.1:8080/status")?
if resp.status == 200 {
    out.print(resp.headers.get("content-type") ?? "no type")
    out.print(resp.body)
}
```

- `Net.get(url)` sends one HTTP/1.1 `GET` (with `Host`, `User-Agent: limen`, `Accept: */*` and `Connection: close`), reads until the server closes, and returns `HttpResponse { status, headers, body }`.
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
| `Int` | `to_uint8()`, `to_uint16()`, `to_uint32()`, `to_uint64()` (wrapping: `(-1).to_uint8()` is `255`), `to_str()`, `to_float()`, `abs()`, `min(n)`, `max(n)`, `to_char()` (the UTF-8 text of a code point, `65.to_char()` is `"A"`; invalid code points give U+FFFD), `shift_right_unsigned(n)` (`>>` shifting in zeros instead of the sign: `(-1).shift_right_unsigned(60)` is `15`; a count of 64 or more gives `0`, a negative one panics) |
| `UInt8` to `UInt64` | `to_int()` (a `UInt64` past the largest `Int` wraps to a negative one), `to_float()` (the nearest `Float`, ties to even, as IEEE 754 rounds: `(2^63 + 1025).to_float()` is `9223372036854777856.0`), `to_str()`, `to_uint8()` to `to_uint64()` (wrapping), `min(n)`, `max(n)`, `rotate_left(n)`, `rotate_right(n)` (by `n` modulo the width) |
| `Float` | `to_str()`, `to_int()` (truncates; NaN gives 0), `abs()`, `sqrt()`, `to_fixed(d)` (exactly `d` decimals, 0 to 30, rounded as C's printf rounds: `3.14159.to_fixed(2)` is `"3.14"`; NaN and infinities give `nan`, `inf`, `-inf`) |
| `Bool` | `to_str()` |

### List[T]

| Method | Result |
| --- | --- |
| `len()`, `is_empty()` | `Int`, `Bool` |
| `get(i)`, `first()`, `last()` | `Option[T]` |
| `push(x)` | `()`, appends in place: a built-in `mut self` method, so the list must be a `mut` place (section 4) |
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

None of these panic, except when a function passed to them does. The methods from `any` on take functions and are written in Limen; they work the same as the others (a file cannot define a method with one of their names, E0106), and natively they compile like your own code, so a call through a closure literal costs a function call per item. A pipeline of `map` and `filter` before one of them still runs as one loop.

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

### Atomic

| Method | Result |
| --- | --- |
| `get()` | `Int` |
| `set(n)` | `()` |
| `add(n)` | `Int`: the new value |
| `swap(n)` | `Int`: the old value |
| `compare_and_set(old, new)` | `Bool`: whether it held `old` and now holds `new` |

### Shared[T]

| Method | Result |
| --- | --- |
| `get()` | `T`: a snapshot |
| `set(v)` | `()` |
| `update(f)` | `T`: `f: fn(T) -> T` applied under the lock, its result stored and returned; `f` must not wait (section 10) |

### Built-in functions

| Function | Meaning |
| --- | --- |
| `panic(msg)` | stop with a message and source location (section 15) |
| `catch(f)` | `Result[T, Str]`: run `f: fn() -> T`; `Err(message)` if it panicked (section 15) |
| `channel(n)` | a new `Chan[T]` with capacity `n` |
| `atomic(n)` | a new `Atomic` holding `n` |
| `shared(v)` | a new `Shared[T]` holding `v` |
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
- It must stay inside that file's package: the directory of the program's limen.toml (or of the main file when there is none), or a dependency's own directory. An absolute path, a `..` that leaves the package or a symlink that points outside it is error E0405. Without this rule a dependency could put `../../.ssh/id_rsa` into the binary, a read that no grant allows.
- The program's own limen.toml can name more directories its files may embed from, its **embed roots**: `[embed] roots = ["../assets"]`, relative to limen.toml (section 18). A file of the program's package may then embed a file under one of them, still by a path relative to itself (`embed("../assets/logo.png")`). Dependencies keep the package-only rule: a dependency's limen.toml that declares embed roots is E0321.
- `embed` requires UTF-8 text (E0405 otherwise); `embed_bytes` takes any file. The standard library embeds nothing.
- `embed_dir` takes every file under the directory and its subdirectories, keyed by its path inside it with `/` between parts, in sorted order. Names starting with `.` (`.git`, `.env`, `.DS_Store`) are left out, at any depth. Every file and subdirectory, links included, must stay inside the package, or the embed root holding the directory (E0405), and a directory with nothing to embed is E0405 too. Values are `Bytes`; `to_str()` costs nothing for text.
- The text is taken as it is: braces in the file do not start interpolation.
- Changing an embedded file changes the program, and the build cache sees it, because the contents are part of the generated C. `limen caps` lists the embed roots (`embed root ../assets`) and every embedded file with its size after the functions (`"embed_roots"` and `"embeds"` in `--json`).
- A function or variable named `embed` in scope is called as usual.

### Capabilities

| Capability | Method | Result |
| --- | --- | --- |
| `Stdout` | `print(s)` | `()`, writes `s` and a newline |
| `Stdout` | `write(s)` | `()`, writes `s` |
| `Stderr` | `print(s)`, `write(s)` | `()`, as `Stdout` does, to standard error: at once, never held in a group's order (section 10), after what stdout has buffered so far |
| `Clock` | `now_ms()` | `Int`, milliseconds since the Unix epoch |
| `Clock` | `sleep(ms)` | `()`, waits `ms` milliseconds (section 10); returns at once if `ms <= 0` |
| `Rand` | `int(lo, hi)` | `Int` in `lo` up to but not including `hi`, every value equally likely; panics if `hi <= lo` |
| `Rand` | `float()` | `Float` in [0, 1) |
| `Rand` | `bytes(n)` | `Bytes`: `n` random bytes, for keys, tokens and UUIDs; a negative `n` panics |
| `Stdin` | `read_line()` | `Option[Str]`, without the newline (or `\r\n`); `None` at the end of input. Waiting for input parks the task; one task reads at a time, so tasks reading together each get whole lines |
| `Stdin` | `read_all()` | `Str`, the rest of the input |
| `Env` | `get(name)` | `Option[Str]`; `None` if unset or not granted |
| `Net` | `get(url)` | `Result[HttpResponse, IoError]`: `status`, `headers` (`Map[Str, Str]`, lowercased names), `body`; http and https (section 10) |
| `Net` | `only(host)` | `Net`; `host` is `name`, `name:port` or `*` |
| `Net` | `connect(addr)` | `Result[Conn, IoError]`; `addr` is `host:port` |
| `Net` | `connect_tls(addr)` | `Result[Conn, IoError]`, a TLS connection whose certificate was verified for `host` |
| `Exec` | `run(program, args)` | `Result[ProcessOutput, IoError]`: `code`, `stdout`, `stderr` (section 10) |
| `Exec` | `only(program)` | `Exec`; `program` is a name or `*` |
| `Exec` | `only_args(program, prefixes)` | `Exec` for `program` with arguments starting with one of `prefixes` (`List[Str]`, each one or more words separated by spaces), those the value already allows (section 9) |
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
| `Conn` | `read_within(clock, ms)`, `write_within(clock, ms, s)` | as `read` and `write`, but `Err(Failed("timed out after MS ms"))` once `ms` milliseconds pass while waiting on the peer; the connection stays usable. The wait is a timer in the scheduler, so it costs about nothing when data is ready |
| `Fs` | `read(path)` | `Result[Str, IoError]` |
| `Fs` | `read_at(path, offset, n)` | `Result[Bytes, IoError]`: up to `n` bytes from `offset`, fewer at the end of the file and none past it, at most 64 MB at a time; for a file larger than memory, a piece at a time (the `stream` module folds over them). The file is opened for each call, so nothing stays open |
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
| `Net`, `Fs`, `Listen`, `Exec`, `Env`, `Foreign` | `revocable()` | `(T, Revoker)`: a copy that the `Revoker` can withdraw (section 9) |
| `Revoker` | `revoke()` | `()`: withdraws the copy, what was narrowed from it, and closes the sockets opened through it |
| `Revoker` | `is_revoked()` | `Bool` |

`Rand`'s numbers and bytes all come from the operating system's cryptographically secure generator (`arc4random_buf` on macOS, `getrandom` on Linux, `/dev/urandom` where that is missing), read a buffer at a time, so they are fit for secrets: keys, tokens, passwords, nonces, and `shuffle` or `choice` built on `int`. Natively `int` gives about 380 million numbers a second. With `--seed N` they come from a fast deterministic generator (splitmix64) seeded with `N` instead, the same stream in both backends, so a seeded run repeats: for tests and simulations, never for secrets. `int` maps a seeded number into the range by its remainder, and an unseeded one without bias, by drawing again when the number falls in the uneven part.

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
- Only `limen test` (with or without `--native`) runs code on virtual time, and only in tests that ask for it; `limen run` and `limen build` always use the real clock, as do tests that take `clock: Clock`.
- `limen test FILE` runs every test, each isolated, and prints `ok` (with the time it took) or `FAIL` with the panic message and location; it exits with status 1 if any test fails. `--native` runs them as native code, built without optimisation (section 17).
- `--run TEXT` runs only the tests (and benchmarks) whose name contains `TEXT`. `--json` prints one JSON object instead, for tools and agents: `{"tests": [{"name", "ok", "message"?, "where"?, "seconds"}], "benches": [...], "passed", "failed", "seconds"}`; a test's own output is left out of it.
- Test names must be unique (E0106). Tests are type-checked by `limen check` and ignored by `limen run` and `limen build`.

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
- `limen test FILE` skips benchmarks. `limen test FILE --bench` runs the tests and then every benchmark, always as native code: each benchmark runs in its own process, first once, then with more and more iterations (at most 100 times more per round), until one round takes at least the time budget, one second by default. `--bench-time 200ms` (or `2s`, `1m`) sets the budget, and `--bench-time 100x` runs exactly 100 iterations.
- It reports the iterations of the last round, the time per operation, and the runtime's allocations per operation (strings, lists, maps, closures and other objects made, whether or not they were freed). A panic fails the benchmark like a test.
- The value of the body's last expression is passed to `keep`, so the C compiler cannot drop the work that made it. Inside a loop, call `keep(x)` on values that are otherwise unused. A constant result (`1 + 2`) is still computed at compile time, as in any program, and a pure call whose arguments do not change in a loop is made once (section 14): vary the arguments to time each call.
- Benchmark names must be unique among benchmarks (E0106); a test and a benchmark may share a name.

## 13. How values print

String interpolation and `to_str()` use these rules. At the top level a `Str` prints raw; inside a container it prints quoted, with `"`, `\`, newline, tab, `\r` and `\b` escaped and every other byte outside 32 to 126 as `\ddd`, three decimal digits.

| Value | Prints as |
| --- | --- |
| `Int` | decimal |
| `Float` | `2.0` if integral and below 10^16 in magnitude, else 15 significant digits (`0.333333333333333`); NaN prints as `nan`, the infinities as `inf` and `-inf` |
| `Bool` | `true` / `false` |
| `()` | `()` |
| `List`, `Array` | `[1, 2, 3]` |
| struct | `Pair { first: 1, second: "a" }` |
| enum | `None`, `Some(3)`, `Rect(2.0, 3.0)` |
| function | `<fn>` |
| capability | `<Net>` |

## 14. Optimizations that affect observable behavior

The native backend may run a chain like `xs.filter(p).map(f).fold(z, g)` as one fused loop, interleaving the stages per element. It does this only when the earlier stages are pure closure literals, which capture no capability (nor a value holding one, section 9) and no function, except one held in a variable that is never reassigned and was given a pure closure literal or a top-level function (`let inc = |x| x + 1`, then `xs.map(|x| inc(x)).filter(...)`). The only observable difference is **which panic happens first** when more than one stage would panic. Everything else (results, output order, values) is the same.

Other uses of purity change nothing a program can observe. A pure call (to a pure function, or a built-in method such as `split` or `to_upper`, or to a function that is pure if its function arguments are, section 9, given closures made from pure functions: a closure literal capturing no capability and no function, or a top-level function) gives the same result for the same arguments, so:

- **In a loop**, a pure call whose arguments do not change in the loop is made the first time it is reached, and later iterations reuse its result. A loop that runs zero times never makes the call, a call under an `if` runs only once the condition holds, and a call that panics or never returns does so at the same point as before.
- **In a block**, a second call with the same arguments, where nothing they name was reassigned or changed in between, reuses the first call's result.

A call with a `mut` argument (section 4) writes to the caller's place, so it is never reused or kept, in a loop or a block, whatever the function, and it counts as a change of that place for the calls around it.

The interpreter reuses a pure call's result in a loop when the same function is called again with the same values. Pure calls on constants are not evaluated at compile time, beyond what the C compiler does itself, and results are never remembered across calls: write a cache yourself, as a `Map` you pass along, where one is worth it.

## 15. Errors

Every diagnostic has a stable code. `limen explain CODE` prints its explanation, and `limen check --json` emits code, span, notes and an auto-fix when there is one. Codes starting with W are warnings: they are printed, but the program still builds and runs.

| Code | Meaning |
| --- | --- |
| W0001 | a renamed built-in method (`map_err`, now `map_error`) |
| W0002 | a `match` arm, or an alternative of an or-pattern, that can never match |
| E0001-E0011 | lexing and parsing: bad character, unclosed string, unexpected token, missing newline, integer too large, bad escape, empty interpolation, untyped parameter, misshapen multi-line string, nested more than 256 deep (brackets, blocks, arguments, unary operators, interpolations, type arguments and sub-patterns inside one another; a chain such as `a + b + c`, `x.f().g()`, `x \|> f` or an `else if` ladder is not nesting), a chain of more than 50,000 links |
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
| E0123 | a parameter's default that is not a constant, uses the function's type parameters, or is on a `mut` parameter |
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
| E0212 | a type `json` or `sql` converts that is not known where it is used, or has no JSON or row form |
| E0213 | an argument whose `mut` marker does not match its parameter: `mut` where the parameter is not `mut` (or the callee has no `mut` parameters), or none where it is |
| E0214 | `mut` places of one call that overlap, or another argument of the call changing one of them |
| E0215 | a function or method with a `mut` parameter used as a value instead of called |
| E0301 | no longer reported: capabilities can be stored in fields (section 9) |
| E0302 | ambient I/O name such as `print` |
| E0303 | bad `main` signature |
| E0304 | no longer reported: functions can be stored in fields (section 4) |
| E0305 | channel, `Atomic` or `Shared` stored in a field, or in a type argument of a struct or enum |
| E0306 | a test parameter that is not `fs: Fs`, `clock: Clock` or `clock: virtual Clock`, or a bench that takes `virtual Clock` |
| E0310 | misuse of a group handle |
| E0311 | `return` or `?` leaving a group |
| E0312 | the function given to a `Shared` value's `update` can wait (a channel, `select`, `group`, `sleep`, stdin, a socket, a program, or another `Shared`) |
| E0320 | a dependency takes a capability limen.toml does not grant it (in its parameters, on a channel, or through its bounds' methods), or reaches a C library its caps do not name |
| E0321 | malformed limen.toml, an embed root that is not a directory, or a dependency's limen.toml that declares embed roots |
| E0322 | a git dependency could not be fetched |
| E0323 | limen.lock does not match: a malformed line, or a dependency whose files changed |
| E0324 | a trusted dependency calls a C library that limen.lock did not record |
| E0330 | a foreign function signature with a type that cannot cross to C, too many parameters, or a bad library name |
| E0331 | a foreign function used as a value instead of called |
| E0332 | a callback passed to C that is not a top-level function |
| E0333 | a foreign function given a `Foreign` narrowed with `only` to other libraries |
| E0401 | module not found, or a qualified name without `use` |
| E0402 | item is not `pub` |
| E0403 | module has no such item |
| E0404 | module name used as a value |
| E0405 | an embedded file outside its package (and, for the program's own files, its embed roots), missing, or not UTF-8 for `embed` |

A **panic** (division by zero, a negative shift count, index out of range, a missing map key, `panic(...)`, no matching arm) stops the code that hit it:

- Inside `catch(|| ...)`, the panic ends the closure and `catch` returns `Err(message)`. Otherwise it returns `Ok(value)`.
- Inside a task, the panic ends only that task; the other tasks of its group run to completion. When the group finishes, it passes the first panic (in the order the tasks were started) on to the code that ran the group, where a `catch` can handle it.
- Anywhere else, it prints `panic: message` and the source location to stderr, and the program exits with status 1. A deadlock always ends the program.
- Running out of stack (recursion too deep) is not a panic that can be caught. Natively each stack ends in a guarded region, and reaching it prints `panic: stack overflow` and which stack ran out (main's or a task's, with its size) to stderr and exits with status 1. Deep non-tail recursion that fits in `main`'s 1 GB can overflow a task's 1 MB, so recursion as deep as the input is long belongs in `main` or in a loop; `LIMEN_STACK_KB` gives tasks more. This is about the program's own recursion: dropping, printing, comparing, ordering, hashing and sharing a value take constant stack however deep it is (section 8). The interpreter's stack grows as needed, up to OCaml's limit (about 1 GB); past that it prints `panic: stack overflow` too.

```
let r = catch(|| parse_config(text))      // Result[Config, Str]
match r {
    Ok(cfg) => run(cfg),
    Err(msg) => out.print("bad config: {msg}"),
}
```

A panic unwinds the stack up to the boundary: everything the abandoned code owned is freed, including values made before the boundary and moved into it, so a loop that catches a panic on every iteration does not grow. Even so, `catch` is for keeping a program alive through bugs, such as a server answering 500 for one request, not for ordinary control flow; use `Result` and `?` for errors you expect.

## 16. Grammar

EBNF. `NL` is a newline, as described in section 2. `UPPER` is an `IDENT` that starts with a capital letter. A character-level version for constrained decoding, with the newline rules spelled out, is in [grammar/limen.gbnf](grammar/limen.gbnf). `{ x }` means zero or more, and `[ x ]` means optional.

```
program    = { "use" IDENT | [ "pub" ] ( fn | struct | enum | interface ) | test | bench | foreign } ;
foreign    = "foreign" STRING "{" { [ "pub" ] [ "blocking" ] "fn" IDENT "(" [ params ] ")" [ "->" type ] ( NL | ";" ) } "}" ;
interface  = "interface" IDENT [ generics ] "{" { "fn" IDENT "(" "self" { "," param } ")" [ "->" type ] ( "," | NL ) } "}" ;
test       = "test" STRING [ "(" [ tparams ] ")" ] [ "for" ( IDENT | tuplepat ) "in" "[" expr { "," expr } [ "," ] "]" ] block ;
bench      = "bench" STRING [ "(" [ params ] ")" ] block ;
fn         = "fn" IDENT [ bgenerics ] "(" [ fnparams ] ")" [ "->" type ] block ;
bgenerics  = "[" IDENT [ ":" bound { "+" bound } ] { "," IDENT [ ":" bound { "+" bound } ] } "]" ;
bound      = IDENT [ "." IDENT ] [ "[" typearg { "," typearg } "]" ] ;   (* an interface, with its arguments *)
generics   = "[" IDENT { "," IDENT } "]" ;
params     = param { sep param } [ "," ] ;
param      = IDENT ":" type ;
fnparams   = fnparam { sep fnparam } [ "," ] ;
fnparam    = [ "mut" ] IDENT ":" type ;          (* `mut` only in a `fn` item's parameters *)
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
arg        = [ IDENT ":" ] [ "mut" ] expr ;       (* a named argument; `mut` takes a place *)
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

- `limen build FILE` compiles the program's C with optimisation (`-O3`), for the binary you ship. `limen build --dev`, `limen run --native` and `limen test --native` compile it without (`-O0`), which builds faster and runs slower; `limen test --bench` always builds optimised, so benchmarks measure the program you ship. Unoptimised code uses more stack per call, so very deep recursion reaches a task's stack limit sooner.
- **Debugging.** A development build's C marks every Limen statement with `#line N "file.lmn"`, on every C line the statement becomes, and is compiled with `-g`, so lldb, gdb, the sanitizers and crash traces show Limen files and lines: `breakpoint set --file app.lmn --line 15` stops there, and a crash in C called from Limen shows the chain of Limen calls that led to it (`bt` in lldb: `wf_deep(...) at app.lmn:7`). The code the compiler adds between functions shows as `<generated>`. On macOS the debug information is collected next to the binary (`app.dSYM`, by `dsymutil`), and such a build is not taken from or put in the build cache, whose binaries' debug maps would name object files that are gone. A variable's C name is its Limen name and a number (`total_15`), in every build, so a debugger shows `total_15 = 42` rather than an anonymous slot; the compiler's own temporaries show as `t_12` and the like. `limen build --emit-c --dev` writes the marked C; an optimised build's C has no marks.
- **Profiling.** `limen build --profile` (and `limen run --native --profile`) builds a program that reports where its CPU time went when it exits, to stderr: for each function, its own CPU time (not counting the functions it calls), its share, and how many times it was called, the busiest first. Every function counts its calls and marks itself as running; a timer samples the running function about every millisecond of CPU, and the CPU the process used (from `getrusage`) is shared out by the samples. A task that waits counts nothing while it waits, and a task switch keeps each task's function apart. Counting calls costs time: a function called millions of times runs slower than unprofiled (fib(36), 48 million calls: 0.42 s against 0.30 s). Time in the runtime and in C outside any Limen function shows as `(runtime and C)`.
- **Coverage.** `limen test --coverage FILE` runs the tests interpreted and then says which lines of the program's own functions (not its tests, not the standard library) they ran: a line counts when a statement or a block's value starts on it. It prints the share per file and the lines that did not run, as ranges (`not run: 51-53, 88`); with `--json` the result has a `coverage` list of files with `lines`, `ran` and `missed`. It cannot be combined with `--native` or `--bench`.
- **The language server** publishes a file's diagnostics as soon as the program checks, and builds the index for go-to-definition, references, rename and highlighting only when one of those is asked for. Of edits that arrive faster than a large program checks, only the newest of each file is checked.
- How a native build is put together: the runtime is compiled once, as an object kept in the build cache, and its declarations once, as a precompiled header where the C compiler is clang. The program's C is cut into pieces of about 3,000 lines that compile at the same time against a shared header, then linked with the runtime; a smaller program is one piece, compiled and linked in one step. Measured on an Apple silicon Mac with the runtime already cached: hello world builds in 0.05 s, a 400-line HTTP and SQLite service in 0.65 s optimised and 0.2 s with `--dev`, and the self-hosted compiler (420,000 lines of C) in about 11 s. The first build with a new C compiler or flags compiles the runtime too, which takes about 0.8 s. The finished binary is cached by the exact C text and command line, so building an unchanged program again costs only the front end, a few hundredths of a second. With `LIMEN_CACHE=off`, a one-piece program is compiled as one file, which leaves out the parts of the runtime it doesn't use. `limen build --emit-c` writes that one file, which needs nothing but a C compiler.
- `limen fmt FILE` prints the canonical formatting (`--write` rewrites, `--check` verifies).
- `limen lsp` is a language server (diagnostics, hover, definition, references, rename, completion, quick fixes, outline); `limen mcp` is an MCP server exposing check, run, test, format, caps, type-at, references, rename and the spec to coding agents. Rename is refused when it is unsafe: the renamed program must check with no new errors, and the new name must refer to exactly the renamed places.
- `limen caps FILE` lists each function's authority, saying for a capability held through a struct or enum which field holds it (`Net (via Client.net)`), and for a function with no capability whether it is `pure`, `pure if f is` (section 9) or `pure itself, runs callbacks`; `limen explain CODE` explains an error.
- `limen search`, `limen add` and `limen index entry` use the package index (section 18).
- `limen doc FILE|MODULE... [--std] [--all] [--html] [-o OUT]` prints documentation built from doc comments (section 2) and signatures, as Markdown, or with `--html` as one self-contained page in the design of limen-lang.org (light and dark, nothing fetched; signatures highlighted, capability types marked). An argument that is not a file names a standard library module; `--std` documents all of them. It lists `pub` items (everything with `--all`); methods appear under their type, and methods on a type the module does not declare (such as `Str`) get their own section. Files are parsed, not type-checked.
- `limen new DIR [--web]` writes a project to start from into DIR, which must not exist or be empty; its name is DIR's last part (letters, digits, `-` and `_`). Without `--web`: limen.toml, main.lmn with a test, and a README. With `--web`, a small website: main.lmn routes requests with `http.route` and embeds `pages/` (plain HTML inside `pages/layout.html`) and `assets/` (the stylesheet of limen-lang.org, the mark, the fonts it serves itself, a theme switch), so it needs only the `listen` grant its limen.toml gives. Both pass `limen test`.

```limen
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
use json                  // json.lmn next to this file, a dependency, or the standard library

pub fn load(text: Str) -> Result[json.Value, Str] {
    json.parse(text)
}
```

- `use NAME` makes module `NAME` available in this file. It looks for `NAME.lmn` next to the file, then a dependency named `NAME` in limen.toml, then the standard library. A file never imports itself.
- A standard library module's `use NAME` always means the standard library's `NAME`, so no file or dependency of the program changes what the standard library does. A program can then hold a module `NAME` of its own (a file or a dependency) and the standard library's at once: its own is `NAME`, and the standard library's is named `std/NAME` wherever the program shows names, in messages, in types (`std/url.Url`) and in `limen caps`. The two never meet: natively each module's functions have C names of their own.
- Refer to a module's items as `NAME.item`: functions `json.parse(s)`, types `json.Value`, struct literals `json.Field { key, value }`, variants `json.Num(1.0)` and patterns `json.Num(x)`. A function can be used as a value: `let f = json.parse`.
- Only items marked `pub` (`pub fn`, `pub struct`, `pub enum`) can be used from another module (E0402). Struct fields and enum variants of a `pub` type are all visible.
- Inside a module, its own items are used unqualified. A local variable shadows a module name.
- Variants are matched by name, so `match v { Num(x) => ... }` works without the module prefix when the type is known.
- `test` blocks in a used module are not run; `limen test` runs the tests of the file you give it.
- Modules can use each other, including in cycles. Type names print with their module: `json.Value`.

### Packages and capabilities

A limen.toml next to the program (or in a parent directory) lists the program's own grants, its dependencies, and what each may do:

```
[package]
name = "app"

[grants]                                           # what `limen run` grants the program
fs = "data:ro"
net = ["api.github.com"]
env = ["HOME", "LANG"]

[dependencies]
json2 = { path = "vendor/json2" }                  # pure: no capabilities
fetch = { path = "../fetch", caps = ["net"] }
color = { git = "https://example.com/color.git", tag = "v1.2.0" }
yaml = { git = "git@example.com:me/yaml.git", commit = "3f9c2e1d7a" }

[embed]                                            # what `embed` may read besides the package
roots = ["../assets"]
```

- `[grants]` takes `net`, `fs`, `listen`, `env`, `exec` and `foreign`, each a string or a list of strings written exactly like the text after `--grant KIND=` (`exec = ["git status|log", "make"]`; an empty argument prefix or prefixes for `*` are E0321). `limen caps` lists them, narrowing included (`grant exec=git status|log`, `"grants"` in `--json`). `fs` takes one directory, relative to limen.toml, with an optional `:ro`. Stdout, Stderr, Stdin, Clock and Rand are always granted, so listing them is an error. Flags on the command line add to these grants.
- Grants belong to the program being run: only the limen.toml found for the main file is read for them. A dependency's own limen.toml never grants anything, so adding or updating a dependency cannot widen what the program may do. A dependency gets capabilities in exactly one way: your code passes them to it, and only of the kinds its `caps` allow. `[grants]` decides what reaches `main`; `caps` decides which of those may cross into each dependency.
- `[embed] roots` lists directories, relative to limen.toml, whose files the program's own files may `embed` besides those of its package (section 11). Each must exist and be a directory, links followed (E0321 otherwise). Like grants, they belong to the program: a dependency embeds only its own directory's files, and a dependency whose limen.toml has an `[embed]` table is E0321, though its limen.toml is otherwise never read. Embedding reads files at compile time, not at run time, so a root grants the program nothing when it runs; it only decides what can be built into it.
- A dependency is a directory holding `NAME.lmn`, plus any modules that file uses from the same directory.
- `caps` lists what the dependency may use: `net` (Net and Conn), `listen` (Listen, Listener and Conn), `fs`, `stdout`, `stderr`, `stdin`, `clock`, `rand`, `env`, `exec`. The default is none. No dependency can take `Root`, and `caps` cannot give `foreign` (E0321); for a dependency named in `[trust] foreign`, `"foreign=LIB"` (`"foreign=z,sqlite3"`) limits the C libraries it may reach (section 9).
- `[trust] foreign = ["name", ...]` names the dependencies that may call C: declare foreign blocks and take `Foreign`. Calling C is full trust, since C code is outside every capability check, so it is a separate, explicit list rather than one more cap, and only the program's limen.toml can hold it. An untrusted dependency that calls C is E0320. limen.lock gets a line per trusted dependency that calls C, `foreign NAME LIB,LIB`, written on the first build; when an update makes the dependency declare a library the line does not have, the build stops (E0324) until you review it and delete the line.
- Limen has no ambient authority, so a function can only use a capability that reaches it as a parameter. The checker therefore knows everything a dependency can ever do from its signatures, and rejects the program (E0320) if a dependency's function takes a capability it was not granted, counting those held by the structs and enums it takes (the note names the field, `through Ctx.fs`). Granting is still only an upper bound: at run time the dependency gets exactly the capabilities you pass it, which can be narrowed with `only` and `under`.
- A dependency is a local directory (`path`) or a git repository (`git`) at a `tag` or a `commit` (lowercase hex, at least 7 characters). The URL is `https://`, `http://`, `ssh://`, `git://`, `file://` or `user@host:path`. No registry holds the code: the URL is the name of the source, and the package index (below) only points at URLs.
- A git dependency is fetched with the `git` command into a cache, one directory per commit: `$LIMEN_CACHE`, else `$XDG_CACHE_HOME/limen`, else `~/.cache/limen`. Fetching needs no grant because it happens in the compiler, before the program exists; `limen lsp` and `limen mcp` never fetch, and report a dependency that is not cached yet (E0322).
- `limen.lock`, next to limen.toml, is written by the commands that produce something, `limen build`, `limen run` and `limen test` (and `limen add`), the first time they meet a new git dependency or a new trusted one that calls C, and should be committed. `limen check`, `limen caps` and the editor and agent servers read it and never write it: a check of a project whose lock is missing a line passes or fails as the build would, and leaves the file as it is. It holds one line per git dependency: name, URL, `tag:NAME` or `commit:HASH` as written in limen.toml, the full commit, and a BLAKE2b hash of the dependency's files:

  ```
  color https://example.com/color.git tag:v1.2.0 8d022128f5f177019f1e6aed46f75c93c90815f1 blake2b:5fea136b...
  ```

  Every build uses the locked commit, even if the tag has moved since, and checks the cached files against the hash (E0323). To update a dependency, change its `tag` or `commit`, or delete its line; limen fetches it and rewrites the line.
- Each dependency must be listed in the program's limen.toml, including dependencies of dependencies: a dependency's own limen.toml is not read.

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

- `commit` is the commit the tag pointed at when the entry was written. `caps` are written as in limen.toml, plus `foreign` for a package that calls C. `description` and `caps` may be left out (no description, no capabilities).
- `LIMEN_INDEX` names the index: a git URL (cloned each time it is used), a directory (every `.toml` file at its top) or one file. It has no default; without it, the commands below say how to set it. An index that cannot be reached is an error, never an empty result.
- `limen search [WORDS...]` lists the packages whose name or description contains every word (ignoring case), each with its description, its latest version and its capabilities. Versions compare as numbers: a leading `v` is dropped, so `v1.10.0` is newer than `v1.2.0`.
- `limen add NAME[@VERSION]` adds the latest version, or the one named (`color@1.2.0` or `color@v1.2.0`), to the nearest limen.toml (a new one in the current directory if there is none) as `NAME = { git = "URL", tag = "TAG", caps = [...] }`, with `caps` exactly what the index lists, and prints those capabilities: they are what you are granting. Before writing anything it fetches the tag and refuses when the tag no longer points at the indexed commit (it was moved), or when the package's code takes a capability the index does not list (the same check as E0320, with `caps` set to the index's list). It writes the checked commit to limen.lock. It never grants `foreign`: for a package that calls C it says so, and the program does not build until you add `[trust] foreign = ["NAME"]` yourself.
- `limen index entry [DIR]`, run at the top of a package's git repository, prints the entry for the tag on the current commit: the name (limen.toml's `[package] name`, else the directory's), the URL of the `origin` remote, the tag and commit, limen.toml's `[package] description`, and the capabilities its functions take, computed from the code. It refuses uncommitted changes and an untagged commit. Publishing a version is adding that entry to the index, for example in a pull request.

## 19. Standard library

Written in Limen and built into the compiler; `use` any of them. They take capabilities only as parameters, like any other code. They use each other (`url` reads hex digits with `hex`), always the standard library's own modules, whatever modules the program has of the same names (section 18).

The standard library is checked like a dependency. `src/std/limen.toml` lists the capabilities each module may take, and every program that uses a module is checked against it (E0320), with or without a limen.toml of its own:

| Module | May take |
| --- | --- |
| `json`, `list`, `text`, `format`, `regex`, `errors`, `hex`, `base64`, `hash`, `csv`, `url`, `flags`, `compress`, `unicode` | nothing: pure |
| `http` | `net` (Net, Conn) and `listen` (Listener, Conn) |
| `time` | `clock` and `fs`, for `within` and `load_zone` alone |
| `sql` | `fs` (the database file's directory); trusted with C (`[trust] foreign = ["sql"]`) to call SQLite |
| `log` | `stdout`, `stderr` and `clock`: a Logger holds where it writes and, for timestamps, a clock |
| `path` | `fs`, for `walk` alone |
| `uuid` | `rand` and `clock`, for `v4` and `v7` alone |
| `postgres` | `net` and `rand` (SCRAM's nonce) |
| `redis` | `net` |
| `stream` | `fs` |
| `websocket` | `net` and `rand` (a client's masking keys) |
| `crypto` | `rand` (keys and nonces); trusted with the runtime's own helper, which calls OpenSSL as TLS does |

A module missing from the list may take nothing. `limen caps FILE` shows what each std function takes. What stays trusted, and why:

- The built-in methods of the capabilities themselves (`Net.get`, `Fs.read`, `Listen.on`, ...), written in OCaml for the interpreter and C for the native runtime. They are where the grants are enforced (host allowlists, the fs root, read-only), so there is nothing below them to check them against; they are covered by the tests of both backends.
- The built-in methods of plain values (`Str`, `List`, `Map`, channels, ...). They take no capability, so they cannot do I/O.
- The std modules' source. It is embedded in the compiler, not fetched, so it is as trustworthy as the compiler binary; the manifest check makes sure that what it can do stays what it declares.
- The C libraries that std modules trusted in the manifest's `[trust]` declare in `foreign` blocks (SQLite for `sql`). Their calls need no `Foreign`, and the declarations are private to the module; the module wraps every call and keeps SQLite inside the `Fs` grant (see `sql` below). A standard library module that is not trusted cannot call C. Trusted modules may also declare the runtime's own helper library, `foreign "limen"`, which no other code may: `limen_copy(p, n)` copies C memory into `Bytes`, and a handle table (`limen_handle_new`, `_acquire`, `_release`, `_close`) keeps C pointers out of Limen values. A trusted module's values hold a random id from that table instead of a `Handle`, since any code can write `Handle { addr: ... }` but no code can turn an id into a pointer; an unknown or closed id is an error, and a pointer is freed only once no use of it is in progress.
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
| `enum Value { Null, Bool(Bool), Num(Float), Int(Int), Str(Str), Arr(List[Value]), Obj(List[Field]) }` | a JSON value; objects keep their fields in order. A number written as a whole number that fits an `Int` (no `.` or exponent, not `-0`) parses as `Int`, exactly, so ids and counts above 2^53 survive a round trip; any other number is a `Num`. A whole-number `Num` prints as digits (`1000.0` as `1000`) and so reads back as an `Int` of the same value |
| `struct Field { key: Str, value: Value }`, `field(key, value)` | an object member |
| `parse(s) -> Result[Value, Str]` | the error names what was expected and the byte offset. Numbers follow RFC 8259 exactly (`01`, `1.`, `.5` and `+1` are errors); one beyond `Float`'s range (`1e999`) is an error, one too small to tell from zero reads as `0`. Where RFC 8259 leaves the choice to the parser, `parse` refuses: half a surrogate pair, `"\ud800"` alone, which no `Str` can hold ("lone surrogate \ud800 at byte 1"); an object that repeats a key, which parsers disagree on and I-JSON (RFC 7493) forbids ("duplicate key `id` at byte 8"); more than 512 arrays and objects inside each other ("nested deeper than 512 at byte 512"); and a number longer than 400 characters ("number longer than 400 characters at byte 0"). It reads nesting with a list rather than recursion, so even the deepest document it accepts fits a task's stack. `decode` reads through `parse`, so the same text is refused there |
| `stringify(v) -> Str` | compact JSON; integral numbers print without `.0`, others in the fewest digits (17 at most) that read back as the same `Float`; NaN and the infinities, which JSON cannot write, print as `null`. An `Obj` is written as it is, so one built with a key twice gives text that `parse` refuses; `encode` never does that, since a struct's fields and a map's keys are distinct. Like `parse`, it tracks the open arrays and objects in a list, so a `Value` of any depth fits a task's stack |
| `quote(s) -> Str` | a JSON string literal |
| `v.get(key)`, `v.at(i)` | methods: `Option[Value]`, an object member or an array item |
| `v.as_str()`, `as_num()`, `as_int()`, `as_bool()`, `as_list()` | methods: `Option` of the payload, `None` for another kind; `as_num` reads an `Int` too, and `as_int` a `Num` that is a whole number |
| `encode(x) -> Str` | the JSON text for any value whose type is made of `Int`, `Float`, `Str`, `Bool`, `Value`, `Option`, `List`, `Map` with `Str` keys, tuples, and structs and enums of those. A struct is an object with its fields in declared order; `None` is `null`; a tuple is an array; an enum's unit variant is its name as a string, `"Point"`, and a variant with a payload an object with one member, `{"Circle": 1.5}`, or for several items `{"Rect": [2, 3]}` |
| `decode(s) -> Result[T, Str]` | reads such a value back: `let t: Todo = json.decode(body)?`. Members the struct does not have are ignored; a missing one reads as `null`, so an `Option` field may be left out and any other is an error. The error says where: "`todos[2].title`: expected a string, found a number", "`id` is missing" |

The compiler writes the conversions for each type `encode` and `decode` are used with, as ordinary functions, so the type must be known where they are used: `let r = json.decode(s)` alone, or `json.encode(x)` inside a generic function where `x` has a type parameter's type, is E0212. So is a type with no JSON form, one holding a capability, a function, a channel, `Bytes`, an array or a `dyn` value, with the message saying which part. For a type that holds itself (a `Node` whose `next` is an `Option[Node]`, an enum list, a tree), they keep their place in a list rather than recursing once per level: `encode` writes such a value however deep it is, and `decode`, which reads at most 512 levels (`parse`), takes the same small stack at any depth, so both work inside a task.

### http

HTTP/1.1, plain or over TLS. The server keeps connections alive between requests and reads pipelined requests in order; bodies can be streamed in pieces both ways. The client uses TLS for `https://` URLs; the server uses TLS when its listener came from `Listen.on_tls`, and nothing else changes.

| Item | Meaning |
| --- | --- |
| `struct Header { name, value }`, `req.header(name)`, `resp.header(name)`, `find_header(headers, name)` | headers; lookup ignores case |
| `struct Request { method, path, version, headers, body }` | `version` is `"HTTP/1.1"` or `"HTTP/1.0"` |
| `struct Response { status: Int, headers, body }`, `text(status, body)`, `json(status, body)`, `reason(status)` | |
| `get(net, url)`, `post(net, url, content_type, body)`, `request(net, method, url, headers = [], body = "")` | `Result[Response, IoError]`; `http://` and `https://` URLs, read with the `url` module: the request names the path and query (`/` when there is no path), never the `#fragment`, which stays with the client; an IPv6 host is written in brackets (`http://[::1]:8080/`); a URL `url.parse` rejects is an error with its message. Chunked responses decoded, one connection per request. Unless `headers` has an Accept-Encoding, the request says `Accept-Encoding: gzip`, and a body sent with `Content-Encoding: gzip` or `deflate` is decompressed with the `compress` module (up to 64 MiB; damaged data is an error), the response then leaving out Content-Encoding and Content-Length, which describe the bytes sent |
| `request_fold(net, method, url, headers, body, init, f)` | `Result[(Response, T), IoError]`: the body is passed to `f: fn(T, Str) -> T` piece by piece as it arrives instead of being held in memory, as it was sent (not decompressed: it asks for no encoding unless `headers` does); the response comes back with an empty body |
| `request_lines(net, method, url, headers, body, f)` | `Result[Response, IoError]`: for a stream that may never end (a Kubernetes watch, server-sent events, a log follow), each line of a 2xx body is passed to `f: fn(Str) -> Bool` as soon as it has arrived, without its `\n` or `\r\n`; `f` returns `false` to stop, and the connection is closed. Any other status is returned with its whole body, and `f` is not called |
| `serve_forever(listener, handler) -> IoError` | serve every connection in its own task until the listener is closed (by another task holding it) or fails; returns that error. The tasks are an `interleaved` group (section 10): what a handler prints goes out at once |
| `route(request, routes) -> Response` | answer with the first `Route` whose pattern matches the method and path, calling its handler; 404 when no path matches, 405 with an `Allow` header when a path matches but no method does (below) |
| `error_page(status, detail) -> Response`, `accepts_html(request) -> Bool` | an HTML page for an error status in the design of limen-lang.org, light or dark as the visitor's system is, with nothing fetched; `detail` is escaped. `accepts_html` is true when the Accept header names `text/html`. `route`'s 404 and 405 and the server's 500 for a handler that panics are this page when the request accepts HTML, and plain text (`not found`, `method not allowed`, `internal error`) otherwise; 400 and 408 for a request that could not be read are always plain text |
| `on(pattern, handler) -> Route`, `type Route = (Str, fn(Request, Params) -> Response)` | a route, its pattern checked now (a malformed one panics) |
| `struct Params`, `p.get(name) -> Option[Str]`, `p.int(name) -> Option[Int]` | what a pattern captured, percent-decoded; `int` is `None` unless the value is a whole number |
| `serve(listener, count, handler) -> Int` | the same for `count` connections; returns how many requests were answered |
| `serve_conn(conn, handler) -> Int` | answer requests on one connection until the client closes it or asks to (`Connection: close`, or HTTP/1.0 without keep-alive); 400 and close on a malformed request, 500 if the handler panics, headers only for HEAD |
| `handle(conn, handler) -> Bool` | answer one request, then close |
| `accept_forever(listener, f) -> IoError` | run `f: fn(Conn) -> Int` on each accepted connection in its own task, until accepting fails; an `interleaved` group too |
| `serve_stream(conn, handler) -> Int` | like `serve_conn` for a handler `fn(Request, Conn) -> Result[(), IoError]` that writes its own response; an error or panic closes the connection |
| `server(clock) -> Server`, `struct Server { clock, head_ms, body_ms, write_ms, idle_ms, conn_ms }` | time limits for a server's connections (below), defaults 10 s, 30 s, 30 s, 60 s and 10 minutes; `0` turns one off |
| `s.serve_forever(listener, handler)`, `s.serve(listener, count, handler)`, `s.serve_conn(conn, handler)`, `s.serve_stream(conn, handler)` | the functions of the same name, within the limits of `Server` `s` |
| `write_head(conn, status, headers)`, `write_chunk(conn, piece)`, `end_body(conn)` | write a response whose body follows in pieces (`Transfer-Encoding: chunked`) |
| `read_request(conn)`, `write_response(conn, r)` | one message on a connection; `write_response` says the connection will close |

Messages are read strictly, against request smuggling (RFC 9112): lines end in CRLF, and a bare CR or LF, a NUL, a folded header line, a header name that is not a token (`Content-Length : 5`), a control byte in a header value, a Content-Length that is not digits or two that differ, a Transfer-Encoding other than `chunked`, one beside a Content-Length, or one in an HTTP/1.0 request is a malformed request (400, connection closed). The client refuses the same in a response, except that a Transfer-Encoding not ending in `chunked` reads to the end of the connection. Writing is strict too: `write_response`, `write_head` and the client return an error, with nothing sent, for a header name that is not a token, a value holding CR, LF, NUL or another control byte (response splitting), a status outside 100 to 999, a Transfer-Encoding header on a whole message, or a Content-Length that is not digits; `write_head` also refuses Content-Length and Transfer-Encoding, since it frames the body itself. A handler of `serve_conn` or `handle` whose response cannot be sent gets 500 instead.

Limits: request heads up to 64 KiB, request bodies up to 8 MiB (read whole, chunked or not), 1000 requests per connection.

Time limits come from a `Server`, `http.server(root.clock)`, whose methods serve as the functions do. Without one, a client may take as long as it likes, so one that sends a byte now and then (slowloris) holds a task forever: use a `Server` for anything that listens on a network. Its limits: `head_ms` for a request's line and headers, from when the server starts waiting for them (the first request) or from its first byte (later ones); `body_ms` for the body after the headers; `write_ms` for sending the response; `idle_ms` for a kept-alive connection waiting for its next request; `conn_ms` for the whole connection, handlers included, after which its task is cancelled. A request that runs out of time once it has started gets 408 and the connection closes; a client that sends nothing, or stays idle too long, is closed without a reply. The reads and writes use `Conn.read_within` and `write_within`, so the limits cost about nothing per request; `conn_ms` is a group with a deadline per connection. `serve_forever` runs each connection as a task that returns `()`, which the group frees as soon as it ends (section 10), so a server's memory stays flat however long it runs.

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

The handler is a plain function value, so it gets no capability unless a closure captures one.

A route pattern is a method, one space and a path (`"GET /todos/:id"`), or a path alone for any method. The path starts with `/`; in it `:name` matches one segment and `*name`, which must be last, matches the remaining segments joined with `/`, possibly none. `route` drops the query string, splits the path on `/`, ignores empty segments (so `/todos/` is `/todos`) and percent-decodes each one, answering 400 if an escape is malformed; literal segments are compared after decoding, and `..` is a segment like any other. Routes are tried in order and the first whose path and method match wins; a GET route also answers HEAD, and 405's `Allow` lists the methods of every route whose path matched, with HEAD when GET is there. The 404 and 405 bodies are plain text. A malformed pattern (no leading `/`, a method that is not a token, more than one space, `*name` before the end) panics in `on`, or in `route` for a tuple built by hand.

```
use http

fn routes(db: sql.Db) -> List[http.Route] {
    [
        http.on("GET /todos", |r, p| list(db)),
        http.on("GET /todos/:id", |r, p| match p.int("id") {
            Some(id) => show(db, id),
            None => http.text(404, "no such todo"),
        }),
    ]
}

fn main(root: Root) -> Result[(), IoError] {
    let api = routes(sql.open(root.fs, "todos.db")?)
    let l = root.listen.on("127.0.0.1:8080")?
    Err(http.serve_forever(l, |r| http.route(r, api)))
}
```

A streamed response:

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
| `grouped(number, sep = ",")` | `sep` between groups of three digits in the integer part of a number already written out: `grouped(x.to_fixed(2), " ")` is `"1 234 567.89"` |
| `zero_pad(n, width)` | zeros after the sign up to `width` characters: `zero_pad(-7, 4)` is `"-007"` |
| `percent(x, decimals)` | `percent(0.4567, 1)` is `"45.7%"` |
| `hex(n)`, `base(n, radix)` | lowercase digits in base 16 or 2 to 36, with `-` for negatives: `hex(255)` is `"ff"`; another radix panics |
| `parse_base(s, radix)` | `Option[Int]`: digits in that base (either case) with an optional sign; `None` on anything else or overflow |
| `align_left(s, w)`, `align_right(s, w)`, `align_center(s, w)` | `s` padded with spaces to `w` characters for table columns; counts code points, not bytes |
| `width(s)` | the number of code points in `s` |

### regex

Regular expressions that never backtrack: a pattern compiles to a program for a Pike VM, which follows every way the pattern can match at once, so a search takes time proportional to the pattern size times the text length, whatever the pattern. `(a+)+$` on a long run of `a`s is as fast as any other pattern. Written in Limen.

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

The syntax is a subset of RE2's: literals; `.` (any character but newline); classes `[abc]`, `[^a-z]`, `[\d_]`; `\d \w \s` and `\D \W \S` (ASCII); `\b \B` word edges; `^ $` (text start and end) and `\A \z`; `* + ? {n} {n,} {n,m}` (counts up to 1000), each lazy with a trailing `?`; `( )` captures, `(?: )` groups without capturing, `(?P<name> )` or `(?<name> )` name a capture, nested at most 250 deep (each capture inside another costs more at every byte searched); `|`; escapes `\n \t \r \f \v \0 \xHH` and `\` before any punctuation. Flags `(?i)` (ASCII case-insensitive), `(?m)` (`^` and `$` match at lines) and `(?s)` (`.` matches newline) apply from where they appear to the end of the pattern. Text is UTF-8: `.` and negated classes match whole characters, and a non-ASCII character is one unit for a quantifier; a byte of a pattern that does not start a well-formed character is a unit of its own, as `chars()` reads it, so `escape(s)` matches `s` whatever bytes it holds. Classes may list non-ASCII characters but not ranges of them, and a negated class cannot list them. Backreferences and lookaround are not supported, since they need backtracking. A Limen string needs `\{` for a literal brace, so `\d{4}` is written `"\\d\{4\}"`.

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
| `parse_iso(s) -> Result[Int, Str]` | a date (midnight UTC) or date and time joined by `T` or a space, with optional seconds and fraction, then `Z`, an offset (`+01:00`, `+0100`, `+01`) or nothing (UTC); the error names the byte. A time an offset moves outside the years 0 to 9999 in UTC is an error, so `iso` can always write it back |
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
| `within(clock, ms, f) -> Result[T, IoError]` | runs `f: fn() -> Result[T, IoError]` with a deadline: its result, or `Err(Failed("timed out after {ms} ms"))`. `f` runs as a task of a group with that deadline (section 10), so at the deadline it stops at its next wait or checkpoint, its defers run and its sockets are closed. With `load_zone`, one of the two functions here that take a capability |

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

`repeat(s, n)`, `pad_left(s, width, fill = " ")`, `pad_right(s, width, fill = " ")`, `count(s, sub)`, `words(s)`, and byte tests `is_digit(b)`, `is_alpha(b)`, `is_space(b)` for use with `bytes()`.

### sql

SQL databases through SQLite, built on foreign functions (section 9) and the system's `libsqlite3` (on macOS it is part of the OS; on Linux install the SQLite development package). The module is trusted to call SQLite itself, so a program needs no foreign grant: `--grant fs=DIR` for a database file, and nothing for `sql.memory()`.

```
use sql

fn main(root: Root) -> Result[(), IoError] {
    let db = sql.open(root.fs, "app.db")?          // --grant fs=DIR
    db.exec("CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT NOT NULL, age INTEGER)")?
    db.exec("INSERT INTO users (name, age) VALUES (?, ?)", [sql.Text("Ada"), sql.Int(36)])?
    for row in db.query("SELECT id, name, age FROM users WHERE age > ?", [sql.Int(30)])? {
        root.stdout.print("{row.int("id") ?? 0} {row.text("name") ?? ""}")
    }
    db.close()
}
```

- `sql.open(fs, name)` opens or creates the database file `name` under the directory `fs` grants (the name is walked from the `Fs` as every `Fs` operation is), read-only when `fs` is read-only (the file must then exist). `sql.memory()` is a fresh in-memory database. Both return `Result[Db, IoError]`: `Denied` when the path is outside the grant, `Failed` with SQLite's message otherwise.
- `Db` is a plain value naming the connection, so `db.exec(text, params)` and `db.query(text, params)` take nothing else. It holds a random id in the runtime's handle table, not the connection's pointer, so a `Db` cannot be forged; code that holds one can change that database and nothing else. It can be passed to tasks, which share the one connection safely (SQLite serialises it). For queries that run in parallel, give tasks connections of their own. `db.close()` closes it once operations running in other tasks finish; after that every operation on it, and a second close, is `Err(Failed("sql: the database is closed"))`. One that is never closed stays open until the program exits.
- Values go in as parameters, never into the SQL text: each `?` takes the next item of `params`, a `List[sql.Value]`, which is `sql.Null`, `sql.Int(n)`, `sql.Float(x)`, `sql.Text(s)` or `sql.Blob(bytes)`. The number of `?`s must match the list.
- `exec` returns the number of rows changed. With no parameters its text may hold several statements separated by `;` (a schema); with parameters it must be exactly one.
- `query` runs one statement and returns every row as a `List[sql.Row]`; `query_one` returns the first, as an `Option`. A row holds `columns` (names) and `values`. `row.get(name)` is the `Value`; `row.int(name)`, `row.float(name)` (an integer reads as a float too), `row.text(name)` and `row.blob(name)` are `Option`s, `None` for NULL; `row.is_null(name)`. Asking for a column the row does not have, or reading one as the wrong type, panics: that is a bug in the query or the code, not a condition to handle.
- `params` defaults to `[]` on `exec`, `query`, `query_one`, `all` and `one`: `db.exec("CREATE TABLE ...")`.
- **Rows as structs.** `db.all(text, params)` returns every row read into a struct, `db.one(text, params)` the first as an `Option`, and `sql.decode(row)` reads one row; `sql.values(x)` is a struct's fields as parameters, in the order the struct declares them:
  ```
  struct User { id: Int, name: Str, age: Option[Int] }

  let users: List[User] = db.all("SELECT id, name, age FROM users WHERE age > ?", [sql.Int(30)])?
  db.exec("INSERT INTO users (id, name, age) VALUES (?, ?, ?)", sql.values(u))?
  ```
  Each field reads the column of its name, so a query names the columns the struct has (`SELECT *` works when they match). A field is `Int`, `Float` (an integer column reads as a float too), `Str`, `Bool` (stored as `0` or `1`), `Bytes`, or an `Option` of one of those, `None` for NULL. A column the row does not have, NULL in a field that is not an `Option`, and a value of the wrong type are `Err(Failed(...))` naming the column and the field: rows come from outside the program, so unlike `row.int(name)` this does not panic. As with `json.encode`, the compiler writes the conversion for each struct it is used with, so the struct must be known where the call is (E0212); a type that is not a struct, or a field of another type, is E0212 too.
- `db.transaction(|| { ...; Ok(v) })` runs the closure between `BEGIN` and `COMMIT`. If it returns `Err` or panics, the transaction is rolled back and the error (or the panic) goes on. Transactions do not nest: starting one inside another is an `Err`.
- `db.last_id()` is the rowid of the last row inserted through the connection, as a `Result[Int, IoError]`.
- SQLite stays inside the grants. `ATTACH` is turned off, and with it `VACUUM` (which attaches a temporary database; `VACUUM INTO` would write anywhere), so SQL cannot open other files. The database file is not opened through a symlink, and `open` refuses a database whose `-journal`, `-wal` or `-shm` file is a symlink out of the granted directory (or a broken one), since SQLite follows those. URI filenames and extensions are off (SQLite's defaults, which the module never changes), and temporary tables and sorts stay in memory. SQLite itself does not follow a link another process plants in place of a side file after the open: the next write fails with "unable to open database file" and the link's target is untouched. Nor does it write once the database's directory has been swapped for a link to another directory ("attempt to write a readonly database"), because it notices the database moved. One case is left: a process that can write both inside the granted directory and in a directory outside it on the same filesystem can hard-link the database into the outside directory and swap the database's directory for a link to it, and SQLite then creates its rollback journal outside the grant. The operating-system sandbox (Landlock, the macOS profile) refuses such a write outside the grant, so Limen leaves that case to it. SQLite writes its journal next to the database, inside the granted directory. A busy database is waited on for up to 5 seconds.
- A database in WAL mode keeps its `-wal` and `-shm` files after the last connection closes, on every platform, so a read-only `Fs` can still open it.
- A statement runs as one call into the runtime, on the blocking-call pool (section 9): it is prepared, bound, stepped through every row and finalized there, and the rows come back together. A slow query, or a wait of up to 5 seconds for a busy database, holds up only its own task, and a row costs no round trip between threads (a scan of 102,400 rows takes about 15 ms natively). Opening and closing run on the pool too. On a connection shared by tasks, SQLite runs one statement at a time; the others wait for it on the pool.
- A task cancelled during a query (a group deadline, `time.within`) stops it: the statement gives up at its next row, within a thousand of SQLite's instructions while it computes one, or within a millisecond while it waits for another connection's lock, and the task waits for that before it unwinds, so the connection is never used after the task let go and stays usable. An open transaction is rolled back. Only the cancelled task's statement stops: other tasks' statements on the same connection go on. A cancelled task whose statement is still waiting for the connection waits with it, and its statement stops as soon as it starts.

### postgres

Postgres through its wire protocol (version 3), written in Limen: no C library, and nothing to grant but the server's host (`--grant net=db.internal`).

```
use postgres

struct Todo { id: Int, title: Str, done: Bool }

fn main(root: Root) -> Result[(), IoError] {
    let db = postgres.connect(root.net, root.rand, "postgres://app:secret@db.internal/app")?
    db.exec("CREATE TABLE IF NOT EXISTS todos (id SERIAL PRIMARY KEY, title TEXT NOT NULL, done BOOLEAN NOT NULL DEFAULT false)")?
    db.exec("INSERT INTO todos (title) VALUES ($1)", [postgres.Text("write")])?
    let todos: List[Todo] = db.all("SELECT id, title, done FROM todos WHERE id > $1", [postgres.Int(0)])?
    db.close()
}
```

- `postgres.connect(net, rand, address, weak_login = false)` connects and logs in. `address` is `postgres://user:password@host:port/database` (`postgresql://` too; percent-escapes in the parts are decoded); the port defaults to 5432 and the database to the user's name. Logging in takes SCRAM-SHA-256 (Postgres's default since 14), and the server must prove it knows the password too: a server that skips that step is refused. `Rand` gives SCRAM's nonce. MD5 and cleartext logins send what an impostor posing as the server could reuse or read, so they are refused unless the call asks for them with `weak_login: true` (on `pool` too). A refused login is `Err(Failed("postgres: password authentication failed for user \"app\" (28P01)"))`, the server's message and its SQLSTATE code, as every server error is.
- The connection is TLS, with the server's certificate checked against the address's host, unless the address says `?sslmode=disable`. Postgres asks for TLS inside the plain connection (an SSLRequest), and the client starts it with `conn.start_tls`; a server that does not offer TLS is an error saying to add `?sslmode=disable`. Every other `sslmode` (`require`, `verify-full`, `prefer`, ...) means TLS with the certificate checked, the only TLS Limen speaks.
- `exec(text, params = [])`, `query`, `query_one`, `all`, `one`, `transaction` and `close` are as in `sql`, with Postgres's placeholders: `$1`, `$2`, ... take `params` in order. Without parameters a statement goes as one simple query, so `exec` may hold several statements separated by `;`; with them it goes as Parse, Bind, Execute and Sync, each value as text. `exec` returns the rows the statement changed, from the server's `INSERT 0 3`.
- `postgres.Value` has the same variants as `sql.Value`: `Null`, `Int`, `Float`, `Text`, `Blob`. A column reads by its type: `bool` as `Int(1)` or `Int(0)`, `smallint`, `integer`, `bigint` and `oid` as `Int`, `real` and `double precision` as `Float`, `bytea` as `Blob`, and everything else (`numeric`, `text`, dates, `json`) as `Text`, the server's text for it. A struct's `Bool` field goes as the text `true` or `false`.
- `postgres.decode(row)`, `db.all(text, params)`, `db.one(text, params)` and `postgres.values(x)` convert rows and structs as `sql`'s do: the compiler writes the conversion for each struct, and a row that does not fit is an `Err` naming the column and the field.
- A `Db` is one connection, used by one task at a time: a statement is a request and its whole answer. `postgres.pool(net, rand, address, size, weak_login = false)` opens `size` connections and returns them in a `Chan[Db]`; `postgres.using(pool, |db| ...)` takes one, waiting while all are in use, runs the closure, and puts the connection back however the closure ends. A server shares the pool with every request.

### redis

Redis through its protocol (RESP2), written in Limen: no C library, and nothing to grant but the server's host.

```
use redis

let cache = redis.connect(root.net, "cache.internal:6379")?
cache.set("todo:1", json.encode(t), ttl: 60)?
match cache.get("todo:1")? {
    Some(text) => ...,
    None => ...,
}
```

- `redis.connect(net, address, password = "", user = "")` connects to `address`: `host:port` or `redis://host:port` in plain TCP, `rediss://host:port` over TLS with the server's certificate checked (the port defaults to 6379). It logs in with `AUTH` when given a password (and a user, for an ACL user).
- Methods for the common commands: `get(key) -> Option[Str]`, `set(key, value, ttl = 0)` (with `ttl`, `SET ... EX`), `del(key) -> Bool`, `exists(key) -> Bool`, `incr(key, by = 1) -> Int`, `expire(key, seconds) -> Bool`, `ttl(key) -> Int` (-1 for no expiry, -2 for no key), `publish(channel, message) -> Int` and `ping() -> Bool`. `run(words)` sends any other command, as its words, and returns the reply, a `redis.Reply`: `Simple(Str)`, `Number(Int)`, `Bulk(Str)`, `Nil`, `Many(List[Reply])`. Each word goes as its own bulk string, so keys and values may hold any bytes. An error reply (`-ERR ...`) is `Err(Failed("redis: ..."))`.
- A `Client` is one connection, used by one task at a time. `redis.pool(net, address, size, password = "", user = "")` and `redis.using(pool, |c| ...)` lend connections to tasks as `postgres.pool` does.

### stream

Files larger than memory, a piece at a time, in constant memory. Each function folds: it hands every piece to `f` with what `f` returned for the piece before, from `init`, and returns what `f` returned last (or the reading error). The file is read with `fs.read_at`, so nothing stays open between pieces.

```
let errors = stream.fold_lines(root.fs, "app.log", 0, |n, line| if line.contains("ERROR") { n + 1 } else { n })?
let total = stream.fold_gzip_lines(root.fs, "big.log.gz", 0, |sum, line| sum + line.len())?
```

| Item | Meaning |
| --- | --- |
| `fold_chunks(fs, path, init, f, size = 65536)` | every chunk of `size` bytes (the last may be shorter): `f: fn(T, Bytes) -> T` |
| `fold_lines(fs, path, init, f)` | every line: `f: fn(T, Str) -> T`. Lines end at `\n`, a `\r` before it is dropped, and a last line without a newline counts too |
| `fold_gzip_chunks(fs, path, init, f)`, `fold_gzip_lines(fs, path, init, f)` | the same over what a gzip file holds, decompressed as it is read (`compress.fold_gunzip`), every member's checksum checked. A 132 MB log, 18 MB gzipped, goes through in 2.1 s with 9.6 MB of memory, natively |
| `each_line(fs, path, f)` | runs `f: fn(Str)` on every line, for its effect |

### websocket

WebSockets (RFC 6455), written in Limen over a `Conn`.

- A server upgrades a request: `websocket.accept(r, conn)` takes the request and connection `http.serve_stream` gives a handler, answers 101 with the `Sec-WebSocket-Accept` the protocol asks for, and returns the server's `Socket`. A request that is not an upgrade (see `websocket.is_upgrade(r)`) gets 400 and is an `Err`. An upgraded connection runs as long as both ends like, so serve it with `http.serve_stream`, not a `Server`, whose time limits would close it.
- A client connects: `websocket.connect(net, rand, "ws://host:port/path")`, or `wss://` over TLS with the certificate checked. It checks the server's `Sec-WebSocket-Accept`, and masks every frame with a random key from `rand`, as the protocol requires of clients.
- `ws.send(text)`, `ws.send_bytes(data)`, `ws.ping(data = "")` and `ws.close(code = 1000, reason = "")` send; each is `Result[(), IoError]`.
- Receiving is a fold: `websocket.fold_messages(ws, init, f, max = 16_777_216)` hands every message, a `websocket.Message` (`Text(Str)`, `Binary(Bytes)` or `Close(code, reason)`), to `f` with what `f` returned before, until the connection closes, and returns what `f` returned last. On the way it joins fragmented messages, answers pings, ignores pongs, and answers a close (handing it to `f` as `Close`). `f` may send: an echo server sends from inside it. A message larger than `max` bytes is refused as soon as its length is known: the connection closes with 1009 and the fold is an `Err`.

### hex

Binary data as hexadecimal text, two digits per byte, as checksums and keys are written. Pure functions; for a number in hex, `format.hex(n)`.

| Item | Meaning |
| --- | --- |
| `encode(data: Bytes) -> Str`, `encode_upper(data)`, `encode_str(s)` | lowercase (or uppercase) digits: `encode(bytes([1, 171]))` is `"01ab"`; `encode_str` takes the bytes of a `Str` |
| `decode(s) -> Result[Bytes, Str]`, `decode_str(s) -> Result[Str, Str]` | digits in either case; an odd number of digits, or anything else, is an error naming the byte: `hex: not a hex digit at byte 3` |
| `digit(b: Int) -> Int` | the value of one hex digit's byte, in either case, or -1: `digit(97)` (`a`) is 10 |

### base64

Binary data as text of 64 characters, four for every three bytes (RFC 4648). The standard alphabet ends in `+` and `/` and pads with `=`; the URL-safe one ends in `-` and `_`, so the text can go in URLs and file names, and is usually written without padding, as JSON Web Tokens do. Pure functions.

| Item | Meaning |
| --- | --- |
| `encode(data: Bytes) -> Str`, `encode_str(s)` | the standard alphabet, padded: `encode_str("hi")` is `"aGk="` |
| `encode_url(data)` | the URL-safe alphabet, unpadded: `"aGk"` |
| `encode_with(data, url: Bool, pad: Bool)` | either alphabet, padded or not |
| `decode(s) -> Result[Bytes, Str]`, `decode_str(s) -> Result[Str, Str]`, `decode_url(s)` | padding is optional but, if there is any, must be right. Anything outside the alphabet (whitespace included), a misplaced `=`, a lone last character or leftover bits that are not zero is an error naming the byte: `base64: `-` belongs to the URL-safe alphabet at byte 4` |

### hash

Cryptographic hashes of `Bytes`: SHA-256 (FIPS 180-4) and HMAC-SHA-256 (RFC 2104) for checksums, signatures and keys, and SHA-1 and MD5 only for formats that still name them (git object ids, TOTP, ETags), since both are broken for security. Digests are `Bytes`; the `_hex` functions give lowercase hex, as `shasum` prints it. Pure functions, written in Limen with 32-bit words in `Int`s; natively SHA-256 runs at about 200 MB/s.

| Item | Meaning |
| --- | --- |
| `sha256(data) -> Bytes`, `sha256_hex(data) -> Str` | the 32-byte digest: `sha256_hex("abc".to_bytes())` is `"ba7816bf...f20015ad"` |
| `hmac_sha256(key, data) -> Bytes`, `hmac_sha256_hex(key, data)` | the MAC of `data` under `key`, any length of key (one over 64 bytes is hashed first, as the RFC says) |
| `sha1(data)`, `sha1_hex(data)`, `hmac_sha1(key, data)` | SHA-1: 20 bytes |
| `md5(data)`, `md5_hex(data)` | MD5 (RFC 1321): 16 bytes |
| `sha256_new()`, `sha1_new()`, `md5_new()`, `hmac_sha256_new(key)`, `hmac_sha1_new(key)`, `new(algo) -> Hasher` | a hash computed a piece at a time, fed nothing yet; `algo` is `hash.Sha256`, `hash.Sha1` or `hash.Md5` |
| `h.update(data) -> Hasher` | the hasher fed `data` after what it was fed before: any split of the data gives the same digest. Whole 64-byte blocks are hashed at once, and only an unfinished block's bytes are kept |
| `h.finish() -> Bytes`, `h.finish_hex() -> Str` | the digest (or MAC) of everything fed, as the one-shot function gives it |
| `equal(a, b) -> Bool` | whether two byte strings are equal, in a time that does not depend on where they differ; compare a MAC or token you were given with it, not `==`, so the time taken does not tell an attacker how much of a guess was right |
| `hex(data) -> Str` | bytes as lowercase hex |

```
use hash

fn sign(secret: Str, body: Str) -> Str {
    hash.hmac_sha256_hex(secret.to_bytes(), body.to_bytes())
}

fn verify(secret: Str, body: Str, mac: Bytes) -> Bool {
    hash.equal(hash.hmac_sha256(secret.to_bytes(), body.to_bytes()), mac)
}
```

A `Hasher` is a plain value, like any other: `update` returns the next one and leaves the old one as it was, so data that arrives in chunks is hashed with `h = h.update(chunk)` (which updates in place, as the old value is not used again), and a common prefix can be fed once and finished with different endings. The one-shot functions are a hasher fed once.

```
fn checksum(chunks: List[Bytes]) -> Str {
    let mut h = hash.sha256_new()
    for c in chunks {
        h = h.update(c)
    }
    h.finish_hex()
}
```

### crypto

Encryption and signatures beyond TLS, on OpenSSL as TLS is. The algorithms are fixed, so there is nothing to choose wrong: ChaCha20-Poly1305 for sealing (RFC 8439), Ed25519 for signatures (RFC 8032) and X25519 for agreeing a key (RFC 7748). Keys are plain `Bytes`; keep secret ones out of logs, since printing shows them. Fresh keys and nonces come from `Rand`, so a function that makes one takes it.

| Item | Meaning |
| --- | --- |
| `key(rand) -> Bytes` | a fresh 32-byte secret key for `seal` and `open` |
| `seal(rand, key, plaintext, associated: Str = "") -> Bytes` | encrypted and authenticated under a fresh random nonce: 12 bytes of nonce, the ciphertext, a 16-byte tag. `associated` is authenticated but not encrypted (a header, a record's id); `open` needs the same |
| `open(key, sealed, associated = "") -> Result[Bytes, Str]` | what `seal` sealed, or an Err when the key or the associated data differs or any byte was changed |
| `encrypt(key, nonce, plaintext, associated = "")`, `decrypt(...)` | ChaCha20-Poly1305 with a nonce you choose, for another program's format; a nonce must never repeat under one key |
| `signing_key(rand)`, `public_key(signing)`, `sign(signing, message) -> Bytes`, `verify(public, message, signature) -> Bool` | Ed25519: a 32-byte secret, its 32-byte public key, 64-byte signatures |
| `agreement_key(rand)`, `agreement_public(private)`, `agree(private, peer_public) -> Result[Bytes, Str]` | X25519: both sides get the same 32-byte secret from their own private key and the other's public key; hash it with what the two sides agreed on before using it as a key. An all-zero result (a bad public key) is an Err |

A key of the wrong size is a panic where it was passed. The interpreter and native programs give the same bytes; both are checked against the RFCs' test vectors.

```
use crypto

fn main(root: Root) {
    let key = crypto.key(root.rand)
    let sealed = crypto.seal(root.rand, key, "a secret".to_bytes())
    root.stdout.print("{crypto.open(key, sealed).map(|b| b.to_str())}")      // Ok("a secret")
}
```

### csv

CSV as RFC 4180 has it: fields separated by commas, quoted with `"` when they hold a comma, a quote (written twice) or a line break, and lines ending in `\n` or `\r\n`. Reading is a little lenient: rows may differ in length (`parse_maps` checks them against the header), blank lines are skipped, and a UTF-8 byte order mark at the start is dropped. Pure functions.

| Item | Meaning |
| --- | --- |
| `parse(text) -> Result[List[List[Str]], Str]` | the rows, each a list of fields: `parse("a,b\n1,\"x, y\"\n")` is `Ok([["a", "b"], ["1", "x, y"]])`. The error names the line, counting the lines inside quoted fields: `csv: line 3: a quoted field is not closed` |
| `parse_with(text, sep)` | the same with another separator, one byte that is not a quote or a line break: `"\t"` for TSV, `";"` |
| `parse_maps(text) -> Result[List[Map[Str, Str]], Str]` | the first row names the fields, and each later row is a map from those names to its fields, in the header's order. A row with another number of fields, or a header naming a field twice, is an error |
| `write(rows) -> Str`, `write_with(rows, sep)` | one line per row, each ending in `\n`; fields are quoted only when they need it (and when they start with a byte order mark, which reading skips at the start of the text), so `parse(write(rows))` gives `rows` back for rows of at least one field |
| `write_maps(header, rows) -> Str` | a header line, then each map's values in the header's order, `""` for a name the map lacks |
| `escape(field) -> Str` | one field as it goes in a line: `escape("a,b")` is `"\"a,b\""` |

### unicode

Text beyond `chars()`, from Unicode 17.0: case folding for comparing text people typed, full upper and lower case, and the normal forms NFC and NFD, so the same text typed two ways compares equal. Pure functions; the tables are generated as functions, so a native program looks a code point up with one `switch` and builds nothing when it starts.

| Item | Meaning |
| --- | --- |
| `code_points(s) -> List[Int]`, `from_code_points(cps) -> Str` | the code points of the UTF-8 text, and back; a malformed byte reads as U+FFFD |
| `fold(s)` | case-folded, the form to compare in when case does not matter: `fold("Straße") == fold("STRASSE")` |
| `upper(s)`, `lower(s)` | the full mappings, which can change the length: `upper("ß")` is `"SS"`, where `Str.to_upper` changes ASCII alone. Per code point, so a final sigma lowers to `σ` |
| `nfd(s)`, `nfc(s)` | taken apart into base characters and combining marks in their canonical order, and put back together into the fewest characters: `nfc("e\u{301}") == "\u{e9}"` |
| `same_ignoring_case(a, b) -> Bool` | `fold(nfd(a)) == fold(nfd(b))` |

There are no compatibility forms (NFKC, NFKD) and no locale rules, such as Turkish dotted and dotless i.

### url

URLs (RFC 3986): parsing one into its parts and back, percent-encoding, and query strings with repeated keys. Pure functions.

| Item | Meaning |
| --- | --- |
| `struct Url { scheme, user, password, host, port: Option[Int], path, query, fragment }` | `scheme` lowercased, `host` without the brackets of an IPv6 address, `query` without its `?`, `fragment` without its `#`; a part that is not there is `""` (or `None`). `path`, `query` and `fragment` stay percent-encoded as written, since decoding `%2F` would change a path's meaning; `user` and `password` are decoded |
| `parse(s) -> Result[Url, Str]` | an absolute URL or a reference without a scheme (`/a?b`, `//host/x`); a bad scheme, a port that is not a number up to 65535, an unclosed `[`, a `[` or `]` in a host not in brackets, a space in the host or a control character is an error naming the byte |
| `u.to_str()` | the URL as text, `user` and `password` encoded; `parse(s)?.to_str()` gives `s` back for URLs in the usual form, and always text that `parse` reads as the same `Url` (a path starting with `//` gets an empty host before it, `////x`) |
| `u.params() -> Result[List[(Str, Str)], Str]` | the query's pairs, as `parse_query` reads them |
| `resolve(base: Url, ref: Str) -> Result[Url, Str]` | the URL that the reference `ref` names in the document at `base`, as RFC 3986 section 5.2 resolves it (a link's `href` made absolute): a reference with a scheme stands alone, `//host/x` keeps the base's scheme, `/x` replaces the path, `x` and `../x` go from the base's directory, `?q` and `#f` change only the query or the fragment. Dot segments are removed, and `..` stops at the root: with the base `http://a/b/c/d;p?q`, `"../g"` is `http://a/b/g` and `"../../../g"` is `http://a/g`. An error if `ref` does not parse or `base` has no scheme |
| `resolve_str(base: Str, ref: Str) -> Result[Str, Str]` | the same on text: `resolve_str("https://x.org/docs/", "../img/a.png")` is `Ok("https://x.org/img/a.png")` |
| `remove_dot_segments(path) -> Str` | the path without its `.` and `..` segments (RFC 3986 section 5.2.4): `"/a/b/c/./../../g"` is `"/a/g"` |
| `default_port(scheme) -> Option[Int]` | 80 for `http` and `ws`, 443 for `https` and `wss`, 21 for `ftp` |
| `encode(s)`, `encode_path(s)` | every byte but letters, digits and `-._~` as `%XX` (`encode_path` keeps `/` too): `encode("a b&c")` is `"a%20b%26c"` |
| `decode(s) -> Result[Str, Str]` | each `%XX` as its byte; a `%` without two hex digits is an error |
| `parse_query(q) -> Result[List[(Str, Str)], Str]` | the `key=value` pairs in order, repeated keys kept, decoded, `+` as a space (as HTML forms send it); a pair without `=` has the value `""` |
| `build_query(pairs) -> Str` | pairs encoded and joined: `build_query([("q", "a b")])` is `"q=a%20b"` |
| `param(pairs, key) -> Option[Str]`, `param_all(pairs, key) -> List[Str]` | the first value of a key, and all of them |

```
use url

fn main(root: Root) -> Result[(), Str] {
    let u = url.parse("https://example.com:8443/search?q=limen+lang&page=2#top")?
    let q = u.params()?
    root.stdout.print("{u.host} {u.port} {url.param(q, "q") ?? ""}")   // example.com Some(8443) limen lang
    Ok(())
}
```

### compress

Compression with DEFLATE (RFC 1951), raw or in its two wrappers: zlib (RFC 1950), as HTTP's `deflate` and PNG use it, and gzip (RFC 1952), the `.gz` files and HTTP's `gzip`. Pure functions over `Bytes`, written in Limen, so they need no C library and behave the same everywhere.

| Item | Meaning |
| --- | --- |
| `gzip(data, level) -> Bytes`, `gunzip(data, max) -> Result[Bytes, Str]` | the gzip format, as `gzip -c` writes it (no file name or time in the header); `gunzip` reads every member of a file made by joining several, and skips a header's name, comment and extra field, checking its CRC if it has one |
| `zlib(data, level) -> Bytes`, `unzlib(data, max) -> Result[Bytes, Str]` | the zlib format: a two-byte header, the stream and an Adler-32. A preset dictionary is not supported (an error) |
| `deflate(data, level) -> Bytes`, `inflate(data, max) -> Result[Bytes, Str]` | a raw deflate stream, without header or checksum |
| `fold_gunzip(more, init, f) -> Result[T, Str]` | a gzip stream that arrives in pieces, decompressed as it comes: `more(offset)` gives the stream's bytes from `offset` on (empty at the end), and each piece of what it holds goes to `f: fn(T, Bytes) -> T` with what `f` returned before, from `init`. Memory stays at about 100 KB however large the data is; every member's CRC-32 and length are checked, as in `gunzip` |
| `crc32_more(crc, data) -> Int` | the CRC-32 of what came before and then `data`, from the CRC-32 `crc` of what came before (0 at the start): `crc32_more(crc32(a), b) == crc32(a + b)` |
| `crc32(data) -> Int`, `adler32(data) -> Int` | the checksums gzip and zlib use: `crc32("abc".to_bytes())` is `891568578` (`0x352441c2`) |

- `level` is 0 to 9: 0 stores the data as it is, 1 is fastest, 9 compresses most, and 6 is the usual choice; another level panics. Repeats are found with hash chains, as zlib finds them (from level 4 a repeat is put off when the next byte starts a longer one), and each block of about 16,000 symbols is written with Huffman codes made for it, with the fixed codes or stored, whichever is smallest. The result is about the size zlib makes at the same level. The output is the same for the same input and level, in both backends.
- `max` is the most bytes the result may have. Decompressing stops with an error as soon as it would pass it, so a few kilobytes made to expand to gigabytes (a decompression bomb) cost no more than `max` bytes of memory; pass the most you are prepared to hold. A negative `max` panics.
- Decompressing checks everything it reads, and hostile data is an error, never a panic or a read out of bounds: data cut off, a block of the reserved type, a stored block whose length does not match its complement, impossible or incomplete Huffman codes, a code that is not in its code, a distance back past the start, a checksum or length that does not match, bytes after the end. Messages start with `compress:`: `compress: the gzip data's CRC-32 does not match`.
- Natively, compressing runs at about 16 MB/s at level 6 (60 at level 1) and decompressing at about 120 MB/s, a few times slower than zlib's C.

```
use compress

fn main(root: Root) -> Result[(), Str] {
    let page = "<p>hello</p>".to_bytes()
    let packed = compress.gzip(page, 6)
    let back = compress.gunzip(packed, 1048576)?      // at most 1 MB
    root.stdout.print("{packed.len()} {back == page}")
    Ok(())
}
```

`http.request` (and `get` and `post`) ask for gzip and decompress a body sent with `Content-Encoding: gzip` or `deflate` (section 19, http).

### flags

Command-line flags. A program declares what it takes, parses `root.args()` and prints help built from the declarations. Pure: the arguments go in and the help comes back as text.

```
use flags

fn main(root: Root) -> Result[Int, Str] {
    let spec = flags.spec("grep", "Print the lines of FILE that match PATTERN.")
        .flag("ignore-case", "i", "match without regard to case")
        .option("max", "m", "stop after NUM matching lines").value_name("NUM").default("0")
        .positional("PATTERN", "what to look for")
        .rest("FILE", "files to read")
    let p = spec.parse(root.args())?
    if p.help {
        root.stdout.print(spec.help())
        return Ok(0)
    }
    let max = p.int("max")?
    ...
}
```

- Long flags are `--name value` or `--name=value`, and `--name` alone for a boolean flag; short ones are `-m value` or `-mvalue`, and boolean ones combine, `-iv`. Flags and positional arguments may come in any order. After `--` everything is positional, and so is `-` alone and a negative number (`-5`) when no short flag is a digit.
- `-h` and `--help` set `p.help` and skip the other checks, unless the program declares them itself.
- An unknown flag, a flag without its value, a value for a boolean flag, a missing required flag or positional argument, and an argument too many are errors: `flags: unknown flag --nope`. Declaring a flag twice, or `default` with no flag before it, is a bug in the program and panics.

| Item | Meaning |
| --- | --- |
| `spec(program, about) -> Spec` | a spec with a sentence for the help |
| `s.flag(name, short = "", help = "")` | a boolean flag, `--name` and `-short` (`short` may be `""`, or left out: `s.flag("verbose", help: "say more")`) |
| `s.option(name, short = "", help = "")` | a flag with a value; `.default(v)`, `.required()` and `.value_name(n)` after it change it |
| `s.positional(name, help = "")`, `s.rest(name, help = "")` | a required positional argument; any number after them (none without `rest`) |
| `s.parse(args) -> Result[Parsed, Str]` | the flags and arguments given |
| `s.usage()`, `s.help()` | the usage line, and the whole help with every flag, its default and whether it is required |
| `p.bool(name)`, `p.count(name)` | whether a boolean flag was given, and how many times (`-vvv` is 3) |
| `p.get(name) -> Option[Str]`, `p.str(name)`, `p.all(name)` | the last value given or the default; that or `""`; every value given (`--tag a --tag b`) |
| `p.int(name) -> Result[Int, Str]`, `p.float(name)` | the value as a number; an error if it is missing or not one |
| `p.arg(name)`, `p.rest()`, `p.args`, `p.help` | a positional argument by name, those after the declared ones, all of them, and whether help was asked for |

Reading a flag that was not declared panics, like reading a missing SQL column.

### log

Leveled logging, one line per message, as text or as JSON, to the `Stdout` or `Stderr` a `Logger` holds, with key-value fields. A Logger is a value: each method returns a new one, so a part of the program adds its own fields without changing anyone else's. Lines carry a timestamp only from a Logger given a `Clock`, so code that logs without one stays free of the time.

```
use log

fn main(root: Root) {
    let l = log.to_stderr(root.stderr).with_clock(root.clock).level(log.Debug)
    let req = l.field("request", "42")
    req.field("path", "/index.html").info("served")
    // 2026-11-14T20:01:02.003Z INFO  served request=42 path=/index.html
    req.json().warn("slow")
    // {"time":"2026-11-14T20:01:02.004Z","level":"warn","msg":"slow","request":"42"}
}
```

| Item | Meaning |
| --- | --- |
| `enum Level { Debug, Info, Warn, Error }` | in that order, so `Warn > Info` |
| `to_stdout(out)`, `to_stderr(err)` | a Logger writing text lines of `Info` and above |
| `l.level(min)`, `l.json()`, `l.text()`, `l.with_clock(clock)` | the Logger writing `min` and above, as JSON or text, with timestamps (UTC, ISO 8601) |
| `l.field(key, value)`, `l.fields(pairs)` | the Logger adding fields to every line, after the ones it has |
| `l.debug(msg)`, `l.info(msg)`, `l.warn(msg)`, `l.error(msg)`, `l.log(level, msg)` | write a line if the level is enabled |
| `l.enabled(level)` | whether a line of that level would be written, to skip building an expensive message |
| `parse_level(s) -> Result[Level, Str]`, `level_name(level)` | `debug`, `info`, `warn` (or `warning`) and `error`, ignoring case, for a `--log-level` flag |
| `format_line(format, time: Option[Int], level, msg, fields) -> Str` | the line without writing it, for tests and other destinations |

In text, the message's line breaks and other control characters are escaped, so a message is always one line, and a value that is empty or holds spaces, quotes or `=` is quoted. A function taking a Logger takes the capabilities it holds, so `limen caps` shows them: `Stderr (via Logger.sink.ToStderr)`.

### path

Paths with `/` between their parts, as macOS and Linux write them and `Fs` takes them. Everything works on the text alone, without looking at any file, except `walk`, which lists a tree through an `Fs`.

| Item | Meaning |
| --- | --- |
| `join(a, b)`, `join_all(parts)` | joined with one `/` and cleaned: `join("a/", "../b")` is `"b"`; a `b` starting with `/` is still joined under `a` (`join("data", "/x")` is `"data/x"`), and empty parts are left out |
| `clean(p)` | the shortest path naming the same file by the text: one `/` between parts, no `.` parts, a `..` removing the part before it, no trailing `/`; a `..` at the start of a relative path stays, and `/..` is `/`; `""` is `"."` |
| `dir(p)`, `base(p)` | everything before the last `/`, cleaned, and the last part: `dir("a/b/c.txt")` is `"a/b"`, `base` is `"c.txt"`; `dir("c.txt")` is `"."`, `base("/")` is `"/"` |
| `ext(p)`, `stem(p)`, `with_ext(p, e)` | `".gz"` and `"b.tar"` for `"a/b.tar.gz"`, `""` for a name with no dot or only a leading one (`.bashrc`); `with_ext("notes.md", ".html")` is `"notes.html"` |
| `parts(p)`, `is_absolute(p)` | the parts of the cleaned path, `["a", "b"]` for `/a/b/`; whether it starts with `/` |
| `relative(from, to) -> Result[Str, Str]` | the path from the directory `from` to `to`: `relative("a/b", "a/c/d")` is `Ok("../c/d")`. Both must be absolute or both relative, and `from` cannot climb past more `..`s than `to` |
| `struct Entry { path, is_dir }`, `walk(fs, dir) -> Result[List[Entry], IoError]` | every file and directory under `dir` (`""` for the whole of `fs`), depth first, each directory before what it holds, names sorted. Links are followed, but each directory is entered once (by its real path, `fs.full_path`), so a link back up the tree is listed and not entered again |

### uuid

UUIDs (RFC 9562), as 16 bytes written `f81d4fae-7dec-41d0-a765-00a0c91e6bf6`. Version 4 is random; version 7 starts with the time in milliseconds, so ids made later sort later and sit well in a database index. Both take their random bits from `Rand.bytes`, the system's secure generator. A `Uuid` is plain data: compare it, sort it, use it as a map key.

| Item | Meaning |
| --- | --- |
| `struct Uuid { bytes: Bytes }` | the 16 bytes, most significant first |
| `v4(rand) -> Uuid`, `v7(rand, clock) -> Uuid` | a random one, and a time-ordered one |
| `from_random(random: Bytes)`, `v7_from(ms, random: Bytes)` | the same from bytes you give (16, and 10), with the version and variant bits set; fewer bytes panic |
| `parse(s) -> Result[Uuid, Str]` | 32 hex digits in either case, as 8-4-4-4-12 or without dashes, optionally in braces or after `urn:uuid:`; the error names the byte |
| `u.to_str()` | lowercase, with dashes |
| `u.version()`, `u.time_ms() -> Option[Int]` | 4, 7, or 0 for `nil()`; the time of a version 7 one |
| `nil()`, `from_bytes(b) -> Result[Uuid, Str]` | all zeros; exactly 16 bytes |

```
use uuid

fn main(root: Root) {
    let id = uuid.v7(root.rand, root.clock)
    root.stdout.print(id.to_str())                       // 01a1dc01-3110-78de-b5ab-5f70bff3dee5
}
```

## 20. Not yet

The core language is close to complete; what is missing is mostly around it.

- Platforms: no Windows (fibers, the poller and the sandbox are macOS and Linux only); x86-64 has not run on real hardware; no cross-compilation or WebAssembly target.
- Standard library: no compatibility normal forms (NFKC, NFKD) or locale-specific case rules in `unicode`.
- Tooling: no incremental checking; test coverage only in the interpreter.
- Distribution: releases for macOS on Apple silicon (Homebrew) and Linux on x86-64 and arm64 (`.deb` and tarballs), but no apt repository; the package index has no hosted default; no stability promise yet.
- Language: no signed integer types other than `Int` outside FFI, and conversions the compiler writes only for JSON and SQL rows. No `mut` parameters in interface methods (and so on `dyn` values), foreign functions or function types; closures cannot take one either (E0003).
- A native backend other than C: Cranelift was decided against, since native code is C compiled by the system's compiler.

[ROADMAP.md](ROADMAP.md) has the details, in order.
