(* Diagnostics: one record per problem, rendered for humans or as JSON. *)
type severity = Error | Warning
type edit = { at : Span.t; replacement : string }
type fix = { fmsg : string; edits : edit list }

type t = {
  code : string;
  severity : severity;
  message : string;
  span : Span.t;
  notes : string list;
  fix : fix option;
}

(* Raised by the lexer and parser, which stop at the first error. *)
exception Fatal of t

let make ?(severity = Error) ?(notes = []) ?fix code span message =
  { code; severity; message; span; notes; fix }

(* Warnings are reported but do not stop a program from building or running. *)
let errors ds = List.filter (fun d -> d.severity = Error) ds
let warnings ds = List.filter (fun d -> d.severity = Warning) ds

let explanations = [
  "E0001", "The lexer found a character that is not part of Tessera's syntax.";
  "E0002", "A string literal or interpolation was not closed before the end of the file.";
  "E0003", "The parser expected a different token here, or found a construct where it is not allowed (a list pattern in `let`, which needs `match`; `as _`). The message names what it expected.";
  "E0004", "Statements are separated by a newline or `;`. Two statements were found on the same line.";
  "E0005", "An integer literal does not fit in Int.";
  "E0006", "Unknown escape in a string. Valid escapes: \\n \\t \\r \\\\ \\\" \\{ \\} \\0.";
  "E0007", "An interpolation `{...}` cannot be empty. Write `\\{` for a literal brace.";
  "E0008", "Every function parameter needs a type, for example `fn f(x: Int)`. Tessera only infers types inside function bodies.";
  "E0101", "A name was used that is not a local variable, function or enum variant in scope.";
  "E0102", "A type name is not defined, or a struct literal or struct pattern names something that is not a struct (an enum is built and matched with its variants). Built-in types: Int, Float, Bool, Str, List, Array[T, N], Option, Result, IoError, Root, Net, Fs, Stdout, Stderr, Clock, Listen, Listener, Conn, Rand, Stdin, Env, Exec, Foreign, Handle, FileInfo.";
  "E0103", "The struct (or Root) has no field with this name. A struct pattern names the struct's own fields; a field promoted from an embedded struct is matched through the embedded field.";
  "E0104", "The enum has no such variant, or the variant name is ambiguous. Qualify it as `Enum.Variant`.";
  "E0105", "The type has no method with this name. User-defined types have no methods in phase 1; write a function that takes the value.";
  "E0106", "Something is defined twice: a type, function, field or variant, or a field is set twice in a struct literal or matched twice in a struct pattern. A struct, enum or interface cannot take the name of a built-in type or capability (`Env`, `Option`, `Int`, ...), which every program already has: name it something else.";
  "E0107", "A function, variant or pattern was given the wrong number of arguments.";
  "E0108", "Only functions and closures can be called.";
  "E0109", "A generic type was given the wrong number of type arguments, for example `List` instead of `List[Int]`, or a generic interface named without its arguments, `Container` instead of `Container[Int]`.";
  "E0110", "`self` names the receiver of a method: `fn area(self: Shape) -> Float` is called as `shape.area()`. It must be the first parameter, and its type a struct or enum declared in the program, or Int, Float, Bool, Str, List, Map, Option, Result or IoError. A method on one of those built-in types is private to its file, so it cannot be `pub`; capabilities, channels and arrays cannot get methods.";
  "E0111", "A type used for a bounded type parameter (of a function, struct or enum) does not satisfy the interface. Interfaces are satisfied structurally: the type needs every method the interface declares, with the same parameter and result types. Define the missing method as `fn name(self: T, ...)`. The built-in bounds `Ordered` (the type has `<`) and `Key` (the type can be a map key) have no methods: the type itself must be ordered, or a valid key.";
  "E0112", "Only struct and enum values can become `dyn Interface`, and only if they hold no capability, function or channel, directly or through the fields of the structs and enums inside them (`struct App { client: Client }` holds the `Net` of `struct Client { net: Net }`; the message names the path). A `dyn` value hides its concrete type, so it must not hide authority either; pass such values with a type parameter bounded by the interface instead: `fn show[T: Named](x: T)`.";
  "E0115", "A `defer` body runs while its block is being left, by falling off the end, `return`, `?`, `break`, `continue`, a panic or a cancellation. It cannot start another way out: no `return`, no `?`, and no `break` or `continue` for a loop outside it (a loop inside the body is fine). Handle errors inside it, for example with `??` or a `match`.";
  "E0116", "A type alias is another name for a type, so it cannot contain itself, directly or through other aliases: `type Tree = List[Tree]` would never end. Declare a struct or enum for a type that contains itself: `enum Tree { Leaf(Int), Node(List[Tree]) }`.";
  "E0114", "A struct holds its fields inline, so a struct that contains itself, directly or through other structs, tuples or arrays, would be infinitely large. Put the recursive part in an `Option`, an enum or a `List`, which store their contents indirectly: `struct Node { value: Int, next: Option[Node] }`. Tessera has no `box`: those types already are the indirection.";
  "E0113", "Map keys must be Int, Str or Bool, or tuples, structs and enums made of them, so they hash and compare by value the same way in both backends. Float (NaN is not equal to itself), lists, maps and capabilities cannot be keys, nor can a struct or enum that holds one, however deeply; key the map by an id or a name instead. In a generic function, a type parameter can be a key once it has the bound `Key`: `fn index[K: Key, V](...)`.";
  "E0120", "A name is promoted from two embedded structs at the same depth, so `x.name` could mean either. In `struct C { A, B }` where both A and B have `name`, write `c.A.name` or `c.B.name`. Ambiguity is only an error where the name is used; the outer struct can also declare its own `name`, which comes first.";
  "E0121", "Only a struct can be embedded: `struct Admin { User, level: Int }`. An `Int`, an enum, a list or a type parameter needs a field name, as in `count: Int`.";
  "E0122", "A named argument, `f(x, retries: 3)`, must name one of the callee's parameters, each parameter at most once, after all the positional arguments. Only functions and methods declared with `fn` (and interface methods) have parameter names: closures, function values, enum variants, built-in methods and functions, and foreign functions take their arguments by position. Every parameter must still be passed; Tessera has no default values.";
  "E0201", "Type mismatch. The message shows the expected and the actual type. It is also an operator applied to a type that does not have it, such as `==` on functions, channels, `dyn` values and capabilities; a value that holds a capability (in a field, a payload, a list...) cannot be compared either, since capabilities have no equality.";
  "W0001", "A built-in method was renamed: `map_err` is now `map_error`. The old name still works for now; `tessera check --json` and the editor offer a fix that renames it.";
  "E0202", "`?` returns early with the error, so the enclosing function must return `Result` (or `Option`) with the same error type, or `Result[_, dyn Error]`, which any error type with a `message` method converts to (Str and IoError included). Convert errors by hand with `.map_error(...)`.";
  "E0203", "An `if` without `else` is a statement, so its block must have type `()`.";
  "E0204", "A `match` must cover every possible value. Add the missing arms or a `_` arm. An arm with an `if` guard covers nothing, because its guard may be false.";
  "E0205", "Variables are immutable unless declared with `let mut`. Parameters and pattern bindings are always immutable, and closures cannot modify captured variables because they capture by value.";
  "E0206", "A struct literal must set every field. A struct pattern must name every field too, unless it ends with `..`, which matches any value of the fields it leaves out: `Point { x: 0, .. }`.";
  "E0207", "The type of this value is not known at this point. Add a type annotation, for example `let xs: List[Int] = []`.";
  "E0208", "`break` and `continue` only work inside `while` and `for` loops (not inside closures).";
  "E0209", "Only variables, struct fields, list elements and map entries (and paths through them, such as `m[k].x`) can be assigned to. A method value such as `p.area` cannot.";
  "E0210", "Every alternative of an or-pattern `A | B` must bind the same names, with the same types, because the arm's body uses them whichever alternative matched: `Circle(r) | Ring(r, _) => r` is fine, `Circle(r) | Point => r` is not. Give an alternative the missing name, or use `_` in all of them.";
  "E0211", "In a tuple assignment, `(a, b) = value`, the value must be a tuple with one item for each target on the left: `(x, p) = (1, 2)`, `(count, rest) = split(text)?`. Write `_` on the left for an item to ignore.";
  "W0002", "This arm (or this alternative of an or-pattern) can never match: the unguarded arms before it already match every value it would. Remove it, or move it above the arm that covers it. A guarded arm does not count as covering anything, since its guard may be false.";
  "E0301", "No longer reported. Capabilities could not be stored in struct or enum fields; now they can, and the authority a struct or enum holds through its fields is tracked through the types: a function taking `c: Client`, where `struct Client { net: Net }`, takes `Net` (`tessera caps` shows `Net (via Client.net)`), a dependency's grant covers it (E0320), and such a value cannot become `dyn` (E0112), be a map key (E0113) or be compared (E0201).";
  "E0302", "Tessera has no ambient I/O: there is no global print, open, http or clock. I/O goes through capability values. `main` receives `root: Root`; pass `root.stdout`, `root.stderr`, `root.fs`, `root.net` or `root.clock` to the functions that need them.";
  "E0303", "`main` must be `fn main(root: Root)`, `fn main(root: Root) -> Int` (the exit code), `fn main(root: Root) -> Result[(), E]` or `fn main(root: Root) -> Result[Int, E]`, with no type parameters. `root` may be left out.";
  "E0304", "Functions cannot be stored in struct or enum fields, because a stored closure could carry a capability out of sight.";
  "E0305", "Channels cannot be stored in struct or enum fields: like capabilities, they are passed as parameters or captured by a task's closure.";
  "E0306", "A test can ask the runner for capabilities, and only these: `fs: Fs` (a fresh, writable temporary directory, removed after the test) and `clock: Clock` (the real clock), or `clock: virtual Clock` (a clock on virtual time, for tests only). Each at most once, for example `test \"saves\" (fs: Fs) { ... }`. For anything else, pass a fake through an interface.";
  "E0310", "A group handle can only be used as `g.go(closure)`, directly in the group's body: it cannot be stored, passed, returned or captured by a closure.";
  "E0311", "Code inside a `group` block cannot leave it early with `return` or `?`, because every task must finish before the group returns its results.";
  "E0330", "A foreign declaration can only use types that cross to C: Int (int64_t), Int32 (int32_t), Float (double), Bool (int), Str and Bytes (a pointer to the bytes, valid during the call), Handle (void *), a struct declared in the foreign block (`struct Div { quot: Int32, rem: Int32 }`, 1 to 8 fields of Int, Int32, Float, Bool or Handle), a callback `fn(A, B) -> R` (A and B of Int, Int32, Float, Bool, Str or Handle; R one of those but Str, or `()`), and Out[Int | Int32 | Float | Handle] for an out-parameter. It returns one of those (not Bytes or a callback) or nothing. At most 8 integer-like and 8 Float values, each field of a struct counting as one. A variadic function (`fn printf(format: Str, ...) -> Int32`) has 1 to 6 integer-like parameters before `...` and at most 8 after it, of Int, Int32, Float, Bool, Str, Bytes or Handle, and does not return a struct. `= \"name\"` after the signature names the C function, which must be a C identifier.";
  "E0331", "A foreign function can only be called directly, as in `strlen(foreign, s)`; it cannot be stored, passed or returned as a value. Wrap it in a closure when you need one: `|s| strlen(foreign, s)`.";
  "E0332", "A callback passed to C is a top-level function, named as in `qsort(f, base, n, 8, compare)`. C calls it with its arguments alone, so it has nowhere to keep a closure's captured values: a closure, a function value in a variable, or a generic function cannot be passed. A callback may take a `Foreign` first, before what C passes: it gets the Foreign value the foreign call was given.";
  "E0320", "A dependency's functions take a capability that tessera.toml does not grant it. Tessera has no ambient authority, so the capabilities in a dependency's signatures are all it can ever use. This counts the capabilities held by the structs and enums its functions take, through their fields: a function taking `c: Ctx` where `struct Ctx { fs: Fs }` takes `Fs`, and the note names the field (`through Ctx.fs`), so authority cannot be passed in a struct around the grant. Grant it with `caps = [\"net\"]` (net, fs, listen, stdout, clock) on its line in [dependencies], or use a dependency that needs less. No dependency can take Root. Standard library modules are checked the same way against src/std/tessera.toml; an error there is a bug in the standard library.";
  "E0321", "tessera.toml could not be read. It has a [package] table with `name`, a [grants] table with the program's own grants (`fs = \"data:ro\"`, `net = [\"api.github.com\"]`; net, fs, listen and env), a [dependencies] table with lines like `json = { path = \"vendor/json\", caps = [] }`, and an [embed] table, `roots = [\"../assets\"]`, naming directories (each must exist) whose files the program may embed besides its own package's. Only the program's tessera.toml can have [embed]: a dependency's that declares embed roots is this error too, since a dependency embeds only its own files.";
  "E0322", "A git dependency could not be fetched. Tessera runs `git clone` on the URL in tessera.toml and checks out its `tag` or `commit`; the message has git's own error. Check the URL, that the tag or commit exists, and that git is installed. The editor and agent servers never fetch: run `tessera check FILE` once to fill the cache.";
  "E0323", "tessera.lock does not match. tessera.lock records the commit and a hash of the files of every git dependency; the cached copy is checked against it on every build, so a changed cache or a rewritten history is caught. Delete the cached directory named in the note to fetch it again, or delete the dependency's line from tessera.lock to accept a new version on purpose.";
  "E0324", "A dependency trusted with C (named in [trust] foreign in tessera.toml) declares C libraries that tessera.lock did not record when it was trusted. Calling C is full trust, so a dependency update that starts using a new library stops the build. Review the change, then delete the dependency's `foreign NAME ...` line from tessera.lock; the next build records the libraries it uses now.";
  "E0401", "A module could not be found. `use NAME` looks for NAME.tsr next to the importing file, then in the dependencies listed in tessera.toml, then in the standard library. A qualified name such as `json.parse` needs `use json` in the same file.";
  "E0402", "The item is private to its module. Only items marked `pub` can be used from another module.";
  "E0403", "The module has no exported item with this name. The note lists what it exports.";
  "E0404", "A module name is not a value. Use one of its items, such as `json.parse(text)`.";
  "E0405", "`embed(\"path\")` and `embed_bytes(\"path\")` read a file when the program is compiled. The path must be a string literal, relative to the file that names it, and inside that file's package: the directory of its tessera.toml (or of the main file when there is none), or a dependency's own directory. The program's own tessera.toml can add directories its files may embed from, `[embed] roots = [\"../assets\"]` (relative to tessera.toml); a dependency's never can. Use `embed` for UTF-8 text and `embed_bytes` for anything else; `embed_dir(\"public\")` takes every file under a directory (names starting with `.` are left out), and none of them may point outside the package. The standard library embeds nothing.";
  "E0901", "This feature is designed but not implemented yet.";
]

let explain code = List.assoc_opt code explanations

let jstr s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let jpos file off =
  let l, c = Source.line_col file off in
  Printf.sprintf {|{"line":%d,"col":%d,"offset":%d}|} l c off

let jspan (sp : Span.t) =
  Printf.sprintf {|{"file":%s,"start":%s,"end":%s}|} (jstr sp.file) (jpos sp.file sp.lo)
    (jpos sp.file sp.hi)

let to_json d =
  let fix =
    match d.fix with
    | None -> "null"
    | Some f ->
      Printf.sprintf {|{"message":%s,"edits":[%s]}|} (jstr f.fmsg)
        (String.concat ","
           (List.map
              (fun e -> Printf.sprintf {|{"span":%s,"replacement":%s}|} (jspan e.at) (jstr e.replacement))
              f.edits))
  in
  Printf.sprintf {|{"code":%s,"severity":%s,"message":%s,"span":%s,"notes":[%s],"fix":%s,"explain":%s}|}
    (jstr d.code)
    (jstr (match d.severity with Error -> "error" | Warning -> "warning"))
    (jstr d.message) (jspan d.span)
    (String.concat "," (List.map jstr d.notes))
    fix
    (jstr ("tessera explain " ^ d.code))

let list_to_json ds = "{\"diagnostics\":[" ^ String.concat "," (List.map to_json ds) ^ "]}"

let render d =
  let b = Buffer.create 256 in
  let sev = match d.severity with Error -> "error" | Warning -> "warning" in
  Printf.bprintf b "%s[%s]: %s\n" sev d.code d.message;
  let file = d.span.file in
  if file <> "" then begin
    let l, c = Source.line_col file d.span.lo in
    Printf.bprintf b "  --> %s:%d:%d\n" file l c;
    let text = Source.line_text file l in
    let gutter = String.make (String.length (string_of_int l)) ' ' in
    let l2, c2 = Source.line_col file d.span.hi in
    let width = if l2 = l then max 1 (c2 - c) else max 1 (String.length text - c + 1) in
    Printf.bprintf b " %s |\n %d | %s\n %s | %s%s\n" gutter l text gutter
      (String.make (max 0 (c - 1)) ' ')
      (String.make width '^')
  end;
  List.iter (fun n -> Printf.bprintf b "  = note: %s\n" n) d.notes;
  (match d.fix with Some f -> Printf.bprintf b "  = help: %s\n" f.fmsg | None -> ());
  Buffer.contents b
