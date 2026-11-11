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
  "E0001", "The lexer found a character that is not part of Vela's syntax.";
  "E0002", "A string literal or interpolation was not closed before the end of the file.";
  "E0003", "The parser expected a different token here. The message names what it expected.";
  "E0004", "Statements are separated by a newline or `;`. Two statements were found on the same line.";
  "E0005", "An integer literal does not fit in Int.";
  "E0006", "Unknown escape in a string. Valid escapes: \\n \\t \\r \\\\ \\\" \\{ \\} \\0.";
  "E0007", "An interpolation `{...}` cannot be empty. Write `\\{` for a literal brace.";
  "E0008", "Every function parameter needs a type, for example `fn f(x: Int)`. Vela only infers types inside function bodies.";
  "E0101", "A name was used that is not a local variable, function or enum variant in scope.";
  "E0102", "A type name is not defined. Built-in types: Int, Float, Bool, Str, List, Array[T, N], Option, Result, IoError, Root, Net, Fs, Stdout, Clock, Listen, Listener, Conn, Rand, Stdin, Env, Exec, Foreign, Handle.";
  "E0103", "The struct (or Root) has no field with this name.";
  "E0104", "The enum has no such variant, or the variant name is ambiguous. Qualify it as `Enum.Variant`.";
  "E0105", "The type has no method with this name. User-defined types have no methods in phase 1; write a function that takes the value.";
  "E0106", "Something is defined twice: a type, function, field or variant.";
  "E0107", "A function, variant or pattern was given the wrong number of arguments.";
  "E0108", "Only functions and closures can be called.";
  "E0109", "A generic type was given the wrong number of type arguments, for example `List` instead of `List[Int]`.";
  "E0110", "`self` names the receiver of a method: `fn area(self: Shape) -> Float` is called as `shape.area()`. It must be the first parameter, and its type a struct or enum declared in the program, or Int, Float, Bool, Str, List, Map, Option, Result or IoError. A method on one of those built-in types is private to its file, so it cannot be `pub`; capabilities, channels and arrays cannot get methods.";
  "E0111", "A type used for a bounded type parameter does not satisfy the interface. Interfaces are satisfied structurally: the type needs every method the interface declares, with the same parameter and result types. Define the missing method as `fn name(self: T, ...)`. The built-in bounds `Ordered` (the type has `<`) and `Key` (the type can be a map key) have no methods: the type itself must be ordered, or a valid key.";
  "E0112", "Only struct and enum values can become `dyn Interface`, and only if they hold no capability, function or channel. A `dyn` value hides its concrete type, so it must not hide authority either; pass capabilities with a type parameter bounded by the interface instead.";
  "E0115", "A `defer` body runs while its block is being left, by falling off the end, `return`, `?`, `break`, `continue`, a panic or a cancellation. It cannot start another way out: no `return`, no `?`, and no `break` or `continue` for a loop outside it (a loop inside the body is fine). Handle errors inside it, for example with `??` or a `match`.";
  "E0114", "A struct holds its fields inline, so a struct that contains itself, directly or through other structs, tuples or arrays, would be infinitely large. Put the recursive part in an `Option`, an enum or a `List`, which store their contents indirectly: `struct Node { value: Int, next: Option[Node] }`. Vela has no `box`: those types already are the indirection.";
  "E0113", "Map keys must be Int, Str or Bool, or tuples, structs and enums made of them, so they hash and compare by value the same way in both backends. Float (NaN is not equal to itself), lists and maps cannot be keys; key the map by an id or a name instead. In a generic function, a type parameter can be a key once it has the bound `Key`: `fn index[K: Key, V](...)`.";
  "E0120", "A name is promoted from two embedded structs at the same depth, so `x.name` could mean either. In `struct C { A, B }` where both A and B have `name`, write `c.A.name` or `c.B.name`. Ambiguity is only an error where the name is used; the outer struct can also declare its own `name`, which comes first.";
  "E0121", "Only a struct can be embedded: `struct Admin { User, level: Int }`. An `Int`, an enum, a list or a type parameter needs a field name, as in `count: Int`.";
  "E0201", "Type mismatch. The message shows the expected and the actual type.";
  "W0001", "A built-in method was renamed: `map_err` is now `map_error`. The old name still works for now; `vela check --json` and the editor offer a fix that renames it.";
  "E0202", "`?` returns early with the error, so the enclosing function must return `Result` (or `Option`) with the same error type, or `Result[_, dyn Error]`, which any error type with a `message` method converts to (Str and IoError included). Convert errors by hand with `.map_error(...)`.";
  "E0203", "An `if` without `else` is a statement, so its block must have type `()`.";
  "E0204", "A `match` must cover every possible value. Add the missing arms or a `_` arm. An arm with an `if` guard covers nothing, because its guard may be false.";
  "E0205", "Variables are immutable unless declared with `let mut`. Parameters and pattern bindings are always immutable, and closures cannot modify captured variables because they capture by value.";
  "E0206", "A struct literal must set every field.";
  "E0207", "The type of this value is not known at this point. Add a type annotation, for example `let xs: List[Int] = []`.";
  "E0208", "`break` and `continue` only work inside `while` and `for` loops (not inside closures).";
  "E0209", "Only variables, struct fields, list elements and map entries (and paths through them, such as `m[k].x`) can be assigned to. A method value such as `p.area` cannot.";
  "E0301", "Capabilities (Root, Net, Fs, Stdout, Clock) cannot be stored in struct or enum fields. Authority must stay visible in function signatures, so pass capabilities as parameters.";
  "E0302", "Vela has no ambient I/O: there is no global print, open, http or clock. I/O goes through capability values. `main` receives `root: Root`; pass `root.stdout`, `root.fs`, `root.net` or `root.clock` to the functions that need them.";
  "E0303", "`main` must be `fn main(root: Root)`, `fn main(root: Root) -> Int` (the exit code), `fn main(root: Root) -> Result[(), E]` or `fn main(root: Root) -> Result[Int, E]`, with no type parameters. `root` may be left out.";
  "E0304", "Functions cannot be stored in struct or enum fields, because a stored closure could carry a capability out of sight.";
  "E0305", "Channels cannot be stored in struct or enum fields: like capabilities, they are passed as parameters or captured by a task's closure.";
  "E0306", "A test can ask the runner for capabilities, and only these: `fs: Fs` (a fresh, writable temporary directory, removed after the test) and `clock: Clock`. Each at most once, for example `test \"saves\" (fs: Fs) { ... }`. For anything else, pass a fake through an interface.";
  "E0310", "A group handle can only be used as `g.go(closure)`, directly in the group's body: it cannot be stored, passed, returned or captured by a closure.";
  "E0311", "Code inside a `group` block cannot leave it early with `return` or `?`, because every task must finish before the group returns its results.";
  "E0330", "A foreign function's signature can only use types that cross to C: Int (int64_t), Int32 (int32_t), Float (double), Bool (int), Str and Bytes (a pointer to the bytes, valid during the call), Handle (void *), and Out[Int | Int32 | Float | Handle] for an out-parameter. It returns one of those (not Bytes) or nothing. At most 8 integer-like and 8 Float parameters; no variadic functions.";
  "E0331", "A foreign function can only be called directly, as in `strlen(foreign, s)`; it cannot be stored, passed or returned as a value. Wrap it in a closure when you need one: `|s| strlen(foreign, s)`.";
  "E0320", "A dependency's functions take a capability that vela.toml does not grant it. Vela has no ambient authority, so the capabilities in a dependency's signatures are all it can ever use. Grant it with `caps = [\"net\"]` (net, fs, listen, stdout, clock) on its line in [dependencies], or use a dependency that needs less. No dependency can take Root. Standard library modules are checked the same way against src/std/vela.toml; an error there is a bug in the standard library.";
  "E0321", "vela.toml could not be read. It has a [package] table with `name`, a [grants] table with the program's own grants (`fs = \"data:ro\"`, `net = [\"api.github.com\"]`; net, fs, listen and env), and a [dependencies] table with lines like `json = { path = \"vendor/json\", caps = [] }`.";
  "E0322", "A git dependency could not be fetched. Vela runs `git clone` on the URL in vela.toml and checks out its `tag` or `commit`; the message has git's own error. Check the URL, that the tag or commit exists, and that git is installed. The editor and agent servers never fetch: run `vela check FILE` once to fill the cache.";
  "E0323", "vela.lock does not match. vela.lock records the commit and a hash of the files of every git dependency; the cached copy is checked against it on every build, so a changed cache or a rewritten history is caught. Delete the cached directory named in the note to fetch it again, or delete the dependency's line from vela.lock to accept a new version on purpose.";
  "E0324", "A dependency trusted with C (named in [trust] foreign in vela.toml) declares C libraries that vela.lock did not record when it was trusted. Calling C is full trust, so a dependency update that starts using a new library stops the build. Review the change, then delete the dependency's `foreign NAME ...` line from vela.lock; the next build records the libraries it uses now.";
  "E0401", "A module could not be found. `use NAME` looks for NAME.vela next to the importing file, then in the dependencies listed in vela.toml, then in the standard library. A qualified name such as `json.parse` needs `use json` in the same file.";
  "E0402", "The item is private to its module. Only items marked `pub` can be used from another module.";
  "E0403", "The module has no exported item with this name. The note lists what it exports.";
  "E0404", "A module name is not a value. Use one of its items, such as `json.parse(text)`.";
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
    (jstr ("vela explain " ^ d.code))

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
