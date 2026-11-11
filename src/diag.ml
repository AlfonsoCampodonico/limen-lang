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
  "E0102", "A type name is not defined. Built-in types: Int, Float, Bool, Str, List, Array[T, N], Option, Result, IoError, Root, Net, Fs, Stdout, Clock, Listen, Listener, Conn, Rand, Stdin, Env.";
  "E0103", "The struct (or Root) has no field with this name.";
  "E0104", "The enum has no such variant, or the variant name is ambiguous. Qualify it as `Enum.Variant`.";
  "E0105", "The type has no method with this name. User-defined types have no methods in phase 1; write a function that takes the value.";
  "E0106", "Something is defined twice: a type, function, field or variant.";
  "E0107", "A function, variant or pattern was given the wrong number of arguments.";
  "E0108", "Only functions and closures can be called.";
  "E0109", "A generic type was given the wrong number of type arguments, for example `List` instead of `List[Int]`.";
  "E0110", "`self` names the receiver of a method: `fn area(self: Shape) -> Float` is called as `shape.area()`. It must be the first parameter, and its type a struct or enum declared in the program (not a built-in type such as Int, List, Option or Result).";
  "E0111", "A type used for a bounded type parameter does not satisfy the interface. Interfaces are satisfied structurally: the type needs every method the interface declares, with the same parameter and result types. Define the missing method as `fn name(self: T, ...)`.";
  "E0112", "Only struct and enum values can become `dyn Interface`, and only if they hold no capability, function or channel. A `dyn` value hides its concrete type, so it must not hide authority either; pass capabilities with a type parameter bounded by the interface instead.";
  "E0201", "Type mismatch. The message shows the expected and the actual type.";
  "E0202", "`?` returns early with the error, so the enclosing function must return `Result` (or `Option`) with the same error type. Convert errors with `.map_err(...)`.";
  "E0203", "An `if` without `else` is a statement, so its block must have type `()`.";
  "E0204", "A `match` must cover every possible value. Add the missing arms or a `_` arm.";
  "E0205", "Variables are immutable unless declared with `let mut`. Parameters and pattern bindings are always immutable, and closures cannot modify captured variables because they capture by value.";
  "E0206", "A struct literal must set every field.";
  "E0207", "The type of this value is not known at this point. Add a type annotation, for example `let xs: List[Int] = []`.";
  "E0208", "`break` and `continue` only work inside `while` and `for` loops (not inside closures).";
  "E0209", "Only variables, struct fields and list elements can be assigned to.";
  "E0301", "Capabilities (Root, Net, Fs, Stdout, Clock) cannot be stored in struct or enum fields. Authority must stay visible in function signatures, so pass capabilities as parameters.";
  "E0302", "Vela has no ambient I/O: there is no global print, open, http or clock. I/O goes through capability values. `main` receives `root: Root`; pass `root.stdout`, `root.fs`, `root.net` or `root.clock` to the functions that need them.";
  "E0303", "`main` must be `fn main(root: Root)` or `fn main(root: Root) -> Result[(), E]`, with no type parameters.";
  "E0304", "Functions cannot be stored in struct or enum fields, because a stored closure could carry a capability out of sight.";
  "E0305", "Channels cannot be stored in struct or enum fields: like capabilities, they are passed as parameters or captured by a task's closure.";
  "E0310", "A group handle can only be used as `g.go(closure)`, directly in the group's body: it cannot be stored, passed, returned or captured by a closure.";
  "E0311", "Code inside a `group` block cannot leave it early with `return` or `?`, because every task must finish before the group returns its results.";
  "E0320", "A dependency's functions take a capability that vela.toml does not grant it. Vela has no ambient authority, so the capabilities in a dependency's signatures are all it can ever use. Grant it with `caps = [\"net\"]` (net, fs, listen, stdout, clock) on its line in [dependencies], or use a dependency that needs less. No dependency can take Root.";
  "E0321", "vela.toml could not be read. It has a [package] table with `name`, and a [dependencies] table with lines like `json = { path = \"vendor/json\", caps = [] }`.";
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
