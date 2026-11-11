(* Types every program can use without declaring them. *)
let file = "<prelude>"

let text =
  {|enum Option[T] { Some(T), None }
enum Result[T, E] { Ok(T), Err(E) }
enum IoError { Denied(Str), Failed(Str) }
struct ProcessOutput { code: Int, stdout: Str, stderr: Str }
struct HttpResponse { status: Int, headers: Map[Str, Str], body: Str }
struct Handle { addr: Int }
interface Error {
    fn message(self) -> Str
    fn cause(self) -> Option[dyn Error]
}
struct WrappedError { context: Str, inner: dyn Error }
fn message(self: WrappedError) -> Str { "{self.context}: {self.inner.message()}" }
fn cause(self: WrappedError) -> Option[dyn Error] { Some(self.inner) }
fn str_message(s: Str) -> Str { s }
fn ioerror_message(e: IoError) -> Str { match e { Denied(m) => "denied: {m}", Failed(m) => m } }
fn no_cause[T](x: T) -> Option[dyn Error] { None }
|}

(* Functions programs cannot name: the `Error` methods of `Str` and
   `IoError`, and the `cause` a type without one gets (None). The backends
   call them by these names. *)
let hidden = [ ("str_message", "$Str::message"); ("ioerror_message", "$IoError::message"); ("no_cause", "$no_cause") ]

(* Tuples of 2 to 8 items: generic structs with fields `0`, `1`, ... *)
let tuples =
  let sp = Span.dummy in
  List.init 7 (fun k ->
      let n = k + 2 in
      let gen = List.init n (fun i -> "T" ^ string_of_int i) in
      Ast.IStruct
        { st_name = Ast.tuple_name n; st_span = sp; st_gen = gen; st_pub = true; st_embeds = [];
          st_fields = List.mapi (fun i g -> (string_of_int i, sp, { Ast.tdesc = Ast.TyName (g, []); tspan = sp })) gen })

(* Built-in methods of List and Map written in Vela (std/methods.vela). They
   are keyed like methods (`List::group_by`) and every file can call them. *)
let methods_file = "<builtin>"

let methods =
  lazy
    (Source.add methods_file Methods_text.text;
     List.map
       (function
         | Ast.IFn ({ fn_params = { prm_name = "self"; prm_ty = { tdesc = TyName (tn, _); _ }; _ } :: _; _ } as d) ->
           Ast.IFn { d with fn_name = tn ^ "::" ^ d.fn_name }
         | it -> it)
       (Parser.parse_program methods_file Methods_text.text))

(* The names of the built-in methods written in Vela for type [tn]. *)
let method_names tn =
  List.filter_map
    (function
      | Ast.IFn d when String.starts_with ~prefix:(tn ^ "::") d.fn_name ->
        Some (String.sub d.fn_name (String.length tn + 2) (String.length d.fn_name - String.length tn - 2))
      | _ -> None)
    (Lazy.force methods)

(* Methods are keyed `Type::name`, as Modules names them in a program. *)
let rename = function
  | Ast.IFn ({ fn_params = { prm_name = "self"; prm_ty = { tdesc = Ast.TyName (tn, _); _ }; _ } :: _; _ } as d) ->
    Ast.IFn { d with fn_name = tn ^ "::" ^ d.fn_name }
  | Ast.IFn d when List.mem_assoc d.fn_name hidden -> Ast.IFn { d with fn_name = List.assoc d.fn_name hidden }
  | it -> it

let parsed = lazy (Source.add file text; List.map rename (Parser.parse_program file text) @ tuples @ Lazy.force methods)
let items () = Lazy.force parsed
