(* Types every program can use without declaring them. *)
let file = "<prelude>"

let text =
  {|enum Option[T] { Some(T), None }
enum Result[T, E] { Ok(T), Err(E) }
enum IoError { Denied(Str), Failed(Str) }
struct ProcessOutput { code: Int, stdout: Str, stderr: Str }
struct HttpResponse { status: Int, headers: Map[Str, Str], body: Str }
struct Handle { addr: Int }
|}

(* Tuples of 2 to 8 items: generic structs with fields `0`, `1`, ... *)
let tuples =
  let sp = Span.dummy in
  List.init 7 (fun k ->
      let n = k + 2 in
      let gen = List.init n (fun i -> "T" ^ string_of_int i) in
      Ast.IStruct
        { st_name = Ast.tuple_name n; st_span = sp; st_gen = gen; st_pub = true;
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

let parsed = lazy (Source.add file text; Parser.parse_program file text @ tuples @ Lazy.force methods)
let items () = Lazy.force parsed
