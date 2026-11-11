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

let parsed = lazy (Source.add file text; Parser.parse_program file text @ tuples)
let items () = Lazy.force parsed
