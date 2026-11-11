(* Types every program can use without declaring them. *)
let file = "<prelude>"

let text =
  {|enum Option[T] { Some(T), None }
enum Result[T, E] { Ok(T), Err(E) }
enum IoError { Denied(Str), Failed(Str) }
|}

let parsed = lazy (Source.add file text; Parser.parse_program file text)
let items () = Lazy.force parsed
