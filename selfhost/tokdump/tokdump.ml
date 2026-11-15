(* Print the tokens of FILE one per line, as the Limen lexer in
   selfhost/lexer.lmn does:  LO HI NL KIND VALUE
   NL is 1 when a newline came before the token. Text is escaped the same
   way on both sides: printable ASCII as it is, except \ and |, which are
   written as \x5c and \x7c like every other byte. A lexing error prints
   ERROR CODE LO HI and nothing else. *)
open Limen

let esc s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      let k = Char.code c in
      if k >= 0x20 && k <= 0x7e && c <> '\\' && c <> '|' then Buffer.add_char b c
      else Buffer.add_string b (Printf.sprintf "\\x%02x" k))
    s;
  Buffer.contents b

let () =
  let file = Sys.argv.(1) in
  let src = In_channel.with_open_bin file In_channel.input_all in
  match Lexer.lex file src with
  | t ->
    for i = 0 to t.count - 1 do
      let kind, value =
        match t.toks.(i) with
        | Lexer.INT n -> ("INT", Int64.to_string n)
        | FLOAT _ -> ("FLOAT", esc (String.sub src (Lexer.lo t i) (Lexer.hi t i - Lexer.lo t i)))
        | STR parts ->
          ( "STR",
            String.concat "|"
              (List.map (function Lexer.Lit s -> "L" ^ esc s | Interp (s, at) -> Printf.sprintf "I%d:%s" at (esc s)) parts) )
        | IDENT s -> ("IDENT", s)
        | KW s -> ("KW", s)
        | SYM s -> ("SYM", esc s)
        | EOF -> ("EOF", "")
      in
      Printf.printf "%d %d %d %s %s\n" (Lexer.lo t i) (Lexer.hi t i) (if Lexer.nl_before t i then 1 else 0) kind value
    done
  | exception Diag.Fatal d -> Printf.printf "ERROR %s %d %d\n" d.code d.span.lo d.span.hi
