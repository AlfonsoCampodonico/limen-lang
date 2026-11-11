(* Lexer. Each token records whether a newline came before it; the parser
   uses that to separate statements without semicolons. *)

type strpart = Lit of string | Interp of string * int (* source text, absolute offset *)

type tok =
  | INT of int
  | FLOAT of float
  | STR of strpart list
  | IDENT of string
  | KW of string
  | SYM of string
  | EOF

type token = { tok : tok; span : Span.t; nl : bool }

let keywords =
  [ "fn"; "let"; "mut"; "if"; "else"; "match"; "for"; "in"; "while"; "return"; "struct";
    "enum"; "true"; "false"; "group"; "break"; "continue" ]

let syms2 = [ "->"; "=>"; "=="; "!="; "<="; ">="; "&&"; "||" ]
let syms1 = "(){}[],:;.|+-*/%!?=<>"

let describe = function
  | INT n -> Printf.sprintf "number `%d`" n
  | FLOAT f -> Printf.sprintf "number `%g`" f
  | STR _ -> "a string"
  | IDENT s -> Printf.sprintf "`%s`" s
  | KW s -> Printf.sprintf "keyword `%s`" s
  | SYM s -> Printf.sprintf "`%s`" s
  | EOF -> "end of file"

let fatal code file lo hi msg = raise (Diag.Fatal (Diag.make code { Span.file; lo; hi } msg))
let is_digit c = c >= '0' && c <= '9'
let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_ident c = is_ident_start c || is_digit c

(* Index just past the string literal starting at [i] (a '"'). Handles
   escapes and nested interpolations, so `"{xs.join("-")}"` works. *)
let rec skip_string src i on_eof =
  let n = String.length src in
  let rec go j =
    if j >= n then on_eof ()
    else
      match src.[j] with
      | '"' -> j + 1
      | '\\' -> go (j + 2)
      | '{' -> go (skip_braces src (j + 1) on_eof)
      | _ -> go (j + 1)
  in
  go (i + 1)

and skip_braces src i on_eof =
  let n = String.length src in
  let rec go j depth =
    if j >= n then on_eof ()
    else
      match src.[j] with
      | '{' -> go (j + 1) (depth + 1)
      | '}' -> if depth = 0 then j + 1 else go (j + 1) (depth - 1)
      | '"' -> go (skip_string src j on_eof) depth
      | _ -> go (j + 1) depth
  in
  go i 0

(* Comments seen by the last top-level lex, as (start, end, text); the
   formatter puts them back. *)
let comments : (int * int * string) list ref = ref []

let lex ?(base = 0) file (src : string) : token array =
  if base = 0 then comments := [];
  let n = String.length src in
  let toks = ref [] in
  let i = ref 0 in
  let nl = ref false in
  let push tok lo hi =
    toks := { tok; span = { Span.file; lo = base + lo; hi = base + hi }; nl = !nl } :: !toks;
    nl := false
  in
  let fail code lo hi msg = fatal code file (base + lo) (base + hi) msg in
  while !i < n do
    let c = src.[!i] in
    if c = '\n' then (nl := true; incr i)
    else if c = ' ' || c = '\t' || c = '\r' then incr i
    else if c = '/' && !i + 1 < n && src.[!i + 1] = '/' then begin
      let lo = !i in
      while !i < n && src.[!i] <> '\n' do incr i done;
      if base = 0 then comments := (lo, !i, String.trim (String.sub src lo (!i - lo))) :: !comments
    end
    else if is_digit c then begin
      let lo = !i in
      while !i < n && (is_digit src.[!i] || src.[!i] = '_') do incr i done;
      let is_float = !i + 1 < n && src.[!i] = '.' && is_digit src.[!i + 1] in
      if is_float then begin
        incr i;
        while !i < n && (is_digit src.[!i] || src.[!i] = '_') do incr i done
      end;
      (* exponent: 1e9, 2.5E-3 *)
      let has_exp =
        !i < n
        && (src.[!i] = 'e' || src.[!i] = 'E')
        && (( !i + 1 < n && is_digit src.[!i + 1])
           || (!i + 2 < n && (src.[!i + 1] = '+' || src.[!i + 1] = '-') && is_digit src.[!i + 2]))
      in
      if has_exp then begin
        i := !i + 2;
        while !i < n && is_digit src.[!i] do incr i done
      end;
      let is_float = is_float || has_exp in
      let text = String.concat "" (String.split_on_char '_' (String.sub src lo (!i - lo))) in
      if is_float then push (FLOAT (float_of_string text)) lo !i
      else
        match int_of_string_opt text with
        | Some v -> push (INT v) lo !i
        | None -> fail "E0005" lo !i (Printf.sprintf "integer literal `%s` is too large" text)
    end
    else if is_ident_start c then begin
      let lo = !i in
      while !i < n && is_ident src.[!i] do incr i done;
      let s = String.sub src lo (!i - lo) in
      push (if List.mem s keywords then KW s else IDENT s) lo !i
    end
    else if c = '"' then begin
      let lo = !i in
      incr i;
      let parts = ref [] in
      let buf = Buffer.create 16 in
      let flush () =
        if Buffer.length buf > 0 then begin
          parts := Lit (Buffer.contents buf) :: !parts;
          Buffer.clear buf
        end
      in
      let closed = ref false in
      while not !closed do
        if !i >= n then fail "E0002" lo n "unterminated string literal";
        let c = src.[!i] in
        if c = '"' then (closed := true; incr i)
        else if c = '\\' then begin
          if !i + 1 >= n then fail "E0002" lo n "unterminated string literal";
          let e = src.[!i + 1] in
          (match e with
           | 'n' -> Buffer.add_char buf '\n'
           | 't' -> Buffer.add_char buf '\t'
           | 'r' -> Buffer.add_char buf '\r'
           | '\\' -> Buffer.add_char buf '\\'
           | '"' -> Buffer.add_char buf '"'
           | '{' -> Buffer.add_char buf '{'
           | '}' -> Buffer.add_char buf '}'
           | '0' -> Buffer.add_char buf '\000'
           | _ -> fail "E0006" !i (!i + 2) (Printf.sprintf "unknown escape `\\%c`" e));
          i := !i + 2
        end
        else if c = '{' then begin
          flush ();
          let open_at = !i in
          let start = !i + 1 in
          let depth = ref 1 in
          incr i;
          while !depth > 0 do
            if !i >= n then fail "E0002" open_at n "unterminated interpolation `{...}`";
            (match src.[!i] with
             | '{' -> incr depth; incr i
             | '}' -> decr depth; incr i
             | '"' -> i := skip_string src !i (fun () -> fail "E0002" open_at n "unterminated string literal")
             | _ -> incr i)
          done;
          let inner = String.sub src start (!i - 1 - start) in
          if String.trim inner = "" then
            fail "E0007" open_at !i "empty interpolation `{}`; write `\\{` for a literal brace";
          parts := Interp (inner, base + start) :: !parts
        end
        else (Buffer.add_char buf c; incr i)
      done;
      flush ();
      push (STR (List.rev !parts)) lo !i
    end
    else begin
      let lo = !i in
      let two = if !i + 1 < n then String.sub src !i 2 else "" in
      if List.mem two syms2 then (i := !i + 2; push (SYM two) lo !i)
      else if String.contains syms1 c then (incr i; push (SYM (String.make 1 c)) lo !i)
      else fail "E0001" lo (lo + 1) (Printf.sprintf "unexpected character `%c`" c)
    end
  done;
  push EOF n n;
  Array.of_list (List.rev !toks)
