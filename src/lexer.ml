(* Lexer. Each token records whether a newline came before it; the parser
   uses that to separate statements without semicolons. *)

type strpart = Lit of string | Interp of string * int (* source text, absolute offset *)

type tok =
  | INT of int64
  | FLOAT of float
  | STR of strpart list
  | IDENT of string
  | KW of string
  | SYM of string
  | EOF

type token = { tok : tok; span : Span.t; nl : bool }

(* The tokens of a file, stored flat: a file can have millions, and a record
   and a span for each kept the garbage collector busy. Token [i] is
   [toks.(i)] for [i < count]; its span is read with [lo]/[hi] and [nl_before]
   says whether a newline came before it. Keywords, symbols and each name are
   one shared value, so most tokens allocate nothing. The last token is EOF. *)
type tokens = {
  file : string;
  toks : tok array;
  count : int;
  bounds : Bytes.t; (* each token's lo and hi, 32 bits each *)
  nls : Bytes.t; (* 1 where a newline came before the token *)
}

let lo t i = Int32.to_int (Bytes.get_int32_le t.bounds (8 * i))
let hi t i = Int32.to_int (Bytes.get_int32_le t.bounds ((8 * i) + 4))
let nl_before t i = Bytes.unsafe_get t.nls i <> '\000'
let span t i = { Span.file = t.file; lo = lo t i; hi = hi t i }
let token t i = { tok = t.toks.(i); span = span t i; nl = nl_before t i }

let keywords =
  [ "fn"; "let"; "mut"; "if"; "else"; "match"; "for"; "in"; "while"; "return"; "struct";
    "enum"; "true"; "false"; "group"; "break"; "continue"; "pub"; "use"; "interface"; "dyn"; "select"; "defer" ]

let syms2 = [ "->"; "=>"; "=="; "!="; "<="; ">="; "&&"; "||"; "??"; "|>"; ".."; "<<"; ">>" ]
let syms1 = "(){}[],:;.|+-*/%!?=<>&^~"

(* Every word's one token: KW for a keyword, IDENT for a name. Every
   occurrence of a name is the same string, so the interpreter's lookups
   usually match by physical equality before comparing text. The table is
   open addressing over the source text itself, so looking a word up neither
   copies it out nor calls into C. *)
module Words = struct
  let keys = ref (Array.make 1024 "")
  let toks = ref (Array.make 1024 EOF)
  let used = ref 0

  let hash src lo hi =
    let h = ref 5381 in
    for i = lo to hi - 1 do
      h := (!h * 33) lxor Char.code (String.unsafe_get src i)
    done;
    !h land max_int

  let rec same_from key src lo len i = i = len || (String.unsafe_get key i = String.unsafe_get src (lo + i) && same_from key src lo len (i + 1))
  let same key src lo len = String.length key = len && same_from key src lo len 0

  (* the slot holding [src.[lo..hi)], or the empty slot where it goes *)
  let rec probe keys mask src lo hi i =
    let k = Array.unsafe_get keys i in
    if String.length k = 0 || same k src lo (hi - lo) then i else probe keys mask src lo hi ((i + 1) land mask)

  let slot src lo hi =
    let keys = !keys in
    let mask = Array.length keys - 1 in
    probe keys mask src lo hi (hash src lo hi land mask)

  let rec add key tok =
    if 2 * (!used + 1) > Array.length !keys then begin
      let old_keys = !keys and old_toks = !toks in
      keys := Array.make (2 * Array.length old_keys) "";
      toks := Array.make (2 * Array.length old_keys) EOF;
      used := 0;
      Array.iteri (fun i k -> if String.length k > 0 then add k old_toks.(i)) old_keys
    end;
    let i = slot key 0 (String.length key) in
    !keys.(i) <- key;
    !toks.(i) <- tok;
    incr used

  let () = List.iter (fun k -> add k (KW k)) keywords

  let find src lo hi =
    let i = slot src lo hi in
    if String.length !keys.(i) > 0 then !toks.(i)
    else begin
      let s = String.sub src lo (hi - lo) in
      let t = IDENT s in
      add s t;
      t
    end
end

(* each symbol's one token, so a symbol allocates nothing *)
let sym1_tok = Array.init 256 (fun c -> let ch = Char.chr c in if String.contains syms1 ch then SYM (String.make 1 ch) else EOF)
let sym_range = SYM "..<"
let sym_dots = SYM "..."
let sym_safe = SYM "?."

(* the two-character symbol at [i], or EOF *)
let sym2_tok = Array.make (256 * 256) EOF
let () = List.iter (fun s -> sym2_tok.((Char.code s.[0] * 256) + Char.code s.[1]) <- SYM s) syms2
let sym2_at src i = sym2_tok.((Char.code src.[i] * 256) + Char.code src.[i + 1])

let describe = function
  | INT n -> Printf.sprintf "number `%Ld`" n
  | FLOAT f -> Printf.sprintf "number `%g`" f
  | STR _ -> "a string"
  | IDENT s -> Printf.sprintf "`%s`" s
  | KW s -> Printf.sprintf "keyword `%s`" s
  | SYM s -> Printf.sprintf "`%s`" s
  | EOF -> "end of file"

let fatal code file lo hi msg = raise (Diag.Fatal (Diag.make code { Span.file; lo; hi } msg))
let[@inline] is_digit c = c >= '0' && c <= '9'
let[@inline] is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let[@inline] is_ident c = is_ident_start c || is_digit c

(* Where the text of a multi-line string opened at [i] starts: [i] is the
   first of three quotes followed by nothing but spaces and tabs on their
   line; -1 if they are not. *)
let ml_start src i =
  let n = String.length src in
  if i + 2 < n && src.[i] = '"' && src.[i + 1] = '"' && src.[i + 2] = '"' then begin
    let j = ref (i + 3) in
    while !j < n && (src.[!j] = ' ' || src.[!j] = '\t' || src.[!j] = '\r') do incr j done;
    if !j < n && src.[!j] = '\n' then !j + 1 else -1
  end
  else -1

(* After the spaces and tabs at [j]. *)
let skip_indent src j =
  let n = String.length src in
  let k = ref j in
  while !k < n && (src.[!k] = ' ' || src.[!k] = '\t') do incr k done;
  !k

(* The index after the escape whose backslash is at [j]: `\u{...}` runs to
   its closing brace, every other escape is two bytes. *)
let esc_end src j =
  let n = String.length src in
  if j + 2 < n && src.[j + 1] = 'u' && src.[j + 2] = '{' then begin
    let k = ref (j + 3) in
    while !k < n && !k < j + 12 && src.[!k] <> '}' && src.[!k] <> '"' && src.[!k] <> '\n' do incr k done;
    if !k < n && src.[!k] = '}' then !k + 1 else j + 2
  end
  else j + 2

(* Index just past the string literal starting at [i] (a '"'). Handles
   escapes and nested interpolations, so `"{xs.join("-")}"` works, and
   multi-line strings. *)
let rec skip_string src i on_eof =
  let first = ml_start src i in
  if first >= 0 then skip_indent src (ml_close src first on_eof) + 3
  else begin
    let n = String.length src in
    let rec go j =
      if j >= n then on_eof ()
      else
        match src.[j] with
        | '"' -> j + 1
        | '\\' -> go (esc_end src j)
        | '{' -> go (skip_braces src (j + 1) on_eof)
        | _ -> go (j + 1)
    in
    go (i + 1)
  end

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

(* The start of the line that closes the multi-line string whose first line
   starts at [i]: the first line holding only indentation and three quotes
   (and whatever code follows them). An interpolation's code may span lines. *)
and ml_close src i on_eof =
  let n = String.length src in
  let rec line j =
    let k = skip_indent src j in
    if k + 2 < n && src.[k] = '"' && src.[k + 1] = '"' && src.[k + 2] = '"' then j else rest j
  and rest j =
    if j >= n then on_eof ()
    else
      match src.[j] with
      | '\n' -> line (j + 1)
      | '\\' -> if j + 1 < n && src.[j + 1] = '\n' then line (j + 2) else rest (esc_end src j)
      | '{' -> rest (skip_braces src (j + 1) on_eof)
      | _ -> rest (j + 1)
  in
  line i

(* Comments seen by the last top-level lex, as (start, end, text); the
   formatter puts them back. *)
let comments : (int * int * string) list ref = ref []

let lex ?(base = 0) file (src : string) : tokens =
  if base = 0 then comments := [];
  let n = String.length src in
  (* about one token per five bytes of ordinary code; grown as needed *)
  let cap = (n / 5) + 16 in
  let toks = ref (Array.make cap EOF) and bounds = ref (Bytes.create (8 * cap)) and nls = ref (Bytes.create cap) in
  let count = ref 0 in
  let grow () =
    let cap = 2 * Array.length !toks in
    let t = Array.make cap EOF in
    Array.blit !toks 0 t 0 !count;
    toks := t;
    bounds := Bytes.extend !bounds 0 (8 * cap - Bytes.length !bounds);
    nls := Bytes.extend !nls 0 (cap - Bytes.length !nls)
  in
  let i = ref 0 in
  let nl = ref false in
  let push tok lo hi =
    let k = !count in
    if k = Array.length !toks then grow ();
    Array.unsafe_set !toks k tok;
    Bytes.set_int32_le !bounds (8 * k) (Int32.of_int (base + lo));
    Bytes.set_int32_le !bounds ((8 * k) + 4) (Int32.of_int (base + hi));
    Bytes.unsafe_set !nls k (if !nl then '\001' else '\000');
    count := k + 1;
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
    else if c = '0' && !i + 1 < n && (src.[!i + 1] = 'x' || src.[!i + 1] = 'b') then begin
      (* 0xff, 0b1010: a whole number in base 16 or 2, which must fit in Int *)
      let lo = !i in
      let hex = src.[!i + 1] = 'x' in
      let radix = if hex then 16 else 2 in
      let digit c =
        if c >= '0' && c <= '9' then Char.code c - 48
        else if c >= 'a' && c <= 'f' then Char.code c - 87
        else if c >= 'A' && c <= 'F' then Char.code c - 55
        else 99
      in
      i := !i + 2;
      let v = ref 0L and big = ref false and seen = ref false in
      while !i < n && (is_ident src.[!i]) do
        let c = src.[!i] in
        if c <> '_' then begin
          let d = digit c in
          if d >= radix then
            fail "E0005" !i (!i + 1)
              (Printf.sprintf "`%c` is not a %s digit" c (if hex then "hex" else "binary"));
          seen := true;
          if Int64.compare !v (Int64.div (Int64.sub Int64.max_int (Int64.of_int d)) (Int64.of_int radix)) > 0 then big := true
          else v := Int64.add (Int64.mul !v (Int64.of_int radix)) (Int64.of_int d)
        end;
        incr i
      done;
      if not !seen then
        fail "E0005" lo !i (Printf.sprintf "`%s` has no digits; write it as in `%s`" (String.sub src lo (!i - lo)) (if hex then "0xff" else "0b1010"));
      if !big then fail "E0005" lo !i (Printf.sprintf "integer literal `%s` is too large" (String.sub src lo (!i - lo)));
      push (INT !v) lo !i
    end
    else if is_digit c then begin
      let lo = !i in
      (* `t.0.1` is two tuple fields, not the float 0.1 *)
      let after_dot =
        !count > 0
        && (match !toks.(!count - 1) with SYM ("." | "?.") -> Int32.to_int (Bytes.get_int32_le !bounds ((8 * (!count - 1)) + 4)) = base + lo | _ -> false)
      in
      while !i < n && (is_digit src.[!i] || src.[!i] = '_') do incr i done;
      let is_float = (not after_dot) && !i + 1 < n && src.[!i] = '.' && is_digit src.[!i + 1] in
      if is_float then begin
        incr i;
        while !i < n && (is_digit src.[!i] || src.[!i] = '_') do incr i done
      end;
      (* exponent: 1e9, 2.5E-3 *)
      let has_exp =
        (not after_dot) && !i < n
        && (src.[!i] = 'e' || src.[!i] = 'E')
        && (( !i + 1 < n && is_digit src.[!i + 1])
           || (!i + 2 < n && (src.[!i + 1] = '+' || src.[!i + 1] = '-') && is_digit src.[!i + 2]))
      in
      if has_exp then begin
        i := !i + 2;
        while !i < n && is_digit src.[!i] do incr i done
      end;
      let is_float = is_float || has_exp in
      let text = String.sub src lo (!i - lo) in
      let text = if String.contains text '_' then String.concat "" (String.split_on_char '_' text) else text in
      if is_float then push (FLOAT (float_of_string text)) lo !i
      else
        match Int64.of_string_opt text with
        | Some v -> push (INT v) lo !i
        | None -> fail "E0005" lo !i (Printf.sprintf "integer literal `%s` is too large" text)
    end
    else if is_ident_start c then begin
      let lo = !i in
      while !i < n && is_ident src.[!i] do incr i done;
      push (Words.find src lo !i) lo !i
    end
    else if c = '"' then begin
      let lo = !i in
      let parts = ref [] in
      let buf = Buffer.create 16 in
      let flush () =
        if Buffer.length buf > 0 then begin
          parts := Lit (Buffer.contents buf) :: !parts;
          Buffer.clear buf
        end
      in
      (* the escape `\e` at [j]; the index after it *)
      let escape j =
        let e = src.[j + 1] in
        let one ch = Buffer.add_char buf ch; j + 2 in
        match e with
        | 'n' -> one '\n'
        | 't' -> one '\t'
        | 'r' -> one '\r'
        | '\\' -> one '\\'
        | '"' -> one '"'
        | '{' -> one '{'
        | '}' -> one '}'
        | '0' -> one '\000'
        | 'u' ->
          let hi = esc_end src j in
          let digits = if hi > j + 2 then String.sub src (j + 3) (hi - j - 4) else "" in
          let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') in
          if hi = j + 2 || digits = "" || String.length digits > 6 || not (String.for_all is_hex digits) then
            fail "E0006" j (if hi > j + 2 then hi else j + 2) "`\\u` takes 1 to 6 hex digits in braces, as in `\\u{e9}`";
          let cp = int_of_string ("0x" ^ digits) in
          if cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF) then
            fail "E0006" j hi (Printf.sprintf "`\\u{%s}` is not a Unicode character%s" digits
              (if cp > 0x10FFFF then ": the last is `10ffff`" else ": `d800` to `dfff` are reserved for UTF-16"));
          Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
          hi
        | _ -> fail "E0006" j (j + 2) (Printf.sprintf "unknown escape `\\%c`" e)
      in
      (* the interpolation opened at [open_at], which must close before
         [limit]; the index after it *)
      let interp open_at limit =
        flush ();
        let start = open_at + 1 in
        let depth = ref 1 in
        let j = ref start in
        while !depth > 0 do
          if !j >= limit then fail "E0002" open_at limit "unterminated interpolation `{...}`";
          (match src.[!j] with
           | '{' -> incr depth; incr j
           | '}' -> decr depth; incr j
           | '"' -> j := skip_string src !j (fun () -> fail "E0002" open_at n "unterminated string literal")
           | '\\' ->
             (* code has no backslashes, so this is an escaped quote meant for a nested string *)
             fail "E0006" !j (!j + 2)
               "a `\\` inside `{...}`: an interpolation holds code, so a string in it uses plain quotes, as in `\"{f(\"x\")}\"`"
           | _ -> incr j)
        done;
        let inner = String.sub src start (!j - 1 - start) in
        if String.trim inner = "" then
          fail "E0007" open_at !j "empty interpolation `{}`; write `\\{` for a literal brace";
        parts := Interp (inner, base + start) :: !parts;
        !j
      in
      if !i + 2 < n && src.[!i + 1] = '"' && src.[!i + 2] = '"' then begin
        (* a multi-line string: the closing line's indentation is taken off
           every line, and the line breaks after the opening quotes and
           before the closing ones are not part of it *)
        let cs = ml_start src lo in
        if cs < 0 then begin
          let j = ref (lo + 3) in
          while !j < n && (src.[!j] = ' ' || src.[!j] = '\t' || src.[!j] = '\r') do incr j done;
          if !j >= n then fail "E0002" lo n "unterminated multi-line string; end it with `\"\"\"` on a line of its own"
          else fail "E0009" lo (lo + 3) "a multi-line string starts with `\"\"\"` at the end of a line; its text begins on the next line"
        end;
        let cls = ml_close src cs (fun () -> fail "E0002" lo n "unterminated multi-line string; end it with `\"\"\"` on a line of its own") in
        let k = skip_indent src cls in
        let ind = k - cls in
        let indent = String.sub src cls ind in
        (* the text runs up to the line break before the closing line *)
        let ce = cls - 1 in
        (* where the text of the line starting at [j] starts *)
        let start_line j =
          if j + ind <= ce && String.sub src j ind = indent then j + ind
          else begin
            let w = ref j in
            while !w < ce && (src.[!w] = ' ' || src.[!w] = '\t' || src.[!w] = '\r') do incr w done;
            if !w = ce || src.[!w] = '\n' then !w
            else
              fail "E0009" j (if !w > j then !w else j + 1)
                "this line is less indented than the closing `\"\"\"`; every line of a multi-line string starts with the closing line's indentation"
          end
        in
        if cls > cs then begin
          let j = ref (start_line cs) in
          while !j < ce do
            let c = src.[!j] in
            if c = '\n' then (Buffer.add_char buf '\n'; j := start_line (!j + 1))
            else if c = '\r' && src.[!j + 1] = '\n' then incr j
            else if c = '\\' then begin
              let e = src.[!j + 1] in
              if e = '\n' || (e = '\r' && src.[!j + 2] = '\n') then begin
                (* a line ending in `\` goes on with the next, with no line break *)
                let at = if e = '\n' then !j + 1 else !j + 2 in
                if at >= ce then fail "E0009" !j (!j + 1) "a `\\` at the end of the last line has no next line to join";
                j := start_line (at + 1)
              end
              else j := escape !j
            end
            else if c = '{' then j := interp !j ce
            else (Buffer.add_char buf c; incr j)
          done
        end;
        i := k + 3
      end
      else begin
        incr i;
        let closed = ref false in
        while not !closed do
          if !i >= n then fail "E0002" lo n "unterminated string literal";
          let c = src.[!i] in
          if c = '"' then (closed := true; incr i)
          else if c = '\\' then begin
            if !i + 1 >= n then fail "E0002" lo n "unterminated string literal";
            i := escape !i
          end
          else if c = '{' then i := interp !i n
          else (Buffer.add_char buf c; incr i)
        done
      end;
      flush ();
      push (STR (List.rev !parts)) lo !i
    end
    else begin
      let lo = !i in
      let two = if !i + 1 < n then sym2_at src !i else EOF in
      if !i + 2 < n && c = '.' && src.[!i + 1] = '.' && src.[!i + 2] = '<' then (i := !i + 3; push sym_range lo !i)
        (* `...` marks the variadic arguments of a foreign function *)
      else if !i + 2 < n && c = '.' && src.[!i + 1] = '.' && src.[!i + 2] = '.' then (i := !i + 3; push sym_dots lo !i)
        (* `?.` is safe navigation, unless it is `?` before a range: `x?..<n` *)
      else if c = '?' && !i + 1 < n && src.[!i + 1] = '.' && not (!i + 2 < n && src.[!i + 2] = '.') then (i := !i + 2; push sym_safe lo !i)
      else match two with
      | EOF -> (
        match sym1_tok.(Char.code c) with
        | EOF -> fail "E0001" lo (lo + 1) (Printf.sprintf "unexpected character `%c`" c)
        | t -> incr i; push t lo !i)
      | t -> i := !i + 2; push t lo !i
    end
  done;
  push EOF n n;
  { file; toks = !toks; count = !count; bounds = !bounds; nls = !nls }
