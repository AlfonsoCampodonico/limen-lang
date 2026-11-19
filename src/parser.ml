(* Recursive-descent parser with Pratt-style binary expressions.

   Newline rule: a newline ends an expression unless the next line starts
   with `.` (method chains). So `f\n(x)` is two statements, not a call. *)
open Ast
open Lexer

type st = { toks : tokens; mutable pos : int; file : string }

(* The token at [st.pos], its kind and whether a newline came before it,
   read without building a token. *)
let cur st = st.toks.toks.(st.pos)
let cur_nl st = nl_before st.toks st.pos
let tok_at st i = st.toks.toks.(min i (st.toks.count - 1))
let next st = tok_at st (st.pos + 1)
let peek st = token st.toks st.pos
let cur_span st = span st.toks st.pos
let skip st = match cur st with EOF -> () | _ -> st.pos <- st.pos + 1

let advance st =
  let t = peek st in
  skip st;
  t

let fail (t : token) expected =
  raise
    (Diag.Fatal
       (Diag.make "E0003" t.span (Printf.sprintf "expected %s, found %s" expected (describe t.tok))))

let sym_is tok s = match tok with SYM t -> String.equal t s | _ -> false
let is_sym st s = match cur st with SYM t -> String.equal t s | _ -> false
let is_kw st s = match cur st with KW t -> String.equal t s | _ -> false
let accept_sym st s = if is_sym st s then (skip st; true) else false
let expect_sym st s = if is_sym st s then advance st else fail (peek st) (Printf.sprintf "`%s`" s)
let expect_kw st s = if is_kw st s then advance st else fail (peek st) (Printf.sprintf "`%s`" s)

(* [expect_sym] and [expect_kw] for when the token itself is not needed *)
let need_sym st s = if is_sym st s then skip st else ignore (fail (peek st) (Printf.sprintf "`%s`" s))
let need_kw st s = if is_kw st s then skip st else ignore (fail (peek st) (Printf.sprintf "`%s`" s))

let expect_ident st =
  match cur st with
  | IDENT s ->
    let sp = span st.toks st.pos in
    skip st;
    (s, sp)
  | KW k ->
    raise
      (Diag.Fatal
         (Diag.make ~notes:[ "keywords cannot be used as names; pick another name, for example `" ^ k ^ "s` or `my_" ^ k ^ "`" ]
            "E0003" (peek st).span
            (Printf.sprintf "expected a name, found keyword `%s`" k)))
  | _ -> fail (peek st) "a name"

let last_span st = span st.toks (max 0 (st.pos - 1))
let merge = Span.merge
let is_upper s = s <> "" && s.[0] >= 'A' && s.[0] <= 'Z'
let mk d sp = { edesc = d; espan = sp; ety = Types.TErr; einst = []; ecallee = None }

(* Comma-separated list up to [close]; a newline also separates items. *)

(* names for the value inside an Option in `a?.b`; reset per file so a
   formatted file parses to the same tree *)
let safe_count = ref 0

(* What `let` and `for` can take apart: names and `_`, in tuples and struct
   patterns, as deep as they like. *)
let rec irrefutable_tuple (p : pat) =
  match p.pdesc with
  | PBind _ | PWild -> true
  | PTuple ps -> List.for_all irrefutable_tuple ps
  | PStruct (_, _, fs, _) -> List.for_all (fun (_, _, q) -> irrefutable_tuple q) fs
  | _ -> false

(* Whether a struct pattern starts [k] tokens after the current one:
   `Point {` or `geo.Point {`. *)
let struct_pat_ahead ?(k = 0) st =
  let at i = tok_at st (st.pos + k + i) in
  match (at 0, at 1) with
  | IDENT v, SYM "{" -> is_upper v
  | IDENT _, SYM "." -> ( match (at 2, at 3) with IDENT v, SYM "{" -> is_upper v | _ -> false)
  | _ -> false

let comma_list st close f =
  let rec go acc =
    if accept_sym st close then List.rev acc
    else begin
      let x = f st in
      if accept_sym st "," then go (x :: acc)
      else if accept_sym st close then List.rev (x :: acc)
      else if cur_nl st then go (x :: acc)
      else fail (peek st) (Printf.sprintf "`,` or `%s`" close)
    end
  in
  go []

let rec parse_type st : ty_expr =
  let t = peek st in
  match t.tok with
  | KW "dyn" ->
    (* `dyn Shape` is written in the AST as `dyn[Shape]`, `dyn Container[Int]` as `dyn[Container[Int]]` *)
    skip st;
    let a, asp = expect_ident st in
    let name, nsp =
      if is_sym st "." then begin
        skip st;
        let b, bsp = expect_ident st in
        (a ^ "." ^ b, merge asp bsp)
      end
      else (a, asp)
    in
    let args, nsp =
      if is_sym st "[" && not (cur_nl st) then begin
        skip st;
        let args = comma_list st "]" parse_type in
        (args, merge nsp (last_span st))
      end
      else ([], nsp)
    in
    { tdesc = TyName ("dyn", [ { tdesc = TyName (name, args); tspan = nsp } ]); tspan = merge t.span nsp }
  | SYM "(" ->
    skip st;
    if is_sym st ")" then
      let r = advance st in
      { tdesc = TyUnit; tspan = merge t.span r.span }
    else begin
      (* `(A, B)` is a tuple type; `(A)` is just A *)
      let ts = comma_list st ")" parse_type in
      let sp = merge t.span (last_span st) in
      match ts with
      | [ one ] -> { one with tspan = sp }
      | _ -> { tdesc = TyName (tuple_name (List.length ts), ts); tspan = sp }
    end
  | KW "fn" ->
    skip st;
    need_sym st "(";
    let ps = comma_list st ")" parse_type in
    (* `fn(A)` returns nothing, as a function declared without `->` does:
       its result is `()`, with an empty span after the `)` so the
       formatter writes it back the way it was written *)
    if accept_sym st "->" then
      let r = parse_type st in
      { tdesc = TyFn (ps, r); tspan = merge t.span r.tspan }
    else
      let close = last_span st in
      { tdesc = TyFn (ps, { tdesc = TyUnit; tspan = { close with lo = close.hi } }); tspan = merge t.span close }
  | INT n ->
    skip st;
    { tdesc = TyInt (if n > Int64.of_int max_int then max_int else Int64.to_int n); tspan = t.span }
  | IDENT n0 ->
    skip st;
    (* a type from another module: `json.Value` *)
    let n =
      if is_sym st "." then begin
        skip st;
        let m, _ = expect_ident st in
        n0 ^ "." ^ m
      end
      else n0
    in
    if is_sym st "[" && not (cur_nl st) then begin
      skip st;
      let args = comma_list st "]" parse_type in
      { tdesc = TyName (n, args); tspan = merge t.span (last_span st) }
    end
    else { tdesc = TyName (n, []); tspan = merge t.span (last_span st) }
  | _ -> fail t "a type"

(* Precedence levels are doubled so a range fits between comparison and
   arithmetic: `0..<n + 1` is `0..<(n + 1)`. *)
let binop_of = function
  | SYM "||" -> Some (2, Or)
  | SYM "&&" -> Some (4, And)
  | SYM "==" -> Some (6, Eq)
  | SYM "!=" -> Some (6, Ne)
  | SYM "<" -> Some (8, Lt)
  | SYM "<=" -> Some (8, Le)
  | SYM ">" -> Some (8, Gt)
  | SYM ">=" -> Some (8, Ge)
  | SYM "..<" -> Some (9, Range)
  | SYM "+" -> Some (10, Add)
  | SYM "-" -> Some (10, Sub)
  | SYM "*" -> Some (12, Mul)
  | SYM "/" -> Some (12, Div)
  | SYM "%" -> Some (12, Mod)
  (* as in Go: `|` and `^` with `+`, `&` and the shifts with `*`, so
     `x & mask == 0` is `(x & mask) == 0` *)
  | SYM "|" -> Some (10, BitOr)
  | SYM "^" -> Some (10, BitXor)
  | SYM "&" -> Some (12, BitAnd)
  | SYM "<<" -> Some (12, Shl)
  | SYM ">>" -> Some (12, Shr)
  (* tighter than arithmetic: `m.get(k) ?? 0 + 1` is `(m.get(k) ?? 0) + 1` *)
  | SYM "??" -> Some (14, Else)
  | _ -> None

(* [ns] = "no struct literal": set in `if`/`while`/`for`/`match` heads so that
   `if x == y {` does not parse `y { ... }` as a struct literal. *)
let rec parse_expr st = parse_bin st false 0
and parse_expr_ns st = parse_bin st true 0

(* `|>` is the loosest binary operator (level 1, left-associative) and may
   start a line, so a pipeline can be written one step per line. *)
and parse_bin st ns minp =
  let lhs = ref (parse_unary st ns) in
  let go = ref true in
  while !go do
    let tok = cur st in
    match if sym_is tok "|>" then None else if cur_nl st then None else binop_of tok with
    | _ when sym_is tok "|>" && minp < 1 ->
      skip st;
      let rhs = parse_bin st ns 1 in
      if pipe_call !lhs rhs = None then
        raise
          (Diag.Fatal
             (Diag.make "E0003" rhs.espan
                ~notes:[ "`x |> f(a)` means `f(x, a)`: the value goes in as the first argument" ]
                "the right side of `|>` must be a function or a call, as in `x |> f`, `x |> f(a)` or `x |> list.map(g)`"));
      lhs := mk (EPipe (!lhs, rhs)) (merge (!lhs).espan rhs.espan)
    | Some (p, op) when p > minp ->
      skip st;
      let rhs = parse_bin st ns p in
      lhs := mk (EBin (op, !lhs, rhs)) (merge (!lhs).espan rhs.espan)
    | _ -> go := false
  done;
  !lhs

and parse_unary st ns =
  match cur st with
  | SYM "-" ->
    let sp = cur_span st in
    skip st;
    let e = parse_unary st ns in
    mk (EUn (Neg, e)) (merge sp e.espan)
  | SYM "!" ->
    let sp = cur_span st in
    skip st;
    let e = parse_unary st ns in
    mk (EUn (Not, e)) (merge sp e.espan)
  | SYM "~" ->
    let sp = cur_span st in
    skip st;
    let e = parse_unary st ns in
    mk (EUn (BitNot, e)) (merge sp e.espan)
  | _ -> parse_postfix st ns (parse_primary st ns)

(* [in_safe]: parsing the rest of a `?.` chain, which takes every postfix
   operation up to a `?` (that one applies to the whole chain). *)
and parse_postfix ?(in_safe = false) st ns e =
  let nl = cur_nl st in
  match cur st with
  | SYM "(" when not nl ->
    skip st;
    let args = call_args st ns in
    parse_postfix ~in_safe st ns (mk (ECall (e, args)) (merge e.espan (last_span st)))
  | SYM "{" when (not nl) && (match e.edesc with EVar n -> not (is_upper n) | _ -> false) && starts_trailing st ns ->
    (* `catch { ... }`: a trailing closure and no parentheses *)
    let args = trailing_closure st ns in
    parse_postfix ~in_safe st ns (mk (ECall (e, args)) (merge e.espan (last_span st)))
  | SYM "[" when not nl ->
    skip st;
    let i = parse_expr st in
    let r = expect_sym st "]" in
    parse_postfix ~in_safe st ns (mk (EIndex (e, i)) (merge e.espan r.span))
  | SYM "." ->
    skip st;
    parse_postfix ~in_safe st ns (parse_member st ns e)
  | SYM "?." ->
    (* safe navigation: the rest of the chain runs on the value inside the
       Option, and None short-circuits all of it *)
    let sp = cur_span st in
    skip st;
    incr safe_count;
    let x = Printf.sprintf "safe$%d" !safe_count in
    let hole = mk (EVar x) sp in
    let rest = parse_postfix ~in_safe:true st ns (parse_member st ns hole) in
    let node = mk (ESafe (e, x, rest, false)) (merge e.espan rest.espan) in
    if in_safe then node else parse_postfix ~in_safe st ns node
  | SYM "?" when (not nl) && not in_safe -> (
    let sp = cur_span st in
    skip st;
    (* `x? "reading config"`: a string on the same line adds context to the error *)
    match cur st with
    | STR _ when not (cur_nl st) ->
      let c = parse_primary st ns in
      parse_postfix ~in_safe st ns (mk (ETry (e, Some c)) (merge e.espan c.espan))
    | _ -> parse_postfix ~in_safe st ns (mk (ETry (e, None)) (merge e.espan sp)))
  | _ -> e

(* `.name`, `.name(args)` or `.0` after the dot *)
and parse_member st ns e =
  match cur st with
  | INT k ->
    let it = advance st in
    mk (EField (e, Int64.to_string k, it.span)) (merge e.espan it.span)
  | _ ->
    let name, nsp = expect_ident st in
    if is_sym st "(" && not (cur_nl st) then begin
      skip st;
      let args = call_args st ns in
      mk (EMethod (e, name, nsp, args)) (merge e.espan (last_span st))
    end
    else if is_sym st "{" && (not (cur_nl st)) && starts_trailing st ns then begin
      (* `items.each { |x| ... }` *)
      let args = trailing_closure st ns in
      mk (EMethod (e, name, nsp, args)) (merge e.espan (last_span st))
    end
    else mk (EField (e, name, nsp)) (merge e.espan nsp)

(* The arguments of a call whose `(` has been read, with a trailing closure
   after the `)` if there is one. *)
and call_args st ns =
  let args = comma_list st ")" parse_arg in
  match trailing_closure st ns with [] -> args | closure -> args @ closure

(* A call argument: an expression, or `name: expression` naming the
   parameter it is for. *)
and parse_arg st =
  match (cur st, next st) with
  | IDENT n, SYM ":" ->
    let t = advance st in
    skip st;
    let v = parse_expr st in
    mk (ENamed (n, t.span, v)) (merge t.span v.espan)
  | _ -> parse_expr st

(* Is the `{` ahead the start of a trailing closure? Not in the head of an
   `if`, `while`, `for`, `match` or group deadline, where `{` opens the body;
   there a closure goes inside the parentheses. A body that starts with
   closure parameters is reported, since that is a trailing closure written
   where it cannot go. *)
and starts_trailing st ns =
  if not ns then true
  else begin
    (match next st with
     | SYM "|" | SYM "||" ->
       raise
         (Diag.Fatal
            (Diag.make "E0003" (peek st).span
               ~notes:[ "in the head of `if`, `while`, `for` and `match`, `{` starts the body; pass the closure in parentheses, as in `xs.filter(|x| x > 0)`, or wrap the whole condition in parentheses" ]
               "a trailing closure cannot be used in a condition"))
     | _ -> ());
    false
  end

(* `f(a) { |x| ... }`: a closure written after the call's parentheses, passed
   as its last argument. [] when there is none. *)
and trailing_closure st ns =
  if is_sym st "{" && (not (cur_nl st)) && starts_trailing st ns then begin
    let lb = advance st in
    let params =
      match cur st with
      | SYM "||" -> skip st; []
      | SYM "|" ->
        skip st;
        comma_list st "|" (fun st ->
            let n, sp = expect_ident st in
            let ty = if accept_sym st ":" then Some (parse_type st) else None in
            (n, ty, sp))
      | _ -> []
    in
    let b = parse_block_body st lb in
    [ mk (EClosure (params, mk (EBlock b) b.bspan)) b.bspan ]
  end
  else []

and parse_primary st ns =
  let sp = cur_span st in
  match cur st with
  | INT n -> skip st; mk (EInt n) sp
  | FLOAT f -> skip st; mk (EFloat f) sp
  | KW "true" -> skip st; mk (EBool true) sp
  | KW "false" -> skip st; mk (EBool false) sp
  | STR parts ->
    skip st;
    mk (EStr (List.map (str_part st) parts)) sp
  | IDENT name ->
    skip st;
    if (not ns) && is_upper name && is_sym st "{" then parse_struct_lit st name sp
    else if (not ns) && is_sym st "." && (match next st with IDENT b -> is_upper b | _ -> false) && sym_is (tok_at st (st.pos + 2)) "{"
            && not (nl_before st.toks (min (st.pos + 1) (st.toks.count - 1))) then begin
      (* a struct from another module: `json.Field { ... }` *)
      skip st;
      let b, bsp = expect_ident st in
      parse_struct_lit st (name ^ "." ^ b) (merge sp bsp)
    end
    else mk (EVar name) sp
  | SYM "(" ->
    skip st;
    if is_sym st ")" then
      let r = advance st in
      mk EUnit (merge sp r.span)
    else
      let e = parse_expr st in
      if is_sym st "," then begin
        (* a tuple: `(a, b)` *)
        skip st;
        let rest = comma_list st ")" parse_expr in
        let es = e :: rest in
        let whole = merge sp (last_span st) in
        mk (EStruct (tuple_name (List.length es), sp, List.mapi (fun i x -> (string_of_int i, x.espan, x)) es)) whole
      end
      else
        let r = expect_sym st ")" in
        { e with espan = merge sp r.span }
  | SYM "[" ->
    skip st;
    let es = comma_list st "]" parse_expr in
    mk (EList es) (merge sp (last_span st))
  | SYM "{" when is_with_lit st -> parse_with st
  | SYM "{" ->
    let b = parse_block st in
    mk (EBlock b) b.bspan
  | KW "if" -> parse_if st
  | KW "match" -> parse_match st
  | KW "select" -> parse_select st
  | SYM "|" ->
    skip st;
    let params =
      comma_list st "|" (fun st ->
          let n, sp = expect_ident st in
          let ty = if accept_sym st ":" then Some (parse_type st) else None in
          (n, ty, sp))
    in
    let body = parse_expr st in
    mk (EClosure (params, body)) (merge sp body.espan)
  | SYM "||" ->
    skip st;
    let body = parse_expr st in
    mk (EClosure ([], body)) (merge sp body.espan)
  | KW "group" ->
    skip st;
    let name, _ = expect_ident st in
    let interleaved = match cur st with IDENT "interleaved" -> skip st; true | _ -> false in
    let until =
      match cur st with
      | IDENT "until" -> (
        skip st;
        let d = parse_expr_ns st in
        match d.edesc with
        | EMethod (clk, "after", _, [ ms ]) -> Some (clk, ms)
        | _ -> raise (Diag.Fatal (Diag.make "E0003" d.espan "a group deadline is `group g until clock.after(ms) { ... }`")))
      | _ -> None
    in
    let b = parse_block st in
    mk (EGroup (name, interleaved, until, b)) (merge sp b.bspan)
  | _ -> fail (peek st) "an expression"

and str_part st = function
  | Lit s -> SPLit s
  | Interp (src, off) ->
    let toks = Lexer.lex ~base:off st.file src in
    let sub = { toks; pos = 0; file = st.file } in
    let e = parse_expr sub in
    if cur sub <> EOF then fail (peek sub) "`}` to close the interpolation";
    SPExpr e

(* At a `{`: is this `{ base with field: ... }` rather than a block? It is
   when `with` follows the base on the first line, outside any brackets, and
   is followed by a field: `with x:`, `with x,` or `with x }`. *)
and is_with_lit st =
  let n = st.toks.count in
  let rec scan i depth =
    if i >= n then false
    else
      if depth = 0 && nl_before st.toks i && i > st.pos + 1 then false
      else
        match st.toks.toks.(i) with
        | EOF -> false
        | SYM ("(" | "[" | "{") -> scan (i + 1) (depth + 1)
        | SYM (")" | "]" | "}") -> if depth = 0 then false else scan (i + 1) (depth - 1)
        | SYM ";" when depth = 0 -> false
        | IDENT "with" when depth = 0 && i > st.pos + 1 && i + 2 < n -> (
          match (st.toks.toks.(i + 1), st.toks.toks.(i + 2)) with
          | IDENT _, SYM (":" | "," | "}") -> true
          | _ -> scan (i + 1) depth)
        | _ -> scan (i + 1) depth
  in
  scan (st.pos + 1) 0

and parse_with st =
  let lb = expect_sym st "{" in
  let base = parse_expr st in
  (match cur st with IDENT "with" -> skip st | _ -> fail (peek st) "`with`");
  let fields =
    comma_list st "}" (fun st ->
        let f, fsp = expect_ident st in
        if accept_sym st ":" then (f, fsp, parse_expr st) else (f, fsp, mk (EVar f) fsp))
  in
  mk (EWith (base, fields)) (merge lb.span (last_span st))

and parse_struct_lit st name nsp =
  need_sym st "{";
  let fields =
    comma_list st "}" (fun st ->
        let f, fsp = expect_ident st in
        if accept_sym st ":" then (f, fsp, parse_expr st) else (f, fsp, mk (EVar f) fsp))
  in
  mk (EStruct (name, nsp, fields)) (merge nsp (last_span st))

and parse_if st =
  let t = expect_kw st "if" in
  let c = parse_expr_ns st in
  let b = parse_block st in
  if is_kw st "else" then begin
    skip st;
    let e =
      if is_kw st "if" then parse_if st
      else
        let b2 = parse_block st in
        mk (EBlock b2) b2.bspan
    in
    mk (EIf (c, b, Some e)) (merge t.span e.espan)
  end
  else mk (EIf (c, b, None)) (merge t.span b.bspan)

and parse_match st =
  let t = expect_kw st "match" in
  let s = parse_expr_ns st in
  need_sym st "{";
  let arms = ref [] in
  while not (accept_sym st "}") do
    let p = parse_pat st in
    let guard = if is_kw st "if" then (skip st; Some (parse_expr st)) else None in
    need_sym st "=>";
    let body = parse_expr st in
    arms := { apat = p; aguard = guard; abody = body } :: !arms;
    if (not (accept_sym st ",")) && (not (is_sym st "}")) && not (cur_nl st) then
      fail (peek st) "`,` or a new line between match arms"
  done;
  mk (EMatch (s, List.rev !arms)) (merge t.span (last_span st))

and parse_select st =
  let t = expect_kw st "select" in
  need_sym st "{";
  let arms = ref [] in
  while not (accept_sym st "}") do
    let a = peek st in
    let op =
      match (a.tok, next st) with
      | IDENT "_", SYM "=>" -> skip st; SelDefault
      | IDENT n, SYM "=" ->
        (* `v = ch.recv()` *)
        skip st;
        skip st;
        let e = parse_expr st in
        (match e.edesc with
         | EMethod (ch, "recv", _, []) -> SelRecv ((if n = "_" then None else Some (n, a.span)), ch)
         | _ -> raise (Diag.Fatal (Diag.make "E0003" e.espan "a `select` arm receives with `name = ch.recv()`")))
      | _ -> (
        let e = parse_expr st in
        match e.edesc with
        | EMethod (ch, "recv", _, []) -> SelRecv (None, ch)
        | EMethod (ch, "send", _, [ v ]) -> SelSend (ch, v)
        | EMethod (clk, "after", _, [ ms ]) -> SelAfter (clk, ms)
        | _ ->
          raise
            (Diag.Fatal
               (Diag.make "E0003" e.espan
                  "a `select` arm is `name = ch.recv()`, `ch.send(value)`, `clock.after(ms)` (a timeout) or `_` (when nothing is ready)")))
    in
    let guard = if is_kw st "if" then (skip st; Some (parse_expr st)) else None in
    need_sym st "=>";
    let body = parse_expr st in
    arms := { sop = op; sguard = guard; sbody = body; sspan_ = merge a.span body.espan } :: !arms;
    if (not (accept_sym st ",")) && (not (is_sym st "}")) && not (cur_nl st) then
      fail (peek st) "`,` or a new line between select arms"
  done;
  mk (ESelect (List.rev !arms)) (merge t.span (last_span st))

(* A pattern, with alternatives: `A | B`, and a name for the whole: `p as
   name`, which takes in every alternative before it (`A | B as x` is
   `(A | B) as x`). Alternatives inside alternatives, `(A | B) | C`, are
   flattened into one list. *)
and parse_pat st =
  let first = parse_pat1 st in
  let p =
    if not (is_sym st "|") then first
    else begin
      let alts = ref (List.rev (alternatives first)) in
      while accept_sym st "|" do
        alts := List.rev_append (alternatives (parse_pat1 st)) !alts
      done;
      { pdesc = POr (List.rev !alts); pspan = merge first.pspan (last_span st) }
    end
  in
  match cur st with
  | IDENT "as" ->
    skip st;
    let n, nsp = expect_ident st in
    if n = "_" then raise (Diag.Fatal (Diag.make "E0003" nsp "`as` needs a name for the value; `as _` names nothing, so leave it out"));
    { pdesc = PAs (p, n, nsp); pspan = merge p.pspan nsp }
  | _ -> p

(* The fields of a struct pattern after its `{`: `x`, `x: pattern`, and a
   last `..` for the fields not named. *)
and struct_pat_fields st =
  let rest = ref false in
  let fields =
    comma_list st "}" (fun st ->
        if !rest then raise (Diag.Fatal (Diag.make "E0003" (peek st).span "`..` comes last in a struct pattern"));
        if is_sym st ".." then (skip st; rest := true; None)
        else
          let f, fsp = expect_ident st in
          if accept_sym st ":" then Some (f, fsp, parse_pat st) else Some (f, fsp, { pdesc = PBind f; pspan = fsp }))
  in
  (List.filter_map Fun.id fields, !rest)

(* The items of a tuple, variant or list pattern up to [close]: patterns,
   and at most one `..` for the rest, which a list pattern can name:
   `[first, ..rest]`. *)
and pat_items st close ~list =
  let seen = ref false in
  comma_list st close (fun st ->
      if is_sym st ".." then begin
        let t = advance st in
        if !seen then raise (Diag.Fatal (Diag.make "E0003" t.span "a pattern can have only one `..`"));
        seen := true;
        match cur st with
        | IDENT n when n <> "_" && not (cur_nl st) ->
          let nt = advance st in
          if not list then
            raise
              (Diag.Fatal
                 (Diag.make "E0003" (merge t.span nt.span)
                    ~notes:[ "here `..` alone stands for the remaining parts" ]
                    "only a list pattern can name the rest, as in `[first, ..rest]`"));
          { pdesc = PRest (Some n); pspan = merge t.span nt.span }
        | _ -> { pdesc = PRest None; pspan = t.span }
      end
      else parse_pat st)

and parse_pat1 st =
  let t = peek st in
  let mkp d sp = { pdesc = d; pspan = sp } in
  match t.tok with
  | IDENT "_" -> skip st; mkp PWild t.span
  | INT n -> skip st; mkp (PInt n) t.span
  | SYM "-" -> (
    skip st;
    match cur st with
    | INT n ->
      let t2 = advance st in
      mkp (PInt (Int64.neg n)) (merge t.span t2.span)
    | _ -> fail (peek st) "a number")
  | STR [] -> skip st; mkp (PStr "") t.span
  | STR [ Lit s ] -> skip st; mkp (PStr s) t.span
  | STR _ -> fail t "a plain string (no interpolation) in a pattern"
  | KW "true" -> skip st; mkp (PBool true) t.span
  | KW "false" -> skip st; mkp (PBool false) t.span
  | IDENT a ->
    skip st;
    let q, v, vsp =
      if is_sym st "." then begin
        skip st;
        let v, vsp = expect_ident st in
        (Some a, v, vsp)
      end
      else (None, a, t.span)
    in
    if is_sym st "(" then begin
      skip st;
      let args = pat_items st ")" ~list:false in
      mkp (PVariant (q, v, args)) (merge t.span (last_span st))
    end
    else if is_sym st "{" && is_upper v then begin
      (* a struct pattern: `Point { x, y: 0, .. }`, `geo.Point { .. }` *)
      skip st;
      let fields, rest = struct_pat_fields st in
      let name = match q with Some m -> m ^ "." ^ v | None -> v in
      mkp (PStruct (name, merge t.span vsp, fields, rest)) (merge t.span (last_span st))
    end
    else if q <> None || is_upper v then mkp (PVariant (q, v, [])) (merge t.span vsp)
    else mkp (PBind v) t.span
  | SYM "(" ->
    skip st;
    let ps = pat_items st ")" ~list:false in
    let sp = merge t.span (last_span st) in
    (match ps with
     | [ { pdesc = PRest _; _ } ] -> mkp (PTuple ps) sp
     | [ one ] -> { one with pspan = sp }
     | _ -> mkp (PTuple ps) sp)
  | SYM "[" ->
    skip st;
    let ps = pat_items st "]" ~list:true in
    mkp (PList ps) (merge t.span (last_span st))
  | _ -> fail t "a pattern"

and parse_block st =
  let lb = expect_sym st "{" in
  parse_block_body st lb

(* The statements of a block whose `{` ([lb]) has been read. *)
and parse_block_body st (lb : token) =
  let stmts = ref [] and tail = ref None in
  let rec loop () =
    if not (is_sym st "}") then begin
      let s = parse_stmt st in
      let semi = accept_sym st ";" in
      if is_sym st "}" then begin
        match s.sdesc with
        | SExpr e when not semi -> tail := Some e
        | _ -> stmts := s :: !stmts
      end
      else begin
        if (not semi) && not (cur_nl st) then
          raise
            (Diag.Fatal
               (Diag.make "E0004" (peek st).span
                  (Printf.sprintf "expected a new line or `;` before %s" (describe (cur st)))));
        stmts := s :: !stmts;
        loop ()
      end
    end
  in
  loop ();
  let rb = expect_sym st "}" in
  { stmts = List.rev !stmts; tail = !tail; bspan = merge lb.span rb.span }

and parse_stmt st =
  let sp = cur_span st in
  let mks d sp = { sdesc = d; sspan = sp } in
  match cur st with
  | KW "let" ->
    skip st;
    let m = if is_kw st "mut" then (skip st; true) else false in
    if is_sym st "(" || struct_pat_ahead st then begin
      (* `let (a, b) = e`, `let Point { x, y } = p` *)
      let p = parse_pat st in
      if not (irrefutable_tuple p) then
        raise
          (Diag.Fatal
             (Diag.make "E0003" p.pspan
                "`let` can only take apart tuples and structs into names and `_`, as in `let (a, _) = t` or `let Point { x, .. } = p`"));
      need_sym st "=";
      let e = parse_expr st in
      mks (SLetPat (p, e)) (merge sp e.espan)
    end
    else if is_sym st "[" then begin
      (* `let [a, b] = xs`: a list can have any length *)
      let p = parse_pat st in
      raise
        (Diag.Fatal
           (Diag.make "E0003" p.pspan ~notes:[ "a list can have any length, so take it apart with `match`, as in `match xs { [a, b] => ..., _ => ... }`" ]
              "`let` cannot unpack a list: list patterns need `match`"))
    end
    else
    let n, nsp = expect_ident st in
    let ann = if accept_sym st ":" then Some (parse_type st) else None in
    need_sym st "=";
    let e = parse_expr st in
    mks (SLet (m, n, nsp, ann, e)) (merge sp e.espan)
  | KW "return" ->
    skip st;
    let nt = peek st in
    if nt.nl || sym_is nt.tok "}" || sym_is nt.tok ";" then mks (SReturn None) sp
    else
      let e = parse_expr st in
      mks (SReturn (Some e)) (merge sp e.espan)
  | KW "while" ->
    skip st;
    let c = parse_expr_ns st in
    let b = parse_block st in
    mks (SWhile (c, b)) (merge sp b.bspan)
  | KW "for" when (match next st with SYM "(" -> true | _ -> false) || struct_pat_ahead ~k:1 st ->
    skip st;
    let p = parse_pat st in
    if not (irrefutable_tuple p) then
      raise
        (Diag.Fatal
           (Diag.make "E0003" p.pspan
              "`for` can only take apart tuples and structs into names and `_`, as in `for (k, v) in m.entries()` or `for Todo { id, .. } in todos`"));
    need_kw st "in";
    let e = parse_expr_ns st in
    let b = parse_block st in
    mks (SForPat (p, e, b)) (merge sp b.bspan)
  | KW "for" ->
    skip st;
    let n, nsp = expect_ident st in
    need_kw st "in";
    let e = parse_expr_ns st in
    let b = parse_block st in
    mks (SFor (n, nsp, e, b)) (merge sp b.bspan)
  | KW "break" -> skip st; mks SBreak sp
  | KW "continue" -> skip st; mks SContinue sp
  | KW "defer" ->
    skip st;
    if is_sym st "{" then
      let b = parse_block st in
      mks (SDefer b) (merge sp b.bspan)
    else
      (* one expression or assignment: `defer f.close()`, `defer n = n + 1` *)
      let s = parse_stmt st in
      (match s.sdesc with
       | SExpr e -> mks (SDefer { stmts = []; tail = Some e; bspan = e.espan }) (merge sp e.espan)
       | SLet _ | SLetPat _ ->
         raise
           (Diag.Fatal
              (Diag.make "E0003" s.sspan "a `let` after `defer` would bind a name nothing can use"
                 ~notes:[ "wrap statements in braces: `defer { ... }`" ]))
       | _ -> mks (SDefer { stmts = [ s ]; tail = None; bspan = s.sspan }) (merge sp s.sspan))
  | _ ->
    let e = parse_expr st in
    if is_sym st "=" then begin
      skip st;
      let r = parse_expr st in
      mks (SAssign (e, r)) (merge e.espan r.espan)
    end
    else mks (SExpr e) e.espan

(* `[A, B: Shape + Show]`: names, and the interfaces each must satisfy,
   written as types: `Shape`, `geo.Area`, `Container[Int]`. *)
let parse_bounded_generics st =
  if is_sym st "[" then begin
    skip st;
    comma_list st "]" (fun st ->
        let g, _ = expect_ident st in
        let bounds =
          if accept_sym st ":" then begin
            let one () =
              let t = peek st in
              let b = parse_type st in
              match b.tdesc with
              | TyName (n, _) when n <> "dyn" && not (is_tuple_name n) -> b
              | _ -> raise (Diag.Fatal (Diag.make "E0003" t.span "a bound is an interface, as in `Shape` or `Container[Int]`"))
            in
            let first = one () in
            let rest = ref [] in
            while accept_sym st "+" do rest := one () :: !rest done;
            first :: List.rev !rest
          end
          else []
        in
        (g, bounds))
  end
  else []

(* Type parameters with their bounds, the ones without bounds left out. *)
let split_generics gs = (List.map fst gs, List.filter (fun (_, b) -> b <> []) gs)

(* The type parameters of an interface or an alias, which take no bounds. *)
let parse_generics st =
  let t = peek st in
  let gs = parse_bounded_generics st in
  if List.exists (fun (_, b) -> b <> []) gs then
    raise (Diag.Fatal (Diag.make "E0003" t.span "bounds go on the type parameters of a function, struct or enum"));
  List.map fst gs

let test_count = ref 0

(* Whether [e] can be a parameter's default: a constant, the same at every
   call. *)
let rec is_constant (e : expr) =
  match e.edesc with
  | EInt _ | EFloat _ | EBool _ | EUnit -> true
  | EStr ([] | [ SPLit _ ]) -> true
  | EUn (Neg, ({ edesc = EInt _ | EFloat _; _ } as _n)) -> true
  | EVar "None" -> true
  | EList [] -> true
  | _ -> false

(* A parameter: `name: Type`, and in a function's own parameters (not a
   foreign function's) a default, `name: Type = constant`. *)
let parse_param ?(defaults = false) st =
  let pn, psp = expect_ident st in
  if not (is_sym st ":") then
    raise (Diag.Fatal (Diag.make "E0008" psp (Printf.sprintf "parameter `%s` needs a type, for example `%s: Int`" pn pn)));
  skip st;
  let ty = parse_type st in
  let default =
    if defaults && is_sym st "=" then begin
      skip st;
      let e = parse_expr st in
      if not (is_constant e) then
        raise
          (Diag.Fatal
             (Diag.make "E0123" e.espan
                ~notes:[ "a default is the same at every call: a number, a plain string, `true`, `false`, `None` or `[]`" ]
                (Printf.sprintf "the default for `%s` must be a constant" pn)));
      Some e
    end
    else None
  in
  { prm_name = pn; prm_span = psp; prm_ty = ty; prm_default = default }

let parse_item st =
  let t = peek st in
  match t.tok with
  | IDENT (("test" | "bench") as kind) when (match next st with STR _ -> true | _ -> false) ->
    skip st;
    let s = advance st in
    let name =
      match s.tok with
      | STR [] -> ""
      | STR [ Lit n ] -> n
      | _ -> raise (Diag.Fatal (Diag.make "E0003" s.span (Printf.sprintf "a %s name must be a plain string, without interpolation" kind)))
    in
    (* optional capabilities the runner hands the test: `(fs: Fs, clock: Clock)`;
       `clock: virtual Clock` asks for a clock on virtual time *)
    let virtual_clock = ref false in
    let test_param st =
      let pn, psp = expect_ident st in
      if not (is_sym st ":") then
        raise (Diag.Fatal (Diag.make "E0008" psp (Printf.sprintf "parameter `%s` needs a type, for example `%s: Int`" pn pn)));
      skip st;
      (match (cur st, next st) with
       | IDENT "virtual", IDENT "Clock" -> skip st; virtual_clock := true
       | IDENT "virtual", _ ->
         raise (Diag.Fatal (Diag.make "E0003" (cur_span st) "only a test's clock can be virtual: `clock: virtual Clock`"))
       | _ -> ());
      { prm_name = pn; prm_span = psp; prm_ty = parse_type st; prm_default = None }
    in
    let params = if accept_sym st "(" then comma_list st ")" test_param else [] in
    (* a table test: `for (a, b, want) in [(1, 2, 3), (2, 2, 4)]`, each case run on its own *)
    let table =
      if kind = "test" && is_kw st "for" then begin
        let ft = advance st in
        let p = parse_pat st in
        if not (irrefutable_tuple p) then
          raise (Diag.Fatal (Diag.make "E0003" p.pspan "a table test names each case's parts, as in `for (a, b, want) in [(1, 2, 3)]`"));
        need_kw st "in";
        let e = parse_expr_ns st in
        match e.edesc with
        | EList (_ :: _) -> Some (p, e)
        | EList [] -> raise (Diag.Fatal (Diag.make "E0003" (merge ft.span e.espan) "a table test needs at least one case"))
        | _ ->
          raise
            (Diag.Fatal
               (Diag.make "E0003" e.espan
                  ~notes:[ "each case runs on its own and is named by its text, so the cases must be written out" ]
                  "a table test lists its cases in place, as in `for (a, want) in [(1, 2), (2, 4)]`"))
      end
      else None
    in
    let body = parse_block st in
    incr test_count;
    (* the generated name cannot be written in source, so tests cannot be called *)
    IFn
      { fn_name = Printf.sprintf "%s#%d" kind !test_count; fn_span = s.span; fn_gen = []; fn_params = params; fn_ret = None;
        fn_body = body; fn_test = Some name; fn_bench = kind = "bench"; fn_virtual = !virtual_clock; fn_table = table; fn_pub = false; fn_bounds = [];
        fn_foreign = None }
  | KW "fn" ->
    skip st;
    let name, nsp = expect_ident st in
    let gen, bounds = split_generics (parse_bounded_generics st) in
    need_sym st "(";
    let params = comma_list st ")" (parse_param ~defaults:true) in
    let ret = if accept_sym st "->" then Some (parse_type st) else None in
    let body = parse_block st in
    IFn { fn_name = name; fn_span = nsp; fn_gen = gen; fn_params = params; fn_ret = ret; fn_body = body; fn_test = None; fn_bench = false; fn_virtual = false; fn_table = None; fn_pub = false; fn_bounds = bounds; fn_foreign = None }
  | KW "struct" ->
    skip st;
    let name, nsp = expect_ident st in
    let gen, bounds = split_generics (parse_bounded_generics st) in
    need_sym st "{";
    (* a field is `name: Type`, or a bare struct type that is embedded:
       `User`, `auth.User`, `Pair[Int]`, stored as a field named `User` *)
    let embeds = ref [] in
    let fields =
      comma_list st "}" (fun st ->
          match (cur st, next st) with
          | IDENT _, SYM ":" ->
            let f, fsp = expect_ident st in
            need_sym st ":";
            (f, fsp, parse_type st)
          | IDENT n, nxt
            when is_upper n
                 || (nxt = SYM "." && match tok_at st (st.pos + 2) with IDENT b -> is_upper b | _ -> false) ->
            let te = parse_type st in
            let f =
              match te.tdesc with
              | TyName (n, _) -> ( match String.rindex_opt n '.' with Some i -> String.sub n (i + 1) (String.length n - i - 1) | None -> n)
              | _ -> ""
            in
            embeds := f :: !embeds;
            (f, te.tspan, te)
          | _ ->
            let f, fsp = expect_ident st in
            need_sym st ":";
            (f, fsp, parse_type st))
    in
    IStruct { st_name = name; st_span = nsp; st_gen = gen; st_bounds = bounds; st_fields = fields; st_embeds = List.rev !embeds; st_pub = false; st_foreign = None }
  | KW "enum" ->
    skip st;
    let name, nsp = expect_ident st in
    let gen, bounds = split_generics (parse_bounded_generics st) in
    need_sym st "{";
    let variants =
      comma_list st "}" (fun st ->
          let v, vsp = expect_ident st in
          let payload = if accept_sym st "(" then comma_list st ")" parse_type else [] in
          (v, vsp, payload))
    in
    IEnum { en_name = name; en_span = nsp; en_gen = gen; en_variants = variants; en_bounds = bounds; en_pub = false }
  | KW "interface" ->
    skip st;
    let name, nsp = expect_ident st in
    let gen = parse_generics st in
    need_sym st "{";
    let methods =
      comma_list st "}" (fun st ->
          if not (is_kw st "fn") then fail (peek st) "`fn name(self, ...)` in an interface";
          skip st;
          let m, msp = expect_ident st in
          need_sym st "(";
          let selft = peek st in
          (match selft.tok with
           | IDENT "self" -> skip st
           | _ -> raise (Diag.Fatal (Diag.make "E0003" selft.span "an interface method's first parameter is `self`, without a type")));
          let params =
            if accept_sym st "," then
              comma_list st ")" (fun st ->
                  let pn, psp = expect_ident st in
                  need_sym st ":";
                  { prm_name = pn; prm_span = psp; prm_ty = parse_type st; prm_default = None })
            else (need_sym st ")"; [])
          in
          let ret = if accept_sym st "->" then Some (parse_type st) else None in
          (m, msp, params, ret))
    in
    IIface { if_name = name; if_span = nsp; if_gen = gen; if_methods = methods; if_pub = false }
  | IDENT "type" when (match next st with IDENT _ -> true | _ -> false) ->
    (* `type` is a word only here, so it stays usable as a name elsewhere *)
    skip st;
    let name, nsp = expect_ident st in
    let gen = parse_generics st in
    need_sym st "=";
    IType { ty_name = name; ty_span = nsp; ty_gen = gen; ty_body = parse_type st; ty_pub = false }
  | _ -> fail t "an item (`fn`, `struct`, `enum`, `interface` or `type`)"

(* `foreign "lib" { fn name(p: T, ...) -> R  ... }`: C functions from library
   [lib], each with a signature and no body. `pub fn` exports one. A `...`
   among the parameters makes the function variadic: the parameters after it
   are the variadic arguments this declaration passes. `= "sym"` after the
   signature names the C function when it differs from the Limen name. A
   `struct Name { field: T, ... }` in the block declares a C struct passed by
   value. *)
let parse_foreign st =
  skip st;
  let s = advance st in
  let lib =
    match s.tok with
    | STR [ Lit n ] -> n
    | _ -> raise (Diag.Fatal (Diag.make "E0003" s.span "a foreign library name must be a plain string, as in `foreign \"sqlite3\" { ... }`"))
  in
  need_sym st "{";
  let items = ref [] in
  while not (is_sym st "}") do
    if cur st = EOF then fail (peek st) "`}` to end the foreign block";
    let pub = if is_kw st "pub" then (skip st; true) else false in
    (* `blocking fn`: a word only here, not a keyword *)
    let blocking = match (cur st, next st) with IDENT "blocking", KW "fn" -> skip st; true | _ -> false in
    if is_kw st "struct" then begin
      skip st;
      let name, nsp = expect_ident st in
      if is_sym st "[" then raise (Diag.Fatal (Diag.make "E0003" (peek st).span "a foreign struct cannot have type parameters"));
      need_sym st "{";
      let fields =
        comma_list st "}" (fun st ->
            let f, fsp = expect_ident st in
            need_sym st ":";
            (f, fsp, parse_type st))
      in
      items :=
        IStruct
          { st_name = name; st_span = nsp; st_gen = []; st_bounds = []; st_fields = fields; st_embeds = []; st_pub = pub;
            st_foreign = Some (lib, List.map (fun (_, _, t) -> t) fields) }
        :: !items
    end
    else begin
      if not (is_kw st "fn") then fail (peek st) "`fn name(...) -> Type` or `struct Name { ... }` in a foreign block";
      skip st;
      let name, nsp = expect_ident st in
      if is_sym st "[" then
        raise (Diag.Fatal (Diag.make "E0003" (peek st).span "a foreign function cannot have type parameters"));
      need_sym st "(";
      let variadic = ref None and count = ref 0 in
      let params =
        List.filter_map Fun.id
          (comma_list st ")" (fun st ->
               if is_sym st "..." then begin
                 let t = advance st in
                 if !variadic <> None then raise (Diag.Fatal (Diag.make "E0003" t.span "a foreign function has one `...`"));
                 variadic := Some !count;
                 None
               end
               else (incr count; Some (parse_param st))))
      in
      List.iter
        (fun p -> if p.prm_name = "self" then raise (Diag.Fatal (Diag.make "E0003" p.prm_span "a foreign function is not a method: name the parameter something other than `self`")))
        params;
      let ret = if accept_sym st "->" then Some (parse_type st) else None in
      let hi = match ret with Some t -> t.tspan.hi | None -> nsp.hi in
      (* `= "printf"`: the C function, when it is not the Limen name *)
      let sym, hi =
        if accept_sym st "=" then
          let t = advance st in
          match t.tok with
          | STR [ Lit n ] -> (n, t.span.hi)
          | _ -> raise (Diag.Fatal (Diag.make "E0003" t.span "the C function is named by a plain string, as in `= \"printf\"`"))
        else (name, hi)
      in
      if is_sym st "{" then
        raise (Diag.Fatal (Diag.make "E0003" (peek st).span "a foreign function has no body: its code is in the C library"));
      let body = { stmts = []; tail = None; bspan = { nsp with hi } } in
      items :=
        IFn
          { fn_name = name; fn_span = nsp; fn_gen = []; fn_params = params; fn_ret = ret; fn_body = body; fn_test = None; fn_bench = false; fn_virtual = false;
            fn_table = None; fn_pub = pub; fn_bounds = []; fn_foreign = Some { flib = lib; fsym = sym; ftrusted = false; fvariadic = !variadic; fblocking = blocking } }
        :: !items
    end;
    (* one declaration per line (or separated by `;`) *)
    if not (accept_sym st ";" || is_sym st "}" || cur_nl st) then fail (peek st) "a new line after a foreign declaration"
  done;
  skip st;
  List.rev !items

let set_pub = function
  | IFn d -> IFn { d with fn_pub = true }
  | IStruct s -> IStruct { s with st_pub = true }
  | IEnum e -> IEnum { e with en_pub = true }
  | IIface i -> IIface { i with if_pub = true }
  | IType a -> IType { a with ty_pub = true }

let parse_file file src : file =
  test_count := 0;
  safe_count := 0;
  let toks = Lexer.lex file src in
  let st = { toks; pos = 0; file } in
  let items = ref [] and uses = ref [] in
  while cur st <> EOF do
    if is_kw st "use" then begin
      skip st;
      let m, msp = expect_ident st in
      uses := (m, msp) :: !uses
    end
    else if is_kw st "pub" then begin
      skip st;
      match cur st with
      | KW ("fn" | "struct" | "enum" | "interface") | IDENT "type" -> items := set_pub (parse_item st) :: !items
      | _ -> fail (peek st) "`fn`, `struct`, `enum`, `interface` or `type` after `pub`"
    end
    else if (match cur st with IDENT "foreign" -> (match next st with STR _ -> true | _ -> false) | _ -> false) then
      items := List.rev_append (parse_foreign st) !items
    else items := parse_item st :: !items
  done;
  { uses = List.rev !uses; items = List.rev !items }

let parse_program file src : item list = (parse_file file src).items
