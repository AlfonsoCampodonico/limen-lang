(* Recursive-descent parser with Pratt-style binary expressions.

   Newline rule: a newline ends an expression unless the next line starts
   with `.` (method chains). So `f\n(x)` is two statements, not a call. *)
open Ast
open Lexer

type st = { toks : token array; mutable pos : int; file : string }

let peek st = st.toks.(st.pos)

let advance st =
  let t = st.toks.(st.pos) in
  if t.tok <> EOF then st.pos <- st.pos + 1;
  t

let fail (t : token) expected =
  raise
    (Diag.Fatal
       (Diag.make "E0003" t.span (Printf.sprintf "expected %s, found %s" expected (describe t.tok))))

let is_sym st s = (peek st).tok = SYM s
let is_kw st s = (peek st).tok = KW s
let accept_sym st s = if is_sym st s then (ignore (advance st); true) else false
let expect_sym st s = if is_sym st s then advance st else fail (peek st) (Printf.sprintf "`%s`" s)
let expect_kw st s = if is_kw st s then advance st else fail (peek st) (Printf.sprintf "`%s`" s)

let expect_ident st =
  match (peek st).tok with
  | IDENT s ->
    let t = advance st in
    (s, t.span)
  | KW k ->
    raise
      (Diag.Fatal
         (Diag.make ~notes:[ "keywords cannot be used as names; pick another name, for example `" ^ k ^ "s` or `my_" ^ k ^ "`" ]
            "E0003" (peek st).span
            (Printf.sprintf "expected a name, found keyword `%s`" k)))
  | _ -> fail (peek st) "a name"

let last_span st = st.toks.(max 0 (st.pos - 1)).span
let merge = Span.merge
let is_upper s = s <> "" && s.[0] >= 'A' && s.[0] <= 'Z'
let mk d sp = { edesc = d; espan = sp; ety = Types.TErr; einst = [] }

(* Comma-separated list up to [close]; a newline also separates items. *)
let peek2 st = st.toks.(min (st.pos + 1) (Array.length st.toks - 1))

let rec irrefutable_tuple (p : pat) =
  match p.pdesc with PBind _ | PWild -> true | PTuple ps -> List.for_all irrefutable_tuple ps | _ -> false

let comma_list st close f =
  let rec go acc =
    if accept_sym st close then List.rev acc
    else begin
      let x = f st in
      if accept_sym st "," then go (x :: acc)
      else if accept_sym st close then List.rev (x :: acc)
      else if (peek st).nl then go (x :: acc)
      else fail (peek st) (Printf.sprintf "`,` or `%s`" close)
    end
  in
  go []

let rec parse_type st : ty_expr =
  let t = peek st in
  match t.tok with
  | KW "dyn" ->
    (* `dyn Shape` is written in the AST as `dyn[Shape]` *)
    ignore (advance st);
    let a, asp = expect_ident st in
    let name, nsp =
      if is_sym st "." then begin
        ignore (advance st);
        let b, bsp = expect_ident st in
        (a ^ "." ^ b, merge asp bsp)
      end
      else (a, asp)
    in
    { tdesc = TyName ("dyn", [ { tdesc = TyName (name, []); tspan = nsp } ]); tspan = merge t.span nsp }
  | SYM "(" ->
    ignore (advance st);
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
    ignore (advance st);
    ignore (expect_sym st "(");
    let ps = comma_list st ")" parse_type in
    ignore (expect_sym st "->");
    let r = parse_type st in
    { tdesc = TyFn (ps, r); tspan = merge t.span r.tspan }
  | INT n ->
    ignore (advance st);
    { tdesc = TyInt (if n > Int64.of_int max_int then max_int else Int64.to_int n); tspan = t.span }
  | IDENT n0 ->
    ignore (advance st);
    (* a type from another module: `json.Value` *)
    let n =
      if is_sym st "." then begin
        ignore (advance st);
        let m, _ = expect_ident st in
        n0 ^ "." ^ m
      end
      else n0
    in
    if is_sym st "[" && not (peek st).nl then begin
      ignore (advance st);
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
    let t = peek st in
    match if t.tok = SYM "|>" then None else if t.nl then None else binop_of t.tok with
    | _ when t.tok = SYM "|>" && minp < 1 ->
      ignore (advance st);
      let rhs = parse_bin st ns 1 in
      if pipe_call !lhs rhs = None then
        raise
          (Diag.Fatal
             (Diag.make "E0003" rhs.espan
                ~notes:[ "`x |> f(a)` means `f(x, a)`: the value goes in as the first argument" ]
                "the right side of `|>` must be a function or a call, as in `x |> f`, `x |> f(a)` or `x |> list.map(g)`"));
      lhs := mk (EPipe (!lhs, rhs)) (merge (!lhs).espan rhs.espan)
    | Some (p, op) when p > minp ->
      ignore (advance st);
      let rhs = parse_bin st ns p in
      lhs := mk (EBin (op, !lhs, rhs)) (merge (!lhs).espan rhs.espan)
    | _ -> go := false
  done;
  !lhs

and parse_unary st ns =
  let t = peek st in
  match t.tok with
  | SYM "-" ->
    ignore (advance st);
    let e = parse_unary st ns in
    mk (EUn (Neg, e)) (merge t.span e.espan)
  | SYM "!" ->
    ignore (advance st);
    let e = parse_unary st ns in
    mk (EUn (Not, e)) (merge t.span e.espan)
  | _ -> parse_postfix st ns (parse_primary st ns)

and parse_postfix st ns e =
  let t = peek st in
  match t.tok with
  | SYM "(" when not t.nl ->
    ignore (advance st);
    let args = comma_list st ")" parse_expr in
    parse_postfix st ns (mk (ECall (e, args)) (merge e.espan (last_span st)))
  | SYM "[" when not t.nl ->
    ignore (advance st);
    let i = parse_expr st in
    let r = expect_sym st "]" in
    parse_postfix st ns (mk (EIndex (e, i)) (merge e.espan r.span))
  | SYM "." when (match (peek2 st).tok with INT _ -> true | _ -> false) ->
    (* tuple field: `t.0` *)
    ignore (advance st);
    let it = advance st in
    let k = match it.tok with INT k -> Int64.to_string k | _ -> "0" in
    parse_postfix st ns (mk (EField (e, k, it.span)) (merge e.espan it.span))
  | SYM "." ->
    ignore (advance st);
    let name, nsp = expect_ident st in
    if is_sym st "(" && not (peek st).nl then begin
      ignore (advance st);
      let args = comma_list st ")" parse_expr in
      parse_postfix st ns (mk (EMethod (e, name, nsp, args)) (merge e.espan (last_span st)))
    end
    else parse_postfix st ns (mk (EField (e, name, nsp)) (merge e.espan nsp))
  | SYM "?" when not t.nl ->
    ignore (advance st);
    parse_postfix st ns (mk (ETry e) (merge e.espan t.span))
  | _ -> e

and parse_primary st ns =
  let t = peek st in
  match t.tok with
  | INT n -> ignore (advance st); mk (EInt n) t.span
  | FLOAT f -> ignore (advance st); mk (EFloat f) t.span
  | KW "true" -> ignore (advance st); mk (EBool true) t.span
  | KW "false" -> ignore (advance st); mk (EBool false) t.span
  | STR parts ->
    ignore (advance st);
    mk (EStr (List.map (str_part st) parts)) t.span
  | IDENT name ->
    ignore (advance st);
    let tok k = st.toks.(min (st.pos + k) (Array.length st.toks - 1)) in
    if (not ns) && is_upper name && is_sym st "{" then parse_struct_lit st name t.span
    else if (not ns) && is_sym st "." && (match (tok 1).tok with IDENT b -> is_upper b | _ -> false) && (tok 2).tok = SYM "{"
            && not (tok 1).nl then begin
      (* a struct from another module: `json.Field { ... }` *)
      ignore (advance st);
      let b, bsp = expect_ident st in
      parse_struct_lit st (name ^ "." ^ b) (merge t.span bsp)
    end
    else mk (EVar name) t.span
  | SYM "(" ->
    ignore (advance st);
    if is_sym st ")" then
      let r = advance st in
      mk EUnit (merge t.span r.span)
    else
      let e = parse_expr st in
      if is_sym st "," then begin
        (* a tuple: `(a, b)` *)
        ignore (advance st);
        let rest = comma_list st ")" parse_expr in
        let es = e :: rest in
        let sp = merge t.span (last_span st) in
        mk (EStruct (tuple_name (List.length es), t.span, List.mapi (fun i x -> (string_of_int i, x.espan, x)) es)) sp
      end
      else
        let r = expect_sym st ")" in
        { e with espan = merge t.span r.span }
  | SYM "[" ->
    ignore (advance st);
    let es = comma_list st "]" parse_expr in
    mk (EList es) (merge t.span (last_span st))
  | SYM "{" ->
    let b = parse_block st in
    mk (EBlock b) b.bspan
  | KW "if" -> parse_if st
  | KW "match" -> parse_match st
  | KW "select" -> parse_select st
  | SYM "|" ->
    ignore (advance st);
    let params =
      comma_list st "|" (fun st ->
          let n, sp = expect_ident st in
          let ty = if accept_sym st ":" then Some (parse_type st) else None in
          (n, ty, sp))
    in
    let body = parse_expr st in
    mk (EClosure (params, body)) (merge t.span body.espan)
  | SYM "||" ->
    ignore (advance st);
    let body = parse_expr st in
    mk (EClosure ([], body)) (merge t.span body.espan)
  | KW "group" ->
    ignore (advance st);
    let name, _ = expect_ident st in
    let until =
      match (peek st).tok with
      | IDENT "until" -> (
        ignore (advance st);
        let d = parse_expr_ns st in
        match d.edesc with
        | EMethod (clk, "after", _, [ ms ]) -> Some (clk, ms)
        | _ -> raise (Diag.Fatal (Diag.make "E0003" d.espan "a group deadline is `group g until clock.after(ms) { ... }`")))
      | _ -> None
    in
    let b = parse_block st in
    mk (EGroup (name, until, b)) (merge t.span b.bspan)
  | _ -> fail t "an expression"

and str_part st = function
  | Lit s -> SPLit s
  | Interp (src, off) ->
    let toks = Lexer.lex ~base:off st.file src in
    let sub = { toks; pos = 0; file = st.file } in
    let e = parse_expr sub in
    if (peek sub).tok <> EOF then fail (peek sub) "`}` to close the interpolation";
    SPExpr e

and parse_struct_lit st name nsp =
  ignore (expect_sym st "{");
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
    ignore (advance st);
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
  ignore (expect_sym st "{");
  let arms = ref [] in
  while not (accept_sym st "}") do
    let p = parse_pat st in
    ignore (expect_sym st "=>");
    let body = parse_expr st in
    arms := { apat = p; abody = body } :: !arms;
    if (not (accept_sym st ",")) && (not (is_sym st "}")) && not (peek st).nl then
      fail (peek st) "`,` or a new line between match arms"
  done;
  mk (EMatch (s, List.rev !arms)) (merge t.span (last_span st))

and parse_select st =
  let t = expect_kw st "select" in
  ignore (expect_sym st "{");
  let arms = ref [] in
  while not (accept_sym st "}") do
    let a = peek st in
    let op =
      match (a.tok, (peek2 st).tok) with
      | IDENT "_", SYM "=>" -> ignore (advance st); SelDefault
      | IDENT n, SYM "=" ->
        (* `v = ch.recv()` *)
        ignore (advance st);
        ignore (advance st);
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
    let guard = if is_kw st "if" then (ignore (advance st); Some (parse_expr st)) else None in
    ignore (expect_sym st "=>");
    let body = parse_expr st in
    arms := { sop = op; sguard = guard; sbody = body; sspan_ = merge a.span body.espan } :: !arms;
    if (not (accept_sym st ",")) && (not (is_sym st "}")) && not (peek st).nl then
      fail (peek st) "`,` or a new line between select arms"
  done;
  mk (ESelect (List.rev !arms)) (merge t.span (last_span st))

and parse_pat st =
  let t = peek st in
  let mkp d sp = { pdesc = d; pspan = sp } in
  match t.tok with
  | IDENT "_" -> ignore (advance st); mkp PWild t.span
  | INT n -> ignore (advance st); mkp (PInt n) t.span
  | SYM "-" -> (
    ignore (advance st);
    match (peek st).tok with
    | INT n ->
      let t2 = advance st in
      mkp (PInt (Int64.neg n)) (merge t.span t2.span)
    | _ -> fail (peek st) "a number")
  | STR [] -> ignore (advance st); mkp (PStr "") t.span
  | STR [ Lit s ] -> ignore (advance st); mkp (PStr s) t.span
  | STR _ -> fail t "a plain string (no interpolation) in a pattern"
  | KW "true" -> ignore (advance st); mkp (PBool true) t.span
  | KW "false" -> ignore (advance st); mkp (PBool false) t.span
  | IDENT a ->
    ignore (advance st);
    let q, v, vsp =
      if is_sym st "." then begin
        ignore (advance st);
        let v, vsp = expect_ident st in
        (Some a, v, vsp)
      end
      else (None, a, t.span)
    in
    if is_sym st "(" then begin
      ignore (advance st);
      let args = comma_list st ")" parse_pat in
      mkp (PVariant (q, v, args)) (merge t.span (last_span st))
    end
    else if q <> None || is_upper v then mkp (PVariant (q, v, [])) (merge t.span vsp)
    else mkp (PBind v) t.span
  | SYM "(" ->
    ignore (advance st);
    let ps = comma_list st ")" parse_pat in
    let sp = merge t.span (last_span st) in
    (match ps with
     | [ one ] -> { one with pspan = sp }
     | _ -> mkp (PTuple ps) sp)
  | _ -> fail t "a pattern"

and parse_block st =
  let lb = expect_sym st "{" in
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
        if (not semi) && not (peek st).nl then
          raise
            (Diag.Fatal
               (Diag.make "E0004" (peek st).span
                  (Printf.sprintf "expected a new line or `;` before %s" (describe (peek st).tok))));
        stmts := s :: !stmts;
        loop ()
      end
    end
  in
  loop ();
  let rb = expect_sym st "}" in
  { stmts = List.rev !stmts; tail = !tail; bspan = merge lb.span rb.span }

and parse_stmt st =
  let t = peek st in
  let mks d sp = { sdesc = d; sspan = sp } in
  match t.tok with
  | KW "let" ->
    ignore (advance st);
    let m = if is_kw st "mut" then (ignore (advance st); true) else false in
    if is_sym st "(" then begin
      (* `let (a, b) = e` *)
      let p = parse_pat st in
      if not (irrefutable_tuple p) then
        raise (Diag.Fatal (Diag.make "E0003" p.pspan "`let` can only unpack a tuple into names and `_`, as in `let (a, _) = t`"));
      ignore (expect_sym st "=");
      let e = parse_expr st in
      mks (SLetPat (p, e)) (merge t.span e.espan)
    end
    else
    let n, nsp = expect_ident st in
    let ann = if accept_sym st ":" then Some (parse_type st) else None in
    ignore (expect_sym st "=");
    let e = parse_expr st in
    mks (SLet (m, n, nsp, ann, e)) (merge t.span e.espan)
  | KW "return" ->
    ignore (advance st);
    let nt = peek st in
    if nt.nl || nt.tok = SYM "}" || nt.tok = SYM ";" then mks (SReturn None) t.span
    else
      let e = parse_expr st in
      mks (SReturn (Some e)) (merge t.span e.espan)
  | KW "while" ->
    ignore (advance st);
    let c = parse_expr_ns st in
    let b = parse_block st in
    mks (SWhile (c, b)) (merge t.span b.bspan)
  | KW "for" when (match (peek2 st).tok with SYM "(" -> true | _ -> false) ->
    ignore (advance st);
    let p = parse_pat st in
    if not (irrefutable_tuple p) then
      raise (Diag.Fatal (Diag.make "E0003" p.pspan "`for` can only unpack a tuple into names and `_`, as in `for (k, v) in m.entries()`"));
    ignore (expect_kw st "in");
    let e = parse_expr_ns st in
    let b = parse_block st in
    mks (SForPat (p, e, b)) (merge t.span b.bspan)
  | KW "for" ->
    ignore (advance st);
    let n, nsp = expect_ident st in
    ignore (expect_kw st "in");
    let e = parse_expr_ns st in
    let b = parse_block st in
    mks (SFor (n, nsp, e, b)) (merge t.span b.bspan)
  | KW "break" -> ignore (advance st); mks SBreak t.span
  | KW "continue" -> ignore (advance st); mks SContinue t.span
  | _ ->
    let e = parse_expr st in
    if is_sym st "=" then begin
      ignore (advance st);
      let r = parse_expr st in
      mks (SAssign (e, r)) (merge e.espan r.espan)
    end
    else mks (SExpr e) e.espan

(* `[A, B: Shape + Show]`: names, and the interfaces each must satisfy. *)
let parse_bounded_generics st =
  if is_sym st "[" then begin
    ignore (advance st);
    comma_list st "]" (fun st ->
        let g, _ = expect_ident st in
        let bounds =
          if accept_sym st ":" then begin
            let one () =
              let a, asp = expect_ident st in
              if is_sym st "." then begin
                ignore (advance st);
                let b, bsp = expect_ident st in
                (a ^ "." ^ b, merge asp bsp)
              end
              else (a, asp)
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

let parse_generics st =
  let t = peek st in
  let gs = parse_bounded_generics st in
  if List.exists (fun (_, b) -> b <> []) gs then
    raise (Diag.Fatal (Diag.make "E0003" t.span "interface bounds go on a function's type parameters, not on a struct or enum"));
  List.map fst gs

let test_count = ref 0

let parse_param st =
  let pn, psp = expect_ident st in
  if not (is_sym st ":") then
    raise (Diag.Fatal (Diag.make "E0008" psp (Printf.sprintf "parameter `%s` needs a type, for example `%s: Int`" pn pn)));
  ignore (advance st);
  { prm_name = pn; prm_span = psp; prm_ty = parse_type st }

let parse_item st =
  let t = peek st in
  match t.tok with
  | IDENT "test" when (match st.toks.(min (st.pos + 1) (Array.length st.toks - 1)).tok with STR _ -> true | _ -> false) ->
    ignore (advance st);
    let s = advance st in
    let name =
      match s.tok with
      | STR [] -> ""
      | STR [ Lit n ] -> n
      | _ -> raise (Diag.Fatal (Diag.make "E0003" s.span "a test name must be a plain string, without interpolation"))
    in
    (* optional capabilities the runner hands the test: `(fs: Fs, clock: Clock)` *)
    let params = if accept_sym st "(" then comma_list st ")" parse_param else [] in
    let body = parse_block st in
    incr test_count;
    (* the generated name cannot be written in source, so tests cannot be called *)
    IFn
      { fn_name = Printf.sprintf "test#%d" !test_count; fn_span = s.span; fn_gen = []; fn_params = params; fn_ret = None;
        fn_body = body; fn_test = Some name; fn_pub = false; fn_bounds = []; fn_foreign = None }
  | KW "fn" ->
    ignore (advance st);
    let name, nsp = expect_ident st in
    let bgen = parse_bounded_generics st in
    let gen = List.map fst bgen in
    let bounds = List.filter (fun (_, b) -> b <> []) bgen in
    ignore (expect_sym st "(");
    let params = comma_list st ")" parse_param in
    let ret = if accept_sym st "->" then Some (parse_type st) else None in
    let body = parse_block st in
    IFn { fn_name = name; fn_span = nsp; fn_gen = gen; fn_params = params; fn_ret = ret; fn_body = body; fn_test = None; fn_pub = false; fn_bounds = bounds; fn_foreign = None }
  | KW "struct" ->
    ignore (advance st);
    let name, nsp = expect_ident st in
    let gen = parse_generics st in
    ignore (expect_sym st "{");
    let fields =
      comma_list st "}" (fun st ->
          let f, fsp = expect_ident st in
          ignore (expect_sym st ":");
          (f, fsp, parse_type st))
    in
    IStruct { st_name = name; st_span = nsp; st_gen = gen; st_fields = fields; st_pub = false }
  | KW "enum" ->
    ignore (advance st);
    let name, nsp = expect_ident st in
    let gen = parse_generics st in
    ignore (expect_sym st "{");
    let variants =
      comma_list st "}" (fun st ->
          let v, vsp = expect_ident st in
          let payload = if accept_sym st "(" then comma_list st ")" parse_type else [] in
          (v, vsp, payload))
    in
    IEnum { en_name = name; en_span = nsp; en_gen = gen; en_variants = variants; en_pub = false }
  | KW "interface" ->
    ignore (advance st);
    let name, nsp = expect_ident st in
    ignore (expect_sym st "{");
    let methods =
      comma_list st "}" (fun st ->
          if not (is_kw st "fn") then fail (peek st) "`fn name(self, ...)` in an interface";
          ignore (advance st);
          let m, msp = expect_ident st in
          ignore (expect_sym st "(");
          let selft = peek st in
          (match selft.tok with
           | IDENT "self" -> ignore (advance st)
           | _ -> raise (Diag.Fatal (Diag.make "E0003" selft.span "an interface method's first parameter is `self`, without a type")));
          let params =
            if accept_sym st "," then
              comma_list st ")" (fun st ->
                  let pn, psp = expect_ident st in
                  ignore (expect_sym st ":");
                  { prm_name = pn; prm_span = psp; prm_ty = parse_type st })
            else (ignore (expect_sym st ")"); [])
          in
          let ret = if accept_sym st "->" then Some (parse_type st) else None in
          (m, msp, params, ret))
    in
    IIface { if_name = name; if_span = nsp; if_methods = methods; if_pub = false }
  | _ -> fail t "an item (`fn`, `struct`, `enum` or `interface`)"

(* `foreign "lib" { fn name(p: T, ...) -> R  ... }`: C functions from library
   [lib], each with a signature and no body. `pub fn` exports one. *)
let parse_foreign st =
  ignore (advance st);
  let s = advance st in
  let lib =
    match s.tok with
    | STR [ Lit n ] -> n
    | _ -> raise (Diag.Fatal (Diag.make "E0003" s.span "a foreign library name must be a plain string, as in `foreign \"sqlite3\" { ... }`"))
  in
  ignore (expect_sym st "{");
  let items = ref [] in
  while not (is_sym st "}") do
    if (peek st).tok = EOF then fail (peek st) "`}` to end the foreign block";
    let pub = if is_kw st "pub" then (ignore (advance st); true) else false in
    if not (is_kw st "fn") then fail (peek st) "`fn name(...) -> Type` in a foreign block";
    ignore (advance st);
    let name, nsp = expect_ident st in
    if is_sym st "[" then
      raise (Diag.Fatal (Diag.make "E0003" (peek st).span "a foreign function cannot have type parameters"));
    ignore (expect_sym st "(");
    let params = comma_list st ")" parse_param in
    List.iter
      (fun p -> if p.prm_name = "self" then raise (Diag.Fatal (Diag.make "E0003" p.prm_span "a foreign function is not a method: name the parameter something other than `self`")))
      params;
    let ret = if accept_sym st "->" then Some (parse_type st) else None in
    if is_sym st "{" then
      raise (Diag.Fatal (Diag.make "E0003" (peek st).span "a foreign function has no body: its code is in the C library"));
    let hi = match ret with Some t -> t.tspan.hi | None -> nsp.hi in
    let body = { stmts = []; tail = None; bspan = { nsp with hi } } in
    items :=
      IFn
        { fn_name = name; fn_span = nsp; fn_gen = []; fn_params = params; fn_ret = ret; fn_body = body; fn_test = None;
          fn_pub = pub; fn_bounds = []; fn_foreign = Some { flib = lib; fsym = name; ftrusted = false } }
      :: !items;
    (* one declaration per line (or separated by `;`) *)
    if not (accept_sym st ";" || is_sym st "}" || (peek st).nl) then fail (peek st) "a new line after a foreign function's signature"
  done;
  ignore (advance st);
  List.rev !items

let set_pub = function
  | IFn d -> IFn { d with fn_pub = true }
  | IStruct s -> IStruct { s with st_pub = true }
  | IEnum e -> IEnum { e with en_pub = true }
  | IIface i -> IIface { i with if_pub = true }

let parse_file file src : file =
  test_count := 0;
  let toks = Lexer.lex file src in
  let st = { toks; pos = 0; file } in
  let items = ref [] and uses = ref [] in
  while (peek st).tok <> EOF do
    if is_kw st "use" then begin
      ignore (advance st);
      let m, msp = expect_ident st in
      uses := (m, msp) :: !uses
    end
    else if is_kw st "pub" then begin
      ignore (advance st);
      match (peek st).tok with
      | KW ("fn" | "struct" | "enum" | "interface") -> items := set_pub (parse_item st) :: !items
      | _ -> fail (peek st) "`fn`, `struct` or `enum` after `pub`"
    end
    else if (match (peek st).tok with IDENT "foreign" -> (match (peek2 st).tok with STR _ -> true | _ -> false) | _ -> false) then
      items := List.rev_append (parse_foreign st) !items
    else items := parse_item st :: !items
  done;
  { uses = List.rev !uses; items = List.rev !items }

let parse_program file src : item list = (parse_file file src).items
