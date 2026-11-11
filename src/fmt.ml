(* writ fmt: print a program in the canonical style.

   - 4-space indentation, one struct field / enum variant / match arm per line.
   - Statements one per line (`;` separators are dropped); a single blank line
     between statements is kept where the author had one; one blank line
     between items.
   - Parentheses only where precedence needs them.
   - Kept from the source: comments, number spellings (`1_000`, `2.5E-3`),
     string literals exactly as written, struct field shorthand, and the
     author's choice to break a list, an argument list, a struct literal or a
     method chain over several lines.
   Comments inside an expression move to the next statement boundary;
   comments between statements, items, fields, variants, arms and list
   elements stay where they are. *)
open Ast

type out = { mutable lines : (int * Buffer.t) list (* reversed: indent, text *) }

type ctx = {
  src : string;
  o : out;
  mutable pending : (int * int * string) list; (* comments not printed yet, by position *)
}

let new_line c ind = c.o.lines <- (ind, Buffer.create 64) :: c.o.lines
let add c s = match c.o.lines with (_, b) :: _ -> Buffer.add_string b s | [] -> ()
let blank c = new_line c 0
let slice c (sp : Span.t) = String.sub c.src sp.lo (sp.hi - sp.lo)

let line_of c off =
  let n = ref 0 in
  for i = 0 to min off (String.length c.src) - 1 do if c.src.[i] = '\n' then incr n done;
  !n

let has_newline c lo hi =
  let r = ref false in
  for i = max 0 lo to min hi (String.length c.src) - 1 do if c.src.[i] = '\n' then r := true done;
  !r

(* Was there an empty line in the source between offsets lo and hi? *)
let blank_between c lo hi =
  let count = ref 0 and found = ref false in
  for i = max 0 lo to min hi (String.length c.src) - 1 do
    match c.src.[i] with
    | '\n' -> incr count; if !count >= 2 then found := true
    | ' ' | '\t' | '\r' -> ()
    | _ -> count := 0
  done;
  !found

(* Comments that start before [lo]: printed on their own lines at [ind]. *)
let leading c ind ~prev lo =
  let rec go prev =
    match c.pending with
    | (clo, chi, text) :: rest when clo < lo ->
      c.pending <- rest;
      if prev >= 0 && blank_between c prev clo then blank c;
      new_line c ind;
      add c text;
      go chi
    | _ -> prev
  in
  go prev

(* Comments on the same source line, right after a node that ends at [hi]. *)
let trailing c hi =
  let l = line_of c hi in
  let rec go () =
    match c.pending with
    | (clo, _, text) :: rest when clo >= hi && line_of c clo = l ->
      c.pending <- rest;
      add c ("  " ^ text);
      go ()
    | _ -> ()
  in
  go ()

(* After [after], is the next non-space character followed by a newline?
   i.e. did the author break right after this opening bracket? *)
let breaks_after_open c from open_ch =
  let n = String.length c.src in
  let i = ref from in
  while !i < n && c.src.[!i] <> open_ch do incr i done;
  incr i;
  while !i < n && (c.src.[!i] = ' ' || c.src.[!i] = '\t' || c.src.[!i] = '\r') do incr i done;
  !i < n && (c.src.[!i] = '\n' || (!i + 1 < n && c.src.[!i] = '/' && c.src.[!i + 1] = '/'))

(* ---------- types and patterns ---------- *)

let rec ty (t : ty_expr) =
  match t.tdesc with
  | TyUnit -> "()"
  | TyInt n -> string_of_int n
  | TyName (n, []) -> n
  | TyName (n, args) -> n ^ "[" ^ String.concat ", " (List.map ty args) ^ "]"
  | TyFn (ps, r) -> "fn(" ^ String.concat ", " (List.map ty ps) ^ ") -> " ^ ty r

let escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '\\' -> Buffer.add_string b "\\\\" | '"' -> Buffer.add_string b "\\\"" | '{' -> Buffer.add_string b "\\{"
      | '}' -> Buffer.add_string b "\\}" | '\n' -> Buffer.add_string b "\\n" | '\t' -> Buffer.add_string b "\\t"
      | '\r' -> Buffer.add_string b "\\r" | '\000' -> Buffer.add_string b "\\0" | ch -> Buffer.add_char b ch)
    s;
  Buffer.contents b

let rec pat (p : pat) =
  match p.pdesc with
  | PWild -> "_"
  | PBind n -> n
  | PInt n -> string_of_int n
  | PStr s -> "\"" ^ escape s ^ "\""
  | PBool b -> string_of_bool b
  | PVariant (q, v, args) ->
    (match q with Some e -> e ^ "." ^ v | None -> v) ^ if args = [] then "" else "(" ^ String.concat ", " (List.map pat args) ^ ")"

(* ---------- expressions ---------- *)

let prec = function
  | Or -> 1 | And -> 2 | Eq | Ne -> 3 | Lt | Le | Gt | Ge -> 4 | Add | Sub -> 5 | Mul | Div | Mod -> 6

let op_str = function
  | Or -> "||" | And -> "&&" | Eq -> "==" | Ne -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"

(* A struct literal that would be read as the start of a block in an
   `if`/`while`/`for`/`match` head. *)
let rec bare_struct e =
  match e.edesc with
  | EStruct _ -> true
  | EBin (_, a, b) -> bare_struct a || bare_struct b
  | EUn (_, a) | ETry a | EField (a, _, _) | EIndex (a, _) -> bare_struct a
  | EMethod (r, _, _, _) -> bare_struct r
  | ECall (f, _) -> bare_struct f
  | _ -> false

let rec simple_expr e =
  match e.edesc with
  | EBlock _ | EIf _ | EMatch _ | EGroup _ -> false
  | EClosure (_, b) -> simple_expr b
  | EBin (_, a, b) | EIndex (a, b) -> simple_expr a && simple_expr b
  | EUn (_, a) | ETry a | EField (a, _, _) -> simple_expr a
  | ECall (f, args) -> simple_expr f && List.for_all simple_expr args
  | EMethod (r, _, _, args) -> simple_expr r && List.for_all simple_expr args
  | EList es -> List.for_all simple_expr es
  | EStruct (_, _, fs) -> List.for_all (fun (_, _, x) -> simple_expr x) fs
  | _ -> true

let rec expr c ind ?(min = 0) e =
  let p =
    match e.edesc with
    | EBin (op, _, _) -> prec op
    | EUn _ -> 7
    | EClosure _ -> 0
    | _ -> 9
  in
  if p < min then (add c "("; expr_inner c ind e; add c ")") else expr_inner c ind e

and expr_inner c ind e =
  match e.edesc with
  | EInt _ | EFloat _ | EStr _ -> add c (slice c e.espan)
  | EBool b -> add c (string_of_bool b)
  | EUnit -> add c "()"
  | EVar n -> add c n
  | EBin (op, a, b) ->
    let p = prec op in
    expr c ind ~min:p a;
    add c (" " ^ op_str op ^ " ");
    expr c ind ~min:(p + 1) b
  | EUn (op, a) ->
    add c (match op with Neg -> "-" | Not -> "!");
    expr c ind ~min:8 a
  | ECall (f, args) ->
    expr c ind ~min:8 f;
    seq c ind "(" ")" (breaks_after_open c f.espan.hi '(') (fun x -> expr c (ind + 4) x) (fun x -> x.espan) args
  | EMethod (r, name, nsp, args) ->
    expr c ind ~min:8 r;
    if has_newline c r.espan.hi nsp.lo then new_line c (ind + 4);
    add c ("." ^ name);
    seq c ind "(" ")" (breaks_after_open c nsp.hi '(') (fun x -> expr c (ind + 4) x) (fun x -> x.espan) args
  | EField (r, name, nsp) ->
    expr c ind ~min:8 r;
    if has_newline c r.espan.hi nsp.lo then new_line c (ind + 4);
    add c ("." ^ name)
  | EIndex (a, i) ->
    expr c ind ~min:8 a;
    add c "[";
    expr c ind i;
    add c "]"
  | ETry a ->
    expr c ind ~min:8 a;
    add c "?"
  | EList es -> seq c ind "[" "]" (breaks_after_open c e.espan.lo '[') (fun x -> expr c (ind + 4) x) (fun x -> x.espan) es
  | EStruct (name, nsp, fields) ->
    add c (name ^ " ");
    let field (f, fsp, v) =
      if v.espan = fsp then add c f (* shorthand *)
      else (add c (f ^ ": "); expr c (ind + 4) v)
    in
    if fields = [] then add c "{}"
    else if breaks_after_open c nsp.hi '{' then begin
      add c "{";
      List.iter
        (fun ((_, (fsp : Span.t), v) as fl) ->
          ignore (leading c (ind + 4) ~prev:(-1) fsp.lo);
          new_line c (ind + 4);
          field fl;
          add c ",";
          trailing c (if v.espan = fsp then fsp.hi else v.espan.hi))
        fields;
      ignore (leading c (ind + 4) ~prev:(-1) e.espan.hi);
      new_line c ind;
      add c "}"
    end
    else begin
      add c "{ ";
      List.iteri (fun i fl -> if i > 0 then add c ", "; field fl) fields;
      add c " }"
    end
  | EBlock b -> block c ind b
  | EIf _ -> if_chain c ind e
  | EMatch (s, arms) ->
    add c "match ";
    head c ind s;
    add c " {";
    let prev = ref (-1) in
    List.iter
      (fun a ->
        ignore (leading c (ind + 4) ~prev:!prev a.apat.pspan.lo);
        new_line c (ind + 4);
        add c (pat a.apat ^ " => ");
        expr c (ind + 4) a.abody;
        add c ",";
        trailing c a.abody.espan.hi;
        prev := a.abody.espan.hi)
      arms;
    ignore (leading c (ind + 4) ~prev:!prev (e.espan.hi - 1));
    new_line c ind;
    add c "}"
  | EClosure (ps, body) ->
    if ps = [] then add c "||"
    else
      add c
        ("|" ^ String.concat ", " (List.map (fun (n, t, _) -> match t with Some t -> n ^ ": " ^ ty t | None -> n) ps) ^ "|");
    add c " ";
    expr c ind body
  | EGroup (name, b) ->
    add c ("group " ^ name ^ " ");
    block c ind b

and head c ind e =
  if bare_struct e then (add c "("; expr c ind e; add c ")") else expr c ind e

and if_chain c ind e =
  match e.edesc with
  | EIf (cond, b, els) -> (
    add c "if ";
    head c ind cond;
    add c " ";
    block c ind b;
    match els with
    | None -> ()
    | Some ({ edesc = EIf _; _ } as x) -> add c " else "; if_chain c ind x
    | Some x -> add c " else "; expr c ind x)
  | _ -> expr c ind e

(* `( ... )` / `[ ... ]`: on one line, or one item per line with a trailing
   comma when the author broke after the opening bracket. *)
and seq : 'a. ctx -> int -> string -> string -> bool -> ('a -> unit) -> ('a -> Span.t) -> 'a list -> unit =
 fun c ind op cl multi item span xs ->
  if multi && xs <> [] then begin
    add c op;
    let prev = ref (-1) in
    List.iter
      (fun x ->
        let sp = span x in
        ignore (leading c (ind + 4) ~prev:!prev sp.lo);
        new_line c (ind + 4);
        item x;
        add c ",";
        trailing c sp.hi;
        prev := sp.hi)
      xs;
    new_line c ind;
    add c cl
  end
  else begin
    add c op;
    List.iteri (fun i x -> if i > 0 then add c ", "; item x) xs;
    add c cl
  end

and block c ind b =
  let inline =
    b.stmts = [] && (match b.tail with None -> true | Some e -> simple_expr e) && not (has_newline c b.bspan.lo b.bspan.hi)
  in
  if inline then
    match b.tail with
    | None -> add c "{}"
    | Some e -> add c "{ "; expr c ind e; add c " }"
  else begin
    add c "{";
    let prev = ref (-1) in
    let elems = List.map (fun s -> `S s) b.stmts @ (match b.tail with Some e -> [ `E e ] | None -> []) in
    List.iter
      (fun el ->
        let lo, hi = match el with `S s -> (s.sspan.lo, s.sspan.hi) | `E e -> (e.espan.lo, e.espan.hi) in
        let after_comments = leading c (ind + 4) ~prev:!prev lo in
        if after_comments >= 0 && blank_between c after_comments lo then blank c;
        new_line c (ind + 4);
        (match el with `S s -> stmt c (ind + 4) s | `E e -> expr c (ind + 4) e);
        trailing c hi;
        prev := hi)
      elems;
    ignore (leading c (ind + 4) ~prev:!prev (b.bspan.hi - 1));
    new_line c ind;
    add c "}"
  end

and stmt c ind s =
  match s.sdesc with
  | SLet (m, n, _, t, e) ->
    add c ("let " ^ (if m then "mut " else "") ^ n ^ (match t with Some t -> ": " ^ ty t | None -> "") ^ " = ");
    expr c ind e
  | SAssign (l, r) ->
    expr c ind l;
    add c " = ";
    expr c ind r
  | SExpr e -> expr c ind e
  | SReturn None -> add c "return"
  | SReturn (Some e) -> add c "return "; expr c ind e
  | SWhile (cond, b) ->
    add c "while ";
    head c ind cond;
    add c " ";
    block c ind b
  | SFor (n, _, e, b) ->
    add c ("for " ^ n ^ " in ");
    head c ind e;
    add c " ";
    block c ind b
  | SBreak -> add c "break"
  | SContinue -> add c "continue"

(* ---------- items ---------- *)

let gens = function [] -> "" | g -> "[" ^ String.concat ", " g ^ "]"

(* Offset of the `}` that closes a struct or enum, scanning from [from] and
   skipping comments. *)
let closing_brace c from =
  let n = String.length c.src in
  let rec go i =
    if i >= n then n - 1
    else if c.src.[i] = '/' && i + 1 < n && c.src.[i + 1] = '/' then
      go (match String.index_from_opt c.src i '\n' with Some j -> j | None -> n)
    else if c.src.[i] = '}' then i
    else go (i + 1)
  in
  go from

let struct_close c (s : struct_decl) =
  closing_brace c (match List.rev s.st_fields with (_, _, t) :: _ -> t.tspan.hi | [] -> s.st_span.hi)

let enum_close c (e : enum_decl) =
  closing_brace c
    (match List.rev e.en_variants with
     | (_, vsp, ts) :: _ -> (match List.rev ts with t :: _ -> t.tspan.hi + 1 | [] -> vsp.hi)
     | [] -> e.en_span.hi)

let item c (it : item) =
  match it with
  | IFn ({ fn_test = Some name; _ } as d) ->
    new_line c 0;
    add c ("test \"" ^ escape name ^ "\" ");
    block c 0 d.fn_body;
    trailing c d.fn_body.bspan.hi
  | IFn d ->
    new_line c 0;
    let params = List.map (fun p -> p.prm_name ^ ": " ^ ty p.prm_ty) d.fn_params in
    add c ((if d.fn_pub then "pub " else "") ^ "fn " ^ d.fn_name ^ gens d.fn_gen);
    if breaks_after_open c d.fn_span.hi '(' && params <> [] then begin
      add c "(";
      List.iter (fun p -> new_line c 4; add c (p ^ ",")) params;
      new_line c 0;
      add c ")"
    end
    else add c ("(" ^ String.concat ", " params ^ ")");
    (match d.fn_ret with Some t -> add c (" -> " ^ ty t) | None -> ());
    add c " ";
    block c 0 d.fn_body;
    trailing c d.fn_body.bspan.hi
  | IStruct s ->
    new_line c 0;
    add c ((if s.st_pub then "pub " else "") ^ "struct " ^ s.st_name ^ gens s.st_gen ^ " {");
    let close = struct_close c s in
    (* a struct written on one line keeps its trailing comment after the brace *)
    let single = not (has_newline c s.st_span.lo close) in
    let prev = ref (-1) in
    List.iter
      (fun (f, (fsp : Span.t), t) ->
        ignore (leading c 4 ~prev:!prev fsp.lo);
        new_line c 4;
        add c (f ^ ": " ^ ty t ^ ",");
        if not single then trailing c t.tspan.hi;
        prev := t.tspan.hi)
      s.st_fields;
    if s.st_fields = [] then add c "}"
    else begin
      ignore (leading c 4 ~prev:!prev close);
      new_line c 0;
      add c "}"
    end;
    trailing c (close + 1)
  | IEnum e ->
    new_line c 0;
    add c ((if e.en_pub then "pub " else "") ^ "enum " ^ e.en_name ^ gens e.en_gen ^ " {");
    let close = enum_close c e in
    let single = not (has_newline c e.en_span.lo close) in
    let prev = ref (-1) in
    List.iter
      (fun (v, (vsp : Span.t), ts) ->
        ignore (leading c 4 ~prev:!prev vsp.lo);
        new_line c 4;
        add c (v ^ (if ts = [] then "" else "(" ^ String.concat ", " (List.map ty ts) ^ ")") ^ ",");
        let hi = match List.rev ts with t :: _ -> t.tspan.hi + 1 | [] -> vsp.hi in
        if not single then trailing c hi;
        prev := hi)
      e.en_variants;
    if e.en_variants = [] then add c "}"
    else begin
      ignore (leading c 4 ~prev:!prev close);
      new_line c 0;
      add c "}"
    end;
    trailing c (close + 1)

let item_span c = function
  | IFn d -> (d.fn_span.lo, d.fn_body.bspan.hi)
  | IStruct s -> (s.st_span.lo, struct_close c s + 1)
  | IEnum e -> (e.en_span.lo, enum_close c e + 1)

(* Format [src]; raises Diag.Fatal if it does not parse. *)
let format ~file src =
  Source.add file src;
  let { uses; items } : file = Parser.parse_file file src in
  let comments = List.sort compare !Lexer.comments in
  let c = { src; o = { lines = [] }; pending = comments } in
  let prev = ref (-1) in
  List.iter
    (fun (name, (sp : Span.t)) ->
      ignore (leading c 0 ~prev:!prev sp.lo);
      new_line c 0;
      add c ("use " ^ name);
      trailing c sp.hi;
      prev := sp.hi)
    uses;
  let nuses = List.length uses in
  List.iteri
    (fun i it ->
      let i = i + nuses in
      let lo, _ = item_span c it in
      (* the item keyword starts a little before its name span *)
      let kw_lo =
        let k = ref lo in
        while !k > 0 && c.src.[!k - 1] <> '\n' do decr k done;
        !k
      in
      if i > 0 then blank c;
      let after = leading c 0 ~prev:!prev kw_lo in
      if after >= 0 && List.length c.o.lines > 0 && blank_between c after kw_lo then blank c;
      item c it;
      prev := snd (item_span c it))
    items;
  (* comments after the last item *)
  ignore (leading c 0 ~prev:!prev max_int);
  let lines = List.rev_map (fun (ind, b) -> (ind, Buffer.contents b)) c.o.lines in
  (* drop leading blank lines and collapse runs of blank lines *)
  let rec squeeze prev_blank = function
    | [] -> []
    | (_, "") :: rest -> if prev_blank then squeeze true rest else (0, "") :: squeeze true rest
    | l :: rest -> l :: squeeze false rest
  in
  let lines = squeeze true lines in
  let lines = List.rev (match List.rev lines with (_, "") :: r -> r | r -> r) in
  String.concat "" (List.map (fun (ind, t) -> if t = "" then "\n" else String.make ind ' ' ^ t ^ "\n") lines)
