(* vela fmt: print a program in the canonical style.

   - 4-space indentation, one struct field / enum variant / match arm per line.
   - Statements one per line (`;` separators are dropped); a single blank line
     between statements is kept where the author had one; one blank line
     between items.
   - Parentheses only where precedence needs them.
   - Kept from the source: comments, number spellings (`1_000`, `2.5E-3`),
     string literals exactly as written, struct field shorthand, and the
     author's choice to break a list, an argument list, a struct literal or a
     method chain over several lines.
   Comments stay where they are: between statements, items, fields,
   variants, arms and list elements, and inside an expression. A comment
   inside an argument list, list or struct literal written on one line puts
   that list one element per line; one after a binary operator or a closure's
   parameters breaks the line there; one in a method chain stays between its
   links. *)
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

(* Is there a comment not printed yet that starts in [lo, hi)? *)
let comment_in c lo hi = List.exists (fun (clo, _, _) -> clo >= lo && clo < hi) c.pending

(* The expression goes on past a comment: the comments on this line after
   [hi], those on their own lines before [lo], then a new line at [ind]. *)
let break_at_comments c ind hi lo =
  trailing c hi;
  ignore (leading c ind ~prev:(-1) lo);
  new_line c ind

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
  | TyName ("dyn", [ i ]) -> "dyn " ^ ty i
  | TyName (n, args) when is_tuple_name n -> "(" ^ String.concat ", " (List.map ty args) ^ ")"
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
  | PInt n -> Int64.to_string n
  | PStr s -> "\"" ^ escape s ^ "\""
  | PBool b -> string_of_bool b
  | PVariant (q, v, args) ->
    (match q with Some e -> e ^ "." ^ v | None -> v) ^ if args = [] then "" else "(" ^ String.concat ", " (List.map pat args) ^ ")"
  | PTuple ps -> "(" ^ String.concat ", " (List.map pat ps) ^ ")"

(* ---------- expressions ---------- *)

(* doubled, so a range fits between comparison and arithmetic *)
let prec = function
  | Or -> 2 | And -> 4 | Eq | Ne -> 6 | Lt | Le | Gt | Ge -> 8 | Range -> 9 | Add | Sub -> 10 | Mul | Div | Mod -> 12 | Else -> 14

let op_str = function
  | Or -> "||" | And -> "&&" | Eq -> "==" | Ne -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%" | Else -> "??" | Range -> "..<"

(* A struct literal that would be read as the start of a block in an
   `if`/`while`/`for`/`match` head. *)
let rec bare_struct e =
  match e.edesc with
  | EStruct (n, _, _) -> not (is_tuple_name n)
  | EBin (_, a, b) | EPipe (a, b) -> bare_struct a || bare_struct b
  | EUn (_, a) | ETry a | EField (a, _, _) | EIndex (a, _) -> bare_struct a
  | EMethod (r, _, _, _) -> bare_struct r
  | ECall (f, _) -> bare_struct f
  | _ -> false

let rec simple_expr e =
  match e.edesc with
  | EBlock _ | EIf _ | EMatch _ | EGroup _ -> false
  | EClosure (_, b) -> simple_expr b
  | EBin (_, a, b) | EIndex (a, b) | EPipe (a, b) -> simple_expr a && simple_expr b
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
    | EPipe _ -> 1
    | EUn _ -> 14
    | EClosure _ -> 0
    | _ -> 18
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
    if comment_in c a.espan.hi b.espan.lo then begin
      (* `a +  // why` then `b` on the next line: an operator at the end of a line continues it *)
      add c (if op = Range then op_str op else " " ^ op_str op);
      break_at_comments c (ind + 4) a.espan.hi b.espan.lo
    end
    else add c (if op = Range then op_str op else " " ^ op_str op ^ " ");
    expr c ind ~min:(p + 1) b
  | EPipe (a, b) ->
    (* a pipeline written one step per line stays that way *)
    expr c ind ~min:1 a;
    if has_newline c a.espan.hi b.espan.lo then begin
      break_at_comments c (ind + 4) a.espan.hi b.espan.lo;
      add c "|> "
    end
    else add c " |> ";
    expr c ind ~min:2 b
  | EUn (op, a) ->
    add c (match op with Neg -> "-" | Not -> "!");
    expr c ind ~min:16 a
  | ECall (f, args) ->
    expr c ind ~min:16 f;
    seq c ind "(" ")" (breaks_after_open c f.espan.hi '(') ~lo:f.espan.hi ~hi:e.espan.hi (fun x -> expr c (ind + 4) x) (fun x -> x.espan) args
  | EMethod (r, name, nsp, args) ->
    expr c ind ~min:16 r;
    if has_newline c r.espan.hi nsp.lo then break_at_comments c (ind + 4) r.espan.hi nsp.lo;
    add c ("." ^ name);
    seq c ind "(" ")" (breaks_after_open c nsp.hi '(') ~lo:nsp.hi ~hi:e.espan.hi (fun x -> expr c (ind + 4) x) (fun x -> x.espan) args
  | EField (r, name, nsp) ->
    expr c ind ~min:16 r;
    if has_newline c r.espan.hi nsp.lo then break_at_comments c (ind + 4) r.espan.hi nsp.lo;
    add c ("." ^ name)
  | EIndex (a, i) ->
    expr c ind ~min:16 a;
    add c "[";
    expr c ind i;
    add c "]"
  | ETry a ->
    expr c ind ~min:16 a;
    add c "?"
  | EList es -> seq c ind "[" "]" (breaks_after_open c e.espan.lo '[') ~lo:e.espan.lo ~hi:e.espan.hi (fun x -> expr c (ind + 4) x) (fun x -> x.espan) es
  | EStruct (name, _, fields) when is_tuple_name name ->
    seq c ind "(" ")" (breaks_after_open c e.espan.lo '(') ~lo:e.espan.lo ~hi:e.espan.hi (fun (_, _, x) -> expr c (ind + 4) x) (fun (_, _, x) -> x.espan) fields
  | EStruct (name, nsp, fields) ->
    add c (name ^ " ");
    let field (f, fsp, v) =
      if v.espan = fsp then add c f (* shorthand *)
      else (add c (f ^ ": "); expr c (ind + 4) v)
    in
    if fields = [] then add c "{}"
    else if breaks_after_open c nsp.hi '{' || comment_in c nsp.hi e.espan.hi then begin
      add c "{";
      List.iter
        (fun ((_, (fsp : Span.t), v) as fl) ->
          ignore (leading c (ind + 4) ~prev:(-1) fsp.lo);
          new_line c (ind + 4);
          field fl;
          add c ",";
          trailing c (if v.espan = fsp then fsp.hi else v.espan.hi))
        fields;
      ignore (leading c (ind + 4) ~prev:(-1) (e.espan.hi - 1));
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
  | ESelect arms ->
    add c "select {";
    let prev = ref (-1) in
    List.iter
      (fun a ->
        ignore (leading c (ind + 4) ~prev:!prev a.sspan_.lo);
        new_line c (ind + 4);
        (match a.sop with
         | SelRecv (b, ch) ->
           (match b with Some (n, _) -> add c (n ^ " = ") | None -> ());
           expr c (ind + 4) ch;
           add c ".recv()"
         | SelSend (ch, v) ->
           expr c (ind + 4) ch;
           add c ".send(";
           expr c (ind + 4) v;
           add c ")"
         | SelAfter (clk, ms) ->
           expr c (ind + 4) clk;
           add c ".after(";
           expr c (ind + 4) ms;
           add c ")"
         | SelDefault -> add c "_");
        (match a.sguard with Some g -> add c " if "; expr c (ind + 4) g | None -> ());
        add c " => ";
        expr c (ind + 4) a.sbody;
        add c ",";
        trailing c a.sbody.espan.hi;
        prev := a.sbody.espan.hi)
      arms;
    ignore (leading c (ind + 4) ~prev:!prev (e.espan.hi - 1));
    new_line c ind;
    add c "}"
  | EClosure (ps, body) ->
    if ps = [] then add c "||"
    else
      add c
        ("|" ^ String.concat ", " (List.map (fun (n, t, _) -> match t with Some t -> n ^ ": " ^ ty t | None -> n) ps) ^ "|");
    if comment_in c e.espan.lo body.espan.lo then begin
      (* the body on the next line, after the comment *)
      let after_params = List.fold_left (fun h (_, t, (sp : Span.t)) -> max h (match t with Some t -> t.tspan.hi | None -> sp.hi)) e.espan.lo ps in
      break_at_comments c (ind + 4) after_params body.espan.lo;
      expr c (ind + 4) body
    end
    else begin
      add c " ";
      expr c ind body
    end
  | EGroup (name, until, b) ->
    add c ("group " ^ name ^ " ");
    (match until with
     | Some (clk, ms) ->
       add c "until ";
       expr c ind clk;
       add c ".after(";
       expr c ind ms;
       add c ") "
     | None -> ());
    block c ind b
  | EDyn (x, _) -> expr c ind x

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
and seq : 'a. ctx -> int -> string -> string -> bool -> lo:int -> hi:int -> ('a -> unit) -> ('a -> Span.t) -> 'a list -> unit =
 fun c ind op cl multi ~lo ~hi item span xs ->
  (* a comment inside puts one element per line, so it can stay where it is *)
  let multi = multi || comment_in c lo hi in
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
    ignore (leading c (ind + 4) ~prev:!prev (hi - 1));
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
        (* `{ f(); }` is a block of type (), unlike `{ f() }`: keep the `;` *)
        (match (el, b.tail, List.rev b.stmts) with
         | `S ({ sdesc = SExpr _; _ } as s), None, last :: _ when s == last -> add c ";"
         | _ -> ());
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
  | SLetPat (p, e) ->
    add c ("let " ^ pat p ^ " = ");
    expr c ind e
  | SForPat (p, e, b) ->
    add c ("for " ^ pat p ^ " in ");
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

let iface_last_hi (i : iface_decl) =
  match List.rev i.if_methods with
  | (_, msp, ps, r) :: _ -> (
    match r with Some t -> t.tspan.hi | None -> ( match List.rev ps with p :: _ -> p.prm_ty.tspan.hi | [] -> msp.hi))
  | [] -> i.if_span.hi

let item c (it : item) =
  match it with
  | IIface i ->
    new_line c 0;
    add c ((if i.if_pub then "pub " else "") ^ "interface " ^ i.if_name ^ " {");
    let close = closing_brace c (iface_last_hi i) in
    let prev = ref (-1) in
    List.iter
      (fun (m, (msp : Span.t), ps, r) ->
        ignore (leading c 4 ~prev:!prev msp.lo);
        new_line c 4;
        let params = "self" :: List.map (fun p -> p.prm_name ^ ": " ^ ty p.prm_ty) ps in
        add c ("fn " ^ m ^ "(" ^ String.concat ", " params ^ ")" ^ match r with Some t -> " -> " ^ ty t | None -> "");
        let hi = match r with Some t -> t.tspan.hi | None -> ( match List.rev ps with p :: _ -> p.prm_ty.tspan.hi + 1 | [] -> msp.hi) in
        trailing c hi;
        prev := hi)
      i.if_methods;
    if i.if_methods = [] then add c "}"
    else begin
      ignore (leading c 4 ~prev:!prev close);
      new_line c 0;
      add c "}"
    end;
    trailing c (close + 1)
  | IFn ({ fn_test = Some name; _ } as d) ->
    new_line c 0;
    add c ("test \"" ^ escape name ^ "\" ");
    if d.fn_params <> [] then add c ("(" ^ String.concat ", " (List.map (fun p -> p.prm_name ^ ": " ^ ty p.prm_ty) d.fn_params) ^ ") ");
    block c 0 d.fn_body;
    trailing c d.fn_body.bspan.hi
  | IFn d ->
    new_line c 0;
    let params = List.map (fun p -> p.prm_name ^ ": " ^ ty p.prm_ty) d.fn_params in
    let bgens =
      match d.fn_gen with
      | [] -> ""
      | gs ->
        "[" ^ String.concat ", "
          (List.map (fun g -> match List.assoc_opt g d.fn_bounds with Some bs -> g ^ ": " ^ String.concat " + " (List.map fst bs) | None -> g) gs)
        ^ "]"
    in
    add c ((if d.fn_pub then "pub " else "") ^ "fn " ^ d.fn_name ^ bgens);
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

(* A `foreign "lib" { ... }` block: its declarations are consecutive items. *)
let foreign_sig (d : fn_decl) =
  (if d.fn_pub then "pub " else "") ^ "fn " ^ d.fn_name ^ "(" ^ String.concat ", " (List.map (fun p -> p.prm_name ^ ": " ^ ty p.prm_ty) d.fn_params) ^ ")"
  ^ match d.fn_ret with Some t -> " -> " ^ ty t | None -> ""

(* where the block's `foreign` keyword starts: the nearest line before [lo] that begins with it *)
let foreign_open c lo =
  let rec go k =
    if k <= 0 then 0
    else
      let ls = match String.rindex_from_opt c.src (k - 1) '\n' with Some j -> j + 1 | None -> 0 in
      let line = String.trim (String.sub c.src ls (min (String.length c.src - ls) 9)) in
      if String.starts_with ~prefix:"foreign" line then ls else if ls = 0 then 0 else go (ls - 1)
  in
  go lo

let foreign_block c lib (ds : fn_decl list) =
  new_line c 0;
  add c ("foreign \"" ^ escape lib ^ "\" {");
  let prev = ref (-1) in
  List.iter
    (fun (d : fn_decl) ->
      let k = ref d.fn_span.lo in
      while !k > 0 && c.src.[!k - 1] <> '\n' do decr k done;
      if !prev >= 0 && blank_between c !prev !k then blank c;
      let after = leading c 4 ~prev:!prev !k in
      if after >= 0 && !prev >= 0 && blank_between c after !k then blank c;
      new_line c 4;
      add c (foreign_sig d);
      trailing c d.fn_body.bspan.hi;
      prev := d.fn_body.bspan.hi)
    ds;
  let close = closing_brace c !prev in
  ignore (leading c 4 ~prev:!prev close);
  new_line c 0;
  add c "}";
  trailing c (close + 1);
  close + 1

let item_span c = function
  | IFn d -> (d.fn_span.lo, d.fn_body.bspan.hi)
  | IStruct s -> (s.st_span.lo, struct_close c s + 1)
  | IEnum e -> (e.en_span.lo, enum_close c e + 1)
  | IIface i -> (i.if_span.lo, closing_brace c (iface_last_hi i) + 1)

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
  (* consecutive declarations of one foreign block print as that block *)
  let rec groups = function
    | [] -> []
    | IFn ({ fn_foreign = Some { flib = lib; _ }; _ } as d) :: rest ->
      let rec take acc = function
        | IFn ({ fn_foreign = Some { flib = l; _ }; _ } as d) :: r when l = lib -> take (d :: acc) r
        | r -> (List.rev acc, r)
      in
      let ds, rest = take [ d ] rest in
      `Foreign (lib, ds) :: groups rest
    | it :: rest -> `Item it :: groups rest
  in
  List.iteri
    (fun i g ->
      let i = i + nuses in
      match g with
      | `Foreign (lib, (ds : fn_decl list)) ->
        let kw_lo = foreign_open c (List.hd ds).fn_span.lo in
        if i > 0 then blank c;
        let after = leading c 0 ~prev:!prev kw_lo in
        if after >= 0 && List.length c.o.lines > 0 && blank_between c after kw_lo then blank c;
        prev := foreign_block c lib ds
      | `Item it ->
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
    (groups items);
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
