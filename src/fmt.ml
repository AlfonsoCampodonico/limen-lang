(* tessera fmt: print a program in the canonical style.

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
      (* at least two spaces, more if the author lined comments up *)
      let k = ref clo in
      while !k > 0 && c.src.[!k - 1] = ' ' do decr k done;
      add c (String.make (max 2 (clo - !k)) ' ' ^ text);
      go ()
    | _ -> ()
  in
  go ()

(* Is there a comment not printed yet that starts in [lo, hi)? *)
let comment_in c lo hi = List.exists (fun (clo, _, _) -> clo >= lo && clo < hi) c.pending

(* ... in [lo, hi) but outside every span in [inner]: a comment inside an
   argument belongs to that argument, not to the list around it *)
let comment_between c lo hi (inner : Span.t list) =
  List.exists
    (fun (clo, _, _) -> clo >= lo && clo < hi && not (List.exists (fun (sp : Span.t) -> clo >= sp.lo && clo < sp.hi) inner))
    c.pending

(* Does the source text of [lo, hi) sit inside one pair of parentheses,
   `(...)`, with the one at [lo] closed by the one at [hi - 1]? The parser
   gives a parenthesized expression the span of its parentheses. *)
let wrapped_span c lo hi =
  let n = String.length c.src in
  lo >= 0 && hi <= n && hi - lo >= 2 && c.src.[lo] = '(' && c.src.[hi - 1] = ')'
  &&
  let depth = ref 1 and i = ref (lo + 1) and ok = ref true in
  while !ok && !i < hi - 1 do
    (match c.src.[!i] with
     | '"' ->
       (* skip a string; interpolations inside it are balanced *)
       incr i;
       while !i < hi - 1 && c.src.[!i] <> '"' do if c.src.[!i] = '\\' then incr i; incr i done
     | '(' | '[' | '{' -> incr depth
     | ')' | ']' | '}' -> decr depth; if !depth = 0 then ok := false
     | _ -> ());
    incr i
  done;
  !ok

(* The source of a literal without parentheses written around it. *)
let rec bare_slice c (sp : Span.t) =
  if wrapped_span c sp.lo sp.hi then begin
    let lo = ref (sp.lo + 1) and hi = ref (sp.hi - 1) in
    while !lo < !hi && (c.src.[!lo] = ' ' || c.src.[!lo] = '\n') do incr lo done;
    while !hi > !lo && (c.src.[!hi - 1] = ' ' || c.src.[!hi - 1] = '\n') do decr hi done;
    bare_slice c { sp with lo = !lo; hi = !hi }
  end
  else String.sub c.src sp.lo (sp.hi - sp.lo)

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
  | PList ps -> "[" ^ String.concat ", " (List.map pat ps) ^ "]"
  | PRest None -> ".."
  | PRest (Some n) -> ".." ^ n
  | POr ps -> String.concat " | " (List.map pat ps)

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
  | EUn (_, a) | ETry (a, _) | EField (a, _, _) | EIndex (a, _) | ESafe (a, _, _, _) -> bare_struct a
  | EMethod (r, _, _, _) -> bare_struct r
  | ECall (f, _) -> bare_struct f
  | _ -> false

(* A closure the author wrote after the call's parentheses,
   `f(a) { |x| ... }`: the arguments before it, and the closure. fmt keeps
   the form the author chose; both are canonical. *)
let trailing_arg c args =
  match List.rev args with
  | ({ edesc = EClosure (_, { edesc = EBlock _; _ }); espan; _ } as last) :: rest
    when espan.lo < String.length c.src && c.src.[espan.lo] = '{' ->
    Some (List.rev rest, last)
  | _ -> None

(* An expression with a trailing closure, which an `if`/`while`/`for`/`match`
   head would read as the start of its body. *)
let rec bare_trailing c e =
  match e.edesc with
  | ECall (f, args) -> trailing_arg c args <> None || bare_trailing c f
  | EMethod (r, _, _, args) -> trailing_arg c args <> None || bare_trailing c r
  | EBin (_, a, b) -> bare_trailing c a || bare_trailing c b
  | EUn (_, a) | ETry (a, _) | EField (a, _, _) | EIndex (a, _) -> bare_trailing c a
  | _ -> false

let rec simple_expr e =
  match e.edesc with
  | EBlock _ | EIf _ | EMatch _ | EGroup _ -> false
  | EClosure (_, b) -> simple_expr b
  | EBin (_, a, b) | EIndex (a, b) | EPipe (a, b) -> simple_expr a && simple_expr b
  | EUn (_, a) | ETry (a, None) | EField (a, _, _) -> simple_expr a
  | ETry (a, Some c) -> simple_expr a && simple_expr c
  | ECall (f, args) -> simple_expr f && List.for_all simple_expr args
  | EMethod (r, _, _, args) -> simple_expr r && List.for_all simple_expr args
  | EList es -> List.for_all simple_expr es
  | EStruct (_, _, fs) -> List.for_all (fun (_, _, x) -> simple_expr x) fs
  | EWith (b, fs) -> simple_expr b && List.for_all (fun (_, _, x) -> simple_expr x) fs
  | ESafe (b, _, rest, _) -> simple_expr b && simple_expr rest
  | _ -> true

(* [keep]: this is an operand, and parentheses the author wrote around a
   compound one stay even where precedence does not need them, as in
   `(a && b) || c` *)
let rec expr c ind ?(min = 0) ?(keep = false) e =
  let p =
    match e.edesc with
    | EBin (op, _, _) -> prec op
    | EPipe _ -> 1
    (* tighter than any binary operator, so `x ?? -1` needs no parentheses *)
    | EUn _ -> 15
    | EClosure _ -> 0
    | _ -> 18
  in
  let kept =
    keep
    && (match e.edesc with EBin _ | EPipe _ | EIf _ | EMatch _ -> true | _ -> false)
    && wrapped_span c e.espan.lo e.espan.hi
  in
  if p < min || kept then (add c "("; expr_inner c ind e; add c ")") else expr_inner c ind e

and expr_inner c ind e =
  match e.edesc with
  | EInt _ | EFloat _ | EStr _ -> add c (bare_slice c e.espan)
  | EBool b -> add c (string_of_bool b)
  | EUnit -> add c "()"
  | EVar n -> add c n
  | EBin (op, a, b) ->
    let p = prec op in
    expr c ind ~min:p ~keep:true a;
    if comment_in c a.espan.hi b.espan.lo then begin
      (* `a +  // why` then `b` on the next line: an operator at the end of a line continues it *)
      add c (if op = Range then op_str op else " " ^ op_str op);
      break_at_comments c (ind + 4) a.espan.hi b.espan.lo
    end
    else add c (if op = Range then op_str op else " " ^ op_str op ^ " ");
    expr c ind ~min:(p + 1) ~keep:true b
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
    call_args c ind f.espan.hi e.espan.hi args
  | ENamed (n, _, x) ->
    add c (n ^ ": ");
    expr c ind x
  | EMethod (r, name, nsp, args) ->
    receiver c ind r nsp;
    add c name;
    call_args c ind nsp.hi e.espan.hi args
  | EField (r, name, nsp) ->
    receiver c ind r nsp;
    add c name
  | EIndex (a, i) ->
    expr c ind ~min:16 a;
    add c "[";
    expr c ind i;
    add c "]"
  | ETry (a, ctx) ->
    expr c ind ~min:16 a;
    add c "?";
    Option.iter (fun x -> add c " "; expr c ind x) ctx
  | EList es -> seq c ind "[" "]" (breaks_after_open c e.espan.lo '[') ~lo:e.espan.lo ~hi:e.espan.hi (fun i x -> expr c i x) (fun x -> x.espan) es
  | EStruct (name, _, fields) when is_tuple_name name ->
    seq c ind "(" ")" (breaks_after_open c e.espan.lo '(') ~lo:e.espan.lo ~hi:e.espan.hi (fun i (_, _, x) -> expr c i x) (fun (_, _, x) -> x.espan) fields
  | EStruct (name, nsp, fields) ->
    add c (name ^ " ");
    let field i (f, fsp, v) =
      if v.espan = fsp then add c f (* shorthand *)
      else (add c (f ^ ": "); expr c i v)
    in
    let values = List.map (fun (_, _, v) -> v.espan) fields in
    if fields = [] then add c "{}"
    else if breaks_after_open c nsp.hi '{' || comment_between c nsp.hi e.espan.hi values then begin
      add c "{";
      List.iter
        (fun ((_, (fsp : Span.t), v) as fl) ->
          ignore (leading c (ind + 4) ~prev:(-1) fsp.lo);
          new_line c (ind + 4);
          field (ind + 4) fl;
          add c ",";
          trailing c (if v.espan = fsp then fsp.hi else v.espan.hi))
        fields;
      ignore (leading c (ind + 4) ~prev:(-1) (e.espan.hi - 1));
      new_line c ind;
      add c "}"
    end
    else begin
      (* on one line: a value that breaks hugs the braces, at this indentation *)
      add c "{ ";
      List.iteri (fun i fl -> if i > 0 then add c ", "; field ind fl) fields;
      add c " }"
    end
  | ESafe (base, x, rest, _) ->
    (* `rest` starts from the value inside the Option: print it with that
       value written as `base?`, so `.b` after it reads `base?.b` *)
    let rec fill (e : expr) =
      let d =
        match e.edesc with
        | EVar n when n = x -> ETry (base, None)
        | EField (r, n, sp) -> EField (fill r, n, sp)
        | EMethod (r, n, sp, args) -> EMethod (fill r, n, sp, args)
        | ECall (g, args) -> ECall (fill g, args)
        | EIndex (a, i) -> EIndex (fill a, i)
        | ESafe (b, y, r, fl) -> ESafe (fill b, y, r, fl)
        | d -> d
      in
      (* the filled-in `base?` keeps the span of the `?.`; [receiver] knows it by that *)
      { e with edesc = d }
    in
    expr_inner c ind (fill rest)
  | EWith (base, fields) ->
    (* like a struct literal: one line, or one field per line if written so *)
    let field i (f, fsp, v) =
      if v.espan = fsp then add c f (* shorthand *)
      else (add c (f ^ ": "); expr c i v)
    in
    add c "{ ";
    expr c ind base;
    add c " with";
    let first = match fields with (_, (fsp : Span.t), _) :: _ -> fsp.lo | [] -> e.espan.hi in
    let values = List.map (fun (_, _, v) -> v.espan) fields in
    if has_newline c base.espan.hi first || comment_between c base.espan.hi e.espan.hi values then begin
      List.iter
        (fun ((_, (fsp : Span.t), v) as fl) ->
          ignore (leading c (ind + 4) ~prev:(-1) fsp.lo);
          new_line c (ind + 4);
          field (ind + 4) fl;
          add c ",";
          trailing c (if v.espan = fsp then fsp.hi else v.espan.hi))
        fields;
      ignore (leading c (ind + 4) ~prev:(-1) (e.espan.hi - 1));
      new_line c ind;
      add c "}"
    end
    else begin
      add c " ";
      List.iteri (fun i fl -> if i > 0 then add c ", "; field ind fl) fields;
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
        add c (pat a.apat);
        (match a.aguard with Some g -> add c " if "; expr c (ind + 4) g | None -> ());
        add c " => ";
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
  | EGroup (name, interleaved, until, b) ->
    add c ("group " ^ name ^ " ");
    if interleaved then add c "interleaved ";
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

(* The receiver of `.name`, then the dot: on the next line if the author
   broke the chain there. A `base?` whose span starts after its operand is
   the start of a safe-navigation chain (see ESafe), printed `base?.`. *)
and receiver c ind r nsp =
  match r.edesc with
  | ETry (b, None) when r.espan.lo >= b.espan.hi ->
    expr c ind ~min:16 b;
    if has_newline c b.espan.hi nsp.lo then break_at_comments c (ind + 4) b.espan.hi r.espan.lo;
    add c "?."
  | _ ->
    (* `(e?).name`: without the parentheses it would read as `e?.name` *)
    (match r.edesc with ETry _ -> add c "("; expr c ind r; add c ")" | _ -> expr c ind ~min:16 r);
    if has_newline c r.espan.hi nsp.lo then break_at_comments c (ind + 4) r.espan.hi nsp.lo;
    add c "."

(* `(a, b)`, or `(a) { |x| ... }` / ` { ... }` with a trailing closure;
   the arguments start after [lo]. *)
and call_args c ind lo hi args =
  let plain xs hi = seq c ind "(" ")" (breaks_after_open c lo '(') ~lo ~hi (fun i x -> expr c i x) (fun x -> x.espan) xs in
  match trailing_arg c args with
  | Some (rest, ({ edesc = EClosure (ps, { edesc = EBlock b; _ }); _ } as clo)) ->
    if rest <> [] then plain rest clo.espan.lo;
    add c " ";
    let params =
      if ps = [] then ""
      else "|" ^ String.concat ", " (List.map (fun (n, t, _) -> match t with Some t -> n ^ ": " ^ ty t | None -> n) ps) ^ "|"
    in
    block c ind ~params b
  | _ -> plain args hi

and head c ind e =
  if bare_struct e || bare_trailing c e then (add c "("; expr c ind e; add c ")") else expr c ind e

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
   comma when the author broke after the opening bracket. [item] prints an
   element at the indentation it is given. On one line an element that
   breaks hugs the brackets: `f([` ... `])`, its insides one level in. *)
and seq : 'a. ctx -> int -> string -> string -> bool -> lo:int -> hi:int -> (int -> 'a -> unit) -> ('a -> Span.t) -> 'a list -> unit =
 fun c ind op cl multi ~lo ~hi item span xs ->
  (* a comment between elements puts one element per line, so it can stay where it is *)
  let multi = multi || comment_between c lo hi (List.map span xs) in
  if multi && xs <> [] then begin
    add c op;
    let prev = ref (-1) in
    List.iter
      (fun x ->
        let sp = span x in
        ignore (leading c (ind + 4) ~prev:!prev sp.lo);
        new_line c (ind + 4);
        item (ind + 4) x;
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
    List.iteri (fun i x -> if i > 0 then add c ", "; item ind x) xs;
    add c cl
  end

(* [params]: the block is a trailing closure's body, `{ |x| ... }`. *)
and block c ind ?(params = "") b =
  let inline =
    b.stmts = [] && (match b.tail with None -> true | Some e -> simple_expr e) && not (has_newline c b.bspan.lo b.bspan.hi)
  in
  (* `?? { return None }`, `=> { done = true }`: one short jump or
     assignment, written on one line, stays on it *)
  let one_line_stmt =
    match (b.stmts, b.tail) with
    | [ s ], None when params = "" && not (has_newline c b.bspan.lo b.bspan.hi) -> (
      match s.sdesc with
      | SReturn None | SBreak | SContinue -> Some s
      | SReturn (Some e) when simple_expr e -> Some s
      | SAssign (l, r) when simple_expr l && simple_expr r -> Some s
      | _ -> None)
    | _ -> None
  in
  if inline then
    match b.tail with
    | None -> add c (if params = "" then "{}" else "{ " ^ params ^ " }")
    | Some e -> add c ("{ " ^ (if params = "" then "" else params ^ " ")); expr c ind e; add c " }"
  else if one_line_stmt <> None && not (comment_in c b.bspan.lo b.bspan.hi) then begin
    add c "{ ";
    Option.iter (stmt c ind) one_line_stmt;
    add c " }"
  end
  else begin
    add c (if params = "" then "{" else "{ " ^ params);
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
  | SDefer { stmts = []; tail = Some e; _ } -> add c "defer "; expr c ind e
  | SDefer { stmts = [ ({ sdesc = SAssign _ | SReturn _ | SBreak | SContinue; _ } as s) ]; tail = None; _ } -> add c "defer "; stmt c ind s
  | SDefer b -> add c "defer "; block c ind b

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

(* Type parameters with their bounds: `[K: Key, V]`. *)
let bgens gs bounds =
  match gs with
  | [] -> ""
  | gs ->
    "[" ^ String.concat ", " (List.map (fun g -> match List.assoc_opt g bounds with Some bs -> g ^ ": " ^ String.concat " + " (List.map ty bs) | None -> g) gs) ^ "]"

let item c (it : item) =
  match it with
  | IType a ->
    new_line c 0;
    add c ((if a.ty_pub then "pub " else "") ^ "type " ^ a.ty_name ^ gens a.ty_gen ^ " = " ^ ty a.ty_body);
    trailing c a.ty_body.tspan.hi
  | IIface i ->
    new_line c 0;
    add c ((if i.if_pub then "pub " else "") ^ "interface " ^ i.if_name ^ gens i.if_gen ^ " {");
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
    add c ((if d.fn_bench then "bench" else "test") ^ " \"" ^ escape name ^ "\" ");
    let cap p = p.prm_name ^ ": " ^ (if d.fn_virtual && p.prm_ty.tdesc = TyName ("Clock", []) then "virtual " else "") ^ ty p.prm_ty in
    if d.fn_params <> [] then add c ("(" ^ String.concat ", " (List.map cap d.fn_params) ^ ") ");
    (match d.fn_table with
     | Some (p, cases) ->
       add c ("for " ^ pat p ^ " in ");
       head c 0 cases;
       add c " "
     | None -> ());
    block c 0 d.fn_body;
    trailing c d.fn_body.bspan.hi
  | IFn d ->
    new_line c 0;
    let params = List.map (fun p -> p.prm_name ^ ": " ^ ty p.prm_ty) d.fn_params in
    add c ((if d.fn_pub then "pub " else "") ^ "fn " ^ d.fn_name ^ bgens d.fn_gen d.fn_bounds);
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
    add c ((if s.st_pub then "pub " else "") ^ "struct " ^ s.st_name ^ bgens s.st_gen s.st_bounds ^ " {");
    let close = struct_close c s in
    (* a struct written on one line keeps its trailing comment after the brace *)
    let single = not (has_newline c s.st_span.lo close) in
    let prev = ref (-1) in
    List.iter
      (fun (f, (fsp : Span.t), t) ->
        ignore (leading c 4 ~prev:!prev fsp.lo);
        new_line c 4;
        (* an embedded struct is written as its type alone *)
        add c ((if List.mem f s.st_embeds then ty t else f ^ ": " ^ ty t) ^ ",");
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
    add c ((if e.en_pub then "pub " else "") ^ "enum " ^ e.en_name ^ bgens e.en_gen e.en_bounds ^ " {");
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
  let params = List.map (fun p -> p.prm_name ^ ": " ^ ty p.prm_ty) d.fn_params in
  let fo = Option.get d.fn_foreign in
  (* a variadic function's `...` goes back among its parameters *)
  let params =
    match fo.fvariadic with Some k -> List.filteri (fun i _ -> i < k) params @ [ "..." ] @ List.filteri (fun i _ -> i >= k) params | None -> params
  in
  (if d.fn_pub then "pub " else "") ^ "fn " ^ d.fn_name ^ "(" ^ String.concat ", " params ^ ")"
  ^ (match d.fn_ret with Some t -> " -> " ^ ty t | None -> "")
  ^ if fo.fsym <> d.fn_name then " = \"" ^ escape fo.fsym ^ "\"" else ""

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

(* A C struct in a foreign block: on one line, unless it was written over several. *)
let foreign_struct c (s : struct_decl) =
  let close = struct_close c s in
  let head = (if s.st_pub then "pub " else "") ^ "struct " ^ s.st_name ^ " {" in
  if not (has_newline c s.st_span.lo close) then
    add c (head ^ (if s.st_fields = [] then "}" else " " ^ String.concat ", " (List.map (fun (f, _, t) -> f ^ ": " ^ ty t) s.st_fields) ^ " }"))
  else begin
    add c head;
    let prev = ref (-1) in
    List.iter
      (fun (f, (fsp : Span.t), t) ->
        ignore (leading c 8 ~prev:!prev fsp.lo);
        new_line c 8;
        add c (f ^ ": " ^ ty t ^ ",");
        trailing c t.tspan.hi;
        prev := t.tspan.hi)
      s.st_fields;
    ignore (leading c 8 ~prev:!prev close);
    new_line c 4;
    add c "}"
  end;
  close + 1

let foreign_block c lib (ds : item list) =
  new_line c 0;
  add c ("foreign \"" ^ escape lib ^ "\" {");
  let prev = ref (-1) in
  List.iter
    (fun it ->
      let lo = match it with IFn d -> d.fn_span.lo | IStruct s -> s.st_span.lo | _ -> 0 in
      let k = ref lo in
      while !k > 0 && c.src.[!k - 1] <> '\n' do decr k done;
      if !prev >= 0 && blank_between c !prev !k then blank c;
      let after = leading c 4 ~prev:!prev !k in
      if after >= 0 && !prev >= 0 && blank_between c after !k then blank c;
      new_line c 4;
      let hi =
        match it with
        | IFn d -> add c (foreign_sig d); d.fn_body.bspan.hi
        | IStruct s -> foreign_struct c s
        | _ -> !prev
      in
      trailing c hi;
      prev := hi)
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
  | IType a -> (a.ty_span.lo, a.ty_body.tspan.hi)

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
  let foreign_lib = function
    | IFn { fn_foreign = Some { flib; _ }; _ } -> Some flib
    | IStruct { st_foreign = Some (l, _); _ } -> Some l
    | _ -> None
  in
  let rec groups = function
    | [] -> []
    | it :: rest when foreign_lib it <> None ->
      let lib = foreign_lib it in
      let rec take acc = function
        | it :: r when foreign_lib it = lib -> take (it :: acc) r
        | r -> (List.rev acc, r)
      in
      let ds, rest = take [ it ] rest in
      `Foreign (Option.get lib, ds) :: groups rest
    | it :: rest -> `Item it :: groups rest
  in
  let last_alias = ref false in
  List.iteri
    (fun i g ->
      let i = i + nuses in
      (* consecutive aliases stay together, like `use` lines, unless the author spaced them *)
      let alias = match g with `Item (IType _) -> true | _ -> false in
      let follows_alias = alias && !last_alias in
      last_alias := alias;
      match g with
      | `Foreign (lib, ds) ->
        let kw_lo = foreign_open c (match List.hd ds with IFn d -> d.fn_span.lo | IStruct s -> s.st_span.lo | _ -> 0) in
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
      if i > 0 && not (follows_alias && not (blank_between c !prev kw_lo)) then blank c;
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
