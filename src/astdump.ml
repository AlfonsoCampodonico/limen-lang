(* A position-free rendering of the syntax tree, used to check that
   formatting never changes a program's meaning. *)
open Ast

let rec ty (t : ty_expr) =
  match t.tdesc with
  | TyUnit -> "()"
  | TyInt n -> string_of_int n
  | TyName (n, args) -> n ^ "[" ^ String.concat "," (List.map ty args) ^ "]"
  | TyFn (ps, r) -> "fn(" ^ String.concat "," (List.map ty ps) ^ ")" ^ ty r

let rec pat (p : pat) =
  match p.pdesc with
  | PWild -> "_"
  | PBind n -> "b:" ^ n
  | PInt n -> Int64.to_string n
  | PStr s -> String.escaped s
  | PBool b -> string_of_bool b
  | PVariant (q, v, a) -> Option.value ~default:"" q ^ "." ^ v ^ "(" ^ String.concat "," (List.map pat a) ^ ")"
  | PTuple ps -> "(" ^ String.concat "," (List.map pat ps) ^ ")"
  | PList ps -> "[" ^ String.concat "," (List.map pat ps) ^ "]"
  | PRest None -> ".."
  | PRest (Some n) -> "..b:" ^ n
  | POr ps -> "or(" ^ String.concat "|" (List.map pat ps) ^ ")"
  | PStruct (n, _, fs, rest) ->
    n ^ "{" ^ String.concat "," (List.map (fun (f, _, q) -> f ^ ":" ^ pat q) fs @ if rest then [ ".." ] else []) ^ "}"
  | PAs (q, n, _) -> "as(" ^ pat q ^ "," ^ n ^ ")"

let op = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%" | Eq -> "==" | Ne -> "!=" | Lt -> "<"
  | Le -> "<=" | Gt -> ">" | Ge -> ">=" | And -> "&&" | Or -> "||" | Else -> "??" | Range -> "..<"
  | BitAnd -> "&" | BitOr -> "|" | BitXor -> "^" | Shl -> "<<" | Shr -> ">>"

let rec expr e =
  match e.edesc with
  | EInt n -> Int64.to_string n
  | EFloat f -> Printf.sprintf "%h" f
  | EBool b -> string_of_bool b
  | EUnit -> "()"
  | EStr ps -> "\"" ^ String.concat "" (List.map (function SPLit s -> String.escaped s | SPExpr x -> "{" ^ expr x ^ "}") ps) ^ "\""
  | EVar n when String.starts_with ~prefix:"safe$" n -> "$"
  | EVar n -> n
  | EList es -> "[" ^ String.concat "," (List.map expr es) ^ "]"
  | EBin (o, a, b) -> "(" ^ expr a ^ op o ^ expr b ^ ")"
  | EPipe (a, b) -> "(" ^ expr a ^ "|>" ^ expr b ^ ")"
  | EUn (Neg, a) -> "(-" ^ expr a ^ ")"
  | EUn (Not, a) -> "(!" ^ expr a ^ ")"
  | EUn (BitNot, a) -> "(~" ^ expr a ^ ")"
  | ECall (f, a) -> expr f ^ "(" ^ String.concat "," (List.map expr a) ^ ")"
  | ENamed (n, _, a) -> n ^ ":" ^ expr a
  | EMethod (r, n, _, a) -> expr r ^ "." ^ n ^ "(" ^ String.concat "," (List.map expr a) ^ ")"
  | EField (r, n, _) -> expr r ^ "." ^ n
  | EIndex (a, i) -> expr a ^ "[" ^ expr i ^ "]"
  | ETry (a, None) -> expr a ^ "?"
  | ETry (a, Some c) -> expr a ^ "?" ^ expr c
  | EIf (c, b, e) -> "if(" ^ expr c ^ ")" ^ block b ^ (match e with Some x -> "else" ^ expr x | None -> "")
  | EMatch (s, arms) -> "match(" ^ expr s ^ "){" ^ String.concat ";" (List.map (fun a -> pat a.apat ^ (match a.aguard with Some g -> " if " ^ expr g | None -> "") ^ "=>" ^ expr a.abody) arms) ^ "}"
  | EBlock b -> block b
  | EClosure (ps, b) ->
    "|" ^ String.concat "," (List.map (fun (n, t, _) -> n ^ match t with Some t -> ":" ^ ty t | None -> "") ps) ^ "|" ^ expr b
  | EStruct (n, _, fs) -> n ^ "{" ^ String.concat "," (List.map (fun (f, _, x) -> f ^ ":" ^ expr x) fs) ^ "}"
  | ESafe (b, _, rest, _) -> "safe(" ^ expr b ^ "," ^ expr rest ^ ")"
  | EWith (b, fs) -> "{" ^ expr b ^ " with " ^ String.concat "," (List.map (fun (f, _, x) -> f ^ ":" ^ expr x) fs) ^ "}"
  | EGroup (n, il, None, b) -> "group " ^ n ^ (if il then " interleaved" else "") ^ block b
  | EGroup (n, il, Some (c, ms), b) ->
    "group " ^ n ^ (if il then " interleaved" else "") ^ " until " ^ expr c ^ ".after(" ^ expr ms ^ ")" ^ block b
  | EDyn (x, _) -> expr x
  | ESelect arms ->
    "select{"
    ^ String.concat ";"
        (List.map
           (fun a ->
             (match a.sop with
              | SelRecv (b, c) -> (match b with Some (n, _) -> n | None -> "_") ^ "=" ^ expr c ^ ".recv()"
              | SelSend (c, v) -> expr c ^ ".send(" ^ expr v ^ ")"
              | SelAfter (c, ms) -> expr c ^ ".after(" ^ expr ms ^ ")"
              | SelDefault -> "_")
             ^ (match a.sguard with Some g -> " if " ^ expr g | None -> "")
             ^ "=>" ^ expr a.sbody)
           arms)
    ^ "}"

and block b =
  "{" ^ String.concat ";" (List.map stmt b.stmts) ^ (match b.tail with Some e -> "|tail " ^ expr e | None -> "") ^ "}"

and stmt s =
  match s.sdesc with
  | SLet (m, n, _, t, e) -> "let " ^ (if m then "mut " else "") ^ n ^ (match t with Some t -> ":" ^ ty t | None -> "") ^ "=" ^ expr e
  | SAssign (a, b) -> expr a ^ "=" ^ expr b
  | SExpr e -> expr e
  | SReturn e -> "return " ^ (match e with Some e -> expr e | None -> "")
  | SWhile (c, b) -> "while(" ^ expr c ^ ")" ^ block b
  | SFor (n, _, e, b) -> "for " ^ n ^ " in " ^ expr e ^ block b
  | SLetPat (p, e) -> "let " ^ pat p ^ "=" ^ expr e
  | SForPat (p, e, b) -> "for " ^ pat p ^ " in " ^ expr e ^ block b
  | SBreak -> "break"
  | SContinue -> "continue"
  | SDefer b -> "defer" ^ block b

let gens gs bounds =
  String.concat "," (List.map (fun g -> g ^ (match List.assoc_opt g bounds with Some bs -> ":" ^ String.concat "+" (List.map ty bs) | None -> "")) gs)

let item = function
  | IFn { fn_test = Some n; fn_body; fn_params; fn_bench; fn_virtual; fn_table; _ } ->
    (if fn_bench then "bench " else "test ") ^ String.escaped n ^ (if fn_virtual then " virtual" else "") ^ "("
    ^ String.concat "," (List.map (fun p -> p.prm_name ^ ":" ^ ty p.prm_ty) fn_params)
    ^ ")" ^ (match fn_table with Some (p, e) -> "for " ^ pat p ^ " in " ^ expr e | None -> "") ^ block fn_body
  | IIface i ->
    "interface " ^ i.if_name ^ "[" ^ String.concat "," i.if_gen ^ "]{"
    ^ String.concat ";"
        (List.map
           (fun (m, _, ps, r) ->
             m ^ "(" ^ String.concat "," (List.map (fun p -> p.prm_name ^ ":" ^ ty p.prm_ty) ps) ^ ")" ^ (match r with Some t -> ty t | None -> ""))
           i.if_methods)
    ^ "}"
  | IFn d ->
    (match d.fn_foreign with
     | Some fo ->
       "foreign " ^ String.escaped fo.flib ^ " " ^ (if fo.fsym <> d.fn_name then "=" ^ fo.fsym ^ " " else "")
       ^ (match fo.fvariadic with Some k -> "..." ^ string_of_int k ^ " " | None -> "")
       ^ (if fo.fblocking then "blocking " else "")
     | None -> "")
    ^ (if d.fn_pub then "pub " else "")
    ^ "fn " ^ d.fn_name ^ "["
    ^ gens d.fn_gen d.fn_bounds
    ^ "]("
    ^ String.concat "," (List.map (fun p -> p.prm_name ^ ":" ^ ty p.prm_ty) d.fn_params)
    ^ ")" ^ (match d.fn_ret with Some t -> ty t | None -> "") ^ block d.fn_body
  | IStruct s -> (match s.st_foreign with Some (l, _) -> "foreign " ^ String.escaped l ^ " " | None -> "") ^ "struct " ^ s.st_name ^ "[" ^ gens s.st_gen s.st_bounds ^ "]{" ^ String.concat "," (List.map (fun (f, _, t) -> if List.mem f s.st_embeds then "embed " ^ ty t else f ^ ":" ^ ty t) s.st_fields) ^ "}"
  | IEnum e ->
    "enum " ^ e.en_name ^ "[" ^ gens e.en_gen e.en_bounds ^ "]{"
    ^ String.concat "," (List.map (fun (v, _, ts) -> v ^ "(" ^ String.concat "," (List.map ty ts) ^ ")") e.en_variants)
    ^ "}"
  | IType a -> "type " ^ a.ty_name ^ "[" ^ String.concat "," a.ty_gen ^ "]=" ^ ty a.ty_body

let pub_of = function IFn d -> d.fn_pub | IStruct s -> s.st_pub | IEnum e -> e.en_pub | IIface i -> i.if_pub | IType a -> a.ty_pub
let items is = String.concat "\n" (List.map (fun i -> (if pub_of i then "pub " else "") ^ item i) is)
let file (f : file) = String.concat "\n" (List.map (fun (u, _) -> "use " ^ u) f.uses) ^ "\n" ^ items f.items
