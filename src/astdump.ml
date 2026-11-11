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

let op = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%" | Eq -> "==" | Ne -> "!=" | Lt -> "<"
  | Le -> "<=" | Gt -> ">" | Ge -> ">=" | And -> "&&" | Or -> "||"

let rec expr e =
  match e.edesc with
  | EInt n -> Int64.to_string n
  | EFloat f -> Printf.sprintf "%h" f
  | EBool b -> string_of_bool b
  | EUnit -> "()"
  | EStr ps -> "\"" ^ String.concat "" (List.map (function SPLit s -> String.escaped s | SPExpr x -> "{" ^ expr x ^ "}") ps) ^ "\""
  | EVar n -> n
  | EList es -> "[" ^ String.concat "," (List.map expr es) ^ "]"
  | EBin (o, a, b) -> "(" ^ expr a ^ op o ^ expr b ^ ")"
  | EUn (Neg, a) -> "(-" ^ expr a ^ ")"
  | EUn (Not, a) -> "(!" ^ expr a ^ ")"
  | ECall (f, a) -> expr f ^ "(" ^ String.concat "," (List.map expr a) ^ ")"
  | EMethod (r, n, _, a) -> expr r ^ "." ^ n ^ "(" ^ String.concat "," (List.map expr a) ^ ")"
  | EField (r, n, _) -> expr r ^ "." ^ n
  | EIndex (a, i) -> expr a ^ "[" ^ expr i ^ "]"
  | ETry a -> expr a ^ "?"
  | EIf (c, b, e) -> "if(" ^ expr c ^ ")" ^ block b ^ (match e with Some x -> "else" ^ expr x | None -> "")
  | EMatch (s, arms) -> "match(" ^ expr s ^ "){" ^ String.concat ";" (List.map (fun a -> pat a.apat ^ "=>" ^ expr a.abody) arms) ^ "}"
  | EBlock b -> block b
  | EClosure (ps, b) ->
    "|" ^ String.concat "," (List.map (fun (n, t, _) -> n ^ match t with Some t -> ":" ^ ty t | None -> "") ps) ^ "|" ^ expr b
  | EStruct (n, _, fs) -> n ^ "{" ^ String.concat "," (List.map (fun (f, _, x) -> f ^ ":" ^ expr x) fs) ^ "}"
  | EGroup (n, b) -> "group " ^ n ^ block b
  | EDyn (x, _) -> expr x

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
  | SBreak -> "break"
  | SContinue -> "continue"

let item = function
  | IFn { fn_test = Some n; fn_body; _ } -> "test " ^ String.escaped n ^ block fn_body
  | IIface i ->
    "interface " ^ i.if_name ^ "{"
    ^ String.concat ";"
        (List.map
           (fun (m, _, ps, r) ->
             m ^ "(" ^ String.concat "," (List.map (fun p -> p.prm_name ^ ":" ^ ty p.prm_ty) ps) ^ ")" ^ (match r with Some t -> ty t | None -> ""))
           i.if_methods)
    ^ "}"
  | IFn d ->
    "fn " ^ d.fn_name ^ "["
    ^ String.concat "," (List.map (fun g -> g ^ (match List.assoc_opt g d.fn_bounds with Some bs -> ":" ^ String.concat "+" (List.map fst bs) | None -> "")) d.fn_gen)
    ^ "]("
    ^ String.concat "," (List.map (fun p -> p.prm_name ^ ":" ^ ty p.prm_ty) d.fn_params)
    ^ ")" ^ (match d.fn_ret with Some t -> ty t | None -> "") ^ block d.fn_body
  | IStruct s -> "struct " ^ s.st_name ^ "[" ^ String.concat "," s.st_gen ^ "]{" ^ String.concat "," (List.map (fun (f, _, t) -> f ^ ":" ^ ty t) s.st_fields) ^ "}"
  | IEnum e ->
    "enum " ^ e.en_name ^ "[" ^ String.concat "," e.en_gen ^ "]{"
    ^ String.concat "," (List.map (fun (v, _, ts) -> v ^ "(" ^ String.concat "," (List.map ty ts) ^ ")") e.en_variants)
    ^ "}"

let pub_of = function IFn d -> d.fn_pub | IStruct s -> s.st_pub | IEnum e -> e.en_pub | IIface i -> i.if_pub
let items is = String.concat "\n" (List.map (fun i -> (if pub_of i then "pub " else "") ^ item i) is)
let file (f : file) = String.concat "\n" (List.map (fun (u, _) -> "use " ^ u) f.uses) ^ "\n" ^ items f.items
