(* Every place a name is defined or used, for go to definition, find
   references and rename in the language server and the MCP server.

   Works on the checked program (all modules merged, see Modules), so a use
   in one module finds its definition in another. Each occurrence covers just
   the name as written: `parse` in `json.parse(s)`, `area` in `s.area()`.

   Symbols are keyed by kind and qualified name:
     f:NAME       function or method (`json.parse`, `Point::area`)
     t:NAME       struct, enum or interface
     v:ENUM#V     enum variant
     d:STRUCT#F   struct field
     m:IFACE#M    interface method
     l:FILE#OFF   local variable, parameter or group handle, by its definition *)
open Ast

type shorthand =
  | Plain
  | Field_side (* the field of a `S { name }` shorthand, which is also a local *)
  | Var_side of string (* the local of a shorthand; the field's name *)

type occ = { key : string; name : string; sp : Span.t; def : bool; short : shorthand }

let text_of (sp : Span.t) = Source.text sp.file

let is_ident_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

let bare n =
  let rec last_colons i = if i < 0 then None else if String.sub n i 2 = "::" then Some i else last_colons (i - 1) in
  let n = match last_colons (String.length n - 2) with Some i -> String.sub n (i + 2) (String.length n - i - 2) | None -> n in
  match String.rindex_opt n '.' with Some i -> String.sub n (i + 1) (String.length n - i - 1) | None -> n

(* The span of [name] where [sp] starts: [sp] itself when it is just the
   name, otherwise the last segment of the dotted identifier at its start that
   reads [name] (`json.Value[Int]` -> `Value`). None when the source does not
   say [name] there, as for nodes the checker made up. *)
let name_span (sp : Span.t) name =
  match text_of sp with
  | None -> None
  | Some t ->
    let n = String.length t in
    let len = String.length name in
    if sp.lo < 0 || sp.lo + len > n then None
    else if sp.hi - sp.lo = len && String.sub t sp.lo len = name then Some sp
    else begin
      let rec seg i found =
        let e = ref i in
        while !e < n && is_ident_char t.[!e] do incr e done;
        let found = if !e - i = len && String.sub t i len = name then Some { sp with lo = i; hi = !e } else found in
        if !e < n - 1 && t.[!e] = '.' && is_ident_char t.[!e + 1] then seg (!e + 1) found else found
      in
      seg sp.lo None
    end

(* The first whole-word [w] in [sp]. *)
let find_word (sp : Span.t) w =
  match text_of sp with
  | None -> None
  | Some t ->
    let len = String.length w in
    let rec go i =
      if i + len > min sp.hi (String.length t) then None
      else if String.sub t i len = w && (i = 0 || not (is_ident_char t.[i - 1])) && (i + len >= String.length t || not (is_ident_char t.[i + len]))
      then Some { sp with Span.lo = i; hi = i + len }
      else go (i + 1)
    in
    go (max 0 sp.lo)

let index (items : item list) : occ list =
  let acc = ref [] in
  let add ?(def = false) ?(short = Plain) key name sp =
    if name <> "" && not (String.contains name '$') then
      match name_span sp name with Some sp -> acc := { key; name; sp; def; short } :: !acc | None -> ()
  in
  let fns = Hashtbl.create 64 and types = Hashtbl.create 32 and enums = ref [] and structs = Hashtbl.create 16 in
  List.iter
    (function
      | IFn d when d.fn_test = None -> Hashtbl.replace fns d.fn_name ()
      | IFn _ -> ()
      | IStruct s -> Hashtbl.replace types s.st_name (); Hashtbl.replace structs s.st_name s
      | IEnum e -> Hashtbl.replace types e.en_name (); enums := e :: !enums
      | IIface i -> Hashtbl.replace types i.if_name ())
    items;
  let enum_with_variant file v =
    match List.filter (fun e -> List.exists (fun (n, _, _) -> n = v) e.en_variants) !enums with
    | [ e ] -> Some e.en_name
    | [] -> None
    | es -> (
      (* several enums share the name: the one in the same file *)
      match List.find_opt (fun e -> e.en_span.Span.file = file) es with Some e -> Some e.en_name | None -> None)
  in
  let rec enum_of_ty t =
    match Types.repr t with
    | Types.TNamed (n, _) when List.exists (fun e -> e.en_name = n) !enums -> Some n
    | Types.TFn (_, r) -> enum_of_ty r
    | _ -> None
  in
  let struct_of t = match Types.repr t with Types.TNamed (n, _) when Hashtbl.mem structs n -> Some n | _ -> None in
  let local (sp : Span.t) = "l:" ^ sp.file ^ "#" ^ string_of_int sp.lo in
  let rec ty (te : ty_expr) =
    match te.tdesc with
    | TyName (n, args) ->
      if Hashtbl.mem types n then add ("t:" ^ n) (bare n) te.tspan;
      List.iter ty args
    | TyFn (ps, r) -> List.iter ty ps; ty r
    | TyUnit | TyInt _ -> ()
  in
  let type_ref n sp = if Hashtbl.mem types n then add ("t:" ^ n) (bare n) sp in
  let rec pat env (p : pat) =
    match p.pdesc with
    | PBind n -> add ~def:true (local p.pspan) n p.pspan; (n, local p.pspan) :: env
    | PVariant (q, v, args) ->
      let en = match q with Some q when Hashtbl.mem types q -> Some q | _ -> enum_with_variant p.pspan.file v in
      Option.iter (fun en -> add ("v:" ^ en ^ "#" ^ v) v p.pspan) en;
      (match q with Some q -> type_ref q p.pspan | None -> ());
      List.fold_left pat env args
    | PTuple ps -> List.fold_left pat env ps
    | _ -> env
  in
  let rec expr env e =
    let x = expr env in
    match e.edesc with
    | EVar n -> (
      match List.assoc_opt n env with
      | Some k -> add k n e.espan
      | None ->
        if Hashtbl.mem fns n then add ("f:" ^ n) (bare n) e.espan
        else if Hashtbl.mem types n then type_ref n e.espan
        else (match enum_of_ty e.ety with Some en -> add ("v:" ^ en ^ "#" ^ n) n e.espan | None -> ()))
    | EInt _ | EFloat _ | EBool _ | EUnit -> ()
    | EStr ps -> List.iter (function SPExpr y -> x y | SPLit _ -> ()) ps
    | EList es -> List.iter x es
    | EBin (_, a, b) | EIndex (a, b) -> x a; x b
    | EUn (_, a) | ETry a -> x a
    | (EField ({ edesc = EVar en; espan; _ }, v, nsp) | EMethod ({ edesc = EVar en; espan; _ }, v, nsp, _))
      when (not (List.mem_assoc en env)) && List.exists (fun d -> d.en_name = en) !enums ->
      type_ref en espan;
      add ("v:" ^ en ^ "#" ^ v) v nsp;
      (match e.edesc with EMethod (_, _, _, args) -> List.iter x args | _ -> ())
    | EField (r, name, nsp) ->
      x r;
      (match struct_of r.ety with Some s -> add ("d:" ^ s ^ "#" ^ name) name nsp | None -> ())
    | ECall (c, args) -> x c; List.iter x args
    | EMethod (r, name, nsp, args) ->
      x r;
      (match Types.repr r.ety with
       | Types.TDyn i -> add ("m:" ^ i ^ "#" ^ name) name nsp
       | Types.TNamed (n, _) when Hashtbl.mem fns (n ^ "::" ^ name) -> add ("f:" ^ n ^ "::" ^ name) name nsp
       | _ -> ());
      List.iter x args
    | EIf (c, b, eo) -> x c; block env b; Option.iter x eo
    | EMatch (s, arms) -> x s; List.iter (fun a -> expr (pat env a.apat) a.abody) arms
    | EBlock b -> block env b
    | EGroup (n, until, b) ->
      Option.iter (fun (c, ms) -> x c; x ms) until;
      let env =
        match find_word { e.espan with hi = b.bspan.lo } n with
        | Some sp -> add ~def:true (local sp) n sp; (n, local sp) :: env
        | None -> env
      in
      block env b
    | EClosure (ps, body) ->
      List.iter (fun (_, t, _) -> Option.iter ty t) ps;
      let env = List.fold_left (fun env (n, _, sp) -> add ~def:true (local sp) n sp; (n, local sp) :: env) env ps in
      expr env body
    | EStruct (name, nsp, fs) ->
      type_ref name nsp;
      List.iter
        (fun (f, (fsp : Span.t), v) ->
          match v.edesc with
          | EVar n when n = f && v.espan = fsp ->
            (* `S { f }` is `S { f: f }`: one name, a field and a local *)
            add ~short:Field_side ("d:" ^ name ^ "#" ^ f) f fsp;
            (match List.assoc_opt n env with
             | Some k -> add ~short:(Var_side f) k n fsp
             | None -> if Hashtbl.mem fns n then add ~short:(Var_side f) ("f:" ^ n) n fsp)
          | _ -> add ("d:" ^ name ^ "#" ^ f) f fsp; x v)
        fs
    | EDyn (y, _) -> x y
    | ESelect arms ->
      List.iter
        (fun a ->
          Option.iter x a.sguard;
          match a.sop with
          | SelRecv (b, c) ->
            x c;
            let env = match b with Some (n, sp) -> add ~def:true (local sp) n sp; (n, local sp) :: env | None -> env in
            expr env a.sbody
          | SelSend (c, v) | SelAfter (c, v) -> x c; x v; x a.sbody
          | SelDefault -> x a.sbody)
        arms
  and block env b =
    let env =
      List.fold_left
        (fun env s ->
          match s.sdesc with
          | SLet (_, n, nsp, t, e) -> Option.iter ty t; expr env e; add ~def:true (local nsp) n nsp; (n, local nsp) :: env
          | SExpr e -> expr env e; env
          | SAssign (a, v) -> expr env a; expr env v; env
          | SReturn eo -> Option.iter (expr env) eo; env
          | SWhile (c, b) -> expr env c; block env b; env
          | SFor (n, nsp, e, b) ->
            expr env e;
            add ~def:true (local nsp) n nsp;
            block ((n, local nsp) :: env) b;
            env
          | SLetPat (p, e) -> expr env e; pat env p
          | SForPat (p, e, b) -> expr env e; block (pat env p) b; env
          | SBreak | SContinue -> env)
        env b.stmts
    in
    Option.iter (expr env) b.tail
  in
  List.iter
    (function
      | IFn d ->
        if d.fn_test = None then add ~def:true ("f:" ^ d.fn_name) (bare d.fn_name) d.fn_span;
        List.iter (fun (_, bs) -> List.iter (fun (b, sp) -> type_ref b sp) bs) d.fn_bounds;
        let env =
          List.map
            (fun p ->
              ty p.prm_ty;
              add ~def:true (local p.prm_span) p.prm_name p.prm_span;
              (p.prm_name, local p.prm_span))
            d.fn_params
        in
        Option.iter ty d.fn_ret;
        block env d.fn_body
      | IStruct s ->
        add ~def:true ("t:" ^ s.st_name) (bare s.st_name) s.st_span;
        List.iter (fun (f, sp, te) -> add ~def:true ("d:" ^ s.st_name ^ "#" ^ f) f sp; ty te) s.st_fields
      | IEnum e ->
        add ~def:true ("t:" ^ e.en_name) (bare e.en_name) e.en_span;
        List.iter (fun (v, sp, tes) -> add ~def:true ("v:" ^ e.en_name ^ "#" ^ v) v sp; List.iter ty tes) e.en_variants
      | IIface i ->
        add ~def:true ("t:" ^ i.if_name) (bare i.if_name) i.if_span;
        List.iter
          (fun (m, sp, ps, r) -> add ~def:true ("m:" ^ i.if_name ^ "#" ^ m) m sp; List.iter (fun p -> ty p.prm_ty) ps; Option.iter ty r)
          i.if_methods)
    items;
  (* one occurrence per place (method values and shorthand can repeat a node) *)
  let seen = Hashtbl.create 256 in
  List.filter
    (fun o ->
      let k = (o.key, o.sp.Span.file, o.sp.lo) in
      if Hashtbl.mem seen k then false else (Hashtbl.replace seen k (); true))
    (List.rev !acc)

(* The occurrence under [off] in [file]. A shorthand `S { f }` is both a field
   and a local; [prefer_var] picks the local. *)
let at ?(prefer_var = false) occs file off =
  let here = List.filter (fun o -> o.sp.Span.file = file && o.sp.lo <= off && off <= o.sp.hi) occs in
  match here with
  | [] -> None
  | [ o ] -> Some o
  | o :: _ as l -> (
    let want = if prefer_var then function Var_side _ -> true | _ -> false else function Field_side -> true | _ -> false in
    match List.find_opt (fun o -> want o.short) l with Some o -> Some o | None -> Some o)

let references occs key = List.filter (fun o -> o.key = key) occs
let definition occs key = List.find_opt (fun o -> o.key = key && o.def) occs

(* ---------- rename ---------- *)

let keywords =
  [ "fn"; "let"; "mut"; "if"; "else"; "match"; "for"; "in"; "while"; "return"; "struct"; "enum"; "true"; "false"; "group";
    "break"; "continue"; "pub"; "use"; "interface"; "dyn"; "select"; "test"; "until"; "self" ]

let valid_ident s =
  s <> "" && (not (s.[0] >= '0' && s.[0] <= '9')) && String.for_all is_ident_char s && not (List.mem s keywords)

type edit = { at : Span.t; replacement : string }

(* What [o] becomes when its symbol is renamed to [nw]. *)
let edit_for nw o =
  match o.short with
  | Plain -> { at = o.sp; replacement = nw }
  | Field_side -> { at = o.sp; replacement = nw ^ ": " ^ o.name }
  | Var_side f -> { at = o.sp; replacement = f ^ ": " ^ nw }

let apply_edits text edits =
  let edits = List.sort (fun a b -> compare a.at.Span.lo b.at.Span.lo) edits in
  let b = Buffer.create (String.length text + 64) in
  let pos = List.fold_left (fun pos e -> Buffer.add_string b (String.sub text pos (e.at.lo - pos)); Buffer.add_string b e.replacement; e.at.hi) 0 edits in
  Buffer.add_string b (String.sub text pos (String.length text - pos));
  Buffer.contents b

(* Where the renamed name sits after the edits: the start of its text. *)
let new_position edits (o : occ) nw =
  let shift = List.fold_left (fun s e -> if e.at.Span.file = o.sp.file && e.at.hi <= o.sp.lo then s + String.length e.replacement - (e.at.hi - e.at.lo) else s) 0 edits in
  let lo = o.sp.lo + shift in
  ignore nw;
  match o.short with Var_side f -> lo + String.length f + 2 | _ -> lo

(* Rename the symbol at [off] in [file] to [nw]. [load] type-checks the
   program with some files' text replaced and returns its items and
   diagnostics. The rename is refused unless, after it, the program checks
   with no new errors and the new name refers to exactly the renamed places. *)
let rename ~(load : (string * string) list -> item list * Diag.t list) items file off nw : ((string * edit list) list, string) result =
  let occs = index items in
  match at occs file off with
  | None -> Error "there is no name to rename here"
  | Some o -> (
    let kind = o.key.[0] in
    match definition occs o.key with
    | None -> Error (Printf.sprintf "`%s` is built in; only names defined in the program can be renamed" o.name)
    | Some d when String.starts_with ~prefix:"<std>" d.sp.file -> Error (Printf.sprintf "`%s` is defined in the standard library" o.name)
    | Some d ->
      let upper = nw <> "" && nw.[0] >= 'A' && nw.[0] <= 'Z' in
      let iface_methods =
        List.concat_map (function IIface i -> List.map (fun (m, _, _, _) -> (m, i.if_name)) i.if_methods | _ -> []) items
      in
      if not (valid_ident nw) then Error (Printf.sprintf "`%s` is not a valid name" nw)
      else if nw = o.name then Ok []
      else if (kind = 't' || kind = 'v') && not upper then Error (Printf.sprintf "type and variant names start with an uppercase letter; `%s` does not" nw)
      else if (kind = 'f' || kind = 'l' || kind = 'd') && upper then Error (Printf.sprintf "`%s` starts with an uppercase letter, which patterns read as a variant" nw)
      else if kind = 'm' then Error (Printf.sprintf "`%s` is an interface method; renaming it means renaming every type's method too" o.name)
      else if kind = 'f' && o.key = "f:main" then Error "`main` is the program's entry point"
      else if kind = 'f' && Interp_util.find_sub o.key "::" 0 <> None && List.mem_assoc o.name iface_methods then
        Error (Printf.sprintf "method `%s` may satisfy interface `%s`; renaming it could break that" o.name (List.assoc o.name iface_methods))
      else begin
        let refs = references occs o.key in
        let edits = List.map (edit_for nw) refs in
        let files = List.sort_uniq compare (List.map (fun e -> e.at.Span.file) edits) in
        match List.find_opt (fun f -> String.starts_with ~prefix:"<std>" f) files with
        | Some f -> Error ("the rename would change " ^ f)
        | None -> (
          let texts = List.map (fun f -> (f, Option.value ~default:"" (Source.text f))) files in
          let _, before = load [] in
          let changed = List.map (fun (f, t) -> (f, apply_edits t (List.filter (fun e -> e.at.Span.file = f) edits))) texts in
          let after_items, after = load changed in
          let key (dg : Diag.t) = dg.code ^ " " ^ dg.message in
          let before_keys = List.map key before in
          let fresh = List.filter (fun dg -> not (List.mem (key dg) before_keys)) after in
          let result =
          match fresh with
          | dg :: _ -> Error (Printf.sprintf "renaming `%s` to `%s` would cause %s: %s" o.name nw dg.code dg.message)
          | [] when after_items = [] -> Error (Printf.sprintf "renaming `%s` to `%s` breaks the program" o.name nw)
          | [] ->
            begin
              (* the new name must refer to exactly the renamed places, no more and no fewer *)
              let new_occs = index after_items in
              let def_pos = new_position edits d nw in
              let want = List.sort compare (List.map (fun r -> (r.sp.Span.file, new_position edits r nw)) refs) in
              let got =
                match List.find_opt (fun n -> n.def && n.sp.Span.file = d.sp.file && n.sp.lo = def_pos) new_occs with
                | Some nd -> List.sort compare (List.map (fun r -> (r.sp.Span.file, r.sp.lo)) (references new_occs nd.key))
                | None -> []
              in
              if got <> want then Error (Printf.sprintf "renaming `%s` to `%s` would change what some names refer to (`%s` is already used nearby)" o.name nw nw)
              else Ok (List.map (fun f -> (f, List.filter (fun e -> e.at.Span.file = f) edits)) files)
            end
          in
          (* back to the program as it is, for the caller's later lookups *)
          ignore (load []);
          result)
      end)
