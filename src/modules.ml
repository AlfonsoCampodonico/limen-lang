(* Modules: `use NAME` loads NAME.vela (next to the importing file, then a
   vela.toml dependency, then the standard library) and merges everything into
   one flat program. Every item of module M is renamed `M.item`; the main file
   keeps its names. References are rewritten scope-aware, so the checker and
   both backends see a single program with qualified names.

   Inside module M, an unqualified name that is one of M's items means that
   item. `m.f(...)`, `m.f`, `m.Type`, `m.Type { ... }`, `m.Variant(...)` and the
   pattern `m.Variant(...)` refer to module m, which the file must `use`. Only
   `pub` items can be used from another module. *)
open Ast

type modinfo = {
  mname : string; (* "" for the main file *)
  mfile : string;
  mitems : item list;
  muses : (string * span) list;
  mpkg : string option; (* the vela.toml dependency it belongs to *)
}

(* module name -> dependency, for the last program loaded *)
let module_pkg : (string * string) list ref = ref []

(* the standard library modules in the last program loaded *)
let std_loaded : string list ref = ref []

let std_modules : (string * string) list ref = ref Std_text.modules (* name -> source *)

(* A method is a function whose first parameter is `self`; it is named
   `Type::name` and called as `value.name(...)`, never by its bare name. *)
let is_method = function
  | IFn { fn_test = None; fn_params = { prm_name = "self"; _ } :: _; _ } -> true
  | _ -> false

let item_name = function IFn d -> d.fn_name | IStruct s -> s.st_name | IEnum e -> e.en_name | IIface i -> i.if_name
let item_span = function IFn d -> d.fn_span | IStruct s -> s.st_span | IEnum e -> e.en_span | IIface i -> i.if_span
let item_pub = function IFn d -> d.fn_pub | IStruct s -> s.st_pub | IEnum e -> e.en_pub | IIface i -> i.if_pub

let errors : Diag.t list ref = ref []
let err ?(notes = []) code sp msg = errors := Diag.make ~notes code sp msg :: !errors

(* ---------- loading ---------- *)

(* Text to use instead of a file on disk, by path: the editor's unsaved
   buffers, or a rename being tried out. *)
let overlay : (string * string) list ref = ref []

let read_file path =
  match List.assoc_opt path !overlay with
  | Some t -> Some t
  | None -> try Some (In_channel.with_open_bin path In_channel.input_all) with Sys_error _ -> None

(* deps: name -> directory holding NAME.vela (from vela.toml) *)
(* Returns the path, the text, and whether it came from a dependency. *)
let same_file a b = try Unix.realpath a = Unix.realpath b with Unix.Unix_error _ -> false

let find_module ~deps ~from dir name =
  let local = Filename.concat dir (name ^ ".vela") in
  (* a file never imports itself: `use http` in http.vela means another http *)
  if dir <> "" && (Sys.file_exists local || List.mem_assoc local !overlay) && not (same_file local from) then Some (local, read_file local, false)
  else
    match List.assoc_opt name deps with
    | Some d ->
      let p = Filename.concat d (name ^ ".vela") in
      Some (p, read_file p, true)
    | None -> (
      match List.assoc_opt name !std_modules with
      | Some src -> Some ("<std>/" ^ name ^ ".vela", Some src, false)
      | None -> None)

let parse path src =
  Source.add path src;
  Parser.parse_file path src

(* ---------- renaming ---------- *)

type env = {
  self : modinfo;
  mods : (string, modinfo) Hashtbl.t;
  own : (string, item) Hashtbl.t; (* this module's items by bare name *)
  gens : string list; (* type parameters in scope *)
}

(* Built-in types a file can add methods to. *)
let builtin_method_types = [ "Int"; "Float"; "Bool"; "Str"; "Bytes"; "List"; "Map"; "Option"; "Result"; "IoError" ]

let qual (m : modinfo) n = if m.mname = "" then n else m.mname ^ "." ^ n

let module_of env a sp =
  if List.exists (fun (u, _) -> u = a) env.self.muses then Hashtbl.find_opt env.mods a
  else begin
    ignore sp;
    None
  end

let is_module env locals a = (not (List.mem a locals)) && List.exists (fun (u, _) -> u = a) env.self.muses

(* Look up item [n] of module [m] from another module, checking it is pub. *)
let member _env (m : modinfo) n sp =
  match List.find_opt (fun i -> item_name i = n && not (is_method i)) m.mitems with
  | Some i ->
    if not (item_pub i) then err "E0402" sp (Printf.sprintf "`%s` is private to module `%s`" n m.mname)
        ~notes:[ Printf.sprintf "mark it `pub fn`, `pub struct` or `pub enum` in %s to export it" m.mfile ];
    Some i
  | None -> None

(* A pub enum of [m] that has variant [v]. *)
let variant_enum (m : modinfo) v =
  List.find_map
    (function IEnum e when List.exists (fun (n, _, _) -> n = v) e.en_variants -> Some e | _ -> None)
    m.mitems

let no_member (m : modinfo) n sp =
  let names = List.filter_map (fun i -> if item_pub i && not (is_method i) then Some (item_name i) else None) m.mitems in
  err "E0403" sp (Printf.sprintf "module `%s` has no `%s`" m.mname n)
    ~notes:(if names = [] then [] else [ "it exports: " ^ String.concat ", " (List.sort compare names) ])

let rec ty env (t : ty_expr) : ty_expr =
  match t.tdesc with
  | TyName (n, args) ->
    let args = List.map (ty env) args in
    let n =
      match String.index_opt n '.' with
      | Some i -> (
        let a = String.sub n 0 i and b = String.sub n (i + 1) (String.length n - i - 1) in
        match module_of env a t.tspan with
        | Some m -> (
          match member env m b t.tspan with
          | Some (IStruct _ | IEnum _ | IIface _) -> qual m b
          | _ -> no_member m b t.tspan; n)
        | None -> err "E0401" t.tspan (Printf.sprintf "unknown module `%s`; add `use %s` at the top of the file" a a); n)
      | None -> (
        if List.mem n env.gens then n
        else match Hashtbl.find_opt env.own n with Some (IStruct _ | IEnum _ | IIface _) -> qual env.self n | _ -> n)
    in
    { t with tdesc = TyName (n, args) }
  | TyFn (ps, r) -> { t with tdesc = TyFn (List.map (ty env) ps, ty env r) }
  | TyUnit | TyInt _ -> t

let mk d sp = { edesc = d; espan = sp; ety = Types.TErr; einst = [] }

let rec pat_binds (p : pat) acc =
  match p.pdesc with
  | PBind n -> n :: acc
  | PVariant (_, _, ps) | PTuple ps -> List.fold_left (fun acc p -> pat_binds p acc) acc ps
  | _ -> acc

let rec pat env locals (p : pat) : pat =
  match p.pdesc with
  | PVariant (Some q, v, ps) ->
    let ps = List.map (pat env locals) ps in
    let q =
      if is_module env locals q then
        match Hashtbl.find_opt env.mods q with
        | Some m -> (
          match variant_enum m v with
          | Some e ->
            ignore (member env m e.en_name p.pspan);
            Some (qual m e.en_name)
          | None -> no_member m v p.pspan; Some q)
        | None -> Some q
      else
        match Hashtbl.find_opt env.own q with Some (IEnum _) -> Some (qual env.self q) | _ -> Some q
    in
    { p with pdesc = PVariant (q, v, ps) }
  | PVariant (None, v, ps) -> { p with pdesc = PVariant (None, v, List.map (pat env locals) ps) }
  | PTuple ps -> { p with pdesc = PTuple (List.map (pat env locals) ps) }
  | _ -> p

let rec expr env locals (e : expr) : expr =
  let ex = expr env locals in
  let d =
    match e.edesc with
    | EInt _ | EFloat _ | EBool _ | EUnit -> e.edesc
    | EStr parts -> EStr (List.map (function SPLit s -> SPLit s | SPExpr x -> SPExpr (ex x)) parts)
    | EVar n ->
      if List.mem n locals then e.edesc
      else (
        match Hashtbl.find_opt env.own n with
        | Some (IFn _) -> EVar (qual env.self n)
        | _ -> if is_module env locals n then (err "E0404" e.espan (Printf.sprintf "module `%s` is not a value; use one of its items, such as `%s.name`" n n); e.edesc) else e.edesc)
    | EList es -> EList (List.map ex es)
    | EPipe (x, rhs) -> (
      (* `x |> f(a)` is `f(x, a)` from here on *)
      match pipe_call x rhs with
      | Some d -> (ex { e with edesc = d }).edesc
      | None -> e.edesc (* the parser rejects it *))
    | EBin (op, a, b) -> EBin (op, ex a, ex b)
    | EUn (op, a) -> EUn (op, ex a)
    | ECall (c, args) -> ECall (ex c, List.map ex args)
    | EMethod ({ edesc = EVar a; espan = asp; _ }, name, nsp, args) when is_module env locals a -> (
      let args = List.map ex args in
      match Hashtbl.find_opt env.mods a with
      | None -> e.edesc
      | Some m -> (
        let sp = Span.merge asp nsp in
        match member env m name nsp with
        | Some (IFn _) -> ECall (mk (EVar (qual m name)) sp, args)
        | Some _ -> err "E0108" sp (Printf.sprintf "`%s.%s` is a type, not a function" a name); e.edesc
        | None -> (
          match variant_enum m name with
          | Some en ->
            ignore (member env m en.en_name nsp);
            EMethod (mk (EVar (qual m en.en_name)) asp, name, nsp, args)
          | None -> no_member m name nsp; e.edesc)))
    | EMethod (({ edesc = EVar en; _ } as r), name, nsp, args)
      when (not (List.mem en locals)) && (match Hashtbl.find_opt env.own en with Some (IEnum _) -> true | _ -> false) ->
      EMethod ({ r with edesc = EVar (qual env.self en) }, name, nsp, List.map ex args)
    | EMethod (r, name, nsp, args) -> EMethod (ex r, name, nsp, List.map ex args)
    | EField ({ edesc = EVar a; espan = asp; _ }, name, nsp) when is_module env locals a -> (
      match Hashtbl.find_opt env.mods a with
      | None -> e.edesc
      | Some m -> (
        match member env m name nsp with
        | Some (IFn _) -> EVar (qual m name)
        | Some _ -> err "E0404" (Span.merge asp nsp) (Printf.sprintf "`%s.%s` is a type, not a value" a name); e.edesc
        | None -> (
          match variant_enum m name with
          | Some en ->
            ignore (member env m en.en_name nsp);
            EField (mk (EVar (qual m en.en_name)) asp, name, nsp)
          | None -> no_member m name nsp; e.edesc)))
    | EField (({ edesc = EVar en; _ } as r), name, nsp)
      when (not (List.mem en locals)) && (match Hashtbl.find_opt env.own en with Some (IEnum _) -> true | _ -> false) ->
      EField ({ r with edesc = EVar (qual env.self en) }, name, nsp)
    | EField (r, name, nsp) -> EField (ex r, name, nsp)
    | EIndex (a, i) -> EIndex (ex a, ex i)
    | ETry (a, c) -> ETry (ex a, Option.map ex c)
    | EIf (c, b, el) -> EIf (ex c, block env locals b, Option.map ex el)
    | EMatch (s, arms) ->
      EMatch
        ( ex s,
          List.map
            (fun a ->
              let inner = pat_binds a.apat locals in
              { apat = pat env locals a.apat; aguard = Option.map (expr env inner) a.aguard; abody = expr env inner a.abody })
            arms )
    | EBlock b -> EBlock (block env locals b)
    | EClosure (ps, body) ->
      let locals' = List.fold_left (fun acc (n, _, _) -> n :: acc) locals ps in
      EClosure (List.map (fun (n, t, sp) -> (n, Option.map (ty env) t, sp)) ps, expr env locals' body)
    | EStruct (n, sp, fields) ->
      let n =
        match String.index_opt n '.' with
        | Some i -> (
          let a = String.sub n 0 i and b = String.sub n (i + 1) (String.length n - i - 1) in
          match if List.mem a locals then None else module_of env a sp with
          | Some m -> (
            match member env m b sp with
            | Some (IStruct _) -> qual m b
            | _ -> no_member m b sp; n)
          | None -> err "E0401" sp (Printf.sprintf "unknown module `%s`; add `use %s` at the top of the file" a a); n)
        | None -> ( match Hashtbl.find_opt env.own n with Some (IStruct _) -> qual env.self n | _ -> n)
      in
      EStruct (n, sp, List.map (fun (f, fsp, x) -> (f, fsp, ex x)) fields)
    | EGroup (g, until, b) -> EGroup (g, Option.map (fun (c, ms) -> (ex c, ex ms)) until, block env (g :: locals) b)
    | EWith (b, fields) -> EWith (ex b, List.map (fun (f, fsp, x) -> (f, fsp, ex x)) fields)
    | ESafe (b, x, rest, flat) -> ESafe (ex b, x, expr env (x :: locals) rest, flat)
    | EDyn (x, i) -> EDyn (ex x, i)
    | ESelect arms ->
      ESelect
        (List.map
           (fun a ->
             let a = { a with sguard = Option.map ex a.sguard } in
             match a.sop with
             | SelRecv (b, c) ->
               let locals' = match b with Some (n, _) -> n :: locals | None -> locals in
               { a with sop = SelRecv (b, ex c); sbody = expr env locals' a.sbody }
             | SelSend (c, v) -> { a with sop = SelSend (ex c, ex v); sbody = ex a.sbody }
             | SelAfter (c, ms) -> { a with sop = SelAfter (ex c, ex ms); sbody = ex a.sbody }
             | SelDefault -> { a with sbody = ex a.sbody })
           arms)
  in
  { e with edesc = d }

(* `let (a, (b, _)) = e` becomes `let t = e; let a = t.0; let u = t.1; let b = u.0`,
   and `for (k, v) in xs { body }` becomes `for t in xs { let (k, v) = t; body }`. *)
and unpack_counter = ref 0

and unpack (p : pat) (e : expr) : stmt list =
  incr unpack_counter;
  let tmp = "tuple$" ^ string_of_int !unpack_counter in
  let mk d = { edesc = d; espan = p.pspan; ety = Types.TErr; einst = [] } in
  let first = { sdesc = SLet (false, tmp, p.pspan, None, e); sspan = p.pspan } in
  let rest =
    match p.pdesc with
    | PTuple ps ->
      List.concat
        (List.mapi
           (fun i (sub : pat) ->
             let field = mk (EField (mk (EVar tmp), string_of_int i, sub.pspan)) in
             match sub.pdesc with
             | PBind n -> [ { sdesc = SLet (false, n, sub.pspan, None, field); sspan = sub.pspan } ]
             | PWild -> []
             | _ -> unpack sub field)
           ps)
    | PBind n -> [ { sdesc = SLet (false, n, p.pspan, None, mk (EVar tmp)); sspan = p.pspan } ]
    | _ -> []
  in
  first :: rest

and expand_stmts (ss : stmt list) : stmt list =
  List.concat_map
    (fun (s : stmt) ->
      match s.sdesc with
      | SLetPat (p, e) -> unpack p e
      | SForPat (p, it, body) ->
        incr unpack_counter;
        let tmp = "item$" ^ string_of_int !unpack_counter in
        let v = { edesc = EVar tmp; espan = p.pspan; ety = Types.TErr; einst = [] } in
        [ { s with sdesc = SFor (tmp, p.pspan, it, { body with stmts = unpack p v @ body.stmts }) } ]
      | _ -> [ s ])
    ss

and block env locals (b : block) : block =
  let b = { b with stmts = expand_stmts b.stmts } in
  let locals = ref locals in
  let stmts =
    List.map
      (fun (s : stmt) ->
        let d =
          match s.sdesc with
          | SLet (m, n, sp, t, x) ->
            let x = expr env !locals x in
            locals := n :: !locals;
            SLet (m, n, sp, Option.map (ty env) t, x)
          | SAssign (a, b) -> SAssign (expr env !locals a, expr env !locals b)
          | SExpr x -> SExpr (expr env !locals x)
          | SReturn x -> SReturn (Option.map (expr env !locals) x)
          | SWhile (c, body) -> SWhile (expr env !locals c, block env !locals body)
          | SFor (n, sp, it, body) -> SFor (n, sp, expr env !locals it, block env (n :: !locals) body)
          | SBreak | SContinue -> s.sdesc
          | SDefer body -> SDefer (block env !locals body)
          | SLetPat _ | SForPat _ -> s.sdesc (* already expanded *)
        in
        { s with sdesc = d })
      b.stmts
  in
  { b with stmts; tail = Option.map (expr env !locals) b.tail }

let rename_item mods (m : modinfo) own (it : item) : item =
  match it with
  | IFn d ->
    let env = { self = m; mods; own; gens = d.fn_gen } in
    let locals = List.map (fun p -> p.prm_name) d.fn_params in
    let params = List.map (fun p -> { p with prm_ty = ty env p.prm_ty }) d.fn_params in
    let name =
      if d.fn_test <> None then d.fn_name
      else if is_method it then
        match params with
        | { prm_ty = { tdesc = TyName (tn, _); _ }; _ } :: _ when List.mem tn builtin_method_types ->
          (* a method on a built-in type belongs to its file: keyed by module *)
          qual m (tn ^ "::" ^ d.fn_name)
        | { prm_ty = { tdesc = TyName (tn, _); _ }; _ } :: _ -> tn ^ "::" ^ d.fn_name
        | _ -> qual m d.fn_name (* the checker reports the bad `self` *)
      else qual m d.fn_name
    in
    (* bounds name interfaces, qualified like types *)
    let bound (b, sp) =
      match String.index_opt b '.' with
      | Some i -> (
        let a = String.sub b 0 i and n = String.sub b (i + 1) (String.length b - i - 1) in
        match module_of env a sp with
        | Some md -> (
          match member env md n sp with
          | Some (IIface _) -> (qual md n, sp)
          | _ -> no_member md n sp; (b, sp))
        | None -> err "E0401" sp (Printf.sprintf "unknown module `%s`; add `use %s` at the top of the file" a a); (b, sp))
      | None -> ( match Hashtbl.find_opt own b with Some (IIface _) -> (qual m b, sp) | _ -> (b, sp))
    in
    IFn
      { d with
        fn_name = name;
        fn_bounds = List.map (fun (g, bs) -> (g, List.map bound bs)) d.fn_bounds;
        fn_params = params;
        fn_ret = Option.map (ty env) d.fn_ret;
        fn_body = block env locals d.fn_body;
        (* a standard library module trusted with C (src/std/vela.toml [trust]) *)
        fn_foreign =
          Option.map
            (fun (fo : foreign) ->
              { fo with ftrusted = String.starts_with ~prefix:"<std>" m.mfile && List.mem m.mname (Lazy.force Package.std_manifest).trust })
            d.fn_foreign }
  | IStruct s ->
    let env = { self = m; mods; own; gens = s.st_gen } in
    IStruct { s with st_name = qual m s.st_name; st_fields = List.map (fun (f, sp, t) -> (f, sp, ty env t)) s.st_fields }
  | IIface i ->
    let env = { self = m; mods; own; gens = [] } in
    IIface
      { i with
        if_name = qual m i.if_name;
        if_methods =
          List.map
            (fun (n, sp, ps, r) -> (n, sp, List.map (fun p -> { p with prm_ty = ty env p.prm_ty }) ps, Option.map (ty env) r))
            i.if_methods }
  | IEnum e ->
    let env = { self = m; mods; own; gens = e.en_gen } in
    IEnum
      { e with en_name = qual m e.en_name; en_variants = List.map (fun (v, sp, ts) -> (v, sp, List.map (ty env) ts)) e.en_variants }

(* A table test, `test "add" for (a, b, want) in [(1, 2, 3), (2, 2, 4)] { body }`,
   becomes one test per case, named by the case's text ("add (1, 2, 3)"),
   whose body starts with `let (a, b, want) = (1, 2, 3)`. Each copy is renamed
   on its own afterwards, which gives every copy fresh expression nodes. A
   bench's value is passed to `keep`. *)
let expand_table (it : item) : item list =
  match it with
  | IFn ({ fn_table = Some (p, { edesc = EList cases; _ }); fn_test = Some base; _ } as d) ->
    List.mapi
      (fun i (c : expr) ->
        let text = Source.one_line c.espan in
        let name = if base = "" then text else base ^ " " ^ text in
        let unpack = { sdesc = SLetPat (p, c); sspan = c.espan } in
        IFn
          { d with
            fn_name = Printf.sprintf "%s/%d" d.fn_name i;
            fn_test = Some name;
            fn_table = Some (p, c); (* the one case this copy runs, for the checker's notes *)
            fn_body = { d.fn_body with stmts = unpack :: d.fn_body.stmts } })
      cases
  | IFn ({ fn_bench = true; fn_body = { tail = Some e; _ }; _ } as d) ->
    (* a benchmark's value is kept, so the work that made it is not optimized away *)
    let keep = mk (ECall (mk (EVar "keep") e.espan, [ e ])) e.espan in
    [ IFn { d with fn_body = { d.fn_body with tail = Some keep } } ]
  | _ -> [ it ]

(* ---------- entry point ---------- *)

(* Load the main file at [path] (whose text is [src]) and every module it uses.
   Returns the flat program, or the diagnostics. *)
let load ?(deps = []) path src : item list * Diag.t list =
  errors := [];
  module_pkg := [];
  std_loaded := [];
  let mods : (string, modinfo) Hashtbl.t = Hashtbl.create 8 in
  match parse path src with
  | exception Diag.Fatal d -> ([], [ d ])
  | f -> (
    let main = { mname = ""; mfile = path; mitems = f.items; muses = f.uses; mpkg = None } in
    let rec visit (m : modinfo) =
      let dir = if String.starts_with ~prefix:"<std>" m.mfile then "" else Filename.dirname m.mfile in
      List.iter
        (fun (name, sp) ->
          if not (Hashtbl.mem mods name) then
            match find_module ~deps ~from:m.mfile dir name with
            | None ->
              err "E0401" sp (Printf.sprintf "cannot find module `%s`" name)
                ~notes:[ Printf.sprintf "looked for %s, the dependencies in vela.toml and the standard library" (Filename.concat dir (name ^ ".vela")) ]
            | Some (p, None, _) -> err "E0401" sp (Printf.sprintf "cannot read module `%s` at %s" name p)
            | Some (p, Some src, from_dep) ->
              let f = parse p src in
              (* tests and main belong to the program being run, not its modules *)
              let items = List.filter (function IFn { fn_test = Some _; _ } -> false | _ -> true) f.items in
              let mpkg = if from_dep then Some name else if String.starts_with ~prefix:"<std>" p then None else m.mpkg in
              let mi = { mname = name; mfile = p; mitems = items; muses = f.uses; mpkg } in
              Hashtbl.replace mods name mi;
              visit mi)
        m.muses
    in
    match visit main with
    | exception Diag.Fatal d -> ([], [ d ])
    | () ->
      if !errors <> [] then ([], List.rev !errors)
      else begin
        let all = main :: List.of_seq (Hashtbl.to_seq_values mods) in
        let all = main :: List.sort (fun a b -> compare a.mname b.mname) (List.tl all) in
        module_pkg := List.filter_map (fun m -> Option.map (fun p -> (m.mname, p)) m.mpkg) all;
        std_loaded := List.filter_map (fun m -> if String.starts_with ~prefix:"<std>" m.mfile then Some m.mname else None) all;
        let items =
          List.concat_map
            (fun m ->
              let own = Hashtbl.create 16 in
              List.iter (fun i -> if not (is_method i) then Hashtbl.replace own (item_name i) i) m.mitems;
              List.map (rename_item mods m own) (List.concat_map expand_table m.mitems))
            (List.rev all)
        in
        (* methods promoted through struct embedding, so interfaces see them *)
        if !errors <> [] then ([], List.rev !errors) else (items @ Embed.forwarders items, [])
      end)

(* Who calls C: (dependency, libraries) for each dependency whose modules
   declare foreign blocks, from the module each declaration came from. *)
let c_users (items : item list) : (string * string list) list =
  List.fold_left
    (fun acc it ->
      match it with
      | IFn { fn_name; fn_foreign = Some fo; _ } -> (
        match String.index_opt fn_name '.' with
        | Some k -> (
          match List.assoc_opt (String.sub fn_name 0 k) !module_pkg with
          | Some dep ->
            let libs = Option.value ~default:[] (List.assoc_opt dep acc) in
            if List.mem fo.flib libs then acc else (dep, libs @ [ fo.flib ]) :: List.remove_assoc dep acc
          | None -> acc)
        | None -> acc)
      | _ -> acc)
    [] items

(* Load a program with the vela.toml around it (if any), then type-check it
   and enforce the capabilities of each standard library module and each
   dependency. *)
let load_and_check path src : item list * Diag.t list * Check.fn_info list =
  match Package.load path with
  | Error d -> ([], [ d ], [])
  | Ok pkg -> (
    let deps = match pkg with Some p -> List.map (fun (d : Package.dep) -> (d.dname, d.dir)) p.deps | None -> [] in
    match load ~deps path src with
    | [], (_ :: _ as ds) -> ([], ds, [])
    | items, _ ->
      let ds, infos = Check.check_program items in
      (* standard library modules are checked against src/std/vela.toml,
         with or without a vela.toml of the program's own *)
      let warnings = Diag.warnings ds in
      let ds = Diag.errors ds in
      let ds = if ds = [] then Package.enforce ~std:true (Lazy.force Package.std_manifest) (List.map (fun m -> (m, m)) !std_loaded) infos else ds in
      let ds = match pkg with Some p when ds = [] -> Package.enforce p !module_pkg infos | _ -> ds in
      let ds =
        match pkg with
        | Some p when ds = [] -> Package.check_foreign_lock p (c_users items)
        | _ -> ds
      in
      (items, ds @ warnings, infos))
