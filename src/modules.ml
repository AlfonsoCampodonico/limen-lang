(* Modules: `use NAME` loads NAME.lmn (next to the importing file, then a
   limen.toml dependency, then the standard library) and merges everything into
   one flat program. Every item of module M is renamed `M.item`; the main file
   keeps its names. References are rewritten scope-aware, so the checker and
   both backends see a single program with qualified names.

   A standard library module's `use NAME` always means the standard library's
   NAME, whatever the program has. So a program can hold its own module NAME
   (a file or a dependency) and the standard library's at once: the program's
   is `NAME`, and the standard library's is then named `std/NAME`, in its
   items' qualified names, in messages and in `limen caps`.

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
  mpkg : string option; (* the limen.toml dependency it belongs to *)
}

(* module name -> dependency, for the last program loaded *)
let module_pkg : (string * string) list ref = ref []

(* the standard library modules in the last program loaded: the name the
   program gives each (`std/NAME` when the program has a module NAME of its
   own) and its name in the standard library *)
let std_loaded : (string * string) list ref = ref []

let is_std (m : modinfo) = String.starts_with ~prefix:"<std>" m.mfile

(* A standard library module's name in the standard library: its file's. *)
let std_name (m : modinfo) = Filename.remove_extension (Filename.basename m.mfile)

let std_modules : (string * string) list ref = ref Std_text.modules (* name -> source *)

(* A method is a function whose first parameter is `self`; it is named
   `Type::name` and called as `value.name(...)`, never by its bare name. *)
let is_method = function
  | IFn { fn_test = None; fn_params = { prm_name = "self"; _ } :: _; _ } -> true
  | _ -> false

let item_name = function IFn d -> d.fn_name | IStruct s -> s.st_name | IEnum e -> e.en_name | IIface i -> i.if_name | IType a -> a.ty_name
let item_span = function IFn d -> d.fn_span | IStruct s -> s.st_span | IEnum e -> e.en_span | IIface i -> i.if_span | IType a -> a.ty_span
let item_pub = function IFn d -> d.fn_pub | IStruct s -> s.st_pub | IEnum e -> e.en_pub | IIface i -> i.if_pub | IType a -> a.ty_pub

(* A module's items by bare name, methods left out. *)
let own_items (items : item list) =
  let own = Hashtbl.create 16 in
  List.iter (fun i -> if not (is_method i) then Hashtbl.replace own (item_name i) i) items;
  own

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

(* deps: name -> directory holding NAME.lmn (from limen.toml) *)
(* Returns the path, the text, and whether it came from a dependency. *)
let same_file a b = try Unix.realpath a = Unix.realpath b with Unix.Unix_error _ -> false

let find_module ~deps ~from dir name =
  let local = Filename.concat dir (name ^ ".lmn") in
  (* a file never imports itself: `use http` in http.lmn means another http *)
  if dir <> "" && (Sys.file_exists local || List.mem_assoc local !overlay) && not (same_file local from) then Some (local, read_file local, false)
  else
    match List.assoc_opt name deps with
    | Some d ->
      let p = Filename.concat d (name ^ ".lmn") in
      Some (p, read_file p, true)
    | None -> (
      match List.assoc_opt name !std_modules with
      | Some src -> Some ("<std>/" ^ name ^ ".lmn", Some src, false)
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
  fresh : bool; (* give every expression a new node, even one renaming leaves alone *)
}

(* Renaming keeps the parts of the tree it leaves alone, so a large program
   is not copied whole: [map_same f l] is [List.map f l], or [l] itself when
   [f] returned every element unchanged (and likewise [opt_same] for an
   option). Both call [f] in the order List.map and Option.map do. *)
let rec map_same f l =
  match l with
  | [] -> l
  | x :: rest ->
    let y = f x in
    let rest' = map_same f rest in
    if y == x && rest' == rest then l else y :: rest'

let opt_same f o = match o with None -> o | Some x -> let y = f x in if y == x then o else Some y

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

(* aliases already reported as referring to themselves, so each is reported once *)
let cyclic : (string, unit) Hashtbl.t = Hashtbl.create 4

(* Where each alias is written, by its qualified name, from the last load:
   expanding it removes the name from the program, so find-references and
   rename read its uses from here. *)
let alias_uses : (string * Span.t) list ref = ref []

(* A type with module names resolved and aliases expanded. [seen]: the
   aliases being expanded around this one, to catch `type A = List[A]`. *)
let rec ty ?(seen = []) env (t : ty_expr) : ty_expr =
  match t.tdesc with
  | TyName (n0, args0) -> (
    let n = n0 in
    let args = map_same (ty ~seen env) args0 in
    let named n = if n == n0 && args == args0 then t else { t with tdesc = TyName (n, args) } in
    match String.index_opt n '.' with
    | Some i -> (
      let a = String.sub n 0 i and b = String.sub n (i + 1) (String.length n - i - 1) in
      match module_of env a t.tspan with
      | Some m -> (
        match member env m b t.tspan with
        | Some (IType al) -> expand ~seen env m al args t
        | Some (IStruct _ | IEnum _ | IIface _) -> named (qual m b)
        | _ -> no_member m b t.tspan; named n)
      | None -> err "E0401" t.tspan (Printf.sprintf "unknown module `%s`; add `use %s` at the top of the file" a a); named n)
    | None -> (
      if List.mem n env.gens then named n
      else
        match Hashtbl.find_opt env.own n with
        | Some (IType al) -> expand ~seen env env.self al args t
        | Some (IStruct _ | IEnum _ | IIface _) -> named (qual env.self n)
        | _ -> named n))
  | TyFn (ps, r) ->
    let r' = ty ~seen env r in
    let ps' = map_same (ty ~seen env) ps in
    if r' == r && ps' == ps then t else { t with tdesc = TyFn (ps', r') }
  | TyUnit | TyInt _ -> t

(* alias [al] of module [m], used at [t] with [args] (already resolved): its
   body, resolved in [m], with the arguments put in for its parameters *)
and expand ~seen env (m : modinfo) (al : alias_decl) args (t : ty_expr) : ty_expr =
  let key = qual m al.ty_name in
  alias_uses := (key, t.tspan) :: !alias_uses;
  let bad = { t with tdesc = TyName (key, []) } in
  if List.mem key seen then begin
    if not (Hashtbl.mem cyclic key) then begin
      Hashtbl.replace cyclic key ();
      err "E0116" al.ty_span (Printf.sprintf "type alias `%s` refers to itself" al.ty_name)
        ~notes:[ "an alias is only another name; for a type that contains itself, declare a struct or enum" ]
    end;
    bad
  end
  else if List.length args <> List.length al.ty_gen then begin
    let k = List.length al.ty_gen in
    err "E0109" t.tspan
      (Printf.sprintf "`%s` takes %d type argument%s, found %d" al.ty_name k (if k = 1 then "" else "s") (List.length args));
    bad
  end
  else
    let aenv = { env with self = m; own = (if m == env.self then env.own else own_items m.mitems); gens = al.ty_gen } in
    let body = ty ~seen:(key :: seen) aenv al.ty_body in
    let pairs = List.combine al.ty_gen args in
    let rec subst (x : ty_expr) =
      match x.tdesc with
      | TyName (g, []) when List.mem_assoc g pairs -> List.assoc g pairs
      | TyName (n, xs) -> { x with tdesc = TyName (n, List.map subst xs) }
      | TyFn (ps, r) -> { x with tdesc = TyFn (List.map subst ps, subst r) }
      | TyUnit | TyInt _ -> x
    in
    (* errors about the expanded type point where the alias is used *)
    { (subst body) with tspan = t.tspan }

let mk d sp = { edesc = d; espan = sp; ety = Types.TErr; einst = []; ecallee = None }

(* ---------- embed ---------- *)

(* `embed("page.html")` and `embed_bytes("logo.png")` are read by the compiler
   and become a literal, so a program that serves its own files needs no `Fs`.
   The path is relative to the file that names it and must stay inside that
   file's package (the directory of its limen.toml, or of the main file when
   there is none; a dependency's own directory): otherwise a dependency could
   bake `../../.ssh/id_rsa` into the binary, a read no grant allows. The
   program's own limen.toml (never a dependency's) can add directories its
   files may embed from too, its embed roots: `[embed] roots = ["../assets"]`. *)

(* the main package's root, its embed roots (as written, and the path), and
   the dependencies' directories, for this load *)
let main_root = ref ""
let embed_roots : (string * string) list ref = ref []
let dep_dirs : (string * string) list ref = ref []

(* embedded files for this load: (module, path as written, size) *)
let embedded : (string * string * int) list ref = ref []

(* The directories a module's embedded files may come from: its
   dependency's, or the main package's and then its embed roots. *)
let embed_dirs (m : modinfo) =
  if String.starts_with ~prefix:"<std>" m.mfile then None
  else
    match m.mpkg with
    | Some dep -> Option.map (fun d -> [ d ]) (List.assoc_opt dep !dep_dirs)
    | None -> Some (!main_root :: List.map snd !embed_roots)

let inside_of root r = String.starts_with ~prefix:(if root = "/" then root else root ^ "/") r || r = root

(* The real path of the first of [roots] that holds the real path [real]
   (the first root must exist), or the message saying [path] is outside
   them all. *)
let pick_root roots real path =
  let reals = List.filter_map (fun r -> try Some (Unix.realpath r) with Unix.Unix_error _ -> None) (List.tl roots) in
  let first = Unix.realpath (List.hd roots) in
  match List.find_opt (fun r -> inside_of r real) (first :: reals) with
  | Some r -> Ok r
  | None ->
    if reals = [] then
      Error (Printf.sprintf "`%s` is outside this package (%s); a program can embed only its own package's files" path first)
    else
      Error
        (Printf.sprintf "`%s` is outside this package (%s) and its embed roots (%s); a program can embed only its own package's files and those under the roots its limen.toml declares"
           path first (String.concat ", " reals))

(* The message for a path outside every root. *)
let outside_msg roots path = match pick_root roots "" path with Error m -> m | Ok _ -> ""

let embed_file (m : modinfo) name path sp : expr_desc option =
  let fail msg = err "E0405" sp msg; None in
  match embed_dirs m with
  | None -> fail (Printf.sprintf "the standard library cannot use `%s`" name)
  | Some roots ->
    if path = "" || not (Filename.is_relative path) then
      fail (Printf.sprintf "`%s` takes a path relative to this file, not `%s`" name path)
    else (
      let full = Filename.concat (Filename.dirname m.mfile) path in
      match (Unix.realpath full, Unix.realpath (List.hd roots)) with
      | exception Unix.Unix_error _ -> fail (Printf.sprintf "cannot find `%s` (looked for %s)" path full)
      | real, _ ->
        match pick_root roots real path with
        | Error msg -> fail msg
        (* a single file is never a root itself *)
        | Ok r when r = real -> fail (outside_msg roots path)
        | Ok _ ->
        if Sys.is_directory real then fail (Printf.sprintf "`%s` is a directory, not a file" path)
        else
          match In_channel.with_open_bin real In_channel.input_all with
          | exception Sys_error e -> fail (Printf.sprintf "cannot read `%s`: %s" path e)
          | text ->
            if name = "embed" && not (String.is_valid_utf_8 text) then
              fail (Printf.sprintf "`%s` is not UTF-8 text; use `embed_bytes(\"%s\")` for a Bytes value" path path)
            else (
              embedded := (m.mname, path, String.length text) :: !embedded;
              let lit = mk (EStr [ SPLit text ]) sp in
              Some (if name = "embed" then lit.edesc else EMethod (lit, "to_bytes", sp, []))))

(* `embed_dir("public")`: every file under a directory, read at compile time,
   as a `Map[Str, Bytes]` from the path inside the directory ("css/site.css")
   to the contents, in sorted path order. Names starting with `.` are left
   out (.git, .DS_Store); every file and subdirectory must stay inside the
   package, as for `embed`. *)
let embed_dir (m : modinfo) path sp : expr_desc option =
  let name = "embed_dir" in
  let fail msg = err "E0405" sp msg; None in
  match embed_dirs m with
  | None -> fail (Printf.sprintf "the standard library cannot use `%s`" name)
  | Some roots ->
    if path = "" || not (Filename.is_relative path) then
      fail (Printf.sprintf "`%s` takes a path relative to this file, not `%s`" name path)
    else (
      let full = Filename.concat (Filename.dirname m.mfile) path in
      match (Unix.realpath full, Unix.realpath (List.hd roots)) with
      | exception Unix.Unix_error _ -> fail (Printf.sprintf "cannot find `%s` (looked for %s)" path full)
      | real, _ ->
        match pick_root roots real path with
        | Error msg -> fail msg
        | Ok root ->
        let inside = inside_of root in
        if not (Sys.is_directory real) then fail (Printf.sprintf "`%s` is a file, not a directory; use `embed` or `embed_bytes` for one file" path)
        else
          (* (key, contents) for every file, or the first problem *)
          let rec walk dir key_prefix acc =
            match acc with
            | Error _ -> acc
            | Ok _ ->
              let names = Sys.readdir dir |> Array.to_list |> List.filter (fun n -> n <> "" && n.[0] <> '.') |> List.sort compare in
              List.fold_left
                (fun acc n ->
                  match acc with
                  | Error _ -> acc
                  | Ok _ -> (
                    let key = if key_prefix = "" then n else key_prefix ^ "/" ^ n in
                    match Unix.realpath (Filename.concat dir n) with
                    | exception Unix.Unix_error _ -> Error (Printf.sprintf "cannot read `%s/%s`" path key)
                    | r when not (inside r) ->
                      Error (Printf.sprintf "`%s/%s` points outside this package (%s); a program can embed only its own package's files" path key root)
                    | r when Sys.is_directory r -> walk r key acc
                    | r -> (
                      match In_channel.with_open_bin r In_channel.input_all with
                      | exception Sys_error e -> Error (Printf.sprintf "cannot read `%s/%s`: %s" path key e)
                      | text -> ( match acc with Ok fs -> Ok ((key, text) :: fs) | e -> e))))
                acc names
          in
          match walk real "" (Ok []) with
          | Error msg -> fail msg
          | Ok [] -> fail (Printf.sprintf "`%s` has no files to embed (names starting with `.` are left out)" path)
          | Ok files ->
            let files = List.rev files in
            List.iter (fun (k, text) -> embedded := (m.mname, path ^ "/" ^ k, String.length text) :: !embedded) files;
            let pair (k, text) =
              mk (EStruct (tuple_name 2, sp, [ ("0", sp, mk (EStr [ SPLit k ]) sp); ("1", sp, mk (EMethod (mk (EStr [ SPLit text ]) sp, "to_bytes", sp, [])) sp) ])) sp
            in
            Some (ECall (mk (EVar "map") sp, [ mk (EList (List.map pair files)) sp ])))

let rec pat_binds (p : pat) acc =
  match p.pdesc with
  | PBind n -> n :: acc
  | PRest (Some n) -> n :: acc
  | PVariant (_, _, ps) | PTuple ps | PList ps | POr ps -> List.fold_left (fun acc p -> pat_binds p acc) acc ps
  | PStruct (_, _, fs, _) -> List.fold_left (fun acc (_, _, p) -> pat_binds p acc) acc fs
  | PAs (q, n, _) -> pat_binds q (n :: acc)
  | _ -> acc

(* The qualified name of the struct a literal or a pattern names: `geo.Point`
   from module geo, or `Point` of this module. *)
let struct_name env locals n sp =
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

let rec pat env locals (p : pat) : pat =
  match p.pdesc with
  | PStruct (n0, nsp, fs, rest) ->
    let n = struct_name env locals n0 nsp in
    let fs' = map_same (fun ((f, fsp, q) as fl) -> let q' = pat env locals q in if q' == q then fl else (f, fsp, q')) fs in
    if n == n0 && fs' == fs then p else { p with pdesc = PStruct (n, nsp, fs', rest) }
  | PAs (q, n, nsp) ->
    let q' = pat env locals q in
    if q' == q then p else { p with pdesc = PAs (q', n, nsp) }
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
  | PVariant (None, v, ps) ->
    let ps' = map_same (pat env locals) ps in
    if ps' == ps then p else { p with pdesc = PVariant (None, v, ps') }
  | PTuple ps ->
    let ps' = map_same (pat env locals) ps in
    if ps' == ps then p else { p with pdesc = PTuple ps' }
  | PList ps ->
    let ps' = map_same (pat env locals) ps in
    if ps' == ps then p else { p with pdesc = PList ps' }
  | POr ps ->
    let ps' = map_same (pat env locals) ps in
    if ps' == ps then p else { p with pdesc = POr ps' }
  | _ -> p

let rec expr env locals (e : expr) : expr =
  let ex = expr env locals in
  let d =
    match e.edesc with
    | EInt _ | EFloat _ | EBool _ | EUnit -> e.edesc
    | EStr parts ->
      let parts' = map_same (function SPLit _ as p -> p | SPExpr x as p -> let y = ex x in if y == x then p else SPExpr y) parts in
      if parts' == parts then e.edesc else EStr parts'
    | EVar n ->
      if List.mem n locals then e.edesc
      else (
        match Hashtbl.find_opt env.own n with
        | Some (IFn _) -> EVar (qual env.self n)
        | _ -> if is_module env locals n then (err "E0404" e.espan (Printf.sprintf "module `%s` is not a value; use one of its items, such as `%s.name`" n n); e.edesc) else e.edesc)
    | EList es -> let es' = map_same ex es in if es' == es then e.edesc else EList es'
    | EPipe (x, rhs) -> (
      (* `x |> f(a)` is `f(x, a)` from here on *)
      match pipe_call x rhs with
      | Some d -> (ex { e with edesc = d }).edesc
      | None -> e.edesc (* the parser rejects it *))
    | EBin (op, a, b) ->
      let b' = ex b in
      let a' = ex a in
      if a' == a && b' == b then e.edesc else EBin (op, a', b')
    | EUn (op, a) -> let a' = ex a in if a' == a then e.edesc else EUn (op, a')
    | ECall ({ edesc = EVar (("embed" | "embed_bytes" | "embed_dir") as name); _ }, args)
      when (not (List.mem name locals)) && not (Hashtbl.mem env.own name) -> (
      let literal = match args with [ { edesc = EStr [ SPLit p ]; _ } ] -> Some p | [ { edesc = EStr []; _ } ] -> Some "" | _ -> None in
      match literal with
      | Some path when name = "embed_dir" -> (
        match embed_dir env.self path e.espan with
        | Some d -> d
        | None -> ECall (mk (EVar "map") e.espan, [ mk (EList [ mk (EStruct (tuple_name 2, e.espan, [ ("0", e.espan, mk (EStr []) e.espan); ("1", e.espan, mk (EMethod (mk (EStr []) e.espan, "to_bytes", e.espan, [])) e.espan) ])) e.espan ]) e.espan ]))
      | Some path -> ( match embed_file env.self name path e.espan with Some d -> d | None -> EStr [])
      | None ->
        err "E0405" e.espan
          (Printf.sprintf "`%s` takes one argument, a string literal with the path, as in `%s(\"page.html\")`" name name);
        EStr [])
    | ECall (c, args) ->
      let args' = map_same ex args in
      let c' = ex c in
      if c' == c && args' == args then e.edesc else ECall (c', args')
    | ENamed (n, sp, a) -> let a' = ex a in if a' == a then e.edesc else ENamed (n, sp, a')
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
    | EMethod (r, name, nsp, args) ->
      let args' = map_same ex args in
      let r' = ex r in
      if r' == r && args' == args then e.edesc else EMethod (r', name, nsp, args')
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
    | EField (r, name, nsp) -> let r' = ex r in if r' == r then e.edesc else EField (r', name, nsp)
    | EIndex (a, i) ->
      let i' = ex i in
      let a' = ex a in
      if a' == a && i' == i then e.edesc else EIndex (a', i')
    | ETry (a, c) ->
      let c' = opt_same ex c in
      let a' = ex a in
      if a' == a && c' == c then e.edesc else ETry (a', c')
    | EIf (c, b, el) ->
      let el' = opt_same ex el in
      let b' = block env locals b in
      let c' = ex c in
      if c' == c && b' == b && el' == el then e.edesc else EIf (c', b', el')
    | EMatch (s, arms) ->
      let arms' =
        map_same
          (fun a ->
            let inner = pat_binds a.apat locals in
            let abody = expr env inner a.abody in
            let aguard = opt_same (expr env inner) a.aguard in
            let apat = pat env locals a.apat in
            if apat == a.apat && aguard == a.aguard && abody == a.abody then a else { apat; aguard; abody })
          arms
      in
      let s' = ex s in
      if s' == s && arms' == arms then e.edesc else EMatch (s', arms')
    | EBlock b -> let b' = block env locals b in if b' == b then e.edesc else EBlock b'
    | EClosure (ps, body) ->
      let locals' = List.fold_left (fun acc (n, _, _) -> n :: acc) locals ps in
      let body' = expr env locals' body in
      let ps' = map_same (fun ((n, t, sp) as p) -> let t' = opt_same (ty env) t in if t' == t then p else (n, t', sp)) ps in
      if ps' == ps && body' == body then e.edesc else EClosure (ps', body')
    | EStruct (n0, sp, fields) ->
      let n = struct_name env locals n0 sp in
      let fields' = map_same (fun ((f, fsp, x) as fl) -> let y = ex x in if y == x then fl else (f, fsp, y)) fields in
      if n == n0 && fields' == fields then e.edesc else EStruct (n, sp, fields')
    | EGroup (g, il, until, b) ->
      let b' = block env (g :: locals) b in
      let until' = opt_same (fun ((c, ms) as u) -> let ms' = ex ms in let c' = ex c in if c' == c && ms' == ms then u else (c', ms')) until in
      if b' == b && until' == until then e.edesc else EGroup (g, il, until', b')
    | EWith (b, fields) ->
      let fields' = map_same (fun ((f, fsp, x) as fl) -> let y = ex x in if y == x then fl else (f, fsp, y)) fields in
      let b' = ex b in
      if b' == b && fields' == fields then e.edesc else EWith (b', fields')
    | ESafe (b, x, rest, flat) ->
      let rest' = expr env (x :: locals) rest in
      let b' = ex b in
      if b' == b && rest' == rest then e.edesc else ESafe (b', x, rest', flat)
    | EDyn (x, i) -> let x' = ex x in if x' == x then e.edesc else EDyn (x', i)
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
  if d == e.edesc && not env.fresh then e else { e with edesc = d }

(* `let (a, (b, _)) = e` becomes `let t = e; let a = t.0; let u = t.1; let b = u.0`,
   and `for (k, v) in xs { body }` becomes `for t in xs { let (k, v) = t; body }`.
   A struct pattern checks the value's type with a match that cannot fail:
   `let Point { x, y: (a, _) } = e` becomes
   `let t = match e { Point { x: _, y: _ } as s => s }; let x = t.x; ...`,
   so the checker reports a wrong type, an unknown field or a missing `..`
   at the pattern, as it does in `match`. *)
and unpack_counter = ref 0

and unpack (p : pat) (e : expr) : stmt list =
  incr unpack_counter;
  let tmp = "tuple$" ^ string_of_int !unpack_counter in
  let mk d = { edesc = d; espan = p.pspan; ety = Types.TErr; einst = []; ecallee = None } in
  let part name (sub : pat) =
    let field = mk (EField (mk (EVar tmp), name, sub.pspan)) in
    match sub.pdesc with
    | PBind n -> [ { sdesc = SLet (false, n, sub.pspan, None, field); sspan = sub.pspan } ]
    | PWild -> []
    | _ -> unpack sub field
  in
  match p.pdesc with
  | PStruct (name, nsp, fields, rest) ->
    let whole = "struct$" ^ string_of_int !unpack_counter in
    let shape = { p with pdesc = PStruct (name, nsp, List.map (fun (f, fsp, q) -> (f, fsp, { q with pdesc = PWild })) fields, rest) } in
    let arm = { apat = { p with pdesc = PAs (shape, whole, p.pspan) }; aguard = None; abody = mk (EVar whole) } in
    { sdesc = SLet (false, tmp, p.pspan, None, mk (EMatch (e, [ arm ]))); sspan = p.pspan }
    :: List.concat_map (fun (f, _, q) -> part f q) fields
  | _ ->
    let first = { sdesc = SLet (false, tmp, p.pspan, None, e); sspan = p.pspan } in
    let rest =
      match p.pdesc with
      | PTuple ps -> List.concat (List.mapi (fun i sub -> part (string_of_int i) sub) ps)
      | PBind n -> [ { sdesc = SLet (false, n, p.pspan, None, mk (EVar tmp)); sspan = p.pspan } ]
      | _ -> []
    in
    first :: rest

(* `(a, (b, _)) = e` becomes `let $assign1 = e; a = $assign1.0;
   let $assign2 = $assign1.1; b = $assign2.0`: the right side first, then
   each target, left to right. Each `let` carries the tuple's type name with
   no arguments (`#tuple2`) as its annotation, which says how many parts the
   left side takes; the checker checks the right side against it. *)
and assign_parts (lhs : expr) (e : expr) (sp : Span.t) : stmt list =
  match lhs.edesc with
  | EStruct (n, _, fields) when is_tuple_name n ->
    incr unpack_counter;
    let tmp = "$assign" ^ string_of_int !unpack_counter in
    let ann = { tdesc = TyName (n, []); tspan = lhs.espan } in
    let first = { sdesc = SLet (false, tmp, lhs.espan, Some ann, e); sspan = sp } in
    let part i (_, _, (target : expr)) =
      let mk d = { edesc = d; espan = target.espan; ety = Types.TErr; einst = []; ecallee = None } in
      match target.edesc with
      | EVar "_" -> []
      | _ -> assign_parts target (mk (EField (mk (EVar tmp), string_of_int i, target.espan))) target.espan
    in
    first :: List.concat (List.mapi part fields)
  | _ -> [ { sdesc = SAssign (lhs, e); sspan = sp } ]

and is_tuple_assign (s : stmt) =
  match s.sdesc with SAssign ({ edesc = EStruct (n, _, _); _ }, _) -> is_tuple_name n | _ -> false

and expand_stmts (ss : stmt list) : stmt list =
  if not (List.exists (fun (s : stmt) -> match s.sdesc with SLetPat _ | SForPat _ -> true | _ -> is_tuple_assign s) ss) then ss
  else
  List.concat_map
    (fun (s : stmt) ->
      match s.sdesc with
      | SAssign (lhs, e) when is_tuple_assign s -> assign_parts lhs e s.sspan
      | SLetPat (p, e) -> unpack p e
      | SForPat (p, it, body) ->
        incr unpack_counter;
        let tmp = "item$" ^ string_of_int !unpack_counter in
        let v = { edesc = EVar tmp; espan = p.pspan; ety = Types.TErr; einst = []; ecallee = None } in
        [ { s with sdesc = SFor (tmp, p.pspan, it, { body with stmts = unpack p v @ body.stmts }) } ]
      | _ -> [ s ])
    ss

and block env locals (b0 : block) : block =
  let expanded = expand_stmts b0.stmts in
  let b = if expanded == b0.stmts then b0 else { b0 with stmts = expanded } in
  let locals = ref locals in
  let stmts =
    map_same
      (fun (s : stmt) ->
        let d =
          match s.sdesc with
          | SLet (m, n, sp, t, x) ->
            let x' = expr env !locals x in
            locals := n :: !locals;
            let t' = opt_same (ty env) t in
            if x' == x && t' == t then s.sdesc else SLet (m, n, sp, t', x')
          | SAssign (a, b) ->
            let b' = expr env !locals b in
            let a' = expr env !locals a in
            if a' == a && b' == b then s.sdesc else SAssign (a', b')
          | SExpr x -> let x' = expr env !locals x in if x' == x then s.sdesc else SExpr x'
          | SReturn x -> let x' = opt_same (expr env !locals) x in if x' == x then s.sdesc else SReturn x'
          | SWhile (c, body) ->
            let body' = block env !locals body in
            let c' = expr env !locals c in
            if c' == c && body' == body then s.sdesc else SWhile (c', body')
          | SFor (n, sp, it, body) ->
            let body' = block env (n :: !locals) body in
            let it' = expr env !locals it in
            if it' == it && body' == body then s.sdesc else SFor (n, sp, it', body')
          | SBreak | SContinue -> s.sdesc
          | SDefer body -> let body' = block env !locals body in if body' == body then s.sdesc else SDefer body'
          | SLetPat _ | SForPat _ -> s.sdesc (* already expanded *)
        in
        if d == s.sdesc then s else { s with sdesc = d })
      b.stmts
  in
  let tail = opt_same (expr env !locals) b.tail in
  if stmts == b.stmts && tail == b.tail then b else { b with stmts; tail }

let rename_item ?(fresh = false) mods (m : modinfo) own (it : item) : item =
  match it with
  | IFn d ->
    let env = { self = m; mods; own; gens = d.fn_gen; fresh } in
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
    IFn
      { d with
        fn_name = name;
        (* bounds name interfaces, qualified and expanded like types *)
        fn_bounds = List.map (fun (g, bs) -> (g, List.map (ty env) bs)) d.fn_bounds;
        fn_params = params;
        fn_ret = Option.map (ty env) d.fn_ret;
        fn_body = block env locals d.fn_body;
        (* a standard library module trusted with C (src/std/limen.toml [trust]) *)
        fn_foreign =
          Option.map
            (fun (fo : foreign) ->
              { fo with ftrusted = is_std m && List.mem (std_name m) (Lazy.force Package.std_manifest).trust })
            d.fn_foreign }
  | IStruct s ->
    let env = { self = m; mods; own; gens = s.st_gen; fresh } in
    (* a C struct's Int32 fields are Ints to the rest of Limen, as Int32
       parameters are; the types as written stay in st_foreign *)
    let int32_as_int (t : ty_expr) = match t.tdesc with TyName ("Int32", []) when s.st_foreign <> None -> { t with tdesc = TyName ("Int", []) } | _ -> t in
    IStruct
      { s with
        st_name = qual m s.st_name;
        st_fields = List.map (fun (f, sp, t) -> (f, sp, ty env (int32_as_int t))) s.st_fields;
        st_foreign = Option.map (fun (l, ts) -> (l, List.map (ty env) ts)) s.st_foreign;
        st_bounds = List.map (fun (g, bs) -> (g, List.map (ty env) bs)) s.st_bounds }
  | IIface i ->
    let env = { self = m; mods; own; gens = i.if_gen; fresh } in
    IIface
      { i with
        if_name = qual m i.if_name;
        if_methods =
          List.map
            (fun (n, sp, ps, r) -> (n, sp, List.map (fun p -> { p with prm_ty = ty env p.prm_ty }) ps, Option.map (ty env) r))
            i.if_methods }
  | IEnum e ->
    let env = { self = m; mods; own; gens = e.en_gen; fresh } in
    IEnum
      { e with
        en_name = qual m e.en_name;
        en_variants = List.map (fun (v, sp, ts) -> (v, sp, List.map (ty env) ts)) e.en_variants;
        en_bounds = List.map (fun (g, bs) -> (g, List.map (ty env) bs)) e.en_bounds }
  | IType a ->
    (* expanded where it is used; kept, expanded, so the checker checks its
       body even if nothing uses it *)
    let env = { self = m; mods; own; gens = a.ty_gen; fresh } in
    IType { a with ty_name = qual m a.ty_name; ty_body = ty ~seen:[ qual m a.ty_name ] env a.ty_body }

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

(* An alias may not take a name another item of its module or a built-in
   type has. *)
let builtin_type_names =
  [ "Int"; "Int32"; "Float"; "Bool"; "Str"; "Bytes"; "List"; "Map"; "Array"; "Option"; "Result"; "IoError"; "Chan"; "Atomic"; "Shared"; "Root"; "Net";
    "Fs"; "Listen"; "Listener"; "Conn"; "Env"; "Exec"; "Foreign"; "Revoker"; "Stdout"; "Stderr"; "Stdin"; "Clock"; "Rand"; "Handle"; "Out" ]

let check_aliases (m : modinfo) =
  List.iter
    (function
      | IType a as it ->
        if List.mem a.ty_name builtin_type_names then
          err "E0106" a.ty_span (Printf.sprintf "`%s` is a built-in type; name the alias something else" a.ty_name)
        else if List.exists (fun i -> i != it && (not (is_method i)) && item_name i = a.ty_name) m.mitems then
          err "E0106" a.ty_span (Printf.sprintf "type `%s` is defined more than once" a.ty_name)
      | _ -> ())
    m.mitems

(* ---------- entry point ---------- *)

(* Load the main file at [path] (whose text is [src]) and every module it uses.
   Returns the flat program, or the diagnostics. *)
let load ?(deps = []) ?root ?roots path src : item list * Diag.t list =
  errors := [];
  (* without one (the editor, the agent server), the limen.toml around the
     file decides, as in a build; finding it fetches nothing *)
  main_root :=
    (match root with
     | Some r -> r
     | None -> ( match Package.find path with Some toml -> Filename.dirname toml | None -> Filename.dirname path));
  (embed_roots :=
     match roots with
     | Some rs -> rs
     | None -> (
       (* the same for the embed roots: a limen.toml that cannot be read declares none *)
       match Package.find path with
       | Some toml -> (
         try
           let p = Package.parse toml (In_channel.with_open_bin toml In_channel.input_all) in
           List.map (fun (r, full, _) -> (r, full)) p.embed_roots
         with _ -> [])
       | None -> []));
  dep_dirs := deps;
  embedded := [];
  Hashtbl.reset cyclic;
  alias_uses := [];
  module_pkg := [];
  std_loaded := [];
  (* modules by key: the program's by name, the standard library's by
     "<std>" ^ name, so a module of each name can coexist *)
  let mods : (string, modinfo) Hashtbl.t = Hashtbl.create 8 in
  (* what each name the program's own modules use resolved to, the first
     time one used it: the key of the module *)
  let resolved : (string, string) Hashtbl.t = Hashtbl.create 8 in
  let std_key name = "<std>" ^ name in
  match parse path src with
  | exception Diag.Fatal d -> ([], [ d ])
  | f -> (
    let main = { mname = ""; mfile = path; mitems = f.items; muses = f.uses; mpkg = None } in
    let rec visit (m : modinfo) =
      let dir = if is_std m then "" else Filename.dirname m.mfile in
      List.iter
        (fun (name, sp) ->
          (* the standard library uses only itself *)
          let found =
            if is_std m then
              if Hashtbl.mem mods (std_key name) then None
              else
                Some
                  (match List.assoc_opt name !std_modules with
                   | Some src -> Some ("<std>/" ^ name ^ ".lmn", Some src, false)
                   | None -> None)
            else if Hashtbl.mem resolved name then None
            else Some (find_module ~deps ~from:m.mfile dir name)
          in
          match found with
          | None -> ()
          | Some None ->
              err "E0401" sp (Printf.sprintf "cannot find module `%s`" name)
                ~notes:[ Printf.sprintf "looked for %s, the dependencies in limen.toml and the standard library" (Filename.concat dir (name ^ ".lmn")) ]
          | Some (Some (p, None, _)) -> err "E0401" sp (Printf.sprintf "cannot read module `%s` at %s" name p)
          | Some (Some (p, Some src, from_dep)) ->
              let std = String.starts_with ~prefix:"<std>" p in
              let key = if std then std_key name else name in
              if not (is_std m) then Hashtbl.replace resolved name key;
              if not (Hashtbl.mem mods key) then begin
              (* only the program declares embed roots *)
              let real d = try Unix.realpath d with Unix.Unix_error _ -> d in
              (if from_dep && real (Filename.dirname p) <> real !main_root then
                 let toml = Filename.concat (Filename.dirname p) "limen.toml" in
                 match read_file toml with
                 | Some text -> (
                   match Package.declares_embed toml text with
                   | Some tsp ->
                     err "E0321" tsp
                       (Printf.sprintf "dependency `%s` declares embed roots in its limen.toml; only the program's own limen.toml can" name)
                       ~notes:[ "a dependency embeds only files of its own directory; remove its [embed] table" ]
                   | None -> ())
                 | None -> ());
              let f = parse p src in
              (* tests and main belong to the program being run, not its modules *)
              let items = List.filter (function IFn { fn_test = Some _; _ } -> false | _ -> true) f.items in
              let mpkg = if from_dep then Some name else if std then None else m.mpkg in
              let mi = { mname = name; mfile = p; mitems = items; muses = f.uses; mpkg } in
              Hashtbl.replace mods key mi;
              visit mi
              end)
        m.muses
    in
    match visit main with
    | exception Diag.Fatal d -> ([], [ d ])
    | () ->
      if !errors <> [] then ([], List.rev !errors)
      else begin
        (* a standard library module the program has a module of the same
           name besides is `std/NAME` *)
        let named = Hashtbl.create 8 in
        Hashtbl.iter
          (fun key (m : modinfo) ->
            let m = if is_std m && Hashtbl.mem mods m.mname then { m with mname = "std/" ^ m.mname } else m in
            Hashtbl.replace named key m)
          mods;
        (* each module sees the modules it uses by the names it uses them by *)
        let view (m : modinfo) =
          let v : (string, modinfo) Hashtbl.t = Hashtbl.create 8 in
          List.iter
            (fun (name, _) ->
              let key = if is_std m then Some (std_key name) else Hashtbl.find_opt resolved name in
              match Option.bind key (Hashtbl.find_opt named) with Some u -> Hashtbl.replace v name u | None -> ())
            m.muses;
          v
        in
        let all = main :: List.sort (fun a b -> compare a.mname b.mname) (List.of_seq (Hashtbl.to_seq_values named)) in
        module_pkg := List.filter_map (fun m -> Option.map (fun p -> (m.mname, p)) m.mpkg) all;
        std_loaded := List.filter_map (fun m -> if is_std m then Some (m.mname, std_name m) else None) all;
        let items =
          List.concat_map
            (fun m ->
              let own = own_items m.mitems in
              let mods = view m in
              check_aliases m;
              List.concat_map
                (fun it ->
                  (* the copies of a table test share their body until renaming makes each its own *)
                  let fresh = match it with IFn { fn_table = Some _; _ } -> true | _ -> false in
                  List.map (rename_item ~fresh mods m own) (expand_table it))
                m.mitems)
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

(* Load a program with the limen.toml around it (if any), then type-check it
   and enforce the capabilities of each standard library module and each
   dependency. *)
let load_and_check path src : item list * Diag.t list * Check.fn_info list =
  match Package.load path with
  | Error d -> ([], [ d ], [])
  | Ok pkg -> (
    let deps = match pkg with Some p -> List.map (fun (d : Package.dep) -> (d.dname, d.dir)) p.deps | None -> [] in
    let root = match pkg with Some p -> Filename.dirname p.file | None -> Filename.dirname path in
    let roots = match pkg with Some p -> List.map (fun (r, full, _) -> (r, full)) p.embed_roots | None -> [] in
    match load ~deps ~root ~roots path src with
    | [], (_ :: _ as ds) -> ([], ds, [])
    | items, _ ->
      let ds, infos = Check.check_program items in
      (* `json.encode` and `json.decode` get their conversions written once
         the types are known, and the program is checked again with them *)
      let items, ds, infos =
        let loaded name = Option.map fst (List.find_opt (fun (_, std) -> std = name) !std_loaded) in
        (* the modules that read rows into structs, in this order *)
        let rows = List.filter_map loaded [ "sql"; "postgres" ] in
        match loaded "json" with
        | json when (json <> None || rows <> []) && Diag.errors ds = [] -> (
          match Derive.expand ?json ~rows items with
          | None -> (items, ds, infos)
          | Some (items, (_ :: _ as errs)) -> (items, errs @ ds, infos)
          | Some (items, []) -> let ds, infos = Check.check_program items in (items, ds, infos))
        | _ -> (items, ds, infos)
      in
      (* standard library modules are checked against src/std/limen.toml,
         with or without a limen.toml of the program's own *)
      let warnings = Diag.warnings ds in
      let ds = Diag.errors ds in
      let ds = if ds = [] then Package.enforce ~std:true (Lazy.force Package.std_manifest) !std_loaded infos else ds in
      let ds = match pkg with Some p when ds = [] -> Package.enforce p !module_pkg infos | _ -> ds in
      let ds =
        match pkg with
        | Some p when ds = [] -> Package.check_foreign_lock p (c_users items)
        | _ -> ds
      in
      (items, ds @ warnings, infos))
