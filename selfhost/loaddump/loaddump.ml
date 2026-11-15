(* Print the program the module loader (src/modules.ml) makes of FILE, as
   selfhost/load.lmn prints the Limen loader's, so the two can be
   compared. The package around the file is loaded first, as a build does
   (src/package.ml: limen.toml, its dependencies).

   A loaded program prints as its items, one per line, in the compiler's
   position-free format (src/astdump.ml), with qualified names and every
   expansion done; then `foreign`, what the loader decided about C
   declarations; then `embed roots`, the directories the program's
   limen.toml lets it embed from (as written, and the path); then
   `embedded`, the files `embed` read; then `spans`,
   one line per item: the item's file and every span in it, in the order a
   walk of the tree meets them. A span from another file (a type alias's
   body) is written with that file after an `@`.

   Otherwise it prints the diagnostics in order: ERROR CODE FILE LO HI,
   the message and the notes. *)
open Limen
open Ast

let out = Buffer.create 4096

(* the file of the item being written *)
let home = ref ""

let sp (s : Span.t) =
  if s.file = !home then Printf.bprintf out " %d:%d" s.lo s.hi else Printf.bprintf out " %d:%d@%s" s.lo s.hi s.file

let rec ty (t : ty_expr) =
  sp t.tspan;
  match t.tdesc with
  | TyName (_, args) -> List.iter ty args
  | TyUnit | TyInt _ -> ()
  | TyFn (ps, r) -> List.iter ty ps; ty r

let rec pat (p : pat) =
  sp p.pspan;
  match p.pdesc with
  | PVariant (_, _, args) | PTuple args | PList args | POr args -> List.iter pat args
  | PStruct (_, nsp, fields, _) -> sp nsp; List.iter (fun (_, fsp, q) -> sp fsp; pat q) fields
  | PAs (q, _, nsp) -> pat q; sp nsp
  | PWild | PBind _ | PInt _ | PStr _ | PBool _ | PRest _ -> ()

let opt f = function Some x -> f x | None -> ()

let rec expr (e : expr) =
  sp e.espan;
  match e.edesc with
  | EInt _ | EFloat _ | EBool _ | EUnit | EVar _ -> ()
  | EStr parts -> List.iter (function SPLit _ -> () | SPExpr x -> expr x) parts
  | EList es -> List.iter expr es
  | EBin (_, a, b) | EPipe (a, b) | EIndex (a, b) -> expr a; expr b
  | EUn (_, a) -> expr a
  | ECall (f, args) -> expr f; List.iter expr args
  | EMethod (r, _, msp, args) -> expr r; sp msp; List.iter expr args
  | EField (r, _, fsp) -> expr r; sp fsp
  | ETry (a, c) -> expr a; opt expr c
  | EIf (c, b, e) -> expr c; block b; opt expr e
  | EMatch (s, arms) ->
    expr s;
    List.iter (fun a -> pat a.apat; opt expr a.aguard; expr a.abody) arms
  | EBlock b -> block b
  | EClosure (ps, body) -> List.iter (fun (_, t, psp) -> sp psp; opt ty t) ps; expr body
  | EStruct (_, nsp, fields) -> sp nsp; List.iter (fun (_, fsp, x) -> sp fsp; expr x) fields
  | EWith (b, fields) -> expr b; List.iter (fun (_, fsp, x) -> sp fsp; expr x) fields
  | EGroup (_, _, until, b) -> opt (fun (c, ms) -> expr c; expr ms) until; block b
  | EDyn (x, _) -> expr x
  | ESelect arms ->
    List.iter
      (fun a ->
        sp a.sspan_;
        (match a.sop with
         | SelRecv (b, c) -> opt (fun (_, bsp) -> sp bsp) b; expr c
         | SelSend (c, v) -> expr c; expr v
         | SelAfter (c, ms) -> expr c; expr ms
         | SelDefault -> ());
        opt expr a.sguard;
        expr a.sbody)
      arms
  | ESafe (b, _, rest, _) -> expr b; expr rest
  | ENamed (_, nsp, x) -> sp nsp; expr x

and block b =
  sp b.bspan;
  List.iter stmt b.stmts;
  opt expr b.tail

and stmt s =
  sp s.sspan;
  match s.sdesc with
  | SLet (_, _, nsp, t, e) -> sp nsp; opt ty t; expr e
  | SAssign (a, b) -> expr a; expr b
  | SExpr e -> expr e
  | SReturn e -> opt expr e
  | SWhile (c, b) -> expr c; block b
  | SFor (_, nsp, e, b) -> sp nsp; expr e; block b
  | SLetPat (p, e) -> pat p; expr e
  | SForPat (p, e, b) -> pat p; expr e; block b
  | SBreak | SContinue -> ()
  | SDefer b -> block b

let param p = sp p.prm_span; ty p.prm_ty
let bounds bs = List.iter (fun (_, ts) -> List.iter ty ts) bs

let item = function
  | IFn d ->
    sp d.fn_span;
    List.iter param d.fn_params;
    opt ty d.fn_ret;
    bounds d.fn_bounds;
    opt (fun (p, e) -> pat p; expr e) d.fn_table;
    block d.fn_body
  | IStruct s ->
    sp s.st_span;
    bounds s.st_bounds;
    List.iter (fun (_, fsp, t) -> sp fsp; ty t) s.st_fields;
    opt (fun (_, ts) -> List.iter ty ts) s.st_foreign
  | IEnum e ->
    sp e.en_span;
    bounds e.en_bounds;
    List.iter (fun (_, vsp, ts) -> sp vsp; List.iter ty ts) e.en_variants
  | IIface i ->
    sp i.if_span;
    List.iter (fun (_, msp, ps, r) -> sp msp; List.iter param ps; opt ty r) i.if_methods
  | IType a -> sp a.ty_span; ty a.ty_body

let item_span = function IFn d -> d.fn_span | IStruct s -> s.st_span | IEnum e -> e.en_span | IIface i -> i.if_span | IType a -> a.ty_span

let spans i =
  home := (item_span i).file;
  Buffer.clear out;
  item i;
  print_endline ("  " ^ !home ^ " " ^ String.trim (Buffer.contents out))

let program items =
  if items <> [] then print_endline (Astdump.items items);
  print_endline "foreign";
  List.iter
    (function
      | IFn { fn_name; fn_foreign = Some fo; _ } -> Printf.printf "  fn %s %s\n" fn_name (if fo.ftrusted then "trusted" else "untrusted")
      | IStruct { st_name; st_foreign = Some (lib, ts); _ } ->
        Printf.printf "  struct %s %s %s\n" st_name (String.escaped lib) (String.concat "," (List.map Astdump.ty ts))
      | _ -> ())
    items;
  print_endline "embed roots";
  List.iter (fun (r, full) -> Printf.printf "  %s %s\n" r full) !Modules.embed_roots;
  print_endline "embedded";
  List.iter (fun (m, p, n) -> Printf.printf "  %s %s %d\n" (if m = "" then "main" else m) p n) (List.rev !Modules.embedded);
  print_endline "spans";
  List.iter spans items

let diag (d : Diag.t) =
  Printf.printf "ERROR %s %s %d %d\n" d.code d.span.file d.span.lo d.span.hi;
  print_endline d.message;
  List.iter (fun n -> print_endline ("note: " ^ n)) d.notes

let () =
  let path = Sys.argv.(1) in
  let src = In_channel.with_open_bin path In_channel.input_all in
  match Package.load path with
  | Error d -> diag d
  | Ok pkg -> (
    let deps = match pkg with Some p -> List.map (fun (d : Package.dep) -> (d.dname, d.dir)) p.deps | None -> [] in
    let root = match pkg with Some p -> Filename.dirname p.file | None -> Filename.dirname path in
    let roots = match pkg with Some p -> List.map (fun (r, full, _) -> (r, full)) p.embed_roots | None -> [] in
    match Modules.load ~deps ~root ~roots path src with
    | [], (_ :: _ as ds) -> List.iter diag ds
    | items, _ -> program items)
