(* Print the syntax tree of FILE as the Tessera parser in selfhost/parser.tsr
   does. First the tree in the compiler's own position-free format
   (src/astdump.ml, the one the formatter tests compare), then the spans the
   tree carries: a line `spans`, then one line per `use` and per item with
   every span in it, in the order a walk of the tree meets them. A parse
   error prints ERROR CODE LO HI, followed by its message and notes. *)
open Tessera
open Ast

let out = Buffer.create 4096
let sp (s : Span.t) = Printf.bprintf out " %d:%d" s.lo s.hi

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
    List.iter (fun (_, fsp, t) -> sp fsp; ty t) s.st_fields
  | IEnum e ->
    sp e.en_span;
    bounds e.en_bounds;
    List.iter (fun (_, vsp, ts) -> sp vsp; List.iter ty ts) e.en_variants
  | IIface i ->
    sp i.if_span;
    List.iter (fun (_, msp, ps, r) -> sp msp; List.iter param ps; opt ty r) i.if_methods
  | IType a -> sp a.ty_span; ty a.ty_body

let line f =
  Buffer.clear out;
  f ();
  print_endline ("  " ^ String.trim (Buffer.contents out))

let () =
  let file = Sys.argv.(1) in
  let src = In_channel.with_open_bin file In_channel.input_all in
  match Parser.parse_file file src with
  | f ->
    print_endline (Astdump.file f);
    print_endline "spans";
    List.iter (fun (_, usp) -> line (fun () -> sp usp)) f.uses;
    List.iter (fun i -> line (fun () -> item i)) f.items
  | exception Diag.Fatal d ->
    Printf.printf "ERROR %s %d %d\n" d.code d.span.lo d.span.hi;
    print_endline d.message;
    List.iter (fun n -> print_endline ("note: " ^ n)) d.notes
