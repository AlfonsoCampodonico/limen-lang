(* Print what the type checker (src/check.ml) makes of FILE, as
   selfhost/check.tsr prints the Tessera checker's, so the two can be
   compared. The program is loaded as selfhost/loaddump loads it; if that
   fails, the loader's diagnostics are printed as loaddump prints them.

   Otherwise the checker's diagnostics come first, in the order it reports
   them (sorted by position): ERROR or WARNING, CODE FILE LO HI, the
   message, the notes, and the fix, if any, with each edit as FILE LO HI
   and the replacement. Then each item of the program in load order: the
   item as the checker rewrote it, in the position-free format of
   src/astdump.ml, and below it one line per expression, in the order
   selfhost/walk.tsr visits them (an expression, then the ones inside it,
   as written): the expression's type, the type arguments of a generic
   function's use (`inst`), and for `a?.b` whose rest is an Option already,
   `nested`. *)
open Tessera
open Ast

let rec walk (e : expr) =
  e :: below e

and walk_opt = function Some e -> walk e | None -> []
and walk_all es = List.concat_map walk es

and below (e : expr) =
  match e.edesc with
  | EInt _ | EFloat _ | EBool _ | EUnit | EVar _ -> []
  | EStr parts -> List.concat_map (function SPExpr x -> walk x | SPLit _ -> []) parts
  | EList es -> walk_all es
  | EBin (_, a, b) | EPipe (a, b) | EIndex (a, b) | ESafe (a, _, b, _) -> walk a @ walk b
  | EUn (_, a) | EField (a, _, _) | ENamed (_, _, a) | EClosure (_, a) | EDyn (a, _) -> walk a
  | ECall (f, args) | EMethod (f, _, _, args) -> walk f @ walk_all args
  | ETry (a, c) -> walk a @ walk_opt c
  | EIf (c, b, e) -> walk c @ block b @ walk_opt e
  | EMatch (s, arms) -> walk s @ List.concat_map (fun a -> walk_opt a.aguard @ walk a.abody) arms
  | EBlock b -> block b
  | EStruct (_, _, fs) -> walk_all (List.map (fun (_, _, x) -> x) fs)
  | EWith (b, fs) -> walk b @ walk_all (List.map (fun (_, _, x) -> x) fs)
  | EGroup (_, _, until, b) -> (match until with Some (c, ms) -> walk c @ walk ms | None -> []) @ block b
  | ESelect arms ->
    List.concat_map
      (fun a ->
        (match a.sop with
         | SelRecv (_, c) -> walk c
         | SelSend (c, v) -> walk c @ walk v
         | SelAfter (c, ms) -> walk c @ walk ms
         | SelDefault -> [])
        @ walk_opt a.sguard @ walk a.sbody)
      arms

and block b = List.concat_map stmt b.stmts @ walk_opt b.tail

and stmt s =
  match s.sdesc with
  | SLet (_, _, _, _, e) | SExpr e | SLetPat (_, e) -> walk e
  | SAssign (a, b) -> walk a @ walk b
  | SReturn e -> walk_opt e
  | SWhile (c, b) | SFor (_, _, c, b) | SForPat (_, c, b) -> walk c @ block b
  | SDefer b -> block b
  | SBreak | SContinue -> []

let item_exprs = function
  | IFn d -> (match d.fn_table with Some (_, c) -> walk c | None -> []) @ block d.fn_body
  | _ -> []

let show = Types.show

let expr_line (e : expr) =
  let inst = if e.einst = [] then "" else " inst " ^ String.concat "," (List.map (fun (g, t) -> g ^ "=" ^ show t) e.einst) in
  let nested = match e.edesc with ESafe (_, _, _, true) -> " nested" | _ -> "" in
  let dyn = match e.edesc with EDyn (_, i) -> " dyn " ^ i | _ -> "" in
  print_endline ("  " ^ show e.ety ^ inst ^ nested ^ dyn)

let check_diag (d : Diag.t) =
  let sev = match d.severity with Diag.Error -> "ERROR" | Diag.Warning -> "WARNING" in
  Printf.printf "%s %s %s %d %d\n" sev d.code d.span.file d.span.lo d.span.hi;
  print_endline d.message;
  List.iter (fun n -> print_endline ("note: " ^ n)) d.notes;
  match d.fix with
  | None -> ()
  | Some f ->
    print_endline ("fix: " ^ f.fmsg);
    List.iter
      (fun (ed : Diag.edit) -> Printf.printf "edit %s %d %d %s\n" ed.at.file ed.at.lo ed.at.hi (String.escaped ed.replacement))
      f.edits

let lexer_codes = [ "E0001"; "E0002"; "E0005"; "E0006"; "E0007" ]

let load_diag (d : Diag.t) =
  Printf.printf "ERROR %s %s %d %d\n" d.code d.span.file d.span.lo d.span.hi;
  if not (List.mem d.code lexer_codes) then print_endline d.message;
  List.iter (fun n -> print_endline ("note: " ^ n)) d.notes

let () =
  let path = Sys.argv.(1) in
  let src = In_channel.with_open_bin path In_channel.input_all in
  match Package.load path with
  | Error d -> load_diag d
  | Ok pkg -> (
    let deps = match pkg with Some p -> List.map (fun (d : Package.dep) -> (d.dname, d.dir)) p.deps | None -> [] in
    let root = match pkg with Some p -> Filename.dirname p.file | None -> Filename.dirname path in
    match Modules.load ~deps ~root path src with
    | [], (_ :: _ as ds) -> List.iter load_diag ds
    | items, _ ->
      let ds, _ = Check.check_program items in
      List.iter check_diag ds;
      List.iter
        (fun it ->
          print_endline (Astdump.items [ it ]);
          List.iter expr_line (item_exprs it))
        items)
