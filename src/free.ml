(* Free variables of an expression, and whether a closure body can return
   early (`return` or `?`) from itself. Used to decide inlining and fusion. *)
open Ast

let rec pat_binds p acc =
  match p.pdesc with
  | PBind n -> n :: acc
  | PVariant (_, _, args) -> List.fold_left (fun acc a -> pat_binds a acc) acc args
  | _ -> acc

let rec fv bound acc e =
  let go = fv bound in
  match e.edesc with
  | EVar n -> if List.mem n bound || List.mem n acc then acc else n :: acc
  | EInt _ | EFloat _ | EBool _ | EUnit -> acc
  | EGroup (n, b) -> fv_block (n :: bound) acc b
  | EStr ps -> List.fold_left (fun acc -> function SPExpr x -> go acc x | SPLit _ -> acc) acc ps
  | EList es -> List.fold_left go acc es
  | EBin (_, a, b) | EIndex (a, b) -> go (go acc a) b
  | EUn (_, a) | ETry a | EField (a, _, _) -> go acc a
  | ECall (c, args) -> List.fold_left go (go acc c) args
  | EMethod (r, _, _, args) -> List.fold_left go (go acc r) args
  | EIf (c, b, eo) ->
    let acc = fv_block bound (go acc c) b in
    (match eo with Some x -> go acc x | None -> acc)
  | EMatch (s, arms) ->
    List.fold_left (fun acc a -> fv (pat_binds a.apat bound) acc a.abody) (go acc s) arms
  | EBlock b -> fv_block bound acc b
  | EClosure (ps, body) -> fv (List.map (fun (n, _, _) -> n) ps @ bound) acc body
  | EStruct (_, _, fs) -> List.fold_left (fun acc (_, _, x) -> go acc x) acc fs
  | EDyn (x, _) -> go acc x

and fv_block bound acc b =
  let bound, acc =
    List.fold_left
      (fun (bound, acc) s ->
        match s.sdesc with
        | SLet (_, n, _, _, e) -> (n :: bound, fv bound acc e)
        | SAssign (l, r) -> (bound, fv bound (fv bound acc l) r)
        | SExpr e -> (bound, fv bound acc e)
        | SReturn (Some e) -> (bound, fv bound acc e)
        | SReturn None | SBreak | SContinue -> (bound, acc)
        | SWhile (c, body) -> (bound, fv_block bound (fv bound acc c) body)
        | SFor (n, _, e, body) -> (bound, fv_block (n :: bound) (fv bound acc e) body))
      (bound, acc) b.stmts
  in
  match b.tail with Some e -> fv bound acc e | None -> acc

let expr bound e = fv bound [] e

(* `return` or `?` at this closure's own level (not inside a nested closure). *)
let rec has_escape e =
  match e.edesc with
  | ETry _ -> true
  | EClosure _ -> false
  | EVar _ | EInt _ | EFloat _ | EBool _ | EUnit -> false
  | EGroup (_, b) -> block_escape b
  | EStr ps -> List.exists (function SPExpr x -> has_escape x | SPLit _ -> false) ps
  | EList es -> List.exists has_escape es
  | EBin (_, a, b) | EIndex (a, b) -> has_escape a || has_escape b
  | EUn (_, a) | EField (a, _, _) -> has_escape a
  | ECall (c, args) -> has_escape c || List.exists has_escape args
  | EMethod (r, _, _, args) -> has_escape r || List.exists has_escape args
  | EIf (c, b, eo) -> has_escape c || block_escape b || (match eo with Some x -> has_escape x | None -> false)
  | EMatch (s, arms) -> has_escape s || List.exists (fun a -> has_escape a.abody) arms
  | EBlock b -> block_escape b
  | EStruct (_, _, fs) -> List.exists (fun (_, _, x) -> has_escape x) fs
  | EDyn (x, _) -> has_escape x

and block_escape b =
  List.exists
    (fun s ->
      match s.sdesc with
      | SReturn _ -> true
      | SLet (_, _, _, _, e) | SExpr e -> has_escape e
      | SAssign (l, r) -> has_escape l || has_escape r
      | SWhile (c, body) -> has_escape c || block_escape body
      | SFor (_, _, e, body) -> has_escape e || block_escape body
      | SBreak | SContinue -> false)
    b.stmts
  || match b.tail with Some e -> has_escape e | None -> false
