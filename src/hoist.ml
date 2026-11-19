(* Analysis for hoisting copy-on-write checks out of loops (used by Cgen). *)
open Ir

(* List variables that a loop body writes through: `v[i] = ...`, `v.push(...)`. *)
let mutated_lists body =
  let acc = ref [] in
  let rec go = function
    | Mutate (v, (PIndex _ :: _ | []), _) when not (List.exists (fun (w : var) -> w.id = v.id) !acc) -> (
      match v.ty with Types.TList _ -> acc := v :: !acc | _ -> ())
    | If (_, a, b) -> List.iter go a; List.iter go b
    | Loop b | Guard (_, _, b) | Scope (_, b) -> List.iter go b
    | _ -> ()
  in
  List.iter go body;
  !acc

let mentions (v : var) atoms = List.exists (fun a -> match atom_var a with Some w -> w.id = v.id | None -> false) atoms

(* Could the list's reference count rise inside the loop? Any retain or move
   of the variable, or a reassignment, rules the hoist out; so does a list
   declared inside the loop, which does not exist yet where the hoisted check
   would go. *)
let escapes (v : var) body =
  let rec go = function
    | Let (w, r) | Init (w, r) -> w.id = v.id || mentions v (consumed r)
    | Set (w, r, _) -> w.id = v.id || mentions v (consumed r)
    | Decl w when w.id = v.id -> true
    | Mutate (_, _, m) -> mentions v (mut_atoms m)
    | Return a -> mentions v [ a ]
    | If (_, a, b) -> List.exists go a || List.exists go b
    | Loop b -> List.exists go b
    | Guard _ | Scope _ -> true (* code after a panic or deadline can see the list: keep the checks *)
    | Decl _ | Break | Continue | Drop _ | GuardEnd _ | Line _ -> false
  in
  List.exists go body

(* A counted fill loop:
     loop { c = i < n (or <=); if c {} else { break }; v.push(x); t = i + 1; i = t }
   where x is a scalar constant or a variable the loop does not assign, and n
   likewise. Returns (i, inclusive, n, v, x). Drops of scalars are ignored. *)
let fill_loop body =
  let body = List.filter (function Drop (w : var) -> not (Types.(match w.ty with TInt | TFloat | TBool | TUnit -> true | _ -> false)) | _ -> true) body in
  let assigned = List.filter_map (function Set (w, _, _) -> Some w.id | Let (w, _) -> Some w.id | _ -> None) body in
  let invariant = function
    | V (w : var) -> not (List.mem w.id assigned)
    | AInt _ | AFloat _ | ABool _ | AUnit -> true
    | _ -> false
  in
  let only_drops l = List.for_all (function Drop _ -> true | _ -> false) l in
  match body with
  | [ Let (c, Bin (((Ast.Lt | Ast.Le) as cmp), Types.TInt, V i, n, _));
      If (V c', yes, brk);
      Mutate (v, [], MPush x);
      Let (t, Bin (Ast.Add, Types.TInt, V i', AInt 1L, _));
      Set (i'', Atom (V t'), _) ]
    when c.id = c'.id && i.id = i'.id && i.id = i''.id && t.id = t'.id && only_drops yes
         && (match List.rev brk with Break :: rest -> only_drops rest | _ -> false)
         && invariant n && invariant x && v.id <> i.id
         && (match v.ty with Types.TList (Types.TInt | Types.TFloat | Types.TBool | Types.TUnit) -> true | _ -> false) ->
    Some (i, cmp = Ast.Le, n, v, x)
  | _ -> None

(* Loop versioning for bounds checks. Matches
     loop { c = m < n (or <=); if c {} else { break }; ...; t = m + step; m = t; ... }
   where m is assigned only by that increment, n and step are not assigned in
   the loop, and the lists indexed by m keep their length (no push, no
   reassignment). Returns (m, inclusive, n, step, lists). The caller emits the
   fast version guarded by: m >= 0, step >= 0, n + step does not overflow, and
   n < len (or n <= len) for every list. *)
let rec all_stmts body =
  List.concat_map
    (function
      | If (_, a, b) as s -> s :: all_stmts a @ all_stmts b
      | Loop b as s -> s :: all_stmts b
      | Guard (_, h, b) as s -> s :: all_stmts h @ all_stmts b
      | Scope (_, b) as s -> s :: all_stmts b
      | s -> [ s ])
    body

let versionable body =
  match body with
  | Let (c, Bin (((Ast.Lt | Ast.Le) as cmp), Types.TInt, V m, n, _)) :: If (V c', _, _) :: _ when c.id = c'.id -> (
    let ss = all_stmts body in
    let assigns_of (w : var) = List.filter (function Set (x, _, _) | Let (x, _) | Init (x, _) -> x.id = w.id | _ -> false) ss in
    let invariant = function V (w : var) -> assigns_of w = [] | AInt _ -> true | _ -> false in
    (* m's only assignment: m = t, with t = m + step *)
    let incr =
      match assigns_of m with
      | [ Set (_, Atom (V t), _) ] -> (
        match List.find_opt (function Let (x, _) -> x.id = t.id | _ -> false) ss with
        | Some (Let (_, Bin (Ast.Add, Types.TInt, V m', step, _))) when m'.id = m.id && invariant step -> Some step
        | _ -> None)
      | _ -> None
    in
    (* lists indexed by m directly (first step), in reads and writes *)
    let indexed = ref [] in
    let add (v : var) = if not (List.exists (fun (w : var) -> w.id = v.id) !indexed) then indexed := v :: !indexed in
    List.iter
      (function
        | Let (_, Get (V v, Types.TList _, PIndex (V i, _) :: _)) when i.id = m.id -> add v
        | Mutate (v, PIndex (V i, _) :: _, _) when i.id = m.id -> ( match v.ty with Types.TList _ -> add v | _ -> ())
        | _ -> ())
      ss;
    let keeps_len (v : var) =
      assigns_of v = []
      && not (List.exists (function Mutate (w, [], MPush _) -> w.id = v.id | _ -> false) ss)
    in
    (* Accesses must come before the increment: the guard only covers indexes
       up to n. The increment must be a top-level statement of the body. *)
    let uses_m s =
      List.exists
        (function
          | Let (_, Get (_, _, PIndex (V i, _) :: _)) | Mutate (_, PIndex (V i, _) :: _, _) -> i.id = m.id
          | _ -> false)
        (all_stmts [ s ])
    in
    let rec ordered seen_incr = function
      | [] -> seen_incr
      | (Set (x, _, _) as _s) :: rest when x.id = m.id -> ordered true rest
      | s :: rest -> if seen_incr && uses_m s then false else ordered seen_incr rest
    in
    match incr with
    | Some step when invariant n && !indexed <> [] && List.for_all keeps_len !indexed && ordered false body ->
      Some (m, cmp = Ast.Le, n, step, !indexed)
    | _ -> None)
  | _ -> None
