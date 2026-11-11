(* Ownership: insert retains and releases (Perceus-style, without reuse).

   1. Backward liveness over the structured IR (loops iterate to a fixpoint).
   2. Forward pass tracking which variables currently own their value:
      - a consuming use of a variable that is dead afterwards is a move;
        otherwise it becomes VDup (retain);
      - an owned variable is released (Drop) right after its last use, at the
        start of a branch that does not use it, and before break/continue/return.
   Moving the last use is what keeps reference counts at 1, so `xs.push` and
   `xs[i] = v` mutate in place instead of copying. *)
open Ir
module S = Set.Make (Int)

type ann = { s : stmt; out : S.t; sub : sub }
and sub = NoSub | IfSub of ablock * ablock | LoopSub of ablock
and ablock = { stmts : ann list; lin : S.t }

let ids atoms = List.fold_left (fun acc a -> match atom_var a with Some v -> S.add v.id acc | None -> acc) S.empty atoms
let step_atoms steps = List.filter_map (function PIndex (a, _) | PIndexU a | PKey (a, _) -> Some a | PField _ -> None) steps

let rhs_uses r = ids (consumed r @ borrowed r)

let rec live_block stmts out ~brk ~cont : ablock =
  let anns, lin =
    List.fold_right
      (fun s (acc, live) ->
        let a, l = live_stmt s live ~brk ~cont in
        (a :: acc, l))
      stmts ([], out)
  in
  { stmts = anns; lin }

and live_stmt s out ~brk ~cont : ann * S.t =
  let plain l = ({ s; out; sub = NoSub }, l) in
  match s with
  | Let (v, r) | Init (v, r) | Set (v, r, _) -> plain (S.union (S.remove v.id out) (rhs_uses r))
  | Decl v -> plain (S.remove v.id out)
  | Mutate (v, steps, m) -> plain (S.add v.id (S.union out (ids (mut_atoms m @ step_atoms steps))))
  | If (c, a, b) ->
    let ba = live_block a out ~brk ~cont and bb = live_block b out ~brk ~cont in
    ({ s; out; sub = IfSub (ba, bb) }, S.union (ids [ c ]) (S.union ba.lin bb.lin))
  | Loop body ->
    let rec fix l =
      let b = live_block body l ~brk:out ~cont:l in
      if S.equal b.lin l then b else fix (S.union l b.lin)
    in
    let b = fix S.empty in
    ({ s; out; sub = LoopSub b }, b.lin)
  | Break -> plain brk
  | Continue -> plain cont
  | Return a -> plain (ids [ a ])
  | Drop _ -> plain out

(* ---------- forward pass ---------- *)

let fn_vars : (int, var) Hashtbl.t = Hashtbl.create 64
let drops set = List.map (fun id -> Drop (Hashtbl.find fn_vars id)) (S.elements set)

(* Rewrite the consumed atoms of a statement: move or retain. *)
let consume (atoms : atom list) ~live_after ~borrowed ~owned =
  let count = Hashtbl.create 8 in
  List.iter
    (fun a -> match atom_var a with Some v -> Hashtbl.replace count v.id (1 + Option.value ~default:0 (Hashtbl.find_opt count v.id)) | None -> ())
    atoms;
  let owned = ref owned in
  let atoms =
    List.map
      (fun a ->
        match a with
        | V v ->
          let left = Hashtbl.find count v.id in
          Hashtbl.replace count v.id (left - 1);
          if S.mem v.id live_after || left > 1 || S.mem v.id borrowed || not (S.mem v.id !owned) then VDup v
          else begin
            owned := S.remove v.id !owned;
            V v
          end
        | a -> a)
      atoms
  in
  (atoms, !owned)

let rewrite_rhs r ~live_after ~owned =
  let borrowed = ids (borrowed r) in
  let c atoms = consume atoms ~live_after ~borrowed ~owned in
  match r with
  | Atom a ->
    let xs, o = c [ a ] in
    (Atom (List.hd xs), o)
  | Call (n, args) ->
    let xs, o = c args in
    (Call (n, xs), o)
  | CallClo (f, args) ->
    let xs, o = c args in
    (CallClo (f, xs), o)
  | MkStruct (t, args) ->
    let xs, o = c args in
    (MkStruct (t, xs), o)
  | MkEnum (t, k, args) ->
    let xs, o = c args in
    (MkEnum (t, k, xs), o)
  | MkList (t, args) ->
    let xs, o = c args in
    (MkList (t, xs), o)
  | MkClosure (n, t, args) ->
    let xs, o = c args in
    (MkClosure (n, t, xs), o)
  | Spawn (g, task, t, keep) ->
    let xs, o = c [ task ] in
    (Spawn (g, List.hd xs, t, keep), o)
  | ChanSend (ch, v, sp) ->
    let xs, o = c [ v ] in
    (ChanSend (ch, List.hd xs, sp), o)
  | Select (id, ops, d, sp) ->
    let sends = List.filter_map (function SSend (_, v, _, _) -> Some v | SRecv _ | SAfter _ -> None) ops in
    let xs, o = c sends in
    let rest = ref xs in
    let ops =
      List.map
        (function
          | SSend (ch, _, t, g) -> ( match !rest with x :: r -> rest := r; SSend (ch, x, t, g) | [] -> failwith "select atoms")
          | op -> op)
        ops
    in
    (Select (id, ops, d, sp), o)
  | MkDyn (t, i, v, ms) ->
    let xs, o = c [ v ] in
    (MkDyn (t, i, List.hd xs, ms), o)
  | CallDyn (k, ps, r, recv, args) ->
    let xs, o = c args in
    (CallDyn (k, ps, r, recv, xs), o)
  | r -> (r, owned)

let rec fwd_block (b : ablock) owned ~brk ~cont : stmt list * S.t =
  let entry = S.diff owned b.lin in
  let acc = ref (List.rev (drops entry)) in
  let owned = ref (S.diff owned entry) in
  List.iter
    (fun a ->
      let ss, o = fwd_stmt a !owned ~brk ~cont in
      acc := List.rev_append ss !acc;
      owned := o)
    b.stmts;
  (List.rev !acc, !owned)

and fwd_stmt (a : ann) owned ~brk ~cont : stmt list * S.t =
  let after s owned =
    let d = S.diff owned a.out in
    (s :: drops d, S.diff owned d)
  in
  match (a.s, a.sub) with
  | Let (v, r), _ ->
    let r, o = rewrite_rhs r ~live_after:a.out ~owned in
    after (Let (v, r)) (S.add v.id o)
  | Init (v, r), _ ->
    let r, o = rewrite_rhs r ~live_after:a.out ~owned in
    after (Init (v, r)) (S.add v.id o)
  | Set (v, r, flag), _ ->
    let r, o = rewrite_rhs r ~live_after:(S.remove v.id a.out) ~owned in
    flag := S.mem v.id o;
    after (Set (v, r, flag)) (S.add v.id o)
  | Decl v, _ -> ([ Decl v ], owned)
  | Mutate (v, steps, m), _ ->
    let borrowed = S.add v.id (ids (step_atoms steps)) in
    let xs, o = consume (mut_atoms m) ~live_after:a.out ~borrowed ~owned in
    let m =
      match (m, xs) with
      | MSet _, [ x ] -> MSet x
      | MPush _, [ x ] -> MPush x
      | MRemove _, [ x ] -> MRemove x
      | MPut _, [ k; x ] -> MPut (k, x)
      | _ -> failwith "mutate atoms"
    in
    after (Mutate (v, steps, m)) o
  | If (c, _, _), IfSub (ba, bb) ->
    let sa, oa = fwd_block ba owned ~brk ~cont in
    let sb, ob = fwd_block bb owned ~brk ~cont in
    ([ If (c, sa, sb) ], S.inter a.out (S.union oa ob))
  | Loop _, LoopSub b ->
    let body, _ = fwd_block b owned ~brk:a.out ~cont:b.lin in
    ([ Loop body ], S.inter owned a.out)
  | Break, _ -> (drops (S.diff owned brk) @ [ Break ], S.empty)
  | Continue, _ -> (drops (S.diff owned cont) @ [ Continue ], S.empty)
  | Return x, _ ->
    let xs, o = consume [ x ] ~live_after:S.empty ~borrowed:S.empty ~owned in
    (drops o @ [ Return (List.hd xs) ], S.empty)
  | s, _ -> ([ s ], owned)

let rec collect_vars stmts =
  List.iter
    (fun s ->
      let add (v : var) = Hashtbl.replace fn_vars v.id v in
      match s with
      | Let (v, _) | Decl v | Init (v, _) | Set (v, _, _) | Mutate (v, _, _) | Drop v -> add v
      | If (_, a, b) -> collect_vars a; collect_vars b
      | Loop b -> collect_vars b
      | _ -> ())
    stmts

let fn (f : fn) : fn =
  Hashtbl.reset fn_vars;
  List.iter (fun (v : var) -> Hashtbl.replace fn_vars v.id v) (f.params @ f.captures);
  collect_vars f.body;
  let b = live_block f.body S.empty ~brk:S.empty ~cont:S.empty in
  let owned = List.fold_left (fun acc (v : var) -> S.add v.id acc) S.empty (f.params @ f.captures) in
  let body, _ = fwd_block b owned ~brk:S.empty ~cont:S.empty in
  { f with body }

let program (p : program) = { p with fns = List.map fn p.fns }
