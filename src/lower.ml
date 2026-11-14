(* Typed AST -> IR. Generic functions are instantiated per concrete type
   arguments (monomorphization); closures are lifted to top-level functions
   that receive their captured values; list map/filter/fold and the Option and
   Result helpers become plain loops and branches. *)
open Ast
open Types
module I = Ir

let sprintf = Printf.sprintf

type gctx = {
  fns : (string, fn_decl) Hashtbl.t;
  structs : (string, struct_decl) Hashtbl.t;
  enums : (string, enum_decl) Hashtbl.t;
  ctors : (string, string) Hashtbl.t; (* variant -> enum, when unambiguous *)
  instances : (string, string) Hashtbl.t; (* "name[args]" -> C name *)
  mutable queue : (string * fn_decl * (string * ty) list) list;
  mutable out : I.fn list;
  mutable counter : int;
}

(* A `defer` being lowered: its body, the names in scope at the defer, the
   loop depth there, its guard number and the defers around it. *)
type dframe = { dbody : block; dscopes : (string, I.var) Hashtbl.t list; dloop : int; did : int; douter : dframe list }

type fctx = {
  g : gctx;
  genv : (string * ty) list;
  mutable scopes : (string, I.var) Hashtbl.t list;
  mutable buf : I.stmt list; (* reversed *)
  mutable dead : bool; (* control cannot reach this point *)
  ret : ty;
  parent : fctx option;
  caps : (I.var * I.var) list ref; (* captured: outer var, inner var *)
  mutable defers : dframe list; (* innermost first *)
  mutable loop_depth : int;
}

let guard_counter = ref 0

let var_counter = ref 0

let fresh_var name ty =
  incr var_counter;
  { I.id = !var_counter; name; ty }

let new_fctx g genv ret parent =
  { g; genv; scopes = [ Hashtbl.create 8 ]; buf = []; dead = false; ret; parent; caps = ref []; defers = []; loop_depth = 0 }

let emit f s = if not f.dead then f.buf <- s :: f.buf

let tmp f ty rhs =
  let v = fresh_var "t" ty in
  emit f (I.Let (v, rhs));
  I.V v

let bind f n v = Hashtbl.replace (List.hd f.scopes) n v

(* Lower a loop body one loop deeper. *)
let in_loop f k =
  f.loop_depth <- f.loop_depth + 1;
  k ();
  f.loop_depth <- f.loop_depth - 1

(* Run [k] collecting its statements into a separate block with its own scope. *)
let in_block f k =
  let saved = f.buf and saved_dead = f.dead in
  f.buf <- [];
  f.dead <- false;
  f.scopes <- Hashtbl.create 8 :: f.scopes;
  let r = k () in
  let stmts = List.rev f.buf and d = f.dead in
  f.scopes <- List.tl f.scopes;
  f.buf <- saved;
  f.dead <- saved_dead;
  (stmts, r, d)

let rec lookup f n =
  let rec local = function
    | [] -> None
    | s :: rest -> ( match Hashtbl.find_opt s n with Some v -> Some v | None -> local rest)
  in
  match local f.scopes with
  | Some v -> Some v
  | None -> (
    match f.parent with
    | None -> None
    | Some p -> (
      match lookup p n with
      | None -> None
      | Some outer -> (
        match List.find_opt (fun ((o : I.var), _) -> o.id = outer.id) !(f.caps) with
        | Some (_, inner) -> Some inner
        | None ->
          let inner = fresh_var n outer.ty in
          f.caps := !(f.caps) @ [ (outer, inner) ];
          Some inner)))

(* ---------- types ---------- *)

let rec resolve env (te : ty_expr) : ty =
  match te.tdesc with
  | TyUnit -> TUnit
  | TyFn (ps, r) -> TFn (List.map (resolve env) ps, resolve env r)
  | TyInt _ -> TUnit
  | TyName ("Array", [ elt; { tdesc = TyInt n; _ } ]) -> TArray (resolve env elt, TLen n)
  | TyName ("Array", [ elt; { tdesc = TyName (g, []); _ } ]) when List.mem_assoc g env -> TArray (resolve env elt, List.assoc g env)
  | TyName ("dyn", [ { tdesc = TyName (i, args); _ } ]) -> TDyn (i, List.map (resolve env) args)
  | TyName (n, args) -> (
    let a = List.map (resolve env) args in
    match (n, a) with
    | "Int", _ -> TInt
    | "Float", _ -> TFloat
    | "Bool", _ -> TBool
    | "Str", _ -> TStr
    | "Bytes", _ -> TBytes
    | "List", [ x ] -> TList x
    | "Root", _ -> TCap Root
    | "Net", _ -> TCap Net
    | "Fs", _ -> TCap Fs
    | "Stdout", _ -> TCap Stdout
    | "Clock", _ -> TCap Clock
    | "Listen", _ -> TCap Listen
    | "Listener", _ -> TCap Listener
    | "Conn", _ -> TCap Conn
    | "Rand", _ -> TCap Rand
    | "Stdin", _ -> TCap Stdin
    | "Env", _ -> TCap Env
    | "Exec", _ -> TCap Exec
    | "Foreign", _ -> TCap Foreign
    | "Chan", [ x ] -> TChan x
    | "Map", [ k; v ] -> TMap (k, v)
    | _ -> ( match List.assoc_opt n env with Some t -> t | None -> TNamed (n, a)))

let field_types g = function
  | TNamed (n, args) ->
    let s = Hashtbl.find g.structs n in
    let env = List.combine s.st_gen args in
    List.map (fun (f, _, te) -> (f, resolve env te)) s.st_fields
  | _ -> failwith "field_types"

let variants g = function
  | TNamed (n, args) ->
    let e = Hashtbl.find g.enums n in
    let env = List.combine e.en_gen args in
    List.map (fun (v, _, tes) -> (v, List.map (resolve env) tes)) e.en_variants
  | _ -> failwith "variants"

let index_of x l =
  let rec go i = function [] -> failwith ("index_of " ^ x) | y :: r -> if y = x then i else go (i + 1) r in
  go 0 l

let field_index g t name = index_of name (List.map fst (field_types g t))
let tag_of g t v = index_of v (List.map fst (variants g t))
let tyof f (e : expr) = zonk f.genv e.ety
let is_struct g = function TNamed (n, _) -> Hashtbl.mem g.structs n | _ -> false

(* ---------- function instances ---------- *)

let instance g name (targs : (string * ty) list) =
  let key = name ^ "[" ^ String.concat "," (List.map (fun (_, t) -> show t) targs) ^ "]" in
  match Hashtbl.find_opt g.instances key with
  | Some c -> c
  | None ->
    let safe = String.map (fun ch -> if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch = '_' then ch else '_') name in
    let c = if targs = [] then "wf_" ^ safe else sprintf "wf_%s_%d" safe (Hashtbl.length g.instances) in
    Hashtbl.replace g.instances key c;
    g.queue <- g.queue @ [ (c, Hashtbl.find g.fns name, targs) ];
    c

let lift g ~params ~ret ~body =
  g.counter <- g.counter + 1;
  let name = sprintf "wf_lift%d" g.counter in
  g.out <- { I.fname = name; params; captures = []; is_closure = true; ret; body } :: g.out;
  name

(* A top-level function or a variant constructor used as a value. *)
let wrapper g ty make =
  match ty with
  | TFn (ps, r) ->
    let params = List.map (fun t -> fresh_var "a" t) ps in
    let res = fresh_var "r" r in
    let body = [ I.Let (res, make (List.map (fun v -> I.V v) params)); I.Return (I.V res) ] in
    lift g ~params ~ret:r ~body
  | _ -> failwith "wrapper"

let not_local f n = lookup f n = None
let is_enum_name f n = not_local f n && Hashtbl.mem f.g.enums n

(* ---------- expressions ---------- *)

let rec lower f (e : expr) : I.atom =
  if f.dead then I.AUnit
  else
    let ty = tyof f e in
    match e.edesc with
    | EInt n -> I.AInt n
    | EFloat x -> I.AFloat x
    | EBool b -> I.ABool b
    | EUnit -> I.AUnit
    | EStr [] -> I.AStr ""
    | EStr [ SPLit s ] -> I.AStr s
    | EStr parts ->
      let ps =
        List.map
          (function
            | SPLit s -> I.IStr s
            | SPExpr x ->
              let a = lower f x in
              I.IVal (a, tyof f x))
          parts
      in
      tmp f TStr (I.Interp ps)
    | EVar n -> ( match lookup f n with Some v -> I.V v | None -> global_value f e n ty)
    | EList es ->
      let xs = List.map (lower f) es in
      tmp f ty (I.MkList (ty, xs))
    | EBin (((And | Or) as op), a, b) ->
      let x = lower f a in
      let r = fresh_var "b" TBool in
      emit f (I.Decl r);
      let rhs_branch, _, _ =
        in_block f (fun () ->
            let y = lower f b in
            emit f (I.Init (r, I.Atom y)))
      in
      let const_branch = [ I.Init (r, I.Atom (I.ABool (op = Or))) ] in
      emit f (if op = And then I.If (x, rhs_branch, const_branch) else I.If (x, const_branch, rhs_branch));
      I.V r
    | EBin (Else, a, b) ->
      (* `a ?? b`: the payload of Some/Ok (tag 0), else b, evaluated only then *)
      let x = lower f a in
      let at = tyof f a in
      let r = fresh_var "v" ty in
      emit f (I.Decl r);
      let has = tmp f TBool (I.IsTag (x, at, 0)) in
      let some_b, _, _ = in_block f (fun () -> emit f (I.Init (r, I.Payload (x, at, 0, 0)))) in
      let none_b, _, _ = in_block f (fun () -> let y = lower f b in emit f (I.Init (r, I.Atom y))) in
      emit f (I.If (has, some_b, none_b));
      I.V r
    | EBin (Range, a, b) ->
      let x = lower f a in
      let y = lower f b in
      tmp f ty (I.Prim ("int_range", TInt, [ x; y ], e.espan))
    | EBin (op, a, b) ->
      let x = lower f a in
      let y = lower f b in
      tmp f ty (I.Bin (op, tyof f a, x, y, e.espan))
    | EUn (Neg, a) ->
      let x = lower f a in
      tmp f ty (I.Neg (ty, x))
    | EUn (Not, a) ->
      let x = lower f a in
      tmp f TBool (I.Not x)
    | ECall (callee, args) -> lower_call f e callee args ty
    | ENamed _ -> failwith "internal: the checker puts named arguments in order"
    | EMethod ({ edesc = EVar en; _ }, name, _, args) when is_enum_name f en ->
      let xs = List.map (lower f) args in
      tmp f ty (I.MkEnum (ty, tag_of f.g ty name, xs))
    | EMethod (recv, "remove", _, [ k ]) when (match tyof f recv with TMap _ -> true | _ -> false) ->
      let v, steps = lower_place f recv in
      let ka = lower f k in
      emit f (I.Mutate (v, steps, I.MRemove ka));
      I.AUnit
    | EMethod (recv, "push", _, [ a ]) when (match tyof f recv with TList _ -> true | _ -> false) ->
      let v, steps = lower_place f recv in
      let x = lower f a in
      emit f (I.Mutate (v, steps, I.MPush x));
      I.AUnit
    | EMethod (recv, (("go" | "#go") as go), _, [ task ]) when (match tyof f recv with TGroup _ -> true | _ -> false) ->
      let g = lower f recv in
      let c = lower f task in
      let keep = go = "go" in
      let t = match tyof f recv with TGroup t when keep -> t | _ -> TUnit in
      ignore (tmp f TUnit (I.Spawn (g, c, t, keep)));
      I.AUnit
    | EMethod (recv, "send", _, [ v ]) when (match tyof f recv with TChan _ -> true | _ -> false) ->
      let c = lower f recv in
      let x = lower f v in
      ignore (tmp f TUnit (I.ChanSend (c, x, e.espan)));
      I.AUnit
    | EMethod (recv, name, _, args) -> lower_method f e recv name args ty
    | EField ({ edesc = EVar en; _ }, name, _) when is_enum_name f en -> ctor_value f name ty
    | EField _ | EIndex _ -> (
      let root, rty, steps = access f e in
      match steps with [] -> root | _ -> tmp f ty (I.Get (root, rty, steps)))
    | ETry (x, c) -> lower_try f x c
    | EIf (c, b, els) ->
      let ca = lower f c in
      let r = fresh_var "if" ty in
      emit f (I.Decl r);
      let s1, _, d1 =
        in_block f (fun () ->
            let a = lower_block_inner f b in
            emit f (I.Init (r, I.Atom a)))
      in
      let s2, _, d2 =
        in_block f (fun () ->
            match els with
            | Some x ->
              let a = lower f x in
              emit f (I.Init (r, I.Atom a))
            | None -> emit f (I.Init (r, I.Atom I.AUnit)))
      in
      emit f (I.If (ca, s1, s2));
      if d1 && d2 then f.dead <- true;
      I.V r
    | EMatch (s, arms) -> lower_match f e s arms ty
    | ESelect arms -> lower_select f e arms ty
    | EBlock b -> lower_block f b
    | EClosure (ps, body) -> lower_closure f ps body ty
    | EPipe _ -> failwith "pipes are expanded when modules load"
    | ESafe (base, x, rest, flat) ->
      (* `a?.b`: if a is Some(v), the rest of the chain on v (wrapped in Some
         unless it is an Option already), else None *)
      let a = lower f base in
      let bt = tyof f base in
      let inner = match bt with TNamed ("Option", [ t ]) -> t | _ -> TUnit in
      let r = fresh_var "safe" ty in
      emit f (I.Decl r);
      let has = tmp f TBool (I.IsTag (a, bt, 0)) in
      let some_b, _, d =
        in_block f (fun () ->
            let v = fresh_var x inner in
            emit f (I.Let (v, I.Payload (a, bt, 0, 0)));
            bind f x v;
            let c = lower f rest in
            emit f (I.Init (r, if flat then I.Atom c else I.MkEnum (ty, 0, [ c ]))))
      in
      let none_b, _, _ = in_block f (fun () -> emit f (I.Init (r, I.MkEnum (ty, 1, [])))) in
      emit f (I.If (has, some_b, none_b));
      ignore d;
      I.V r
    | EWith (base, fields) ->
      (* Take the base, then overwrite the fields in place. When the base is
         not used afterwards it is moved, so its storage is reused and the
         unchanged fields are not touched; otherwise the ownership pass
         retains it (a copy) first. A plain variable is read after the new
         values, so `p = { p with x: p.x + 1 }` still moves `p`. *)
      let bt = tyof f base in
      let early = match base.edesc with EVar _ -> None | _ -> Some (lower f base) in
      let vals = List.map (fun (fname, _, x) -> (field_index f.g bt fname, lower f x)) fields in
      let b = match early with Some a -> a | None -> lower f base in
      let v = fresh_var "with" bt in
      emit f (I.Let (v, I.Atom b));
      List.iter (fun (k, a) -> emit f (I.Mutate (v, [ I.PField k ], I.MSet a))) vals;
      I.V v
    | EStruct (name, _, fields) ->
      let vals = List.map (fun (fname, _, x) -> (fname, lower f x)) fields in
      let decl = Hashtbl.find f.g.structs name in
      tmp f ty (I.MkStruct (ty, List.map (fun (fn, _, _) -> List.assoc fn vals) decl.st_fields))
    | EDyn (x, i) ->
      let t = tyof f x in
      let a = lower f x in
      mk_dyn f t (match ty with TDyn (_, args) -> (i, args) | _ -> (i, [])) a
    | EGroup (name, interleaved, until, b) ->
      (* with a deadline, each result is an Option: None for a cancelled task *)
      let t = match (ty, until) with TList (TNamed ("Option", [ t ])), Some _ -> t | TList t, None -> t | _ -> TUnit in
      let dl = Option.map (fun (clk, ms) -> let c = lower f clk in (c, lower f ms)) until in
      let g = fresh_var name (TGroup t) in
      emit f (I.Let (g, I.GroupNew (dl, interleaved)));
      f.scopes <- Hashtbl.create 8 :: f.scopes;
      bind f name g;
      (if until = None then ignore (lower_block f b)
       else
         let body, _, _ = in_block f (fun () -> ignore (lower_block f b)) in
         emit f (I.Scope (I.V g, body)));
      f.scopes <- List.tl f.scopes;
      tmp f ty (I.GroupJoin (I.V g, ty, until <> None))

and global_value f e n ty =
  if Hashtbl.mem f.g.fns n then
    let c = instance f.g n (List.map (fun (k, t) -> (k, zonk f.genv t)) e.einst) in
    let w = wrapper f.g ty (fun args -> I.Call (c, args)) in
    tmp f ty (I.MkClosure (w, ty, []))
  else ctor_value f n ty

and ctor_value f vname ty =
  match ty with
  | TFn (_, rt) ->
    let tag = tag_of f.g rt vname in
    let w = wrapper f.g ty (fun args -> I.MkEnum (rt, tag, args)) in
    tmp f ty (I.MkClosure (w, ty, []))
  | _ -> tmp f ty (I.MkEnum (ty, tag_of f.g ty vname, []))

and lower_call f e callee args ty =
  match callee.edesc with
  | EVar "assert" when not_local f "assert" && not (Hashtbl.mem f.g.fns "assert") ->
    let c = lower f (List.hd args) in
    let fail, _, _ = in_block f (fun () -> ignore (tmp f TUnit (I.Panic (I.AStr "assertion failed", e.espan))); f.dead <- true) in
    emit f (I.If (c, [], fail));
    I.AUnit
  | EVar "assert_eq" when not_local f "assert_eq" && not (Hashtbl.mem f.g.fns "assert_eq") ->
    let x = lower f (List.nth args 0) in
    let y = lower f (List.nth args 1) in
    let t = tyof f (List.nth args 0) in
    let same = tmp f TBool (I.Bin (Eq, t, x, y, e.espan)) in
    let fail, _, _ =
      in_block f (fun () ->
          let msg = tmp f TStr (I.Interp [ I.IStr "assertion failed: "; I.IValQ (x, t); I.IStr " != "; I.IValQ (y, t) ]) in
          ignore (tmp f TUnit (I.Panic (msg, e.espan)));
          f.dead <- true)
    in
    emit f (I.If (same, [], fail));
    I.AUnit
  | EVar "channel" when not_local f "channel" && not (Hashtbl.mem f.g.fns "channel") ->
    let cap = lower f (List.hd args) in
    tmp f ty (I.ChanNew (ty, cap, e.espan))
  | EVar "catch" when not_local f "catch" && not (Hashtbl.mem f.g.fns "catch") ->
    let c = lower f (List.hd args) in
    let inner = match tyof f (List.hd args) with TFn (_, r) -> r | _ -> TUnit in
    tmp f ty (I.Catch (c, inner))
  | EVar "keep" when not_local f "keep" && not (Hashtbl.mem f.g.fns "keep") ->
    let x = lower f (List.hd args) in
    tmp f TUnit (I.Prim ("keep", tyof f (List.hd args), [ x ], e.espan))
  | EVar "bytes" when not_local f "bytes" && not (Hashtbl.mem f.g.fns "bytes") ->
    let xs = List.map (lower f) args in
    tmp f ty (I.Prim ("bytes_new", TBytes, xs, e.espan))
  | EVar "map" when not_local f "map" && not (Hashtbl.mem f.g.fns "map") ->
    let xs = List.map (lower f) args in
    tmp f ty (I.Prim ((if xs = [] then "map_new" else "map_from"), ty, xs, e.espan))
  | EVar "panic" when not_local f "panic" && not (Hashtbl.mem f.g.fns "panic") ->
    let m = lower f (List.hd args) in
    let r = tmp f ty (I.Panic (m, e.espan)) in
    f.dead <- true;
    r
  | EVar n when not_local f n && (match Hashtbl.find_opt f.g.fns n with Some { fn_foreign = Some _; _ } -> true | _ -> false) ->
    let cs =
      match Ffi.of_decl ~structs:(Hashtbl.find_opt f.g.structs) (Hashtbl.find f.g.fns n) with
      | Ok cs -> cs
      | Error _ -> failwith "lower: foreign signature"
    in
    (* a callback is a top-level function (the checker made sure): C calls
       its compiled code through a wrapper, so it is not lowered as a value *)
    let kinds = (if cs.trusted then [] else [ Ffi.Unit ]) @ List.filter_map (fun (p : Ffi.param) -> if p.out then None else Some p.cty) cs.params in
    let xs, cbs =
      List.fold_left2
        (fun (xs, cbs) (a : expr) k ->
          match (k, a.edesc) with
          | Ffi.Fn (ps, _), EVar m ->
            let d = Hashtbl.find f.g.fns m in
            (xs, cbs @ [ (instance f.g m [], m, d.fn_span, List.length d.fn_params > List.length ps) ])
          | _ -> (xs @ [ lower f a ], cbs))
        ([], []) args kinds
    in
    tmp f ty (I.CallC (cs, xs, cbs, e.espan))
  | EVar n when not_local f n && Hashtbl.mem f.g.fns n ->
    let c = instance f.g n (List.map (fun (k, t) -> (k, zonk f.genv t)) callee.einst) in
    let xs = List.map (lower f) args in
    tmp f ty (I.Call (c, xs))
  | EVar n when not_local f n && Hashtbl.mem f.g.ctors n ->
    let xs = List.map (lower f) args in
    tmp f ty (I.MkEnum (ty, tag_of f.g ty n, xs))
  | EField ({ edesc = EVar en; _ }, v, _) when is_enum_name f en ->
    let xs = List.map (lower f) args in
    tmp f ty (I.MkEnum (ty, tag_of f.g ty v, xs))
  | _ ->
    let c = lower f callee in
    let xs = List.map (lower f) args in
    tmp f ty (I.CallClo (c, xs))

(* A chain of field and index reads, so `a[i].x` is one Get, not two copies. *)
and access f e : I.atom * ty * I.step list =
  match e.edesc with
  | EField (r, name, _) when is_struct f.g (tyof f r) ->
    let a, rt, steps = access f r in
    (a, rt, steps @ [ I.PField (field_index f.g (tyof f r) name) ])
  | EIndex (r, i) when tyof f r = TBytes ->
    let b = lower f r in
    let ia = lower f i in
    (tmp f TInt (I.Prim ("bytes_at", TBytes, [ b; ia ], e.espan)), TInt, [])
  | EIndex (r, k) when (match tyof f r with TMap _ -> true | _ -> false) ->
    (* a map lookup starts a new path: the value it finds *)
    let m = lower f r in
    let ka = lower f k in
    let vt = tyof f e in
    (tmp f vt (I.Prim ("map_at", tyof f r, [ m; ka ], e.espan)), vt, [])
  | EIndex (r, i) ->
    let a, rt, steps = access f r in
    let ia = lower f i in
    (a, rt, steps @ [ I.PIndex (ia, e.espan) ])
  | EField (r, name, _) when tyof f r = TCap Root ->
    let a = lower f r in
    (tmp f (tyof f e) (I.RootField (a, name)), tyof f e, [])
  | _ -> (lower f e, tyof f e, [])

and lower_place f e : I.var * I.step list =
  match e.edesc with
  | EVar n -> ( match lookup f n with Some v -> (v, []) | None -> failwith ("place " ^ n))
  | EField (r, name, _) ->
    let v, s = lower_place f r in
    (v, s @ [ I.PField (field_index f.g (tyof f r) name) ])
  | EIndex (r, k) when (match tyof f r with TMap _ -> true | _ -> false) ->
    (* `m[k].x = v`: the entry, changed in place *)
    let v, s = lower_place f r in
    let ka = lower f k in
    (v, s @ [ I.PKey (ka, e.espan) ])
  | EIndex (r, i) ->
    let v, s = lower_place f r in
    let ia = lower f i in
    (v, s @ [ I.PIndex (ia, e.espan) ])
  | _ -> failwith "place"

(* Box [a] of type [t] as `dyn i`: a vtable of t's methods. *)
and mk_dyn f t (i, args) a =
  let ms = Check.iface_methods (i, args) in
  let entries = List.map (fun (m, (ps, r)) -> (method_fn f t m ps, ps, r)) ms in
  (* the vtable is per interface and arguments *)
  let key = if args = [] then i else show (TDyn (i, args)) in
  tmp f (TDyn (i, args)) (I.MkDyn (t, key, a, entries))

(* The function that is method [m] of type [t], receiver first. *)
and method_fn f t m ps =
  let key =
    match t with
    | TNamed (n, _) when Hashtbl.mem f.g.fns (n ^ "::" ^ m) -> n ^ "::" ^ m
    | TStr when m = "message" -> "$Str::message"
    | TNamed ("IoError", _) when m = "message" -> "$IoError::message"
    | _ when m = "cause" -> "$no_cause" (* `cause` is optional in `Error` *)
    | _ -> failwith ("no method " ^ m)
  in
  let d = Hashtbl.find f.g.fns key in
  instance f.g key (derive_targs d (t :: ps))

and lower_try f x context =
  let a = lower f x in
  let xt = tyof f x in
  let fn_err = match f.ret with TNamed ("Result", [ _; e ]) -> Some e | _ -> None in
  (* the error the function returns: converted to `dyn Error` if it returns
     one, and wrapped with the context if there is one *)
  let to_ret_err ev et =
    let ev = match (fn_err, et) with Some (TDyn ("Error", [])), TDyn ("Error", []) -> ev | Some (TDyn ("Error", [])), _ -> mk_dyn f et ("Error", []) ev | _ -> ev in
    match context with
    | None -> ev
    | Some c ->
      let ca = lower f c in
      let wt = TNamed ("WrappedError", []) in
      mk_dyn f wt ("Error", []) (tmp f wt (I.MkStruct (wt, [ ca; ev ])))
  in
  let ok_ty, fail =
    match xt with
    | TNamed ("Result", [ t; et ]) ->
      ( t,
        fun () ->
          let ev = tmp f et (I.Payload (a, xt, 1, 0)) in
          tmp f f.ret (I.MkEnum (f.ret, 1, [ to_ret_err ev et ])) )
    | TNamed ("Option", [ t ]) when context <> None ->
      (* `opt? "context"`: the context is the error *)
      let c = Option.get context in
      (t, fun () -> let ca = lower f c in tmp f f.ret (I.MkEnum (f.ret, 1, [ mk_dyn f TStr ("Error", []) ca ])))
    | TNamed ("Option", [ t ]) -> (t, fun () -> tmp f f.ret (I.MkEnum (f.ret, 1, [])))
    | _ -> failwith "try"
  in
  let ok = tmp f TBool (I.IsTag (a, xt, 0)) in
  let fail_block, _, _ =
    in_block f (fun () ->
        let r = fail () in
        run_defers f f.defers;
        emit f (I.Return r);
        f.dead <- true)
  in
  emit f (I.If (ok, [], fail_block));
  tmp f ok_ty (I.Payload (a, xt, 0, 0))

and lower_match f e s arms ty =
  let sa = lower f s in
  let st = tyof f s in
  let r = fresh_var "m" ty in
  emit f (I.Decl r);
  let rec chain = function
    | [] ->
      ignore (tmp f TUnit (I.Panic (I.AStr "no `match` arm matched", e.espan)));
      f.dead <- true
    | arm :: rest ->
      let body () =
        bind_pat f sa st arm.apat;
        let a = lower f arm.abody in
        emit f (I.Init (r, I.Atom a))
      in
      if irrefutable arm.apat then begin
        let b, _, d = in_block f body in
        List.iter (emit f) b;
        if d then f.dead <- true
      end
      else begin
        let t = tmp f TBool (I.PatTest (sa, st, arm.apat)) in
        let b1, _, d1 = in_block f body in
        let b2, _, d2 = in_block f (fun () -> chain rest) in
        emit f (I.If (t, b1, b2));
        if d1 && d2 then f.dead <- true
      end
  in
  if List.for_all (fun a -> a.aguard = None) arms then chain arms
  else begin
    (* With guards, first find the arm: [k] is the index of the first arm
       whose pattern matches and whose guard holds (-1 for none), trying
       each arm only while none has been found. Then run that arm's body.
       Each arm's bindings are made again for its body; this keeps the
       code linear in the number of arms. *)
    let k = fresh_var "arm" TInt in
    emit f (I.Let (k, I.Atom (I.AInt (-1L))));
    List.iteri
      (fun i arm ->
        let set () = emit f (I.Set (k, I.Atom (I.AInt (Int64.of_int i)), ref false)) in
        let applies () =
          match arm.aguard with
          | None -> set ()
          | Some g ->
            bind_pat f sa st arm.apat;
            let ga = lower f g in
            let yes, _, _ = in_block f set in
            emit f (I.If (ga, yes, []))
        in
        let try_arm () =
          let b, _, _ = in_block f applies in
          if irrefutable arm.apat then List.iter (emit f) b
          else
            let t = tmp f TBool (I.PatTest (sa, st, arm.apat)) in
            emit f (I.If (t, b, []))
        in
        if i = 0 then try_arm ()
        else
          let pending = tmp f TBool (I.Bin (Eq, TInt, I.V k, I.AInt (-1L), e.espan)) in
          let b, _, _ = in_block f try_arm in
          emit f (I.If (pending, b, [])))
      arms;
    let rec dispatch i = function
      | [] ->
        ignore (tmp f TUnit (I.Panic (I.AStr "no `match` arm matched", e.espan)));
        f.dead <- true
      | arm :: rest ->
        let t = tmp f TBool (I.Bin (Eq, TInt, I.V k, I.AInt (Int64.of_int i), e.espan)) in
        let b1, _, d1 =
          in_block f (fun () ->
              bind_pat f sa st arm.apat;
              let a = lower f arm.abody in
              emit f (I.Init (r, I.Atom a)))
        in
        let b2, _, d2 = in_block f (fun () -> dispatch (i + 1) rest) in
        emit f (I.If (t, b1, b2));
        if d1 && d2 then f.dead <- true
    in
    dispatch 0 arms
  end;
  I.V r

and lower_select f e arms ty =
  f.g.counter <- f.g.counter + 1;
  let id = f.g.counter in
  (* channels and send values first, in order *)
  let ops =
    List.filter_map
      (fun a ->
        let on = match a.sguard with Some g -> lower f g | None -> I.ABool true in
        match a.sop with
        | SelRecv (_, ch) ->
          let el = match tyof f ch with TChan t -> t | _ -> TUnit in
          Some (I.SRecv (lower f ch, el, on))
        | SelSend (ch, v) ->
          let el = match tyof f ch with TChan t -> t | _ -> TUnit in
          let ca = lower f ch in
          Some (I.SSend (ca, lower f v, el, on))
        | SelAfter (clk, ms) ->
          let ca = lower f clk in
          Some (I.SAfter (ca, lower f ms, on))
        | SelDefault -> None)
      arms
  in
  let has_default = List.exists (fun a -> a.sop = SelDefault) arms in
  let k = tmp f TInt (I.Select (id, ops, has_default, e.espan)) in
  let r = fresh_var "s" ty in
  emit f (I.Decl r);
  (* number the channel arms; `_` is -1 *)
  let numbered =
    let n = ref 0 in
    List.map (fun a -> if a.sop = SelDefault then (-1, a) else (let i = !n in incr n; (i, a))) arms
  in
  let rec chain = function
    | [] -> ()
    | (i, a) :: rest ->
      let body () =
        (match a.sop with
         | SelRecv (b, ch) ->
           (* always take the value out, so an unnamed one is released *)
           let el = match tyof f ch with TChan t -> t | _ -> TUnit in
           let ot = TNamed ("Option", [ el ]) in
           let v = fresh_var (match b with Some (n, _) -> n | None -> "got") ot in
           emit f (I.Let (v, I.SelGot (id, i, ot)));
           Option.iter (fun (name, _) -> bind f name v) b
         | _ -> ());
        let x = lower f a.sbody in
        emit f (I.Init (r, I.Atom x))
      in
      if rest = [] then begin
        let b, _, d = in_block f body in
        List.iter (emit f) b;
        if d then f.dead <- true
      end
      else begin
        let t = tmp f TBool (I.Bin (Eq, TInt, k, I.AInt (Int64.of_int i), e.espan)) in
        let b1, _, d1 = in_block f body in
        let b2, _, d2 = in_block f (fun () -> chain rest) in
        emit f (I.If (t, b1, b2));
        if d1 && d2 then f.dead <- true
      end
  in
  chain numbered;
  I.V r

(* Bind the names in [p], which matches [a] of type [t]. *)
and bind_pat f a t (p : pat) =
  let has_bind p = Free.pat_binds p [] <> [] in
  match p.pdesc with
  | PBind n ->
    let v = fresh_var n t in
    emit f (I.Let (v, I.Atom a));
    bind f n v
  | PVariant (_, vn, args) ->
    let tag = tag_of f.g t vn in
    let ptys = List.assoc vn (variants f.g t) in
    List.iteri
      (fun i (sub, pt) ->
        if has_bind sub then
          let pv = tmp f pt (I.Payload (a, t, tag, i)) in
          bind_pat f pv pt sub)
      (List.combine (fill_rest args (List.length ptys)) ptys)
  | PTuple ps ->
    let fts = field_types f.g t in
    List.iteri
      (fun i (sub, (_, ft)) ->
        if has_bind sub then
          let pv = tmp f ft (I.Get (a, t, [ I.PField i ])) in
          bind_pat f pv ft sub)
      (List.combine (fill_rest ps (List.length fts)) fts)
  | PList ps when has_bind p ->
    let el = match t with TList el -> el | _ -> failwith "bind_pat: list" in
    let before, rest, after = list_parts ps in
    let item i sub = if has_bind sub then bind_pat f (tmp f el (I.Get (a, t, [ I.PIndexU i ]))) el sub in
    List.iteri (fun i sub -> item (I.AInt (Int64.of_int i)) sub) before;
    if rest <> None || after <> [] then begin
      let n = tmp f TInt (I.Prim ("list_len", t, [ a ], p.pspan)) in
      let j = List.length after in
      List.iteri (fun i sub -> if has_bind sub then item (tmp f TInt (I.Bin (Sub, TInt, n, I.AInt (Int64.of_int (j - i)), p.pspan))) sub) after;
      match rest with
      | Some (Some name) ->
        let hi = tmp f TInt (I.Bin (Sub, TInt, n, I.AInt (Int64.of_int j), p.pspan)) in
        let v = fresh_var name t in
        emit f (I.Let (v, I.Prim ("list_slice", t, [ a; I.AInt (Int64.of_int (List.length before)); hi ], p.pspan)));
        bind f name v
      | _ -> ()
    end
  | POr alts when has_bind p ->
    (* the names come from whichever alternative matched: each gets a
       variable declared here, set in the branch for that alternative *)
    let names = List.sort_uniq compare (Free.pat_binds p []) in
    let outs = Hashtbl.create 4 in
    let branch alt =
      let b, _, _ =
        in_block f (fun () ->
            bind_pat f a t alt;
            List.iter
              (fun n ->
                let v = Hashtbl.find (List.hd f.scopes) n in
                let o = match Hashtbl.find_opt outs n with Some o -> o | None -> let o = fresh_var n v.I.ty in Hashtbl.replace outs n o; o in
                emit f (I.Init (o, I.Atom (I.V v))))
              names)
      in
      (alt, b)
    in
    let branches = List.map branch alts in
    List.iter (fun n -> emit f (I.Decl (Hashtbl.find outs n))) names;
    let rec chain = function
      | [] -> ()
      | [ (_, b) ] -> List.iter (emit f) b
      | (alt, b) :: rest ->
        let c = tmp f TBool (I.PatTest (a, t, alt)) in
        let other, _, _ = in_block f (fun () -> chain rest) in
        emit f (I.If (c, b, other))
    in
    chain branches;
    List.iter (fun n -> bind f n (Hashtbl.find outs n)) names
  | _ -> ()

(* The type arguments of generic function [d] called with arguments of the
   given concrete types. *)
and derive_targs d act =
  let found = ref [] in
  let rec m decl act =
    match (decl, act) with
    | TParam g, t -> if not (List.mem_assoc g !found) then found := (g, t) :: !found
    | TList a, TList b | TChan a, TChan b -> m a b
    | TArray (a, n), TArray (b, k) -> m a b; m n k
    | TNamed (_, xs), TNamed (_, ys) when List.length xs = List.length ys -> List.iter2 m xs ys
    | TFn (xs, r1), TFn (ys, r2) when List.length xs = List.length ys -> List.iter2 m xs ys; m r1 r2
    | _ -> ()
  in
  let genv = List.map (fun g -> (g, TParam g)) d.fn_gen in
  let decl = List.map (fun p -> resolve genv p.prm_ty) d.fn_params in
  if List.length decl = List.length act then List.iter2 m decl act;
  List.map (fun g -> (g, Option.value ~default:TUnit (List.assoc_opt g !found))) d.fn_gen

and lower_method f e recv name args ty =
  let rt = tyof f recv in
  match rt with
  | TDyn (i, targs) ->
    (* through the vtable *)
    let ms = Check.iface_methods (i, targs) in
    let rec index k = function [] -> failwith "dyn method" | (m, sg) :: rest -> if m = name then (k, sg) else index (k + 1) rest in
    let k, (ps, r) = index 0 ms in
    let ra = lower f recv in
    let xs = List.map (lower f) args in
    tmp f ty (I.CallDyn (k, ps, r, ra, xs))
  | (TStr | TNamed ("IoError", _)) when name = "message" ->
    let c = method_fn f rt name [] in
    tmp f ty (I.Call (c, [ lower f recv ]))
  | (TStr | TNamed _) when name = "cause" && args = [] && (match rt with TNamed (n, _) -> not (Hashtbl.mem f.g.fns (n ^ "::cause")) | _ -> true) ->
    (* reached through an `Error` bound on a type without a `cause` *)
    let c = method_fn f rt name [] in
    tmp f ty (I.Call (c, [ lower f recv ]))
  | TNamed (n, _) when Hashtbl.mem f.g.fns (n ^ "::" ^ name) ->
    (* a method on a user type: a call with the receiver first *)
    let key = n ^ "::" ^ name in
    let d = Hashtbl.find f.g.fns key in
    let env = List.map (fun (k, t) -> (k, zonk f.genv t)) e.einst in
    let targs =
      if List.for_all (fun g -> List.mem_assoc g env) d.fn_gen then env
      else begin
        (* reached through an interface bound, where the checker only knew a
           type parameter: read the instantiation off the actual types *)
        derive_targs d (List.map (tyof f) (recv :: args))
      end
    in
    let c = instance f.g key targs in
    let xs = List.map (lower f) (recv :: args) in
    tmp f ty (I.Call (c, xs))
  | _ ->
  let sp = e.espan in
  let branch_init r cond then_ else_ =
    let s1, _, _ = in_block f (fun () -> emit f (I.Init (r, then_ ()))) in
    let s2, _, _ = in_block f (fun () -> emit f (I.Init (r, else_ ()))) in
    emit f (I.If (cond, s1, s2));
    I.V r
  in
  match (rt, name, args) with
  | TNamed ("Option", _), "is_some", [] -> tmp f TBool (I.IsTag (lower f recv, rt, 0))
  | TNamed ("Option", _), "is_none", [] -> tmp f TBool (I.IsTag (lower f recv, rt, 1))
  | TNamed ("Result", _), "is_ok", [] -> tmp f TBool (I.IsTag (lower f recv, rt, 0))
  | TNamed ("Result", _), "is_err", [] -> tmp f TBool (I.IsTag (lower f recv, rt, 1))
  | TNamed ("Option", [ _ ]), "map", [ fn ] ->
    let ra = lower f recv in
    let ca = lower f fn in
    let c = tmp f TBool (I.IsTag (ra, rt, 0)) in
    let r = fresh_var "u" ty in
    emit f (I.Decl r);
    let ut = match ty with TNamed (_, [ u ]) -> u | _ -> TUnit in
    let xt = match rt with TNamed (_, [ x ]) -> x | _ -> TUnit in
    branch_init r c
      (fun () ->
        let p = tmp f xt (I.Payload (ra, rt, 0, 0)) in
        let y = tmp f ut (I.CallClo (ca, [ p ])) in
        I.MkEnum (ty, 0, [ y ]))
      (fun () -> I.MkEnum (ty, 1, []))
  | TNamed ("Result", [ at; et ]), "map_error", [ fn ] ->
    let ra = lower f recv in
    let ca = lower f fn in
    let c = tmp f TBool (I.IsTag (ra, rt, 1)) in
    let r = fresh_var "u" ty in
    emit f (I.Decl r);
    let ut = match ty with TNamed (_, [ _; u ]) -> u | _ -> TUnit in
    branch_init r c
      (fun () ->
        let p = tmp f et (I.Payload (ra, rt, 1, 0)) in
        let y = tmp f ut (I.CallClo (ca, [ p ])) in
        I.MkEnum (ty, 1, [ y ]))
      (fun () ->
        let p = tmp f at (I.Payload (ra, rt, 0, 0)) in
        I.MkEnum (ty, 0, [ p ]))
  | TList _, ("map" | "filter" | "fold"), _ ->
    (* Fuse a chain like xs.filter(p).map(g).fold(z, h) into one loop. Only
       pure closure literals are fused into earlier stages: they capture no
       capability and no function, so running them interleaved instead of
       stage by stage cannot be observed (except which panic comes first). *)
    let src_e, stages = stages_of f recv in
    let src = lower f src_e in
    let stages = List.map (fun (k, fn, t) -> (k, prepare f fn, t)) stages in
    let elem = function TList t -> t | _ -> TUnit in
    let run_stages x terminal =
      let rec go (v : I.atom) vty = function
        | [] -> terminal v vty
        | ("map", p, t) :: rest ->
          let y = apply f p [ v ] (elem t) in
          go y (elem t) rest
        | (_, p, _) :: rest ->
          let keep = apply f p [ v ] TBool in
          let blk, _, _ = in_block f (fun () -> go v vty rest) in
          emit f (I.If (keep, blk, []))
      in
      go (I.V x) (elem (tyof f src_e)) stages
    in
    (match (name, args) with
     | "fold", [ init; fn ] ->
       let ia = lower f init in
       let p = prepare f fn in
       let acc = fresh_var "acc" ty in
       emit f (I.Let (acc, I.Atom ia));
       emit_for f src (tyof f src_e) sp (fun x ->
           run_stages x (fun v _ ->
               let r = apply f p [ I.V acc; v ] ty in
               emit f (I.Set (acc, I.Atom r, ref false))));
       I.V acc
     | _, [ fn ] ->
       let p = prepare f fn in
       let out = fresh_var "out" ty in
       emit f (I.Let (out, I.MkList (ty, [])));
       emit_for f src (tyof f src_e) sp (fun x ->
           run_stages x (fun v _ ->
               if name = "map" then begin
                 let y = apply f p [ v ] (elem ty) in
                 emit f (I.Mutate (out, [], I.MPush y))
               end
               else begin
                 let keep = apply f p [ v ] TBool in
                 let push, _, _ = in_block f (fun () -> emit f (I.Mutate (out, [], I.MPush v))) in
                 emit f (I.If (keep, push, []))
               end));
       I.V out
     | _ -> failwith "list method arity")
  | _ ->
    let ra = lower f recv in
    let xs = List.map (lower f) args in
    let prefix =
      match rt with
      | TStr -> "str_"
      | TBytes -> "bytes_"
      | TInt -> "int_"
      | TFloat -> "float_"
      | TBool -> "bool_"
      | TList _ -> "list_"
      | TMap _ -> "map_"
      | TArray _ -> "array_"
      | TChan _ -> "chan_"
      | TCap _ -> "cap_"
      | _ -> failwith ("method on " ^ show rt)
    in
    tmp f ty (I.Prim (prefix ^ name, rt, ra :: xs, sp))

(* Split `src.filter(a).map(b)` into the source and its fusable stages. *)
and stages_of f e =
  match e.edesc with
  | EMethod (r, (("map" | "filter") as k), _, [ fn ])
    when (match tyof f r with TList _ -> true | _ -> false) && pure_closure f fn ->
    let src, st = stages_of f r in
    (src, st @ [ (k, fn, tyof f e) ])
  | _ -> (e, [])

(* A closure literal is inlined where it is applied (no closure object, no
   indirect call) unless its body can return early from itself. *)
and prepare f fn =
  match fn.edesc with
  | EClosure (ps, body) when not (has_escape body) -> `Inline (List.map (fun (n, _, _) -> n) ps, body)
  | _ -> `Value (lower f fn)

and apply f prep args ret_ty =
  match prep with
  | `Value c -> tmp f ret_ty (I.CallClo (c, args))
  | `Inline (ps, body) ->
    f.scopes <- Hashtbl.create 8 :: f.scopes;
    List.iter2
      (fun n a ->
        let ty = match a with I.V v | I.VDup v -> v.ty | I.AInt _ -> TInt | I.AFloat _ -> TFloat | I.ABool _ -> TBool | I.AUnit -> TUnit | I.AStr _ -> TStr in
        let v = fresh_var n ty in
        emit f (I.Let (v, I.Atom a));
        bind f n v)
      ps args;
    let r = lower f body in
    f.scopes <- List.tl f.scopes;
    r

(* Pure: a closure literal whose free variables hold no capability and no
   function value. It can then only compute, which is what fusion needs. *)
and pure_closure f fn =
  match fn.edesc with
  | EClosure (ps, body) when not (has_escape body) ->
    let free = Free.expr (List.map (fun (n, _, _) -> n) ps) body in
    List.for_all
      (fun n ->
        match lookup f n with
        | Some (v : I.var) -> caps_in v.ty [] = [] && not (has_fn v.ty) && not (has_chan v.ty)
        | None -> true (* top-level function or constructor *))
      free
  | _ -> false

and has_escape body = Free.has_escape body

(* for x in list: iterate over a snapshot of the list by index. *)
and emit_for f la lty sp body =
  let elt = match lty with TList t | TArray (t, _) -> t | _ -> TUnit in
  let it = fresh_var "it" lty in
  emit f (I.Let (it, I.Atom la));
  let i = fresh_var "i" TInt in
  emit f (I.Let (i, I.Atom (I.AInt 0L)));
  let stmts, _, _ =
    in_block f (fun () ->
        let n = tmp f TInt (I.Prim ("list_len", lty, [ I.V it ], sp)) in
        let c = tmp f TBool (I.Bin (Ge, TInt, I.V i, n, sp)) in
        let brk, _, _ = in_block f (fun () -> emit f I.Break; f.dead <- true) in
        emit f (I.If (c, brk, []));
        let x = fresh_var "x" elt in
        (* i < len was checked just above, on a snapshot nothing else can change *)
        emit f (I.Let (x, I.Get (I.V it, lty, [ I.PIndexU (I.V i) ])));
        emit f (I.Set (i, I.Bin (Add, TInt, I.V i, I.AInt 1L, sp), ref false));
        body x)
  in
  emit f (I.Loop stmts)

and lower_closure f ps body ty =
  let pts, rt = match ty with TFn (a, b) -> (a, b) | _ -> failwith "closure type" in
  let cf = new_fctx f.g f.genv rt (Some f) in
  let params =
    List.map2
      (fun (n, _, _) t ->
        let v = fresh_var n t in
        bind cf n v;
        v)
      ps pts
  in
  let a = lower cf body in
  emit cf (I.Return a);
  f.g.counter <- f.g.counter + 1;
  let name = sprintf "wf_clo%d" f.g.counter in
  let caps = !(cf.caps) in
  f.g.out <-
    { I.fname = name; params; captures = List.map snd caps; is_closure = true; ret = rt; body = List.rev cf.buf }
    :: f.g.out;
  tmp f ty (I.MkClosure (name, ty, List.map (fun ((o : I.var), _) -> I.V o) caps))

and lower_block f b =
  f.scopes <- Hashtbl.create 8 :: f.scopes;
  let a = lower_block_inner f b in
  f.scopes <- List.tl f.scopes;
  a

and lower_block_inner f b = lower_stmts f b.stmts b.tail

and lower_stmts f ss tail =
  match ss with
  | [] -> ( match tail with Some e -> lower f e | None -> I.AUnit)
  | { sdesc = SDefer d; _ } :: rest when not f.dead -> lower_defer f d rest tail
  | s :: rest ->
    lower_stmt f s;
    lower_stmts f rest tail

(* `defer d` followed by [rest]: the rest runs under a guard whose handler is
   [d] (for panics and cancellations); every normal way out of the rest runs
   [d] itself after popping the guard: the end of the block here, and
   return, `?`, break and continue through run_defers. *)
and lower_defer f d rest tail =
  incr guard_counter;
  let fr = { dbody = d; dscopes = List.map Hashtbl.copy f.scopes; dloop = f.loop_depth; did = !guard_counter; douter = f.defers } in
  let r = fresh_var "dr" (match tail with Some e -> tyof f e | None -> TUnit) in
  emit f (I.Decl r);
  let handler, _, _ = in_block f (fun () -> lower_defer_body f fr) in
  let body, _, dead =
    in_block f (fun () ->
        f.defers <- fr :: f.defers;
        let a = lower_stmts f rest tail in
        f.defers <- fr.douter;
        if not f.dead then begin
          emit f (I.GuardEnd fr.did);
          lower_defer_body f fr;
          emit f (I.Init (r, I.Atom a))
        end)
  in
  emit f (I.Guard (fr.did, handler, body));
  if dead then f.dead <- true;
  I.V r

(* The deferred body, in the scope of its defer, with only the defers
   around it still pending. *)
and lower_defer_body f fr =
  let saved_scopes = f.scopes and saved_defers = f.defers in
  f.scopes <- fr.dscopes;
  f.defers <- fr.douter;
  ignore (lower_block f fr.dbody);
  f.scopes <- saved_scopes;
  f.defers <- saved_defers

(* Leaving the given defers' blocks normally: innermost first, pop each
   guard and run its body. *)
and run_defers f frames =
  List.iter
    (fun fr ->
      emit f (I.GuardEnd fr.did);
      lower_defer_body f fr)
    frames

(* the defers inside the innermost loop, which break and continue leave *)
and loop_defers f = List.filter (fun fr -> fr.dloop >= f.loop_depth) f.defers

and lower_stmt f s =
  if not f.dead then
    match s.sdesc with
    | SLetPat _ | SForPat _ -> failwith "destructuring is expanded when modules load"
    | SLet (_, n, _, _, e) ->
      let a = lower f e in
      let v = fresh_var n (tyof f e) in
      emit f (I.Let (v, I.Atom a));
      bind f n v
    | SAssign ({ edesc = EVar n; _ }, rhs) ->
      let a = lower f rhs in
      let v = match lookup f n with Some v -> v | None -> failwith "assign" in
      emit f (I.Set (v, I.Atom a, ref false))
    | SAssign ({ edesc = EIndex (m, k); _ }, rhs) when (match tyof f m with TMap _ -> true | _ -> false) ->
      (* `m[k] = v` inserts or replaces *)
      let a = lower f rhs in
      let ka = lower f k in
      let v, steps = lower_place f m in
      emit f (I.Mutate (v, steps, I.MPut (ka, a)))
    | SAssign (lhs, rhs) ->
      let a = lower f rhs in
      let v, steps = lower_place f lhs in
      emit f (I.Mutate (v, steps, I.MSet a))
    | SExpr e -> ignore (lower f e)
    | SReturn eo ->
      let a = match eo with Some e -> lower f e | None -> I.AUnit in
      run_defers f f.defers;
      emit f (I.Return a);
      f.dead <- true
    | SWhile (c, b) ->
      let body, _, _ =
        in_block f (fun () ->
            let ca = lower f c in
            let brk, _, _ = in_block f (fun () -> emit f I.Break; f.dead <- true) in
            emit f (I.If (ca, [], brk));
            in_loop f (fun () -> ignore (lower_block f b)))
      in
      emit f (I.Loop body)
    | SFor (n, _, { edesc = EBin (Range, lo, hi); espan; _ }, b) ->
      (* count from lo while below hi; the list is never built *)
      let la = lower f lo in
      let ha = lower f hi in
      let i = fresh_var "i" TInt in
      emit f (I.Let (i, I.Atom la));
      let h = fresh_var "hi" TInt in
      emit f (I.Let (h, I.Atom ha));
      let stmts, _, _ =
        in_block f (fun () ->
            let c = tmp f TBool (I.Bin (Ge, TInt, I.V i, I.V h, espan)) in
            let brk, _, _ = in_block f (fun () -> emit f I.Break; f.dead <- true) in
            emit f (I.If (c, brk, []));
            let x = fresh_var n TInt in
            emit f (I.Let (x, I.Atom (I.V i)));
            (* i < hi, so i + 1 cannot overflow *)
            emit f (I.Set (i, I.Bin (Add, TInt, I.V i, I.AInt 1L, espan), ref false));
            bind f n x;
            in_loop f (fun () -> ignore (lower_block f b)))
      in
      emit f (I.Loop stmts)
    | SFor (n, _, e, b) ->
      let la = lower f e in
      emit_for f la (tyof f e) e.espan (fun x ->
          bind f n x;
          in_loop f (fun () -> ignore (lower_block f b)))
    | SBreak -> run_defers f (loop_defers f); emit f I.Break; f.dead <- true
    | SContinue -> run_defers f (loop_defers f); emit f I.Continue; f.dead <- true
    | SDefer _ -> failwith "defer is lowered with its block"

let lower_fn g c (d : fn_decl) env =
  let ret = match d.fn_ret with Some te -> resolve env te | None -> TUnit in
  let f = new_fctx g env ret None in
  let params =
    List.map
      (fun p ->
        let v = fresh_var p.prm_name (resolve env p.prm_ty) in
        bind f p.prm_name v;
        v)
      d.fn_params
  in
  let a = lower_block_inner f d.fn_body in
  emit f (I.Return a);
  { I.fname = c; params; captures = []; is_closure = false; ret; body = List.rev f.buf }

let lower_program ?(tests = false) (user_items : item list) : I.program * gctx =
  let g =
    { fns = Hashtbl.create 16; structs = Hashtbl.create 16; enums = Hashtbl.create 16;
      ctors = Hashtbl.create 16; instances = Hashtbl.create 16; queue = []; out = []; counter = 0 }
  in
  let owners = Hashtbl.create 16 in
  List.iter
    (function
      | IIface _ | IType _ -> ()
      | IFn d -> Hashtbl.replace g.fns d.fn_name d
      | IStruct s -> Hashtbl.replace g.structs s.st_name s
      | IEnum e ->
        Hashtbl.replace g.enums e.en_name e;
        List.iter
          (fun (v, _, _) -> Hashtbl.replace owners v (e.en_name :: Option.value ~default:[] (Hashtbl.find_opt owners v)))
          e.en_variants)
    (Prelude.items () @ user_items);
  Hashtbl.iter (fun v ens -> match ens with [ en ] -> Hashtbl.replace g.ctors v en | _ -> ()) owners;
  let test_names =
    let field (p : param) = match p.prm_ty.tdesc with TyName ("Fs", _) -> "fs" | _ -> "clock" in
    if tests then
      List.filter_map
        (function
          | IFn ({ fn_test = Some _; fn_bench = false; _ } as d) ->
            Some (instance g d.fn_name [], List.map field d.fn_params, d.fn_virtual)
          | _ -> None)
        user_items
    else []
  in
  let bench_names =
    let field (p : param) = match p.prm_ty.tdesc with TyName ("Fs", _) -> "fs" | _ -> "clock" in
    if tests then
      List.filter_map
        (function
          | IFn ({ fn_test = Some _; fn_bench = true; _ } as d) -> Some (instance g d.fn_name [], List.map field d.fn_params)
          | _ -> None)
        user_items
    else []
  in
  let main_decl =
    match Hashtbl.find_opt g.fns "main" with
    | Some d when not tests -> Some d
    | _ -> None
  in
  let main = match main_decl with Some _ -> instance g "main" [] | None -> "" in
  let rec drain () =
    match g.queue with
    | (c, d, env) :: rest ->
      g.queue <- rest;
      let f = lower_fn g c d env in
      (* lowering may have added lifted closures to g.out; read it afterwards *)
      g.out <- f :: g.out;
      drain ()
    | [] -> ()
  in
  drain ();
  let main_ret = match main_decl with Some { fn_ret = Some te; _ } -> resolve [] te | _ -> TUnit in
  let takes_root = match main_decl with Some d -> d.fn_params <> [] | None -> false in
  ({ I.fns = List.rev g.out; main; main_takes_root = takes_root; main_ret; tests = test_names; benches = bench_names }, g)
