(* What the native backend does with pure functions. A function whose
   parameters hold no capability, no function and no channel can only
   compute and panic (SPEC section 9). Here the test is made on the IR and
   is a little stricter: the body (and every function it calls) also starts
   no task, makes no channel and calls no closure or `dyn` method, so the
   same arguments always give the same result. A function whose only other
   effects are calls of its function parameters is pure when the functions
   it is given are (see analyse).

   Two rewrites use it, and neither can be observed by the program:

   - [cse], before Own: a second call with the same arguments in the same
     block (or a block inside it), with nothing reassigned in between, takes
     the first call's result instead of calling again.
   - [plan], for Cgen: a call in a loop whose arguments do not change in
     the loop is made the first time it is reached and its result kept for
     the later iterations. Being lazy keeps everything the program can see:
     a loop that runs zero times never calls, a call under a condition runs
     only once the condition holds, and a call that panics or never returns
     does so at the same point as before. *)
open Types
open Ir
module S = Set.Make (Int)

(* ---------- which functions are pure ---------- *)

let is_scalar = function TInt | TFloat | TBool | TUnit -> true | _ -> false

let pure_type t = caps_in t [] = [] && not (has_fn t) && not (has_chan t)

(* Built-in methods worth keeping across a loop: they allocate or walk a
   whole value, so the C compiler cannot move them itself. Cheap ones
   (`len`, `abs`, `min`) it already handles. *)
let heavy_prims =
  [ "list_contains"; "list_reverse"; "list_sort"; "list_take"; "list_skip"; "list_join"; "list_sum";
    "str_lines"; "str_split"; "str_trim"; "str_contains"; "str_starts_with"; "str_ends_with"; "str_to_upper";
    "str_to_lower"; "str_replace"; "str_parse_int"; "str_parse_float"; "str_slice"; "str_index_of"; "str_last_index_of"; "list_slice";
    "str_bytes"; "str_chars"; "str_to_bytes"; "bytes_to_str"; "bytes_slice"; "bytes_to_list"; "bytes_new";
    "int_range"; "int_to_str"; "float_to_str"; "bool_to_str"; "float_to_fixed"; "map_keys"; "map_values";
    "map_entries"; "map_from"; "array_to_list"; "array_contains" ]

(* A built-in method a pure function may use: anything but those on
   capabilities and channels, and `keep` (the benchmark black box). *)
let pure_prim name t =
  name <> "keep"
  && (match t with TCap _ | TChan _ | TAtomic | TShared _ -> false | _ -> true)
  && not (String.starts_with ~prefix:"cap_" name || String.starts_with ~prefix:"chan_" name
          || String.starts_with ~prefix:"atomic_" name || String.starts_with ~prefix:"shared_" name)

type info = {
  heavy : bool; (* worth keeping across a loop: not a short scalar computation the C compiler sees through *)
  cbs : int list;
      (* the positions of its function parameters: a call is pure only when
         the functions passed there are (conditionally pure); [] for a
         function pure whatever it is passed *)
}

let rec stmts_iter f ss =
  List.iter
    (fun s ->
      f s;
      match s with
      | If (_, a, b) -> stmts_iter f a; stmts_iter f b
      | Loop b | Scope (_, b) -> stmts_iter f b
      | Guard (_, h, b) -> stmts_iter f h; stmts_iter f b
      | _ -> ())
    ss

let rhs_of = function Let (_, r) | Init (_, r) | Set (_, r, _) -> Some r | _ -> None

let is_fn = function TFn _ -> true | _ -> false

(* The atoms a statement reads or moves, besides those of its rhs. *)
let stmt_atoms = function
  | Let (_, r) | Init (_, r) | Set (_, r, _) -> consumed r @ borrowed r
  | Mutate (_, steps, m) ->
    List.filter_map (function PIndex (i, _) | PIndexU i | PKey (i, _) -> Some i | PField _ -> None) steps @ mut_atoms m
  | If (a, _, _) | Return a | Scope (a, _) -> [ a ]
  | _ -> []

(* The closures a function makes and never reassigns: variable to the
   lifted function. *)
let closures_of (f : fn) =
  let made = Hashtbl.create 8 and set = Hashtbl.create 8 in
  stmts_iter
    (function
      | Let (v, MkClosure (h, _, _)) -> Hashtbl.replace made v.id h
      | Set (v, _, _) | Init (v, _) | Decl v | Mutate (v, _, _) -> Hashtbl.replace set v.id ()
      | _ -> ())
    f.body;
  Hashtbl.filter_map_inplace (fun id h -> if Hashtbl.mem set id then None else Some h) made;
  made

(* The pure functions of a program, by C name. Calls between functions are
   taken as pure until shown otherwise (recursion is fine).

   A function whose parameters of function type are only called, or passed
   on where another such function takes a function, is pure when the
   functions it is passed are (SPEC section 14): its info lists those
   positions. A call to it is pure when each argument there is a closure the
   caller made from a pure function. Storing such a parameter, sending it,
   capturing it or returning it makes the function impure. *)
let analyse (p : program) : (string, info) Hashtbl.t =
  let by_name = Hashtbl.create 64 in
  List.iter (fun (f : fn) -> Hashtbl.replace by_name f.fname f) p.fns;
  let calls = Hashtbl.create 64 in
  let candidates = Hashtbl.create 64 in
  let cb_of (f : fn) = List.filter (fun (v : var) -> is_fn v.ty) f.params in
  List.iter
    (fun (f : fn) ->
      let cbs = cb_of f in
      let is_cb a = match atom_var a with Some v -> List.exists (fun (c : var) -> c.id = v.id) cbs | None -> false in
      let ok = ref (List.for_all (fun (v : var) -> pure_type v.ty || is_fn v.ty) f.params && List.for_all (fun (v : var) -> pure_type v.ty) f.captures) in
      let callees = ref [] in
      let scalar_only = ref (List.for_all (fun (v : var) -> is_scalar v.ty) f.params && is_scalar f.ret) in
      let straight = ref true in
      stmts_iter
        (fun s ->
          (match s with
           | Let (v, _) | Init (v, _) | Decl v -> if not (is_scalar v.ty) then scalar_only := false
           | Loop _ -> straight := false
           | Scope _ -> ok := false
           | _ -> ());
          (* a function parameter is only called or passed on *)
          (match (s, rhs_of s) with
           | Set (v, _, _), _ when List.exists (fun (c : var) -> c.id = v.id) cbs -> ok := false
           | _, Some (CallClo (c, args)) -> if not (is_cb c) || List.exists is_cb args then ok := false
           | _, Some (Call _) -> () (* where the callee takes a function: checked below *)
           | _ -> if List.exists is_cb (stmt_atoms s) then ok := false);
          match rhs_of s with
          | Some (Call (g, args)) ->
            straight := false;
            callees := (g, args) :: !callees;
            if not (Hashtbl.mem by_name g) then ok := false
          | Some (Prim (n, t, _, _)) -> straight := false; if not (pure_prim n t) then ok := false
          | Some (CallClo _) -> straight := false
          | Some
              ( CallDyn _ | Catch _ | Spawn _ | GroupNew _ | GroupJoin _ | ChanNew _ | ChanSend _ | Select _
              | SelGot _ | RootField _ | CallC _ ) ->
            ok := false
          | _ -> ())
        f.body;
      if !ok then begin
        let ids = List.map (fun (v : var) -> v.id) f.params in
        let pos = List.filter_map (fun (c : var) -> List.find_index (fun id -> id = c.id) ids) cbs in
        Hashtbl.replace candidates f.fname { heavy = not (!scalar_only && !straight); cbs = pos };
        Hashtbl.replace calls f.fname (closures_of f, List.map (fun (c : var) -> c.id) cbs, !callees)
      end)
    p.fns;
  (* drop functions that call a non-pure one, or pass a function that is not
     pure where a conditionally pure one takes one, until nothing changes;
     a function calling a heavy one is heavy *)
  let changed = ref true in
  while !changed do
    changed := false;
    let pure_now = Hashtbl.copy candidates in
    Hashtbl.filter_map_inplace
      (fun name info ->
        let clos, own_cbs, cs = Hashtbl.find calls name in
        let arg_ok a =
          match atom_var a with
          | Some v -> List.mem v.id own_cbs || (match Hashtbl.find_opt clos v.id with Some h -> (match Hashtbl.find_opt pure_now h with Some i -> i.cbs = [] | None -> false) | None -> false)
          | None -> false
        in
        let call_ok (g, args) =
          match Hashtbl.find_opt pure_now g with
          | Some i -> List.for_all (fun k -> arg_ok (List.nth args k)) i.cbs
          | None -> false
        in
        if List.for_all call_ok cs then begin
          let heavy = info.heavy || List.exists (fun (g, _) -> match Hashtbl.find_opt pure_now g with Some i -> i.heavy | None -> false) cs in
          if heavy <> info.heavy then changed := true;
          Some { info with heavy }
        end
        else begin
          changed := true;
          None
        end)
      candidates
  done;
  candidates

(* A call whose result depends only on its arguments: to a pure function,
   passing a closure made from a pure function wherever it takes one.
   [clos] are the closures of the function the call is in (closures_of). *)
let pure_rhs pure clos = function
  | Call (f, args) -> (
    match Hashtbl.find_opt pure f with
    | Some i ->
      List.for_all
        (fun k ->
          match atom_var (List.nth args k) with
          | Some v -> (
            match Hashtbl.find_opt clos v.id with Some h -> (match Hashtbl.find_opt pure h with Some j -> j.cbs = [] | None -> false) | None -> false)
          | None -> false)
        i.cbs
    | None -> false)
  | Prim (n, t, _, _) -> pure_prim n t
  | _ -> false

let worth_keeping pure clos = function
  | Call (f, _) as r -> ( match Hashtbl.find_opt pure f with Some i -> i.heavy && pure_rhs pure clos r | None -> false)
  | Prim (n, t, _, _) -> pure_prim n t && List.mem n heavy_prims
  | _ -> false

(* Variables a statement list assigns other than by `let`. *)
let assigned ss =
  let acc = ref S.empty in
  stmts_iter
    (function
      | Set (v, _, _) | Mutate (v, _, _) | Init (v, _) | Decl v -> acc := S.add v.id !acc
      | _ -> ())
    ss;
  !acc

(* ---------- reusing a repeated call ---------- *)

type key = K of string * string * katom list
and katom = KV of int | KI of int64 | KF of float | KB of bool | KU | KS of string

let katom = function
  | V v | VDup v -> KV v.id
  | AInt n -> KI n
  | AFloat x -> KF x
  | ABool b -> KB b
  | AUnit -> KU
  | AStr s -> KS s

let key_of = function
  | Call (f, args) -> Some (K ("call", f, List.map katom args))
  | Prim (n, t, args, _) -> Some (K (n, show t, List.map katom args))
  | _ -> None

let mentions_any killed (K (_, _, args), (v : var)) =
  S.mem v.id killed || List.exists (function KV id -> S.mem id killed | _ -> false) args

(* [avail]: calls made earlier on every path to here, with the variable
   holding each result. A float argument is compared by its bits, so NaN
   constants never match and -0.0 differs from 0.0. *)
let same_key (K (a, b, xs)) (K (c, d, ys)) =
  a = c && b = d
  && List.length xs = List.length ys
  && List.for_all2 (fun x y -> match (x, y) with KF p, KF q -> Int64.bits_of_float p = Int64.bits_of_float q | _ -> x = y) xs ys

let cse_fn pure (f : fn) : fn =
  let clos = closures_of f in
  let pure_rhs pure = pure_rhs pure clos in
  let rec block avail ss =
    let avail = ref avail in
    let out =
      List.map
        (fun s ->
          match s with
          | Let (v, r) when pure_rhs pure r -> (
            let k = Option.get (key_of r) in
            match List.find_opt (fun (k', _) -> same_key k k') !avail with
            | Some (_, w) -> Let (v, Atom (V w))
            | _ ->
              avail := (k, v) :: !avail;
              s)
          | Let _ | Drop _ | Break | Continue | Return _ | GuardEnd _ | Line _ -> s
          | Set (v, _, _) | Mutate (v, _, _) | Init (v, _) | Decl v ->
            avail := List.filter (fun e -> not (mentions_any (S.singleton v.id) e)) !avail;
            s
          | If (c, a, b) ->
            let a' = block !avail a and b' = block !avail b in
            let killed = assigned (a @ b) in
            avail := List.filter (fun e -> not (mentions_any killed e)) !avail;
            If (c, a', b')
          | Loop body ->
            let killed = assigned body in
            avail := List.filter (fun e -> not (mentions_any killed e)) !avail;
            Loop (block !avail body)
          | Guard (n, h, b) ->
            let killed = assigned (h @ b) in
            avail := List.filter (fun e -> not (mentions_any killed e)) !avail;
            Guard (n, block [] h, block [] b)
          | Scope (g, b) ->
            let killed = assigned b in
            avail := List.filter (fun e -> not (mentions_any killed e)) !avail;
            Scope (g, block [] b))
        ss
    in
    out
  in
  { f with body = block [] f.body }

let cse (p : program) : program =
  let pure = analyse p in
  { p with fns = List.map (cse_fn pure) p.fns }

(* ---------- keeping a call across a loop ---------- *)

(* A kept call's result lives in a slot declared before the loop it is
   kept across; the slot is filled the first time the call runs. *)
type slot = { sid : int; sty : ty }

type plan = {
  at_loop : (stmt list * slot list) list; (* by the loop's body, physically *)
  kept : (int, slot) Hashtbl.t; (* by the variable the call's result goes to *)
}

let no_plan () = { at_loop = []; kept = Hashtbl.create 1 }

type loop = { lbody : stmt list; lassigned : S.t; defs : (int, rhs) Hashtbl.t; memo : (int, bool) Hashtbl.t; mutable slots : slot list; lclos : (int, string) Hashtbl.t }

let slot_counter = ref 0

(* Does an atom have the same value on every iteration of the loop? A
   variable does if the loop never reassigns it and either it comes from
   outside the loop or the loop computes it from such values alone. *)
let rec same_each_time pure l a =
  match atom_var a with
  | None -> true
  | Some v -> (
    (not (S.mem v.id l.lassigned))
    &&
    match Hashtbl.find_opt l.defs v.id with
    | None -> true
    | Some r -> (
      match Hashtbl.find_opt l.memo v.id with
      | Some b -> b
      | None ->
        Hashtbl.replace l.memo v.id false;
        let b =
          (match r with
           | Atom _ | Bin _ | Neg _ | Not _ | Get _ | Payload _ | IsTag _ | PatTest _ | Interp _ | MkStruct _ | MkEnum _ | MkList _
           | MkClosure _ -> true
           | Call _ | Prim _ -> pure_rhs pure l.lclos r
           | _ -> false)
          && List.for_all (same_each_time pure l) (consumed r @ borrowed r)
        in
        Hashtbl.replace l.memo v.id b;
        b))

let plan pure (f : fn) : plan =
  let has_boundary = ref false in
  stmts_iter (function Guard _ | Scope _ -> has_boundary := true | _ -> ()) f.body;
  (* a defer or a deadline jumps back into the frame with setjmp's rules;
     keep the plain code there *)
  if !has_boundary then no_plan ()
  else begin
    let kept = Hashtbl.create 8 in
    let loops = ref [] in
    let clos = closures_of f in
    let rec walk (outer : loop list) ss =
      List.iter
        (fun s ->
          match s with
          | (Let (v, r) | Init (v, r)) when outer <> [] && worth_keeping pure clos r -> (
            let atoms = consumed r @ borrowed r in
            (* the outermost loop the arguments do not change in *)
            match List.find_opt (fun l -> List.for_all (same_each_time pure l) atoms) (List.rev outer) with
            | Some l ->
              incr slot_counter;
              let sl = { sid = !slot_counter; sty = v.ty } in
              l.slots <- l.slots @ [ sl ];
              Hashtbl.replace kept v.id sl
            | None -> ())
          | If (_, a, b) -> walk outer a; walk outer b
          | Loop body ->
            let defs = Hashtbl.create 16 in
            stmts_iter (function Let (v, r) -> Hashtbl.replace defs v.id r | _ -> ()) body;
            let l = { lbody = body; lassigned = assigned body; defs; memo = Hashtbl.create 16; slots = []; lclos = clos } in
            loops := l :: !loops;
            walk (l :: outer) body
          | _ -> ())
        ss
    in
    walk [] f.body;
    { at_loop = List.filter_map (fun l -> if l.slots = [] then None else Some (l.lbody, l.slots)) !loops; kept }
  end
