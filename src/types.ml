(* Semantic types and unification. *)
type cap = Root | Net | Fs | Stdout | Stderr | Clock | Listen | Listener | Conn | Rand | Stdin | Env | Exec | Foreign | Revoker

type ty =
  | TInt
  | TFloat
  | TBool
  | TStr
  | TBytes (* immutable bytes; natively a WStr *)
  | TUInt of int
      (* UInt8, UInt16, UInt32, UInt64: unsigned integers of that many bits,
         whose arithmetic wraps modulo 2^bits *)
  | TUnit
  | TList of ty
  | TFn of ty list * ty
  | TNamed of string * ty list (* user or prelude struct/enum *)
  | TCap of cap
  | TGroup of ty (* handle of a `group` block whose tasks return ty *)
  | TArray of ty * ty
      (* fixed-size array, a value type: the element and the length, a TLen, or
         a type parameter (or variable) standing for one until it is instantiated *)
  | TLen of int (* an array's length, `3` in `Array[Int, 3]`; never a value's type *)
  | TChan of ty (* channel between tasks *)
  | TAtomic (* an Int tasks share, changed atomically *)
  | TShared of ty (* a value tasks share, behind a lock *)
  | TMap of ty * ty (* keys (Int, Str, Bool, or tuples, structs and enums of them) to values, in insertion order *)
  | TDyn of string * ty list (* a value of any type that satisfies the interface (with these arguments), boxed with its methods *)
  | TParam of string (* a generic parameter inside its own definition *)
  | TVar of tvar ref
  | TErr (* already reported; unifies with anything *)

and tvar = Unbound of int | Link of ty

let all_caps = [ Root; Net; Fs; Stdout; Stderr; Clock; Listen; Listener; Conn; Rand; Stdin; Env; Exec; Foreign; Revoker ]

let cap_name = function
  | Root -> "Root"
  | Net -> "Net"
  | Fs -> "Fs"
  | Stdout -> "Stdout"
  | Stderr -> "Stderr"
  | Clock -> "Clock"
  | Listen -> "Listen"
  | Listener -> "Listener"
  | Conn -> "Conn"
  | Rand -> "Rand"
  | Stdin -> "Stdin"
  | Env -> "Env"
  | Exec -> "Exec"
  | Foreign -> "Foreign"
  | Revoker -> "Revoker"

let counter = ref 0

let fresh () =
  incr counter;
  TVar (ref (Unbound !counter))

let rec repr t =
  match t with
  | TVar ({ contents = Link t' } as r) ->
    let t'' = repr t' in
    r := Link t'';
    t''
  | _ -> t

let rec show t =
  match repr t with
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TStr -> "Str"
  | TBytes -> "Bytes"
  | TUInt n -> "UInt" ^ string_of_int n
  | TUnit -> "()"
  | TList a -> "List[" ^ show a ^ "]"
  | TFn (ps, r) -> "fn(" ^ String.concat ", " (List.map show ps) ^ ") -> " ^ show r
  | TNamed (n, args) when String.length n > 6 && String.sub n 0 6 = "#tuple" -> "(" ^ String.concat ", " (List.map show args) ^ ")"
  | TNamed (n, []) -> n
  | TNamed (n, args) -> n ^ "[" ^ String.concat ", " (List.map show args) ^ "]"
  | TCap c -> cap_name c
  | TGroup a -> "Group[" ^ show a ^ "]"
  | TArray (a, n) -> "Array[" ^ show a ^ ", " ^ show n ^ "]"
  | TLen n -> string_of_int n
  | TChan a -> "Chan[" ^ show a ^ "]"
  | TAtomic -> "Atomic"
  | TShared a -> "Shared[" ^ show a ^ "]"
  | TDyn (i, []) -> "dyn " ^ i
  | TDyn (i, args) -> "dyn " ^ i ^ "[" ^ String.concat ", " (List.map show args) ^ "]"
  | TMap (k, v) -> "Map[" ^ show k ^ ", " ^ show v ^ "]"
  | TParam n -> n
  | TVar _ -> "_"
  | TErr -> "?"

(* The types directly inside [t], in order: every walk over a type goes
   through here. There is no wildcard, so a new type constructor does not
   compile until it says what it holds, and every analysis below then sees
   inside it. *)
let children t =
  match repr t with
  | TInt | TFloat | TBool | TStr | TBytes | TUInt _ | TUnit | TCap _ | TLen _ | TAtomic | TParam _ | TVar _ | TErr -> []
  | TList a | TGroup a | TChan a | TShared a -> [ a ]
  | TArray (a, n) -> [ a; n ]
  | TMap (k, v) -> [ k; v ]
  | TFn (ps, r) -> ps @ [ r ]
  | TNamed (_, args) | TDyn (_, args) -> args

(* [f] folded over every type inside [t], children first, then [t]. *)
let rec fold f acc t = f (List.fold_left (fold f) acc (children t)) t

(* The first result [f] gives, trying [t] first, then the types inside it
   in order. *)
let rec find_map f t = match f t with Some _ as r -> r | None -> List.find_map (find_map f) (children t)

let exists p t = find_map (fun u -> if p u then Some () else None) t <> None

(* What a type is at its top, for the rules about authority, shared state
   and equality. *)
type kind =
  | Plain (* data, or a container whose items [children] gives *)
  | Cap of cap
  | State of string (* state tasks share: "channel", "Atomic" or "Shared" *)
  | Func
  | Dyn
  | Named of string (* a struct or enum, which may hold authority through its fields *)

let kind t =
  match repr t with
  | TInt | TFloat | TBool | TStr | TBytes | TUInt _ | TUnit | TLen _ | TParam _ | TVar _ | TErr | TList _ | TArray _ | TMap _ | TGroup _ -> Plain
  | TCap c -> Cap c
  | TChan _ -> State "channel"
  | TAtomic -> State "Atomic"
  | TShared _ -> State "Shared"
  | TFn _ -> Func
  | TDyn _ -> Dyn
  | TNamed (n, _) -> Named n

let occurs r t = exists (fun u -> match repr u with TVar r' -> r == r' | _ -> false) t

exception Mismatch

let rec unify a b =
  let a = repr a and b = repr b in
  match (a, b) with
  | TErr, _ | _, TErr -> ()
  | TVar r1, TVar r2 when r1 == r2 -> ()
  | TVar r, t | t, TVar r -> if occurs r t then raise Mismatch else r := Link t
  | TInt, TInt | TFloat, TFloat | TBool, TBool | TStr, TStr | TBytes, TBytes | TUnit, TUnit -> ()
  | TUInt m, TUInt n when m = n -> ()
  | TList x, TList y | TGroup x, TGroup y -> unify x y
  | TArray (x, n), TArray (y, m) -> unify n m; unify x y
  | TLen n, TLen m when n = m -> ()
  | TChan x, TChan y | TShared x, TShared y -> unify x y
  | TAtomic, TAtomic -> ()
  | TMap (k1, v1), TMap (k2, v2) -> unify k1 k2; unify v1 v2
  | TFn (p1, r1), TFn (p2, r2) ->
    if List.length p1 <> List.length p2 then raise Mismatch;
    List.iter2 unify p1 p2;
    unify r1 r2
  | TNamed (n1, a1), TNamed (n2, a2) when n1 = n2 && List.length a1 = List.length a2 ->
    List.iter2 unify a1 a2
  | TCap c1, TCap c2 when c1 = c2 -> ()
  | TParam x, TParam y when x = y -> ()
  | TDyn (x, xs), TDyn (y, ys) when x = y && List.length xs = List.length ys -> List.iter2 unify xs ys
  | _ -> raise Mismatch

let rec subst env t =
  match repr t with
  | TParam n -> ( match List.assoc_opt n env with Some t' -> t' | None -> t)
  | TList a -> TList (subst env a)
  | TArray (a, n) -> TArray (subst env a, subst env n)
  | TDyn (i, xs) -> TDyn (i, List.map (subst env) xs)
  | TChan a -> TChan (subst env a)
  | TShared a -> TShared (subst env a)
  | TMap (k, v) -> TMap (subst env k, subst env v)
  | TGroup a -> TGroup (subst env a)
  | TFn (ps, r) -> TFn (List.map (subst env) ps, subst env r)
  | TNamed (n, args) -> TNamed (n, List.map (subst env) args)
  | t -> t

(* The authority a struct or enum holds through its own fields and
   payloads, by name: each capability with the first path that reaches it,
   `Client.net` or `App.client.net` (type arguments are not counted here;
   [caps_in] walks them where the type is used). Filled by the checker once
   the declarations are known (see [compute_authority]); a type holding no
   capability has no entry. *)
let authority : (string, (cap * string) list) Hashtbl.t = Hashtbl.create 16

(* The struct and enum types that hold a function through their own fields
   and payloads, by name, each with the path to the first one, `Route.run`
   or `App.routes.run` (type arguments are not counted here; [fn_path]
   walks them where the type is used). Filled with [authority] (see
   [compute_authority]); a type holding no function has no entry. *)
let fn_holders : (string, string) Hashtbl.t = Hashtbl.create 16

let add_cap c acc = if List.mem c acc then acc else c :: acc

(* Capabilities a value of the type holds: written in the type, or reached
   through the fields and payloads of the structs and enums in it. *)
let caps_in t acc =
  fold
    (fun acc u ->
      match kind u with
      | Cap c -> add_cap c acc
      | Named n -> (match Hashtbl.find_opt authority n with Some held -> List.fold_left (fun acc (c, _) -> add_cap c acc) acc held | None -> acc)
      | Plain | State _ | Func | Dyn -> acc)
    acc t

(* The capabilities written in the type itself, not those its structs and
   enums hold through their fields. *)
let caps_written t acc = fold (fun acc u -> match kind u with Cap c -> add_cap c acc | Plain | State _ | Func | Dyn | Named _ -> acc) acc t

(* Where a type that does not write capability [c] gets it from: the path
   through the first struct or enum in it that holds [c] (its own entry
   first, then its type arguments in order). *)
let via t c =
  find_map
    (fun u ->
      match kind u with
      | Named n -> Option.bind (Hashtbl.find_opt authority n) (List.assoc_opt c)
      | Plain | Cap _ | State _ | Func | Dyn -> None)
    t

(* Fill [authority] and [fn_holders] for the program's structs and enums:
   [fields] gives a type's fields (a variant's payloads are named by the
   variant), in declaration order, with its type parameters as TParam.
   Each type is walked depth first from its first field, every type entered
   once, so recursive types end. *)
let compute_authority (names : string list) (fields : string -> (string * ty) list option) =
  Hashtbl.reset authority;
  Hashtbl.reset fn_holders;
  List.iter
    (fun n ->
      let seen = Hashtbl.create 8 and found = ref [] and fn = ref None in
      let add c path = if not (List.mem_assoc c !found) then found := !found @ [ (c, path) ] in
      let add_fn path = if !fn = None then fn := Some path in
      let rec enter m path =
        if not (Hashtbl.mem seen m) then begin
          Hashtbl.replace seen m ();
          match fields m with
          | Some fs -> List.iter (fun (f, t) -> walk (path ^ "." ^ f) t) fs
          | None -> ()
        end
      and walk path t =
        fold (fun () u -> match kind u with Cap c -> add c path | Func -> add_fn path | Named m -> enter m path | Plain | State _ | Dyn -> ()) () t
      in
      enter n n;
      if !found <> [] then Hashtbl.replace authority n !found;
      Option.iter (Hashtbl.replace fn_holders n) !fn)
    names

(* Channels, Atomics and Shareds: state tasks share, so values holding one
   are never pure, never `dyn` and never stored in a struct. *)
let has_chan t = exists (fun u -> match kind u with State _ -> true | Plain | Cap _ | Func | Dyn | Named _ -> false) t

(* What [has_chan] found first: "channel", or the Atomic or Shared type. *)
let chan_kind t = find_map (fun u -> match kind u with State w -> Some w | Plain | Cap _ | Func | Dyn | Named _ -> None) t

(* Does [t] mention a `dyn` type? *)
let has_dyn t = exists (fun u -> match kind u with Dyn -> true | Plain | Cap _ | State _ | Func | Named _ -> false) t

(* Where a type holds a function, if it does: the first function type
   written in it (itself or inside it), shown, as `fn() -> Int`, or the path
   through the first struct or enum in it that holds one in its fields, as
   `App.routes.run`, whichever comes first (the type itself first, then the
   types inside it in order). *)
let fn_path t =
  find_map
    (fun u ->
      match kind u with
      | Func -> Some (show u)
      | Named n -> Hashtbl.find_opt fn_holders n
      | Plain | Cap _ | State _ | Dyn -> None)
    t

let has_fn t = fn_path t <> None

(* Does [t] write a function type, itself or inside it, rather than only
   hold one through a struct's or enum's fields? *)
let writes_fn t = exists (fun u -> kind u = Func) t

(* The path through which [t] holds a function when it does not write one:
   `App.routes.run`. *)
let fn_via t = if writes_fn t then None else fn_path t

(* How far a rule looks into a type for a kind of value: not at all, at the
   type itself only, or anywhere in it (element, key, payload, parameter
   and result types, and the type arguments of structs and enums). *)
type depth = Never | Top | Deep

(* What a rule refuses, and how deep it looks for each: state tasks share
   (channels, Atomics, Shareds), functions, `dyn` values, and capabilities
   written in the type (those held through a struct's fields are
   [caps_in]'s, which the rules about authority ask separately). *)
type policy = { state : depth; fns : depth; dyns : depth; caps : depth }

type found = Itself | Inside of ty

(* The part of [t] that [p] refuses: [t] itself, or else the first type
   inside it whose kind [p] refuses at any depth, shared state first, then
   functions, `dyn` values and capabilities. *)
let refused p t =
  (* a struct or enum holding a function through its fields counts as one
     for a policy that looks for functions anywhere *)
  let holds_fn u = match kind u with Named n -> p.fns = Deep && Hashtbl.mem fn_holders n | Plain | Cap _ | State _ | Func | Dyn -> false in
  let reach u =
    match kind u with
    | State _ -> p.state
    | Func -> p.fns
    | Dyn -> p.dyns
    | Cap _ -> p.caps
    | Named _ -> if holds_fn u then p.fns else Never
    | Plain -> Never
  in
  if reach t <> Never then Some Itself
  else
    let deep k = find_map (fun u -> if k u && reach u = Deep then Some (Inside (repr u)) else None) t in
    List.find_map deep
      [ (fun u -> match kind u with State _ -> true | Plain | Cap _ | Func | Dyn | Named _ -> false);
        (fun u -> kind u = Func || holds_fn u);
        (fun u -> kind u = Dyn);
        (fun u -> match kind u with Cap _ -> true | Plain | State _ | Func | Dyn | Named _ -> false) ]

(* Substitute generics and default any still-unknown type to (). The result
   contains no TVar or TParam, which is what code generation needs. *)
let rec zonk env t =
  match repr t with
  | TParam n -> ( match List.assoc_opt n env with Some t' -> zonk [] t' | None -> TUnit)
  | TVar _ -> TUnit
  | TList a -> TList (zonk env a)
  | TArray (a, n) -> TArray (zonk env a, (match zonk env n with TLen _ as l -> l | _ -> TLen 0))
  | TDyn (i, xs) -> TDyn (i, List.map (zonk env) xs)
  | TChan a -> TChan (zonk env a)
  | TShared a -> TShared (zonk env a)
  | TMap (k, v) -> TMap (zonk env k, zonk env v)
  | TGroup a -> TGroup (zonk env a)
  | TFn (ps, r) -> TFn (List.map (zonk env) ps, zonk env r)
  | TNamed (n, args) -> TNamed (n, List.map (zonk env) args)
  | t -> t

(* The length of an array type's length position, once it is known. *)
let arr_len t = match repr t with TLen n -> n | _ -> 0

let opt t = TNamed ("Option", [ t ])
let res t e = TNamed ("Result", [ t; e ])
let io_err = TNamed ("IoError", [])
