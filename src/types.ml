(* Semantic types and unification. *)
type cap = Root | Net | Fs | Stdout | Stderr | Clock | Listen | Listener | Conn | Rand | Stdin | Env | Exec | Foreign

type ty =
  | TInt
  | TFloat
  | TBool
  | TStr
  | TBytes (* immutable bytes; natively a WStr *)
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
  | TMap of ty * ty (* keys (Int, Str, Bool, or tuples, structs and enums of them) to values, in insertion order *)
  | TDyn of string * ty list (* a value of any type that satisfies the interface (with these arguments), boxed with its methods *)
  | TParam of string (* a generic parameter inside its own definition *)
  | TVar of tvar ref
  | TErr (* already reported; unifies with anything *)

and tvar = Unbound of int | Link of ty

let all_caps = [ Root; Net; Fs; Stdout; Stderr; Clock; Listen; Listener; Conn; Rand; Stdin; Env; Exec; Foreign ]

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
  | TDyn (i, []) -> "dyn " ^ i
  | TDyn (i, args) -> "dyn " ^ i ^ "[" ^ String.concat ", " (List.map show args) ^ "]"
  | TMap (k, v) -> "Map[" ^ show k ^ ", " ^ show v ^ "]"
  | TParam n -> n
  | TVar _ -> "_"
  | TErr -> "?"

let rec occurs r t =
  match repr t with
  | TVar r' -> r == r'
  | TList a | TGroup a | TChan a -> occurs r a
  | TArray (a, n) -> occurs r a || occurs r n
  | TDyn (_, a) -> List.exists (occurs r) a
  | TMap (k, v) -> occurs r k || occurs r v
  | TFn (ps, b) -> List.exists (occurs r) ps || occurs r b
  | TNamed (_, a) -> List.exists (occurs r) a
  | _ -> false

exception Mismatch

let rec unify a b =
  let a = repr a and b = repr b in
  match (a, b) with
  | TErr, _ | _, TErr -> ()
  | TVar r1, TVar r2 when r1 == r2 -> ()
  | TVar r, t | t, TVar r -> if occurs r t then raise Mismatch else r := Link t
  | TInt, TInt | TFloat, TFloat | TBool, TBool | TStr, TStr | TBytes, TBytes | TUnit, TUnit -> ()
  | TList x, TList y | TGroup x, TGroup y -> unify x y
  | TArray (x, n), TArray (y, m) -> unify n m; unify x y
  | TLen n, TLen m when n = m -> ()
  | TChan x, TChan y -> unify x y
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

(* Capabilities a value of the type holds: written in the type, or reached
   through the fields and payloads of the structs and enums in it. *)
let rec caps_in t acc =
  match repr t with
  | TCap c -> if List.mem c acc then acc else c :: acc
  | TList a | TArray (a, _) -> caps_in a acc
  | TMap (k, v) -> caps_in v (caps_in k acc)
  | TFn (ps, r) -> List.fold_left (fun acc p -> caps_in p acc) (caps_in r acc) ps
  | TNamed (n, args) ->
    let acc = List.fold_left (fun acc a -> caps_in a acc) acc args in
    (match Hashtbl.find_opt authority n with
     | Some held -> List.fold_left (fun acc (c, _) -> if List.mem c acc then acc else c :: acc) acc held
     | None -> acc)
  | _ -> acc

(* The capabilities written in the type itself, not those its structs and
   enums hold through their fields. *)
let rec caps_written t acc =
  match repr t with
  | TCap c -> if List.mem c acc then acc else c :: acc
  | TList a | TArray (a, _) -> caps_written a acc
  | TMap (k, v) -> caps_written v (caps_written k acc)
  | TFn (ps, r) -> List.fold_left (fun acc p -> caps_written p acc) (caps_written r acc) ps
  | TNamed (_, args) -> List.fold_left (fun acc a -> caps_written a acc) acc args
  | _ -> acc

(* Where a type that does not write capability [c] gets it from: the path
   through the first struct or enum in it that holds [c] (its own entry
   first, then its type arguments in order). *)
let rec via t c =
  match repr t with
  | TList a | TArray (a, _) -> via a c
  | TMap (k, v) -> (match via k c with Some p -> Some p | None -> via v c)
  | TFn (ps, r) -> List.find_map (fun p -> via p c) (ps @ [ r ])
  | TNamed (n, args) -> (
    match Option.bind (Hashtbl.find_opt authority n) (List.assoc_opt c) with
    | Some p -> Some p
    | None -> List.find_map (fun a -> via a c) args)
  | _ -> None

(* Fill [authority] for the program's structs and enums: [fields] gives a
   type's fields (a variant's payloads are named by the variant), in
   declaration order, with its type parameters as TParam. Each type is
   walked depth first from its first field, every type entered once, so
   recursive types end. *)
let compute_authority (names : string list) (fields : string -> (string * ty) list option) =
  Hashtbl.reset authority;
  List.iter
    (fun n ->
      let seen = Hashtbl.create 8 and found = ref [] in
      let add c path = if not (List.mem_assoc c !found) then found := !found @ [ (c, path) ] in
      let rec enter m path =
        if not (Hashtbl.mem seen m) then begin
          Hashtbl.replace seen m ();
          match fields m with
          | Some fs -> List.iter (fun (f, t) -> walk (path ^ "." ^ f) t) fs
          | None -> ()
        end
      and walk path t =
        match repr t with
        | TCap c -> add c path
        | TList a | TArray (a, _) -> walk path a
        | TMap (k, v) -> walk path k; walk path v
        | TFn (ps, r) -> List.iter (walk path) ps; walk path r
        | TNamed (m, args) -> List.iter (walk path) args; enter m path
        | _ -> ()
      in
      enter n n;
      if !found <> [] then Hashtbl.replace authority n !found)
    names

(* Channels: shared communication, so values holding one are never pure. *)
let rec has_chan t =
  match repr t with
  | TChan _ -> true
  | TList a | TArray (a, _) -> has_chan a
  | TMap (k, v) -> has_chan k || has_chan v
  | TFn (ps, r) -> List.exists has_chan ps || has_chan r
  | TNamed (_, args) -> List.exists has_chan args
  | _ -> false

(* Does [t] mention a `dyn` type? *)
let rec has_dyn t =
  match repr t with
  | TDyn _ -> true
  | TList a | TChan a | TArray (a, _) -> has_dyn a
  | TMap (k, v) -> has_dyn k || has_dyn v
  | TNamed (_, xs) -> List.exists has_dyn xs
  | _ -> false

let rec has_fn t =
  match repr t with
  | TFn _ -> true
  | TList a | TArray (a, _) -> has_fn a
  | TMap (k, v) -> has_fn k || has_fn v
  | TNamed (_, args) -> List.exists has_fn args
  | _ -> false

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
