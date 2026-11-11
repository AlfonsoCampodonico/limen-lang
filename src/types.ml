(* Semantic types and unification. *)
type cap = Root | Net | Fs | Stdout | Clock

type ty =
  | TInt
  | TFloat
  | TBool
  | TStr
  | TUnit
  | TList of ty
  | TFn of ty list * ty
  | TNamed of string * ty list (* user or prelude struct/enum *)
  | TCap of cap
  | TGroup of ty (* handle of a `group` block whose tasks return ty *)
  | TArray of ty * int (* fixed-size array, a value type *)
  | TParam of string (* a generic parameter inside its own definition *)
  | TVar of tvar ref
  | TErr (* already reported; unifies with anything *)

and tvar = Unbound of int | Link of ty

let all_caps = [ Root; Net; Fs; Stdout; Clock ]

let cap_name = function
  | Root -> "Root"
  | Net -> "Net"
  | Fs -> "Fs"
  | Stdout -> "Stdout"
  | Clock -> "Clock"

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
  | TUnit -> "()"
  | TList a -> "List[" ^ show a ^ "]"
  | TFn (ps, r) -> "fn(" ^ String.concat ", " (List.map show ps) ^ ") -> " ^ show r
  | TNamed (n, []) -> n
  | TNamed (n, args) -> n ^ "[" ^ String.concat ", " (List.map show args) ^ "]"
  | TCap c -> cap_name c
  | TGroup a -> "Group[" ^ show a ^ "]"
  | TArray (a, n) -> "Array[" ^ show a ^ ", " ^ string_of_int n ^ "]"
  | TParam n -> n
  | TVar _ -> "_"
  | TErr -> "?"

let rec occurs r t =
  match repr t with
  | TVar r' -> r == r'
  | TList a | TGroup a | TArray (a, _) -> occurs r a
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
  | TInt, TInt | TFloat, TFloat | TBool, TBool | TStr, TStr | TUnit, TUnit -> ()
  | TList x, TList y | TGroup x, TGroup y -> unify x y
  | TArray (x, n), TArray (y, m) when n = m -> unify x y
  | TFn (p1, r1), TFn (p2, r2) ->
    if List.length p1 <> List.length p2 then raise Mismatch;
    List.iter2 unify p1 p2;
    unify r1 r2
  | TNamed (n1, a1), TNamed (n2, a2) when n1 = n2 && List.length a1 = List.length a2 ->
    List.iter2 unify a1 a2
  | TCap c1, TCap c2 when c1 = c2 -> ()
  | TParam x, TParam y when x = y -> ()
  | _ -> raise Mismatch

let rec subst env t =
  match repr t with
  | TParam n -> ( match List.assoc_opt n env with Some t' -> t' | None -> t)
  | TList a -> TList (subst env a)
  | TArray (a, n) -> TArray (subst env a, n)
  | TGroup a -> TGroup (subst env a)
  | TFn (ps, r) -> TFn (List.map (subst env) ps, subst env r)
  | TNamed (n, args) -> TNamed (n, List.map (subst env) args)
  | t -> t

(* Capabilities mentioned anywhere in a type. *)
let rec caps_in t acc =
  match repr t with
  | TCap c -> if List.mem c acc then acc else c :: acc
  | TList a | TArray (a, _) -> caps_in a acc
  | TFn (ps, r) -> List.fold_left (fun acc p -> caps_in p acc) (caps_in r acc) ps
  | TNamed (_, args) -> List.fold_left (fun acc a -> caps_in a acc) acc args
  | _ -> acc

let rec has_fn t =
  match repr t with
  | TFn _ -> true
  | TList a | TArray (a, _) -> has_fn a
  | TNamed (_, args) -> List.exists has_fn args
  | _ -> false

(* Substitute generics and default any still-unknown type to (). The result
   contains no TVar or TParam, which is what code generation needs. *)
let rec zonk env t =
  match repr t with
  | TParam n -> ( match List.assoc_opt n env with Some t' -> zonk [] t' | None -> TUnit)
  | TVar _ -> TUnit
  | TList a -> TList (zonk env a)
  | TArray (a, n) -> TArray (zonk env a, n)
  | TGroup a -> TGroup (zonk env a)
  | TFn (ps, r) -> TFn (List.map (zonk env) ps, zonk env r)
  | TNamed (n, args) -> TNamed (n, List.map (zonk env) args)
  | t -> t

let opt t = TNamed ("Option", [ t ])
let res t e = TNamed ("Result", [ t; e ])
let io_err = TNamed ("IoError", [])
