(* Tree-walking interpreter. Runs only programs that passed the checker.
   Values have value semantics: lists and structs are never shared mutably. *)
open Ast

let sprintf = Printf.sprintf

type cap = CRoot | CNet of string list | CFs of Caps.fs | CStdout | CClock

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VStr of string
  | VUnit
  | VList of value array
  | VStruct of string * (string * value) list
  | VEnum of string * string * value list
  | VClosure of string list * expr * env
  | VFn of string
  | VCtor of string * string
  | VPanicFn
  | VCap of cap
  | VGroup of value list ref (* closures spawned so far, newest first *)

and env = (string, value ref) Hashtbl.t list

exception Return of value
exception Break_
exception Continue_
exception Panic of string * Span.t

type grants = { net : string list; fs : (string * bool) option (* realpath, read-only *) }

let no_grants = { net = []; fs = None }

type rt = {
  fns : (string, fn_decl) Hashtbl.t;
  structs : (string, struct_decl) Hashtbl.t;
  enums : (string, enum_decl) Hashtbl.t;
  ctors : (string, string) Hashtbl.t; (* variant -> enum, when unambiguous *)
  out : string -> unit;
  grants : grants;
}

(* ---------- printing and comparison ---------- *)

let fmt_float x =
  if Float.is_integer x && Float.abs x < 1e16 then sprintf "%.1f" x else sprintf "%.15g" x

let cap_name = function
  | CRoot -> "Root" | CNet _ -> "Net" | CFs _ -> "Fs" | CStdout -> "Stdout" | CClock -> "Clock"

let rec show ?(top = true) v =
  match v with
  | VInt n -> string_of_int n
  | VFloat x -> fmt_float x
  | VBool b -> string_of_bool b
  | VStr s -> if top then s else "\"" ^ String.escaped s ^ "\""
  | VUnit -> "()"
  | VList a -> "[" ^ String.concat ", " (Array.to_list (Array.map (show ~top:false) a)) ^ "]"
  | VStruct (n, fs) ->
    n ^ " { " ^ String.concat ", " (List.map (fun (k, x) -> k ^ ": " ^ show ~top:false x) fs) ^ " }"
  | VEnum (_, vn, []) -> vn
  | VEnum (_, vn, args) -> vn ^ "(" ^ String.concat ", " (List.map (show ~top:false) args) ^ ")"
  | VClosure _ | VFn _ | VCtor _ | VPanicFn -> "<fn>"
  | VCap c -> "<" ^ cap_name c ^ ">"
  | VGroup _ -> "<group>"

let rec equal a b =
  match (a, b) with
  | VInt x, VInt y -> x = y
  | VFloat x, VFloat y -> x = y
  | VBool x, VBool y -> x = y
  | VStr x, VStr y -> x = y
  | VUnit, VUnit -> true
  | VList x, VList y -> Array.length x = Array.length y && Array.for_all2 equal x y
  | VStruct (n, xs), VStruct (m, ys) -> n = m && List.for_all2 (fun (_, x) (_, y) -> equal x y) xs ys
  | VEnum (e, v, xs), VEnum (f, w, ys) ->
    e = f && v = w && List.length xs = List.length ys && List.for_all2 equal xs ys
  | _ -> false

let compare_v a b =
  match (a, b) with
  | VInt x, VInt y -> compare x y
  | VFloat x, VFloat y -> compare x y
  | VStr x, VStr y -> compare x y
  | _ -> 0

let as_bool = function VBool b -> b | _ -> failwith "internal: expected Bool"
let some x = VEnum ("Option", "Some", [ x ])
let none = VEnum ("Option", "None", [])
let ok x = VEnum ("Result", "Ok", [ x ])

let io_result f = function
  | Ok x -> ok (f x)
  | Error (Caps.Denied m) -> VEnum ("Result", "Err", [ VEnum ("IoError", "Denied", [ VStr m ]) ])
  | Error (Caps.Failed m) -> VEnum ("Result", "Err", [ VEnum ("IoError", "Failed", [ VStr m ]) ])

(* ---------- string helpers ---------- *)

let find_sub s sub from =
  let n = String.length s and m = String.length sub in
  let rec go i = if i + m > n then None else if String.sub s i m = sub then Some i else go (i + 1) in
  if m = 0 then Some from else go from

let split_on s sep =
  if sep = "" then List.init (String.length s) (fun i -> String.make 1 s.[i])
  else
    let rec go from acc =
      match find_sub s sep from with
      | Some i -> go (i + String.length sep) (String.sub s from (i - from) :: acc)
      | None -> List.rev (String.sub s from (String.length s - from) :: acc)
    in
    go 0 []

let replace_all s a b = if a = "" then s else String.concat b (split_on s a)

let lines s =
  let ls = String.split_on_char '\n' s in
  let ls = List.map (fun l -> if String.ends_with ~suffix:"\r" l then String.sub l 0 (String.length l - 1) else l) ls in
  match List.rev ls with "" :: rest -> List.rev rest | _ -> ls

(* Decimal only, optional sign, surrounding whitespace allowed (same as the C runtime). *)
let parse_decimal s =
  let s = String.trim s in
  let n = String.length s in
  let start = if n > 0 && (s.[0] = '-' || s.[0] = '+') then 1 else 0 in
  if n = start then None
  else if String.exists (fun c -> c < '0' || c > '9') (String.sub s start (n - start)) then None
  else int_of_string_opt s

let strs l = VList (Array.of_list (List.map (fun s -> VStr s) l))

(* ---------- evaluation ---------- *)

let rec find_var env n =
  match env with
  | [] -> None
  | s :: r -> ( match Hashtbl.find_opt s n with Some v -> Some v | None -> find_var r n)

(* Closures capture by value: copy every visible binding. *)
let snapshot env =
  let t = Hashtbl.create 16 in
  List.iter (fun s -> Hashtbl.iter (fun k v -> if not (Hashtbl.mem t k) then Hashtbl.replace t k (ref !v)) s) env;
  t

let binop op a b sp =
  match (op, a, b) with
  | Add, VInt x, VInt y -> VInt (x + y)
  | Sub, VInt x, VInt y -> VInt (x - y)
  | Mul, VInt x, VInt y -> VInt (x * y)
  | (Div | Mod), VInt _, VInt 0 -> raise (Panic ("division by zero", sp))
  | Div, VInt x, VInt y -> VInt (x / y)
  | Mod, VInt x, VInt y -> VInt (x mod y)
  | Add, VFloat x, VFloat y -> VFloat (x +. y)
  | Sub, VFloat x, VFloat y -> VFloat (x -. y)
  | Mul, VFloat x, VFloat y -> VFloat (x *. y)
  | Div, VFloat x, VFloat y -> VFloat (x /. y)
  | Add, VStr x, VStr y -> VStr (x ^ y)
  | Lt, VFloat x, VFloat y -> VBool (x < y)
  | Le, VFloat x, VFloat y -> VBool (x <= y)
  | Gt, VFloat x, VFloat y -> VBool (x > y)
  | Ge, VFloat x, VFloat y -> VBool (x >= y)
  | Eq, _, _ -> VBool (equal a b)
  | Ne, _, _ -> VBool (not (equal a b))
  | Lt, _, _ -> VBool (compare_v a b < 0)
  | Le, _, _ -> VBool (compare_v a b <= 0)
  | Gt, _, _ -> VBool (compare_v a b > 0)
  | Ge, _, _ -> VBool (compare_v a b >= 0)
  | _ -> failwith "internal: bad operands"

let rec pmatch p v binds =
  match (p.pdesc, v) with
  | PWild, _ -> true
  | PBind n, _ -> Hashtbl.replace binds n (ref v); true
  | PInt n, VInt m -> n = m
  | PStr s, VStr t -> s = t
  | PBool b, VBool c -> b = c
  | PVariant (_, vn, args), VEnum (_, vn', vals) ->
    vn = vn' && List.length args = List.length vals && List.for_all2 (fun p v -> pmatch p v binds) args vals
  | _ -> false

let rec eval rt env e : value =
  match e.edesc with
  | EInt n -> VInt n
  | EFloat x -> VFloat x
  | EBool b -> VBool b
  | EUnit -> VUnit
  | EStr parts ->
    VStr (String.concat "" (List.map (function SPLit s -> s | SPExpr x -> show (eval rt env x)) parts))
  | EVar n -> eval_var rt env n e.espan
  | EList es -> VList (Array.of_list (List.map (eval rt env) es))
  | EBin (And, a, b) -> if as_bool (eval rt env a) then eval rt env b else VBool false
  | EBin (Or, a, b) -> if as_bool (eval rt env a) then VBool true else eval rt env b
  | EBin (op, a, b) ->
    let x = eval rt env a in
    let y = eval rt env b in
    binop op x y e.espan
  | EUn (Neg, a) -> (
    match eval rt env a with VInt n -> VInt (-n) | VFloat x -> VFloat (-.x) | _ -> failwith "internal: neg")
  | EUn (Not, a) -> VBool (not (as_bool (eval rt env a)))
  | ECall (c, args) ->
    let fv = eval rt env c in
    let avs = List.map (eval rt env) args in
    apply rt fv avs e.espan
  | EMethod (recv, "push", _, [ arg ]) -> (
    match eval rt env recv with
    | VList arr ->
      let v = eval rt env arg in
      assign rt env recv (VList (Array.append arr [| v |]));
      VUnit
    | _ -> failwith "internal: push")
  | EMethod ({ edesc = EVar en; _ }, name, _, args) when find_var env en = None && Hashtbl.mem rt.enums en ->
    apply rt (ctor_value rt en name) (List.map (eval rt env) args) e.espan
  | EMethod (recv, name, _, args) ->
    let rv = eval rt env recv in
    let avs = List.map (eval rt env) args in
    call_method rt rv name avs e.espan
  | EField ({ edesc = EVar en; _ }, name, _) when find_var env en = None && Hashtbl.mem rt.enums en ->
    ctor_value rt en name
  | EField (recv, name, _) -> field rt (eval rt env recv) name
  | EIndex (a, i) -> (
    match (eval rt env a, eval rt env i) with
    | VList arr, VInt k ->
      if k < 0 || k >= Array.length arr then
        raise (Panic (sprintf "index %d is out of range for a list of length %d" k (Array.length arr), e.espan));
      arr.(k)
    | _ -> failwith "internal: index")
  | ETry x -> (
    match eval rt env x with
    | VEnum (_, ("Ok" | "Some"), [ v ]) -> v
    | VEnum (_, ("Err" | "None"), _) as r -> raise (Return r)
    | _ -> failwith "internal: try")
  | EIf (c, b, els) -> (
    if as_bool (eval rt env c) then eval_block rt env b
    else match els with Some x -> eval rt env x | None -> VUnit)
  | EMatch (s, arms) ->
    let v = eval rt env s in
    let rec go = function
      | [] -> raise (Panic ("no `match` arm matched " ^ show ~top:false v, e.espan))
      | a :: rest ->
        let binds = Hashtbl.create 4 in
        if pmatch a.apat v binds then eval rt (binds :: env) a.abody else go rest
    in
    go arms
  | EBlock b -> eval_block rt env b
  | EClosure (ps, body) -> VClosure (List.map (fun (n, _, _) -> n) ps, body, [ snapshot env ])
  | EStruct (name, _, fields) ->
    let decl = Hashtbl.find rt.structs name in
    let vals = List.map (fun (f, _, x) -> (f, eval rt env x)) fields in
    VStruct (name, List.map (fun (f, _, _) -> (f, List.assoc f vals)) decl.st_fields)
  | EGroup (name, b) ->
    (* The interpreter runs tasks one after another, in spawn order. *)
    let tasks = ref [] in
    let sc = Hashtbl.create 1 in
    Hashtbl.replace sc name (ref (VGroup tasks));
    ignore (eval_block rt (sc :: env) b);
    VList (Array.of_list (List.map (fun c -> apply rt c [] e.espan) (List.rev !tasks)))

and eval_var rt env n sp =
  match find_var env n with
  | Some r -> !r
  | None -> (
    if Hashtbl.mem rt.fns n then VFn n
    else
      match Hashtbl.find_opt rt.ctors n with
      | Some en -> ctor_value rt en n
      | None -> if n = "panic" then VPanicFn else raise (Panic ("internal: unbound name " ^ n, sp)))

and ctor_value rt en vn =
  let e = Hashtbl.find rt.enums en in
  match List.find (fun (v, _, _) -> v = vn) e.en_variants with
  | _, _, [] -> VEnum (en, vn, [])
  | _ -> VCtor (en, vn)

and field rt v name =
  match (v, name) with
  | VStruct (_, fs), _ -> List.assoc name fs
  | VCap CRoot, "net" -> VCap (CNet rt.grants.net)
  | VCap CRoot, "fs" -> (
    match rt.grants.fs with
    | Some (root, ro) -> VCap (CFs { Caps.root = Some root; ro })
    | None -> VCap (CFs { Caps.root = None; ro = true }))
  | VCap CRoot, "stdout" -> VCap CStdout
  | VCap CRoot, "clock" -> VCap CClock
  | _ -> failwith ("internal: field " ^ name)

and apply rt fv args sp =
  match fv with
  | VFn n -> call_fn rt (Hashtbl.find rt.fns n) args
  | VClosure (ps, body, cenv) -> (
    let sc = Hashtbl.create 8 in
    List.iter2 (fun p a -> Hashtbl.replace sc p (ref a)) ps args;
    try eval rt (sc :: cenv) body with Return v -> v)
  | VCtor (en, vn) -> VEnum (en, vn, args)
  | VPanicFn -> (
    match args with [ VStr m ] -> raise (Panic (m, sp)) | _ -> raise (Panic ("panic", sp)))
  | _ -> failwith "internal: not callable"

and call_fn rt d args =
  let sc = Hashtbl.create 8 in
  List.iter2 (fun p a -> Hashtbl.replace sc p.prm_name (ref a)) d.fn_params args;
  try eval_block rt [ sc ] d.fn_body with Return v -> v

and eval_block rt env b =
  let env = Hashtbl.create 8 :: env in
  List.iter (exec rt env) b.stmts;
  match b.tail with Some e -> eval rt env e | None -> VUnit

and exec rt env s =
  match s.sdesc with
  | SLet (_, n, _, _, e) ->
    let v = eval rt env e in
    Hashtbl.replace (List.hd env) n (ref v)
  | SAssign (lhs, rhs) -> assign rt env lhs (eval rt env rhs)
  | SExpr e -> ignore (eval rt env e)
  | SReturn eo -> raise (Return (match eo with Some e -> eval rt env e | None -> VUnit))
  | SWhile (c, b) -> (
    try
      while as_bool (eval rt env c) do
        try ignore (eval_block rt env b) with Continue_ -> ()
      done
    with Break_ -> ())
  | SFor (n, _, e, b) -> (
    match eval rt env e with
    | VList arr -> (
      try
        Array.iter
          (fun v ->
            let sc = Hashtbl.create 1 in
            Hashtbl.replace sc n (ref v);
            try ignore (eval_block rt (sc :: env) b) with Continue_ -> ())
          arr
      with Break_ -> ())
    | _ -> failwith "internal: for")
  | SBreak -> raise Break_
  | SContinue -> raise Continue_

(* Value semantics: assigning into a field or element rebuilds the container. *)
and assign rt env lhs v =
  match lhs.edesc with
  | EVar n -> ( match find_var env n with Some r -> r := v | None -> failwith "internal: assign")
  | EField (r, f, _) -> (
    match eval rt env r with
    | VStruct (sn, fs) -> assign rt env r (VStruct (sn, List.map (fun (k, x) -> if k = f then (k, v) else (k, x)) fs))
    | _ -> failwith "internal: assign field")
  | EIndex (r, i) -> (
    match (eval rt env r, eval rt env i) with
    | VList arr, VInt k ->
      if k < 0 || k >= Array.length arr then
        raise (Panic (sprintf "index %d is out of range for a list of length %d" k (Array.length arr), lhs.espan));
      let a = Array.copy arr in
      a.(k) <- v;
      assign rt env r (VList a)
    | _ -> failwith "internal: assign index")
  | _ -> failwith "internal: assign target"

and call_method rt v name args sp =
  let call f xs = apply rt f xs sp in
  match (v, name, args) with
  | VStr s, "len", [] -> VInt (String.length s)
  | VStr s, "is_empty", [] -> VBool (s = "")
  | VStr s, "lines", [] -> strs (lines s)
  | VStr s, "split", [ VStr sep ] -> strs (split_on s sep)
  | VStr s, "trim", [] -> VStr (String.trim s)
  | VStr s, "contains", [ VStr t ] -> VBool (find_sub s t 0 <> None)
  | VStr s, "starts_with", [ VStr t ] -> VBool (String.starts_with ~prefix:t s)
  | VStr s, "ends_with", [ VStr t ] -> VBool (String.ends_with ~suffix:t s)
  | VStr s, "to_upper", [] -> VStr (String.uppercase_ascii s)
  | VStr s, "to_lower", [] -> VStr (String.lowercase_ascii s)
  | VStr s, "replace", [ VStr a; VStr b ] -> VStr (replace_all s a b)
  | VStr s, "parse_int", [] -> ( match parse_decimal s with Some n -> some (VInt n) | None -> none)
  | VInt n, "to_str", [] -> VStr (string_of_int n)
  | VInt n, "to_float", [] -> VFloat (float_of_int n)
  | VInt n, "abs", [] -> VInt (abs n)
  | VFloat x, "to_str", [] -> VStr (fmt_float x)
  | VFloat x, "to_int", [] -> VInt (int_of_float x)
  | VFloat x, "abs", [] -> VFloat (Float.abs x)
  | VFloat x, "sqrt", [] -> VFloat (Float.sqrt x)
  | VBool b, "to_str", [] -> VStr (string_of_bool b)
  | VList a, "len", [] -> VInt (Array.length a)
  | VList a, "to_list", [] -> VList a
  | VList a, "is_empty", [] -> VBool (Array.length a = 0)
  | VList a, "get", [ VInt i ] -> if i >= 0 && i < Array.length a then some a.(i) else none
  | VList a, "first", [] -> if Array.length a > 0 then some a.(0) else none
  | VList a, "last", [] -> if Array.length a > 0 then some a.(Array.length a - 1) else none
  | VList a, "contains", [ x ] -> VBool (Array.exists (equal x) a)
  | VList a, "reverse", [] ->
    let n = Array.length a in
    VList (Array.init n (fun i -> a.(n - 1 - i)))
  | VList a, "sort", [] ->
    let c = Array.copy a in
    Array.stable_sort compare_v c;
    VList c
  | VList a, "take", [ VInt k ] -> VList (Array.sub a 0 (max 0 (min k (Array.length a))))
  | VList a, "skip", [ VInt k ] ->
    let k = max 0 (min k (Array.length a)) in
    VList (Array.sub a k (Array.length a - k))
  | VList a, "filter", [ f ] -> VList (Array.of_list (List.filter (fun x -> as_bool (call f [ x ])) (Array.to_list a)))
  | VList a, "map", [ f ] -> VList (Array.map (fun x -> call f [ x ]) a)
  | VList a, "fold", [ init; f ] -> Array.fold_left (fun acc x -> call f [ acc; x ]) init a
  | VList a, "join", [ VStr sep ] -> VStr (String.concat sep (List.map (fun x -> show x) (Array.to_list a)))
  | VList a, "sum", [] -> VInt (Array.fold_left (fun acc x -> match x with VInt n -> acc + n | _ -> acc) 0 a)
  | VEnum ("Option", vn, _), "is_some", [] -> VBool (vn = "Some")
  | VEnum ("Option", vn, _), "is_none", [] -> VBool (vn = "None")
  | VEnum (("Option" | "Result"), ("Some" | "Ok"), [ x ]), "unwrap_or", [ _ ] -> x
  | VEnum (("Option" | "Result"), _, _), "unwrap_or", [ d ] -> d
  | VEnum ("Option", "Some", [ x ]), "map", [ f ] -> some (call f [ x ])
  | VEnum ("Option", _, _), "map", [ _ ] -> none
  | VEnum ("Result", vn, _), "is_ok", [] -> VBool (vn = "Ok")
  | VEnum ("Result", vn, _), "is_err", [] -> VBool (vn = "Err")
  | VEnum ("Result", "Err", [ e ]), "map_err", [ f ] -> VEnum ("Result", "Err", [ call f [ e ] ])
  | (VEnum ("Result", _, _) as r), "map_err", [ _ ] -> r
  | VGroup r, "go", [ c ] -> r := c :: !r; VUnit
  | VCap c, _, _ -> cap_call rt c name args
  | _ -> raise (Panic (sprintf "internal: no method `%s` on %s" name (show ~top:false v), sp))

and cap_call rt c name args =
  match (c, name, args) with
  | CStdout, "print", [ VStr s ] -> rt.out (s ^ "\n"); VUnit
  | CStdout, "write", [ VStr s ] -> rt.out s; VUnit
  | CClock, "now_ms", [] -> VInt (int_of_float (Unix.gettimeofday () *. 1000.))
  | CNet allowed, "only", [ VStr h ] -> VCap (CNet (Caps.attenuate allowed h))
  | CNet allowed, "get", [ VStr url ] -> io_result (fun s -> VStr s) (Caps.net_get allowed url)
  | CFs fs, "under", [ VStr p ] -> VCap (CFs (Caps.under fs p))
  | CFs fs, "read_only", [] -> VCap (CFs { fs with ro = true })
  | CFs fs, "read", [ VStr p ] -> io_result (fun s -> VStr s) (Caps.read fs p)
  | CFs fs, "write", [ VStr p; VStr data ] -> io_result (fun () -> VUnit) (Caps.write fs p data)
  | CFs fs, "exists", [ VStr p ] -> VBool (Caps.exists fs p)
  | _ -> failwith ("internal: capability method " ^ name)

(* Run `main`. Returns its value, or the panic message and location. *)
let run ~out ~grants (user_items : item list) : (value, string * Span.t) result =
  let rt =
    { fns = Hashtbl.create 16; structs = Hashtbl.create 16; enums = Hashtbl.create 16;
      ctors = Hashtbl.create 16; out; grants }
  in
  let owners = Hashtbl.create 16 in
  List.iter
    (function
      | IFn d -> Hashtbl.replace rt.fns d.fn_name d
      | IStruct s -> Hashtbl.replace rt.structs s.st_name s
      | IEnum e ->
        Hashtbl.replace rt.enums e.en_name e;
        List.iter
          (fun (v, _, _) -> Hashtbl.replace owners v (e.en_name :: Option.value ~default:[] (Hashtbl.find_opt owners v)))
          e.en_variants)
    (Prelude.items () @ user_items);
  Hashtbl.iter (fun v ens -> match ens with [ en ] -> Hashtbl.replace rt.ctors v en | _ -> ()) owners;
  match Hashtbl.find_opt rt.fns "main" with
  | None -> Error ("no `main` function", Span.dummy)
  | Some d -> (
    let args = match d.fn_params with [] -> [] | _ -> [ VCap CRoot ] in
    try Ok (call_fn rt d args) with
    | Panic (m, sp) -> Error (m, sp)
    | Stack_overflow -> Error ("stack overflow", d.fn_span))
