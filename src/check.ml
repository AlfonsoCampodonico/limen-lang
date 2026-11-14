(* Type checker. Collects every error instead of stopping at the first, and
   enforces the capability rules:
   - there is no ambient I/O (E0302),
   - capabilities and functions cannot hide in struct/enum fields (E0301, E0304),
   - main receives the root capability (E0303). *)
open Ast
open Types

let sprintf = Printf.sprintf

(* Interfaces: name -> methods (parameter types without `self`, result). *)
let ifaces : (string, (string * (ty list * ty)) list) Hashtbl.t = Hashtbl.create 8

(* Built-in bounds with no methods: `Ordered` (the type has `<`) and `Key`
   (the type can be a map key; keys are ordered too). *)
let builtin_bounds = [ "Ordered"; "Key" ]

(* The type parameters of generic interfaces, `interface Container[T]`; the
   methods in [ifaces] mention them as TParam. *)
let iface_gen : (string, string list) Hashtbl.t = Hashtbl.create 8

(* A bound: an interface and its type arguments, `Container[Int]`. *)
type bound = string * ty list

let show_bound ((i, args) : bound) = if args = [] then i else i ^ "[" ^ String.concat ", " (List.map show args) ^ "]"
let dyn_of ((i, a) : bound) = TDyn (i, a)
let subst_bound env ((i, args) : bound) : bound = (i, List.map (subst env) args)

(* The methods of an interface, its parameters replaced by the bound's arguments. *)
let iface_methods ((i, args) : bound) =
  let ms = Option.value ~default:[] (Hashtbl.find_opt ifaces i) in
  match Hashtbl.find_opt iface_gen i with
  | Some gs when gs <> [] && List.length gs = List.length args ->
    let env = List.combine gs args in
    List.map (fun (m, (ps, r)) -> (m, (List.map (subst env) ps, subst env r))) ms
  | _ -> ms

(* Bounds on the type parameters of structs and enums, `struct Sorted[T: Ordered]`:
   type -> its parameters, and each parameter's bounds in terms of them. *)
let type_bounds : (string, string list * (string * bound list) list) Hashtbl.t = Hashtbl.create 8

(* Instantiations to check against their bounds once the body's types are
   known: (where, the type used, the bound, the type parameter). *)
let pending : (Span.t * ty * bound * string) list ref = ref []

(* Bounds of generic interfaces at calls: their arguments that are type
   parameters of the callee are inferred from the type's methods as soon as
   the type is known (see [infer_pending]). *)
let to_infer : (ty * bound) list ref = ref []

(* A use of struct or enum [n] with these type arguments: its bounds apply. *)
let add_type_bounds n args sp =
  match Hashtbl.find_opt type_bounds n with
  | Some (gen, bs) when List.length gen = List.length args ->
    let env = List.combine gen args in
    List.iter (fun (g, ifs) -> let t = List.assoc g env in List.iter (fun b -> pending := (sp, t, subst_bound env b, g) :: !pending) ifs) bs
  | _ -> ()

(* Type parameters of the declaration being resolved, used as an array's
   length (true) or as a type (false). One cannot be both. *)
let param_kinds : (string, bool) Hashtbl.t = Hashtbl.create 8



type var_kind = KLet | KParam | KPat | KGroup
type var_info = { vty : ty; vmut : bool; vspan : Span.t; vkind : var_kind }
type fsig = { gen : string list; ps : ty list; ret : ty }
type scope = { mutable vars : (string, var_info) Hashtbl.t; barrier : bool }

(* The variables of a scope that has none yet; [bind] gives the scope a
   table of its own, so the many blocks that bind nothing allocate none. *)
let no_vars : (string, var_info) Hashtbl.t = Hashtbl.create 1

type ctx = {
  structs : (string, struct_decl) Hashtbl.t;
  enums : (string, enum_decl) Hashtbl.t;
  sfields : (string, (string * ty) list) Hashtbl.t; (* field types use TParam *)
  variants : (string, (string * ty list) list) Hashtbl.t;
  owners : (string, string list) Hashtbl.t; (* variant -> enums defining it *)
  fns : (string, fsig) Hashtbl.t;
  mutable diags : Diag.t list;
}

type fctx = {
  ret_ty : ty;
  mutable scopes : scope list;
  mutable loops : int;
  genv : (string * ty) list;
  mutable in_group : int; (* inside a group body: no return, no `?` *)
  mutable in_defer : int; (* inside a deferred body: nothing may leave it *)
  ret_te : ty_expr option; (* the declared result type, for fix-its *)
}

(* [via]: for each capability not written in the parameters' types, the
   path through the struct or enum that holds it, `Client.net`. *)
type fn_info = { name : string; span : Span.t; caps : cap list; via : (cap * string) list; callbacks : bool }

(* The capabilities values of these types hold, in [all_caps] order, each
   with where it comes from when no type writes it: the first type's path. *)
let authority_of ts =
  let found = List.fold_left (fun acc t -> caps_in t acc) [] ts in
  let written = List.fold_left (fun acc t -> caps_written t acc) [] ts in
  List.filter_map
    (fun c ->
      if not (List.mem c found) then None
      else if List.mem c written then Some (c, None)
      else Some (c, List.find_map (fun t -> via t c) ts))
    all_caps

(* "`T` holds capability `C`", and through which field when T does not
   write it, for the messages that refuse a type holding authority. *)
let held t =
  match authority_of [ t ] with
  | (c, Some path) :: _ -> Some (sprintf "`%s` holds capability `%s` through `%s`" (show t) (cap_name c) path)
  | (c, None) :: _ -> Some (sprintf "`%s` holds capability `%s`" (show t) (cap_name c))
  | [] -> None

let err ctx ?notes ?fix code span msg = ctx.diags <- Diag.make ?notes ?fix code span msg :: ctx.diags
let plural n = if n = 1 then "" else "s"

let lev a b =
  let la = String.length a and lb = String.length b in
  let d = Array.make_matrix (la + 1) (lb + 1) 0 in
  for i = 0 to la do d.(i).(0) <- i done;
  for j = 0 to lb do d.(0).(j) <- j done;
  for i = 1 to la do
    for j = 1 to lb do
      let c = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      d.(i).(j) <- min (min (d.(i - 1).(j) + 1) (d.(i).(j - 1) + 1)) (d.(i - 1).(j - 1) + c)
    done
  done;
  d.(la).(lb)

let suggest name cands =
  let limit = if String.length name <= 3 then 1 else max 2 (String.length name / 3) in
  List.fold_left
    (fun best c ->
      if c = name then best
      else
        let d = lev name c in
        match best with
        | Some (_, bd) when bd <= d -> best
        | _ -> if d <= limit then Some (c, d) else best)
    None cands
  |> Option.map fst

let did_you_mean ctx ?(notes = []) code span msg name cands =
  match suggest name cands with
  | Some s ->
    err ctx ~notes
      ~fix:{ Diag.fmsg = sprintf "did you mean `%s`?" s; edits = [ { Diag.at = span; replacement = s } ] }
      code span msg
  | None -> err ctx ~notes code span msg

let param_kind ctx g len sp =
  match Hashtbl.find_opt param_kinds g with
  | Some k when k <> len ->
    err ctx "E0102" sp (sprintf "`%s` is used both as a type and as an array length" g)
      ~notes:[ "a type parameter in an array's size stands for a length, as in `fn sum[N](xs: Array[Int, N])`; use another parameter for the type" ]
  | Some _ -> ()
  | None -> Hashtbl.replace param_kinds g len

let unify_at ctx span want got =
  try unify want got
  with Mismatch -> err ctx "E0201" span (sprintf "expected `%s`, found `%s`" (show want) (show got))

let builtin_types = [ "Int"; "Float"; "Bool"; "Str"; "Bytes"; "List"; "Array"; "Chan"; "Root"; "Net"; "Fs"; "Stdout"; "Stderr"; "Clock"; "Listen"; "Listener"; "Conn"; "Rand"; "Stdin"; "Env"; "Exec"; "Foreign"; "Map" ]

(* Names other languages use for ambient I/O, mapped to the capability way. *)
let ambient =
  [ ("print", "Stdout", "out.print(...)"); ("println", "Stdout", "out.print(...)");
    ("eprint", "Stderr", "stderr.print(...)"); ("puts", "Stdout", "out.print(...)");
    ("open", "Fs", "fs.read(path)"); ("read_file", "Fs", "fs.read(path)");
    ("write_file", "Fs", "fs.write(path, text)"); ("fetch", "Net", "net.get(url)");
    ("http_get", "Net", "net.get(url)"); ("now", "Clock", "clock.now_ms()");
    ("time", "Clock", "clock.now_ms()"); ("sleep", "Clock", "clock.sleep(ms)"); ("random", "Rand", "rand.int(lo, hi)");
    ("rand", "Rand", "rand.int(lo, hi)"); ("input", "Stdin", "stdin.read_line()");
    ("read_line", "Stdin", "stdin.read_line()"); ("getenv", "Env", "env.get(name)");
    ("system", "Exec", "exec.run(program, args)"); ("spawn", "Exec", "exec.run(program, args)");
    ("popen", "Exec", "exec.run(program, args)") ]

(* ---------- type expressions ---------- *)

let rec resolve ctx env (te : ty_expr) : ty =
  let iface_note n =
    let gs = Option.value ~default:[] (Hashtbl.find_opt iface_gen n) in
    if gs = [] then sprintf "take a type parameter bounded by it: `fn f[T: %s](x: T)`" n
    else
      let gs = String.concat ", " gs in
      sprintf "take a type parameter bounded by it: `fn f[C: %s[%s], %s](x: C)`" n gs gs
  in
  match te.tdesc with
  | TyUnit -> TUnit
  | TyFn (ps, r) -> TFn (List.map (resolve ctx env) ps, resolve ctx env r)
  | TyInt _ ->
    err ctx "E0102" te.tspan "a number is only allowed as the size of an `Array`, as in `Array[Int, 3]`";
    TErr
  | TyName ("Array", [ elt; { tdesc = TyInt n; _ } ]) ->
    if n < 0 then (err ctx "E0102" te.tspan "an array size cannot be negative"; TErr)
    else TArray (resolve ctx env elt, TLen n)
  | TyName ("Array", [ elt; { tdesc = TyName (g, []); tspan } ]) when List.mem_assoc g env ->
    (* generic over the length: `fn sum[N](xs: Array[Int, N])` *)
    param_kind ctx g true tspan;
    TArray (resolve ctx env elt, List.assoc g env)
  | TyName ("Array", _) ->
    err ctx "E0109" te.tspan "`Array` takes an element type and a size, as in `Array[Float, 4]`"
      ~notes:[ "the size is a number, or a type parameter that stands for one: `fn sum[N](xs: Array[Int, N])`" ];
    TErr
  | TyName ("dyn", [ { tdesc = TyName (i, _); tspan } ]) when List.mem i builtin_bounds ->
    err ctx "E0102" tspan (sprintf "`%s` is a built-in bound, not an interface with methods; it cannot be `dyn`" i)
      ~notes:[ sprintf "use it on a type parameter: `fn f[T: %s](x: T)`" i ];
    TErr
  | TyName ("dyn", [ { tdesc = TyName (i, args); tspan } ]) ->
    if Hashtbl.mem ifaces i then (match bound_args ctx env i args tspan with Some a -> TDyn (i, a) | None -> TErr)
    else begin
      did_you_mean ctx "E0102" tspan (sprintf "unknown interface `%s`" i) i (List.of_seq (Hashtbl.to_seq_keys ifaces));
      TErr
    end
  | TyName (n, args) -> (
    let args' = List.map (resolve ctx env) args in
    let want k =
      if List.length args <> k then begin
        err ctx "E0109" te.tspan
          (sprintf "`%s` takes %d type argument%s, found %d" n k (plural k) (List.length args));
        false
      end
      else true
    in
    let simple t = if want 0 then t else TErr in
    match n with
    | "Int" -> simple TInt
    | "Float" -> simple TFloat
    | "Bool" -> simple TBool
    | "Str" -> simple TStr
    | "Bytes" -> simple TBytes
    | "List" -> if want 1 then TList (List.hd args') else TErr
    | "Root" -> simple (TCap Root)
    | "Net" -> simple (TCap Net)
    | "Fs" -> simple (TCap Fs)
    | "Stdout" -> simple (TCap Stdout)
    | "Stderr" -> simple (TCap Stderr)
    | "Clock" -> simple (TCap Clock)
    | "Listen" -> simple (TCap Listen)
    | "Listener" -> simple (TCap Listener)
    | "Conn" -> simple (TCap Conn)
    | "Rand" -> simple (TCap Rand)
    | "Stdin" -> simple (TCap Stdin)
    | "Env" -> simple (TCap Env)
    | "Exec" -> simple (TCap Exec)
    | "Foreign" -> simple (TCap Foreign)
    | "Chan" -> if want 1 then TChan (List.hd args') else TErr
    | "Map" -> if want 2 then TMap (List.nth args' 0, List.nth args' 1) else TErr
    | _ -> (
      match List.assoc_opt n env with
      | Some t -> param_kind ctx n false te.tspan; simple t
      | None -> (
        let gen =
          match Hashtbl.find_opt ctx.structs n with
          | Some s -> Some s.st_gen
          | None -> Option.map (fun e -> e.en_gen) (Hashtbl.find_opt ctx.enums n)
        in
        match gen with
        | Some g ->
          if want (List.length g) then begin
            add_type_bounds n args' te.tspan;
            TNamed (n, args')
          end
          else TErr
        | None ->
          let cands =
            builtin_types
            @ List.of_seq (Hashtbl.to_seq_keys ctx.structs)
            @ List.of_seq (Hashtbl.to_seq_keys ctx.enums)
            @ List.map fst env
          in
          if Hashtbl.mem ifaces n then
            err ctx "E0102" te.tspan (sprintf "`%s` is an interface, not a type" n) ~notes:[ iface_note n ]
          else did_you_mean ctx "E0102" te.tspan (sprintf "unknown type `%s`" n) n cands;
          TErr)))

(* The type arguments of interface [i], written [args]: exactly as many as it has parameters. *)
and bound_args ctx env i args sp =
  let gs = Option.value ~default:[] (Hashtbl.find_opt iface_gen i) in
  if List.length gs = List.length args then Some (List.map (resolve ctx env) args)
  else begin
    let k = List.length gs in
    err ctx "E0109" sp (sprintf "interface `%s` takes %d type argument%s, found %d" i k (plural k) (List.length args))
      ~notes:
        [ (if k = 0 then sprintf "write it without arguments: `%s`" i
           else sprintf "write it with its type argument%s: `%s[%s]`" (plural k) i (String.concat ", " gs)) ];
    None
  end

(* A bound on a type parameter: `Shape`, `Ordered`, `Container[Int]`. *)
and resolve_bound ctx env (b : ty_expr) : bound option =
  match b.tdesc with
  | TyName (i, args) when Hashtbl.mem ifaces i -> Option.map (fun a -> (i, a)) (bound_args ctx env i args b.tspan)
  | TyName (i, _) ->
    did_you_mean ctx "E0102" b.tspan (sprintf "unknown interface `%s`" i) i (List.of_seq (Hashtbl.to_seq_keys ifaces));
    None
  | _ -> err ctx "E0102" b.tspan "a bound is an interface"; None

(* Channels and functions cannot hide in a struct or enum; capabilities can,
   and the types holding them are tracked (Types.authority). *)
let check_storable ctx span t =
  if has_chan t then
    err ctx "E0305" span
      (sprintf "channel type `%s` cannot be stored in a struct or enum field" (show t))
      ~notes:[ "pass the channel as a parameter or capture it in a task's closure instead" ]
  else if has_fn t then
    err ctx "E0304" span
      (sprintf "function type `%s` cannot be stored in a struct or enum field" (show t))
      ~notes:[ "a stored closure could carry a capability out of sight; pass it as a parameter instead" ]

(* ---------- scopes ---------- *)

let push f = f.scopes <- { vars = no_vars; barrier = false } :: f.scopes
let pop f = f.scopes <- List.tl f.scopes
let bind f name info =
  let s = List.hd f.scopes in
  if s.vars == no_vars then s.vars <- Hashtbl.create 8;
  Hashtbl.replace s.vars name info

(* Returns the variable and whether the lookup crossed a closure boundary. *)
let rec lookup_in name crossed = function
  | [] -> None
  | s :: rest -> (
    match if s.vars == no_vars then None else Hashtbl.find_opt s.vars name with
    | Some v -> Some (v, crossed)
    | None -> lookup_in name (crossed || s.barrier) rest)

let lookup f name = lookup_in name false f.scopes

(* [lookup] for when whether it crossed a closure does not matter *)
let rec find_in name = function
  | [] -> None
  | s :: rest -> ( match if s.vars == no_vars then None else Hashtbl.find_opt s.vars name with None -> find_in name rest | found -> found)

let find_var f name = find_in name f.scopes
let rec bound_in name = function [] -> false | s :: rest -> (s.vars != no_vars && Hashtbl.mem s.vars name) || bound_in name rest
let is_local f name = bound_in name f.scopes

let visible_names ctx f =
  List.concat_map (fun s -> List.of_seq (Hashtbl.to_seq_keys s.vars)) f.scopes
  @ List.of_seq (Hashtbl.to_seq_keys ctx.fns)
  @ List.of_seq (Hashtbl.to_seq_keys ctx.owners)
  @ [ "panic"; "channel"; "assert"; "assert_eq"; "map"; "catch"; "bytes"; "keep"; "embed"; "embed_bytes"; "embed_dir" ]

(* Generic functions' bounds: function -> type parameter -> interfaces. *)
let fn_bounds : (string, (string * bound list) list) Hashtbl.t = Hashtbl.create 8

(* Every function's bounds as seen inside it, declared and implied, and its
   type parameters that stand for array lengths. *)
let fn_cur_bounds : (string, (string * bound list) list) Hashtbl.t = Hashtbl.create 8
let fn_kinds : (string, (string * bool) list) Hashtbl.t = Hashtbl.create 8

(* Bounds of the type parameters of the function being checked: the ones
   it declares, and the ones its parameter types imply (`self: Sorted[T]`
   gives T the bounds of `Sorted`'s parameter). *)
let cur_bounds : (string * bound list) list ref = ref []

let bounds_of g = Option.value ~default:[] (List.assoc_opt g !cur_bounds)

(* Whether type parameter [g] of the function being checked has bound [b]. *)
let bounded g b =
  let bs = List.map fst (bounds_of g) in
  List.mem b bs || (b = "Ordered" && List.mem "Key" bs)

(* Key types of maps created in the function being checked: they must end
   up Int, Str or Bool. *)
let map_keys : (Span.t * ty) list ref = ref []

let add_bounds fname env sp =
  match Hashtbl.find_opt fn_bounds fname with
  | None -> ()
  | Some bs ->
    List.iter
      (fun (g, ifs) ->
        match List.assoc_opt g env with
        | Some t ->
          List.iter
            (fun b ->
              let b = subst_bound env b in
              pending := (sp, t, b, g) :: !pending;
              if snd b <> [] then to_infer := (t, b) :: !to_infer)
            ifs
        | None -> ())
      bs

(* The built-in methods written in Tessera that the program calls: only their
   bodies are checked (and only they are compiled). *)
let used_methods : (string, unit) Hashtbl.t = Hashtbl.create 16

(* Methods by `Type::name`: whether they are `pub`, and the file (module)
   that defines them. A private method can only be called from its own file. *)
let method_owner : (string, bool * string) Hashtbl.t = Hashtbl.create 16

(* Foreign functions can only be called directly, not used as values. *)
let foreign_fns : (string, unit) Hashtbl.t = Hashtbl.create 8

(* Parameter names, for calls with named arguments: declared functions and
   methods (keyed like [ctx.fns], `self` included) and interface methods
   (keyed `Interface::method`, without `self`). *)
let param_names : (string, string list) Hashtbl.t = Hashtbl.create 64

let is_named (a : expr) = match a.edesc with ENamed _ -> true | _ -> false

let unlabel (a : expr) = match a.edesc with ENamed (_, _, v) -> v | _ -> a

let arg_counter = ref 0
let calling = ref false

(* The methods defined on user type [n], by name. *)
let user_methods ctx n =
  let pre = n ^ "::" in
  Hashtbl.fold
    (fun k _ acc ->
      if String.starts_with ~prefix:pre k then String.sub k (String.length pre) (String.length k - String.length pre) :: acc else acc)
    ctx.fns []
  |> List.sort compare

let instantiate s =
  let env = List.map (fun g -> (g, fresh ())) s.gen in
  (env, TFn (List.map (subst env) s.ps, subst env s.ret))

(* The enums a bare variant name can mean in [file]: the file's own enums
   shadow the prelude's, which shadow other modules'. More than one at the
   nearest level is ambiguous. *)
let variant_scope ctx v file =
  let ens = Option.value ~default:[] (Hashtbl.find_opt ctx.owners v) in
  let from fl = List.filter (fun en -> (Hashtbl.find ctx.enums en).en_span.file = fl) ens in
  match from file with
  | [] -> ( match from Prelude.file with [] -> ens | p -> p)
  | own -> own

let ctor_ty ctx en v sp =
  let e = Hashtbl.find ctx.enums en in
  let targs = List.map (fun _ -> fresh ()) e.en_gen in
  add_type_bounds en targs sp;
  let env = List.combine e.en_gen targs in
  let payload = List.assoc v (Hashtbl.find ctx.variants en) in
  let result = TNamed (en, targs) in
  match payload with [] -> result | ps -> TFn (List.map (subst env) ps, result)

let struct_fields ctx n args =
  let s = Hashtbl.find ctx.structs n in
  let env = List.combine s.st_gen args in
  List.map (fun (f, t) -> (f, subst env t)) (Hashtbl.find ctx.sfields n)

(* ---------- builtin methods ---------- *)

(* The name a built-in type's methods are keyed by (`Str::shout`), if a
   program can add methods to it. *)
let builtin_tname t =
  match repr t with
  | TInt -> Some "Int"
  | TFloat -> Some "Float"
  | TBool -> Some "Bool"
  | TStr -> Some "Str"
  | TBytes -> Some "Bytes"
  | TList _ -> Some "List"
  | TMap _ -> Some "Map"
  | TNamed (("Option" | "Result" | "IoError") as n, _) -> Some n
  | _ -> None

let build_methods (t : ty) : (string * (unit -> ty list * ty)) list =
  let m name ps r = (name, fun () -> (ps, r)) in
  match t with
  | TStr ->
    [ m "len" [] TInt; m "is_empty" [] TBool; m "lines" [] (TList TStr);
      m "split" [ TStr ] (TList TStr); m "trim" [] TStr; m "contains" [ TStr ] TBool;
      m "starts_with" [ TStr ] TBool; m "ends_with" [ TStr ] TBool; m "to_upper" [] TStr;
      m "to_lower" [] TStr; m "replace" [ TStr; TStr ] TStr; m "parse_int" [] (opt TInt);
      m "parse_float" [] (opt TFloat); m "slice" [ TInt; TInt ] TStr; m "index_of" [ TStr ] (opt TInt);
      m "last_index_of" [ TStr ] (opt TInt);
      m "bytes" [] (TList TInt); m "chars" [] (TList TStr); m "to_bytes" [] TBytes; m "message" [] TStr ]
  | TBytes ->
    [ m "len" [] TInt; m "is_empty" [] TBool; m "get" [ TInt ] (opt TInt); m "slice" [ TInt; TInt ] TBytes;
      m "to_list" [] (TList TInt); m "to_str" [] TStr ]
  | TInt ->
    [ m "to_str" [] TStr; m "to_float" [] TFloat; m "abs" [] TInt; m "to_char" [] TStr; m "min" [ TInt ] TInt;
      m "max" [ TInt ] TInt; m "shift_right_unsigned" [ TInt ] TInt ]
  | TFloat -> [ m "to_str" [] TStr; m "to_int" [] TInt; m "abs" [] TFloat; m "sqrt" [] TFloat;
      m "to_fixed" [ TInt ] TStr ]
  | TBool -> [ m "to_str" [] TStr ]
  (* the types of a type with parameters are built only for the method asked for *)
  | TList a ->
    [ ("len", fun () -> ([], TInt)); ("is_empty", fun () -> ([], TBool)); ("get", fun () -> ([ TInt ], opt a)); ("first", fun () -> ([], opt a));
      ("last", fun () -> ([], opt a)); ("push", fun () -> ([ a ], TUnit)); ("contains", fun () -> ([ a ], TBool));
      ("reverse", fun () -> ([], TList a)); ("sort", fun () -> ([], TList a)); ("take", fun () -> ([ TInt ], TList a));
      ("skip", fun () -> ([ TInt ], TList a)); ("filter", fun () -> ([ TFn ([ a ], TBool) ], TList a));
      ("map", fun () -> let u = fresh () in ([ TFn ([ a ], u) ], TList u));
      ("fold", fun () -> let u = fresh () in ([ u; TFn ([ u; a ], u) ], u));
      ("join", fun () -> ([ TStr ], TStr)); ("sum", fun () -> ([], TInt)) ]
  | TMap (k, v) ->
    [ ("len", fun () -> ([], TInt)); ("is_empty", fun () -> ([], TBool)); ("get", fun () -> ([ k ], opt v)); ("contains", fun () -> ([ k ], TBool));
      ("remove", fun () -> ([ k ], TUnit)); ("keys", fun () -> ([], TList k)); ("values", fun () -> ([], TList v));
      ("entries", fun () -> ([], TList (TNamed (Ast.tuple_name 2, [ k; v ])))) ]
  | TArray (a, _) ->
    [ ("len", fun () -> ([], TInt)); ("get", fun () -> ([ TInt ], opt a)); ("to_list", fun () -> ([], TList a)); ("contains", fun () -> ([ a ], TBool)) ]
  | TNamed ("Option", [ a ]) ->
    [ ("is_some", fun () -> ([], TBool)); ("is_none", fun () -> ([], TBool));
      ("map", fun () -> let u = fresh () in ([ TFn ([ a ], u) ], opt u)) ]
  | TNamed ("IoError", []) -> [ m "message" [] TStr ]
  | TNamed ("Result", [ a; e ]) ->
    [ m "is_ok" [] TBool; m "is_err" [] TBool;
      ("map_error", fun () -> let u = fresh () in ([ TFn ([ e ], u) ], res a u)) ]
  | TCap Net -> [ m "get" [ TStr ] (res (TNamed ("HttpResponse", [])) io_err); m "only" [ TStr ] (TCap Net); m "connect" [ TStr ] (res (TCap Conn) io_err); m "connect_tls" [ TStr ] (res (TCap Conn) io_err) ]
  | TCap Listen -> [ m "on" [ TStr ] (res (TCap Listener) io_err); m "on_tls" [ TStr; TStr; TStr ] (res (TCap Listener) io_err) ]
  | TCap Listener -> [ m "accept" [] (res (TCap Conn) io_err); m "port" [] TInt; m "close" [] TUnit ]
  | TCap Conn -> [ m "read" [] (res TStr io_err); m "write" [ TStr ] (res TUnit io_err); m "close" [] TUnit ]
  | TCap Fs ->
    [ m "read" [ TStr ] (res TStr io_err); m "write" [ TStr; TStr ] (res TUnit io_err);
      m "append" [ TStr; TStr ] (res TUnit io_err); m "list" [ TStr ] (res (TList TStr) io_err);
      m "remove" [ TStr ] (res TUnit io_err); m "make_dir" [ TStr ] (res TUnit io_err);
      m "is_dir" [ TStr ] TBool; m "full_path" [ TStr ] (res TStr io_err); m "is_read_only" [] TBool;
      m "exists" [ TStr ] TBool; m "under" [ TStr ] (TCap Fs); m "read_only" [] (TCap Fs);
      m "rename" [ TStr; TStr ] (res TUnit io_err); m "info" [ TStr ] (res (TNamed ("FileInfo", [])) io_err) ]
  | TCap (Stdout | Stderr) -> [ m "print" [ TStr ] TUnit; m "write" [ TStr ] TUnit ]
  | TCap Clock -> [ m "now_ms" [] TInt; m "sleep" [ TInt ] TUnit ]
  | TCap Rand -> [ m "int" [ TInt; TInt ] TInt; m "float" [] TFloat ]
  | TCap Stdin -> [ m "read_line" [] (opt TStr); m "read_all" [] TStr ]
  | TCap Env -> [ m "get" [ TStr ] (opt TStr) ]
  | TCap Exec ->
    [ m "run" [ TStr; TList TStr ] (res (TNamed ("ProcessOutput", [])) io_err); m "only" [ TStr ] (TCap Exec) ]
  | TCap Foreign ->
    [ m "only" [ TStr ] (TCap Foreign); m "allows" [ TStr ] TBool; m "copy" [ TNamed ("Handle", []); TInt ] TBytes ]
  | TCap Root -> [ m "args" [] (TList TStr) ]
  | TChan a -> [ ("send", fun () -> ([ a ], TUnit)); ("recv", fun () -> ([], opt a)); ("close", fun () -> ([], TUnit)) ]
  | _ -> []

(* The methods of a built-in type, by name. Checking asks for them at every
   method call, so the lists of the types without parameters are built once. *)
let str_methods = build_methods TStr
let bytes_methods = build_methods TBytes
let int_methods = build_methods TInt
let float_methods = build_methods TFloat
let bool_methods = build_methods TBool

let methods (t : ty) =
  match t with
  | TStr -> str_methods
  | TBytes -> bytes_methods
  | TInt -> int_methods
  | TFloat -> float_methods
  | TBool -> bool_methods
  | _ -> build_methods t

(* Types with an order, for `<` and `sort()`: numbers, strings (bytes),
   Bool (false first), (), and lists, arrays, tuples, structs and enums of
   ordered types, compared item by item (enum variants in declaration order).
   Returns the first type in the way, if any. *)
let unordered ctx t : ty option =
  let seen = Hashtbl.create 8 in
  let rec go t =
    match repr t with
    | TInt | TFloat | TStr | TBytes | TBool | TUnit | TVar _ | TErr -> None
    | TParam g when bounded g "Ordered" -> None
    | TList a | TArray (a, _) -> go a
    | TNamed (n, args) as nt -> (
      let key = show nt in
      if Hashtbl.mem seen key then None
      else begin
        Hashtbl.replace seen key ();
        match Hashtbl.find_opt ctx.structs n with
        | Some s ->
          let env = List.combine s.st_gen args in
          List.find_map (fun (_, ft) -> go (subst env ft)) (Option.value ~default:[] (Hashtbl.find_opt ctx.sfields n))
        | None -> (
          match Hashtbl.find_opt ctx.enums n with
          | Some e ->
            let env = List.combine e.en_gen args in
            List.find_map (fun (_, ps) -> List.find_map (fun p -> go (subst env p)) ps) (Option.value ~default:[] (Hashtbl.find_opt ctx.variants n))
          | None -> Some nt)
      end)
    | t -> Some t
  in
  go t

let root_fields = [ ("net", TCap Net); ("fs", TCap Fs); ("stdout", TCap Stdout); ("stderr", TCap Stderr); ("clock", TCap Clock); ("listen", TCap Listen);
    ("rand", TCap Rand); ("stdin", TCap Stdin); ("env", TCap Env); ("exec", TCap Exec);
    ("foreign", TCap Foreign) ]

(* ---------- expressions ---------- *)

(* ---------- struct embedding (see embed.ml) ---------- *)

(* A method struct [t] declares itself, not one forwarded from an embedded struct. *)
let own_method ctx t name = Hashtbl.mem ctx.fns (t ^ "::" ^ name) && not (Embed.is_forwarder (t ^ "::" ^ name))

(* Where member [name] of struct [n] lives. A field access (`x.name`) looks
   for fields and methods; a call (`x.name()`) for methods only. *)
let promote ctx ~fields n name =
  Embed.lookup ~structs:(Hashtbl.find_opt ctx.structs)
    ~has:(fun t -> (fields && List.mem_assoc name (Option.value ~default:[] (Hashtbl.find_opt ctx.sfields t))) || own_method ctx t name)
    n

(* [recv] (of type [rt], already checked) followed by the embedded fields of
   [path]: the explicit form of a promoted access, `admin.User`. The added
   field names get an empty span, so tools see only what was written. *)
let embed_path ctx (recv : expr) rt path =
  List.fold_left
    (fun ((x : expr), t) fld ->
      let ft =
        match repr t with
        | TNamed (n, args) when Hashtbl.mem ctx.sfields n -> Option.value ~default:TErr (List.assoc_opt fld (struct_fields ctx n args))
        | _ -> TErr
      in
      ({ edesc = EField (x, fld, { x.espan with lo = x.espan.hi }); espan = x.espan; ety = ft; einst = []; ecallee = None }, ft))
    (recv, rt) path

let ambiguity n name paths =
  sprintf "`%s` is ambiguous in `%s`: it is reached through %s, at the same depth" name n
    (String.concat " and " (List.map (fun p -> "`" ^ String.concat "." (p @ [ name ]) ^ "`") paths))

let ambiguous ctx sp n name paths =
  err ctx "E0120" sp (ambiguity n name paths)
    ~notes:[ sprintf "name the embedded struct you mean: %s" (String.concat " or " (List.map (fun p -> "`." ^ String.concat "." (p @ [ name ]) ^ "`") paths)) ]

(* Does [t] have every method of bound [b], with the same types? Unifying
   the method types infers the bound's arguments where they are unknown. *)
let satisfies ctx t ((i, args) as b : bound) : string option =
  let ms = iface_methods b in
  let t = repr t in
  let name = show_bound b in
  let same_args a = List.length a = List.length args && (try List.iter2 unify args a; true with Mismatch -> false) in
  let check (m, (ps, r)) =
    let want = TFn (ps, r) in
    let trial got = try unify want got; None with Mismatch -> Some (sprintf "its `%s` has type `%s`, but `%s` needs `%s`" m (show got) name (show want)) in
    match t with
    | TVar _ | TErr -> None
    | TDyn (j, ja) -> (
      match List.assoc_opt m (iface_methods (j, ja)) with
      | Some (ps', r') -> trial (TFn (ps', r'))
      | None -> Some (sprintf "`%s` has no method `%s`" (show t) m))
    | TParam g -> (
      match List.find_map (fun b' -> List.assoc_opt m (iface_methods b')) (bounds_of g) with
      | Some (ps', r') -> trial (TFn (ps', r'))
      | None -> Some (sprintf "type parameter `%s` is not bounded by `%s`; add it: `[%s: %s]`" g name g name))
    | TNamed (n, _) when Hashtbl.mem ctx.fns (n ^ "::" ^ m) -> (
      let _, ft = instantiate (Hashtbl.find ctx.fns (n ^ "::" ^ m)) in
      match ft with
      | TFn (p0 :: rest, r') -> (try unify p0 t; trial (TFn (rest, r')) with Mismatch -> trial (TFn (rest, r')))
      | _ -> Some (sprintf "its `%s` is not a method" m))
    | TNamed (n, _) when (match promote ctx ~fields:false n m with Embed.Ambiguous _ -> true | _ -> false) -> (
      match promote ctx ~fields:false n m with Embed.Ambiguous ps -> Some (ambiguity n m ps) | _ -> None)
    | _ -> (
      match List.assoc_opt m (methods t) with
      | Some mk -> let ps', r' = mk () in trial (TFn (ps', r'))
      | None -> Some (sprintf "`%s` has no method `%s`" (show t) m))
  in
  (* `cause` is optional in `Error`: a type without one has no cause *)
  let check (m, sg) =
    let own = match t with TNamed (n, _) -> Hashtbl.mem ctx.fns (n ^ "::" ^ m) || List.mem_assoc m (methods t) | TParam _ | TDyn _ -> true | _ -> List.mem_assoc m (methods t) in
    if i = "Error" && m = "cause" && not own then None else check (m, sg)
  in
  match t with
  | TDyn (j, ja) when j = i && same_args ja -> None
  | TParam g when List.exists (fun (j, ja) -> j = i && same_args ja) (bounds_of g) -> None
  | _ -> List.find_map check ms

(* Infer the arguments of the generic-interface bounds of the calls made so
   far whose type is now known, so `first(xs)` has its result type at once. *)
let infer_pending ctx =
  if !to_infer <> [] then
    to_infer := List.filter (fun (t, b) -> match repr t with TVar _ -> true | _ -> ignore (satisfies ctx t b); false) !to_infer


(* The functions `Type::name` and `module.Type::name` by their `Type::name`
   part, so finding a method a file added to a built-in type is a lookup, not
   a scan of every function (which made checking quadratic). Rebuilt when the
   set of functions has changed. *)
let methods_index : (string, string) Hashtbl.t = Hashtbl.create 64
let methods_index_of = ref None (* the table indexed, and its size then *)

let methods_named ctx suffix =
  let fresh = match !methods_index_of with Some (t, n) -> t == ctx.fns && n = Hashtbl.length ctx.fns | None -> false in
  if not fresh then begin
    Hashtbl.reset methods_index;
    Hashtbl.iter
      (fun k _ ->
        match Interp_util.find_sub k "::" 0 with
        | Some i ->
          let ty = String.sub k 0 i in
          let base = match String.rindex_opt ty '.' with Some d -> String.sub ty (d + 1) (String.length ty - d - 1) | None -> ty in
          Hashtbl.add methods_index (base ^ String.sub k i (String.length k - i)) k
        | None -> ())
      ctx.fns;
    methods_index_of := Some (ctx.fns, Hashtbl.length ctx.fns)
  end;
  Hashtbl.find_all methods_index suffix

let rec check_expr ctx f e : ty =
  let t = check_expr_inner ctx f e in
  (match e.edesc with ECall _ | EMethod _ -> infer_pending ctx | _ -> ());
  e.ety <- t;
  t

and check_expr_inner ctx f e : ty =
  match e.edesc with
  | EInt _ -> TInt
  | EFloat _ -> TFloat
  | EBool _ -> TBool
  | EUnit -> TUnit
  | EStr parts ->
    List.iter
      (function
        | SPLit _ -> ()
        | SPExpr x -> (
          let t = check_expr ctx f x in
          match repr t with
          | TFn _ | TCap _ | TChan _ ->
            err ctx "E0201" x.espan (sprintf "cannot interpolate a value of type `%s`" (show t))
          | _ -> ()))
      parts;
    TStr
  | EVar n -> check_var ctx f e n e.espan
  | EList es ->
    let el = fresh () in
    List.iter (fun x -> expect ctx f x el) es;
    TList el
  | EBin (op, a, b) -> check_bin ctx f op a b e.espan
  | EUn (Neg, a) -> (
    let t = check_expr ctx f a in
    match repr t with
    | TInt | TFloat | TErr -> t
    | TVar _ -> unify_at ctx a.espan TInt t; TInt
    | _ ->
      err ctx "E0201" a.espan (sprintf "cannot negate a value of type `%s`" (show t));
      TErr)
  | EUn (Not, a) -> expect ctx f a TBool; TBool
  | EUn (BitNot, a) -> (
    let t = check_expr ctx f a in
    match repr t with
    | TInt | TErr -> t
    | TVar _ -> unify_at ctx a.espan TInt t; TInt
    | _ ->
      err ctx "E0201" a.espan (sprintf "operator `~` cannot be applied to `%s`" (show t));
      TErr)
  | ECall (callee, args) -> check_call ctx f e callee args e.espan
  | EMethod (({ edesc = EVar en; _ } as recv), name, nsp, args)
    when not (is_local f en) && Hashtbl.mem ctx.enums en ->
    (* `Enum.Variant(args)` is a constructor call, not a method call. *)
    let callee = { edesc = EField (recv, name, nsp); espan = Span.merge recv.espan nsp; ety = TErr; einst = []; ecallee = None } in
    check_call ctx f e callee args e.espan
  | EMethod (recv, name, nsp, args) -> check_method ctx f e recv name nsp args
  | EField (recv, name, nsp) -> check_field ctx f e recv name nsp
  | EIndex (a, i) -> (
    let t = check_expr ctx f a in
    match repr t with
    | TMap (k, v) -> expect ctx f i k; v
    | TBytes -> expect ctx f i TInt; TInt
    | _ ->
    expect ctx f i TInt;
    match repr t with
    | TList el | TArray (el, _) -> el
    | TErr -> TErr
    | _ ->
      err ctx "E0201" a.espan (sprintf "only lists, arrays and maps can be indexed, found `%s`" (show t));
      TErr)
  | ETry (x, c) -> check_try ctx f x c e.espan
  | EIf (c, b, els) -> (
    expect ctx f c TBool;
    let tt = check_block ctx f b in
    match els with
    | None -> (
      try unify TUnit tt; TUnit
      with Mismatch ->
        err ctx "E0203" b.bspan
          (sprintf "this `if` has no `else`, so its block must have type `()`, found `%s`" (show tt))
          ~notes:[ "add an `else` branch, or end the block with a statement" ];
        TErr)
    | Some x -> expect ctx f x tt; tt)
  | EMatch (s, arms) -> check_match ctx f s arms e.espan
  | ESelect arms -> check_select ctx f arms e.espan
  | EBlock b -> check_block ctx f b
  | EClosure (ps, body) -> check_closure ctx f ps body None
  | EStruct (name, nsp, fields) -> check_struct_lit ctx f name nsp fields e.espan
  | EDyn (x, i) -> (
    (* checked again (a table test's copy): the type it was boxed as *)
    let before = e.ety in
    ignore (check_expr ctx f x);
    match repr before with TDyn _ as t -> t | _ -> TDyn (i, []))
  | EPipe _ -> failwith "pipes are expanded when modules load"
  | EWith (base, fields) -> check_with ctx f base fields e.espan
  | ESafe (base, x, rest, _) -> check_safe ctx f e base x rest
  | ENamed (n, nsp, x) ->
    (* a label the call did not take away: the callee has no parameter names *)
    err ctx "E0122" nsp (sprintf "`%s:` names a parameter, but this call takes its arguments by position" n)
      ~fix:{ Diag.fmsg = "remove the label"; edits = [ { Diag.at = { nsp with hi = x.espan.lo }; replacement = "" } ] };
    check_expr ctx f x
  | EGroup (name, _, until, b) ->
    let t = fresh () in
    Option.iter
      (fun (clk, ms) ->
        (match repr (check_expr ctx f clk) with
         | TCap Clock | TErr -> ()
         | TVar _ as ct -> unify_at ctx clk.espan (TCap Clock) ct
         | ct ->
           err ctx "E0201" clk.espan
             ~notes:[ "a group deadline is `group g until clock.after(ms) { ... }`, with the `Clock` from `root.clock`" ]
             (sprintf "`after` in a group deadline needs a `Clock`, found `%s`" (show ct)));
        expect ctx f ms TInt)
      until;
    push f;
    bind f name { vty = TGroup t; vmut = false; vspan = e.espan; vkind = KGroup };
    let saved_loops = f.loops in
    f.loops <- 0;
    f.in_group <- f.in_group + 1;
    ignore (check_block ctx f b);
    f.in_group <- f.in_group - 1;
    f.loops <- saved_loops;
    pop f;
    (* with a deadline, a task cancelled by it has no result: None *)
    if until = None then TList t else TList (opt t)

(* Check [e] against an expected type; closures get their parameter types from it. *)
and expect ctx f e want =
  match (e.edesc, repr want) with
  | EList es, TArray (el, n) ->
    List.iter (fun x -> expect ctx f x el) es;
    (match repr n with
     | TLen n ->
       if List.length es <> n then
         err ctx "E0201" e.espan
           (sprintf "expected %d element%s for `%s`, found %d" n (plural n) (show want) (List.length es))
     | _ ->
       (* a length still to infer, as at a call of `fn sum[N](xs: Array[Int, N])` *)
       unify_at ctx e.espan n (TLen (List.length es)));
    e.ety <- want
  | EList es, TList el when (match repr el with TDyn _ -> true | _ -> false) ->
    (* each element is boxed on its own, so a list can mix types *)
    List.iter (fun x -> expect ctx f x el) es;
    e.ety <- want
  (* the expected type reaches each branch, so `if` and `match` arms can
     become `dyn` one by one, and closures in them get their types *)
  | EIf (c, b, Some els), _ ->
    expect ctx f c TBool;
    expect_block ctx f b want;
    expect ctx f els want;
    e.ety <- want
  | EMatch (s, arms), _ -> e.ety <- check_match ~want ctx f s arms e.espan
  (* `Err(x)` or `Some(x)` where a `dyn` payload is expected: the expected
     type reaches the arguments, so `x` becomes `dyn` *)
  | ECall (callee, args), TNamed (_, targs) when List.exists has_dyn targs ->
    let t = check_call ~want ctx f e callee args e.espan in
    e.ety <- t;
    unify_at ctx e.espan want t
  | EBlock b, _ -> expect_block ctx f b want; e.ety <- want
  | _, TDyn (i, a) -> coerce ctx e (check_expr ctx f e) (i, a)
  | _ -> expect_plain ctx f e want

(* A value used where `dyn I` is expected: box it if its type satisfies I. *)
and coerce ctx e t ((i, _) as b) =
  let bn = show_bound b in
  match repr t with
  | TDyn (j, _) when j = i -> unify_at ctx e.espan (dyn_of b) t
  | TVar _ -> unify_at ctx e.espan (dyn_of b) t
  | TErr -> ()
  | TStr | TNamed ("IoError", _) when i = "Error" ->
    (* the built-in errors: a message, no cause *)
    let inner = { e with edesc = e.edesc } in
    e.edesc <- EDyn (inner, i);
    e.ety <- dyn_of b
  | TNamed (n, _) as ct when (Hashtbl.mem ctx.structs n || Hashtbl.mem ctx.enums n) && not (List.mem n [ "Option"; "Result"; "IoError" ]) ->
    (match held ct with
    | Some h ->
      err ctx "E0112" e.espan
        (sprintf "%s, so it cannot become `dyn %s`" h bn)
        ~notes:[ "authority must stay visible in types; pass it with a type parameter, as in `[T: " ^ bn ^ "]`" ]
    | None ->
    if has_fn ct || has_chan ct then
      err ctx "E0112" e.espan
        (sprintf "`%s` holds a function or channel, so it cannot become `dyn %s`" (show ct) bn)
        ~notes:[ "a function or channel could carry authority out of sight; pass it with a type parameter, as in `[T: " ^ bn ^ "]`" ]
    else begin
      match satisfies ctx ct b with
      | Some why -> err ctx "E0111" e.espan (sprintf "`%s` does not satisfy interface `%s`" (show ct) bn) ~notes:[ why ]
      | None ->
        let inner = { e with edesc = e.edesc } in
        e.edesc <- EDyn (inner, i);
        e.ety <- dyn_of b
    end)
  | t ->
    err ctx "E0112" e.espan
      (sprintf "only struct and enum values can become `dyn %s`, not `%s`" bn (show t))
      ~notes:[ "wrap the value in a struct that has the interface's methods" ]

and expect_plain ctx f e want =
  match e.edesc with
  | EClosure (ps, body) ->
    let t = check_closure ctx f ps body (Some want) in
    e.ety <- t;
    unify_at ctx e.espan want t
  | _ ->
    let t = check_expr ctx f e in
    unify_at ctx e.espan want t

and check_var ctx f e n sp =
  match find_var f n with
  | Some v when v.vkind = KGroup ->
    err ctx "E0310" sp (sprintf "`%s` is a group handle; the only thing you can do with it is `%s.go(...)`" n n);
    TErr
  | Some v -> v.vty
  | None -> (
    let direct = !calling in
    calling := false;
    match Hashtbl.find_opt ctx.fns n with
    | Some _ when Hashtbl.mem foreign_fns n && not direct ->
      err ctx "E0331" sp (sprintf "foreign function `%s` can only be called, not used as a value" n)
        ~notes:[ sprintf "wrap it in a closure, as in `|x| %s(foreign, x)`" n ];
      TErr
    | Some s ->
      let env, t = instantiate s in
      e.einst <- env;
      add_bounds n env sp;
      t
    | None -> (
      match Option.map (fun _ -> variant_scope ctx n sp.file) (Hashtbl.find_opt ctx.owners n) with
      | Some [ en ] ->
        (* name the enum when the variant name alone would be ambiguous to the backends *)
        if List.length (Hashtbl.find ctx.owners n) > 1 then
          e.edesc <- EField ({ edesc = EVar en; espan = sp; ety = TErr; einst = []; ecallee = None }, n, sp);
        ctor_ty ctx en n sp
      | Some ens ->
        err ctx "E0104" sp
          (sprintf "`%s` is a variant of several enums (%s); qualify it, for example `%s.%s`" n
             (String.concat ", " ens) (List.hd ens) n);
        TErr
      | None -> (
        match List.find_opt (fun (a, _, _) -> a = n) ambient with
        | Some (_, cap, how) ->
          err ctx "E0302" sp
            (sprintf "`%s` does not exist: Tessera has no ambient I/O" n)
            ~notes:
              [ sprintf "I/O goes through capabilities: take a `%s` parameter and call `%s`" cap how;
                "in `main`, the capabilities live on `root`, for example `root.stdout.print(\"hi\")`" ];
          TErr
        | None ->
          let owners = Hashtbl.fold (fun k _ acc -> if String.ends_with ~suffix:("::" ^ n) k then k :: acc else acc) ctx.fns [] in
          let notes =
            match owners with
            | [] -> []
            | k :: _ ->
              let tn = String.sub k 0 (String.length k - String.length n - 2) in
              [ sprintf "`%s` is a method of `%s`; call it on a value: `x.%s(...)`" n tn n ]
          in
          did_you_mean ctx ~notes "E0101" sp (sprintf "unknown name `%s`" n) n (visible_names ctx f);
          TErr)))

and check_args ctx f args = List.iter (fun a -> ignore (check_expr ctx f (unlabel a))) args

(* Named arguments, `connect(host, retries: 3)`: positional arguments first,
   then named ones in any order, every parameter passed exactly once (there
   are no default values). A closure written last, after named arguments,
   fills the last parameter: that is where a trailing closure goes. Returns
   the arguments in parameter order without their labels, or None after
   reporting what is wrong. [names] are the callee's parameter names, or why
   it has none. *)
and order_args ctx ~what ~sp names args =
  if not (List.exists is_named args) then Some args
  else
    match names with
    | Error why ->
      (match (List.find is_named args).edesc with
       | ENamed (l, lsp, v) ->
         err ctx "E0122" lsp (sprintf "`%s:` names a parameter, but %s" l why)
           ~fix:{ Diag.fmsg = "remove the label"; edits = [ { Diag.at = { lsp with hi = v.espan.lo }; replacement = "" } ] }
       | _ -> ());
      None
    | Ok names ->
      let n = List.length names in
      let slots = Array.make n None in
      let ok = ref true in
      let total = List.length args in
      let named_seen = ref false and misplaced = ref [] and extra = ref 0 and unknown = ref [] and dup = ref false in
      List.iteri
        (fun i (a : expr) ->
          match a.edesc with
          | ENamed (l, lsp, v) -> (
            named_seen := true;
            match List.find_index (( = ) l) names with
            | None -> ok := false; unknown := (l, lsp) :: !unknown
            | Some j -> (
              match slots.(j) with
              | Some _ ->
                ok := false;
                dup := true;
                err ctx "E0122" lsp (sprintf "parameter `%s` is passed twice" l)
                  ~notes:[ "positional arguments fill the parameters in order, before the named ones" ]
              | None -> slots.(j) <- Some v))
          | EClosure _ when !named_seen && i = total - 1 && n > 0 && slots.(n - 1) = None -> slots.(n - 1) <- Some a
          | _ when !named_seen -> misplaced := a :: !misplaced
          | _ -> if i < n then slots.(i) <- Some a else incr extra)
        args;
      if !extra > 0 then begin
        ok := false;
        err ctx "E0107" sp
          (sprintf "%s takes %d argument%s but %d %s supplied" what n (plural n) total (if total = 1 then "was" else "were"))
      end;
      let missing = ref (List.filteri (fun j _ -> slots.(j) = None) names) in
      let notes = [ sprintf "its parameters are: %s" (if names = [] then "none" else String.concat ", " names) ] in
      (match (!unknown, !missing) with
       | [ (l, lsp) ], [ m ] ->
         (* one wrong label and one parameter left: that must be the one *)
         missing := [];
         err ctx ~notes "E0122" lsp (sprintf "%s has no parameter `%s`" what l)
           ~fix:{ Diag.fmsg = sprintf "did you mean `%s`?" m; edits = [ { Diag.at = lsp; replacement = m } ] }
       | us, _ ->
         if us <> [] then missing := [];
         List.iter (fun (l, lsp) -> did_you_mean ctx ~notes "E0122" lsp (sprintf "%s has no parameter `%s`" what l) l names) (List.rev us));
      List.iter
        (fun (a : expr) ->
          ok := false;
          let fix =
            match !missing with
            | m :: rest ->
              missing := rest;
              Some { Diag.fmsg = sprintf "name it: `%s: ...`" m; edits = [ { Diag.at = Span.at_start a.espan; replacement = m ^ ": " } ] }
            | [] -> None
          in
          err ctx ?fix "E0122" a.espan "a positional argument cannot come after a named one"
            ~notes:[ "write the positional arguments first, or name this one too" ])
        (List.rev !misplaced);
      if !misplaced = [] && !extra = 0 && not !dup then
        List.iter
          (fun m ->
            ok := false;
            err ctx "E0107" sp (sprintf "%s is missing the argument for parameter `%s`" what m)
              ~notes:[ "every parameter must be passed; Tessera has no default values" ])
          !missing;
      if !ok then Some (List.map Option.get (Array.to_list slots)) else None

(* Arguments run in the order they are written. When labels moved one that
   has an effect past another, the checked call [e] becomes
   `{ let $arg1 = first; let $arg2 = second; call($arg2, $arg1) }`, the lets
   in written order. Literals, names, closures and field reads have no
   effect, so they stay in place. [written] is the arguments as written,
   after the receiver if there is one. *)
and keep_order e written r =
  let rec pure (x : expr) =
    match x.edesc with
    | EInt _ | EFloat _ | EBool _ | EUnit | EVar _ | EClosure _ -> true
    | EStr ps -> List.for_all (function SPLit _ -> true | SPExpr _ -> false) ps
    | EField (a, _, _) | EDyn (a, _) -> pure a
    | _ -> false
  in
  let effects = List.filter (fun x -> not (pure x)) written in
  let final = match e.edesc with ECall (_, xs) -> xs | EMethod (rv, _, _, xs) -> rv :: xs | _ -> [] in
  let in_final = List.filter (fun x -> List.memq x effects) final in
  if List.length in_final = List.length effects && not (List.for_all2 ( == ) in_final effects) then begin
    let temps =
      List.map (fun (x : expr) -> incr arg_counter; (x, sprintf "$arg%d" !arg_counter)) effects
    in
    let var (x : expr) =
      match List.assq_opt x temps with
      | Some n -> { edesc = EVar n; espan = x.espan; ety = x.ety; einst = []; ecallee = None }
      | None -> x
    in
    let desc =
      match e.edesc with
      | ECall (c, xs) -> ECall (c, List.map var xs)
      | EMethod (rv, m, nsp, xs) -> EMethod (var rv, m, nsp, List.map var xs)
      | d -> d
    in
    let call = { edesc = desc; espan = e.espan; ety = r; einst = e.einst; ecallee = None } in
    e.edesc <-
      EBlock
        { stmts = List.map (fun ((x : expr), n) -> { sdesc = SLet (false, n, x.espan, None, x); sspan = x.espan }) temps;
          tail = Some call; bspan = e.espan }
  end

(* Parameter names of the method [key] (`Type::name`), without `self`. *)
and method_params key =
  match Hashtbl.find_opt param_names key with
  | Some (_ :: ps) -> Ok ps
  | _ -> Error "this method takes its arguments by position"

and check_call ?want ctx f e callee args sp =
  match callee.edesc with
  | EVar "assert" when not (is_local f "assert") && not (Hashtbl.mem ctx.fns "assert") ->
    (match args with
     | [ a ] -> expect ctx f a TBool
     | _ -> err ctx "E0107" sp "`assert` takes 1 argument, a Bool"; check_args ctx f args);
    TUnit
  | EVar "assert_eq" when not (is_local f "assert_eq") && not (Hashtbl.mem ctx.fns "assert_eq") ->
    (match args with
     | [ a; b ] -> (
       let t = check_expr ctx f a in
       expect ctx f b t;
       match repr t with
       | TFn _ | TCap _ | TChan _ | TDyn _ -> err ctx "E0201" sp (sprintf "values of type `%s` cannot be compared" (show t))
       | _ -> ())
     | _ -> err ctx "E0107" sp "`assert_eq` takes 2 arguments"; check_args ctx f args);
    TUnit
  | EVar "keep" when not (is_local f "keep") && not (Hashtbl.mem ctx.fns "keep") ->
    (* `keep(x)`: x counts as used, so a benchmark's work is not optimized away *)
    (match args with
     | [ a ] -> ignore (check_expr ctx f a)
     | _ -> err ctx "E0107" sp "`keep` takes 1 argument"; check_args ctx f args);
    TUnit
  | EVar "channel" when not (is_local f "channel") && not (Hashtbl.mem ctx.fns "channel") ->
    (match args with
     | [ a ] -> expect ctx f a TInt
     | _ -> err ctx "E0107" sp "`channel` takes 1 argument, its capacity"; check_args ctx f args);
    TChan (fresh ())
  | EVar "catch" when not (is_local f "catch") && not (Hashtbl.mem ctx.fns "catch") -> (
    (* catch(|| ...): Ok(value), or Err(message) if anything inside panicked *)
    let t = fresh () in
    match args with
    | [ a ] -> expect ctx f a (TFn ([], t)); TNamed ("Result", [ t; TStr ])
    | _ ->
      err ctx "E0107" sp "`catch` takes 1 argument, a closure with no parameters, as in `catch(|| risky())`";
      check_args ctx f args;
      TErr)
  | EVar "bytes" when not (is_local f "bytes") && not (Hashtbl.mem ctx.fns "bytes") ->
    (* bytes([104, 105]): each item 0 to 255, or it panics *)
    (match args with
     | [ a ] -> expect ctx f a (TList TInt)
     | _ -> err ctx "E0107" sp "`bytes` takes 1 argument, a list of Ints from 0 to 255"; check_args ctx f args);
    TBytes
  | EVar "map" when not (is_local f "map") && not (Hashtbl.mem ctx.fns "map") ->
    let k = fresh () and v = fresh () in
    (match args with
     | [] -> ()
     | [ a ] -> expect ctx f a (TList (TNamed (tuple_name 2, [ k; v ])))
     | _ -> err ctx "E0107" sp "`map` takes no arguments, or a list of (key, value) pairs"; check_args ctx f args);
    map_keys := (sp, k) :: !map_keys;
    TMap (k, v)
  | EVar "panic" when not (is_local f "panic") && not (Hashtbl.mem ctx.fns "panic") ->
    (match args with
     | [ a ] -> expect ctx f a TStr
     | _ -> err ctx "E0107" sp "`panic` takes 1 argument (a message)"; check_args ctx f args);
    fresh ()
  | _ -> (
    let ct =
      match callee.edesc with
      | EVar _ -> calling := true; Fun.protect ~finally:(fun () -> calling := false) (fun () -> check_expr ctx f callee)
      | _ -> check_expr ctx f callee
    in
    let what, names =
      match callee.edesc with
      | EVar n when not (is_local f n) && Hashtbl.mem param_names n -> (sprintf "function `%s`" n, Ok (Hashtbl.find param_names n))
      | EVar n when not (is_local f n) && Hashtbl.mem foreign_fns n -> ("", Error (sprintf "foreign function `%s` takes its arguments by position" n))
      | EVar n when is_local f n -> ("", Error (sprintf "`%s` is a function value, and function values have no parameter names" n))
      | EVar n when Hashtbl.mem ctx.owners n -> ("", Error "an enum variant takes its payload by position")
      | EField _ -> ("", Error "an enum variant takes its payload by position")
      | _ -> ("", Error "a function value has no parameter names")
    in
    match repr ct with
    | TFn (ps, r) -> (
      (* a constructor's type parameters come from the expected type first *)
      (match (want, callee.edesc) with
       | Some w, (EVar _ | EField _) when (match repr r with TNamed (n, _) -> Hashtbl.mem ctx.enums n | _ -> false) ->
         (try unify r w with Mismatch -> ())
       | _ -> ());
      match order_args ctx ~what ~sp names args with
      | None -> check_args ctx f args; r
      | Some args' ->
      let labelled = List.exists is_named args in
      let args, written = (args', List.map unlabel args) in
      if List.length ps <> List.length args then begin
        err ctx "E0107" sp
          (sprintf "this function takes %d argument%s but %d %s supplied" (List.length ps)
             (plural (List.length ps)) (List.length args)
             (if List.length args = 1 then "was" else "were"));
        check_args ctx f args
      end
      else begin
        match callee.edesc with
        | EVar n when not (is_local f n) && Hashtbl.mem foreign_fns n ->
          (* a callback may take the call's Foreign first, before what C passes *)
          let ps =
            match ps with
            | TCap Foreign :: _ ->
              List.map2
                (fun (a : expr) p ->
                  match (repr p, a.edesc) with
                  | TFn (cps, r), EVar m when not (is_local f m) -> (
                    match Hashtbl.find_opt ctx.fns m with
                    | Some s when (match s.ps with TCap Foreign :: rest -> List.length rest = List.length cps | _ -> false) ->
                      TFn (TCap Foreign :: cps, r)
                    | _ -> p)
                  | _ -> p)
                args ps
            | _ -> ps
          in
          List.iter2 (fun a p -> expect ctx f a p) args ps;
          check_callbacks ctx f args ps
        | _ -> List.iter2 (fun a p -> expect ctx f a p) args ps
      end;
      if labelled then begin
        e.edesc <- ECall (callee, args);
        keep_order e written r
      end;
      r)
    | TErr -> check_args ctx f args; TErr
    | TVar _ ->
      let ps = List.map (fun _ -> fresh ()) args and r = fresh () in
      unify_at ctx callee.espan (TFn (ps, r)) ct;
      List.iter2 (fun a p -> expect ctx f a p) args ps;
      r
    | t ->
      err ctx "E0108" callee.espan (sprintf "a value of type `%s` is not a function" (show t));
      check_args ctx f args;
      TErr)

(* A callback passed to C names a top-level function: C calls it with its
   arguments alone, so it has nowhere to keep a closure's captured values. *)
and check_callbacks ctx f args ps =
  List.iter2
    (fun (a : expr) p ->
      match repr p with
      | TFn _ -> (
        match a.edesc with
        | EVar m when not (is_local f m) && (match Hashtbl.find_opt ctx.fns m with Some s -> s.gen = [] | None -> false) -> ()
        | _ ->
          err ctx "E0332" a.espan "a callback to C is a top-level function, passed by its name"
            ~notes:
              [ "C calls it with its arguments alone, so it has nowhere to keep a closure's captured values; pass what it needs through C (most C functions that take a callback also take a pointer for it)" ])
      | _ -> ())
    args ps

and check_field ctx f e recv name nsp =
  match recv.edesc with
  | EVar en when not (is_local f en) && Hashtbl.mem ctx.enums en ->
    let vs = Hashtbl.find ctx.variants en in
    if List.mem_assoc name vs then ctor_ty ctx en name nsp
    else begin
      did_you_mean ctx "E0104" nsp (sprintf "enum `%s` has no variant `%s`" en name) name (List.map fst vs);
      TErr
    end
  | _ -> field_of ctx f e recv (check_expr ctx f recv) name nsp

(* `recv.name` where [recv] is checked and has type [t]. *)
and field_of ctx f e recv t name nsp =
    let arity = method_arity ~file:nsp.file ctx t name in
    match repr t with
    | TNamed (n, _) as rt when Hashtbl.mem ctx.sfields n && (match promote ctx ~fields:true n name with Embed.Path _ | Embed.Ambiguous _ -> true | _ -> false) -> (
      (* promoted from an embedded struct: `admin.name` is `admin.User.name` *)
      match promote ctx ~fields:true n name with
      | Embed.Path (path, _) ->
        let r, rt' = embed_path ctx recv rt path in
        e.edesc <- EField (r, name, nsp);
        field_of ctx f e r rt' name nsp
      | Embed.Ambiguous ps -> ambiguous ctx nsp n name ps; TErr
      | _ -> TErr)
    | TNamed (n, args) when Hashtbl.mem ctx.sfields n && not (List.mem_assoc name (struct_fields ctx n args)) && arity <> None ->
      method_value ctx f e recv t name nsp (Option.get arity)
    | TNamed (n, args) when Hashtbl.mem ctx.sfields n -> (
      let fs = struct_fields ctx n args in
      match List.assoc_opt name fs with
      | Some ft -> ft
      | None ->
        let notes = if Hashtbl.mem ctx.fns (n ^ "::" ^ name) then [ sprintf "`%s` is a method; call it with parentheses: `.%s()`" name name ] else [] in
        did_you_mean ctx ~notes "E0103" nsp (sprintf "struct `%s` has no field `%s`" n name) name (List.map fst fs);
        TErr)
    | TCap Root when (not (List.mem_assoc name root_fields)) && arity <> None -> method_value ctx f e recv t name nsp (Option.get arity)
    | TCap Root -> (
      match List.assoc_opt name root_fields with
      | Some ft -> ft
      | None ->
        did_you_mean ctx "E0103" nsp (sprintf "`Root` has no field `%s`" name) name (List.map fst root_fields);
        TErr)
    | TErr -> TErr
    | TVar _ ->
      err ctx "E0207" recv.espan "the type of this value is not known here; add a type annotation";
      TErr
    | _ when arity <> None -> method_value ctx f e recv t name nsp (Option.get arity)
    | t ->
      let notes =
        if List.mem_assoc name (methods t) then [ sprintf "`%s` changes its receiver in place, so it cannot be a value; call it as `%s(...)`" name name ] else []
      in
      err ctx ~notes "E0103" nsp (sprintf "`%s` has no field `%s`" (show t) name);
      TErr

(* How many arguments (besides the receiver) method [name] of a value of
   type [t] takes, if it has one that can be used as a value. *)
and method_arity ?(file = "") ctx t name =
  match builtin_method ctx t name file with
  | Some key -> Some (List.length (Hashtbl.find ctx.fns key).ps - 1)
  | None ->
  match repr t with
  | TNamed (n, _) when Hashtbl.mem ctx.fns (n ^ "::" ^ name) && builtin_tname t = None -> Some (List.length (Hashtbl.find ctx.fns (n ^ "::" ^ name)).ps - 1)
  | TDyn (i, a) -> Option.map (fun (ps, _) -> List.length ps) (List.assoc_opt name (iface_methods (i, a)))
  | TParam g -> List.find_map (fun b -> Option.map (fun (ps, _) -> List.length ps) (List.assoc_opt name (iface_methods b))) (bounds_of g)
  | (TList _ | TMap _) when name = "push" || name = "remove" -> None (* they change the receiver *)
  | t -> Option.map (fun mk -> List.length (fst (mk ()))) (List.assoc_opt name (methods t))

(* `x.name` without a call, where `name` is a method: a closure that calls it
   on the value `x` had when the expression ran, as if written
   `{ let $recv = x; |a, b| $recv.name(a, b) }`. The rewritten tree is what
   both backends see; [recv] is already checked. *)
and method_value ctx f e recv rt name nsp arity =
  let sp = e.espan in
  let mk d = { edesc = d; espan = sp; ety = TErr; einst = []; ecallee = None } in
  let params = List.init arity (fun i -> ("$a" ^ string_of_int i, None, sp)) in
  let call = mk (EMethod (mk (EVar "$recv"), name, nsp, List.map (fun (n, _, _) -> mk (EVar n)) params)) in
  let clo = mk (EClosure (params, call)) in
  e.edesc <- EBlock { stmts = [ { sdesc = SLet (false, "$recv", recv.espan, None, recv); sspan = sp } ]; tail = Some clo; bspan = sp };
  push f;
  bind f "$recv" { vty = rt; vmut = false; vspan = recv.espan; vkind = KLet };
  let t = check_expr ctx f clo in
  pop f;
  t

(* The method [name] that file [file] added to built-in type [t], if any. *)
and builtin_method ctx t name file =
  match builtin_tname t with
  | None -> None
  | Some tn ->
    let suffix = tn ^ "::" ^ name in
    let own =
      List.find_opt
        (fun k -> match Hashtbl.find_opt method_owner k with Some (_, fl) -> fl = file | None -> false)
        (methods_named ctx suffix)
    in
    (* the built-in methods written in Tessera (std/methods.tsr) *)
    if own = None && Hashtbl.mem ctx.fns suffix
       && (match Hashtbl.find_opt method_owner suffix with Some (_, fl) -> fl = Prelude.methods_file | None -> false)
    then (Hashtbl.replace used_methods suffix (); Some suffix)
    else own

and check_method ctx f e recv name nsp args =
  match recv.edesc with
  | EVar g when (match lookup f g with Some (v, _) -> v.vkind = KGroup | None -> false) -> (
    let v, crossed = Option.get (lookup f g) in
    recv.ety <- v.vty;
    let t = match repr v.vty with TGroup t -> t | _ -> TErr in
    if crossed then
      err ctx "E0310" recv.espan (sprintf "cannot spawn into `%s` from inside a closure" g)
        ~notes:[ "call `go` directly in the group's body" ];
    match (name, args) with
    | ("go" | "#go"), [ a ] ->
      (* a task that returns () runs for its effect and adds no item to the
         group's list (the backends see it as `#go`); the others share one type *)
      let r = fresh () in
      expect ctx f a (TFn ([], r));
      (match repr r with
       | TUnit -> e.edesc <- EMethod (recv, "#go", nsp, args)
       | _ -> unify_at ctx a.espan t r);
      TUnit
    | ("go" | "#go"), _ -> err ctx "E0107" nsp "`go` takes 1 argument, a closure with no parameters"; check_args ctx f args; TUnit
    | _ ->
      err ctx "E0105" nsp (sprintf "a group handle only has `go`, not `%s`" name);
      check_args ctx f args;
      TErr)
  | _ -> check_method_value ctx f e recv name nsp args

(* [rt]: the receiver's type, when [recv] is already checked *)
and check_method_value ?rt ctx f e recv name nsp args =
  let rt = repr (match rt with Some t -> t | None -> check_expr ctx f recv) in
  let name =
    match rt with
    | TNamed ("Result", _) when name = "map_err" ->
      (* the old name still works, with a warning and a fix that renames it *)
      ctx.diags <-
        Diag.make ~severity:Diag.Warning "W0001" nsp "`map_err` is now called `map_error`"
          ~fix:{ Diag.fmsg = "rename it to `map_error`"; edits = [ { Diag.at = nsp; replacement = "map_error" } ] }
        :: ctx.diags;
      (match e.edesc with EMethod (r, _, sp, a) -> e.edesc <- EMethod (r, "map_error", sp, a) | _ -> ());
      "map_error"
    | _ -> name
  in
  match rt with
  | TErr -> check_args ctx f args; TErr
  | TVar _ ->
    err ctx "E0207" recv.espan "the type of this value is not known here; add a type annotation";
    check_args ctx f args;
    TErr
  | TDyn _ | TParam _ -> (
    (* `x.m(...)` where `x: T` and `T` is bounded by interfaces that declare
       `m`, or `x: dyn I` *)
    let g, bs =
      match rt with
      | TDyn (i, a) -> ("dyn " ^ show_bound (i, a), [ (i, a) ])
      | TParam g -> (g, bounds_of g)
      | _ -> ("", [])
    in
    let found = List.find_map (fun ((bi, _) as b) -> Option.map (fun m -> (bi, m)) (List.assoc_opt name (iface_methods b))) bs in
    match found with
    | Some (b, (ps, r)) -> (
      let names = Option.to_result ~none:"" (Hashtbl.find_opt param_names (b ^ "::" ^ name)) in
      match order_args ctx ~what:(sprintf "method `%s`" name) ~sp:nsp names args with
      | None -> check_args ctx f args; r
      | Some args' ->
      let labelled = List.exists is_named args in
      let args, written = (args', recv :: List.map unlabel args) in
      if List.length ps <> List.length args then begin
        err ctx "E0107" nsp
          (sprintf "method `%s` takes %d argument%s besides `self`, but %d %s supplied" name (List.length ps)
             (plural (List.length ps)) (List.length args) (if List.length args = 1 then "was" else "were"));
        check_args ctx f args
      end
      else List.iter2 (fun a p -> expect ctx f a p) args ps;
      if labelled then begin
        e.edesc <- EMethod (recv, name, nsp, args);
        keep_order e written r
      end;
      r)
    | None ->
      let declared = List.concat_map (fun b -> List.map fst (iface_methods b)) bs in
      let notes =
        if bs = [] then [ sprintf "`%s` is a type parameter with no bounds; add one, as in `[%s: Interface]`, whose interface declares `fn %s(self, ...)`" g g name ]
        else [ sprintf "the bounds of `%s` (%s) declare: %s" g (String.concat ", " (List.map show_bound bs)) (if declared = [] then "nothing" else String.concat ", " declared) ]
      in
      did_you_mean ctx ~notes "E0105" nsp (sprintf "no method `%s` on `%s`" name g) name declared;
      check_args ctx f args;
      TErr)
  | _ when builtin_method ctx rt name nsp.file <> None -> (
    (* a method this file added to a built-in type: rewritten to a plain call
       of `Str::name` (or `mod.Str::name`), receiver first *)
    let key = Option.get (builtin_method ctx rt name nsp.file) in
    let env, t = instantiate (Hashtbl.find ctx.fns key) in
    add_bounds key env nsp;
    match t with
    | TFn (p0 :: ps, r) -> (
      unify_at ctx recv.espan p0 rt;
      match order_args ctx ~what:(sprintf "method `%s`" name) ~sp:nsp (method_params key) args with
      | None -> check_args ctx f args; r
      | Some args' ->
      let labelled = List.exists is_named args in
      let args, written = (args', recv :: List.map unlabel args) in
      if List.length ps <> List.length args then begin
        err ctx "E0107" nsp
          (sprintf "method `%s` takes %d argument%s besides `self`, but %d %s supplied" name (List.length ps)
             (plural (List.length ps)) (List.length args) (if List.length args = 1 then "was" else "were"));
        check_args ctx f args
      end
      else List.iter2 (fun a p -> expect ctx f a p) args ps;
      e.edesc <- ECall ({ edesc = EVar key; espan = nsp; ety = t; einst = env; ecallee = None }, recv :: args);
      if labelled then keep_order e written r;
      r)
    | _ -> check_args ctx f args; TErr)
  | TNamed (n, _) when Hashtbl.mem ctx.sfields n && (match promote ctx ~fields:false n name with Embed.Path _ | Embed.Ambiguous _ -> true | _ -> false) -> (
    (* promoted from an embedded struct: `admin.greet()` is `admin.User.greet()` *)
    match promote ctx ~fields:false n name with
    | Embed.Path (path, _) ->
      let r, rt' = embed_path ctx recv rt path in
      e.edesc <- EMethod (r, name, nsp, args);
      check_method_value ~rt:rt' ctx f e r name nsp args
    | Embed.Ambiguous ps -> ambiguous ctx nsp n name ps; check_args ctx f args; TErr
    | _ -> check_args ctx f args; TErr)
  | TNamed (n, _) when Hashtbl.mem ctx.fns (n ^ "::" ^ name) && builtin_tname rt = None -> (
    (* a method: `fn name(self: T, ...)`, called with the receiver first *)
    let env, t = instantiate (Hashtbl.find ctx.fns (n ^ "::" ^ name)) in
    e.einst <- env;
    add_bounds (n ^ "::" ^ name) env nsp;
    (match Hashtbl.find_opt method_owner (n ^ "::" ^ name) with
     | Some (false, file) when file <> nsp.file ->
       err ctx "E0402" nsp (sprintf "method `%s` is private to the module that defines `%s`" name n)
         ~notes:[ sprintf "mark it `pub fn` in %s to use it here" file ]
     | _ -> ());
    match t with
    | TFn (p0 :: ps, r) -> (
      unify_at ctx recv.espan p0 rt;
      match order_args ctx ~what:(sprintf "method `%s`" name) ~sp:nsp (method_params (n ^ "::" ^ name)) args with
      | None -> check_args ctx f args; r
      | Some args' ->
      let labelled = List.exists is_named args in
      let args, written = (args', recv :: List.map unlabel args) in
      if List.length ps <> List.length args then begin
        err ctx "E0107" nsp
          (sprintf "method `%s` takes %d argument%s besides `self`, but %d %s supplied" name (List.length ps)
             (plural (List.length ps)) (List.length args) (if List.length args = 1 then "was" else "were"));
        check_args ctx f args
      end
      else List.iter2 (fun a p -> expect ctx f a p) args ps;
      if labelled then begin
        e.edesc <- EMethod (recv, name, nsp, args);
        keep_order e written r
      end;
      r)
    | _ -> check_args ctx f args; TErr)
  | _ -> (
    let ms = methods rt in
    match List.assoc_opt name ms with
    | None when name = "unwrap_or" && (match rt with TNamed (("Option" | "Result"), _) -> true | _ -> false) ->
      (* `x.unwrap_or(d)` is written `x ?? d` *)
      let fix =
        match args with
        | [ d ] ->
          let src = Option.value ~default:"" (Source.text recv.espan.file) in
          let text (sp : Span.t) = if sp.hi <= String.length src then String.sub src sp.lo (sp.hi - sp.lo) else "" in
          (* parenthesize where `??` would bind differently from the method call *)
          let simple (x : expr) = match x.edesc with EBin _ | EIf _ | EMatch _ | EClosure _ -> false | _ -> true in
          let dflt = if simple d then text d.espan else "(" ^ text d.espan ^ ")" in
          let recv_t = if simple recv then text recv.espan else "(" ^ text recv.espan ^ ")" in
          let next = if e.espan.hi < String.length src then src.[e.espan.hi] else ' ' in
          let prev = if recv.espan.lo > 0 then src.[recv.espan.lo - 1] else ' ' in
          let core = recv_t ^ " ?? " ^ dflt in
          let core = if next = '.' || next = '[' || next = '?' || prev = '-' || prev = '!' then "(" ^ core ^ ")" else core in
          Some { Diag.fmsg = "write it with `??`"; edits = [ { Diag.at = Span.merge recv.espan e.espan; replacement = core } ] }
        | _ -> None
      in
      err ctx ?fix "E0105" nsp "there is no `unwrap_or`; write `value ?? default`"
        ~notes:[ "`??` gives the value inside a `Some` or `Ok`, or the default otherwise, evaluating the default only then" ];
      check_args ctx f args;
      TErr
    | None when name = "and_then" && (match rt with TNamed (("Option" | "Result"), _) -> true | _ -> false) ->
      (* the same operation is called `flat_map` here, as on lists *)
      let fix = { Diag.fmsg = "rename it to `flat_map`"; edits = [ { Diag.at = nsp; replacement = "flat_map" } ] } in
      err ctx ~fix "E0105" nsp "there is no `and_then`; it is called `flat_map`, as on lists"
        ~notes:[ "`x.flat_map(f)` passes the value inside a `Some` or `Ok` to `f`, which returns an Option or Result itself" ];
      check_args ctx f args;
      TErr
    | None ->
      let notes =
        match rt with
        | TNamed (n, _) when Hashtbl.mem ctx.sfields n && List.mem_assoc name (Hashtbl.find ctx.sfields n) ->
          [ sprintf "`%s` is a field; access it without parentheses" name ]
        | TCap Root when List.mem_assoc name root_fields ->
          [ sprintf "`%s` is a field of `Root`; write `root.%s`" name name ]
        | TCap Clock when name = "after" ->
          [ "`clock.after(ms)` is a timeout: use it as a `select` arm, `clock.after(ms) => ...`, or as a group deadline, `group g until clock.after(ms) { ... }`";
            "to wait, call `clock.sleep(ms)`" ]
        | TNamed (n, _) when ms = [] -> (
          match user_methods ctx n with
          | [] -> [ sprintf "`%s` has no methods; define one as `fn %s(self: %s) { ... }`" n name n ]
          | us -> [ sprintf "`%s` has: %s" n (String.concat ", " us) ])
        | _ -> []
      in
      let notes =
        if ms <> [] && suggest name (List.map fst ms) = None then
          notes @ [ sprintf "`%s` has: %s" (show rt) (String.concat ", " (List.sort_uniq compare (List.map fst ms @ match builtin_tname rt with Some tn -> Prelude.method_names tn | None -> []))) ]
        else notes
      in
      did_you_mean ctx ~notes "E0105" nsp
        (sprintf "no method `%s` on type `%s`" name (show rt))
        name (List.map fst ms @ (match rt with TNamed (n, _) -> user_methods ctx n | _ -> [])
              @ match builtin_tname rt with Some tn -> Prelude.method_names tn | None -> []);
      check_args ctx f args;
      TErr
    | Some mk ->
      let ps, r = mk () in
      (match (name, rt) with
       | "join", TList a -> unify_at ctx recv.espan TStr a
       | "sum", TList a -> unify_at ctx recv.espan TInt a
       | "push", TList _ | "remove", TMap _ -> check_place ctx f recv
       | _ -> ());
      if List.length ps <> List.length args then begin
        err ctx "E0107" nsp
          (sprintf "`%s` takes %d argument%s but %d %s supplied" name (List.length ps)
             (plural (List.length ps)) (List.length args)
             (if List.length args = 1 then "was" else "were"));
        check_args ctx f args
      end
      else List.iter2 (fun a p -> expect ctx f a p) args ps;
      (match (name, rt) with
       | "sort", TList a -> (
         match unordered ctx a with
         | None -> ()
         | Some t ->
           err ctx "E0201" nsp (sprintf "`List[%s]` cannot be sorted: `%s` has no order" (show a) (show t))
             ~notes:[ "numbers, strings, Bool, and lists, tuples, structs and enums of them are ordered; to sort by a key use `xs.sort_by(key)`, for another order `list.sort_by(xs, less)`" ])
       | _ -> ());
      r)

and check_place ctx f e =
  match e.edesc with
  | EVar n -> (
    match lookup f n with
    | Some (_, true) ->
      err ctx "E0205" e.espan (sprintf "cannot modify `%s` inside a closure" n)
        ~notes:[ "closures capture variables by value; return the new value from the closure instead" ]
    | Some (v, false) when not v.vmut -> (
      match v.vkind with
      | KLet ->
        err ctx "E0205" e.espan
          (sprintf "cannot modify `%s`, it is not declared `mut`" n)
          ~fix:
            { Diag.fmsg = sprintf "declare it as `let mut %s`" n;
              edits = [ { Diag.at = Span.at_start v.vspan; replacement = "mut " } ] }
      | KParam ->
        err ctx "E0205" e.espan (sprintf "cannot modify parameter `%s`" n)
          ~notes:[ sprintf "parameters are immutable; make a copy with `let mut %s = %s`" n n ]
      | KPat -> err ctx "E0205" e.espan (sprintf "cannot modify `%s`, pattern and loop bindings are immutable" n)
      | KGroup -> err ctx "E0310" e.espan (sprintf "`%s` is a group handle; the only thing you can do with it is `%s.go(...)`" n n))
    | Some _ -> ()
    | None -> err ctx "E0209" e.espan "this cannot be assigned to")
  | EField (r, _, _) | EIndex (r, _) -> check_place ctx f r
  | _ -> err ctx "E0209" e.espan "only variables, fields and list elements can be assigned to"

(* Can errors of type [e] become `dyn Error`? None if so, else why not. *)
and error_conversion ctx e =
  match repr e with
  | TDyn ("Error", []) | TErr | TStr | TNamed ("IoError", _) -> None
  | TVar _ -> unify e (TDyn ("Error", [])); None
  | TNamed (n, _) as ct when (Hashtbl.mem ctx.structs n || Hashtbl.mem ctx.enums n) && not (List.mem n [ "Option"; "Result" ]) ->
    (match held ct with
    | Some h -> Some h
    | None ->
    if has_fn ct || has_chan ct then Some (sprintf "`%s` holds a function or channel" (show ct))
    else satisfies ctx ct ("Error", []))
  | t -> Some (sprintf "`%s` does not satisfy `Error`" (show t))

(* The fix-it that changes the function's error type to `dyn Error`. *)
and dyn_error_fix f =
  match f.ret_te with
  | Some { tdesc = TyName ("Result", [ _; et ]); _ } ->
    Some { Diag.fmsg = "return `dyn Error`, which any error converts to"; edits = [ { Diag.at = et.tspan; replacement = "dyn Error" } ] }
  | _ -> None

and check_try ctx f x context sp =
  if f.in_defer > 0 then begin
    err ctx "E0115" sp "`?` cannot leave a `defer` body"
      ~notes:[ "handle the error inside it, for example with `??` or a `match`" ];
    ignore (check_expr ctx f x);
    TErr
  end
  else check_try_ok ctx f x context sp

and check_try_ok ctx f x context sp =
  if f.in_group > 0 then
    err ctx "E0311" sp "`?` cannot leave a `group` block early; every task must finish first"
      ~notes:[ "handle the error inside the task, or after the group returns its results" ];
  let t = repr (check_expr ctx f x) in
  let is_dyn_error t = match repr t with TDyn ("Error", []) -> true | _ -> false in
  let wrong_fn rt kind =
    err ctx "E0202" sp
      (sprintf "`?` can only be used in a function that returns `%s`, this one returns `%s`" kind (show rt))
      ~notes:[ "for main, write `fn main(root: Root) -> Result[(), IoError]`" ]
  in
  (* `x? "context"` wraps the error, so the function must return `dyn Error` *)
  let with_context () =
    match context with
    | None -> false
    | Some c ->
      expect ctx f c TStr;
      (match repr f.ret_ty with
       | TNamed ("Result", [ _; fe ]) when is_dyn_error fe -> ()
       | TNamed ("Result", [ _; fe ]) ->
         err ctx "E0202" sp
           (sprintf "`?` with context returns a `dyn Error`, but this function returns errors of type `%s`" (show fe))
           ?fix:(dyn_error_fix f)
       | TVar _ -> unify_at ctx sp f.ret_ty (res (fresh ()) (TDyn ("Error", [])))
       | TErr -> ()
       | rt -> wrong_fn rt "Result[_, dyn Error]");
      true
  in
  match t with
  | TErr -> Option.iter (fun c -> ignore (check_expr ctx f c)) context; TErr
  | TNamed ("Result", [ a; e ]) ->
    let ctxd = with_context () in
    (match repr f.ret_ty with
     | TNamed ("Result", [ _; fe ]) when is_dyn_error fe || ctxd -> (
       if is_dyn_error fe then
         match error_conversion ctx e with
         | None -> ()
         | Some why ->
           err ctx "E0202" sp (sprintf "`?` cannot turn an error of type `%s` into `dyn Error`" (show e))
             ~notes:
               [ why;
                 (if held e <> None || has_fn e || has_chan e then "an error cannot carry authority: keep capabilities, functions and channels out of the error type"
                  else "give it a method `fn message(self: " ^ show e ^ ") -> Str`") ])
     | TNamed ("Result", [ _; fe ]) -> (
       try unify fe e
       with Mismatch ->
         let fix = if error_conversion ctx e = None && error_conversion ctx fe = None then dyn_error_fix f else None in
         err ctx "E0202" sp
           (sprintf "`?` would return an error of type `%s`, but this function returns errors of type `%s`"
              (show e) (show fe))
           ?fix
           ~notes:[ "convert the error first with `.map_error(|e| ...)`, or return `dyn Error`, which any error type with a `message` method converts to" ])
     | TVar _ -> if not ctxd then unify_at ctx sp f.ret_ty (res (fresh ()) e)
     | TErr -> ()
     | rt -> if not ctxd then wrong_fn rt "Result");
    a
  | TNamed ("Option", [ a ]) ->
    (* with context, a None becomes an error whose message is the context *)
    if not (with_context ()) then
      (match repr f.ret_ty with
       | TNamed ("Option", _) | TErr -> ()
       | TVar _ -> unify_at ctx sp f.ret_ty (opt (fresh ()))
       | rt -> wrong_fn rt "Option");
    a
  | TVar _ ->
    err ctx "E0207" x.espan "the type of this value is not known here; add a type annotation";
    TErr
  | t ->
    err ctx "E0202" x.espan (sprintf "`?` needs a `Result` or `Option`, found `%s`" (show t));
    TErr

and check_bin ctx f op a b sp =
  let ta = check_expr ctx f a in
  match op with
  | And | Or ->
    unify_at ctx a.espan TBool ta;
    expect ctx f b TBool;
    TBool
  | Range ->
    (* `lo..<hi`: the Ints from lo up to but not including hi *)
    unify_at ctx a.espan TInt ta;
    expect ctx f b TInt;
    TList TInt
  | Else -> (
    (* `opt ?? default`: the value inside, or the default when there is none *)
    match repr ta with
    | TNamed ("Option", [ t ]) | TNamed ("Result", [ t; _ ]) -> expect ctx f b t; t
    | TErr -> ignore (check_expr ctx f b); TErr
    | TVar _ ->
      err ctx "E0207" a.espan "the type of this value is not known here; add a type annotation";
      ignore (check_expr ctx f b);
      TErr
    | t ->
      err ctx "E0201" sp (sprintf "`??` needs an `Option` or `Result` on its left, found `%s`" (show t));
      ignore (check_expr ctx f b);
      TErr)
  | _ -> (
    expect ctx f b ta;
    let t = repr ta in
    let name =
      match op with
      | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%" | Eq -> "==" | Ne -> "!="
      | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">=" | And -> "&&" | Or -> "||" | Else -> "??" | Range -> "..<"
      | BitAnd -> "&" | BitOr -> "|" | BitXor -> "^" | Shl -> "<<" | Shr -> ">>"
    in
    let bad () =
      err ctx "E0201" sp (sprintf "operator `%s` cannot be applied to `%s`" name (show t))
    in
    match op with
    | Add -> (match t with TInt | TFloat | TStr | TBytes | TList _ | TErr | TVar _ -> t | _ -> bad (); TErr)
    | Sub | Mul | Div -> (match t with TInt | TFloat | TErr | TVar _ -> t | _ -> bad (); TErr)
    | Mod | BitAnd | BitOr | BitXor | Shl | Shr -> (match t with TInt | TErr | TVar _ -> t | _ -> bad (); TErr)
    | Lt | Le | Gt | Ge -> (match unordered ctx t with None -> () | Some _ -> bad ()); TBool
    | _ ->
      (match t with
       | TFn _ | TCap _ | TChan _ | TDyn _ -> bad ()
       | _ -> (
         (* capabilities have no equality, so neither has a value holding one *)
         match held t with
         | Some h ->
           err ctx "E0201" sp (sprintf "operator `%s` cannot be applied to `%s`" name (show t))
             ~notes:[ h ^ ", and capabilities cannot be compared" ]
         | None -> ()));
      TBool)

and check_closure ctx f ps body want =
  let want_ps, want_ret =
    match want with
    | Some w -> (
      match repr w with
      | TFn (wp, wr) when List.length wp = List.length ps -> (Some wp, Some wr)
      | _ -> (None, None))
    | None -> (None, None)
  in
  let scope = { vars = Hashtbl.create 8; barrier = true } in
  let pts =
    List.mapi
      (fun i (n, ann, psp) ->
        let t =
          match (ann, want_ps) with
          | Some te, _ -> resolve ctx f.genv te
          | None, Some wp -> List.nth wp i
          | None, None -> fresh ()
        in
        Hashtbl.replace scope.vars n { vty = t; vmut = false; vspan = psp; vkind = KParam };
        t)
      ps
  in
  let ret = match want_ret with Some r -> r | None -> fresh () in
  let cf = { ret_ty = ret; scopes = scope :: f.scopes; loops = 0; genv = f.genv; in_group = 0; in_defer = 0; ret_te = None } in
  let bt = check_expr ctx cf body in
  unify_at ctx body.espan ret bt;
  TFn (pts, ret)

and check_struct_lit ctx f name nsp fields sp =
  match Hashtbl.find_opt ctx.sfields name with
  | None ->
    if Hashtbl.mem ctx.enums name then
      err ctx "E0102" nsp (sprintf "`%s` is an enum; build it with a variant, for example `%s.Variant(...)`" name name)
    else
      did_you_mean ctx "E0102" nsp (sprintf "unknown struct `%s`" name) name
        (List.of_seq (Hashtbl.to_seq_keys ctx.sfields));
    List.iter (fun (_, _, e) -> ignore (check_expr ctx f e)) fields;
    TErr
  | Some decl_fields ->
    let gen = (Hashtbl.find ctx.structs name).st_gen in
    let targs = List.map (fun _ -> fresh ()) gen in
    add_type_bounds name targs nsp;
    let env = List.combine gen targs in
    let seen = Hashtbl.create 8 in
    List.iter
      (fun (fname, fsp, e) ->
        if Hashtbl.mem seen fname then err ctx "E0106" fsp (sprintf "field `%s` is set twice" fname);
        Hashtbl.replace seen fname ();
        match List.assoc_opt fname decl_fields with
        | Some ft -> expect ctx f e (subst env ft)
        | None ->
          let notes =
            match promote ctx ~fields:true name fname with
            | Embed.Path (p, _) ->
              let top = List.hd p in
              [ sprintf "`%s` belongs to the embedded `%s`; set it there: `%s: %s { %s: ... }`" fname (String.concat "." p) top top fname ]
            | _ -> []
          in
          did_you_mean ctx ~notes "E0103" fsp
            (sprintf "struct `%s` has no field `%s`" name fname)
            fname (List.map fst decl_fields);
          ignore (check_expr ctx f e))
      fields;
    let missing = List.filter (fun (fn, _) -> not (Hashtbl.mem seen fn)) decl_fields in
    if missing <> [] then
      err ctx "E0206" sp
        (sprintf "missing field%s %s in `%s`" (plural (List.length missing))
           (String.concat ", " (List.map (fun (n, _) -> "`" ^ n ^ "`") missing))
           name);
    TNamed (name, targs)

(* `a?.b`: [rest] runs on the value inside the Option [a], bound to [x].
   The result is an Option: the rest's own when it is one already (so
   `a?.b?.c` is not nested), otherwise the rest wrapped in Some. *)
and check_safe ctx f e base x rest =
  let bt = check_expr ctx f base in
  let inner =
    match repr bt with
    | TNamed ("Option", [ t ]) -> t
    | TErr -> TErr
    | TVar _ ->
      err ctx "E0207" base.espan "the type of this value is not known here; add a type annotation";
      TErr
    | TNamed ("Result", _) as t ->
      err ctx "E0201" base.espan
        (sprintf "`?.` works on an `Option`, but this is `%s`" (show t))
        ~notes:
          [ "`?.` is safe navigation; to return early on `Err` and then use the value, write `(e?).name`" ];
      TErr
    | t ->
      err ctx "E0201" base.espan
        (sprintf "`?.` needs an `Option` on its left, found `%s`" (show t))
        ~notes:[ "a value that is always there takes a plain `.`" ];
      TErr
  in
  push f;
  bind f x { vty = inner; vmut = false; vspan = base.espan; vkind = KPat };
  let rt = check_expr ctx f rest in
  pop f;
  match (repr bt, repr rt) with
  | TNamed ("Option", _), TNamed ("Option", _) ->
    e.edesc <- ESafe (base, x, rest, true);
    rt
  | TNamed ("Option", _), TErr -> TErr
  | TNamed ("Option", _), _ -> opt rt
  | _ -> TErr

(* `{ base with x: 1, y }`: a copy of a struct value with some fields
   replaced; the same field errors as a struct literal, minus "missing". *)
and check_with ctx f base fields sp =
  let bt = check_expr ctx f base in
  let others () = List.iter (fun (_, _, x) -> ignore (check_expr ctx f x)) fields in
  match repr bt with
  | TNamed (name, targs) when Hashtbl.mem ctx.sfields name && not (is_tuple_name name) ->
    let decl_fields = Hashtbl.find ctx.sfields name in
    let env = List.combine (Hashtbl.find ctx.structs name).st_gen targs in
    let seen = Hashtbl.create 8 in
    List.iter
      (fun (fname, fsp, x) ->
        if Hashtbl.mem seen fname then err ctx "E0106" fsp (sprintf "field `%s` is set twice" fname);
        Hashtbl.replace seen fname ();
        match List.assoc_opt fname decl_fields with
        | Some ft -> expect ctx f x (subst env ft)
        | None ->
          did_you_mean ctx "E0103" fsp (sprintf "struct `%s` has no field `%s`" name fname) fname (List.map fst decl_fields);
          ignore (check_expr ctx f x))
      fields;
    bt
  | TErr -> others (); TErr
  | TVar _ ->
    err ctx "E0207" base.espan "the type of this value is not known here; add a type annotation";
    others ();
    TErr
  | t ->
    err ctx "E0201" base.espan
      (sprintf "`{ ... with ... }` copies a struct, but this is `%s`" (show t))
      ~notes:[ "only struct values have named fields to replace" ];
    others ();
    TErr

and check_match ?want ctx f s arms sp =
  let st = check_expr ctx f s in
  let errors_before = List.length (Diag.errors ctx.diags) in
  let result = match want with Some w -> w | None -> fresh () in
  List.iter
    (fun arm ->
      push f;
      check_pat ctx f st arm.apat;
      Option.iter (fun g -> expect ctx f g TBool) arm.aguard;
      (match want with
       | Some w -> expect ctx f arm.abody w
       | None ->
         let bt = check_expr ctx f arm.abody in
         unify_at ctx arm.abody.espan result bt);
      pop f)
    arms;
  (* an arm, or an alternative in one, that the unguarded arms before it
     already cover can never run (not looked for after an error in the arms,
     whose patterns may then not fit their type) *)
  if List.length (Diag.errors ctx.diags) = errors_before then
  ignore
    (List.fold_left
       (fun before arm ->
         let alts = alternatives arm.apat in
         if not (useful ~unknown:true ctx before [ arm.apat ] [ st ]) then
           warn_unreachable ctx arm.apat.pspan "this arm can never match: the arms before it cover every value it matches"
         else if List.length alts > 1 then
           ignore
             (List.fold_left
                (fun seen alt ->
                  if not (useful ~unknown:true ctx seen [ alt ] [ st ]) then
                    warn_unreachable ctx alt.pspan "this alternative can never match: the arms and alternatives before it cover every value it matches";
                  [ alt ] :: seen)
                before alts);
         if arm.aguard = None then [ arm.apat ] :: before else before)
       [] arms);
  (match arms with
   | [] -> err ctx "E0204" sp "`match` needs at least one arm"
   | _ -> (
     (* a guarded arm may not apply, so it covers nothing *)
     match missing_arms ctx st (List.filter_map (fun a -> if a.aguard = None then Some a.apat else None) arms) with
     | [] -> ()
     | missing ->
       (* Insert new arms on their own lines just above the closing brace. *)
       let close = sp.hi - 1 in
       let src = Option.value ~default:"" (Source.text sp.file) in
       let line_start = try String.rindex_from src (close - 1) '\n' + 1 with Not_found | Invalid_argument _ -> 0 in
       let indent = if close <= String.length src then String.sub src line_start (close - line_start) else "" in
       let own_line = String.trim indent = "" in
       let at_ = if own_line then line_start else close in
       let arm_indent = (if own_line then indent else "") ^ "    " in
       let at = { sp with Span.lo = at_; hi = at_ } in
       let text =
         (if own_line then "" else "\n")
         ^ String.concat "" (List.map (fun m -> sprintf "%s%s => panic(\"unhandled\"),\n" arm_indent m) missing)
       in
       err ctx "E0204" sp
         (sprintf "`match` does not cover %s" (String.concat ", " (List.map (sprintf "`%s`") missing)))
         ~fix:{ Diag.fmsg = "add the missing arms"; edits = [ { Diag.at = at; replacement = text } ] }));
  result

and warn_unreachable ctx span msg =
  ctx.diags <- Diag.make ~severity:Diag.Warning "W0002" span msg :: ctx.diags

and check_pat ctx f t p =
  match p.pdesc with
  | PWild -> ()
  | PBind n -> bind f n { vty = t; vmut = false; vspan = p.pspan; vkind = KPat }
  | PInt _ -> unify_at ctx p.pspan t TInt
  | PStr _ -> unify_at ctx p.pspan t TStr
  | PBool _ -> unify_at ctx p.pspan t TBool
  | PTuple ps when List.exists is_rest ps -> (
    (* `(a, ..)`: the tuple's size comes from its type *)
    match repr t with
    | TNamed (n, ts) when is_tuple_name n ->
      if List.length ps - 1 > List.length ts then begin
        err ctx "E0107" p.pspan
          (sprintf "the tuple has %d items, the pattern has at least %d" (List.length ts) (List.length ps - 1));
        List.iter (check_pat ctx f TErr) ps
      end
      else List.iter2 (check_pat ctx f) ts (fill_rest ps (List.length ts))
    | TErr -> List.iter (check_pat ctx f TErr) ps
    | TVar _ ->
      err ctx "E0207" p.pspan "the type of this value is not known here, so `..` cannot stand for its other items; add a type annotation";
      List.iter (check_pat ctx f TErr) ps
    | t ->
      err ctx "E0201" p.pspan (sprintf "this pattern is a tuple, but the value is `%s`" (show t));
      List.iter (check_pat ctx f TErr) ps)
  | PTuple ps ->
    let ts = List.map (fun _ -> fresh ()) ps in
    unify_at ctx p.pspan (TNamed (tuple_name (List.length ps), ts)) t;
    List.iter2 (check_pat ctx f) ts ps
  | PList ps ->
    let el = fresh () in
    unify_at ctx p.pspan (TList el) t;
    List.iter
      (fun q ->
        match q.pdesc with
        | PRest (Some n) -> bind f n { vty = TList el; vmut = false; vspan = q.pspan; vkind = KPat }
        | _ -> check_pat ctx f el q)
      ps
  | PRest _ -> () (* only inside a tuple, variant or list pattern, which handle it *)
  | PAs (q, n, nsp) ->
    check_pat ctx f t q;
    bind f n { vty = t; vmut = false; vspan = nsp; vkind = KPat }
  | PStruct (name, nsp, fields, rest) -> (
    let skip () = List.iter (fun (_, _, q) -> check_pat ctx f TErr q) fields in
    match Hashtbl.find_opt ctx.sfields name with
    | None ->
      if Hashtbl.mem ctx.enums name then
        err ctx "E0102" nsp (sprintf "`%s` is an enum; match it with its variants, for example `%s.Variant(...)`" name name)
      else did_you_mean ctx "E0102" nsp (sprintf "unknown struct `%s`" name) name (List.of_seq (Hashtbl.to_seq_keys ctx.sfields));
      skip ()
    | Some decl_fields ->
      let gen = (Hashtbl.find ctx.structs name).st_gen in
      let targs = List.map (fun _ -> fresh ()) gen in
      unify_at ctx p.pspan t (TNamed (name, targs));
      let env = List.combine gen targs in
      let seen = Hashtbl.create 8 in
      List.iter
        (fun (fname, fsp, q) ->
          if Hashtbl.mem seen fname then err ctx "E0106" fsp (sprintf "field `%s` is matched twice" fname);
          Hashtbl.replace seen fname ();
          match List.assoc_opt fname decl_fields with
          | Some ft -> check_pat ctx f (subst env ft) q
          | None ->
            did_you_mean ctx "E0103" fsp (sprintf "struct `%s` has no field `%s`" name fname) fname (List.map fst decl_fields);
            check_pat ctx f TErr q)
        fields;
      let missing = List.filter (fun (fn, _) -> not (Hashtbl.mem seen fn)) decl_fields in
      if missing <> [] && not rest then
        err ctx "E0206" p.pspan
          (sprintf "missing field%s %s in the pattern for `%s`" (plural (List.length missing))
             (String.concat ", " (List.map (fun (n, _) -> "`" ^ n ^ "`") missing))
             name)
          ~notes:[ "name every field, or end the pattern with `..` to match any value of the others" ])
  | POr alts ->
    (* each alternative in a scope of its own, to compare what they bind *)
    let binds_of alt =
      push f;
      check_pat ctx f t alt;
      let s = List.hd f.scopes in
      let vs = if s.vars == no_vars then [] else List.of_seq (Hashtbl.to_seq s.vars) in
      pop f;
      List.sort (fun (a, _) (b, _) -> compare a b) vs
    in
    let first = binds_of (List.hd alts) in
    List.iter
      (fun alt ->
        let vs = binds_of alt in
        let names l = List.map fst l in
        let only_in a b = List.filter (fun n -> not (List.mem n (names b))) (names a) in
        match (only_in first vs, only_in vs first) with
        | n :: _, _ ->
          err ctx "E0210" alt.pspan
            (sprintf "`%s` is bound in the first alternative but not in this one" n)
            ~notes:[ "every alternative of `A | B` must bind the same names, with the same types" ]
        | [], n :: _ ->
          err ctx "E0210" alt.pspan
            (sprintf "`%s` is bound in this alternative but not in the first one" n)
            ~notes:[ "every alternative of `A | B` must bind the same names, with the same types" ]
        | [], [] ->
          List.iter
            (fun (n, (v : var_info)) ->
              let v0 = List.assoc n first in
              match unify v0.vty v.vty with
              | () -> ()
              | exception Mismatch ->
                err ctx "E0210" v.vspan
                  (sprintf "`%s` is `%s` in the first alternative and `%s` in this one" n (show v0.vty) (show v.vty))
                  ~notes:[ "every alternative of `A | B` must bind the same names, with the same types" ])
            vs)
      (List.tl alts);
    List.iter (fun (n, v) -> bind f n v) first
  | PVariant (q, v, args) -> (
    let rt = repr t in
    let skip () = List.iter (fun a -> check_pat ctx f TErr a) args in
    let en =
      match q with
      | Some e ->
        if Hashtbl.mem ctx.enums e then Some e
        else begin
          did_you_mean ctx "E0102" p.pspan (sprintf "unknown enum `%s`" e) e
            (List.of_seq (Hashtbl.to_seq_keys ctx.enums));
          None
        end
      | None -> (
        match rt with
        | TNamed (n, _) when Hashtbl.mem ctx.variants n && List.mem_assoc v (Hashtbl.find ctx.variants n) -> Some n
        | _ -> (
          match Option.map (fun _ -> variant_scope ctx v p.pspan.file) (Hashtbl.find_opt ctx.owners v) with
          | Some [ e ] -> Some e
          | Some (e :: _ as es) ->
            err ctx "E0104" p.pspan
              (sprintf "`%s` is a variant of several enums (%s); qualify it, for example `%s.%s`" v
                 (String.concat ", " es) e v);
            None
          | _ ->
            let cands =
              match rt with
              | TNamed (n, _) when Hashtbl.mem ctx.variants n -> List.map fst (Hashtbl.find ctx.variants n)
              | _ -> List.of_seq (Hashtbl.to_seq_keys ctx.owners)
            in
            did_you_mean ctx "E0104" p.pspan (sprintf "unknown variant `%s`" v) v cands;
            None))
    in
    match en with
    | None -> skip ()
    | Some en -> (
      match List.assoc_opt v (Hashtbl.find ctx.variants en) with
      | None ->
        did_you_mean ctx "E0104" p.pspan
          (sprintf "enum `%s` has no variant `%s`" en v)
          v (List.map fst (Hashtbl.find ctx.variants en));
        skip ()
      | Some payload ->
        let gen = (Hashtbl.find ctx.enums en).en_gen in
        let targs = List.map (fun _ -> fresh ()) gen in
        unify_at ctx p.pspan t (TNamed (en, targs));
        let ptys = List.map (subst (List.combine gen targs)) payload in
        let rest = List.exists is_rest args in
        let given = List.length args - if rest then 1 else 0 in
        if (if rest then given > List.length ptys else given <> List.length ptys) then begin
          err ctx "E0107" p.pspan
            (sprintf "variant `%s` has %d field%s, the pattern has %s%d" v (List.length ptys)
               (plural (List.length ptys)) (if rest then "at least " else "") given);
          skip ()
        end
        else List.iter2 (fun a pt -> check_pat ctx f pt a) (fill_rest args (List.length ptys)) ptys))

(* Patterns still unmatched, as source text. Single-field payloads are
   checked recursively; multi-field payloads only need one arm each. *)
and missing_arms ctx t pats =
  let rec flat p = match p.pdesc with PAs (q, _, _) -> flat q | POr alts -> List.concat_map flat alts | _ -> [ p ] in
  let pats = List.concat_map flat pats in
  if List.exists irrefutable pats then []
  else
    match repr t with
    | TNamed (n, _) when is_tuple_name n || Hashtbl.mem ctx.sfields n ->
      if exhaustive ctx (List.map (fun p -> [ p ]) pats) [ t ] then [] else [ "_" ]
    | TBool ->
      List.filter_map
        (fun b -> if List.exists (fun p -> p.pdesc = PBool b) pats then None else Some (string_of_bool b))
        [ true; false ]
    | TList el ->
      (* the lengths not covered: `[]`, `[_]`, ..., and `[_, .., ]` for the longer ones *)
      let rows = List.map (fun p -> [ p ]) pats in
      let m = list_longest rows in
      List.filter_map
        (fun l ->
          if exhaustive ctx (list_rows l rows) (List.init l (fun _ -> el)) then None
          else
            let items = List.init l (fun _ -> "_") @ if l > m then [ ".." ] else [] in
            Some ("[" ^ String.concat ", " items ^ "]"))
        (List.init (m + 2) Fun.id)
    | TNamed (n, args) when Hashtbl.mem ctx.variants n ->
      let gen = (Hashtbl.find ctx.enums n).en_gen in
      let env = List.combine gen args in
      List.concat_map
        (fun (v, payload) ->
          let rows =
            List.filter_map
              (fun p ->
                match p.pdesc with
                | PVariant (_, v', a) when v' = v ->
                  let a = fill_rest a (List.length payload) in
                  (* the wrong number of items, reported already: as if `_` *)
                  Some (if List.length a = List.length payload then a else List.map (fun _ -> { p with pdesc = PWild }) payload)
                | _ -> None)
              pats
          in
          match (payload, rows) with
          | _, [] ->
            [ (match payload with [] -> v | ps -> v ^ "(" ^ String.concat ", " (List.map (fun _ -> "_") ps) ^ ")") ]
          | [], _ -> []
          | [ pt ], _ ->
            let inner = List.filter_map (function [ p ] -> Some p | _ -> None) rows in
            List.map (fun m -> v ^ "(" ^ m ^ ")") (missing_arms ctx (subst env pt) inner)
          | _ -> if exhaustive ctx rows (List.map (subst env) payload) then [] else [ v ^ "(" ^ String.concat ", " (List.map (fun _ -> "_") payload) ^ ")" ])
        (Hashtbl.find ctx.variants n)
    | TErr | TVar _ -> []
    | _ -> [ "_" ]

(* The longest list a list pattern at the head of these rows spells out,
   not counting a `..`: lists longer than that all match the same rows. *)
and list_longest rows =
  List.fold_left
    (fun m row ->
      match row with
      | { pdesc = PList ps; _ } :: _ -> max m (List.length (List.filter (fun p -> not (is_rest p)) ps))
      | _ -> m)
    0 (expand_heads rows)

(* The rows that match a list of length [l] at their head, with that list's
   [l] items in its place. *)
and list_rows l rows =
  List.filter_map
    (fun row ->
      match row with
      | [] -> None
      | p :: r -> ( match list_items l p with Some items -> Some (items @ r) | None -> None))
    (expand_heads rows)

(* What pattern [p] says of the items of a list of length [l]: one pattern
   each, or None when it does not match that length. *)
and list_items l (p : pat) =
  let wild = { pdesc = PWild; pspan = p.pspan } in
  match p.pdesc with
  | PWild | PBind _ -> Some (List.init l (fun _ -> wild))
  | PList ps -> (
    match list_parts ps with
    | before, None, _ -> if List.length before = l then Some before else None
    | before, Some _, after ->
      let k = List.length before + List.length after in
      if k <= l then Some (before @ List.init (l - k) (fun _ -> wild) @ after) else None)
  | _ -> None

(* Each row whose first pattern is an or-pattern becomes one row per
   alternative; a name given with `as` is left out. *)
and expand_heads rows =
  List.concat_map
    (function
      | { pdesc = POr alts; _ } :: r -> expand_heads (List.map (fun a -> a :: r) alts)
      | { pdesc = PAs (q, _, _); _ } :: r -> expand_heads [ q :: r ]
      | row -> [ row ])
    rows

(* Do the rows of patterns cover every value of the column types? *)
and exhaustive ctx (rows : pat list list) (tys : ty list) : bool =
  not (useful ~unknown:false ctx rows (List.map (fun _ -> { pdesc = PWild; pspan = Span.dummy }) tys) tys)

(* Does the row of patterns [q] match some value that none of [rows] does?
   The classic matrix check, splitting on the first column's constructors:
   enum variants, `true` and `false`, a tuple's one shape, a list's lengths;
   Int and Str literals never cover their type. [unknown] is the answer
   where a type is not known (after an error). *)
and useful ~unknown ctx (rows : pat list list) (q : pat list) (tys : ty list) : bool =
  match (q, tys) with
  | [], _ | _, [] -> rows = []
  | qh :: qr, t :: rest_tys -> (
    let rows = expand_heads rows in
    let wild p = match p.pdesc with PWild | PBind _ -> true | _ -> false in
    let wilds n = List.init n (fun _ -> { pdesc = PWild; pspan = Span.dummy }) in
    (* rows (and q) specialised to a constructor with [n] arguments *)
    let special matches n =
      List.filter_map
        (function
          | p :: r when wild p -> Some (wilds n @ r)
          | p :: r -> (
            match matches p with
            | Some ps when List.length ps = n -> Some (ps @ r)
            | Some _ -> Some (wilds n @ r) (* the wrong number of items, reported already *)
            | None -> None)
          | [] -> None)
        rows
    in
    let go matches args =
      let n = List.length args in
      if wild qh then useful ~unknown ctx (special matches n) (wilds n @ qr) (args @ rest_tys)
      else
        match matches qh with
        | Some ps when List.length ps = n -> useful ~unknown ctx (special matches n) (ps @ qr) (args @ rest_tys)
        | _ -> unknown
    in
    (* a `_` against constructors the rows do not all name: the rows that
       start with `_` decide, since some value has a constructor no other row
       matches (this also keeps a recursive type from being unfolded forever) *)
    let default () = useful ~unknown ctx (List.filter_map (function p :: r when wild p -> Some r | _ -> None) rows) qr rest_tys in
    let heads = List.filter_map (function p :: _ when not (wild p) -> Some p | _ -> None) rows in
    match qh.pdesc with
    | POr alts -> List.exists (fun a -> useful ~unknown ctx rows (a :: qr) tys) alts
    | PAs (q, _, _) -> useful ~unknown ctx rows (q :: qr) tys
    | _ -> (
      match repr t with
      | TNamed (n, args) when is_tuple_name n ->
        go (fun p -> match p.pdesc with PTuple ps -> Some (fill_rest ps (List.length args)) | _ -> None) args
      | TNamed (n, targs) when Hashtbl.mem ctx.sfields n ->
        (* a struct has one shape: its fields, in declaration order *)
        let fields = Hashtbl.find ctx.sfields n in
        let env = List.combine (Hashtbl.find ctx.structs n).st_gen targs in
        go
          (fun p -> match p.pdesc with PStruct (_, _, fs, _) -> Some (struct_items fs (List.map fst fields) p.pspan) | _ -> None)
          (List.map (fun (_, ft) -> subst env ft) fields)
      | TBool -> (
        let one b = go (fun p -> if p.pdesc = PBool b then Some [] else None) [] in
        let named b = List.exists (fun p -> p.pdesc = PBool b) heads in
        match qh.pdesc with
        | PBool b -> one b
        | _ when wild qh -> if named true && named false then one true || one false else default ()
        | _ -> unknown)
      | TNamed (n, targs) when Hashtbl.mem ctx.variants n -> (
        let env = List.combine (Hashtbl.find ctx.enums n).en_gen targs in
        let one (v, payload) =
          let ptys = List.map (subst env) payload in
          go (fun p -> match p.pdesc with PVariant (_, v', a) when v' = v -> Some (fill_rest a (List.length ptys)) | _ -> None) ptys
        in
        let vs = Hashtbl.find ctx.variants n in
        let named (v, _) = List.exists (fun p -> match p.pdesc with PVariant (_, v', _) -> v' = v | _ -> false) heads in
        match qh.pdesc with
        | PVariant (_, v, _) -> ( match List.assoc_opt v vs with Some payload -> one (v, payload) | None -> unknown)
        | _ when wild qh -> if List.for_all named vs then List.exists one vs else default ()
        | _ -> unknown)
      | TList el ->
        let m = list_longest ([ qh ] :: rows) in
        let lengths = List.init (m + 2) Fun.id in
        let at l = useful ~unknown ctx (list_rows l rows) in
        if wild qh then
          (* the lengths up to the longest pattern, and one past it for all the longer ones *)
          if List.for_all (fun l -> List.exists (fun p -> list_items l p <> None) heads) lengths then
            List.exists (fun l -> at l (wilds l @ qr) (List.init l (fun _ -> el) @ rest_tys)) lengths
          else default ()
        else if (match qh.pdesc with PList _ -> false | _ -> true) then unknown
        else
          List.exists
            (fun l ->
              match list_items l qh with
              | Some items -> at l (items @ qr) (List.init l (fun _ -> el) @ rest_tys)
              | None -> false)
            lengths
      | TErr | TVar _ -> unknown
      | _ -> (
        (* Int, Str, Float and the like: literals never cover the type *)
        match qh.pdesc with
        | PInt _ | PStr _ | PBool _ ->
          useful ~unknown ctx
            (List.filter_map (function p :: r when wild p || p.pdesc = qh.pdesc -> Some r | _ -> None) rows)
            qr rest_tys
        | _ when wild qh -> default ()
        | _ -> unknown)))

and check_select ctx f arms sp =
  let defaults = List.filter (fun a -> a.sop = SelDefault) arms in
  if List.length defaults > 1 then err ctx "E0106" sp "`select` has more than one `_` arm";
  List.iter
    (fun a -> if a.sop = SelDefault && a.sguard <> None then err ctx "E0003" a.sspan_ "the `_` arm of a `select` cannot have a guard")
    arms;
  if List.length defaults = List.length arms then
    err ctx "E0003" sp "`select` needs at least one channel arm: `v = ch.recv() => ...` or `ch.send(x) => ...`";
  let afters = List.filter (fun a -> match a.sop with SelAfter _ -> true | _ -> false) arms in
  if afters <> [] && defaults <> [] then
    err ctx "E0003" (List.hd afters).sspan_ "a `select` with a `_` arm never waits, so it cannot also have a `clock.after` timeout";
  let rt = fresh () in
  let chan_el ch =
    let ct = check_expr ctx f ch in
    match repr ct with
    | TChan el -> el
    | TErr -> TErr
    | TVar _ -> let el = fresh () in unify_at ctx ch.espan (TChan el) ct; el
    | t -> err ctx "E0201" ch.espan (sprintf "`select` works on channels, found `%s`" (show t)); TErr
  in
  List.iter
    (fun a ->
      Option.iter (fun g -> expect ctx f g TBool) a.sguard;
      push f;
      (match a.sop with
       | SelRecv (b, ch) ->
         let el = chan_el ch in
         Option.iter (fun (n, nsp) -> bind f n { vty = opt el; vmut = false; vspan = nsp; vkind = KPat }) b
       | SelSend (ch, v) -> let el = chan_el ch in expect ctx f v el
       | SelAfter (clk, ms) ->
         (match repr (check_expr ctx f clk) with
          | TCap Clock | TErr -> ()
          | TVar _ as t -> unify_at ctx clk.espan (TCap Clock) t
          | t ->
            err ctx "E0201" clk.espan
              ~notes:[ "a timeout arm is `clock.after(ms) => ...`, with the `Clock` from `root.clock`" ]
              (sprintf "`after` in a `select` needs a `Clock`, found `%s`" (show t)));
         expect ctx f ms TInt
       | SelDefault -> ());
      expect ctx f a.sbody rt;
      pop f)
    arms;
  rt

and check_block ctx f b =
  push f;
  let diverges = List.fold_left (fun d s -> check_stmt ctx f s || d) false b.stmts in
  let t = match b.tail with Some e -> check_expr ctx f e | None -> if diverges then fresh () else TUnit in
  pop f;
  t

(* A block checked against the type its value must have. *)
and expect_block ctx f b want =
  push f;
  let diverges = List.fold_left (fun d s -> check_stmt ctx f s || d) false b.stmts in
  (match b.tail with
   | Some e -> expect ctx f e want
   | None -> if not diverges then unify_at ctx { b.bspan with lo = max b.bspan.lo (b.bspan.hi - 1) } want TUnit);
  pop f

(* Returns true when the statement never falls through. *)
and check_stmt ctx f s : bool =
  match s.sdesc with
  | SLet (_, n, nsp, Some { tdesc = TyName (tn, []); _ }, e) when String.starts_with ~prefix:"$assign" n && is_tuple_name tn ->
    (* the right side of `(a, b) = e`, expanded when modules load; the
       annotation `#tupleN` says how many parts the left side takes *)
    let k = int_of_string (String.sub tn 6 (String.length tn - 6)) in
    let t = check_expr ctx f e in
    let t =
      match repr t with
      | TNamed (tn', ts) when is_tuple_name tn' && List.length ts = k -> t
      | TErr -> TErr
      | TVar _ ->
        let u = TNamed (tn, List.init k (fun _ -> fresh ())) in
        unify_at ctx e.espan u t;
        u
      | t' ->
        err ctx "E0211" e.espan
          (sprintf "the left side assigns %d values, but the right side is `%s`" k (show t'))
          ~notes:
            [ sprintf "the right side of `(%s) = value` must be a tuple of %d items"
                (String.concat ", " (List.init k (fun i -> String.make 1 (Char.chr (97 + (i mod 26))))))
                k ];
        TErr
    in
    bind f n { vty = t; vmut = false; vspan = nsp; vkind = KLet };
    false
  | SLet (m, n, nsp, ann, e) ->
    let t =
      match ann with
      | Some te ->
        let at = resolve ctx f.genv te in
        expect ctx f e at;
        at
      | None -> check_expr ctx f e
    in
    bind f n { vty = t; vmut = m; vspan = nsp; vkind = KLet };
    false
  | SAssign (lhs, rhs) ->
    check_place ctx f lhs;
    let is_group = match lhs.edesc with EVar n -> (match lookup f n with Some (v, _) -> v.vkind = KGroup | None -> false) | _ -> false in
    let lt = if is_group then TErr else check_expr ctx f lhs in
    (match lhs.edesc with
     | EBlock _ -> err ctx "E0209" lhs.espan "a method cannot be assigned to"
     | EIndex (b, _) when repr b.ety = TBytes ->
       err ctx "E0209" lhs.espan "`Bytes` cannot be changed in place"
         ~notes:[ "build a new value: `b.slice(0, i) + bytes([x]) + b.slice(i + 1, b.len())`" ]
     | _ -> ());
    expect ctx f rhs lt;
    false
  | SExpr e -> ignore (check_expr ctx f e); false
  | SReturn eo ->
    if f.in_defer > 0 then err ctx "E0115" s.sspan "`return` cannot leave a `defer` body"
    else if f.in_group > 0 then
      err ctx "E0311" s.sspan "`return` cannot leave a `group` block early; every task must finish first";
    (match eo with
     | Some e -> expect ctx f e f.ret_ty
     | None -> unify_at ctx s.sspan f.ret_ty TUnit);
    true
  | SWhile (c, b) ->
    expect ctx f c TBool;
    f.loops <- f.loops + 1;
    ignore (check_block ctx f b);
    f.loops <- f.loops - 1;
    false
  | SLetPat _ | SForPat _ -> failwith "destructuring is expanded when modules load"
  | SFor (n, nsp, e, b) ->
    let t = check_expr ctx f e in
    let el =
      match repr t with
      | TList a | TArray (a, _) -> a
      | TErr -> TErr
      | TVar _ ->
        let a = fresh () in
        unify_at ctx e.espan (TList a) t;
        a
      | t ->
        err ctx "E0201" e.espan
          (sprintf "`for` can only iterate over a `List`, found `%s`" (show t))
          ~notes:(if t = TStr then [ "use `.lines()` or `.split(...)` to get a `List[Str]`" ] else []);
        TErr
    in
    push f;
    bind f n { vty = el; vmut = false; vspan = nsp; vkind = KPat };
    f.loops <- f.loops + 1;
    ignore (check_block ctx f b);
    f.loops <- f.loops - 1;
    pop f;
    false
  | SBreak | SContinue ->
    if f.loops = 0 then
      if f.in_defer > 0 then err ctx "E0115" s.sspan "`break` and `continue` cannot leave a `defer` body"
      else err ctx "E0208" s.sspan "`break` and `continue` can only be used inside a loop";
    true
  | SDefer b ->
    (* the body runs when the block is left: it sees the names in scope here,
       and nothing in it may leave it; its value is dropped *)
    let saved = f.loops in
    f.loops <- 0;
    f.in_defer <- f.in_defer + 1;
    ignore (check_block ctx f b);
    f.in_defer <- f.in_defer - 1;
    f.loops <- saved;
    false

(* ---------- program ---------- *)

let check_main ctx (d : fn_decl) (s : fsig) =
  let ok_params = match s.ps with [] -> true | [ t ] -> repr t = TCap Root | _ -> false in
  let ok_ret =
    match repr s.ret with
    | TUnit | TInt -> true
    | TNamed ("Result", [ t; _ ]) -> (match repr t with TUnit | TInt -> true | _ -> false)
    | _ -> false
  in
  if s.gen <> [] || not (ok_params && ok_ret) then
    err ctx "E0303" d.fn_span "`main` has the wrong signature"
      ~notes:[ "write `fn main(root: Root)`, `fn main(root: Root) -> Int` (the exit code) or `fn main(root: Root) -> Result[(), E]`" ]

(* Map keys: Int, Str, Bool, and tuples, structs and enums made of them.
   Returns the first type in the way, if any. *)
let unkeyable ctx t : ty option =
  let seen = Hashtbl.create 8 in
  let rec go t =
    match repr t with
    | TInt | TStr | TBytes | TBool | TVar _ | TErr -> None
    | TParam g when bounded g "Key" -> None
    | TNamed (n, args) as nt -> (
      let key = show nt in
      if Hashtbl.mem seen key then None
      else begin
        Hashtbl.replace seen key ();
        match Hashtbl.find_opt ctx.structs n with
        | Some s ->
          let env = List.combine s.st_gen args in
          List.find_map (fun (_, ft) -> go (subst env ft)) (Option.value ~default:[] (Hashtbl.find_opt ctx.sfields n))
        | None -> (
          match Hashtbl.find_opt ctx.enums n with
          | Some e ->
            let env = List.combine e.en_gen args in
            List.find_map (fun (_, ps) -> List.find_map (fun p -> go (subst env p)) ps) (Option.value ~default:[] (Hashtbl.find_opt ctx.variants n))
          | None -> Some nt)
      end)
    | t -> Some t
  in
  go t

(* A bound failure written in several places in one function (its result
   type, and the literal in its body that builds the value) is reported once,
   at the first. [fn_extents]: each function's span, from its name to the end
   of its body, set by check_program. *)
let reported_bounds : (string * int * string * string * string, unit) Hashtbl.t = Hashtbl.create 8
let fn_extents : (string * int * int) list ref = ref []

(* The start of the function [sp] lies in, or [sp]'s own start outside any. *)
let enclosing_fn (sp : Span.t) =
  match List.find_opt (fun (f, lo, hi) -> f = sp.file && lo <= sp.lo && sp.hi <= hi) !fn_extents with
  | Some (_, lo, _) -> lo
  | None -> sp.lo

let check_pending ctx =
  List.iter
    (fun (sp, k) ->
      match unkeyable ctx k with
      | None -> ()
      | Some t ->
        let inner = if show t = show (repr k) then "" else sprintf " (it holds `%s`)" (show t) in
        err ctx "E0113" sp (sprintf "`%s` cannot be a map key%s" (show (repr k)) inner)
          ~notes:("keys are Int, Str, Bool, and tuples, structs and enums made of them; Float, List and Map cannot be keys"
                  :: match t with TParam g -> [ sprintf "a type parameter can be a key with the bound `[%s: Key]`" g ] | _ -> []))
    !map_keys;
  map_keys := [];
  let ps = List.rev !pending in
  pending := [];
  List.iter
    (fun (sp, t, ((i, _) as b), g) ->
      let key = (sp.Span.file, enclosing_fn sp, show (repr t), show_bound b, g) in
      if not (Hashtbl.mem reported_bounds key) then
      let why =
        match i with
        | "Ordered" -> Option.map (fun u -> sprintf "`%s` has no order: numbers, strings, Bool, and lists, tuples, structs and enums of them are ordered" (show u)) (unordered ctx t)
        | "Key" -> Option.map (fun u -> sprintf "`%s` cannot be a map key: keys are Int, Str, Bytes, Bool, and tuples, structs and enums of them" (show u)) (unkeyable ctx t)
        | _ -> satisfies ctx t b
      in
      match why with
      | None -> ()
      | Some why ->
        Hashtbl.replace reported_bounds key ();
        err ctx "E0111" sp
          (if List.mem i builtin_bounds then sprintf "`%s` does not satisfy `%s`, required of `%s`" (show (repr t)) i g
           else sprintf "`%s` does not satisfy interface `%s`, required of `%s`" (show (repr t)) (show_bound b) g)
          ~notes:(why :: match repr t with TParam p when List.mem i builtin_bounds -> [ sprintf "add the bound: `[%s: %s]`" p i ] | _ -> []))
    ps

(* The bounds a function's parameter types give its type parameters: in
   `fn insert[T](self: Sorted[T], x: T)`, T has the bounds of `Sorted`'s
   parameter, since every `Sorted` value was checked where it was made. Not
   the result type: a function that makes one must declare the bounds. *)
let implied_bounds ctx gen ts =
  let acc = ref [] in
  let add g b =
    let prev = Option.value ~default:[] (List.assoc_opt g !acc) in
    if not (List.mem b prev) then acc := (g, prev @ [ b ]) :: List.remove_assoc g !acc
  in
  let rec walk t =
    match repr t with
    | TNamed (n, args) ->
      (match Hashtbl.find_opt type_bounds n with
       | Some (tg, bs) when List.length tg = List.length args ->
         let env = List.combine tg args in
         List.iter
           (fun (p, pbs) -> match repr (List.assoc p env) with TParam g when List.mem g gen -> List.iter (fun b -> add g (subst_bound env b)) pbs | _ -> ())
           bs
       | _ -> ());
      List.iter walk args
    | TList a | TChan a | TGroup a | TArray (a, _) -> walk a
    | TMap (k, v) -> walk k; walk v
    | TFn (ps, r) -> List.iter walk ps; walk r
    | TDyn (_, xs) -> List.iter walk xs
    | _ -> ()
  in
  ignore ctx;
  List.iter walk ts;
  List.rev !acc

let merge_bounds a b =
  List.fold_left
    (fun acc (g, bs) ->
      let prev = Option.value ~default:[] (List.assoc_opt g acc) in
      let extra = List.filter (fun x -> not (List.mem x prev)) bs in
      if List.mem_assoc g acc then List.map (fun (h, p) -> if h = g then (h, p @ extra) else (h, p)) acc else acc @ [ (g, extra) ])
    a b

let check_fn ctx (d : fn_decl) (s : fsig) =
  cur_bounds := Option.value ~default:[] (Hashtbl.find_opt fn_cur_bounds d.fn_name);
  Hashtbl.reset param_kinds;
  List.iter (fun (g, k) -> Hashtbl.replace param_kinds g k) (Option.value ~default:[] (Hashtbl.find_opt fn_kinds d.fn_name));
  pending := [];
  to_infer := [];
  map_keys := [];
  Fun.protect ~finally:(fun () -> check_pending ctx) @@ fun () ->
  let genv = List.map (fun g -> (g, TParam g)) d.fn_gen in
  let scope = { vars = Hashtbl.create 8; barrier = false } in
  List.iter2
    (fun p t -> Hashtbl.replace scope.vars p.prm_name { vty = t; vmut = false; vspan = p.prm_span; vkind = KParam })
    d.fn_params s.ps;
  let f = { ret_ty = s.ret; scopes = [ scope ]; loops = 0; genv; in_group = 0; in_defer = 0; ret_te = d.fn_ret } in
  match repr s.ret with
  | t when has_dyn t -> expect_block ctx f d.fn_body s.ret (* the body's value becomes `dyn`, branch by branch *)
  | _ ->
  let bt = check_block ctx f d.fn_body in
  try unify s.ret bt
  with Mismatch ->
    let b = d.fn_body in
    let sp = match b.tail with Some e -> e.espan | None -> { b.bspan with lo = b.bspan.hi - 1 } in
    let notes =
      match (repr s.ret, repr bt) with
      | TUnit, _ -> [ sprintf "add `-> %s` to the signature, or end the body with a statement" (show bt) ]
      | _, TUnit -> [ "the last expression of a body is its value; add it, or use `return`" ]
      | _ -> []
    in
    err ctx ~notes "E0201" sp
      (sprintf "`%s` returns `%s`, but its body has type `%s`" d.fn_name (show s.ret) (show bt))

(* True when [x] is the declaration registered under [name] (not a duplicate). *)
let registered tbl name x = match Hashtbl.find_opt tbl name with Some y -> y == x | None -> false

(* E0110: `self` is only the first parameter of a method on a struct or enum
   declared in the program. *)
let check_self ctx d ps =
  List.iteri
    (fun i p ->
      if p.prm_name = "self" && i > 0 then
        err ctx "E0110" p.prm_span "`self` can only be the first parameter, which makes the function a method")
    d.fn_params;
  match (d.fn_params, ps) with
  | { prm_name = "self"; prm_span; _ } :: _, t0 :: _ when d.fn_test = None -> (
    let builtin n = List.mem n [ "Option"; "Result"; "IoError"; "ProcessOutput"; "HttpResponse"; "Handle" ] in
    match t0 with
    | TNamed (n, _) when (Hashtbl.mem ctx.structs n || Hashtbl.mem ctx.enums n) && not (builtin n) -> ()
    | TErr -> ()
    | _ when d.fn_span.file = Prelude.methods_file -> ()
    | t when builtin_tname t <> None ->
      (* a method on a built-in type: private to its file, and no clash with a built-in method *)
      let name = match String.rindex_opt d.fn_name ':' with Some i -> String.sub d.fn_name (i + 1) (String.length d.fn_name - i - 1) | None -> d.fn_name in
      if List.mem_assoc name (methods t) || List.mem name (Prelude.method_names (Option.get (builtin_tname t))) then
        err ctx "E0106" d.fn_span (sprintf "`%s` already has a built-in method `%s`" (show t) name)
      else if d.fn_pub then
        err ctx "E0110" d.fn_span (sprintf "a method on built-in type `%s` cannot be `pub`: it is private to the file that defines it" (show t))
          ~notes:[ "export a plain function instead, as in `pub fn shout(s: Str) -> Str`" ]
    | t ->
      err ctx "E0110" prm_span
        (sprintf "methods can only be defined on struct and enum types and on Int, Float, Bool, Str, List, Map, Option, Result and IoError, not on `%s`" (show t))
        ~notes:[ "write a plain function that takes the value instead, and rename the parameter" ])
  | _ -> ()

let case_note = "in the table case "

let check_program (user_items : item list) : Diag.t list * fn_info list =
  let ctx =
    { structs = Hashtbl.create 16; enums = Hashtbl.create 16; sfields = Hashtbl.create 16;
      variants = Hashtbl.create 16; owners = Hashtbl.create 16; fns = Hashtbl.create 16; diags = [] }
  in
  Hashtbl.reset method_owner;
  Hashtbl.reset Types.authority;
  Hashtbl.reset used_methods;
  Hashtbl.reset foreign_fns;
  Hashtbl.reset reported_bounds;
  fn_extents :=
    List.filter_map
      (function IFn d -> Some (d.fn_span.Span.file, d.fn_span.lo, max d.fn_span.hi d.fn_body.bspan.hi) | _ -> None)
      user_items;
  Hashtbl.reset param_names;
  Hashtbl.reset ifaces;
  List.iter (fun b -> Hashtbl.replace ifaces b []) builtin_bounds;
  Hashtbl.reset fn_bounds;
  Hashtbl.reset fn_cur_bounds;
  Hashtbl.reset fn_kinds;
  Hashtbl.reset iface_gen;
  Hashtbl.reset type_bounds;
  pending := [];
  (* bound checks of the types in declarations, made once every method is known *)
  let deferred = ref [] in
  let take_pending bounds = deferred := (bounds, !pending) :: !deferred; pending := [] in
  let items = Prelude.items () @ user_items in
  let type_defined n = Hashtbl.mem ctx.structs n || Hashtbl.mem ctx.enums n || List.mem n builtin_types in
  (* a type of the program's own named like a built-in one: the built-in
     types, the capabilities, and the types of the prelude *)
  let prelude_types =
    List.filter_map
      (function
        | IStruct s when s.st_span.file = Prelude.file -> Some s.st_name
        | IEnum e when e.en_span.file = Prelude.file -> Some e.en_name
        | IIface i when i.if_span.file = Prelude.file -> Some i.if_name
        | _ -> None)
      items
  in
  let builtin_clash (sp : Span.t) n what =
    if sp.file <> Prelude.file && (List.mem n builtin_types || List.mem n prelude_types) then begin
      let kind = if List.exists (fun c -> Types.cap_name c = n) Types.all_caps then "capability" else "type" in
      err ctx "E0106" sp (sprintf "`%s` is a built-in %s; name the %s something else" n kind what);
      true
    end
    else false
  in
  (* 1. register type names *)
  List.iter
    (function
      | IStruct s ->
        if builtin_clash s.st_span s.st_name "struct" then ()
        else if type_defined s.st_name then err ctx "E0106" s.st_span (sprintf "type `%s` is defined more than once" s.st_name)
        else Hashtbl.replace ctx.structs s.st_name s
      | IEnum e ->
        if builtin_clash e.en_span e.en_name "enum" then ()
        else if type_defined e.en_name then err ctx "E0106" e.en_span (sprintf "type `%s` is defined more than once" e.en_name)
        else Hashtbl.replace ctx.enums e.en_name e
      | IIface i ->
        if List.mem i.if_name builtin_bounds then
          err ctx "E0106" i.if_span (sprintf "`%s` is a built-in bound; name the interface something else" i.if_name)
        else if builtin_clash i.if_span i.if_name "interface" then ()
        else if type_defined i.if_name || Hashtbl.mem ifaces i.if_name then
          err ctx "E0106" i.if_span (sprintf "`%s` is defined more than once" i.if_name)
        else begin
          Hashtbl.replace ifaces i.if_name [];
          Hashtbl.replace iface_gen i.if_name i.if_gen
        end
      | IFn _ | IType _ -> ())
    items;
  (* 1b. bounds on the type parameters of structs and enums, before the
     interface methods, whose signatures can name those types *)
  let declare_bounds name gen (bounds : Ast.bounds) =
    let env = List.map (fun g -> (g, TParam g)) gen in
    let bs = List.filter_map (fun (g, tes) -> match List.filter_map (resolve_bound ctx env) tes with [] -> None | bs -> Some (g, bs)) bounds in
    if bs <> [] then Hashtbl.replace type_bounds name (gen, bs);
    take_pending bs
  in
  List.iter
    (function
      | IStruct s when registered ctx.structs s.st_name s && s.st_bounds <> [] -> declare_bounds s.st_name s.st_gen s.st_bounds
      | IEnum e when registered ctx.enums e.en_name e && e.en_bounds <> [] -> declare_bounds e.en_name e.en_gen e.en_bounds
      | _ -> ())
    items;
  (* 1c. interface methods *)
  List.iter
    (function
      | IIface i ->
        let seen = Hashtbl.create 8 in
        let ms =
          List.filter_map
            (fun (m, msp, ps, r) ->
              if Hashtbl.mem seen m then (err ctx "E0106" msp (sprintf "method `%s` is declared twice in `%s`" m i.if_name); None)
              else begin
                Hashtbl.replace seen m ();
                Hashtbl.replace param_names (i.if_name ^ "::" ^ m) (List.map (fun p -> p.prm_name) ps);
                let env = List.map (fun g -> (g, TParam g)) i.if_gen in
                Some (m, (List.map (fun p -> resolve ctx env p.prm_ty) ps, match r with Some te -> resolve ctx env te | None -> TUnit))
              end)
            i.if_methods
        in
        Hashtbl.replace ifaces i.if_name ms;
        (* the interface's own parameters have no bounds to check *)
        pending := List.filter (fun (_, t, _, _) -> match repr t with TParam g -> not (List.mem g i.if_gen) | _ -> true) !pending;
        take_pending []
      | _ -> ())
    items;
  let own_bounds n = match Hashtbl.find_opt type_bounds n with Some (_, bs) -> bs | None -> [] in
  (* 2. resolve field and payload types; an alias's body, which every use
     already has in its place, only to report a type that does not exist *)
  List.iter
    (function
      | IType a ->
        Hashtbl.reset param_kinds;
        (match a.ty_body.tdesc with
         | TyName (i, args) when Hashtbl.mem ifaces i && not (List.mem i builtin_bounds) ->
           (* another name for an interface with its arguments, used as a bound: `type Ints = Container[Int]` *)
           ignore (bound_args ctx (List.map (fun g -> (g, TParam g)) a.ty_gen) i args a.ty_body.tspan)
         | _ -> ignore (resolve ctx (List.map (fun g -> (g, TParam g)) a.ty_gen) a.ty_body));
        (* an alias is checked where it is used, with the arguments it is given there *)
        pending := []
      | IStruct s when not (registered ctx.structs s.st_name s) -> ()
      | IStruct s ->
        Hashtbl.reset param_kinds;
        (if s.st_foreign <> None then match Ffi.struct_fields s with Ok _ -> () | Error es -> List.iter (fun (sp, m) -> err ctx "E0330" sp m) es);
        let env = List.map (fun g -> (g, TParam g)) s.st_gen in
        let seen = Hashtbl.create 8 in
        let fs =
          List.filter_map
            (fun (fname, fsp, te) ->
              if Hashtbl.mem seen fname then begin
                err ctx "E0106" fsp (sprintf "field `%s` is declared twice" fname);
                None
              end
              else begin
                Hashtbl.replace seen fname ();
                let t = resolve ctx env te in
                check_storable ctx te.tspan t;
                (if List.mem fname s.st_embeds then
                   match repr t with
                   | TNamed (n, _) when Hashtbl.mem ctx.structs n && not (is_tuple_name n) -> ()
                   | TErr -> ()
                   | t ->
                     err ctx "E0121" te.tspan (sprintf "only a struct can be embedded, and `%s` is not one" (show t))
                       ~notes:[ sprintf "give it a field name instead, as in `%s: %s`" (String.lowercase_ascii fname) (show t) ]);
                Some (fname, t)
              end)
            s.st_fields
        in
        Hashtbl.replace ctx.sfields s.st_name fs;
        take_pending (own_bounds s.st_name)
      | IEnum e ->
        if registered ctx.enums e.en_name e then begin
          Hashtbl.reset param_kinds;
          let env = List.map (fun g -> (g, TParam g)) e.en_gen in
          let vs =
            List.filter_map
              (fun (v, vsp, tes) ->
                let prev = Option.value ~default:[] (Hashtbl.find_opt ctx.owners v) in
                if List.mem e.en_name prev then begin
                  err ctx "E0106" vsp (sprintf "variant `%s` is declared twice" v);
                  None
                end
                else begin
                  Hashtbl.replace ctx.owners v (prev @ [ e.en_name ]);
                  let ts = List.map (fun te -> let t = resolve ctx env te in check_storable ctx te.tspan t; t) tes in
                  Some (v, ts)
                end)
              e.en_variants
          in
          Hashtbl.replace ctx.variants e.en_name vs;
          take_pending (own_bounds e.en_name)
        end
      | IFn _ | IIface _ -> ())
    items;
  (* 2b. a struct cannot hold itself by value (it would be infinitely large);
     enums, lists and maps hold their contents indirectly, so recursion goes
     through them: `next: Option[Node]` *)
  List.iter
    (function
      | IStruct s when registered ctx.structs s.st_name s ->
        let seen = Hashtbl.create 8 in
        let rec reaches path t =
          match repr t with
          | TArray (a, _) -> reaches path a
          | TNamed (n, args) when Hashtbl.mem ctx.sfields n ->
            if n = s.st_name && path <> [] then Some path
            else if Hashtbl.mem seen (show t) then None
            else begin
              Hashtbl.replace seen (show t) ();
              let env = List.combine (Hashtbl.find ctx.structs n).st_gen args in
              List.find_map (fun (fname, ft) -> reaches (path @ [ fname ]) (subst env ft)) (Hashtbl.find ctx.sfields n)
            end
          | _ -> None
        in
        (match reaches [] (TNamed (s.st_name, List.map (fun g -> TParam g) s.st_gen)) with
         | Some path ->
           err ctx "E0114" s.st_span
             (sprintf "struct `%s` contains itself through `%s`, so it would be infinitely large" s.st_name (String.concat "." path))
             ~notes:[ sprintf "hold the inner value in an `Option`, an enum or a `List`, as in `next: Option[%s]`; those store it indirectly" s.st_name ]
         | None -> ())
      | _ -> ())
    items;
  (* 2c. the authority each struct and enum holds through its fields *)
  Types.compute_authority
    (List.filter_map
       (function
         | IStruct s when registered ctx.structs s.st_name s -> Some s.st_name
         | IEnum e when registered ctx.enums e.en_name e -> Some e.en_name
         | _ -> None)
       items)
    (fun n ->
      match Hashtbl.find_opt ctx.sfields n with
      | Some fs -> Some fs
      | None -> Option.map (List.concat_map (fun (v, ts) -> List.map (fun t -> (v, t)) ts)) (Hashtbl.find_opt ctx.variants n));
  (* 3. function signatures *)
  let sigs =
    List.filter_map
      (function
        | IFn d ->
          Hashtbl.reset param_kinds;
          let env = List.map (fun g -> (g, TParam g)) d.fn_gen in
          let ps, ret =
            match d.fn_foreign with
            | None ->
              let ps = List.map (fun p -> resolve ctx env p.prm_ty) d.fn_params in
              (ps, match d.fn_ret with Some te -> resolve ctx env te | None -> TUnit)
            | Some _ -> (
              Hashtbl.replace foreign_fns d.fn_name ();
              (* the Tessera type of a C function: Foreign first, Out parameters moved to the result *)
              if d.fn_name = "main" then err ctx "E0330" d.fn_span "`main` cannot be a foreign function";
              match Ffi.of_decl ~structs:(Hashtbl.find_opt ctx.structs) d with
              | Ok cs -> (Ffi.visible_params cs, Ffi.result_ty cs)
              | Error es ->
                List.iter (fun (sp, m) -> err ctx "E0330" sp m) es;
                (TCap Foreign :: List.map (fun _ -> TErr) d.fn_params, TErr))
          in
          let s = { gen = d.fn_gen; ps; ret } in
          if d.fn_foreign = None && not (Hashtbl.mem ctx.fns d.fn_name) then
            Hashtbl.replace param_names d.fn_name (List.map (fun p -> p.prm_name) d.fn_params);
          check_self ctx d ps;
          let declared =
            List.filter_map (fun (g, tes) -> match List.filter_map (resolve_bound ctx env) tes with [] -> None | bs -> Some (g, bs)) d.fn_bounds
          in
          if declared <> [] then Hashtbl.replace fn_bounds d.fn_name declared;
          let all = merge_bounds declared (implied_bounds ctx d.fn_gen ps) in
          Hashtbl.replace fn_cur_bounds d.fn_name all;
          Hashtbl.replace fn_kinds d.fn_name (List.of_seq (Hashtbl.to_seq param_kinds));
          take_pending all;
          (* a clash with a built-in method written in Tessera is reported by check_self *)
          let builtin_clash = match Hashtbl.find_opt method_owner d.fn_name with Some (_, fl) -> fl = Prelude.methods_file | None -> false in
          if String.contains d.fn_name ':' && not builtin_clash then Hashtbl.replace method_owner d.fn_name (d.fn_pub, d.fn_span.file);
          if Hashtbl.mem ctx.fns d.fn_name then begin
            if not builtin_clash then
              err ctx "E0106" d.fn_span (sprintf "function `%s` is defined more than once" d.fn_name)
          end
          else Hashtbl.replace ctx.fns d.fn_name s;
          Some (d, s)
        | _ -> None)
      items
  in
  (* the bounds of the types written in declarations *)
  List.iter (fun (bs, ps) -> cur_bounds := bs; pending := ps; check_pending ctx) (List.rev !deferred);
  (* test names are unique *)
  let seen_tests = Hashtbl.create 8 in
  List.iter
    (fun (d, _) ->
      match d.fn_test with
      | Some n ->
        let kind = if d.fn_bench then "bench" else "test" in
        if Hashtbl.mem seen_tests (kind, n) then err ctx "E0106" d.fn_span (sprintf "%s \"%s\" is defined more than once" kind n);
        Hashtbl.replace seen_tests (kind, n) ()
      | None -> ())
    sigs;
  (* E0306: a test's parameters are capabilities the runner provides *)
  List.iter
    (fun ((d : fn_decl), (s : fsig)) ->
      if d.fn_test <> None then
        ignore
          (List.fold_left2
             (fun seen (p : param) t ->
               match repr t with
               | (TCap Fs | TCap Clock) as c when not (List.mem c seen) -> c :: seen
               | TErr -> seen
               | t ->
                 err ctx "E0306" p.prm_span
                   (if List.mem t seen then sprintf "a test can take `%s` only once" (show t)
                    else sprintf "a test cannot take `%s`" (show t))
                   ~notes:[ "a test can take `fs: Fs` (a fresh temporary directory) and `clock: Clock`; pass anything else through an interface" ];
                 seen)
             [] d.fn_params s.ps);
      (* a bench measures real time *)
      if d.fn_bench && d.fn_virtual then
        err ctx "E0306" d.fn_span "a bench cannot take a virtual clock"
          ~notes:[ "a bench is timed on the real clock; take `clock: Clock`" ])
    sigs;
  (* 4. main *)
  List.iter (fun (d, s) -> if d.fn_name = "main" then check_main ctx d s) sigs;
  (* 5. bodies *)
  (* a table test's copy: say which case a problem comes from *)
  let check_one (d : fn_decl) s =
    let before = List.length ctx.diags in
    check_fn ctx d s;
    match d.fn_table with
    | Some (_, c) ->
      let fresh = List.length ctx.diags - before in
      let note = case_note ^ Source.one_line c.espan in
      ctx.diags <- List.mapi (fun i (x : Diag.t) -> if i < fresh then { x with notes = x.notes @ [ note ] } else x) ctx.diags
    | _ -> ()
  in
  let builtin (d : fn_decl) = d.fn_span.file = Prelude.methods_file in
  List.iter (fun (d, s) -> if d.fn_foreign = None && not (builtin d) then check_one d s) sigs;
  (* the built-in methods written in Tessera that were called, and those they call *)
  let checked = Hashtbl.create 16 in
  let rec more () =
    let todo = List.filter (fun ((d : fn_decl), _) -> builtin d && Hashtbl.mem used_methods d.fn_name && not (Hashtbl.mem checked d.fn_name)) sigs in
    if todo <> [] then begin
      List.iter (fun ((d : fn_decl), s) -> Hashtbl.replace checked d.fn_name (); check_one d s) todo;
      more ()
    end
  in
  more ();
  let diags =
    List.sort
      (fun (a : Diag.t) (b : Diag.t) -> compare (a.span.file, a.span.lo) (b.span.file, b.span.lo))
      ctx.diags
  in
  (* a table test's body is checked once per case: say each problem once,
     naming the case only when just one has it *)
  let diags =
    List.rev
      (List.fold_left
         (fun acc (d : Diag.t) ->
           let same (e : Diag.t) = e.span = d.span && e.code = d.code && e.message = d.message in
           if List.exists same acc then
             List.map
               (fun (e : Diag.t) ->
                 if same e then { e with notes = List.filter (fun n -> not (String.starts_with ~prefix:case_note n)) e.notes } else e)
               acc
           else d :: acc)
         [] diags)
  in
  let infos =
    List.map
      (fun ((d : fn_decl), s) ->
        let held = authority_of s.ps in
        { name = d.fn_name; span = d.fn_span;
          caps = List.map fst held;
          via = List.filter_map (fun (c, p) -> Option.map (fun p -> (c, p)) p) held;
          callbacks = List.exists has_fn s.ps || d.fn_bounds <> [] })
      (* forwarders only pass their arguments on; the methods they call are reported *)
      (List.filter (fun ((d : fn_decl), _) -> d.fn_test = None && d.fn_span.file <> Prelude.methods_file && d.fn_span.Span.file <> Prelude.file && not (Embed.is_forwarder d.fn_name)) sigs)
  in
  (diags, infos)
