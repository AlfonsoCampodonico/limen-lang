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



type var_kind = KLet | KParam | KPat | KGroup
type var_info = { vty : ty; vmut : bool; vspan : Span.t; vkind : var_kind }
type fsig = { gen : string list; ps : ty list; ret : ty }
type scope = { vars : (string, var_info) Hashtbl.t; barrier : bool }

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
}

type fn_info = { name : string; span : Span.t; caps : cap list; callbacks : bool }

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

let unify_at ctx span want got =
  try unify want got
  with Mismatch -> err ctx "E0201" span (sprintf "expected `%s`, found `%s`" (show want) (show got))

let builtin_types = [ "Int"; "Float"; "Bool"; "Str"; "Bytes"; "List"; "Array"; "Chan"; "Root"; "Net"; "Fs"; "Stdout"; "Clock"; "Listen"; "Listener"; "Conn"; "Rand"; "Stdin"; "Env"; "Exec"; "Foreign"; "Map" ]

(* Names other languages use for ambient I/O, mapped to the capability way. *)
let ambient =
  [ ("print", "Stdout", "out.print(...)"); ("println", "Stdout", "out.print(...)");
    ("eprint", "Stdout", "out.print(...)"); ("puts", "Stdout", "out.print(...)");
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
  match te.tdesc with
  | TyUnit -> TUnit
  | TyFn (ps, r) -> TFn (List.map (resolve ctx env) ps, resolve ctx env r)
  | TyInt _ ->
    err ctx "E0102" te.tspan "a number is only allowed as the size of an `Array`, as in `Array[Int, 3]`";
    TErr
  | TyName ("Array", [ elt; { tdesc = TyInt n; _ } ]) ->
    if n < 0 then (err ctx "E0102" te.tspan "an array size cannot be negative"; TErr)
    else TArray (resolve ctx env elt, n)
  | TyName ("Array", _) ->
    err ctx "E0109" te.tspan "`Array` takes an element type and a size, as in `Array[Float, 4]`";
    TErr
  | TyName ("dyn", [ { tdesc = TyName (i, []); tspan } ]) when List.mem i builtin_bounds ->
    err ctx "E0102" tspan (sprintf "`%s` is a built-in bound, not an interface with methods; it cannot be `dyn`" i)
      ~notes:[ sprintf "use it on a type parameter: `fn f[T: %s](x: T)`" i ];
    TErr
  | TyName ("dyn", [ { tdesc = TyName (i, []); tspan } ]) ->
    if Hashtbl.mem ifaces i then TDyn i
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
      | Some t -> simple t
      | None -> (
        let gen =
          match Hashtbl.find_opt ctx.structs n with
          | Some s -> Some s.st_gen
          | None -> Option.map (fun e -> e.en_gen) (Hashtbl.find_opt ctx.enums n)
        in
        match gen with
        | Some g -> if want (List.length g) then TNamed (n, args') else TErr
        | None ->
          let cands =
            builtin_types
            @ List.of_seq (Hashtbl.to_seq_keys ctx.structs)
            @ List.of_seq (Hashtbl.to_seq_keys ctx.enums)
            @ List.map fst env
          in
          if Hashtbl.mem ifaces n then
            err ctx "E0102" te.tspan (sprintf "`%s` is an interface, not a type" n)
              ~notes:[ sprintf "take a type parameter bounded by it: `fn f[T: %s](x: T)`" n ]
          else did_you_mean ctx "E0102" te.tspan (sprintf "unknown type `%s`" n) n cands;
          TErr)))

let check_storable ctx span t =
  if caps_in t [] <> [] then
    err ctx "E0301" span
      (sprintf "capability type `%s` cannot be stored in a struct or enum field" (show t))
      ~notes:[ "pass the capability as a function parameter instead, so it stays visible in signatures" ]
  else if has_chan t then
    err ctx "E0305" span
      (sprintf "channel type `%s` cannot be stored in a struct or enum field" (show t))
      ~notes:[ "pass the channel as a parameter or capture it in a task's closure instead" ]
  else if has_fn t then
    err ctx "E0304" span
      (sprintf "function type `%s` cannot be stored in a struct or enum field" (show t))
      ~notes:[ "a stored closure could carry a capability out of sight; pass it as a parameter instead" ]

(* ---------- scopes ---------- *)

let push f = f.scopes <- { vars = Hashtbl.create 8; barrier = false } :: f.scopes
let pop f = f.scopes <- List.tl f.scopes
let bind f name info = Hashtbl.replace (List.hd f.scopes).vars name info

(* Returns the variable and whether the lookup crossed a closure boundary. *)
let lookup f name =
  let rec go crossed = function
    | [] -> None
    | s :: rest -> (
      match Hashtbl.find_opt s.vars name with
      | Some v -> Some (v, crossed)
      | None -> go (crossed || s.barrier) rest)
  in
  go false f.scopes

let visible_names ctx f =
  List.concat_map (fun s -> List.of_seq (Hashtbl.to_seq_keys s.vars)) f.scopes
  @ List.of_seq (Hashtbl.to_seq_keys ctx.fns)
  @ List.of_seq (Hashtbl.to_seq_keys ctx.owners)
  @ [ "panic"; "channel"; "assert"; "assert_eq"; "map"; "catch"; "bytes"; "keep" ]

(* Generic functions' bounds: function -> type parameter -> interfaces. *)
let fn_bounds : (string, (string * string list) list) Hashtbl.t = Hashtbl.create 8

(* Bounds of the type parameters of the function being checked. *)
let cur_bounds : (string * string list) list ref = ref []

(* Whether type parameter [g] of the function being checked has bound [b]. *)
let bounded g b =
  let bs = Option.value ~default:[] (List.assoc_opt g !cur_bounds) in
  List.mem b bs || (b = "Ordered" && List.mem "Key" bs)

(* Instantiations to check against their bounds once the body's types are
   known: (where, the type used, the interface, the type parameter). *)
let pending : (Span.t * ty * string * string) list ref = ref []

(* Key types of maps created in the function being checked: they must end
   up Int, Str or Bool. *)
let map_keys : (Span.t * ty) list ref = ref []

let add_bounds fname env sp =
  match Hashtbl.find_opt fn_bounds fname with
  | None -> ()
  | Some bs ->
    List.iter
      (fun (g, ifs) -> match List.assoc_opt g env with Some t -> List.iter (fun i -> pending := (sp, t, i, g) :: !pending) ifs | None -> ())
      bs

(* The built-in methods written in Vela that the program calls: only their
   bodies are checked (and only they are compiled). *)
let used_methods : (string, unit) Hashtbl.t = Hashtbl.create 16

(* Methods by `Type::name`: whether they are `pub`, and the file (module)
   that defines them. A private method can only be called from its own file. *)
let method_owner : (string, bool * string) Hashtbl.t = Hashtbl.create 16

(* Foreign functions can only be called directly, not used as values. *)
let foreign_fns : (string, unit) Hashtbl.t = Hashtbl.create 8
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

let ctor_ty ctx en v =
  let e = Hashtbl.find ctx.enums en in
  let targs = List.map (fun _ -> fresh ()) e.en_gen in
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

let methods (t : ty) : (string * (unit -> ty list * ty)) list =
  let m name ps r = (name, fun () -> (ps, r)) in
  match t with
  | TStr ->
    [ m "len" [] TInt; m "is_empty" [] TBool; m "lines" [] (TList TStr);
      m "split" [ TStr ] (TList TStr); m "trim" [] TStr; m "contains" [ TStr ] TBool;
      m "starts_with" [ TStr ] TBool; m "ends_with" [ TStr ] TBool; m "to_upper" [] TStr;
      m "to_lower" [] TStr; m "replace" [ TStr; TStr ] TStr; m "parse_int" [] (opt TInt);
      m "parse_float" [] (opt TFloat); m "slice" [ TInt; TInt ] TStr; m "index_of" [ TStr ] (opt TInt);
      m "bytes" [] (TList TInt); m "chars" [] (TList TStr); m "to_bytes" [] TBytes ]
  | TBytes ->
    [ m "len" [] TInt; m "is_empty" [] TBool; m "get" [ TInt ] (opt TInt); m "slice" [ TInt; TInt ] TBytes;
      m "to_list" [] (TList TInt); m "to_str" [] TStr ]
  | TInt ->
    [ m "to_str" [] TStr; m "to_float" [] TFloat; m "abs" [] TInt; m "to_char" [] TStr; m "min" [ TInt ] TInt;
      m "max" [ TInt ] TInt ]
  | TFloat -> [ m "to_str" [] TStr; m "to_int" [] TInt; m "abs" [] TFloat; m "sqrt" [] TFloat;
      m "to_fixed" [ TInt ] TStr ]
  | TBool -> [ m "to_str" [] TStr ]
  | TList a ->
    [ m "len" [] TInt; m "is_empty" [] TBool; m "get" [ TInt ] (opt a); m "first" [] (opt a);
      m "last" [] (opt a); m "push" [ a ] TUnit; m "contains" [ a ] TBool;
      m "reverse" [] (TList a); m "sort" [] (TList a); m "take" [ TInt ] (TList a);
      m "skip" [ TInt ] (TList a); m "filter" [ TFn ([ a ], TBool) ] (TList a);
      ("map", fun () -> let u = fresh () in ([ TFn ([ a ], u) ], TList u));
      ("fold", fun () -> let u = fresh () in ([ u; TFn ([ u; a ], u) ], u));
      m "join" [ TStr ] TStr; m "sum" [] TInt ]
  | TMap (k, v) ->
    [ m "len" [] TInt; m "is_empty" [] TBool; m "get" [ k ] (opt v); m "contains" [ k ] TBool;
      m "remove" [ k ] TUnit; m "keys" [] (TList k); m "values" [] (TList v);
      m "entries" [] (TList (TNamed (Ast.tuple_name 2, [ k; v ]))) ]
  | TArray (a, _) ->
    [ m "len" [] TInt; m "get" [ TInt ] (opt a); m "to_list" [] (TList a); m "contains" [ a ] TBool ]
  | TNamed ("Option", [ a ]) ->
    [ m "is_some" [] TBool; m "is_none" [] TBool;
      ("map", fun () -> let u = fresh () in ([ TFn ([ a ], u) ], opt u)) ]
  | TNamed ("Result", [ a; e ]) ->
    [ m "is_ok" [] TBool; m "is_err" [] TBool;
      ("map_err", fun () -> let u = fresh () in ([ TFn ([ e ], u) ], res a u)) ]
  | TCap Net -> [ m "get" [ TStr ] (res (TNamed ("HttpResponse", [])) io_err); m "only" [ TStr ] (TCap Net); m "connect" [ TStr ] (res (TCap Conn) io_err); m "connect_tls" [ TStr ] (res (TCap Conn) io_err) ]
  | TCap Listen -> [ m "on" [ TStr ] (res (TCap Listener) io_err); m "on_tls" [ TStr; TStr; TStr ] (res (TCap Listener) io_err) ]
  | TCap Listener -> [ m "accept" [] (res (TCap Conn) io_err); m "port" [] TInt; m "close" [] TUnit ]
  | TCap Conn -> [ m "read" [] (res TStr io_err); m "write" [ TStr ] (res TUnit io_err); m "close" [] TUnit ]
  | TCap Fs ->
    [ m "read" [ TStr ] (res TStr io_err); m "write" [ TStr; TStr ] (res TUnit io_err);
      m "append" [ TStr; TStr ] (res TUnit io_err); m "list" [ TStr ] (res (TList TStr) io_err);
      m "remove" [ TStr ] (res TUnit io_err); m "make_dir" [ TStr ] (res TUnit io_err);
      m "is_dir" [ TStr ] TBool; m "full_path" [ TStr ] (res TStr io_err); m "is_read_only" [] TBool;
      m "exists" [ TStr ] TBool; m "under" [ TStr ] (TCap Fs); m "read_only" [] (TCap Fs) ]
  | TCap Stdout -> [ m "print" [ TStr ] TUnit; m "write" [ TStr ] TUnit ]
  | TCap Clock -> [ m "now_ms" [] TInt; m "sleep" [ TInt ] TUnit ]
  | TCap Rand -> [ m "int" [ TInt; TInt ] TInt; m "float" [] TFloat ]
  | TCap Stdin -> [ m "read_line" [] (opt TStr); m "read_all" [] TStr ]
  | TCap Env -> [ m "get" [ TStr ] (opt TStr) ]
  | TCap Exec ->
    [ m "run" [ TStr; TList TStr ] (res (TNamed ("ProcessOutput", [])) io_err); m "only" [ TStr ] (TCap Exec) ]
  | TCap Foreign ->
    [ m "only" [ TStr ] (TCap Foreign); m "allows" [ TStr ] TBool; m "copy" [ TNamed ("Handle", []); TInt ] TBytes ]
  | TCap Root -> [ m "args" [] (TList TStr) ]
  | TChan a -> [ m "send" [ a ] TUnit; m "recv" [] (opt a); m "close" [] TUnit ]
  | _ -> []

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

let root_fields = [ ("net", TCap Net); ("fs", TCap Fs); ("stdout", TCap Stdout); ("clock", TCap Clock); ("listen", TCap Listen);
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
      ({ edesc = EField (x, fld, { x.espan with lo = x.espan.hi }); espan = x.espan; ety = ft; einst = [] }, ft))
    (recv, rt) path

let ambiguity n name paths =
  sprintf "`%s` is ambiguous in `%s`: it is reached through %s, at the same depth" name n
    (String.concat " and " (List.map (fun p -> "`" ^ String.concat "." (p @ [ name ]) ^ "`") paths))

let ambiguous ctx sp n name paths =
  err ctx "E0120" sp (ambiguity n name paths)
    ~notes:[ sprintf "name the embedded struct you mean: %s" (String.concat " or " (List.map (fun p -> "`." ^ String.concat "." (p @ [ name ]) ^ "`") paths)) ]

(* Does [t] have every method of interface [i], with the same types? *)
let satisfies ctx t i : string option =
  let ms = Option.value ~default:[] (Hashtbl.find_opt ifaces i) in
  let t = repr t in
  let check (m, (ps, r)) =
    let want = TFn (ps, r) in
    let trial got = try unify want got; None with Mismatch -> Some (sprintf "its `%s` has type `%s`, but `%s` needs `%s`" m (show got) i (show want)) in
    match t with
    | TVar _ | TErr -> None
    | TDyn j when j = i -> None
    | TDyn j -> (
      match Option.bind (Hashtbl.find_opt ifaces j) (List.assoc_opt m) with
      | Some (ps', r') -> trial (TFn (ps', r'))
      | None -> Some (sprintf "`dyn %s` has no method `%s`" j m))
    | TParam g ->
      let bs = Option.value ~default:[] (List.assoc_opt g !cur_bounds) in
      if List.mem i bs then None
      else (
        match List.find_map (fun b -> Option.bind (Hashtbl.find_opt ifaces b) (List.assoc_opt m)) bs with
        | Some (ps', r') -> trial (TFn (ps', r'))
        | None -> Some (sprintf "type parameter `%s` is not bounded by `%s`; add it: `[%s: %s]`" g i g i))
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
  List.find_map check ms

let rec check_expr ctx f e : ty =
  let t = check_expr_inner ctx f e in
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
  | ECall (callee, args) -> check_call ctx f callee args e.espan
  | EMethod (({ edesc = EVar en; _ } as recv), name, nsp, args)
    when lookup f en = None && Hashtbl.mem ctx.enums en ->
    (* `Enum.Variant(args)` is a constructor call, not a method call. *)
    let callee = { edesc = EField (recv, name, nsp); espan = Span.merge recv.espan nsp; ety = TErr; einst = [] } in
    check_call ctx f callee args e.espan
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
  | ETry x -> check_try ctx f x e.espan
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
  | EDyn (x, i) -> ignore (check_expr ctx f x); TDyn i
  | EGroup (name, until, b) ->
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
    if List.length es <> n then
      err ctx "E0201" e.espan
        (sprintf "expected %d element%s for `%s`, found %d" n (plural n) (show want) (List.length es));
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
  | EBlock b, _ -> expect_block ctx f b want; e.ety <- want
  | _, TDyn i -> coerce ctx e (check_expr ctx f e) i
  | _ -> expect_plain ctx f e want

(* A value used where `dyn I` is expected: box it if its type satisfies I. *)
and coerce ctx e t i =
  match repr t with
  | TDyn j when j = i -> ()
  | TVar _ -> unify_at ctx e.espan (TDyn i) t
  | TErr -> ()
  | TNamed (n, _) as ct when (Hashtbl.mem ctx.structs n || Hashtbl.mem ctx.enums n) && not (List.mem n [ "Option"; "Result"; "IoError" ]) ->
    if caps_in ct [] <> [] || has_fn ct || has_chan ct then
      err ctx "E0112" e.espan
        (sprintf "`%s` holds a capability, function or channel, so it cannot become `dyn %s`" (show ct) i)
        ~notes:[ "authority must stay visible in types; pass it with a type parameter, as in `[T: " ^ i ^ "]`" ]
    else begin
      match satisfies ctx ct i with
      | Some why -> err ctx "E0111" e.espan (sprintf "`%s` does not satisfy interface `%s`" (show ct) i) ~notes:[ why ]
      | None ->
        let inner = { e with edesc = e.edesc } in
        e.edesc <- EDyn (inner, i);
        e.ety <- TDyn i
    end
  | t ->
    err ctx "E0112" e.espan
      (sprintf "only struct and enum values can become `dyn %s`, not `%s`" i (show t))
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
  match lookup f n with
  | Some (v, _) when v.vkind = KGroup ->
    err ctx "E0310" sp (sprintf "`%s` is a group handle; the only thing you can do with it is `%s.go(...)`" n n);
    TErr
  | Some (v, _) -> v.vty
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
          e.edesc <- EField ({ edesc = EVar en; espan = sp; ety = TErr; einst = [] }, n, sp);
        ctor_ty ctx en n
      | Some ens ->
        err ctx "E0104" sp
          (sprintf "`%s` is a variant of several enums (%s); qualify it, for example `%s.%s`" n
             (String.concat ", " ens) (List.hd ens) n);
        TErr
      | None -> (
        match List.find_opt (fun (a, _, _) -> a = n) ambient with
        | Some (_, cap, how) ->
          err ctx "E0302" sp
            (sprintf "`%s` does not exist: Vela has no ambient I/O" n)
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

and check_args ctx f args = List.iter (fun a -> ignore (check_expr ctx f a)) args

and check_call ctx f callee args sp =
  match callee.edesc with
  | EVar "assert" when lookup f "assert" = None && not (Hashtbl.mem ctx.fns "assert") ->
    (match args with
     | [ a ] -> expect ctx f a TBool
     | _ -> err ctx "E0107" sp "`assert` takes 1 argument, a Bool"; check_args ctx f args);
    TUnit
  | EVar "assert_eq" when lookup f "assert_eq" = None && not (Hashtbl.mem ctx.fns "assert_eq") ->
    (match args with
     | [ a; b ] -> (
       let t = check_expr ctx f a in
       expect ctx f b t;
       match repr t with
       | TFn _ | TCap _ | TChan _ | TDyn _ -> err ctx "E0201" sp (sprintf "values of type `%s` cannot be compared" (show t))
       | _ -> ())
     | _ -> err ctx "E0107" sp "`assert_eq` takes 2 arguments"; check_args ctx f args);
    TUnit
  | EVar "keep" when lookup f "keep" = None && not (Hashtbl.mem ctx.fns "keep") ->
    (* `keep(x)`: x counts as used, so a benchmark's work is not optimized away *)
    (match args with
     | [ a ] -> ignore (check_expr ctx f a)
     | _ -> err ctx "E0107" sp "`keep` takes 1 argument"; check_args ctx f args);
    TUnit
  | EVar "channel" when lookup f "channel" = None && not (Hashtbl.mem ctx.fns "channel") ->
    (match args with
     | [ a ] -> expect ctx f a TInt
     | _ -> err ctx "E0107" sp "`channel` takes 1 argument, its capacity"; check_args ctx f args);
    TChan (fresh ())
  | EVar "catch" when lookup f "catch" = None && not (Hashtbl.mem ctx.fns "catch") -> (
    (* catch(|| ...): Ok(value), or Err(message) if anything inside panicked *)
    let t = fresh () in
    match args with
    | [ a ] -> expect ctx f a (TFn ([], t)); TNamed ("Result", [ t; TStr ])
    | _ ->
      err ctx "E0107" sp "`catch` takes 1 argument, a closure with no parameters, as in `catch(|| risky())`";
      check_args ctx f args;
      TErr)
  | EVar "bytes" when lookup f "bytes" = None && not (Hashtbl.mem ctx.fns "bytes") ->
    (* bytes([104, 105]): each item 0 to 255, or it panics *)
    (match args with
     | [ a ] -> expect ctx f a (TList TInt)
     | _ -> err ctx "E0107" sp "`bytes` takes 1 argument, a list of Ints from 0 to 255"; check_args ctx f args);
    TBytes
  | EVar "map" when lookup f "map" = None && not (Hashtbl.mem ctx.fns "map") ->
    let k = fresh () and v = fresh () in
    (match args with
     | [] -> ()
     | [ a ] -> expect ctx f a (TList (TNamed (tuple_name 2, [ k; v ])))
     | _ -> err ctx "E0107" sp "`map` takes no arguments, or a list of (key, value) pairs"; check_args ctx f args);
    map_keys := (sp, k) :: !map_keys;
    TMap (k, v)
  | EVar "panic" when lookup f "panic" = None && not (Hashtbl.mem ctx.fns "panic") ->
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
    match repr ct with
    | TFn (ps, r) ->
      if List.length ps <> List.length args then begin
        err ctx "E0107" sp
          (sprintf "this function takes %d argument%s but %d %s supplied" (List.length ps)
             (plural (List.length ps)) (List.length args)
             (if List.length args = 1 then "was" else "were"));
        check_args ctx f args
      end
      else List.iter2 (fun a p -> expect ctx f a p) args ps;
      r
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

and check_field ctx f e recv name nsp =
  match recv.edesc with
  | EVar en when lookup f en = None && Hashtbl.mem ctx.enums en ->
    let vs = Hashtbl.find ctx.variants en in
    if List.mem_assoc name vs then ctor_ty ctx en name
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
  | TDyn i -> Option.map (fun (ps, _) -> List.length ps) (Option.bind (Hashtbl.find_opt ifaces i) (List.assoc_opt name))
  | TParam g ->
    let bs = Option.value ~default:[] (List.assoc_opt g !cur_bounds) in
    List.find_map (fun b -> Option.map (fun (ps, _) -> List.length ps) (Option.bind (Hashtbl.find_opt ifaces b) (List.assoc_opt name))) bs
  | (TList _ | TMap _) when name = "push" || name = "remove" -> None (* they change the receiver *)
  | t -> Option.map (fun mk -> List.length (fst (mk ()))) (List.assoc_opt name (methods t))

(* `x.name` without a call, where `name` is a method: a closure that calls it
   on the value `x` had when the expression ran, as if written
   `{ let $recv = x; |a, b| $recv.name(a, b) }`. The rewritten tree is what
   both backends see; [recv] is already checked. *)
and method_value ctx f e recv rt name nsp arity =
  let sp = e.espan in
  let mk d = { edesc = d; espan = sp; ety = TErr; einst = [] } in
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
      Hashtbl.fold
        (fun k _ acc ->
          if acc = None && (k = suffix || String.ends_with ~suffix:("." ^ suffix) k)
             && (match Hashtbl.find_opt method_owner k with Some (_, fl) -> fl = file | None -> false)
          then Some k
          else acc)
        ctx.fns None
    in
    (* the built-in methods written in Vela (std/methods.vela) *)
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
      | TDyn i -> ("dyn " ^ i, [ i ])
      | TParam g -> (g, Option.value ~default:[] (List.assoc_opt g !cur_bounds))
      | _ -> ("", [])
    in
    let found = List.find_map (fun b -> Option.bind (Hashtbl.find_opt ifaces b) (List.assoc_opt name)) bs in
    match found with
    | Some (ps, r) ->
      if List.length ps <> List.length args then begin
        err ctx "E0107" nsp
          (sprintf "method `%s` takes %d argument%s besides `self`, but %d %s supplied" name (List.length ps)
             (plural (List.length ps)) (List.length args) (if List.length args = 1 then "was" else "were"));
        check_args ctx f args
      end
      else List.iter2 (fun a p -> expect ctx f a p) args ps;
      r
    | None ->
      let declared = List.concat_map (fun b -> List.map fst (Option.value ~default:[] (Hashtbl.find_opt ifaces b))) bs in
      let notes =
        if bs = [] then [ sprintf "`%s` is a type parameter with no bounds; add one, as in `[%s: Interface]`, whose interface declares `fn %s(self, ...)`" g g name ]
        else [ sprintf "the bounds of `%s` (%s) declare: %s" g (String.concat ", " bs) (if declared = [] then "nothing" else String.concat ", " declared) ]
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
    | TFn (p0 :: ps, r) ->
      unify_at ctx recv.espan p0 rt;
      if List.length ps <> List.length args then begin
        err ctx "E0107" nsp
          (sprintf "method `%s` takes %d argument%s besides `self`, but %d %s supplied" name (List.length ps)
             (plural (List.length ps)) (List.length args) (if List.length args = 1 then "was" else "were"));
        check_args ctx f args
      end
      else List.iter2 (fun a p -> expect ctx f a p) args ps;
      e.edesc <- ECall ({ edesc = EVar key; espan = nsp; ety = t; einst = env }, recv :: args);
      r
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
    | TFn (p0 :: ps, r) ->
      unify_at ctx recv.espan p0 rt;
      if List.length ps <> List.length args then begin
        err ctx "E0107" nsp
          (sprintf "method `%s` takes %d argument%s besides `self`, but %d %s supplied" name (List.length ps)
             (plural (List.length ps)) (List.length args) (if List.length args = 1 then "was" else "were"));
        check_args ctx f args
      end
      else List.iter2 (fun a p -> expect ctx f a p) args ps;
      r
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

and check_try ctx f x sp =
  if f.in_defer > 0 then begin
    err ctx "E0115" sp "`?` cannot leave a `defer` body"
      ~notes:[ "handle the error inside it, for example with `??` or a `match`" ];
    ignore (check_expr ctx f x);
    TErr
  end
  else check_try_ok ctx f x sp

and check_try_ok ctx f x sp =
  if f.in_group > 0 then
    err ctx "E0311" sp "`?` cannot leave a `group` block early; every task must finish first"
      ~notes:[ "handle the error inside the task, or after the group returns its results" ];
  let t = repr (check_expr ctx f x) in
  let wrong_fn rt kind =
    err ctx "E0202" sp
      (sprintf "`?` can only be used in a function that returns `%s`, this one returns `%s`" kind (show rt))
      ~notes:[ "for main, write `fn main(root: Root) -> Result[(), IoError]`" ]
  in
  match t with
  | TErr -> TErr
  | TNamed ("Result", [ a; e ]) ->
    (match repr f.ret_ty with
     | TNamed ("Result", [ _; fe ]) -> (
       try unify fe e
       with Mismatch ->
         err ctx "E0202" sp
           (sprintf "`?` would return an error of type `%s`, but this function returns errors of type `%s`"
              (show e) (show fe))
           ~notes:[ "convert the error first with `.map_err(|e| ...)`" ])
     | TVar _ -> unify_at ctx sp f.ret_ty (res (fresh ()) e)
     | TErr -> ()
     | rt -> wrong_fn rt "Result");
    a
  | TNamed ("Option", [ a ]) ->
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
    in
    let bad () =
      err ctx "E0201" sp (sprintf "operator `%s` cannot be applied to `%s`" name (show t))
    in
    match op with
    | Add -> (match t with TInt | TFloat | TStr | TBytes | TErr | TVar _ -> t | _ -> bad (); TErr)
    | Sub | Mul | Div -> (match t with TInt | TFloat | TErr | TVar _ -> t | _ -> bad (); TErr)
    | Mod -> (match t with TInt | TErr | TVar _ -> t | _ -> bad (); TErr)
    | Lt | Le | Gt | Ge -> (match unordered ctx t with None -> () | Some _ -> bad ()); TBool
    | _ -> (match t with TFn _ | TCap _ | TChan _ | TDyn _ -> bad () | _ -> ()); TBool)

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
  let cf = { ret_ty = ret; scopes = scope :: f.scopes; loops = 0; genv = f.genv; in_group = 0; in_defer = 0 } in
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

and check_match ?want ctx f s arms sp =
  let st = check_expr ctx f s in
  let result = match want with Some w -> w | None -> fresh () in
  List.iter
    (fun arm ->
      push f;
      check_pat ctx f st arm.apat;
      (match want with
       | Some w -> expect ctx f arm.abody w
       | None ->
         let bt = check_expr ctx f arm.abody in
         unify_at ctx arm.abody.espan result bt);
      pop f)
    arms;
  (match arms with
   | [] -> err ctx "E0204" sp "`match` needs at least one arm"
   | _ -> (
     match missing_arms ctx st (List.map (fun a -> a.apat) arms) with
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

and check_pat ctx f t p =
  match p.pdesc with
  | PWild -> ()
  | PBind n -> bind f n { vty = t; vmut = false; vspan = p.pspan; vkind = KPat }
  | PInt _ -> unify_at ctx p.pspan t TInt
  | PStr _ -> unify_at ctx p.pspan t TStr
  | PBool _ -> unify_at ctx p.pspan t TBool
  | PTuple ps ->
    let ts = List.map (fun _ -> fresh ()) ps in
    unify_at ctx p.pspan (TNamed (tuple_name (List.length ps), ts)) t;
    List.iter2 (check_pat ctx f) ts ps
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
        if List.length ptys <> List.length args then begin
          err ctx "E0107" p.pspan
            (sprintf "variant `%s` has %d field%s, the pattern has %d" v (List.length ptys)
               (plural (List.length ptys)) (List.length args));
          skip ()
        end
        else List.iter2 (fun a pt -> check_pat ctx f pt a) args ptys))

(* Patterns still unmatched, as source text. Single-field payloads are
   checked recursively; multi-field payloads only need one arm each. *)
and missing_arms ctx t pats =
  let rec irrefutable p = match p.pdesc with PWild | PBind _ -> true | PTuple ps -> List.for_all irrefutable ps | _ -> false in
  if List.exists irrefutable pats then []
  else
    match repr t with
    | TNamed (n, _) when is_tuple_name n -> if exhaustive ctx (List.map (fun p -> [ p ]) pats) [ t ] then [] else [ "_" ]
    | TBool ->
      List.filter_map
        (fun b -> if List.exists (fun p -> p.pdesc = PBool b) pats then None else Some (string_of_bool b))
        [ true; false ]
    | TNamed (n, args) when Hashtbl.mem ctx.variants n ->
      let gen = (Hashtbl.find ctx.enums n).en_gen in
      let env = List.combine gen args in
      List.concat_map
        (fun (v, payload) ->
          let rows =
            List.filter_map
              (fun p -> match p.pdesc with PVariant (_, v', a) when v' = v -> Some a | _ -> None)
              pats
          in
          match (payload, rows) with
          | _, [] ->
            [ (match payload with [] -> v | ps -> v ^ "(" ^ String.concat ", " (List.map (fun _ -> "_") ps) ^ ")") ]
          | [], _ -> []
          | [ pt ], _ ->
            let inner = List.filter_map (function [ p ] -> Some p | _ -> None) rows in
            List.map (fun m -> v ^ "(" ^ m ^ ")") (missing_arms ctx (subst env pt) inner)
          | _ -> [])
        (Hashtbl.find ctx.variants n)
    | TErr | TVar _ -> []
    | _ -> [ "_" ]

(* Do the rows of patterns cover every value of the column types? The
   classic matrix check: split on the first column's constructors. *)
and exhaustive ctx (rows : pat list list) (tys : ty list) : bool =
  match tys with
  | [] -> rows <> []
  | t :: rest_tys -> (
    let wild p = match p.pdesc with PWild | PBind _ -> true | _ -> false in
    let wild_p = { pdesc = PWild; pspan = Span.dummy } in
    let default () = exhaustive ctx (List.filter_map (function p :: r when wild p -> Some r | _ -> None) rows) rest_tys in
    (* rows specialised to a constructor with these argument types *)
    let special matches args =
      List.filter_map
        (function
          | p :: r when wild p -> Some (List.map (fun _ -> wild_p) args @ r)
          | p :: r -> ( match matches p with Some ps -> Some (ps @ r) | None -> None)
          | [] -> None)
        rows
    in
    match repr t with
    | TNamed (n, args) when is_tuple_name n ->
      exhaustive ctx (special (fun p -> match p.pdesc with PTuple ps -> Some ps | _ -> None) args) (args @ rest_tys)
    | TBool ->
      List.for_all
        (fun b -> exhaustive ctx (special (fun p -> if p.pdesc = PBool b then Some [] else None) []) rest_tys)
        [ true; false ]
    | TNamed (n, targs) when Hashtbl.mem ctx.variants n ->
      let env = List.combine (Hashtbl.find ctx.enums n).en_gen targs in
      List.for_all
        (fun (v, payload) ->
          let ptys = List.map (subst env) payload in
          exhaustive ctx
            (special (fun p -> match p.pdesc with PVariant (_, v', a) when v' = v -> Some a | _ -> None) ptys)
            (ptys @ rest_tys))
        (Hashtbl.find ctx.variants n)
    | TErr | TVar _ -> true
    | _ -> default ())

and check_select ctx f arms sp =
  let defaults = List.filter (fun a -> a.sop = SelDefault) arms in
  if List.length defaults > 1 then err ctx "E0106" sp "`select` has more than one `_` arm";
  List.iter
    (fun a -> if a.sop = SelDefault && a.sguard <> None then err ctx "E0003" a.sspan_ "the `_` arm of a `select` cannot have a guard")
    arms;
  if List.length defaults = List.length arms then
    err ctx "E0003" sp "`select` needs at least one channel arm: `v = ch.recv() => ...` or `ch.send(x) => ...`";
  let afters = List.filter (fun a -> match a.sop with SelAfter _ -> true | _ -> false) arms in
  (match afters with
   | _ :: second :: _ -> err ctx "E0106" second.sspan_ "`select` has more than one `clock.after` arm"
   | _ -> ());
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
    (fun (sp, t, i, g) ->
      let why =
        match i with
        | "Ordered" -> Option.map (fun u -> sprintf "`%s` has no order: numbers, strings, Bool, and lists, tuples, structs and enums of them are ordered" (show u)) (unordered ctx t)
        | "Key" -> Option.map (fun u -> sprintf "`%s` cannot be a map key: keys are Int, Str, Bytes, Bool, and tuples, structs and enums of them" (show u)) (unkeyable ctx t)
        | _ -> satisfies ctx t i
      in
      match why with
      | None -> ()
      | Some why ->
        err ctx "E0111" sp
          (if List.mem i builtin_bounds then sprintf "`%s` does not satisfy `%s`, required of `%s`" (show (repr t)) i g
           else sprintf "`%s` does not satisfy interface `%s`, required of `%s`" (show (repr t)) i g)
          ~notes:(why :: match repr t with TParam p when List.mem i builtin_bounds -> [ sprintf "add the bound: `[%s: %s]`" p i ] | _ -> []))
    ps

let check_fn ctx (d : fn_decl) (s : fsig) =
  cur_bounds := List.map (fun (g, bs) -> (g, List.map fst bs)) d.fn_bounds;
  pending := [];
  map_keys := [];
  Fun.protect ~finally:(fun () -> check_pending ctx) @@ fun () ->
  let genv = List.map (fun g -> (g, TParam g)) d.fn_gen in
  let scope = { vars = Hashtbl.create 8; barrier = false } in
  List.iter2
    (fun p t -> Hashtbl.replace scope.vars p.prm_name { vty = t; vmut = false; vspan = p.prm_span; vkind = KParam })
    d.fn_params s.ps;
  let f = { ret_ty = s.ret; scopes = [ scope ]; loops = 0; genv; in_group = 0; in_defer = 0 } in
  match repr s.ret with
  | TDyn _ -> expect_block ctx f d.fn_body s.ret (* the body's value becomes `dyn`, branch by branch *)
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
  Hashtbl.reset used_methods;
  Hashtbl.reset foreign_fns;
  Hashtbl.reset ifaces;
  List.iter (fun b -> Hashtbl.replace ifaces b []) builtin_bounds;
  Hashtbl.reset fn_bounds;
  let items = Prelude.items () @ user_items in
  let type_defined n = Hashtbl.mem ctx.structs n || Hashtbl.mem ctx.enums n || List.mem n builtin_types in
  (* 1. register type names *)
  List.iter
    (function
      | IStruct s ->
        if type_defined s.st_name then err ctx "E0106" s.st_span (sprintf "type `%s` is defined more than once" s.st_name)
        else Hashtbl.replace ctx.structs s.st_name s
      | IEnum e ->
        if type_defined e.en_name then err ctx "E0106" e.en_span (sprintf "type `%s` is defined more than once" e.en_name)
        else Hashtbl.replace ctx.enums e.en_name e
      | IIface i ->
        if List.mem i.if_name builtin_bounds then
          err ctx "E0106" i.if_span (sprintf "`%s` is a built-in bound; name the interface something else" i.if_name)
        else if type_defined i.if_name || Hashtbl.mem ifaces i.if_name then
          err ctx "E0106" i.if_span (sprintf "`%s` is defined more than once" i.if_name)
        else Hashtbl.replace ifaces i.if_name []
      | IFn _ -> ())
    items;
  (* 1b. interface methods *)
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
                Some (m, (List.map (fun p -> resolve ctx [] p.prm_ty) ps, match r with Some te -> resolve ctx [] te | None -> TUnit))
              end)
            i.if_methods
        in
        Hashtbl.replace ifaces i.if_name ms
      | _ -> ())
    items;
  (* 2. resolve field and payload types *)
  List.iter
    (function
      | IStruct s when not (registered ctx.structs s.st_name s) -> ()
      | IStruct s ->
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
        Hashtbl.replace ctx.sfields s.st_name fs
      | IEnum e ->
        if registered ctx.enums e.en_name e then begin
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
          Hashtbl.replace ctx.variants e.en_name vs
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
  (* 3. function signatures *)
  let sigs =
    List.filter_map
      (function
        | IFn d ->
          let env = List.map (fun g -> (g, TParam g)) d.fn_gen in
          let ps, ret =
            match d.fn_foreign with
            | None ->
              let ps = List.map (fun p -> resolve ctx env p.prm_ty) d.fn_params in
              (ps, match d.fn_ret with Some te -> resolve ctx env te | None -> TUnit)
            | Some _ -> (
              Hashtbl.replace foreign_fns d.fn_name ();
              (* the Vela type of a C function: Foreign first, Out parameters moved to the result *)
              if d.fn_name = "main" then err ctx "E0330" d.fn_span "`main` cannot be a foreign function";
              match Ffi.of_decl d with
              | Ok cs -> (Ffi.visible_params cs, Ffi.result_ty cs)
              | Error es ->
                List.iter (fun (sp, m) -> err ctx "E0330" sp m) es;
                (TCap Foreign :: List.map (fun _ -> TErr) d.fn_params, TErr))
          in
          let s = { gen = d.fn_gen; ps; ret } in
          check_self ctx d ps;
          List.iter
            (fun (_, bs) ->
              List.iter
                (fun (b, bsp) ->
                  if not (Hashtbl.mem ifaces b) then
                    did_you_mean ctx "E0102" bsp (sprintf "unknown interface `%s`" b) b (List.of_seq (Hashtbl.to_seq_keys ifaces)))
                bs)
            d.fn_bounds;
          if d.fn_bounds <> [] then Hashtbl.replace fn_bounds d.fn_name (List.map (fun (g, bs) -> (g, List.map fst bs)) d.fn_bounds);
          (* a clash with a built-in method written in Vela is reported by check_self *)
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
             [] d.fn_params s.ps))
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
  (* the built-in methods written in Vela that were called, and those they call *)
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
        let found = List.fold_left (fun acc t -> caps_in t acc) [] s.ps in
        { name = d.fn_name; span = d.fn_span;
          caps = List.filter (fun c -> List.mem c found) all_caps;
          callbacks = List.exists has_fn s.ps || d.fn_bounds <> [] })
      (* forwarders only pass their arguments on; the methods they call are reported *)
      (List.filter (fun ((d : fn_decl), _) -> d.fn_test = None && d.fn_span.file <> Prelude.methods_file && not (Embed.is_forwarder d.fn_name)) sigs)
  in
  (diags, infos)
