(* IR -> C. Value types (Int, Float, Bool, (), structs, non-recursive enums)
   are C values; Str, List, closures, capabilities and recursive enums are
   reference-counted pointers. Every composite type gets generated
   dup/drop/eq/show functions named <CName>_dup and so on. *)
open Types
module I = Ir

let sprintf = Printf.sprintf

type ctx = {
  g : Lower.gctx;
  names : (string, string) Hashtbl.t; (* show t -> C name *)
  mutable types : ty list; (* composite types, in registration order *)
  lits : (string, string) Hashtbl.t;
  mutable lit_order : (string * string) list;
  mutable boxed : string list; (* C names of boxed (recursive) enums *)
  envs : (string, I.var list) Hashtbl.t; (* closure fn -> captures *)
  mutable uniq : int list; (* list vars known unique inside the current loop *)
  mutable task_tys : ty list; (* result types of spawned tasks *)
  mutable inrange : (int * int) list; (* (list var, index var): index proven in range *)
  mutable chan_tys : ty list; (* element types of channels *)
}

let is_enum c = function TNamed (n, _) -> Hashtbl.mem c.g.enums n | _ -> false
let is_scalar = function TInt | TFloat | TBool | TUnit -> true | _ -> false

let rec cname c t =
  match t with
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TUnit -> "Unit"
  | TStr -> "Str"
  | TFn (ps, r) ->
    List.iter (fun p -> ignore (cname c p)) ps;
    ignore (cname c r);
    "Clo"
  | TCap _ -> "Cap"
  | TGroup a ->
    ignore (cname c a);
    "Group"
  | TChan a ->
    ignore (cname c a);
    if not (List.mem a c.chan_tys) then c.chan_tys <- c.chan_tys @ [ a ];
    "Chan"
  | TList TInt -> "L_Int"
  | TList TStr -> "L_Str"
  | TList _ | TNamed _ | TArray _ -> (
    let k = show t in
    match Hashtbl.find_opt c.names k with
    | Some n -> n
    | None ->
      let n = sprintf "T%d" (Hashtbl.length c.names) in
      Hashtbl.replace c.names k n;
      (* register components first so they get definitions too *)
      (match t with
       | TList e | TArray (e, _) -> ignore (cname c e)
       | TNamed _ when is_enum c t ->
         List.iter (fun (_, ps) -> List.iter (fun p -> ignore (cname c p)) ps) (Lower.variants c.g t)
       | TNamed _ -> List.iter (fun (_, ft) -> ignore (cname c ft)) (Lower.field_types c.g t)
       | _ -> ());
      c.types <- c.types @ [ t ];
      n)
  | TParam _ | TVar _ | TErr -> failwith ("cgen: non-concrete type " ^ show t)

let ctype c t =
  match t with
  | TInt -> "int64_t"
  | TFloat -> "double"
  | TBool -> "bool"
  | TUnit -> "W_Unit"
  | TStr -> "WStr*"
  | TFn _ -> "WClo*"
  | TCap _ -> "WCap*"
  | TGroup _ -> "WGroup*"
  | TChan _ -> "WChan*"
  | TList _ -> cname c t ^ "*"
  | _ -> cname c t (* structs, enums and arrays are C values *)

(* ---------- literals ---------- *)

let c_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun ch ->
      let code = Char.code ch in
      if code >= 32 && code < 127 && ch <> '"' && ch <> '\\' && ch <> '?' then Buffer.add_char b ch
      else Buffer.add_string b (sprintf "\\%03o" code))
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let lit c s =
  match Hashtbl.find_opt c.lits s with
  | Some n -> n
  | None ->
    let n = sprintf "lit%d" (Hashtbl.length c.lits) in
    Hashtbl.replace c.lits s n;
    c.lit_order <- c.lit_order @ [ (n, s) ];
    n

let where (sp : Span.t) =
  let l, col = Source.line_col sp.file sp.lo in
  c_string (sprintf "%s:%d:%d" sp.file l col)

let var (v : I.var) = sprintf "v%d" v.id

let float_lit x =
  if Float.is_nan x then "NAN"
  else if x = Float.infinity then "INFINITY"
  else if x = Float.neg_infinity then "(-INFINITY)"
  else sprintf "%h" x

let atom c (a : I.atom) =
  match a with
  | I.V v -> var v
  | I.VDup v -> if is_scalar v.ty then var v else sprintf "%s_dup(%s)" (cname c v.ty) (var v)
  | I.AInt n -> sprintf "INT64_C(%d)" n
  | I.AFloat x -> float_lit x
  | I.ABool b -> if b then "true" else "false"
  | I.AUnit -> "0"
  | I.AStr s -> sprintf "(&%s)" (lit c s)

(* ---------- enum helpers ---------- *)

let tag_expr c t e = sprintf "%s_tag(%s)" (cname c t) e
let payload_expr c t e k i = sprintf "%s_p_%d_%d(%s)" (cname c t) k i e
let mk_enum c t k args = sprintf "%s_mk_%d(%s)" (cname c t) k (String.concat ", " args)

let option_of c t = match t with TNamed ("Option", [ a ]) -> a | _ -> failwith "option_of"

(* ---------- expressions ---------- *)

let dup_of c t e = if is_scalar t then e else sprintf "%s_dup(%s)" (cname c t) e

(* Follow a chain of steps from [root]; returns the lvalue and its type. *)
let walk c root rty steps =
  List.fold_left
    (fun (e, t) step ->
      match step with
      | I.PField k ->
        let ft = snd (List.nth (Lower.field_types c.g t) k) in
        (sprintf "(%s).f%d" e k, ft)
      | I.PIndex (i, sp) -> (
        match t with
        | TList el -> (sprintf "(*%s_at(%s, %s, %s))" (cname c t) e (atom c i) (where sp), el)
        | TArray (el, n) -> (sprintf "(%s).a[w_idx(%s, %d, %s)]" e (atom c i) n (where sp), el)
        | _ -> failwith "walk index")
      | I.PIndexU i -> (
        match t with
        | TList el -> (sprintf "(%s)->data[%s]" e (atom c i), el)
        | TArray (el, _) -> (sprintf "(%s).a[%s]" e (atom c i), el)
        | _ -> failwith "walk index"))
    (root, rty) steps

let rec pat_cond c e t (p : Ast.pat) =
  match p.pdesc with
  | Ast.PWild | Ast.PBind _ -> "true"
  | Ast.PInt n -> sprintf "(%s == INT64_C(%d))" e n
  | Ast.PBool b -> sprintf "(%s == %s)" e (if b then "true" else "false")
  | Ast.PStr s -> sprintf "Str_eq(%s, &%s)" e (lit c s)
  | Ast.PVariant (_, vn, args) ->
    let k = Lower.tag_of c.g t vn in
    let ptys = List.assoc vn (Lower.variants c.g t) in
    let subs = List.mapi (fun i (a, pt) -> pat_cond c (payload_expr c t e k i) pt a) (List.combine args ptys) in
    String.concat " && " (sprintf "(%s == %d)" (tag_expr c t e) k :: List.filter (fun s -> s <> "true") subs)

let binop c op t a b sp =
  let open Ast in
  match (t, op) with
  | TInt, Add -> sprintf "(%s + %s)" a b
  | TInt, Sub -> sprintf "(%s - %s)" a b
  | TInt, Mul -> sprintf "(%s * %s)" a b
  | TInt, Div -> sprintf "w_div(%s, %s, %s)" a b (where sp)
  | TInt, Mod -> sprintf "w_mod(%s, %s, %s)" a b (where sp)
  | TFloat, Add -> sprintf "(%s + %s)" a b
  | TFloat, Sub -> sprintf "(%s - %s)" a b
  | TFloat, Mul -> sprintf "(%s * %s)" a b
  | TFloat, Div -> sprintf "(%s / %s)" a b
  | TStr, Add -> sprintf "Str_concat(%s, %s)" a b
  | (TInt | TFloat | TBool), (Lt | Le | Gt | Ge | Eq | Ne) ->
    let o = match op with Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">=" | Eq -> "==" | _ -> "!=" in
    sprintf "(%s %s %s)" a o b
  | TStr, (Lt | Le | Gt | Ge) ->
    let o = match op with Lt -> "<" | Le -> "<=" | Gt -> ">" | _ -> ">=" in
    sprintf "(Str_cmp(%s, %s) %s 0)" a b o
  | _, Eq -> sprintf "%s_eq(%s, %s)" (cname c t) a b
  | _, Ne -> sprintf "(!%s_eq(%s, %s))" (cname c t) a b
  | _ -> failwith ("cgen binop on " ^ show t)

(* Emit statements that store [rhs] into the C lvalue [dst] of type [t]. *)
let rhs_into c (out : Buffer.t) ind dst t (rhs : I.rhs) =
  let line s = Buffer.add_string out (ind ^ s ^ "\n") in
  let set e = line (sprintf "%s = %s;" dst e) in
  let a = atom c in
  let io_result call ok_payload =
    let ioe = TNamed ("IoError", []) in
    line "{";
    line (sprintf "  WIo r_ = %s;" call);
    line
      (sprintf "  if (r_.kind == 0) %s = %s; else %s = %s;" dst
         (mk_enum c t 0 [ ok_payload ])
         dst
         (mk_enum c t 1
            [ sprintf "(r_.kind == 1 ? %s : %s)" (mk_enum c ioe 0 [ "r_.msg" ]) (mk_enum c ioe 1 [ "r_.msg" ]) ]));
    line "}"
  in
  let show_into parts =
    line "{";
    line "  WBuf b_; buf_init(&b_);";
    List.iter
      (function
        | `Lit s -> line (sprintf "  buf_putn(&b_, %s, %d);" (c_string s) (String.length s))
        | `Val (e, ty) -> line (sprintf "  %s_show(&b_, %s, true);" (cname c ty) e)
        | `ValQ (e, ty) -> line (sprintf "  %s_show(&b_, %s, false);" (cname c ty) e))
      parts;
    line (sprintf "  %s = buf_take(&b_);" dst);
    line "}"
  in
  match rhs with
  | I.Atom x -> set (a x)
  | I.Bin (op, ty, x, y, sp) -> set (binop c op ty (a x) (a y) sp)
  | I.Neg (TInt, x) -> set (sprintf "(int64_t)(0 - (uint64_t)%s)" (a x))
  | I.Neg (_, x) -> set (sprintf "(-%s)" (a x))
  | I.Not x -> set (sprintf "(!%s)" (a x))
  | I.Call (f, args) -> set (sprintf "%s(%s)" f (String.concat ", " (List.map a args)))
  | I.CallClo (f, args) ->
    let fv = match f with I.V v | I.VDup v -> v | _ -> failwith "callclo" in
    let ps, r = match fv.ty with TFn (ps, r) -> (ps, r) | _ -> failwith "callclo type" in
    let sigt = sprintf "%s (*)(%s)" (ctype c r) (String.concat ", " ("WClo*" :: List.map (ctype c) ps)) in
    set (sprintf "((%s)(%s)->fn)(%s)" sigt (var fv) (String.concat ", " (var fv :: List.map a args)))
  | I.MkStruct (ty, args) ->
    set (if args = [] then sprintf "(%s){0}" (cname c ty) else sprintf "(%s){ %s }" (cname c ty) (String.concat ", " (List.map a args)))
  | I.MkEnum (ty, k, args) -> set (mk_enum c ty k (List.map a args))
  | I.MkList ((TArray _ as ty), args) ->
    set (if args = [] then sprintf "(%s){0}" (cname c ty) else sprintf "(%s){ { %s } }" (cname c ty) (String.concat ", " (List.map a args)))
  | I.MkList (ty, args) ->
    let n = cname c ty in
    line "{";
    line (sprintf "  %s* l_ = %s_new(%d);" n n (List.length args));
    List.iteri (fun i x -> line (sprintf "  l_->data[%d] = %s;" i (a x))) args;
    line (sprintf "  l_->len = %d;" (List.length args));
    line (sprintf "  %s = l_;" dst);
    line "}"
  | I.MkClosure (fname, _, []) ->
    line "{";
    line "  WClo* k_ = (WClo*)w_alloc(sizeof(WClo));";
    line (sprintf "  k_->rc = 1; k_->fn = (void*)%s; k_->drop = NULL; k_->share = NULL;" fname);
    line (sprintf "  %s = k_;" dst);
    line "}"
  | I.MkClosure (fname, _, args) ->
    line "{";
    line (sprintf "  struct Env_%s* e_ = (struct Env_%s*)w_alloc(sizeof(struct Env_%s));" fname fname fname);
    line (sprintf "  e_->base.rc = 1; e_->base.fn = (void*)%s; e_->base.drop = Env_%s_drop; e_->base.share = Env_%s_share;" fname fname fname);
    List.iteri (fun i x -> line (sprintf "  e_->c%d = %s;" i (a x))) args;
    line (sprintf "  %s = (WClo*)e_;" dst);
    line "}"
  | I.Get (root, rty, steps) ->
    let steps =
      match (root, steps) with
      | (I.V v | I.VDup v), I.PIndex ((I.V i as ia), _) :: rest when List.mem (v.id, i.id) c.inrange -> I.PIndexU ia :: rest
      | _ -> steps
    in
    let e, ft = walk c (a root) rty steps in
    set (dup_of c ft e)
  | I.Payload (x, ety, k, i) ->
    let pt = List.nth (snd (List.nth (Lower.variants c.g ety) k)) i in
    set (dup_of c pt (payload_expr c ety (a x) k i))
  | I.IsTag (x, ety, k) -> set (sprintf "(%s == %d)" (tag_expr c ety (a x)) k)
  | I.PatTest (x, ty, p) -> set (pat_cond c (a x) ty p)
  | I.Interp parts ->
    show_into (List.map (function I.IStr s -> `Lit s | I.IVal (x, ty) -> `Val (a x, ty) | I.IValQ (x, ty) -> `ValQ (a x, ty)) parts)
  | I.RootField (x, f) -> set (sprintf "Cap_root_field(%s, %s)" (a x) (c_string f))
  | I.GroupNew -> set "w_group_new()"
  | I.Spawn (g, task, rt) ->
    if not (List.mem rt c.task_tys) then c.task_tys <- c.task_tys @ [ rt ];
    line (sprintf "w_spawn(%s, %s, wtask_%s, sizeof(%s));" (a g) (a task) (cname c rt) (ctype c rt));
    set "0"
  | I.GroupJoin (g, lty) ->
    let el = match lty with TList e -> e | _ -> TUnit in
    let n = cname c lty in
    line "{";
    line (sprintf "  WGroup* g_ = %s;" (a g));
    line "  w_group_wait(g_);";
    line (sprintf "  %s* l_ = %s_new(g_->n);" n n);
    line (sprintf "  for (int64_t i_ = 0; i_ < g_->n; i_++) memcpy(&l_->data[i_], g_->tasks[i_]->slot, sizeof(%s));" (ctype c el));
    line "  l_->len = g_->n;";
    line "  w_group_release_tasks(g_);";
    line (sprintf "  %s = l_;" dst);
    line "}"
  | I.ChanNew (TChan el, cap, sp) ->
    ignore (cname c (TChan el));
    set (sprintf "w_chan_new(%s, sizeof(%s), %s, %s, %s)" (a cap) (ctype c el)
           (if is_scalar el then "NULL" else "chdrop_" ^ cname c el)
           (if is_scalar el then "NULL" else "chshare_" ^ cname c el) (where sp))
  | I.ChanNew _ -> failwith "channel type"
  | I.ChanSend (ch, v, sp) ->
    let el = match (match ch with I.V x | I.VDup x -> x.ty | _ -> TUnit) with TChan e -> e | _ -> TUnit in
    line "{";
    line (sprintf "  %s v_ = %s;" (ctype c el) (a v));
    line (sprintf "  w_chan_send(%s, &v_, %s);" (a ch) (where sp));
    line "}";
    set "0"
  | I.Panic (m, sp) ->
    line (sprintf "memset(&%s, 0, sizeof %s);" dst dst);
    line (sprintf "w_panic_c((%s)->data, %s);" (a m) (where sp))
  | I.Prim (name, rt, args, sp) -> (
    let args = List.map a args in
    let arg i = List.nth args i in
    let opt_from cond value =
      let el = option_of c t in
      ignore el;
      set (sprintf "(%s) ? %s : %s" cond (mk_enum c t 0 [ value ]) (mk_enum c t 1 []))
    in
    match (name, rt) with
    | ("list_len" | "array_len"), TArray (_, n) -> set (string_of_int n)
    | "array_get", TArray (el, n) ->
      opt_from (sprintf "%s >= 0 && %s < %d" (arg 1) (arg 1) n) (dup_of c el (sprintf "(%s).a[%s]" (arg 0) (arg 1)))
    | "array_contains", TArray (el, n) ->
      set (sprintf "({ bool f_ = false; for (int64_t i_ = 0; i_ < %d; i_++) if (%s) { f_ = true; break; } f_; })" n
             (binop c Ast.Eq el (sprintf "(%s).a[i_]" (arg 0)) (arg 1) Span.dummy))
    | "array_to_list", TArray (el, n) ->
      let ln = cname c (TList el) in
      line "{";
      line (sprintf "  %s* l_ = %s_new(%d);" ln ln n);
      line (sprintf "  for (int64_t i_ = 0; i_ < %d; i_++) l_->data[i_] = %s;" n (dup_of c el (sprintf "(%s).a[i_]" (arg 0))));
      line (sprintf "  l_->len = %d;" n);
      line (sprintf "  %s = l_;" dst);
      line "}"
    | "chan_recv", TChan el ->
      line "{";
      line (sprintf "  %s v_;" (ctype c el));
      line (sprintf "  %s = w_chan_recv(%s, &v_) ? %s : %s;" dst (arg 0) (mk_enum c t 0 [ "v_" ]) (mk_enum c t 1 []));
      line "}"
    | "chan_close", _ -> line (sprintf "w_chan_close(%s, %s);" (arg 0) (where sp)); set "0"
    | "list_len", _ | "str_len", _ -> set (sprintf "(%s)->len" (arg 0))
    | "list_is_empty", _ | "str_is_empty", _ -> set (sprintf "((%s)->len == 0)" (arg 0))
    | "list_get", TList el ->
      opt_from
        (sprintf "%s >= 0 && %s < (%s)->len" (arg 1) (arg 1) (arg 0))
        (dup_of c el (sprintf "(%s)->data[%s]" (arg 0) (arg 1)))
    | "list_first", TList el -> opt_from (sprintf "(%s)->len > 0" (arg 0)) (dup_of c el (sprintf "(%s)->data[0]" (arg 0)))
    | "list_last", TList el ->
      opt_from (sprintf "(%s)->len > 0" (arg 0)) (dup_of c el (sprintf "(%s)->data[(%s)->len - 1]" (arg 0) (arg 0)))
    | "list_contains", _ -> set (sprintf "%s_contains(%s, %s)" (cname c rt) (arg 0) (arg 1))
    | "list_reverse", _ -> set (sprintf "%s_reverse(%s)" (cname c rt) (arg 0))
    | "list_sort", _ -> set (sprintf "%s_sort(%s)" (cname c rt) (arg 0))
    | "list_take", _ -> set (sprintf "%s_slice(%s, 0, %s)" (cname c rt) (arg 0) (arg 1))
    | "list_skip", _ -> set (sprintf "%s_slice(%s, %s, (%s)->len)" (cname c rt) (arg 0) (arg 1) (arg 0))
    | "list_join", _ -> set (sprintf "L_Str_join(%s, %s)" (arg 0) (arg 1))
    | "list_sum", _ -> set (sprintf "L_Int_sum(%s)" (arg 0))
    | "str_lines", _ -> set (sprintf "Str_lines(%s)" (arg 0))
    | "str_split", _ -> set (sprintf "Str_split(%s, %s)" (arg 0) (arg 1))
    | "str_trim", _ -> set (sprintf "Str_trim(%s)" (arg 0))
    | "str_contains", _ -> set (sprintf "(Str_find(%s, %s, 0) >= 0)" (arg 0) (arg 1))
    | "str_starts_with", _ -> set (sprintf "Str_starts(%s, %s)" (arg 0) (arg 1))
    | "str_ends_with", _ -> set (sprintf "Str_ends(%s, %s)" (arg 0) (arg 1))
    | "str_to_upper", _ -> set (sprintf "Str_map_case(%s, true)" (arg 0))
    | "str_to_lower", _ -> set (sprintf "Str_map_case(%s, false)" (arg 0))
    | "str_replace", _ -> set (sprintf "Str_replace(%s, %s, %s)" (arg 0) (arg 1) (arg 2))
    | "str_parse_int", _ ->
      line "{";
      line "  int64_t n_;";
      line (sprintf "  %s = Str_parse_int(%s, &n_) ? %s : %s;" dst (arg 0) (mk_enum c t 0 [ "n_" ]) (mk_enum c t 1 []));
      line "}"
    | "str_parse_float", _ ->
      line "{";
      line "  double x_;";
      line (sprintf "  %s = Str_parse_float(%s, &x_) ? %s : %s;" dst (arg 0) (mk_enum c t 0 [ "x_" ]) (mk_enum c t 1 []));
      line "}"
    | "str_slice", _ -> set (sprintf "Str_slice(%s, %s, %s)" (arg 0) (arg 1) (arg 2))
    | "str_index_of", _ ->
      line "{";
      line (sprintf "  int64_t i_ = Str_find(%s, %s, 0);" (arg 0) (arg 1));
      line (sprintf "  %s = i_ >= 0 ? %s : %s;" dst (mk_enum c t 0 [ "i_" ]) (mk_enum c t 1 []));
      line "}"
    | "str_bytes", _ -> set (sprintf "Str_bytes(%s)" (arg 0))
    | "int_to_char", _ -> set (sprintf "Int_to_char(%s)" (arg 0))
    | "int_min", _ -> set (sprintf "(%s < %s ? %s : %s)" (arg 0) (arg 1) (arg 0) (arg 1))
    | "int_max", _ -> set (sprintf "(%s > %s ? %s : %s)" (arg 0) (arg 1) (arg 0) (arg 1))
    | ("int_to_str" | "float_to_str" | "bool_to_str"), _ -> show_into [ `Val (arg 0, rt) ]
    | "int_to_float", _ -> set (sprintf "(double)%s" (arg 0))
    | "int_abs", _ -> set (sprintf "w_abs(%s)" (arg 0))
    | "float_to_int", _ -> set (sprintf "w_f2i(%s)" (arg 0))
    | "float_abs", _ -> set (sprintf "fabs(%s)" (arg 0))
    | "float_sqrt", _ -> set (sprintf "sqrt(%s)" (arg 0))
    | "cap_print", TCap Stdout -> line (sprintf "Cap_print(%s, true);" (arg 1)); set "0"
    | "cap_write", TCap Stdout -> line (sprintf "Cap_print(%s, false);" (arg 1)); set "0"
    | "cap_now_ms", _ -> set "Cap_now_ms()"
    | "cap_only", _ -> set (sprintf "Cap_net_only(%s, %s)" (arg 0) (arg 1))
    | "cap_get", _ -> io_result (sprintf "Cap_net_get(%s, %s)" (arg 0) (arg 1)) "r_.val"
    | "cap_under", _ -> set (sprintf "Cap_fs_under(%s, %s)" (arg 0) (arg 1))
    | "cap_read_only", _ -> set (sprintf "Cap_fs_read_only(%s)" (arg 0))
    | "cap_read", TCap Conn -> io_result (sprintf "Cap_conn_read(%s)" (arg 0)) "r_.val"
    | "cap_write", TCap Conn -> io_result (sprintf "Cap_conn_write(%s, %s)" (arg 0) (arg 1)) "0"
    | "cap_on", _ -> io_result (sprintf "Cap_listen_on(%s, %s)" (arg 0) (arg 1)) "(WCap*)r_.ptr"
    | "cap_accept", _ -> io_result (sprintf "Cap_accept(%s)" (arg 0)) "(WCap*)r_.ptr"
    | "cap_connect", _ -> io_result (sprintf "Cap_net_connect(%s, %s)" (arg 0) (arg 1)) "(WCap*)r_.ptr"
    | "cap_port", _ -> set (sprintf "(int64_t)(%s)->port" (arg 0))
    | "cap_close", _ -> line (sprintf "Cap_sock_close(%s);" (arg 0)); set "0"
    | "cap_read", _ -> io_result (sprintf "Cap_fs_read(%s, %s)" (arg 0) (arg 1)) "r_.val"
    | "cap_write", TCap Fs -> io_result (sprintf "Cap_fs_write(%s, %s, %s)" (arg 0) (arg 1) (arg 2)) "0"
    | "cap_exists", _ -> set (sprintf "Cap_fs_exists(%s, %s)" (arg 0) (arg 1))
    | _ ->
      ignore sp;
      failwith (sprintf "cgen: unknown primitive %s on %s" name (show rt)))

(* ---------- statements ---------- *)

let rec stmts c out ind (ss : I.stmt list) = List.iter (stmt c out ind) ss

and stmt c out ind (s : I.stmt) =
  let line s = Buffer.add_string out (ind ^ s ^ "\n") in
  match s with
  | I.Let (v, r) ->
    line (sprintf "%s %s;" (ctype c v.ty) (var v));
    rhs_into c out ind (var v) v.ty r
  | I.Decl v -> line (sprintf "%s %s;" (ctype c v.ty) (var v))
  | I.Init (v, r) -> rhs_into c out ind (var v) v.ty r
  | I.Set (v, r, flag) ->
    line "{";
    line (sprintf "  %s n_;" (ctype c v.ty));
    rhs_into c out (ind ^ "  ") "n_" v.ty r;
    if !flag && not (is_scalar v.ty) then line (sprintf "  %s_drop(%s);" (cname c v.ty) (var v));
    line (sprintf "  %s = n_;" (var v));
    line "}"
  | I.Mutate (v, steps, m) ->
    line "{";
    line (sprintf "  %s* p0_ = &%s;" (ctype c v.ty) (var v));
    let p, t =
      List.fold_left
        (fun (p, t) (i, step) ->
          let q = sprintf "p%d_" (i + 1) in
          match step with
          | I.PField k ->
            let ft = snd (List.nth (Lower.field_types c.g t) k) in
            line (sprintf "  %s* %s = &(%s->f%d);" (ctype c ft) q p k);
            (q, ft)
          | I.PIndex (ia, sp) -> (
            match t with
            | TList el ->
              if not (i = 0 && List.mem v.id c.uniq) then line (sprintf "  %s_unique(%s);" (cname c t) p);
              let proven = i = 0 && (match ia with I.V iv -> List.mem (v.id, iv.id) c.inrange | _ -> false) in
              if proven then line (sprintf "  %s* %s = &(*%s)->data[%s];" (ctype c el) q p (atom c ia))
              else line (sprintf "  %s* %s = %s_at(*%s, %s, %s);" (ctype c el) q (cname c t) p (atom c ia) (where sp));
              (q, el)
            | TArray (el, n) ->
              line (sprintf "  %s* %s = &(%s->a[w_idx(%s, %d, %s)]);" (ctype c el) q p (atom c ia) n (where sp));
              (q, el)
            | _ -> failwith "mutate index")
          | I.PIndexU _ -> failwith "mutate through unchecked index")
        ("p0_", v.ty)
        (List.mapi (fun i s -> (i, s)) steps)
    in
    (match m with
     | I.MSet x ->
       if not (is_scalar t) then line (sprintf "  %s_drop(*%s);" (cname c t) p);
       line (sprintf "  *%s = %s;" p (atom c x))
     | I.MPush x ->
       let unchecked = steps = [] && List.mem v.id c.uniq in
       line (sprintf "  %s_push%s(%s, %s);" (cname c t) (if unchecked then "_u" else "") p (atom c x)));
    line "}"
  | I.If (cond, a, b) ->
    line (sprintf "if (%s) {" (atom c cond));
    stmts c out (ind ^ "  ") a;
    if b <> [] then begin
      line "} else {";
      stmts c out (ind ^ "  ") b
    end;
    line "}"
  | I.Loop body ->
    (* A list that the loop mutates but never shares (no retain, no move,
       no reassignment) stays unique: make it unique once, here, and skip the
       copy-on-write check on every write inside the loop. *)
    let hoisted = List.filter (fun (v : I.var) -> not (List.mem v.id c.uniq) && not (Hoist.escapes v body)) (Hoist.mutated_lists body) in
    List.iter (fun (v : I.var) -> line (sprintf "%s_unique(&%s);" (cname c v.ty) (var v))) hoisted;
    let saved = c.uniq in
    c.uniq <- List.map (fun (v : I.var) -> v.id) hoisted @ c.uniq;
    (match Hoist.fill_loop body with
     | Some (i, inclusive, n, v, x) ->
       (* Fill in one go: reserve once, then a plain store loop the C compiler vectorises. *)
       let ln = cname c v.ty in
       line "{";
       line (sprintf "  int64_t cnt_;");
       line (sprintf "  if (!__builtin_sub_overflow(%s, %s, &cnt_) && cnt_ < INT64_MAX - 1) {" (atom c n) (var i));
       line (sprintf "    cnt_ += %d;" (if inclusive then 1 else 0));
       line "    int64_t want_;";
       line (sprintf "    if (cnt_ > 0 && __builtin_add_overflow(%s->len, cnt_, &want_)) w_oom();" (var v));
       line "    if (cnt_ > 0) {";
       line (sprintf "      %s_reserve(&%s, want_);" ln (var v));
       line (sprintf "      %s* d_ = %s->data + %s->len;" (ctype c (match v.ty with TList e -> e | _ -> TUnit)) (var v) (var v));
       line (sprintf "      for (int64_t k_ = 0; k_ < cnt_; k_++) d_[k_] = %s;" (atom c x));
       line (sprintf "      %s->len += cnt_; %s += cnt_;" (var v) (var i));
       line "    }";
       line "  } else {";
       line "  for (;;) {";
       stmts c out (ind ^ "    ") body;
       line "  }";
       line "  }";
       line "}"
     | None -> (
       match Hoist.versionable body with
       | Some (m, inclusive, n, step, lists) ->
         (* Loop versioning: one range check up front, no bounds checks inside. *)
         let conds =
           [ sprintf "%s >= 0" (var m); sprintf "%s >= 0" (atom c step); sprintf "%s <= INT64_MAX - %s" (atom c n) (atom c step) ]
           @ List.map (fun (v : I.var) -> sprintf "%s %s %s->len" (atom c n) (if inclusive then "<" else "<=") (var v)) lists
         in
         line (sprintf "if (%s) {" (String.concat " && " conds));
         let saved_r = c.inrange in
         c.inrange <- List.map (fun (v : I.var) -> (v.id, m.id)) lists @ c.inrange;
         line "  for (;;) {";
         stmts c out (ind ^ "    ") body;
         line "  }";
         c.inrange <- saved_r;
         line "} else {";
         line "  for (;;) {";
         stmts c out (ind ^ "    ") body;
         line "  }";
         line "}"
       | None ->
         line "for (;;) {";
         stmts c out (ind ^ "  ") body;
         line "}"));
    c.uniq <- saved
  | I.Break -> line "break;"
  | I.Continue -> line "continue;"
  | I.Return x -> line (sprintf "return %s;" (atom c x))
  | I.Drop v -> if not (is_scalar v.ty) then line (sprintf "%s_drop(%s);" (cname c v.ty) (var v))

let signature c (f : I.fn) =
  let ps = List.map (fun (v : I.var) -> sprintf "%s %s" (ctype c v.ty) (var v)) f.params in
  let ps = if f.is_closure then "WClo* self_" :: ps else ps in
  sprintf "static %s %s(%s)" (ctype c f.ret) f.fname (if ps = [] then "void" else String.concat ", " ps)

let fn_body c out (f : I.fn) =
  Buffer.add_string out (signature c f ^ " {\n");
  if f.captures <> [] then begin
    Buffer.add_string out (sprintf "  struct Env_%s* env_ = (struct Env_%s*)self_;\n" f.fname f.fname);
    List.iteri
      (fun i (v : I.var) ->
        Buffer.add_string out (sprintf "  %s %s = %s;\n" (ctype c v.ty) (var v) (dup_of c v.ty (sprintf "env_->c%d" i))))
      f.captures
  end;
  stmts c out "  " f.body;
  Buffer.add_string out "  w_unreachable();\n}\n\n"

(* ---------- type definitions ---------- *)

(* Named types a type contains by value (lists, strings, closures are pointers). *)
let rec byval c t =
  match t with
  | TNamed _ | TArray _ -> [ t ]
  | _ -> []

and contents c t =
  match t with
  | TNamed _ when is_enum c t -> List.concat_map (fun (_, ps) -> List.concat_map (byval c) ps) (Lower.variants c.g t)
  | TNamed _ -> List.concat_map (fun (_, ft) -> byval c ft) (Lower.field_types c.g t)
  | TArray (e, _) -> byval c e
  | _ -> []

let compute_boxed c =
  let named = List.filter (function TNamed _ -> true | _ -> false) c.types in
  let reaches_self t =
    let seen = Hashtbl.create 8 in
    let rec go u =
      List.exists
        (fun w ->
          if show w = show t then true
          else if Hashtbl.mem seen (show w) then false
          else begin
            Hashtbl.replace seen (show w) ();
            go w
          end)
        (contents c u)
    in
    go t
  in
  c.boxed <- List.filter_map (fun t -> if is_enum c t && reaches_self t then Some (cname c t) else None) named

let is_boxed c t = List.mem (cname c t) c.boxed

(* Order named types so every by-value dependency is defined first. *)
let topo c =
  let named = List.filter (function TNamed _ | TArray _ -> true | _ -> false) c.types in
  let done_ = Hashtbl.create 16 and visiting = Hashtbl.create 16 in
  let order = ref [] in
  let rec visit t =
    let k = show t in
    if not (Hashtbl.mem done_ k) then begin
      if Hashtbl.mem visiting k then failwith ("recursive type needs an enum to break the cycle: " ^ k);
      Hashtbl.replace visiting k ();
      List.iter (fun d -> if not (is_boxed c d) then visit d) (contents c t);
      Hashtbl.replace done_ k ();
      order := t :: !order
    end
  in
  List.iter visit named;
  List.rev !order

let named_decl c t =
  match t with TNamed (n, _) -> n | _ -> ""

let type_defs c (out : Buffer.t) =
  let p s = Buffer.add_string out (s ^ "\n") in
  compute_boxed c;
  let lists = List.filter (function TList _ -> true | _ -> false) c.types in
  let named = topo c in
  (* forward typedefs *)
  List.iter
    (fun t ->
      let n = cname c t in
      match t with
      | TList _ -> p (sprintf "typedef struct %s %s;" n n)
      | _ -> if is_boxed c t then p (sprintf "typedef struct %s_box* %s;" n n) else p (sprintf "typedef struct %s %s;" n n))
    (lists @ named);
  List.iter (fun t -> match t with TList e -> p (sprintf "WRIT_LIST_STRUCT(%s, %s)" (cname c t) (ctype c e)) | _ -> ()) lists;
  (* struct bodies *)
  List.iter
    (fun t ->
      let n = cname c t in
      match t with
      | TArray (e, k) -> p (sprintf "struct %s { %s a[%d]; };" n (ctype c e) (max k 1))
      | _ ->
      if is_enum c t then begin
        let vs = Lower.variants c.g t in
        let has_payload = List.exists (fun (_, ps) -> ps <> []) vs in
        p (sprintf "struct %s%s {%s int32_t tag;" n (if is_boxed c t then "_box" else "") (if is_boxed c t then " int64_t rc;" else ""));
        if has_payload then begin
          p "  union {";
          List.iteri
            (fun k (_, ps) ->
              if ps <> [] then
                p
                  (sprintf "    struct { %s } v%d;"
                     (String.concat " " (List.mapi (fun i pt -> sprintf "%s f%d;" (ctype c pt) i) ps))
                     k))
            vs;
          p "  } u;"
        end;
        p "};"
      end
      else begin
        let fs = Lower.field_types c.g t in
        p (sprintf "struct %s { %s };" n
             (if fs = [] then "char empty_;" else String.concat " " (List.mapi (fun i (_, ft) -> sprintf "%s f%d;" (ctype c ft) i) fs)))
      end)
    named;
  (* prototypes *)
  List.iter
    (fun t ->
      let n = cname c t and ct = ctype c t in
      match t with
      | TList _ -> ()
      | _ ->
        p (sprintf "static %s %s_dup(%s v);" ct n ct);
        p (sprintf "static void %s_drop(%s v);" n ct);
        p (sprintf "static bool %s_eq(%s a, %s b);" n ct ct);
        p (sprintf "static void %s_show(WBuf* b, %s v, bool top);" n ct);
        p (sprintf "static void %s_share(%s v);" n ct))
    named;
  (* enum constructors and accessors *)
  List.iter
    (fun t ->
      if is_enum c t then begin
        let n = cname c t in
        let boxed = is_boxed c t in
        let acc = if boxed then "->" else "." in
        p (sprintf "#define %s_tag(v) ((v)%stag)" n acc);
        List.iteri
          (fun k (_, ps) ->
            List.iteri (fun i _ -> p (sprintf "#define %s_p_%d_%d(v) ((v)%su.v%d.f%d)" n k i acc k i)) ps;
            let params = List.mapi (fun i pt -> sprintf "%s a%d" (ctype c pt) i) ps in
            p (sprintf "static inline %s %s_mk_%d(%s) {" n n k (if params = [] then "void" else String.concat ", " params));
            if boxed then p (sprintf "  %s r = (%s)w_alloc(sizeof(struct %s_box)); r->rc = 1;" n n n)
            else p (sprintf "  %s r;" n);
            p (sprintf "  r%stag = %d;" acc k);
            List.iteri (fun i _ -> p (sprintf "  r%su.v%d.f%d = a%d;" acc k i i)) ps;
            p "  return r;";
            p "}")
          (Lower.variants c.g t)
      end)
    named;
  (* list functions, inner lists first *)
  let rec depth = function TList e -> 1 + depth e | _ -> 0 in
  List.iter
    (fun t ->
      match t with
      | TList e ->
        p (sprintf "WRIT_LIST_FUNCS(%s, %s, %s)" (cname c t) (ctype c e) (cname c e));
        if e = TFloat then p (sprintf "WRIT_LIST_SORT(%s, double, Float)" (cname c t))
      | _ -> ())
    (List.stable_sort (fun a b -> compare (depth a) (depth b)) lists);
  (* dup/drop/eq/show definitions *)
  List.iter
    (fun t ->
      let n = cname c t and ct = ctype c t in
      let boxed = is_boxed c t in
      let acc = if boxed then "->" else "." in
      let name = named_decl c t in
      match t with
      | TArray (e, k) ->
        let en = cname c e in
        let each f = if not (is_scalar e) then p (sprintf "  for (int64_t i = 0; i < %d; i++) %s;" k f) in
        p (sprintf "static %s %s_dup(%s v) {" ct n ct);
        each (sprintf "v.a[i] = %s_dup(v.a[i])" en);
        p "  return v;\n}";
        p (sprintf "static void %s_drop(%s v) {" n ct);
        p "  (void)v;";
        each (sprintf "%s_drop(v.a[i])" en);
        p "}";
        p (sprintf "static void %s_share(%s v) {" n ct);
        p "  (void)v;";
        each (sprintf "%s_share(v.a[i])" en);
        p "}";
        p (sprintf "static bool %s_eq(%s a, %s b) {" n ct ct);
        p (sprintf "  for (int64_t i = 0; i < %d; i++) if (!(%s)) return false;" k (binop c Ast.Eq e "a.a[i]" "b.a[i]" Span.dummy));
        p "  (void)a; (void)b; return true;\n}";
        p (sprintf "static void %s_show(WBuf* b, %s v, bool top) {" n ct);
        p "  (void)top; buf_puts(b, \"[\");";
        p (sprintf "  for (int64_t i = 0; i < %d; i++) { if (i) buf_puts(b, \", \"); %s_show(b, v.a[i], false); }" k en);
        p "  buf_puts(b, \"]\");\n}"
      | _ ->
      if is_enum c t then begin
        let vs = Lower.variants c.g t in
        let cases f =
          List.iteri
            (fun k (vn, ps) ->
              p (sprintf "    case %d: {" k);
              List.iteri (fun i pt -> f vn k i pt) ps;
              p "      break; }")
            vs
        in
        p (sprintf "static %s %s_dup(%s v) {" ct n ct);
        if boxed then p "  w_rc_inc(&v->rc);"
        else begin
          p "  switch (v.tag) {";
          cases (fun _ k i pt -> if not (is_scalar pt) then p (sprintf "      v.u.v%d.f%d = %s_dup(v.u.v%d.f%d);" k i (cname c pt) k i));
          p "  }"
        end;
        p "  return v;\n}";
        p (sprintf "static void %s_drop(%s v) {" n ct);
        if boxed then p "  if (!w_rc_dec(&v->rc)) return;";
        p (sprintf "  switch (v%stag) {" acc);
        cases (fun _ k i pt -> if not (is_scalar pt) then p (sprintf "      %s_drop(v%su.v%d.f%d);" (cname c pt) acc k i));
        p "  }";
        if boxed then p "  w_free(v);";
        p "}";
        p (sprintf "static void %s_share(%s v) {" n ct);
        if boxed then p "  if (!w_rc_share(&v->rc)) return;";
        p (sprintf "  switch (v%stag) {" acc);
        cases (fun _ k i pt -> if not (is_scalar pt) then p (sprintf "      %s_share(v%su.v%d.f%d);" (cname c pt) acc k i));
        p "  }";
        p "}";
        p (sprintf "static bool %s_eq(%s a, %s b) {" n ct ct);
        p (sprintf "  if (a%stag != b%stag) return false;" acc acc);
        p (sprintf "  switch (a%stag) {" acc);
        cases (fun _ k i pt ->
            p (sprintf "      if (!(%s)) return false;" (binop c Ast.Eq pt (sprintf "a%su.v%d.f%d" acc k i) (sprintf "b%su.v%d.f%d" acc k i) Span.dummy)));
        p "  }";
        p "  return true;\n}";
        p (sprintf "static void %s_show(WBuf* b, %s v, bool top) {" n ct);
        p "  (void)top;";
        p (sprintf "  switch (v%stag) {" acc);
        List.iteri
          (fun k (vn, ps) ->
            p (sprintf "    case %d:" k);
            p (sprintf "      buf_puts(b, %s);" (c_string (if ps = [] then vn else vn ^ "(")));
            List.iteri
              (fun i pt ->
                if i > 0 then p "      buf_puts(b, \", \");";
                p (sprintf "      %s_show(b, v%su.v%d.f%d, false);" (cname c pt) acc k i))
              ps;
            if ps <> [] then p "      buf_puts(b, \")\");";
            p "      break;")
          vs;
        p "  }\n}"
      end
      else begin
        let fs = Lower.field_types c.g t in
        p (sprintf "static %s %s_dup(%s v) {" ct n ct);
        List.iteri (fun i (_, ft) -> if not (is_scalar ft) then p (sprintf "  v.f%d = %s_dup(v.f%d);" i (cname c ft) i)) fs;
        p "  return v;\n}";
        p (sprintf "static void %s_drop(%s v) {" n ct);
        p "  (void)v;";
        List.iteri (fun i (_, ft) -> if not (is_scalar ft) then p (sprintf "  %s_drop(v.f%d);" (cname c ft) i)) fs;
        p "}";
        p (sprintf "static void %s_share(%s v) {" n ct);
        p "  (void)v;";
        List.iteri (fun i (_, ft) -> if not (is_scalar ft) then p (sprintf "  %s_share(v.f%d);" (cname c ft) i)) fs;
        p "}";
        p (sprintf "static bool %s_eq(%s a, %s b) {" n ct ct);
        p "  (void)a; (void)b;";
        List.iteri
          (fun i (_, ft) ->
            p (sprintf "  if (!(%s)) return false;" (binop c Ast.Eq ft (sprintf "a.f%d" i) (sprintf "b.f%d" i) Span.dummy)))
          fs;
        p "  return true;\n}";
        p (sprintf "static void %s_show(WBuf* b, %s v, bool top) {" n ct);
        p "  (void)top; (void)v;";
        p (sprintf "  buf_puts(b, %s);" (c_string (name ^ " { ")));
        List.iteri
          (fun i (fname, ft) ->
            p (sprintf "  buf_puts(b, %s);" (c_string ((if i > 0 then ", " else "") ^ fname ^ ": ")));
            p (sprintf "  %s_show(b, v.f%d, false);" (cname c ft) i))
          fs;
        p "  buf_puts(b, \" }\");\n}"
      end)
    named

(* ---------- program ---------- *)

let program (g : Lower.gctx) (prog : I.program) : string =
  let c =
    { g; names = Hashtbl.create 32; types = []; lits = Hashtbl.create 32; lit_order = []; boxed = [];
      envs = Hashtbl.create 16; uniq = []; task_tys = []; inrange = []; chan_tys = [] }
  in
  (* bodies first: this discovers every type and literal *)
  let bodies = Buffer.create 65536 in
  List.iter (fun f -> fn_body c bodies f) prog.fns;
  let protos = Buffer.create 4096 in
  List.iter (fun (f : I.fn) -> Buffer.add_string protos (signature c f ^ ";\n")) prog.fns;
  let envs = Buffer.create 4096 in
  List.iter
    (fun (f : I.fn) ->
      if f.captures <> [] then begin
        Buffer.add_string envs
          (sprintf "struct Env_%s { WClo base; %s };\n" f.fname
             (String.concat " " (List.mapi (fun i (v : I.var) -> sprintf "%s c%d;" (ctype c v.ty) i) f.captures)));
        Buffer.add_string envs (sprintf "static void Env_%s_drop(WClo* k) {\n  struct Env_%s* e = (struct Env_%s*)k;\n  (void)e;\n" f.fname f.fname f.fname);
        List.iteri
          (fun i (v : I.var) -> if not (is_scalar v.ty) then Buffer.add_string envs (sprintf "  %s_drop(e->c%d);\n" (cname c v.ty) i))
          f.captures;
        Buffer.add_string envs "}\n";
        Buffer.add_string envs (sprintf "static void Env_%s_share(WClo* k) {\n  struct Env_%s* e = (struct Env_%s*)k;\n  (void)e;\n" f.fname f.fname f.fname);
        List.iteri
          (fun i (v : I.var) -> if not (is_scalar v.ty) then Buffer.add_string envs (sprintf "  %s_share(e->c%d);\n" (cname c v.ty) i))
          f.captures;
        Buffer.add_string envs "}\n"
      end)
    prog.fns;
  let tasks = Buffer.create 1024 in
  List.iter
    (fun t ->
      if not (is_scalar t) then
        Buffer.add_string tasks
          (sprintf "static void chdrop_%s(void* p) { %s_drop(*(%s*)p); }\nstatic void chshare_%s(void* p) { %s_share(*(%s*)p); }\n"
             (cname c t) (cname c t) (ctype c t) (cname c t) (cname c t) (ctype c t)))
    c.chan_tys;
  List.iter
    (fun t ->
      Buffer.add_string tasks
        (sprintf "static void wtask_%s(WTask* t) {\n  %s r = ((%s (*)(WClo*))t->fn->fn)(t->fn);\n  memcpy(t->slot, &r, sizeof r);\n  Clo_drop(t->fn);\n}\n"
           (cname c t) (ctype c t) (ctype c t)))
    c.task_tys;
  let main = Buffer.create 1024 in
  let mp s = Buffer.add_string main (s ^ "\n") in
  let rt = prog.main_ret in
  if prog.tests <> [] then begin
    (* test binary: `PROGRAM N` runs test N (each test in its own process) *)
    mp "static int wtest_index_;";
    mp "static void wtest_entry_(void* a) {";
    mp "  (void)a;";
    mp "  switch (wtest_index_) {";
    List.iteri (fun i t -> mp (sprintf "    case %d: %s(); break;" i t)) prog.tests;
    mp "  }";
    mp "  w_main_finished();";
    mp "}";
    mp "int main(int argc, char** argv) {";
    mp "  if (argc < 2) return 2;";
    mp "  wtest_index_ = atoi(argv[1]);";
    mp "  w_run(wtest_entry_, NULL);";
    mp "  fflush(stdout);";
    mp "  w_leakcheck();";
    mp "  return 0;";
    mp "}"
  end
  else begin
  (* main runs as the main fiber, so it can wait on groups, channels and sockets *)
  mp (sprintf "static %s wmain_result_;" (ctype c rt));
  mp (sprintf "static void wmain_entry_(void* a) { (void)a; wmain_result_ = %s(%s); w_main_finished(); }" prog.main
        (if prog.main_takes_root then "w_root()" else ""));
  mp "int main(int argc, char** argv) {";
  mp "  w_init(argc, argv);";
  mp "  w_run(wmain_entry_, NULL);";
  mp (sprintf "  %s r_ = wmain_result_;" (ctype c rt));
  (match rt with
   | TNamed ("Result", [ _; et ]) ->
     mp (sprintf "  if (%s == 1) {" (tag_expr c rt "r_"));
     mp "    WBuf b_; buf_init(&b_); buf_puts(&b_, \"error: \");";
     mp (sprintf "    %s_show(&b_, %s, true);" (cname c et) (payload_expr c rt "r_" 1 0));
     mp "    buf_puts(&b_, \"\\n\"); fflush(stdout); fwrite(b_.p, 1, (size_t)b_.len, stderr); return 1;";
     mp "  }"
   | _ -> ());
  if not (is_scalar rt) then mp (sprintf "  %s_drop(r_);" (cname c rt)) else mp "  (void)r_;";
  mp "  fflush(stdout);";
  mp "  w_leakcheck();";
  mp "  return 0;";
  mp "}"
  end;
  let types = Buffer.create 16384 in
  type_defs c types;
  let lits = Buffer.create 4096 in
  List.iter
    (fun (n, s) -> Buffer.add_string lits (sprintf "static WStr %s = { 0, %d, (char*)%s };\n" n (String.length s) (c_string s)))
    c.lit_order;
  String.concat "\n"
    [ Runtime_text.text; "/* ---- types ---- */"; Buffer.contents types; "/* ---- literals ---- */";
      Buffer.contents lits; "/* ---- closures ---- */"; Buffer.contents envs; Buffer.contents tasks; "/* ---- functions ---- */";
      Buffer.contents protos; Buffer.contents bodies; Buffer.contents main ]
