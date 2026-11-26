(* IR -> C. Value types (Int, Float, Bool, (), structs, non-recursive enums)
   are C values; Str, List, closures, capabilities and recursive enums are
   reference-counted pointers. Every composite type gets generated
   dup/drop/eq/show functions named <CName>_dup and so on. *)
open Types
module I = Ir

let sprintf = Printf.sprintf

(* Set when the program calls something that may need TLS (Net.get,
   Net.connect_tls, Listen.on_tls). The output then starts with
   [tls_marker], and Native links OpenSSL and pastes in the TLS core. *)
let uses_tls = ref false
let tls_marker = "/* limen: uses tls */\n"

(* Whether [text] contains [word]. *)
let contains text word =
  let n = String.length text and m = String.length word in
  let rec at i = i + m <= n && (String.sub text i m = word || at (i + 1)) in
  at 0

(* Follows the runtime in a program where no task can be cancelled. *)
let no_checkpoints = "/* no group here has a deadline, so no task is ever cancelled */\n#undef W_CHECK\n#define W_CHECK() do { } while (0)"

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
  mutable dyns : (string * ty * (string * ty list * ty) list) list; (* vtables: C name, boxed type, methods *)
  mutable cell_tys : ty list; (* types of owned locals, which need a drop-through-pointer function *)
  mutable cmp_tys : ty list; (* composite types that need a <CName>_cmp order function *)
  mutable sort_tys : ty list; (* list types that need <CName>_sort *)
  mutable hash_tys : ty list; (* composite map key types that need <CName>_hash *)
  mutable csyms : string list; (* C functions that foreign calls name *)
  mutable sql_api : bool; (* limen_sql_run or limen_sql_open is called: emit the table of SQLite functions they use *)
  mutable cstructs : (string * string * (string * Ffi.cty) list) list;
      (* C structs passed by value: Limen name, C name, fields *)
  mutable cbs : ((string * bool * Ffi.cty list * Ffi.cty) * (string * string * Span.t)) list;
  mutable kepts : Ffi.cty list list; (* the C signatures of kept callbacks, each with its trampolines *)
      (* callback wrappers: (Limen function's C name, whether it takes a
         Foreign first, parameters, result) -> (wrapper name, Limen name,
         where the function is declared) *)
  mutable bjobs : string list; (* blocking calls' jobs, vbj<n>_: each one's struct and the function that makes the call *)
  mutable vols : int list;
      (* variables of the current function a `defer` needs volatile: a panic
         longjmps back into the function, and the deferred body (and the
         cleanups after it) must see their latest values and owned flags *)
  mutable pure : (string, Pure.info) Hashtbl.t; (* the program's pure functions *)
  mutable plan : Pure.plan; (* pure calls the current function keeps across a loop *)
  mutable slotted : I.stmt list list; (* loops whose slots are being emitted *)
}

let is_enum c = function TNamed (n, _) -> Hashtbl.mem c.g.enums n | _ -> false
let is_scalar = function TInt | TFloat | TBool | TUnit | TUInt _ -> true | _ -> false

let rec cname c t =
  match t with
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TUnit -> "Unit"
  | TStr -> "Str"
  | TBytes -> "Bytes"
  | TUInt n -> "UInt" ^ string_of_int n
  | TFn (ps, r) ->
    List.iter (fun p -> ignore (cname c p)) ps;
    ignore (cname c r);
    "Clo"
  | TCap _ -> "Cap"
  | TDyn _ -> "Dyn"
  | TGroup a ->
    ignore (cname c a);
    "Group"
  | TChan a ->
    ignore (cname c a);
    if not (List.mem a c.chan_tys) then c.chan_tys <- c.chan_tys @ [ a ];
    "Chan"
  | TAtomic -> "Atomic"
  | TShared a ->
    (* its value's drop and share functions are a channel element's *)
    ignore (cname c a);
    if not (List.mem a c.chan_tys) then c.chan_tys <- c.chan_tys @ [ a ];
    "Shared"
  | TList TInt -> "L_Int"
  | TList TStr -> "L_Str"
  | TList _ | TNamed _ | TArray _ | TMap _ -> (
    let k = show t in
    match Hashtbl.find_opt c.names k with
    | Some n -> n
    | None ->
      let n = sprintf "T%d" (Hashtbl.length c.names) in
      Hashtbl.replace c.names k n;
      (* register components first so they get definitions too *)
      (match t with
       | TList e | TArray (e, _) -> ignore (cname c e)
       | TMap (k, v) -> ignore (cname c k); ignore (cname c v)
       | TNamed _ when is_enum c t ->
         List.iter (fun (_, ps) -> List.iter (fun p -> ignore (cname c p)) ps) (Lower.variants c.g t)
       | TNamed _ -> List.iter (fun (_, ft) -> ignore (cname c ft)) (Lower.field_types c.g t)
       | _ -> ());
      c.types <- c.types @ [ t ];
      n)
  | TParam _ | TVar _ | TErr | TLen _ -> failwith ("cgen: non-concrete type " ^ show t)

let ctype c t =
  match t with
  | TInt -> "int64_t"
  | TFloat -> "double"
  | TBool -> "bool"
  | TUnit -> "W_Unit"
  | TStr | TBytes -> "WStr*"
  | TUInt n -> sprintf "uint%d_t" n
  | TFn _ -> "WClo*"
  | TDyn _ -> "WDyn*"
  | TCap _ -> "WCap*"
  | TGroup _ -> "WGroup*"
  | TChan _ -> "WChan*"
  | TAtomic -> "WAtomic*"
  | TShared _ -> "WShared*"
  | TList _ | TMap _ -> cname c t ^ "*"
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

(* A variable's C name: its Limen name and its number (`total_15`), so a
   debugger and the C itself show which variable is which. Characters C
   does not allow in a name (the `$` of the compiler's own variables)
   become `_`; a variable with no name is `v15`. The number keeps every
   name apart from the others and from C's own. *)
let var_name (v : I.var) =
  let ok c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' in
  let n = String.map (fun c -> if ok c then c else '_') v.name in
  if n = "" || (n.[0] >= '0' && n.[0] <= '9') then sprintf "v%d" v.id else sprintf "%s_%d" n v.id

(* A `mut` parameter is a pointer to the caller's place: every use goes
   through it. *)
let var (v : I.var) = if v.by_ref then sprintf "(*%s)" (var_name v) else var_name v

let float_lit x =
  if Float.is_nan x then "NAN"
  else if x = Float.infinity then "INFINITY"
  else if x = Float.neg_infinity then "(-INFINITY)"
  else sprintf "%h" x

let atom c (a : I.atom) =
  match a with
  | I.V v -> var v
  | I.VDup v -> if is_scalar v.ty then var v else sprintf "%s_dup(%s)" (cname c v.ty) (var v)
  | I.AInt n -> if n = Int64.min_int then "INT64_MIN" else sprintf "INT64_C(%Ld)" n
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
        | TArray (el, TLen n) -> (sprintf "(%s).a[w_idx(%s, %d, %s)]" e (atom c i) n (where sp), el)
        | _ -> failwith "walk index")
      | I.PIndexU i -> (
        match t with
        | TList el -> (sprintf "(%s)->data[%s]" e (atom c i), el)
        | TArray (el, _) -> (sprintf "(%s).a[%s]" e (atom c i), el)
        | _ -> failwith "walk index")
      | I.PKey (k, sp) -> (
        match t with
        | TMap (_, vt) -> (sprintf "(*%s_slot(%s, %s, %s))" (cname c t) e (atom c k) (where sp), vt)
        | _ -> failwith "walk key"))
    (root, rty) steps

let rec pat_cond c e t (p : Ast.pat) =
  match p.pdesc with
  | Ast.PWild | Ast.PBind _ -> "true"
  | Ast.PInt n -> if n = Int64.min_int then sprintf "(%s == INT64_MIN)" e else sprintf "(%s == INT64_C(%Ld))" e n
  | Ast.PBool b -> sprintf "(%s == %s)" e (if b then "true" else "false")
  | Ast.PStr s -> sprintf "Str_eq(%s, &%s)" e (lit c s)
  | Ast.PVariant (_, vn, args) ->
    let k = Lower.tag_of c.g t vn in
    let ptys = List.assoc vn (Lower.variants c.g t) in
    let subs = List.mapi (fun i (a, pt) -> pat_cond c (payload_expr c t e k i) pt a) (List.combine (Ast.fill_rest args (List.length ptys)) ptys) in
    String.concat " && " (sprintf "(%s == %d)" (tag_expr c t e) k :: List.filter (fun s -> s <> "true") subs)
  | Ast.PTuple ps ->
    let fts = Lower.field_types c.g t in
    let subs = List.mapi (fun i (p, (_, ft)) -> pat_cond c (sprintf "(%s).f%d" e i) ft p) (List.combine (Ast.fill_rest ps (List.length fts)) fts) in
    (match List.filter (fun s -> s <> "true") subs with [] -> "true" | xs -> String.concat " && " xs)
  | Ast.PList ps ->
    let el = match t with TList el -> el | _ -> failwith "pat_cond: list" in
    let before, rest, after = Ast.list_parts ps in
    let k = List.length before and j = List.length after in
    let len = sprintf "(%s)->len" e in
    let size = if rest = None then sprintf "(%s == %d)" len k else sprintf "(%s >= %d)" len (k + j) in
    let subs =
      List.mapi (fun i p -> pat_cond c (sprintf "(%s)->data[%d]" e i) el p) before
      @ List.mapi (fun i p -> pat_cond c (sprintf "(%s)->data[%s - %d]" e len (j - i)) el p) after
    in
    String.concat " && " (size :: List.filter (fun s -> s <> "true") subs)
  | Ast.PStruct (_, _, fs, _) ->
    let fts = Lower.field_types c.g t in
    let items = Ast.struct_items fs (List.map fst fts) p.pspan in
    let subs = List.mapi (fun i (p, (_, ft)) -> pat_cond c (sprintf "(%s).f%d" e i) ft p) (List.combine items fts) in
    (match List.filter (fun s -> s <> "true") subs with [] -> "true" | xs -> String.concat " && " xs)
  | Ast.PAs (q, _, _) -> pat_cond c e t q
  | Ast.PRest _ -> "true"
  | Ast.POr alts ->
    let conds = List.map (pat_cond c e t) alts in
    if List.mem "true" conds then "true" else "(" ^ String.concat " || " conds ^ ")"

(* The prefix of the order function for [t] (used as <prefix>_cmp); composite
   types get one generated, with the types inside them. *)
let rec cmp_prefix c t =
  match t with
  | TInt -> "Int"
  | TFloat -> "Float"
  | TStr -> "Str"
  | TBytes -> "Bytes"
  | TBool -> "Bool"
  | TUnit -> "Unit"
  | TUInt n -> "UInt" ^ string_of_int n
  | _ ->
    let n = cname c t in
    if not (List.exists (fun u -> cname c u = n) c.cmp_tys) then begin
      c.cmp_tys <- c.cmp_tys @ [ t ];
      match t with
      | TList e | TArray (e, _) -> ignore (cmp_prefix c e)
      | TNamed _ when is_enum c t -> List.iter (fun (_, ps) -> List.iter (fun p -> ignore (cmp_prefix c p)) ps) (Lower.variants c.g t)
      | TNamed _ -> List.iter (fun (_, ft) -> ignore (cmp_prefix c ft)) (Lower.field_types c.g t)
      | _ -> failwith ("cgen: no order on " ^ show t)
    end;
    n

(* The prefix of the hash function for map key type [t]; tuple, struct and
   enum keys get one generated, with the types inside them. *)
let rec hash_prefix c t =
  match t with
  | TInt -> "Int"
  | TStr -> "Str"
  | TBytes -> "Bytes"
  | TBool -> "Bool"
  | TUnit -> "Unit"
  | TUInt n -> "UInt" ^ string_of_int n
  | _ ->
    let n = cname c t in
    if not (List.exists (fun u -> cname c u = n) c.hash_tys) then begin
      c.hash_tys <- c.hash_tys @ [ t ];
      match t with
      | TNamed _ when is_enum c t -> List.iter (fun (_, ps) -> List.iter (fun p -> ignore (hash_prefix c p)) ps) (Lower.variants c.g t)
      | TNamed _ -> List.iter (fun (_, ft) -> ignore (hash_prefix c ft)) (Lower.field_types c.g t)
      | _ -> failwith ("cgen: no hash for " ^ show t)
    end;
    n

let binop c op t a b sp =
  let open Ast in
  match (t, op) with
  | TInt, Add -> sprintf "(%s + %s)" a b
  | TInt, Sub -> sprintf "(%s - %s)" a b
  | TInt, Mul -> sprintf "(%s * %s)" a b
  | TInt, Div -> sprintf "w_div(%s, %s, %s)" a b (where sp)
  | TInt, Mod -> sprintf "w_mod(%s, %s, %s)" a b (where sp)
  | TInt, BitAnd -> sprintf "(%s & %s)" a b
  | TInt, BitOr -> sprintf "(%s | %s)" a b
  | TInt, BitXor -> sprintf "(%s ^ %s)" a b
  | TInt, Shl -> sprintf "w_shl(%s, %s, %s)" a b (where sp)
  | TInt, Shr -> sprintf "w_shr(%s, %s, %s)" a b (where sp)
  | TUInt n, (Add | Sub | Mul | BitAnd | BitOr | BitXor) ->
    let o = match op with Add -> "+" | Sub -> "-" | Mul -> "*" | BitAnd -> "&" | BitOr -> "|" | _ -> "^" in
    sprintf "((uint%d_t)(%s %s %s))" n a o b
  | TUInt n, Div -> sprintf "((uint%d_t)w_udiv(%s, %s, %s))" n a b (where sp)
  | TUInt n, Mod -> sprintf "((uint%d_t)w_umod(%s, %s, %s))" n a b (where sp)
  | TUInt n, Shl -> sprintf "((uint%d_t)w_ushl_n(%s, %s, %d))" n a b n
  | TUInt n, Shr -> sprintf "((uint%d_t)w_ushr_n(%s, %s, %d))" n a b n
  | TFloat, Add -> sprintf "(%s + %s)" a b
  | TFloat, Sub -> sprintf "(%s - %s)" a b
  | TFloat, Mul -> sprintf "(%s * %s)" a b
  | TFloat, Div -> sprintf "(%s / %s)" a b
  | (TStr | TBytes), Add -> sprintf "Str_concat(%s, %s)" a b
  | TList _, Add -> sprintf "%s_concat(%s, %s)" (cname c t) a b
  | (TInt | TFloat | TBool | TUInt _), (Lt | Le | Gt | Ge | Eq | Ne) ->
    let o = match op with Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">=" | Eq -> "==" | _ -> "!=" in
    sprintf "(%s %s %s)" a o b
  | TStr, (Lt | Le | Gt | Ge) ->
    let o = match op with Lt -> "<" | Le -> "<=" | Gt -> ">" | _ -> ">=" in
    sprintf "(Str_cmp(%s, %s) %s 0)" a b o
  | _, Eq -> sprintf "%s_eq(%s, %s)" (cname c t) a b
  | _, Ne -> sprintf "(!%s_eq(%s, %s))" (cname c t) a b
  | _, (Lt | Le | Gt | Ge) ->
    let o = match op with Lt -> "<" | Le -> "<=" | Gt -> ">" | _ -> ">=" in
    sprintf "(%s_cmp(%s, %s) %s 0)" (cmp_prefix c t) a b o
  | _ -> failwith ("cgen binop on " ^ show t)

(* ---------- foreign calls ---------- *)

(* The C type of a boundary type: a C struct gets a typedef of its own
   (VFS<n>, fields c0, c1, ... at C's layout), a callback is a pointer to a
   function. *)
let rec ffi_ctype c (t : Ffi.cty) =
  match t with
  | Ffi.Struct (n, fs) -> (
    match List.find_opt (fun (m, _, _) -> m = n) c.cstructs with
    | Some (_, cn, _) -> cn
    | None ->
      let cn = sprintf "VFS%d" (List.length c.cstructs) in
      c.cstructs <- c.cstructs @ [ (n, cn, fs) ];
      cn)
  | Ffi.Fn (ps, r) -> sprintf "%s (*)(%s)" (ffi_ctype c r) (if ps = [] then "void" else String.concat ", " (List.map (ffi_ctype c) ps))
  | Ffi.Kept ps -> ffi_ctype c (Ffi.Fn (ps, Ffi.Unit))
  | t -> Ffi.c_name t

(* The number of a kept callback's C signature, its trampolines named
   vkt<n>_ (an array of W_KEPT), registered on first use. *)
let kept_sig c ps =
  let rec find i = function [] -> None | q :: r -> if q = ps then Some i else find (i + 1) r in
  match find 0 c.kepts with
  | Some i -> i
  | None ->
    c.kepts <- c.kepts @ [ ps ];
    List.length c.kepts - 1

(* A Limen value [v] as the C value of boundary type [t]. [what] names it in
   an Int32 range panic. In the variadic part of a call, Int32 and Bool go as
   int and Int as int64_t, written out so C's promotions see the right type. *)
let ffi_arg c ?(variadic = false) (t : Ffi.cty) v what wh =
  let i32 v what = sprintf "w_ffi_i32(%s, %s, %s)" v (c_string what) wh in
  match t with
  | Ffi.Int -> if variadic then sprintf "(int64_t)%s" v else v
  | Ffi.Float -> if variadic then sprintf "(double)%s" v else v
  | Ffi.Int32 -> if variadic then sprintf "(int)%s" (i32 v what) else i32 v what
  | Ffi.Bool -> sprintf "(%s(%s ? 1 : 0))" (if variadic then "(int)" else "") v
  | Ffi.Str -> sprintf "(const char *)(%s)->data" v
  | Ffi.Bytes -> sprintf "(const void *)(%s)->data" v
  | Ffi.Handle -> sprintf "(void *)(intptr_t)(%s).f0" v
  | Ffi.Struct (n, fs) ->
    let field i (f, ft) =
      let x = sprintf "(%s).f%d" v i in
      match ft with
      | Ffi.Int32 -> i32 x (sprintf "field `%s` of `%s`" f n)
      | Ffi.Bool -> sprintf "(%s ? 1 : 0)" x
      | Ffi.Handle -> sprintf "(void *)(intptr_t)(%s).f0" x
      | _ -> x
    in
    sprintf "(%s){ %s }" (ffi_ctype c t) (String.concat ", " (List.mapi field fs))
  | Ffi.Kept ps -> sprintf "vkt%d_[w_kept_slot(%s, %s)]" (kept_sig c ps) v wh
  | Ffi.Unit | Ffi.Fn _ -> "0"

(* A C value [e] of boundary type [t] as a Limen value. *)
let ffi_result c (t : Ffi.cty) e =
  let hd = cname c Ffi.handle_ty in
  let one t e =
    match t with
    | Ffi.Int | Ffi.Float -> e
    | Ffi.Int32 -> sprintf "(int64_t)%s" e
    | Ffi.Bool -> sprintf "(%s != 0)" e
    | Ffi.Str -> sprintf "w_ffi_str(%s)" e
    | Ffi.Handle -> sprintf "(%s){ (int64_t)(intptr_t)%s }" hd e
    | _ -> "0"
  in
  match t with
  | Ffi.Struct (n, fs) -> sprintf "(%s){ %s }" (cname c (TNamed (n, []))) (String.concat ", " (List.mapi (fun i (_, ft) -> one ft (sprintf "%s.c%d" e i)) fs))
  | t -> one t e

(* The wrapper C calls for callback [fn] (a top-level function's C name),
   created on first use. *)
let ffi_callback c (fn, vname, sp, takes_foreign) ps r =
  match List.assoc_opt (fn, takes_foreign, ps, r) c.cbs with
  | Some (w, _, _) -> w
  | None ->
    let w = sprintf "vcb%d_" (List.length c.cbs) in
    c.cbs <- c.cbs @ [ ((fn, takes_foreign, ps, r), (w, vname, sp)) ];
    w

(* A callback wrapper's definition: it converts C's arguments, calls the
   compiled Limen function under a panic boundary of its own, and converts its
   result. Nothing may unwind through C's frames, so a panic there ends the
   program (w_cb_fatal), and so does waiting (see w_park). *)
let ffi_callback_def c ((fn, takes_foreign, ps, r), (w, vname, sp)) =
  let b = Buffer.create 512 in
  let p s = Buffer.add_string b (s ^ "\n") in
  let wh = where sp in
  let params = List.mapi (fun i t -> sprintf "%s a%d" (ffi_ctype c t) i) ps in
  p (sprintf "static %s %s(%s) {" (ffi_ctype c r) w (if params = [] then "void" else String.concat ", " params));
  p (sprintf "  WFiber *f_ = w_cb_fiber(%s);" wh);
  p "  const char *prev_ = f_->in_cb;";
  p (sprintf "  f_->in_cb = %s;" wh);
  if r <> Ffi.Unit then p (sprintf "  %s volatile r_ = 0;" (ffi_ctype c r));
  p "  WCatch c_;";
  p "  w_catch_init(&c_);";
  p "  WCatch **slot_ = &f_->catch_top;";
  p "  if (_setjmp(c_.jb) == 0) {";
  p "    WCatch *guard_ __attribute__((cleanup(w_catch_land))) = &c_;";
  p "    *slot_ = &c_;";
  let hd = cname c Ffi.handle_ty in
  let arg i t =
    let a = sprintf "a%d" i in
    match t with
    | Ffi.Int32 -> sprintf "(int64_t)%s" a
    | Ffi.Bool -> sprintf "(%s != 0)" a
    | Ffi.Handle -> sprintf "(%s){ (int64_t)(intptr_t)%s }" hd a
    | Ffi.Str -> sprintf "w_ffi_str(%s)" a
    | _ -> a
  in
  (* a callback that takes a Foreign gets the one its foreign call was given *)
  let fa = if takes_foreign then [ "Cap_dup(f_->cb_foreign)" ] else [] in
  let call = sprintf "%s(%s)" fn (String.concat ", " (fa @ List.mapi arg ps)) in
  (match r with
   | Ffi.Unit -> p (sprintf "    %s;" call)
   | Ffi.Int32 -> p (sprintf "    r_ = w_ffi_i32(%s, %s, %s);" call (c_string (sprintf "the result of callback `%s`" vname)) wh)
   | Ffi.Bool -> p (sprintf "    r_ = %s ? 1 : 0;" call)
   | Ffi.Handle -> p (sprintf "    r_ = (void *)(intptr_t)(%s).f0;" call)
   | _ -> p (sprintf "    r_ = %s;" call));
  p "    *slot_ = c_.prev;";
  p "    w_catch_end(&c_);";
  p (sprintf "  } else w_cb_fatal(&c_, %s);" wh);
  p "  f_->in_cb = prev_;";
  if r <> Ffi.Unit then p "  return r_;";
  p "}";
  Buffer.contents b

(* A kept callback signature's trampolines: W_KEPT functions with C's
   parameters, each building the channel's value from them and sending it
   on its slot's channel (w_kept_send), and the array C's pointers come
   from. *)
let kept_def c i ps =
  let b = Buffer.create 1024 in
  let p s = Buffer.add_string b (s ^ "\n") in
  let hd = cname c Ffi.handle_ty in
  let elem = Ffi.kept_elem ps in
  let cps = List.mapi (fun k t -> sprintf "%s a%d" (ffi_ctype c t) k) ps in
  let params = if ps = [] then "void" else String.concat ", " cps in
  let conv k t =
    let a = sprintf "a%d" k in
    match t with
    | Ffi.Int32 -> sprintf "(int64_t)%s" a
    | Ffi.Bool -> sprintf "(%s != 0)" a
    | Ffi.Handle -> sprintf "(%s){ (int64_t)(intptr_t)%s }" hd a
    | Ffi.Str -> sprintf "w_ffi_str(%s)" a
    | _ -> a
  in
  let value =
    match ps with
    | [] -> "0"
    | [ t ] -> conv 0 t
    | ps -> sprintf "(%s){ %s }" (cname c elem) (String.concat ", " (List.mapi conv ps))
  in
  p (sprintf "static void vkt%d_fire(int64_t i_%s) {" i (String.concat "" (List.map (fun x -> ", " ^ x) cps)));
  p (sprintf "  %s v_ = %s;" (ctype c elem) value);
  p "  w_kept_send(i_, &v_);";
  p "}";
  let names = List.init 32 (fun k -> sprintf "vkt%d_%d" i k) in
  List.iteri
    (fun k n ->
      p (sprintf "static void %s(%s) { vkt%d_fire(%d%s); }" n params i k (String.concat "" (List.mapi (fun j _ -> sprintf ", a%d" j) ps))))
    names;
  p (sprintf "static void (*const vkt%d_[W_KEPT])(%s) = { %s };" i params (String.concat ", " names));
  Buffer.contents b

(* A pointer to the place [v] and [steps], made unique on the way (the
   checks a loop hoisted are skipped): the C names [prefix]0_, [prefix]1_
   and so on, one per step. Returns the last and the place's type.
   The cast drops `volatile` from a variable a `defer` or guard needs
   volatile (see ctx.vols). The callee, a `mut` parameter or a runtime
   function such as List_push, takes a plain pointer, and giving it a
   volatile one would change every such function. It is safe in practice:
   the variable's address is taken, so it lives in memory; a write through
   the pointer reaches memory before any call that can panic, since that
   call might read it; and after a longjmp the caller reads the variable
   through its volatile name, from memory. *)
let place_ptr c line prefix (v : I.var) steps =
  line (sprintf "  %s* %s0_ = (%s*)&%s;" (ctype c v.ty) prefix (ctype c v.ty) (var v));
  List.fold_left
    (fun (p, t) (i, step) ->
      let q = sprintf "%s%d_" prefix (i + 1) in
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
        | TArray (el, TLen n) ->
          line (sprintf "  %s* %s = &(%s->a[w_idx(%s, %d, %s)]);" (ctype c el) q p (atom c ia) n (where sp));
          (q, el)
        | _ -> failwith "mutate index")
      | I.PKey (k, sp) -> (
        match t with
        | TMap (_, vt) ->
          line (sprintf "  %s_unique(%s);" (cname c t) p);
          line (sprintf "  %s* %s = %s_slot(*%s, %s, %s);" (ctype c vt) q (cname c t) p (atom c k) (where sp));
          (q, vt)
        | _ -> failwith "mutate key")
      | I.PIndexU _ -> failwith "mutate through unchecked index")
    (prefix ^ "0_", v.ty)
    (List.mapi (fun i s -> (i, s)) steps)

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
  | I.CallMut (f, args) ->
    (* the values first, in order, so a value that names a place is
       retained before the place is made unique; then a pointer to each
       place *)
    line "{";
    List.iteri
      (fun k -> function
        | I.ByValue x ->
          let ct = ctype c (I.atom_ty x) in
          let ax = a x in
          line (sprintf "  %s a%d_ = %s;" ct k ax)
        | I.ByPlace _ -> ())
      args;
    let xs =
      List.mapi
        (fun k -> function
          | I.ByValue _ -> sprintf "a%d_" k
          | I.ByPlace (v, steps) -> fst (place_ptr c line (sprintf "r%d_" k) v steps))
        args
    in
    line (sprintf "  %s = %s(%s);" dst f (String.concat ", " xs));
    line "}"
  | I.CallClo (f, args) ->
    let fv = match f with I.V v | I.VDup v -> v | _ -> failwith "callclo" in
    let ps, r = match fv.ty with TFn (ps, r) -> (ps, r) | _ -> failwith "callclo type" in
    let sigt = sprintf "%s (*)(%s)" (ctype c r) (String.concat ", " ("WClo*" :: List.map (ctype c) ps)) in
    set (sprintf "((%s)(%s)->fn)(%s)" sigt (var fv) (String.concat ", " (var fv :: List.map a args)))
  | I.MkDyn (t, i, x, ms) ->
    let key = cname c t ^ "_as_" ^ String.map (fun ch -> if ch = '.' || ch = '[' || ch = ']' || ch = ',' || ch = ' ' || ch = '(' || ch = ')' then '_' else ch) i in
    if not (List.exists (fun (k, _, _) -> k = key) c.dyns) then c.dyns <- c.dyns @ [ (key, t, ms) ];
    line "{";
    line (sprintf "  struct Dyn_%s* d_ = (struct Dyn_%s*)w_alloc_k(sizeof(struct Dyn_%s), WK_PLAIN);" key key key);
    line (sprintf "  d_->base.rc = 1; d_->base.vt = (const WVt*)&Dyn_%s_vt; d_->val = %s;" key (a x));
    line "  w_set_kids(d_, w_dyn_kids);";
    line (sprintf "  %s = (WDyn*)d_;" dst);
    line "}"
  | I.Select (id, ops, has_default, sp) ->
    let n = List.length ops in
    line (sprintf "WSelOp sel%d_ops[%d];" id n);
    List.iteri
      (fun i op ->
        match op with
        | I.SRecv (ch, el, on) ->
          line (sprintf "%s sel%d_v%d;" (ctype c el) id i);
          line (sprintf "sel%d_ops[%d] = (WSelOp){ %s, 0, &sel%d_v%d, 0, %s };" id i (a ch) id i (a on))
        | I.SSend (ch, v, el, on) ->
          line (sprintf "%s sel%d_s%d = %s;" (ctype c el) id i (a v));
          line (sprintf "sel%d_ops[%d] = (WSelOp){ %s, 1, &sel%d_s%d, 0, %s };" id i (a ch) id i (a on))
        | I.SAfter (_, ms, on) -> line (sprintf "sel%d_ops[%d] = (WSelOp){ NULL, 2, NULL, 0, %s, %s };" id i (a on) (a ms)))
      ops;
    set (sprintf "w_select(sel%d_ops, %d, %d, %s)" id n (if has_default then 1 else 0) (where sp));
    (* a value that was not sent is still ours *)
    List.iteri
      (fun i op ->
        match op with
        | I.SSend (_, _, el, _) when not (is_scalar el) -> line (sprintf "if (%s != %d) %s_drop(sel%d_s%d);" dst i (cname c el) id i)
        | _ -> ())
      ops
  | I.SelGot (id, i, ot) ->
    set (sprintf "(sel%d_ops[%d].ok ? %s : %s)" id i (mk_enum c ot 0 [ sprintf "sel%d_v%d" id i ]) (mk_enum c ot 1 []))
  | I.Catch (clo, rt) ->
    (* run the closure under a panic boundary: Ok(value), or Err(message) *)
    let fv = match clo with I.V v | I.VDup v -> var v | _ -> failwith "catch closure" in
    line "{";
    line "  WCatch c_;";
    line "  w_catch_init(&c_);";
    line "  WCatch **slot_ = w_catch_slot();";
    line "  if (_setjmp(c_.jb) == 0) {";
    line "    WCatch *guard_ __attribute__((cleanup(w_catch_land))) = &c_;";
    line "    *slot_ = &c_;";
    line (sprintf "    %s r_ = ((%s (*)(WClo*))(%s)->fn)(%s);" (ctype c rt) (ctype c rt) fv fv);
    line "    *slot_ = c_.prev;";
    line "    w_catch_end(&c_);";
    line (sprintf "    %s = %s;" dst (mk_enum c t 0 [ "r_" ]));
    line "  } else {";
    line "    w_catch_release(&c_);";
    line "    if (c_.cancel) w_cancel_unwind();   /* a cancellation passes through `catch` */";
    line "    WStr *m_ = Str_from(c_.msg, (int64_t)strlen(c_.msg));";
    line "    free(c_.msg); free(c_.where);";
    line (sprintf "    %s = %s;" dst (mk_enum c t 1 [ "m_" ]));
    line "  }";
    line "}"
  | I.CallC ({ lib = "limen"; sym; _ }, args, _, sp) -> (
    (* the runtime's own helpers (Ffi.runtime_fns) *)
    let hd = cname c Ffi.handle_ty in
    let use_sql_api () =
      List.iter (fun sym -> if not (List.mem sym c.csyms) then c.csyms <- c.csyms @ [ sym ]) Ffi.sql_syms;
      c.sql_api <- true
    in
    match (sym, List.map a args) with
    | "limen_copy", [ p; n ] -> set (sprintf "w_ffi_copy((%s).f0, %s, %s)" p n (where sp))
    | "limen_handle_new", [ p ] -> set (sprintf "w_handle_new((%s).f0)" p)
    | "limen_sql_run", [ db; text; params; mode ] ->
      use_sql_api ();
      set (sprintf "w_sql_run(w_sql_api, (%s).f0, %s, %s, %s)" db text params mode)
    | "limen_sql_open", [ root; name; flags ] ->
      use_sql_api ();
      set (sprintf "w_sql_open(w_sql_api, %s, %s, %s)" root name flags)
    | "limen_bytes_int", [ b; at ] -> set (sprintf "w_bytes_int(%s, %s, %s)" b at (where sp))
    | "limen_bytes_float", [ b; at ] -> set (sprintf "w_bytes_float(%s, %s, %s)" b at (where sp))
    | "limen_float_bits", [ x ] -> set (sprintf "w_float_bits(%s)" x)
    | "limen_crypto", [ op; x1; x2; x3; x4 ] -> uses_tls := true; set (sprintf "w_crypto(%s, %s, %s, %s, %s)" op x1 x2 x3 x4)
    | ("limen_handle_acquire" | "limen_handle_release" | "limen_handle_close"), [ id ] ->
      set (sprintf "(%s){ w_%s(%s) }" hd (let p = String.length "limen_" in String.sub sym p (String.length sym - p)) id)
    | _ -> failwith ("cgen: runtime helper " ^ sym))
  | I.CallC (cs, args, cbs, sp) ->
    if not (List.mem cs.sym c.csyms) then c.csyms <- c.csyms @ [ cs.sym ];
    line "{";
    let vals =
      if cs.trusted then List.map a args
      else
        match List.map a args with
        | cap :: vs ->
          line (sprintf "  w_foreign_check(%s, %s, %s);" cap (c_string cs.lib) (where sp));
          vs
        | [] -> failwith "cgen: foreign call"
    in
    let pass_foreign = List.exists (fun (_, _, _, tf) -> tf) cbs in
    let rest = ref vals and cbs = ref cbs and outs = ref [] in
    let fixed = match cs.fixed with Some k -> k | None -> List.length cs.params in
    (* a blocking call's arguments go into its job: (field, C type, value) *)
    let fields = ref [] in
    let cargs =
      List.mapi
        (fun i (p : Ffi.param) ->
          if p.out then begin
            if cs.blocking then begin
              outs := (sprintf "j_->o%d" i, p.cty) :: !outs;
              sprintf "&j->o%d" i
            end
            else begin
              let o = sprintf "o%d_" i in
              line (sprintf "  %s %s = 0;" (Ffi.c_name p.cty) o);
              outs := (o, p.cty) :: !outs;
              "&" ^ o
            end
          end
          else
            match p.cty with
            | Ffi.Fn (ps, r) -> (
              match !cbs with
              | cb :: rest -> cbs := rest; ffi_callback c cb ps r
              | [] -> failwith "cgen: foreign callback")
            | t ->
              let v = match !rest with v :: r -> rest := r; v | [] -> failwith "cgen: foreign arity" in
              if cs.blocking then begin
                (* a Str or Bytes is lent to C through a reference the job keeps *)
                let x =
                  match t with
                  | Ffi.Str -> sprintf "(const char *)w_block_keep(&j_->h, %s)" v
                  | Ffi.Bytes -> sprintf "w_block_keep(&j_->h, %s)" v
                  | _ -> ffi_arg c ~variadic:(i >= fixed) t v (sprintf "argument `%s` of `%s`" p.name cs.sym) (where sp)
                in
                fields := (sprintf "a%d" i, (if i >= fixed && (t = Ffi.Int32 || t = Ffi.Bool) then "int" else ffi_ctype c t), x) :: !fields;
                sprintf "j->a%d" i
              end
              else ffi_arg c ~variadic:(i >= fixed) t v (sprintf "argument `%s` of `%s`" p.name cs.sym) (where sp))
        cs.params
    in
    let outs = List.rev !outs in
    let ptys = List.filteri (fun i _ -> i < fixed) cs.params in
    let proto =
      sprintf "%s (*)(%s)" (ffi_ctype c cs.ret)
        (if ptys = [] then "void"
         else
           String.concat ", " (List.map (fun (p : Ffi.param) -> ffi_ctype c p.cty ^ if p.out then " *" else "") ptys)
           ^ if cs.fixed <> None then ", ..." else "")
    in
    if cs.blocking then begin
      (* Blocking calls: the call runs on the blocking-call pool, from a job
         holding its arguments, out-parameters and result (WBlock in the
         runtime), while the task waits. A struct's C type is named by
         ffi_ctype already, for the prototype. *)
      let fields = List.rev !fields in
      let n = sprintf "vbj%d_" (List.length c.bjobs) in
      let call = sprintf "((%s)vf_%s)(%s)" proto cs.sym (String.concat ", " cargs) in
      let rty = ffi_ctype c cs.ret in
      let members =
        List.mapi (fun i (p : Ffi.param) -> if p.out then [ sprintf "%s o%d;" (Ffi.c_name p.cty) i ] else []) cs.params
        |> List.concat
      in
      let members = List.map (fun (f, ty, _) -> sprintf "%s %s;" ty f) fields @ members in
      let members = members @ (match cs.ret with Ffi.Unit | Ffi.Str -> [] | _ -> [ sprintf "%s r;" rty ]) in
      let body =
        match cs.ret with
        | Ffi.Unit -> sprintf "%s;" call
        | Ffi.Str -> sprintf "j->h.res = w_ffi_str(%s);" call
        | _ -> sprintf "j->r = %s;" call
      in
      c.bjobs <-
        c.bjobs
        @ [ sprintf "typedef struct { WBlock h; %s } %s;\nstatic void %srun(WBlock *b_) { %s *j = (%s *)b_; %s }\n" (String.concat " " members) n n n n body ];
      line (sprintf "  %s *j_ __attribute__((cleanup(w_block_end))) = (%s *)w_block_new(sizeof(%s), %srun);" n n n n);
      List.iter (fun (f, _, x) -> line (sprintf "  j_->%s = %s;" f x)) fields;
      line "  w_block_wait(&j_->h);";
      let vals =
        (match cs.ret with Ffi.Unit -> [] | Ffi.Str -> [ "w_block_str(&j_->h)" ] | _ -> [ ffi_result c cs.ret "j_->r" ])
        @ List.map (fun (o, t) -> ffi_result c t o) outs
      in
      (match vals with
       | [] -> line (sprintf "  %s = 0;" dst)
       | [ v ] -> line (sprintf "  %s = %s;" dst v)
       | vs -> line (sprintf "  %s = (%s){ %s };" dst (cname c t) (String.concat ", " vs)));
      line "}"
    end
    else begin
    let call = sprintf "((%s)vf_%s)(%s)" proto cs.sym (String.concat ", " cargs) in
    (* the Foreign value for callbacks that take one, kept on the task's fiber *)
    if pass_foreign then begin
      line "  WFiber *cf_ = w_current();";
      line "  WCap *cfp_ = cf_->cb_foreign;";
      line (sprintf "  cf_->cb_foreign = %s;" (List.hd (List.map a args)))
    end;
    if cs.ret = Ffi.Unit then line (sprintf "  %s;" call) else line (sprintf "  %s r_ = %s;" (ffi_ctype c cs.ret) call);
    if pass_foreign then line "  cf_->cb_foreign = cfp_;";
    let vals = (if cs.ret = Ffi.Unit then [] else [ ffi_result c cs.ret "r_" ]) @ List.map (fun (o, t) -> ffi_result c t o) outs in
    (match vals with
     | [] -> line (sprintf "  %s = 0;" dst)
     | [ v ] -> line (sprintf "  %s = %s;" dst v)
     | vs -> line (sprintf "  %s = (%s){ %s };" dst (cname c t) (String.concat ", " vs)));
    line "}"
    end
  | I.CallDyn (k, ps, r, recv, args) ->
    let sigt = sprintf "%s (*)(%s)" (ctype c r) (String.concat ", " ("WDyn*" :: List.map (ctype c) ps)) in
    set (sprintf "((%s)(%s)->vt->m[%d])(%s)" sigt (a recv) k (String.concat ", " (a recv :: List.map a args)))
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
    line "  WClo* k_ = (WClo*)w_alloc_k(sizeof(WClo), WK_PLAIN);";
    line (sprintf "  k_->rc = 1; k_->fn = (void*)%s; k_->drop = NULL; k_->share = NULL;" fname);
    line (sprintf "  %s = k_;" dst);
    line "}"
  | I.MkClosure (fname, _, args) ->
    line "{";
    line (sprintf "  struct Env_%s* e_ = (struct Env_%s*)w_alloc_k(sizeof(struct Env_%s), WK_PLAIN);" fname fname fname);
    line (sprintf "  e_->base.rc = 1; e_->base.fn = (void*)%s; e_->base.drop = Env_%s_drop; e_->base.share = Env_%s_share;" fname fname fname);
    line (sprintf "  w_set_kids(e_, (void (*)(void*))Env_%s_drop);" fname);
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
  | I.GroupNew (None, il) -> set (sprintf "w_group_new(%d)" (Bool.to_int (not il)))
  | I.GroupNew (Some (_, ms), il) -> set (sprintf "w_group_new_until(%s, %d)" (a ms) (Bool.to_int (not il)))
  | I.Spawn (g, task, rt, keep) ->
    if not (List.mem rt c.task_tys) then c.task_tys <- c.task_tys @ [ rt ];
    line (sprintf "w_spawn(%s, %s, wtask_%s, sizeof(%s), %s);" (a g) (a task) (cname c rt) (ctype c rt) (if keep then "true" else "false"));
    set "0"
  | I.GroupJoin (g, lty, until) ->
    let el = match lty with TList e -> e | _ -> TUnit in
    let n = cname c lty in
    line "{";
    line (sprintf "  WGroup* g_ = %s;" (a g));
    line "  w_group_wait(g_, 1);";
    line "  w_group_reraise(g_);";
    line (sprintf "  %s* l_ = %s_new(g_->n);" n n);
    (* tasks that return () are not kept: the list holds the others' results *)
    if until then begin
      (* a cancelled task has no result: None *)
      let rt = match el with TNamed ("Option", [ r ]) -> r | _ -> TUnit in
      line "  for (int64_t i_ = 0; i_ < g_->n; i_++) {";
      line "    if (!g_->tasks[i_] || !g_->tasks[i_]->keep) continue;";
      line (sprintf "    if (g_->tasks[i_]->cancelled) l_->data[l_->len++] = %s;" (mk_enum c el 1 []));
      line (sprintf "    else { %s r_; memcpy(&r_, g_->tasks[i_]->slot, sizeof(%s)); l_->data[l_->len++] = %s; }" (ctype c rt) (ctype c rt) (mk_enum c el 0 [ "r_" ]));
      line "  }"
    end
    else
      line (sprintf "  for (int64_t i_ = 0; i_ < g_->n; i_++) if (g_->tasks[i_] && g_->tasks[i_]->keep) memcpy(&l_->data[l_->len++], g_->tasks[i_]->slot, sizeof(%s));" (ctype c el));
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
    | ("list_len" | "array_len"), TArray (_, TLen n) -> set (string_of_int n)
    | "array_get", TArray (el, TLen n) ->
      opt_from (sprintf "%s >= 0 && %s < %d" (arg 1) (arg 1) n) (dup_of c el (sprintf "(%s).a[%s]" (arg 0) (arg 1)))
    | "array_contains", TArray (el, TLen n) ->
      set (sprintf "({ bool f_ = false; for (int64_t i_ = 0; i_ < %d; i_++) if (%s) { f_ = true; break; } f_; })" n
             (binop c Ast.Eq el (sprintf "(%s).a[i_]" (arg 0)) (arg 1) Span.dummy))
    | "array_to_list", TArray (el, TLen n) ->
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
    | "keep", _ ->
      (* the value escapes into an empty asm statement the compiler cannot see through *)
      line (sprintf "{ __typeof__(%s) k_ = %s; __asm__ volatile(\"\" : : \"r\"(&k_) : \"memory\"); }" (arg 0) (arg 0));
      set "0"
    | "chan_close", _ -> line (sprintf "w_chan_close(%s, %s);" (arg 0) (where sp)); set "0"
    (* Atomic and Shared (the runtime's "Atomic and Shared") *)
    | "atomic_new", _ -> set (sprintf "w_atomic_new(%s)" (arg 0))
    | "atomic_get", _ -> set (sprintf "w_atomic_get(%s)" (arg 0))
    | "atomic_set", _ -> line (sprintf "w_atomic_set(%s, %s);" (arg 0) (arg 1)); set "0"
    | "atomic_add", _ -> set (sprintf "w_atomic_add(%s, %s)" (arg 0) (arg 1))
    | "atomic_swap", _ -> set (sprintf "w_atomic_swap(%s, %s)" (arg 0) (arg 1))
    | "atomic_compare_and_set", _ -> set (sprintf "w_atomic_cas(%s, %s, %s)" (arg 0) (arg 1) (arg 2))
    | "shared_new", TShared el ->
      (* the value is the Shared's own reference, switched to shared: any task may read it *)
      ignore (cname c rt);
      let cn = cname c el in
      let ct = ctype c el in
      let fns = if is_scalar el then "NULL, NULL" else sprintf "chdrop_%s, chshare_%s" cn cn in
      line "{";
      line (sprintf "  WShared *s_ = w_shared_new(sizeof(%s), %s);" ct fns);
      line (sprintf "  %s v_ = %s;" ct (dup_of c el (arg 0)));
      if not (is_scalar el) then line (sprintf "  %s_share(v_);" cn);
      line "  memcpy(s_->v, &v_, sizeof v_);";
      line (sprintf "  %s = s_;" dst);
      line "}"
    | "shared_get", TShared el ->
      (* a snapshot: another reference to the value, taken under the lock *)
      let ct = ctype c el in
      line "{";
      line (sprintf "  w_shared_lock(%s, %s);" (arg 0) (where sp));
      line (sprintf "  %s v_; memcpy(&v_, (%s)->v, sizeof v_);" ct (arg 0));
      line (sprintf "  %s = %s;" dst (dup_of c el "v_"));
      line (sprintf "  w_shared_unlock(%s);" (arg 0));
      line "}"
    | "shared_set", TShared el ->
      let ct = ctype c el in
      line "{";
      line (sprintf "  %s n_ = %s;" ct (dup_of c el (arg 1)));
      if not (is_scalar el) then line (sprintf "  %s_share(n_);" (cname c el));
      line (sprintf "  w_shared_lock(%s, %s);" (arg 0) (where sp));
      line (sprintf "  %s o_; memcpy(&o_, (%s)->v, sizeof o_); memcpy((%s)->v, &n_, sizeof n_);" ct (arg 0) (arg 0));
      line (sprintf "  w_shared_unlock(%s);" (arg 0));
      if not (is_scalar el) then line (sprintf "  %s_drop(o_);" (cname c el));
      line "}";
      set "0"
    | "shared_update", TShared el ->
      (* the function runs holding the lock, on its own reference to the
         value (so it copies what it changes, and a panic or a cancellation
         in it leaves the value as it was: the cleanup lets go of the lock);
         its result becomes the value *)
      let ct = ctype c el in
      line "{";
      line (sprintf "  WShared *u_ __attribute__((cleanup(w_update_end))) = w_update_begin(%s, %s);" (arg 0) (where sp));
      line (sprintf "  %s a_; memcpy(&a_, u_->v, sizeof a_);" ct);
      line (sprintf "  %s r_ = ((%s (*)(WClo*, %s))(%s)->fn)(%s, %s);" ct ct ct (arg 1) (arg 1) (dup_of c el "a_"));
      if not (is_scalar el) then line (sprintf "  %s_share(r_);" (cname c el));
      line (sprintf "  %s o_; memcpy(&o_, u_->v, sizeof o_); memcpy(u_->v, &r_, sizeof r_);" ct);
      line (sprintf "  %s = %s;" dst (dup_of c el "r_"));
      line "  w_update_end(&u_);";
      line "  u_ = NULL;";
      if not (is_scalar el) then line (sprintf "  %s_drop(o_);" (cname c el));
      line "}"
    | "map_new", TMap _ -> set (sprintf "%s_new()" (cname c rt))
    | "map_from", TMap (k, v) ->
      let lt = TList (TNamed (Ast.tuple_name 2, [ k; v ])) in
      line "{";
      line (sprintf "  %s* m_ = %s_new();" (cname c rt) (cname c rt));
      line (sprintf "  for (int64_t i_ = 0; i_ < (%s)->len; i_++) %s_put(&m_, %s, %s);" (arg 0) (cname c rt)
              (dup_of c k (sprintf "(%s)->data[i_].f0" (arg 0))) (dup_of c v (sprintf "(%s)->data[i_].f1" (arg 0))));
      ignore (cname c lt);
      line (sprintf "  %s = m_;" dst);
      line "}"
    | "map_at", TMap _ -> set (sprintf "%s_at(%s, %s, %s)" (cname c rt) (arg 0) (arg 1) (where sp))
    | "map_get", TMap (_, v) ->
      line "{";
      line (sprintf "  %s* p_ = %s_get(%s, %s);" (ctype c v) (cname c rt) (arg 0) (arg 1));
      line (sprintf "  %s = p_ ? %s : %s;" dst (mk_enum c t 0 [ dup_of c v "*p_" ]) (mk_enum c t 1 []));
      line "}"
    | "map_contains", TMap _ -> set (sprintf "(%s_find(%s, %s) >= 0)" (cname c rt) (arg 0) (arg 1))
    | "map_len", _ -> set (sprintf "(%s)->len" (arg 0))
    | "map_is_empty", _ -> set (sprintf "((%s)->len == 0)" (arg 0))
    | ("map_keys" | "map_values" | "map_entries"), TMap (k, v) ->
      let lt = t in
      let ln = cname c lt in
      let item =
        match name with
        | "map_keys" -> dup_of c k "(m_)->keys[i_]"
        | "map_values" -> dup_of c v "(m_)->vals[i_]"
        | _ ->
          let tt = TNamed (Ast.tuple_name 2, [ k; v ]) in
          sprintf "(%s){ %s, %s }" (cname c tt) (dup_of c k "(m_)->keys[i_]") (dup_of c v "(m_)->vals[i_]")
      in
      line "{";
      line (sprintf "  %s* m_ = %s;" (cname c rt) (arg 0));
      line (sprintf "  %s* l_ = %s_new(m_->len);" ln ln);
      line (sprintf "  for (int64_t i_ = 0; i_ < m_->n; i_++) if (m_->live[i_]) %s_push_u(&l_, %s);" ln item);
      line (sprintf "  %s = l_;" dst);
      line "}"
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
    | "list_sort", TList el ->
      ignore (cmp_prefix c el);
      (match el with
       | TInt | TStr | TFloat -> ()
       | _ -> if not (List.exists (fun u -> cname c u = cname c rt) c.sort_tys) then c.sort_tys <- c.sort_tys @ [ rt ]);
      set (sprintf "%s_sort(%s)" (cname c rt) (arg 0))
    | "list_slice", _ -> set (sprintf "%s_slice(%s, %s, %s)" (cname c rt) (arg 0) (arg 1) (arg 2))
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
    | "str_last_index_of", _ ->
      line "{";
      line (sprintf "  int64_t i_ = Str_rfind(%s, %s);" (arg 0) (arg 1));
      line (sprintf "  %s = i_ >= 0 ? %s : %s;" dst (mk_enum c t 0 [ "i_" ]) (mk_enum c t 1 []));
      line "}"
    | "str_bytes", _ -> set (sprintf "Str_bytes(%s)" (arg 0))
    | "str_chars", _ -> set (sprintf "Str_chars(%s)" (arg 0))
    | ("str_to_bytes" | "bytes_to_str"), _ -> set (sprintf "Str_dup(%s)" (arg 0))
    | "bytes_new", _ -> set (sprintf "Bytes_from_list(%s, %s)" (arg 0) (where sp))
    | "bytes_at", _ -> set (sprintf "Bytes_at(%s, %s, %s)" (arg 0) (arg 1) (where sp))
    | "bytes_len", _ -> set (sprintf "(%s)->len" (arg 0))
    | "bytes_is_empty", _ -> set (sprintf "((%s)->len == 0)" (arg 0))
    | "bytes_get", _ -> opt_from (sprintf "%s >= 0 && %s < (%s)->len" (arg 1) (arg 1) (arg 0)) (sprintf "(int64_t)(unsigned char)(%s)->data[%s]" (arg 0) (arg 1))
    | "bytes_slice", _ -> set (sprintf "Str_slice(%s, %s, %s)" (arg 0) (arg 1) (arg 2))
    | "bytes_to_list", _ -> set (sprintf "Str_bytes(%s)" (arg 0))
    | "int_range", _ -> set (sprintf "L_Int_range(%s, %s)" (arg 0) (arg 1))
    | "int_to_char", _ -> set (sprintf "Int_to_char(%s)" (arg 0))
    | "int_min", _ -> set (sprintf "(%s < %s ? %s : %s)" (arg 0) (arg 1) (arg 0) (arg 1))
    | "int_max", _ -> set (sprintf "(%s > %s ? %s : %s)" (arg 0) (arg 1) (arg 0) (arg 1))
    | ("int_to_str" | "float_to_str" | "bool_to_str"), _ -> show_into [ `Val (arg 0, rt) ]
    | "int_to_float", _ -> set (sprintf "(double)%s" (arg 0))
    | "int_abs", _ -> set (sprintf "w_abs(%s)" (arg 0))
    | "int_shift_right_unsigned", _ -> set (sprintf "w_ushr(%s, %s, %s)" (arg 0) (arg 1) (where sp))
    | ("int_to_uint8" | "uint_to_uint8"), _ -> set (sprintf "(uint8_t)(%s)" (arg 0))
    | ("int_to_uint16" | "uint_to_uint16"), _ -> set (sprintf "(uint16_t)(%s)" (arg 0))
    | ("int_to_uint32" | "uint_to_uint32"), _ -> set (sprintf "(uint32_t)(%s)" (arg 0))
    | ("int_to_uint64" | "uint_to_uint64"), _ -> set (sprintf "(uint64_t)(%s)" (arg 0))
    | "uint_to_int", _ -> set (sprintf "(int64_t)(%s)" (arg 0))
    | "uint_to_str", _ -> show_into [ `Val (arg 0, rt) ]
    | "uint_to_float", _ -> set (sprintf "w_u2f(%s)" (arg 0))
    | "uint_min", _ -> set (sprintf "(%s < %s ? %s : %s)" (arg 0) (arg 1) (arg 0) (arg 1))
    | "uint_max", _ -> set (sprintf "(%s > %s ? %s : %s)" (arg 0) (arg 1) (arg 0) (arg 1))
    | ("uint_rotate_left" | "uint_rotate_right"), _ ->
      let bits = match rt with TUInt n -> n | _ -> 64 in
      set (sprintf "(uint%d_t)w_urot(%s, %s, %d, %d)" bits (arg 0) (arg 1) bits (if name = "uint_rotate_left" then 1 else 0))
    | "float_to_int", _ -> set (sprintf "w_f2i(%s)" (arg 0))
    | "float_abs", _ -> set (sprintf "fabs(%s)" (arg 0))
    | "float_sqrt", _ -> set (sprintf "sqrt(%s)" (arg 0))
    | "float_to_fixed", _ -> set (sprintf "Float_to_fixed(%s, %s)" (arg 0) (arg 1))
    | "cap_print", TCap Stdout -> line (sprintf "Cap_print(%s, true);" (arg 1)); set "0"
    | "cap_write", TCap Stdout -> line (sprintf "Cap_print(%s, false);" (arg 1)); set "0"
    | "cap_print", TCap Stderr -> line (sprintf "Cap_eprint(%s, true);" (arg 1)); set "0"
    | "cap_write", TCap Stderr -> line (sprintf "Cap_eprint(%s, false);" (arg 1)); set "0"
    | "cap_now_ms", _ -> set "Cap_now_ms()"
    | "cap_sleep", TCap Clock -> line (sprintf "Cap_sleep(%s);" (arg 1)); set "0"
    | "cap_int", TCap Rand -> set (sprintf "Cap_rand_int(%s, %s, %s)" (arg 1) (arg 2) (where sp))
    | "cap_float", TCap Rand -> set "Cap_rand_float()"
    | "cap_bytes", TCap Rand -> set (sprintf "Cap_rand_bytes(%s, %s)" (arg 1) (where sp))
    | "cap_args", TCap Root -> set "Cap_args()"
    | "cap_read_line", TCap Stdin ->
      line "{";
      line "  bool ok_;";
      line "  WStr *s_ = Cap_stdin_line(&ok_);";
      line (sprintf "  %s = ok_ ? %s : %s;" dst (mk_enum c t 0 [ "s_" ]) (mk_enum c t 1 []));
      line "}"
    | "cap_read_all", TCap Stdin -> set "Cap_stdin_all()"
    | "cap_get", TCap Env ->
      line "{";
      line "  bool ok_;";
      line (sprintf "  WStr *s_ = Cap_env_get(%s, %s, &ok_);" (arg 0) (arg 1));
      line (sprintf "  %s = ok_ ? %s : %s;" dst (mk_enum c t 0 [ "s_" ]) (mk_enum c t 1 []));
      line "}"
    | "cap_append", TCap Fs -> io_result (sprintf "Cap_fs_append(%s, %s, %s)" (arg 0) (arg 1) (arg 2)) "0"
    | "cap_list", TCap Fs -> io_result (sprintf "Cap_fs_list(%s, %s)" (arg 0) (arg 1)) "(L_Str*)r_.ptr"
    | "cap_remove", TCap Fs -> io_result (sprintf "Cap_fs_remove(%s, %s)" (arg 0) (arg 1)) "0"
    | "cap_make_dir", TCap Fs -> io_result (sprintf "Cap_fs_make_dir(%s, %s)" (arg 0) (arg 1)) "0"
    | "cap_is_dir", TCap Fs -> set (sprintf "Cap_fs_is_dir(%s, %s)" (arg 0) (arg 1))
    | "cap_rename", TCap Fs -> io_result (sprintf "Cap_fs_rename(%s, %s, %s)" (arg 0) (arg 1) (arg 2)) "0"
    | "cap_copy", TCap Fs -> io_result (sprintf "Cap_fs_copy(%s, %s, %s)" (arg 0) (arg 1) (arg 2)) "0"
    | "cap_touch", TCap Fs -> io_result (sprintf "Cap_fs_touch(%s, %s)" (arg 0) (arg 1)) "0"
    | "cap_set_executable", TCap Fs -> io_result (sprintf "Cap_fs_set_executable(%s, %s, %s)" (arg 0) (arg 1) (arg 2)) "0"
    | "cap_read_link", TCap Fs -> io_result (sprintf "Cap_fs_read_link(%s, %s)" (arg 0) (arg 1)) "r_.val"
    | "cap_info", TCap Fs ->
      let fi = TNamed ("FileInfo", []) in
      io_result
        (sprintf "Cap_fs_info(%s, %s)" (arg 0) (arg 1))
        (sprintf "({ WFileInfo *i_ = (WFileInfo*)r_.ptr; %s v_ = { i_->size, i_->mtime_ms, i_->is_file, i_->is_dir }; free(i_); v_; })" (ctype c fi))
    | "cap_only", TCap Exec -> set (sprintf "Cap_exec_only(%s, %s)" (arg 0) (arg 1))
    | "cap_only_args", TCap Exec -> set (sprintf "Cap_exec_only_args(%s, %s, %s)" (arg 0) (arg 1) (arg 2))
    | "cap_only", TCap Foreign -> set (sprintf "Cap_foreign_only(%s, %s)" (arg 0) (arg 1))
    | "cap_allows", TCap Foreign -> set (sprintf "Cap_foreign_allows(%s, %s)" (arg 0) (arg 1))
    | "cap_copy", TCap Foreign -> set (sprintf "Cap_foreign_copy(%s, (%s).f0, %s, %s)" (arg 0) (arg 1) (arg 2) (where sp))
    | "cap_run", TCap Exec ->
      let po = TNamed ("ProcessOutput", []) in
      io_result
        (sprintf "Cap_exec_run(%s, %s, %s)" (arg 0) (arg 1) (arg 2))
        (sprintf "({ WProc *p_ = (WProc*)r_.ptr; %s v_ = { p_->code, p_->out, p_->err }; free(p_); v_; })" (ctype c po))
    | "cap_only", _ -> set (sprintf "Cap_net_only(%s, %s)" (arg 0) (arg 1))
    | "cap_get", _ ->
      uses_tls := true;
      let ht = TNamed ("HttpResponse", []) and mt = TMap (TStr, TStr) in
      io_result
        (sprintf "Cap_net_get(%s, %s)" (arg 0) (arg 1))
        (sprintf
           "({ WHttp *h_ = (WHttp*)r_.ptr; %s* m_ = %s_new(); for (int i_ = 0; i_ < h_->n; i_++) %s_put(&m_, h_->names[i_], h_->values[i_]); %s v_ = { h_->status, m_, h_->body }; free(h_->names); free(h_->values); free(h_); v_; })"
           (cname c mt) (cname c mt) (cname c mt) (ctype c ht))
    | "cap_under", _ -> set (sprintf "Cap_fs_under(%s, %s)" (arg 0) (arg 1))
    | "cap_read_only", _ -> set (sprintf "Cap_fs_read_only(%s)" (arg 0))
    | "cap_read", TCap Conn -> io_result (sprintf "Cap_conn_read(%s)" (arg 0)) "r_.val"
    | "cap_write", TCap Conn -> io_result (sprintf "Cap_conn_write(%s, %s)" (arg 0) (arg 1)) "0"
    | "cap_start_tls", TCap Conn -> uses_tls := true; io_result (sprintf "Cap_conn_start_tls(%s, %s)" (arg 0) (arg 1)) "0"
    | "cap_read_within", TCap Conn -> io_result (sprintf "Cap_conn_read_within(%s, %s, %s)" (arg 0) (arg 1) (arg 2)) "r_.val"
    | "cap_write_within", TCap Conn -> io_result (sprintf "Cap_conn_write_within(%s, %s, %s, %s)" (arg 0) (arg 1) (arg 2) (arg 3)) "0"
    | "cap_on", _ -> io_result (sprintf "Cap_listen_on(%s, %s)" (arg 0) (arg 1)) "(WCap*)r_.ptr"
    | "cap_accept", _ -> io_result (sprintf "Cap_accept(%s)" (arg 0)) "(WCap*)r_.ptr"
    | "cap_connect", _ -> io_result (sprintf "Cap_net_connect(%s, %s)" (arg 0) (arg 1)) "(WCap*)r_.ptr"
    | "cap_connect_tls", _ -> uses_tls := true; io_result (sprintf "Cap_net_connect_tls(%s, %s)" (arg 0) (arg 1)) "(WCap*)r_.ptr"
    | "cap_on_tls", _ ->
      uses_tls := true;
      io_result (sprintf "Cap_listen_on_tls(%s, %s, %s, %s)" (arg 0) (arg 1) (arg 2) (arg 3)) "(WCap*)r_.ptr"
    | "cap_port", _ -> set (sprintf "(int64_t)(%s)->port" (arg 0))
    | "cap_close", _ -> line (sprintf "Cap_sock_close(%s);" (arg 0)); set "0"
    | "cap_read", _ -> io_result (sprintf "Cap_fs_read(%s, %s)" (arg 0) (arg 1)) "r_.val"
    | "cap_read_at", _ -> io_result (sprintf "Cap_fs_read_at(%s, %s, %s, %s)" (arg 0) (arg 1) (arg 2) (arg 3)) "r_.val"
    | "cap_write", TCap Fs -> io_result (sprintf "Cap_fs_write(%s, %s, %s)" (arg 0) (arg 1) (arg 2)) "0"
    | "cap_exists", _ -> set (sprintf "Cap_fs_exists(%s, %s)" (arg 0) (arg 1))
    | "cap_full_path", TCap Fs -> io_result (sprintf "Cap_fs_full_path(%s, %s)" (arg 0) (arg 1)) "r_.val"
    | "cap_is_read_only", TCap Fs -> set (sprintf "Cap_fs_is_read_only(%s)" (arg 0))
    | "cap_revocable", TCap _ ->
      line "{";
      line (sprintf "  WCap *k_; WCap *c_ = Cap_revocable(%s, &k_);" (arg 0));
      line (sprintf "  %s = (%s){ c_, k_ };" dst (cname c t));
      line "}"
    | "cap_revoke", TCap Revoker -> line (sprintf "Cap_revoke(%s);" (arg 0)); set "0"
    | "cap_is_revoked", TCap Revoker -> set (sprintf "Cap_is_revoked(%s)" (arg 0))
    | _ ->
      ignore sp;
      failwith (sprintf "cgen: unknown primitive %s on %s" name (show rt)))

(* ---------- statements ---------- *)

(* ---------- owned locals: unwinding drops them after a panic ---------- *)

(* A local that owns memory gets a flag (does it still own its value?) and a
   cleanup that drops it when the flag is set as its scope is left. Normal
   code clears the flag before moving or dropping the value, so the cleanup
   only acts during unwinding. *)
(* A group is left out: unwinding must not free it while its tasks run. It
   stays on the boundary's list, and w_catch_release waits for them first. *)
let owns_memory _c t = not (is_scalar t) && (match t with TGroup _ -> false | _ -> true)

(* A `mut` parameter is never volatile: its pointer never changes, and
   writes through it reach memory before any call that can panic. *)
let vol c (v : I.var) = if List.mem v.id c.vols && not v.by_ref then " volatile" else ""

(* A `mut` parameter has no cleanup: the caller's place owns the value. *)
let cell_decl c line (v : I.var) on =
  if owns_memory c v.ty && not v.by_ref then begin
    if not (List.exists (fun t -> cname c t = cname c v.ty) c.cell_tys) then c.cell_tys <- c.cell_tys @ [ v.ty ];
    line (sprintf "uint8_t%s %s_o = %d;" (vol c v) (var v) (if on then 1 else 0));
    line (sprintf "WCl %s_cl __attribute__((cleanup(w_cl_run))) = { (void*)&%s, (uint8_t*)&%s_o, w_dp_%s };" (var v) (var v) (var v) (cname c v.ty))
  end

let set_owned c line (v : I.var) on = if owns_memory c v.ty && not v.by_ref then line (sprintf "%s_o = %d;" (var v) (if on then 1 else 0))

(* clear the flags of variables an rhs moves out of, before it runs *)
let release_moved c line atoms =
  List.iter (function I.V (v : I.var) -> set_owned c line v false | _ -> ()) atoms

(* The value of a pure call kept across a loop (Pure.plan): the first time,
   make the call and keep a reference in its slot; after that, take the
   kept value and release what the call would have consumed. *)
let kept_or c out ind (v : I.var) r =
  let line s = Buffer.add_string out (ind ^ s ^ "\n") in
  match Hashtbl.find_opt c.plan.kept v.id with
  | None ->
    release_moved c line (I.consumed r);
    rhs_into c out ind (var v) v.ty r
  | Some sl ->
    let k = sprintf "k%d_" sl.sid in
    line (sprintf "if (__builtin_expect(%s_o, 1)) {" k);
    List.iter
      (function
        | I.V (x : I.var) when not (is_scalar x.ty) ->
          set_owned c (fun s -> line ("  " ^ s)) x false;
          line (sprintf "  %s_drop(%s);" (cname c x.ty) (var x))
        | _ -> ())
      (I.consumed r);
    line (sprintf "  %s = %s;" (var v) (dup_of c v.ty k));
    line "} else {";
    release_moved c (fun s -> line ("  " ^ s)) (I.consumed r);
    rhs_into c out (ind ^ "  ") (var v) v.ty r;
    line (sprintf "  %s = %s; %s_o = 1;" k (dup_of c v.ty (var v)) k);
    line "}"

let scope_counter = ref 0

(* `#line N "file"` for where [sp] starts, so a debugger, the sanitizers
   and crash traces show the Limen line. The line starts of each file are
   found once. *)
let line_starts : (string, int array) Hashtbl.t = Hashtbl.create 8

let line_directive (sp : Span.t) =
  let starts =
    match Hashtbl.find_opt line_starts sp.file with
    | Some a -> a
    | None ->
      let text = Option.value ~default:"" (Source.text sp.file) in
      let acc = ref [ 0 ] in
      String.iteri (fun i ch -> if ch = '\n' then acc := (i + 1) :: !acc) text;
      let a = Array.of_list (List.rev !acc) in
      Hashtbl.replace line_starts sp.file a;
      a
  in
  (* the last line start at or before sp.lo *)
  let rec find lo hi = if lo >= hi then lo else let mid = (lo + hi + 1) / 2 in if starts.(mid) <= sp.lo then find mid hi else find lo (mid - 1) in
  let ln = find 0 (Array.length starts - 1) + 1 in
  let file = String.concat "\\\\" (String.split_on_char '\\' sp.file) in
  let file = String.concat "\\\"" (String.split_on_char '"' file) in
  sprintf "#line %d \"%s\"" ln file

(* A `#line` counts on from itself: the C line after it is that line, the
   next one the line after. A statement's C takes several lines, so every
   line up to the next mark gets the mark again, and stays on the Limen
   line. A function's closing `}` (alone at the start of a line) gets
   `#line 1 "<generated>"` instead, so the code the compiler adds between
   functions is not taken for Limen lines. *)
let pin_lines text =
  if not (contains text "#line ") then text
  else begin
    let cur = ref None in
    let out =
      List.concat_map
        (fun l ->
          if String.starts_with ~prefix:"#line " l then (cur := Some l; [])
          else
            match !cur with
            | Some _ when l = "}" -> cur := None; [ "#line 1 \"<generated>\""; l ]
            | Some d -> [ d; l ]
            | None -> [ l ])
        (String.split_on_char '\n' text)
    in
    String.concat "\n" out
  end

(* A long run of tests of one integer against a literal, each failing into the
   next, as a `match` on integer literals lowers to: the variable, the
   (literal, body) arms in order, and what runs when none matches. The
   chain becomes a C `switch`, which compiles to a jump table or a binary
   search, where nested `if`s are a linear scan that also runs into the C
   compiler's nesting limit after a couple of thousand arms. A `break` in
   an arm would leave the switch instead of the loop, so then it stays. *)
let switch_min = 8

let rec has_break (ss : I.stmt list) =
  List.exists
    (function
      | I.Break -> true
      | I.If (_, a, b) -> has_break a || has_break b
      | I.Guard (_, d, p) -> has_break d || has_break p
      | I.Scope (_, b) -> has_break b
      | _ -> false)
    ss

let int_test = function
  | I.Let (t, I.PatTest (I.V x, (TInt | TUInt _), { Ast.pdesc = Ast.PInt k; _ })) -> Some (t, x, k)
  | _ -> None

let int_chain (ss : I.stmt list) =
  (* what comes around the next test writes no C: a development build's
     line mark, or the end of a scalar such as the last test's flag *)
  let quiet = function I.Line _ | I.Drop { ty = TInt | TFloat | TBool | TUnit | TUInt _; _ } -> true | _ -> false in
  let rec go (x : I.var) acc ss =
    match List.filter (fun s -> not (quiet s)) ss with
    | [ l; I.If (I.V t', a, b) ] -> (
      match int_test l with
      | Some (t, x', k) when t.id = t'.id && x'.id = x.id -> go x ((k, a) :: acc) b
      | _ -> (List.rev acc, ss))
    | _ -> (List.rev acc, ss)
  in
  match ss with
  | l :: I.If (I.V t', a, b) :: rest -> (
    match int_test l with
    | Some (t, x, k) when t.id = t'.id ->
      let arms, default = go x [ (k, a) ] b in
      let ks = List.map fst arms in
      (* a repeated literal (its later arm never runs) would be a repeated
         C case: such a match stays as it is *)
      if
        List.length arms >= switch_min
        && List.length (List.sort_uniq compare ks) = List.length ks
        && not (List.exists (fun (_, a) -> has_break a) arms || has_break default)
      then Some (x, arms, default, rest)
      else None
    | _ -> None)
  | _ -> None

(* An `else if` chain is written flat, `} else if (...) {` link after
   link, so its C nests no deeper however long it is (older C compilers
   stop at 256 brackets). A link's `else` holds the statements that work out
   its test and then the `if` on it; those statements go into the test as
   a statement expression, `} else if (({ ...; t; })) {`. *)

(* Every variable [ss] writes C for, to [k]: the end of a scalar writes
   none. *)
let rec iter_uses k (ss : I.stmt list) =
  let atoms xs = List.iter (fun a -> Option.iter (fun (v : I.var) -> k v.id) (I.atom_var a)) xs in
  let rhs r = atoms (I.consumed r @ I.borrowed r) in
  List.iter
    (function
      | I.Let (v, r) | I.Init (v, r) | I.Set (v, r, _) -> k v.id; rhs r
      | I.Decl v -> k v.id
      | I.Mutate (v, steps, m) -> k v.id; atoms (I.step_atoms steps @ I.mut_atoms m)
      | I.If (cnd, a, b) -> atoms [ cnd ]; iter_uses k a; iter_uses k b
      | I.Loop b | I.Scope (_, b) -> iter_uses k b
      | I.Guard (_, h, b) -> iter_uses k h; iter_uses k b
      | I.Return a -> atoms [ a ]
      | I.Drop v -> if not (is_scalar v.ty) then k v.id
      | I.Break | I.Continue | I.GuardEnd _ | I.Line _ -> ())
    ss

(* No panic boundary (a `defer`, a deadline) in [ss]: those stay out of a
   test's statement expression. *)
let rec unguarded (ss : I.stmt list) =
  List.for_all
    (function
      | I.Guard _ | I.GuardEnd _ | I.Scope _ -> false
      | I.If (_, a, b) -> unguarded a && unguarded b
      | I.Loop b -> unguarded b
      | _ -> true)
    ss

(* The links of the chain in an `else` [b]: each one's test statements,
   flag and body, and the `else` after the last. A test that a `switch`
   takes (int_chain) ends the chain, and so does one declaring a variable
   that is used past the test, which a statement expression would end. *)
let else_chain (b : I.stmt list) =
  let split ss =
    (* the end of a scalar after the `if` writes no C *)
    let rec drop_quiet = function I.Drop v :: r when is_scalar v.ty -> drop_quiet r | r -> r in
    match drop_quiet (List.rev ss) with
    | (I.If (t, a, b) as s) :: pre when unguarded pre && int_chain (match pre with l :: _ -> [ l; s ] | [] -> []) = None ->
      Some (List.rev pre, t, a, b)
    | _ -> None
  in
  let rec peel acc ss = match split ss with Some (pre, t, a, b) -> peel ((pre, t, a, ss) :: acc) b | None -> (List.rev acc, ss) in
  let links, last = peel [] b in
  (* where each test's variables are declared: link k *)
  let decl = Hashtbl.create 16 in
  List.iteri (fun k (pre, _, _, _) -> List.iter (function I.Let (v, _) | I.Decl v -> Hashtbl.replace decl v.id k | _ -> ()) pre) links;
  let cut = ref (List.length links) in
  let past k = iter_uses (fun id -> match Hashtbl.find_opt decl id with Some j when j < k -> cut := min !cut j | _ -> ()) in
  List.iteri (fun k (pre, t, a, _) -> past k (I.If (t, [], []) :: pre); past (k + 1) a) links;
  past max_int last;
  if !cut = List.length links then (List.map (fun (pre, t, a, _) -> (pre, t, a)) links, last)
  else
    let keep = List.filteri (fun k _ -> k < !cut) links in
    let _, _, _, rest = List.nth links !cut in
    (List.map (fun (pre, t, a, _) -> (pre, t, a)) keep, rest)

let rec stmts c out ind (ss : I.stmt list) =
  match int_chain ss with
  | Some (x, arms, default, rest) ->
    let line s = Buffer.add_string out (ind ^ s ^ "\n") in
    line (sprintf "switch (%s) {" (var x));
    List.iter
      (fun (k, a) ->
        line (sprintf "case %s: {" (atom c (I.AInt k)));
        stmts c out (ind ^ "  ") a;
        line "  break;";
        line "}")
      arms;
    line "default: {";
    stmts c out (ind ^ "  ") default;
    line "}";
    line "}";
    stmts c out ind rest
  | None -> (
    match ss with
    | [] -> ()
    | s :: rest ->
      stmt c out ind s;
      stmts c out ind rest)

(* A loop's body: a cancellation checkpoint first, on every iteration
   (W_CHECK in the runtime), so a cancelled task stops in code that never waits. *)
and loop_body c out ind (ss : I.stmt list) =
  Buffer.add_string out (ind ^ "W_CHECK();\n");
  stmts c out ind ss

and stmt c out ind (s : I.stmt) =
  let line s = Buffer.add_string out (ind ^ s ^ "\n") in
  match s with
  | I.Let (v, r) ->
    line (sprintf "%s%s %s;" (ctype c v.ty) (vol c v) (var v));
    cell_decl c line v false;
    kept_or c out ind v r;
    set_owned c line v true
  | I.Decl v ->
    line (sprintf "%s%s %s;" (ctype c v.ty) (vol c v) (var v));
    cell_decl c line v false
  | I.Init (v, r) ->
    kept_or c out ind v r;
    set_owned c line v true
  | I.Set (v, r, flag) ->
    release_moved c line (I.consumed r);
    line "{";
    line (sprintf "  %s n_;" (ctype c v.ty));
    rhs_into c out (ind ^ "  ") "n_" v.ty r;
    if !flag && not (is_scalar v.ty) then line (sprintf "  %s_drop(%s);" (cname c v.ty) (var v));
    line (sprintf "  %s = n_;" (var v));
    line "}";
    set_owned c line v true
  | I.Mutate (v, steps, m) ->
    release_moved c line (I.mut_atoms m);
    line "{";
    let p, t = place_ptr c line "p" v steps in
    (match m with
     | I.MSet x ->
       if not (is_scalar t) then line (sprintf "  %s_drop(*%s);" (cname c t) p);
       line (sprintf "  *%s = %s;" p (atom c x))
     | I.MPush x ->
       let unchecked = steps = [] && List.mem v.id c.uniq in
       line (sprintf "  %s_push%s(%s, %s);" (cname c t) (if unchecked then "_u" else "") p (atom c x))
     | I.MPut (k, x) -> line (sprintf "  %s_put(%s, %s, %s);" (cname c t) p (atom c k) (atom c x))
     | I.MRemove k -> line (sprintf "  %s_remove(%s, %s);" (cname c t) p (atom c k)));
    line "}"
  | I.If (cond, a, b) ->
    line (sprintf "if (%s) {" (atom c cond));
    stmts c out (ind ^ "  ") a;
    let links, last = else_chain b in
    List.iter
      (fun (pre, t, a) ->
        if pre = [] then line (sprintf "} else if (%s) {" (atom c t))
        else begin
          line "} else if (({";
          stmts c out (ind ^ "    ") pre;
          line (sprintf "    %s; })) {" (atom c t))
        end;
        stmts c out (ind ^ "  ") a)
      links;
    if last <> [] then begin
      line "} else {";
      stmts c out (ind ^ "  ") last
    end;
    line "}"
  | I.Loop body when List.exists (fun (b, _) -> b == body) c.plan.at_loop && not (List.memq body c.slotted) ->
    (* Slots for the pure calls kept across this loop: filled the first
       time each call runs, released when the loop is left by any way. *)
    let slots = snd (List.find (fun (b, _) -> b == body) c.plan.at_loop) in
    c.slotted <- body :: c.slotted;
    line "{";
    List.iter
      (fun (sl : Pure.slot) ->
        let k = sprintf "k%d_" sl.sid in
        line (sprintf "  %s %s; uint8_t %s_o = 0;" (ctype c sl.sty) k k);
        if owns_memory c sl.sty then begin
          if not (List.exists (fun t -> cname c t = cname c sl.sty) c.cell_tys) then c.cell_tys <- c.cell_tys @ [ sl.sty ];
          line (sprintf "  WCl %s_cl __attribute__((cleanup(w_cl_run))) = { (void*)&%s, (uint8_t*)&%s_o, w_dp_%s };" k k k (cname c sl.sty))
        end)
      slots;
    stmt c out (ind ^ "  ") s;
    c.slotted <- List.tl c.slotted;
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
       loop_body c out (ind ^ "    ") body;
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
         loop_body c out (ind ^ "    ") body;
         line "  }";
         c.inrange <- saved_r;
         line "} else {";
         line "  for (;;) {";
         loop_body c out (ind ^ "    ") body;
         line "  }";
         line "}"
       | None ->
         line "for (;;) {";
         loop_body c out (ind ^ "  ") body;
         line "}"));
    c.uniq <- saved
  | I.Break -> line "break;"
  | I.Continue -> line "continue;"
  | I.Return x ->
    release_moved c line [ x ];
    line (sprintf "return %s;" (atom c x))
  | I.Drop v ->
    if not (is_scalar v.ty) then begin
      set_owned c line v false;
      line (sprintf "%s_drop(%s);" (cname c v.ty) (var v))
    end
  | I.Guard (n, handler, body) ->
    (* A defer. The rest of its block runs under a panic boundary; normal
       exits pop it (GuardEnd) and run the deferred body themselves. A panic
       or a cancellation lands here (the guard cleanup is declared after this
       frame's other locals, so it jumps back before they are touched), runs
       the deferred body, and passes the panic or the cancellation on. What
       the rest allocated stays with the enclosing boundary: an outer
       variable may now point at it. *)
    let g = sprintf "g%d_" n in
    line "{";
    line (sprintf "  WCatch %s;" g);
    line (sprintf "  w_catch_init(&%s);" g);
    line (sprintf "  WCatch **%sslot = w_catch_slot();" g);
    line (sprintf "  if (_setjmp(%s.jb) == 0) {" g);
    line (sprintf "    WCatch *%sland __attribute__((cleanup(w_catch_land))) = &%s;" g g);
    line (sprintf "    if (%sslot) *%sslot = &%s;" g g g);
    stmts c out (ind ^ "    ") body;
    line "  } else {";
    line (sprintf "    w_catch_end(&%s);" g);
    line (sprintf "    int cancel_ = %s.cancel;" g);
    line "    char m_[1024] = \"\", w_[512] = \"\";";
    line (sprintf "    if (!cancel_) { snprintf(m_, sizeof m_, \"%%s\", %s.msg); snprintf(w_, sizeof w_, \"%%s\", %s.where); free(%s.msg); free(%s.where); }" g g g g);
    stmts c out (ind ^ "    ") handler;
    line "    if (cancel_) w_cancel_unwind();";
    line "    w_panic_c(m_, w_);";
    line "  }";
    line "}"
  | I.Scope (g, body) ->
    (* A group body under its deadline: the deadline cancels it at its next
       wait, like a task, and it unwinds to this boundary; then the join
       after it goes on. A panic, or a cancellation from further out, passes
       on (w_scope_land). *)
    incr scope_counter;
    let s = sprintf "s%d_" !scope_counter in
    line "{";
    line (sprintf "  WScope %ssc; w_scope_push(&%ssc, %s);" s s (atom c g));
    line (sprintf "  WCatch %s;" s);
    line (sprintf "  w_catch_init(&%s);" s);
    line (sprintf "  WCatch **%sslot = w_catch_slot();" s);
    line (sprintf "  if (_setjmp(%s.jb) == 0) {" s);
    line (sprintf "    WCatch *%sland __attribute__((cleanup(w_catch_land))) = &%s;" s s);
    line (sprintf "    *%sslot = &%s;" s s);
    stmts c out (ind ^ "    ") body;
    line (sprintf "    *%sslot = %s.prev;" s s);
    line (sprintf "    w_catch_end(&%s);" s);
    line (sprintf "  } else w_scope_land(&%s, &%ssc);" s s);
    line (sprintf "  w_scope_pop(&%ssc);" s);
    line "}"
  | I.GuardEnd n ->
    let g = sprintf "g%d_" n in
    line (sprintf "if (%sslot) *%sslot = %s.prev;" g g g);
    line (sprintf "w_catch_end(&%s);" g)
  | I.Line sp -> Buffer.add_string out (line_directive sp ^ "\n")

let signature c (f : I.fn) =
  let ps =
    List.map
      (fun (v : I.var) -> if v.by_ref then sprintf "%s* %s" (ctype c v.ty) (var_name v) else sprintf "%s%s %s" (ctype c v.ty) (vol c v) (var v))
      f.params
  in
  let ps = if f.is_closure then "WClo* self_" :: ps else ps in
  sprintf "static %s %s(%s)" (ctype c f.ret) f.fname (if ps = [] then "void" else String.concat ", " ps)

(* The variables a guard needs volatile (see ctx.vols): those its deferred
   body uses, and those declared outside its guarded statements that those
   statements use (a write there changes a value or an owned flag that the
   frame's cleanups read after the longjmp). Variables declared inside the
   guarded statements are cleaned up by unwinding itself, before the jump. *)
let guard_vols (body : I.stmt list) =
  let module S = Set.Make (Int) in
  let atoms_ids xs = List.filter_map (fun a -> Option.map (fun (v : I.var) -> v.id) (I.atom_var a)) xs in
  let rhs_ids r = atoms_ids (I.consumed r @ I.borrowed r) in
  let step_ids steps = atoms_ids (List.filter_map (function I.PIndex (a, _) | I.PIndexU a | I.PKey (a, _) -> Some a | I.PField _ -> None) steps) in
  (* (declared, used) in a statement list *)
  let rec scan ss =
    List.fold_left
      (fun (d, u) s ->
        let d', u' = scan1 s in
        (S.union d d', S.union u u'))
      (S.empty, S.empty) ss
  and scan1 = function
    | I.Let (v, r) -> (S.singleton v.id, S.of_list (rhs_ids r))
    | I.Decl v -> (S.singleton v.id, S.empty)
    | I.Init (v, r) | I.Set (v, r, _) -> (S.empty, S.add v.id (S.of_list (rhs_ids r)))
    | I.Mutate (v, steps, m) -> (S.empty, S.add v.id (S.of_list (step_ids steps @ atoms_ids (I.mut_atoms m))))
    | I.If (cnd, a, b) ->
      let d1, u1 = scan a and d2, u2 = scan b in
      (S.union d1 d2, S.union (S.of_list (atoms_ids [ cnd ])) (S.union u1 u2))
    | I.Loop b -> scan b
    | I.Return a -> (S.empty, S.of_list (atoms_ids [ a ]))
    | I.Drop v -> (S.empty, S.singleton v.id)
    | I.Guard (_, h, b) ->
      let d1, u1 = scan h and d2, u2 = scan b in
      (S.union d1 d2, S.union u1 u2)
    | I.Scope (_, b) -> scan b
    | I.Break | I.Continue | I.GuardEnd _ | I.Line _ -> (S.empty, S.empty)
  in
  let rec walk acc ss =
    List.fold_left
      (fun acc s ->
        match s with
        | I.Guard (_, h, b) ->
          let _, hu = scan h in
          let bd, bu = scan b in
          walk (walk (S.union acc (S.union hu (S.diff bu bd))) h) b
        | I.Scope (_, b) ->
          (* the code after a stopped body reads what the body wrote *)
          let bd, bu = scan b in
          walk (S.union acc (S.diff bu bd)) b
        | I.If (_, a, b) -> walk (walk acc a) b
        | I.Loop b -> walk acc b
        | _ -> acc)
      acc ss
  in
  S.elements (walk S.empty body)

(* `--profile`: each function counts its calls and marks itself current
   (W_PROF), numbered from 1 in the order the functions are written; 0 is
   time outside them. *)
let profile = ref false
let profile_ids : (string, int) Hashtbl.t = Hashtbl.create 64

let fn_body c out (f : I.fn) =
  c.vols <- guard_vols f.body;
  c.plan <- Pure.plan c.pure f;
  Buffer.add_string out (signature c f ^ " {\n");
  let line s = Buffer.add_string out ("  " ^ s ^ "\n") in
  (* parameters are owned by the function (`mut` ones by the caller) *)
  List.iter (fun (v : I.var) -> cell_decl c line v true) f.params;
  if f.captures <> [] then begin
    Buffer.add_string out (sprintf "  struct Env_%s* env_ = (struct Env_%s*)self_;\n" f.fname f.fname);
    List.iteri
      (fun i (v : I.var) ->
        Buffer.add_string out (sprintf "  %s%s %s = %s;\n" (ctype c v.ty) (vol c v) (var v) (dup_of c v.ty (sprintf "env_->c%d" i)));
        cell_decl c line v true)
      f.captures
  end;
  (match Hashtbl.find_opt profile_ids f.fname with Some k when !profile -> Buffer.add_string out (sprintf "  W_PROF(%d);\n" k) | _ -> ());
  (* a checkpoint on entry: deep recursion stops too *)
  Buffer.add_string out "  W_CHECK();\n";
  stmts c out "  " f.body;
  Buffer.add_string out "  w_unreachable();\n}\n\n";
  c.vols <- [];
  c.plan <- Pure.no_plan ()

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

(* [flat]'s answers by C name, for the program being written *)
let flat_memo : (string, bool) Hashtbl.t = Hashtbl.create 64

let rec type_defs c (out : Buffer.t) =
  let p s = Buffer.add_string out (s ^ "\n") in
  compute_boxed c;
  let lists = List.filter (function TList _ -> true | _ -> false) c.types in
  let maps = List.filter (function TMap _ -> true | _ -> false) c.types in
  let named = List.filter (function TMap _ -> false | _ -> true) (topo c) in
  (* forward typedefs *)
  List.iter
    (fun t ->
      let n = cname c t in
      match t with
      | TList _ -> p (sprintf "typedef struct %s %s;" n n)
      | _ -> if is_boxed c t then p (sprintf "typedef struct %s_box* %s;" n n) else p (sprintf "typedef struct %s %s;" n n))
    (lists @ named);
  List.iter (fun t -> p (sprintf "typedef struct %s %s;" (cname c t) (cname c t))) maps;
  List.iter (fun t -> match t with TList e -> p (sprintf "LIMEN_LIST_STRUCT(%s, %s)" (cname c t) (ctype c e)) | _ -> ()) lists;
  List.iter
    (fun t -> match t with TMap (k, v) -> p (sprintf "LIMEN_MAP_STRUCT(%s, %s, %s)" (cname c t) (ctype c k) (ctype c v)) | _ -> ())
    maps;
  (* map prototypes: lists and structs of maps refer to them *)
  List.iter
    (fun t ->
      let n = cname c t in
      p (sprintf "static %s* %s_dup(%s* m);\nstatic void %s_drop(%s* m);\nstatic void %s_share(%s* m);\nstatic bool %s_eq(%s* a, %s* b);\nstatic void %s_show(WBuf* b, %s* m, bool top);"
           n n n n n n n n n n n n))
    maps;
  (* struct bodies *)
  List.iter
    (fun t ->
      let n = cname c t in
      match t with
      | TArray (e, TLen k) -> p (sprintf "struct %s { %s a[%d]; };" n (ctype c e) (max k 1))
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
        p (sprintf "static void %s_share(%s v);" n ct);
        if is_boxed c t then p (sprintf "static void %s_kids(void* o);" n))
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
            if boxed then p (sprintf "  %s r = (%s)w_alloc_k(sizeof(struct %s_box), WK_PLAIN); r->rc = 1; w_set_kids(r, %s_kids);" n n n n)
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
        p (sprintf "LIMEN_LIST_FUNCS(%s, %s, %s)" (cname c t) (ctype c e) (cname c e));
        if e = TFloat then p (sprintf "LIMEN_LIST_SORT(%s, double, Float)" (cname c t))
      | _ -> ())
    (List.stable_sort (fun a b -> compare (depth a) (depth b)) lists);
  List.iter (fun t -> match t with TMap (k, _) -> ignore (hash_prefix c k) | _ -> ()) maps;
  List.iter (fun t -> p (sprintf "static uint64_t %s_hash(%s v);" (cname c t) (ctype c t))) c.hash_tys;
  List.iter
    (fun t ->
      match t with
      | TMap (k, v) -> p (sprintf "LIMEN_MAP_FUNCS(%s, %s, %s, %s, %s)" (cname c t) (ctype c k) (cname c k) (ctype c v) (cname c v))
      | _ -> ())
    maps;
  (* dup/drop/eq/show definitions *)
  List.iter
    (fun t ->
      let n = cname c t and ct = ctype c t in
      let boxed = is_boxed c t in
      let acc = if boxed then "->" else "." in
      let name = named_decl c t in
      match t with
      | TArray (e, TLen k) ->
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
        if boxed then begin
          p (sprintf "static void %s_kids(void* o) {" n);
          p (sprintf "  %s v = (%s)o;" n n);
          p (sprintf "  switch (v->tag) {");
          cases (fun _ k i pt -> if not (is_scalar pt) then p (sprintf "      %s_drop(v->u.v%d.f%d);" (cname c pt) k i));
          p "  }";
          p "}"
        end;
        p (sprintf "static void %s_drop(%s v) {" n ct);
        if boxed then begin
          (* through w_release, which frees a long chain without recursing *)
          p "  if (!w_rc_dec(&v->rc)) return;";
          p "  w_release(v);"
        end
        else begin
          p (sprintf "  switch (v%stag) {" acc);
          cases (fun _ k i pt -> if not (is_scalar pt) then p (sprintf "      %s_drop(v%su.v%d.f%d);" (cname c pt) acc k i));
          p "  }"
        end;
        p "}";
        (* A boxed enum's share, == and show go through w_share, w_eq and
           w_show, which recurse only so deep and keep the rest on a stack *)
        if boxed then begin
          p (sprintf "static void %s_share_kids(void* o) {" n);
          p (sprintf "  %s v = (%s)o;" n n)
        end
        else p (sprintf "static void %s_share(%s v) {" n ct);
        p (sprintf "  switch (v%stag) {" acc);
        cases (fun _ k i pt -> if not (is_scalar pt) then p (sprintf "      %s_share(v%su.v%d.f%d);" (cname c pt) acc k i));
        p "  }";
        p "}";
        if boxed then begin
          p (sprintf "static void %s_share(%s v) {" n ct);
          p (sprintf "  if (w_rc_share(&v->rc)) w_share(v, %s_share_kids);" n);
          p "}";
          p (sprintf "static bool %s_eq_kids(void* pa, void* pb, int64_t i) {" n);
          p (sprintf "  %s a = (%s)pa, b = (%s)pb;" n n n);
          p "  (void)i;"
        end
        else p (sprintf "static bool %s_eq(%s a, %s b) {" n ct ct);
        p (sprintf "  if (a%stag != b%stag) return false;" acc acc);
        p (sprintf "  switch (a%stag) {" acc);
        cases (fun _ k i pt ->
            p (sprintf "      if (!(%s)) return false;" (binop c Ast.Eq pt (sprintf "a%su.v%d.f%d" acc k i) (sprintf "b%su.v%d.f%d" acc k i) Span.dummy)));
        p "  }";
        p "  return true;\n}";
        if boxed then begin
          p (sprintf "static bool %s_eq(%s a, %s b) {" n ct ct);
          p (sprintf "  return w_eq(%s_eq_kids, a, b, 0);" n);
          p "}";
          p (sprintf "static void %s_show_kids(WBuf* b, void* o, int64_t i) {" n);
          p (sprintf "  %s v = (%s)o;" n n);
          p "  (void)i;"
        end
        else begin
          p (sprintf "static void %s_show(WBuf* b, %s v, bool top) {" n ct);
          p "  (void)top;"
        end;
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
        p "  }\n}";
        if boxed then begin
          p (sprintf "static void %s_show(WBuf* b, %s v, bool top) {" n ct);
          p "  (void)top;";
          p (sprintf "  w_show(b, %s_show_kids, v, 0);" n);
          p "}"
        end
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
        let tuple = Ast.is_tuple_name name in
        p (sprintf "  buf_puts(b, %s);" (c_string (if tuple then "(" else name ^ " { ")));
        List.iteri
          (fun i (fname, ft) ->
            p (sprintf "  buf_puts(b, %s);" (c_string ((if i > 0 then ", " else "") ^ if tuple then "" else fname ^ ": ")));
            p (sprintf "  %s_show(b, v.f%d, false);" (cname c ft) i))
          fs;
        p (if tuple then "  buf_puts(b, \")\");\n}" else "  buf_puts(b, \" }\");\n}")
      end)
    named
  ;
  cmp_defs c p

(* Order functions: item by item, as Interp.compare_v; and hash functions
   for tuple, struct and enum map keys. A type is flat when no enum that
   holds itself is inside it, so its values are only so deep: its order and
   hash are plain recursion (worked out once per type). Any other goes
   through w_cmp and w_hash
   (limen_rt.h): a list or boxed enum orders from item i on, so it can go on
   the stack and resume there, and a key hashes from part i on. *)
and flat c t =
  let rec go seen t =
    match t with
    | TInt | TFloat | TStr | TBytes | TBool | TUnit | TUInt _ -> true
    | TList e | TArray (e, _) -> go seen e
    | TNamed _ -> (
      let n = cname c t in
      match Hashtbl.find_opt flat_memo n with
      | Some b -> b
      | None ->
        if List.mem n seen then false
        else begin
          (* a false answer found on the way is true of every type on it *)
          let seen = n :: seen in
          let r =
            (not (is_boxed c t))
            &&
            if is_enum c t then List.for_all (fun (_, ps) -> List.for_all (go seen) ps) (Lower.variants c.g t)
            else List.for_all (fun (_, ft) -> go seen ft) (Lower.field_types c.g t)
          in
          Hashtbl.replace flat_memo n r;
          r
        end)
    | _ -> true
  in
  go [] t

and cmp_defs c p =
  List.iter (fun t -> if not (flat c t) then p (sprintf "static uint64_t %s_hash_k(void* o, int64_t i, uint64_t h);" (cname c t))) c.hash_tys;
  List.iter
    (fun t ->
      let n = cname c t and ct = ctype c t in
      p (sprintf "static uint64_t %s_hash(%s v) {" n ct);
      if flat c t then begin
        let item e x = p (sprintf "  h = w_hash_mix(h, %s_hash(%s));" (hash_prefix c e) x) in
        p "  uint64_t h = 0x2545F4914F6CDD1DULL; (void)v;";
        (match t with
         | TNamed _ when is_enum c t ->
           let acc = if is_boxed c t then "->" else "." in
           p (sprintf "  h = w_hash_mix(h, Int_hash(v%stag));" acc);
           p (sprintf "  switch (v%stag) {" acc);
           List.iteri
             (fun k (_, ps) ->
               p (sprintf "    case %d:" k);
               List.iteri (fun i pt -> item pt (sprintf "v%su.v%d.f%d" acc k i)) ps;
               p "      break;")
             (Lower.variants c.g t);
           p "  }"
         | _ -> List.iteri (fun i (_, ft) -> item ft (sprintf "v.f%d" i)) (Lower.field_types c.g t));
        p "  return h;";
        p "}"
      end
      else begin
        let boxed = is_boxed c t in
        p (sprintf "  return w_hash_outer(%s_hash_k, %s);" n (if boxed then "v" else "&v"));
        p "}"
      end)
    c.hash_tys;
  (* the kids of the keys that are not flat, after every _hash *)
  List.iter
    (fun t ->
      if not (flat c t) then begin
        let n = cname c t and ct = ctype c t in
        let boxed = is_boxed c t in
        (* part j: a flat one is hashed at once; any other through w_hash, and
           if it went on the stack, this call waits for it there *)
        let item j e x =
          if flat c e then p (sprintf "  if (i <= %d) h = w_hash_mix(h, %s_hash(%s));" j (hash_prefix c e) x)
          else begin
            let hp = hash_prefix c e in
            p (sprintf "  if (i <= %d) {" j);
            p (sprintf "    uint64_t k_ = w_hash(%s_hash_k, %s%s);" hp (if is_boxed c e then "" else "&") x);
            p (sprintf "    if (w_hash_susp) { w_hash_wait(%s_hash_k, o, %d, h); return 0; }" n j);
            p "    h = w_hash_mix(h, k_);";
            p "  }"
          end
        in
        p (sprintf "static uint64_t %s_hash_k(void* o, int64_t i, uint64_t h) {" n);
        if boxed then p (sprintf "  %s v = (%s)o;" ct ct) else p (sprintf "  %s* v = (%s*)o;" ct ct);
        (match t with
         | TNamed _ when is_enum c t ->
           p "  if (i <= 0) h = w_hash_mix(h, Int_hash(v->tag));";
           p "  switch (v->tag) {";
           List.iteri
             (fun k (_, ps) ->
               p (sprintf "    case %d:" k);
               List.iteri (fun i pt -> item (i + 1) pt (sprintf "v->u.v%d.f%d" k i)) ps;
               p "      break;")
             (Lower.variants c.g t);
           p "  }"
         | _ -> List.iteri (fun i (_, ft) -> item i ft (sprintf "v->f%d" i)) (Lower.field_types c.g t));
        p "  return h;";
        p "}"
      end)
    c.hash_tys;
  List.iter (fun t -> p (sprintf "static int %s_cmp(%s a, %s b);" (cname c t) (ctype c t) (ctype c t))) c.cmp_tys;
  List.iter
    (fun t ->
      let n = cname c t and ct = ctype c t in
      let fl = flat c t in
      (* a difference: as it is in a flat type, else through w_cmp_res *)
      let ret = if fl then "r_" else "w_cmp_res(r_)" in
      let item e x y = p (sprintf "  { int r_ = %s_cmp(%s, %s); if (r_) return %s; }" (cmp_prefix c e) x y ret) in
      (* item j of a list or boxed enum, inside a block: when it put
         something on the stack, the rest goes on after it, from item next *)
      let cursor e x y next =
        p (sprintf "    int r_ = %s_cmp(%s, %s); if (r_) return w_cmp_res(r_);" (cmp_prefix c e) x y);
        match next with
        | Some j when not (flat c e) -> p (sprintf "    if (w_cmp_n != m_) { w_cmp_push(%s_cmp_k, a, b, %s); return 0; }" n j)
        | _ -> ()
      in
      let mark e next = if next <> None && not (flat c e) then p "    int64_t m_ = w_cmp_n;" in
      match t with
      | TList e when not fl ->
        p (sprintf "static int %s_cmp_k(void* pa, void* pb, int64_t i) {" n);
        p (sprintf "  %s a = (%s)pa;" ct ct);
        p (sprintf "  %s b = (%s)pb;" ct ct);
        p "  int64_t n_ = a->len < b->len ? a->len : b->len;";
        p "  for (; i < n_; i++) {";
        mark e (Some "i + 1");
        cursor e "a->data[i]" "b->data[i]" (Some "i + 1");
        p "  }";
        p "  return a->len == b->len ? 0 : w_cmp_res(a->len < b->len ? -1 : 1);";
        p "}";
        p (sprintf "static int %s_cmp(%s a, %s b) { return w_cmp(%s_cmp_k, a, b, 0); }" n ct ct n)
      | TNamed _ when is_enum c t && is_boxed c t && not fl ->
        p (sprintf "static int %s_cmp_k(void* pa, void* pb, int64_t i) {" n);
        p (sprintf "  %s a = (%s)pa;" ct ct);
        p (sprintf "  %s b = (%s)pb;" ct ct);
        p "  if (a->tag != b->tag) return w_cmp_res(a->tag < b->tag ? -1 : 1);";
        p "  switch (a->tag) {";
        List.iteri
          (fun k (_, ps) ->
            p (sprintf "    case %d:" k);
            let last = List.length ps - 1 in
            List.iteri
              (fun i pt ->
                let next = if i < last then Some (string_of_int (i + 1)) else None in
                p (sprintf "  if (i <= %d) {" i);
                mark pt next;
                cursor pt (sprintf "a->u.v%d.f%d" k i) (sprintf "b->u.v%d.f%d" k i) next;
                p "  }")
              ps;
            p "      break;")
          (Lower.variants c.g t);
        p "  }";
        p "  return 0;";
        p "}";
        p (sprintf "static int %s_cmp(%s a, %s b) { return w_cmp(%s_cmp_k, a, b, 0); }" n ct ct n)
      | _ ->
      p (sprintf "static int %s_cmp(%s a, %s b) {" n ct ct);
      p "  (void)a; (void)b;";
      (match t with
       | TList e ->
         p "  int64_t n_ = a->len < b->len ? a->len : b->len;";
         p "  for (int64_t i = 0; i < n_; i++)";
         item e "a->data[i]" "b->data[i]";
         p "  return a->len < b->len ? -1 : a->len > b->len;"
       | TArray (e, TLen k) ->
         p (sprintf "  for (int64_t i = 0; i < %d; i++)" k);
         item e "a.a[i]" "b.a[i]";
         p "  return 0;"
       | TNamed _ when is_enum c t ->
         let acc = if is_boxed c t then "->" else "." in
         p (sprintf "  if (a%stag != b%stag) return %s;" acc acc (if fl then sprintf "a%stag < b%stag ? -1 : 1" acc acc else sprintf "w_cmp_res(a%stag < b%stag ? -1 : 1)" acc acc));
         p (sprintf "  switch (a%stag) {" acc);
         List.iteri
           (fun k (_, ps) ->
             p (sprintf "    case %d:" k);
             List.iteri (fun i pt -> item pt (sprintf "a%su.v%d.f%d" acc k i) (sprintf "b%su.v%d.f%d" acc k i)) ps;
             p "      break;")
           (Lower.variants c.g t);
         p "  }";
         p "  return 0;"
       | _ ->
         List.iteri (fun i (_, ft) -> item ft (sprintf "a.f%d" i) (sprintf "b.f%d" i)) (Lower.field_types c.g t);
         p "  return 0;");
      p "}")
    c.cmp_tys;
  List.iter
    (fun t -> match t with TList e -> p (sprintf "LIMEN_LIST_SORT(%s, %s, %s)" (cname c t) (ctype c e) (cmp_prefix c e)) | _ -> ())
    c.sort_tys

(* ---------- program ---------- *)

let program (g : Lower.gctx) (prog : I.program) : string =
  uses_tls := false;
  Hashtbl.reset flat_memo;
  let c =
    { g; names = Hashtbl.create 32; types = []; lits = Hashtbl.create 32; lit_order = []; boxed = [];
      envs = Hashtbl.create 16; uniq = []; task_tys = []; inrange = []; chan_tys = []; dyns = []; cell_tys = []; vols = [];
      cmp_tys = []; sort_tys = []; hash_tys = []; csyms = []; sql_api = false; cstructs = []; cbs = []; kepts = []; bjobs = [];
      pure = Pure.analyse prog; plan = Pure.no_plan (); slotted = [] }
  in
  (* bodies first: this discovers every type and literal *)
  let bodies = Buffer.create 65536 in
  Hashtbl.reset profile_ids;
  if !profile then List.iteri (fun i (f : I.fn) -> Hashtbl.replace profile_ids f.fname (i + 1)) prog.fns;
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
  let dynbuf = Buffer.create 1024 in
  (* drop-through-pointer functions for owned locals *)
  List.iter
    (fun t -> Buffer.add_string dynbuf (sprintf "static void w_dp_%s(void* p) { %s_drop(*(%s*)p); }\n" (cname c t) (cname c t) (ctype c t)))
    c.cell_tys;
  List.iter
    (fun (key, t, ms) ->
      let ct = ctype c t and cn = cname c t in
      let p = sprintf "((struct Dyn_%s*)d)->val" key in
      Buffer.add_string dynbuf (sprintf "struct Dyn_%s { WDyn base; %s val; };\n" key ct);
      Buffer.add_string dynbuf (sprintf "static void Dyn_%s_drop(WDyn* d) { %s }\n" key (if is_scalar t then "(void)d;" else sprintf "%s_drop(%s);" cn p));
      Buffer.add_string dynbuf (sprintf "static void Dyn_%s_share(WDyn* d) { %s }\n" key (if is_scalar t then "(void)d;" else sprintf "%s_share(%s);" cn p));
      Buffer.add_string dynbuf (sprintf "static void Dyn_%s_show(WBuf* b, WDyn* d, bool top) { %s_show(b, %s, top); }\n" key cn p);
      List.iteri
        (fun k (fn, ps, r) ->
          let params = List.mapi (fun j pt -> sprintf "%s a%d" (ctype c pt) j) ps in
          let self = if is_scalar t then p else sprintf "%s_dup(%s)" cn p in
          Buffer.add_string dynbuf
            (sprintf "static %s Dyn_%s_m%d(%s) { return %s(%s); }\n" (ctype c r) key k
               (String.concat ", " ("WDyn* d" :: params)) fn
               (String.concat ", " (self :: List.mapi (fun j _ -> sprintf "a%d" j) ps))))
        ms;
      Buffer.add_string dynbuf
        (sprintf "static const struct { WVtHead h; void *m[%d]; } Dyn_%s_vt = { { Dyn_%s_drop, Dyn_%s_share, Dyn_%s_show }, { %s } };\n"
           (max 1 (List.length ms)) key key key key
           (if ms = [] then "NULL" else String.concat ", " (List.mapi (fun k _ -> sprintf "(void*)Dyn_%s_m%d" key k) ms))))
    c.dyns;
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
        (sprintf "static void wtask_%s(WTask* t) {\n  %s r = ((%s (*)(WClo*))t->fn->fn)(t->fn);\n  %s\n  memcpy(t->slot, &r, sizeof r);\n  Clo_drop(t->fn);\n}\n"
           (cname c t) (ctype c t) (ctype c t) (if is_scalar t then "" else sprintf "%s_share(r);" (cname c t))))
    c.task_tys;
  let main = Buffer.create 1024 in
  let mp s = Buffer.add_string main (s ^ "\n") in
  let rt = prog.main_ret in
  if prog.tests <> [] || prog.benches <> [] then begin
    (* test binary: `PROGRAM N` runs test N (each test in its own process);
       `PROGRAM bench N NS ITERS` runs bench N, ITERS times, or (ITERS = 0)
       as many times as fit in NS nanoseconds, and reports on stderr *)
    let cap_args fields = String.concat ", " (List.map (sprintf "Cap_root_field(NULL, %S)") fields) in
    mp "static int wtest_index_, wtest_bench_;";
    mp "static int64_t wbench_budget_, wbench_fixed_;";
    if prog.benches <> [] then begin
      mp "static WCap* wbench_caps_[2];";
      List.iteri
        (fun i (t, fields) ->
          mp (sprintf "static void wbench_%d_(void) {" i);
          List.iteri (fun k _ -> mp (sprintf "  Cap_dup(wbench_caps_[%d]);" k)) fields;
          mp (sprintf "  %s(%s);" t (String.concat ", " (List.mapi (fun k _ -> sprintf "wbench_caps_[%d]" k) fields)));
          mp "}")
        prog.benches;
      (* Go's scaling: run once, then grow the count toward the time budget,
         at most 100 times per round, until one round fills the budget *)
      mp "static void wbench_loop_(void (*body)(void)) {";
      mp "  int64_t n = wbench_fixed_ > 0 ? wbench_fixed_ : 1;";
      mp "  for (;;) {";
      mp "    int64_t a0 = __atomic_load_n(&w_allocs, __ATOMIC_RELAXED);";
      mp "    int64_t t0 = w_mono_ns();";
      mp "    for (int64_t i = 0; i < n; i++) body();";
      mp "    int64_t ns = w_mono_ns() - t0;";
      mp "    int64_t al = __atomic_load_n(&w_allocs, __ATOMIC_RELAXED) - a0;";
      mp "    if (wbench_fixed_ > 0 || ns >= wbench_budget_ || n >= 1000000000) {";
      mp "      fflush(stdout);";
      mp "      fprintf(stderr, \"limen-bench: %lld %lld %lld\\n\", (long long)n, (long long)ns, (long long)al);";
      mp "      return;";
      mp "    }";
      mp "    int64_t next = ns > 0 ? (int64_t)(1.2 * (double)wbench_budget_ * (double)n / (double)ns) : n * 100;";
      mp "    if (next > n * 100) next = n * 100;";
      mp "    if (next <= n) next = n + 1;";
      mp "    if (next > 1000000000) next = 1000000000;";
      mp "    n = next;";
      mp "  }";
      mp "}"
    end;
    mp "static void wtest_entry_(void* a) {";
    mp "  (void)a;";
    mp "  if (!wtest_bench_) switch (wtest_index_) {";
    List.iteri (fun i (t, fields, _) -> mp (sprintf "    case %d: %s(%s); break;" i t (cap_args fields))) prog.tests;
    mp "  }";
    if prog.benches <> [] then begin
      mp "  else switch (wtest_index_) {";
      List.iteri
        (fun i (_, fields) ->
          mp (sprintf "    case %d:" i);
          List.iteri (fun k f -> mp (sprintf "      wbench_caps_[%d] = Cap_root_field(NULL, %S);" k f)) fields;
          mp (sprintf "      wbench_loop_(wbench_%d_);" i);
          List.iteri (fun k _ -> mp (sprintf "      Cap_drop(wbench_caps_[%d]);" k)) fields;
          mp "      break;")
        prog.benches;
      mp "  }"
    end;
    mp "  w_main_finished();";
    mp "}";
    mp "int main(int argc, char** argv) {";
    mp "  if (argc < 2) return 2;";
    mp "  if (strcmp(argv[1], \"bench\") == 0) {";
    mp "    if (argc < 5) return 2;";
    mp "    wtest_bench_ = 1;";
    mp "    wtest_index_ = atoi(argv[2]);";
    mp "    wbench_budget_ = atoll(argv[3]);";
    mp "    wbench_fixed_ = atoll(argv[4]);";
    mp "    w_init(argc - 4, argv + 4);   /* then `--grant fs=DIR` for a bench that takes Fs */";
    mp "  } else {";
    mp "    wtest_index_ = atoi(argv[1]);";
    (* a test that takes `clock: virtual Clock` runs on virtual time, from before any thread starts *)
    let virtual_tests = List.concat (List.mapi (fun i (_, _, v) -> if v then [ sprintf "case %d:" i ] else []) prog.tests) in
    if virtual_tests <> [] then mp (sprintf "    switch (wtest_index_) { %s w_virtual_start(); }" (String.concat " " virtual_tests));
    mp "    w_init(argc - 1, argv + 1);   /* `--grant fs=DIR` for a test that takes Fs */";
    mp "  }";
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
  if !profile then begin
    (* a function's name without the `wf_` C names start with *)
    let shown n = if String.starts_with ~prefix:"wf_" n then String.sub n 3 (String.length n - 3) else n in
    let n = List.length prog.fns + 1 in
    mp (sprintf "static const char *const w_prof_names_[%d] = { \"(runtime and C)\"%s };" n
          (String.concat "" (List.map (fun (f : I.fn) -> sprintf ", \"%s\"" (shown f.fname)) prog.fns)));
    mp (sprintf "static int64_t w_prof_calls_[%d];" n);
    mp (sprintf "static int64_t w_prof_samples_[%d];" n)
  end;
  mp "int main(int argc, char** argv) {";
  mp "  w_init(argc, argv);";
  if !profile then mp (sprintf "  w_prof_start(w_prof_names_, w_prof_calls_, w_prof_samples_, %d);" (List.length prog.fns + 1));
  mp "  w_run(wmain_entry_, NULL);";
  mp (sprintf "  %s r_ = wmain_result_;" (ctype c rt));
  (match rt with
   | TNamed ("Result", [ _; et ]) ->
     mp (sprintf "  if (%s == 1) {" (tag_expr c rt "r_"));
     mp "    WBuf b_; buf_init(&b_); buf_puts(&b_, \"error: \");";
     (match et with
      | TDyn ("Error", []) ->
        (* a `dyn Error` is reported by its message: method 0 of its vtable *)
        let d = payload_expr c rt "r_" 1 0 in
        mp (sprintf "    WStr* m_ = ((WStr* (*)(WDyn*))(%s)->vt->m[0])(%s);" d d);
        mp "    Str_show(&b_, m_, true); Str_drop(m_);"
      | _ -> mp (sprintf "    %s_show(&b_, %s, true);" (cname c et) (payload_expr c rt "r_" 1 0)));
     mp "    buf_puts(&b_, \"\\n\"); fflush(stdout); fwrite(b_.p, 1, (size_t)b_.len, stderr); return 1;";
     mp "  }"
   | _ -> ());
  (* `main` returning Int (or Ok(Int)) chooses the exit status; the OS keeps its low 8 bits *)
  let code =
    match rt with
    | TInt -> "(int)(r_ & 255)"
    | TNamed ("Result", [ TInt; _ ]) -> sprintf "(int)(%s & 255)" (payload_expr c rt "r_" 0 0)
    | _ -> "0"
  in
  mp (sprintf "  int code_ = %s;" code);
  if not (is_scalar rt) then mp (sprintf "  %s_drop(r_);" (cname c rt)) else mp "  (void)r_;";
  mp "  fflush(stdout);";
  mp "  w_leakcheck();";
  mp "  return code_;";
  mp "}"
  end;
  (* callback wrappers, after the prototypes of the functions they call *)
  let cbdefs = String.concat "" (List.map (ffi_callback_def c) c.cbs) in
  let cbdefs = cbdefs ^ String.concat "" (List.mapi (kept_def c) c.kepts) in
  let types = Buffer.create 16384 in
  type_defs c types;
  List.iter
    (fun (_, cn, fs) ->
      Buffer.add_string types
        (sprintf "typedef struct { %s } %s;\n" (String.concat " " (List.mapi (fun i (_, t) -> sprintf "%s c%d;" (Ffi.c_name t) i) fs)) cn))
    c.cstructs;
  let lits = Buffer.create 4096 in
  List.iter
    (fun (n, s) -> Buffer.add_string lits (sprintf "static WStr %s = { 0, %d, (char*)%s };\n" n (String.length s) (c_string s)))
    c.lit_order;
  let program =
    [ "/* ---- types ---- */"; Buffer.contents types; "/* ---- literals ---- */";
      Buffer.contents lits; "/* ---- closures ---- */"; Buffer.contents envs; Buffer.contents tasks; "/* ---- functions ---- */";
      Buffer.contents protos;
      (* each foreign C function under a private name, bound to its symbol by
         an asm label, so it clashes with no header's declaration of it *)
      String.concat "" (List.map (fun sym -> sprintf "extern void vf_%s(void) __asm__(W_CSYM(\"%s\"));\n" sym sym) c.csyms)
      (* the SQLite functions limen_sql_run calls, in the order of its table *)
      ^ (if c.sql_api then
           sprintf "static const void *const w_sql_api[] = { %s };\n"
             (String.concat ", " (List.map (fun sym -> sprintf "(const void *)vf_%s" sym) Ffi.sql_syms))
         else "");
      cbdefs ^ String.concat "" c.bjobs;
      "/* ---- dyn vtables ---- */"; Buffer.contents dynbuf; Buffer.contents bodies; Buffer.contents main ]
  in
  (* Only a group with a deadline can cancel a task (the deadline cancels
     the group, and a cancelled group's tasks are cancelled with it). In a
     program with none, every checkpoint is a load of a counter that stays
     0: say so, so the C compiler drops them from loops. *)
  let never_stops =
    if List.exists (fun part -> contains part "w_group_new_until(") program then []
    else [ no_checkpoints ]
  in
  let program = if !Lower.lines then List.map pin_lines program else program in
  (if !uses_tls then tls_marker else "") ^ String.concat "\n" ((Runtime_text.text :: never_stops) @ program)
