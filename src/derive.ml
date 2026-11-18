(* `json.encode(x)` and `json.decode(s)`: the compiler writes the conversion.

   The standard json module declares both as generic functions whose bodies
   never run, so the checker types every use like any call. Once a program
   has checked, this pass finds each use, takes the type it was used at, and
   writes ordinary Limen functions for that type and every type inside it,
   into the json module: `json.to$N(x: T) -> json.Value` and
   `json.from$N(v: json.Value, at: Str) -> Result[T, Str]`, built from the
   helpers at the end of json.lmn. Each use is pointed at a wrapper,
   `json.encode$N` or `json.decode$N`, and the program is checked again with
   the new functions, so the interpreter and the C backend run them as any
   other code. `$` cannot be written in source, so the names are the
   compiler's own.

   The type must be known where `encode` or `decode` is used: a type
   parameter is E0212, since the interpreter does not instantiate generic
   functions. Types that have no JSON form (capabilities, functions,
   channels, `Bytes`, arrays, maps without `Str` keys, `dyn` values, `()`)
   are E0212 too, naming where in the type they are. *)
open Ast
open Types

let sprintf = Printf.sprintf

type st = {
  q : string; (* the json module's name in this program: "json" or "std/json" *)
  mutable sp : span; (* the use being expanded, for every node written *)
  structs : (string, struct_decl) Hashtbl.t;
  enums : (string, enum_decl) Hashtbl.t;
  names : (string, int) Hashtbl.t; (* a type, shown -> its number *)
  mutable count : int;
  mutable out : item list; (* reversed *)
  mutable diags : Diag.t list;
}

exception Unsupported of string

let ex st d = { edesc = d; espan = st.sp; ety = TUnit; einst = []; ecallee = None }
let var st n = ex st (EVar n)
let call st f args = ex st (ECall (var st f, args))
let str st s = ex st (EStr [ SPLit s ])
let jf st n = st.q ^ "." ^ n
let te st d = { tdesc = d; tspan = st.sp }
let pat st d = { pdesc = d; pspan = st.sp }
let block st stmts tail = { stmts; tail = Some tail; bspan = st.sp }
let try_ st e = ex st (ETry (e, None))

(* A type as written, for a generated function's parameters and result. *)
let rec written st t =
  match repr t with
  | TInt -> te st (TyName ("Int", []))
  | TFloat -> te st (TyName ("Float", []))
  | TBool -> te st (TyName ("Bool", []))
  | TStr -> te st (TyName ("Str", []))
  | TUnit -> te st TyUnit
  | TList a -> te st (TyName ("List", [ written st a ]))
  | TMap (k, v) -> te st (TyName ("Map", [ written st k; written st v ]))
  | TNamed (n, args) -> te st (TyName (n, List.map (written st) args))
  | t -> raise (Unsupported (show t))

(* The type of a declared field or payload, with the declaration's type
   parameters replaced by [env]. Names are qualified once modules load, and
   aliases are gone. *)
let rec field_ty env (t : ty_expr) =
  match t.tdesc with
  | TyUnit -> TUnit
  | TyName (n, []) when List.mem_assoc n env -> List.assoc n env
  | TyName ("Int", []) -> TInt
  | TyName ("Float", []) -> TFloat
  | TyName ("Bool", []) -> TBool
  | TyName ("Str", []) -> TStr
  | TyName ("Bytes", []) -> TBytes
  | TyName ("List", [ a ]) -> TList (field_ty env a)
  | TyName ("Map", [ k; v ]) -> TMap (field_ty env k, field_ty env v)
  | TyName (n, _) when List.exists (fun c -> cap_name c = n) all_caps -> raise (Unsupported (sprintf "`%s`" n))
  | TyName (("Array" | "dyn" | "Chan" | "Atomic" | "Shared" | "Group") as n, _) -> raise (Unsupported (sprintf "`%s`" n))
  | TyName (n, args) -> TNamed (n, List.map (field_ty env) args)
  | TyFn _ -> raise (Unsupported "a function")
  | TyInt _ -> raise (Unsupported "a number")

let value_ty st = TNamed (jf st "Value", [])

(* The encoder and decoder of [t], as function names: a helper of json.lmn
   for the simple types, written ones otherwise. *)
let rec coders st t : string * string =
  match repr t with
  | TInt -> (jf st "of_int", jf st "from_int")
  | TFloat -> (jf st "of_float", jf st "from_float")
  | TStr -> (jf st "of_str", jf st "from_str")
  | TBool -> (jf st "of_bool", jf st "from_bool")
  | TNamed (n, []) when n = jf st "Value" -> (jf st "of_value", jf st "from_value")
  | t -> (
    let key = show t in
    match Hashtbl.find_opt st.names key with
    | Some k -> (jf st (sprintf "to$%d" k), jf st (sprintf "from$%d" k))
    | None ->
      st.count <- st.count + 1;
      let k = st.count in
      Hashtbl.replace st.names key k;
      let to_ = jf st (sprintf "to$%d" k) and from = jf st (sprintf "from$%d" k) in
      let enc, dec = bodies st t in
      let fn name params ret body =
        IFn
          { fn_name = name; fn_span = st.sp; fn_gen = []; fn_params = params; fn_ret = Some ret; fn_body = body;
            fn_test = None; fn_bench = false; fn_virtual = false; fn_table = None; fn_pub = false; fn_bounds = [];
            fn_foreign = None }
      in
      let prm n t = { prm_name = n; prm_span = st.sp; prm_ty = t } in
      st.out <- fn from [ prm "v" (written st (value_ty st)); prm "at" (written st TStr) ]
                  (written st (TNamed ("Result", [ t; TStr ]))) dec
                :: fn to_ [ prm "x" (written st t) ] (written st (value_ty st)) enc :: st.out;
      (to_, from))

(* The bodies of [t]'s encoder (of `x`) and decoder (of `v` at `at`). *)
and bodies st t : block * block =
  let x = var st "x" and v = var st "v" and at = var st "at" in
  let fnv n = var st n in
  let enc_of t e = let e', _ = coders st t in call st e' [ e ] in
  let dec_of t v at = let _, d = coders st t in try_ st (call st d [ v; at ]) in
  let only e = block st [] e in
  let fields_obj fs = call st (jf st "of_fields") [ ex st (EList (List.map (fun (n, e) -> call st (jf st "field") [ str st n; e ]) fs)) ] in
  let list_of es = call st (jf st "of_list") [ ex st (EList es) ] in
  let ok e = call st "Ok" [ e ] in
  (* [n] values from an array of exactly [n] items at [at], decoded by
     [decs]: a `let` of the items, and the expressions *)
  let items_of n v at decs =
    let s = { sdesc = SLet (false, "items", st.sp, None, try_ st (call st (jf st "from_items") [ v; at; ex st (EInt (Int64.of_int n)) ])); sspan = st.sp } in
    let es =
      List.mapi
        (fun i t -> dec_of t (ex st (EIndex (var st "items", ex st (EInt (Int64.of_int i))))) (call st (jf st "item_at") [ at; ex st (EInt (Int64.of_int i)) ]))
        decs
    in
    (s, es)
  in
  match repr t with
  | TNamed ("Option", [ a ]) ->
    let e, d = coders st a in
    (only (call st (jf st "of_option") [ x; fnv e ]), only (call st (jf st "from_option") [ v; at; fnv d ]))
  | TList a ->
    let e, d = coders st a in
    (only (call st (jf st "of_list") [ ex st (EMethod (x, "map", st.sp, [ fnv e ])) ]), only (call st (jf st "from_list") [ v; at; fnv d ]))
  | TMap (k, a) -> (
    match repr k with
    | TStr ->
      let e, d = coders st a in
      (only (call st (jf st "of_map") [ x; fnv e ]), only (call st (jf st "from_map") [ v; at; fnv d ]))
    | k -> raise (Unsupported (sprintf "`%s` (a map's keys must be `Str`)" (show (TMap (k, a))))))
  | TNamed (n, args) when is_tuple_name n ->
    let enc = list_of (List.mapi (fun i a -> enc_of a (ex st (EField (x, string_of_int i, st.sp)))) args) in
    let s, es = items_of (List.length args) v at args in
    (only enc, block st [ s ] (ok (ex st (EStruct (n, st.sp, List.mapi (fun i e -> (string_of_int i, st.sp, e)) es)))))
  | TNamed (n, args) when Hashtbl.mem st.structs n ->
    let sd = Hashtbl.find st.structs n in
    let env = List.combine sd.st_gen args in
    let fs = List.map (fun (f, _, fte) -> (f, field_ty env fte)) sd.st_fields in
    let enc = fields_obj (List.map (fun (f, ft) -> (f, enc_of ft (ex st (EField (x, f, st.sp))))) fs) in
    let check = { sdesc = SExpr (try_ st (call st (jf st "need_object") [ v; at ])); sspan = st.sp } in
    let dec =
      ex st
        (EStruct
           ( n, st.sp,
             List.map
               (fun (f, ft) -> (f, st.sp, try_ st (call st (jf st "member") [ v; at; str st f; fnv (snd (coders st ft)) ])))
               fs ))
    in
    (only enc, block st [ check ] (ok dec))
  | TNamed (n, args) when Hashtbl.mem st.enums n ->
    let ed = Hashtbl.find st.enums n in
    let env = List.combine ed.en_gen args in
    let vs = List.map (fun (vn, _, ts) -> (vn, List.map (field_ty env) ts)) ed.en_variants in
    let arms =
      List.map
        (fun (vn, ts) ->
          let names = List.mapi (fun i _ -> sprintf "p%d" i) ts in
          let body =
            match ts with
            | [] -> str st vn |> fun s -> call st (jf st "of_str") [ s ]
            | [ a ] -> call st (jf st "of_variant") [ str st vn; enc_of a (var st "p0") ]
            | _ -> call st (jf st "of_variant") [ str st vn; list_of (List.map2 (fun a p -> enc_of a (var st p)) ts names) ]
          in
          { apat = pat st (PVariant (Some n, vn, List.map (fun p -> pat st (PBind p)) names)); aguard = None; abody = body })
        vs
    in
    let enc = ex st (EMatch (x, arms)) in
    let got = var st "got" in
    let name = ex st (EField (got, "0", st.sp)) and payload = ex st (EField (got, "1", st.sp)) in
    let inner = call st (jf st "inside") [ at; name ] in
    let darms =
      List.map
        (fun (vn, ts) ->
          let made =
            match ts with
            | [] -> ok (ex st (EField (var st n, vn, st.sp)))
            | [ a ] -> ok (ex st (EMethod (var st n, vn, st.sp, [ dec_of a payload inner ])))
            | _ ->
              let s, es = items_of (List.length ts) payload inner ts in
              ex st (EBlock (block st [ s ] (ok (ex st (EMethod (var st n, vn, st.sp, es))))))
          in
          { apat = pat st (PStr vn); aguard = None; abody = made })
        vs
    in
    let names = String.concat ", " (List.map (fun (vn, _) -> sprintf "`%s`" vn) vs) in
    let other = { apat = pat st PWild; aguard = None; abody = call st "Err" [ call st (jf st "no_variant") [ at; name; str st names ] ] } in
    let s = { sdesc = SLet (false, "got", st.sp, None, try_ st (call st (jf st "from_variant") [ v; at ])); sspan = st.sp } in
    (only enc, block st [ s ] (ex st (EMatch (name, darms @ [ other ]))))
  | t -> raise (Unsupported (sprintf "`%s`" (show t)))

(* Every expression of a function body, outermost first, in source order. *)
let rec iter_expr f (e : expr) =
  f e;
  let go = iter_expr f in
  let blk = iter_block f in
  match e.edesc with
  | EInt _ | EFloat _ | EBool _ | EUnit | EVar _ -> ()
  | EStr parts -> List.iter (function SPExpr x -> go x | SPLit _ -> ()) parts
  | EList es -> List.iter go es
  | EBin (_, a, b) -> go a; go b
  | EUn (_, a) -> go a
  | ECall (c, args) -> go c; List.iter go args
  | EMethod (r, _, _, args) -> go r; List.iter go args
  | EField (r, _, _) -> go r
  | EIndex (a, i) -> go a; go i
  | ETry (a, c) -> go a; Option.iter go c
  | EIf (c, b, els) -> go c; blk b; Option.iter go els
  | EMatch (s, arms) -> go s; List.iter (fun a -> Option.iter go a.aguard; go a.abody) arms
  | EBlock b -> blk b
  | EClosure (_, body) -> go body
  | EStruct (_, _, fs) -> List.iter (fun (_, _, x) -> go x) fs
  | EGroup (_, _, d, b) -> Option.iter (fun (c, ms) -> go c; go ms) d; blk b
  | EDyn (x, _) -> go x
  | ESelect arms ->
    List.iter
      (fun a ->
        (match a.sop with
         | SelRecv (_, c) -> go c
         | SelSend (c, x) -> go c; go x
         | SelAfter (c, ms) -> go c; go ms
         | SelDefault -> ());
        Option.iter go a.sguard;
        go a.sbody)
      arms
  | EPipe (a, b) -> go a; go b
  | EWith (b, fs) -> go b; List.iter (fun (_, _, x) -> go x) fs
  | ESafe (b, _, rest, _) -> go b; go rest
  | ENamed (_, _, x) -> go x

and iter_block f (b : block) =
  List.iter
    (fun s ->
      match s.sdesc with
      | SLet (_, _, _, _, e) | SExpr e -> iter_expr f e
      | SAssign (a, b) -> iter_expr f a; iter_expr f b
      | SReturn e -> Option.iter (iter_expr f) e
      | SWhile (c, b) -> iter_expr f c; iter_block f b
      | SFor (_, _, e, b) -> iter_expr f e; iter_block f b
      | SLetPat (_, e) -> iter_expr f e
      | SForPat (_, e, b) -> iter_expr f e; iter_block f b
      | SBreak | SContinue -> ()
      | SDefer b -> iter_block f b)
    b.stmts;
  Option.iter (iter_expr f) b.tail

(* Is the type known all the way down? *)
let rec concrete t =
  match repr t with
  | TParam _ | TVar _ -> false
  | TList a | TChan a | TShared a | TGroup a -> concrete a
  | TMap (a, b) | TArray (a, b) -> concrete a && concrete b
  | TNamed (_, args) | TDyn (_, args) -> List.for_all concrete args
  | TFn (ps, r) -> List.for_all concrete ps && concrete r
  | _ -> true

(* The program with every `json.encode` and `json.decode` pointed at its
   conversion and those conversions added, or None when it uses neither. *)
let expand ~json:q (items : item list) : (item list * Diag.t list) option =
  begin
    let encode = q ^ ".encode" and decode = q ^ ".decode" in
    let structs = Hashtbl.create 16 and enums = Hashtbl.create 16 in
    (* the prelude's types too: Result is an enum like any other *)
    List.iter (function IStruct s -> Hashtbl.replace structs s.st_name s | IEnum e -> Hashtbl.replace enums e.en_name e | _ -> ()) (Prelude.items () @ items);
    let st = { q; sp = Span.dummy; structs; enums; names = Hashtbl.create 16; count = 0; out = []; diags = [] } in
    let wrappers = Hashtbl.create 16 in
    let found = ref false in
    let visit (e : expr) =
      match e.edesc with
      | EVar n when n = encode || n = decode -> (
        found := true;
        st.sp <- e.espan;
        let t = match e.einst with [ (_, t) ] -> repr t | _ -> TErr in
        let what = if n = encode then "json.encode" else "json.decode" in
        if not (concrete t) then
          st.diags <-
            Diag.make "E0212" e.espan
              (sprintf "`%s` needs to know the type it converts here, and it is %s" what
                 (match t with TParam p -> sprintf "the type parameter `%s`" p | _ -> "not known yet"))
              ~notes:[ (if n = decode then "say the type: `let t: Todo = json.decode(s)?`" else "convert where the type is known, and pass the text") ]
            :: st.diags
        else
          let key = what ^ " " ^ show t in
          match Hashtbl.find_opt wrappers key with
          | Some w -> e.edesc <- EVar w; e.einst <- []
          | None -> (
            try
              let to_, from = coders st t in
              let w = jf st (sprintf "%s$%d" (if n = encode then "encode" else "decode") (Hashtbl.length wrappers + 1)) in
              let prm n t = { prm_name = n; prm_span = e.espan; prm_ty = t } in
              let body, params, ret =
                if n = encode then (block st [] (call st (jf st "stringify") [ call st to_ [ var st "x" ] ]), [ prm "x" (written st t) ], written st TStr)
                else (block st [] (call st (jf st "from_text") [ var st "s"; var st from ]), [ prm "s" (written st TStr) ], written st (TNamed ("Result", [ t; TStr ])))
              in
              st.out <-
                IFn
                  { fn_name = w; fn_span = e.espan; fn_gen = []; fn_params = params; fn_ret = Some ret; fn_body = body;
                    fn_test = None; fn_bench = false; fn_virtual = false; fn_table = None; fn_pub = false; fn_bounds = [];
                    fn_foreign = None }
                :: st.out;
              Hashtbl.replace wrappers key w;
              e.edesc <- EVar w;
              e.einst <- []
            with Unsupported inner ->
              st.diags <-
                Diag.make "E0212" e.espan
                  (if inner = sprintf "`%s`" (show t) then sprintf "`%s` cannot convert `%s`: it has no JSON form" what (show t)
                   else sprintf "`%s` cannot convert `%s`: it holds %s, which has no JSON form" what (show t) inner)
                  ~notes:[ "JSON holds Int, Float, Str, Bool, json.Value, Option, List, Map with Str keys, tuples, and structs and enums of those" ]
                :: st.diags))
      | _ -> ()
    in
    (* a table test's case, then the body, as selfhost/walk.lmn orders them *)
    List.iter (function IFn d -> Option.iter (fun (_, c) -> iter_expr visit c) d.fn_table; iter_block visit d.fn_body | _ -> ()) items;
    if not !found then None else Some (items @ List.rev st.out, List.rev st.diags)
  end
