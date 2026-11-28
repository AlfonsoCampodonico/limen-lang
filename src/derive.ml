(* `json.encode(x)` and `json.decode(s)`, and `sql.decode(row)`,
   `db.all(text, params)`, `db.one(text, params)` and `sql.values(x)`: the
   compiler writes the conversion.

   The standard json module declares both as generic functions whose bodies
   never run, so the checker types every use like any call. Once a program
   has checked, this pass finds each use, takes the type it was used at, and
   writes ordinary Limen functions for that type and every type inside it,
   into the json module: `json.to$N(x: T) -> json.Value` (for a type that
   holds a type that holds itself, `json.put$N(x: T) -> List[json.Part]`,
   the text in pieces) and `json.from$N(v: json.Value, at: Str) -> Result[T, Str]`,
   built from the helpers at the end of json.lmn; for a type that holds
   itself also `json.step$N`, its decoder a step at a time. Each use is pointed at a wrapper,
   `json.encode$N` or `json.decode$N`, and the program is checked again with
   the new functions, so the interpreter and the C backend run them as any
   other code. `$` cannot be written in source, so the names are the
   compiler's own.

   The type must be known where `encode` or `decode` is used: a type
   parameter is E0212, since the interpreter does not instantiate generic
   functions. Types that have no JSON form (capabilities, functions,
   channels, `Bytes`, arrays, maps without `Str` keys, `dyn` values, `()`)
   are E0212 too, naming where in the type they are.

   `yaml.encode(x)` and `yaml.decode(s)` are written by the same walk, into
   the yaml module, over `yaml.Node` in place of `json.Value`: yaml.lmn has
   helpers of the same names and shapes as json.lmn's, so only the module
   and its value type differ.

   The sql and postgres modules' work the same way, for structs whose fields are Int,
   Float, Str, Bool, Bytes or an Option of one: `sql.row$N(row: Row) ->
   Result[T, IoError]` reads each field from the column of its key (its
   name, or its `as "key"`), and
   `sql.values$N(x: T) -> List[Value]` writes the fields in order. A use of
   `decode` or `values` is pointed at them; a call `db.all(...)` or
   `db.one(...)` becomes a call of a wrapper, `sql.all$N(db, ...)`. *)
open Ast
open Types

let sprintf = Printf.sprintf

type st = {
  q : string; (* the module's name in this program: "json" or "std/json", "yaml", "sql", ... *)
  vt : string; (* its value type, converted as it is: "Value" for json, "Node" for yaml *)
  mutable sp : span; (* the use being expanded, for every node written *)
  structs : (string, struct_decl) Hashtbl.t;
  enums : (string, enum_decl) Hashtbl.t;
  names : (string, int) Hashtbl.t; (* a type, shown -> its number *)
  cyc : (string, bool) Hashtbl.t; (* [cyclic]'s answers, by type shown *)
  pln : (string, bool) Hashtbl.t; (* [plain]'s *)
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
  | TBytes -> te st (TyName ("Bytes", []))
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

let value_ty st = TNamed (jf st st.vt, [])

(* What a type holds, for [cyclic]: nothing for a type converted by a
   helper, and nothing when a part has no JSON form (that is reported when
   its conversion is written). *)
let kids st t =
  try
    match repr t with
    | TNamed (n, []) when n = jf st st.vt -> []
    | TNamed ("Option", [ a ]) | TList a | TMap (_, a) -> [ a ]
    | TNamed (n, args) when is_tuple_name n -> args
    | TNamed (n, args) when Hashtbl.mem st.structs n ->
      let sd = Hashtbl.find st.structs n in
      let env = List.combine sd.st_gen args in
      List.map (fun (_, _, fte) -> field_ty env fte) sd.st_fields
    | TNamed (n, args) when Hashtbl.mem st.enums n ->
      let ed = Hashtbl.find st.enums n in
      let env = List.combine ed.en_gen args in
      List.concat_map (fun (_, _, ts) -> List.map (field_ty env) ts) ed.en_variants
    | _ -> []
  with Unsupported _ | Invalid_argument _ -> []

(* Every type reachable from [t] whose answer is not known yet, answered at
   once: the strongly connected parts of the graph of what holds what
   (Tarjan), a type holding itself when its part has more than one type or
   it holds itself directly. Linear in the types, where asking each type
   alone would be quadratic. *)
let classify st t =
  let idx = Hashtbl.create 16 and low = Hashtbl.create 16 and on = Hashtbl.create 16 in
  let stack = ref [] and next = ref 0 in
  let rec connect v =
    let k = show v in
    Hashtbl.replace idx k !next;
    Hashtbl.replace low k !next;
    incr next;
    stack := v :: !stack;
    Hashtbl.replace on k ();
    List.iter
      (fun w ->
        let wk = show w in
        if Hashtbl.mem st.cyc wk then ()
        else if not (Hashtbl.mem idx wk) then begin
          connect w;
          Hashtbl.replace low k (min (Hashtbl.find low k) (Hashtbl.find low wk))
        end
        else if Hashtbl.mem on wk then Hashtbl.replace low k (min (Hashtbl.find low k) (Hashtbl.find idx wk)))
      (kids st v);
    if Hashtbl.find low k = Hashtbl.find idx k then begin
      let rec pop acc =
        match !stack with
        | u :: rest ->
          stack := rest;
          Hashtbl.remove on (show u);
          if show u = k then u :: acc else pop (u :: acc)
        | [] -> acc
      in
      let part = pop [] in
      let many = List.length part > 1 in
      List.iter
        (fun u ->
          let uk = show u in
          Hashtbl.replace st.cyc uk (many || List.exists (fun w -> show w = uk) (kids st u)))
        part
    end
  in
  if not (Hashtbl.mem st.cyc (show t)) then connect t

(* Does [t] hold itself? Then a value can be as deep as it is long, so its
   encoder leaves those parts for later and its decoder goes a step at a
   time (json.lmn), rather than recursing once per level. *)
let cyclic st t =
  classify st t;
  Hashtbl.find st.cyc (show t)

(* Is no type that holds itself inside [t], [t] included? Then its values
   are only so deep, and it is encoded as before: a json.Value, then
   `stringify`. A type that is not inside itself has parts that are not
   inside it either, so this recursion ends. *)
let rec plain st t =
  let k = show t in
  match Hashtbl.find_opt st.pln k with
  | Some b -> b
  | None ->
    let b = (not (cyclic st t)) && List.for_all (plain st) (kids st t) in
    Hashtbl.replace st.pln k b;
    b

(* The encoder and decoder of [t], as function names: a helper of json.lmn
   for the simple types, written ones otherwise. The encoder of a plain
   type is `to$N`, giving a json.Value; any other's is `put$N`, giving the
   text in pieces. A type that holds itself has a third, its decoder a step
   at a time, `step$N`. *)
let rec coders st t : string * string =
  match repr t with
  | TInt -> (jf st "of_int", jf st "from_int")
  | TFloat -> (jf st "of_float", jf st "from_float")
  | TStr -> (jf st "of_str", jf st "from_str")
  | TBool -> (jf st "of_bool", jf st "from_bool")
  | TNamed (n, []) when n = jf st st.vt -> (jf st "of_value", jf st "from_value")
  | t -> (
    let key = show t in
    let enc_name k = jf st (sprintf (if plain st t then "to$%d" else "put$%d") k) in
    match Hashtbl.find_opt st.names key with
    | Some k -> (enc_name k, jf st (sprintf "from$%d" k))
    | None ->
      st.count <- st.count + 1;
      let k = st.count in
      Hashtbl.replace st.names key k;
      let put = enc_name k and from = jf st (sprintf "from$%d" k) in
      let enc, dec, step = bodies st t in
      let fn ?(gen = []) name params ret body =
        IFn
          { fn_name = name; fn_span = st.sp; fn_gen = gen; fn_params = params; fn_ret = Some ret; fn_body = body;
            fn_test = None; fn_bench = false; fn_virtual = false; fn_table = None; fn_pub = false; fn_bounds = [];
            fn_foreign = None }
      in
      let prm n t = { prm_name = n; prm_span = st.sp; prm_ty = t; prm_default = None; prm_mut = false } in
      let parts = te st (TyName ("List", [ te st (TyName (jf st "Part", [])) ])) in
      let result = written st (TNamed ("Result", [ t; TStr ])) in
      let dec_params = [ prm "v" (written st (value_ty st)); prm "at" (written st TStr) ] in
      let encoder = fn put [ prm "x" (written st t) ] (if plain st t then written st (value_ty st) else parts) enc in
      (match step with
       | None -> st.out <- fn from dec_params result dec :: encoder :: st.out
       | Some body ->
         (* the decoder runs the steps *)
         let start = call st (jf st "stop") [ var st "r" ] in
         let dec = block st [] (call st (jf st "run") [ call st (jf st (sprintf "step$%d" k)) [ var st "v"; var st "at"; ex st (EClosure ([ ("r", None, st.sp) ], start)) ] ]) in
         let next = te st (TyName (jf st "Next", [ te st (TyName ("R$", [])) ])) in
         let kont = te st (TyFn ([ result ], next)) in
         let stepper = fn ~gen:[ "R$" ] (jf st (sprintf "step$%d" k)) (dec_params @ [ prm "k" kont ]) next body in
         st.out <- fn from dec_params result dec :: stepper :: encoder :: st.out);
      (put, from))

and step_name st t = jf st (sprintf "step$%d" (Hashtbl.find st.names (show t)))

(* The bodies of [t]'s encoder (of `x`) and decoder (of `v` at `at`), and
   when [t] holds itself, its decoder a step at a time (of `v` at `at`, on
   to `k`). The parts' conversions are written first, in order. *)
and bodies st t : block * block * block option =
  let x = var st "x" and v = var st "v" and at = var st "at" in
  let fnv n = var st n in
  let only e = block st [] e in
  let ok e = call st "Ok" [ e ] in
  let int n = ex st (EInt (Int64.of_int n)) in
  let need ts = List.iter (fun a -> ignore (coders st a)) ts in
  (* old encoder: a part's Value *)
  let enc_of a e = let p, _ = coders st a in call st p [ e ] in
  (* the pieces of a part: a plain one's Value as text, later when it holds
     itself *)
  let put_of a e =
    let p, _ = coders st a in
    let simple = match repr a with TInt -> Some "put_int" | TFloat -> Some "put_float" | TStr -> Some "put_str" | TBool -> Some "put_bool" | TNamed (n, []) when n = jf st st.vt -> Some "put_value" | _ -> None in
    match simple with
    | Some h -> call st (jf st h) [ e ]
    | None ->
    if plain st a then call st (jf st "put_value") [ call st p [ e ] ]
    else if cyclic st a then call st (jf st "later") [ ex st (EClosure ([], call st p [ e ])) ]
    else call st p [ e ]
  in
  let fields_obj fs = call st (jf st "of_fields") [ ex st (EList (List.map (fun (n, e) -> call st (jf st "field") [ str st n; e ]) fs)) ] in
  let list_of es = call st (jf st "of_list") [ ex st (EList es) ] in
  let pl = plain st t in
  let dec_of a v at = let _, d = coders st a in try_ st (call st d [ v; at ]) in
  let items_of n v at decs =
    let s = { sdesc = SLet (false, "items", st.sp, None, try_ st (call st (jf st "from_items") [ v; at; int n ])); sspan = st.sp } in
    let es = List.mapi (fun i t -> dec_of t (ex st (EIndex (var st "items", int i))) (call st (jf st "item_at") [ at; int i ])) decs in
    (s, es)
  in
  (* a step at a time: [kont e] hands e on, [seq parts made] decodes the
     parts in order, each [(type, direct, by_step)] binding d0, d1, ...,
     then hands on [made] *)
  let go e = call st (jf st "go") [ ex st (EClosure ([], e)) ] in
  let kont e = go (call st "k" [ e ]) in
  let fail = { apat = pat st (PVariant (None, "Err", [ pat st (PBind "e") ])); aguard = None; abody = kont (call st "Err" [ var st "e" ]) } in
  let on_ok r name body =
    let bind = if name = "_" then PWild else PBind name in
    ex st (EMatch (r, [ { apat = pat st (PVariant (None, "Ok", [ pat st bind ])); aguard = None; abody = body }; fail ]))
  in
  let seq parts made =
    let rec loop i = function
      | [] -> kont made
      | (a, direct, by_step) :: rest ->
        let name = sprintf "d%d" i in
        if cyclic st a then go (by_step (ex st (EClosure ([ ("r", None, st.sp) ], on_ok (var st "r") name (loop (i + 1) rest)))))
        else on_ok (direct ()) name (loop (i + 1) rest)
    in
    loop 0 parts
  in
  let ds n = List.init n (fun i -> var st (sprintf "d%d" i)) in
  (* the parts of an array of exactly [ts] items at [at] *)
  let item_parts at ts =
    List.mapi
      (fun i a ->
        let iv = ex st (EIndex (var st "items", int i)) and iat = call st (jf st "item_at") [ at; int i ] in
        (a, (fun () -> call st (snd (coders st a)) [ iv; iat ]), fun kk -> call st (step_name st a) [ iv; iat; kk ]))
      ts
  in
  let items_then v at ts made = on_ok (call st (jf st "from_items") [ v; at; int (List.length ts) ]) "items" (seq (item_parts at ts) made) in
  let self = cyclic st t in
  match repr t with
  | TNamed ("Option", [ a ]) ->
    need [ a ];
    let e, d = coders st a in
    ( only (if pl then call st (jf st "of_option") [ x; fnv e ] else call st (jf st "put_option") [ x; fnv e ]),
      only (call st (jf st "from_option") [ v; at; fnv d ]),
      if self then Some (only (call st (jf st "option_k") [ v; at; fnv (step_name st a); var st "k" ])) else None )
  | TList a ->
    need [ a ];
    let e, d = coders st a in
    ( only (if pl then call st (jf st "of_list") [ ex st (EMethod (x, "map", st.sp, [ fnv e ])) ] else call st (jf st "put_list") [ x; fnv e ]),
      only (call st (jf st "from_list") [ v; at; fnv d ]),
      if self then Some (only (call st (jf st "list_k") [ v; at; fnv (step_name st a); var st "k" ])) else None )
  | TMap (k, a) -> (
    match repr k with
    | TStr ->
      need [ a ];
      let e, d = coders st a in
      ( only (if pl then call st (jf st "of_map") [ x; fnv e ] else call st (jf st "put_map") [ x; fnv e ]),
        only (call st (jf st "from_map") [ v; at; fnv d ]),
        if self then Some (only (call st (jf st "map_k") [ v; at; fnv (step_name st a); var st "k" ])) else None )
    | k -> raise (Unsupported (sprintf "`%s` (a map's keys must be `Str`)" (show (TMap (k, a))))))
  | TNamed (n, args) when is_tuple_name n ->
    need args;
    let enc =
      if pl then list_of (List.mapi (fun i a -> enc_of a (ex st (EField (x, string_of_int i, st.sp)))) args)
      else call st (jf st "put_items") [ ex st (EList (List.mapi (fun i a -> put_of a (ex st (EField (x, string_of_int i, st.sp)))) args)) ]
    in
    let s, es = items_of (List.length args) v at args in
    let build es = ex st (EStruct (n, st.sp, List.mapi (fun i e -> (string_of_int i, st.sp, e)) es)) in
    (only enc, block st [ s ] (ok (build es)), if self then Some (only (items_then v at args (ok (build (ds (List.length args)))))) else None)
  | TNamed (n, args) when Hashtbl.mem st.structs n ->
    let sd = Hashtbl.find st.structs n in
    let env = List.combine sd.st_gen args in
    let fs = List.map (fun (f, _, fte) -> (f, field_ty env fte)) sd.st_fields in
    need (List.map snd fs);
    (* a field is read and written by its key, `as "key"` or its name *)
    let key f = str st (field_key sd f) in
    let enc =
      if pl then fields_obj (List.map (fun (f, ft) -> (field_key sd f, enc_of ft (ex st (EField (x, f, st.sp))))) fs)
      else
      call st (jf st "put_fields")
        [ ex st (EList (List.map (fun (f, _) -> key f) fs)); ex st (EList (List.map (fun (f, ft) -> put_of ft (ex st (EField (x, f, st.sp)))) fs)) ]
    in
    let check = { sdesc = SExpr (try_ st (call st (jf st "need_object") [ v; at ])); sspan = st.sp } in
    let dec =
      ex st
        (EStruct
           ( n, st.sp,
             List.map
               (fun (f, ft) -> (f, st.sp, try_ st (call st (jf st "member") [ v; at; key f; fnv (snd (coders st ft)) ])))
               fs ))
    in
    let step () =
      let parts =
        List.map
          (fun (f, ft) ->
            ( ft,
              (fun () -> call st (jf st "member") [ v; at; key f; fnv (snd (coders st ft)) ]),
              fun kk -> call st (jf st "member_k") [ v; at; key f; fnv (step_name st ft); kk ] ))
          fs
      in
      let made = ok (ex st (EStruct (n, st.sp, List.mapi (fun i (f, _) -> (f, st.sp, var st (sprintf "d%d" i))) fs))) in
      only (on_ok (call st (jf st "need_object") [ v; at ]) "_" (seq parts made))
    in
    (only enc, block st [ check ] (ok dec), if self then Some (step ()) else None)
  | TNamed (n, args) when Hashtbl.mem st.enums n ->
    let ed = Hashtbl.find st.enums n in
    let env = List.combine ed.en_gen args in
    let vs = List.map (fun (vn, _, ts) -> (vn, List.map (field_ty env) ts)) ed.en_variants in
    need (List.concat_map snd vs);
    let arms =
      List.map
        (fun (vn, ts) ->
          let names = List.mapi (fun i _ -> sprintf "p%d" i) ts in
          let body =
            match ts with
            | [] when pl -> call st (jf st "of_str") [ str st vn ]
            | [ a ] when pl -> call st (jf st "of_variant") [ str st vn; enc_of a (var st "p0") ]
            | _ when pl -> call st (jf st "of_variant") [ str st vn; list_of (List.map2 (fun a p -> enc_of a (var st p)) ts names) ]
            | [] -> call st (jf st "put_str") [ str st vn ]
            | [ a ] -> call st (jf st "put_variant") [ str st vn; put_of a (var st "p0") ]
            | _ -> call st (jf st "put_variant") [ str st vn; call st (jf st "put_items") [ ex st (EList (List.map2 (fun a p -> put_of a (var st p)) ts names)) ] ]
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
    let step () =
      let sarms =
        List.map
          (fun (vn, ts) ->
            let made es = ok (if ts = [] then ex st (EField (var st n, vn, st.sp)) else ex st (EMethod (var st n, vn, st.sp, es))) in
            let body =
              match ts with
              | [] -> kont (made [])
              | [ a ] -> seq [ (a, (fun () -> call st (snd (coders st a)) [ payload; inner ]), fun kk -> call st (step_name st a) [ payload; inner; kk ]) ] (made (ds 1))
              | _ -> items_then payload inner ts (made (ds (List.length ts)))
            in
            { apat = pat st (PStr vn); aguard = None; abody = body })
          vs
      in
      let other = { apat = pat st PWild; aguard = None; abody = kont (call st "Err" [ call st (jf st "no_variant") [ at; name; str st names ] ]) } in
      only (on_ok (call st (jf st "from_variant") [ v; at ]) "got" (ex st (EMatch (name, sarms @ [ other ]))))
    in
    (only enc, block st [ s ] (ex st (EMatch (name, darms @ [ other ]))), if self then Some (step ()) else None)
  | t -> raise (Unsupported (sprintf "`%s`" (show t)))

(* ---------- SQL rows ---------- *)

(* The helper suffix for a column type, if [t] is one. *)
let column t = match repr t with TInt -> Some "int" | TFloat -> Some "float" | TStr -> Some "str" | TBool -> Some "bool" | TBytes -> Some "bytes" | _ -> None

(* The struct's name without its module, for errors. *)
let short n = match String.rindex_opt n '.' with Some i -> String.sub n (i + 1) (String.length n - i - 1) | None -> n

(* The reader and writer of the struct [t] as rows, `sql.row$N` and
   `sql.values$N`, written the first time [t] is asked for. [st.q] is the
   sql module's name here. *)
let row_coders st t : string * string =
  let key = "row " ^ show t in
  match Hashtbl.find_opt st.names key with
  | Some k -> (jf st (sprintf "row$%d" k), jf st (sprintf "values$%d" k))
  | None -> (
    match repr t with
    | TNamed (n, args) when Hashtbl.mem st.structs n && not (is_tuple_name n) ->
      let sd = Hashtbl.find st.structs n in
      let env = List.combine sd.st_gen args in
      let fs =
        List.map
          (fun (f, _, fte) ->
            try (f, field_ty env fte)
            with Unsupported w -> raise (Unsupported (sprintf "its field `%s` holds %s, which is not a column type" f w)))
          sd.st_fields
      in
      let owner = short n in
      let row = var st "row" and x = var st "x" in
      let parts =
        List.map
          (fun (f, ft) ->
            (* the column of the field's key, `as "key"` or its name *)
            let col = field_key sd f in
            let path = owner ^ "." ^ f in
            let at k = try_ st (call st (jf st (k ^ "_at")) [ row; str st col; str st path ]) in
            let value = ex st (EField (x, f, st.sp)) in
            match (column ft, repr ft) with
            | Some k, _ -> (try_ st (call st (jf st "required") [ at k; str st col; str st path ]), call st (jf st ("of_" ^ k)) [ value ])
            | None, TNamed ("Option", [ a ]) when column a <> None ->
              let k = Option.get (column a) in
              (at k, call st (jf st "of_option") [ value; var st (jf st ("of_" ^ k)) ])
            | _ -> raise (Unsupported (sprintf "its field `%s` is a `%s`, which is not a column type" f (show ft))))
          fs
      in
      st.count <- st.count + 1;
      let k = st.count in
      Hashtbl.replace st.names key k;
      let reader = jf st (sprintf "row$%d" k) and writer = jf st (sprintf "values$%d" k) in
      let fn name params ret body =
        IFn
          { fn_name = name; fn_span = st.sp; fn_gen = []; fn_params = params; fn_ret = Some ret; fn_body = body;
            fn_test = None; fn_bench = false; fn_virtual = false; fn_table = None; fn_pub = false; fn_bounds = [];
            fn_foreign = None }
      in
      let prm n t = { prm_name = n; prm_span = st.sp; prm_ty = t; prm_default = None; prm_mut = false } in
      let value_list = TList (TNamed (jf st "Value", [])) in
      let made = ex st (EStruct (n, st.sp, List.map2 (fun (f, _) (r, _) -> (f, st.sp, r)) fs parts)) in
      st.out <-
        fn writer [ prm "x" (written st t) ] (written st value_list) (block st [] (ex st (EList (List.map snd parts))))
        :: fn reader [ prm "row" (written st (TNamed (jf st "Row", []))) ] (written st (TNamed ("Result", [ t; TNamed ("IoError", []) ])))
             (block st [] (call st "Ok" [ made ]))
        :: st.out;
      (reader, writer)
    | _ -> raise (Unsupported "it is not a struct"))

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
  | ENamed (_, _, x) | EMut x -> go x

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

(* The program with every `json.encode` and `json.decode`, every
   `yaml.encode` and `yaml.decode`, and every `sql.decode`, `sql.values`,
   `db.all` and `db.one`, pointed at its conversion and those conversions
   added, or None when it uses none of them. [json], [yaml] and [rows] are
   those modules' names here, when loaded. *)
let expand ?json ?yaml ?(rows = []) (items : item list) : (item list * Diag.t list) option =
  begin
    let structs = Hashtbl.create 16 and enums = Hashtbl.create 16 in
    (* the prelude's types too: Result is an enum like any other *)
    List.iter (function IStruct s -> Hashtbl.replace structs s.st_name s | IEnum e -> Hashtbl.replace enums e.en_name e | _ -> ()) (Prelude.items () @ items);
    let mk vt q = { q; vt; sp = Span.dummy; structs; enums; names = Hashtbl.create 16; cyc = Hashtbl.create 16; pln = Hashtbl.create 16; count = 0; out = []; diags = [] } in
    let js = mk "Value" (Option.value ~default:"" json) in
    (* yaml's conversions are json's, over yaml.Node, in the yaml module *)
    let ys = mk "Node" (Option.value ~default:"" yaml) in
    (* the modules that read rows into structs, each writing its own conversions *)
    let rgs = List.map (mk "Value") rows in
    let wrappers = Hashtbl.create 16 in
    let found = ref false in
    let is_json n = json <> None && (n = jf js "encode" || n = jf js "decode") in
    let is_yaml n = yaml <> None && (n = jf ys "encode" || n = jf ys "decode") in
    let row_module n = List.find_opt (fun st -> n = jf st "decode" || n = jf st "values") rgs in
    let db_module (recv : expr) =
      match repr recv.ety with TNamed (n, []) -> List.find_opt (fun st -> n = jf st "Db") rgs | _ -> None
    in
    let fn_item w sp params ret body =
      IFn
        { fn_name = w; fn_span = sp; fn_gen = []; fn_params = params; fn_ret = Some ret; fn_body = body;
          fn_test = None; fn_bench = false; fn_virtual = false; fn_table = None; fn_pub = false; fn_bounds = [];
          fn_foreign = None }
    in
    (* the type a use converts, or an E0212 when it is not known *)
    let known st (e : expr) what note =
      st.sp <- e.espan;
      let t = match e.einst with [ (_, t) ] -> repr t | _ -> TErr in
      if concrete t then Some t
      else begin
        st.diags <-
          Diag.make "E0212" e.espan
            (sprintf "`%s` needs to know the type it converts here, and it is %s" what
               (match t with TParam p -> sprintf "the type parameter `%s`" p | _ -> "not known yet"))
            ~notes:[ note ]
          :: st.diags;
        None
      end
    in
    let no_form st (e : expr) what t inner holds =
      st.diags <-
        Diag.make "E0212" e.espan
          (if inner = sprintf "`%s`" (show t) then sprintf "`%s` cannot convert `%s`: it has no %s form" what (show t) holds
           else if holds = "SQL" then sprintf "`%s` cannot convert `%s`: %s" what (show t) inner
           else sprintf "`%s` cannot convert `%s`: it holds %s, which has no %s form" what (show t) inner holds)
          ~notes:
            [ (if holds = "SQL" then "a row is read into a struct whose fields are Int, Float, Str, Bool, Bytes, or an Option of one of those"
               else if holds = "YAML" then "YAML holds Int, Float, Str, Bool, yaml.Node, Option, List, Map with Str keys, tuples, and structs and enums of those"
               else "JSON holds Int, Float, Str, Bool, json.Value, Option, List, Map with Str keys, tuples, and structs and enums of those") ]
        :: st.diags
    in
    (* a use of `encode` or `decode` of json (or yaml: [m], [holds]) *)
    let convert st m holds n (e : expr) =
      let encode = n = jf st "encode" in
      let what = m ^ (if encode then ".encode" else ".decode") in
      match known st e what (if encode then "convert where the type is known, and pass the text" else sprintf "say the type: `let t: Todo = %s.decode(s)?`" m) with
      | None -> ()
      | Some t -> (
        let key = what ^ " " ^ show t in
        match Hashtbl.find_opt wrappers key with
        | Some w -> e.edesc <- EVar w; e.einst <- []
        | None -> (
          try
            let to_, from = coders st t in
            let w = jf st (sprintf "%s$%d" (if encode then "encode" else "decode") (Hashtbl.length wrappers + 1)) in
            let prm n t = { prm_name = n; prm_span = e.espan; prm_ty = t; prm_default = None; prm_mut = false } in
            let body, params, ret =
              if encode then (block st [] (call st (jf st (if plain st t then "stringify" else "write")) [ call st to_ [ var st "x" ] ]), [ prm "x" (written st t) ], written st TStr)
              else (block st [] (call st (jf st "from_text") [ var st "s"; var st from ]), [ prm "s" (written st TStr) ], written st (TNamed ("Result", [ t; TStr ])))
            in
            st.out <- fn_item w e.espan params ret body :: st.out;
            Hashtbl.replace wrappers key w;
            e.edesc <- EVar w;
            e.einst <- []
          with Unsupported inner ->
            (* a function held through a struct's fields is named by its path *)
            let inner = match fn_via t with Some p when inner = "a function" -> sprintf "a function through `%s`" p | _ -> inner in
            no_form st e what t inner holds))
    in
    let visit (e : expr) =
      match e.edesc with
      | EVar n when is_json n -> found := true; convert js "json" "JSON" n e
      | EVar n when is_yaml n ->
        found := true;
        convert ys "yaml" "YAML" n e;
        (* one list of problems for both, in source order *)
        js.diags <- ys.diags @ js.diags;
        ys.diags <- []
      | EVar n when row_module n <> None -> (
        found := true;
        let st = Option.get (row_module n) in
        (* `sql`, also when the program names the module `std/sql` *)
        let m = match String.rindex_opt st.q '/' with Some i -> String.sub st.q (i + 1) (String.length st.q - i - 1) | None -> st.q in
        let decode = n = jf st "decode" in
        let what = m ^ (if decode then ".decode" else ".values") in
        match known st e what (if decode then sprintf "say the type: `let t: Todo = %s.decode(row)?`" m else "convert where the type is known") with
        | None -> ()
        | Some t -> (
          try
            let reader, writer = row_coders st t in
            e.edesc <- EVar (if decode then reader else writer);
            e.einst <- []
          with Unsupported inner -> no_form st e what t inner "SQL"))
      | EMethod (recv, (("all" | "one") as m), _, args) when db_module recv <> None -> (
        found := true;
        let st = Option.get (db_module recv) in
        let what = "db." ^ m in
        match known st e what (sprintf "say the type: `let t: %s = db.%s(...)?`" (if m = "all" then "List[Todo]" else "Option[Todo]") m) with
        | None -> ()
        | Some t -> (
          (* the module too: the same struct can be read from sql and postgres *)
          let key = st.q ^ " " ^ what ^ " " ^ show t in
          let point w = e.edesc <- ECall ({ (var st w) with espan = e.espan }, recv :: args); e.einst <- [] in
          match Hashtbl.find_opt wrappers key with
          | Some w -> point w
          | None -> (
            try
              let reader, _ = row_coders st t in
              let w = jf st (sprintf "%s$%d" m (Hashtbl.length wrappers + 1)) in
              let prm n t = { prm_name = n; prm_span = e.espan; prm_ty = t; prm_default = None; prm_mut = false } in
              let db = TNamed (jf st "Db", []) and values = TList (TNamed (jf st "Value", [])) in
              let query = if m = "all" then "query" else "query_one" in
              let rows = try_ st (ex st (EMethod (var st "self", query, st.sp, [ var st "text"; var st "params" ]))) in
              let body = block st [] (call st (jf st (if m = "all" then "rows_as" else "row_as")) [ rows; var st reader ]) in
              let ret = TNamed ("Result", [ (if m = "all" then TList t else TNamed ("Option", [ t ])); TNamed ("IoError", []) ]) in
              st.out <-
                fn_item w e.espan
                  [ prm "self" (written st db); prm "text" (written st TStr);
                    (* as the method it stands for, so a call that left it out still fits *)
                    { (prm "params" (written st values)) with prm_default = Some (ex st (EList [])) } ]
                  (written st ret) body
                :: st.out;
              Hashtbl.replace wrappers key w;
              point w
            with Unsupported inner -> no_form st e what t inner "SQL")))
      | _ -> ()
    in
    (* a table test's case, then the body, as selfhost/walk.lmn orders them *)
    List.iter (function IFn d -> Option.iter (fun (_, c) -> iter_expr visit c) d.fn_table; iter_block visit d.fn_body | _ -> ()) items;
    if not !found then None
    else
      Some
        ( items @ List.rev js.out @ List.rev ys.out @ List.concat_map (fun st -> List.rev st.out) rgs,
          List.rev js.diags @ List.concat_map (fun st -> List.rev st.diags) rgs )
  end
