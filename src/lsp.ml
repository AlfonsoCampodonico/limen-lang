(* Language server: diagnostics, hover (types), go to definition, completion,
   quick fixes from the checker's auto-fixes, and document symbols.
   [handle] is pure request -> messages, so it can be tested without stdio.
   Positions: LSP lines and characters are mapped to byte offsets, which is
   exact for ASCII source. *)
open Ast
module J = Json

type doc = { path : string; text : string; items : item list option; diags : Diag.t list }
type state = { docs : (string, doc) Hashtbl.t; mutable shutdown : bool; mutable exit : bool }

let create () = { docs = Hashtbl.create 8; shutdown = false; exit = false }

(* ---------- positions ---------- *)

let path_of_uri uri =
  let p = if String.starts_with ~prefix:"file://" uri then String.sub uri 7 (String.length uri - 7) else uri in
  let b = Buffer.create (String.length p) in
  let i = ref 0 in
  while !i < String.length p do
    if p.[!i] = '%' && !i + 2 < String.length p then begin
      (match int_of_string_opt ("0x" ^ String.sub p (!i + 1) 2) with
       | Some c -> Buffer.add_char b (Char.chr c)
       | None -> Buffer.add_string b (String.sub p !i 3));
      i := !i + 3
    end
    else (Buffer.add_char b p.[!i]; incr i)
  done;
  Buffer.contents b

let offset_of text line ch =
  let n = String.length text in
  let rec go i l = if l = line then i else if i >= n then n else if text.[i] = '\n' then go (i + 1) (l + 1) else go (i + 1) l in
  let start = go 0 0 in
  let rec col i k = if k = 0 || i >= n || text.[i] = '\n' then i else col (i + 1) (k - 1) in
  col start ch

let pos_of text off =
  let off = min off (String.length text) in
  let line = ref 0 and col = ref 0 in
  for i = 0 to off - 1 do
    if text.[i] = '\n' then (incr line; col := 0) else incr col
  done;
  J.Obj [ ("line", J.Int !line); ("character", J.Int !col) ]

let range text lo hi = J.Obj [ ("start", pos_of text lo); ("end", pos_of text hi) ]
let srange text (sp : Span.t) = range text sp.lo sp.hi
let slice text (sp : Span.t) = String.sub text sp.lo (max 0 (min sp.hi (String.length text) - sp.lo))

(* ---------- analysis ---------- *)

let analyze path text =
  match Modules.load path text with
  | [], (_ :: _ as ds) -> (None, List.filter (fun (d : Diag.t) -> d.span.file = path) ds)
  | items, _ ->
    let ds, _ = Check.check_program items in
    (* hovers and definitions only look at this file's items *)
    let mine = List.filter (fun it -> (Modules.item_span it).Span.file = path) items in
    (Some mine, List.filter (fun (d : Diag.t) -> d.span.file = path) ds)

let diag_json text (d : Diag.t) =
  let msg = String.concat "\n" (d.message :: List.map (fun n -> "note: " ^ n) d.notes) in
  J.Obj
    [ ("range", srange text d.span); ("severity", J.Int (match d.severity with Diag.Error -> 1 | Diag.Warning -> 2));
      ("code", J.String d.code); ("source", J.String "vela"); ("message", J.String msg) ]

let publish uri doc =
  J.Obj
    [ ("jsonrpc", J.String "2.0"); ("method", J.String "textDocument/publishDiagnostics");
      ("params", J.Obj [ ("uri", J.String uri); ("diagnostics", J.List (List.map (diag_json doc.text) doc.diags)) ]) ]

(* ---------- walking the tree ---------- *)

let rec walk_expr (f : expr -> unit) e =
  f e;
  let w = walk_expr f in
  match e.edesc with
  | EInt _ | EFloat _ | EBool _ | EUnit | EVar _ -> ()
  | EStr ps -> List.iter (function SPExpr x -> w x | SPLit _ -> ()) ps
  | EList es -> List.iter w es
  | EBin (_, a, b) | EIndex (a, b) -> w a; w b
  | EUn (_, a) | ETry a | EField (a, _, _) -> w a
  | ECall (c, args) -> w c; List.iter w args
  | EMethod (r, _, _, args) -> w r; List.iter w args
  | EIf (c, b, eo) -> w c; walk_block f b; Option.iter w eo
  | EMatch (s, arms) -> w s; List.iter (fun a -> w a.abody) arms
  | EBlock b -> walk_block f b
  | EGroup (_, until, b) -> Option.iter (fun (c, ms) -> w c; w ms) until; walk_block f b
  | EClosure (_, body) -> w body
  | EStruct (_, _, fs) -> List.iter (fun (_, _, x) -> w x) fs
  | EDyn (x, _) -> w x
  | ESelect arms ->
    List.iter
      (fun a ->
        (match a.sop with SelRecv (_, c) -> w c | SelSend (c, v) | SelAfter (c, v) -> w c; w v | SelDefault -> ());
        Option.iter w a.sguard;
        w a.sbody)
      arms

and walk_block f b =
  List.iter
    (fun s ->
      match s.sdesc with
      | SLet (_, _, _, _, e) | SExpr e -> walk_expr f e
      | SAssign (a, b) -> walk_expr f a; walk_expr f b
      | SReturn eo -> Option.iter (walk_expr f) eo
      | SWhile (c, b) -> walk_expr f c; walk_block f b
      | SFor (_, _, e, b) -> walk_expr f e; walk_block f b
      | SLetPat (_, e) -> walk_expr f e
      | SForPat (_, e, b) -> walk_expr f e; walk_block f b
      | SBreak | SContinue -> ())
    b.stmts;
  Option.iter (walk_expr f) b.tail

let fns items = List.filter_map (function IFn d -> Some d | _ -> None) items
let structs items = List.filter_map (function IStruct s -> Some s | _ -> None) items
let enums items = List.filter_map (function IEnum e -> Some e | _ -> None) items
let show t = Types.show t

let method_sig name (ps, r) = Printf.sprintf "%s(%s) -> %s" name (String.concat ", " (List.map show ps)) (show r)

let struct_decl items t =
  match Types.repr t with
  | Types.TNamed (n, _) -> List.find_opt (fun s -> s.st_name = n) (structs items)
  | _ -> None

(* ---------- hover ---------- *)

let fn_signature text (d : fn_decl) =
  let gens = if d.fn_gen = [] then "" else "[" ^ String.concat ", " d.fn_gen ^ "]" in
  let ps = List.map (fun p -> p.prm_name ^ ": " ^ slice text p.prm_ty.tspan) d.fn_params in
  let ret = match d.fn_ret with Some te -> " -> " ^ slice text te.tspan | None -> "" in
  (* a method `Shape::area` is written `fn area(self: Shape)` *)
  let name = match String.rindex_opt d.fn_name ':' with Some i -> String.sub d.fn_name (i + 1) (String.length d.fn_name - i - 1) | None -> d.fn_name in
  Printf.sprintf "fn %s%s(%s)%s" name gens (String.concat ", " ps) ret

let hover_items text items =
  let acc = ref [] in
  let add (sp : Span.t) s = acc := (sp, s) :: !acc in
  List.iter
    (function
      | IFn d ->
        add d.fn_span (match d.fn_test with Some n -> "test \"" ^ n ^ "\"" | None -> fn_signature text d);
        List.iter (fun p -> add p.prm_span (p.prm_name ^ ": " ^ slice text p.prm_ty.tspan)) d.fn_params;
        walk_block
          (fun e ->
            (match e.ety with Types.TErr -> () | t -> add e.espan (show t));
            match e.edesc with
            | EMethod (r, name, nsp, _) -> (
              match List.assoc_opt name (Check.methods (Types.repr r.ety)) with
              | Some mk -> add nsp (method_sig name (mk ()))
              | None -> ())
            | EClosure (ps, _) -> (
              match Types.repr e.ety with
              | Types.TFn (pts, _) when List.length pts = List.length ps ->
                (* `$` names come from method values (`p.area`), which the checker rewrites *)
                List.iter2 (fun (n, _, sp) t -> if n.[0] <> '$' then add sp (n ^ ": " ^ show t)) ps pts
              | _ -> ())
            | _ -> ())
          d.fn_body;
        (* let bindings and for variables *)
        let rec stmts b =
          List.iter
            (fun s ->
              match s.sdesc with
              | SLet (m, n, nsp, _, e) when n.[0] <> '$' -> add nsp (Printf.sprintf "let %s%s: %s" (if m then "mut " else "") n (show e.ety))
              | SFor (n, nsp, e, b) ->
                (match Types.repr e.ety with
                 | Types.TList t | Types.TArray (t, _) -> add nsp (n ^ ": " ^ show t)
                 | _ -> ());
                stmts b
              | SWhile (_, b) -> stmts b
              | _ -> ())
            b.stmts
        in
        stmts d.fn_body;
        walk_block (fun e -> match e.edesc with EBlock b | EGroup (_, _, b) -> stmts b | EIf (_, b, _) -> stmts b | EClosure (_, { edesc = EBlock b; _ }) -> stmts b | _ -> ()) d.fn_body
      | IIface i ->
        add i.if_span ("interface " ^ i.if_name);
        List.iter
          (fun (m, sp, ps, r) ->
            add sp
              ("fn " ^ m ^ "(" ^ String.concat ", " ("self" :: List.map (fun p -> p.prm_name ^ ": " ^ slice text p.prm_ty.tspan) ps) ^ ")"
              ^ match r with Some t -> " -> " ^ slice text t.tspan | None -> ""))
          i.if_methods
      | IStruct s ->
        add s.st_span ("struct " ^ s.st_name);
        List.iter (fun (f, sp, te) -> add sp (f ^ ": " ^ slice text te.tspan)) s.st_fields
      | IEnum e ->
        add e.en_span ("enum " ^ e.en_name);
        List.iter
          (fun (v, sp, tes) ->
            add sp (if tes = [] then v else v ^ "(" ^ String.concat ", " (List.map (fun te -> slice text te.tspan) tes) ^ ")"))
          e.en_variants)
    items;
  !acc

let smallest_at off pairs =
  List.fold_left
    (fun best ((sp : Span.t), v) ->
      if sp.lo <= off && off < max sp.hi (sp.lo + 1) then
        match best with
        | Some ((b : Span.t), _) when b.hi - b.lo <= sp.hi - sp.lo -> best
        | _ -> Some (sp, v)
      else best)
    None pairs

(* ---------- definitions ---------- *)

(* All (use span, definition span) pairs in the document. *)
let definitions items =
  let acc = ref [] in
  let add u d = acc := (u, d) :: !acc in
  let fn_spans = List.map (fun d -> (d.fn_name, d.fn_span)) (fns items) in
  let type_spans =
    List.map (fun s -> (s.st_name, s.st_span)) (structs items) @ List.map (fun e -> (e.en_name, e.en_span)) (enums items)
  in
  let variant_spans = List.concat_map (fun e -> List.map (fun (v, sp, _) -> (v, sp)) e.en_variants) (enums items) in
  let rec ty (te : ty_expr) =
    match te.tdesc with
    | TyName (n, args) ->
      (match List.assoc_opt n type_spans with
       | Some d -> add { te.tspan with Span.hi = te.tspan.lo + String.length n } d
       | None -> ());
      List.iter ty args
    | TyFn (ps, r) -> List.iter ty ps; ty r
    | TyUnit | TyInt _ -> ()
  in
  let rec pat env (p : pat) =
    match p.pdesc with
    | PBind n -> (n, p.pspan) :: env
    | PVariant (_, v, args) ->
      (match List.assoc_opt v variant_spans with Some d -> add p.pspan d | None -> ());
      List.fold_left pat env args
    | PTuple ps -> List.fold_left pat env ps
    | _ -> env
  in
  let rec expr env e =
    let x = expr env in
    match e.edesc with
    | EVar n -> (
      match List.assoc_opt n env with
      | Some d -> add e.espan d
      | None -> (
        match List.assoc_opt n fn_spans with
        | Some d -> add e.espan d
        | None -> ( match List.assoc_opt n variant_spans with Some d -> add e.espan d | None -> ())))
    | EInt _ | EFloat _ | EBool _ | EUnit -> ()
    | EStr ps -> List.iter (function SPExpr y -> x y | SPLit _ -> ()) ps
    | EList es -> List.iter x es
    | EBin (_, a, b) | EIndex (a, b) -> x a; x b
    | EUn (_, a) | ETry a -> x a
    | EField ({ edesc = EVar en; _ }, v, nsp) when not (List.mem_assoc en env) && List.mem_assoc en type_spans ->
      (match List.assoc_opt en type_spans with Some d -> add { nsp with Span.lo = e.espan.lo; hi = e.espan.lo + String.length en } d | None -> ());
      (match List.assoc_opt v variant_spans with Some d -> add nsp d | None -> ())
    | EField (r, name, nsp) ->
      x r;
      (match struct_decl items r.ety with
       | Some s -> ( match List.find_opt (fun (f, _, _) -> f = name) s.st_fields with Some (_, d, _) -> add nsp d | None -> ())
       | None -> ())
    | ECall (c, args) -> x c; List.iter x args
    | EMethod ({ edesc = EVar en; _ }, v, nsp, args) when not (List.mem_assoc en env) && List.mem_assoc en type_spans ->
      (match List.assoc_opt v variant_spans with Some d -> add nsp d | None -> ());
      List.iter x args
    | EMethod (r, _, _, args) -> x r; List.iter x args
    | EIf (c, b, eo) -> x c; block env b; Option.iter x eo
    | EMatch (s, arms) -> x s; List.iter (fun a -> expr (pat env a.apat) a.abody) arms
    | EBlock b -> block env b
    | EGroup (n, until, b) -> Option.iter (fun (c, ms) -> x c; x ms) until; block ((n, e.espan) :: env) b
    | EClosure (ps, body) ->
      List.iter (fun (_, t, _) -> Option.iter ty t) ps;
      expr (List.map (fun (n, _, sp) -> (n, sp)) ps @ env) body
    | EStruct (name, nsp, fs) ->
      (match List.assoc_opt name type_spans with Some d -> add nsp d | None -> ());
      let decl = List.find_opt (fun s -> s.st_name = name) (structs items) in
      List.iter
        (fun (f, fsp, v) ->
          (match decl with
           | Some s -> ( match List.find_opt (fun (g, _, _) -> g = f) s.st_fields with Some (_, d, _) -> add fsp d | None -> ())
           | None -> ());
          x v)
        fs
    | EDyn (y, _) -> x y
    | ESelect arms ->
      List.iter
        (fun a ->
          Option.iter x a.sguard;
          match a.sop with
          | SelRecv (b, c) -> x c; expr (match b with Some (n, sp) -> (n, sp) :: env | None -> env) a.sbody
          | SelSend (c, v) | SelAfter (c, v) -> x c; x v; x a.sbody
          | SelDefault -> x a.sbody)
        arms
  and block env b =
    let env =
      List.fold_left
        (fun env s ->
          match s.sdesc with
          | SLet (_, n, nsp, t, e) -> Option.iter ty t; expr env e; (n, nsp) :: env
          | SExpr e -> expr env e; env
          | SAssign (a, v) -> expr env a; expr env v; env
          | SReturn eo -> Option.iter (expr env) eo; env
          | SWhile (c, b) -> expr env c; block env b; env
          | SFor (n, nsp, e, b) -> expr env e; block ((n, nsp) :: env) b; env
          | SLetPat (p, e) -> expr env e; pat env p
          | SForPat (p, e, b) -> expr env e; block (pat env p) b; env
          | SBreak | SContinue -> env)
        env b.stmts
    in
    Option.iter (expr env) b.tail
  in
  List.iter
    (function
      | IFn d ->
        List.iter (fun p -> ty p.prm_ty) d.fn_params;
        Option.iter ty d.fn_ret;
        block (List.map (fun p -> (p.prm_name, p.prm_span)) d.fn_params) d.fn_body
      | IStruct s -> List.iter (fun (_, _, te) -> ty te) s.st_fields
      | IEnum e -> List.iter (fun (_, _, tes) -> List.iter ty tes) e.en_variants
      | IIface i -> List.iter (fun (_, _, ps, r) -> List.iter (fun p -> ty p.prm_ty) ps; Option.iter ty r) i.if_methods)
    items;
  !acc

(* ---------- completion ---------- *)

let keywords = [ "fn"; "let"; "mut"; "if"; "else"; "match"; "for"; "in"; "while"; "return"; "struct"; "enum"; "true"; "false"; "group"; "break"; "continue" ]

let item kind label detail = J.Obj [ ("label", J.String label); ("kind", J.Int kind); ("detail", J.String detail) ]

let is_ident_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

let member_completions path text off =
  (* cursor after `recv.` or `recv.pre`: drop `.pre`, check, and look up the receiver's type *)
  let start = ref off in
  while !start > 0 && is_ident_char text.[!start - 1] do decr start done;
  if !start = 0 || text.[!start - 1] <> '.' then None
  else begin
    let dot = !start - 1 in
    (* Two ways to turn the text into a program where the receiver ends at
       the dot: cut the rest of the line (handles `recv.|name(args)`), or drop
       just the word being completed (handles receivers spanning lines). *)
    let stop = ref off in
    while !stop < String.length text && is_ident_char text.[!stop] do incr stop done;
    let eol = match String.index_from_opt text off '\n' with Some i -> i | None -> String.length text in
    let candidates =
      [ String.sub text 0 dot ^ String.sub text eol (String.length text - eol);
        String.sub text 0 dot ^ String.sub text !stop (String.length text - !stop) ]
    in
    let receiver patched =
      match analyze path patched with
      | None, _ -> None
      | Some items, _ ->
        let best = ref None in
        List.iter
          (fun d ->
            walk_block
              (fun e ->
                if e.espan.hi = dot then
                  match !best with
                  | Some (b : expr) when b.espan.hi - b.espan.lo <= e.espan.hi - e.espan.lo -> ()
                  | _ -> best := Some e)
              d.fn_body)
          (fns items);
        Option.map (fun e -> (e, items, patched)) !best
    in
    match List.find_map receiver candidates with
    | None -> Some []
    | Some (e, items, patched) ->
      let t = Types.repr e.ety in
      let fields =
        match t with
        | Types.TCap Types.Root -> List.map (fun (f, ft) -> item 5 f (show ft)) Check.root_fields
        | _ -> (
          match struct_decl items t with
          | Some s -> List.map (fun (f, _, te) -> item 5 f (slice patched te.tspan)) s.st_fields
          | None -> [])
      in
      let methods =
        match t with
        | Types.TGroup r -> [ item 2 "go" (Printf.sprintf "go(fn() -> %s) -> ()" (show r)) ]
        | Types.TNamed (n, _) ->
          (* methods declared with `self: n`, in this file or another module *)
          let pre = n ^ "::" in
          Hashtbl.fold
            (fun k _ acc ->
              if String.starts_with ~prefix:pre k then begin
                let m = String.sub k (String.length pre) (String.length k - String.length pre) in
                let sg = match List.find_opt (fun d -> d.fn_name = k) (fns items) with Some d -> fn_signature patched d | None -> "method" in
                item 2 m sg :: acc
              end
              else acc)
            Check.method_owner []
          @ List.map (fun (m, mk) -> item 2 m (method_sig m (mk ()))) (Check.methods t)
        | _ -> List.map (fun (m, mk) -> item 2 m (method_sig m (mk ()))) (Check.methods t)
      in
      Some (fields @ methods)
  end

let general_completions doc off =
  let kws = List.map (fun k -> item 14 k "keyword") keywords in
  match doc.items with
  | None -> kws
  | Some items ->
    let globals =
      List.map (fun d -> item 3 d.fn_name (fn_signature doc.text d)) (List.filter (fun d -> d.fn_test = None && not (String.contains d.fn_name ':')) (fns items))
      @ List.map (fun s -> item 22 s.st_name "struct") (structs items)
      @ List.concat_map (fun e -> item 13 e.en_name "enum" :: List.map (fun (v, _, _) -> item 20 v e.en_name) e.en_variants) (enums items)
    in
    (* locals: definitions inside the enclosing function that start before the cursor *)
    let locals =
      List.concat_map
        (fun d ->
          if d.fn_body.bspan.lo <= off && off <= d.fn_body.bspan.hi then begin
            let seen = Hashtbl.create 16 in
            let out = ref [] in
            List.iter (fun p -> Hashtbl.replace seen p.prm_name (); out := item 6 p.prm_name (slice doc.text p.prm_ty.tspan) :: !out) d.fn_params;
            let rec stmts b =
              List.iter
                (fun s ->
                  match s.sdesc with
                  | SLet (_, n, nsp, _, e) when nsp.lo < off && not (Hashtbl.mem seen n) && n.[0] <> '$' ->
                    Hashtbl.replace seen n ();
                    out := item 6 n (show e.ety) :: !out
                  | SFor (n, nsp, _, b) -> if nsp.lo < off && not (Hashtbl.mem seen n) then (Hashtbl.replace seen n (); out := item 6 n "" :: !out); stmts b
                  | SWhile (_, b) -> stmts b
                  | _ -> ())
                b.stmts
            in
            stmts d.fn_body;
            walk_block (fun e -> match e.edesc with EBlock b | EIf (_, b, _) | EGroup (_, _, b) -> stmts b | _ -> ()) d.fn_body;
            List.rev !out
          end
          else [])
        (fns items)
    in
    locals @ globals @ kws
    @ [ item 3 "panic" "panic(Str)"; item 3 "assert" "assert(Bool)"; item 3 "assert_eq" "assert_eq(T, T)";
        item 3 "channel" "channel(Int) -> Chan[T]" ]

(* ---------- protocol ---------- *)

let response id result = J.Obj [ ("jsonrpc", J.String "2.0"); ("id", id); ("result", result) ]

let error id code msg =
  J.Obj [ ("jsonrpc", J.String "2.0"); ("id", id); ("error", J.Obj [ ("code", J.Int code); ("message", J.String msg) ]) ]

let capabilities =
  J.Obj
    [ ("textDocumentSync", J.Int 1); ("hoverProvider", J.Bool true); ("definitionProvider", J.Bool true);
      ("completionProvider", J.Obj [ ("triggerCharacters", J.List [ J.String "." ]) ]);
      ("codeActionProvider", J.Bool true); ("documentSymbolProvider", J.Bool true) ]

let open_doc st uri text =
  let path = path_of_uri uri in
  let items, diags = analyze path text in
  let d = { path; text; items; diags } in
  Hashtbl.replace st.docs uri d;
  [ publish uri d ]

let position st params =
  let uri = J.to_str (J.path params [ "textDocument"; "uri" ]) in
  match Hashtbl.find_opt st.docs uri with
  | None -> None
  | Some d ->
    let line = J.to_int (J.path params [ "position"; "line" ]) and ch = J.to_int (J.path params [ "position"; "character" ]) in
    Some (uri, d, offset_of d.text line ch)

let handle st (msg : J.t) : J.t list =
  let id = J.member "id" msg in
  let params = J.member "params" msg in
  let reply r = if id = J.Null then [] else [ response id r ] in
  match J.to_str (J.member "method" msg) with
  | "initialize" ->
    reply (J.Obj [ ("capabilities", capabilities); ("serverInfo", J.Obj [ ("name", J.String "vela"); ("version", J.String "0.3") ]) ])
  | "initialized" -> []
  | "shutdown" -> st.shutdown <- true; reply J.Null
  | "exit" -> st.exit <- true; []
  | "textDocument/didOpen" ->
    open_doc st (J.to_str (J.path params [ "textDocument"; "uri" ])) (J.to_str (J.path params [ "textDocument"; "text" ]))
  | "textDocument/didChange" -> (
    match J.member "contentChanges" params with
    | J.List changes when changes <> [] ->
      open_doc st (J.to_str (J.path params [ "textDocument"; "uri" ])) (J.to_str (J.member "text" (List.nth changes (List.length changes - 1))))
    | _ -> [])
  | "textDocument/didClose" ->
    let uri = J.to_str (J.path params [ "textDocument"; "uri" ]) in
    Hashtbl.remove st.docs uri;
    [ J.Obj [ ("jsonrpc", J.String "2.0"); ("method", J.String "textDocument/publishDiagnostics");
              ("params", J.Obj [ ("uri", J.String uri); ("diagnostics", J.List []) ]) ] ]
  | "textDocument/hover" -> (
    match position st params with
    | Some (_, ({ items = Some items; _ } as d), off) -> (
      match smallest_at off (hover_items d.text items) with
      | Some (sp, s) ->
        reply
          (J.Obj
             [ ("contents", J.Obj [ ("kind", J.String "markdown"); ("value", J.String ("```vela\n" ^ s ^ "\n```")) ]);
               ("range", srange d.text sp) ])
      | None -> reply J.Null)
    | _ -> reply J.Null)
  | "textDocument/definition" -> (
    match position st params with
    | Some (uri, ({ items = Some items; _ } as d), off) -> (
      match smallest_at off (definitions items) with
      | Some (_, def) -> reply (J.Obj [ ("uri", J.String uri); ("range", srange d.text def) ])
      | None -> reply J.Null)
    | _ -> reply J.Null)
  | "textDocument/completion" -> (
    match position st params with
    | Some (_, d, off) ->
      let items = match member_completions d.path d.text off with Some l -> l | None -> general_completions d off in
      (* analysis of the patched text replaced the source table entry; restore it *)
      Source.add d.path d.text;
      reply (J.List items)
    | None -> reply (J.List []))
  | "textDocument/codeAction" -> (
    let uri = J.to_str (J.path params [ "textDocument"; "uri" ]) in
    match Hashtbl.find_opt st.docs uri with
    | None -> reply (J.List [])
    | Some d ->
      let r = J.member "range" params in
      let lo = offset_of d.text (J.to_int (J.path r [ "start"; "line" ])) (J.to_int (J.path r [ "start"; "character" ])) in
      let hi = offset_of d.text (J.to_int (J.path r [ "end"; "line" ])) (J.to_int (J.path r [ "end"; "character" ])) in
      let actions =
        List.filter_map
          (fun (dg : Diag.t) ->
            match dg.fix with
            | Some fx when dg.span.lo <= hi && lo <= dg.span.hi ->
              let edits =
                List.map
                  (fun (e : Diag.edit) -> J.Obj [ ("range", srange d.text e.at); ("newText", J.String e.replacement) ])
                  fx.edits
              in
              Some
                (J.Obj
                   [ ("title", J.String fx.fmsg); ("kind", J.String "quickfix");
                     ("diagnostics", J.List [ diag_json d.text dg ]); ("isPreferred", J.Bool true);
                     ("edit", J.Obj [ ("changes", J.Obj [ (uri, J.List edits) ]) ]) ])
            | _ -> None)
          d.diags
      in
      reply (J.List actions))
  | "textDocument/documentSymbol" -> (
    let uri = J.to_str (J.path params [ "textDocument"; "uri" ]) in
    match Hashtbl.find_opt st.docs uri with
    | Some { items = Some items; text; _ } ->
      let sym name kind (sp : Span.t) (whole : Span.t) =
        J.Obj [ ("name", J.String name); ("kind", J.Int kind); ("range", srange text whole); ("selectionRange", srange text sp) ]
      in
      reply
        (J.List
           (List.map
              (function
                | IFn d ->
                  let name = match d.fn_test with Some n -> "test \"" ^ n ^ "\"" | None -> d.fn_name in
                  sym name 12 d.fn_span (Span.merge d.fn_span d.fn_body.bspan)
                | IStruct s -> sym s.st_name 23 s.st_span s.st_span
                | IEnum e -> sym e.en_name 10 e.en_span e.en_span
                | IIface i -> sym i.if_name 11 i.if_span i.if_span)
              items))
    | _ -> reply (J.List []))
  | m -> if id = J.Null then [] else [ error id (-32601) ("method not supported: " ^ m) ]

(* ---------- stdio transport ---------- *)

let read_message ic =
  let rec headers len =
    match In_channel.input_line ic with
    | None -> None
    | Some l ->
      let l = String.trim l in
      if l = "" then len
      else
        match String.index_opt l ':' with
        | Some i when String.lowercase_ascii (String.sub l 0 i) = "content-length" ->
          headers (int_of_string_opt (String.trim (String.sub l (i + 1) (String.length l - i - 1))))
        | _ -> headers len
  in
  match headers None with
  | None -> None
  | Some n -> ( match In_channel.really_input_string ic n with Some s -> Some s | None -> None)

let write_message oc (j : J.t) =
  let s = J.to_string j in
  Printf.fprintf oc "Content-Length: %d\r\n\r\n%s" (String.length s) s;
  flush oc

let serve () =
  (* never run git from here: a git dependency is fetched by the CLI *)
  Package.offline := true;
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  let st = create () in
  let rec loop () =
    if not st.exit then
      match read_message stdin with
      | None -> ()
      | Some body ->
        (match J.parse body with
         | msg -> List.iter (write_message stdout) (handle st msg)
         | exception J.Error m -> prerr_endline ("vela lsp: bad JSON: " ^ m));
        loop ()
  in
  loop ();
  exit (if st.shutdown then 0 else 1)
