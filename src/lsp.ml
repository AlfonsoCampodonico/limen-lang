(* Language server: diagnostics, hover (types and doc comments), go to
   definition, find references, rename, completion, quick fixes from the
   checker's auto-fixes, and document symbols. Definitions, references and
   rename work across the modules the open file uses (see Refs).
   [handle] is pure request -> messages, so it can be tested without stdio.
   Positions: LSP lines and characters are mapped to byte offsets, which is
   exact for ASCII source. *)
open Ast
module J = Json

(* [items]: the whole checked program, every module the file uses included *)
type doc = { path : string; text : string; items : item list option; diags : Diag.t list; occs : Refs.occ list }
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
(* The source of [sp], from its own file when that is another module. *)
let slice text (sp : Span.t) =
  let text = match Source.text sp.file with Some t when sp.file <> "" -> t | _ -> text in
  if sp.lo >= String.length text then "" else String.sub text sp.lo (max 0 (min sp.hi (String.length text) - sp.lo))

(* ---------- analysis ---------- *)

(* The program as loaded from [path], all diagnostics included. *)
let load_program path text =
  match Modules.load path text with
  | [], (_ :: _ as ds) -> ([], ds)
  | items, _ ->
    let ds, _ = Check.check_program items in
    (Embed.user_items items, ds)

let analyze path text =
  match load_program path text with
  | [], ds -> (None, List.filter (fun (d : Diag.t) -> d.span.file = path) ds)
  | items, ds -> (Some items, List.filter (fun (d : Diag.t) -> d.span.file = path) ds)

let mine path items = List.filter (fun it -> (Modules.item_span it).Span.file = path) items

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
  | EBin (_, a, b) | EIndex (a, b) | EPipe (a, b) -> w a; w b
  | EUn (_, a) | ETry (a, None) | EField (a, _, _) -> w a
  | ETry (a, Some c) -> w a; w c
  | ECall (c, args) -> w c; List.iter w args
  | ENamed (_, _, a) -> w a
  | EMethod (r, _, _, args) -> w r; List.iter w args
  | EIf (c, b, eo) -> w c; walk_block f b; Option.iter w eo
  | EMatch (s, arms) -> w s; List.iter (fun a -> Option.iter w a.aguard; w a.abody) arms
  | EBlock b -> walk_block f b
  | EGroup (_, until, b) -> Option.iter (fun (c, ms) -> w c; w ms) until; walk_block f b
  | EClosure (_, body) -> w body
  | EStruct (_, _, fs) -> List.iter (fun (_, _, x) -> w x) fs
  | EWith (b, fs) -> w b; List.iter (fun (_, _, x) -> w x) fs
  | ESafe (b, _, rest, _) -> w b; w rest
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
      | SDefer b -> walk_block f b
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

(* `test "name"` or `bench "name"` *)
let test_label (d : fn_decl) n = (if d.fn_bench then "bench" else "test") ^ " \"" ^ n ^ "\""

let hover_items text items =
  let acc = ref [] in
  let add (sp : Span.t) s = acc := (sp, s) :: !acc in
  List.iter
    (function
      | IFn d ->
        add d.fn_span (match d.fn_test with Some n -> test_label d n | None -> fn_signature text d);
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
        List.iter
          (fun (f, sp, te) -> add sp (if List.mem f s.st_embeds then "embedded " ^ slice text te.tspan else f ^ ": " ^ slice text te.tspan))
          s.st_fields
      | IEnum e ->
        add e.en_span ("enum " ^ e.en_name);
        List.iter
          (fun (v, sp, tes) ->
            add sp (if tes = [] then v else v ^ "(" ^ String.concat ", " (List.map (fun te -> slice text te.tspan) tes) ^ ")"))
          e.en_variants
      | IType a -> add a.ty_span ("type " ^ a.ty_name ^ " = " ^ slice text a.ty_body.tspan))
    items;
  !acc

let smallest_at ?file off pairs =
  List.fold_left
    (fun best ((sp : Span.t), v) ->
      if (match file with Some f -> sp.file = f | None -> true) && sp.lo <= off && off < max sp.hi (sp.lo + 1) then
        match best with
        | Some ((b : Span.t), _) when b.hi - b.lo <= sp.hi - sp.lo -> best
        | _ -> Some (sp, v)
      else best)
    None pairs

(* ---------- completion ---------- *)

let keywords = [ "fn"; "let"; "mut"; "if"; "else"; "match"; "for"; "in"; "while"; "return"; "struct"; "enum"; "true"; "false"; "group"; "break"; "continue" ]

let item kind label detail = J.Obj [ ("label", J.String label); ("kind", J.Int kind); ("detail", J.String detail) ]

let is_ident_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

(* The modules the file names with `use`. *)
let uses_of text =
  List.filter_map
    (fun l ->
      let l = String.trim l in
      if String.starts_with ~prefix:"use " l then
        let r = String.trim (String.sub l 4 (String.length l - 4)) in
        let n = ref 0 in
        while !n < String.length r && is_ident_char r.[!n] do incr n done;
        if !n > 0 then Some (String.sub r 0 !n) else None
      else None)
    (String.split_on_char '\n' text)

(* After `json.`: the pub functions, types and variants of module json. *)
let module_completions path text name =
  if not (List.mem name (uses_of text)) then None
  else
    match Modules.find_module ~deps:[] ~from:path (Filename.dirname path) name with
    | Some (p, Some src, _) -> (
      match Parser.parse_file p src with
      | exception Diag.Fatal _ -> Some []
      | f ->
        let documented kind label detail (sp : Span.t) =
          let d = Doc.doc_above src sp.lo in
          let base = [ ("label", J.String label); ("kind", J.Int kind); ("detail", J.String detail) ] in
          J.Obj (if d = "" then base else base @ [ ("documentation", J.Obj [ ("kind", J.String "markdown"); ("value", J.String d) ]) ])
        in
        Some
          (List.concat_map
             (function
               | IFn d when d.fn_pub && d.fn_test = None && not (Modules.is_method (IFn d)) -> [ documented 3 d.fn_name (Doc.fn_sig src d) d.fn_span ]
               | IStruct st when st.st_pub -> [ documented 22 st.st_name "struct" st.st_span ]
               | IIface i when i.if_pub -> [ documented 8 i.if_name "interface" i.if_span ]
               | IType a when a.ty_pub -> [ documented 25 a.ty_name "type" a.ty_span ]
               | IEnum e when e.en_pub ->
                 documented 13 e.en_name "enum" e.en_span :: List.map (fun (v, vsp, _) -> documented 20 v e.en_name vsp) e.en_variants
               | _ -> [])
             f.items))
    | _ -> Some []

let member_completions path text off =
  (* cursor after `recv.` or `recv.pre`: drop `.pre`, check, and look up the receiver's type *)
  let start = ref off in
  while !start > 0 && is_ident_char text.[!start - 1] do decr start done;
  if !start = 0 || text.[!start - 1] <> '.' then None
  else
    let dot = !start - 1 in
    (* after `recv?.`, the receiver ends before the `?` and the members are those of the value in the Option *)
    let safe = dot > 0 && text.[dot - 1] = '?' in
    let recv_end = if safe then dot - 1 else dot in
    let w = ref dot in
    while !w > 0 && is_ident_char text.[!w - 1] do decr w done;
    let before = String.sub text !w (dot - !w) in
    let module_name = if before <> "" && (!w = 0 || text.[!w - 1] <> '.') then module_completions path text before else None in
    match module_name with
    | Some l -> Some l
    | None -> begin
    (* Two ways to turn the text into a program where the receiver ends at
       the dot: cut the rest of the line (handles `recv.|name(args)`), or drop
       just the word being completed (handles receivers spanning lines). *)
    let stop = ref off in
    while !stop < String.length text && is_ident_char text.[!stop] do incr stop done;
    let eol = match String.index_from_opt text off '\n' with Some i -> i | None -> String.length text in
    let candidates =
      [ String.sub text 0 recv_end ^ String.sub text eol (String.length text - eol);
        String.sub text 0 recv_end ^ String.sub text !stop (String.length text - !stop) ]
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
                if e.espan.hi = recv_end && e.espan.file = path then
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
      let t = match t with Types.TNamed ("Option", [ inner ]) when safe -> Types.repr inner | _ -> t in
      let fields =
        match t with
        | Types.TCap Types.Root -> List.map (fun (f, ft) -> item 5 f (show ft)) Check.root_fields
        | _ -> (
          match struct_decl items t with
          | Some s ->
            List.map (fun (f, _, te) -> item 5 f (slice patched te.tspan)) s.st_fields
            (* and those promoted from embedded structs *)
            @ List.map
                (fun (f, te, owner) -> item 5 f (Embed.show_ty te ^ " (from " ^ owner ^ ")"))
                (Embed.promoted_fields
                   ~structs:(fun n -> List.find_opt (fun s -> s.st_name = n) (structs items))
                   ~has_method:(fun t m -> Hashtbl.mem Check.method_owner (t ^ "::" ^ m) && not (Embed.is_forwarder (t ^ "::" ^ m)))
                   s)
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
                (* a method promoted from an embedded struct: the signature of the one it calls *)
                let k' = Option.value ~default:k (Hashtbl.find_opt Embed.forwarder_of k) in
                let sg = match List.find_opt (fun d -> d.fn_name = k') (fns items) with Some d -> fn_signature patched d | None -> "method" in
                item 2 m sg :: acc
              end
              else acc)
            Check.method_owner []
          @ List.map (fun (m, mk) -> item 2 m (method_sig m (mk ()))) (Check.methods t)
        | _ -> List.map (fun (m, mk) -> item 2 m (method_sig m (mk ()))) (Check.methods t)
      in
      (* the built-in methods written in Vela (std/methods.vela) *)
      let vela_methods =
        match Check.builtin_tname t with
        | None -> []
        | Some tn ->
          let pre = tn ^ "::" in
          List.filter_map
            (function
              | IFn d when String.starts_with ~prefix:pre d.fn_name ->
                Some (item 2 (String.sub d.fn_name (String.length pre) (String.length d.fn_name - String.length pre)) (fn_signature Methods_text.text d))
              | _ -> None)
            (Lazy.force Prelude.methods)
      in
      Some (fields @ methods @ vela_methods)
  end

(* Inside the parentheses of `f(` or `x.m(`: the parameter names of [f] (or
   of the methods named [m]) as named-argument labels, `retries: `. When the
   file does not parse (the call is not closed yet), the call is closed
   right after its `(` and the rest of the line dropped. *)
let label_completions path text off items =
  let rec back i depth =
    if i < 0 then None
    else
      match text.[i] with
      | ')' | ']' | '}' -> back (i - 1) (depth + 1)
      | '(' when depth = 0 -> Some i
      | '(' | '[' | '{' -> if depth = 0 then None else back (i - 1) (depth - 1)
      | _ -> back (i - 1) depth
  in
  match back (min off (String.length text) - 1) 0 with
  | None -> []
  | Some p ->
    let s = ref p in
    while !s > 0 && is_ident_char text.[!s - 1] do decr s done;
    let name = String.sub text !s (p - !s) in
    let is_method = !s > 0 && text.[!s - 1] = '.' in
    let ends_with n = String.ends_with ~suffix:("::" ^ name) n in
    let items =
      match items with
      | Some items -> Some items
      | None ->
        let eol = match String.index_from_opt text p '\n' with Some i -> i | None -> String.length text in
        fst (analyze path (String.sub text 0 p ^ "()" ^ String.sub text eol (String.length text - eol)))
    in
    match items with
    | None -> []
    | Some items ->
    let items = mine path items in
    if name = "" then []
    else
      List.concat_map
        (fun d ->
          if d.fn_test = None && (if is_method then ends_with d.fn_name else d.fn_name = name) then
            List.filter_map
              (fun p ->
                if p.prm_name = "self" then None
                else
                  Some
                    (J.Obj
                       [ ("label", J.String (p.prm_name ^ ":")); ("kind", J.Int 5);
                         ("detail", J.String (slice text p.prm_ty.tspan)); ("insertText", J.String (p.prm_name ^ ": ")) ]))
              d.fn_params
          else [])
        (fns items)

let general_completions doc off =
  let kws = List.map (fun k -> item 14 k "keyword") keywords in
  let labels = label_completions doc.path doc.text off doc.items in
  match doc.items with
  | None -> labels @ kws
  | Some items ->
    let items = mine doc.path items in
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
    labels @ locals @ globals @ kws
    @ [ item 3 "panic" "panic(Str)"; item 3 "assert" "assert(Bool)"; item 3 "assert_eq" "assert_eq(T, T)"; item 3 "keep" "keep(T)";
        item 3 "channel" "channel(Int) -> Chan[T]"; item 3 "embed" "embed(\"path\") -> Str, read at compile time";
        item 3 "embed_bytes" "embed_bytes(\"path\") -> Bytes, read at compile time";
        item 3 "embed_dir" "embed_dir(\"path\") -> Map[Str, Bytes], every file under a directory, read at compile time" ]

(* ---------- protocol ---------- *)

let response id result = J.Obj [ ("jsonrpc", J.String "2.0"); ("id", id); ("result", result) ]

let error id code msg =
  J.Obj [ ("jsonrpc", J.String "2.0"); ("id", id); ("error", J.Obj [ ("code", J.Int code); ("message", J.String msg) ]) ]

let capabilities =
  J.Obj
    [ ("textDocumentSync", J.Int 1); ("hoverProvider", J.Bool true); ("definitionProvider", J.Bool true);
      ("completionProvider", J.Obj [ ("triggerCharacters", J.List [ J.String "." ]) ]);
      ("codeActionProvider", J.Bool true); ("documentSymbolProvider", J.Bool true); ("referencesProvider", J.Bool true);
      ("renameProvider", J.Obj [ ("prepareProvider", J.Bool true) ]) ]

(* Other open files are read from the editor, not from disk. *)
let set_overlay st path =
  Modules.overlay := Hashtbl.fold (fun _ d acc -> if d.path = path then acc else (d.path, d.text) :: acc) st.docs []

let open_doc st uri text =
  let path = path_of_uri uri in
  set_overlay st path;
  let items, diags = analyze path text in
  let occs = match items with Some it -> Refs.index it | None -> [] in
  let d = { path; text; items; diags; occs } in
  Hashtbl.replace st.docs uri d;
  [ publish uri d ]

let position st params =
  let uri = J.to_str (J.path params [ "textDocument"; "uri" ]) in
  match Hashtbl.find_opt st.docs uri with
  | None -> None
  | Some d ->
    let line = J.to_int (J.path params [ "position"; "line" ]) and ch = J.to_int (J.path params [ "position"; "character" ]) in
    Some (uri, d, offset_of d.text line ch)

(* ---------- locations in any module ---------- *)

let uri_of_path p = if String.starts_with ~prefix:"file://" p then p else "file://" ^ p

(* A standard library module has no file; write its text to a temporary one so
   an editor can open it. *)
let std_file p =
  let name = String.sub p 6 (String.length p - 6) in
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "vela-std" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error _ -> ());
  let f = Filename.concat dir name in
  let text = Option.value ~default:"" (Source.text p) in
  if Modules.read_file f <> Some text then (try Out_channel.with_open_bin f (fun oc -> output_string oc text) with Sys_error _ -> ());
  f

(* The editor's own URI for an open file, so it matches what the client sent. *)
let uri_for st path =
  match Hashtbl.fold (fun uri d acc -> if d.path = path then Some uri else acc) st.docs None with
  | Some u -> u
  | None -> uri_of_path (if String.starts_with ~prefix:"<std>/" path then std_file path else path)

let location st (sp : Span.t) =
  let text = Option.value ~default:"" (Source.text sp.file) in
  J.Obj [ ("uri", J.String (uri_for st sp.file)); ("range", range text sp.lo sp.hi) ]

(* Hover for a name defined elsewhere: its declaration and doc comment. *)
let symbol_hover d items off =
  match Refs.at d.occs d.path off with
  | Some o when o.key.[0] <> 'l' -> (
    match Refs.definition d.occs o.key with
    | Some def -> (
      let text = Option.value ~default:"" (Source.text def.sp.file) in
      match smallest_at ~file:def.sp.file def.sp.lo (hover_items text items) with
      | Some (_, sg) -> Some (o.sp, sg, Doc.doc_above text def.sp.lo)
      | None -> None)
    | None -> None)
  | _ -> None

let rename_load st d overrides =
  let main = match List.assoc_opt d.path overrides with Some t -> t | None -> d.text in
  set_overlay st d.path;
  Modules.overlay := List.filter (fun (p, _) -> p <> d.path) overrides @ !Modules.overlay;
  let r = load_program d.path main in
  set_overlay st d.path;
  r

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
      let shown sp s doc =
        reply
          (J.Obj
             [ ("contents", J.Obj [ ("kind", J.String "markdown"); ("value", J.String ("```vela\n" ^ s ^ "\n```" ^ if doc = "" then "" else "\n\n" ^ doc)) ]);
               ("range", srange d.text sp) ])
      in
      match symbol_hover d items off with
      | Some (sp, s, doc) -> shown sp s doc
      | None -> (
        match smallest_at ~file:d.path off (hover_items d.text items) with
        | Some (sp, s) -> shown sp s ""
        | None -> reply J.Null))
    | _ -> reply J.Null)
  | "textDocument/definition" -> (
    match position st params with
    | Some (_, d, off) -> (
      match Refs.at d.occs d.path off with
      | Some o -> ( match Refs.definition d.occs o.key with Some def -> reply (location st def.sp) | None -> reply J.Null)
      | None -> reply J.Null)
    | _ -> reply J.Null)
  | "textDocument/references" -> (
    match position st params with
    | Some (_, d, off) -> (
      let with_decl = J.path params [ "context"; "includeDeclaration" ] <> J.Bool false in
      match Refs.at d.occs d.path off with
      | Some o ->
        reply (J.List (List.filter_map (fun (r : Refs.occ) -> if r.def && not with_decl then None else Some (location st r.sp)) (Refs.references d.occs o.key)))
      | None -> reply J.Null)
    | None -> reply J.Null)
  | "textDocument/prepareRename" -> (
    match position st params with
    | Some (_, d, off) -> (
      match Refs.at d.occs d.path off with
      | Some o -> reply (J.Obj [ ("range", srange d.text o.sp); ("placeholder", J.String o.name) ])
      | None -> reply J.Null)
    | None -> reply J.Null)
  | "textDocument/rename" -> (
    match position st params with
    | Some (_, ({ items = Some items; _ } as d), off) -> (
      let nw = J.to_str (J.member "newName" params) in
      match Refs.rename ~load:(rename_load st d) items d.path off nw with
      | Error m -> [ error id (-32803) m ]
      | Ok files ->
        Source.add d.path d.text;
        reply
          (J.Obj
             [ ("changes",
                J.Obj
                  (List.map
                     (fun (f, edits) ->
                       let text = Option.value ~default:"" (Source.text f) in
                       ( uri_for st f,
                         J.List (List.map (fun (e : Refs.edit) -> J.Obj [ ("range", srange text e.at); ("newText", J.String e.replacement) ]) edits) ))
                     files)) ]))
    | Some _ -> [ error id (-32803) "the file has errors that stop it from loading" ]
    | None -> reply J.Null)
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
    | Some { items = Some items; text; path; _ } ->
      let items = mine path items in
      let sym name kind (sp : Span.t) (whole : Span.t) =
        J.Obj [ ("name", J.String name); ("kind", J.Int kind); ("range", srange text whole); ("selectionRange", srange text sp) ]
      in
      reply
        (J.List
           (List.map
              (function
                | IFn d ->
                  let name = match d.fn_test with Some n -> test_label d n | None -> d.fn_name in
                  (* a table test's case selects its own text *)
                  let sel = match d.fn_table with Some (_, c) -> c.espan | None -> d.fn_span in
                  sym name 12 sel (Span.merge d.fn_span d.fn_body.bspan)
                | IStruct s -> sym s.st_name 23 s.st_span s.st_span
                | IEnum e -> sym e.en_name 10 e.en_span e.en_span
                | IIface i -> sym i.if_name 11 i.if_span i.if_span
                | IType a -> sym a.ty_name 26 a.ty_span a.ty_span)
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
