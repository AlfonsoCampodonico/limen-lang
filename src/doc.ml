(* vela doc: documentation from doc comments and signatures.

   A doc comment is the block of `//` lines directly above an item, a struct
   field, an enum variant or an interface method, with no blank line in
   between. The comment block at the top of a file, followed by a blank line,
   documents the module. Only `pub` items are documented unless [all] is set;
   methods are listed under their type. Output is Markdown or one static
   HTML page. Files are only parsed, not checked, so a module with type
   errors still gets its documentation. *)
open Ast

type member = { m_name : string; m_sig : string; m_doc : string }

type entry = {
  e_kind : string; (* fn, struct, enum, interface *)
  e_name : string;
  e_sig : string;
  e_doc : string;
  e_members : member list; (* fields, variants or interface methods *)
  mutable e_methods : member list;
}

type module_doc = {
  md_name : string;
  md_doc : string;
  md_types : entry list;
  md_fns : entry list;
  md_extra : (string * member list) list; (* methods on types declared elsewhere, such as Str *)
}

(* ---------- reading comments ---------- *)

let line_start text off =
  let i = ref (min off (String.length text)) in
  while !i > 0 && text.[!i - 1] <> '\n' do decr i done;
  !i

(* The previous line of [text] before the line starting at [ls], as (start, contents). *)
let prev_line text ls =
  if ls = 0 then None
  else
    let e = ls - 1 in
    let s = line_start text e in
    Some (s, String.sub text s (e - s))

let strip_comment l =
  let l = String.trim l in
  let l = String.sub l 2 (String.length l - 2) in
  if String.length l > 0 && l.[0] = ' ' then String.sub l 1 (String.length l - 1) else l

let is_comment_line l =
  let t = String.trim l in
  String.length t >= 2 && String.sub t 0 2 = "//"

(* The doc comment directly above the line holding offset [off]. *)
let doc_above text off =
  let rec go ls acc =
    match prev_line text ls with
    | Some (s, l) when is_comment_line l -> go s (strip_comment l :: acc)
    | _ -> acc
  in
  String.concat "\n" (go (line_start text off) [])

(* The comment block that opens the file, when a blank line separates it from what follows. *)
let module_comment text =
  let lines = String.split_on_char '\n' text in
  let rec take acc = function
    | l :: rest when is_comment_line l -> take (strip_comment l :: acc) rest
    | l :: _ when String.trim l = "" && acc <> [] -> String.concat "\n" (List.rev acc)
    | _ -> ""
  in
  take [] lines

(* ---------- signatures ---------- *)

let slice text (sp : Span.t) = String.sub text sp.lo (max 0 (min sp.hi (String.length text) - sp.lo))
let pub_kw p = if p then "pub " else ""

let generics names bounds =
  if names = [] then ""
  else
    "["
    ^ String.concat ", "
        (List.map
           (fun g -> match List.assoc_opt g bounds with Some bs when bs <> [] -> g ^ ": " ^ String.concat " + " (List.map fst bs) | _ -> g)
           names)
    ^ "]"

let fn_sig text (d : fn_decl) =
  let ps = List.map (fun p -> p.prm_name ^ ": " ^ slice text p.prm_ty.tspan) d.fn_params in
  let ret = match d.fn_ret with Some te -> " -> " ^ slice text te.tspan | None -> "" in
  Printf.sprintf "%sfn %s%s(%s)%s" (pub_kw d.fn_pub) d.fn_name (generics d.fn_gen d.fn_bounds) (String.concat ", " ps) ret

let iface_method_sig text (m, _, ps, r) =
  "fn " ^ m ^ "(" ^ String.concat ", " ("self" :: List.map (fun p -> p.prm_name ^ ": " ^ slice text p.prm_ty.tspan) ps) ^ ")"
  ^ match r with Some t -> " -> " ^ slice text t.tspan | None -> ""

let block_sig head lines = if lines = [] then head ^ " {}" else head ^ " {\n" ^ String.concat "" (List.map (fun l -> "    " ^ l ^ ",\n") lines) ^ "}"

(* ---------- building ---------- *)

let is_method (d : fn_decl) = d.fn_test = None && match d.fn_params with { prm_name = "self"; _ } :: _ -> true | _ -> false

let self_type (d : fn_decl) =
  match d.fn_params with { prm_ty = { tdesc = TyName (n, _); _ }; _ } :: _ -> n | _ -> "?"

let of_file ?(all = false) name text (f : file) =
  let shown pub = all || pub in
  let types = ref [] and fns = ref [] and methods = ref [] in
  List.iter
    (function
      | IFn d when d.fn_test <> None || not (shown d.fn_pub) -> ()
      | IFn d when is_method d ->
        methods := (self_type d, { m_name = d.fn_name; m_sig = fn_sig text d; m_doc = doc_above text d.fn_span.lo }) :: !methods
      | IFn d ->
        fns := { e_kind = "fn"; e_name = d.fn_name; e_sig = fn_sig text d; e_doc = doc_above text d.fn_span.lo; e_members = []; e_methods = [] } :: !fns
      | IStruct s when shown s.st_pub ->
        let fields =
          List.map
            (fun (f, (fsp : Span.t), te) ->
              (* an embedded struct is written as its type alone *)
              let sg = if List.mem f s.st_embeds then slice text te.tspan else f ^ ": " ^ slice text te.tspan in
              { m_name = f; m_sig = sg; m_doc = doc_above text fsp.lo })
            s.st_fields
        in
        types :=
          { e_kind = "struct"; e_name = s.st_name;
            e_sig = block_sig (Printf.sprintf "%sstruct %s%s" (pub_kw s.st_pub) s.st_name (generics s.st_gen [])) (List.map (fun m -> m.m_sig) fields);
            e_doc = doc_above text s.st_span.lo; e_members = List.filter (fun m -> m.m_doc <> "") fields; e_methods = [] }
          :: !types
      | IEnum e when shown e.en_pub ->
        let vs =
          List.map
            (fun (v, (vsp : Span.t), tes) ->
              { m_name = v; m_sig = (if tes = [] then v else v ^ "(" ^ String.concat ", " (List.map (fun te -> slice text te.tspan) tes) ^ ")");
                m_doc = doc_above text vsp.lo })
            e.en_variants
        in
        types :=
          { e_kind = "enum"; e_name = e.en_name;
            e_sig = block_sig (Printf.sprintf "%senum %s%s" (pub_kw e.en_pub) e.en_name (generics e.en_gen [])) (List.map (fun m -> m.m_sig) vs);
            e_doc = doc_above text e.en_span.lo; e_members = List.filter (fun m -> m.m_doc <> "") vs; e_methods = [] }
          :: !types
      | IIface i when shown i.if_pub ->
        let ms = List.map (fun ((m, (msp : Span.t), _, _) as x) -> { m_name = m; m_sig = iface_method_sig text x; m_doc = doc_above text msp.lo }) i.if_methods in
        types :=
          { e_kind = "interface"; e_name = i.if_name;
            e_sig = block_sig (Printf.sprintf "%sinterface %s" (pub_kw i.if_pub) i.if_name) (List.map (fun m -> m.m_sig) ms);
            e_doc = doc_above text i.if_span.lo; e_members = List.filter (fun m -> m.m_doc <> "") ms; e_methods = [] }
          :: !types
      | IType a when shown a.ty_pub ->
        types :=
          { e_kind = "type"; e_name = a.ty_name;
            e_sig = Printf.sprintf "%stype %s%s = %s" (pub_kw a.ty_pub) a.ty_name (generics a.ty_gen []) (slice text a.ty_body.tspan);
            e_doc = doc_above text a.ty_span.lo; e_members = []; e_methods = [] }
          :: !types
      | _ -> ())
    f.items;
  let types = List.rev !types in
  let extra = ref [] in
  List.iter
    (fun (t, m) ->
      match List.find_opt (fun e -> e.e_name = t) types with
      | Some e -> e.e_methods <- e.e_methods @ [ m ]
      | None -> (
        match List.assoc_opt t !extra with
        | Some l -> extra := (t, l @ [ m ]) :: List.remove_assoc t !extra
        | None -> extra := !extra @ [ (t, [ m ]) ]))
    (List.rev !methods);
  { md_name = name; md_doc = module_comment text; md_types = types; md_fns = List.rev !fns; md_extra = !extra }

let load_source ?all name path text = of_file ?all name text (Parser.parse_file path text)

(* ---------- Markdown ---------- *)

let code_block s = "```vela\n" ^ s ^ "\n```\n"

let markdown (mods : module_doc list) =
  let b = Buffer.create 4096 in
  let p s = Buffer.add_string b s in
  let para d = if d <> "" then p (d ^ "\n\n") in
  let methods title ms =
    if ms <> [] then begin
      p (title ^ "\n\n");
      List.iter (fun m -> p (code_block m.m_sig); p "\n"; para m.m_doc) ms
    end
  in
  List.iteri
    (fun i md ->
      if i > 0 then p "\n";
      p ("# Module `" ^ md.md_name ^ "`\n\n");
      para md.md_doc;
      if md.md_types <> [] then begin
        p "## Types\n\n";
        List.iter
          (fun e ->
            p (Printf.sprintf "### %s `%s`\n\n" e.e_kind e.e_name);
            p (code_block e.e_sig);
            p "\n";
            para e.e_doc;
            List.iter (fun m -> p (Printf.sprintf "- `%s`: %s\n" m.m_name (String.concat " " (String.split_on_char '\n' m.m_doc)))) e.e_members;
            if e.e_members <> [] then p "\n";
            methods "#### Methods" e.e_methods)
          md.md_types
      end;
      List.iter (fun (t, ms) -> p (Printf.sprintf "## Methods on `%s`\n\n" t); List.iter (fun m -> p (code_block m.m_sig); p "\n"; para m.m_doc) ms) md.md_extra;
      if md.md_fns <> [] then begin
        p "## Functions\n\n";
        List.iter (fun e -> p (Printf.sprintf "### fn `%s`\n\n" e.e_name); p (code_block e.e_sig); p "\n"; para e.e_doc) md.md_fns
      end)
    mods;
  (* one trailing newline *)
  let s = Buffer.contents b in
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = '\n' do decr n done;
  String.sub s 0 !n ^ "\n"

(* ---------- HTML ---------- *)

let escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '<' -> Buffer.add_string b "&lt;" | '>' -> Buffer.add_string b "&gt;" | '&' -> Buffer.add_string b "&amp;"
      | '"' -> Buffer.add_string b "&quot;" | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* Doc text: paragraphs split at blank lines, `code` spans. *)
let prose s =
  let inline l =
    let parts = String.split_on_char '`' l in
    String.concat "" (List.mapi (fun i x -> if i mod 2 = 1 then "<code>" ^ escape x ^ "</code>" else escape x) parts)
  in
  let paras = ref [] and cur = ref [] in
  let flush () = if !cur <> [] then (paras := String.concat " " (List.rev !cur) :: !paras; cur := []) in
  List.iter (fun l -> if String.trim l = "" then flush () else cur := String.trim l :: !cur) (String.split_on_char '\n' s);
  flush ();
  String.concat "" (List.rev_map (fun x -> "<p>" ^ inline x ^ "</p>\n") !paras)

let anchor parts = String.concat "-" (List.map (String.map (fun c -> if c = ':' || c = ' ' then '-' else c)) parts)

let style =
  {|:root{--bg:#fff;--fg:#1d1d1f;--muted:#6e6e73;--line:#e5e5ea;--code:#f5f5f7;--link:#0a58ca}
@media (prefers-color-scheme:dark){:root{--bg:#161618;--fg:#ececf0;--muted:#9a9aa2;--line:#2e2e33;--code:#222226;--link:#7ab0ff}}
*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--fg);font:16px/1.55 system-ui,-apple-system,sans-serif}
a{color:var(--link);text-decoration:none}a:hover{text-decoration:underline}
.wrap{display:flex;max-width:1100px;margin:0 auto}nav{flex:0 0 220px;padding:24px 16px;border-right:1px solid var(--line);position:sticky;top:0;align-self:flex-start;max-height:100vh;overflow:auto;font-size:14px}
nav ul{list-style:none;margin:0 0 12px;padding:0}nav li{margin:2px 0}nav .mod{font-weight:600;margin-top:12px}
main{flex:1;min-width:0;padding:24px 32px}h1{font-size:28px;margin:32px 0 8px}h1:first-child{margin-top:0}h2{font-size:20px;margin:28px 0 8px;border-bottom:1px solid var(--line);padding-bottom:4px}
h3{font-size:17px;margin:22px 0 6px}h4{font-size:15px;margin:16px 0 6px;color:var(--muted)}
pre{background:var(--code);padding:10px 12px;border-radius:6px;overflow:auto;font-size:14px;margin:6px 0}code{font:14px ui-monospace,SFMono-Regular,Menlo,monospace}
p code{background:var(--code);padding:1px 4px;border-radius:4px}dl{margin:6px 0 6px 12px}dt{font-family:ui-monospace,Menlo,monospace;font-size:14px}dd{margin:0 0 6px 16px;color:var(--muted)}
.kind{color:var(--muted);font-weight:400}
@media (max-width:720px){.wrap{display:block}nav{position:static;border-right:0;border-bottom:1px solid var(--line);max-height:none}main{padding:16px}}|}

let html ?(title = "Vela documentation") (mods : module_doc list) =
  let b = Buffer.create 8192 in
  let p s = Buffer.add_string b s in
  p "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n";
  p ("<title>" ^ escape title ^ "</title>\n<style>\n" ^ style ^ "\n</style>\n</head>\n<body>\n<div class=\"wrap\">\n<nav>\n");
  List.iter
    (fun md ->
      p (Printf.sprintf "<div class=\"mod\"><a href=\"#%s\">%s</a></div>\n<ul>\n" (anchor [ md.md_name ]) (escape md.md_name));
      List.iter (fun e -> p (Printf.sprintf "<li><a href=\"#%s\">%s</a></li>\n" (anchor [ md.md_name; e.e_name ]) (escape e.e_name))) md.md_types;
      List.iter (fun e -> p (Printf.sprintf "<li><a href=\"#%s\">%s()</a></li>\n" (anchor [ md.md_name; e.e_name ]) (escape e.e_name))) md.md_fns;
      p "</ul>\n")
    mods;
  p "</nav>\n<main>\n";
  let pre s = p ("<pre><code>" ^ escape s ^ "</code></pre>\n") in
  let methods md owner ms =
    List.iter
      (fun m ->
        p (Printf.sprintf "<div id=\"%s\">" (anchor [ md.md_name; owner; m.m_name ]));
        pre m.m_sig;
        p (prose m.m_doc);
        p "</div>\n")
      ms
  in
  List.iter
    (fun md ->
      p (Printf.sprintf "<h1 id=\"%s\"><span class=\"kind\">module</span> %s</h1>\n" (anchor [ md.md_name ]) (escape md.md_name));
      p (prose md.md_doc);
      if md.md_types <> [] then p "<h2>Types</h2>\n";
      List.iter
        (fun e ->
          p (Printf.sprintf "<h3 id=\"%s\"><span class=\"kind\">%s</span> %s</h3>\n" (anchor [ md.md_name; e.e_name ]) e.e_kind (escape e.e_name));
          pre e.e_sig;
          p (prose e.e_doc);
          if e.e_members <> [] then begin
            p "<dl>\n";
            List.iter (fun m -> p (Printf.sprintf "<dt>%s</dt><dd>%s</dd>\n" (escape m.m_name) (prose m.m_doc))) e.e_members;
            p "</dl>\n"
          end;
          if e.e_methods <> [] then (p "<h4>Methods</h4>\n"; methods md e.e_name e.e_methods))
        md.md_types;
      List.iter (fun (t, ms) -> p (Printf.sprintf "<h2>Methods on %s</h2>\n" (escape t)); methods md t ms) md.md_extra;
      if md.md_fns <> [] then p "<h2>Functions</h2>\n";
      List.iter
        (fun e ->
          p (Printf.sprintf "<h3 id=\"%s\"><span class=\"kind\">fn</span> %s</h3>\n" (anchor [ md.md_name; e.e_name ]) (escape e.e_name));
          pre e.e_sig;
          p (prose e.e_doc))
        md.md_fns)
    mods;
  p "</main>\n</div>\n</body>\n</html>\n";
  Buffer.contents b

(* ---------- inputs ---------- *)

let std_names () = List.map fst !Modules.std_modules

(* A file path, or the name of a standard library module. *)
let load ?all arg =
  if Sys.file_exists arg then
    let text = In_channel.with_open_bin arg In_channel.input_all in
    Source.add arg text;
    load_source ?all (Filename.remove_extension (Filename.basename arg)) arg text
  else
    match List.assoc_opt arg !Modules.std_modules with
    | Some text ->
      let path = "<std>/" ^ arg ^ ".vela" in
      Source.add path text;
      load_source ?all arg path text
    | None -> raise (Sys_error (arg ^ ": no such file or standard library module"))
