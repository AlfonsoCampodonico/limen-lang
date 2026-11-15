(* The package index: a searchable list of git URLs and tags, with the
   capabilities each package needs. Nothing is hosted: an entry points at a
   tag of a git repository, and `limen add` fetches it from there like any
   git dependency.

     [[package]]
     name = "color"
     git = "https://example.com/color.git"
     tag = "v1.2.0"
     commit = "8d022128f5f177019f1e6aed46f75c93c90815f1"
     description = "Colors for terminal output"
     caps = ["stdout"]

   One [[package]] table per version. `commit` is what the tag pointed at
   when the entry was written, so a tag moved since is caught. `caps` are
   written as in limen.toml, plus "foreign" for a package that calls C.

   LIMEN_INDEX names the index: a git URL (cloned on each use), a directory
   (every .toml file at its top), or one file. There is no default. *)

open Printf

type entry = { name : string; git : string; tag : string; commit : string; description : string; caps : string list }

exception Failed of string

let fail fmt = ksprintf (fun m -> raise (Failed m)) fmt

let help = "set LIMEN_INDEX to a package index: a git URL, or a directory or file of [[package]] entries"

let name_ok n =
  n <> "" && (match n.[0] with 'a' .. 'z' | '_' -> true | _ -> false)
  && String.for_all (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false) n

let where (sp : Span.t) = let l, _ = Source.line_col sp.file sp.lo in sprintf "%s:%d" sp.file l

(* ---------- reading the index ---------- *)

let fields = [ "name"; "git"; "tag"; "commit"; "description"; "caps" ]

let parse_index file src : entry list =
  let bad sp m = raise (Package.Bad (sp, m)) in
  let entries =
    List.map
      (fun (tb : Package.table) ->
        if not (tb.tarray && tb.tname = "package") then bad tb.tspan "an index holds only [[package]] tables, one for each version of a package";
        let get k = List.find_map (fun (k', v, sp) -> if k' = k then Some (v, sp) else None) tb.keys in
        let str k =
          match get k with
          | Some (Package.VStr s, _) -> s
          | Some (_, sp) -> bad sp (sprintf "`%s` is a string" k)
          | None -> bad tb.tspan (sprintf "this entry has no `%s`" k)
        in
        List.iter
          (fun (k, _, sp) -> if not (List.mem k fields) then bad sp (sprintf "unknown field `%s`; an entry has %s" k (String.concat ", " fields)))
          tb.keys;
        let name = str "name" and git = str "git" and tag = str "tag" and commit = str "commit" in
        let sp k = match get k with Some (_, sp) -> sp | None -> tb.tspan in
        if not (name_ok name) then bad (sp "name") "a package name is lowercase letters, digits and `_`, as a module name";
        if not (Package.git_url_ok git) then bad (sp "git") "`git` must be an https://, http://, ssh://, git:// or file:// URL, or user@host:path";
        if not (Package.tag_ok tag) then bad (sp "tag") "bad tag";
        if not (Package.commit_ok commit) then bad (sp "commit") "`commit` is the commit the tag points at, in lowercase hex";
        let description = match get "description" with None -> "" | Some _ -> str "description" in
        let caps =
          match get "caps" with
          | None -> []
          | Some (Package.VArr cs, sp) ->
            List.iter
              (fun c ->
                if not (List.mem c (Package.cap_grants @ [ "foreign" ])) then
                  bad sp (sprintf "unknown capability `%s`; use %s or foreign" c (String.concat ", " Package.cap_grants)))
              cs;
            cs
          | Some (_, sp) -> bad sp "`caps` is a list of strings"
        in
        ({ name; git; tag; commit; description; caps }, tb.tspan))
      (Package.read file src)
  in
  List.iteri
    (fun i (e, sp) ->
      if List.exists (fun (e', _) -> e'.name = e.name && e'.tag = e.tag) (List.filteri (fun j _ -> j < i) entries) then
        bad sp (sprintf "%s %s is in the index twice" e.name e.tag))
    entries;
  List.map fst entries

(* [files] are (path to read, name to show in errors). *)
let read_files files =
  List.concat_map
    (fun (path, shown) ->
      let src = try In_channel.with_open_bin path In_channel.input_all with Sys_error m -> fail "cannot read the package index: %s" m in
      try parse_index shown src with Package.Bad (sp, m) -> fail "%s: %s" (where sp) m)
    files

let toml_files dir =
  List.filter (fun f -> Filename.check_suffix f ".toml") (List.sort compare (Array.to_list (Sys.readdir dir)))

let load () : entry list =
  match Sys.getenv_opt "LIMEN_INDEX" with
  | None -> fail "no package index is set; %s" help
  | Some loc when String.trim loc = "" -> fail "no package index is set; %s" help
  | Some loc ->
    if Sys.file_exists loc then
      if Sys.is_directory loc then begin
        let fs = toml_files loc in
        if fs = [] then fail "the package index %s has no .toml files" loc;
        read_files (List.map (fun f -> (Filename.concat loc f, Filename.concat loc f)) fs)
      end
      else read_files [ (loc, loc) ]
    else if Package.git_url_ok loc then begin
      let tmp = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "limen-index-%d-%d" (Unix.getpid ()) (Random.bits ())) in
      Fun.protect
        ~finally:(fun () -> try Package.rm_rf tmp with _ -> ())
        (fun () ->
          (try ignore (Package.git [ "clone"; "--quiet"; "--depth"; "1"; "--"; loc; tmp ])
           with Package.Fetch m -> fail "cannot reach the package index at %s: %s" loc m);
          let fs = toml_files tmp in
          if fs = [] then fail "the package index at %s has no .toml files at its top" loc;
          read_files (List.map (fun f -> (Filename.concat tmp f, loc ^ " " ^ f)) fs))
    end
    else fail "LIMEN_INDEX is `%s`, which is neither a directory, a file nor a git URL; %s" loc help

(* ---------- versions ---------- *)

(* Tags compare as versions: a leading `v` is dropped, and the parts between
   dots compare as numbers when both are. *)
let compare_versions a b =
  let parts t =
    let t = if t <> "" && (t.[0] = 'v' || t.[0] = 'V') then String.sub t 1 (String.length t - 1) else t in
    String.split_on_char '.' t
  in
  let rec go = function
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
      let c = match (int_of_string_opt x, int_of_string_opt y) with Some i, Some j -> compare i j | _ -> compare x y in
      if c <> 0 then c else go (xs, ys)
  in
  go (parts a, parts b)

(* The versions of [name], newest first. *)
let versions entries name = List.sort (fun a b -> compare_versions b.tag a.tag) (List.filter (fun e -> e.name = name) entries)

let caps_text caps =
  let plain = List.filter (( <> ) "foreign") caps in
  match (plain, List.mem "foreign" caps) with
  | [], false -> "none (pure)"
  | [], true -> "none, but it calls C"
  | cs, false -> String.concat ", " cs
  | cs, true -> String.concat ", " cs ^ ", and it calls C"

(* ---------- limen search ---------- *)

let search words =
  let entries = load () in
  let words = List.map String.lowercase_ascii words in
  let names = List.sort_uniq compare (List.map (fun e -> e.name) entries) in
  let found =
    List.filter_map
      (fun n ->
        let all = versions entries n in
        let latest = List.hd all in
        let text = String.lowercase_ascii (n ^ " " ^ latest.description) in
        if List.for_all (fun w -> Interp_util.find_sub text w 0 <> None) words then Some (latest, List.tl all) else None)
      names
  in
  if found = [] then print_endline (if words = [] then "the index is empty" else "no package matches " ^ String.concat " " words)
  else
    List.iter
      (fun (e, older) ->
        printf "%s %s%s\n" e.name e.tag (if e.description = "" then "" else ": " ^ e.description);
        printf "  caps: %s\n" (caps_text e.caps);
        if older <> [] then printf "  older: %s\n" (String.concat ", " (List.map (fun e -> e.tag) older)))
      found

(* ---------- what a package uses ---------- *)

(* Check package [name] in [dir] as a dependency of an empty program that
   uses it, with [deps] the other dependencies it may use itself. Returns the
   program, its functions' capabilities and the modules that belong to the
   package. *)
let examine ?(deps = []) name dir =
  let main = Filename.concat (Filename.concat (Filename.get_temp_dir_name ()) (sprintf "limen-package-%d" (Unix.getpid ()))) "main.lmn" in
  let src = sprintf "use %s\n\nfn main(root: Root) {\n}\n" name in
  let failed ds = fail "`%s` does not check:\n%s" name (String.concat "" (List.map Diag.render ds)) in
  match Modules.load ~deps:((name, dir) :: deps) ~root:(Filename.dirname main) main src with
  | [], (_ :: _ as ds) -> failed ds
  | items, _ ->
    let ds, infos = Check.check_program items in
    let ds = Diag.errors ds in
    let ds =
      if ds = [] then Package.enforce ~std:true (Lazy.force Package.std_manifest) !Modules.std_loaded infos else ds
    in
    if ds <> [] then failed ds;
    (items, infos, List.filter (fun (_, p) -> p = name) !Modules.module_pkg)

let in_package mine (i : Check.fn_info) =
  match String.index_opt i.name '.' with Some k -> List.mem_assoc (String.sub i.name 0 k) mine | None -> false

(* The capabilities the package's functions take, written as in limen.toml,
   plus "foreign" when it calls C. *)
let caps_used mine (infos : Check.fn_info list) =
  let infos = List.filter (in_package mine) infos in
  (match List.find_opt (fun (i : Check.fn_info) -> List.mem Types.Root i.caps) infos with
   | Some i -> fail "`%s` takes Root, which no dependency can be given" i.name
   | None -> ());
  let caps = List.concat_map (fun (i : Check.fn_info) -> i.caps) infos in
  let first = List.concat_map (fun c -> if c = Types.Conn then [] else match Package.grant_of c with g :: _ -> [ g ] | [] -> []) caps in
  (* Conn comes with either net or listen *)
  let first = if List.mem Types.Conn caps && not (List.mem "net" first || List.mem "listen" first) then "net" :: first else first in
  List.filter (fun g -> List.mem g first) (Package.cap_grants @ [ "foreign" ])

let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter (function '"' -> Buffer.add_string b "\\\"" | '\\' -> Buffer.add_string b "\\\\" | '\n' -> Buffer.add_string b "\\n" | c -> Buffer.add_char b c) s;
  Buffer.add_char b '"';
  Buffer.contents b

let list l = "[" ^ String.concat ", " (List.map quote l) ^ "]"

(* ---------- limen index entry ---------- *)

let entry dir =
  let dir = try Unix.realpath dir with Unix.Unix_error _ -> fail "no such directory %s" dir in
  let git args = Package.git ([ "-C"; dir ] @ args) in
  let top =
    try Unix.realpath (git [ "rev-parse"; "--show-toplevel" ])
    with Package.Fetch _ | Unix.Unix_error _ -> fail "%s is not in a git repository; an index entry is a tag of a git repository" dir
  in
  if top <> dir then fail "run this at the top of the repository (%s): a git dependency is the whole repository" top;
  let toml = Filename.concat dir "limen.toml" in
  let package_key k =
    if not (Sys.file_exists toml) then None
    else
      let src = In_channel.with_open_bin toml In_channel.input_all in
      match Package.read toml src with
      | exception Package.Bad (sp, m) -> fail "%s: %s" (where sp) m
      | tables ->
        List.find_map
          (fun (t : Package.table) ->
            if t.tname <> "package" then None else List.find_map (fun (k', v, _) -> match v with Package.VStr s when k' = k -> Some s | _ -> None) t.keys)
          tables
  in
  let name = match package_key "name" with Some n -> n | None -> Filename.basename dir in
  if not (name_ok name) then fail "`%s` cannot be a package name: use lowercase letters, digits and `_` ([package] name in limen.toml)" name;
  let file = Filename.concat dir (name ^ ".lmn") in
  if not (Sys.file_exists file) then fail "there is no %s.lmn in %s: a package is a directory holding NAME.lmn" name dir;
  if git [ "status"; "--porcelain"; "--untracked-files=no" ] <> "" then
    fail "%s has uncommitted changes; commit them and tag the commit, so the entry describes what is in the tag" dir;
  let tag =
    try git [ "describe"; "--tags"; "--exact-match"; "HEAD" ]
    with Package.Fetch _ -> fail "the current commit has no tag; tag this version first, for example `git tag v1.0.0`"
  in
  let commit = git [ "rev-parse"; "HEAD" ] in
  let url = try git [ "remote"; "get-url"; "origin" ] with Package.Fetch _ -> "" in
  let url = if url <> "" && url.[0] = '/' then "file://" ^ url else url in
  let url = if Package.git_url_ok url then url else "" in
  (* its own dependencies, as its limen.toml lists them *)
  let deps =
    match Package.load file with
    | Ok (Some p) -> List.map (fun (d : Package.dep) -> (d.dname, d.dir)) p.deps
    | Ok None -> []
    | Error d -> fail "%s" (Diag.render d)
  in
  let _, infos, mine = examine ~deps name dir in
  let caps = caps_used mine infos in
  print_endline "[[package]]";
  printf "name = %s\n" (quote name);
  if url = "" then print_endline "# no git remote named origin: put the repository's public URL here";
  printf "git = %s\n" (quote url);
  printf "tag = %s\n" (quote tag);
  printf "commit = %s\n" (quote commit);
  printf "description = %s\n" (quote (Option.value ~default:"" (package_key "description")));
  printf "caps = %s\n" (list caps);
  prerr_endline "limen: to publish this version, add the entry above to the package index, for example in a pull request to its repository"

(* ---------- limen add ---------- *)

(* [line] added at the end of the [dependencies] table of limen.toml's text. *)
let insert_dependency old line =
  if String.trim old = "" then "[dependencies]\n" ^ line ^ "\n"
  else
    let lines = Array.of_list (String.split_on_char '\n' old) in
    let n = Array.length lines in
    let is_header i = let t = String.trim lines.(i) in t <> "" && t.[0] = '[' in
    let rec find i = if i >= n then None else if String.starts_with ~prefix:"[dependencies]" (String.trim lines.(i)) then Some i else find (i + 1) in
    match find 0 with
    | None -> old ^ (if String.ends_with ~suffix:"\n" old then "" else "\n") ^ "\n[dependencies]\n" ^ line ^ "\n"
    | Some h ->
      let rec next i = if i >= n || is_header i then i else next (i + 1) in
      let j = ref (next (h + 1)) in
      while !j - 1 > h && String.trim lines.(!j - 1) = "" do decr j done;
      let before = Array.to_list (Array.sub lines 0 !j) and after = Array.to_list (Array.sub lines !j (n - !j)) in
      String.concat "\n" (before @ [ line ] @ after)

let add spec =
  let name, version =
    match String.index_opt spec '@' with
    | Some i -> (String.sub spec 0 i, Some (String.sub spec (i + 1) (String.length spec - i - 1)))
    | None -> (spec, None)
  in
  let entries = load () in
  let all = versions entries name in
  if all = [] then fail "the index has no package named `%s`; try `limen search %s`" name name;
  let e =
    match version with
    | None -> List.hd all
    | Some v -> (
      match List.find_opt (fun e -> e.tag = v || e.tag = "v" ^ v) all with
      | Some e -> e
      | None -> fail "the index has no version %s of `%s`; it has %s" v name (String.concat ", " (List.map (fun e -> e.tag) all)))
  in
  (* the program's limen.toml: the nearest one, or a new one here *)
  let here = Filename.concat (Sys.getcwd ()) "main.lmn" in
  let toml = match Package.find here with Some p -> p | None -> Filename.concat (Sys.getcwd ()) "limen.toml" in
  let old = if Sys.file_exists toml then In_channel.with_open_bin toml In_channel.input_all else "" in
  let pkg =
    if old = "" then None
    else match Package.load here with Ok p -> p | Error d -> fail "%s" (Diag.render d)
  in
  let existing = match pkg with Some p -> p.deps | None -> [] in
  if List.exists (fun (d : Package.dep) -> d.dname = name) existing then
    fail "`%s` is already a dependency in %s; to change its version, edit its tag there" name toml;
  (* the tag must still point where it did when the package was indexed *)
  let commit = try Package.fetch name e.git (Package.Tag e.tag) with Package.Fetch m -> fail "cannot fetch `%s` from %s: %s" name e.git m in
  if not (String.starts_with ~prefix:e.commit commit) then
    fail
      "tag %s of %s now points at commit %s, but the index recorded %s: the tag was moved after the package was indexed. Not adding it; ask its author or the index which commit is right"
      e.tag e.git commit e.commit;
  let dir = Package.commit_dir e.git commit in
  let items, infos, mine = examine ~deps:(List.map (fun (d : Package.dep) -> (d.dname, d.dir)) existing) name dir in
  (* the package at that tag may use no more than the index lists: the same
     check as for any dependency, with exactly those caps *)
  let caps = List.filter (( <> ) "foreign") e.caps and calls_c = List.mem "foreign" e.caps in
  let dep = { Package.dname = name; dir; caps; dspan = Span.dummy; source = Package.Git (e.git, Package.Tag e.tag) } in
  let as_listed = { Package.pname = ""; file = toml; deps = [ dep ]; grants = []; trust = (if calls_c then [ name ] else []); embed_roots = [] } in
  (match Package.enforce as_listed mine infos with
   | [] -> ()
   | d :: _ ->
     let extra = List.filter (fun c -> not (List.mem c e.caps)) (try caps_used mine infos with Failed _ -> []) in
     fail "`%s` %s %s, which the index does not list (it lists %s). The index entry does not match the code at that tag, so it was not added"
       name e.tag
       (if extra = [] then "fails the capability check: " ^ d.message else "uses " ^ String.concat ", " extra)
       (if e.caps = [] then "none" else String.concat ", " e.caps));
  let line = sprintf "%s = { git = %s, tag = %s, caps = %s }" name (quote e.git) (quote e.tag) (list caps) in
  let text = insert_dependency old line in
  (match Package.parse toml text with
   | exception Package.Bad (sp, m) -> fail "could not add `%s` to %s: %s: %s" name toml (where sp) m
   | _ -> ());
  (* pin the commit that was checked, so the first build does not fetch the tag again *)
  let lock = Filename.concat (Filename.dirname toml) "limen.lock" in
  let kept =
    if Sys.file_exists lock then
      List.filter
        (fun l -> l <> "" && match String.split_on_char ' ' (String.trim l) with w :: _ -> w <> name | [] -> true)
        (String.split_on_char '\n' (In_channel.with_open_bin lock In_channel.input_all))
    else [ String.trim Package.lock_header ]
  in
  let lock_line = sprintf "%s %s %s %s %s" name e.git (Package.rev_text (Package.Tag e.tag)) commit (Package.content_hash dir) in
  (try
     Out_channel.with_open_bin lock (fun oc -> List.iter (fun l -> output_string oc (l ^ "\n")) (kept @ [ lock_line ]));
     Out_channel.with_open_bin toml (fun oc -> output_string oc text)
   with Sys_error m -> fail "cannot write %s" m);
  printf "added %s %s (%s) to %s\n" name e.tag e.git toml;
  if caps = [] then printf "%s may use no capabilities: it is pure\n" name
  else begin
    printf "%s may use: %s\n" name (String.concat ", " caps);
    print_endline "these are what you grant it; it gets each one only when your code passes it in"
  end;
  if calls_c then begin
    let libs = Option.value ~default:[] (List.assoc_opt name (Modules.c_users items)) in
    printf "%s calls C%s. Calling C is full trust, so it was not granted:\n" name
      (if libs = [] then "" else " (" ^ String.concat ", " libs ^ ")");
    printf "if you trust %s with everything your program can do, add this to limen.toml yourself:\n\n[trust]\nforeign = [%s]\n" name (quote name)
  end
