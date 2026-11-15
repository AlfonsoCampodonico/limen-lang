(* limen.toml: a package's name, the program's own grants, and its
   dependencies, each with the capabilities it may use.

     [package]
     name = "app"

     [grants]
     fs = "data:ro"
     net = ["api.github.com"]

     [dependencies]
     json = { path = "vendor/json" }
     fetch = { path = "../fetch", caps = ["net"] }
     zlib = { path = "../zlib" }

     [trust]
     foreign = ["zlib"]

     [embed]
     roots = ["../assets"]

   A dependency is a directory holding NAME.lmn (and any modules it uses from
   the same directory). Because Limen has no ambient authority, a function can
   only use a capability that reaches it as a parameter, so the capabilities a
   dependency can ever use are exactly those in its functions' signatures. The
   checker compares them with `caps` and rejects the program if a dependency
   asks for more than it was granted (E0320). Only a subset of TOML is read:
   tables, strings, string arrays and inline tables, plus arrays of tables
   ([[package]]) for the package index (registry.ml). *)

(* A dependency comes from a local directory or from a git repository at a
   tag or a commit. A git dependency's [dir] is "" until [load] fetches it
   into the cache and fills it in. *)
type rev = Tag of string | Commit of string
type source = Path | Git of string * rev
type dep = { dname : string; dir : string; caps : string list; dspan : Span.t; source : source }
(* [grants]: the program's own grants, as (kind, value) pairs written exactly
   like the text after `--grant KIND=`. An fs directory is made absolute
   relative to limen.toml. Only the limen.toml of the program being run is read
   for grants; a dependency's never is. *)
(* [trust]: `foreign = [...]` names the dependencies that may call C. C code
   can do anything the process can, so this is full trust, given only here,
   by the program; `caps` cannot grant it. In src/std/limen.toml it names the
   standard library modules whose foreign calls need no Foreign capability. *)
(* [embed]: `roots = [...]`, directories (relative to limen.toml) whose
   files the program may `embed` besides its own package's: each as written,
   the path it names, and where. Only the program's limen.toml can declare
   them; a dependency that tries is E0321 (Modules.load). *)
type t = {
  pname : string;
  file : string;
  deps : dep list;
  grants : (string * string) list;
  trust : string list;
  embed_roots : (string * string * Span.t) list;
}

let grant_kinds = [ "net"; "fs"; "listen"; "env"; "exec"; "foreign" ]

let cap_grants = [ "net"; "fs"; "listen"; "stdout"; "stderr"; "clock"; "rand"; "stdin"; "env"; "exec" ]

(* The grant that covers a capability type. *)
let grant_of (c : Types.cap) =
  match c with
  | Types.Net -> [ "net" ]
  | Conn -> [ "net"; "listen" ]
  | Listen | Listener -> [ "listen" ]
  | Fs -> [ "fs" ]
  | Stdout -> [ "stdout" ]
  | Stderr -> [ "stderr" ]
  | Clock -> [ "clock" ]
  | Rand -> [ "rand" ]
  | Stdin -> [ "stdin" ]
  | Env -> [ "env" ]
  | Exec -> [ "exec" ]
  | Foreign -> [ "foreign" ]
  | Root -> []

exception Bad of Span.t * string

(* Git URLs we hand to `git clone`. No `-` at the start (it would be read as
   an option) and no `transport::address` forms, which can run commands. *)
let git_url_ok u =
  u <> ""
  && u.[0] <> '-'
  && (not (String.exists (fun c -> Char.code c <= 0x20 || Char.code c >= 0x7f) u))
  && Interp_util.find_sub u "::" 0 = None
  && (List.exists (fun p -> String.starts_with ~prefix:p u) [ "https://"; "http://"; "ssh://"; "git://"; "file://" ]
     || (* scp-like user@host:path *)
     match (String.index_opt u '@', String.index_opt u ':') with
     | Some a, Some c -> a > 0 && c > a + 1 && not (String.contains (String.sub u 0 c) '/')
     | _ -> false)

(* A tag we hand to git, and a commit hash as limen.toml writes one. *)
let tag_ok t = t <> "" && t.[0] <> '-' && not (String.exists (fun c -> Char.code c <= 0x20 || c = '\x7f') t)

let commit_ok c =
  String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) c && String.length c >= 7 && String.length c <= 64

(* ---------- a small TOML reader ---------- *)

type v = VStr of string | VArr of string list | VTable of (string * v) list

(* A table of the file: its name ("" for keys before the first header),
   whether it was written [[name]] (one of an array of tables), the span of
   its header, and its keys in order. *)
type table = { tname : string; tarray : bool; tspan : Span.t; keys : (string * v * Span.t) list }

let read file src : table list =
  Source.add file src;
  let n = String.length src in
  let pos = ref 0 in
  let span lo hi = { Span.file; lo; hi } in
  let fail lo msg = raise (Bad (span lo (min n (lo + 1)), msg)) in
  let peek () = if !pos < n then src.[!pos] else '\000' in
  let rec skip_sp () = match peek () with ' ' | '\t' | '\r' -> incr pos; skip_sp () | _ -> () in
  let rec skip_ws () =
    match peek () with
    | ' ' | '\t' | '\r' | '\n' -> incr pos; skip_ws ()
    | '#' -> while !pos < n && src.[!pos] <> '\n' do incr pos done; skip_ws ()
    | _ -> ()
  in
  let end_line () =
    skip_sp ();
    if peek () = '#' then while !pos < n && src.[!pos] <> '\n' do incr pos done;
    if !pos < n && src.[!pos] <> '\n' then fail !pos "expected the end of the line"
  in
  let key () =
    let lo = !pos in
    while !pos < n && (match src.[!pos] with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true | _ -> false) do incr pos done;
    if !pos = lo then fail lo "expected a key";
    (String.sub src lo (!pos - lo), lo)
  in
  let string () =
    if peek () <> '"' then fail !pos "expected a string in double quotes";
    incr pos;
    let b = Buffer.create 16 in
    while peek () <> '"' do
      if !pos >= n || peek () = '\n' then fail !pos "unterminated string";
      if peek () = '\\' then begin
        incr pos;
        (match peek () with '"' -> Buffer.add_char b '"' | '\\' -> Buffer.add_char b '\\' | 'n' -> Buffer.add_char b '\n' | _ -> fail !pos "unknown escape");
        incr pos
      end
      else (Buffer.add_char b (peek ()); incr pos)
    done;
    incr pos;
    Buffer.contents b
  in
  let rec value () =
    skip_sp ();
    match peek () with
    | '"' -> VStr (string ())
    | '[' ->
      incr pos;
      let items = ref [] in
      skip_ws ();
      while peek () <> ']' do
        items := string () :: !items;
        skip_ws ();
        if peek () = ',' then (incr pos; skip_ws ()) else if peek () <> ']' then fail !pos "expected `,` or `]`"
      done;
      incr pos;
      VArr (List.rev !items)
    | '{' ->
      incr pos;
      let fields = ref [] in
      skip_sp ();
      while peek () <> '}' do
        let k, _ = key () in
        skip_sp ();
        if peek () <> '=' then fail !pos "expected `=`";
        incr pos;
        fields := (k, value ()) :: !fields;
        skip_sp ();
        if peek () = ',' then (incr pos; skip_sp ()) else if peek () <> '}' then fail !pos "expected `,` or `}`"
      done;
      incr pos;
      VTable (List.rev !fields)
    | _ -> fail !pos "expected a string, an array or an inline table"
  in
  let tables = ref [] and cur = ref { tname = ""; tarray = false; tspan = span 0 0; keys = [] } in
  let close () = if !cur.tname <> "" || !cur.keys <> [] then tables := { !cur with keys = List.rev !cur.keys } :: !tables in
  skip_ws ();
  while !pos < n do
    if peek () = '[' then begin
      let lo = !pos in
      incr pos;
      let arr = peek () = '[' in
      if arr then incr pos;
      let k, _ = key () in
      if peek () <> ']' then fail !pos "expected `]`";
      incr pos;
      if arr then (if peek () <> ']' then fail !pos "expected `]]`"; incr pos);
      close ();
      cur := { tname = k; tarray = arr; tspan = span lo !pos; keys = [] };
      end_line ()
    end
    else begin
      let k, klo = key () in
      skip_sp ();
      if peek () <> '=' then fail !pos "expected `=`";
      incr pos;
      let v = value () in
      let sp = span klo !pos in
      end_line ();
      cur := { !cur with keys = (k, v, sp) :: !cur.keys }
    end;
    skip_ws ()
  done;
  close ();
  List.rev !tables

let parse file src : t =
  let tables = read file src in
  let name = ref "" and deps = ref [] and grants = ref [] and trust = ref [] and roots = ref None in
  let span lo hi = { Span.file; lo; hi } in
  let dir = Filename.dirname file in
  List.iter
    (fun tb ->
      let k = tb.tname in
      if tb.tarray || (k <> "" && k <> "package" && k <> "dependencies" && k <> "grants" && k <> "trust" && k <> "embed") then
        raise (Bad (tb.tspan, "unknown table " ^ (if tb.tarray then "[[" ^ k ^ "]]" else "[" ^ k ^ "]") ^ "; limen.toml has [package], [grants], [dependencies], [trust] and [embed]"));
      List.iter
        (fun (k, v, sp) ->
          match (tb.tname, k, v) with
          | "package", "name", VStr s -> name := s
          | "package", ("version" | "description"), VStr _ -> ()
          | "package", _, _ -> raise (Bad (sp, "unknown or malformed key in [package]"))
          | "dependencies", d, VTable fields ->
            let str f = match List.assoc_opt f fields with Some (VStr v) -> Some v | None -> None | Some _ -> raise (Bad (sp, "`" ^ f ^ "` is a string")) in
            let source, path =
              match (str "path", str "git", str "tag", str "commit") with
              | Some p, None, None, None -> (Path, p)
              | None, Some url, tag, commit ->
                if not (git_url_ok url) then
                  raise (Bad (sp, "`git` must be an https://, http://, ssh://, git:// or file:// URL, or user@host:path"));
                let rev =
                  match (tag, commit) with
                  | Some t, None ->
                    if not (tag_ok t) then raise (Bad (sp, "bad tag"));
                    Tag t
                  | None, Some c ->
                    if not (commit_ok c) then
                      raise (Bad (sp, "`commit` is a commit hash in lowercase hex, at least 7 characters"));
                    Commit c
                  | _ -> raise (Bad (sp, "a git dependency needs exactly one of `tag` and `commit`"))
                in
                (Git (url, rev), "")
              | Some _, Some _, _, _ -> raise (Bad (sp, "a dependency has `path` or `git`, not both"))
              | None, None, _, _ -> raise (Bad (sp, "a dependency needs `path = \"DIR\"` or `git = \"URL\"` with a `tag` or `commit`"))
              | Some _, None, _, _ -> raise (Bad (sp, "`tag` and `commit` go with `git`, not `path`"))
            in
            let caps = match List.assoc_opt "caps" fields with Some (VArr cs) -> cs | None -> [] | Some _ -> raise (Bad (sp, "`caps` is a list of strings")) in
            List.iter
              (fun c ->
                if c = "foreign" then
                  raise (Bad (sp, Printf.sprintf "`caps` cannot grant foreign: calling C is full trust; name the dependency in [trust] instead: foreign = [\"%s\"]" d))
                else if not (List.mem c cap_grants) then
                  raise (Bad (sp, Printf.sprintf "unknown capability `%s`; use %s" c (String.concat ", " cap_grants))))
              caps;
            List.iter
              (fun (f, _) -> if not (List.mem f [ "path"; "git"; "tag"; "commit"; "caps" ]) then raise (Bad (sp, "unknown dependency field `" ^ f ^ "`")))
              fields;
            let path = if source = Path && Filename.is_relative path then Filename.concat dir path else path in
            if List.exists (fun d' -> d'.dname = d) !deps then raise (Bad (sp, "dependency `" ^ d ^ "` is listed twice"));
            deps := { dname = d; dir = path; caps; dspan = sp; source } :: !deps
          | "dependencies", _, _ ->
            raise (Bad (sp, "write a dependency as `name = { path = \"DIR\", caps = [...] }` or `name = { git = \"URL\", tag = \"v1.0.0\" }`"))
          | "grants", g, v ->
            if not (List.mem g grant_kinds) then
              raise
                (Bad
                   ( sp,
                     if List.mem g cap_grants then Printf.sprintf "`%s` is always granted; [grants] lists %s" g (String.concat ", " grant_kinds)
                     else Printf.sprintf "unknown grant `%s`; [grants] lists %s" g (String.concat ", " grant_kinds) ));
            if List.mem_assoc g !grants then raise (Bad (sp, Printf.sprintf "grant `%s` is listed twice; give a list instead" g));
            let vs = match v with VStr s -> [ s ] | VArr l -> l | VTable _ -> raise (Bad (sp, "a grant is a string or a list of strings")) in
            if vs = [] then raise (Bad (sp, "an empty grant list grants nothing; remove the line"));
            if g = "fs" && List.length vs > 1 then raise (Bad (sp, "fs takes one directory"));
            let vs =
              if g <> "fs" then vs
              else
                List.map
                  (fun v ->
                    let d, ro = if String.ends_with ~suffix:":ro" v then (String.sub v 0 (String.length v - 3), ":ro") else (v, "") in
                    (if Filename.is_relative d then Filename.concat dir d else d) ^ ro)
                  vs
            in
            List.iter (fun v -> if v = "" then raise (Bad (sp, "a grant cannot be empty"))) vs;
            grants := !grants @ List.map (fun v -> (g, v)) vs
          | "trust", "foreign", VArr names ->
            if !trust <> [] then raise (Bad (sp, "[trust] foreign is listed twice"));
            if names = [] then raise (Bad (sp, "an empty trust list trusts nothing; remove the line"));
            trust := names
          | "trust", _, _ -> raise (Bad (sp, "[trust] takes `foreign = [\"dependency\", ...]`: the dependencies that may call C"))
          | "embed", "roots", VArr rs ->
            if !roots <> None then raise (Bad (sp, "[embed] roots is listed twice"));
            if rs = [] then raise (Bad (sp, "an empty list of embed roots adds nothing; remove the line"));
            List.iteri
              (fun i r ->
                if r = "" then raise (Bad (sp, "an embed root cannot be empty"));
                if List.mem r (List.filteri (fun j _ -> j < i) rs) then raise (Bad (sp, Printf.sprintf "embed root `%s` is listed twice" r)))
              rs;
            roots := Some (List.map (fun r -> (r, (if Filename.is_relative r then Filename.concat dir r else r), sp)) rs)
          | "embed", _, _ ->
            raise (Bad (sp, "[embed] takes `roots = [\"DIR\", ...]`: directories, relative to limen.toml, whose files the program may embed"))
          | _ -> raise (Bad (sp, "keys belong in [package], [grants], [dependencies], [trust] or [embed]")))
        tb.keys)
    tables;
  List.iter
    (fun t ->
      if not (List.exists (fun d -> d.dname = t) !deps) && file <> "<std>/limen.toml" then
        raise (Bad (span 0 0, Printf.sprintf "[trust] names `%s`, which is not in [dependencies]" t)))
    !trust;
  { pname = !name; file; deps = List.rev !deps; grants = !grants; trust = !trust; embed_roots = Option.value ~default:[] !roots }

(* The embed roots must be directories, through any links (E0321 at the
   root's line otherwise). *)
let check_roots (pkg : t) =
  List.iter
    (fun (r, full, sp) ->
      if not (Sys.file_exists full && Sys.is_directory full) then
        raise (Bad (sp, Printf.sprintf "embed root `%s` is not a directory (looked for %s)" r full)))
    pkg.embed_roots

(* Whether the limen.toml text [src] (at [file]) declares embed roots:
   the span of its [embed] table. A dependency's limen.toml is otherwise
   never read, and one that cannot be read declares nothing. *)
let declares_embed file src =
  match read file src with
  | tables -> Option.map (fun tb -> tb.tspan) (List.find_opt (fun tb -> tb.tname = "embed" && not tb.tarray) tables)
  | exception Bad _ -> None

(* limen.toml in the directory of [main] or the nearest parent. *)
let find main =
  let rec up d =
    let p = Filename.concat d "limen.toml" in
    if Sys.file_exists p then Some p
    else
      let parent = Filename.dirname d in
      if parent = d then None else up parent
  in
  let d = Filename.dirname main in
  let beside = Filename.concat d "limen.toml" in
  if Sys.file_exists beside then Some beside
  else up (if Filename.is_relative d then Filename.concat (Sys.getcwd ()) d else d)

(* ---------- git dependencies, the cache and limen.lock ---------- *)

(* A git dependency is cloned with the `git` command into the cache, one
   directory per commit, without its .git directory:

     $LIMEN_CACHE (or $XDG_CACHE_HOME/limen, or ~/.cache/limen)
       git/<url-key>/<commit>/

   limen.lock, next to limen.toml, records the exact commit each one resolved to
   and a hash of its files, one line per dependency:

     json2 https://example.com/json2.git tag:v1.2.0 <commit> blake2b:<hex>

   The first build fetches the dependency and writes its line. Later builds
   use the locked commit, even if the tag has since moved, and check the
   cached files against the hash every time. To update, change the tag (or
   delete the line). *)

(* When set, a dependency that is not in the cache yet is an error instead of
   a fetch: the editor and agent servers never run git. *)
let offline = ref false

(* Whether loading a program may write limen.lock. Only the commands that
   produce something set it (build, run, test): `limen check`, `caps`, the
   editor and agent servers read the lock and never change it. *)
let write_lock = ref false

exception Fetch of string

let cache_dir () =
  match Sys.getenv_opt "LIMEN_CACHE" with
  | Some d when d <> "" -> d
  | _ -> (
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some d when d <> "" -> Filename.concat d "limen"
    | _ -> Filename.concat (Filename.concat (Option.value (Sys.getenv_opt "HOME") ~default:".") ".cache") "limen")

let rec mkdir_p d =
  if not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let rec rm_rf p =
  match (Unix.lstat p).st_kind with
  | Unix.S_DIR ->
    Array.iter (fun f -> rm_rf (Filename.concat p f)) (Sys.readdir p);
    Unix.rmdir p
  | _ -> Unix.unlink p
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

(* Run git, returning its stdout. Hooks and command-running transports are
   off, and it never stops to ask for a password. *)
let git args =
  let argv = Array.of_list ([ "git"; "-c"; "protocol.ext.allow=never"; "-c"; "core.hooksPath=/dev/null" ] @ args) in
  let env = Array.append [| "GIT_TERMINAL_PROMPT=0" |] (Unix.environment ()) in
  match Unix.open_process_args_full "git" argv env with
  | exception Unix.Unix_error (e, _, _) -> raise (Fetch ("cannot run git: " ^ Unix.error_message e))
  | out, inp, err ->
    close_out inp;
    let o = In_channel.input_all out and e = In_channel.input_all err in
    (match Unix.close_process_full (out, inp, err) with
     | Unix.WEXITED 0 -> ()
     | Unix.WEXITED 127 -> raise (Fetch "cannot run git; is it installed?")
     | _ -> raise (Fetch (String.trim e)));
    String.trim o

(* A hash of every file under [dir] (paths and contents, in sorted order). *)
let content_hash dir =
  let b = Buffer.create 4096 in
  let rec walk rel =
    let full = if rel = "" then dir else Filename.concat dir rel in
    let names = List.sort compare (Array.to_list (Sys.readdir full)) in
    List.iter
      (fun n ->
        let r = if rel = "" then n else rel ^ "/" ^ n in
        let p = Filename.concat dir r in
        match (Unix.lstat p).st_kind with
        | Unix.S_DIR -> if n <> ".git" then walk r
        | Unix.S_LNK -> Printf.bprintf b "L %s\000%s\000" r (Unix.readlink p)
        | _ ->
          let c = In_channel.with_open_bin p In_channel.input_all in
          Printf.bprintf b "F %s\000%d\000%s" r (String.length c) c)
      names
  in
  walk "";
  "blake2b:" ^ Digest.BLAKE256.to_hex (Digest.BLAKE256.string (Buffer.contents b))

let url_key url =
  let safe = String.map (fun c -> match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' -> c | _ -> '_') url in
  let safe = if String.length safe > 60 then String.sub safe (String.length safe - 60) 60 else safe in
  safe ^ "-" ^ String.sub (Digest.BLAKE256.to_hex (Digest.BLAKE256.string url)) 0 12

let commit_dir url commit = Filename.concat (Filename.concat (Filename.concat (cache_dir ()) "git") (url_key url)) commit

(* Clone [url], check out [rev], and move the files into the cache. Returns
   the full commit. *)
let fetch name url rev =
  if !offline then raise (Fetch (Printf.sprintf "`%s` is not in the cache yet; run `limen check` on the program to fetch it" name));
  let base = Filename.concat (cache_dir ()) "git" in
  mkdir_p base;
  let tmp = Filename.concat base (Printf.sprintf "tmp-%d-%d" (Unix.getpid ()) (Random.bits ())) in
  Fun.protect ~finally:(fun () -> try rm_rf tmp with _ -> ()) (fun () ->
      ignore (git [ "clone"; "--quiet"; "--no-checkout"; "--"; url; tmp ]);
      let want = match rev with Tag t -> "refs/tags/" ^ t | Commit c -> c in
      let commit =
        try git [ "-C"; tmp; "rev-parse"; "--verify"; "--quiet"; want ^ "^{commit}" ]
        with Fetch _ -> (
          match rev with
          | Tag t -> raise (Fetch (Printf.sprintf "%s has no tag `%s`" url t))
          | Commit c -> raise (Fetch (Printf.sprintf "%s has no commit `%s`" url c)))
      in
      ignore (git [ "-C"; tmp; "checkout"; "--quiet"; "--detach"; commit ]);
      rm_rf (Filename.concat tmp ".git");
      let final = commit_dir url commit in
      mkdir_p (Filename.dirname final);
      (if not (Sys.file_exists final) then try Sys.rename tmp final with Sys_error _ when Sys.file_exists final -> ());
      commit)

type lock_line = { lname : string; lurl : string; lrev : string; lcommit : string; lhash : string }

let rev_text = function Tag t -> "tag:" ^ t | Commit c -> "commit:" ^ c
let lock_header = "# limen.lock: the exact version of each git dependency, and the C libraries of each trusted one, written by limen. Commit it.\n"

let read_lock path =
  if not (Sys.file_exists path) then []
  else
    let src = In_channel.with_open_bin path In_channel.input_all in
    Source.add path src;
    let lines = String.split_on_char '\n' src in
    let lo = ref 0 in
    List.filter_map
      (fun l ->
        let start = !lo in
        lo := !lo + String.length l + 1;
        let t = String.trim l in
        if t = "" || t.[0] = '#' || String.starts_with ~prefix:"foreign " t then None
        else
          match List.filter (( <> ) "") (String.split_on_char ' ' t) with
          | [ lname; lurl; lrev; lcommit; lhash ] -> Some { lname; lurl; lrev; lcommit; lhash }
          | _ ->
            raise
              (Bad
                 ( { Span.file = path; lo = start; hi = start + String.length l },
                   "malformed line in limen.lock; expected NAME URL REV COMMIT HASH (delete the line to fetch again)" )))
      lines

(* Trusted dependencies that call C: `foreign NAME LIB,LIB` lines in
   limen.lock, the libraries each one declared when it was trusted. *)
let foreign_line name libs = Printf.sprintf "foreign %s %s\n" name (String.concat "," libs)

let read_foreign_lock path =
  if not (Sys.file_exists path) then []
  else
    List.filter_map
      (fun l ->
        match List.filter (( <> ) "") (String.split_on_char ' ' (String.trim l)) with
        | [ "foreign"; name; libs ] -> Some (name, String.split_on_char ',' libs)
        | _ -> None)
      (String.split_on_char '\n' (In_channel.with_open_bin path In_channel.input_all))

(* Fetch or find each git dependency, check it against limen.lock, and write
   limen.lock if it changed (with [write_lock]). Errors carry their code:
   E0322 (fetch), E0323 (lock). *)
exception Lock_error of string * Span.t * string * string list

let resolve (pkg : t) : t =
  let lock_path = Filename.concat (Filename.dirname pkg.file) "limen.lock" in
  let locked = try read_lock lock_path with Bad (sp, m) -> raise (Lock_error ("E0323", sp, m, [])) in
  let changed = ref false in
  let lines = ref [] in
  let deps =
    List.map
      (fun d ->
        match d.source with
        | Path -> d
        | Git (url, rev) ->
          let fetch_err m = Lock_error ("E0322", d.dspan, Printf.sprintf "cannot fetch dependency `%s`: %s" d.dname m, []) in
          let line =
            match List.find_opt (fun l -> l.lname = d.dname && l.lurl = url && l.lrev = rev_text rev) locked with
            | Some l ->
              let dir = commit_dir url l.lcommit in
              (if not (Sys.file_exists dir) then
                 try
                   let c = fetch d.dname url (Commit l.lcommit) in
                   if c <> l.lcommit then raise (Fetch (Printf.sprintf "commit %s from limen.lock resolved to %s" l.lcommit c))
                 with Fetch m -> raise (fetch_err m));
              let h = content_hash dir in
              if h <> l.lhash then
                raise
                  (Lock_error
                     ( "E0323",
                       d.dspan,
                       Printf.sprintf "dependency `%s` at %s does not match limen.lock: its files hash to %s, the lock says %s" d.dname
                         (String.sub l.lcommit 0 (min 12 (String.length l.lcommit))) h l.lhash,
                       [ "the cached copy at " ^ dir ^ " was changed, or the repository's history was rewritten";
                         "delete that directory to fetch it again; if it still does not match, do not trust the new contents" ] ));
              l
            | None ->
              let commit = try fetch d.dname url rev with Fetch m -> raise (fetch_err m) in
              (match rev with
               | Commit c when not (String.starts_with ~prefix:c commit) -> raise (fetch_err (Printf.sprintf "`%s` resolved to %s" c commit))
               | _ -> ());
              changed := true;
              { lname = d.dname; lurl = url; lrev = rev_text rev; lcommit = commit; lhash = content_hash (commit_dir url commit) }
          in
          lines := line :: !lines;
          { d with dir = commit_dir url line.lcommit })
      pkg.deps
  in
  let lines = List.rev !lines in
  if !write_lock && (!changed || List.length lines <> List.length locked) then begin
    let text = String.concat "" (List.map (fun l -> Printf.sprintf "%s %s %s %s %s\n" l.lname l.lurl l.lrev l.lcommit l.lhash) lines) in
    let keep = List.map (fun (n, ls) -> foreign_line n ls) (read_foreign_lock lock_path) in
    if not (lines = [] && keep = [] && not (Sys.file_exists lock_path)) then
      try Out_channel.with_open_bin lock_path (fun oc -> output_string oc (lock_header ^ text ^ String.concat "" keep))
      with Sys_error m -> raise (Lock_error ("E0323", Span.dummy, "cannot write limen.lock: " ^ m, []))
  end;
  { pkg with deps }

(* Load the package around [main]: Ok None when there is no limen.toml. *)
let load main : (t option, Diag.t) result =
  match find main with
  | None -> Ok None
  | Some p -> (
    match In_channel.with_open_bin p In_channel.input_all with
    | exception Sys_error m -> Error (Diag.make "E0321" Span.dummy m)
    | src -> (
      match parse p src with
      | exception Bad (sp, m) -> Error (Diag.make "E0321" sp m)
      | pkg -> (
        match check_roots pkg with
        | exception Bad (sp, m) -> Error (Diag.make "E0321" sp m)
        | () -> ( try Ok (Some (resolve pkg)) with Lock_error (code, sp, m, notes) -> Error (Diag.make ~notes code sp m)))))

(* The standard library's own manifest, src/std/limen.toml: each std module
   is a "dependency" with the capabilities it may take. *)
let std_file = "<std>/limen.toml"
let std_manifest = lazy (parse std_file Std_text.manifest)

(* E0320: every function of a dependency may only take capabilities its
   package was granted. [module_pkg] maps a module name to its dependency.
   With [~std], [pkg] is the standard library's manifest and a module it does
   not list may take nothing. *)
let enforce ?(std = false) (pkg : t) (module_pkg : (string * string) list) (infos : Check.fn_info list) : Diag.t list =
  List.filter_map
    (fun (i : Check.fn_info) ->
      match String.index_opt i.name '.' with
      | None -> None
      | Some k -> (
        let m = String.sub i.name 0 k in
        match List.assoc_opt m module_pkg with
        | None -> None
        | Some dn -> (
          let d =
            match List.find_opt (fun d -> d.dname = dn) pkg.deps with
            | Some d -> d
            | None -> { dname = dn; dir = ""; caps = []; dspan = Span.dummy; source = Path }
          in
          let allowed (c : Types.cap) =
            match c with
            | Types.Root -> false
            | Types.Foreign -> (not std) && List.mem dn pkg.trust
            | c -> List.exists (fun g -> List.mem g d.caps) (grant_of c)
          in
          let missing = List.filter (fun c -> c <> Types.Root && not (allowed c)) i.caps in
          let missing = if List.mem Types.Root i.caps then Types.Root :: missing else missing in
          match missing with
          | [] -> None
          | c :: _ ->
            let need = match grant_of c with [] -> "Root, which no dependency can be granted" | g :: _ -> Printf.sprintf "\"%s\"" g in
            (* held through a struct or enum: say which field *)
            let through c = match List.assoc_opt c i.via with Some p -> Printf.sprintf " through `%s`" p | None -> "" in
            let where (sp : Span.t) = let l, _ = Source.line_col sp.file sp.lo in Printf.sprintf "%s:%d" sp.file l in
            if std then
              Some
                (Diag.make "E0320" d.dspan
                   (Printf.sprintf "standard library module `%s` uses capability `%s`, which %s does not declare" dn (Types.cap_name c) std_file)
                   ~notes:
                     [ Printf.sprintf "`%s` takes `%s`%s (%s)" i.name (Types.cap_name c) (through c) (where i.span);
                       "this is a bug in the standard library: declare the capability in src/std/limen.toml, or stop taking it" ])
            else
            Some
              (Diag.make "E0320" d.dspan
                 (Printf.sprintf "dependency `%s` uses capability `%s`, which limen.toml does not grant it" dn (Types.cap_name c))
                 ~notes:
                   [ Printf.sprintf "`%s` takes `%s`%s (%s)" i.name (Types.cap_name c) (through c) (let l, _ = Source.line_col i.span.file i.span.lo in Printf.sprintf "%s:%d" i.span.file l);
                     (if c = Types.Root then "a dependency can never take Root: pass it the narrower capability it needs"
                      else if c = Types.Foreign then
                        Printf.sprintf "calling C is full trust: if you trust `%s` with everything the program can do, add [trust] foreign = [\"%s\"] to limen.toml" dn dn
                      else Printf.sprintf "to allow it, add %s to its caps" need) ]))))
    infos

(* E0324: each trusted dependency's C libraries must match what limen.lock
   recorded when it was trusted, so an update that starts calling another
   library (or C at all, for one trusted only in name) stops the build until
   someone looks. [uses] is (dependency, libraries it declares). The first
   build records them (with [write_lock]); a dependency that no longer calls
   C loses its line. *)
let check_foreign_lock (pkg : t) (uses : (string * string list) list) : Diag.t list =
  let lock_path = Filename.concat (Filename.dirname pkg.file) "limen.lock" in
  let recorded = read_foreign_lock lock_path in
  let uses = List.filter (fun (d, libs) -> libs <> [] && List.mem d pkg.trust) uses in
  let span_of d = match List.find_opt (fun x -> x.dname = d) pkg.deps with Some x -> x.dspan | None -> Span.dummy in
  let errs =
    List.filter_map
      (fun (d, libs) ->
        match List.assoc_opt d recorded with
        | Some old when List.sort compare old = List.sort compare libs -> None
        | Some old ->
          Some
            (Diag.make "E0324" (span_of d)
               (Printf.sprintf "trusted dependency `%s` now calls C library %s; limen.lock recorded %s" d
                  (String.concat ", " (List.map (fun l -> "`" ^ l ^ "`") (List.filter (fun l -> not (List.mem l old)) libs)))
                  (String.concat ", " old))
               ~notes:
                 [ "calling C is full trust: review what changed in the dependency";
                   Printf.sprintf "then delete the line `foreign %s ...` from limen.lock to record the new libraries" d ])
        | None -> None)
      uses
  in
  let fresh = List.map (fun (d, libs) -> (d, match List.assoc_opt d recorded with Some old -> old | None -> libs)) uses in
  if !write_lock && errs = [] && List.sort compare fresh <> List.sort compare recorded then begin
    let others =
      if Sys.file_exists lock_path then
        List.filter
          (fun l -> let t = String.trim l in t <> "" && not (String.starts_with ~prefix:"foreign " t) && t.[0] <> '#')
          (String.split_on_char '\n' (In_channel.with_open_bin lock_path In_channel.input_all))
      else []
    in
    if not (fresh = [] && others = [] && not (Sys.file_exists lock_path)) then
      try
        Out_channel.with_open_bin lock_path (fun oc ->
            output_string oc (lock_header ^ String.concat "" (List.map (fun l -> l ^ "\n") others) ^ String.concat "" (List.map (fun (d, ls) -> foreign_line d ls) fresh)))
      with Sys_error _ -> ()
  end;
  errs
