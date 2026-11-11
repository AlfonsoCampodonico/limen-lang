(* vela.toml: a package's name, the program's own grants, and its
   dependencies, each with the capabilities it may use.

     [package]
     name = "app"

     [grants]
     fs = "data:ro"
     net = ["api.github.com"]

     [dependencies]
     json = { path = "vendor/json" }
     fetch = { path = "../fetch", caps = ["net"] }

   A dependency is a directory holding NAME.vela (and any modules it uses from
   the same directory). Because Vela has no ambient authority, a function can
   only use a capability that reaches it as a parameter, so the capabilities a
   dependency can ever use are exactly those in its functions' signatures. The
   checker compares them with `caps` and rejects the program if a dependency
   asks for more than it was granted (E0320). Only a subset of TOML is read:
   tables, strings, string arrays and inline tables. *)

(* A dependency comes from a local directory or from a git repository at a
   tag or a commit. A git dependency's [dir] is "" until [load] fetches it
   into the cache and fills it in. *)
type rev = Tag of string | Commit of string
type source = Path | Git of string * rev
type dep = { dname : string; dir : string; caps : string list; dspan : Span.t; source : source }
(* [grants]: the program's own grants, as (kind, value) pairs written exactly
   like the text after `--grant KIND=`. An fs directory is made absolute
   relative to vela.toml. Only the vela.toml of the program being run is read
   for grants; a dependency's never is. *)
type t = { pname : string; file : string; deps : dep list; grants : (string * string) list }

let grant_kinds = [ "net"; "fs"; "listen"; "env" ]

let cap_grants = [ "net"; "fs"; "listen"; "stdout"; "clock"; "rand"; "stdin"; "env" ]

(* The grant that covers a capability type. *)
let grant_of (c : Types.cap) =
  match c with
  | Types.Net -> [ "net" ]
  | Conn -> [ "net"; "listen" ]
  | Listen | Listener -> [ "listen" ]
  | Fs -> [ "fs" ]
  | Stdout -> [ "stdout" ]
  | Clock -> [ "clock" ]
  | Rand -> [ "rand" ]
  | Stdin -> [ "stdin" ]
  | Env -> [ "env" ]
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

(* ---------- a small TOML reader ---------- *)

type v = VStr of string | VArr of string list | VTable of (string * v) list

let parse file src : t =
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
  let section = ref "" in
  let name = ref "" and deps = ref [] and grants = ref [] in
  let dir = Filename.dirname file in
  skip_ws ();
  while !pos < n do
    if peek () = '[' then begin
      incr pos;
      let k, _ = key () in
      if peek () <> ']' then fail !pos "expected `]`";
      incr pos;
      if k <> "package" && k <> "dependencies" && k <> "grants" then
        fail (!pos - 1) ("unknown table [" ^ k ^ "]; vela.toml has [package], [grants] and [dependencies]");
      section := k;
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
      match (!section, k, v) with
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
                if t = "" || t.[0] = '-' || String.exists (fun c -> Char.code c <= 0x20 || c = '\x7f') t then raise (Bad (sp, "bad tag"));
                Tag t
              | None, Some c ->
                let hex = String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) c in
                if not (hex && String.length c >= 7 && String.length c <= 64) then
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
          (fun c -> if not (List.mem c cap_grants) then raise (Bad (sp, Printf.sprintf "unknown capability `%s`; use %s" c (String.concat ", " cap_grants))))
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
      | _ -> raise (Bad (sp, "keys belong in [package], [grants] or [dependencies]"))
    end;
    skip_ws ()
  done;
  { pname = !name; file; deps = List.rev !deps; grants = !grants }

(* vela.toml in the directory of [main] or the nearest parent. *)
let find main =
  let rec up d =
    let p = Filename.concat d "vela.toml" in
    if Sys.file_exists p then Some p
    else
      let parent = Filename.dirname d in
      if parent = d then None else up parent
  in
  let d = Filename.dirname main in
  let beside = Filename.concat d "vela.toml" in
  if Sys.file_exists beside then Some beside
  else up (if Filename.is_relative d then Filename.concat (Sys.getcwd ()) d else d)

(* ---------- git dependencies, the cache and vela.lock ---------- *)

(* A git dependency is cloned with the `git` command into the cache, one
   directory per commit, without its .git directory:

     $VELA_CACHE (or $XDG_CACHE_HOME/vela, or ~/.cache/vela)
       git/<url-key>/<commit>/

   vela.lock, next to vela.toml, records the exact commit each one resolved to
   and a hash of its files, one line per dependency:

     json2 https://example.com/json2.git tag:v1.2.0 <commit> blake2b:<hex>

   The first build fetches the dependency and writes its line. Later builds
   use the locked commit, even if the tag has since moved, and check the
   cached files against the hash every time. To update, change the tag (or
   delete the line). *)

(* When set, a dependency that is not in the cache yet is an error instead of
   a fetch: the editor and agent servers never run git. *)
let offline = ref false

exception Fetch of string

let cache_dir () =
  match Sys.getenv_opt "VELA_CACHE" with
  | Some d when d <> "" -> d
  | _ -> (
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some d when d <> "" -> Filename.concat d "vela"
    | _ -> Filename.concat (Filename.concat (Option.value (Sys.getenv_opt "HOME") ~default:".") ".cache") "vela")

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
  if !offline then raise (Fetch (Printf.sprintf "`%s` is not in the cache yet; run `vela check` on the program to fetch it" name));
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
let lock_header = "# vela.lock: the exact version of each git dependency, written by vela. Commit it.\n"

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
        if t = "" || t.[0] = '#' then None
        else
          match List.filter (( <> ) "") (String.split_on_char ' ' t) with
          | [ lname; lurl; lrev; lcommit; lhash ] -> Some { lname; lurl; lrev; lcommit; lhash }
          | _ ->
            raise
              (Bad
                 ( { Span.file = path; lo = start; hi = start + String.length l },
                   "malformed line in vela.lock; expected NAME URL REV COMMIT HASH (delete the line to fetch again)" )))
      lines

(* Fetch or find each git dependency, check it against vela.lock, and write
   vela.lock if it changed. Errors carry their code: E0322 (fetch), E0323
   (lock). *)
exception Lock_error of string * Span.t * string * string list

let resolve (pkg : t) : t =
  let lock_path = Filename.concat (Filename.dirname pkg.file) "vela.lock" in
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
                   if c <> l.lcommit then raise (Fetch (Printf.sprintf "commit %s from vela.lock resolved to %s" l.lcommit c))
                 with Fetch m -> raise (fetch_err m));
              let h = content_hash dir in
              if h <> l.lhash then
                raise
                  (Lock_error
                     ( "E0323",
                       d.dspan,
                       Printf.sprintf "dependency `%s` at %s does not match vela.lock: its files hash to %s, the lock says %s" d.dname
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
  if !changed || List.length lines <> List.length locked then begin
    let text = String.concat "" (List.map (fun l -> Printf.sprintf "%s %s %s %s %s\n" l.lname l.lurl l.lrev l.lcommit l.lhash) lines) in
    if not (lines = [] && not (Sys.file_exists lock_path)) then
      try Out_channel.with_open_bin lock_path (fun oc -> output_string oc (lock_header ^ text))
      with Sys_error m -> raise (Lock_error ("E0323", Span.dummy, "cannot write vela.lock: " ^ m, []))
  end;
  { pkg with deps }

(* Load the package around [main]: Ok None when there is no vela.toml. *)
let load main : (t option, Diag.t) result =
  match find main with
  | None -> Ok None
  | Some p -> (
    match In_channel.with_open_bin p In_channel.input_all with
    | exception Sys_error m -> Error (Diag.make "E0321" Span.dummy m)
    | src -> (
      match parse p src with
      | exception Bad (sp, m) -> Error (Diag.make "E0321" sp m)
      | pkg -> ( try Ok (Some (resolve pkg)) with Lock_error (code, sp, m, notes) -> Error (Diag.make ~notes code sp m))))

(* The standard library's own manifest, src/std/vela.toml: each std module
   is a "dependency" with the capabilities it may take. *)
let std_file = "<std>/vela.toml"
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
          let missing = List.filter (fun c -> c <> Types.Root && not (List.exists (fun g -> List.mem g d.caps) (grant_of c))) i.caps in
          let missing = if List.mem Types.Root i.caps then Types.Root :: missing else missing in
          match missing with
          | [] -> None
          | c :: _ ->
            let need = match grant_of c with [] -> "Root, which no dependency can be granted" | g :: _ -> Printf.sprintf "\"%s\"" g in
            let where (sp : Span.t) = let l, _ = Source.line_col sp.file sp.lo in Printf.sprintf "%s:%d" sp.file l in
            if std then
              Some
                (Diag.make "E0320" d.dspan
                   (Printf.sprintf "standard library module `%s` uses capability `%s`, which %s does not declare" dn (Types.cap_name c) std_file)
                   ~notes:
                     [ Printf.sprintf "`%s` takes `%s` (%s)" i.name (Types.cap_name c) (where i.span);
                       "this is a bug in the standard library: declare the capability in src/std/vela.toml, or stop taking it" ])
            else
            Some
              (Diag.make "E0320" d.dspan
                 (Printf.sprintf "dependency `%s` uses capability `%s`, which vela.toml does not grant it" dn (Types.cap_name c))
                 ~notes:
                   [ Printf.sprintf "`%s` takes `%s` (%s)" i.name (Types.cap_name c) (let l, _ = Source.line_col i.span.file i.span.lo in Printf.sprintf "%s:%d" i.span.file l);
                     (if c = Types.Root then "a dependency can never take Root: pass it the narrower capability it needs"
                      else Printf.sprintf "to allow it, add %s to its caps" need) ]))))
    infos
