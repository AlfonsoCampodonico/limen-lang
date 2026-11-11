(* writ.toml: a package's name and its dependencies, each with the
   capabilities it may use.

     [package]
     name = "app"

     [dependencies]
     json = { path = "vendor/json" }
     fetch = { path = "../fetch", caps = ["net"] }

   A dependency is a directory holding NAME.wr (and any modules it uses from
   the same directory). Because Writ has no ambient authority, a function can
   only use a capability that reaches it as a parameter, so the capabilities a
   dependency can ever use are exactly those in its functions' signatures. The
   checker compares them with `caps` and rejects the program if a dependency
   asks for more than it was granted (E0320). Only a subset of TOML is read:
   tables, strings, string arrays and inline tables. *)

type dep = { dname : string; dir : string; caps : string list; dspan : Span.t }
type t = { pname : string; file : string; deps : dep list }

let cap_grants = [ "net"; "fs"; "listen"; "stdout"; "clock" ]

(* The grant that covers a capability type. *)
let grant_of (c : Types.cap) =
  match c with
  | Types.Net -> [ "net" ]
  | Conn -> [ "net"; "listen" ]
  | Listen | Listener -> [ "listen" ]
  | Fs -> [ "fs" ]
  | Stdout -> [ "stdout" ]
  | Clock -> [ "clock" ]
  | Root -> []

exception Bad of Span.t * string

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
  let name = ref "" and deps = ref [] in
  let dir = Filename.dirname file in
  skip_ws ();
  while !pos < n do
    if peek () = '[' then begin
      incr pos;
      let k, _ = key () in
      if peek () <> ']' then fail !pos "expected `]`";
      incr pos;
      if k <> "package" && k <> "dependencies" then fail (!pos - 1) ("unknown table [" ^ k ^ "]; writ.toml has [package] and [dependencies]");
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
        let path = match List.assoc_opt "path" fields with Some (VStr p) -> p | _ -> raise (Bad (sp, "a dependency needs `path = \"DIR\"`")) in
        let caps = match List.assoc_opt "caps" fields with Some (VArr cs) -> cs | None -> [] | Some _ -> raise (Bad (sp, "`caps` is a list of strings")) in
        List.iter
          (fun c -> if not (List.mem c cap_grants) then raise (Bad (sp, Printf.sprintf "unknown capability `%s`; use %s" c (String.concat ", " cap_grants))))
          caps;
        List.iter (fun (f, _) -> if f <> "path" && f <> "caps" then raise (Bad (sp, "unknown dependency field `" ^ f ^ "`"))) fields;
        let path = if Filename.is_relative path then Filename.concat dir path else path in
        if List.exists (fun d' -> d'.dname = d) !deps then raise (Bad (sp, "dependency `" ^ d ^ "` is listed twice"));
        deps := { dname = d; dir = path; caps; dspan = sp } :: !deps
      | "dependencies", _, _ -> raise (Bad (sp, "write a dependency as `name = { path = \"DIR\", caps = [...] }`"))
      | _ -> raise (Bad (sp, "keys belong in [package] or [dependencies]"))
    end;
    skip_ws ()
  done;
  { pname = !name; file; deps = List.rev !deps }

(* writ.toml in the directory of [main] or the nearest parent. *)
let find main =
  let rec up d =
    let p = Filename.concat d "writ.toml" in
    if Sys.file_exists p then Some p
    else
      let parent = Filename.dirname d in
      if parent = d then None else up parent
  in
  let d = Filename.dirname main in
  let beside = Filename.concat d "writ.toml" in
  if Sys.file_exists beside then Some beside
  else up (if Filename.is_relative d then Filename.concat (Sys.getcwd ()) d else d)

(* Load the package around [main]: Ok None when there is no writ.toml. *)
let load main : (t option, Diag.t) result =
  match find main with
  | None -> Ok None
  | Some p -> (
    match In_channel.with_open_bin p In_channel.input_all with
    | exception Sys_error m -> Error (Diag.make "E0321" Span.dummy m)
    | src -> ( try Ok (Some (parse p src)) with Bad (sp, m) -> Error (Diag.make "E0321" sp m)))

(* E0320: every function of a dependency may only take capabilities its
   package was granted. [module_pkg] maps a module name to its dependency. *)
let enforce (pkg : t) (module_pkg : (string * string) list) (infos : Check.fn_info list) : Diag.t list =
  List.filter_map
    (fun (i : Check.fn_info) ->
      match String.index_opt i.name '.' with
      | None -> None
      | Some k -> (
        let m = String.sub i.name 0 k in
        match List.assoc_opt m module_pkg with
        | None -> None
        | Some dn -> (
          let d = List.find (fun d -> d.dname = dn) pkg.deps in
          let missing = List.filter (fun c -> c <> Types.Root && not (List.exists (fun g -> List.mem g d.caps) (grant_of c))) i.caps in
          let missing = if List.mem Types.Root i.caps then Types.Root :: missing else missing in
          match missing with
          | [] -> None
          | c :: _ ->
            let need = match grant_of c with [] -> "Root, which no dependency can be granted" | g :: _ -> Printf.sprintf "\"%s\"" g in
            Some
              (Diag.make "E0320" d.dspan
                 (Printf.sprintf "dependency `%s` uses capability `%s`, which writ.toml does not grant it" dn (Types.cap_name c))
                 ~notes:
                   [ Printf.sprintf "`%s` takes `%s` (%s)" i.name (Types.cap_name c) (let l, _ = Source.line_col i.span.file i.span.lo in Printf.sprintf "%s:%d" i.span.file l);
                     (if c = Types.Root then "a dependency can never take Root: pass it the narrower capability it needs"
                      else Printf.sprintf "to allow it, add %s to its caps" need) ]))))
    infos
