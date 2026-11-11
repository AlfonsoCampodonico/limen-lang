(* Host side of capabilities: network allowlists and filesystem roots.
   Attenuation only ever narrows; nothing here can widen a grant. *)

let sprintf = Printf.sprintf

type ioerr = Denied of string | Failed of string

(* ---------- network ---------- *)

type url = { host : string; port : int }

(* Strict, so the host we allow is the host curl connects to: http or https
   only; no userinfo; host made of [A-Za-z0-9.-]; port plain digits in
   1..65535; no spaces or control characters. Mirrors parse_url in the C
   runtime. *)
let parse_url u : (url, string) result =
  if String.exists (fun c -> Char.code c <= 0x20 || Char.code c >= 0x7f) u then
    Error "URL contains spaces or non-ASCII characters"
  else
    match Interp_util.find_sub u "://" 0 with
    | None -> Error "URL has no scheme"
    | Some i -> (
      let scheme = String.lowercase_ascii (String.sub u 0 i) in
      if scheme <> "http" && scheme <> "https" then
        Error (sprintf "scheme `%s` is not allowed, only http and https" (if String.length scheme > 32 then String.sub scheme 0 32 else scheme))
      else
        let rest = String.sub u (i + 3) (String.length u - i - 3) in
        let stop = ref (String.length rest) in
        String.iteri (fun j c -> if (c = '/' || c = '?' || c = '#') && j < !stop then stop := j) rest;
        let auth = String.sub rest 0 !stop in
        let host_ok c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '.' || c = '-' in
        let host, port =
          match String.split_on_char ':' auth with
          | [ h ] -> (h, Some (if scheme = "https" then 443 else 80))
          | [ h; p ] ->
            let digits = p <> "" && String.length p <= 5 && String.for_all (fun c -> c >= '0' && c <= '9') p in
            (h, if digits then match int_of_string_opt p with Some n when n >= 1 && n <= 65535 -> Some n | _ -> None else None)
          | _ -> ("", None)
        in
        if String.contains auth '@' || not (String.for_all (fun c -> host_ok c || c = ':') auth) then
          Error "URL host may only contain letters, digits, `.` and `-`"
        else if host = "" then Error "URL has no host"
        else
          match port with
          | None -> Error "bad port in URL"
          | Some p -> Ok { host = String.lowercase_ascii host; port = p })

(* A grant entry: "*", "host" (any port) or "host:port". *)
type entry = Any | Host of string * int option

let parse_entry s =
  if s = "*" then Any
  else
    match String.rindex_opt s ':' with
    | Some k -> (
      match int_of_string_opt (String.sub s (k + 1) (String.length s - k - 1)) with
      | Some p -> Host (String.lowercase_ascii (String.sub s 0 k), Some p)
      | None -> Host (String.lowercase_ascii s, None))
    | None -> Host (String.lowercase_ascii s, None)

let permits allowed ~host ~port =
  List.exists
    (fun s ->
      match parse_entry s with
      | Any -> true
      | Host (h, p) -> h = host && (match p with None -> true | Some p -> p = port))
    allowed

let attenuate allowed req =
  let covered =
    match parse_entry req with
    | Any -> List.mem "*" allowed
    | Host (h, Some p) -> permits allowed ~host:h ~port:p
    | Host (h, None) ->
      List.exists
        (fun s -> match parse_entry s with Any -> true | Host (h', None) -> h' = h | Host _ -> false)
        allowed
  in
  if covered then [ req ] else []

let net_get allowed url : (string, ioerr) result =
  match parse_url url with
  | Error m -> Error (Failed m)
  | Ok u ->
    if not (permits allowed ~host:u.host ~port:u.port) then
      Error (Denied (sprintf "net: %s:%d is not granted" u.host u.port))
    else
      try
        (* No -L: redirects are not followed, so a granted host cannot bounce us elsewhere. *)
        let argv =
          [| "curl"; "-sS"; "--fail"; "--max-time"; "20"; "--proto"; "=http,https"; "--"; url |]
        in
        let out, inp, errc = Unix.open_process_args_full "curl" argv (Unix.environment ()) in
        close_out inp;
        let body = In_channel.input_all out in
        let emsg = In_channel.input_all errc in
        match Unix.close_process_full (out, inp, errc) with
        | Unix.WEXITED 0 -> Ok body
        | _ -> Error (Failed (String.trim emsg))
      with Unix.Unix_error (e, _, _) -> Error (Failed (Unix.error_message e))

(* ---------- filesystem ---------- *)

type fs = { root : string option; ro : bool }

let not_granted = Denied "fs: not granted (run with --grant fs=DIR)"

let normalize root rel : (string, string) result =
  if rel <> "" && rel.[0] = '/' then Error "absolute paths are not allowed; paths are relative to the granted directory"
  else
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | ("" | ".") :: r -> go acc r
      | ".." :: r -> ( match acc with [] -> Error "path escapes the granted directory" | _ :: a -> go a r)
      | p :: r -> go (p :: acc) r
    in
    Result.map
      (fun ps -> if ps = [] then root else Filename.concat root (String.concat "/" ps))
      (go [] (String.split_on_char '/' rel))

let inside root p = p = root || String.starts_with ~prefix:(if root = "/" then "/" else root ^ "/") p
let real p = try Some (Unix.realpath p) with Unix.Unix_error _ -> None

(* Resolve [rel] under the root, rejecting `..` escapes and symlinks that point outside. *)
let resolve fs rel ~for_write =
  match fs.root with
  | None -> Error not_granted
  | Some root -> (
    match normalize root rel with
    | Error m -> Error (Denied ("fs: " ^ m))
    | Ok p -> (
      let target = if for_write && not (Sys.file_exists p) then Filename.dirname p else p in
      match real target with
      | Some rp when inside root rp -> Ok p
      | Some _ -> Error (Denied "fs: path resolves outside the granted directory")
      | None ->
        if for_write then Error (Failed (sprintf "fs: directory of `%s` does not exist" rel))
        else Error (Failed (sprintf "fs: `%s` does not exist" rel))))

let under fs rel =
  match fs.root with
  | None -> fs
  | Some root -> (
    match normalize root rel with
    | Error _ -> { fs with root = None }
    | Ok p -> (
      match real p with
      | Some rp when inside root rp -> { fs with root = Some rp }
      | Some _ -> { fs with root = None }
      | None -> { fs with root = Some p }))

let read fs rel : (string, ioerr) result =
  match resolve fs rel ~for_write:false with
  | Error e -> Error e
  | Ok p -> ( try Ok (In_channel.with_open_bin p In_channel.input_all) with Sys_error m -> Error (Failed m))

let write fs rel data : (unit, ioerr) result =
  if fs.root <> None && fs.ro then Error (Denied "fs: this capability is read-only")
  else
    match resolve fs rel ~for_write:true with
    | Error e -> Error e
    | Ok p -> (
      try Ok (Out_channel.with_open_bin p (fun oc -> Out_channel.output_string oc data))
      with Sys_error m -> Error (Failed m))

let exists fs rel = match resolve fs rel ~for_write:false with Ok p -> Sys.file_exists p | Error _ -> false

let read_only_err = Denied "fs: this capability is read-only"

let append fs rel data : (unit, ioerr) result =
  if fs.root <> None && fs.ro then Error read_only_err
  else
    match resolve fs rel ~for_write:true with
    | Error e -> Error e
    | Ok p -> (
      try
        Ok (Out_channel.with_open_gen [ Open_wronly; Open_append; Open_creat; Open_binary ] 0o644 p (fun oc ->
                Out_channel.output_string oc data))
      with Sys_error m -> Error (Failed m))

(* Names in a directory, sorted, without `.` and `..`. *)
let list fs rel : (string list, ioerr) result =
  match resolve fs rel ~for_write:false with
  | Error e -> Error e
  | Ok p -> (
    try Ok (List.sort compare (Array.to_list (Sys.readdir p)))
    with Sys_error _ -> Error (Failed (sprintf "fs: `%s` is not a directory" rel)))

let is_dir fs rel = match resolve fs rel ~for_write:false with Ok p -> (try Sys.is_directory p with Sys_error _ -> false) | Error _ -> false

(* A file or an empty directory; never the granted directory itself. *)
let remove fs rel : (unit, ioerr) result =
  if fs.root <> None && fs.ro then Error read_only_err
  else
    match resolve fs rel ~for_write:false with
    | Error e -> Error e
    | Ok p when Some p = fs.root -> Error (Denied "fs: cannot remove the granted directory itself")
    | Ok p -> (
      try if Sys.is_directory p then Unix.rmdir p else Unix.unlink p; Ok ()
      with Unix.Unix_error (e, _, _) -> Error (Failed (sprintf "fs: `%s`: %s" rel (Unix.error_message e))))

let make_dir fs rel : (unit, ioerr) result =
  if fs.root <> None && fs.ro then Error read_only_err
  else
    match resolve fs rel ~for_write:true with
    | Error e -> Error e
    | Ok p -> (
      try Unix.mkdir p 0o755; Ok ()
      with Unix.Unix_error (e, _, _) -> Error (Failed (sprintf "fs: `%s`: %s" rel (Unix.error_message e))))

(* ---------- test directories ----------
   A test that takes `fs: Fs` gets a fresh directory, removed afterwards. *)

let test_dir () = Unix.realpath (Filename.temp_dir "vela-test-" "")

let rec remove_tree p =
  match Unix.lstat p with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
    Array.iter (fun n -> remove_tree (Filename.concat p n)) (try Sys.readdir p with Sys_error _ -> [||]);
    (try Unix.rmdir p with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink p with Unix.Unix_error _ -> ())
  | exception Unix.Unix_error _ -> ()

(* Does the test (its parameter types) take Fs? *)
let takes_fs (d : Ast.fn_decl) = List.exists (fun (p : Ast.param) -> match p.prm_ty.tdesc with Ast.TyName ("Fs", _) -> true | _ -> false) d.fn_params
