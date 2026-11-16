(* Host side of capabilities: network allowlists and filesystem roots.
   Attenuation only ever narrows; nothing here can widen a grant. *)

let sprintf = Printf.sprintf

type ioerr = Denied of string | Failed of string

(* ---------- network ---------- *)

type url = { host : string; port : int; https : bool; path : string }

(* Strict, so the host we allow is the host we connect to: http or https
   only; no userinfo; host made of [A-Za-z0-9.-]; port plain digits in
   1..65535; no spaces or control characters. The path is what follows the
   host (`/` if nothing does), without any `#fragment`. Mirrors parse_url in
   the C runtime. *)
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
        let v6 = String.length auth > 0 && auth.[0] = '[' in
        let host, port =
          if v6 then
            match String.index_opt auth ']' with
            | Some k when k > 1 && (match Unix.inet_addr_of_string (String.sub auth 1 (k - 1)) with
                                    | a -> Unix.domain_of_sockaddr (Unix.ADDR_INET (a, 0)) = Unix.PF_INET6
                                    | exception Failure _ -> false) ->
              let h = String.sub auth 0 (k + 1) and tail = String.sub auth (k + 1) (String.length auth - k - 1) in
              if tail = "" then (h, Some (if scheme = "https" then 443 else 80))
              else if tail.[0] = ':' then
                let p = String.sub tail 1 (String.length tail - 1) in
                let digits = p <> "" && String.length p <= 5 && String.for_all (fun c -> c >= '0' && c <= '9') p in
                (h, if digits then match int_of_string_opt p with Some n when n >= 1 && n <= 65535 -> Some n | _ -> None else None)
              else ("", None)
            | _ -> ("", None)
          else
          match String.split_on_char ':' auth with
          | [ h ] -> (h, Some (if scheme = "https" then 443 else 80))
          | [ h; p ] ->
            let digits = p <> "" && String.length p <= 5 && String.for_all (fun c -> c >= '0' && c <= '9') p in
            (h, if digits then match int_of_string_opt p with Some n when n >= 1 && n <= 65535 -> Some n | _ -> None else None)
          | _ -> ("", None)
        in
        let path =
          let p = String.sub rest !stop (String.length rest - !stop) in
          let p = match String.index_opt p '#' with Some k -> String.sub p 0 k | None -> p in
          if p = "" then "/" else if p.[0] = '?' then "/" ^ p else p
        in
        if v6 && host = "" then Error "malformed IPv6 address in URL"
        else if (not v6) && (String.contains auth '@' || not (String.for_all (fun c -> host_ok c || c = ':') auth)) then
          Error "URL host may only contain letters, digits, `.` and `-`"
        else if host = "" then Error "URL has no host"
        else
          match port with
          | None -> Error "bad port in URL"
          | Some p -> Ok { host = String.lowercase_ascii host; port = p; https = scheme = "https"; path })

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

(* ---------- Exec ----------
   An Exec grant, `git` or `git status|remote -v`, as an allowlist entry:
   the program, then a newline before each argument prefix it is limited
   to, its words separated by single spaces (`git\nstatus\nremote -v`).
   None if malformed: an empty prefix, or prefixes for `*`. The C runtime's
   w_exec_entry does the same. *)
let words s = String.concat " " (List.filter (fun w -> w <> "") (String.split_on_char ' ' s))

let exec_entry g =
  match String.index_opt g ' ' with
  | None -> Some g
  | Some i ->
    let prog = String.sub g 0 i in
    let prefixes = List.map words (String.split_on_char '|' (String.sub g (i + 1) (String.length g - i - 1))) in
    if prog = "" || prog = "*" || List.exists (fun p -> p = "" || String.contains p '\n') prefixes then None
    else Some (String.concat "\n" (prog :: prefixes))

let exec_grant_help = "write PROGRAM, or PROGRAM ARGS|ARGS... with no empty prefix and not for *"

let exec_prog e = match String.index_opt e '\n' with Some i -> String.sub e 0 i | None -> e
let exec_prefixes e = match String.split_on_char '\n' e with _ :: ps -> ps | [] -> []

(* ---------- HTTP for Net.get ----------
   The request and the response parser are shared by nothing else, but the C
   runtime (Cap_net_get) implements exactly the same rules:
   - status line `HTTP/1.x NNN ...`;
   - header names lowercased, names and values trimmed of spaces and tabs,
     a repeated header's values joined with ", " in the first one's place;
   - body: chunked if Transfer-Encoding mentions chunked, else Content-Length
     bytes if given, else everything until the server closes. *)

let max_response = 64 * 1024 * 1024
let max_head = 65536 (* bytes before the blank line *)
let max_headers = 1000

let http_request (u : url) =
  let host = if u.port = (if u.https then 443 else 80) then u.host else sprintf "%s:%d" u.host u.port in
  sprintf "GET %s HTTP/1.1\r\nHost: %s\r\nUser-Agent: limen\r\nAccept: */*\r\nConnection: close\r\n\r\n" u.path host

let trim_sp s =
  let n = String.length s in
  let i = ref 0 and j = ref n in
  while !i < n && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
  while !j > !i && (s.[!j - 1] = ' ' || s.[!j - 1] = '\t') do decr j done;
  String.sub s !i (!j - !i)

let hex_size s =
  let s = trim_sp s in
  let v c = match c with '0' .. '9' -> Char.code c - 48 | 'a' .. 'f' -> Char.code c - 87 | 'A' .. 'F' -> Char.code c - 55 | _ -> -1 in
  if s = "" || String.length s > 15 || String.exists (fun c -> v c < 0) s then None
  else Some (String.fold_left (fun n c -> (n * 16) + v c) 0 s)

let dechunk data =
  let b = Buffer.create (String.length data) in
  let rec go pos =
    match Interp_util.find_sub data "\r\n" pos with
    | None -> Error (Failed "http: truncated chunked body")
    | Some eol -> (
      let line = String.sub data pos (eol - pos) in
      let line = match String.index_opt line ';' with Some k -> String.sub line 0 k | None -> line in
      match hex_size line with
      | None -> Error (Failed "http: bad chunk size")
      | Some 0 -> Ok (Buffer.contents b)
      | Some n ->
        let start = eol + 2 in
        if start + n > String.length data then Error (Failed "http: truncated chunked body")
        else (Buffer.add_string b (String.sub data start n); go (start + n + 2)))
  in
  go 0

(* (status, headers in order, body) *)
(* The error text of a TLS stream that ended without the peer's close
   message (VTLS_CUT_MSG in runtime/limen_tls.h). *)
let tls_cut = "tls: the connection was closed without a TLS close message, so the data may be cut short"

(* [cut]: the stream ended that way, so a body that runs until the close may
   be incomplete and is refused; a framed one is checked as usual. *)
let parse_http ?(cut = false) data : (int * (string * string) list * string, ioerr) result =
  match Interp_util.find_sub data "\r\n\r\n" 0 with
  | Some e when e > max_head -> Error (Failed "http: response headers larger than 64 KiB")
  | None when String.length data > max_head -> Error (Failed "http: response headers larger than 64 KiB")
  | None -> Error (Failed "http: incomplete response headers")
  | Some e when List.length (String.split_on_char '\n' (String.sub data 0 e)) > max_headers + 1 ->
    Error (Failed "http: more than 1000 response headers")
  | Some e -> (
    let lines = String.split_on_char '\n' (String.sub data 0 e) in
    let lines = List.map (fun l -> if String.ends_with ~suffix:"\r" l then String.sub l 0 (String.length l - 1) else l) lines in
    let raw = String.sub data (e + 4) (String.length data - e - 4) in
    let status =
      match lines with
      | first :: _ when String.starts_with ~prefix:"HTTP/1." first && String.length first >= 12 && first.[8] = ' '
                       && String.for_all (fun c -> c >= '0' && c <= '9') (String.sub first 9 3)
                       && (String.length first = 12 || first.[12] = ' ') ->
        Some (int_of_string (String.sub first 9 3))
      | _ -> None
    in
    match status with
    | None -> Error (Failed "http: malformed status line")
    | Some status -> (
      let rec headers acc = function
        | [] -> Ok (List.rev acc)
        | l :: rest -> (
          match String.index_opt l ':' with
          | None -> Error (Failed "http: malformed header line")
          | Some k ->
            let name = String.lowercase_ascii (trim_sp (String.sub l 0 k)) in
            let value = trim_sp (String.sub l (k + 1) (String.length l - k - 1)) in
            if List.mem_assoc name acc then
              headers (List.map (fun (n, v) -> if n = name then (n, v ^ ", " ^ value) else (n, v)) acc) rest
            else headers ((name, value) :: acc) rest)
      in
      match headers [] (List.tl lines) with
      | Error e -> Error e
      | Ok hs -> (
        let body =
          match (List.assoc_opt "transfer-encoding" hs, List.assoc_opt "content-length" hs) with
          | Some te, _ when Interp_util.find_sub (String.lowercase_ascii te) "chunked" 0 <> None -> dechunk raw
          | _, Some cl ->
            let cl = trim_sp cl in
            if cl = "" || String.length cl > 15 || not (String.for_all (fun c -> c >= '0' && c <= '9') cl) then
              Error (Failed "http: bad Content-Length")
            else
              let n = int_of_string cl in
              if String.length raw < n then Error (Failed "http: truncated body") else Ok (String.sub raw 0 n)
          | _ -> if cut then Error (Failed tls_cut) else Ok raw
        in
        match body with Ok b -> Ok (status, hs, b) | Error e -> Error e)))

(* ---------- filesystem ---------- *)

(* [top] is the granted directory, [root] this capability's (None: not
   granted). Every operation walks the path in C (limen_sandbox.h, through
   ffi_stubs.c), as the native runtime does: through directory descriptors,
   reading each symbolic link instead of letting the kernel follow it, so a
   directory swapped for a link while the walk goes on cannot lead out of
   the root. *)
type fs = { root : string option; ro : bool; top : string }

let not_granted = Denied "fs: not granted (run with --grant fs=DIR)"
let read_only_err = Denied "fs: this capability is read-only"

external fs_op : int -> string -> string -> string -> string -> int * string = "limen_fs_op"
external fs_stat : string -> string -> string -> int * string * int * int * bool * bool = "limen_fs_stat"

(* A file's modification time in milliseconds (following links), as the
   native runtime has it; -1 when it cannot be read. *)
external mtime_ms : string -> int = "limen_mtime_ms"

let fs_result = function 0, s -> Ok s | 1, m -> Error (Denied m) | _, m -> Error (Failed m)

let op fs n a b = match fs.root with None -> Error not_granted | Some root -> fs_result (fs_op n fs.top root a b)
let write_op fs n a b = if fs.root <> None && fs.ro then Error read_only_err else op fs n a b

let under fs rel =
  match fs.root with
  | None -> fs
  | Some root -> ( match fs_op 8 fs.top root rel "" with 0, r -> { fs with root = Some r } | _ -> { fs with root = None })

let read fs rel : (string, ioerr) result = op fs 0 rel ""
let write fs rel data : (unit, ioerr) result = Result.map ignore (write_op fs 1 rel data)
let append fs rel data : (unit, ioerr) result = Result.map ignore (write_op fs 2 rel data)

(* The absolute path of [rel], checked as for writing (the file need not
   exist, its directory must), for handing to C or another program: the
   real directory and the last name. *)
let full_path fs rel : (string, ioerr) result = op fs 7 rel ""

(* Names in a directory, sorted, without `.` and `..`. *)
let list fs rel : (string list, ioerr) result =
  Result.map
    (fun s -> List.sort compare (List.filter (fun n -> n <> "") (String.split_on_char '\000' s)))
    (op fs 3 rel "")

(* A file or an empty directory; never the granted directory itself. *)
let remove fs rel : (unit, ioerr) result = Result.map ignore (write_op fs 4 rel "")
let make_dir fs rel : (unit, ioerr) result = Result.map ignore (write_op fs 5 rel "")

(* Move [from] to [to] in one step, replacing what is at [to]; never the
   granted directory itself. *)
let rename fs from to_ : (unit, ioerr) result = Result.map ignore (write_op fs 6 from to_)

(* Size, modification time in milliseconds and kind of [rel], following
   links: (size, modified_ms, is_file, is_dir). *)
let info fs rel : (int * int * bool * bool, ioerr) result =
  match fs.root with
  | None -> Error not_granted
  | Some root -> (
    match fs_stat fs.top root rel with
    | 0, _, size, ms, f, d -> Ok (size, ms, f, d)
    | 1, m, _, _, _, _ -> Error (Denied m)
    | _, m, _, _, _, _ -> Error (Failed m))

let exists fs rel = Result.is_ok (info fs rel)
let is_dir fs rel = match info fs rel with Ok (_, _, _, d) -> d | Error _ -> false

(* ---------- test directories ----------
   A test that takes `fs: Fs` gets a fresh directory, removed afterwards. *)

let test_dir () = Unix.realpath (Filename.temp_dir "limen-test-" "")

let rec remove_tree p =
  match Unix.lstat p with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
    Array.iter (fun n -> remove_tree (Filename.concat p n)) (try Sys.readdir p with Sys_error _ -> [||]);
    (try Unix.rmdir p with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink p with Unix.Unix_error _ -> ())
  | exception Unix.Unix_error _ -> ()

(* Does the test (its parameter types) take Fs? *)
let takes_fs (d : Ast.fn_decl) = List.exists (fun (p : Ast.param) -> match p.prm_ty.tdesc with Ast.TyName ("Fs", _) -> true | _ -> false) d.fn_params
