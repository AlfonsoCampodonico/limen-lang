(* Checked program -> C source -> native binary (via the system C compiler). *)

let c_source ?(tests = false) items =
  let prog, g = Lower.lower_program ~tests items in
  let prog = Own.program (Pure.cse prog) in
  Cgen.program g prog

(* Libraries named by `foreign "lib"` blocks, linked as -l<lib>. The C
   library itself ("c") is always linked. A library outside the compiler's
   default search path is found through LIBRARY_PATH. *)
let link_flags libs = List.filter_map (fun l -> if l = "c" then None else Some ("-l" ^ l)) libs

let cc () = match Sys.getenv_opt "CC" with Some c when c <> "" -> c | _ -> "cc"

(* Compile [c_text] to [exe]. Returns the compiler's output on failure. *)
(* No FMA contraction: float results must not depend on the C compiler. *)
(* A program that may use TLS (Cgen marks it) gets the TLS core pasted in
   ahead of the runtime and is linked with OpenSSL, as found when vela was
   built (Openssl_config). Without OpenSSL it still compiles, and its TLS
   operations fail with an error saying so. Returns the C text to compile and
   the extra compiler and linker flags. *)
let prepare c_text =
  if Openssl_config.found && String.starts_with ~prefix:Cgen.tls_marker c_text then
    ("#ifndef _GNU_SOURCE\n#define _GNU_SOURCE\n#endif\n#define VELA_TLS 1\n" ^ Tls_text.text ^ c_text, Openssl_config.cflags, Openssl_config.libs)
  else (c_text, [], [])

(* The compiler's command line, as argv. *)
(* -O3, as Rust's release builds use: it lets clang unroll small fixed-size
   loops (an `Array[T, N]` walked pair by pair). Small programs compile no
   slower; a large one takes about a quarter longer than at -O2 (the
   operator example, 23k lines of C with the http module: 2.7 s, not 2.1 s). *)
let command ?(flags = [ "-O3" ]) ?(cflags = []) ?(libs = []) c_file exe =
  (cc () :: flags) @ cflags
  @ [ "-fwrapv"; "-ffp-contract=off"; "-fstack-clash-protection"; "-fexceptions"; "-w"; "-o"; exe; c_file ]
  @ libs @ [ "-lm" ]

(* ---------- the build cache ----------
   Compiling the generated C is nearly all of a build's time (the front end
   takes milliseconds), so binaries are kept by a digest of everything that
   decides them: the exact C text, the compiler's command line, which
   compiler it is, and the environment variables that change what it finds.
   A rebuild of an unchanged program copies the binary instead. VELA_CACHE
   names another directory, or `off` to compile every time. Failing to read
   or write the cache never fails a build: it just compiles. *)

let cache_dir () =
  match Sys.getenv_opt "VELA_CACHE" with
  | Some "off" -> None
  | Some d when d <> "" -> Some d
  | _ -> (
    match (Sys.getenv_opt "XDG_CACHE_HOME", Sys.getenv_opt "HOME") with
    | Some d, _ when d <> "" -> Some (Filename.concat d "vela")
    | _, Some h when h <> "" ->
      let mac = Filename.concat h "Library/Caches" in
      Some (if Sys.file_exists mac then Filename.concat mac "vela" else Filename.concat h ".cache/vela")
    | _ -> None)

(* The program `name` runs as, found on the PATH. *)
let which name =
  if String.contains name '/' then Some name
  else
    List.find_map
      (fun d -> let p = Filename.concat d name in if d <> "" && Sys.file_exists p then Some p else None)
      (String.split_on_char ':' (Option.value ~default:"" (Sys.getenv_opt "PATH")))

(* Which compiler this is, without starting it (`cc --version` costs 0.1 s
   on macOS): the file the PATH gives, and on macOS the clang behind the
   /usr/bin shim, in the developer directory xcode-select chose. Updating
   either replaces a file, which changes this. *)
let compiler_id () =
  let stat p = try let st = Unix.stat p in Printf.sprintf "%s:%d:%.0f:%d" p st.st_size st.st_mtime st.st_ino with Unix.Unix_error _ -> p ^ ":?" in
  let dev =
    match Sys.getenv_opt "DEVELOPER_DIR" with
    | Some d when d <> "" -> Some d
    | _ -> ( try Some (Unix.readlink "/var/db/xcode_select_link") with Unix.Unix_error _ -> None)
  in
  let behind =
    match dev with
    | Some d ->
      List.filter_map
        (fun rel -> let p = Filename.concat d rel in if Sys.file_exists p then Some (stat p) else None)
        [ "usr/bin/clang"; "Toolchains/XcodeDefault.xctoolchain/usr/bin/clang" ]
    | None -> []
  in
  String.concat "|" ((match which (cc ()) with Some p -> stat p | None -> cc ()) :: behind)

let cache_key c_text argv =
  let env = List.map (fun v -> v ^ "=" ^ Option.value ~default:"" (Sys.getenv_opt v))
      [ "LIBRARY_PATH"; "CPATH"; "C_INCLUDE_PATH"; "SDKROOT"; "MACOSX_DEPLOYMENT_TARGET"; "DEVELOPER_DIR" ] in
  Digest.to_hex (Digest.string (String.concat "\000" (c_text :: compiler_id () :: env @ argv)))

let copy_file src dst =
  let tmp = Printf.sprintf "%s.tmp%d" dst (Unix.getpid ()) in
  In_channel.with_open_bin src (fun ic -> Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] 0o755 tmp (fun oc ->
    let buf = Bytes.create 65536 in
    let rec go () = let n = In_channel.input ic buf 0 65536 in if n > 0 then (Out_channel.output oc buf 0 n; go ()) in
    go ()));
  Unix.chmod tmp 0o755;
  Unix.rename tmp dst

(* Keep the newest [keep] binaries; a hit refreshes its time, so this drops
   the least recently used. *)
let prune dir keep =
  try
    let entries =
      Sys.readdir dir |> Array.to_list
      |> List.filter_map (fun f ->
             let p = Filename.concat dir f in
             try if String.contains f '.' then None else Some (p, (Unix.stat p).st_mtime) with Unix.Unix_error _ -> None)
      |> List.sort (fun (_, a) (_, b) -> compare b a)
    in
    List.iteri (fun i (p, _) -> if i >= keep then try Sys.remove p with Sys_error _ -> ()) entries
  with Sys_error _ -> ()

let cache_keep = 200

let rec mkdir_p d =
  if not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let run_cc argv =
  let out, inp, err = Unix.open_process_args_full (cc ()) (Array.of_list argv) (Unix.environment ()) in
  close_out inp;
  let o = In_channel.input_all out in
  let e = In_channel.input_all err in
  match Unix.close_process_full (out, inp, err) with
  | Unix.WEXITED 0 -> Ok ()
  | _ -> Error (o ^ e)

let compile_c ?flags ?(libs = []) c_text exe : (unit, string) result =
  let c_file = exe ^ ".c" in
  let c_text, tls_cflags, tls_libs = prepare c_text in
  Out_channel.with_open_bin c_file (fun oc -> output_string oc c_text);
  let argv = command ?flags ~cflags:tls_cflags ~libs:(tls_libs @ link_flags libs) c_file exe in
  match cache_dir () with
  | None -> run_cc argv
  | Some dir -> (
    (* the key leaves out where this build's files go *)
    let shape = command ?flags ~cflags:tls_cflags ~libs:(tls_libs @ link_flags libs) "prog.c" "prog" in
    let cached = Filename.concat dir (cache_key c_text shape) in
    let hit = try Sys.file_exists cached && (copy_file cached exe; true) with Sys_error _ | Unix.Unix_error _ -> false in
    if hit then begin
      (try let now = Unix.gettimeofday () in Unix.utimes cached now now with Unix.Unix_error _ -> ());
      Ok ()
    end
    else
      match run_cc argv with
      | Error _ as e -> e
      | Ok () ->
        (try
           mkdir_p dir;
           copy_file exe cached;
           prune dir cache_keep
         with Sys_error _ | Unix.Unix_error _ -> ());
        Ok ())

let build ?flags ?tests items exe =
  match (compile_c ?flags ~libs:(Ffi.libs items) (c_source ?tests items) exe, Ffi.libs items) with
  | Ok (), _ -> Ok ()
  | Error m, [] -> Error m
  | Error m, libs ->
    (* most likely a missing library: say which ones the program links *)
    Error
      (m ^ Printf.sprintf "vela: the program links foreign libraries (%s); check that they are installed where the C compiler finds them\n"
             (String.concat ", " libs))
