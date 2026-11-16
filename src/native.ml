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
   ahead of the runtime and is linked with OpenSSL, as found when limen was
   built (Openssl_config). Without OpenSSL it still compiles, and its TLS
   operations fail with an error saying so. Returns the C text to compile and
   the extra compiler and linker flags. *)
let uses_tls c_text = Openssl_config.found && String.starts_with ~prefix:Cgen.tls_marker c_text

let tls_defines = "#ifndef _GNU_SOURCE\n#define _GNU_SOURCE\n#endif\n#define LIMEN_TLS 1\n"

let prepare c_text =
  if uses_tls c_text then (tls_defines ^ Tls_text.text ^ c_text, Openssl_config.cflags, Openssl_config.libs)
  else (c_text, [], [])

(* The compiler's command line, as argv. *)
(* -O3, as Rust's release builds use: it lets clang unroll small fixed-size
   loops (an `Array[T, N]` walked pair by pair). Small programs compile no
   slower; a large one takes about a quarter longer than at -O2 (the
   operator example, 23k lines of C with the http module: 2.7 s, not 2.1 s). *)
(* A development build: `limen run --native`, `limen test --native` (but
   not `--bench`) and `limen build --dev`. Without optimisation the C
   compiler is faster, and the program runs the same, only slower. *)
let dev_flags = [ "-O0" ]

let c_flags = [ "-fwrapv"; "-ffp-contract=off"; "-fstack-clash-protection"; "-fexceptions"; "-w" ]

let command ?(flags = [ "-O3" ]) ?(cflags = []) ?(libs = []) c_file exe =
  (cc () :: flags) @ cflags @ c_flags @ [ "-o"; exe; c_file ] @ libs @ [ "-lm" ]

(* A split build's commands: one piece to an object, and the objects to the
   program. *)
let object_command ?(flags = [ "-O3" ]) ?(cflags = []) c_file obj = (cc () :: flags) @ cflags @ c_flags @ [ "-c"; "-o"; obj; c_file ]

let link_command ?(flags = [ "-O3" ]) ?(libs = []) objs exe = (cc () :: flags) @ [ "-o"; exe ] @ objs @ libs @ [ "-lm" ]

(* ---------- the build cache ----------
   Compiling the generated C is nearly all of a build's time (the front end
   takes milliseconds), so binaries are kept by a digest of everything that
   decides them: the exact C text, the compiler's command line, which
   compiler it is, and the environment variables that change what it finds.
   A rebuild of an unchanged program copies the binary instead. LIMEN_CACHE
   names another directory, or `off` to compile every time. Failing to read
   or write the cache never fails a build: it just compiles. *)

let cache_dir () =
  match Sys.getenv_opt "LIMEN_CACHE" with
  | Some "off" -> None
  | Some d when d <> "" -> Some d
  | _ -> (
    match (Sys.getenv_opt "XDG_CACHE_HOME", Sys.getenv_opt "HOME") with
    | Some d, _ when d <> "" -> Some (Filename.concat d "limen")
    | _, Some h when h <> "" ->
      let mac = Filename.concat h "Library/Caches" in
      Some (if Sys.file_exists mac then Filename.concat mac "limen" else Filename.concat h ".cache/limen")
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
   either replaces a file, which changes its size or its modification time
   (in milliseconds, as Fs.info gives them to the compiler written in
   Limen, which keys the cache the same way). *)
let compiler_id () =
  let stat p = try let st = Unix.stat p in Printf.sprintf "%s:%d:%d" p st.st_size (Caps.mtime_ms p) with Unix.Unix_error _ -> p ^ ":?" in
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
let prune ?(ext = "") dir keep =
  try
    let entries =
      Sys.readdir dir |> Array.to_list
      |> List.filter_map (fun f ->
             let p = Filename.concat dir f in
             let kind = match String.rindex_opt f '.' with Some i -> String.sub f i (String.length f - i) | None -> "" in
             if kind <> ext then None else match Caps.mtime_ms p with -1 -> None | t -> Some (p, t))
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

(* What a native build says when OpenSSL isn't there for a TLS program. *)
let tls_missing = "limen: this program uses TLS, which needs OpenSSL's headers and libraries: on macOS `brew install openssl@3`, on Debian or Ubuntu `apt install libssl-dev`"


(* Where [sub] next occurs in [text] at or after [from]. *)
let find_sub text sub from =
  let n = String.length text and m = String.length sub in
  let rec at i =
    if i + m > n then None
    else
      let rec same j = j = m || (text.[i + j] = sub.[j] && same (j + 1)) in
      if same 0 then Some i else at (i + 1)
  in
  at from

(* Whether [text] contains [word]. *)
let mentions text word = find_sub text word 0 <> None

(* ---------- split builds ----------
   Compiling the generated C is nearly all of a build, and one C file
   compiles on one core. So a build compiles the runtime once, as an object
   kept in the build cache, and its declarations once, as a precompiled
   header (where the C compiler is clang); and it cuts the program into
   pieces of about [piece_lines] lines, which compile at the same time
   against a shared header: the program's types, constants, and a
   prototype for each function. A small program is one piece, as it was
   generated, compiled and linked in one step. todo_api (40,000 lines of C, most of them the http,
   json and sql modules) went from 4 s to under a second at -O3. *)

(* The runtime's functions are W_RT, `static` unless this says otherwise
   (limen_rt.h). *)
let object_mode = "#define W_RT\n"

let piece_lines = 3000
let pieces_max = 16

(* The change in brace depth over [line], outside strings, characters and
   comments. *)
let brace_delta line =
  let n = String.length line in
  let rec go i d q =
    if i >= n then d
    else
      let c = line.[i] in
      if q <> '\000' then if c = '\\' then go (i + 2) d q else if c = q then go (i + 1) d '\000' else go (i + 1) d q
      else if c = '/' && i + 1 < n && line.[i + 1] = '*' then (
        match find_sub line "*/" (i + 2) with Some j -> go (j + 2) d q | None -> d)
      else if c = '/' && i + 1 < n && line.[i + 1] = '/' then d
      else if c = '"' || c = '\'' then go (i + 1) d c
      else if c = '{' then go (i + 1) (d + 1) q
      else if c = '}' then go (i + 1) (d - 1) q
      else go (i + 1) d q
  in
  go 0 0 '\000'

(* The program's C (what follows the runtime) as the header its pieces
   share, and the pieces. Everything at the top level of generated C starts
   in the first column, and a function's signature is on its first line.
   A definition moves to one piece, no longer `static`, and leaves its
   prototype in the header; so does a variable, as an `extern` declaration.
   Types, `static inline` functions and constant tables stay in the header,
   so each piece has its own copy. None when the C has a shape this does
   not expect: then the program compiles as one file. *)
let split_program text =
  let exception Odd in
  let lines = Array.of_list (String.split_on_char '\n' text) in
  let n = Array.length lines in
  (* the header, newest first; a prototype waits to learn whether its
     function moves to a piece (a function a macro defines stays static) *)
  let shared = ref [] and vars = Buffer.create 4096 in
  let defs = ref [] and moved = Hashtbl.create 1024 in
  let add b s = Buffer.add_string b s; Buffer.add_char b '\n' in
  let unstatic s = String.sub s 7 (String.length s - 7) in
  let name_of s =
    let p = String.index s '(' in
    let rec back i = if i > 0 && (match s.[i - 1] with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false) then back (i - 1) else i in
    let b = back p in
    String.sub s b (p - b)
  in
  try
    let i = ref 0 in
    while !i < n do
      let start = !i in
      let first = lines.(start) in
      let d = ref (brace_delta first) in
      while !d > 0 do
        incr i;
        if !i >= n then raise Odd;
        d := !d + brace_delta lines.(!i)
      done;
      if !d < 0 then raise Odd;
      let last = !i in
      incr i;
      let item () = String.concat "\n" (Array.to_list (Array.sub lines start (last - start + 1))) in
      let starts p = String.starts_with ~prefix:p first in
      if starts "static inline " || starts "static const " then shared := `Text (item ()) :: !shared
      else if starts "int main(" then defs := (last - start + 1, item ()) :: !defs
      else if starts "static " then begin
        let signature = match find_sub first ") {" 0 with Some k when not (mentions (String.sub first 0 k) " = ") -> Some k | _ -> None in
        let assigns = find_sub first " = " 0 in
        match signature with
        | Some k ->
          (* a definition *)
          Hashtbl.replace moved (name_of first) ();
          shared := `Text (unstatic (String.sub first 0 (k + 1)) ^ ";") :: !shared;
          defs := (last - start + 1, unstatic (item ())) :: !defs
        | None when assigns = None && last = start && String.contains first '(' && String.ends_with ~suffix:");" first ->
          (* a prototype *)
          shared := `Proto first :: !shared
        | None when String.ends_with ~suffix:";" lines.(last) && (assigns <> None || last = start) ->
          (* a variable *)
          let decl = match assigns with Some k -> String.sub first 0 k | None -> String.sub first 0 (String.length first - 1) in
          shared := `Text ("extern " ^ unstatic decl ^ ";") :: !shared;
          add vars (unstatic (item ()))
        | None -> raise Odd
      end
      else shared := `Text (item ()) :: !shared
    done;
    let header = Buffer.create (String.length text / 4) in
    List.iter
      (function
        | `Text t -> add header t
        | `Proto p -> add header (if Hashtbl.mem moved (name_of p) then unstatic p else p))
      (List.rev !shared);
    let defs = List.rev !defs in
    let total = List.fold_left (fun a (k, _) -> a + k) 0 defs in
    let count = max 1 (min pieces_max ((total + piece_lines - 1) / piece_lines)) in
    (* one piece: the program as it was generated, everything still static,
       so the C compiler may change how its functions are called (nbody
       runs twice as fast as with them external) *)
    if count = 1 then Some ("", [ text ]) else
    let pieces = Array.init count (fun _ -> Buffer.create (total * 40 / count)) in
    Buffer.add_buffer pieces.(0) vars;
    ignore
      (List.fold_left
         (fun acc (k, t) ->
           add pieces.(min (count - 1) (acc * count / max 1 total)) t;
           acc + k)
         0 defs);
    Some (Buffer.contents header, Array.to_list (Array.map Buffer.contents pieces))
  with Odd -> None

(* Start [argv] with its output going to [log]; the pid. *)
let spawn argv log =
  let fd = Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 in
  let pid = Unix.create_process (List.hd argv) (Array.of_list argv) Unix.stdin fd fd in
  Unix.close fd;
  pid

let rec wait_pid pid = try snd (Unix.waitpid [] pid) with Unix.Unix_error (Unix.EINTR, _, _) -> wait_pid pid

(* Run every command at once, each logging to its own file: Ok, or the
   output of the first that failed. *)
let run_all jobs =
  let started = List.map (fun (argv, log) -> (spawn argv log, log)) jobs in
  List.fold_left
    (fun acc (pid, log) ->
      match (wait_pid pid, acc) with
      | Unix.WEXITED 0, _ -> acc
      | _, Ok () -> Error (try In_channel.with_open_bin log In_channel.input_all with Sys_error _ -> "")
      | _, Error _ -> acc)
    (Ok ()) started

(* The objects and headers a build keeps, by digest; the newest [object_keep]
   are kept. *)
let object_keep = 400

(* [name] in the cache, made by [make] (given a path to write) unless it is
   there already: the path, or None when it could not be made. Writes go to
   a temporary name first, so a build never sees half a file. *)
let cached_file dir name make =
  let path = Filename.concat dir name in
  if Sys.file_exists path then begin
    (try let now = Unix.gettimeofday () in Unix.utimes path now now with Unix.Unix_error _ -> ());
    Some path
  end
  else begin
    let tmp = Printf.sprintf "%s.tmp%d" path (Unix.getpid ()) in
    if make tmp then (
      try Unix.rename tmp path; Some path with Unix.Unix_error _ -> None)
    else (
      (try Sys.remove tmp with Sys_error _ -> ());
      None)
  end

let write_file path text = Out_channel.with_open_bin path (fun oc -> output_string oc text)

(* Build [exe] from the program's [shared] header and [pieces], with the
   runtime as an object. [prefix] is what goes ahead of the runtime (the TLS
   defines and core, or nothing); [defines] what goes ahead of the
   declarations in each piece. Objects and the precompiled header live in
   the cache when there is one, and in [tmp] otherwise. *)
let build_split ?flags ~cflags ~libs ~prefix ~defines ~tmp (shared, pieces) exe =
  let dir = cache_dir () in
  let key text argv = cache_key text argv in
  let obj_argv c o = object_command ?flags ~cflags c o in
  (* the runtime, compiled once *)
  let rt_text = object_mode ^ prefix ^ Runtime_text.text in
  let rt_key = key rt_text (obj_argv "prog.c" "prog.o") ^ ".o" in
  let rt_cached = Option.bind dir (fun d -> let p = Filename.concat d rt_key in if Sys.file_exists p then Some p else None) in
  (* the declarations, precompiled where the compiler can (clang) *)
  let decls = object_mode ^ defines ^ Runtime_text.header in
  let pch_argv h o = (cc () :: Option.value flags ~default:[ "-O3" ]) @ cflags @ c_flags @ [ "-x"; "c-header"; "-o"; o; h ] in
  let pch =
    match dir with
    | None -> None
    | Some d ->
      let k = key decls (pch_argv "decls.h" "decls.pch") in
      if Sys.file_exists (Filename.concat d (k ^ ".nopch")) then None
      else begin
        (* the header stays next to its precompiled form, which records it *)
        let h = Filename.concat d (k ^ ".h") in
        let made =
          try mkdir_p d; if not (Sys.file_exists h) then ignore (cached_file d (k ^ ".h") (fun out -> write_file out decls; true)); Sys.file_exists h
          with Sys_error _ | Unix.Unix_error _ -> false
        in
        let works out =
          (* gcc writes a header file it can't load through -include-pch *)
          let probe = Filename.concat tmp "probe.c" in
          write_file probe "int limen_probe;\n";
          run_all
            [ (pch_argv h out, Filename.concat tmp "pch.log");
            ] = Ok ()
          && run_all [ ((cc () :: Option.value flags ~default:[ "-O3" ]) @ [ "-include-pch"; out ] @ cflags @ c_flags @ [ "-fsyntax-only"; probe ], Filename.concat tmp "probe.log") ] = Ok ()
        in
        match if made then cached_file d (k ^ ".pch") works else None with
        | Some p -> Some p
        | None ->
          (try write_file (Filename.concat d (k ^ ".nopch")) "" with Sys_error _ -> ());
          None
      end
  in
  let use_pch = match pch with Some p -> [ "-include-pch"; p ] | None -> [] in
  let source text = (if pch = None then decls ^ "\n" else "") ^ shared ^ "\n" ^ text in
  let piece_files = List.mapi (fun i text -> let f = Filename.concat tmp (Printf.sprintf "piece%d.c" i) in write_file f (source text); f) pieces in
  match (rt_cached, piece_files) with
  | Some rt, [ only ] ->
    (* one piece, the runtime ready: compile and link in one step *)
    run_cc ((cc () :: Option.value flags ~default:[ "-O3" ]) @ use_pch @ cflags @ c_flags @ [ "-o"; exe; only; rt ] @ libs @ [ "-lm" ])
  | _ ->
    let rt_job, rt_obj =
      match rt_cached with
      | Some p -> ([], p)
      | None ->
        let c = Filename.concat tmp "runtime.c" and o = Filename.concat tmp "runtime.o" in
        write_file c rt_text;
        ([ (obj_argv c o, Filename.concat tmp "runtime.log") ], o)
    in
    let piece_jobs = List.mapi (fun i f -> let o = Filename.chop_suffix f ".c" ^ ".o" in ((cc () :: Option.value flags ~default:[ "-O3" ]) @ use_pch @ cflags @ c_flags @ [ "-c"; "-o"; o; f ], Printf.sprintf "%s.log" f)) piece_files in
    match run_all (rt_job @ piece_jobs) with
    | Error _ as e -> e
    | Ok () ->
      (match (dir, rt_cached) with
       | Some d, None -> (try mkdir_p d; copy_file rt_obj (Filename.concat d rt_key) with Sys_error _ | Unix.Unix_error _ -> ())
       | _ -> ());
      Option.iter (fun d -> prune ~ext:"" d cache_keep; prune ~ext:".o" d object_keep) dir;
      run_cc (link_command ?flags ~libs (rt_obj :: List.map (fun f -> Filename.chop_suffix f ".c" ^ ".o") piece_files) exe)

let compile_c ?flags ?(libs = []) c_text exe : (unit, string) result =
  let full, tls_cflags, tls_libs = prepare c_text in
  let libs = tls_libs @ link_flags libs in
  let hint = function
    | Error out when tls_cflags <> [] && mentions out "openssl/" -> Error (out ^ "\n" ^ tls_missing ^ "\n")
    | r -> r
  in
  let tls = uses_tls c_text in
  (* the program: what follows the runtime *)
  let program =
    let t = if String.starts_with ~prefix:Cgen.tls_marker c_text then String.sub c_text (String.length Cgen.tls_marker) (String.length c_text - String.length Cgen.tls_marker) else c_text in
    let r = String.length Runtime_text.text in
    if String.starts_with ~prefix:Runtime_text.text t then Some (String.sub t r (String.length t - r)) else None
  in
  let whole () =
    let c_file = exe ^ ".c" in
    write_file c_file full;
    run_cc (command ?flags ~cflags:tls_cflags ~libs c_file exe)
  in
  let compile () =
    hint
      (match Option.bind program split_program with
       | Some (_, [ _ ]) when cache_dir () = None ->
         (* nowhere to keep the runtime: one file, which leaves out the
            parts of it this program doesn't use *)
         whole ()
       | Some parts ->
         let tmp = Filename.temp_dir "limen-build" "" in
         Fun.protect
           ~finally:(fun () -> Caps.remove_tree tmp)
           (fun () ->
             build_split ?flags ~cflags:tls_cflags ~libs
               ~prefix:(if tls then tls_defines ^ Tls_text.text else "")
               ~defines:(if tls then tls_defines else "")
               ~tmp parts exe)
       | None -> whole ())
  in
  match cache_dir () with
  | None -> compile ()
  | Some dir -> (
    (* the key leaves out where this build's files go *)
    let shape = command ?flags ~cflags:tls_cflags ~libs "prog.c" "prog" in
    let cached = Filename.concat dir (cache_key full shape) in
    let hit = try Sys.file_exists cached && (copy_file cached exe; true) with Sys_error _ | Unix.Unix_error _ -> false in
    if hit then begin
      (try let now = Unix.gettimeofday () in Unix.utimes cached now now with Unix.Unix_error _ -> ());
      Ok ()
    end
    else
      match compile () with
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
      (m ^ Printf.sprintf "limen: the program links foreign libraries (%s); check that they are installed where the C compiler finds them\n"
             (String.concat ", " libs))

(* The message for a failed C compile: the compiler's output, then either
   the TLS hint (already in it) or the note that this is limen's fault. *)
let cc_failed m =
  if mentions m tls_missing then m
  else m ^ "limen: the C compiler failed (this is a bug in limen)\n"
