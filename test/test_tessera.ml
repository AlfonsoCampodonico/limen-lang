open Tessera

let sprintf = Printf.sprintf
let total = ref 0
let failed = ref 0

(* TESSERA_TEST_ONLY=a,b runs only the program tests whose names start with
   one of those prefixes (for working on a feature; the full run ignores it). *)
let selected name =
  match Sys.getenv_opt "TESSERA_TEST_ONLY" with
  | None | Some "" -> true
  | Some ps -> List.exists (fun p -> String.starts_with ~prefix:p name) (String.split_on_char ',' ps)

let ok name cond msg =
  incr total;
  if not cond then begin
    incr failed;
    Printf.printf "FAIL %s: %s\n" name msg
  end

(* TESSERA_DUMP_DIR=dir writes every program into dir/accept (it parses) or
   dir/reject (lexer or parser error), for checking grammar/tessera.gbnf. *)
(* Rejected by the parser but within the grammar's documented superset
   (keywords are not excluded from identifiers). *)
let grammar_superset =
  [ "tuple-let-refutable"; "select-bad-arm"; "group-until-bad"; "pipe-not-callable"; "pipe-binary-rhs"; "trailing-in-if"; "trailing-in-while";
    "trailing-in-for"; "trailing-in-match" ]

let dump name src parsed =
  match Sys.getenv_opt "TESSERA_DUMP_DIR" with
  | None -> ()
  | Some _ when (not parsed) && List.mem name grammar_superset -> ()
  | Some dir ->
    let sub = Filename.concat dir (if parsed then "accept" else "reject") in
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    (try Unix.mkdir sub 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    Out_channel.with_open_bin (Filename.concat sub (name ^ ".tsr")) (fun oc -> output_string oc src)

(* tessera fmt on every program that parses: the result must parse to the same
   tree (ignoring positions), formatting again must not change it, and no
   comment may be lost. Checked once per program. *)
let fmt_checked : (string, unit) Hashtbl.t = Hashtbl.create 64
let fmt_failures = ref []

let check_fmt name src () =
  if not (Hashtbl.mem fmt_checked name) then begin
    Hashtbl.replace fmt_checked name ();
    let file = name ^ ".tsr" in
    let comments_of s = (try ignore (Lexer.lex "c.tsr" s) with Diag.Fatal _ -> ()); List.sort compare (List.map (fun (_, _, t) -> t) !Lexer.comments) in
    let before = comments_of src in
    match Fmt.format ~file:("fmt-" ^ file) src with
    | exception Diag.Fatal d -> fmt_failures := (name, "formatting failed: " ^ d.message) :: !fmt_failures
    | out -> (
      match Parser.parse_file ("fmt-out-" ^ file) out with
      | exception Diag.Fatal d -> fmt_failures := (name, "output does not parse: " ^ d.message ^ "\n" ^ out) :: !fmt_failures
      | f2 ->
        if Astdump.file (Parser.parse_file ("fmt-in-" ^ file) src) <> Astdump.file f2 then
          fmt_failures := (name, "tree changed:\n" ^ out) :: !fmt_failures
        else if Fmt.format ~file:("fmt2-" ^ file) out <> out then
          fmt_failures := (name, "not idempotent:\n" ^ out ^ "---\n" ^ Fmt.format ~file:("fmt3-" ^ file) out) :: !fmt_failures
        else if comments_of out <> before then fmt_failures := (name, "comments changed:\n" ^ out) :: !fmt_failures)
  end

(* [modules]: other files (name, source) written next to the program, for `use`. *)
let compile ?(modules = []) name src =
  let file = name ^ ".tsr" in
  let file =
    if modules = [] then file
    else begin
      let dir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "tessera-mods-%d-%s" (Unix.getpid ()) name) in
      (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      List.iter (fun (m, s) -> Out_channel.with_open_bin (Filename.concat dir (m ^ ".tsr")) (fun oc -> output_string oc s)) modules;
      Filename.concat dir file
    end
  in
  match Parser.parse_file file src with
  | exception Diag.Fatal d ->
    (* a literal out of range (E0005) is valid syntax, so it is not a grammar rejection *)
    if d.code <> "E0005" then dump name src false;
    ([], [ d ], [])
  | _ -> (
    dump name src true;
    check_fmt name src ();
    Modules.load_and_check file src)

let codes ds = List.map (fun (d : Diag.t) -> d.code) ds

(* [feed]: a shell command whose output becomes the program's stdin, through
   a pipe, so input can arrive late. *)
let with_stdin feed f =
  match feed with
  | None -> f ()
  | Some cmd ->
    let r, w = Unix.pipe ~cloexec:true () in
    let pid = Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |] Unix.stdin w Unix.stderr in
    Unix.close w;
    let saved = Unix.dup Unix.stdin in
    Unix.dup2 r Unix.stdin;
    Unix.close r;
    Fun.protect f ~finally:(fun () ->
        Unix.dup2 saved Unix.stdin;
        Unix.close saved;
        ignore (Unix.waitpid [] pid))

let run ?(grants = Interp.no_grants) ?modules ?feed name src =
  let items, ds, _ = compile ?modules name src in
  if Diag.errors ds <> [] then Error ("diagnostics: " ^ String.concat ", " (List.map (fun (d : Diag.t) -> d.code ^ " " ^ d.message) ds))
  else
    let buf = Buffer.create 64 in
    match with_stdin feed (fun () -> Interp.run ~out:(Buffer.add_string buf) ~grants items) with
    | Ok v -> Ok (Buffer.contents buf, v)
    | Error (m, _) -> Error ("panic: " ^ m)

(* ---------- native backend: same program, compiled to C ---------- *)

let tmpdir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "tessera-native-%d" (Unix.getpid ()))
let () = try Unix.mkdir tmpdir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
let native_enabled = Sys.getenv_opt "TESSERA_SKIP_NATIVE" = None

(* TESSERA_NATIVE_ONLY=a,b runs the native half only for tests whose name starts with a or b. *)
let native_for name =
  native_enabled
  && match Sys.getenv_opt "TESSERA_NATIVE_ONLY" with
     | None | Some "" -> true
     | Some l -> List.exists (fun p -> String.starts_with ~prefix:p name) (String.split_on_char ',' l)

(* AddressSanitizer catches use-after-free and double free in the reference
   counting; TESSERA_LEAKCHECK reports objects still alive at exit. *)
let sanitize = [ "-O1"; "-g"; "-fsanitize=address,undefined"; "-fno-sanitize-recover=all" ]

let read_file p = In_channel.with_open_bin p In_channel.input_all

(* ---------- running native builds in parallel ----------

   Compiling with a sanitizer and running the result is most of the suite's
   time, and each program is independent (sockets use port 0), so they run as
   background jobs, TESSERA_JOBS at a time (default: one per core). The C source
   is generated here, in order; the job compiles it and runs the binary, and a
   callback checks the result when it ends. [drain] waits for all of them
   before the summary.

   Compiled binaries are cached by a digest of the compiler, its flags and the
   C source, in $TESSERA_CACHE/test-native (default ~/.cache/tessera), so a run that
   changes no generated C compiles nothing. TESSERA_TEST_CACHE=0 turns it off.
   Entries unused for a week are removed. *)

let jobs_max =
  match Option.bind (Sys.getenv_opt "TESSERA_JOBS") int_of_string_opt with
  | Some n when n > 0 -> n
  | _ -> (
    try
      let ic = Unix.open_process_in "getconf _NPROCESSORS_ONLN 2>/dev/null || nproc 2>/dev/null" in
      let n = Option.bind (In_channel.input_line ic) (fun l -> int_of_string_opt (String.trim l)) in
      ignore (Unix.close_process_in ic);
      match n with Some n when n > 0 -> n | _ -> 4
    with _ -> 4)

let running : (int * (int -> unit)) list ref = ref []

(* TESSERA_TEST_SLOW=1 lists the slowest jobs at the end. *)
let job_times : (float * string) list ref = ref []
let job_started : (int, float * string) Hashtbl.t = Hashtbl.create 64

(* Wait for one job to end and run its callback with the exit code. *)
let reap () =
  let rec poll () =
    let done_ =
      List.find_map
        (fun (pid, k) ->
          match Unix.waitpid [ Unix.WNOHANG ] pid with
          | 0, _ -> None
          | _, Unix.WEXITED c -> Some (pid, k, c)
          | _, (Unix.WSIGNALED s | Unix.WSTOPPED s) -> Some (pid, k, 128 + abs s)
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> None)
        !running
    in
    match done_ with
    | Some (pid, k, c) ->
      running := List.filter (fun (p, _) -> p <> pid) !running;
      (match Hashtbl.find_opt job_started pid with
       | Some (t0, what) -> job_times := (Unix.gettimeofday () -. t0, what) :: !job_times; Hashtbl.remove job_started pid
       | None -> ());
      k c
    | None -> Unix.sleepf 0.002; poll ()
  in
  poll ()

let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0

(* Run shell command [cmd] as a job; [k] gets its exit status. *)
let spawn cmd k =
  while List.length !running >= jobs_max do reap () done;
  let pid = Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |] devnull Unix.stdout Unix.stderr in
  Hashtbl.replace job_started pid (Unix.gettimeofday (), cmd);
  running := (pid, k) :: !running

let drain () =
  while !running <> [] do reap () done;
  if Sys.getenv_opt "TESSERA_TEST_SLOW" <> None then
    List.iteri
      (fun i (t, cmd) ->
        if i < 25 then
          let exe = match String.split_on_char ';' cmd with _ :: _ -> (match List.rev (String.split_on_char '/' (List.hd (List.rev (String.split_on_char ';' cmd)))) with x :: _ -> x | [] -> cmd) | [] -> cmd in
          Printf.eprintf "%6.2fs %s\n" t (String.sub exe 0 (min 80 (String.length exe))))
      (List.sort (fun (a, _) (b, _) -> compare b a) !job_times)

let cache_dir =
  if Sys.getenv_opt "TESSERA_TEST_CACHE" = Some "0" then None
  else
    let base =
      match Sys.getenv_opt "TESSERA_CACHE" with
      | Some d when d <> "" -> Some d
      | _ -> Option.map (fun h -> Filename.concat (Filename.concat h ".cache") "tessera") (Sys.getenv_opt "HOME")
    in
    match base with
    | None -> None
    | Some base ->
      let d = Filename.concat base "test-native" in
      (try ignore (Sys.command (sprintf "mkdir -p %s" (Filename.quote d))) with _ -> ());
      if Sys.file_exists d then Some d else None

(* The compiler's identity, so a new compiler does not reuse old binaries. *)
let cc_version =
  lazy
    (try
       let ic = Unix.open_process_in (Native.cc () ^ " --version 2>/dev/null") in
       let v = In_channel.input_all ic in
       ignore (Unix.close_process_in ic);
       v
     with _ -> "")

let () =
  (* forget binaries nobody used for a week *)
  match cache_dir with
  | Some d -> ignore (Sys.command (sprintf "find %s -type f -mtime +7 -delete 2>/dev/null" (Filename.quote d)))
  | None -> ()

(* Build [items] into [exe] with [flags] and then run [run_cmd] (which
   redirects the program's output itself), in the background. [k] gets
   Ok (exit status) or Error (compiler output). *)
(* A fresh path in tmpdir for a binary of test [name]: jobs run at the same
   time, so two runs of one name must not share files. *)
let job_count = ref 0
let exe_path name = incr job_count; Filename.concat tmpdir (sprintf "%s-%d" name !job_count)

let native_job ?tests ~flags items exe run_cmd (k : (int, string) result -> unit) =
  let c_text, cflags, libs = Native.prepare (Native.c_source ?tests items) in
  let libs = libs @ Native.link_flags (Ffi.libs items) in
  let c_file = exe ^ ".c" in
  Out_channel.with_open_bin c_file (fun oc -> output_string oc c_text);
  let argv = Native.command ~flags ~cflags ~libs c_file exe in
  let log = exe ^ ".cc" in
  let q = Filename.quote in
  let compile = String.concat " " (List.map q argv) ^ " >" ^ q log ^ " 2>&1" in
  let build =
    match cache_dir with
    | None -> compile
    | Some d ->
      (* the paths differ from run to run; the command around them does not *)
      let shape = Native.command ~flags ~cflags ~libs "prog.c" "prog" in
      (* and where linked libraries are found: a binary that links one (the FFI
         tests' own) records its path, which differs between checkouts *)
      let lib_env = List.map (fun v -> v ^ "=" ^ Option.value ~default:"" (Sys.getenv_opt v)) [ "LIBRARY_PATH"; "LD_LIBRARY_PATH" ] in
      let key = Digest.to_hex (Digest.string (String.concat "\000" ((Lazy.force cc_version :: shape) @ lib_env @ [ c_text ]))) in
      let cached = Filename.concat d key in
      (* a hit is copied (and touched, so it stays); a miss is compiled and stored *)
      sprintf "if [ -f %s ]; then touch %s && cp %s %s; else %s && cp %s %s.$$ && mv -f %s.$$ %s; fi" (q cached) (q cached) (q cached) (q exe)
        compile (q exe) (q cached) (q cached) (q cached)
  in
  let failed = exe ^ ".ccfailed" in
  (try Sys.remove failed with Sys_error _ -> ());
  spawn (sprintf "( %s ) || { : > %s; exit 1; }; %s" build (q failed) run_cmd) (fun code ->
      if Sys.file_exists failed then k (Error (try read_file log with Sys_error _ -> "")) else k (Ok code))

let grant_args (grants : Interp.grants) =
  List.concat_map (fun h -> [ "--grant"; "net=" ^ h ]) grants.net
  @ List.concat_map (fun h -> [ "--grant"; "listen=" ^ h ]) grants.listen
  @ List.concat_map (fun n -> [ "--grant"; "env=" ^ n ]) grants.env
  @ List.concat_map (fun n -> [ "--grant"; "exec=" ^ n ]) grants.exec
  @ List.concat_map (fun n -> [ "--grant"; "foreign=" ^ n ]) grants.foreign
  @ (match grants.seed with Some s -> [ "--seed"; string_of_int s ] | None -> [])
  @ (match grants.fs with Some (d, ro) -> [ "--grant"; "fs=" ^ d ^ if ro then ":ro" else "" ] | None -> [])

(* With [feed], stdin comes from that shell command, and the program runs on
   one worker thread, so a task that blocked its thread would stall the rest.
   [k] gets Ok (exit status, stdout, stderr) or Error. *)
let run_native ?(grants = Interp.no_grants) ?modules ?feed name src k =
  let items, ds, _ = compile ?modules name src in
  if Diag.errors ds <> [] then k (Error "does not compile") else
  let exe = exe_path name in
  let args = grant_args grants @ ("--" :: grants.Interp.args) in
  let o = exe ^ ".out" and e = exe ^ ".err" in
  let cmd = String.concat " " (List.map Filename.quote (exe :: args)) ^ sprintf " >%s 2>%s" (Filename.quote o) (Filename.quote e) in
  let env = "ASAN_OPTIONS=detect_leaks=0 TESSERA_LEAKCHECK=1 " in
  let run_cmd = match feed with None -> env ^ cmd | Some f -> sprintf "(%s) | %sTESSERA_THREADS=1 %s" f env cmd in
  native_job ~flags:sanitize items exe run_cmd (function
    | Error m -> k (Error ("C compile failed:\n" ^ m))
    | Ok code -> k (Ok (code, read_file o, read_file e)))

let check_native name ?grants ?modules ?feed src ~want_out ~want_err_prefix =
  if native_for name then
    run_native ?grants ?modules ?feed name src (function
      | Error m -> ok (name ^ "/native") false m
      | Ok (code, o, e) -> (
        ok (name ^ "/native") (o = want_out) (sprintf "stdout %S, want %S (stderr %S)" o want_out e);
        match want_err_prefix with
        | None ->
          ok (name ^ "/native-clean")
            (code = 0 && e = "tessera-leakcheck: 0 live\n")
            (sprintf "exit %d, stderr %S" code e)
        | Some p -> ok (name ^ "/native-panic") (String.starts_with ~prefix:p e) (sprintf "stderr %S" e)))

(* Concurrency: the same program again under ThreadSanitizer. *)
let tsan = [ "-O1"; "-g"; "-fsanitize=thread" ]

let check_tsan name ?(grants = Interp.no_grants) ?feed src want =
  if native_for name && selected name then begin
    let items, ds, _ = compile name src in
    let exe = exe_path (name ^ "-tsan") in
    if Diag.errors ds <> [] then ok (name ^ "/tsan") false "does not compile"
    else begin
      let args =
        (match grants.Interp.fs with Some (d, ro) -> [ "--grant"; "fs=" ^ d ^ if ro then ":ro" else "" ] | None -> [])
        @ List.concat_map (fun h -> [ "--grant"; "net=" ^ h ]) grants.Interp.net
        @ List.concat_map (fun h -> [ "--grant"; "listen=" ^ h ]) grants.Interp.listen
        @ List.concat_map (fun h -> [ "--grant"; "exec=" ^ h ]) grants.Interp.exec
        @ List.concat_map (fun h -> [ "--grant"; "foreign=" ^ h ]) grants.Interp.foreign
        @ ("--" :: grants.Interp.args)
      in
      let o = exe ^ ".out" and e = exe ^ ".err" in
      let cmd =
        String.concat " " (List.map Filename.quote (exe :: args)) ^ sprintf " >%s 2>%s" (Filename.quote o) (Filename.quote e)
      in
      let env = "TSAN_OPTIONS=halt_on_error=1 TESSERA_THREADS=8 TESSERA_LEAKCHECK=1 " in
      let run_cmd = match feed with None -> env ^ cmd | Some f -> sprintf "(%s) | %s%s" f env cmd in
      native_job ~flags:tsan items exe run_cmd (function
        | Error m -> ok (name ^ "/tsan") false ("C compile failed:\n" ^ m)
        | Ok code ->
          let o = read_file o and e = read_file e in
          ok (name ^ "/tsan")
            (code = 0 && o = want && e = "tessera-leakcheck: 0 live\n")
            (sprintf "exit %d, stdout %S, stderr %S" code o e))
    end
  end

let out name ?grants ?modules ?feed src want =
  if selected name then begin
  (match run ?grants ?modules ?feed name src with
   | Ok (got, _) -> ok name (got = want) (sprintf "got %S, want %S" got want)
   | Error e -> ok name false e);
  check_native name ?grants ?modules ?feed src ~want_out:want ~want_err_prefix:None
  end

let errs ?modules name src want =
  if selected name then
  let _, ds, _ = compile ?modules name src in
  ok name (codes ds = want) (sprintf "got [%s], want [%s]" (String.concat " " (codes ds)) (String.concat " " want))

let panics ?grants name src want =
  if selected name then begin
  (match run ?grants name src with
   | Error e -> ok name (e = "panic: " ^ want) (sprintf "got %S" e)
   | Ok (o, _) -> ok name false ("no panic, output " ^ o));
  check_native name ?grants src ~want_out:"" ~want_err_prefix:(Some ("panic: " ^ want ^ "\n"))
  end

(* A program that chooses its exit status: the same stdout and status from
   both backends, and nothing leaked natively. *)
let exits name ?grants src want_out want_code =
  (match run ?grants name src with
   | Ok (got, v) ->
     let code = match v with Interp.VInt n | Interp.VEnum ("Result", "Ok", [ Interp.VInt n ]) -> Interp.exit_code n | Interp.VEnum ("Result", "Err", _) -> 1 | _ -> 0 in
     ok name (got = want_out && code = want_code) (sprintf "got %S exit %d, want %S exit %d" got code want_out want_code)
   | Error e -> ok name false e);
  if native_for name then
    run_native ?grants name src (function
      | Error m -> ok (name ^ "/native") false m
      | Ok (code, o, e) ->
        ok (name ^ "/native")
          (o = want_out && code = want_code
           && (String.ends_with ~suffix:"tessera-leakcheck: 0 live\n" e || String.starts_with ~prefix:"error: " e))
          (sprintf "stdout %S exit %d stderr %S, want %S exit %d" o code e want_out want_code))

(* Apply every fix's edits and return the new source. *)
let apply_fixes src (ds : Diag.t list) =
  let edits = List.concat_map (fun (d : Diag.t) -> match d.fix with Some f -> f.edits | None -> []) ds in
  let edits = List.sort (fun (a : Diag.edit) b -> compare b.at.lo a.at.lo) edits in
  List.fold_left
    (fun s (e : Diag.edit) ->
      String.sub s 0 e.at.lo ^ e.replacement ^ String.sub s e.at.hi (String.length s - e.at.hi))
    src edits

(* TLS tests trust only the test CA in test/tls, in this process (the
   interpreter) and in the native programs it starts. *)
let tls_dir =
  let d = Filename.concat (Sys.getcwd ()) "tls" in
  if Sys.file_exists (Filename.concat d "ca.pem") then d else Filename.concat (Sys.getcwd ()) "test/tls"
let () = Unix.putenv "SSL_CERT_FILE" (Filename.concat tls_dir "ca.pem")

(* builds made through the CLI keep their binaries in a cache of their own,
   not the user's, removed at the end *)
let test_cache = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "tessera-test-cache-%d" (Unix.getpid ()))
let () =
  Unix.putenv "TESSERA_CACHE" test_cache;
  at_exit (fun () -> ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; test_cache ])))

let main_wrap body = "fn main(root: Root) {\n    let out = root.stdout\n" ^ body ^ "\n}\n"

let () =
  (* ---------- running programs ---------- *)
  out "hello" "fn main(root: Root) { root.stdout.print(\"hi\") }" "hi\n";
  out "arith" (main_wrap "    out.print(\"{1 + 2 * 3} {7 / 2} {7 % 3} {2.5 * 2.0} {-(4)}\")") "7 3 1 5.0 -4\n";
  out "strings"
    (main_wrap "    let s = \"a,b,,c\"\n    out.print(s.split(\",\").join(\"|\"))\n    out.print(\" x \".trim().to_upper() + \"!\")")
    "a|b||c\nX!\n";
  out "newline-ends-expr"
    (main_wrap "    let x = 1\n    -2\n    out.print(\"{x}\")")
    "1\n";
  out "method-chain-across-lines"
    (main_wrap "    let n = [1, 2, 3]\n        .map(|x| x * 2)\n        .sum()\n    out.print(\"{n}\")")
    "12\n";
  out "closures"
    (main_wrap
       "    let xs = [1, 2, 3, 4, 5]\n    let evens = xs.filter(|x| x % 2 == 0)\n    out.print(\"{evens} {xs.fold(0, |a, b| a + b)} {xs.map(|x| x.to_str()).join(\"-\")}\")")
    "[2, 4] 15 1-2-3-4-5\n";
  out "loops"
    (main_wrap
       "    let mut xs = []\n    let mut i = 0\n    while true {\n        i = i + 1\n        if i > 6 { break }\n        if i % 2 == 0 { continue }\n        xs.push(i)\n    }\n    for x in xs { out.write(\"{x};\") }\n    out.print(\"\")")
    "1;3;5;\n";
  out "value-semantics"
    (main_wrap
       "    let mut a = [1]\n    let b = a\n    a.push(2)\n    let mut n = 10\n    let f = || n + 1\n    n = 100\n    out.print(\"{a.len()} {b.len()} {f()}\")")
    "2 1 11\n";
  out "structs"
    ("struct P { x: Int, y: Int }\nfn shift(p: P) -> P { let mut q = p\n q.x = q.x + 10\n q }\n"
    ^ main_wrap "    let p = P { x: 1, y: 2 }\n    let q = shift(p)\n    out.print(\"{p} {q.x}\")")
    "P { x: 1, y: 2 } 11\n";
  out "enums-match-generics"
    ("enum T { Leaf(Int), Pair(Int, Int), Empty }\nfn sum(t: T) -> Int { match t { Leaf(n) => n, Pair(a, b) => a + b, Empty => 0 } }\nfn pick[A](c: Bool, a: A, b: A) -> A { if c { a } else { b } }\n"
    ^ main_wrap "    let ts = [Leaf(3), T.Pair(1, 2), Empty]\n    out.print(\"{ts.map(sum)} {pick(false, \"a\", \"b\")}\")")
    "[3, 3, 0] b\n";
  out "nested-option-match"
    ("fn f(x: Option[Option[Int]]) -> Int { match x { Some(Some(n)) => n, Some(None) => -1, None => 0 } }\n"
    ^ main_wrap "    out.print(\"{f(Some(Some(5)))} {f(Some(None))} {f(None)}\")")
    "5 -1 0\n";
  out "try-operator"
    ("fn half(n: Int) -> Result[Int, Str] { if n % 2 == 0 { Ok(n / 2) } else { Err(\"odd: {n}\") } }\n\
      fn quarter(n: Int) -> Result[Int, Str] { let h = half(n)?\n half(h) }\n"
    ^ main_wrap "    out.print(\"{quarter(8)} {quarter(6)}\")")
    "Ok(2) Err(\"odd: 3\")\n";
  out "string-match"
    (main_wrap "    let r = match \"b\" { \"a\" => 1, \"b\" => 2, _ => 3 }\n    out.print(\"{r}\")")
    "2\n";
  out "early-return"
    ("fn find(xs: List[Int], t: Int) -> Int { let mut i = 0\n for x in xs { if x == t { return i }\n i = i + 1 }\n -1 }\n"
    ^ main_wrap "    out.print(\"{find([5, 6, 7], 7)} {find([1], 9)}\")")
    "2 -1\n";
  (* ownership stress: each also runs natively under ASan with a leak check *)
  (* a list made inside a loop (here by a fused filter) is not hoisted out of it *)
  out "hoist-list-declared-in-loop"
    {v|fn main(root: Root) {
    let mut i = 0
    let mut n = 0
    while i < 3 {
        if i > 0 {
            n = n + "abcd".bytes().filter(|c| c > 97 + i).len()
        }
        i = i + 1
    }
    root.stdout.print("{n}")
}
|v}
    "3\n";
  out "recursive-enum"
    ({|enum Tree { Leaf, Node(Tree, Int, Tree) }
fn insert(t: Tree, v: Int) -> Tree {
    match t {
        Leaf => Node(Leaf, v, Leaf),
        Node(l, x, r) => if v < x { Node(insert(l, v), x, r) } else { Node(l, x, insert(r, v)) },
    }
}
fn walk(t: Tree) -> List[Int] {
    match t {
        Leaf => [],
        Node(l, x, r) => {
            let mut out = walk(l)
            out.push(x)
            for y in walk(r) { out.push(y) }
            out
        },
    }
}
|}
    ^ main_wrap {|    let mut t = Leaf
    for v in [5, 2, 8, 1, 9, 3] { t = insert(t, v) }
    out.print("{walk(t)} {t == t}")|})
    "[1, 2, 3, 5, 8, 9] true\n";
  out "alias-then-mutate"
    (main_wrap {|    let mut a = [[1], [2]]
    let b = a
    a[0].push(10)
    a[1] = [7]
    let c = a[0]
    a[0].push(11)
    out.print("{a} {b} {c}")|})
    "[[1, 10, 11], [7]] [[1], [2]] [1, 10]\n";
  out "struct-in-list-mutation"
    ("struct C { name: Str, n: Int, tags: List[Str] }\n"
    ^ main_wrap {|    let mut cs = [C { name: "a", n: 1, tags: [] }, C { name: "b", n: 2, tags: ["x"] }]
    let snap = cs
    cs[1].n = cs[1].n + 40
    cs[0].tags.push("new")
    cs[0].name = cs[0].name + "!"
    out.print("{cs}")
    out.print("{snap[0]}")|})
    "[C { name: \"a!\", n: 1, tags: [\"new\"] }, C { name: \"b\", n: 42, tags: [\"x\"] }]\nC { name: \"a\", n: 1, tags: [] }\n";
  out "closures-capture-owned"
    (main_wrap {|    let words = ["a", "bb", "ccc"]
    let prefix = "<"
    let wrap = |w: Str| prefix + w + ">"
    let lens = words.map(|w| w.len())
    let wrapped = words.map(wrap)
    let adders = [1, 2, 3].map(|k| |x: Int| x + k)
    let applied = adders.map(|f| f(10))
    out.print("{lens} {wrapped} {applied} {words}")|})
    "[1, 2, 3] [\"<a>\", \"<bb>\", \"<ccc>\"] [11, 12, 13] [\"a\", \"bb\", \"ccc\"]\n";
  out "break-return-with-owned"
    ({|fn first_long(ws: List[Str]) -> Option[Str] {
    for w in ws {
        let up = w.to_upper()
        if up.len() > 2 { return Some(up) }
    }
    None
}
|}
    ^ main_wrap {|    let mut acc = []
    for w in ["x", "yy", "zzz", "w"] {
        let s = w + w
        if s.len() > 5 { break }
        acc.push(s)
    }
    out.print("{acc} {first_long(["a", "bcd"])} {first_long([])}")|})
    "[\"xx\", \"yyyy\"] Some(\"BCD\") None\n";
  out "option-result-helpers"
    ({|fn parse(s: Str) -> Result[Int, Str] { match s.parse_int() { Some(n) => Ok(n), None => Err("bad: " + s) } }
|}
    ^ main_wrap {|    let rs = ["1", "x", " 42 ", "-7", "0x10"].map(parse)
    let oks = rs.filter(|r| r.is_ok()).map(|r| r ?? 0)
    let errs = rs.filter(|r| r.is_err()).map(|r| r.map_error(|e| e.to_upper()))
    let o = Some("hi").map(|s| s + "!")
    out.print("{oks} {errs} {o} {None ?? 5}")|})
    "[1, 42, -7] [Err(\"BAD: X\"), Err(\"BAD: 0X10\")] Some(\"hi!\") 5\n";
  out "list-builtins"
    (main_wrap {|    let xs = [3, 1, 2, 3]
    let ss = ["pear", "apple", "fig"]
    out.print("{xs.sort()} {xs.reverse()} {xs.take(2)} {xs.skip(3)} {xs.skip(9)} {xs.contains(2)} {xs.get(-1)} {xs.last()}")
    out.print("{ss.sort()} {[2.5, -1.0, 0.0].sort()} {xs.sum()} {ss.join("+")} {ss.first()}")|})
    "[1, 2, 3, 3] [3, 2, 1, 3] [3, 1] [3] [] true None Some(3)\n[\"apple\", \"fig\", \"pear\"] [-1.0, 0.0, 2.5] 9 pear+apple+fig Some(\"pear\")\n";
  out "string-builtins"
    (main_wrap {|    let s = "  Hello, World\r\nsecond line\n\nlast"
    out.print("{s.lines()} {s.trim().split(", ")} {"abc".split("")}")
    out.print("{s.contains("World")} {"aXbXc".replace("X", "--")} {"q\"t"} {["tab\t"]}")|})
    "[\"  Hello, World\", \"second line\", \"\", \"last\"] [\"Hello\", \"World\\r\\nsecond line\\n\\nlast\"] [\"a\", \"b\", \"c\"]\ntrue a--b--c q\"t [\"tab\\t\"]\n";
  out "numbers"
    (main_wrap {|    out.print("{7 / -2} {-7 % 3} {2.0 / 4.0} {1.0 / 3.0} {(2.7).to_int()} {(-3).abs()} {10.to_float() * 1.5} {0.1 + 0.2}")|})
    "-3 -1 0.5 0.333333333333333 2 3 15.0 0.3\n";
  out "generic-instances"
    ({|struct Pair[A, B] { a: A, b: B }
fn swap[A, B](p: Pair[A, B]) -> Pair[B, A] { Pair { a: p.b, b: p.a } }
fn twice[T](x: T, f: fn(T) -> T) -> T { f(f(x)) }
|}
    ^ main_wrap {|    let p = swap(Pair { a: 1, b: "one" })
    let q = swap(Pair { a: [1.5], b: true })
    out.print("{p} {q} {twice(3, |x| x * x)} {twice("ab", |s| s + s)}")|})
    "Pair { a: \"one\", b: 1 } Pair { a: true, b: [1.5] } 81 abababab\n";
  out "versioned-loops"
    (main_wrap {|    let mut xs = [0, 0, 0, 0, 0, 0]
    let mut m = 1
    while m <= 5 {
        xs[m] = xs[m] + m
        m = m + 2
    }
    let mut ys = []
    let mut i = 0
    while i < 4 {
        ys.push(7)
        i = i + 1
    }
    let mut k = 0
    let mut hits = 0
    while k < 6 {
        k = k + 1
        if k < 6 { hits = hits + xs[k] }
    }
    out.print("{xs} {ys} {i} {hits}")|})
    "[0, 1, 0, 3, 0, 5] [7, 7, 7, 7] 4 9\n";
  panics "index-after-increment"
    (main_wrap {|    let mut xs = [1, 2, 3]
    let mut m = 0
    while m < 3 {
        m = m + 1
        xs[m] = 0
    }|})
    "index 3 is out of range for a list of length 3";
  out "fold-strings-and-reassign"
    (main_wrap {|    let mut s = ""
    let mut i = 0
    while i < 5 {
        s = s + i.to_str()
        i = i + 1
    }
    let joined = ["a", "b", "c"].fold("", |acc, x| acc + x + ".")
    out.print("{s} {joined}")|})
    "01234 a.b.c.\n";
  out "fusion-keeps-effect-order"
    (main_wrap {|    let xs = [1, 2]
    let ys = xs.map(|x| { out.write("m{x} ")
 x * 10 }).filter(|x| { out.write("f{x} ")
 true })
    let pure = xs.map(|x| x + 1).filter(|x| x > 2).map(|x| x * 100).fold(0, |a, b| a + b)
    out.print("{ys} {pure}")|})
    "m1 m2 f10 f20 [10, 20] 300\n";
  out "closure-with-try-not-inlined"
    ({|fn halves(xs: List[Int]) -> List[Result[Int, Str]] {
    xs.map(|x| {
        let h = if x % 2 == 0 { Ok(x / 2) } else { Err("odd") }
        let v = h?
        Ok(v + 1)
    })
}
|}
    ^ main_wrap {|    out.print("{halves([2, 3, 8])}")|})
    "[Ok(2), Err(\"odd\"), Ok(5)]\n";
  (* ---------- structured concurrency ---------- *)
  let par name ?grants src want =
    out name ?grants src want;
    check_tsan name ?grants src want
  in
  par "group-basic"
    ({|fn work(n: Int) -> Int {
    let mut s = 0
    let mut i = 0
    while i < n {
        s = s + i * i % 7
        i = i + 1
    }
    s
}
|}
    ^ main_wrap {|    let words = ["a", "bb", "ccc"]
    let rs = group g {
        for k in [1000, 2000, 3000] { g.go(|| work(k) + words.len()) }
        g.go(|| 0)
    }
    out.print("{rs}")|})
    "[2004, 4002, 6002, 0]\n";
  par "group-shared-captures"
    ({|struct Item { name: Str, tags: List[Str] }
enum Shape { Dot, Box(Float, Float) }
fn area(s: Shape) -> Float { match s { Dot => 0.0, Box(w, h) => w * h } }
|}
    ^ main_wrap {|    let big = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    let label = "item"
    let item = Item { name: "x", tags: ["t1", "t2"] }
    let scale = |x: Int| x * 3
    let shapes = [Box(1.0, 2.0), Dot, Box(3.0, 0.5)]
    let rs = group g {
        let mut i = 0
        while i < 2000 {
            let k = i
            g.go(|| {
                let total = big.map(scale).fold(0, |a, b| a + b) + k
                let names = item.tags.map(|t| label + "-" + t + "-" + item.name)
                let a = shapes.map(area).fold(0.0, |x, y| x + y)
                let mut tags = names.take(k % 3)
                tags.push("{a}")
                Item { name: "{total}", tags: tags }
            })
            i = i + 1
        }
    }
    out.print("{rs.len()} {rs[0]} {rs[1999]}")
    out.print("{item} {big.len()}")|})
    "2000 Item { name: \"165\", tags: [\"3.5\"] } Item { name: \"2164\", tags: [\"item-t1-x\", \"3.5\"] }\nItem { name: \"x\", tags: [\"t1\", \"t2\"] } 10\n";
  par "group-nested"
    ({|fn sum_range(lo: Int, hi: Int) -> Int {
    if hi - lo <= 1000 {
        let mut s = 0
        let mut i = lo
        while i < hi { s = s + i
 i = i + 1 }
        s
    } else {
        let mid = (lo + hi) / 2
        let parts = group g {
            g.go(|| sum_range(lo, mid))
            g.go(|| sum_range(mid, hi))
        }
        parts[0] + parts[1]
    }
}
|}
    ^ main_wrap {|    out.print("{sum_range(0, 200000)}")|})
    "19999900000\n";
  par "group-empty-and-results"
    (main_wrap {|    let none = group g { }
    let strs = group g { for w in ["x", "y"] { g.go(|| w + w) } }
    let opts = group g { g.go(|| Some([1]))
        g.go(|| None) }
    out.print("{none.len()} {strs} {opts}")|})
    "0 [\"xx\", \"yy\"] [Some([1]), None]\n";
  (* ---------- channels ---------- *)
  par "chan-producer-consumer"
    (main_wrap {|    let c: Chan[Int] = channel(2)
    let done: Chan[Str] = channel(1)
    let rs = group g {
        g.go(|| {
            let mut sum = 0
            let mut more = true
            while more {
                match c.recv() {
                    Some(v) => { sum = sum + v },
                    None => { more = false },
                }
            }
            done.send("consumer saw {sum}")
            sum
        })
        g.go(|| {
            for i in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] { c.send(i) }
            c.close()
            0
        })
    }
    out.print("{rs} {done.recv()}")|})
    "[55, 0] Some(\"consumer saw 55\")\n";
  par "chan-fan-in"
    (main_wrap {|    let c: Chan[List[Str]] = channel(1)
    let rs = group g {
        g.go(|| {
            let parts = group h {
                for k in [1, 2, 3] {
                    h.go(|| {
                        for i in [1, 2, 3] { c.send(["{k}", "{i}"]) }
                        k
                    })
                }
            }
            c.close()
            parts.sum()
        })
        g.go(|| {
            let mut got = []
            let mut more = true
            while more {
                match c.recv() {
                    Some(pair) => { got.push(pair.join("-")) },
                    None => { more = false },
                }
            }
            got.len()
        })
    }
    out.print("{rs}")|})
    "[6, 9]\n";
  par "chan-many-tasks"
    (main_wrap {|    let c: Chan[Int] = channel(1)
    let total = group g {
        g.go(|| {
            let mut s = 0
            let mut n = 0
            while n < 1000 {
                s = s + c.recv() ?? 0
                n = n + 1
            }
            s
        })
        let mut i = 0
        while i < 1000 {
            let k = i
            g.go(|| { c.send(k)
 0 })
            i = i + 1
        }
    }
    out.print("{total.sum()}")|})
    "499500\n";
  (* ---------- sockets: an echo server and 50 clients in one program ---------- *)
  let sock_grants = { Interp.no_grants with net = [ "127.0.0.1" ]; listen = [ "127.0.0.1" ] } in
  par "socket-echo" ~grants:sock_grants
    ({|fn echo(c: Conn) -> Int {
    let mut total = 0
    let mut more = true
    while more {
        match c.read() {
            Ok(s) => {
                if s.is_empty() {
                    more = false
                } else {
                    total = total + s.len()
                    c.write(s) ?? ()
                }
            },
            Err(_) => { more = false },
        }
    }
    c.close()
    total
}

fn client(net: Net, port: Int, k: Int) -> Int {
    match net.connect("127.0.0.1:{port}") {
        Ok(c) => {
            let msg = "hello {k}"
            c.write(msg) ?? ()
            let got = c.read() ?? "?"
            c.close()
            if got == msg { 1 } else { 0 }
        },
        Err(_) => 0,
    }
}
|}
    ^ main_wrap {|    match root.listen.on("127.0.0.1:0") {
        Ok(l) => {
            let port = l.port()
            let net = root.net.only("127.0.0.1")
            let n = 50
            let rs = group g {
                g.go(|| {
                    let served = group h {
                        let mut i = 0
                        while i < n {
                            match l.accept() {
                                Ok(c) => h.go(|| echo(c)),
                                Err(_) => {},
                            }
                            i = i + 1
                        }
                    }
                    served.sum()
                })
                let mut k = 0
                while k < n {
                    let j = k
                    g.go(|| client(net, port, j))
                    k = k + 1
                }
            }
            l.close()
            out.print("bytes {rs[0]}, echoes {rs.skip(1).sum()}")
        },
        Err(e) => out.print("{e}"),
    }|})
    "bytes 390, echoes 50\n";
  par "socket-close-while-reading" ~grants:sock_grants
    (main_wrap {|    match root.listen.on("127.0.0.1:0") {
        Ok(l) => {
            let port = l.port()
            let net = root.net.only("127.0.0.1")
            let rs = group g {
                g.go(|| {
                    match l.accept() {
                        Ok(c) => {
                            let done = group h {
                                h.go(|| "{c.read()}")
                                h.go(|| {
                                    c.close()
                                    "closed"
                                })
                            }
                            done.join(", ")
                        },
                        Err(e) => "{e}",
                    }
                })
                g.go(|| {
                    match net.connect("127.0.0.1:{port}") {
                        Ok(c) => {
                            let r = "{c.read()}"
                            c.close()
                            r
                        },
                        Err(e) => "{e}",
                    }
                })
            }
            out.print(rs.join(" | "))
        },
        Err(e) => out.print("{e}"),
    }|})
    "Err(Failed(\"connection is closed\")), closed | Ok(\"\")\n";
  par "socket-read-and-write-together" ~grants:sock_grants
    ({|fn echo(c: Conn) -> Int {
    let mut n = 0
    let mut more = true
    while more {
        match c.read() {
            Ok(s) => {
                if s.is_empty() {
                    more = false
                } else {
                    n = n + s.len()
                    c.write(s) ?? ()
                }
            },
            Err(_) => {
                more = false
            },
        }
    }
    c.close()
    n
}
|}
    ^ main_wrap {|    match root.listen.on("127.0.0.1:0") {
        Ok(l) => {
            let port = l.port()
            let net = root.net.only("127.0.0.1")
            let rs = group g {
                g.go(|| {
                    match l.accept() {
                        Ok(c) => echo(c),
                        Err(_) => 0,
                    }
                })
                g.go(|| {
                    match net.connect("127.0.0.1:{port}") {
                        Ok(c) => {
                            let parts = group h {
                                h.go(|| {
                                    let mut got = 0
                                    while got < 30 {
                                        match c.read() {
                                            Ok(s) => {
                                                got = got + s.len()
                                            },
                                            Err(_) => {
                                                got = 1000
                                            },
                                        }
                                    }
                                    got
                                })
                                h.go(|| {
                                    for m in ["0123456789", "abcdefghij", "ABCDEFGHIJ"] {
                                        c.write(m) ?? ()
                                    }
                                    0
                                })
                            }
                            c.close()
                            parts[0]
                        },
                        Err(_) => 0,
                    }
                })
            }
            out.print("{rs}")
        },
        Err(e) => out.print("{e}"),
    }|})
    "[30, 30]\n";
  out "socket-denied"
    (main_wrap {|    out.print("{root.listen.on("127.0.0.1:9").is_err()} {root.net.connect("127.0.0.1:9").is_err()}")
    match root.listen.on("127.0.0.1:9") { Ok(_) => out.print("?"), Err(e) => out.print("{e}") }
    match root.net.connect("127.0.0.1:9") { Ok(_) => out.print("?"), Err(e) => out.print("{e}") }|})
    "true true\nDenied(\"listen: 127.0.0.1:9 is not granted\")\nDenied(\"net: 127.0.0.1:9 is not granted\")\n";
  panics "chan-deadlock" (main_wrap "    let c: Chan[Int] = channel(1)\n    c.send(1)\n    c.send(2)") "all tasks are blocked (deadlock)";
  panics "chan-send-closed" (main_wrap "    let c: Chan[Int] = channel(1)\n    c.close()\n    c.send(1)") "send on a closed channel";
  panics "chan-capacity" (main_wrap "    let c: Chan[Int] = channel(-1)") "channel capacity cannot be negative";
  errs "group-handle-assign" (main_wrap "    let r = group g { g = 1 }") [ "E0310" ];
  errs "chan-in-struct" "struct S { c: Chan[Int] }\nfn main(root: Root) {}" [ "E0305" ];
  panics "div-zero" (main_wrap "    let z = 0\n    out.print(\"{1 / z}\")") "division by zero";
  panics "index-oob" (main_wrap "    let xs = [1]\n    out.print(\"{xs[3]}\")") "index 3 is out of range for a list of length 1";
  panics "panic-fn" (main_wrap "    panic(\"boom\")") "boom";

  (* ---------- exit codes ---------- *)
  exits "exit-int" "fn main(root: Root) -> Int {\n    root.stdout.print(\"bye\")\n    3\n}\n" "bye\n" 3;
  exits "exit-zero" "fn main() -> Int { 0 }" "" 0;
  exits "exit-low-bits" "fn main() -> Int { 256 + 7 }" "" 7;
  exits "exit-result-ok"
    "fn half(n: Int) -> Result[Int, Str] { if n % 2 == 0 { Ok(n / 2) } else { Err(\"odd\") } }\nfn main(root: Root) -> Result[Int, Str] {\n    let h = half(root.args().len() + 4)?\n    Ok(h)\n}\n"
    "" 2;
  exits "exit-result-err"
    "fn main(root: Root) -> Result[Int, Str] {\n    let words = [\"a\"]\n    if words.len() == 1 { return Err(\"no\") }\n    Ok(4)\n}\n" "" 1;
  exits "exit-after-group"
    "fn main(root: Root) -> Int {\n    let r = group g {\n        g.go(|| 40)\n        g.go(|| 2)\n    }\n    root.stdout.print(\"{r.sum()}\")\n    r.sum() - 30\n}\n" "42\n" 12;
  errs "main-returns-str" "fn main(root: Root) -> Str { \"x\" }" [ "E0303" ];
  errs "main-returns-result-str" "fn main(root: Root) -> Result[Str, Str] { Ok(\"x\") }" [ "E0303" ];

  (* ---------- Exec: running other programs ---------- *)
  let sh = { Interp.no_grants with exec = [ "sh" ] } in
  out "exec-run" ~grants:sh
    (main_wrap {|    match root.exec.run("sh", ["-c", "echo hi $0; echo oops >&2; exit 7", "a b; rm -rf x"]) {
        Ok(r) => out.print("{r.code} {r.stdout.trim()} {r.stderr.trim()} {r}"),
        Err(e) => out.print("{e}"),
    }|})
    "7 hi a b; rm -rf x oops ProcessOutput { code: 7, stdout: \"hi a b; rm -rf x\\n\", stderr: \"oops\\n\" }\n";
  out "exec-big-output" ~grants:sh
    (main_wrap {|    let r = root.exec.run("sh", ["-c", "i=0; while [ $i -lt 3000 ]; do echo line $i; echo err $i >&2; i=$((i+1)); done"])
    match r {
        Ok(p) => out.print("{p.stdout.lines().len()} {p.stderr.lines().len()} {p.code}"),
        Err(e) => out.print("{e}"),
    }|})
    "3000 3000 0\n";
  out "exec-denied" ~grants:sh
    (main_wrap "    out.print(\"{root.exec.run(\"ls\", [])}\")\n    out.print(\"{root.exec.only(\"ls\").run(\"sh\", [])}\")")
    "Err(Denied(\"exec: `ls` is not granted\"))\nErr(Denied(\"exec: `sh` is not granted\"))\n";
  out "exec-not-granted" (main_wrap "    out.print(\"{root.exec.run(\"sh\", [\"-c\", \"true\"])}\")")
    "Err(Denied(\"exec: `sh` is not granted\"))\n";
  out "exec-missing" ~grants:{ Interp.no_grants with exec = [ "*" ] }
    (main_wrap "    out.print(\"{root.exec.run(\"tessera-no-such-program\", [])}\")")
    "Err(Failed(\"exec: cannot run `tessera-no-such-program`: No such file or directory\"))\n";
  out "exec-signal" ~grants:sh
    (main_wrap "    out.print(\"{root.exec.run(\"sh\", [\"-c\", \"kill -9 $$\"]).map_error(|e| 0)}\")")
    "Ok(ProcessOutput { code: 137, stdout: \"\", stderr: \"\" })\n";
  out "exec-nul" ~grants:sh
    (main_wrap "    out.print(\"{root.exec.run(\"sh\", [\"a\" + 0.to_char()])}\")")
    "Err(Failed(\"exec: an argument contains a NUL byte\"))\n";
  let exec_group = {|fn main(root: Root) {
    let x = root.exec
    let codes = group g {
        for i in [1, 2, 3, 4, 5, 6] {
            g.go(|| match x.run("sh", ["-c", "echo task $0; exit $0", i.to_str()]) {
                Ok(r) => r.code * 10 + r.stdout.len(),
                Err(_) => -1,
            })
        }
    }
    root.stdout.print("{codes}")
}
|} in
  out "exec-group" ~grants:sh exec_group "[17, 27, 37, 47, 57, 67]\n";
  check_tsan "exec-group" ~grants:sh exec_group "[17, 27, 37, 47, 57, 67]\n";

  (* ---------- foreign functions (C libraries) ---------- *)
  let libc = { Interp.no_grants with foreign = [ "c" ] } in
  let cdecls = {|foreign "c" {
    fn strlen(s: Str) -> Int
    fn abs(n: Int32) -> Int32
    fn atof(s: Str) -> Float
    fn ldexp(x: Float, e: Int32) -> Float
    fn strtol(s: Str, end: Out[Handle], base: Int32) -> Int
    fn strtod(s: Str, end: Out[Handle]) -> Float
    fn strchr(s: Str, c: Int32) -> Str
    fn isdigit(c: Int32) -> Bool
    fn getenv(name: Str) -> Handle
    fn memcmp(a: Bytes, b: Bytes, n: Int) -> Int32
    fn labs(n: Int) -> Int
}
|} in
  out "ffi-libc" ~grants:libc
    (cdecls ^ {|
fn main(root: Root) {
    let out = root.stdout
    let c = root.foreign
    out.print("{strlen(c, "hello")} {abs(c, -5)} {labs(c, -9000000000)} {atof(c, "2.5")} {ldexp(c, 1.5, 3)}")
    let (n, end) = strtol(c, "42xyz", 10)
    out.print("{n} {c.copy(end, 3).to_str()}")
    let (x, rest) = strtod(c, "1.25e2!")
    out.print("{x} {c.copy(rest, 1).to_str()}")
    out.print("{strchr(c, "hello", 108)} [{strchr(c, "hello", 122)}] {isdigit(c, 55)} {isdigit(c, 65)}")
    out.print("{getenv(c, "TESSERA_NO_SUCH_VARIABLE").addr == 0}")
    let b = bytes([1, 0, 7, 9])
    out.print("{memcmp(c, b, bytes([1, 0, 7, 8]), 4) > 0} {memcmp(c, b, b, 4)} {c.copy(end, 0).len()}")
    out.print("{strlen(c, "")} {c.allows("c")} {c.allows("sqlite3")}")
}
|})
    "5 5 9000000000 2.5 12.0\n42 xyz\n125.0 !\nllo [] true false\ntrue\ntrue 0 0\n0 true false\n";
  (* the Foreign value names the libraries it may call *)
  panics "ffi-denied"
    (cdecls ^ "\nfn main(root: Root) {\n    root.stdout.print(\"{strlen(root.foreign, \"x\")}\")\n}\n")
    "foreign: library `c` is not granted (grant it with --grant foreign=c)";
  panics "ffi-only" ~grants:{ Interp.no_grants with foreign = [ "*" ] }
    (cdecls
     ^ "\nfn main(root: Root) {\n    let all = root.foreign\n    let some = all.only(\"sqlite3\")\n    root.stdout.print(\"{strlen(some, \"x\")}\")\n}\n")
    "foreign: library `c` is not granted (grant it with --grant foreign=c)";
  out "ffi-only-print" ~grants:{ Interp.no_grants with foreign = [ "*" ] }
    (cdecls
     ^ "\nfn main(root: Root) {\n    let all = root.foreign\n    let some = all.only(\"sqlite3\")\n    root.stdout.print(\"{all.allows(\"c\")} {some.allows(\"c\")} {some.allows(\"sqlite3\")} {all.only(\"*\").allows(\"zlib\")} {some.only(\"*\").allows(\"sqlite3\")} {strlen(all, \"abc\")}\")\n}\n")
    "true false true true false 3\n";
  panics "ffi-int32-range" ~grants:libc
    (cdecls ^ "\nfn main(root: Root) {\n    root.stdout.print(\"{abs(root.foreign, 3000000000)}\")\n}\n")
    "foreign: 3000000000 does not fit in Int32 (argument `n` of `abs`)";
  panics "ffi-copy-ungranted"
    (cdecls ^ "\nfn main(root: Root) {\n    root.stdout.print(\"{root.foreign.copy(Handle { addr: 1 }, 1)}\")\n}\n")
    "foreign: no library is granted, so C memory cannot be read";
  panics "ffi-copy-null" ~grants:libc
    (cdecls ^ "\nfn main(root: Root) {\n    root.stdout.print(\"{root.foreign.copy(Handle { addr: 0 }, 1)}\")\n}\n")
    "foreign.copy: null pointer";
  (* a panic in a foreign call is caught like any other, and frees what it should *)
  out "ffi-catch" ~grants:libc
    (cdecls ^ "\nfn main(root: Root) {\n    let f = root.foreign\n    let s = \"abc\" + \"def\"\n    root.stdout.print(\"{catch(|| abs(f.only(\"zlib\"), -1))} {strlen(f, s)}\")\n}\n")
    "Err(\"foreign: library `c` is not granted (grant it with --grant foreign=c)\") 6\n";
  (* foreign calls from many tasks at once *)
  let ffi_group = cdecls ^ {|
fn main(root: Root) {
    let f = root.foreign
    let lens = group g {
        for i in 0..<8 {
            g.go(|| strlen(f, "abcdefgh".slice(0, i)) + abs(f, -i))
        }
    }
    root.stdout.print("{lens}")
}
|} in
  out "ffi-group" ~grants:libc ffi_group "[0, 2, 4, 6, 8, 10, 12, 14]\n";
  check_tsan "ffi-group" ~grants:libc ffi_group "[0, 2, 4, 6, 8, 10, 12, 14]\n";
  (* declaration errors *)
  errs "ffi-bad-types"
    "foreign \"c\" {\n    fn a(x: List[Int]) -> Int\n    fn b(x: Out[Str])\n    fn c2() -> Bytes\n    fn d(x: Int) -> Option[Int]\n}\n\nfn main() {}\n"
    [ "E0330"; "E0330"; "E0330"; "E0330" ];
  errs "ffi-too-many"
    "foreign \"c\" {\n    fn f(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int)\n}\n\nfn main() {}\n" [ "E0330" ];
  errs "ffi-bad-lib" "foreign \"-lfoo\" {\n    fn f()\n}\n\nfn main() {}\n" [ "E0330" ];
  errs "ffi-as-value"
    "foreign \"c\" {\n    fn strlen(s: Str) -> Int\n}\n\nfn main(root: Root) {\n    let f = strlen\n    let g = |s: Str| strlen(root.foreign, s)\n    root.stdout.print(\"{g(\"ab\")}\")\n}\n"
    [ "E0331" ];
  errs "ffi-needs-cap"
    "foreign \"c\" {\n    fn strlen(s: Str) -> Int\n}\n\nfn main(root: Root) {\n    root.stdout.print(\"{strlen(\"ab\")}\")\n}\n" [ "E0107" ];
  errs "ffi-wrong-cap"
    "foreign \"c\" {\n    fn strlen(s: Str) -> Int\n}\n\nfn main(root: Root) {\n    root.stdout.print(\"{strlen(root.fs, \"ab\")}\")\n}\n" [ "E0201" ];
  errs "ffi-body" "foreign \"c\" {\n    fn f() -> Int { 1 }\n}\n\nfn main() {}\n" [ "E0003" ];
  errs "ffi-generic" "foreign \"c\" {\n    fn f[T](x: T)\n}\n\nfn main() {}\n" [ "E0003" ];
  (* only trusted standard library modules call C without Foreign *)
  errs "ffi-runtime-lib" "foreign \"tessera\" {\n    fn tessera_copy(p: Handle, n: Int) -> Bytes\n}\n\nfn main() {}\n" [ "E0330" ];
  errs "ffi-sql-private" "use sql\n\nfn main() {\n    let x = sql.sqlite3_errstr(1)\n}\n" [ "E0402" ];
  errs "ffi-same-line" "foreign \"c\" {\n    fn a() fn b()\n}\n\nfn main() {}\n" [ "E0003" ];
  errs "ffi-main" "foreign \"c\" {\n    fn main()\n}\n" [ "E0303"; "E0330" ];
  (* a missing symbol is an error before the program runs (natively, a link error) *)
  if selected "ffi-missing" then (match run ~grants:libc "ffi-missing" "foreign \"c\" {\n    fn tessera_no_such_function() -> Int\n}\n\nfn main(root: Root) {\n    root.stdout.print(\"{tessera_no_such_function(root.foreign)}\")\n}\n" with
   | Error e -> ok "ffi-missing" (e = "panic: foreign: library `c` has no function `tessera_no_such_function`") e
   | Ok _ -> ok "ffi-missing" false "ran");
  if selected "ffi-no-lib" then (match run ~grants:{ Interp.no_grants with foreign = [ "*" ] } "ffi-no-lib" "foreign \"tessera_no_such_lib\" {\n    fn f() -> Int\n}\n\nfn main(root: Root) {\n    root.stdout.print(\"{f(root.foreign)}\")\n}\n" with
   | Error e -> ok "ffi-no-lib" (String.starts_with ~prefix:"panic: foreign: cannot load library `tessera_no_such_lib`" e) e
   | Ok _ -> ok "ffi-no-lib" false "ran");
  (* a library the grants do not cover is not even loaded: the call panics first *)
  if selected "ffi-no-lib-ungranted" then (match run ~grants:libc "ffi-no-lib-ungranted" "foreign \"tessera_no_such_lib\" {\n    fn f() -> Int\n}\n\nfn main(root: Root) {\n    root.stdout.print(\"before\")\n    root.stdout.print(\"{f(root.foreign)}\")\n}\n" with
   | Error e -> ok "ffi-no-lib-ungranted" (e = "panic: foreign: library `tessera_no_such_lib` is not granted (grant it with --grant foreign=tessera_no_such_lib)") e
   | Ok _ -> ok "ffi-no-lib-ungranted" false "ran");
  if native_for "ffi-missing" && selected "ffi-missing" then
    run_native ~grants:libc "ffi-missing-native" "foreign \"c\" {\n    fn tessera_no_such_function() -> Int\n}\n\nfn main(root: Root) {\n    root.stdout.print(\"{tessera_no_such_function(root.foreign)}\")\n}\n" (function
     | Error m -> ok "ffi-missing/native" (Interp_util.find_sub m "tessera_no_such_function" 0 <> None) m
     | Ok _ -> ok "ffi-missing/native" false "linked");
  (* variadic functions: each declaration fixes the types it passes after `...` *)
  let vdecls = {|foreign "c" {
    fn malloc(n: Int) -> Handle
    fn free(p: Handle)
    fn snprintf(buf: Handle, size: Int, format: Str, ..., n: Int32, x: Float, s: Str, on: Bool) -> Int32
    fn format_int(buf: Handle, size: Int, format: Str, ..., n: Int) -> Int32 = "snprintf"
    fn format8(buf: Handle, size: Int, format: Str, ..., a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int) -> Int32 = "snprintf"
    fn format_floats(buf: Handle, size: Int, format: Str, ..., a: Float, b: Float, c: Float, d: Float, e: Float, f: Float, g: Float, h: Float) -> Int32 = "snprintf"
    fn format_none(buf: Handle, size: Int, format: Str, ...) -> Int32 = "snprintf"
}
|} in
  out "ffi-variadic" ~grants:libc
    (vdecls ^ {|
fn main(root: Root) {
    let f = root.foreign
    let buf = malloc(f, 128)
    let n = snprintf(f, buf, 128, "%d|%.3f|%s|%d", -42, 2.5, "text", true)
    root.stdout.print("{n} {f.copy(buf, n).to_str()}")
    let m = format_int(f, buf, 128, "<%lld>", 9000000000)
    root.stdout.print("{f.copy(buf, m).to_str()}")
    let k = format8(f, buf, 128, "%lld %lld %lld %lld %lld %lld %lld %lld", 1, 2, 3, 4, 5, 6, 7, -8)
    root.stdout.print("{f.copy(buf, k).to_str()}")
    let j = format_floats(f, buf, 128, "%g %g %g %g %g %g %g %g", 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, -7.5)
    root.stdout.print("{f.copy(buf, j).to_str()}")
    let e = format_none(f, buf, 128, "plain")
    root.stdout.print("{f.copy(buf, e).to_str()}")
    free(f, buf)
}
|})
    "16 -42|2.500|text|1\n<9000000000>\n1 2 3 4 5 6 7 -8\n0.5 1.5 2.5 3.5 4.5 5.5 6.5 -7.5\nplain\n";
  panics "ffi-variadic-int32" ~grants:libc
    (vdecls ^ "\nfn main(root: Root) {\n    let f = root.foreign\n    let buf = malloc(f, 16)\n    snprintf(f, buf, 16, \"%d\", 5000000000, 1.0, \"\", false);\n}\n")
    "foreign: 5000000000 does not fit in Int32 (argument `n` of `snprintf`)";
  (* structs by value: libc's div and lldiv return one *)
  out "ffi-struct-libc" ~grants:libc
    {|foreign "c" {
    struct Div { quot: Int32, rem: Int32 }
    pub struct LongDiv { quot: Int, rem: Int }
    fn div(a: Int32, b: Int32) -> Div
    fn lldiv(a: Int, b: Int) -> LongDiv
}

fn main(root: Root) {
    let f = root.foreign
    let d = div(f, 17, -5)
    let l = lldiv(f, 9000000000, 7)
    root.stdout.print("{d} {d.quot * 10 + d.rem} {l}")
}
|}
    "Div { quot: -3, rem: 2 } -28 LongDiv { quot: 1285714285, rem: 5 }\n";
  (* callbacks: qsort calls a top-level function, which gets the call's Foreign first *)
  let qdecls = {|foreign "c" {
    fn malloc(n: Int) -> Handle
    fn free(p: Handle)
    fn memcpy(dst: Handle, src: Bytes, n: Int) -> Handle
    fn qsort(base: Handle, n: Int, size: Int, cmp: fn(Handle, Handle) -> Int32)
}

fn by_byte(f: Foreign, a: Handle, b: Handle) -> Int {
    f.copy(a, 1)[0] - f.copy(b, 1)[0]
}

fn too_big(a: Handle, b: Handle) -> Int {
    3000000000
}
|} in
  out "ffi-qsort" ~grants:libc
    (qdecls ^ {|
fn main(root: Root) {
    let f = root.foreign
    let data = bytes([5, 3, 9, 1, 7, 200, 0])
    let buf = malloc(f, data.len())
    memcpy(f, buf, data, data.len())
    qsort(f, buf, data.len(), 1, by_byte)
    root.stdout.print("{f.copy(buf, data.len())}")
    free(f, buf)
}
|})
    "bytes([0, 1, 3, 5, 7, 9, 200])\n";
  panics "ffi-callback-int32" ~grants:libc
    (qdecls ^ "\nfn main(root: Root) {\n    let f = root.foreign\n    let buf = malloc(f, 2)\n    memcpy(f, buf, bytes([2, 1]), 2)\n    qsort(f, buf, 2, 1, too_big)\n}\n")
    "in a callback from C: foreign: 3000000000 does not fit in Int32 (the result of callback `too_big`)";
  (* callbacks from many tasks at once, each on its own task's thread *)
  let qsort_group = qdecls ^ {|
fn main(root: Root) {
    let f = root.foreign
    let sorted = group g {
        for i in 0..<8 {
            g.go(|| {
                let data = bytes([9, i, 4, 7 - i, 1])
                let buf = malloc(f, 5)
                memcpy(f, buf, data, 5)
                qsort(f, buf, 5, 1, by_byte)
                let r = f.copy(buf, 5)
                free(f, buf)
                r[0] + r[4] * 100
            })
        }
    }
    root.stdout.print("{sorted}")
}
|} in
  out "ffi-callback-group" ~grants:libc qsort_group "[900, 901, 901, 901, 901, 901, 901, 900]\n";
  check_tsan "ffi-callback-group" ~grants:libc qsort_group "[900, 901, 901, 901, 901, 901, 901, 900]\n";
  (* structs of every shape the calling conventions treat differently, and
     callbacks, against a small C library (test/ffi_test.c) built here *)
  (* next to the test binary, not in $TMPDIR: dune gives each run a temporary
     directory of its own, which can be gone while binaries still point at it *)
  let vt_dir = Filename.concat (Filename.dirname Sys.executable_name) "tessera-ffi-test" in
  (try Unix.mkdir vt_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let vt_src = let d = Filename.concat (Sys.getcwd ()) "ffi_test.c" in if Sys.file_exists d then d else Filename.concat (Sys.getcwd ()) "test/ffi_test.c" in
  let macos = Sys.file_exists "/System/Library/CoreServices" in
  let vt_lib = Filename.concat vt_dir (if macos then "libtesseratest.dylib" else "libtesseratest.so") in
  let built =
    Sys.command
      (String.concat " "
         (List.map Filename.quote
            ([ Native.cc (); "-shared"; "-fPIC"; "-O1"; "-o"; vt_lib; vt_src ] @ if macos then [ "-install_name"; vt_lib ] else []))
      ^ " 2>&1")
    = 0
  in
  ok "ffi-test-lib" built "cannot build test/ffi_test.c";
  let add_path var = Unix.putenv var (vt_dir ^ match Sys.getenv_opt var with Some p when p <> "" -> ":" ^ p | _ -> "") in
  add_path "LIBRARY_PATH";
  if not macos then add_path "LD_LIBRARY_PATH";
  (match Interp.Foreign.dl_open vt_lib with h, _ when h <> 0n -> Hashtbl.replace Interp.Foreign.libs "tesseratest" h | _ -> ());
  let vt = { Interp.no_grants with foreign = [ "tesseratest" ] } in
  let vtdecls = {|foreign "tesseratest" {
    struct P2 { a: Int32, b: Int32 }
    struct T3 { a: Int32, b: Int32, c: Int32 }
    struct V2 { x: Float, y: Float }
    struct V3 { x: Float, y: Float, z: Float }
    struct V4 { a: Float, b: Float, c: Float, d: Float }
    struct NX { n: Int, x: Float }
    struct XK { x: Float, k: Int32 }
    struct Big { a: Int32, b: Int, on: Bool, h: Handle, x: Float }

    fn vt_p2_swap(p: P2) -> P2
    fn vt_t3_rot(t: T3) -> T3
    fn vt_v2_add(a: V2, b: V2) -> V2
    fn vt_v3_scale(v: V3, k: Float) -> V3
    fn vt_v4(x: Float) -> V4
    fn vt_nx(a: NX, m: Int) -> NX
    fn vt_xk(a: XK) -> XK
    fn vt_big(b: Big, d: Int32) -> Big
    fn vt_tight(a: Int, b: Int, c: Int, d: Int, e: Int, t: T3) -> Int
    fn vt_spill(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, n: NX, z: Float) -> Float
    fn vt_pack(a: Int, b: Int, c: Int, d: Int, p: XK, q: XK, h1: Int32, h2: Int32) -> Int
    fn vt_bool(on: Bool) -> Int32
    fn vt_apply(f: fn(Float, Int) -> Float, x: Float, n: Int) -> Float
    fn vt_each(f: fn(Str, Int) -> Int, n: Int) -> Int
    fn vt_call8(f: fn(Int, Int, Int, Int, Int, Int, Int, Int) -> Int) -> Int
    fn vt_mixed(f: fn(Int32, Float, Int32, Float, Handle) -> Int) -> Int
    fn vt_note(n: Int)
    fn vt_noted() -> Int
    fn vt_times(f: fn(Int) -> (), n: Int)
}

fn poly(x: Float, n: Int) -> Float {
    if n > 2 { x * x } else { x }
}

fn nested(f: Foreign, x: Float, n: Int) -> Float {
    vt_apply(f, poly, x, n)
}

fn weigh(name: Str, i: Int) -> Int {
    name.len() * 10 + i
}

fn sum8(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int) -> Int {
    a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f + 7 * g + 8 * h
}

fn mixed(a: Int, x: Float, b: Int, y: Float, h: Handle) -> Int {
    a * 100 + b * 10 + h.addr + if x + y == 0.75 { 1000 } else { 0 }
}

fn note_square(f: Foreign, n: Int) {
    vt_note(f, n * n)
}
|} in
  out "ffi-struct-shapes" ~grants:vt
    (vtdecls ^ {|
fn main(root: Root) {
    let f = root.foreign
    let out = root.stdout
    out.print("{vt_p2_swap(f, P2 { a: 1, b: -2 })} {vt_t3_rot(f, T3 { a: 1, b: 2, c: 3 })}")
    out.print("{vt_v2_add(f, V2 { x: 1.5, y: 2.0 }, V2 { x: 0.25, y: -1.0 })} {vt_v3_scale(f, V3 { x: 1.0, y: 2.0, z: 3.0 }, 2.0)}")
    out.print("{vt_v4(f, 1.5)} {vt_nx(f, NX { n: 3, x: 0.5 }, 4)} {vt_xk(f, XK { x: 2.5, k: 21 })}")
    out.print("{vt_big(f, Big { a: 1, b: 2, on: true, h: Handle { addr: 100 }, x: 9.0 }, 3)}")
    out.print("{vt_tight(f, 1, 2, 3, 4, 5, T3 { a: 6, b: 7, c: 8 })} {vt_spill(f, 1, 2, 3, 4, 5, 6, 7, NX { n: 2, x: 0.5 }, 3.0)}")
    out.print("{vt_pack(f, 1, 2, 3, 4, XK { x: 1.25, k: 5 }, XK { x: 2.75, k: 6 }, 7, 8)} {vt_bool(f, true)} {vt_bool(f, false)}")
}
|})
    ("P2 { a: -2, b: 1 } T3 { a: 2, b: 3, c: 1 }\nV2 { x: 1.75, y: 1.0 } V3 { x: 2.0, y: 4.0, z: 6.0 }\n"
     ^ "V4 { a: 1.5, b: 3.0, c: 4.5, d: 6.0 } NX { n: 12, x: 2.0 } XK { x: 3.5, k: 42 }\n"
     ^ "Big { a: 4, b: 6, on: false, h: Handle { addr: 103 }, x: 3.0 }\n87654321 30728.0\n807049 7 3\n");
  out "ffi-callbacks" ~grants:vt
    (vtdecls ^ {|
fn main(root: Root) {
    let f = root.foreign
    root.stdout.print("{vt_apply(f, poly, 2.0, 3)} {vt_apply(f, nested, 1.0, 3)} {vt_each(f, weigh, 4)}")
    vt_times(f, note_square, 4)
    root.stdout.print("{vt_call8(f, sum8)} {vt_mixed(f, mixed)} {vt_noted(f)}")
}
|})
    "13.0 18.0 156\n204 748 30\n";
  (* nothing unwinds through C: a panic in a callback ends the program, past `catch` *)
  panics "ffi-callback-panic" ~grants:vt
    (vtdecls ^ "\nfn crash(x: Float, n: Int) -> Float {\n    if n > 0 {\n        panic(\"boom\")\n    }\n    x\n}\n\nfn main(root: Root) {\n    let f = root.foreign\n    root.stdout.print(\"{catch(|| vt_apply(f, crash, 1.0, 1))}\")\n}\n")
    "in a callback from C: boom";
  panics "ffi-callback-wait" ~grants:vt
    (vtdecls ^ "\nfn waits(n: Int) {\n    let c: Chan[Int] = channel(0)\n    c.recv();\n}\n\nfn main(root: Root) {\n    vt_times(root.foreign, waits, 1)\n}\n")
    "a callback from C cannot wait (for a task, a channel, a timer or input)";
  (* a foreign struct from another module, named through it *)
  out "ffi-struct-module" ~grants:libc
    ~modules:[ ("cdiv", "foreign \"c\" {\n    pub struct Div { quot: Int32, rem: Int32 }\n    pub fn div(a: Int32, b: Int32) -> Div\n}\n\npub fn half(f: Foreign, n: Int) -> Div {\n    div(f, n, 2)\n}\n") ]
    "use cdiv\n\nfn show(d: cdiv.Div) -> Str {\n    \"{d.quot}r{d.rem}\"\n}\n\nfn main(root: Root) {\n    let f = root.foreign\n    root.stdout.print(\"{show(cdiv.half(f, 7))} {show(cdiv.div(f, 9, 4))} {cdiv.Div { quot: 1, rem: 0 }}\")\n}\n"
    "3r1 2r1 cdiv.Div { quot: 1, rem: 0 }\n";
  (* declaration errors *)
  errs "ffi-struct-bad-field" "foreign \"c\" {\n    struct S { name: Str, n: Int }\n    struct E {}\n}\n\nfn main() {}\n" [ "E0330"; "E0330" ];
  errs "ffi-not-c-struct" "struct Pt { x: Int }\n\nforeign \"c\" {\n    fn f(p: Pt)\n}\n\nfn main() {}\n" [ "E0330" ];
  errs "ffi-variadic-bad"
    "foreign \"c\" {\n    struct S { x: Int }\n    fn a(...)\n    fn b(x: Float, ...)\n    fn c(s: Str, ..., p: S)\n    fn d(s: Str, ..., o: Out[Int])\n    fn e(s: Str, ...) -> S\n}\n\nfn main() {}\n"
    [ "E0330"; "E0330"; "E0330"; "E0330"; "E0330" ];
  errs "ffi-variadic-twice" "foreign \"c\" {\n    fn a(s: Str, ..., ...)\n}\n\nfn main() {}\n" [ "E0003" ];
  errs "ffi-symbol-bad" "foreign \"c\" {\n    fn f() = \"not a name\"\n}\n\nfn main() {}\n" [ "E0330" ];
  errs "ffi-callback-bad-type" "foreign \"c\" {\n    fn f(cb: fn(Str) -> Str)\n    fn g(cb: fn(List[Int]) -> Int)\n    fn h() -> fn(Int) -> Int\n}\n\nfn main() {}\n"
    [ "E0330"; "E0330"; "E0330" ];
  errs "ffi-callback-closure"
    "foreign \"c\" {\n    fn qsort(base: Handle, n: Int, size: Int, cmp: fn(Handle, Handle) -> Int32)\n}\n\nfn cmp(a: Handle, b: Handle) -> Int {\n    0\n}\n\nfn main(root: Root) {\n    let k = 1\n    let g = cmp\n    qsort(root.foreign, Handle { addr: 0 }, 0, 1, |a, b| k)\n    qsort(root.foreign, Handle { addr: 0 }, 0, 1, g)\n}\n"
    [ "E0332"; "E0332" ];
  (* the child sees PATH and only the variables the Env grant names *)
  let env_src = main_wrap {|    match root.exec.run("sh", ["-c", "echo $\{HOME:+home\} $\{PATH:+path\} $\{TESSERA_SECRET_TEST:-none\}"]) {
        Ok(r) => out.write(r.stdout),
        Err(e) => out.print("{e}"),
    }|} in
  Unix.putenv "TESSERA_SECRET_TEST" "s3cret";
  out "exec-env-filtered" ~grants:sh env_src "path none\n";
  out "exec-env-granted" ~grants:{ sh with env = [ "HOME"; "TESSERA_SECRET_TEST" ] } env_src "home path s3cret\n";
  errs "exec-ambient" "fn main(root: Root) { system(\"ls\") }" [ "E0302" ];

  (* ---------- capabilities at runtime ---------- *)
  out "net-not-granted" (main_wrap "    out.print(\"{root.net.get(\"https://example.com/\")}\")")
    "Err(Denied(\"net: example.com:443 is not granted\"))\n";
  out "net-only-cannot-widen" ~grants:{ Interp.no_grants with net = [ "api.example.com" ] }
    (main_wrap "    let n = root.net.only(\"evil.example\")\n    out.print(\"{n.get(\"https://evil.example/\")}\")")
    "Err(Denied(\"net: evil.example:443 is not granted\"))\n";
  out "net-port-narrowing" ~grants:{ Interp.no_grants with net = [ "api.example.com:443" ] }
    (main_wrap "    let n = root.net.only(\"api.example.com\")\n    out.print(\"{n.get(\"http://api.example.com:443/\").is_err()}\")")
    "true\n";
  out "net-file-scheme" ~grants:{ Interp.no_grants with net = [ "*" ] }
    (main_wrap "    out.print(\"{root.net.get(\"file:///etc/passwd\")}\")")
    "Err(Failed(\"scheme `file` is not allowed, only http and https\"))\n";
  (* URL checks must not let a crafted URL reach a host other than the one allowed *)
  out "net-url-strict" ~grants:{ Interp.no_grants with net = [ "api.example.com" ] }
    (main_wrap {|    let mut pad = ""
    let mut i = 0
    while i < 600 {
        pad = pad + "a"
        i = i + 1
    }
    let urls = [
        "https://api.example.com" + pad + "@evil.example/",
        "https://user@api.example.com/",
        "http://api.example.com:+80/",
        "http://api.example.com:0x50/",
        "http://api.example.com:70000/",
        "http://api.example.com\\@evil.example/",
        "http://api.example.com /",
        "HTTPS://evil.example/",
    ]
    for u in urls {
        match root.net.get(u) {
            Ok(_) => out.print("ok?!"),
            Err(Denied(m)) => out.print("denied: {m}"),
            Err(Failed(m)) => out.print("failed: {m}"),
        }
    }|})
    "failed: URL host may only contain letters, digits, `.` and `-`\nfailed: URL host may only contain letters, digits, `.` and `-`\nfailed: URL host may only contain letters, digits, `.` and `-`\nfailed: bad port in URL\nfailed: bad port in URL\nfailed: URL host may only contain letters, digits, `.` and `-`\nfailed: URL contains spaces or non-ASCII characters\ndenied: net: evil.example:443 is not granted\n";
  (* Net.get speaks HTTP/1.1 itself: a server task answers with canned responses *)
  let http_src = {|fn serve(l: Listener, replies: List[Str]) -> Int {
    let mut n = 0
    for r in replies {
        match l.accept() {
            Ok(c) => {
                let mut req = ""
                while !req.contains("\r\n\r\n") {
                    let chunk = c.read() ?? ""
                    if chunk == "" {
                        break
                    }
                    req = req + chunk
                }
                let reply = if r == "echo" { "HTTP/1.1 200 OK\r\nContent-Length: {req.len()}\r\n\r\n{req}" } else { r }
                if c.write(reply).is_ok() {
                    n = n + 1
                }
                c.close()
            },
            Err(_) => (),
        }
    }
    n
}

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let l = root.listen.on("127.0.0.1:0")?
    let port = l.port()
    let net = root.net.only("127.0.0.1")
    let mut many = ""
    let mut big = ""
    let mut i = 0
    while i < 1001 {
        many = many + "X-N: {i}\r\n"
        big = big + "0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz"
        i = i + 1
    }
    let replies = [
        "echo",
        "echo",
        "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nX-A: 1\r\nx-a:  2 \r\nContent-Length: 5\r\n\r\nnope!extra",
        "HTTP/1.1 200 OK\r\nTransfer-Encoding: Chunked\r\n\r\n4;ext=1\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n",
        "HTTP/1.0 301 Moved\r\nLocation: http://elsewhere/\r\n\r\nuntil close",
        "garbage",
        "HTTP/1.1 200 OK\r\nContent-Length: 50\r\n\r\nshort",
        "HTTP/1.1 200 OK\r\nbad header\r\n\r\n",
        "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nzz\r\n",
        "HTTP/1.1 200 OK\r\n" + many + "\r\nok",
        "HTTP/1.1 200 OK\r\nX-Big: " + big + "\r\n\r\nok",
    ]
    let paths = ["/a/b?x=1#frag", "?q", "/", "/", "/", "/", "/", "/", "/", "/", "/"]
    let served = group g {
        g.go(|| serve(l, replies))
        g.go(|| {
            for p in paths {
                match net.get("http://127.0.0.1:{port}" + p) {
                    Ok(r) => {
                        let first = r.body.lines().first() ?? ""
                        out.print("{r.status} {r.headers.keys()} {r.headers.get("x-a")} {first}")
                    },
                    Err(e) => out.print("{e}"),
                }
            }
            0
        })
    }
    out.print("{served}")
    l.close()
    out.print("{net.get("https://127.0.0.1:{port}/")}")
    Ok(())
}
|} in
  let http_want =
    "200 [\"content-length\"] None GET /a/b?x=1 HTTP/1.1\n200 [\"content-length\"] None GET /?q HTTP/1.1\n404 [\"content-type\", \"x-a\", \"content-length\"] Some(\"1, 2\") nope!\n200 [\"transfer-encoding\"] None Wikipedia\n301 [\"location\"] None until close\nFailed(\"http: incomplete response headers\")\nFailed(\"http: truncated body\")\nFailed(\"http: malformed header line\")\nFailed(\"http: bad chunk size\")\nFailed(\"http: more than 1000 response headers\")\nFailed(\"http: response headers larger than 64 KiB\")\n[11, 0]\nErr(Failed(\"Connection refused\"))\n"
  in
  let http_grants = { Interp.no_grants with listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1" ] } in
  out "net-get-http" ~grants:http_grants http_src http_want;
  check_tsan "net-get-http" ~grants:http_grants http_src http_want;
  let dir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "tessera-test-%d" (Unix.getpid ())) in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (try Unix.mkdir (Filename.concat dir "sub") 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Out_channel.with_open_bin (Filename.concat dir "a.txt") (fun oc -> output_string oc "alpha");
  Out_channel.with_open_bin (Filename.concat dir "sub/b.txt") (fun oc -> output_string oc "beta");
  let real = Unix.realpath dir in
  let rw = { Interp.no_grants with fs = Some (real, false) } in
  let ro = { Interp.no_grants with fs = Some (real, true) } in
  out "fs-read" ~grants:ro (main_wrap "    out.print(\"{root.fs.read(\"a.txt\")}\")") "Ok(\"alpha\")\n";
  out "fs-full-path" ~grants:ro
    (main_wrap
       (sprintf "    let fs = root.fs\n    out.print(\"{fs.full_path(\"sub/new.db\") == Ok(%S + \"/sub/new.db\")} {fs.full_path(\"./a.txt\") == Ok(%S + \"/a.txt\")} {fs.is_read_only()}\")\n    out.print(\"{fs.full_path(\"../x\")} {fs.full_path(\"nodir/x\")} {fs.full_path(\"a\" + 0.to_char())}\")\n    out.print(\"{fs.under(\"sub\").full_path(\"b.txt\").is_ok()} {fs.under(\"..\").full_path(\"a.txt\")}\")"
          real real))
    "true true true\nErr(Denied(\"fs: path escapes the granted directory\")) Err(Failed(\"fs: directory of `nodir/x` does not exist\")) Err(Failed(\"fs: the path contains a NUL byte\"))\ntrue Err(Denied(\"fs: not granted (run with --grant fs=DIR)\"))\n";
  out "fs-rw-flag" ~grants:rw (main_wrap "    out.print(\"{root.fs.is_read_only()} {root.fs.read_only().is_read_only()}\")") "false true\n";
  out "fs-dotdot" ~grants:ro
    (main_wrap "    out.print(\"{root.fs.read(\"../a.txt\")}\")")
    "Err(Denied(\"fs: path escapes the granted directory\"))\n";
  out "fs-absolute" ~grants:ro
    (main_wrap "    out.print(\"{root.fs.read(\"/etc/passwd\").is_err()}\")")
    "true\n";
  out "fs-readonly" ~grants:ro
    (main_wrap "    out.print(\"{root.fs.write(\"c.txt\", \"x\")}\")")
    "Err(Denied(\"fs: this capability is read-only\"))\n";
  out "fs-attenuate" ~grants:rw
    (main_wrap
       "    let sub = root.fs.under(\"sub\").read_only()\n    out.print(\"{sub.read(\"b.txt\")} {sub.exists(\"../a.txt\")} {sub.write(\"x\", \"y\").is_err()}\")")
    "Ok(\"beta\") false true\n";
  out "fs-write" ~grants:rw
    (main_wrap "    let w = root.fs.write(\"c.txt\", \"gamma\")\n    out.print(\"{w} {root.fs.read(\"c.txt\")}\")")
    "Ok(()) Ok(\"gamma\")\n";
  par "group-fs-capability" ~grants:ro
    (main_wrap {|    let fs = root.fs
    let rs = group g {
        for name in ["a.txt", "sub/b.txt", "../x"] { g.go(|| fs.read(name).is_ok()) }
    }
    out.print("{rs}")|})
    "[true, true, false]\n";
  out "fs-not-granted" (main_wrap "    out.print(\"{root.fs.read(\"a.txt\").is_err()}\")") "true\n";
  if Sys.file_exists "/etc" then begin
    (try Unix.symlink "/etc" (Filename.concat dir "link") with Unix.Unix_error _ -> ());
    out "fs-symlink-escape" ~grants:ro
      (main_wrap "    out.print(\"{root.fs.read(\"link/hosts\")}\")")
      "Err(Denied(\"fs: path resolves outside the granted directory\"))\n"
  end;

  (* ---------- diagnostics ---------- *)
  errs "ambient-print" (main_wrap "    print(\"x\")") [ "E0302" ];
  errs "cap-in-struct" "struct C { n: Net }\nfn main(root: Root) {}" [ "E0301" ];
  errs "fn-in-struct" "struct C { f: fn(Int) -> Int }\nfn main(root: Root) {}" [ "E0304" ];
  errs "generic-cap-visible-ok" "struct Box[T] { v: T }\nfn f(b: Box[Net]) {}\nfn main(root: Root) {}" [];
  errs "bad-main" "fn main(x: Int) {}" [ "E0303" ];
  errs "immutable" (main_wrap "    let n = 1\n    n = 2") [ "E0205" ];
  errs "param-immutable" "fn f(n: Int) { n = 2 }\nfn main(root: Root) {}" [ "E0205" ];
  errs "closure-capture-assign" (main_wrap "    let mut n = 1\n    let f = || { n = 2 }") [ "E0205" ];
  errs "unknown-name" (main_wrap "    let count = 1\n    out.print(\"{cuont}\")") [ "E0101" ];
  errs "mismatch" "fn f() -> Int { \"no\" }\nfn main(root: Root) {}" [ "E0201" ];
  errs "try-in-unit" (main_wrap "    let x = root.fs.read(\"a\")?") [ "E0202" ];
  errs "try-error-mismatch"
    "fn a() -> Result[Int, Str] { Ok(1) }\nfn b() -> Result[Int, IoError] { let x = a()?\n Ok(x) }\nfn main(root: Root) {}"
    [ "E0202" ];
  errs "non-exhaustive" "enum L { R, G, B }\nfn f(l: L) -> Int { match l { R => 1, G => 2 } }\nfn main(root: Root) {}" [ "E0204" ];
  errs "non-exhaustive-nested"
    "fn f(x: Option[Option[Int]]) -> Int { match x { Some(Some(n)) => n, None => 0 } }\nfn main(root: Root) {}" [ "E0204" ];
  errs "if-no-else" "fn f() -> Int { if true { 1 } }\nfn main(root: Root) {}" [ "E0203" ];
  errs "missing-field" "struct P { x: Int, y: Int }\nfn f() -> P { P { x: 1 } }\nfn main(root: Root) {}" [ "E0206" ];
  errs "unknown-field" "struct P { x: Int }\nfn f(p: P) -> Int { p.z }\nfn main(root: Root) {}" [ "E0103" ];
  errs "unknown-method" (main_wrap "    let s = \"x\".lenght()") [ "E0105" ];
  errs "arity" "fn f(a: Int) -> Int { a }\nfn main(root: Root) { let x = f(1, 2) }" [ "E0107" ];
  errs "break-outside" (main_wrap "    break") [ "E0208" ];
  errs "same-line" (main_wrap "    let a = 1 let b = 2") [ "E0004" ];
  errs "param-needs-type" "fn f(a) {}" [ "E0008" ];
  errs "group-handle-escape" (main_wrap "    let r = group g { let h = g }") [ "E0310" ];
  errs "group-spawn-in-closure" (main_wrap "    let r = group g { g.go(|| { g.go(|| 1)\n 2 }) }") [ "E0310" ];
  errs "group-return" "fn f() -> Int { let r = group g { return 1 }\n 0 }\nfn main(root: Root) {}" [ "E0311" ];
  errs "group-break-outside" (main_wrap "    while true { let r = group g { break } }") [ "E0208" ];
  errs "push-immutable" (main_wrap "    let xs = [1]\n    xs.push(2)") [ "E0205" ];
  errs "for-over-str" (main_wrap "    for c in \"abc\" { }") [ "E0201" ];
  errs "unknown-type" "fn f(x: Strng) {}\nfn main(root: Root) {}" [ "E0102" ];

  (* ---------- machine-applicable fixes ---------- *)
  let fix_cases =
    [ ("fix-mut", main_wrap "    let n = 1\n    n = 2\n    out.print(\"{n}\")");
      ("fix-typo", main_wrap "    let count = 3\n    out.print(\"{cuont}\")");
      ("fix-arms", "enum L { R, G, B }\nfn f(l: L) -> Int {\n    match l {\n        R => 1,\n    }\n}\nfn main(root: Root) {}\n");
      ("fix-arms-inline", "enum L { R, G }\nfn f(l: L) -> Int { match l { R => 1 } }\nfn main(root: Root) {}\n") ]
  in
  List.iter
    (fun (name, src) ->
      let _, ds, _ = compile name src in
      ok (name ^ "-has-fix") (List.exists (fun (d : Diag.t) -> d.fix <> None) ds) "no fix offered";
      let fixed = apply_fixes src ds in
      let _, ds2, _ = compile (name ^ "-fixed") fixed in
      ok (name ^ "-applies") (ds2 = []) (sprintf "still [%s] after fix:\n%s" (String.concat " " (codes ds2)) fixed))
    fix_cases;

  (* ---------- authority report ---------- *)
  let _, _, infos =
    compile "authority"
      "fn pure(x: Int) -> Int { x }\nfn net(n: Net, u: Str) -> Bool { n.get(u).is_ok() }\nfn cb(f: fn(Int) -> Int) -> Int { f(1) }\nfn main(root: Root) {}"
  in
  let find n = List.find (fun (i : Check.fn_info) -> i.name = n) infos in
  ok "authority-pure" ((find "pure").caps = [] && not (find "pure").callbacks) "pure fn has authority";
  ok "authority-net" ((find "net").caps = [ Types.Net ]) "net fn missing Net";
  ok "authority-callback" (find "cb").callbacks "callback not detected";
  ok "authority-main" ((find "main").caps = [ Types.Root ]) "main missing Root";

  (* ---------- JSON output is well formed ---------- *)
  let _, ds, _ = compile "json" (main_wrap "    print(\"a \\\"quoted\\\"\\n\")") in
  let j = Diag.list_to_json ds in
  ok "json-shape" (String.starts_with ~prefix:"{\"diagnostics\":[{\"code\":\"E0302\"" j) j;

  (* ---------- tessera test ---------- *)
  let test_src =
    {|fn add(a: Int, b: Int) -> Int { a + b }

test "adds" {
    assert_eq(add(2, 3), 5)
    assert(add(-1, 1) == 0)
}

test "fails on a list" {
    assert_eq(["a", "b"], ["a", "b", "c"])
}

test "panics are failures" {
    let xs = [1]
    let x = xs[5]
}

test "channels" {
    let c: Chan[Int] = channel(1)
    let r = group g {
        g.go(|| {
            c.send(7)
            0
        })
        g.go(|| c.recv() ?? 0)
    }
    assert_eq(r, [0, 7])
}
|}
  in
  let items, ds, _ = compile "tessera-test" test_src in
  ok "tessera-test-checks" (ds = []) (String.concat "," (codes ds));
  let want =
    [ ("adds", None); ("fails on a list", Some "assertion failed: [\"a\", \"b\"] != [\"a\", \"b\", \"c\"]");
      ("panics are failures", Some "index 5 is out of range for a list of length 1"); ("channels", None) ]
  in
  let got = List.map (fun (n, r) -> (n, match r with Ok () -> None | Error (m, _) -> Some m)) (Interp.run_tests ~out:ignore items) in
  ok "tessera-test-interp" (got = want) (String.concat "; " (List.map (fun (n, m) -> n ^ "=" ^ Option.value ~default:"ok" m) got));
  if native_enabled then begin
    let exe = Filename.concat tmpdir "tessera-test-native" in
    match Native.build ~flags:sanitize ~tests:true items exe with
    | Error m -> ok "tessera-test-native" false m
    | Ok () ->
      let got =
        List.mapi
          (fun i (n, _) ->
            let e = exe ^ ".err" in
            let code = Sys.command (sprintf "ASAN_OPTIONS=detect_leaks=0 %s %d 2>%s" (Filename.quote exe) i (Filename.quote e)) in
            let first = match String.split_on_char '\n' (read_file e) with l :: _ -> l | [] -> "" in
            (n, if code = 0 then None else Some (if String.starts_with ~prefix:"panic: " first then String.sub first 7 (String.length first - 7) else first)))
          want
      in
      ok "tessera-test-native" (got = want) (String.concat "; " (List.map (fun (n, m) -> n ^ "=" ^ Option.value ~default:"ok" m) got))
  end;
  errs "tessera-test-duplicate" "test \"a\" {}\ntest \"a\" {}" [ "E0106" ];

  (* tests can ask for a fresh directory as `fs: Fs` and the real clock *)
  let cap_tests = {|fn save(fs: Fs, name: Str, n: Int) -> Result[(), IoError] {
    fs.write(name, "{n}")
}

test "saves a file" (fs: Fs) {
    assert_eq(fs.list(""), Ok([]))
    assert(save(fs, "a.txt", 42).is_ok())
    assert(fs.make_dir("sub").is_ok())
    assert(fs.write("sub/b.txt", "x").is_ok())
    assert_eq(fs.read("a.txt"), Ok("42"))
    assert(fs.read("../escape").is_err())
}

test "each test starts empty" (fs: Fs, clock: Clock) {
    assert_eq(fs.list(""), Ok([]))
    assert(clock.now_ms() > 1700000000000)
}

test "clock only" (c: Clock) {
    let a = c.now_ms()
    assert(c.now_ms() >= a)
}

test "fails with a dir" (fs: Fs) {
    assert(fs.write("x", "1").is_ok())
    assert_eq(fs.list(""), Ok([]))
}
|} in
  let items, ds, _ = compile "tessera-test-caps" cap_tests in
  ok "tessera-test-caps-checks" (ds = []) (String.concat "," (codes ds));
  let want =
    [ ("saves a file", None); ("each test starts empty", None); ("clock only", None);
      ("fails with a dir", Some "assertion failed: Ok([\"x\"]) != Ok([])") ]
  in
  let test_dirs () =
    List.filter (String.starts_with ~prefix:"tessera-test-") (Array.to_list (Sys.readdir (Filename.get_temp_dir_name ())))
  in
  let before = test_dirs () in
  let got = List.map (fun (n, r) -> (n, match r with Ok () -> None | Error (m, _) -> Some m)) (Interp.run_tests ~out:ignore items) in
  ok "tessera-test-caps-interp" (got = want) (String.concat "; " (List.map (fun (n, m) -> n ^ "=" ^ Option.value ~default:"ok" m) got));
  ok "tessera-test-caps-cleanup" (List.for_all (fun d -> List.mem d before) (test_dirs ())) "test directories left behind";
  if native_for "tessera-test-caps" then begin
    let exe = Filename.concat tmpdir "tessera-test-caps" in
    match Native.build ~flags:sanitize ~tests:true items exe with
    | Error m -> ok "tessera-test-caps-native" false m
    | Ok () ->
      List.iteri
        (fun i (n, w) ->
          let dir = Caps.test_dir () in
          let e = exe ^ ".err" in
          let code =
            Sys.command
              (sprintf "ASAN_OPTIONS=detect_leaks=0 TESSERA_LEAKCHECK=1 %s %d --grant fs=%s 2>%s" (Filename.quote exe) i (Filename.quote dir) (Filename.quote e))
          in
          Caps.remove_tree dir;
          let err = read_file e in
          match w with
          | None -> ok ("tessera-test-caps-native/" ^ n) (code = 0 && err = "tessera-leakcheck: 0 live\n") (sprintf "exit %d, stderr %S" code err)
          | Some m -> ok ("tessera-test-caps-native/" ^ n) (code <> 0 && String.starts_with ~prefix:("panic: " ^ m) err) (sprintf "exit %d, stderr %S" code err))
        want
  end;
  errs "tessera-test-bad-cap" "test \"x\" (n: Net) {}\ntest \"y\" (a: Fs, b: Fs) {}\ntest \"z\" (c: Clock, d: Int) {}" [ "E0306"; "E0306"; "E0306" ];

  (* virtual time: `clock: virtual Clock` starts at 2026-01-01T00:00:00Z and
     jumps to the next timer whenever every task is waiting, the same in both
     backends; an hour of sleeping takes no real time *)
  let virtual_src = {|use time

test "an hour at once" (clock: virtual Clock) {
    let start = clock.now_ms()
    assert_eq(start, 1767225600000)
    clock.sleep(3600000)
    assert_eq(clock.now_ms(), start + 3600000)
    for _ in 0..<1000 {
        clock.sleep(1000)
    }
    clock.sleep(0)
    clock.sleep(-5)
    assert_eq(clock.now_ms(), start + 4600000)
}

test "the message comes first" (clock: virtual Clock) {
    let c: Chan[Str] = channel(0)
    let got = group g {
        g.go(|| {
            clock.sleep(4999)
            c.send("message")
        })
        g.go(|| {
            select {
                v = c.recv() => v ?? "closed",
                clock.after(5000) => "timed out",
            }
        })
    }
    assert_eq(got, ["message"])
    assert_eq(clock.now_ms(), 1767225604999)
}

test "the timeout comes first" (clock: virtual Clock) {
    let c: Chan[Str] = channel(1)
    let got = group g {
        g.go(|| {
            clock.sleep(5001)
            c.send("message")
        })
        g.go(|| {
            let r = select {
                v = c.recv() => v ?? "closed",
                clock.after(5000) => "timed out",
            }
            "{r} at {clock.now_ms() - 1767225600000}"
        })
    }
    assert_eq(got, ["timed out at 5000"])
    assert_eq(clock.now_ms(), 1767225605001)
}

test "the timer set first rings first" (clock: virtual Clock) {
    let c: Chan[Str] = channel(1)
    let got = group g {
        g.go(|| {
            clock.sleep(5000)
            c.send("message")
        })
        g.go(|| {
            select {
                v = c.recv() => v ?? "closed",
                clock.after(5000) => "timed out",
            }
        })
    }
    assert_eq(got, ["message"])
}

test "a deadline cancels the slow task" (clock: virtual Clock) {
    let got = group g until clock.after(100) {
        g.go(|| {
            clock.sleep(50)
            1
        })
        g.go(|| {
            clock.sleep(150)
            2
        })
    }
    assert_eq(got, [Some(1), None])
    assert_eq(clock.now_ms(), 1767225600100)
}

test "nested deadlines" (clock: virtual Clock) {
    let start = clock.now_ms()
    let got = group outer until clock.after(1000) {
        outer.go(|| {
            let inner = group g until clock.after(100) {
                g.go(|| {
                    clock.sleep(50)
                    "a"
                })
                g.go(|| {
                    clock.sleep(500)
                    "b"
                })
            }
            assert_eq(inner, [Some("a"), None])
            clock.now_ms() - start
        })
        outer.go(|| {
            clock.sleep(2000)
            0
        })
    }
    assert_eq(got, [Some(100), None])
    assert_eq(clock.now_ms() - start, 1000)
}

test "within" (clock: virtual Clock) {
    let slow = time.within(clock, 100, || {
        clock.sleep(3600000)
        Ok(1)
    })
    assert_eq(slow, Err(Failed("timed out after 100 ms")))
    let fast = time.within(clock, 100, || {
        clock.sleep(10)
        Ok(2)
    })
    assert_eq(fast, Ok(2))
    assert_eq(clock.now_ms(), 1767225600110)
}

test "tasks wake in order" (clock: virtual Clock) {
    let c: Chan[Str] = channel(10)
    let _ = group g {
        for (name, ms) in [("c", 300), ("a", 100), ("d", 400), ("b", 200), ("a2", 100)] {
            g.go(|| {
                clock.sleep(ms)
                c.send("{name}@{clock.now_ms() - 1767225600000}")
            })
        }
    }
    c.close()
    let mut got: List[Str] = []
    let mut more = true
    while more {
        match c.recv() {
            Some(v) => { got.push(v) },
            None => { more = false },
        }
    }
    assert_eq(got, ["a@100", "a2@100", "b@200", "c@300", "d@400"])
}

test "time stands still while a task works" (fs: Fs, clock: virtual Clock) {
    let got = group g {
        g.go(|| {
            clock.sleep(10)
            clock.now_ms()
        })
        g.go(|| {
            for i in 0..<200 {
                assert(fs.write("f{i}.txt", "x").is_ok())
            }
            clock.now_ms()
        })
    }
    assert_eq(got, [1767225600010, 1767225600000])
}

test "a deadlock with no timers" (clock: virtual Clock) {
    let c: Chan[Int] = channel(0)
    clock.sleep(1000)
    let _ = c.recv()
}

test "for each case" (clock: virtual Clock) for ms in [10, 20] {
    clock.sleep(ms)
    assert_eq(clock.now_ms(), 1767225600000 + ms)
}

test "the real clock" (clock: Clock) {
    assert(clock.now_ms() > 1700000000000)
    assert(clock.now_ms() != 1767225600000)
}
|} in
  let items, ds, _ = compile "tessera-virtual-time" virtual_src in
  ok "tessera-virtual-time-checks" (ds = []) (String.concat "," (codes ds));
  let deadlock = "all tasks are blocked (deadlock)" in
  let want =
    [ ("an hour at once", None); ("the message comes first", None); ("the timeout comes first", None);
      ("the timer set first rings first", None); ("a deadline cancels the slow task", None); ("nested deadlines", None);
      ("within", None); ("tasks wake in order", None); ("time stands still while a task works", None);
      ("a deadlock with no timers", Some deadlock); ("for each case 10", None); ("for each case 20", None); ("the real clock", None) ]
  in
  let show got = String.concat "; " (List.map (fun (n, m) -> n ^ "=" ^ Option.value ~default:"ok" m) got) in
  let t0 = Unix.gettimeofday () in
  let got = List.map (fun (n, r) -> (n, match r with Ok () -> None | Error (m, _) -> Some m)) (Interp.run_tests ~out:ignore items) in
  let secs = Unix.gettimeofday () -. t0 in
  ok "tessera-virtual-time-interp" (got = want) (show got);
  ok "tessera-virtual-time-interp-fast" (secs < 10.) (sprintf "took %.1f s" secs);
  (* natively, each test in its own process (a watchdog stops one that would
     wait in real time), under AddressSanitizer and ThreadSanitizer *)
  let native_virtual suffix flags env =
    let name = "tessera-virtual-time" ^ suffix in
    if native_for name && selected name then begin
      let exe = exe_path name in
      let dir = Caps.test_dir () in
      let q = Filename.quote in
      let runs =
        List.mapi
          (fun i _ ->
            sprintf "%sperl -e 'alarm 60; exec @ARGV' %s %d --grant fs=%s >/dev/null 2>%s.%d.err; echo $? >%s.%d.code" env (q exe) i (q dir) (q exe) i
              (q exe) i)
          want
      in
      let run_cmd = sprintf "s=$(date +%%s); %s; echo $(( $(date +%%s) - s )) >%s.secs" (String.concat "; " runs) (q exe) in
      native_job ~tests:true ~flags items exe run_cmd (function
        | Error m -> Caps.remove_tree dir; ok name false ("C compile failed:\n" ^ m)
        | Ok _ ->
          Caps.remove_tree dir;
          List.iteri
            (fun i (n, w) ->
              let code = int_of_string (String.trim (read_file (sprintf "%s.%d.code" exe i))) in
              let err = read_file (sprintf "%s.%d.err" exe i) in
              match w with
              | None -> ok (name ^ "/" ^ n) (code = 0 && err = "tessera-leakcheck: 0 live\n") (sprintf "exit %d, stderr %S" code err)
              | Some m -> ok (name ^ "/" ^ n) (code <> 0 && String.starts_with ~prefix:("panic: " ^ m) err) (sprintf "exit %d, stderr %S" code err))
            want;
          let secs = int_of_string (String.trim (read_file (exe ^ ".secs"))) in
          ok (name ^ "-fast") (secs < 60) (sprintf "took %d s" secs))
    end
  in
  native_virtual "" sanitize "ASAN_OPTIONS=detect_leaks=0 TESSERA_LEAKCHECK=1 ";
  native_virtual "-tsan" tsan "TSAN_OPTIONS=halt_on_error=1 TESSERA_THREADS=8 TESSERA_LEAKCHECK=1 ";
  errs "tessera-virtual-bench" "bench \"b\" (clock: virtual Clock) {\n    clock.sleep(1)\n}" [ "E0306" ];
  errs "tessera-virtual-not-clock" "test \"t\" (fs: virtual Fs) {\n    assert(true)\n}" [ "E0003" ];
  errs "tessera-virtual-twice" "test \"t\" (a: virtual Clock, b: Clock) {\n    assert(true)\n}" [ "E0306" ];

  (* table tests: one isolated run per case, named by the case's text;
     benches: skipped by tests, run natively by `tessera test --bench` *)
  let table_src = {|fn add(a: Int, b: Int) -> Int {
    a + b
}

test "add" for (a, b, want) in [(1, 2, 3), (2, 2, 4), (2, 2,
        5)] {
    assert_eq(add(a, b), want)
}

test "one name" for w in ["a", "bb"] {
    assert(w.len() > 0)
}

test "with a dir" (fs: Fs, clock: Clock) for (name, (body, _)) in [("a.txt", ("x", 1)), ("b.txt", ("y", 2))] {
    assert_eq(fs.list(""), Ok([]))
    assert(fs.write(name, body).is_ok())
    assert(clock.now_ms() > 0)
}

bench "sum" {
    let mut t = 0
    for i in 0..<100 {
        t = t + i
    }
    t
}

bench "strings" (fs: Fs) {
    keep(fs.read("missing").is_err())
    "a" + "{40 + 2}"
}

bench "boom" {
    [1][3]
}
|} in
  let items, ds, _ = compile "tessera-table-tests" table_src in
  ok "tessera-table-checks" (ds = []) (String.concat "," (codes ds));
  let want =
    [ ("add (1, 2, 3)", None); ("add (2, 2, 4)", None); ("add (2, 2, 5)", Some "assertion failed: 4 != 5"); ("one name \"a\"", None);
      ("one name \"bb\"", None); ("with a dir (\"a.txt\", (\"x\", 1))", None); ("with a dir (\"b.txt\", (\"y\", 2))", None) ]
  in
  let got = List.map (fun (n, r) -> (n, match r with Ok () -> None | Error (m, _) -> Some m)) (Interp.run_tests ~out:ignore items) in
  ok "tessera-table-interp" (got = want) (String.concat "; " (List.map (fun (n, m) -> n ^ "=" ^ Option.value ~default:"ok" m) got));
  let only = List.map (fun (n, _, _) -> n) (Interp.run_tests_timed ~select:(fun n -> String.starts_with ~prefix:"one" n) ~out:ignore items) in
  ok "tessera-table-select" (only = [ "one name \"a\""; "one name \"bb\"" ]) (String.concat "; " only);
  if native_for "tessera-table" then begin
    let exe = Filename.concat tmpdir "tessera-table-tests" in
    match Native.build ~flags:sanitize ~tests:true items exe with
    | Error m -> ok "tessera-table-native" false m
    | Ok () ->
      let run args fs =
        let dir = if fs then Some (Caps.test_dir ()) else None in
        let e = exe ^ ".err" in
        let grant = match dir with Some d -> sprintf " --grant fs=%s" (Filename.quote d) | None -> "" in
        let code =
          Sys.command (sprintf "ASAN_OPTIONS=detect_leaks=0 TESSERA_LEAKCHECK=1 %s %s%s >/dev/null 2>%s" (Filename.quote exe) args grant (Filename.quote e))
        in
        Option.iter Caps.remove_tree dir;
        (code, read_file e)
      in
      List.iteri
        (fun i (n, w) ->
          let code, err = run (string_of_int i) (i >= 5) in
          match w with
          | None -> ok ("tessera-table-native/" ^ n) (code = 0 && err = "tessera-leakcheck: 0 live\n") (sprintf "exit %d, stderr %S" code err)
          | Some m -> ok ("tessera-table-native/" ^ n) (code <> 0 && String.starts_with ~prefix:("panic: " ^ m) err) (sprintf "exit %d, stderr %S" code err))
        want;
      (* `bench N NS ITERS`: a fixed count reports it, its time and allocations *)
      let report err = match String.split_on_char ' ' (List.hd (String.split_on_char '\n' err)) with
        | [ "tessera-bench:"; n; ns; al ] -> Some (int_of_string n, int_of_string ns, int_of_string al)
        | _ -> None
      in
      let code, err = run "bench 0 0 7" false in
      ok "tessera-bench-native/sum"
        (code = 0 && (match report err with Some (7, ns, 0) -> ns >= 0 | _ -> false) && String.ends_with ~suffix:"\ntessera-leakcheck: 0 live\n" err)
        (sprintf "exit %d, stderr %S" code err);
      let code, err = run "bench 1 0 5" true in
      ok "tessera-bench-native/strings"
        (code = 0 && (match report err with Some (5, _, al) -> al >= 10 | _ -> false) && String.ends_with ~suffix:"\ntessera-leakcheck: 0 live\n" err)
        (sprintf "exit %d, stderr %S" code err);
      (* a time budget: the count grows until a round fills it *)
      let code, err = run "bench 0 20000000 0" false in
      ok "tessera-bench-native/scaled" (code = 0 && match report err with Some (n, ns, _) -> n > 1000 && ns >= 20000000 | None -> false) (sprintf "exit %d, stderr %S" code err);
      let code, err = run "bench 2 0 3" false in
      ok "tessera-bench-native/boom" (code <> 0 && String.starts_with ~prefix:"panic: index 3 is out of range" err) (sprintf "exit %d, stderr %S" code err)
  end;
  errs "tessera-table-not-literal" "fn cases() -> List[Int] { [1] }\ntest \"t\" for x in cases() {}" [ "E0003" ];
  errs "tessera-table-empty" "test \"t\" for x in [] {}" [ "E0003" ];
  errs "tessera-table-refutable" "test \"t\" for Some(x) in [Some(1)] {}" [ "E0003" ];
  errs "tessera-table-duplicate-case" "test \"t\" for x in [1, 2, 1] {\n    assert(x > 0)\n}" [ "E0106" ];
  (* the body is checked once per case, and each problem is reported once *)
  errs "tessera-table-error-once" "test \"t\" for (a, b) in [(1, 2), (3, 4), (5, 6)] {\n    let s: Str = a\n}" [ "E0201" ];
  errs "tessera-table-case-types" "test \"t\" for (a, b) in [(1, 2), (\"x\", 4)] {\n    assert(a + 1 > b)\n}" [ "E0201"; "E0201" ];
  errs "tessera-bench-duplicate" "bench \"a\" {}\nbench \"a\" {}\ntest \"a\" {}" [ "E0106" ];
  errs "tessera-bench-bad-cap" "bench \"x\" (n: Net) {}" [ "E0306" ];
  errs "keep-arity" "fn main() {\n    keep(1, 2)\n}" [ "E0107" ];
  (* the `tessera test` command: --run, --json, --bench *)
  let cli_exe =
    let e = Filename.concat (Filename.dirname (Filename.dirname Sys.executable_name)) "bin/main.exe" in
    if Filename.is_relative e then Filename.concat (Sys.getcwd ()) e else e
  in
  let table_file = Filename.concat tmpdir "table-tests.tsr" in
  Out_channel.with_open_bin table_file (fun oc -> output_string oc table_src);
  let cli args =
    let o = table_file ^ ".out" and e = table_file ^ ".err" in
    let code = Sys.command (String.concat " " (List.map Filename.quote (cli_exe :: args)) ^ sprintf " >%s 2>%s" (Filename.quote o) (Filename.quote e)) in
    (code, read_file o, read_file e)
  in
  (* the time in parentheses varies; drop it *)
  let untimed o =
    String.concat "\n"
      (List.map
         (fun l -> match String.rindex_opt l '(' with Some i when String.ends_with ~suffix:"s)" l -> String.sub l 0 (max 0 (i - 1)) | _ -> l)
         (String.split_on_char '\n' o))
  in
  let code, o, _ = cli [ "test"; table_file; "--run"; "add (2" ] in
  ok "cli-test-run"
    (code = 1
    && untimed o
       = sprintf "ok    add (2, 2, 4)\nFAIL  add (2, 2, 5)\n      assertion failed: 4 != 5\n      --> %s:7:5\n2 tests, 1 failed\n" table_file)
    (sprintf "%d %S" code o);
  let code, o, _ = cli [ "test"; table_file; "--run"; "nothing like this" ] in
  ok "cli-test-run-none" (code = 0 && o = "no tests match\n") o;
  let code, o, _ = cli [ "test"; table_file; "--json"; "--run"; "one name" ] in
  let j = try Json.parse o with Json.Error _ -> Json.Null in
  let names = match Json.member "tests" j with Json.List ts -> List.map (fun t -> Json.to_str (Json.member "name" t)) ts | _ -> [] in
  ok "cli-test-json" (code = 0 && names = [ "one name \"a\""; "one name \"bb\"" ] && Json.member "failed" j = Json.Int 0) o;
  let code, o, _ = cli [ "test"; table_file; "--json"; "--run"; "add (2, 2, 5)" ] in
  let j = try Json.parse o with Json.Error _ -> Json.Null in
  let t = match Json.member "tests" j with Json.List [ t ] -> t | _ -> Json.Null in
  ok "cli-test-json-fail"
    (code = 1 && Json.member "ok" t = Json.Bool false && Json.to_str (Json.member "message" t) = "assertion failed: 4 != 5"
    && Json.to_str (Json.member "where" t) = table_file ^ ":7:5")
    o;
  if native_for "cli-test-bench" then begin
    let code, o, _ = cli [ "test"; table_file; "--bench-time"; "3x" ] in
    let lines = String.split_on_char '\n' o in
    let has p = List.exists (fun l -> String.starts_with ~prefix:p l) lines in
    ok "cli-test-bench"
      (code = 1 && has "ok    one name \"bb\"" && has "bench sum " && has "bench strings " && has "FAIL  boom"
      && List.exists (fun l -> String.starts_with ~prefix:"bench sum " l && Interp_util.find_sub l "           3 " 0 <> None && String.ends_with ~suffix:" 0 allocs/op" l) lines
      && has "7 tests, 3 benches, 2 failed")
      o;
    let code, o, _ = cli [ "test"; table_file; "--bench-time"; "2x"; "--json"; "--run"; "boom" ] in
    let j = try Json.parse o with Json.Error _ -> Json.Null in
    let b = match Json.member "benches" j with Json.List [ b ] -> b | _ -> Json.Null in
    ok "cli-test-bench-fail" (code = 1 && Json.member "ok" b = Json.Bool false && String.starts_with ~prefix:"index 3" (Json.to_str (Json.member "message" b))) o
  end;
  let code, _, e = cli [ "test"; table_file; "--bench-time"; "fast" ] in
  ok "cli-test-bench-time-bad" (code = 2 && String.starts_with ~prefix:"tessera: --bench-time" e) e;
  out "keep" "fn main(root: Root) {\n    keep(1 + 2)\n    keep(\"s\" + \"t\")\n    keep([1, 2])\n    root.stdout.print(\"kept\")\n}" "kept\n";

  (* ---------- language server ---------- *)
  let module J = Json in
  let st = Lsp.create () in
  let uri = "file:///tmp/lsp-test.tsr" in
  let doc =
    {|struct Point { x: Int, y: Int }

fn dist(p: Point) -> Int {
    p.x + p.y
}

fn main(root: Root) {
    let out = root.stdout
    let pt = Point { x: 1, y: 2 }
    let total = dist(pt)
    let n = 3
    n = 4
    print("hi")
    out.print("{total}")
}
|}
  in
  (* line/character of the k-th byte of the nth occurrence of needle *)
  let at ?(nth = 1) text needle k =
    let rec find from n = match Interp.find_sub text needle from with Some i -> if n = 1 then i else find (i + 1) (n - 1) | None -> failwith needle in
    let off = find 0 nth + k in
    let line = ref 0 and col = ref 0 in
    String.iteri (fun i c -> if i < off then if c = '\n' then (incr line; col := 0) else incr col) text;
    (!line, !col)
  in
  let req ?(id = 1) m params = J.Obj [ ("jsonrpc", J.String "2.0"); ("id", J.Int id); ("method", J.String m); ("params", params) ] in
  let note m params = J.Obj [ ("jsonrpc", J.String "2.0"); ("method", J.String m); ("params", params) ] in
  let tdp text needle ?nth k =
    let l, c = at ?nth text needle k in
    J.Obj [ ("textDocument", J.Obj [ ("uri", J.String uri) ]); ("position", J.Obj [ ("line", J.Int l); ("character", J.Int c) ]) ]
  in
  let result msgs = match msgs with [ m ] -> J.member "result" m | _ -> J.Null in
  let init = result (Lsp.handle st (req "initialize" (J.Obj []))) in
  ok "lsp-initialize" (J.path init [ "capabilities"; "hoverProvider" ] = J.Bool true) (J.to_string init);
  let opened =
    Lsp.handle st (note "textDocument/didOpen" (J.Obj [ ("textDocument", J.Obj [ ("uri", J.String uri); ("text", J.String doc) ]) ]))
  in
  let diag_codes =
    match opened with
    | [ m ] -> ( match J.path m [ "params"; "diagnostics" ] with J.List ds -> List.map (fun d -> J.to_str (J.member "code" d)) ds | _ -> [])
    | _ -> []
  in
  ok "lsp-diagnostics" (diag_codes = [ "E0205"; "E0302" ]) (String.concat "," diag_codes);
  let hover text needle ?nth k =
    J.to_str (J.path (result (Lsp.handle st (req "textDocument/hover" (tdp text needle ?nth k)))) [ "contents"; "value" ])
  in
  ok "lsp-hover-let" (hover doc "total = " 0 = "```tessera\nlet total: Int\n```") (hover doc "total = " 0);
  (* a call shows the function's declaration (and its doc comment, if any) *)
  ok "lsp-hover-call" (hover doc "dist(pt)" 0 = "```tessera\nfn dist(p: Point) -> Int\n```") (hover doc "dist(pt)" 0);
  ok "lsp-hover-method" (hover doc "print(\"{" 0 = "```tessera\nprint(Str) -> ()\n```") (hover doc "print(\"{" 0);
  ok "lsp-hover-fn" (hover doc "dist(p" 0 = "```tessera\nfn dist(p: Point) -> Int\n```") (hover doc "dist(p" 0);
  let def text needle ?nth k =
    match result (Lsp.handle st (req "textDocument/definition" (tdp text needle ?nth k))) with
    | J.Null -> None
    | r -> Some (J.to_int (J.path r [ "range"; "start"; "line" ]), J.to_int (J.path r [ "range"; "start"; "character" ]))
  in
  ok "lsp-def-fn" (def doc "dist(pt)" 1 = Some (at doc "dist(p" 0)) "dist";
  ok "lsp-def-local" (def doc "(pt)" 1 = Some (at doc "pt = " 0)) "pt";
  ok "lsp-def-field" (def doc "p.x" 2 = Some (at doc "x: Int" 0)) "field x";
  ok "lsp-def-struct" (def doc "Point {" ~nth:2 1 = Some (at doc "Point" 0)) "Point";
  ok "lsp-def-param" (def doc "p.y" 0 = Some (at doc "p: Point" 0)) "param p";
  (* completion after a dot: edit the document to end a line with `out.`, `pt.` and `root.` *)
  let complete text needle k =
    ignore (Lsp.handle st (note "textDocument/didChange"
      (J.Obj [ ("textDocument", J.Obj [ ("uri", J.String uri) ]); ("contentChanges", J.List [ J.Obj [ ("text", J.String text) ] ]) ])));
    match result (Lsp.handle st (req "textDocument/completion" (tdp text needle k))) with
    | J.List items -> List.map (fun i -> J.to_str (J.member "label" i)) items
    | _ -> []
  in
  let with_line l = Interp.replace_all doc "    out.print(\"{total}\")" ("    out.print(\"{total}\")\n" ^ l) in
  let d1 = with_line "    out.pr" in
  let c1 = complete d1 "    out.pr\n" 10 in
  ok "lsp-complete-stdout" (List.mem "print" c1 && List.mem "write" c1) (String.concat "," c1);
  let d2 = with_line "    pt." in
  let c2 = complete d2 "    pt." 7 in
  ok "lsp-complete-fields" (List.mem "x" c2 && List.mem "y" c2) (String.concat "," c2);
  let d3 = with_line "    root." in
  let c3 = complete d3 "    root." 9 in
  ok "lsp-complete-root" (List.mem "net" c3 && List.mem "fs" c3 && List.mem "stdout" c3) (String.concat "," c3);
  let d5 = with_line "    let ys = [1, 2]\n    ys." in
  let c5 = complete d5 "    ys." 7 in
  ok "lsp-complete-list" (List.mem "len" c5 && List.mem "group_by" c5 && List.mem "tally" c5) (String.concat "," c5);
  (* after `?.`, the members of the value inside the Option *)
  let d5 = with_line "    let op = Some(pt)\n    op?." in
  let c5 = complete d5 "    op?." 8 in
  ok "lsp-complete-safe" (List.mem "x" c5 && List.mem "y" c5 && not (List.mem "is_some" c5)) (String.concat "," c5);
  let c4 = complete doc "    let n" 4 in
  ok "lsp-complete-general" (List.mem "total" c4 && List.mem "dist" c4 && List.mem "while" c4) (String.concat "," c4);
  (* inside a call's parentheses: the parameter names, as named-argument labels *)
  let d5 = with_line "    let q = dist(" in
  let c5 = complete d5 "    let q = dist(" 17 in
  ok "lsp-complete-labels" (List.mem "p:" c5) (String.concat "," c5);
  (* quick fix: make n mutable *)
  ignore (complete doc "    let n" 4);
  let l, c = at doc "n = 4" 0 in
  let actions =
    result
      (Lsp.handle st
         (req "textDocument/codeAction"
            (J.Obj
               [ ("textDocument", J.Obj [ ("uri", J.String uri) ]);
                 ("range", J.Obj [ ("start", J.Obj [ ("line", J.Int l); ("character", J.Int c) ]); ("end", J.Obj [ ("line", J.Int l); ("character", J.Int (c + 1)) ]) ]);
                 ("context", J.Obj [ ("diagnostics", J.List []) ]) ])))
  in
  let new_text =
    match actions with
    | J.List [ a ] -> ( match J.path a [ "edit"; "changes"; uri ] with J.List [ e ] -> J.to_str (J.member "newText" e) | _ -> "")
    | _ -> ""
  in
  ok "lsp-quickfix" (new_text = "mut ") (J.to_string actions);
  let syms = result (Lsp.handle st (req "textDocument/documentSymbol" (J.Obj [ ("textDocument", J.Obj [ ("uri", J.String uri) ]) ]))) in
  ok "lsp-symbols" (match syms with J.List l -> List.length l = 3 | _ -> false) (J.to_string syms);
  ok "lsp-unknown-method" (match Lsp.handle st (req "workspace/symbol" (J.Obj [])) with [ m ] -> J.member "error" m <> J.Null | _ -> false) "error";
  ok "lsp-shutdown" (Lsp.handle st (req "shutdown" J.Null) <> [] && st.Lsp.shutdown) "shutdown";
  ok "json-roundtrip"
    (let j = J.Obj [ ("a", J.List [ J.Int 1; J.Float 2.5; J.String "q\"\n\u{e9}"; J.Bool true; J.Null ]) ] in
     J.parse (J.to_string j) = j)
    "json";

  (* ---------- string and int built-ins ---------- *)
  out "str-builtins"
    (main_wrap
       ("    let s = \"héllo, world\"\n    out.print(s.slice(0, 6) + \"|\" + s.slice(8, 100) + \"|\" + s.slice(5, 2) + \"|\" + s.slice(-3, 1))\n"
       ^ "    out.print(\"{s.index_of(\"world\") ?? -1} {s.index_of(\"xyz\") ?? -1}\")\n"
       ^ "    out.print(\"{\"Ab\".bytes()} {\"é\".bytes().len()}\")\n"
       ^ "    out.print(\"{\" 2.5e3 \".parse_float() ?? 0.0} {\"-1\".parse_float() ?? 0.0} {\"nan\".parse_float().is_none()} {\"1x\".parse_float().is_none()}\")\n"
       ^ "    out.print(65.to_char() + 233.to_char() + 8364.to_char() + 128512.to_char() + (-1).to_char().len().to_str())\n"
       ^ "    out.print(\"{3.min(5)} {3.max(5)} {(-2).min(-7)}\")"))
    "h\195\169llo|world||h\n8 -1\n[65, 98] 2\n2500.0 -1.0 true true\nAé€\240\159\152\1283\n3 5 -7\n";

  (* ---------- the standard library ---------- *)
  let example f =
    let dir = if Sys.file_exists "../examples/stdlib.tsr" then "../examples" else "examples" in
    In_channel.with_open_bin (Filename.concat dir f) In_channel.input_all
  in
  out "std-json-list-text" (example "stdlib.tsr") "{\"name\":\"Ada\",\"tags\":[\"x\",\"yé😀\"],\"n\":-125,\"ok\":true,\"none\":null,\"nested\":{\"a\":[1,2,{}]}}\nAda\n-125.0\nexpected a value at byte 3\nexpected `:` at byte 5\nexpected a value at byte 0\nexpected a closing `\"` at byte 4\nexpected end of input at byte 4\nexpected a value at byte 0\n{\"q\":\"a\\\"b\\n\\u0001\",\"xs\":[1,null,false]}\na,b,c\n[0, 1, 2, 3, 4] 2 [1, 3] [1, 2, 3] [1, 2]\n007|ab..|---|2|[\"hi\", \"there\", \"you\"]\n";
  (* embed: files read at compile time become literals, in both backends; the
     program needs no Fs. Text keeps braces and quotes as they are; bytes keep
     NUL and 0xff. *)
  (let dir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "tessera-mods-%d-%s" (Unix.getpid ()) "embed-text-bytes") in
   (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
   (try Unix.mkdir (Filename.concat dir "site") 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
   Out_channel.with_open_bin (Filename.concat dir "site/page.html") (fun oc -> output_string oc "<p class=\"x\">{not interpolated} \\ é</p>\n");
   Out_channel.with_open_bin (Filename.concat dir "logo.bin") (fun oc -> output_string oc "\000\255A");
   let ddir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "tessera-mods-%d-%s" (Unix.getpid ()) "embed-dir") in
   List.iter (fun d -> try Unix.mkdir (Filename.concat ddir d) 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()) [ ""; "site"; "site/css"; "site/.git" ];
   List.iter (fun (f, t) -> Out_channel.with_open_bin (Filename.concat ddir f) (fun oc -> output_string oc t))
     [ ("site/page.html", "<p>x</p>"); ("site/css/a.css", "b{}"); ("site/.git/HEAD", "hidden"); ("site/.env", "hidden") ];
   out "embed-dir" ~modules:[ ("assets", "pub fn all() -> Map[Str, Bytes] {\n    embed_dir(\"site\")\n}\n") ]
     {|use assets

fn main(root: Root) {
    let files = assets.all()
    root.stdout.print("{files.keys()} {files["css/a.css"].to_str()} {files.get(".env")}")
}
|}
     "[\"css/a.css\", \"page.html\"] b{} None\n";
   out "embed-text-bytes"
     ~modules:[ ("pages", "pub fn page() -> Str {\n    embed(\"site/page.html\")\n}\n") ]
     {|use pages

fn main(root: Root) {
    let b = embed_bytes("logo.bin")
    root.stdout.print("{pages.page().trim()} {b.len()} {b.get(0)} {b.get(1)} {b.get(2)}")
}
|}
     "<p class=\"x\">{not interpolated} \\ é</p> 3 Some(0) Some(255) Some(65)\n");
  (* listening on [::] serves IPv4 too (dual stack), as platforms that probe
     over IPv6 and forward over IPv4 need *)
  out "listen-ipv6-dual-stack"
    ~grants:{ Interp.no_grants with listen = [ "[::]" ]; net = [ "127.0.0.1" ] }
    {|use http

fn main(root: Root) -> Result[(), IoError] {
    let l = root.listen.on("[::]:0")?
    let port = l.port()
    let net = root.net
    let rs = group g {
        g.go(|| {
            let _ = http.serve(l, 1, |r| http.text(200, "dual {r.path}"))
        })
        g.go(|| match http.get(net, "http://127.0.0.1:{port}/v4") {
            Ok(r) => r.body,
            Err(e) => "{e}",
        })
    }
    root.stdout.print("{rs}")
    for bad in ["[nope]:1", "[::1]", "[::1]:99999"] {
        match root.listen.on(bad) {
            Ok(_) => root.stdout.print("bound {bad}"),
            Err(e) => root.stdout.print("{e}"),
        }
    }
    Ok(())
}
|}
    "[\"dual /v4\"]\nFailed(\"address must be HOST:PORT\")\nFailed(\"address must be HOST:PORT\")\nFailed(\"address must be HOST:PORT\")\n";
  (* a streamed response, a line at a time as it arrives: a line split
     across chunks, CRLF, an empty chunk, no final line break, a sized body,
     an error answered whole, and an endless stream the caller stops *)
  out "std-http-request-lines"
    ~grants:{ Interp.no_grants with listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1" ] }
    {|use http

fn handler(clock: Clock, r: http.Request, c: Conn) -> Result[(), IoError] {
    match r.path {
        "/lines" => {
            http.write_head(c, 200, [])?
            for piece in ["a\nb", "\nc\r\n", "", "d"] {
                http.write_chunk(c, piece)?
                clock.sleep(2)
            }
            http.end_body(c)
        },
        "/endless" => {
            http.write_head(c, 200, [])?
            let mut i = 0
            while true {
                http.write_chunk(c, "tick {i}\n")?
                i = i + 1
                clock.sleep(5)
            }
            Ok(())
        },
        "/sized" => http.write_response(c, http.text(200, "one\ntwo\n")),
        _ => http.write_response(c, http.text(404, "nope\n")),
    }
}

fn main(root: Root) -> Result[(), IoError] {
    let l = root.listen.on("127.0.0.1:0")?
    let port = l.port()
    let net = root.net
    let out = root.stdout
    let clock = root.clock
    let _ = group g {
        // one connection at a time, as the client makes them
        g.go(|| {
            let mut i = 0
            while i < 4 {
                match l.accept() {
                    Ok(c) => {
                        let _ = http.serve_stream(c, |r, c| handler(clock, r, c))
                    },
                    Err(_) => (),
                }
                i = i + 1
            }
        })
        g.go(|| {
            let base = "http://127.0.0.1:{port}"
            for path in ["/lines", "/sized", "/missing"] {
                let r = http.request_lines(net, "GET", base + path, [], "", |line| {
                    out.print("{path}: [{line}]")
                    true
                })
                match r {
                    Ok(resp) => out.print("{path} -> {resp.status} [{resp.body.trim()}]"),
                    Err(e) => out.print("{path} -> {e}"),
                }
            }
            // a stream that never ends, read until the third line
            let r = http.request_lines(net, "GET", base + "/endless", [], "", |line| {
                out.print(line)
                line != "tick 2"
            })
            out.print("stopped: {r.map(|resp| resp.status)}")
        })
    }
    Ok(())
}
|}
    "/lines: [a]\n/lines: [b]\n/lines: [c]\n/lines: [d]\n/lines -> 200 []\n/sized: [one]\n/sized: [two]\n/sized -> 200 []\n/missing -> 404 [nope]\ntick 0\ntick 1\ntick 2\nstopped: Ok(200)\n";
  out "std-http"
    ~grants:{ Interp.no_grants with listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1" ] }
    (example "http_demo.tsr")
    "served 4\n200 text/plain; charset=utf-8 hi GET\n500 text/plain; charset=utf-8 internal error\n404 text/plain; charset=utf-8 no /missing\n201 {\"got\":[1,{\"a\":true}]}\nchunked sent\n200 hello world\n";
  out "example-ffi" ~grants:{ Interp.no_grants with foreign = [ "c" ] } (example "ffi.tsr")
    "3.25 kg: 3.25, then [ kg]\n-1e3: -1000.0, then []\n0x1p4!: 16.0, then [!]\nabc: not a number\n: not a number\n";
  (* ---------- sql: SQLite, trusted by the standard library ---------- *)
  let sqlg = Interp.no_grants in
  let sql_basic = {v|use sql

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let db = sql.memory()?
    db.exec("CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT NOT NULL, age INTEGER, score REAL, photo BLOB)", [])?
    let names = ["Ada", "Grace", "it's; DROP TABLE people; --", "é 😀"]
    for i in 0..<names.len() {
        let n = names[i]
        let changed = db.exec("INSERT INTO people (name, age, score, photo) VALUES (?, ?, ?, ?)",
            [sql.Text(n), if i == 1 { sql.Null } else { sql.Int(30 + i) }, sql.Float(i.to_float() / 2.0), sql.Blob(bytes([i, 0, 255]))])?
        out.print("{changed} {db.last_id()?}")
    }
    for row in db.query("SELECT id, name, age, score, photo FROM people WHERE id >= ? ORDER BY id", [sql.Int(2)])? {
        out.print("{row.int("id") ?? -1} {row.text("name") ?? "?"} {row.int("age")} {row.float("score") ?? 0.0} {row.blob("photo")} {row.is_null("age")}")
    }
    let one = db.query_one("SELECT count(*) AS n, sum(age) AS total, max(score) AS top FROM people", [])?
    match one {
        Some(r) => out.print("{r.int("n")} {r.int("total")} {r.float("top")} {r.float("n")} {r.columns}"),
        None => out.print("none"),
    }
    out.print("{db.query_one("SELECT name FROM people WHERE name = ?", [sql.Text("nobody")])?}")
    out.print("{db.exec("UPDATE people SET age = age + 1 WHERE age IS NOT NULL", [])?}")
    out.print("{db.exec("CREATE TABLE a (x); CREATE TABLE b (y); INSERT INTO a VALUES (1), (2)", [])?}")
    let r = db.query("SELECT x, NULL AS missing, 'hi' || ? AS greeting FROM a", [sql.Text("!")])?
    out.print("{r[1].get("x")} {r[1].get("missing")} {r[1].get("greeting")} {r.len()}")
    db.close()
}
|v} in
  let sql_basic_want =
    "1 1\n1 2\n1 3\n1 4\n2 Grace None 0.5 Some(bytes([1, 0, 255])) true\n3 it's; DROP TABLE people; -- Some(32) 1.0 Some(bytes([2, 0, 255])) false\n4 é 😀 Some(33) 1.5 Some(bytes([3, 0, 255])) false\nSome(4) Some(95) Some(1.5) Some(4.0) [\"n\", \"total\", \"top\"]\nNone\n3\n2\nInt(2) Null Text(\"hi!\") 2\n"
  in
  out "sql-basic" ~grants:sqlg sql_basic sql_basic_want;
  let sql_errors = {v|use sql

fn main(root: Root) {
    let out = root.stdout
    match sql.memory() {
        Ok(db) => {
            out.print("{db.exec("CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT UNIQUE)", [])}")
            out.print("{db.exec("INSERT INTO t (name) VALUES (?)", [sql.Text("a")])}")
            out.print("{db.exec("INSERT INTO t (name) VALUES (?)", [sql.Text("a")])}")
            out.print("{db.exec("SELEC 1", [])}")
            out.print("{db.query("SELECT * FROM nowhere", [])}")
            out.print("{db.query("SELECT ?", [])}")
            out.print("{db.query("SELECT ?", [sql.Int(1), sql.Int(2)])}")
            out.print("{db.exec("INSERT INTO t (name) VALUES (?); DELETE FROM t", [sql.Text("b")])}")
            out.print("{db.exec("  ", [sql.Int(1)])}")
            out.print("{db.exec("ATTACH DATABASE '/tmp/tessera-sql-attach.db' AS other", [])}")
            out.print("{db.exec("VACUUM INTO '/tmp/tessera-sql-vacuum.db'", [])}")
            match db.query("SELECT name FROM t", []) {
                Ok(rows) => {
                    out.print("{rows}")
                    out.print("{catch(|| rows[0].int("name"))}")
                    out.print("{catch(|| rows[0].text("nope"))}")
                },
                Err(e) => out.print("{e}"),
            }
            out.print("{db.close()}")
        },
        Err(e) => out.print("{e}"),
    }
}
|v} in
  out "sql-errors" ~grants:sqlg sql_errors
    "Ok(0)\nOk(1)\nErr(Failed(\"sql: exec: UNIQUE constraint failed: t.name\"))\nErr(Failed(\"sql: near \\\"SELEC\\\": syntax error\"))\nErr(Failed(\"sql: prepare: no such table: nowhere\"))\nErr(Failed(\"sql: the statement has 1 parameter, but 0 were given\"))\nErr(Failed(\"sql: the statement has 1 parameter, but 2 were given\"))\nErr(Failed(\"sql: a statement with parameters must be alone; run several with `exec` and no parameters\"))\nErr(Failed(\"sql: no statement in the SQL text\"))\nErr(Failed(\"sql: too many attached databases - max 0\"))\nErr(Failed(\"sql: too many attached databases - max 0\"))\n[sql.Row { columns: [\"name\"], values: [Text(\"a\")] }]\nErr(\"sql: column `name` is not an integer: Text(\\\"a\\\")\")\nErr(\"sql: the row has no column `nope` (it has name)\")\nOk(())\n";
  (* transactions: committed on Ok, rolled back on Err and on a panic *)
  let sql_tx = {v|use sql

fn count(db: sql.Db) -> Int {
    match db.query_one("SELECT count(*) AS n FROM t", []) {
        Ok(Some(r)) => r.int("n") ?? -1,
        _ => -1,
    }
}

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let db = sql.memory()?
    db.exec("CREATE TABLE t (x INTEGER NOT NULL)", [])?
    let a = db.transaction(|| {
        db.exec("INSERT INTO t VALUES (?)", [sql.Int(1)])?
        db.exec("INSERT INTO t VALUES (?)", [sql.Int(2)])?
        Ok("committed")
    })
    out.print("{a} {count(db)}")
    let b = db.transaction(|| {
        db.exec("INSERT INTO t VALUES (?)", [sql.Int(3)])?
        db.exec("INSERT INTO t VALUES (?)", [sql.Null])?
        Ok("not reached")
    })
    out.print("{b} {count(db)}")
    let c = catch(|| db.transaction(|| {
        db.exec("INSERT INTO t VALUES (?)", [sql.Int(4)])?
        if count(db) == 3 { panic("boom") }
        Ok(0)
    }))
    out.print("{c} {count(db)}")
    let d = db.transaction(|| db.transaction(|| Ok(1)))
    out.print("{d} {count(db)}")
    db.close()
}
|v} in
  out "sql-transaction" ~grants:sqlg sql_tx
    "Ok(\"committed\") 2\nErr(Failed(\"sql: exec: NOT NULL constraint failed: t.x\")) 2\nErr(\"boom\") 2\nErr(Failed(\"sql: a transaction is already open on this connection\")) 2\n";
  (* a file under the Fs grant; read-only when the Fs is *)
  let sqldir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "tessera-sql-%d" (Unix.getpid ())) in
  (try Unix.mkdir sqldir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let sqlreal = Unix.realpath sqldir in
  let sql_file = {v|use sql

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let fs = root.fs
    fs.remove("notes.db")
    let db = sql.open(fs, "notes.db")?
    db.exec("CREATE TABLE notes (body TEXT)", [])?
    db.exec("INSERT INTO notes VALUES (?)", [sql.Text("saved")])?
    db.close()?
    out.print("{fs.exists("notes.db")}")
    let ro = sql.open(fs.read_only(), "notes.db")?
    out.print("{ro.query("SELECT body FROM notes", [])?[0].text("body")}")
    out.print("{ro.exec("INSERT INTO notes VALUES (?)", [sql.Text("no")])}")
    ro.close()?
    out.print("{sql.open(fs.read_only(), "missing.db").is_err()}")
    out.print("{sql.open(fs, "../escape.db")}")
    out.print("{sql.open(fs, "/tmp/abs.db")}")
    out.print("{sql.open(fs, "no/such/dir.db")}")
    fs.remove("notes.db")
    Ok(())
}
|v} in
  out "sql-file" ~grants:{ sqlg with fs = Some (sqlreal, false) } sql_file
    "true\nSome(\"saved\")\nErr(Failed(\"sql: exec: attempt to write a readonly database\"))\ntrue\nErr(Denied(\"fs: path escapes the granted directory\"))\nErr(Denied(\"fs: absolute paths are not allowed; paths are relative to the granted directory\"))\nErr(Failed(\"fs: directory of `no/such/dir.db` does not exist\"))\n";
  (* SQLite follows symlinked sidecar files: open refuses ones that leave the grant *)
  let outside = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "tessera-sql-outside-%d" (Unix.getpid ())) in
  Out_channel.with_open_bin outside (fun oc -> output_string oc "keep");
  Out_channel.with_open_bin (Filename.concat sqlreal "fine-target") (fun oc -> output_string oc "");
  List.iter (fun (link, target) -> (try Unix.unlink (Filename.concat sqlreal link) with Unix.Unix_error _ -> ()); Unix.symlink target (Filename.concat sqlreal link))
    [ ("evil.db-journal", outside); ("broken.db-wal", "/tessera-no-such-dir/x"); ("fine.db-shm", "fine-target") ];
  out "sql-sidecar-links" ~grants:{ sqlg with fs = Some (sqlreal, false) }
    "use sql\n\nfn main(root: Root) {\n    for name in [\"evil.db\", \"broken.db\"] {\n        root.stdout.print(\"{sql.open(root.fs, name).is_err()}\")\n    }\n    match sql.open(root.fs, \"evil.db\") {\n        Err(Denied(m)) => root.stdout.print(m.slice(0, 50)),\n        _ => root.stdout.print(\"opened\"),\n    }\n    match sql.open(root.fs, \"fine.db\") {\n        Ok(db) => root.stdout.print(\"{db.exec(\"CREATE TABLE IF NOT EXISTS t (x)\", [])} {db.close()}\"),\n        Err(e) => root.stdout.print(\"{e}\"),\n    }\n    let gone = root.fs.remove(\"fine.db\").is_ok()\n}\n"
    "true\ntrue\nsql: refusing to open `evil.db`: `evil.db-journal`\nOk(0) Ok(())\n";
  ok "sql-sidecar-untouched" (read_file outside = "keep") (read_file outside);
  (* a Db holds an id, not a pointer: forged, closed and twice-closed ones are errors *)
  out "sql-handles"
    {v|use sql

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let db = sql.memory()?
    db.exec("CREATE TABLE t (x)", [])?
    let fake = sql.Db { id: 12345 }
    out.print("{fake.exec("DROP TABLE t", [])} {fake.close()}")
    let copy = db
    out.print("{db.close()} {copy.exec("SELECT 1", [])} {db.query("SELECT 1", [])} {db.last_id()} {db.close()}")
    let a = sql.memory()?
    let b = sql.memory()?
    out.print("{a.id != b.id} {a.id > 0}")
    a.close()?
    b.close()
}
|v}
    "Err(Failed(\"sql: the database is closed\")) Err(Failed(\"sql: the database is closed\"))\nOk(()) Err(Failed(\"sql: the database is closed\")) Err(Failed(\"sql: the database is closed\")) Err(Failed(\"sql: the database is closed\")) Err(Failed(\"sql: the database is closed\"))\ntrue true\n";
  (* closing while other tasks use the connection waits for them *)
  let sql_close_race = {v|use sql

fn main(root: Root) -> Result[(), IoError] {
    let db = sql.memory()?
    db.exec("CREATE TABLE t (n INTEGER)", [])?
    let results = group g {
        for i in 0..<8 {
            g.go(|| {
                let mut ok = 0
                for n in 0..<50 {
                    if db.exec("INSERT INTO t VALUES (?)", [sql.Int(n)]).is_ok() {
                        ok = ok + 1
                    }
                }
                ok
            })
        }
        g.go(|| {
            match db.close() {
                Ok(_) => 0,
                Err(_) => -1,
            }
        })
    }
    root.stdout.print("{results.len()} {results[8]} {db.exec("SELECT 1", [])}")
    Ok(())
}
|v} in
  out "sql-close-race" sql_close_race "9 0 Err(Failed(\"sql: the database is closed\"))\n";
  check_tsan "sql-close-race" sql_close_race "9 0 Err(Failed(\"sql: the database is closed\"))\n";
  out "sql-no-fs" ~grants:sqlg
    "use sql\n\nfn main(root: Root) {\n    root.stdout.print(\"{sql.open(root.fs, \"x.db\")}\")\n}\n"
    "Err(Denied(\"fs: not granted (run with --grant fs=DIR)\"))\n";
  (* one connection shared by tasks *)
  let sql_group = {v|use sql

fn main(root: Root) -> Result[(), IoError] {
    let db = sql.memory()?
    db.exec("CREATE TABLE t (task INTEGER, n INTEGER)", [])?
    let done = group g {
        for task in 0..<6 {
            g.go(|| {
                let mut ok = 0
                for n in 0..<20 {
                    if db.exec("INSERT INTO t VALUES (?, ?)", [sql.Int(task), sql.Int(n)]).is_ok() {
                        ok = ok + 1
                    }
                }
                ok
            })
        }
    }
    let r = db.query_one("SELECT count(*) AS c, sum(n) AS s FROM t", [])?
    root.stdout.print("{done} {r}")
    db.close()
}
|v} in
  let sql_group_want = "[20, 20, 20, 20, 20, 20] Some(sql.Row { columns: [\"c\", \"s\"], values: [Int(120), Int(1140)] })\n" in
  out "sql-group" ~grants:sqlg sql_group sql_group_want;
  check_tsan "sql-group" ~grants:sqlg sql_group sql_group_want;
  out "example-notes" (example "notes.tsr")
    "1. The Left Hand of Darkness\n2. Robert'); DROP TABLE notes; --\n3. Paris, Texas ***\n3 notes\n";
  (* the CRUD example's demo calls every route over loopback; its own tests
     run further down, through the CLI *)
  out "example-todo-api"
    ~grants:{ Interp.no_grants with listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1" ] }
    (example "todo_api.tsr")
    "POST /todos -> 201 {\"id\":1,\"title\":\"buy milk\",\"done\":false}\nPOST /todos -> 201 {\"id\":2,\"title\":\"write the README\",\"done\":false}\nPOST /todos -> 400 {\"error\":\"a todo needs a title\"}\nGET /todos -> 200 [{\"id\":1,\"title\":\"buy milk\",\"done\":false},{\"id\":2,\"title\":\"write the README\",\"done\":false}]\nPATCH /todos/1 -> 200 {\"id\":1,\"title\":\"buy milk\",\"done\":true}\nGET /todos/1 -> 200 {\"id\":1,\"title\":\"buy milk\",\"done\":true}\nPATCH /todos/2 -> 400 {\"error\":\"title must be a string\"}\nDELETE /todos/2 -> 204\nDELETE /todos/2 -> 404 {\"error\":\"no todo 2\"}\nGET /todos -> 200 [{\"id\":1,\"title\":\"buy milk\",\"done\":true}]\nGET /users -> 404 {\"error\":\"no route for /users\"}\n";
  (* std code is checked like user code: a json error names the byte *)
  out "std-json-errors"
    "use json\n\nfn main(root: Root) {\n    for s in [\"\\{\\\"a\\\": [1, 2.5e1, \\\"\\\\u0041\\\"]\\}\", \"[1 2]\", \"\\\"\\\\x\\\"\"] {\n        match json.parse(s) {\n            Ok(v) => root.stdout.print(json.stringify(v)),\n            Err(e) => root.stdout.print(e),\n        }\n    }\n}\n"
    "{\"a\":[1,25,\"A\"]}\nexpected `,` or `]` at byte 3\nexpected a valid escape at byte 1\n";

  out "std-format"
    {v|use format

fn main(root: Root) {
    let out = root.stdout
    out.print("{3.14159.to_fixed(2)} {2.5.to_fixed(0)} {(-0.001).to_fixed(2)} {1e21.to_fixed(1)} {(0.0 / 0.0).to_fixed(2)} {1.005.to_fixed(2)} {7.0.to_fixed(-3)}")
    out.print("{format.hex(255)} {format.hex(-1)} {format.base(255, 2)} {format.base(-9223372036854775807 - 1, 16)} {format.base(35, 36)}")
    out.print("{format.parse_base("ff", 16)} {format.parse_base("-8000000000000000", 16)} {format.parse_base("8000000000000000", 16)} {format.parse_base("12z", 10)} {format.parse_base("-", 10)}")
    out.print("{format.thousands(1234567)} {format.thousands(-999)} {format.thousands(-1000)} {format.grouped(1234567.891.to_fixed(2), "_")} {format.grouped("12", ",")}")
    out.print("{format.zero_pad(-7, 4)} {format.zero_pad(42, 5)} {format.zero_pad(12345, 3)} {format.percent(0.4567, 1)}")
    out.print("[{format.align_left("é", 3)}][{format.align_right("ab", 4)}][{format.align_center("ab", 5)}][{format.align_right("long", 2)}]")
}
|v}
    "3.14 2 -0.00 1000000000000000000000.0 nan 1.00 7\nff -1 11111111 -8000000000000000 z\nSome(255) Some(-9223372036854775808) None None None\n1,234,567 -999 -1,000 1_234_567.89 12\n-007 00042 12345 45.7%\n[\195\169  ][  ab][ ab  ][long]\n";
  out "std-time"
    {v|use time

fn show(s: Str) -> Str {
    match time.parse_iso(s) {
        Ok(ms) => "{ms} {time.iso(ms)}",
        Err(e) => e,
    }
}

fn main(root: Root) {
    let out = root.stdout
    let t = 1762885325123
    out.print(time.iso(t))
    out.print("{time.parts(t)}")
    out.print(time.iso(0) + " " + time.iso(-1) + " " + time.iso(time.date(2000, 2, 29)) + " " + time.iso(time.date(-1, 12, 31)) + " " + time.iso(time.date(10000, 1, 1)))
    out.print("{time.parts(-1)}")
    out.print(time.format(t, "%a %d %b %Y %I:%M %p, day %j, week day %u, %s %z %Z %% %q 100%"))
    out.print(time.format_at(t, -330, "%Y-%m-%d %H:%M %Z %z") + " " + time.iso_at(t, 60) + " " + time.iso_date(t))
    out.print(time.iso(time.add_months(time.date(2024, 1, 31) + time.hours(5), 1)) + " " + time.iso(time.add_months(time.date(2024, 3, 15), -14)) + " " + time.iso(time.date(2024, 13, 1)) + " " + time.iso(time.date(2024, 3, 0)))
    for s in ["2026-11-11", "2026-11-11T18:22:05.123Z", "2026-11-11 19:22:05,123456+01:00", "2026-11-11t13:22-0500", "1969-12-31T23:59:59.999Z", "2024-02-29T00:00+05", "2023-02-29", "2026-13-01", "2026-11-11T24:00", "2026-11-11T18", "2026-11-11T18:22:05.Z", "2026-11-11T18:22Zx", "26-11-11", "2026-11-11T18:22+0130x"] {
        out.print(show(s))
    }
    out.print("{time.is_leap_year(1900)} {time.is_leap_year(2000)} {time.days_in_month(2023, 2)} {time.month_name(13)} {time.weekday_name(7)}")
    let p = time.parts(t)
    out.print("{time.from_parts(p) == t} {time.from_parts(time.DateTime { year: 2026, month: 1, day: 1, hour: 25, minute: 0, second: 0, ms: 0, weekday: 0, yday: 0 })}")
}
|v}
    "2025-11-11T18:22:05.123Z\ntime.DateTime { year: 2025, month: 11, day: 11, hour: 18, minute: 22, second: 5, ms: 123, weekday: 2, yday: 315 }\n1970-01-01T00:00:00.000Z 1969-12-31T23:59:59.999Z 2000-02-29T00:00:00.000Z -000001-12-31T00:00:00.000Z +010000-01-01T00:00:00.000Z\ntime.DateTime { year: 1969, month: 12, day: 31, hour: 23, minute: 59, second: 59, ms: 999, weekday: 3, yday: 365 }\nTue 11 Nov 2025 06:22 PM, day 315, week day 2, 1762885325 +0000 UTC % %q 100%\n2025-11-11 12:52 -05:30 -0530 2025-11-11T19:22:05.123+01:00 2025-11-11\n2024-02-29T05:00:00.000Z 2023-01-15T00:00:00.000Z 2025-01-01T00:00:00.000Z 2024-02-29T00:00:00.000Z\n1794355200000 2026-11-11T00:00:00.000Z\n1794421325123 2026-11-11T18:22:05.123Z\n1794421325123 2026-11-11T18:22:05.123Z\n1794421320000 2026-11-11T18:22:00.000Z\n-1 1969-12-31T23:59:59.999Z\n1709146800000 2024-02-28T19:00:00.000Z\ntime: day 29 is out of range for 2023-02\ntime: month 13 is out of range\ntime: 24:00:00 is not a valid time\ntime: expected `:` at byte 13\ntime: expected digits at byte 20\ntime: expected `Z`, an offset or end of input at byte 16\ntime: expected a four-digit year at byte 0\ntime: expected end of input at byte 21\nfalse true 28 ? Sunday\ntrue 1767315600000\n";
  out "std-report-example" (example "report.tsr")
    "Mon 09 Nov caf\195\169        1,234.50\nTue 10 Nov books          98.12\nWed 11 Nov rent    1,500,000.00\n                   1,501,332.62\norder #000042, id 0xbeef\ndue 2026-12-01T08:30:00.000Z, week 2026-305\nprinted in true\n";
  out "std-regex"
    {v|use regex

fn show(p: Str, s: Str) -> Str {
    match regex.compile(p) {
        Ok(r) => match r.find(s) {
            Some(m) => "{p} on {s}: {m.text} at {m.start}..{m.end} {m.groups}",
            None => "{p} on {s}: no match",
        },
        Err(e) => e,
    }
}

fn main(root: Root) {
    let out = root.stdout
    out.print(show("a+b", "xaaab"))
    out.print(show("(\\d+)-(\\d+)?", "tel 12- or 3-4"))
    out.print(show("a|ab", "ab"))
    out.print(show("(?i)hello", "say HeLLo"))
    out.print(show("x*", "abc"))
    out.print(show("é+.", "aééz"))
    out.print(show("[^a]", "aé"))
    out.print(show("\\bfoo\\b", "foobar foo"))
    out.print(show("a\{2,3\}?", "aaaa"))
    out.print(show("(a*)*b", "aaab"))
    out.print(show("[", "x"))
    out.print(show("a)", "x"))
    out.print(show("(?=a)", "x"))
    out.print(show("*a", "x"))
    let r = regex.must("(?P<y>\\d\{4\})-(?P<m>\\d\\d)")
    out.print("{r.matches("2026-11")} {r.matches("2026-11x")} {regex.must("a|ab").matches("ab")}")
    let m = r.find("on 2026-11-11") ?? regex.Match { text: "", start: 0, end: 0, groups: [], names: [] }
    out.print("{m.named("y")} {m.named("m")} {m.named("z")} {m.capture(5)}")
    out.print("{regex.must("\\d+").find_all("a1b22c333").map(|m| m.text)}")
    out.print("{regex.must("a*").find_all("baaac").map(|m| "{m.start}:{m.text}")}")
    out.print(r.replace("from 2026-11 to 2027-01", "$\{m\}/$y $$1"))
    out.print(regex.must("o").replace_first("foo", "0"))
    out.print(regex.must("\\w+").replace_with("hi there", |m| m.text.to_upper()))
    out.print("{regex.must(",\\s*").split("a, b,c,")} {regex.must("").split("abc")} {regex.must("x").split("")}")
    out.print(regex.escape("1+1=2? (yes)"))
    out.print("{regex.must("(?m)^\\w+$").find_all("one\ntwo\n").map(|m| m.text)} {regex.must("^\\w+$").find_all("one\ntwo").len()}")
    // no catastrophic backtracking
    let evil = regex.must("(a+)+$")
    let mut s = ""
    let mut i = 0
    while i < 30 {
        s = s + "a"
        i = i + 1
    }
    out.print("{evil.matches(s + "b")} {evil.find(s + "b").map(|m| m.end)}")
}
|v}
    "a+b on xaaab: aaab at 1..5 [Some(\"aaab\")]\n(\\d+)-(\\d+)? on tel 12- or 3-4: 12- at 4..7 [Some(\"12-\"), Some(\"12\"), None]\na|ab on ab: a at 0..1 [Some(\"a\")]\n(?i)hello on say HeLLo: HeLLo at 4..9 [Some(\"HeLLo\")]\nx* on abc:  at 0..0 [Some(\"\")]\n\195\169+. on a\195\169\195\169z: \195\169\195\169z at 1..6 [Some(\"\\195\\169\\195\\169z\")]\n[^a] on a\195\169: \195\169 at 1..3 [Some(\"\\195\\169\")]\n\\bfoo\\b on foobar foo: foo at 7..10 [Some(\"foo\")]\na{2,3}? on aaaa: aa at 0..2 [Some(\"aa\")]\n(a*)*b on aaab: aaab at 0..4 [Some(\"aaab\"), Some(\"aaa\")]\nregex: missing `]` at byte 0\nregex: unmatched `)`\nregex: unsupported group syntax (no lookaround or backreferences) at byte 0\nregex: a quantifier with nothing to repeat\ntrue false true\nSome(\"2026\") Some(\"11\") None None\n[\"1\", \"22\", \"333\"]\n[\"0:\", \"1:aaa\", \"5:\"]\nfrom 11/$y $1 to 01/$y $1\nf0o\nHI THERE\n[\"a\", \"b\", \"c\", \"\"] [\"a\", \"b\", \"c\"] [\"\"]\n1\\+1=2\\? \\(yes\\)\n[\"one\", \"two\"] 0\nfalse None\n";
  out "std-logscan-example" (example "logscan.tsr")
    "09:15:02 GET  /index.html 200     5,120\n09:15:09 GET  /missing    404         0\nskipped: not a log line\n09:01:44 POST /api/orders 201 1,048,576\ntotal 1,053,696 bytes\ndue 01/12/2026, paid 30/11/2026\n[\"a\", \"b\", \"c\", \"d\"]\n";
  panics "std-regex-must" "use regex\n\nfn main(root: Root) {\n    let r = regex.must(\"(a\")\n    root.stdout.print(r.pattern)\n}\n" "regex: missing `)`";
  out "std-http-keepalive-stream" ~grants:{ Interp.no_grants with listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1" ] }
    {v|use http

fn route(r: http.Request) -> http.Response {
    if r.path == "/boom" {
        let xs: List[Int] = []
        http.text(200, "{xs[1]}")
    } else {
        http.text(200, "{r.method} {r.path} {r.version} body={r.body}")
    }
}

fn stream(r: http.Request, c: Conn) -> Result[(), IoError] {
    if r.path == "/fail" {
        return Err(Failed("handler gave up"))
    }
    http.write_head(c, 200, [http.Header { name: "Content-Type", value: "text/plain" }])?
    for w in ["one ", "two ", "", "three"] {
        http.write_chunk(c, w)?
    }
    http.end_body(c)
}

// Send raw bytes, then read until the server closes the connection.
fn raw(net: Net, port: Int, data: Str) -> Str {
    match net.connect("127.0.0.1:{port}") {
        Ok(c) => {
            let _ = c.write(data)
            let mut got = ""
            let mut open = true
            while open {
                match c.read() {
                    Ok(s) => if s == "" { open = false } else { got = got + s },
                    Err(_) => { open = false },
                }
            }
            c.close()
            got.replace("\r\n", "|")
        },
        Err(e) => "{e}",
    }
}

fn raw_client(net: Net, port: Int) -> Str {
    let pipelined = "GET /a HTTP/1.1\r\nHost: x\r\n\r\nPOST /b HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nabc\r\n2;ext=1\r\nde\r\n0\r\nX-Trailer: 1\r\n\r\nGET /boom HTTP/1.1\r\n\r\nGET /c HTTP/1.1\r\nConnection: close\r\n\r\n"
    [
        raw(net, port, pipelined),
        raw(net, port, "GET /old HTTP/1.0\r\n\r\n"),
        raw(net, port, "HEAD /h HTTP/1.1\r\nConnection: close\r\n\r\n"),
        raw(net, port, "BAD\r\n\r\n"),
    ].join("\n")
}

fn fetch_client(net: Net, p2: Int, p3: Int) -> Str {
    let mut log = []
    match http.get(net, "http://127.0.0.1:{p2}/x") {
        Ok(r) => log.push("{r.status} {r.header("connection") ?? "-"} {r.body}"),
        Err(e) => log.push("{e}"),
    }
    match http.post(net, "http://127.0.0.1:{p2}/y", "text/plain", "hello") {
        Ok(r) => log.push("{r.status} {r.body}"),
        Err(e) => log.push("{e}"),
    }
    match http.request_fold(net, "GET", "http://127.0.0.1:{p3}/s", [], "", "", |a, s| a + s) {
        Ok((r, body)) => log.push("{r.status} {r.header("transfer-encoding") ?? "-"} {body}"),
        Err(e) => log.push("{e}"),
    }
    match http.request_fold(net, "GET", "http://127.0.0.1:{p3}/s", [], "", 0, |n, s| n + s.len()) {
        Ok((_, n)) => log.push("{n} bytes"),
        Err(e) => log.push("{e}"),
    }
    match http.get(net, "http://127.0.0.1:{p3}/fail") {
        Ok(r) => log.push("{r.status}"),
        Err(e) => log.push("{e}"),
    }
    log.join("\n")
}

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let net = root.net
    let l = root.listen.on("127.0.0.1:0")?
    let port = l.port()
    let first = group g {
        g.go(|| "served {http.serve(l, 4, route)}")
        g.go(|| raw_client(net, port))
    }
    out.print(first.join("\n"))
    let l2 = root.listen.on("127.0.0.1:0")?
    let l3 = root.listen.on("127.0.0.1:0")?
    let p2 = l2.port()
    let p3 = l3.port()
    let second = group g {
        g.go(|| "forever: {http.serve_forever(l2, route)}")
        g.go(|| "stream: {http.accept_forever(l3, |c| http.serve_stream(c, stream))}")
        g.go(|| {
            let log = fetch_client(net, p2, p3)
            l2.close()
            l3.close()
            log
        })
    }
    out.print(second.join("\n"))
    Ok(())
}
|v}
    "served 6\nHTTP/1.1 200 OK|Content-Type: text/plain; charset=utf-8|Connection: keep-alive|Content-Length: 21||GET /a HTTP/1.1 body=HTTP/1.1 200 OK|Content-Type: text/plain; charset=utf-8|Connection: keep-alive|Content-Length: 27||POST /b HTTP/1.1 body=abcdeHTTP/1.1 500 Internal Server Error|Content-Type: text/plain; charset=utf-8|Connection: keep-alive|Content-Length: 14||internal errorHTTP/1.1 200 OK|Content-Type: text/plain; charset=utf-8|Connection: close|Content-Length: 21||GET /c HTTP/1.1 body=\nHTTP/1.1 200 OK|Content-Type: text/plain; charset=utf-8|Connection: close|Content-Length: 23||GET /old HTTP/1.0 body=\nHTTP/1.1 200 OK|Content-Type: text/plain; charset=utf-8|Connection: close|Content-Length: 22||\nHTTP/1.1 400 Bad Request|Content-Type: text/plain; charset=utf-8|Connection: close|Content-Length: 12||bad request\n\nforever: Failed(\"listener is closed\")\nstream: Failed(\"listener is closed\")\n200 close GET /x HTTP/1.1 body=\n200 POST /y HTTP/1.1 body=hello\n200 chunked one two three\n13 bytes\nFailed(\"http: connection closed before the response\")\n";
  check_tsan "std-http-keepalive-stream"
    ~grants:{ Interp.no_grants with listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1" ] }
    {v|use http

fn route(r: http.Request) -> http.Response {
    if r.path == "/boom" {
        let xs: List[Int] = []
        http.text(200, "{xs[1]}")
    } else {
        http.text(200, "{r.method} {r.path} {r.version} body={r.body}")
    }
}

fn stream(r: http.Request, c: Conn) -> Result[(), IoError] {
    if r.path == "/fail" {
        return Err(Failed("handler gave up"))
    }
    http.write_head(c, 200, [http.Header { name: "Content-Type", value: "text/plain" }])?
    for w in ["one ", "two ", "", "three"] {
        http.write_chunk(c, w)?
    }
    http.end_body(c)
}

// Send raw bytes, then read until the server closes the connection.
fn raw(net: Net, port: Int, data: Str) -> Str {
    match net.connect("127.0.0.1:{port}") {
        Ok(c) => {
            let _ = c.write(data)
            let mut got = ""
            let mut open = true
            while open {
                match c.read() {
                    Ok(s) => if s == "" { open = false } else { got = got + s },
                    Err(_) => { open = false },
                }
            }
            c.close()
            got.replace("\r\n", "|")
        },
        Err(e) => "{e}",
    }
}

fn raw_client(net: Net, port: Int) -> Str {
    let pipelined = "GET /a HTTP/1.1\r\nHost: x\r\n\r\nPOST /b HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nabc\r\n2;ext=1\r\nde\r\n0\r\nX-Trailer: 1\r\n\r\nGET /boom HTTP/1.1\r\n\r\nGET /c HTTP/1.1\r\nConnection: close\r\n\r\n"
    [
        raw(net, port, pipelined),
        raw(net, port, "GET /old HTTP/1.0\r\n\r\n"),
        raw(net, port, "HEAD /h HTTP/1.1\r\nConnection: close\r\n\r\n"),
        raw(net, port, "BAD\r\n\r\n"),
    ].join("\n")
}

fn fetch_client(net: Net, p2: Int, p3: Int) -> Str {
    let mut log = []
    match http.get(net, "http://127.0.0.1:{p2}/x") {
        Ok(r) => log.push("{r.status} {r.header("connection") ?? "-"} {r.body}"),
        Err(e) => log.push("{e}"),
    }
    match http.post(net, "http://127.0.0.1:{p2}/y", "text/plain", "hello") {
        Ok(r) => log.push("{r.status} {r.body}"),
        Err(e) => log.push("{e}"),
    }
    match http.request_fold(net, "GET", "http://127.0.0.1:{p3}/s", [], "", "", |a, s| a + s) {
        Ok((r, body)) => log.push("{r.status} {r.header("transfer-encoding") ?? "-"} {body}"),
        Err(e) => log.push("{e}"),
    }
    match http.request_fold(net, "GET", "http://127.0.0.1:{p3}/s", [], "", 0, |n, s| n + s.len()) {
        Ok((_, n)) => log.push("{n} bytes"),
        Err(e) => log.push("{e}"),
    }
    match http.get(net, "http://127.0.0.1:{p3}/fail") {
        Ok(r) => log.push("{r.status}"),
        Err(e) => log.push("{e}"),
    }
    log.join("\n")
}

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let net = root.net
    let l = root.listen.on("127.0.0.1:0")?
    let port = l.port()
    let first = group g {
        g.go(|| "served {http.serve(l, 4, route)}")
        g.go(|| raw_client(net, port))
    }
    out.print(first.join("\n"))
    let l2 = root.listen.on("127.0.0.1:0")?
    let l3 = root.listen.on("127.0.0.1:0")?
    let p2 = l2.port()
    let p3 = l3.port()
    let second = group g {
        g.go(|| "forever: {http.serve_forever(l2, route)}")
        g.go(|| "stream: {http.accept_forever(l3, |c| http.serve_stream(c, stream))}")
        g.go(|| {
            let log = fetch_client(net, p2, p3)
            l2.close()
            l3.close()
            log
        })
    }
    out.print(second.join("\n"))
    Ok(())
}
|v}
    "served 6\nHTTP/1.1 200 OK|Content-Type: text/plain; charset=utf-8|Connection: keep-alive|Content-Length: 21||GET /a HTTP/1.1 body=HTTP/1.1 200 OK|Content-Type: text/plain; charset=utf-8|Connection: keep-alive|Content-Length: 27||POST /b HTTP/1.1 body=abcdeHTTP/1.1 500 Internal Server Error|Content-Type: text/plain; charset=utf-8|Connection: keep-alive|Content-Length: 14||internal errorHTTP/1.1 200 OK|Content-Type: text/plain; charset=utf-8|Connection: close|Content-Length: 21||GET /c HTTP/1.1 body=\nHTTP/1.1 200 OK|Content-Type: text/plain; charset=utf-8|Connection: close|Content-Length: 23||GET /old HTTP/1.0 body=\nHTTP/1.1 200 OK|Content-Type: text/plain; charset=utf-8|Connection: close|Content-Length: 22||\nHTTP/1.1 400 Bad Request|Content-Type: text/plain; charset=utf-8|Connection: close|Content-Length: 12||bad request\n\nforever: Failed(\"listener is closed\")\nstream: Failed(\"listener is closed\")\n200 close GET /x HTTP/1.1 body=\n200 POST /y HTTP/1.1 body=hello\n200 chunked one two three\n13 bytes\nFailed(\"http: connection closed before the response\")\n";
  (* ---------- TLS ----------
     test/tls holds a test CA (ca.pem, trusted through SSL_CERT_FILE, set at
     the top of this file), a server certificate for localhost and
     127.0.0.1, one for another name, and a self-signed one nobody trusts.
     The programs read them through an Fs grant. *)
  let tls_grants =
    { Interp.no_grants with fs = Some (tls_dir, true); listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1"; "localhost" ] }
  in
  let tls_conn_src = {v|// Read until a "." arrives, or the peer closes or fails.
fn read_msg(c: Conn) -> Result[Str, IoError] {
    let mut got = ""
    while !got.ends_with(".") {
        let s = c.read()?
        if s == "" {
            return Ok(got)
        }
        got = got + s
    }
    Ok(got)
}

fn serve(l: Listener, n: Int) -> Str {
    let mut log = []
    let mut i = 0
    while i < n {
        match l.accept() {
            Ok(c) => {
                match read_msg(c) {
                    Ok(s) => { let _ = c.write("echo {s.len()}") },
                    Err(_) => log.push("handshake failed"),
                }
                c.close()
            },
            Err(e) => log.push("{e}"),
        }
        i = i + 1
    }
    log.join(", ")
}

fn big(n: Int) -> Str {
    let mut s = "x"
    while s.len() < n {
        s = s + s
    }
    s + "."
}

fn call(net: Net, addr: Str, msg: Str) -> Str {
    match net.connect_tls(addr) {
        Ok(c) => {
            let r = match c.write(msg) {
                Ok(_) => "{c.read()}",
                Err(e) => "{e}",
            }
            c.close()
            r
        },
        Err(e) => "{e}",
    }
}

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let fs = root.fs
    let net = root.net
    let l = root.listen.on_tls("127.0.0.1:0", fs.read("server.pem")?, fs.read("server-key.pem")?)?
    let untrusted = root.listen.on_tls("127.0.0.1:0", fs.read("untrusted.pem")?, fs.read("untrusted-key.pem")?)?
    let other = root.listen.on_tls("127.0.0.1:0", fs.read("other.pem")?, fs.read("other-key.pem")?)?
    let plain = root.listen.on("127.0.0.1:0")?
    let port = l.port()
    let got = group g {
        g.go(|| serve(l, 2))
        g.go(|| serve(untrusted, 1))
        g.go(|| serve(other, 1))
        g.go(|| {
            // read the client's hello first, so closing sends no reset
            match plain.accept() {
                Ok(c) => { let _ = c.read(); c.close() },
                Err(_) => {},
            }
            ""
        })
        g.go(|| call(net, "127.0.0.1:{port}", "hello."))
        g.go(|| call(net, "localhost:{port}", big(300000)))
        g.go(|| call(net, "localhost:{untrusted.port()}", "x."))
        g.go(|| call(net, "127.0.0.1:{other.port()}", "x."))
        g.go(|| call(net, "127.0.0.1:{plain.port()}", "x."))
    }
    for r in got {
        out.print(r)
    }
    // a read waiting on a TLS connection is cancelled by the group's deadline
    let quiet = group g {
        g.go(|| {
            match l.accept() {
                Ok(c) => { let _ = read_msg(c); c.close() },
                Err(_) => {},
            }
        })
        g.go(|| {
            match net.connect_tls("127.0.0.1:{port}") {
                Ok(c) => {
                    let waited = group w until root.clock.after(50) {
                        w.go(|| "{c.read()}")
                    }
                    c.close()
                    "{waited}"
                },
                Err(e) => "{e}",
            }
        })
    }
    out.print("{quiet}")
    out.print("{root.listen.on_tls("127.0.0.1:0", "not a certificate", "")}")
    out.print("{root.listen.on_tls("127.0.0.1:0", fs.read("server.pem")?, fs.read("other-key.pem")?)}")
    out.print("{root.listen.on_tls("127.0.0.1:1", fs.read("server.pem")?, fs.read("server-key.pem")?)}")
    out.print("{net.connect_tls("example.com:443")}")
    out.print("{net.connect_tls("nonsense")}")
    l.close()
    Ok(())
}
|v} in
  let tls_conn_want = "\nhandshake failed\nhandshake failed\n\nOk(\"echo 6\")\nOk(\"echo 524289\")\nFailed(\"tls: certificate verify failed: self-signed certificate\")\nFailed(\"tls: certificate verify failed: IP address mismatch\")\nFailed(\"tls: the connection was closed during the handshake\")\n[\"[None]\"]\nErr(Failed(\"tls: no PEM certificate in the certificate text\"))\nErr(Failed(\"tls: the private key does not match the certificate\"))\nErr(Denied(\"listen: 127.0.0.1:1 is not granted\"))\nErr(Denied(\"net: example.com:443 is not granted\"))\nErr(Failed(\"address must be HOST:PORT\"))\n" in
  out "tls-conn" ~grants:tls_grants tls_conn_src tls_conn_want;
  check_tsan "tls-conn" ~grants:tls_grants tls_conn_src tls_conn_want;
  let tls_http_src = {v|use http

fn route(r: http.Request) -> http.Response {
    http.text(200, "{r.method} {r.path} {r.version} body={r.body}")
}

fn stream(r: http.Request, c: Conn) -> Result[(), IoError] {
    http.write_head(c, 200, [http.Header { name: "Content-Type", value: "text/plain" }])?
    for w in ["one ", "two ", "three"] {
        http.write_chunk(c, w)?
    }
    http.end_body(c)
}

// Two requests on one kept-alive TLS connection, written at once.
fn pipelined(net: Net, port: Int) -> Str {
    match net.connect_tls("localhost:{port}") {
        Ok(c) => {
            let _ = c.write("GET /a HTTP/1.1\r\nHost: x\r\n\r\nGET /b HTTP/1.1\r\nConnection: close\r\n\r\n")
            let mut got = ""
            let mut open = true
            while open {
                match c.read() {
                    Ok(s) => if s == "" { open = false } else { got = got + s },
                    Err(e) => { got = got + "{e}"; open = false },
                }
            }
            c.close()
            got.split("\r\n\r\n").len().to_str()
        },
        Err(e) => "{e}",
    }
}

fn client(net: Net, p1: Int, p2: Int) -> Str {
    let mut log = []
    match http.get(net, "https://127.0.0.1:{p1}/x") {
        Ok(r) => log.push("{r.status} {r.body}"),
        Err(e) => log.push("{e}"),
    }
    match http.post(net, "https://localhost:{p1}/y", "text/plain", "hello") {
        Ok(r) => log.push("{r.status} {r.body}"),
        Err(e) => log.push("{e}"),
    }
    match http.request_fold(net, "GET", "https://127.0.0.1:{p2}/s", [], "", "", |a, s| a + s) {
        Ok((r, body)) => log.push("{r.status} {r.header("transfer-encoding") ?? "-"} {body}"),
        Err(e) => log.push("{e}"),
    }
    match net.get("https://localhost:{p1}/net?q=1") {
        Ok(r) => log.push("{r.status} {r.headers.get("content-type")} {r.body}"),
        Err(e) => log.push("{e}"),
    }
    log.push(pipelined(net, p1))
    match http.get(net, "ftp://127.0.0.1/") {
        Ok(r) => log.push("{r.status}"),
        Err(e) => log.push("{e}"),
    }
    log.join("\n")
}

fn main(root: Root) -> Result[(), IoError] {
    let cert = root.fs.read("server.pem")?
    let key = root.fs.read("server-key.pem")?
    let l1 = root.listen.on_tls("127.0.0.1:0", cert, key)?
    let l2 = root.listen.on_tls("127.0.0.1:0", cert, key)?
    let p1 = l1.port()
    let p2 = l2.port()
    let net = root.net
    let done = group g {
        g.go(|| "forever: {http.serve_forever(l1, route)}")
        g.go(|| "stream: {http.accept_forever(l2, |c| http.serve_stream(c, stream))}")
        g.go(|| {
            let log = client(net, p1, p2)
            l1.close()
            l2.close()
            log
        })
    }
    root.stdout.print(done.join("\n"))
    Ok(())
}
|v} in
  let tls_http_want = "forever: Failed(\"listener is closed\")\nstream: Failed(\"listener is closed\")\n200 GET /x HTTP/1.1 body=\n200 POST /y HTTP/1.1 body=hello\n200 chunked one two three\n200 Some(\"text/plain; charset=utf-8\") GET /net?q=1 HTTP/1.1 body=\n3\nFailed(\"http: the URL must start with http:// or https://\")\n" in
  out "tls-http" ~grants:tls_grants tls_http_src tls_http_want;
  check_tsan "tls-http" ~grants:tls_grants tls_http_src tls_http_want;
  (* A TLS stream that ends without the close message may have been cut
     short: Conn.read says so, and Net.get accepts it only when the response
     is framed (Content-Length or chunked) and complete. *)
  let tls_cut_src = {v|fn main(root: Root) {
    let out = root.stdout
    let port = root.args()[0]
    let net = root.net
    match net.get("https://127.0.0.1:{port}/framed") {
        Ok(r) => out.print("{r.status} {r.body}"),
        Err(e) => out.print("{e}"),
    }
    match net.get("https://127.0.0.1:{port}/close") {
        Ok(r) => out.print("{r.status} {r.body}"),
        Err(e) => out.print("{e}"),
    }
    match net.connect_tls("127.0.0.1:{port}") {
        Ok(c) => {
            let _ = c.write("GET /raw HTTP/1.1\r\n\r\n")
            let mut got = ""
            let mut last = Ok("")
            let mut open = true
            while open {
                match c.read() {
                    Ok(s) => if s == "" { open = false } else { got = got + s },
                    Err(e) => { last = Err(e); open = false },
                }
            }
            c.close()
            out.print("{got.len()} {last}")
        },
        Err(e) => out.print("{e}"),
    }
}
|v} in
  let tls_cut_want =
    "200 hello\nFailed(\"tls: the connection was closed without a TLS close message, so the data may be cut short\")\n30 Err(Failed(\"tls: the connection was closed without a TLS close message, so the data may be cut short\"))\n"
  in
  (if selected "tls-cut" then
     let runs = if native_for "tls-cut" then 9 else 3 in
     let ic =
       Unix.open_process_args_in "python3"
         [| "python3"; Filename.concat tls_dir "cut_server.py"; Filename.concat tls_dir "server.pem";
            Filename.concat tls_dir "server-key.pem"; string_of_int runs |]
     in
     let port = String.trim (input_line ic) in
     let grants = { Interp.no_grants with net = [ "127.0.0.1" ]; args = [ port ] } in
     out "tls-cut" ~grants tls_cut_src tls_cut_want;
     check_tsan "tls-cut" ~grants tls_cut_src tls_cut_want;
     ignore (Unix.close_process_in ic));
  (* ---------- Rand, Env, args and the Fs additions ---------- *)
  let box = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "tessera-box-%d" (Unix.getpid ())) in
  (try Unix.mkdir box 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let box = Unix.realpath box in
  Unix.putenv "TESSERA_TEST_VISIBLE" "yes";
  Unix.putenv "TESSERA_TEST_HIDDEN" "no";
  out "caps-rand-env-args-fs"
    ~grants:{ Interp.no_grants with seed = Some 42; env = [ "TESSERA_TEST_VISIBLE" ]; args = [ "one"; "two" ]; fs = Some (box, false) }
    (String.concat "\n"
       [ "fn main(root: Root) -> Result[(), IoError] {";
         "    let out = root.stdout";
         "    let r = root.rand";
         "    let xs = [r.int(1, 7), r.int(1, 7), r.int(1, 7), r.int(-5, -4), r.int(0, 1000000)]";
         "    out.print(\"{xs} {r.float() < 1.0}\")";
         "    out.print(\"{root.args()} {root.env.get(\"TESSERA_TEST_VISIBLE\")} {root.env.get(\"TESSERA_TEST_HIDDEN\")}\")";
         "    let fs = root.fs";
         "    fs.make_dir(\"d\")?";
         "    fs.write(\"d/a.txt\", \"a\")?";
         "    fs.append(\"d/a.txt\", \"b\")?";
         "    fs.append(\"d/new.txt\", \"n\")?";
         "    out.print(\"{fs.list(\"d\")?} {fs.read(\"d/a.txt\")?} {fs.is_dir(\"d\")} {fs.is_dir(\"d/a.txt\")}\")";
         "    out.print(\"{fs.make_dir(\"d\").is_err()} {fs.remove(\"d\").is_err()} {fs.remove(\"\").is_err()} {fs.list(\"d/a.txt\").is_err()}\")";
         "    fs.remove(\"d/a.txt\")?";
         "    fs.remove(\"d/new.txt\")?";
         "    fs.remove(\"d\")?";
         "    out.print(\"{fs.exists(\"d\")} {fs.read_only().append(\"x\", \"y\")} {fs.remove(\"../x\")}\")";
         "    Ok(())";
         "}";
         "" ])
    "[2, 2, 1, -5, 963250] true\n[\"one\", \"two\"] Some(\"yes\") None\n[\"a.txt\", \"new.txt\"] ab true false\ntrue true true true\nfalse Err(Denied(\"fs: this capability is read-only\")) Err(Denied(\"fs: path escapes the granted directory\"))\n";
  panics "rand-empty-range" (main_wrap "    out.print(\"{root.rand.int(3, 3)}\")") "rand.int: empty range 3..3";
  errs "interp-escaped-quote" (main_wrap "    out.print(\"{\\\"x\\\".len()}\")\n    out.print(\"later\")") [ "E0006" ];
  ok "unknown-method-lists-methods"
    (let _, ds, _ = compile "unknown-method" (main_wrap "    let n = [1].size_of()") in
     List.exists (fun (d : Diag.t) -> List.exists (fun n -> String.starts_with ~prefix:"`List[Int]` has: " n) d.notes) ds)
    "no method list";
  errs "ambient-random" (main_wrap "    let x = random(1, 2)") [ "E0302" ];

  (* ---------- methods ---------- *)
  out "methods" (example "methods.tsr") "8.0 12.0\ncircle at 8.0,12.0 with area 3.0\nrect with area 96.0\none 1 1\n10 25\n";
  errs "method-on-capability" "fn twice(self: Clock) -> Int {\n    self.now_ms() * 2\n}\n\nfn main() {}\n" [ "E0110" ];
  errs "method-self-not-first" "struct P {\n    a: Int,\n}\n\nfn f(x: Int, self: P) -> Int {\n    x\n}\n\nfn main() {}\n" [ "E0110" ];
  errs "method-on-builtin-pub" "pub fn double(self: Int) -> Int {\n    self * 2\n}\n\nfn main() {}\n" [ "E0110" ];
  errs "method-on-builtin-clash" "fn len(self: Str) -> Int {\n    0\n}\n\nfn main() {}\n" [ "E0106" ];
  out "methods-on-builtins"
    ("fn shout(self: Str) -> Str {\n    self.to_upper() + \"!\"\n}\n\nfn double(self: Int) -> Int {\n    self * 2\n}\n\n"
     ^ "fn second[T](self: List[T]) -> Option[T] {\n    self.get(1)\n}\n\nfn or_zero(self: Option[Int]) -> Int {\n    self ?? 0\n}\n\n"
     ^ "fn total[K](self: Map[K, Int]) -> Int {\n    self.values().sum()\n}\n\n"
     ^ "fn describe(self: IoError) -> Str {\n    match self {\n        Denied(m) => \"denied: \" + m,\n        Failed(m) => \"failed: \" + m,\n    }\n}\n\n"
     ^ "fn half(self: Float) -> Float {\n    self / 2.0\n}\n\nfn flip(self: Bool) -> Bool {\n    !self\n}\n\n"
     ^ "fn main(root: Root) {\n    let out = root.stdout\n    let f = \"hi\".shout\n"
     ^ "    out.print(\"{\"hey\".shout()} {21.double().double()} {[1, 2, 3].second()} {[\"a\"].second()} {f()}\")\n"
     ^ "    out.print(\"{Some(4).or_zero()} {None.or_zero()} {map([(\"a\", 1), (\"b\", 2)]).total()} {Denied(\"x\").describe()}\")\n"
     ^ "    out.print(\"{[1, 2].map(|x| x.double())} {3.0.half()} {true.flip()}\")\n}\n")
    "HEY! 84 Some(2) None HI!\n4 0 3 denied: x\n[2, 4] 1.5 false\n";
  (* each file's methods on built-in types are its own *)
  let strs = "pub fn yell(s: Str) -> Str {\n    s.shout()\n}\n\nfn shout(self: Str) -> Str {\n    self + \"!!\"\n}\n" in
  out "methods-on-builtins-per-module" ~modules:[ ("strs", strs) ]
    "use strs\n\nfn shout(self: Str) -> Str {\n    self + \"?\"\n}\n\nfn main(root: Root) {\n    root.stdout.print(\"{\"a\".shout()} {strs.yell(\"b\")}\")\n}\n"
    "a? b!!\n";
  errs "methods-on-builtins-not-imported" ~modules:[ ("strs", strs) ]
    "use strs\n\nfn main(root: Root) {\n    root.stdout.print(\"a\".shout())\n}\n" [ "E0105" ];
  let pdecl = "struct P {\n    a: Int,\n}\n\nfn g(self: P) -> Int {\n    self.a\n}\n\n" in
  errs "method-unknown" (pdecl ^ "fn main() {\n    let x = P { a: 1 }.h()\n}\n") [ "E0105" ];
  errs "method-arity" (pdecl ^ "fn main() {\n    let x = P { a: 1 }.g(2)\n}\n") [ "E0107" ];
  errs "method-as-function" (pdecl ^ "fn main() {\n    let x = g(P { a: 1 })\n}\n") [ "E0101" ];
  errs "method-twice" (pdecl ^ "fn g(self: P) -> Int {\n    2\n}\n\nfn main() {}\n") [ "E0106" ];
  let geo = "pub struct V {\n    x: Int,\n}\n\npub fn len2(self: V) -> Int {\n    self.x * self.x\n}\n\npub fn make(x: Int) -> V {\n    V { x }.twice()\n}\n\nfn twice(self: V) -> V {\n    V { x: self.x * 2 }\n}\n" in
  out "methods-across-modules" ~modules:[ ("geo", geo) ] "use geo\n\nfn main(root: Root) {\n    root.stdout.print(\"{geo.make(3).len2()}\")\n}\n" "36\n";
  errs "methods-private" ~modules:[ ("geo", geo) ] "use geo\n\nfn main(root: Root) {\n    let v = geo.make(3).twice()\n}\n" [ "E0402" ];

  (* ---------- interfaces ---------- *)
  out "interfaces" (example "interfaces.tsr") "15.0 9.0\nsquare of area 4.0\nbox of area 1.0, box of area 1.0\n500 true\n";
  out "generics-example" (example "generics.tsr") "Some(\"Emma\") Some(3)\n2 Some(\"Dune\")\n0 None\n[1, 2, 5, 7]\n28 30\n";
  let shape = "interface Shape {\n    fn area(self) -> Float\n}\n\nstruct Blob {\n    n: Int,\n}\n\nfn area(self: Blob) -> Int {\n    self.n\n}\n\nfn total[T: Shape](xs: List[T]) -> Float {\n    xs.map(|s| s.area()).fold(0.0, |a, b| a + b)\n}\n\n" in
  errs "iface-wrong-signature" (shape ^ "fn main() {\n    let a = total([Blob { n: 1 }])\n}\n") [ "E0111" ];
  errs "iface-builtin-missing" (shape ^ "fn main() {\n    let a = total([1, 2])\n}\n") [ "E0111" ];
  errs "iface-unbounded-method" (shape ^ "fn loose[T](x: T) -> Float {\n    x.area()\n}\n\nfn main() {}\n") [ "E0105" ];
  errs "iface-as-type" (shape ^ "fn bad(s: Shape) {}\n\nfn main() {}\n") [ "E0102" ];
  errs "iface-unknown-bound" (shape ^ "fn bad[T: Shpe](x: T) {}\n\nfn main() {}\n") [ "E0102" ];
  errs "iface-forward-unbounded" (shape ^ "fn outer[T](xs: List[T]) -> Float {\n    total(xs)\n}\n\nfn main() {}\n") [ "E0111" ];
  errs "iface-bound-on-struct" "interface S {\n    fn a(self) -> Int\n}\n\nstruct B[T: S] {\n    x: T,\n}\n\nfn main() {}\n" [];
  (* an interface from another module, satisfied by a type in this one *)
  let geo = "pub interface Area {\n    fn area(self) -> Int\n}\n\npub fn twice[T: Area](x: T) -> Int {\n    x.area() * 2\n}\n" in
  out "iface-across-modules" ~modules:[ ("geo", geo) ]
    "use geo\n\nstruct Sq {\n    s: Int,\n}\n\nfn area(self: Sq) -> Int {\n    self.s * self.s\n}\n\nfn show[T: geo.Area](x: T) -> Int {\n    geo.twice(x) + x.area()\n}\n\nfn main(root: Root) {\n    root.stdout.print(\"{show(Sq { s: 3 })}\")\n}\n"
    "27\n";
  (* generic interfaces: arguments named in bounds, inferred from the type's methods; dyn with arguments *)
  out "iface-generic" "interface Container[T] {\n    fn get(self, i: Int) -> Option[T]\n    fn len(self) -> Int\n}\n\nstruct Bag {\n    items: List[Str],\n}\n\nfn get(self: Bag, i: Int) -> Option[Str] {\n    self.items.get(i)\n}\n\nfn len(self: Bag) -> Int {\n    self.items.len()\n}\n\nfn first[C: Container[T], T](c: C) -> Option[T] {\n    c.get(0)\n}\n\nfn count[C: Container[Int]](c: C) -> Int {\n    let mut total = 0\n    for i in 0..<c.len() {\n        total = total + (c.get(i) ?? 0)\n    }\n    total\n}\n\nfn main(root: Root) {\n    let out = root.stdout\n    let b = Bag { items: [\"a\", \"b\"] }\n    let f = first(b)\n    out.print(\"{f} {first([1, 2, 3])} {count([4, 5, 6])}\")\n    let x = first(b) ?? \"none\"\n    out.print(x.to_upper())\n    let boxes: List[dyn Container[Str]] = [b, Bag { items: [] }]\n    for c in boxes {\n        out.print(\"{c.len()} {c.get(1)}\")\n    }\n}\n"
    "Some(\"a\") Some(1) 15\nA\n2 Some(\"b\")\n0 None\n";
  out "iface-generic-across-modules" ~modules:[ ("coll", "pub interface Container[T] {\n    fn get(self, i: Int) -> Option[T]\n    fn len(self) -> Int\n}\n\npub struct Sorted[T: Ordered] {\n    items: List[T],\n}\n\npub fn last[C: Container[T], T](c: C) -> Option[T] {\n    c.get(c.len() - 1)\n}\n\npub type Ints = Container[Int]\n") ]
    "use coll\n\ntype Strs = coll.Container[Str]\ntype Pair = (Int, Int)\n\nfn total[C: coll.Ints](c: C) -> Int {\n    c.len()\n}\n\nfn firsts[C: Strs](c: C) -> Str {\n    c.get(0) ?? \"-\"\n}\n\nfn pairs[C: coll.Container[Pair]](c: C) -> Int {\n    c.len()\n}\n\nfn main(root: Root) {\n    let s: coll.Sorted[Int] = coll.Sorted { items: [1] }\n    let d: dyn coll.Container[Str] = Wrap { xs: [\"q\"] }\n    root.stdout.print(\"{coll.last([1, 2])} {total([1])} {firsts([\"z\"])} {pairs([(1, 2)])} {s.items} {d.get(0)}\")\n}\n\nstruct Wrap {\n    xs: List[Str],\n}\n\nfn get(self: Wrap, i: Int) -> Option[Str] {\n    self.xs.get(i)\n}\n\nfn len(self: Wrap) -> Int {\n    self.xs.len()\n}\n"
    "Some(2) 1 z 1 [1] Some(\"q\")\n";
  (* built-in types and capabilities satisfy generic interfaces too; a bound passed on to another call *)
  out "iface-generic-builtins" "interface Lookup[T] {\n    fn get(self, key: Str) -> Option[T]\n}\n\nfn find[L: Lookup[T], T](l: L, key: Str) -> Option[T] {\n    l.get(key)\n}\n\nfn again[L: Lookup[T], T](l: L, key: Str) -> Option[T] {\n    find(l, key)\n}\n\nfn main(root: Root) {\n    let m = map([(\"a\", 1)])\n    let home = again(root.env, \"HOME_NOT_SET_XYZ\") ?? \"unset\"\n    root.stdout.print(\"{again(m, \"a\")} {find(m, \"b\")} {home}\")\n}\n" "Some(1) None unset\n";
  let cont = "interface Container[T] {\n    fn get(self, i: Int) -> Option[T]\n}\n\nstruct Blob {\n    n: Int,\n}\n\nfn get(self: Blob, i: Int) -> Option[Str] {\n    None\n}\n\n" in
  errs "iface-generic-no-args" (cont ^ "fn f[C: Container](c: C) {}\n\nfn main() {}\n") [ "E0109" ];
  errs "iface-generic-wrong-count" (cont ^ "fn f[C: Container[Int, Str]](c: C) {}\n\nfn main() {}\n") [ "E0109" ];
  errs "iface-generic-mismatch" (cont ^ "fn f[C: Container[Int]](c: C) {}\n\nfn main() {\n    f(Blob { n: 1 })\n}\n") [ "E0111" ];
  errs "iface-generic-inferred" (cont ^ "fn first[C: Container[T], T](c: C) -> Option[T] {\n    c.get(0)\n}\n\nfn main() {\n    let n: Option[Int] = first(Blob { n: 1 })\n}\n") [ "E0201" ];
  errs "iface-generic-dyn-mismatch" (cont ^ "fn main() {\n    let c: dyn Container[Int] = Blob { n: 1 }\n}\n") [ "E0111" ];
  errs "iface-generic-dyn-no-args" (cont ^ "fn f(c: dyn Container) {}\n\nfn main() {}\n") [ "E0109" ];
  errs "iface-generic-as-type" (cont ^ "fn f(c: Container[Int]) {}\n\nfn main() {}\n") [ "E0102" ];
  errs "iface-generic-bound-on-interface" (cont ^ "interface Holder[T: Container[Int]] {\n    fn get(self) -> T\n}\n\nfn main() {}\n") [ "E0003" ];
  (* bounds on struct and enum type parameters; generic array lengths *)
  out "bounds-on-types-and-array-lengths" "struct Sorted[T: Ordered] {\n    items: List[T],\n}\n\nfn insert[T](self: Sorted[T], x: T) -> Sorted[T] {\n    let mut out: List[T] = []\n    let mut done = false\n    for y in self.items {\n        if !done && x < y {\n            out.push(x)\n            done = true\n        }\n        out.push(y)\n    }\n    if !done {\n        out.push(x)\n    }\n    Sorted { items: out }\n}\n\nenum Tree[K: Key, V] {\n    Leaf,\n    Node(K, V),\n}\n\nfn key[K, V](self: Tree[K, V]) -> Map[K, Int] {\n    match self {\n        Tree.Node(k, _) => map([(k, 1)]),\n        Tree.Leaf => map([]),\n    }\n}\n\ninterface Shape {\n    fn area(self) -> Float\n}\n\nstruct Sq {\n    s: Float,\n}\n\nfn area(self: Sq) -> Float {\n    self.s * self.s\n}\n\nstruct Box[T: Shape] {\n    item: T,\n}\n\nfn double[T](self: Box[T]) -> Float {\n    self.item.area() * 2.0\n}\n\nfn sum[N](xs: Array[Int, N]) -> Int {\n    let mut t = 0\n    for x in xs {\n        t = t + x\n    }\n    t\n}\n\nfn first[T, N](xs: Array[T, N]) -> T {\n    xs[0]\n}\n\nfn size[N](xs: Array[Str, N]) -> Int {\n    xs.len()\n}\n\nfn main(root: Root) {\n    let out = root.stdout\n    let s = Sorted { items: [1, 5] }.insert(3).insert(9)\n    out.print(\"{s.items}\")\n    let t: Tree[Str, Int] = Tree.Node(\"a\", 2)\n    out.print(\"{t.key()}\")\n    out.print(\"{Box { item: Sq { s: 2.0 } }.double()}\")\n    let a: Array[Int, 3] = [1, 2, 3]\n    out.print(\"{sum(a)} {sum([10, 20])} {first(a)} {first([\"x\", \"y\"])} {size([\"a\", \"b\", \"c\", \"d\"])}\")\n}\n"
    "[1, 3, 5, 9]\n{\"a\": 1}\n8.0\n6 30 1 x 4\n";
  let boxes = "interface Shape {\n    fn area(self) -> Float\n}\n\nstruct Box[T: Shape] {\n    item: T,\n}\n\nstruct Sq {\n    s: Float,\n}\n\nfn area(self: Sq) -> Float {\n    self.s\n}\n\nstruct Blob {\n    n: Int,\n}\n\n" in
  errs "bounds-struct-literal" (boxes ^ "fn main() {\n    let b = Box { item: Blob { n: 1 } }\n}\n") [ "E0111" ];
  errs "bounds-annotation" (boxes ^ "fn main() {\n    let b: Option[Box[Blob]] = None\n}\n") [ "E0111" ];
  errs "bounds-param" (boxes ^ "fn f(b: Box[Blob]) {}\n\nfn main() {}\n") [ "E0111" ];
  errs "bounds-field" (boxes ^ "struct H {\n    b: Box[Blob],\n}\n\nfn main() {}\n") [ "E0111" ];
  errs "bounds-field-unbounded" (boxes ^ "struct H[T] {\n    b: Box[T],\n}\n\nfn main() {}\n") [ "E0111" ];
  (* the result type, and the literal in the body *)
  (* reported once per function: the result type and the literal are the same failure *)
  errs "bounds-result-needs-own" (boxes ^ "fn wrap[T](x: T) -> Box[T] {\n    Box { item: x }\n}\n\nfn main() {}\n") [ "E0111" ];
  errs "bounds-each-function"
    (boxes ^ "fn wrap[T](x: T) -> Box[T] {\n    Box { item: x }\n}\n\nfn wrap2[T](x: T) -> Box[T] {\n    Box { item: x }\n}\n\nfn main() {}\n")
    [ "E0111"; "E0111" ];
  (* an interface method's signature is checked like any written type *)
  errs "bounds-in-interface-method"
    "struct Blob {\n    f: Float,\n}\n\nstruct Keyed[T: Key] {\n    items: List[T],\n}\n\ninterface Maker {\n    fn make(self) -> Keyed[Blob]\n}\n\nfn main() {}\n"
    [ "E0111" ];
  errs "bounds-implied-by-param" (boxes ^ "fn twice[T](b: Box[T]) -> Float {\n    b.item.area() * 2.0\n}\n\nfn wrap[T: Shape](x: T) -> Box[T] {\n    Box { item: x }\n}\n\nfn main() {\n    let a = twice(wrap(Sq { s: 1.0 }))\n}\n") [  ];
  errs "bounds-variant" ("enum E[K: Key] {\n    A(K),\n}\n\nfn main() {\n    let e = E.A(1.5)\n}\n") [ "E0111" ];
  errs "bounds-ordered-implied" ("struct Sorted[T: Ordered] {\n    items: List[T],\n}\n\nfn lower[T](self: Sorted[T], x: T, y: T) -> Bool {\n    x < y\n}\n\nfn plain[T](x: T, y: T) -> Bool {\n    x < y\n}\n\nfn main() {}\n") [ "E0201" ];
  errs "array-length-param-is-type" ("fn f[N](a: N, xs: Array[Int, N]) {}\n\nfn main() {}\n") [ "E0102" ];
  errs "array-length-mismatch" ("fn dot[N](a: Array[Int, N], b: Array[Int, N]) -> Int {\n    0\n}\n\nfn main() {\n    let x: Array[Int, 2] = [1, 2]\n    let y: Array[Int, 3] = [1, 2, 3]\n    let d = dot(x, y)\n}\n") [ "E0201" ];

  (* ---------- named arguments ---------- *)
  let named_prelude =
    "struct Server { host: Str, port: Int }\n\
     fn connect(host: Str, port: Int, retries: Int) -> Str { \"{host}:{port} x{retries}\" }\n\
     fn describe(self: Server, prefix: Str, upper: Bool) -> Str {\n\
    \    let s = \"{prefix} {self.host}:{self.port}\"\n\
    \    if upper { s.to_upper() } else { s }\n\
     }\n\
     fn pad(self: Str, width: Int, fill: Str) -> Str {\n\
    \    let mut s = self\n\
    \    while s.len() < width { s = fill + s }\n\
    \    s\n\
     }\n\
     fn noisy(out: Stdout, label: Str, v: Int) -> Int {\n\
    \    out.print(label)\n\
    \    v\n\
     }\n\
     fn pair[A, B](first: A, second: B) -> (A, B) { (first, second) }\n\
     fn server(out: Stdout, host: Str) -> Server { out.print(\"server\"); Server { host, port: 1 } }\n\
     interface Scaler { fn scale(self, by: Int, plus: Int) -> Int }\n\
     struct Num { v: Int }\n\
     fn scale(self: Num, by: Int, plus: Int) -> Int { self.v * by + plus }\n\
     fn run[T: Scaler](s: T) -> Int { s.scale(plus: 1, by: 10) }\n"
  in
  let named body = named_prelude ^ main_wrap body in
  out "named-args"
    (named
       "    out.print(connect(\"a\", 1, 2))\n\
       \    out.print(connect(\"a\", retries: 3, port: 80))\n\
       \    out.print(connect(port: 1, retries: 2, host: \"h\"))\n\
       \    let s = Server { host: \"x\", port: 9 }\n\
       \    out.print(s.describe(upper: true, prefix: \"to\"))\n\
       \    out.print(\"7\".pad(fill: \"0\", width: 3))\n\
       \    let p = pair(second: \"two\", first: [1])\n\
       \    out.print(\"{p.0} {p.1}\")\n\
       \    out.print(\"{run(Num { v: 2 })}\")\n\
       \    let d: dyn Scaler = Num { v: 3 }\n\
       \    out.print(\"{d.scale(plus: 5, by: 2)}\")")
    "a:1 x2\na:80 x3\nh:1 x2\nTO X:9\n007\n[1] two\n21\n11\n";
  (* arguments run in the order they are written, receiver first *)
  out "named-args-order"
    (named
       "    out.print(connect(retries: noisy(out, \"a\", 1), port: noisy(out, \"b\", 2), host: \"z\"))\n\
       \    out.print(server(out, \"h\").describe(upper: noisy(out, \"c\", 0) == 1, prefix: \"p{noisy(out, \"d\", 0)}\"))\n\
       \    let xs = [\"x\"]\n\
       \    out.print(connect(port: xs.len(), host: xs[0], retries: noisy(out, \"e\", 3)))")
    "a\nb\nz:2 x1\nserver\nc\nd\np0 h:1\ne\nx:1 x3\n";
  out "named-args-module"
    ~modules:[ ("net2", "pub fn dial(host: Str, port: Int) -> Str { \"{host}:{port}\" }\n") ]
    "use net2\nfn main(root: Root) { root.stdout.print(net2.dial(port: 8, host: \"h\")) }\n" "h:8\n";
  errs "named-unknown" (named "    let a = connect(\"a\", 1, retry: 3)") [ "E0122" ];
  errs "named-unknown-many" (named "    let a = connect(\"a\", prt: 1, retry: 3)") [ "E0122"; "E0122" ];
  errs "named-positional-after" (named "    let a = connect(port: 1, \"h\", retries: 2)") [ "E0122" ];
  errs "named-twice" (named "    let a = connect(\"a\", host: \"b\", port: 1, retries: 2)") [ "E0122" ];
  errs "named-twice-label" (named "    let a = connect(\"a\", port: 1, port: 2)") [ "E0122" ];
  errs "named-missing" (named "    let a = connect(\"a\", port: 1)") [ "E0107" ];
  errs "named-too-many" (named "    let a = connect(\"a\", 1, 2, 3, retries: 3)") [ "E0107"; "E0122" ];
  errs "named-closure-value" (named "    let f = |x: Int| x\n    let g = f(x: 1)") [ "E0122" ];
  errs "named-variant" (named "    let h = Some(value: 1)") [ "E0122" ];
  errs "named-builtin-method" (named "    let i = \"a,b\".split(sep: \",\")") [ "E0122" ];
  errs "named-builtin-fn" (named "    assert(cond: true)") [ "E0122" ];
  errs "named-method-unknown" (named "    let s = Server { host: \"x\", port: 9 }\n    out.print(s.describe(\"p\", loud: true))") [ "E0122" ];
  errs "named-type" (named "    let a = connect(\"a\", retries: \"3\", port: 1)") [ "E0201" ];
  List.iter
    (fun (name, src) ->
      let _, ds, _ = compile name src in
      ok (name ^ "-has-fix") (List.exists (fun (d : Diag.t) -> d.fix <> None) ds) "no fix offered";
      let fixed = apply_fixes src ds in
      let _, ds2, _ = compile (name ^ "-fixed") fixed in
      ok (name ^ "-applies") (ds2 = []) (sprintf "still [%s] after fix:\n%s" (String.concat " " (codes ds2)) fixed))
    [ ("fix-named-label", named "    out.print(connect(\"a\", retry: 3, port: 80))");
      ("fix-named-positional", named "    out.print(connect(port: 1, \"h\", retries: 2))");
      ("fix-named-value", named "    let f = |x: Int| x\n    out.print(\"{f(x: 1)}\")") ];

  (* ---------- trailing closures ---------- *)
  let trail_prelude =
    "fn each_twice(xs: List[Int], f: fn(Int) -> ()) {\n\
    \    for x in xs {\n\
    \        f(x)\n\
    \        f(x)\n\
    \    }\n\
     }\n\
     fn apply(n: Int, times: Int, f: fn(Int) -> Int) -> Int {\n\
    \    let mut v = n\n\
    \    for i in 0..<times {\n\
    \        v = f(v)\n\
    \    }\n\
    \    v\n\
     }\n\
     struct P { x: Int }\n"
  in
  let trail body = trail_prelude ^ main_wrap body in
  out "trailing-closures"
    (trail
       "    let xs = [1, 2, 3]\n\
       \    out.print(\"{xs.filter { |x| x > 1 }.map { |x| x * 10 }}\")\n\
       \    each_twice([7]) { |x|\n\
       \        out.print(\"{x}\")\n\
       \    }\n\
       \    out.print(\"{apply(1, times: 3) { |v| v * 2 }}\")\n\
       \    out.print(\"{apply(f: |v| v + 1, n: 1, times: 2)}\")\n\
       \    let r = catch { panic(\"boom\") }\n\
       \    let ok = catch { 5 }\n\
       \    out.print(\"{r} {ok}\")\n\
       \    out.print(\"{xs.fold(0) { |a: Int, b: Int| a + b }}\")\n\
       \    if (xs.filter { |x| x > 2 }.len() > 0) {\n\
       \        out.print(\"in parens\")\n\
       \    }\n\
       \    let p = P { x: 1 }\n\
       \    if p.x == 1 {\n\
       \        out.print(\"struct still fine\")\n\
       \    }\n\
       \    let rs = group g {\n\
       \        g.go { out.print(\"side\") }\n\
       \        g.go { 41 + 1 }\n\
       \    }\n\
       \    out.print(\"{rs}\")")
    "[20, 30]\n7\n7\n8\n3\nErr(\"boom\") Ok(5)\n6\nin parens\nstruct still fine\nside\n[42]\n";
  errs "trailing-in-if" (trail "    let xs = [1]\n    if xs.filter { |x| x > 1 }.len() > 0 {\n        out.print(\"no\")\n    }") [ "E0003" ];
  errs "trailing-in-while" (trail "    let xs = [1]\n    while xs.map { |x| x }.len() > 9 {\n    }") [ "E0003" ];
  errs "trailing-in-for" (trail "    let xs = [1]\n    for y in xs.map { |x| x } {\n    }") [ "E0003" ];
  errs "trailing-in-match" (trail "    let xs = [1]\n    match xs.map { |x| x }.len() {\n        _ => out.print(\"no\"),\n    }") [ "E0003" ];
  errs "trailing-extra-arg" (trail "    let k = [1].map(|x| x) { |x| x }") [ "E0107" ];
  errs "trailing-not-closure-param" (trail "    let k = [1].len { 1 }") [ "E0107" ];
  errs "trailing-after-labels-fills-last" (trail "    let n = apply(times: 1, n: 2) { |v| v }\n    let m = apply(times: 1, |v| v, n: 2)") [ "E0122" ];
  (* fmt keeps the form written; empty parentheses before a trailing closure go *)
  let fmt_case_early name src want =
    let got = try Fmt.format ~file:(name ^ ".tsr") src with Diag.Fatal d -> "error: " ^ d.message in
    ok name (got = want) (sprintf "got:\n%s\nwant:\n%s" got want);
    check_fmt name src ()
  in
  fmt_case_early "fmt-trailing"
    "fn main(root: Root) {\n    let xs = [1, 2]\n    let ys = xs.map() { |x| x * 2 }\n    let r = catch() {\n        let a = 1\n        a + 1\n    }\n    let zs = xs.map(|x| { x + 1 })\n    if (xs.map { |x| x }.len() > 0) {\n        root.stdout.print(\"{ys} {r} {zs}\")\n    }\n}\n"
    "fn main(root: Root) {\n    let xs = [1, 2]\n    let ys = xs.map { |x| x * 2 }\n    let r = catch {\n        let a = 1\n        a + 1\n    }\n    let zs = xs.map(|x| { x + 1 })\n    if (xs.map { |x| x }.len() > 0) {\n        root.stdout.print(\"{ys} {r} {zs}\")\n    }\n}\n";

  (* ---------- 64-bit Int, identical in both backends ---------- *)
  out "int64" "fn main(root: Root) {\n    let out = root.stdout\n    let max = 9223372036854775807\n    let min = -max - 1\n    out.print(\"{max} {min} {max + 1} {min - 1} {max * 2} {min / -1} {min % -1}\")\n    out.print(\"{min.abs()} {(-7) / 2} {(-7) % 2} {7 % -2}\")\n    out.print(\"{4611686018427387904 * 2} {3037000500 * 3037000500}\")\n    out.print(\"{1.0e19.to_int()} {(-1.0e19).to_int()} {9.0e18.to_int()} {(0.0 / 0.0).to_int()}\")\n    out.print(\"{\"9223372036854775807\".parse_int()} {\"-9223372036854775808\".parse_int()} {\"9223372036854775808\".parse_int()}\")\n    out.print(\"{max.to_float()} {[1, 2, 3].get(max)} {\"abc\".slice(-max, max)} {[max, max].sum()}\")\n    match max {\n        9223372036854775807 => out.print(\"matched max\"),\n        _ => out.print(\"no\"),\n    }\n}\n"
    "9223372036854775807 -9223372036854775808 -9223372036854775808 9223372036854775807 -2 -9223372036854775808 0\n-9223372036854775808 -3 -1 1\n-9223372036854775808 -9223372036709301616\n9223372036854775807 -9223372036854775808 9000000000000000000 0\nSome(9223372036854775807) Some(-9223372036854775808) None\n9.22337203685478e+18 None abc -2\nmatched max\n";
  errs "int-literal-too-large" (main_wrap "    let x = 9223372036854775808") [ "E0005" ];

  (* ---------- select ---------- *)
  (* defer: every way out of a block, in both backends *)
  out "defer-exits" "fn early(out: Stdout, n: Int) -> Int {\n    defer out.print(\"early: deferred\")\n    if n > 0 {\n        return n * 2\n    }\n    out.print(\"early: end\")\n    0\n}\n\nfn parse(out: Stdout, s: Str) -> Result[Int, Str] {\n    defer out.print(\"parse {s}: done\")\n    let n = s.parse_int() ?? -1\n    if n < 0 {\n        return Err(\"bad\")\n    }\n    Ok(n)\n}\n\nfn chain(out: Stdout, s: Str) -> Result[Int, Str] {\n    defer out.print(\"chain: left\")\n    let n = parse(out, s)?\n    Ok(n + 1)\n}\n\nfn risky(out: Stdout, n: Int) -> Int {\n    let mut log = \"start\"\n    defer out.print(\"risky: {log}\")\n    log = \"middle\"\n    if n == 0 {\n        panic(\"zero\")\n    }\n    log = \"end\"\n    10 / n\n}\n\nfn main(root: Root) {\n    let out = root.stdout\n    out.print(\"{early(out, 2)} {early(out, 0)}\")\n    out.print(\"{chain(out, \"4\")} {chain(out, \"x\")}\")\n    // nested defers run in reverse\n    {\n        defer out.print(\"first\")\n        defer {\n            out.print(\"second\")\n        }\n        let x = 1\n        defer out.print(\"third x={x}\")\n    }\n    // loops: each iteration, and break / continue\n    for i in 0..<4 {\n        defer out.print(\"iter {i} done\")\n        if i == 1 {\n            continue\n        }\n        if i == 2 {\n            break\n        }\n        out.print(\"iter {i} body\")\n    }\n    out.print(\"{catch(|| risky(out, 0))} {catch(|| risky(out, 5))}\")\n    // a later let does not shadow what the defer sees\n    {\n        let y = \"outer\"\n        defer out.print(\"y={y}\")\n        let y = \"inner\"\n        out.print(\"y now {y}\")\n    }\n    let clock = root.clock\n    // the task says when its defer is in place, so the deadline cannot\n    // pass before it starts\n    let started: Chan[Int] = channel(1)\n    let r = group g until clock.after(50) {\n        g.go(|| {\n            defer out.print(\"cancelled task cleaned up\")\n            started.send(1)\n            clock.sleep(5000)\n            1\n        })\n        started.recv()\n    }\n    out.print(\"{r}\")\n}\n" "early: deferred\nearly: end\nearly: deferred\n4 0\nparse 4: done\nchain: left\nparse x: done\nchain: left\nOk(5) Err(\"bad\")\nthird x=1\nsecond\nfirst\niter 0 body\niter 0 done\niter 1 done\niter 2 done\nrisky: middle\nrisky: end\nErr(\"zero\") Ok(2)\ny now inner\ny=outer\ncancelled task cleaned up\n[None]\n";
  out "defer-heap-panic" "struct Log {\n    lines: List[Str],\n}\n\nfn work(out: Stdout, n: Int) -> Int {\n    let mut name = \"start\"\n    let mut xs = [\"a\"]\n    let mut log = Log { lines: [] }\n    defer {\n        out.print(\"work {n}: {name} {xs} {log.lines.len()}\")\n        defer out.print(\"work {n}: inner defer\")\n    }\n    name = \"renamed {n}\"\n    xs.push(\"b\")\n    log.lines.push(\"one\")\n    let tmp = \"temp {n}\"\n    if n % 2 == 0 {\n        panic(\"even {tmp}\")\n    }\n    xs = [\"c\", \"d\", \"e\"]\n    name.len() + xs.len()\n}\n\nfn task(out: Stdout, i: Int) -> Int {\n    defer out.print(\"task done\")\n    let mut total = 0\n    for k in 0..<1000 {\n        defer total = total + 1\n        if k == 500 && i == 3 {\n            panic(\"task {i} failed\")\n        }\n    }\n    total\n}\n\nfn main(root: Root) {\n    let out = root.stdout\n    let mut i = 0\n    while i < 30 {\n        let r = catch(|| work(out, i))\n        if i < 4 {\n            out.print(\"{r}\")\n        }\n        i = i + 1\n    }\n    let f = |x: Int| {\n        defer out.print(\"closure {x}\")\n        x * 3\n    }\n    out.print(\"{f(4)}\")\n    let rs = catch(|| group g {\n        for i in 0..<4 {\n            g.go(|| task(out, i))\n        }\n    })\n    out.print(\"{rs}\")\n    defer out.print(\"main: deferred before the uncaught panic\")\n}\n" "work 0: renamed 0 [\"a\", \"b\"] 1\nwork 0: inner defer\nErr(\"even temp 0\")\nwork 1: renamed 1 [\"c\", \"d\", \"e\"] 1\nwork 1: inner defer\nOk(12)\nwork 2: renamed 2 [\"a\", \"b\"] 1\nwork 2: inner defer\nErr(\"even temp 2\")\nwork 3: renamed 3 [\"c\", \"d\", \"e\"] 1\nwork 3: inner defer\nOk(12)\nwork 4: renamed 4 [\"a\", \"b\"] 1\nwork 4: inner defer\nwork 5: renamed 5 [\"c\", \"d\", \"e\"] 1\nwork 5: inner defer\nwork 6: renamed 6 [\"a\", \"b\"] 1\nwork 6: inner defer\nwork 7: renamed 7 [\"c\", \"d\", \"e\"] 1\nwork 7: inner defer\nwork 8: renamed 8 [\"a\", \"b\"] 1\nwork 8: inner defer\nwork 9: renamed 9 [\"c\", \"d\", \"e\"] 1\nwork 9: inner defer\nwork 10: renamed 10 [\"a\", \"b\"] 1\nwork 10: inner defer\nwork 11: renamed 11 [\"c\", \"d\", \"e\"] 1\nwork 11: inner defer\nwork 12: renamed 12 [\"a\", \"b\"] 1\nwork 12: inner defer\nwork 13: renamed 13 [\"c\", \"d\", \"e\"] 1\nwork 13: inner defer\nwork 14: renamed 14 [\"a\", \"b\"] 1\nwork 14: inner defer\nwork 15: renamed 15 [\"c\", \"d\", \"e\"] 1\nwork 15: inner defer\nwork 16: renamed 16 [\"a\", \"b\"] 1\nwork 16: inner defer\nwork 17: renamed 17 [\"c\", \"d\", \"e\"] 1\nwork 17: inner defer\nwork 18: renamed 18 [\"a\", \"b\"] 1\nwork 18: inner defer\nwork 19: renamed 19 [\"c\", \"d\", \"e\"] 1\nwork 19: inner defer\nwork 20: renamed 20 [\"a\", \"b\"] 1\nwork 20: inner defer\nwork 21: renamed 21 [\"c\", \"d\", \"e\"] 1\nwork 21: inner defer\nwork 22: renamed 22 [\"a\", \"b\"] 1\nwork 22: inner defer\nwork 23: renamed 23 [\"c\", \"d\", \"e\"] 1\nwork 23: inner defer\nwork 24: renamed 24 [\"a\", \"b\"] 1\nwork 24: inner defer\nwork 25: renamed 25 [\"c\", \"d\", \"e\"] 1\nwork 25: inner defer\nwork 26: renamed 26 [\"a\", \"b\"] 1\nwork 26: inner defer\nwork 27: renamed 27 [\"c\", \"d\", \"e\"] 1\nwork 27: inner defer\nwork 28: renamed 28 [\"a\", \"b\"] 1\nwork 28: inner defer\nwork 29: renamed 29 [\"c\", \"d\", \"e\"] 1\nwork 29: inner defer\nclosure 4\n12\ntask done\ntask done\ntask done\ntask done\nErr(\"task 3 failed\")\nmain: deferred before the uncaught panic\n";
  check_tsan "defer-in-tasks-tsan" "fn worker(c: Chan[Int], id: Int) -> Int {\n    let mut sent = 0\n    defer c.send(-1)\n    for k in 0..<200 {\n        defer sent = sent + 1\n        c.send(k)\n        if id == 2 && k == 100 {\n            panic(\"worker {id} stops\")\n        }\n    }\n    sent\n}\n\nfn main(root: Root) {\n    let c: Chan[Int] = channel(8)\n    let r = catch(|| group g {\n        for id in 0..<4 {\n            g.go(|| worker(c, id))\n        }\n        g.go(|| {\n            let mut ends = 0\n            let mut sum = 0\n            while ends < 4 {\n                let v = c.recv() ?? -1\n                if v < 0 {\n                    ends = ends + 1\n                } else {\n                    sum = sum + v\n                }\n            }\n            sum\n        })\n    })\n    root.stdout.print(\"{r}\")\n}\n" "Err(\"worker 2 stops\")\n";
  errs "defer-cannot-leave" "fn f(xs: List[Int]) -> Result[Int, Str] {\n    for x in xs {\n        defer {\n            if x > 1 {\n                break\n            }\n        }\n        defer {\n            for y in xs {\n                if y > 2 {\n                    break\n                }\n            }\n        }\n    }\n    defer return Err(\"no\")\n    defer {\n        let n = \"3\".parse_int()?\n    }\n    Ok(1)\n}\n" [ "E0115"; "E0115"; "E0115" ];
  exits "example-tour" (example "tour.tsr") "-- language\ncircle 12.57\n[2.25, 12.56636]\n[0, 1, 4, 9, 16] [\"h\", \"o\", \"l\", \"a\"]\nbound method 3.142\nTESSERA!\n{Point { x: 0, y: 0 }: [\"origin\", \"home\"]} {(1, 2): 2}\n[(1, \"a\"), (1, \"z\"), (2, \"b\")]\n{\"the\": 3, \"and\": 2} {\"he\": [\"the\"], \"at\": [\"cat\", \"hat\", \"bat\"], \"nd\": [\"and\"]}\n[\"cat\", \"hat\", \"bat\"] 3 [[1, 2], [2, 3]]\n[\"and\", \"the\", \"cat\", \"hat\", \"bat\"] 3 Some((4, \"hat\"))\n3 bytes: hi!\nreading settings\ncaused by: not a port: 80x\n8080\n-- defer\nin the block\ncleanup: runs first\ncleanup: runs last\nchecked 7\nchecked x\nOk(7) Err(\"not a number\")\nchecked 0\nErr(\"zero\")\n-- embedding\nhome at (5, 2) [\"(5, 2)\", \"(5, 2)\"]\n-- syntax\npipe 12\nPoint { x: 0, y: 0 } Point { x: 0, y: 5 }\nbig\nsmall\nnothing\nSome(3) -1\n..tour\n[10, 20, 30] Err(\"oops\")\n-- concurrency\na side task ran\nresults [1, 2]\nwith a deadline [Some(1), None]\nnobody sent anything\n" 3;
  out "select" (example "select.tsr") "nums done; words done; total 15 letters 3\nnothing ready | sent to c | sent to d | d Some(10)\nafter close: Some(7)\nErr(\"every arm of this `select` is turned off by its guard\")\n";
  out "select-stress" "fn produce(c: Chan[Int], n: Int) -> Int {\n    let mut i = 0\n    while i < n {\n        c.send(i)\n        i = i + 1\n    }\n    c.close()\n    0\n}\n\n// receive from all four until every one is closed\nfn consume(a: Chan[Int], b: Chan[Int], c: Chan[Int], d: Chan[Int]) -> Int {\n    let mut sum = 0\n    let mut on = [true, true, true, true]\n    while on[0] || on[1] || on[2] || on[3] {\n        select {\n            v = a.recv() if on[0] => match v {\n                Some(x) => {\n                    sum = sum + x\n                },\n                None => {\n                    on[0] = false\n                },\n            },\n            v = b.recv() if on[1] => match v {\n                Some(x) => {\n                    sum = sum + x\n                },\n                None => {\n                    on[1] = false\n                },\n            },\n            v = c.recv() if on[2] => match v {\n                Some(x) => {\n                    sum = sum + x\n                },\n                None => {\n                    on[2] = false\n                },\n            },\n            v = d.recv() if on[3] => match v {\n                Some(x) => {\n                    sum = sum + x\n                },\n                None => {\n                    on[3] = false\n                },\n            },\n        }\n    }\n    sum\n}\n\nfn drain(c: Chan[Str]) -> Int {\n    let mut n = 0\n    let mut more = true\n    while more {\n        match c.recv() {\n            Some(s) => {\n                n = n + s.len()\n            },\n            None => {\n                more = false\n            },\n        }\n    }\n    n\n}\n\nfn main(root: Root) {\n    let a: Chan[Int] = channel(1)\n    let b: Chan[Int] = channel(1)\n    let c: Chan[Int] = channel(2)\n    let d: Chan[Int] = channel(1)\n    let sums = group g {\n        g.go(|| produce(a, 500))\n        g.go(|| produce(b, 500))\n        g.go(|| produce(c, 500))\n        g.go(|| produce(d, 500))\n        g.go(|| consume(a, b, c, d))\n        g.go(|| consume(a, b, c, d))\n        g.go(|| consume(a, b, c, d))\n    }\n    root.stdout.print(\"{sums.sum()}\")\n    // select between two sends: a dispatcher feeding two workers\n    let w1: Chan[Str] = channel(1)\n    let w2: Chan[Str] = channel(1)\n    let parts = group g {\n        g.go(|| {\n                let mut i = 0\n                while i < 1000 {\n                    let msg = \"m{i}\"\n                    select {\n                        w1.send(msg) => (),\n                        w2.send(msg) => (),\n                    }\n                    i = i + 1\n                }\n                w1.close()\n                w2.close()\n                0\n            })\n        g.go(|| drain(w1))\n        g.go(|| drain(w2))\n    }\n    root.stdout.print(\"{parts.sum()}\")\n}\n" "499000\n3890\n";
  check_tsan "select-stress-tsan" "fn produce(c: Chan[Int], n: Int) -> Int {\n    let mut i = 0\n    while i < n {\n        c.send(i)\n        i = i + 1\n    }\n    c.close()\n    0\n}\n\n// receive from all four until every one is closed\nfn consume(a: Chan[Int], b: Chan[Int], c: Chan[Int], d: Chan[Int]) -> Int {\n    let mut sum = 0\n    let mut on = [true, true, true, true]\n    while on[0] || on[1] || on[2] || on[3] {\n        select {\n            v = a.recv() if on[0] => match v {\n                Some(x) => {\n                    sum = sum + x\n                },\n                None => {\n                    on[0] = false\n                },\n            },\n            v = b.recv() if on[1] => match v {\n                Some(x) => {\n                    sum = sum + x\n                },\n                None => {\n                    on[1] = false\n                },\n            },\n            v = c.recv() if on[2] => match v {\n                Some(x) => {\n                    sum = sum + x\n                },\n                None => {\n                    on[2] = false\n                },\n            },\n            v = d.recv() if on[3] => match v {\n                Some(x) => {\n                    sum = sum + x\n                },\n                None => {\n                    on[3] = false\n                },\n            },\n        }\n    }\n    sum\n}\n\nfn drain(c: Chan[Str]) -> Int {\n    let mut n = 0\n    let mut more = true\n    while more {\n        match c.recv() {\n            Some(s) => {\n                n = n + s.len()\n            },\n            None => {\n                more = false\n            },\n        }\n    }\n    n\n}\n\nfn main(root: Root) {\n    let a: Chan[Int] = channel(1)\n    let b: Chan[Int] = channel(1)\n    let c: Chan[Int] = channel(2)\n    let d: Chan[Int] = channel(1)\n    let sums = group g {\n        g.go(|| produce(a, 500))\n        g.go(|| produce(b, 500))\n        g.go(|| produce(c, 500))\n        g.go(|| produce(d, 500))\n        g.go(|| consume(a, b, c, d))\n        g.go(|| consume(a, b, c, d))\n        g.go(|| consume(a, b, c, d))\n    }\n    root.stdout.print(\"{sums.sum()}\")\n    // select between two sends: a dispatcher feeding two workers\n    let w1: Chan[Str] = channel(1)\n    let w2: Chan[Str] = channel(1)\n    let parts = group g {\n        g.go(|| {\n                let mut i = 0\n                while i < 1000 {\n                    let msg = \"m{i}\"\n                    select {\n                        w1.send(msg) => (),\n                        w2.send(msg) => (),\n                    }\n                    i = i + 1\n                }\n                w1.close()\n                w2.close()\n                0\n            })\n        g.go(|| drain(w1))\n        g.go(|| drain(w2))\n    }\n    root.stdout.print(\"{parts.sum()}\")\n}\n" "499000\n3890\n";
  let chans = "    let c: Chan[Int] = channel(1)\n" in
  errs "select-not-channel" (main_wrap "    let x = select {\n        v = 5.recv() => 1,\n    }") [ "E0201" ];
  errs "select-guard-type" (main_wrap (chans ^ "    let x = select {\n        v = c.recv() if 1 => 1,\n    }")) [ "E0201" ];
  errs "select-only-default" (main_wrap "    let x = select {\n        _ => 1,\n    }") [ "E0003" ];
  errs "select-two-defaults" (main_wrap (chans ^ "    let x = select {\n        v = c.recv() => 1,\n        _ => 2,\n        _ => 3,\n    }")) [ "E0106" ];
  errs "select-arm-types" (main_wrap (chans ^ "    let x = select {\n        v = c.recv() => 1,\n        _ => \"no\",\n    }")) [ "E0201" ];
  errs "select-bad-arm" (main_wrap (chans ^ "    let x = select {\n        c.len() => 1,\n    }")) [ "E0003" ];

  (* ---------- panics end one task; catch ---------- *)
  out "catch-and-task-panics" "fn risky(n: Int) -> Int {\n    if n == 3 {\n        panic(\"three is bad\")\n    }\n    [10, 20][n]\n}\n\nfn main(root: Root) {\n    let out = root.stdout\n    out.print(\"{catch(|| risky(0))} {catch(|| risky(3))} {catch(|| risky(5))}\")\n    out.print(\"{catch(|| 10 / (2 - 2))}\")\n    // a panicking task ends only itself; the group passes the panic on\n    let r = catch(|| group g {\n        for n in [0, 3, 1] {\n            g.go(|| risky(n))\n        }\n    })\n    out.print(\"{r}\")\n    let ok = group g {\n        for n in [0, 1] {\n            g.go(|| catch(|| risky(n + 2)) ?? -1)\n        }\n    }\n    out.print(\"{ok}\")\n    // nested boundaries: the inner one handles its own panic\n    let nested = catch(|| (catch(|| risky(3)) ?? 7) + risky(1))\n    out.print(\"{nested}\")\n}\n" "Ok(10) Err(\"three is bad\") Err(\"index 5 is out of range for a list of length 2\")\nErr(\"division by zero\")\nErr(\"three is bad\")\n[-1, -1]\nOk(27)\n";
  check_tsan "task-panics-tsan"
    "fn work(n: Int) -> Int {\n    if n % 3 == 0 {\n        panic(\"bad {n}\")\n    }\n    n\n}\n\nfn main(root: Root) {\n    let mut i = 0\n    let mut caught = 0\n    while i < 50 {\n        let r = catch(|| group g {\n            for n in [i * 3 + 1, i * 3 + 2, i * 3 + 3] {\n                g.go(|| work(n))\n            }\n        })\n        if r.is_err() {\n            caught = caught + 1\n        }\n        i = i + 1\n    }\n    root.stdout.print(\"{caught}\")\n}\n"
    "50\n";
  (* an uncaught panic in a task still ends the program, once its siblings finish *)
  panics "task-panic-uncaught" "fn main(root: Root) {\n    let xs = group g {\n        g.go(|| 1)\n        g.go(|| [1][3])\n    }\n}\n" "index 3 is out of range for a list of length 1";
  (* what a panic abandons is freed: these must end with 0 objects alive *)
  out "catch-frees-abandoned" "fn work(i: Int) -> Int {\n    let mut xs = []\n    let mut m: Map[Str, List[Int]] = map()\n    let mut k = 0\n    while k < 20 {\n        xs.push(\"item {k}\")\n        m[\"k{k}\"] = [k, k + 1]\n        k = k + 1\n    }\n    let f = |n: Int| xs.len() + n\n    if i % 2 == 0 {\n        panic(\"bad {i}\")\n    }\n    f(m.len())\n}\n\nfn main(root: Root) {\n    let mut ok = 0\n    let mut bad = 0\n    let mut i = 0\n    while i < 10000 {\n        match catch(|| work(i)) {\n            Ok(_) => { ok = ok + 1 },\n            Err(_) => { bad = bad + 1 },\n        }\n        i = i + 1\n    }\n    let r = catch(|| group g {\n        for n in [1, 2, 3] {\n            g.go(|| work(n))\n        }\n    })\n    root.stdout.print(\"{ok} {bad} {r.is_err()}\")\n}\n" "5000 5000 true\n";
  out "group-deadline-skips-unit-tasks" "fn main(root: Root) {\n    let out = root.stdout\n    let clock = root.clock\n    let rs = group g until clock.after(200) {\n        g.go(|| 1)\n        g.go(|| out.print(\"side\"))\n        g.go(|| {\n            clock.sleep(5000)\n            3\n        })\n        g.go(|| 4)\n    }\n    out.print(\"{rs}\")\n    let plain = group g {\n        g.go(|| out.print(\"a\"))\n        g.go(|| 7)\n    }\n    out.print(\"{plain}\")\n}\n" "side\n[Some(1), None, Some(4)]\na\n[7]\n";
  out "catch-unwinds-main-loop" "struct Req { path: Str, tags: List[Str] }\n\nfn handle(r: Req) -> Int {\n    let n = r.tags.len()\n    if r.path == \"/bad\" {\n        panic(\"bad path\")\n    }\n    n\n}\n\nfn main(root: Root) {\n    let mut i = 0\n    let mut errs = 0\n    while i < 100 {\n        let req = Req { path: if i % 2 == 0 { \"/bad\" } else { \"/ok\" }, tags: [\"a{i}\", \"b\"] }\n        match catch(|| handle(req)) {\n            Ok(_) => {},\n            Err(_) => { errs = errs + 1 },\n        }\n        i = i + 1\n    }\n    root.stdout.print(\"{errs}\")\n}\n" "50\n";
  check_tsan "group-body-panics-tsan" "fn slow(n: Int) -> Int {\n    let mut i = 0\n    let mut s = 0\n    while i < 20000 {\n        s = s + i % (n + 1)\n        i = i + 1\n    }\n    s\n}\n\nfn main(root: Root) {\n    let mut k = 0\n    let mut errs = 0\n    while k < 50 {\n        let r = catch(|| group g {\n            g.go(|| slow(1))\n            g.go(|| slow(2))\n            panic(\"boom {k}\")\n        })\n        if r.is_err() { errs = errs + 1 }\n        k = k + 1\n    }\n    root.stdout.print(\"{errs}\")\n}\n" "50\n";

  (* ---------- timers ---------- *)
  let sleepers = {|fn main(root: Root) {
    let out = root.stdout
    let clock = root.clock
    let start = clock.now_ms()
    // tasks wake in the order of their deadlines
    let c: Chan[Str] = channel(4)
    group g {
        g.go(|| {
            clock.sleep(300)
            c.send("c")
            0
        })
        g.go(|| {
            clock.sleep(150)
            c.send("b")
            0
        })
        g.go(|| {
            c.send("a")
            0
        })
        g.go(|| {
            let mut s = ""
            let mut i = 0
            while i < 3 {
                s = s + (c.recv() ?? "?")
                i = i + 1
            }
            out.print(s)
            0
        })
    }
    // a sleeping task parks: a thousand 50 ms sleeps take about 50 ms
    let n = group h {
        let mut i = 0
        while i < 1000 {
            h.go(|| {
                clock.sleep(50)
                1
            })
            i = i + 1
        }
    }
    clock.sleep(0)
    clock.sleep(-5)
    let el = clock.now_ms() - start
    out.print("{n.sum()} {el >= 350} {el < 3000}")
}
|} in
  out "clock-sleep" sleepers "abc\n1000 true true\n";
  check_tsan "clock-sleep-tsan" sleepers "abc\n1000 true true\n";
  (* a task waiting on a channel while another sleeps is not a deadlock *)
  out "clock-sleep-not-deadlock" {|fn main(root: Root) {
    let clock = root.clock
    let c: Chan[Int] = channel(1)
    let r = group g {
        g.go(|| c.recv() ?? 0)
        g.go(|| {
            clock.sleep(400)
            c.send(7)
            0
        })
    }
    root.stdout.print("{r}")
}
|} "[7, 0]\n";
  out "clock-sleep-interface" {|interface Sleeper {
    fn sleep(self, ms: Int)
}

struct FakeClock {
    t: Int,
}

fn sleep(self: FakeClock, ms: Int) {
}

fn nap[C: Sleeper](c: C) -> Int {
    c.sleep(1)
    c.sleep(1)
    2
}

fn main(root: Root) {
    root.stdout.print("{nap(root.clock) + nap(FakeClock { t: 0 })}")
}
|} "4\n";
  errs "sleep-ambient" (main_wrap "    sleep(10)") [ "E0302" ];
  out "select-timeout" {|fn main(root: Root) {
    let out = root.stdout
    let clock = root.clock
    let quiet: Chan[Str] = channel(1)
    let busy: Chan[Str] = channel(1)
    let full: Chan[Str] = channel(1)
    full.send("taken")
    let r = group g {
        g.go(|| {
            // nobody sends on quiet: the timeout fires
            let a = select {
                m = quiet.recv() => m ?? "closed",
                clock.after(100) => "timed out",
            }
            // a value comes long before the timeout
            let b = select {
                m = busy.recv() => m ?? "closed",
                clock.after(5000) => "timed out",
            }
            // full stays full: the value is not sent, and is freed
            let c = select {
                full.send("never {a}") => "sent",
                clock.after(50) => "not sent",
            }
            // after(0) is ready at once, but an earlier ready arm wins
            busy.send("again")
            let d = select {
                m = busy.recv() => m ?? "closed",
                clock.after(0) => "zero",
            }
            let e = select {
                m = quiet.recv() => m ?? "closed",
                clock.after(-1) => "zero",
            }
            // a guard turns the timeout off
            let on = false
            busy.send("guarded")
            let f = select {
                m = busy.recv() => m ?? "closed",
                clock.after(1) if on => "timed out",
            }
            "{a}, {b}, {c}, {d}, {e}, {f}"
        })
        g.go(|| {
            clock.sleep(20)
            busy.send("hello")
            ""
        })
    }
    out.print(r[0])
    // gather until a deadline
    let ticks: Chan[Int] = channel(1)
    let got = group g {
        g.go(|| {
            let deadline = clock.now_ms() + 300
            let mut n = 0
            let mut more = true
            while more {
                select {
                    t = ticks.recv() => match t {
                        Some(_) => {
                            n = n + 1
                        },
                        None => {
                            more = false
                        },
                    },
                    clock.after(deadline - clock.now_ms()) => {
                        more = false
                    },
                }
            }
            n
        })
        g.go(|| {
            ticks.send(1)
            ticks.send(2)
            ticks.send(3)
            0
        })
    }
    out.print("{got[0]}")
}
|} "timed out, hello, not sent, again, zero, guarded\n3\n";
  par "select-timeout-stress" {|fn produce(c: Chan[Int], clock: Clock) -> Int {
    let mut i = 0
    while i < 2000 {
        c.send(i)
        if i % 100 == 0 {
            clock.sleep(2)
        }
        i = i + 1
    }
    c.close()
    0
}

fn consume(c: Chan[Int], clock: Clock) -> Int {
    let mut sum = 0
    let mut more = true
    while more {
        select {
            v = c.recv() => match v {
                Some(x) => {
                    sum = sum + x
                },
                None => {
                    more = false
                },
            },
            clock.after(1) => (),
        }
    }
    sum
}

fn main(root: Root) {
    let clock = root.clock
    let c: Chan[Int] = channel(4)
    let sums = group g {
        g.go(|| produce(c, clock))
        let mut k = 0
        while k < 8 {
            g.go(|| consume(c, clock))
            k = k + 1
        }
    }
    root.stdout.print("{sums.sum()}")
}
|} "1999000\n";
  let tchans = "    let c: Chan[Int] = channel(1)\n    let clock = root.clock\n" in
  let body_src = {|fn main(root: Root) {
    let clock = root.clock
    let out = root.stdout
    let quiet: Chan[Int] = channel(1)
    let mut seen = "start"
    let r = group g until clock.after(400) {
        g.go(|| { clock.sleep(5); 1 })
        g.go(|| { clock.sleep(5000); 2 })
        defer { out.print("defer ran") }
        seen = "body waiting"
        let v = quiet.recv()
        seen = "never"
        g.go(|| 3)
    }
    out.print("{seen} {r}")
    // an inner group the body waits on is cancelled with it
    let r2 = group g until clock.after(30) {
        let inner = group h {
            h.go(|| { clock.sleep(5000); 7 })
        }
        g.go(|| 9)
        ()
    }
    out.print("inner {r2}")
    // a body that finishes in time is not touched
    let r3 = group g until clock.after(1000) {
        g.go(|| 4)
        clock.sleep(5)
    }
    out.print("fast {r3}")
    // nested: the inner deadline stops the inner body only
    let r4 = group g until clock.after(2000) {
        let a = group h until clock.after(20) {
            h.go(|| 5)
            clock.sleep(5000)
        }
        g.go(|| a.len())
    }
    out.print("nested {r4}")
    // a task its group cancels stops inside its own deadline body, and stays stopped
    let r5 = group g until clock.after(20) {
        g.go(|| {
            let inner = group h until clock.after(5000) {
                h.go(|| 1)
                clock.sleep(5000)
            }
            inner.len()
        })
    }
    out.print("task {r5}")
    // a panic in a body still ends the program
    let p = catch(|| {
        let x = group g until clock.after(1000) {
            g.go(|| 1)
            panic("boom")
        }
        x.len()
    })
    out.print("panic {p}")
}
|} in
  out "group-deadline-stops-body" body_src "defer ran\nbody waiting [Some(1), None]\ninner []\nfast [Some(4)]\nnested [Some(1)]\ntask [None]\npanic Err(\"boom\")\n";
  check_tsan "group-deadline-stops-body" body_src "defer ran\nbody waiting [Some(1), None]\ninner []\nfast [Some(4)]\nnested [Some(1)]\ntask [None]\npanic Err(\"boom\")\n";
  out "select-several-afters" {|fn main(root: Root) {
    let clock = root.clock
    let quiet: Chan[Int] = channel(1)
    // the earliest timeout wins, wherever it is written
    let a = select {
        v = quiet.recv() => "value",
        clock.after(5000) => "late",
        clock.after(20) => "early",
    }
    // on a tie the first written wins
    let b = select {
        v = quiet.recv() => "value",
        clock.after(10) => "first",
        clock.after(10) => "second",
    }
    // a guard turns one off
    let on = false
    let c = select {
        v = quiet.recv() => "value",
        clock.after(1) if on => "off",
        clock.after(15) => "on",
    }
    root.stdout.print("{a} {b} {c}")
}
|} "early first on\n";
  errs "select-after-and-default" (main_wrap (tchans ^ "    let x = select {\n        v = c.recv() => 1,\n        clock.after(1) => 2,\n        _ => 3,\n    }")) [ "E0003" ];
  errs "select-after-not-clock" (main_wrap (tchans ^ "    let x = select {\n        v = c.recv() => 1,\n        c.after(1) => 2,\n    }")) [ "E0201" ];
  errs "select-after-ms-type" (main_wrap (tchans ^ "    let x = select {\n        v = c.recv() => 1,\n        clock.after(\"soon\") => 2,\n    }")) [ "E0201" ];
  errs "after-outside-select" (main_wrap (tchans ^ "    clock.after(10)")) [ "E0105" ];

  (* ---------- group deadlines and cancellation ---------- *)
  out "group-until" {|fn stuck(c: Chan[Int]) -> Int {
    c.recv() ?? -1
}

fn main(root: Root) {
    let out = root.stdout
    let clock = root.clock
    let start = clock.now_ms()
    let never: Chan[Int] = channel(1)
    let full: Chan[Str] = channel(1)
    full.send("x")
    let r = group g until clock.after(100) {
        g.go(|| 1)
        g.go(|| stuck(never))
        g.go(|| {
            clock.sleep(10000)
            2
        })
        g.go(|| {
            let mut words = ["a", "b"]
            words.push("c")
            let r = catch(|| stuck(never))
            words.len() + r.is_ok().to_str().len()
        })
        g.go(|| {
            full.send("y {start}")
            3
        })
        g.go(|| {
            select {
                full.send("z {start}") => 4,
                v = never.recv() => 5,
            }
        })
        g.go(|| {
            let inner = group h {
                h.go(|| stuck(never))
                h.go(|| 6)
            }
            inner.len()
        })
        g.go(|| {
            let mut i = 0
            while clock.now_ms() - start < 200 {
                i = i + 1
            }
            7
        })
    }
    out.print("{r}")
    let el = clock.now_ms() - start
    out.print("{el < 2000}")
    // a deadline already passed: no task runs
    let z = group g until clock.after(0) {
        g.go(|| {
            out.print("ran")
            1
        })
    }
    out.print("{z}")
    // the deadline stops the body at its sleep: the late task is never started
    let late = group g until clock.after(30) {
        g.go(|| 1)
        clock.sleep(100)
        g.go(|| {
            out.print("ran late")
            2
        })
    }
    out.print("{late}")
    // a panic still passes on, after the stuck task is cancelled
    let p = catch(|| group g until clock.after(50) {
        g.go(|| stuck(never))
        g.go(|| {
            panic("boom")
            1
        })
    })
    out.print("{p}")
}
|} "[Some(1), None, None, None, None, None, None, Some(7)]\ntrue\n[None]\n[Some(1)]\nErr(\"boom\")\n";
  par "group-until-stress" {|fn stuck(c: Chan[Int]) -> Int {
    c.recv() ?? -1
}

fn produce(c: Chan[Str]) -> Int {
    let mut i = 0
    while i < 1000000 {
        c.send("v{i}")
        i = i + 1
    }
    0
}

fn consume(c: Chan[Str], clock: Clock) -> Int {
    let mut more = true
    while more {
        match c.recv() {
            Some(_) => clock.sleep(1),
            None => {
                more = false
            },
        }
    }
    0
}

fn main(root: Root) {
    let clock = root.clock
    let never: Chan[Int] = channel(1)
    let busy: Chan[Str] = channel(2)
    let mut total = 0
    let mut k = 0
    while k < 15 {
        let r = group g until clock.after(150) {
            // the counted tasks go first, before the busy ones slow the body
            let mut j = 0
            while j < 10 {
                g.go(|| 1)
                j = j + 1
            }
            let mut i = 0
            while i < 10 {
                g.go(|| stuck(never))
                g.go(|| {
                    clock.sleep(5000)
                    1
                })
                g.go(|| {
                    let inner = group h {
                        h.go(|| stuck(never))
                        h.go(|| 1)
                    }
                    inner.len()
                })
                g.go(|| select {
                    v = never.recv() => 1,
                    clock.after(5000) => 2,
                })
                // racing with the deadline: counted as 0 either way
                g.go(|| produce(busy))
                g.go(|| consume(busy, clock))
                g.go(|| {
                    let mut n = 0
                    while n < 100000 {
                        select {
                            busy.send("s{n}") => (),
                            clock.after(1) => (),
                        }
                        n = n + 1
                    }
                    0
                })
                i = i + 1
            }
        }
        for x in r {
            total = total + (x ?? 0)
        }
        k = k + 1
    }
    root.stdout.print("{total}")
}
|} "150\n";
  let zone_dir =
    let d = Filename.concat (Sys.getcwd ()) "zoneinfo" in
    if Sys.file_exists (Filename.concat d "UTC") then d else Filename.concat (Sys.getcwd ()) "test/zoneinfo"
  in
  out "time-zones" ~grants:{ Interp.no_grants with fs = Some (Unix.realpath zone_dir, true) } {|use time

fn at(y: Int, mo: Int, d: Int, h: Int, mi: Int) -> time.DateTime {
    time.DateTime { year: y, month: mo, day: d, hour: h, minute: mi, second: 0, ms: 0, weekday: 0, yday: 0 }
}

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let madrid = time.load_zone(root.fs, "Europe/Madrid")?
    let ny = time.load_zone(root.fs, "America/New_York")?
    let sydney = time.load_zone(root.fs, "Australia/Sydney")?
    let utc = time.load_zone(root.fs, "UTC")?
    let summer = time.parse_iso("2026-07-01T12:00Z") ?? 0
    let winter = time.parse_iso("2026-01-15T12:00Z") ?? 0
    // 2080 is past the files' last listed change: the POSIX rule decides
    let far = time.parse_iso("2080-07-01T12:00Z") ?? 0
    for z in [madrid, ny, sydney, utc] {
        out.print("{z.name}: {time.iso_in(summer, z)} {time.abbr_in(summer, z)} {time.is_dst_in(summer, z)} | {time.iso_in(winter, z)} {time.abbr_in(winter, z)} | {time.format_in(far, z, "%Y-%m-%d %H:%M %Z %z %%Z")}")
    }
    // the hour before and after Madrid's change on 2026-03-29 at 01:00 UTC
    let change = time.parse_iso("2026-03-29T01:00Z") ?? 0
    out.print("{time.offset_in(change - 1, madrid)} {time.offset_in(change, madrid)}")
    // 02:30 is skipped when clocks go forward, and happens twice when they go back
    out.print("{time.iso(time.from_parts_in(at(2026, 3, 29, 2, 30), madrid))} {time.iso(time.from_parts_in(at(2026, 10, 25, 2, 30), madrid))} {time.iso(time.from_parts_in(at(2026, 6, 1, 9, 0), madrid))}")
    out.print("{time.parts_in(summer, sydney).hour} {time.offset_in(time.parse_iso("1950-07-01T00:00Z") ?? 0, ny)}")
    out.print("{time.load_zone(root.fs, "../etc/passwd")}")
    out.print("{time.load_zone(root.fs, "Europe/Nowhere").is_err()}")
    out.print("{time.zone_from_tzif("x", "nope".to_bytes())}")
    Ok(())
}
|} "Europe/Madrid: 2026-07-01T14:00:00.000+02:00 CEST true | 2026-01-15T13:00:00.000+01:00 CET | 2080-07-01 14:00 CEST +0200 %Z\n\
America/New_York: 2026-07-01T08:00:00.000-04:00 EDT true | 2026-01-15T07:00:00.000-05:00 EST | 2080-07-01 08:00 EDT -0400 %Z\n\
Australia/Sydney: 2026-07-01T22:00:00.000+10:00 AEST false | 2026-01-15T23:00:00.000+11:00 AEDT | 2080-07-01 22:00 AEST +1000 %Z\n\
UTC: 2026-07-01T12:00:00.000+00:00 UTC false | 2026-01-15T12:00:00.000+00:00 UTC | 2080-07-01 12:00 UTC +0000 %Z\n\
60 120\n\
2026-03-29T01:30:00.000Z 2026-10-25T00:30:00.000Z 2026-06-01T07:00:00.000Z\n\
22 -240\n\
Err(Failed(\"time: `../etc/passwd` is not a zone name\"))\n\
true\n\
Err(\"time: `x` is not a TZif zone file\")\n";
  par "time-within" ~grants:{ Interp.no_grants with listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1" ] } {|use time

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let clock = root.clock
    let net = root.net
    let l = root.listen.on("127.0.0.1:0")?
    let port = l.port()
    // the server accepts and never answers, so the read waits until the deadline
    let r = group g {
        g.go(|| match l.accept() {
            Ok(c) => {
                clock.sleep(1000)
                c.close()
            },
            Err(_) => (),
        })
        g.go(|| {
            let start = clock.now_ms()
            let got = time.within(clock, 100, || net.get("http://127.0.0.1:{port}/").map(|r| r.status))
            let took = clock.now_ms() - start
            out.print("{got} {took >= 100 && took < 900}")
        })
    }
    out.print("{time.within(clock, 1000, || Ok(7))}")
    out.print("{time.within(clock, 1000, || Err(Failed("no")))}")
    let late: Result[Int, IoError] = time.within(clock, 0, || Ok(1))
    out.print("{late}")
    Ok(())
}
|} "Err(Failed(\"timed out after 100 ms\")) true\nOk(7)\nErr(Failed(\"no\"))\nErr(Failed(\"timed out after 0 ms\"))\n";
  par "net-ipv6" ~grants:{ Interp.no_grants with listen = [ "[::1]:0"; "127.0.0.1:0" ]; net = [ "[::1]"; "localhost" ] } {|fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let net = root.net
    let l = root.listen.on("[::1]:0")?
    let port = l.port()
    let r = group g {
        g.go(|| {
            let mut n = 0
            while n < 2 {
                match l.accept() {
                    Ok(c) => {
                        let _ = c.read()
                        let _ = c.write("HTTP/1.1 204 No Content\r\nContent-Length: 0\r\n\r\n")
                        c.close()
                    },
                    Err(_) => (),
                }
                n = n + 1
            }
        })
        g.go(|| match net.get("http://[::1]:{port}/x") {
            Ok(r) => r.status,
            Err(e) => {
                out.print("{e}")
                0
            },
        })
        g.go(|| match net.connect("[::1]:{port}") {
            Ok(c) => {
                let _ = c.write("GET / HTTP/1.1\r\n\r\n")
                let _ = c.read()
                1
            },
            Err(e) => {
                out.print("{e}")
                0
            },
        })
    }
    out.print("{r}")
    // a name that resolves to IPv4
    let v4 = root.listen.on("127.0.0.1:0")?
    let p4 = v4.port()
    let both = group g {
        g.go(|| match v4.accept() {
            Ok(_) => 1,
            Err(_) => 0,
        })
        g.go(|| match net.connect("localhost:{p4}") {
            Ok(_) => 1,
            Err(_) => 0,
        })
    }
    out.print("{both}")
    out.print("{net.get("http://[::1:80/")}")
    out.print("{net.get("http://[nope]/")}")
    out.print("{net.get("http://[::2]:9/")}")
    Ok(())
}
|} "[204, 1]\n[1, 1]\nErr(Failed(\"malformed IPv6 address in URL\"))\nErr(Failed(\"malformed IPv6 address in URL\"))\nErr(Denied(\"net: [::2]:9 is not granted\"))\n";
  out "group-until-sockets" ~grants:{ Interp.no_grants with listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1" ] } {|fn main(root: Root) -> Result[(), IoError] {
    let clock = root.clock
    let net = root.net
    let l = root.listen.on("127.0.0.1:0")?
    let port = l.port()
    // a read and a sleep that holds a connection, both cancelled
    let a = group g until clock.after(150) {
        g.go(|| match l.accept() {
            Ok(c) => match c.read() {
                Ok(s) => s.len(),
                Err(_) => -1,
            },
            Err(_) => -2,
        })
        g.go(|| match net.connect("127.0.0.1:{port}") {
            Ok(c) => {
                clock.sleep(10000)
                c.close()
                0
            },
            Err(_) => -3,
        })
    }
    // an accept with nobody connecting, cancelled
    let b = group g until clock.after(50) {
        g.go(|| match l.accept() {
            Ok(_) => 1,
            Err(_) => 2,
        })
    }
    // the listener still works
    let c = group g {
        g.go(|| match l.accept() {
            Ok(c) => (c.read() ?? "?").len(),
            Err(_) => -1,
        })
        g.go(|| match net.connect("127.0.0.1:{port}") {
            Ok(c) => {
                c.write("hello") ?? ()
                c.close()
                0
            },
            Err(_) => -3,
        })
    }
    root.stdout.print("{a} {b} {c}")
    Ok(())
}
|} "[None, None] [None] [5, 0]\n";
  errs "group-until-not-clock" (main_wrap "    let c: Chan[Int] = channel(1)\n    let r = group g until c.after(5) {\n        g.go(|| 1)\n    }") [ "E0201" ];
  errs "group-until-ms-type" (main_wrap "    let r = group g until root.clock.after(true) {\n        g.go(|| 1)\n    }") [ "E0201" ];
  errs "group-until-type" (main_wrap "    let r: List[Int] = group g until root.clock.after(5) {\n        g.go(|| 1)\n    }") [ "E0201" ];
  errs "group-until-bad" (main_wrap "    let r = group g until root.clock.sleep(5) {\n        g.go(|| 1)\n    }") [ "E0003" ];

  (* ---------- stdin through the poller ---------- *)
  (* input arrives late; the other task keeps ticking meanwhile, natively on one thread *)
  out "stdin-lines" ~feed:"sleep 0.3; printf 'one\\ntwo\\r\\n'; sleep 0.2; printf three" {|fn main(root: Root) {
    let out = root.stdout
    let clock = root.clock
    let stdin = root.stdin
    let start = clock.now_ms()
    // while one task waits for input, the others run
    let r = group g {
        g.go(|| {
            let mut lines = []
            let mut more = true
            while more {
                match stdin.read_line() {
                    Some(l) => lines.push(l),
                    None => {
                        more = false
                    },
                }
            }
            lines.join("|")
        })
        g.go(|| {
            let mut n = 0
            while clock.now_ms() - start < 100 {
                clock.sleep(10)
                n = n + 1
            }
            "ticked {n > 3}"
        })
    }
    out.print("{r}")
}
|} "[\"one|two|three\", \"ticked true\"]\n";
  out "stdin-all" ~feed:"sleep 0.3; printf 'a\\nb'; sleep 0.1; printf 'c\\n'" {|fn main(root: Root) {
    let clock = root.clock
    let start = clock.now_ms()
    let r = group g {
        g.go(|| root.stdin.read_all())
        g.go(|| {
            let mut n = 0
            while clock.now_ms() - start < 100 {
                clock.sleep(10)
                n = n + 1
            }
            "{n > 3}"
        })
    }
    root.stdout.print("{r}")
    root.stdout.print("{root.stdin.read_line()}")
}
|} "[\"a\\nbc\\n\", \"true\"]\nNone\n";
  (* a reader waiting for input is cancelled; the next read still gets the line *)
  out "stdin-cancel" ~feed:"sleep 0.6; echo late" {|fn main(root: Root) {
    let stdin = root.stdin
    let r = group g until root.clock.after(100) {
        g.go(|| stdin.read_line() ?? "end")
    }
    root.stdout.print("{r} {stdin.read_line()}")
}
|} "[None] Some(\"late\")\n";
  let readers = {|fn sum_lines(stdin: Stdin) -> Int {
    let mut sum = 0
    let mut more = true
    while more {
        match stdin.read_line() {
            Some(l) => {
                sum = sum + (l.parse_int() ?? 0)
            },
            None => {
                more = false
            },
        }
    }
    sum
}

fn main(root: Root) {
    let stdin = root.stdin
    let sums = group g {
        let mut i = 0
        while i < 6 {
            g.go(|| sum_lines(stdin))
            i = i + 1
        }
    }
    root.stdout.print("{sums.sum()}")
}
|} in
  out "stdin-readers" ~feed:"seq 1 1000; sleep 0.1; seq 1001 2000" readers "2001000\n";
  check_tsan "stdin-readers" ~feed:"seq 1 1000; sleep 0.1; seq 1001 2000" readers "2001000\n";

  (* ---------- one reader and one writer per connection, in both backends ---------- *)
  par "socket-one-at-a-time" ~grants:{ Interp.no_grants with listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1" ] } {|fn show(r: Result[Str, IoError]) -> Str {
    match r {
        Ok(s) => "read {s.len()}",
        Err(e) => "{e}",
    }
}

fn wrote(r: Result[(), IoError]) -> Str {
    match r {
        Ok(_) => "wrote",
        Err(e) => "{e}",
    }
}

fn main(root: Root) -> Result[(), IoError] {
    let clock = root.clock
    let net = root.net
    let l = root.listen.on("127.0.0.1:0")?
    let port = l.port()
    let mut chunk = "0123456789abcdef"
    let mut i = 0
    while i < 18 {
        chunk = chunk + chunk
        i = i + 1
    }
    // two readers at once: one waits for data, the other is refused
    let r = group g {
        g.go(|| match l.accept() {
            Ok(c) => {
                let rs = group h {
                    h.go(|| show(c.read()))
                    h.go(|| show(c.read()))
                }
                c.close()
                rs.sort().join(" / ")
            },
            Err(e) => "{e}",
        })
        g.go(|| match net.connect("127.0.0.1:{port}") {
            Ok(c) => {
                clock.sleep(200)
                c.write("hi") ?? ()
                c.read() ?? ""
                c.close()
                ""
            },
            Err(e) => "{e}",
        })
    }
    root.stdout.print(r[0])
    // two accepts at once
    let a = group g {
        g.go(|| match l.accept() {
            Ok(c) => {
                c.close()
                "accepted"
            },
            Err(e) => "{e}",
        })
        g.go(|| match l.accept() {
            Ok(c) => {
                c.close()
                "accepted"
            },
            Err(e) => "{e}",
        })
        g.go(|| {
            clock.sleep(200)
            match net.connect("127.0.0.1:{port}") {
                Ok(c) => {
                    c.close()
                    ""
                },
                Err(e) => "{e}",
            }
        })
    }
    root.stdout.print(a.take(2).sort().join(" / "))
    // two writers at once, while the peer is not reading yet
    let w = group g {
        g.go(|| match l.accept() {
            Ok(c) => {
                let ws = group h {
                    h.go(|| wrote(c.write(chunk)))
                    h.go(|| {
                        clock.sleep(50)
                        wrote(c.write(chunk))
                    })
                }
                c.close()
                ws.sort().join(" / ")
            },
            Err(e) => "{e}",
        })
        g.go(|| match net.connect("127.0.0.1:{port}") {
            Ok(c) => {
                clock.sleep(300)
                let mut got = 0
                let mut more = true
                while more {
                    match c.read() {
                        Ok(s) => {
                            got = got + s.len()
                            more = !s.is_empty()
                        },
                        Err(_) => {
                            more = false
                        },
                    }
                }
                c.close()
                "{got}"
            },
            Err(e) => "{e}",
        })
    }
    root.stdout.print("{w[0]}; peer got {w[1]}")
    Ok(())
}
|} "Failed(\"another task is already reading this connection\") / read 2\nFailed(\"another task is already accepting on this listener\") / accepted\nFailed(\"another task is already writing this connection\") / wrote; peer got 4194304\n";

  (* ---------- unbuffered channels ---------- *)
  out "chan-unbuffered" {|fn main(root: Root) {
    let out = root.stdout
    let clock = root.clock
    let c: Chan[Int] = channel(0)
    let log: Chan[Str] = channel(10)
    // a send waits for the receiver
    let r = group g {
        g.go(|| {
            c.send(1)
            log.send("sent")
            0
        })
        g.go(|| {
            clock.sleep(100)
            log.send("receiving")
            c.recv() ?? 0
        })
    }
    out.print("{r} {log.recv() ?? ""} then {log.recv() ?? ""}")
    // nobody on the other side: neither arm is ready
    let a = select {
        c.send(1) => "sent",
        _ => "nobody receiving",
    }
    let b = select {
        v = c.recv() => "got",
        _ => "nobody sending",
    }
    out.print("{a}, {b}")
    // a select send meets a waiting receiver, and a select receive a waiting sender
    let s = group g {
        g.go(|| c.recv() ?? 0)
        g.go(|| {
            clock.sleep(50)
            select {
                c.send(5) => 1,
                clock.after(5000) => 0,
            }
        })
    }
    let t = group g {
        g.go(|| {
            c.send(6)
            0
        })
        g.go(|| {
            clock.sleep(50)
            select {
                v = c.recv() => v ?? -1,
                clock.after(5000) => -2,
            }
        })
    }
    out.print("{s} {t}")
    // closing wakes a waiting receiver with None
    let u = group g {
        g.go(|| c.recv() ?? -1)
        g.go(|| {
            clock.sleep(50)
            c.close()
            0
        })
    }
    out.print("{u}")
    // and makes a waiting sender panic
    let d: Chan[Str] = channel(0)
    let p = catch(|| group g {
        g.go(|| {
            d.send("lost")
            0
        })
        g.go(|| {
            clock.sleep(50)
            d.close()
            0
        })
    })
    out.print("{p}")
}
|} "[0, 1] receiving then sent\nnobody receiving, nobody sending\n[5, 1] [0, 6]\n[-1, 0]\nErr(\"send on a closed channel\")\n";
  par "chan-unbuffered-stress" {|fn produce(c: Chan[Int], n: Int) -> Int {
    let mut i = 0
    while i < n {
        c.send(i)
        i = i + 1
    }
    0
}

fn consume(c: Chan[Int], done: Chan[Int], clock: Clock) -> Int {
    let mut sum = 0
    let mut more = true
    while more {
        select {
            v = c.recv() => {
                sum = sum + (v ?? 0)
            },
            v = done.recv() => {
                more = false
            },
            clock.after(1) => (),
        }
    }
    sum
}

fn main(root: Root) {
    let clock = root.clock
    let c: Chan[Int] = channel(0)
    let done: Chan[Int] = channel(0)
    let w1: Chan[Str] = channel(0)
    let w2: Chan[Str] = channel(0)
    let lonely: Chan[Str] = channel(0)
    let never: Chan[Int] = channel(0)
    let sums = group g {
        g.go(|| {
            let sent = group h {
                h.go(|| produce(c, 1000))
                h.go(|| produce(c, 1000))
                h.go(|| produce(c, 1000))
            }
            done.close()
            0
        })
        g.go(|| consume(c, done, clock))
        g.go(|| consume(c, done, clock))
        // a dispatcher choosing between two unbuffered workers
        g.go(|| {
            let mut i = 0
            while i < 1000 {
                select {
                    w1.send("m{i}") => (),
                    w2.send("m{i}") => (),
                }
                i = i + 1
            }
            w1.close()
            w2.close()
            0
        })
        g.go(|| {
            let mut n = 0
            while w1.recv().is_some() {
                n = n + 1
            }
            n
        })
        g.go(|| {
            let mut n = 0
            while w2.recv().is_some() {
                n = n + 1
            }
            n
        })
        // cancelled while waiting to hand over a string
        g.go(|| {
            let r = group h until clock.after(20) {
                h.go(|| {
                    lonely.send("never taken")
                    0
                })
                h.go(|| {
                    select {
                        d = never.recv() => 0,
                        lonely.send("nor this") => 0,
                    }
                })
            }
            0
        })
    }
    root.stdout.print("{sums[1] + sums[2]} {sums[4] + sums[5]}")
}
|} "1498500 1000\n";
  out "catch-in-task-frees-request" "struct Req {\n    path: Str,\n    hs: List[Str],\n}\n\nfn handler(r: Req) -> Str {\n    let e: List[Str] = []\n    r.path + e[0]\n}\n\nfn serve(n: Int) -> Str {\n    let req = Req { path: \"/p{n}\", hs: [\"a{n}\", \"b\"] }\n    catch(|| handler(req)) ?? \"500\"\n}\n\nfn main(root: Root) {\n    let rs = group g {\n        for n in [1, 2, 3] {\n            g.go(|| serve(n))\n        }\n    }\n    root.stdout.print(\"{rs}\")\n}\n" "[\"500\", \"500\", \"500\"]\n";
  errs "catch-arity" (main_wrap "    let r = catch(1, 2)") [ "E0107" ];

  (* ---------- `??` ---------- *)
  out "else-operator" "fn find(xs: List[Int], x: Int) -> Option[Int] {\n    let mut i = 0\n    while i < xs.len() {\n        if xs[i] == x {\n            return Some(i)\n        }\n        i = i + 1\n    }\n    None\n}\n\nfn boom() -> Int {\n    panic(\"evaluated\")\n}\n\nfn main(root: Root) {\n    let out = root.stdout\n    let xs = [5, 6, 7]\n    out.print(\"{find(xs, 6) ?? -1} {find(xs, 9) ?? -1} {(find(xs, 7) ?? 0) + 10}\")\n    out.print(\"{\"42\".parse_int() ?? 0} {\"x\".parse_int() ?? 0 + 1} {find(xs, 5) ?? boom()}\")\n    let r: Result[Str, Str] = Err(\"no\")\n    out.print(r ?? \"fallback\")\n    let nested: Option[Option[Int]] = Some(None)\n    out.print(\"{nested ?? Some(3) ?? 4}\")\n}\n" "1 -1 12\n42 1 0\nfallback\n4\n";
  (let src = "fn main(root: Root) {\n    let x = \"4\".parse_int().unwrap_or(0).to_str() + \"!\"\n    let y = \"4\".parse_int().unwrap_or(1 + 2) * 2\n}\n" in
   let _, ds, _ = compile "unwrap-or-fix" src in
   ok "unwrap-or-gone" (codes ds = [ "E0105"; "E0105" ]) (String.concat " " (codes ds));
   let fixed = apply_fixes src ds in
   ok "unwrap-or-fixed" (fixed = "fn main(root: Root) {\n    let x = (\"4\".parse_int() ?? 0).to_str() + \"!\"\n    let y = \"4\".parse_int() ?? (1 + 2) * 2\n}\n") fixed;
   let _, ds2, _ = compile "unwrap-or-fixed2" fixed in
   ok "unwrap-or-fixed-checks" (ds2 = []) (String.concat " " (codes ds2)));

  (* the interpreter changes a list in place when one variable owns it: no
     change may show through another variable, an argument, a closure or a
     struct that holds the same list *)
  out "list-in-place-value-semantics"
    {|struct Box {
    items: List[Int],
}

fn grow(xs: List[Int]) -> List[Int] {
    let mut ys = xs
    ys.push(99)
    ys[0] = -1
    ys
}

fn main(root: Root) {
    let out = root.stdout
    // a copy keeps its value when the original changes, and the other way round
    let mut a = [1, 2]
    a.push(3)
    let b = a
    a.push(4)
    a[0] = 10
    out.print("{a} {b}")
    let mut c = b
    c.push(5)
    out.print("{b} {c}")
    // passing a list does not let the callee change the caller's
    let d = grow(a)
    out.print("{a} {d}")
    // a closure keeps the value it captured
    let mut e = [7]
    e.push(8)
    let f = || e.len()
    e.push(9)
    out.print("{f()} {e.len()}")
    // a struct field keeps its list
    let mut g = [1]
    g.push(2)
    let bx = Box { items: g }
    g.push(3)
    g[0] = 0
    out.print("{bx.items} {g}")
    // iterating sees the list as it was; pushing while iterating is safe
    let mut h = [1, 2, 3]
    for x in h {
        h.push(x * 10)
    }
    out.print("{h}")
    // the argument may read the list being pushed to
    let mut k = [5]
    k.push(k.len())
    k.push(k[0] + k[1])
    k.push(grow(k).len())
    k[0] = k.len()
    out.print("{k}")
    // lists of lists
    let mut rows = [[1], [2]]
    let first = rows[0]
    rows[0] = [9, 9]
    rows.push(first)
    out.print("{rows} {first}")
    // growing far past the first capacity, then reading back
    let mut big = []
    let mut i = 0
    while i < 1000 {
        big.push(i)
        i = i + 1
    }
    let mut sum = 0
    i = 0
    while i < big.len() {
        sum = sum + big[i]
        i = i + 1
    }
    let copy = big
    big[999] = 0
    out.print("{sum} {big.len()} {copy[999]} {big[999]} {big.is_empty()} {big.contains(500)}")
    // out of range, in place
    let r = catch(|| {
        let mut z = [1]
        z.push(2)
        z[5] = 1
    })
    out.print("{r}")
}
|}
    "[10, 2, 3, 4] [1, 2, 3]\n[1, 2, 3] [1, 2, 3, 5]\n[10, 2, 3, 4] [-1, 2, 3, 4, 99]\n2 3\n[1, 2] [0, 2, 3]\n[1, 2, 3, 10, 20, 30]\n[4, 1, 6, 4]\n[[9, 9], [2], [1]] [1]\n499500 1000 999 0 false true\nErr(\"index 5 is out of range for a list of length 2\")\n";

  (* ---------- type aliases ---------- *)
  let shapes = "pub type Point = (Float, Float)\npub type Path = List[Point]\ntype Secret = Int\n" in
  out "type-alias" ~modules:[ ("shapes", shapes) ]
    {|use shapes

type Counts = Map[Str, Int]
type Pair[T] = (T, T)
type Handler = fn(Str) -> Str
type Parsed[T] = Result[T, Str]

fn tally(words: List[Str]) -> Counts {
    let mut c: Counts = map()
    for w in words {
        c[w] = c.get(w) ?? 0 + 1
    }
    c
}

fn swap[T](p: Pair[T]) -> Pair[T] {
    (p.1, p.0)
}

fn words(self: Counts) -> Str {
    "{self.len()} words"
}

fn number(s: Str) -> Parsed[Int] {
    match s.parse_int() {
        Some(n) => Ok(n),
        None => Err("not a number: {s}"),
    }
}

fn length(p: shapes.Path) -> Int {
    p.len()
}

struct Ev {
    type: Str,
}

fn main(root: Root) {
    let out = root.stdout
    let c = tally(["a", "b", "a"])
    let up: Handler = |s| s.to_upper()
    let m: Map[Str, Int] = c
    out.print("{c} {swap((1, 2))} {swap(("x", "y"))} {up("hi")} {c.words()} {m == c}")
    out.print("{number("12")} {number("x")} {length([(0.0, 1.0), (2.0, 3.0)])}")
    let type = Ev { type: "click" }
    out.print(type.type)
}
|}
    "{\"a\": 2, \"b\": 1} (2, 1) (\"y\", \"x\") HI 2 words true\nOk(12) Err(\"not a number: x\") 2\nclick\n";
  errs "type-alias-cycle" "type A = List[B]\ntype B = Option[A]\n\nfn main(root: Root) {\n    let x: A = []\n}\n" [ "E0116"; "E0116" ];
  errs "type-alias-self" "type T = T\n\nfn main(root: Root) {}\n" [ "E0116" ];
  errs "type-alias-arity" "type Pair[T] = (T, T)\n\nfn main(root: Root) {\n    let p: Pair = (1, 2)\n    let q: Pair[Int, Int] = (1, 2)\n}\n" [ "E0109"; "E0109" ];
  errs "type-alias-twice" "struct P {\n    x: Int,\n}\n\ntype P = Int\n\nfn main(root: Root) {}\n" [ "E0106" ];
  errs "type-alias-builtin" "type Int = Float\n\nfn main(root: Root) {}\n" [ "E0106" ];
  errs "type-alias-unknown" "type X = Nope\n\nfn main(root: Root) {}\n" [ "E0102" ];
  errs "type-alias-private" ~modules:[ ("shapes", shapes) ] "use shapes\n\nfn main(root: Root) {\n    let s: shapes.Secret = 1\n}\n" [ "E0402" ];
  errs "type-alias-mismatch" "type Meters = Float\n\nfn main(root: Root) {\n    let m: Meters = \"far\"\n}\n" [ "E0201" ];

  (* ---------- map and flat_map on Result and Option ---------- *)
  out "result-map-flat-map"
    {|fn half(n: Int) -> Result[Int, Str] {
    if n % 2 == 0 { Ok(n / 2) } else { Err("{n} is odd") }
}

fn main(root: Root) {
    let out = root.stdout
    let r: Result[Int, Str] = Ok(20)
    out.print("{r.map(|x| x + 1)} {r.flat_map(half)} {r.flat_map(half).flat_map(half).flat_map(half)}")
    let e: Result[Int, Str] = Err("no")
    out.print("{e.map(|x| x * 2)} {e.flat_map(half)} {r.map(|x| "#{x}")}")
    let o = Some(8)
    out.print("{o.flat_map(|x| if x > 5 { Some([x]) } else { None })} {"7".parse_int().flat_map(|n| half(n).map(|h| Some(h)) ?? None)}")
    let none: Option[Int] = None
    out.print("{none.flat_map(|x| Some(x))} {[1, 2].flat_map(|x| [x, x])}")
}
|}
    "Ok(21) Ok(10) Err(\"5 is odd\")\nErr(\"no\") Err(\"no\") Ok(\"#20\")\nSome([8]) None\nNone [1, 1, 2, 2]\n";
  (let src = "fn main(root: Root) {\n    let r: Result[Int, Str] = Ok(1)\n    let o = Some(2)\n    let a = r.and_then(|x| Ok(x + 1))\n    let b = o.and_then(|x| Some(x))\n}\n" in
   let _, ds, _ = compile "and-then-fix" src in
   ok "and-then-gone" (codes ds = [ "E0105"; "E0105" ]) (String.concat " " (codes ds));
   let fixed = apply_fixes src ds in
   let _, ds2, _ = compile "and-then-fixed" fixed in
   ok "and-then-fixed-checks" (ds2 = [] && Interp_util.find_sub fixed "r.flat_map(" 0 <> None) fixed);

  (* ---------- collection methods (std/methods.tsr) ---------- *)
  out "collections-search"
    {|struct P {
    name: Str,
    age: Int,
}

fn main(root: Root) {
    let out = root.stdout
    let xs = [3, 1, 4, 1, 5, 9, 2, 6]
    let none: List[Int] = []
    out.print("{xs.any(|x| x > 8)} {xs.any(|x| x > 9)} {none.any(|x| true)}")
    out.print("{xs.all(|x| x > 0)} {xs.all(|x| x > 1)} {none.all(|x| false)}")
    out.print("{xs.count(|x| x % 2 == 0)} {none.count(|x| true)}")
    out.print("{xs.find(|x| x > 3)} {xs.find(|x| x > 30)} {none.find(|x| true)}")
    out.print("{xs.sum_by(|x| x * 2)} {none.sum_by(|x| x)}")
    out.print("{xs.min()} {xs.max()} {none.min()} {none.max()} {["pear", "fig", "apple"].min()} {[2.5, -1.0].max()}")
    let ps = [P { name: "ann", age: 30 }, P { name: "bob", age: 25 }, P { name: "cy", age: 30 }, P { name: "di", age: 25 }]
    out.print("{ps.min_by(|p| p.age).map(|p| p.name)} {ps.max_by(|p| p.age).map(|p| p.name)}")
    out.print("{ps.min_by(|p| p.name.len())} {none.min_by(|x| x)} {none.max_by(|x| x)}")
    out.print("{ps.max_by(|p| (p.age, p.name)).map(|p| p.name)}")
}
|}
    {|true false false
true false true
3 0
Some(4) None None
62 0
Some(1) Some(9) None None Some("apple") Some(2.5)
Some("bob") Some("ann")
Some(P { name: "cy", age: 30 }) None None
Some("cy")
|};
  out "collections-reshape"
    {|fn main(root: Root) {
    let out = root.stdout
    let xs = [3, 1, 4, 1, 5, 9, 2, 6]
    let none: List[Int] = []
    out.print("{xs.flat_map(|x| [x, x * 10]).take(6)} {none.flat_map(|x| [x])} {xs.flat_map(|x| if x > 4 { [x] } else { [] })}")
    out.print("{["a b", "c", ""].flat_map(|s| s.split(" "))}")
    out.print("{xs.take_while(|x| x < 5)} {xs.take_while(|x| x < 100)} {xs.take_while(|x| x > 100)} {none.take_while(|x| true)}")
    out.print("{xs.skip_while(|x| x < 5)} {xs.skip_while(|x| x < 100)} {xs.skip_while(|x| x > 100)} {none.skip_while(|x| true)}")
    out.print("{["a", "b"].with_index()} {none.with_index()}")
    for p in ["x", "y"].with_index() {
        out.print("{p.0}={p.1}")
    }
    out.print("{xs.zip(["a", "b", "c"])} {["a"].zip(xs)} {none.zip(xs)} {xs.zip(none)}")
    out.print("{xs.distinct()} {["b", "a", "b", "c", "a"].distinct()} {[(1, "x"), (1, "y"), (1, "x")].distinct()} {none.distinct()}")
    out.print("{["apple", "avocado", "banana", "blueberry", "cherry"].distinct_by(|s| s.slice(0, 1))} {none.distinct_by(|x| x)}")
    out.print("{xs.sort_by(|x| -x)} {["ccc", "a", "bb", "dd", "e"].sort_by(|s| s.len())} {none.sort_by(|x| x)}")
    out.print("{[(2, "b"), (1, "z"), (2, "a"), (1, "y")].sort_by(|p| p.0)} {[1.5, -0.5, 0.0].sort_by(|x| x)}")
    out.print("{xs.reverse()}")
}
|}
    {|[3, 30, 1, 10, 4, 40] [] [5, 9, 6]
["a", "b", "c", ""]
[3, 1, 4, 1] [3, 1, 4, 1, 5, 9, 2, 6] [] []
[5, 9, 2, 6] [] [3, 1, 4, 1, 5, 9, 2, 6] []
[(0, "a"), (1, "b")] []
0=x
1=y
[(3, "a"), (1, "b"), (4, "c")] [("a", 3)] [] []
[3, 1, 4, 5, 9, 2, 6] ["b", "a", "c"] [(1, "x"), (1, "y")] []
["apple", "banana", "cherry"] []
[9, 6, 5, 4, 3, 2, 1, 1] ["a", "e", "bb", "dd", "ccc"] []
[(1, "z"), (1, "y"), (2, "b"), (2, "a")] [-0.5, 0.0, 1.5]
[6, 2, 9, 5, 1, 4, 1, 3]
|};
  out "collections-split"
    {|fn main(root: Root) {
    let out = root.stdout
    let xs = [1, 2, 3, 4, 5, 6, 7]
    let none: List[Int] = []
    out.print("{xs.partition(|x| x % 2 == 0)} {none.partition(|x| true)} {xs.partition(|x| true).1}")
    let halves = xs.partition(|x| x <= 3)
    out.print("{halves.0.len()} {halves.1.sum()}")
    out.print("{xs.chunks(3)} {xs.chunks(7)} {xs.chunks(10)} {xs.chunks(1).len()}")
    out.print("{xs.chunks(0)} {xs.chunks(-2)} {none.chunks(2)}")
    out.print("{xs.windows(3)} {xs.windows(7)} {xs.windows(8)} {xs.windows(0)} {none.windows(1)}")
    out.print("{[1, 1, 2, 3, 3, 3, 1].chunk_by(|x| x)} {xs.chunk_by(|x| x <= 4)} {none.chunk_by(|x| x)} {[5].chunk_by(|x| x)}")
    let words = ["apple", "avocado", "banana", "cherry", "cranberry", "apricot"]
    out.print("{words.chunk_by(|w| w.slice(0, 1)).map(|run| "{run.len()}{run[0].slice(0, 1)}")}")
}
|}
    {|([2, 4, 6], [1, 3, 5, 7]) ([], []) []
3 22
[[1, 2, 3], [4, 5, 6], [7]] [[1, 2, 3, 4, 5, 6, 7]] [[1, 2, 3, 4, 5, 6, 7]] 7
[] [] []
[[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 6], [5, 6, 7]] [[1, 2, 3, 4, 5, 6, 7]] [] [] []
[[1, 1], [2], [3, 3, 3], [1]] [[1, 2, 3, 4], [5, 6, 7]] [] [[5]]
["2a", "1b", "2c", "1a"]
|};
  out "collections-maps"
    {|struct Sale {
    city: Str,
    amount: Int,
}

fn main(root: Root) {
    let out = root.stdout
    let none: List[Int] = []
    let sales = [Sale { city: "Oslo", amount: 10 }, Sale { city: "Rome", amount: 5 }, Sale { city: "Oslo", amount: 7 }]
    let by_city = sales.group_by(|s| s.city)
    out.print("{by_city.keys()} {by_city["Oslo"].map(|s| s.amount)} {none.group_by(|x| x)}")
    out.print("{[1, 2, 3, 4, 5, 6].group_by(|x| x % 3)}")
    out.print("{[1, 2, 3, 4, 5, 6].group_by(|x| (x % 2 == 0, x > 3))}")
    out.print("{["a", "bb", "cc", "d", "eee"].count_by(|s| s.len())} {none.count_by(|x| x)}")
    out.print("{"mississippi".chars().tally()} {none.tally()} {[true, false, true].tally()}")
    let totals = by_city.map_values(|ss| ss.sum_by(|s| s.amount))
    out.print("{totals} {totals.map_values(|n| "{n}!")}")
    let counts = "the cat and the hat and the bat".split(" ").tally()
    out.print("{counts.filter(|w, n| n > 1)} {counts.filter(|w, n| w.len() > 5)} {counts.filter(|w, n| true).len()}")
    let a = map([("x", 1), ("y", 2)])
    let b = map([("y", 20), ("z", 30)])
    let e: Map[Str, Int] = map()
    out.print("{a.merge(b)} {b.merge(a)} {a.merge(e)} {e.merge(a)} {a}")
}
|}
    {|["Oslo", "Rome"] [10, 7] {}
{1: [1, 4], 2: [2, 5], 0: [3, 6]}
{(false, false): [1, 3], (true, false): [2], (true, true): [4, 6], (false, true): [5]}
{1: 2, 2: 2, 3: 1} {}
{"m": 1, "i": 4, "s": 4, "p": 2} {} {true: 2, false: 1}
{"Oslo": 17, "Rome": 5} {"Oslo": "17!", "Rome": "5!"}
{"the": 3, "and": 2} {} 5
{"x": 1, "y": 20, "z": 30} {"y": 2, "z": 30, "x": 1} {"x": 1, "y": 2} {"x": 1, "y": 2} {"x": 1, "y": 2}
|};
  out "collections-heap"
    {|struct Item {
    name: Str,
    tags: List[Str],
}

fn main(root: Root) {
    let out = root.stdout
    let words = "one two three four five six seven eight nine ten".split(" ")
    let items = words.map(|w| Item { name: w + "!", tags: w.chars() })
    out.print("{words.group_by(|w| w.len()).values()}")
    out.print("{words.chunks(4)} {words.windows(9).len()} {words.windows(2)[0]}")
    out.print("{words.flat_map(|w| [w, w.to_upper()]).take(4)} {words.zip(items.map(|i| i.tags.len())).take(3)}")
    out.print("{items.partition(|i| i.tags.contains("e")).0.map(|i| i.name)}")
    out.print("{items.sort_by(|i| i.name).map(|i| i.name).take(3)} {items.max_by(|i| i.tags.len()).map(|i| i.tags)}")
    out.print("{items.min_by(|i| i.name).map(|i| i.name)} {items.find(|i| i.tags.len() == 5).map(|i| i.name)}")
    out.print("{words.distinct_by(|w| w.len())} {words.chunk_by(|w| w.len()).len()} {words.tally().len()}")
    let groups = items.group_by(|i| i.tags.len())
    let names = groups.map_values(|is| is.map(|i| i.name))
    out.print("{names} {names.filter(|k, v| v.len() > 2)}")
    out.print("{names.merge(map([(3, ["x"]), (9, ["y"])]))}")
    out.print("{words.with_index().filter(|p| p.0 % 4 == 0).map(|p| p.1)} {words.take_while(|w| w.len() == 3)} {words.skip_while(|w| w != "eight")}")
}
|}
    {|[["one", "two", "six", "ten"], ["three", "seven", "eight"], ["four", "five", "nine"]]
[["one", "two", "three", "four"], ["five", "six", "seven", "eight"], ["nine", "ten"]] 2 ["one", "two"]
["one", "ONE", "two", "TWO"] [("one", 3), ("two", 3), ("three", 5)]
["one!", "three!", "five!", "seven!", "eight!", "nine!", "ten!"]
["eight!", "five!", "four!"] Some(["t", "h", "r", "e", "e"])
Some("eight!") Some("three!")
["one", "three", "four"] 7 10
{3: ["one!", "two!", "six!", "ten!"], 5: ["three!", "seven!", "eight!"], 4: ["four!", "five!", "nine!"]} {3: ["one!", "two!", "six!", "ten!"], 5: ["three!", "seven!", "eight!"], 4: ["four!", "five!", "nine!"]}
{3: ["x"], 5: ["three!", "seven!", "eight!"], 4: ["four!", "five!", "nine!"], 9: ["y"]}
["one", "five", "nine"] ["one", "two"] ["eight", "nine", "ten"]
|};
  out "collections-generic"
    {|fn lowest[T: Ordered](xs: List[T]) -> Option[T] {
    let mut best = None
    for x in xs {
        best = match best {
            Some(b) => if x < b { Some(x) } else { Some(b) },
            None => Some(x),
        }
    }
    best
}

fn index[K: Key, V](xs: List[V], key: fn(V) -> K) -> Map[K, V] {
    let mut m: Map[K, V] = map()
    for x in xs {
        m[key(x)] = x
    }
    m
}

fn biggest_key[K: Key, V](m: Map[K, V]) -> Option[K] {
    m.keys().max()
}

fn main(root: Root) {
    let out = root.stdout
    out.print("{lowest([3, 1, 2])} {lowest(["b", "a"])} {lowest([(2, "x"), (1, "y")])}")
    let byname = index(["ann", "bob", "al"], |s| s.slice(0, 1))
    out.print("{byname} {biggest_key(byname)}")
    let any = [1, 2, 3].any
    let tally = ["a", "b", "a"].tally
    out.print("{any(|x| x > 2)} {tally()} {[[1, 2], [3]].map(|l| l.sum_by(|x| x * x))}")
    let n = [1, 2, 3, 4, 5, 6].filter(|x| x > 1).map(|x| x * 10).count(|x| x > 30)
    out.print("{n} {[1, 2, 3].map(|x| x + 1).zip([1, 2, 3].filter(|x| x > 1))}")
}
|}
    {|Some(1) Some("a") Some((1, "y"))
{"a": "al", "b": "bob"} Some("b")
true {"a": 2, "b": 1} [5, 9]
3 [(2, 2), (3, 3)]
|};
  errs "collections-distinct-float" (main_wrap "    let a = [1.5].distinct()") [ "E0111" ];
  errs "collections-tally-list" (main_wrap "    let a = [[1]].tally()") [ "E0111" ];
  errs "collections-group-by-float" (main_wrap "    let a = [1.5, 2.0].group_by(|x| x)") [ "E0111" ];
  errs "collections-min-unordered" (main_wrap "    let a = [map([(1, 2)])].min()") [ "E0111" ];
  errs "collections-sort-by-less" (main_wrap "    let a = [1].sort_by(|a, b| a < b)") [ "E0201" ];
  errs "collections-count-arity" (main_wrap "    let a = [1].count()") [ "E0107" ];
  errs "collections-map-filter-pair" (main_wrap "    let m = map([(1, 2)]).filter(|e| true)") [ "E0201" ];
  errs "collections-clash" "fn tally(self: List[Int]) -> Int {\n    0\n}\n\nfn main() {\n}\n" [ "E0106" ];
  errs "bound-ordered-missing" "fn f[T](xs: List[T]) -> Option[T] {\n    xs.max()\n}\n\nfn main() {\n}\n" [ "E0111" ];
  errs "bound-ordered-lt" "fn f[T](a: T, b: T) -> Bool {\n    a < b\n}\n\nfn main() {\n}\n" [ "E0201" ];
  errs "bound-key-missing" "fn f[K, V](k: K, v: V) -> Map[K, V] {\n    map([(k, v)])\n}\n\nfn main() {\n}\n" [ "E0113" ];
  errs "bound-key-float" "fn f[K: Key](k: K) -> Map[K, Int] {\n    map([(k, 1)])\n}\n\nfn main() {\n    let m = f(1.5)\n}\n" [ "E0111" ];
  errs "bound-dyn" (main_wrap "    let d: dyn Key = 1") [ "E0102" ];
  errs "bound-iface-name" "interface Ordered {\n    fn k(self) -> Int\n}\n\nfn main() {\n}\n" [ "E0106" ];
  panics "collections-callback-panic" (main_wrap "    let xs = [1, 0]\n    out.print(\"{xs.find(|x| 10 / x > 100)}\")") "division by zero";
  (* the methods written in Tessera stay out of `tessera caps` *)
  let _, _, infos = compile "collections-caps" (main_wrap "    out.print(\"{[1].any(|x| x > 0)}\")") in
  ok "collections-caps" (List.for_all (fun (i : Check.fn_info) -> i.span.file <> Prelude.methods_file) infos) "a built-in method is listed";

  (* ---------- pipes: `x |> f(a)` is `f(x, a)` ---------- *)
  out "pipe-basic" {|use list

struct Acc {
    total: Int,
}

fn add(self: Acc, n: Int, k: Int) -> Acc { Acc { total: self.total + n * k } }

fn double(x: Int) -> Int { x * 2 }

fn plus(x: Int, y: Int) -> Int { x + y }

fn evens(xs: List[Int]) -> List[Int] { xs.filter(|x| x % 2 == 0) }

fn total(xs: List[Int]) -> Int { xs.fold(0, |a, b| a + b) }

fn parse(s: Str) -> Result[Int, Str] {
    if s == "" { Err("empty") } else { Ok(s.len()) }
}

fn twice(s: Str) -> Result[Int, Str] {
    let n = s |> parse()?
    Ok(n |> plus(n))
}

fn main(root: Root) {
    let out = root.stdout
    out.print("{3 |> double |> plus(1)}")
    let t = [1, 2, 3, 4, 5, 6]
        |> evens
        |> total
    out.print("{t} {(t |> double) + 1} {1 + 2 |> double}")
    out.print("{5 |> (|x| x * 10)}")
    let words = ["pear", "fig", "apple"]
        |> list.sort_by(|a, b| a.len() < b.len())
    out.print("{words} {(words |> list.index_of("pear")) ?? -1}")
    let acc = Acc { total: 0 }
    out.print("{(4 |> acc.add(10)).total}")
    out.print("{twice("abc")} {twice("")}")
    // strings and lists move through the pipeline without leaking
    let s = "a,b,c".split(",") |> evens_str
    out.print(s)
}

fn evens_str(xs: List[Str]) -> Str { xs.join("+") }
|} "7\n12 25 6\n50\n[\"fig\", \"pear\", \"apple\"] 1\n40\nOk(6) Err(\"empty\")\na+b+c\n";
  errs "pipe-not-callable" (main_wrap "    let x = 3 |> 4") [ "E0003" ];
  errs "pipe-binary-rhs" (main_wrap "    let x = 3 |> (1 + 2)") [ "E0003" ];
  errs "pipe-arity" "fn plus(x: Int, y: Int) -> Int { x + y }\n\nfn main(root: Root) {\n    let x = 3 |> plus(1, 2)\n}\n" [ "E0107" ];
  errs "pipe-type" "fn plus(x: Int, y: Int) -> Int { x + y }\n\nfn main(root: Root) {\n    let x = \"a\" |> plus(1)\n}\n" [ "E0201" ];
  (* a line may start with `|>`, but not with another binary operator *)
  errs "pipe-newline" (main_wrap "    let x = 3\n        |> double\n}\n\nfn double(x: Int) -> Int {\n    x * 2") [];

  (* ---------- copy with changes: `{ p with x: 1 }` ---------- *)
  let with_src = {|struct User {
    name: Str,
    tags: List[Str],
    age: Int,
}

struct Pair[A, B] {
    first: A,
    second: B,
}

fn older(u: User) -> User { { u with age: u.age + 1 } }

fn make(out: Stdout) -> User {
    out.print("make")
    User { name: "m", tags: [], age: 0 }
}

fn side(out: Stdout, n: Int) -> Int {
    out.print("side {n}")
    n
}

fn main(root: Root) {
    let out = root.stdout
    let a = User { name: "ann", tags: ["x"], age: 30 }
    let b = { a with name: "bob" }
    out.print("{a} {b}")
    // the original is used again, so this one is a copy
    let mut c = older(b)
    out.print("{b.age} {c.age}")
    c = { c with tags: ["y", "z"], age: 1 }
    let name = "cy"
    out.print("{c} {{ c with name }}")
    // generic structs, and a base that is not a variable: evaluated first
    let p = Pair { first: 1, second: "one" }
    out.print("{{ p with second: "uno" }} {{ make(out) with age: side(out, 7) }.age}")
    // in a loop, and captured by a closure
    let mut u = User { name: "loop", tags: [], age: 0 }
    for i in 0..<3 {
        let mut t = u.tags
        t.push("t{i}")
        u = { u with age: u.age + i, tags: t }
    }
    let rename = |n: Str| { u with name: n }
    out.print("{u} {rename("other").name} {u.name}")
    let users = [a, b].map(|x| { x with tags: [] })
    out.print("{users}")
    let multi = {
        u with
        name: "multi",
        age: 99,
    }
    out.print("{multi.name} {multi.age}")
}
|} in
  out "with-copy" with_src
    "User { name: \"ann\", tags: [\"x\"], age: 30 } User { name: \"bob\", tags: [\"x\"], age: 30 }\n30 31\nUser { name: \"bob\", tags: [\"y\", \"z\"], age: 1 } User { name: \"cy\", tags: [\"y\", \"z\"], age: 1 }\nmake\nside 7\nPair { first: 1, second: \"uno\" } 7\nUser { name: \"loop\", tags: [\"t0\", \"t1\", \"t2\"], age: 3 } other loop\n[User { name: \"ann\", tags: [], age: 30 }, User { name: \"bob\", tags: [], age: 30 }]\nmulti 99\n";
  (* natively, a base that is not used afterwards is reused in place: moved,
     with only the replaced field written, never copied *)
  (let items, ds, _ = compile "with-reuse" with_src in
   let c = if ds = [] then Native.c_source items else "" in
   let has s sub =
     let n = String.length s and m = String.length sub in
     let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
     go 0
   in
   let rec from = function
     | [] -> []
     | l :: r -> if String.starts_with ~prefix:"static" l && String.ends_with ~suffix:"{" l && has l "wf_older(" then upto r else from r
   and upto = function [] | "}" :: _ -> [] | l :: r -> l :: upto r in
   let body = String.concat "\n" (from (String.split_on_char '\n' c)) in
   ok "with-reuse" (body <> "" && not (has body "_dup(")) body);
  errs "with-unknown-field" "struct P {\n    x: Int,\n}\n\nfn main(root: Root) {\n    let p = P { x: 1 }\n    let q = { p with y: 2 }\n}\n" [ "E0103" ];
  errs "with-twice" "struct P {\n    x: Int,\n}\n\nfn main(root: Root) {\n    let p = P { x: 1 }\n    let q = { p with x: 2, x: 3 }\n}\n" [ "E0106" ];
  errs "with-field-type" "struct P {\n    x: Int,\n}\n\nfn main(root: Root) {\n    let p = P { x: 1 }\n    let q = { p with x: \"a\" }\n}\n" [ "E0201" ];
  errs "with-not-struct" (main_wrap "    let n = 5\n    let q = { n with x: 1 }") [ "E0201" ];
  errs "with-option" (main_wrap "    let o = Some(1)\n    let q = { o with x: 1 }") [ "E0201" ];
  errs "with-tuple" (main_wrap "    let t = (1, 2)\n    let q = { t with x: 1 }") [ "E0201" ];
  errs "with-unknown-base" (main_wrap "    let q = { nope with x: 1 }") [ "E0101" ];
  (* `with` only starts a copy right after the base, followed by a field *)
  errs "with-not-a-copy" (main_wrap "    let with = 1\n    let x = { with }\n    out.print(\"{x}\")") [];

  (* ---------- guards in match arms ---------- *)
  out "match-guard" {|enum Shape {
    Circle(Float),
    Rect(Float, Float),
}

enum Msg {
    Text(Str),
    Batch(List[Str]),
}

fn size(o: Option[Int]) -> Str {
    match o {
        Some(n) if n > 10 => "big {n}",
        Some(n) if n < 0 => "negative",
        Some(n) => "small {n}",
        None => "none",
    }
}

fn kind(s: Shape) -> Str {
    match s {
        Rect(w, h) if w == h => "square",
        Rect(_, _) => "rect",
        c if false => "never",
        Circle(r) => "circle {r}",
    }
}

fn seen(out: Stdout, tag: Str, ok: Bool) -> Bool {
    out.print("guard {tag}")
    ok
}

// heap payloads bound for a guard that fails are released
fn describe(m: Msg) -> Str {
    match m {
        Text(s) if s.len() > 3 => "long text {s}",
        Batch(xs) if xs.len() == 0 => "empty batch",
        Batch(xs) if xs.contains("stop") => "stop in {xs}",
        Text(s) => "text {s}",
        Batch(xs) => "batch of {xs.len()}",
    }
}

fn main(root: Root) {
    let out = root.stdout
    out.print("{size(Some(42))} {size(Some(-1))} {size(Some(3))} {size(None)}")
    out.print("{kind(Rect(2.0, 2.0))} {kind(Rect(1.0, 2.0))} {kind(Circle(1.5))}")
    // guards run in order, only until an arm applies
    let r = match 5 {
        n if seen(out, "a", n > 7) => "a",
        n if seen(out, "b", n > 3) => "b",
        n if seen(out, "c", true) => "c",
        _ => "none",
    }
    out.print(r)
    for m in [Text("hi"), Text("hello"), Batch([]), Batch(["go", "stop"]), Batch(["x"])] {
        out.print(describe(m))
    }
    let pair = ("hello", 3)
    let t = match pair {
        (s, k) if s.len() > k => "long {s}",
        (s, _) => "short {s}",
    }
    out.print(t)
}
|} "big 42 negative small 3 none\nsquare rect circle 1.5\nguard a\nguard b\nb\ntext hi\nlong text hello\nempty batch\nstop in [\"go\", \"stop\"]\nbatch of 1\nlong hello\n";
  (* a guarded arm covers nothing, so the match must still be exhaustive *)
  errs "match-guard-not-exhaustive" "fn f(o: Option[Int]) -> Int {\n    match o {\n        Some(n) if n > 0 => n,\n        None => 0,\n    }\n}\n" [ "E0204" ];
  errs "match-guard-all-guarded" "fn f(n: Int) -> Int {\n    match n {\n        k if k > 0 => 1,\n        k if k <= 0 => 0,\n    }\n}\n" [ "E0204" ];
  errs "match-guard-exhaustive" "fn f(o: Option[Int]) -> Int {\n    match o {\n        Some(n) if n > 0 => n,\n        Some(_) => 0,\n        None => -1,\n    }\n}\n" [];
  errs "match-guard-type" "fn f(o: Option[Int]) -> Int {\n    match o {\n        Some(n) if n => n,\n        _ => 0,\n    }\n}\n" [ "E0201" ];
  errs "match-guard-unbound" "fn f(o: Option[Int]) -> Int {\n    match o {\n        Some(n) if m > 0 => n,\n        _ => 0,\n    }\n}\n" [ "E0101" ];
  (let _, ds, _ = compile "match-guard-fix" "fn f(o: Option[Int]) -> Int {\n    match o {\n        Some(n) if n > 0 => n,\n        None => 0,\n    }\n}\n" in
   ok "match-guard-fix" (List.exists (fun (d : Diag.t) -> d.message = "`match` does not cover `Some(_)`") ds) (String.concat "; " (List.map (fun (d : Diag.t) -> d.message) ds)));

  (* ---------- safe navigation: `a?.b` on an Option ---------- *)
  out "safe-nav" {|struct Address {
    city: Str,
    zip: Option[Str],
    lines: List[Str],
}

struct User {
    name: Str,
    address: Option[Address],
}

fn find(id: Int) -> Option[User] {
    if id == 1 {
        Some(User { name: "ann", address: Some(Address { city: "Rome", zip: None, lines: ["Via Roma 1"] }) })
    } else if id == 2 {
        Some(User { name: "bob", address: None })
    } else {
        None
    }
}

fn shout(self: Str) -> Str { self.to_upper() }

// `?` after a chain applies to the whole chain: None returns early
fn city_len(id: Int) -> Option[Int] {
    let n = find(id)?.address?.city.len()?
    Some(n * 10)
}

fn parse(s: Str) -> Result[User, Str] {
    if s == "" { Err("empty") } else { Ok(User { name: s, address: None }) }
}

// `(e?).name` is try-then-field; `e?.name` would be safe navigation
fn name_of(s: Str) -> Result[Str, Str] {
    Ok((parse(s)?).name)
}

fn upto(o: Option[Int]) -> Option[List[Int]] {
    Some(0..<o?)
}

fn upto2(o: Option[Int]) -> Option[List[Int]] {
    Some(o?..<3)
}

fn main(root: Root) {
    let out = root.stdout
    for id in [1, 2, 3] {
        let u = find(id)
        out.print("{u?.name ?? "nobody"} {u?.address?.city ?? "unknown"} {u?.name.shout().len()} {u?.address?.zip ?? "-"} {u?.address?.lines.get(0)}")
        out.print("{city_len(id)}")
    }
    let t: Option[(Int, Str)] = Some((1, "x"))
    out.print("{t?.1} {t?.0 ?? 0}")
    let xs: Option[List[Int]] = Some([1, 2, 3])
    let none: Option[List[Int]] = None
    // an Option result is not wrapped again
    out.print("{xs?.map(|x| x * 2)} {xs?.get(1)} {xs?.len()} {none?.get(0)}")
    let deep = find(1)
        ?.address
        ?.city
    out.print("{deep}")
    let get_city = |id: Int| find(id)?.address?.city ?? "?"
    out.print("{get_city(1)} {get_city(2)}")
    out.print("{name_of("zed")} {name_of("")} {upto(Some(3))} {upto2(Some(1))} {upto(None)}")
}
|} "ann Rome Some(3) - Some(\"Via Roma 1\")\nSome(40)\nbob unknown Some(3) - None\nNone\nnobody unknown None - None\nNone\nSome(\"x\") 1\nSome([2, 4, 6]) Some(2) Some(3) None\nSome(\"Rome\")\nRome ?\nOk(\"zed\") Err(\"empty\") Some([0, 1, 2]) Some([1, 2]) None\n";
  errs "safe-nav-not-option" (main_wrap "    let n = 5\n    let x = n?.abs()") [ "E0201" ];
  errs "safe-nav-result" "fn parse(s: Str) -> Result[Int, Str] {\n    Ok(1)\n}\n\nfn f() -> Result[Int, Str] {\n    let n = parse(\"x\")?.abs()\n    Ok(1)\n}\n" [ "E0201" ];
  errs "safe-nav-unknown-field" "struct P {\n    x: Int,\n}\n\nfn main(root: Root) {\n    let p = Some(P { x: 1 })\n    let y = p?.y\n}\n" [ "E0103" ];
  errs "safe-nav-assign" "struct P {\n    x: Int,\n}\n\nfn main(root: Root) {\n    let mut p = Some(P { x: 1 })\n    p?.x = 2\n}\n" [ "E0209"; "E0201" ];
  errs "safe-nav-type" "struct P {\n    x: Int,\n}\n\nfn main(root: Root) {\n    let p = Some(P { x: 1 })\n    let y: Int = p?.x\n}\n" [ "E0201" ];

  (* ---------- tuples and maps ---------- *)
  out "tuples" (example "tuples.tsr") "3 2 (2, 1)\n(1, \"one\", 1.5) 1 one 1.5 (2, \"a\")\n4 ((1, 2), (3, (4, 5)))\n1 3 4 5\na1 b2 c3 \nyes zero yes no 7\ntrue false 6\n";
  out "maps" (example "maps.tsr") "{\"the\": 3, \"cat\": 1, \"and\": 2, \"hat\": 1, \"bat\": 1} 5 3 None\n[\"the\", \"cat\", \"and\", \"hat\", \"bat\"] [3, 1, 2, 1, 1]\nthe=3 and=2 \n{\"the\": 100, \"and\": 2, \"hat\": 1, \"bat\": 1, \"zebra\": 9}\n{\"the\": 3, \"cat\": 1, \"and\": 2, \"hat\": 1, \"bat\": 1}\ntrue false false\n{1: 1, 2: 4, 3: 9} true false 0\n1 {\"a\": 1}\nyes\n";
  panics "map-missing-key" (main_wrap "    let m = map([(\"a\", 1)])\n    out.print(\"{m[\"b\"]}\")") "key \"b\" is not in the map";
  out "maps-stress" ~grants:{ Interp.no_grants with seed = Some 7 } "fn main(root: Root) {\n    let out = root.stdout\n    let r = root.rand\n    let mut m: Map[Int, Str] = map()\n    let mut sum = 0\n    let mut i = 0\n    while i < 20000 {\n        let k = r.int(0, 500)\n        let op = r.int(0, 3)\n        if op == 0 {\n            m[k] = \"v{k}\"\n        } else if op == 1 {\n            m.remove(k)\n        } else {\n            sum = sum + m.get(k).map(|s| s.len()) ?? 0\n        }\n        i = i + 1\n    }\n    out.print(\"{m.len()} {sum} {m.keys().take(8)} {m.values().take(3)}\")\n    let copy = m\n    m.remove(m.keys()[0])\n    out.print(\"{copy.len()} {m.len()} {copy == m}\")\n    // maps handed to concurrent tasks\n    let parts = group g {\n        for d in [0, 1, 2, 3] {\n            g.go(|| copy.keys().filter(|k| k % 4 == d).len())\n        }\n    }\n    out.print(\"{parts} {parts.sum() == copy.len()}\")\n}\n" "273 12086 [461, 206, 21, 361, 292, 414, 473, 199] [\"v461\", \"v206\", \"v21\"]\n273 272 false\n[76, 71, 63, 63] true\n";
  errs "map-key-type" (main_wrap "    let mut m = map()\n    m[1.5] = 1") [ "E0113" ];
  errs "map-key-list" (main_wrap "    let mut m = map()\n    m[[1]] = 1") [ "E0113" ];
  errs "map-key-struct-float" "struct P {\n    x: Float,\n}\n\nfn main() {\n    let mut m = map()\n    m[P { x: 1.0 }] = 1\n}\n" [ "E0113" ];
  errs "map-entry-immutable" "struct P {\n    x: Int,\n}\n\nfn main() {\n    let m = map([(1, P { x: 1 })])\n    m[1].x = 2\n}\n" [ "E0205" ];
  out "map-keys-composite"
    ("struct Pos {\n    x: Int,\n    y: Int,\n}\n\nenum Color {\n    Red,\n    Rgb(Int, Int, Int),\n}\n\nenum Path {\n    End,\n    Step(Str, Path),\n}\n\n"
     ^ "fn main(root: Root) {\n    let out = root.stdout\n    let mut grid: Map[Pos, Str] = map()\n    grid[Pos { x: 1, y: 2 }] = \"a\"\n    grid[Pos { x: 0, y: 0 }] = \"b\"\n    grid[Pos { x: 1, y: 2 }] = \"c\"\n"
     ^ "    out.print(\"{grid} {grid.get(Pos { x: 0, y: 0 })} {grid.contains(Pos { x: 5, y: 5 })}\")\n"
     ^ "    let mut pairs = map([((1, \"a\"), 10), ((2, \"b\"), 20)])\n    pairs[(1, \"a\")] = pairs[(1, \"a\")] + 1\n    pairs.remove((2, \"b\"))\n    out.print(\"{pairs} {pairs.keys()}\")\n"
     ^ "    let mut colors = map([(Red, \"red\"), (Rgb(0, 0, 255), \"blue\")])\n    colors[Rgb(0, 255, 0)] = \"green\"\n    out.print(\"{colors[Rgb(0, 0, 255)]} {colors.entries()}\")\n"
     ^ "    let mut paths = map()\n    paths[Step(\"a\", Step(\"b\", End))] = 1\n    paths[Step(\"a\", End)] = 2\n    out.print(\"{paths[Step(\"a\", Step(\"b\", End))]} {paths.get(End)} {paths == map([(Step(\"a\", End), 2), (Step(\"a\", Step(\"b\", End)), 1)])}\")\n"
     ^ "    let mut big: Map[(Int, Int), Int] = map()\n    for i in 0..<1000 {\n        big[(i % 37, i % 11)] = i\n    }\n    out.print(\"{big.len()} {big[(5, 6)]}\")\n}\n")
    ("{Pos { x: 1, y: 2 }: \"c\", Pos { x: 0, y: 0 }: \"b\"} Some(\"b\") false\n{(1, \"a\"): 11} [(1, \"a\")]\n"
     ^ "blue [(Red, \"red\"), (Rgb(0, 0, 255), \"blue\"), (Rgb(0, 255, 0), \"green\")]\n1 None true\n407 930\n");
  out "map-entry-update"
    ("struct Stats {\n    hits: Int,\n    names: List[Str],\n}\n\n"
     ^ "fn main(root: Root) {\n    let out = root.stdout\n    let mut st: Map[Str, Stats] = map()\n    st[\"a\"] = Stats { hits: 0, names: [] }\n"
     ^ "    st[\"a\"].hits = st[\"a\"].hits + 5\n    st[\"a\"].names.push(\"x\")\n    st[\"a\"].names.push(\"y\")\n    st[\"a\"].names[0] = \"z\"\n"
     ^ "    let copy = st\n    st[\"a\"].hits = 99\n    out.print(\"{st} {copy}\")\n"
     ^ "    let mut nested: Map[Str, Map[(Int, Bool), List[Int]]] = map()\n    nested[\"n\"] = map()\n    nested[\"n\"][(3, true)] = [1]\n    nested[\"n\"][(3, true)].push(2)\n    nested[\"n\"].remove((4, false))\n"
     ^ "    let mut lists = [map([(1, Stats { hits: 1, names: [] })])]\n    lists[0][1].hits = 7\n    out.print(\"{nested} {lists}\")\n}\n")
    ("{\"a\": Stats { hits: 99, names: [\"z\", \"y\"] }} {\"a\": Stats { hits: 5, names: [\"z\", \"y\"] }}\n"
     ^ "{\"n\": {(3, true): [1, 2]}} [{1: Stats { hits: 7, names: [] }}]\n");
  panics "map-entry-missing" "struct P {\n    x: Int,\n}\n\nfn main() {\n    let mut m = map([(\"a\", P { x: 1 })])\n    m[\"b\"].x = 2\n}\n" "key \"b\" is not in the map";
  errs "map-remove-immutable" (main_wrap "    let m = map([(1, 2)])\n    m.remove(1)") [ "E0205" ];
  errs "tuple-match-not-exhaustive" "fn f(p: (Bool, Bool)) -> Int {\n    match p {\n        (true, _) => 1,\n        (false, true) => 2,\n    }\n}\n\nfn main() {}\n" [ "E0204" ];
  errs "tuple-let-refutable" "fn main() {\n    let (1, b) = (1, 2)\n}\n" [ "E0003" ];

  (* ---------- dyn ---------- *)
  out "dyn" (example "dyn.tsr")
    "3 5 triangle square\ncircle,square,triangle,circle,square\n41.0\ndemo: 5 Square { s: 2.0 }\n2.25 Square { s: 1.5 }\n[4.0, 2.0, 2.25] abcd\n";
  let dshape = "interface Shape {\n    fn area(self) -> Float\n}\n\nstruct C {\n    r: Float,\n}\n\nfn area(self: C) -> Float {\n    self.r\n}\n\nstruct Bad {\n    n: Int,\n}\n\nstruct Holder[T] {\n    x: T,\n}\n\nfn area[T](self: Holder[T]) -> Float {\n    1.0\n}\n\n" in
  errs "dyn-not-satisfied" (dshape ^ "fn main() {\n    let s: dyn Shape = Bad { n: 1 }\n}\n") [ "E0111" ];
  errs "dyn-builtin" (dshape ^ "fn main() {\n    let s: dyn Shape = 3\n}\n") [ "E0112" ];
  errs "dyn-hides-capability" (dshape ^ "fn main(root: Root) {\n    let s: dyn Shape = Holder { x: root.clock }\n}\n") [ "E0112" ];
  errs "dyn-compare" (dshape ^ "fn main() {\n    let a: dyn Shape = C { r: 1.0 }\n    let b = a == a\n}\n") [ "E0201" ];
  errs "dyn-unknown-method" (dshape ^ "fn main() {\n    let a: dyn Shape = C { r: 1.0 }\n    let b = a.name()\n}\n") [ "E0105" ];
  errs "dyn-unknown-interface" (dshape ^ "fn f(x: dyn Shap) {}\n\nfn main() {}\n") [ "E0102" ];
  let geo = "pub interface Area {\n    fn area(self) -> Int\n}\n\npub fn sum(xs: List[dyn Area]) -> Int {\n    xs.map(|x| x.area()).sum()\n}\n" in
  out "dyn-across-modules" ~modules:[ ("geo", geo) ]
    "use geo\n\nstruct Sq {\n    s: Int,\n}\n\nstruct Ln {\n    n: Int,\n}\n\nfn area(self: Sq) -> Int {\n    self.s * self.s\n}\n\nfn area(self: Ln) -> Int {\n    self.n\n}\n\nfn main(root: Root) {\n    let xs: List[dyn geo.Area] = [Sq { s: 3 }, Ln { n: 4 }]\n    root.stdout.print(\"{geo.sum(xs)} {geo.sum([Ln { n: 1 }])}\")\n}\n"
    "13 1\n";

  (* the expected `dyn` type reaches every branch of `if` and `match` *)
  out "dyn-branches"
    (dshape ^ "struct Sq {\n    s: Float,\n}\n\nfn area(self: Sq) -> Float {\n    self.s * self.s\n}\n\n"
     ^ "fn pick(n: Int) -> dyn Shape {\n    if n > 1 { C { r: 1.5 } } else { Sq { s: 2.0 } }\n}\n\n"
     ^ "fn by_match(n: Int) -> dyn Shape {\n    match n {\n        0 => Sq { s: 3.0 },\n        _ => {\n            let r = 0.5\n            C { r }\n        },\n    }\n}\n\n"
     ^ "fn main(root: Root) {\n    let big = false\n    let s: dyn Shape = if big { C { r: 2.0 } } else { Sq { s: 1.0 } }\n"
     ^ "    let t: dyn Shape = match 3 {\n        1 => Sq { s: 1.5 },\n        _ => C { r: 7.0 },\n    }\n"
     ^ "    let xs: List[dyn Shape] = [s, t, pick(0), pick(2), by_match(0), by_match(1)]\n"
     ^ "    let ys: List[dyn Shape] = if big { [C { r: 1.0 }] } else { [Sq { s: 4.0 }, C { r: 9.0 }] }\n"
     ^ "    root.stdout.print(\"{xs.map(|x| x.area())} {ys} {ys.map(|y| y.area())}\")\n}\n")
    "[1.0, 7.0, 4.0, 1.5, 9.0, 0.5] [Sq { s: 4.0 }, C { r: 9.0 }] [16.0, 9.0]\n";
  errs "dyn-branch-not-satisfied" (dshape ^ "fn main() {\n    let s: dyn Shape = if true { C { r: 1.0 } } else { Bad { n: 1 } }\n}\n") [ "E0111" ];
  errs "if-branches-still-agree" "fn main() {\n    let x = if true { 1 } else { \"a\" }\n}\n" [ "E0201" ];

  (* ---------- the Error interface and `?` into `dyn Error` ---------- *)
  let perr = "struct ParseError {\n    line: Int,\n}\n\nfn message(self: ParseError) -> Str {\n    \"bad line {self.line}\"\n}\n\n" in
  out "error-dyn-try" (perr ^ {|enum Problem {
    Missing(Str),
    Late,
}

fn message(self: Problem) -> Str {
    match self {
        Missing(what) => "missing {what}",
        Late => "late",
    }
}

fn parse(s: Str) -> Result[Int, ParseError] {
    match s.parse_int() {
        Some(n) => Ok(n),
        None => Err(ParseError { line: 3 }),
    }
}

fn risky() -> Result[Int, Str] {
    Err("boom")
}

fn check(n: Int) -> Result[Int, Problem] {
    if n > 10 {
        Err(Late)
    } else if n < 0 {
        Err(Missing("sign"))
    } else {
        Ok(n)
    }
}

fn step(s: Str, fail: Bool) -> Result[Int, dyn Error] {
    let n = parse(s)?
    let m = check(n)?
    let r = if fail { risky()? } else { 0 }
    let caught = catch(|| [1][m])?
    Ok(m + r + caught)
}

fn describe(r: Result[Int, dyn Error]) -> Str {
    match r {
        Ok(n) => "ok {n}",
        Err(e) => "err {e.message()} cause {e.cause().is_some()}",
    }
}

fn main(root: Root) {
    let out = root.stdout
    out.print(describe(step("x", false)))
    out.print(describe(step("20", false)))
    out.print(describe(step("-1", false)))
    out.print(describe(step("0", true)))
    out.print(describe(step("2", false)))
    out.print(describe(step("0", false)))
    let errs: List[dyn Error] = ["plain", ParseError { line: 7 }, Denied("no")]
    out.print(errs.map(|e| e.message()).join(" | "))
}
|}) "err bad line 3 cause false\nerr late cause false\nerr missing sign cause false\nerr boom cause false\nerr index 2 is out of range for a list of length 1 cause false\nok 1\nplain | bad line 7 | denied: no\n";
  out "error-io" {|fn load(fs: Fs) -> Result[Str, dyn Error] {
    let text = fs.read("no-such-file.txt")?
    Ok(text)
}

fn main(root: Root) {
    match load(root.fs) {
        Ok(t) => root.stdout.print(t),
        Err(e) => root.stdout.print("failed: {e.message()}"),
    }
    let d: IoError = Denied("x")
    root.stdout.print("{d.message()} {Failed("y").message()} {"z".message()}")
}
|} "failed: denied: fs: not granted (run with --grant fs=DIR)\ndenied: x y z\n";
  (* a type with its own cause, and a generic function bounded by Error *)
  out "error-own-cause" (perr ^ {|struct ConfigError {
    file: Str,
    why: ParseError,
}

fn message(self: ConfigError) -> Str {
    "in {self.file}"
}

fn cause(self: ConfigError) -> Option[dyn Error] {
    Some(self.why)
}

fn show_all[E: Error](e: E) -> Str {
    let tail = match e.cause() {
        Some(c) => " <- {c.message()}",
        None => "",
    }
    e.message() + tail
}

fn load() -> Result[(), ConfigError] {
    Err(ConfigError { file: "app.toml", why: ParseError { line: 2 } })
}

fn run() -> Result[(), dyn Error] {
    load()?
    Ok(())
}

fn main(root: Root) {
    let out = root.stdout
    out.print(show_all(ConfigError { file: "a", why: ParseError { line: 1 } }))
    out.print(show_all(ParseError { line: 9 }))
    out.print(show_all("text"))
    match run() {
        Err(e) => out.print("{e.message()}; {e.cause().map(|c| c.message()) ?? "-"}"),
        Ok(_) => out.print("ok"),
    }
}
|}) "in a <- bad line 1\nbad line 9\ntext\nin app.toml; bad line 2\n";
  (let src = {|fn fail() -> Result[Int, Str] {
    Err("disk full")
}

fn main() -> Result[(), dyn Error] {
    fail()?
    Ok(())
}
|} in
   if selected "error-main" then begin
     (match run "error-main" src with
      | Ok (_, Interp.VEnum ("Result", "Err", [ Interp.VStr m ])) -> ok "error-main" (m = "disk full") m
      | Ok (_, v) -> ok "error-main" false (Interp.show v)
      | Error e -> ok "error-main" false e);
     if native_for "error-main" then
       run_native "error-main" src (function
         | Ok (code, _, e) -> ok "error-main/native" (code = 1 && e = "error: disk full\n") (sprintf "exit %d stderr %S" code e)
         | Error m -> ok "error-main/native" false m)
   end);
  let noerr = "struct Quiet {\n    n: Int,\n}\n\nfn source() -> Result[Int, Quiet] {\n    Err(Quiet { n: 1 })\n}\n\n" in
  errs "error-not-satisfied" (noerr ^ "fn f() -> Result[Int, dyn Error] {\n    let x = source()?\n    Ok(x)\n}\n\nfn main() {}\n") [ "E0202" ];
  errs "error-quiet-not-error" (noerr ^ "fn main() {\n    let e: dyn Error = Quiet { n: 1 }\n}\n") [ "E0111" ];
  errs "error-int-not-error" "fn main() {\n    let e: dyn Error = 3\n}\n" [ "E0112" ];
  errs "error-str-not-other-dyn" "interface Named {\n    fn message(self) -> Str\n}\n\nfn main() {\n    let e: dyn Named = \"x\"\n}\n" [ "E0112" ];
  errs "error-bad-cause" (noerr ^ "fn message(self: Quiet) -> Str {\n    \"q\"\n}\n\nfn cause(self: Quiet) -> Str {\n    \"no\"\n}\n\nfn main() {\n    let e: dyn Error = Quiet { n: 1 }\n}\n") [ "E0111" ];
  errs "error-capability" "struct Holds[T] {\n    c: T,\n}\n\nfn message[T](self: Holds[T]) -> Str {\n    \"h\"\n}\n\nfn get(c: Clock) -> Result[Int, Holds[Clock]] {\n    Err(Holds { c: c })\n}\n\nfn f(c: Clock) -> Result[Int, dyn Error] {\n    let x = get(c)?\n    Ok(x)\n}\n\nfn main() {}\n" [ "E0202" ];
  out "error-constructors" (perr ^ {|fn check(n: Int) -> Result[Int, dyn Error] {
    if n < 0 {
        return Err("negative")
    }
    if n > 9 {
        Err(ParseError { line: n })
    } else {
        Ok(n)
    }
}

fn main(root: Root) {
    let out = root.stdout
    for n in [-1, 12, 3] {
        out.print(match check(n) {
            Ok(v) => "ok {v}",
            Err(e) => e.message(),
        })
    }
    let maybe: Option[dyn Error] = Some(Failed("io"))
    out.print(maybe.map(|e| e.message()) ?? "")
}
|}) "negative\nbad line 12\nok 3\nio\n";
  errs "error-str-message-taken" "fn message(self: Str) -> Str {\n    self\n}\n\nfn main() {}\n" [ "E0106" ];
  (* E0202 offers `dyn Error` when both error types could convert to it *)
  (let src = "fn a() -> Result[Int, Str] {\n    Ok(1)\n}\n\nfn b() -> Result[Int, IoError] {\n    let x = a()?\n    Ok(x)\n}\n\nfn main() {}\n" in
   let _, ds, _ = compile "error-fix" src in
   ok "error-fix-offered" (codes ds = [ "E0202" ] && List.exists (fun (d : Diag.t) -> d.fix <> None) ds) (String.concat " " (codes ds));
   let fixed = apply_fixes src ds in
   ok "error-fix-text" (fixed = "fn a() -> Result[Int, Str] {\n    Ok(1)\n}\n\nfn b() -> Result[Int, dyn Error] {\n    let x = a()?\n    Ok(x)\n}\n\nfn main() {}\n") fixed;
   let _, ds2, _ = compile "error-fix-fixed" fixed in
   ok "error-fix-checks" (ds2 = []) (String.concat " " (codes ds2)));
  (let src = "struct Quiet {\n    n: Int,\n}\n\nfn a() -> Result[Int, Quiet] {\n    Ok(1)\n}\n\nfn b() -> Result[Int, Str] {\n    let x = a()?\n    Ok(x)\n}\n\nfn main() {}\n" in
   let _, ds, _ = compile "error-nofix" src in
   ok "error-no-fix-when-not-error" (codes ds = [ "E0202" ] && List.for_all (fun (d : Diag.t) -> d.fix = None) ds) (String.concat " " (codes ds)));

  (* ---------- error context: `x? "context"`, and the errors module ---------- *)
  out "error-context" (perr ^ {|use errors

fn parse(s: Str) -> Result[Int, ParseError] {
    match s.parse_int() {
        Some(n) => Ok(n),
        None => Err(ParseError { line: 4 }),
    }
}

fn read(fs: Fs, name: Str) -> Result[Int, dyn Error] {
    let text = fs.read(name)? "reading {name}"
    Ok(text.len())
}

fn setting(s: Str, all: Map[Str, Str]) -> Result[Int, dyn Error] {
    let raw = all.get(s)? "no setting {s}"
    let n = parse(raw)? "parsing {s}"
    let checked = catch(|| [10, 20][n])? "looking up {s}"
    Ok(checked)
}

fn start(fs: Fs) -> Result[Int, dyn Error] {
    let n = read(fs, "app.toml")? "starting"
    Ok(n)
}

fn main(root: Root) {
    let out = root.stdout
    let all = map([("a", "1"), ("b", "x"), ("c", "7")])
    for s in ["a", "b", "c", "d"] {
        match setting(s, all) {
            Ok(n) => out.print("{s} = {n}"),
            Err(e) => out.print("{e.message()} [{errors.chain(e).len()}]"),
        }
    }
    match start(root.fs) {
        Ok(_) => out.print("read it"),
        Err(e) => {
            out.print(errors.report(e))
            out.print(errors.root(e).message())
            out.print(errors.chain(e).map(|x| errors.own_message(x)).join(" / "))
            out.print(errors.wrap(e, "outer").message())
            out.print(errors.report("plain"))
        },
    }
}
|})
    ("a = 20\nparsing b: bad line 4 [2]\nlooking up c: index 7 is out of range for a list of length 2 [2]\nno setting d [1]\n"
     ^ "starting\ncaused by: reading app.toml\ncaused by: denied: fs: not granted (run with --grant fs=DIR)\n"
     ^ "denied: fs: not granted (run with --grant fs=DIR)\nstarting / reading app.toml / denied: fs: not granted (run with --grant fs=DIR)\n"
     ^ "outer: starting: reading app.toml: denied: fs: not granted (run with --grant fs=DIR)\nplain\n");
  (* the context is evaluated only on the error path, and in a loop its strings are freed *)
  out "error-context-loop" {|fn get(i: Int) -> Result[Int, Str] {
    if i % 3 == 0 { Err("bad {i}") } else { Ok(i) }
}

fn tally(n: Int) -> Result[Int, dyn Error] {
    let mut sum = 0
    for i in 1..<n {
        sum = sum + get(i)? "item {i} of {n}"
    }
    Ok(sum)
}

fn main(root: Root) {
    let mut errs = 0
    for n in 0..<200 {
        match tally(n) {
            Ok(_) => {},
            Err(e) => {
                errs = errs + e.message().len()
            },
        }
    }
    root.stdout.print("{errs} {tally(3).is_ok()} {tally(4).is_err()}")
}
|} "3818 true true\n";
  (let src = {|fn fail() -> Result[Int, Str] {
    Err("disk full")
}

fn main() -> Result[(), dyn Error] {
    fail()? "saving"
    Ok(())
}
|} in
   if selected "error-context-main" && native_for "error-context-main" then
     run_native "error-context-main" src (function
       | Ok (code, _, e) -> ok "error-context-main/native" (code = 1 && e = "error: saving: disk full\n") (sprintf "exit %d stderr %S" code e)
       | Error m -> ok "error-context-main/native" false m));
  errs "error-context-not-str" "fn f(fs: Fs) -> Result[Str, dyn Error] {\n    let t = fs.read(\"a\")? 3\n    Ok(t)\n}\n\nfn main() {}\n" [ "E0004" ];
  errs "error-context-option-fn" "fn f(xs: List[Int]) -> Option[Int] {\n    let x = xs.first()? \"empty\"\n    Some(x)\n}\n\nfn main() {}\n" [ "E0202" ];
  errs "error-context-in-unit" (main_wrap "    let x = root.fs.read(\"a\")? \"reading\"") [ "E0202" ];
  errs "error-context-not-error" (noerr ^ "fn f() -> Result[Int, dyn Error] {\n    let x = source()? \"getting\"\n    Ok(x)\n}\n\nfn main() {}\n") [ "E0202" ];
  errs "error-context-interp-type" "fn f(fs: Fs) -> Result[Str, dyn Error] {\n    let t = fs.read(\"a\")? \"reading {nope}\"\n    Ok(t)\n}\n\nfn main() {}\n" [ "E0101" ];
  (let src = "fn f(fs: Fs) -> Result[Str, IoError] {\n    let t = fs.read(\"a\")? \"reading\"\n    Ok(t)\n}\n\nfn main() {}\n" in
   let _, ds, _ = compile "error-context-fix" src in
   ok "error-context-fix" (codes ds = [ "E0202" ]) (String.concat " " (codes ds));
   let fixed = apply_fixes src ds in
   let _, ds2, _ = compile "error-context-fixed" fixed in
   ok "error-context-fix-applies" (ds2 = [] && fixed = "fn f(fs: Fs) -> Result[Str, dyn Error] {\n    let t = fs.read(\"a\")? \"reading\"\n    Ok(t)\n}\n\nfn main() {}\n") fixed);
  ok "error-context-fmt" (Fmt.format ~file:"ec.tsr" "fn f(fs: Fs) -> Result[Str, dyn Error] {\n    let t = fs.read(\"a\")?\"reading {1}\"\n    Ok(t)\n}\n"
     = "fn f(fs: Fs) -> Result[Str, dyn Error] {\n    let t = fs.read(\"a\")? \"reading {1}\"\n    Ok(t)\n}\n")
    (Fmt.format ~file:"ec.tsr" "fn f(fs: Fs) -> Result[Str, dyn Error] {\n    let t = fs.read(\"a\")?\"reading {1}\"\n    Ok(t)\n}\n");
  ok "error-context-astdump"
    (let items = Parser.parse_program "ad.tsr" "fn f() {\n    g()? \"c\"\n}\n" in
     match items with
     | [ Ast.IFn { fn_body = { tail = Some { edesc = Ast.ETry (_, Some _); _ }; _ }; _ } ] -> true
     | _ -> false)
    "context not parsed";

  (* ---------- map_error, and the old name map_err ---------- *)
  let mapsrc m = sprintf {|fn half(n: Int) -> Result[Int, Str] {
    if n %% 2 == 0 { Ok(n / 2) } else { Err("odd {n}") }
}

fn main(root: Root) {
    let codes = [4, 5].map(|n| half(n).%s(|e| e.len()))
    root.stdout.print("{codes}")
}
|} m in
  out "error-map-error" (mapsrc "map_error") "[Ok(2), Err(5)]\n";
  (* the old name still runs, with a warning whose fix renames it *)
  out "error-map-err-still-works" (mapsrc "map_err") "[Ok(2), Err(5)]\n";
  (let src = mapsrc "map_err" in
   let _, ds, _ = compile "error-map-err-warns" src in
   ok "error-map-err-warns" (codes ds = [ "W0001" ] && List.for_all (fun (d : Diag.t) -> d.severity = Diag.Warning && d.fix <> None) ds) (String.concat " " (codes ds));
   let fixed = apply_fixes src ds in
   ok "error-map-err-fixed" (fixed = mapsrc "map_error") fixed;
   let _, ds2, _ = compile "error-map-err-fixed2" fixed in
   ok "error-map-err-fixed-clean" (ds2 = []) (String.concat " " (codes ds2)));
  errs "error-map-err-not-on-option" (main_wrap "    let x = Some(1).map_err(|e| e)") [ "E0105" ];
  ok "error-w0001-explained" (List.mem_assoc "W0001" Diag.explanations) "no explanation";

  (* ---------- ordering: sort() and < on any ordered type ---------- *)
  out "sort-any"
    ("struct P {\n    x: Int,\n    name: Str,\n}\n\nenum Tree {\n    Leaf,\n    Node(Tree, Int, Tree),\n}\n\nenum Size {\n    Small,\n    Big(Int),\n}\n\n"
     ^ "fn main(root: Root) {\n    let out = root.stdout\n"
     ^ "    out.print(\"{[(2, \"b\"), (1, \"z\"), (2, \"a\"), (1, \"a\")].sort()}\")\n"
     ^ "    out.print(\"{[P { x: 2, name: \"a\" }, P { x: 1, name: \"b\" }, P { x: 1, name: \"a\" }].sort()}\")\n"
     ^ "    out.print(\"{[Big(3), Small, Big(1), Small].sort()} {Small < Big(0)} {Big(2) >= Big(2)}\")\n"
     ^ "    out.print(\"{[[3, 1], [2], [], [3]].sort()} {[true, false].sort()} {[[\"b\"], [\"a\", \"c\"]].sort()}\")\n"
     ^ "    out.print(\"{[Some(2.5), None, Some(-1.0)].sort()} {(1, \"b\") < (1, \"c\")} {[(), ()].sort()}\")\n"
     ^ "    let t = Node(Leaf, 2, Leaf)\n    out.print(\"{[t, Leaf, Node(t, 1, Leaf), Node(Leaf, 1, t)].sort()}\")\n"
     ^ "    let a: Array[(Int, Str), 2] = [(2, \"x\"), (1, \"y\")]\n    out.print(\"{[a, [(1, \"z\"), (5, \"q\")]].sort()}\")\n}\n")
    ("[(1, \"a\"), (1, \"z\"), (2, \"a\"), (2, \"b\")]\n[P { x: 1, name: \"a\" }, P { x: 1, name: \"b\" }, P { x: 2, name: \"a\" }]\n"
     ^ "[Small, Small, Big(1), Big(3)] true true\n[[], [2], [3], [3, 1]] [false, true] [[\"a\", \"c\"], [\"b\"]]\n"
     ^ "[Some(-1.0), Some(2.5), None] true [(), ()]\n"
     ^ "[Leaf, Node(Leaf, 1, Node(Leaf, 2, Leaf)), Node(Leaf, 2, Leaf), Node(Node(Leaf, 2, Leaf), 1, Leaf)]\n"
     ^ "[[(1, \"z\"), (5, \"q\")], [(2, \"x\"), (1, \"y\")]]\n");
  errs "sort-unordered" "fn main() {\n    let m: Map[Int, Int] = map()\n    let xs = [m].sort()\n}\n" [ "E0201" ];
  errs "sort-unordered-field" "struct H {\n    m: Map[Str, Int],\n}\n\nfn main() {\n    let xs = [H { m: map() }].sort()\n}\n" [ "E0201" ];
  errs "less-unordered" "fn main() {\n    let a: Map[Int, Int] = map()\n    let b = a < a\n}\n" [ "E0201" ];
  errs "sort-generic" "fn f[T](xs: List[T]) -> List[T] {\n    xs.sort()\n}\n\nfn main() {}\n" [ "E0201" ];

  (* ---------- ranges and characters ---------- *)
  out "range-for"
    (main_wrap
       "    let n = 6\n    let mut total = 0\n    for i in 0..<n {\n        if i == 1 {\n            continue\n        }\n        if i == 5 {\n            break\n        }\n        total = total + i\n    }\n    out.print(\"{total} {0..<4} {(2..<n + 1).map(|x| x * x)} {5..<2} {(0..<0).len()} {-2..<1}\")\n    let xs = [\"a\", \"b\"]\n    for k in 0..<xs.len() {\n        out.write(\"{k}={xs[k]} \")\n    }\n    let mut big = 0\n    for i in 9223372036854775805..<9223372036854775807 {\n        big = big + 1\n    }\n    let r = 1..<3\n    for x in r {\n        out.write(\"{x}\")\n    }\n    out.print(\" {big} {(0..<3) == [0, 1, 2]}\")")
    "9 [0, 1, 2, 3] [4, 9, 16, 25, 36] [] 0 [-2, -1, 0]\n0=a 1=b 12 2 true\n";
  out "range-in-closure-and-task"
    (main_wrap "    let r = group g {\n        for i in 0..<3 {\n            g.go(|| (0..<i + 1).fold(0, |a, b| a + b))\n        }\n    }\n    out.print(\"{r}\")")
    "[0, 1, 3]\n";
  errs "range-not-int" "fn main() {\n    let r = 0..<2.5\n}\n" [ "E0201" ];
  errs "range-not-int-lo" "fn main() {\n    for i in \"a\"..<3 {\n    }\n}\n" [ "E0201" ];
  out "str-chars"
    (main_wrap
       "    let mut seen = \"\"\n    for c in \"h\u{e9}llo, \u{4e16}\u{754c}!\".chars() {\n        seen = seen + \"[\" + c + \"]\"\n    }\n    out.print(seen)\n    let broken = \"\u{e9}\".slice(0, 1) + \"x\" + \"\u{4e16}\".slice(0, 2) + \"\u{1f600}\"\n    out.print(\"{broken.chars().map(|c| c.len())} {\"\".chars()} {\"ab\".chars()} {broken.chars().join(\"\") == broken}\")")
    "[h][\u{e9}][l][l][o][,][ ][\u{4e16}][\u{754c}][!]\n[1, 1, 1, 1, 4] [] [\"a\", \"b\"] true\n";

  (* ---------- methods as values ---------- *)
  out "method-values"
    ("interface Shape {\n    fn area(self) -> Float\n}\n\nstruct Circle {\n    r: Float,\n}\n\nstruct Counter {\n    n: Int,\n    name: Str,\n}\n\nenum Op {\n    Add(Int),\n    Neg,\n}\n\n"
     ^ "fn area(self: Circle) -> Float {\n    3.0 * self.r * self.r\n}\n\nfn plus(self: Counter, k: Int) -> Int {\n    self.n + k\n}\n\n"
     ^ "fn apply(self: Op, x: Int) -> Int {\n    match self {\n        Add(k) => x + k,\n        Neg => -x,\n    }\n}\n\n"
     ^ "fn run_all(fs: List[fn() -> Float]) -> List[Float] {\n    fs.map(|f| f())\n}\n\nfn later[T: Shape](x: T) -> fn() -> Float {\n    x.area\n}\n\n"
     ^ "fn main(root: Root) {\n    let out = root.stdout\n    let mut c = Counter { n: 10, name: \"c\" }\n    let add = c.plus\n    c.n = 100\n"
     ^ "    out.print(\"{add(1)} {[1, 2, 3].map(c.plus)} {[1, 2].map(Add(5).apply)} {[3].map(Neg.apply)}\")\n"
     ^ "    let a = Circle { r: 1.0 }.area\n    let d: dyn Shape = Circle { r: 2.0 }\n    out.print(\"{run_all([a, d.area, later(Circle { r: 3.0 })])}\")\n"
     ^ "    let say = out.print\n    say(\"said\")\n    let up = \"abc\".to_upper\n    let n = c.name.len\n    let get = [5, 6].get\n"
     ^ "    let tasks = group g {\n        g.go(c.name.to_upper)\n        g.go(up)\n    }\n"
     ^ "    out.print(\"{up()} {n()} {get(1)} {get(7)} {tasks}\")\n}\n")
    "11 [101, 102, 103] [6, 7] [-3]\n[3.0, 12.0, 27.0]\nsaid\nABC 1 Some(6) None [\"C\", \"ABC\"]\n";
  errs "method-value-push" "fn main() {\n    let mut xs = [1]\n    let p = xs.push\n}\n" [ "E0103" ];
  errs "method-value-assign" "struct P {\n    x: Int,\n}\n\nfn get(self: P) -> Int {\n    self.x\n}\n\nfn main() {\n    let mut p = P { x: 1 }\n    p.get = p.get\n}\n" [ "E0209" ];
  errs "method-value-unknown" "struct P {\n    x: Int,\n}\n\nfn main() {\n    let p = P { x: 1 }\n    let f = p.nope\n}\n" [ "E0103" ];

  (* ---------- groups mixing () tasks with value tasks ---------- *)
  let mixed =
    "fn work(n: Int) -> Int {\n    n * n\n}\n\nfn spawn_all[T](fs: List[fn() -> T]) -> List[T] {\n    group g {\n        for f in fs {\n            g.go(f)\n        }\n    }\n}\n\n"
    ^ "fn main(root: Root) {\n    let out = root.stdout\n    let c: Chan[Str] = channel(4)\n    let squares = group g {\n        g.go(|| work(2))\n        g.go(|| {\n            c.send(\"hello\")\n            c.close()\n        })\n        g.go(|| work(3))\n        g.go(|| {\n            work(5);\n        })\n    }\n"
    ^ "    out.print(\"{squares} {c.recv()}\")\n    let none = group g {\n        g.go(|| out.print(\"a\"))\n        g.go(|| out.print(\"a\"))\n    }\n"
    ^ "    out.print(\"{none.len()} {spawn_all([|| (), || ()]).len()} {spawn_all([|| 1, || 2])}\")\n"
    ^ "    let r = catch(|| group g {\n        g.go(|| {\n            let x: Int = panic(\"first\")\n        })\n        g.go(|| panic(\"second\"))\n        g.go(|| work(4))\n    })\n    out.print(\"{r}\")\n}\n"
  in
  out "group-mixed-tasks" mixed "[4, 9] Some(\"hello\")\na\na\n0 2 [1, 2]\nErr(\"first\")\n";
  check_tsan "group-mixed-tasks" mixed "[4, 9] Some(\"hello\")\na\na\n0 2 [1, 2]\nErr(\"first\")\n";

  (* ---------- output in start order ---------- *)
  let both name ?feed src want =
    out name ?feed src want;
    check_tsan name ?feed src want
  in
  both "output-order"
    {|fn main(root: Root) {
    let out = root.stdout
    let clock = root.clock
    out.print("before")
    // the later tasks finish first, and each runs a group of its own
    let rs = group g {
        for i in 0..<4 {
            g.go(|| {
                clock.sleep(30 - i * 10)
                out.print("task {i} a")
                let inner = group h {
                    h.go(|| {
                        clock.sleep(5)
                        out.print("  inner {i}.0")
                        0
                    })
                    h.go(|| {
                        out.write("  inner {i}.1\n")
                        1
                    })
                }
                out.print("task {i} b {inner}")
                i
            })
        }
        out.print("body")
    }
    out.print("after {rs}")
    // a task the deadline stops still has its output written in its turn
    let t = group g until clock.after(50) {
        g.go(|| {
            out.print("slow starts")
            clock.sleep(5000)
            out.print("never")
            1
        })
        g.go(|| {
            out.print("fast")
            2
        })
    }
    out.print("deadline {t}")
    // the panic is passed on after every task's output
    let r = catch(|| group g {
        g.go(|| {
            clock.sleep(20)
            out.print("first")
            1
        })
        g.go(|| {
            out.print("second, then panics")
            panic("boom")
        })
        g.go(|| {
            out.print("third")
            3
        })
    })
    out.print("{r}")
    // the receiver was started first, so its output comes first
    let c: Chan[Int] = channel(0)
    let _ = group g {
        g.go(|| out.print("got {c.recv() ?? 0}"))
        g.go(|| {
            out.print("sending")
            c.send(7)
        })
    }
    // interleaved: in the order it happens
    let d: Chan[Int] = channel(0)
    let _ = group g interleaved {
        g.go(|| out.print("got {d.recv() ?? 0}"))
        g.go(|| {
            out.print("sending")
            d.send(8)
        })
    }
}
|}
    "before\nbody\ntask 0 a\n  inner 0.0\n  inner 0.1\ntask 0 b [0, 1]\ntask 1 a\n  inner 1.0\n  inner 1.1\ntask 1 b [0, 1]\n\
     task 2 a\n  inner 2.0\n  inner 2.1\ntask 2 b [0, 1]\ntask 3 a\n  inner 3.0\n  inner 3.1\ntask 3 b [0, 1]\nafter [0, 1, 2, 3]\n\
     slow starts\nfast\ndeadline [None, Some(2)]\nfirst\nsecond, then panics\nthird\nErr(\"boom\")\ngot 7\nsending\nsending\ngot 8\n";
  (* many tasks that wait now and then, so natively they run in any order *)
  let many =
    String.concat ""
      (List.init 40 (fun i ->
           String.concat "" (List.init 12 (fun k -> sprintf "t%d %d\n" i k))
           ^ String.concat "" (List.init 3 (fun j -> sprintf "  t%d.%d\n" i j))))
  in
  both "output-order-many"
    {|fn main(root: Root) {
    let out = root.stdout
    let clock = root.clock
    let c: Chan[Int] = channel(4)
    let _ = group g {
        for i in 0..<40 {
            g.go(|| {
                for k in 0..<12 {
                    out.print("t{i} {k}")
                    if k % 4 == i % 4 {
                        clock.sleep(i % 3)
                    }
                    c.send(k)
                    let _ = c.recv()
                }
                let _ = group h {
                    for j in 0..<3 {
                        h.go(|| out.print("  t{i}.{j}"))
                    }
                }
            })
        }
    }
}
|}
    many;
  (* past 4 MiB held, the group gives up order: the second task's lines go
     out before the first task, which waits for them, prints *)
  let mib = String.make (1 lsl 20) 'x' in
  both "output-order-limit"
    {|fn main(root: Root) {
    let out = root.stdout
    let mut s = "x"
    for _ in 0..<20 {
        s = s + s
    }
    let c: Chan[Int] = channel(0)
    let _ = group g {
        g.go(|| out.print("got {c.recv() ?? 0}"))
        g.go(|| {
            for _ in 0..<5 {
                out.print(s)
            }
            c.send(5)
        })
    }
    out.print("end")
}
|}
    (String.concat "" (List.init 5 (fun _ -> mib ^ "\n")) ^ "got 5\nend\n");
  (* reading stdin shows the prompt the task printed, though it is held *)
  both "output-order-stdin" ~feed:"echo ana"
    {|fn main(root: Root) {
    let out = root.stdout
    let input = root.stdin
    let c: Chan[Str] = channel(1)
    let _ = group g {
        g.go(|| out.print("done: {c.recv() ?? ""}"))
        g.go(|| {
            out.write("name? ")
            let name = input.read_line() ?? "nobody"
            out.print("hello {name}")
            c.send(name)
        })
    }
}
|}
    "name? hello ana\ndone: ana\n";
  (* a program that ends with a panic or a deadlock writes what is held first *)
  let held_then name src want_out want_err =
    if selected name then begin
      let items, _, _ = compile name src in
      let buf = Buffer.create 64 in
      (match Interp.run ~out:(Buffer.add_string buf) ~grants:Interp.no_grants items with
       | Error (m, _) ->
         ok name (Buffer.contents buf = want_out && m = want_err) (sprintf "stdout %S, panic %S" (Buffer.contents buf) m)
       | Ok _ -> ok name false "no panic");
      check_native name src ~want_out ~want_err_prefix:(Some ("panic: " ^ want_err ^ "\n"))
    end
  in
  held_then "output-order-panic"
    {|fn main(root: Root) {
    let out = root.stdout
    let clock = root.clock
    let _ = group g {
        g.go(|| {
            clock.sleep(20)
            out.print("first")
        })
        g.go(|| {
            out.print("second")
            panic("boom")
        })
    }
}
|}
    "first\nsecond\n" "boom";
  held_then "output-order-deadlock"
    {|fn main(root: Root) {
    let out = root.stdout
    let c: Chan[Int] = channel(0)
    let _ = group g {
        g.go(|| out.print("first {c.recv() ?? 0}"))
        g.go(|| {
            out.print("held")
            let _ = c.recv()
        })
    }
}
|}
    "held\n" "all tasks are blocked (deadlock)";
  errs "group-mixed-values" "fn main() {\n    let r = group g {\n        g.go(|| 1)\n        g.go(|| \"a\")\n    }\n}\n" [ "E0201" ];

  (* ---------- variant names: the file's own enums first ---------- *)
  let tok =
    "pub enum Tok {\n    Num(Int),\n    Word(Str),\n    End,\n}\n\npub fn first() -> Tok {\n    Num(7)\n}\n\n"
    ^ "pub fn show(t: Tok) -> Str {\n    match t {\n        Num(n) => \"num {n}\",\n        Word(w) => \"word {w}\",\n        End => \"end\",\n    }\n}\n"
  in
  out "variant-scope" ~modules:[ ("tok", tok) ]
    ("use tok\nuse json\n\nenum Val {\n    Num(Float),\n    Null,\n    Some(Int),\n}\n\n"
     ^ "fn main(root: Root) {\n    let out = root.stdout\n    let a = Num(1.5)\n    let b = tok.Num(2)\n    let c = json.Num(3.0)\n    let d = Word(\"w\")\n"
     ^ "    let e: Option[Int] = None\n    let f = Some(3)\n    let g = Option.Some(4)\n"
     ^ "    out.print(\"{a} {tok.show(b)} {json.stringify(c)} {tok.show(d)} {tok.show(tok.first())} {e} {f} {g}\")\n"
     ^ "    let r = match a {\n        Num(x) => x,\n        Null => 0.0,\n        Some(_) => 1.0,\n    }\n"
     ^ "    let fs = [Num, Val.Num].map(|k| k(1.0))\n    out.print(\"{r} {[Null, Num(2.0)]} {[End, tok.End].len()} {fs}\")\n}\n")
    "Num(1.5) num 2 3 word w num 7 None Some(3) Some(4)\n1.5 [Null, Num(2.0)] 2 [Num(1.0), Num(1.0)]\n";
  errs "variant-scope-ambiguous" ~modules:[ ("tok", tok); ("tok2", "pub enum T2 {\n    Word(Int),\n}\n") ]
    "use tok\nuse tok2\n\nfn main() {\n    let w = Word(1)\n}\n" [ "E0104" ];

  (* ---------- Bytes ---------- *)
  out "bytes"
    ("fn checksum(self: Bytes) -> Int {\n    self.to_list().fold(0, |a, b| (a + b) % 256)\n}\n\n"
     ^ "fn main(root: Root) {\n    let out = root.stdout\n    let b = bytes([104, 105, 0, 255])\n    let s = \"h\u{e9}\".to_bytes()\n"
     ^ "    out.print(\"{b} {b.len()} {b[1]} {b.get(9)} {b.slice(1, 3)} {s} {s.to_str()} {b.is_empty()}\")\n"
     ^ "    let c = b + s\n    out.print(\"{c.len()} {c == b + s} {b < s} {[s, b].sort()} {c.checksum()}\")\n"
     ^ "    let mut seen: Map[Bytes, Int] = map()\n    seen[b] = 1\n    seen[bytes([1])] = 2\n    seen[b] = seen[b] + 10\n"
     ^ "    let ch: Chan[Bytes] = channel(1)\n    let got = group g {\n        g.go(|| ch.send(bytes([7])))\n        g.go(|| ch.recv() ?? bytes([]))\n    }\n"
     ^ "    out.print(\"{seen} {bytes([]).to_str() == \"\"} {got}\")\n"
     ^ "    out.print(\"{catch(|| bytes([1, 256]))} {catch(|| b[4])}\")\n}\n")
    ("bytes([104, 105, 0, 255]) 4 105 None bytes([105, 0]) bytes([104, 195, 169]) h\u{e9} false\n"
     ^ "7 true true [bytes([104, 105, 0, 255]), bytes([104, 195, 169])] 164\n"
     ^ "{bytes([104, 105, 0, 255]): 11, bytes([1]): 2} true [bytes([7])]\n"
     ^ "Err(\"byte 256 is out of range 0 to 255\") Err(\"index 4 is out of range for bytes of length 4\")\n");
  errs "bytes-immutable" "fn main() {\n    let mut b = bytes([1])\n    b[0] = 2\n}\n" [ "E0209" ];
  errs "bytes-from-str" "fn main() {\n    let b = bytes(\"x\")\n}\n" [ "E0201" ];
  errs "bytes-not-str" "fn main() {\n    let b: Str = bytes([1])\n}\n" [ "E0201" ];

  (* ---------- recursive structs: through an enum or a list, never inline ---------- *)
  errs "struct-contains-itself" "struct Pair {\n    a: Int,\n    rest: Wrap,\n}\n\nstruct Wrap {\n    p: Pair,\n}\n\nfn main() {}\n" [ "E0114"; "E0114" ];
  errs "struct-contains-itself-generic" "struct Holder[A] {\n    x: A,\n}\n\nstruct T {\n    h: Holder[(Int, T)],\n}\n\nfn main() {}\n" [ "E0114" ];
  errs "struct-contains-itself-array" "struct G {\n    cells: Array[G, 2],\n}\n\nfn main() {}\n" [ "E0114" ];
  out "struct-recursive-indirect"
    ("struct Node {\n    v: Int,\n    next: Option[Node],\n}\n\nstruct Tree {\n    v: Int,\n    kids: List[Tree],\n}\n\n"
     ^ "fn sum(t: Tree) -> Int {\n    t.kids.fold(t.v, |a, k| a + sum(k))\n}\n\n"
     ^ "fn main(root: Root) {\n    let n = Node { v: 1, next: Some(Node { v: 2, next: None }) }\n"
     ^ "    let t = Tree { v: 1, kids: [Tree { v: 2, kids: [] }, Tree { v: 3, kids: [Tree { v: 4, kids: [] }] }] }\n"
     ^ "    root.stdout.print(\"{n} {sum(t)}\")\n}\n")
    "Node { v: 1, next: Some(Node { v: 2, next: None }) } 10\n";

  (* ---------- modules ---------- *)
  let geom =
    "// geometry\npub struct Point {\n    x: Int,\n    y: Int,\n}\n\npub enum Shape {\n    Dot(Point),\n    Box(Point, Point),\n}\n\n"
    ^ "fn sq(n: Int) -> Int {\n    n * n\n}\n\npub fn dist2(a: Point, b: Point) -> Int {\n    sq(a.x - b.x) + sq(a.y - b.y)\n}\n\n"
    ^ "pub fn origin() -> Point {\n    Point { x: 0, y: 0 }\n}\n\n"
    ^ "pub fn area(s: Shape) -> Int {\n    match s {\n        Shape.Dot(_) => 0,\n        Box(a, b) => (b.x - a.x) * (b.y - a.y),\n    }\n}\n\n"
    ^ "test \"ignored when imported\" {\n    assert(false)\n}\n"
  in
  let modules = [ ("geom", geom) ] in
  out "modules" ~modules
    ("use geom\n\nfn describe(s: geom.Shape) -> Str {\n    match s {\n        geom.Dot(p) => \"dot at {p.x},{p.y}\",\n        geom.Box(_, _) => \"box of area {geom.area(s)}\",\n    }\n}\n\n"
     ^ "fn sq(s: Str) -> Str {\n    s + s\n}\n\n"
     ^ "fn main(root: Root) {\n    let p = geom.Point { x: 3, y: 4 }\n    let o = geom.origin()\n    root.stdout.print(\"d2 = {geom.dist2(p, o)} {sq(\"ab\")}\")\n"
     ^ "    for s in [geom.Dot(p), geom.Box(o, p)] {\n        root.stdout.print(describe(s))\n    }\n    let f = geom.dist2\n    root.stdout.print(\"{f(o, p)}\")\n}\n")
    "d2 = 25 abab\ndot at 3,4\nbox of area 12\n25\n";
  (* a local variable shadows a module name *)
  out "modules-shadow" ~modules "use geom\n\nfn main(root: Root) {\n    let geom = [1, 2]\n    root.stdout.print(\"{geom.len()}\")\n}\n" "2\n";
  (* a module that uses another module *)
  out "modules-nested"
    ~modules:[ ("a", "use b\n\npub fn twice(n: Int) -> Int {\n    b.inc(b.inc(n))\n}\n"); ("b", "pub fn inc(n: Int) -> Int {\n    n + 1\n}\n") ]
    "use a\n\nfn main(root: Root) {\n    root.stdout.print(\"{a.twice(1)}\")\n}\n" "3\n";
  errs "modules-private" ~modules "use geom\n\nfn main(root: Root) {\n    root.stdout.print(\"{geom.sq(2)}\")\n}\n" [ "E0402" ];
  errs "modules-missing-member" ~modules "use geom\n\nfn main(root: Root) {\n    geom.nope()\n}\n" [ "E0403" ];
  errs "modules-not-found" "use nowhere\n\nfn main(root: Root) {}\n" [ "E0401" ];
  errs "modules-no-use" ~modules "fn main(root: Root) {\n    let p: geom.Point = geom.origin()\n}\n" [ "E0401" ];
  errs "modules-type-check" ~modules "use geom\n\nfn main(root: Root) {\n    let p: geom.Point = 1\n}\n" [ "E0201" ];
  errs "modules-not-value" ~modules "use geom\n\nfn main(root: Root) {\n    let g = geom\n}\n" [ "E0404" ];
  (* packages: each dependency may only use the capabilities tessera.toml grants it *)
  let pdir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "tessera-pkg-%d" (Unix.getpid ())) in
  List.iter (fun d -> try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    [ pdir; Filename.concat pdir "app"; Filename.concat pdir "fetch" ];
  let put f s = Out_channel.with_open_bin (Filename.concat pdir f) (fun oc -> output_string oc s) in
  put "fetch/fetch.tsr" "use util\n\npub fn get(net: Net, u: Str) -> Str {\n    match net.get(u) {\n        Ok(r) => util.first(r.body),\n        Err(_) => \"none\",\n    }\n}\n";
  put "fetch/util.tsr" "pub fn first(s: Str) -> Str {\n    s.lines().first() ?? \"\"\n}\n\npub fn sneaky(fs: Fs) -> Bool {\n    fs.exists(\"x\")\n}\n";
  let app = "use fetch\n\nfn main(root: Root) {\n    root.stdout.print(fetch.get(root.net, \"http://h\"))\n}\n" in
  put "app/main.tsr" app;
  let pkg_codes toml =
    put "app/tessera.toml" toml;
    let path = Filename.concat pdir "app/main.tsr" in
    let _, ds, _ = Modules.load_and_check path app in
    codes ds
  in
  ok "pkg-denied" (pkg_codes "[package]\nname = \"app\"\n\n[dependencies]\nfetch = { path = \"../fetch\" }\n" = [ "E0320"; "E0320" ]) "two functions over their grant";
  ok "pkg-partial" (pkg_codes "[package]\nname = \"app\"\n[dependencies]\nfetch = { path = \"../fetch\", caps = [\"net\"] }\n" = [ "E0320" ]) "util.sneaky takes Fs";
  ok "pkg-granted" (pkg_codes "[package]\nname = \"app\"\n[dependencies]\nfetch = { path = \"../fetch\", caps = [\"net\", \"fs\"] }  # both\n" = []) "granted";
  ok "pkg-bad-toml" (pkg_codes "[package]\nname = \"app\"\n[dependencies]\nfetch = { path = \"../fetch\", caps = [\"disk\"] }\n" = [ "E0321" ]) "bad cap";
  ok "pkg-missing-dir" (pkg_codes "[package]\nname = \"app\"\n[dependencies]\nother = { path = \"../nope\" }\n" = [ "E0401" ]) "fetch not found";
  (* embed stays inside the package: the main program's (app/, where its
     tessera.toml is) and each dependency's own directory *)
  put "app/data.txt" "app data";
  (try Unix.mkdir (Filename.concat pdir "embdep") 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  put "embdep/note.txt" "dep data!!";
  let embed_codes ~dep_src main_src =
    put "embdep/embdep.tsr" dep_src;
    put "app/tessera.toml" "[package]\nname = \"app\"\n[dependencies]\nembdep = { path = \"../embdep\" }\n";
    let path = Filename.concat pdir "app/embmain.tsr" in
    put "app/embmain.tsr" main_src;
    let _, ds, _ = Modules.load_and_check path main_src in
    codes ds
  in
  let main_uses e = "use embdep\n\nfn main(root: Root) {\n    let x = " ^ e ^ "\n    root.stdout.print(\"{embdep.text()} {x}\")\n}\n" in
  let dep_embeds p = "pub fn text() -> Str {\n    embed(\"" ^ p ^ "\")\n}\n" in
  let inside = dep_embeds "note.txt" in
  ok "embed-own-package" (embed_codes ~dep_src:inside (main_uses "embed(\"data.txt\")") = []) "both inside";
  ok "embed-dep-escapes" (embed_codes ~dep_src:(dep_embeds "../app/data.txt") (main_uses "1") = [ "E0405" ]) "dependency reads the app";
  ok "embed-main-escapes" (embed_codes ~dep_src:inside (main_uses "embed(\"../embdep/note.txt\")") = [ "E0405" ]) "app reads outside its root";
  ok "embed-absolute" (embed_codes ~dep_src:inside (main_uses "embed(\"/etc/hosts\")") = [ "E0405" ]) "absolute path";
  ok "embed-not-literal" (embed_codes ~dep_src:inside (main_uses "embed(embdep.text())") = [ "E0405" ]) "computed path";
  ok "embed-missing" (embed_codes ~dep_src:(dep_embeds "nope.txt") (main_uses "1") = [ "E0405" ]) "missing file";
  put "app/bin.dat" "\255\254";
  ok "embed-not-utf8" (embed_codes ~dep_src:inside (main_uses "embed(\"bin.dat\")") = [ "E0405" ]) "binary as text";
  ok "embed-bytes-binary" (embed_codes ~dep_src:inside (main_uses "embed_bytes(\"bin.dat\").len()") = []) "binary as bytes";
  ok "embed-recorded"
    (embed_codes ~dep_src:inside (main_uses "embed(\"data.txt\")") = []
     && List.sort compare !Modules.embedded = [ ("", "data.txt", 8); ("embdep", "note.txt", 10) ])
    "tessera caps lists embeds";
  (try Unix.mkdir (Filename.concat pdir "app/pub") 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (try Unix.mkdir (Filename.concat pdir "app/empty") 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  put "app/pub/x.txt" "xx";
  ok "embed-dir-recorded"
    (embed_codes ~dep_src:inside (main_uses "embed_dir(\"pub\").len()") = []
     && List.mem ("", "pub/x.txt", 2) !Modules.embedded)
    "each file is listed";
  ok "embed-dir-escapes" (embed_codes ~dep_src:inside (main_uses "embed_dir(\"..\").len()") = [ "E0405" ]) "the parent of the package";
  ok "embed-dir-file" (embed_codes ~dep_src:inside (main_uses "embed_dir(\"data.txt\").len()") = [ "E0405" ]) "a file, not a directory";
  ok "embed-dir-empty" (embed_codes ~dep_src:inside (main_uses "embed_dir(\"empty\").len()") = [ "E0405" ]) "nothing to embed";
  (try Unix.symlink "../../embdep" (Filename.concat pdir "app/pub/out") with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  ok "embed-dir-symlink-out" (embed_codes ~dep_src:inside (main_uses "embed_dir(\"pub\").len()") = [ "E0405" ]) "a link leaving the package";
  Sys.remove (Filename.concat pdir "app/pub/out");
  ok "embed-shadowed"
    (embed_codes ~dep_src:inside "use embdep\n\nfn embed(s: Str) -> Str {\n    s\n}\n\nfn main(root: Root) {\n    root.stdout.print(embed(\"../anything\"))\n}\n" = [])
    "a function named embed is an ordinary call";
  Sys.remove (Filename.concat pdir "app/tessera.toml");

  (* calling C is full trust: a dependency gets it only from [trust], never from caps,
     and tessera.lock records its libraries so a change is noticed *)
  (try Unix.mkdir (Filename.concat pdir "cdep") 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (try Unix.mkdir (Filename.concat pdir "capp") 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let cdep_src libs =
    String.concat "" (List.map (fun (l, fn) -> sprintf "foreign \"%s\" {\n    fn %s() -> Int32\n}\n\n" l fn) libs)
    ^ "pub fn pid(f: Foreign) -> Int {\n    getpid(f)\n}\n"
  in
  put "cdep/cdep.tsr" (cdep_src [ ("c", "getpid") ]);
  let capp = "use cdep\n\nfn main(root: Root) {\n    root.stdout.print(\"{cdep.pid(root.foreign) > 0}\")\n}\n" in
  put "capp/main.tsr" capp;
  let lock = Filename.concat pdir "capp/tessera.lock" in
  let cpkg toml =
    put "capp/tessera.toml" toml;
    let _, ds, _ = Modules.load_and_check (Filename.concat pdir "capp/main.tsr") capp in
    ds
  in
  let dep = "[package]\nname = \"capp\"\n\n[dependencies]\ncdep = { path = \"../cdep\" }\n" in
  let show ds = String.concat "; " (List.map (fun (d : Diag.t) -> d.code ^ " " ^ d.message) ds) in
  (try Sys.remove lock with Sys_error _ -> ());
  let ds = cpkg dep in
  ok "trust-required" (codes ds = [ "E0320"; "E0320" ] && List.exists (fun (d : Diag.t) -> List.exists (fun n -> Interp_util.find_sub n "[trust]" 0 <> None) d.notes) ds) (show ds);
  ok "trust-not-caps" (codes (cpkg "[package]\nname = \"capp\"\n\n[dependencies]\ncdep = { path = \"../cdep\", caps = [\"foreign\"] }\n") = [ "E0321" ]) "caps = [foreign] is refused";
  ok "trust-unknown" (codes (cpkg (dep ^ "\n[trust]\nforeign = [\"other\"]\n")) = [ "E0321" ]) "trust names a dependency";
  let trusted = dep ^ "\n[trust]\nforeign = [\"cdep\"]\n" in
  let ds = cpkg trusted in
  ok "trust-granted" (ds = []) (show ds);
  ok "trust-locked" (Sys.file_exists lock && Interp_util.find_sub (read_file lock) "foreign cdep c\n" 0 <> None) (if Sys.file_exists lock then read_file lock else "no lock");
  ok "trust-stable" (cpkg trusted = []) "same libraries";
  (* an update that starts calling another library stops the build *)
  put "cdep/cdep.tsr" (cdep_src [ ("c", "getpid"); ("z", "zlibVersion") ]);
  let ds = cpkg trusted in
  ok "trust-lock-changed" (codes ds = [ "E0324" ] && Interp_util.find_sub (List.hd ds).message "`z`" 0 <> None) (show ds);
  (* deleting the line accepts the new libraries *)
  Out_channel.with_open_bin lock (fun oc -> output_string oc "# reviewed\n");
  ok "trust-relock" (cpkg trusted = [] && Interp_util.find_sub (read_file lock) "foreign cdep c,z\n" 0 <> None) (read_file lock);
  put "cdep/cdep.tsr" (cdep_src [ ("c", "getpid") ]);
  Sys.remove (Filename.concat pdir "capp/tessera.toml");

  (* [grants] in tessera.toml: the program's own grants, with --grant flags adding to them *)
  (* the CLI built next to this test binary, wherever it is run from *)
  let tessera_exe = Filename.concat (Filename.dirname (Filename.dirname Sys.executable_name)) "bin/main.exe" in
  let tessera_exe = if Filename.is_relative tessera_exe then Filename.concat (Sys.getcwd ()) tessera_exe else tessera_exe in
  let tessera_cli args =
    let o = Filename.concat pdir "cli.out" and e = Filename.concat pdir "cli.err" in
    let code =
      Sys.command
        (String.concat " " (List.map Filename.quote (tessera_exe :: args)) ^ sprintf " >%s 2>%s" (Filename.quote o) (Filename.quote e))
    in
    (code, read_file o, read_file e)
  in
  (* the build cache: the same program again is a copy of the first build
     wherever it goes, a changed program builds anew, and TESSERA_CACHE=off
     never touches the cache *)
  (let dir = Filename.concat pdir "own-cache" in
   let prog = Filename.concat pdir "cached.tsr" in
   let write text = Out_channel.with_open_bin prog (fun oc -> output_string oc text) in
   let build ?(cache = dir) out =
     Sys.command (sprintf "TESSERA_CACHE=%s %s build %s -o %s >/dev/null 2>&1" (Filename.quote cache) (Filename.quote tessera_exe) (Filename.quote prog) (Filename.quote out))
   in
   let entries () = try Array.length (Sys.readdir dir) with Sys_error _ -> 0 in
   let out n = Filename.concat pdir n in
   write "fn main(root: Root) {\n    root.stdout.print(\"cached\")\n}\n";
   let c1 = build (out "cached1") in
   let n1 = entries () in
   let c2 = build (out "cached2") in
   let n2 = entries () in
   let same = c1 = 0 && c2 = 0 && read_file (out "cached1") = read_file (out "cached2") in
   let ran = Sys.command (sprintf "%s > %s" (Filename.quote (out "cached2")) (Filename.quote (out "cached2.out"))) = 0 && read_file (out "cached2.out") = "cached\n" in
   write "fn main(root: Root) {\n    root.stdout.print(\"changed\")\n}\n";
   let c3 = build (out "cached3") in
   let n3 = entries () in
   let c4 = build ~cache:"off" (out "cached4") in
   let n4 = entries () in
   ok "build-cache" (same && ran && n1 = 1 && n2 = 1 && c3 = 0 && n3 = 2 && c4 = 0 && n4 = 2)
     (sprintf "codes %d %d %d %d, entries %d %d %d %d, same %b, ran %b" c1 c2 c3 c4 n1 n2 n3 n4 same ran));
  (* the examples' own tests, in both backends: the CRUD API's call its
     router directly, the operator's need no cluster *)
  List.iter
    (fun (name, file, summary, extra) ->
      let dir = if Sys.file_exists "../examples/todo_api.tsr" then "../examples" else "examples" in
      let code, o, e = tessera_cli (("test" :: extra) @ [ Filename.concat dir file ]) in
      let last = List.nth_opt (List.rev (String.split_on_char '\n' (String.trim o))) 0 |> Option.value ~default:"" in
      ok name (code = 0 && String.starts_with ~prefix:summary last) (o ^ e))
    [
      ("example-todo-api-tests", "todo_api.tsr", "12 tests, 0 failed", []);
      ("example-todo-api-tests-native", "todo_api.tsr", "12 tests, 0 failed", [ "--native" ]);
      ("example-site-operator-tests", "site_operator.tsr", "19 tests, 0 failed", []);
      ("example-site-operator-tests-native", "site_operator.tsr", "19 tests, 0 failed", [ "--native" ]);
    ];
  (* the foreign grant is announced as full access, and `tessera check` lists who calls C *)
  put "capp/tessera.toml" "[package]\nname = \"capp\"\n\n[dependencies]\ncdep = { path = \"../cdep\" }\n\n[trust]\nforeign = [\"cdep\"]\n\n[grants]\nforeign = \"c\"\n";
  let cmain = Filename.concat pdir "capp/main.tsr" in
  (try Sys.remove lock with Sys_error _ -> ());
  let code, o, e = tessera_cli [ "run"; cmain ] in
  ok "cli-foreign-warning"
    (code = 0 && o = "true\n" && e = "tessera: warning: --grant foreign=c is full access: C code can do anything this process can, past every other grant\n")
    (sprintf "%d %S %S" code o e);
  let code, o, _ = tessera_cli [ "check"; cmain ] in
  ok "cli-check-calls-c" (code = 0 && o = "ok\ncalls C: module `cdep` (dependency `cdep`, trusted in tessera.toml [trust]): c\n") o;
  put "capp/sqlmain.tsr" "use sql\n\nforeign \"c\" {\n    fn getpid() -> Int32\n}\n\nfn main(root: Root) {\n    root.stdout.print(\"{sql.memory().is_ok()} {getpid(root.foreign) > 0}\")\n}\n";
  let code, o, _ = tessera_cli [ "check"; Filename.concat pdir "capp/sqlmain.tsr" ] in
  ok "cli-check-calls-c-std" (code = 0 && o = "ok\ncalls C: the program itself (needs --grant foreign): c\ncalls C: module `sql` (standard library, trusted by src/std/tessera.toml): sqlite3\n") o;
  (* the operating-system sandbox: even C code stays inside the grants, in
     both backends, unless --no-sandbox *)
  List.iter (fun d -> try Unix.mkdir (Filename.concat pdir d) 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()) [ "sbox"; "sbox/data" ];
  let secret = Filename.concat pdir "sbox/secret.txt" in
  put "sbox/secret.txt" "secret";
  put "sbox/data/a.txt" "granted";
  let escape_file = Filename.concat pdir "sbox/escaped.txt" in
  let src =
    {v|foreign "c" {
    fn open(path: Str, flags: Int32) -> Int32
    fn creat(path: Str, mode: Int32) -> Int32
    fn close(fd: Int32) -> Int32
    fn system(cmd: Str) -> Int32
}

// O_RDONLY is 0 everywhere; creating goes through creat(), whose flags C fixes
fn try_open(c: Foreign, path: Str, create: Bool) -> Bool {
    let fd = if create { creat(c, path, 420) } else { open(c, path, 0) }
    if fd >= 0 {
        let closed = close(c, fd)
    }
    fd >= 0
}

fn main(root: Root) {
    let c = root.foreign
    let fs = root.fs
    let read = try_open(c, "SECRET", false)
    let write = try_open(c, "ESCAPE", true)
    let run = system(c, "true") == 0
    root.stdout.print("{read} {write} {run}")
    let mine = try_open(c, fs.full_path("a.txt") ?? "", false)
    root.stdout.print("{fs.read("a.txt")} {fs.write("b.txt", "x").is_ok()} {mine}")
}
|v}
  in
  let replace_all pat by s =
    let b = Buffer.create (String.length s) in
    let n = String.length pat in
    let i = ref 0 in
    while !i < String.length s do
      if !i + n <= String.length s && String.sub s !i n = pat then (Buffer.add_string b by; i := !i + n)
      else (Buffer.add_char b s.[!i]; incr i)
    done;
    Buffer.contents b
  in
  put "sbox/main.tsr" (replace_all "ESCAPE" escape_file (replace_all "SECRET" secret src));
  let smain = Filename.concat pdir "sbox/main.tsr" in
  let grant = [ "--grant"; "foreign=c"; "--grant"; "fs=" ^ Filename.concat pdir "sbox/data" ] in
  List.iter
    (fun (name, extra, want) ->
      (try Sys.remove escape_file with Sys_error _ -> ());
      let code, o, e = tessera_cli ((("run" :: extra) @ [ smain ]) @ grant) in
      ok name (code = 0 && o = want && Sys.file_exists escape_file = (extra = [ "--no-sandbox" ])) (sprintf "%d %S %S" code o e))
    [ ("sandbox-interp", [], "false false false\nOk(\"granted\") true true\n");
      ("sandbox-native", [ "--native" ], "false false false\nOk(\"granted\") true true\n");
      ("sandbox-off", [ "--no-sandbox" ], "true true true\nOk(\"granted\") true true\n") ];
  (* a built binary takes --no-sandbox too, and refuses unknown arguments *)
  let sbin = Filename.concat pdir "sbox/prog" in
  let code, _, _ = tessera_cli [ "build"; smain; "-o"; sbin ] in
  if code = 0 then begin
    let run args =
      let o = Filename.concat pdir "sbox/out" in
      let c = Sys.command (String.concat " " (List.map Filename.quote (sbin :: args @ grant)) ^ " >" ^ Filename.quote o ^ " 2>/dev/null") in
      (c, read_file o)
    in
    let _, o1 = run [] in
    let _, o2 = run [ "--no-sandbox" ] in
    ok "sandbox-binary" (String.starts_with ~prefix:"false false false" o1 && String.starts_with ~prefix:"true true true" o2) (o1 ^ o2)
  end
  else ok "sandbox-binary" false "tessera build failed";
  (try Sys.remove escape_file with Sys_error _ -> ());
  (* without Net, Listen or Exec, the sandbox also refuses the ways out that
     are not files: another program's Unix socket (here a listener of the
     test's own, which could as well be a Docker daemon or an ssh-agent),
     another process's /proc entries, and signalling another process *)
  let linux = (try String.trim (In_channel.with_open_text "/proc/sys/kernel/ostype" In_channel.input_all) = "Linux" with Sys_error _ -> false) in
  let sock_path = sprintf "/tmp/tessera-sb-%d.sock" (Unix.getpid ()) in
  (try Sys.remove sock_path with Sys_error _ -> ());
  let lsock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind lsock (Unix.ADDR_UNIX sock_path);
  Unix.listen lsock 4;
  let sleeper = Unix.create_process "sleep" [| "sleep"; "60" |] Unix.stdin Unix.stdout Unix.stderr in
  let probe =
    {v|foreign "c" {
    fn socket(domain: Int32, kind: Int32, protocol: Int32) -> Int32
    fn connect(fd: Int32, addr: Bytes, len: Int32) -> Int32
    fn open(path: Str, flags: Int32) -> Int32
    fn close(fd: Int32) -> Int32
    fn kill(pid: Int32, sig: Int32) -> Int32
}

// a sockaddr_un (a path ends in a NUL; an abstract name does not): Linux starts with a 16-bit family, macOS with a length byte and a family byte
fn unix_addr(path: Bytes) -> Bytes {
    let head = if LINUX { bytes([1, 0]) } else { bytes([0, 1]) }
    head + path
}

fn reach(c: Foreign, path: Bytes) -> Bool {
    let fd = socket(c, 1, 1, 0)
    if fd < 0 {
        return false
    }
    let addr = unix_addr(path)
    let ok = connect(c, fd, addr, addr.len()) == 0
    let closed = close(c, fd)
    ok
}

fn main(root: Root) {
    let c = root.foreign
    let sock = reach(c, "SOCK".to_bytes() + bytes([0]))
    let fd = open(c, "/proc/PID/cmdline", 0)
    let proc = fd >= 0
    if proc {
        let closed = close(c, fd)
    }
    let signal = kill(c, PID, 0) == 0
    // an abstract socket (Linux): a leading NUL byte, no file at all
    let abstract = LINUX && reach(c, bytes([0]) + "tessera-sb-abstract".to_bytes())
    root.stdout.print("{sock} {proc} {signal} {abstract}")
}
|v}
  in
  let probe = replace_all "SOCK" sock_path (replace_all "PID" (string_of_int sleeper) (replace_all "LINUX" (string_of_bool linux) probe)) in
  put "sbox/probe.tsr" probe;
  let pmain = Filename.concat pdir "sbox/probe.tsr" in
  (* an abstract listener for the last probe *)
  let asock =
    if linux then begin
      let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      Unix.bind s (Unix.ADDR_UNIX "\000tessera-sb-abstract");
      Unix.listen s 4;
      Some s
    end
    else None
  in
  (* /proc exists on Linux only *)
  let open_all = sprintf "true %b true %b\n" linux linux in
  List.iter
    (fun (name, extra, want) ->
      let code, o, e = tessera_cli ((("run" :: extra) @ [ pmain ]) @ [ "--grant"; "foreign=c" ]) in
      ok name (code = 0 && o = want) (sprintf "%d %S %S" code o e))
    [ ("sandbox-ipc-interp", [], "false false false false\n");
      ("sandbox-ipc-native", [ "--native" ], "false false false false\n");
      ("sandbox-ipc-off", [ "--no-sandbox" ], open_all) ];
  Unix.kill sleeper Sys.sigkill;
  ignore (Unix.waitpid [] sleeper);
  Unix.close lsock;
  Option.iter Unix.close asock;
  (try Sys.remove sock_path with Sys_error _ -> ());
  Sys.remove (Filename.concat pdir "capp/tessera.toml");
  let _, o, e = tessera_cli [ "run"; Filename.concat pdir "capp/sqlmain.tsr" ] in
  ok "cli-no-foreign-no-warning" (o = "" && String.starts_with ~prefix:"panic: foreign: library `c` is not granted" e) (o ^ e);
  List.iter (fun d -> try Unix.mkdir (Filename.concat pdir d) 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()) [ "grants"; "grants/data"; "envy" ];
  put "grants/data/a.txt" "hi";
  Unix.putenv "TESSERA_T_A" "a";
  Unix.putenv "TESSERA_T_B" "b";
  put "grants/tessera.toml" "[package]\nname = \"g\"\n\n[grants]\nfs = \"data:ro\"   # read-only\nenv = [\"TESSERA_T_A\"]\n";
  put "grants/main.tsr"
    "fn main(root: Root) {\n    root.stdout.print(root.fs.read(\"a.txt\") ?? \"denied\")\n    root.stdout.print(root.env.get(\"TESSERA_T_A\") ?? \"none\")\n    root.stdout.print(root.env.get(\"TESSERA_T_B\") ?? \"none\")\n    root.stdout.print(\"{root.fs.write(\"b.txt\", \"x\").is_ok()}\")\n}\n";
  let gmain = Filename.concat pdir "grants/main.tsr" in
  (match Package.load gmain with
   | Ok (Some p) ->
     ok "grants-toml-parse"
       (p.grants = [ ("fs", Filename.concat (Filename.concat pdir "grants") "data" ^ ":ro"); ("env", "TESSERA_T_A") ])
       (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) p.grants))
   | _ -> ok "grants-toml-parse" false "no package");
  List.iter
    (fun (name, extra) ->
      let code, o, e = tessera_cli ([ "run" ] @ extra @ [ gmain ]) in
      ok ("grants-toml-" ^ name) (code = 0 && o = "hi\na\nnone\nfalse\n") (sprintf "code %d, out %S, err %S" code o e);
      let code, o, e = tessera_cli ([ "run" ] @ extra @ [ gmain; "--grant"; "env=TESSERA_T_B" ]) in
      ok ("grants-toml-flag-adds-" ^ name) (code = 0 && o = "hi\na\nb\nfalse\n") (sprintf "code %d, out %S, err %S" code o e))
    ([ ("interp", []) ] @ if native_enabled then [ ("native", [ "--native" ]) ] else []);
  (* a flag's fs replaces the toml's: here a writable one *)
  let code, o, e = tessera_cli [ "run"; gmain; "--grant"; "fs=" ^ Filename.concat pdir "grants/data" ] in
  ok "grants-toml-fs-flag" (code = 0 && o = "hi\na\nnone\ntrue\n") (sprintf "code %d, out %S, err %S" code o e);
  let grant_err toml =
    put "grants/tessera.toml" toml;
    let code, _, e = tessera_cli [ "run"; gmain ] in
    code = 1 && Interp_util.find_sub e "E0321" 0 <> None
  in
  ok "grants-toml-always" (grant_err "[grants]\nstdout = \"x\"\n") "stdout is always granted";
  ok "grants-toml-unknown" (grant_err "[grants]\ndisk = \"x\"\n") "unknown grant";
  ok "grants-toml-two-fs" (grant_err "[grants]\nfs = [\"a\", \"b\"]\n") "one fs";
  ok "grants-toml-twice" (grant_err "[grants]\nnet = \"a\"\nnet = \"b\"\n") "listed twice";
  Sys.remove (Filename.concat pdir "grants/tessera.toml");
  (* a dependency's own tessera.toml never grants anything: only the program's does *)
  put "envy/envy.tsr" "pub fn get(env: Env) -> Str {\n    env.get(\"TESSERA_T_A\") ?? \"none\"\n}\n";
  put "envy/tessera.toml" "[package]\nname = \"envy\"\n\n[grants]\nenv = \"*\"\n";
  put "grants/tessera.toml" "[package]\nname = \"g\"\n\n[dependencies]\nenvy = { path = \"../envy\", caps = [\"env\"] }\n";
  put "grants/dep.tsr" "use envy\n\nfn main(root: Root) {\n    root.stdout.print(envy.get(root.env))\n}\n";
  let code, o, e = tessera_cli [ "run"; Filename.concat pdir "grants/dep.tsr" ] in
  ok "grants-dep-toml-ignored" (code = 0 && o = "none\n") (sprintf "code %d, out %S, err %S" code o e);
  Sys.remove (Filename.concat pdir "grants/tessera.toml");

  (* git dependencies: a local file:// repository, fetched into a temporary
     cache, pinned by tessera.lock *)
  let gdir = Filename.concat pdir "git" in
  List.iter (fun d -> try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()) [ gdir; Filename.concat gdir "repo"; Filename.concat gdir "app" ];
  Unix.putenv "TESSERA_CACHE" (Filename.concat gdir "cache");
  let repo = Filename.concat gdir "repo" in
  let git args = Sys.command (String.concat " " (List.map Filename.quote ([ "git"; "-C"; repo; "-c"; "user.name=t"; "-c"; "user.email=t@t" ] @ args)) ^ " >/dev/null 2>&1") in
  let rev_of r =
    let o = Filename.concat gdir "rev" in
    ignore (Sys.command (sprintf "git -C %s rev-parse %s >%s" (Filename.quote repo) (Filename.quote r) (Filename.quote o)));
    String.trim (read_file o)
  in
  let version v = Out_channel.with_open_bin (Filename.concat repo "greet.tsr") (fun oc -> output_string oc (sprintf "pub fn hi() -> Str {\n    \"%s\"\n}\n" v)) in
  ok "git-setup"
    (git [ "init"; "-q" ] = 0
     && (version "v1"; git [ "add"; "." ] = 0)
     && git [ "commit"; "-qm"; "one" ] = 0
     && git [ "tag"; "v1" ] = 0
     && (version "v2"; git [ "commit"; "-qam"; "two" ] = 0)
     && git [ "tag"; "v2" ] = 0)
    "could not create the test repository";
  let v1 = rev_of "v1" and v2 = rev_of "v2" in
  let url = "file://" ^ repo in
  let gapp = "use greet\n\nfn main(root: Root) {\n    root.stdout.print(greet.hi())\n}\n" in
  let gmain = Filename.concat gdir "app/main.tsr" in
  put "git/app/main.tsr" gapp;
  let lock = Filename.concat gdir "app/tessera.lock" in
  let gtoml dep = put "git/app/tessera.toml" (sprintf "[package]\nname = \"app\"\n\n[dependencies]\ngreet = { %s }\n" dep) in
  let grun name want =
    match Modules.load_and_check gmain gapp with
    | items, [], _ -> (
      let buf = Buffer.create 16 in
      match Interp.run ~out:(Buffer.add_string buf) ~grants:Interp.no_grants items with
      | Ok _ -> ok name (Buffer.contents buf = want ^ "\n") (sprintf "got %S, want %S" (Buffer.contents buf) want)
      | Error (m, _) -> ok name false m)
    | _, ds, _ -> ok name false (String.concat "; " (List.map (fun (d : Diag.t) -> d.code ^ " " ^ d.message) ds))
  in
  let gcodes () = let _, ds, _ = Modules.load_and_check gmain gapp in codes ds in
  let lock_has s = Sys.file_exists lock && Interp_util.find_sub (read_file lock) s 0 <> None in
  gtoml (sprintf "git = %S, tag = \"v1\"" url);
  grun "git-tag" "v1";
  ok "git-lock-written" (lock_has (sprintf "greet %s tag:v1 %s blake2b:" url v1)) (if Sys.file_exists lock then read_file lock else "no tessera.lock");
  if native_enabled then begin
    let code, o, e = tessera_cli [ "run"; "--native"; gmain ] in
    ok "git-tag/native" (code = 0 && o = "v1\n") (sprintf "code %d, out %S, err %S" code o e)
  end;
  (* the tag moves upstream: the lock still pins the old commit *)
  ok "git-move-tag" (git [ "tag"; "-f"; "v1"; "v2" ] = 0) "git tag -f";
  grun "git-locked" "v1";
  (* the editor and agent servers do not fetch, but use what is cached *)
  Package.offline := true;
  grun "git-offline-cached" "v1";
  (* a changed cached copy no longer matches the lock *)
  let cached = Filename.concat (Package.commit_dir url v1) "greet.tsr" in
  let orig = read_file cached in
  Out_channel.with_open_bin cached (fun oc -> output_string oc (orig ^ "// changed\n"));
  ok "git-hash-mismatch" (gcodes () = [ "E0323" ]) (String.concat "," (gcodes ()));
  Out_channel.with_open_bin cached (fun oc -> output_string oc orig);
  grun "git-hash-restored" "v1";
  (* a changed tag in tessera.toml is fetched, except offline *)
  gtoml (sprintf "git = %S, tag = \"v2\"" url);
  ok "git-offline-unfetched" (gcodes () = [ "E0322" ]) (String.concat "," (gcodes ()));
  Package.offline := false;
  grun "git-retag" "v2";
  ok "git-lock-updated" (lock_has ("tag:v2 " ^ v2) && not (lock_has "tag:v1")) (read_file lock);
  (* a commit, abbreviated *)
  gtoml (sprintf "git = %S, commit = %S" url (String.sub v1 0 10));
  grun "git-commit" "v1";
  ok "git-lock-commit" (lock_has (sprintf "commit:%s %s" (String.sub v1 0 10) v1)) (read_file lock);
  (* a deleted cache is fetched again at the locked commit *)
  Package.rm_rf (Filename.concat gdir "cache");
  grun "git-refetch" "v1";
  (* errors *)
  gtoml (sprintf "git = %S, tag = \"v9\"" url);
  ok "git-no-tag" (gcodes () = [ "E0322" ]) (String.concat "," (gcodes ()));
  gtoml (sprintf "git = %S, commit = \"%s\"" url (String.make 40 'a'));
  ok "git-no-commit" (gcodes () = [ "E0322" ]) (String.concat "," (gcodes ()));
  List.iter
    (fun (name, dep) ->
      gtoml dep;
      ok name (gcodes () = [ "E0321" ]) (String.concat "," (gcodes ())))
    [ ("git-bad-url-dash", "git = \"-uhttps://x\", tag = \"v1\"");
      ("git-bad-url-ext", "git = \"ext::sh -c touch% /tmp/x\", tag = \"v1\"");
      ("git-bad-url-scheme", "git = \"ftp://x/y\", tag = \"v1\"");
      ("git-no-rev", sprintf "git = %S" url);
      ("git-tag-and-commit", sprintf "git = %S, tag = \"v1\", commit = %S" url v1);
      ("git-short-commit", sprintf "git = %S, commit = \"abc\"" url);
      ("git-and-path", sprintf "git = %S, path = \"x\", tag = \"v1\"" url) ];
  ok "git-url-scp" (Package.git_url_ok "git@github.com:user/repo.git") "scp-like";
  gtoml (sprintf "git = %S, tag = \"v2\"" url);
  Out_channel.with_open_bin lock (fun oc -> output_string oc "greet only-two-fields\n");
  ok "git-bad-lock" (gcodes () = [ "E0323" ]) (String.concat "," (gcodes ()));
  Sys.remove lock;
  grun "git-relock" "v2";
  (* capabilities of a git dependency are checked like any other *)
  version "v3";
  Out_channel.with_open_bin (Filename.concat repo "greet.tsr") (fun oc ->
      output_string oc "pub fn hi() -> Str {\n    \"v3\"\n}\n\npub fn peek(fs: Fs) -> Bool {\n    fs.exists(\"x\")\n}\n");
  ok "git-v3" (git [ "commit"; "-qam"; "three" ] = 0 && git [ "tag"; "v3" ] = 0) "v3";
  gtoml (sprintf "git = %S, tag = \"v3\"" url);
  ok "git-caps-denied" (gcodes () = [ "E0320" ]) (String.concat "," (gcodes ()));
  gtoml (sprintf "git = %S, tag = \"v3\", caps = [\"fs\"]" url);
  grun "git-caps-granted" "v3";

  (* the package index: tessera index entry, tessera search and tessera add, with an
     index directory and git repositories made here *)
  let rdir = Filename.concat pdir "registry" in
  let rpath p = Filename.concat rdir p in
  List.iter (fun d -> try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    (rdir :: List.map rpath [ "index"; "color"; "peek"; "mover"; "cfun"; "app1"; "app2"; "app3"; "app4" ]);
  let rgit repo args = Sys.command (String.concat " " (List.map Filename.quote ([ "git"; "-C"; rpath repo; "-c"; "user.name=t"; "-c"; "user.email=t@t" ] @ args)) ^ " >/dev/null 2>&1") = 0 in
  let rput f s = Out_channel.with_open_bin (rpath f) (fun oc -> output_string oc s) in
  (* run tessera in [dir]; returns (exit code, stdout, stderr) *)
  let rcli dir args =
    let o = rpath "cli.out" and e = rpath "cli.err" in
    let code =
      Sys.command
        (sprintf "cd %s && %s >%s 2>%s" (Filename.quote dir) (String.concat " " (List.map Filename.quote (tessera_exe :: args))) (Filename.quote o) (Filename.quote e))
    in
    (code, read_file o, read_file e)
  in
  let has s sub = Interp_util.find_sub s sub 0 <> None in
  let show3 (c, o, e) = sprintf "code %d, out %S, err %S" c o e in
  (* a package whose current tag goes into the index as `tessera index entry` prints it *)
  let index_entry repo file =
    let ((c, o, _) as r) = rcli (rpath repo) [ "index"; "entry" ] in
    if c = 0 then Out_channel.with_open_bin (rpath ("index/" ^ file)) (fun oc -> output_string oc o);
    r
  in
  let url repo = "file://" ^ rpath repo in
  ok "registry-setup"
    (rgit "color" [ "init"; "-q" ]
     && (rput "color/tessera.toml" "[package]\nname = \"color\"\ndescription = \"Colors for terminal output\"\n";
         rput "color/color.tsr" "pub fn name() -> Str {\n    \"red\"\n}\n";
         rgit "color" [ "add"; "." ])
     && rgit "color" [ "commit"; "-qm"; "one" ] && rgit "color" [ "tag"; "v1.2.0" ]
     && rgit "color" [ "remote"; "add"; "origin"; url "color" ])
    "could not create the color repository";
  (* a package with no tag yet, or with uncommitted changes, has no entry *)
  rput "color/color.tsr" "pub fn name() -> Str {\n    \"red\"\n}\n\npub fn red(out: Stdout, s: Str) {\n    out.print(s)\n}\n";
  let c, _, e = rcli (rpath "color") [ "index"; "entry" ] in
  ok "registry-entry-dirty" (c = 1 && has e "uncommitted changes") e;
  ok "registry-commit" (rgit "color" [ "commit"; "-qam"; "two" ]) "commit";
  let c, _, e = rcli (rpath "color") [ "index"; "entry" ] in
  ok "registry-entry-untagged" (c = 1 && has e "has no tag") e;
  ok "registry-tag" (rgit "color" [ "tag"; "v1.10.0" ]) "tag";
  let ((c, o, _) as r) = index_entry "color" "color-1.10.0.toml" in
  ok "registry-entry"
    (c = 0 && has o "[[package]]\nname = \"color\"\n" && has o (sprintf "git = %S\n" (url "color")) && has o "tag = \"v1.10.0\"\n"
     && has o "description = \"Colors for terminal output\"\n" && has o "caps = [\"stdout\"]\n" && has o "commit = \"")
    (show3 r);
  ok "registry-checkout-old" (rgit "color" [ "checkout"; "-q"; "v1.2.0" ]) "checkout";
  let ((c, o, _) as r) = index_entry "color" "color-1.2.0.toml" in
  ok "registry-entry-pure" (c = 0 && has o "tag = \"v1.2.0\"\n" && has o "caps = []\n") (show3 r);
  (* a package that uses Fs, indexed as pure: the index entry lies *)
  ok "registry-peek"
    (rgit "peek" [ "init"; "-q" ]
     && (rput "peek/peek.tsr" "pub fn peek(fs: Fs) -> Bool {\n    fs.exists(\"x\")\n}\n"; rgit "peek" [ "add"; "." ])
     && rgit "peek" [ "commit"; "-qm"; "one" ] && rgit "peek" [ "tag"; "v1" ])
    "peek";
  let c, o, _ = rcli (rpath "peek") [ "index"; "entry" ] in
  ok "registry-entry-fs" (c = 0 && has o "caps = [\"fs\"]\n" && has o "# no git remote") o;
  let rev repo r =
    let f = rpath "rev" in
    ignore (Sys.command (sprintf "git -C %s rev-parse %s >%s" (Filename.quote (rpath repo)) (Filename.quote r) (Filename.quote f)));
    String.trim (read_file f)
  in
  let entry name repo tag commit desc caps =
    sprintf "[[package]]\nname = %S\ngit = %S\ntag = %S\ncommit = %S\ndescription = %S\ncaps = [%s]\n" name (url repo) tag commit desc
      (String.concat ", " (List.map (sprintf "%S") caps))
  in
  rput "index/peek.toml" (entry "peek" "peek" "v1" (rev "peek" "v1") "Looks for a file" []);
  (* a package whose tag moves after it is indexed *)
  ok "registry-mover"
    (rgit "mover" [ "init"; "-q" ]
     && (rput "mover/mover.tsr" "pub fn hi() -> Str {\n    \"one\"\n}\n"; rgit "mover" [ "add"; "." ])
     && rgit "mover" [ "commit"; "-qm"; "one" ] && rgit "mover" [ "tag"; "v1" ])
    "mover";
  rput "index/mover.toml" (entry "mover" "mover" "v1" (rev "mover" "v1") "Says hi" []);
  ok "registry-mover-moved"
    ((rput "mover/mover.tsr" "pub fn hi() -> Str {\n    \"two\"\n}\n"; rgit "mover" [ "commit"; "-qam"; "two" ]) && rgit "mover" [ "tag"; "-f"; "v1" ])
    "move the tag";
  (* a package that calls C *)
  ok "registry-cfun"
    (rgit "cfun" [ "init"; "-q" ]
     && (rput "cfun/cfun.tsr" "foreign \"c\" {\n    fn getpid() -> Int32\n}\n\npub fn pid(f: Foreign) -> Int {\n    getpid(f)\n}\n";
         rgit "cfun" [ "add"; "." ])
     && rgit "cfun" [ "commit"; "-qm"; "one" ] && rgit "cfun" [ "tag"; "v0.1.0" ])
    "cfun";
  let c, o, e = rcli (rpath "cfun") [ "index"; "entry" ] in
  ok "registry-entry-foreign" (c = 0 && has o "caps = [\"foreign\"]\n") (o ^ e);
  rput "index/cfun.toml" (entry "cfun" "cfun" "v0.1.0" (rev "cfun" "v0.1.0") "The process id, from C" [ "foreign" ]);

  (* no index, an unreachable one, a malformed one *)
  Unix.putenv "TESSERA_INDEX" "";
  let c, _, e = rcli rdir [ "search"; "color" ] in
  ok "registry-no-index" (c = 1 && has e "no package index is set" && has e "TESSERA_INDEX") e;
  Unix.putenv "TESSERA_INDEX" ("file://" ^ rpath "nowhere");
  let c, _, e = rcli rdir [ "search"; "color" ] in
  ok "registry-unreachable" (c = 1 && has e "cannot reach the package index") e;
  rput "bad.toml" "[[package]]\nname = \"x\"\ngit = \"ftp://x\"\ntag = \"v1\"\ncommit = \"abcdef0\"\n";
  Unix.putenv "TESSERA_INDEX" (rpath "bad.toml");
  let c, _, e = rcli rdir [ "search" ] in
  ok "registry-bad-index" (c = 1 && has e "bad.toml:3: `git` must be") e;

  (* search: every word in the name or description; the latest version by number *)
  Unix.putenv "TESSERA_INDEX" (rpath "index");
  let ((c, o, _) as r) = rcli rdir [ "search"; "terminal"; "COLORS" ] in
  ok "registry-search"
    (c = 0 && has o "color v1.10.0: Colors for terminal output\n  caps: stdout\n  older: v1.2.0\n" && not (has o "peek"))
    (show3 r);
  let c, o, _ = rcli rdir [ "search" ] in
  ok "registry-search-all" (c = 0 && has o "peek v1: Looks for a file\n  caps: none (pure)\n" && has o "cfun v0.1.0" && has o "it calls C") o;
  let c, o, _ = rcli rdir [ "search"; "nothing-like-this" ] in
  ok "registry-search-none" (c = 0 && has o "no package matches") o;
  (* the index as a git repository *)
  ok "registry-index-git" (rgit "index" [ "init"; "-q" ] && rgit "index" [ "add"; "." ] && rgit "index" [ "commit"; "-qm"; "index" ]) "index repo";
  Unix.putenv "TESSERA_INDEX" (url "index");
  let c, o, e = rcli rdir [ "search"; "color" ] in
  ok "registry-search-git" (c = 0 && has o "color v1.10.0") (o ^ e);
  Unix.putenv "TESSERA_INDEX" (rpath "index");

  (* add: the latest version, with the caps the index lists; then it builds and runs *)
  let app_src = "use color\n\nfn main(root: Root) {\n    color.red(root.stdout, color.name())\n}\n" in
  rput "app1/main.tsr" app_src;
  rput "app1/tessera.toml" "[package]\nname = \"app\"\n\n[dependencies]\n\n[grants]\nenv = [\"HOME\"]\n";
  let ((c, o, _) as r) = rcli (rpath "app1") [ "add"; "color" ] in
  ok "registry-add" (c = 0 && has o "added color v1.10.0" && has o "color may use: stdout\n") (show3 r);
  let toml = read_file (rpath "app1/tessera.toml") in
  ok "registry-add-toml"
    (toml = sprintf "[package]\nname = \"app\"\n\n[dependencies]\ncolor = { git = %S, tag = \"v1.10.0\", caps = [\"stdout\"] }\n\n[grants]\nenv = [\"HOME\"]\n" (url "color"))
    toml;
  ok "registry-add-lock" (has (read_file (rpath "app1/tessera.lock")) (sprintf "color %s tag:v1.10.0 %s blake2b:" (url "color") (rev "color" "v1.10.0"))) (read_file (rpath "app1/tessera.lock"));
  let c, o, e = rcli (rpath "app1") [ "run"; "main.tsr" ] in
  ok "registry-add-run" (c = 0 && o = "red\n") (show3 (c, o, e));
  let c, _, e = rcli (rpath "app1") [ "add"; "color" ] in
  ok "registry-add-twice" (c = 1 && has e "already a dependency") e;
  (* a chosen version, with or without its `v`; a new tessera.toml when there is none *)
  let c, o, _ = rcli (rpath "app2") [ "add"; "color@1.2.0" ] in
  ok "registry-add-version"
    (c = 0 && has o "color may use no capabilities: it is pure"
     && read_file (rpath "app2/tessera.toml") = sprintf "[dependencies]\ncolor = { git = %S, tag = \"v1.2.0\", caps = [] }\n" (url "color"))
    (o ^ read_file (rpath "app2/tessera.toml"));
  let c, _, e = rcli (rpath "app3") [ "add"; "color@v9" ] in
  ok "registry-add-no-version" (c = 1 && has e "no version v9 of `color`; it has v1.10.0, v1.2.0") e;
  let c, _, e = rcli (rpath "app3") [ "add"; "nosuch" ] in
  ok "registry-add-unknown" (c = 1 && has e "no package named `nosuch`") e;
  (* refused: the code uses more than the index lists, or the tag moved *)
  let c, _, e = rcli (rpath "app3") [ "add"; "peek" ] in
  ok "registry-add-caps-mismatch" (c = 1 && has e "`peek` v1 uses fs, which the index does not list (it lists none)" && not (Sys.file_exists (rpath "app3/tessera.toml"))) e;
  let c, _, e = rcli (rpath "app3") [ "add"; "mover" ] in
  ok "registry-add-moved-tag" (c = 1 && has e "tag v1 of" && has e "was moved after the package was indexed" && not (Sys.file_exists (rpath "app3/tessera.toml"))) e;
  (* a package that calls C is added without trust; the program builds once the user trusts it *)
  rput "app4/main.tsr" "use cfun\n\nfn main(root: Root) {\n    root.stdout.print(\"{cfun.pid(root.foreign) > 0}\")\n}\n";
  let ((c, o, _) as r) = rcli (rpath "app4") [ "add"; "cfun" ] in
  ok "registry-add-foreign" (c = 0 && has o "cfun calls C (c)" && has o "[trust]\nforeign = [\"cfun\"]" && not (has (read_file (rpath "app4/tessera.toml")) "[trust]")) (show3 r);
  let c, _, e = rcli (rpath "app4") [ "check"; "main.tsr" ] in
  ok "registry-add-foreign-untrusted" (c = 1 && has e "E0320") e;
  rput "app4/tessera.toml" (read_file (rpath "app4/tessera.toml") ^ "\n[trust]\nforeign = [\"cfun\"]\n");
  let c, o, e = rcli (rpath "app4") [ "check"; "main.tsr" ] in
  ok "registry-add-foreign-trusted" (c = 0 && has o "ok") (o ^ e);
  Unix.putenv "TESSERA_INDEX" "";

  (* the standard library declares its capabilities in src/std/tessera.toml and is checked against it *)
  let manifest = Lazy.force Package.std_manifest in
  let listed = List.sort compare (List.map (fun (d : Package.dep) -> d.dname) manifest.deps) in
  ok "std-manifest-complete" (listed = List.sort compare (List.map fst Std_text.modules)) (String.concat ", " listed);
  ok "std-manifest-http" (List.exists (fun (d : Package.dep) -> d.dname = "http" && d.caps = [ "net"; "listen" ]) manifest.deps) "http: net, listen";
  ok "std-manifest-sql" (List.exists (fun (d : Package.dep) -> d.dname = "sql" && d.caps = [ "fs" ]) manifest.deps && manifest.trust = [ "sql" ]) "sql: fs, trusted with C";
  ok "std-manifest-time" (List.exists (fun (d : Package.dep) -> d.dname = "time" && d.caps = [ "clock"; "fs" ]) manifest.deps) "time: clock (within), fs (load_zone)";
  ok "std-manifest-pure" (List.for_all (fun (d : Package.dep) -> d.dname = "http" || d.dname = "sql" || d.dname = "time" || d.caps = []) manifest.deps) "the others are pure";
  let real_std = !Modules.std_modules in
  let std_errs name extra src =
    Modules.std_modules := extra @ real_std;
    let _, ds, _ = Modules.load_and_check (name ^ ".tsr") src in
    Modules.std_modules := real_std;
    ds
  in
  (* a std module the manifest does not list may take nothing *)
  let ds = std_errs "std-unlisted" [ ("sneaky", "pub fn peek(fs: Fs) -> Bool {\n    fs.exists(\"x\")\n}\n") ]
      "use sneaky\n\nfn main(root: Root) {\n    root.stdout.print(\"{sneaky.peek(root.fs)}\")\n}\n" in
  ok "std-unlisted" (codes ds = [ "E0320" ] && Interp_util.find_sub (List.hd ds).message "standard library module `sneaky`" 0 <> None)
    (String.concat "; " (List.map (fun (d : Diag.t) -> d.code ^ " " ^ d.message) ds));
  (* a listed std module taking more than it declares *)
  let ds = std_errs "std-over" [ ("json", "pub fn leak(net: Net) -> Bool {\n    net.get(\"http://x\").is_ok()\n}\n") ]
      "use json\n\nfn main(root: Root) {\n    root.stdout.print(\"{json.leak(root.net)}\")\n}\n" in
  ok "std-over" (codes ds = [ "E0320" ]) (String.concat "; " (List.map (fun (d : Diag.t) -> d.code ^ " " ^ d.message) ds));
  (* a foreign block is a capability use: a module not granted `foreign` cannot declare one *)
  let ds = std_errs "std-foreign" [ ("sneakyc", "foreign \"c\" {\n    fn getpid() -> Int32\n}\n\npub fn pid(f: Foreign) -> Int {\n    getpid(f)\n}\n") ]
      "use sneakyc\n\nfn main(root: Root) {\n    root.stdout.print(\"{sneakyc.pid(root.foreign)}\")\n}\n" in
  ok "std-foreign" (codes ds = [ "E0320"; "E0320" ]) (String.concat "; " (List.map (fun (d : Diag.t) -> d.code ^ " " ^ d.message) ds));
  (* checked without any tessera.toml, and fine for the real std *)
  let _, ds, _ = Modules.load_and_check "std-http.tsr" "use http\nuse json\n\nfn main(root: Root) {\n    root.stdout.print(http.reason(200))\n}\n" in
  ok "std-real-ok" (ds = []) (String.concat "; " (List.map (fun (d : Diag.t) -> d.code ^ " " ^ d.message) ds));

  (* find references and rename on real programs: renaming every function,
     struct, enum and variant of each example must be accepted (a missed
     reference would make the renamed program fail to check), and the result
     must check cleanly *)
  let refs_failures = ref [] and renamed = ref 0 in
  Array.iter
    (fun f ->
      if Filename.check_suffix f ".tsr" then begin
        let path = Filename.concat "../examples" f in
        let src = In_channel.with_open_bin path In_channel.input_all in
        let load overrides =
          Modules.overlay := List.filter (fun (p, _) -> p <> path) overrides;
          let text = Option.value ~default:src (List.assoc_opt path overrides) in
          let r = Lsp.load_program path text in
          Modules.overlay := [];
          r
        in
        match load [] with
        | [], _ -> ()
        | items, ds when ds = [] ->
          let occs = Refs.index items in
          List.iter
            (fun (o : Refs.occ) ->
              if o.def && o.sp.file = path && (match o.key.[0] with 'f' | 't' | 'v' | 'd' -> true | _ -> false) && o.key <> "f:main" then begin
                let items, _ = load [] in
                let nw = if o.name.[0] >= 'A' && o.name.[0] <= 'Z' then o.name ^ "Renamed" else o.name ^ "_renamed" in
                match Refs.rename ~load items path o.sp.lo nw with
                | Ok files ->
                  incr renamed;
                  let text = Refs.apply_edits src (List.concat_map snd (List.filter (fun (p, _) -> p = path) files)) in
                  (match load [ (path, text) ] with
                   | _, [] -> ()
                   | _, d :: _ -> refs_failures := (f ^ " " ^ o.key ^ ": " ^ d.message) :: !refs_failures)
                | Error m when Interp_util.find_sub m "interface" 0 <> None -> ()
                | Error m -> refs_failures := (f ^ " " ^ o.key ^ ": " ^ m) :: !refs_failures
              end)
            occs
        | _ -> ()
      end)
    (if Sys.file_exists "../examples" then Sys.readdir "../examples" else [||]);
  ok "refs-rename-examples" (!refs_failures = [] && (!renamed > 50 || not (Sys.file_exists "../examples"))) (sprintf "%d renamed; %s" !renamed (String.concat "\n" (List.rev !refs_failures)));

  (* tessera doc: doc comments directly above items, fields, variants and interface methods *)
  let doc_src =
    "// Geometry helpers.\n//\n// Second paragraph.\n\n// A point.\npub struct Point {\n    // Distance from the left.\n    x: Float,\n    y: Float,\n}\n\n// not a doc: a blank line follows\n\n// How far from the origin.\npub fn norm(self: Point) -> Float {\n    self.x\n}\n\npub enum Shape {\n    // A round one.\n    Circle(Float),\n    Dot,\n}\n\n// Things with an area.\npub interface Area {\n    // The area, in square units.\n    fn area(self) -> Float\n}\n\n// Shouts.\npub fn loud(self: Str) -> Str {\n    self.upper()\n}\n\n// Private.\nfn hidden() -> Int {\n    1\n}\n\npub fn pick[T: Area](xs: List[T], i: Int) -> T {\n    xs[i]\n}\n"
  in
  let md = Doc.load_source "geom" "geom.tsr" doc_src in
  let mdtext = Doc.markdown [ md ] in
  let has s = Interp_util.find_sub mdtext s 0 <> None in
  ok "doc-markdown"
    (has "# Module `geom`\n\nGeometry helpers.\n\nSecond paragraph.\n"
     && has "### struct `Point`\n\n```tessera\npub struct Point {\n    x: Float,\n    y: Float,\n}\n```\n\nA point.\n\n- `x`: Distance from the left.\n"
     && has "#### Methods\n\n```tessera\npub fn norm(self: Point) -> Float\n```\n\nHow far from the origin.\n"
     && has "- `Circle`: A round one.\n" && has "Things with an area.\n\n- `area`: The area, in square units.\n"
     && has "## Methods on `Str`\n\n```tessera\npub fn loud(self: Str) -> Str\n```\n\nShouts.\n"
     && has "pub fn pick[T: Area](xs: List[T], i: Int) -> T\n"
     && (not (has "hidden")) && not (has "not a doc"))
    mdtext;
  let all = Doc.markdown [ Doc.load_source ~all:true "geom" "geom.tsr" doc_src ] in
  ok "doc-all" (Interp_util.find_sub all "fn hidden() -> Int\n```\n\nPrivate.\n" 0 <> None) all;
  let page = Doc.html [ md ] in
  let hp s = Interp_util.find_sub page s 0 <> None in
  ok "doc-html"
    (hp "<!doctype html>" && hp "<p>How far from the origin.</p>" && hp "<h2>Methods on Str</h2>"
     && hp "id=\"geom-Point\"" && hp "pick[T: Area](xs: List[T], i: Int) -&gt; T" && hp "<p>Geometry helpers.</p>\n<p>Second paragraph.</p>")
    page;
  (* every standard library module documents without error, and its functions have docs *)
  let std = List.map (fun n -> Doc.load n) (Doc.std_names ()) in
  let std_md = Doc.markdown std in
  ok "doc-std" (List.length std = List.length !Modules.std_modules && Interp_util.find_sub std_md "pub fn sort_by[T]" 0 <> None) std_md;

  (* comments inside an expression stay where they were written *)
  let fmt_case name src want =
    let got = try Fmt.format ~file:(name ^ ".tsr") src with Diag.Fatal d -> "error: " ^ d.message in
    ok name (got = want) (sprintf "got:\n%s\nwant:\n%s" got want);
    check_fmt name src ()
  in
  let canonical name src = fmt_case name src src in
  canonical "fmt-comment-chain"
    "fn main(root: Root) {\n    let n = [1, 2, 3]\n        // keep the odd ones\n        .filter(|x| x % 2 == 1)  // odd\n        .map(|x| x * 10)\n    root.stdout.print(\"{n}\")\n}\n";
  canonical "fmt-comment-list-end"
    "fn main(root: Root) {\n    let xs = [\n        1,\n        2,\n        // more later\n    ]\n    root.stdout.print(\"{xs}\")\n}\n";
  canonical "fmt-comment-call-end"
    "fn add(a: Int, b: Int) -> Int {\n    a + b\n}\n\nfn main(root: Root) {\n    let s = add(\n        1,\n        2,  // two\n        // that is all\n    )\n    root.stdout.print(\"{s}\")\n}\n";
  fmt_case "fmt-comment-args"
    "fn add(a: Int, b: Int) -> Int {\n    a + b\n}\n\nfn main(root: Root) {\n    let s = add(1, // one\n        2)\n    root.stdout.print(\"{s}\")\n}\n"
    "fn add(a: Int, b: Int) -> Int {\n    a + b\n}\n\nfn main(root: Root) {\n    let s = add(\n        1,  // one\n        2,\n    )\n    root.stdout.print(\"{s}\")\n}\n";
  fmt_case "fmt-comment-binary"
    "fn main(root: Root) {\n    let t = 1 + // first\n      2 * // second\n      3\n    root.stdout.print(\"{t}\")\n}\n"
    "fn main(root: Root) {\n    let t = 1 +  // first\n        2 *  // second\n        3\n    root.stdout.print(\"{t}\")\n}\n";
  fmt_case "fmt-comment-struct"
    "struct P {\n    x: Int,\n    y: Int,\n}\n\nfn main(root: Root) {\n    let p = P { x: 1, // the x\n        y: 2 }\n    root.stdout.print(\"{p.x}\")\n}\n"
    "struct P {\n    x: Int,\n    y: Int,\n}\n\nfn main(root: Root) {\n    let p = P {\n        x: 1,  // the x\n        y: 2,\n    }\n    root.stdout.print(\"{p.x}\")\n}\n";
  fmt_case "fmt-comment-closure"
    "fn main(root: Root) {\n    let f = |x: Int| // add one\n        x + 1\n    root.stdout.print(\"{f(1)}\")\n}\n"
    "fn main(root: Root) {\n    let f = |x: Int|  // add one\n        x + 1\n    root.stdout.print(\"{f(1)}\")\n}\n";
  (* an argument list on one line hugs an argument that breaks: its insides
     one level in, its closing bracket back at the call's indentation *)
  canonical "fmt-hug-list"
    "fn main(root: Root) {\n    let xs = f([\n        1,\n        2,\n    ])\n    root.stdout.print(\"{xs}\")\n}\n";
  canonical "fmt-hug-struct"
    "fn main(root: Root) {\n    let p = Some(P {\n        x: 1,\n    })\n    root.stdout.print(\"{p}\")\n}\n";
  canonical "fmt-hug-closure"
    "fn main(root: Root) {\n    group g {\n        g.go(|| {\n            root.stdout.print(\"a\")\n        })\n    }\n}\n";
  (* a comment inside an argument breaks only that argument, not the call around it *)
  canonical "fmt-hug-nested"
    "fn main(root: Root) {\n    let v = obj([\n        (\"a\", one(obj([\n            // the first\n            1,\n        ]))),\n    ])\n    root.stdout.print(\"{v}\")\n}\n";
  fmt_case "fmt-hug-from-broken"
    "fn main(root: Root) {\n    let xs = f([\n            1,\n        ])\n}\n"
    "fn main(root: Root) {\n    let xs = f([\n        1,\n    ])\n}\n";
  (* parentheses the author put around a compound operand stay; a negative
     number after `??` needs none *)
  canonical "fmt-keep-parens"
    "fn main(root: Root) {\n    let b = (1 < 2 && 2 < 3) || 3 < 4\n    let d = (9 / 3) % 2\n    let n = Some(1) ?? -1\n    root.stdout.print(\"{b} {d} {n}\")\n}\n";
  fmt_case "fmt-drop-parens"
    "fn main(root: Root) {\n    let n = (2) + (f(1))\n    let m = (-1)\n}\n"
    "fn main(root: Root) {\n    let n = 2 + f(1)\n    let m = -1\n}\n";
  (* trailing comments keep the author's alignment *)
  canonical "fmt-aligned-comments"
    "struct P {\n    x: Int,     // across\n    yy: Int,    // down\n}\n";
  (* a block of one short return or assignment, written on one line, stays there *)
  (* consecutive aliases stay together, unless the author spaced them *)
  canonical "fmt-type-alias" "use json\n\ntype A = Int\npub type B[T] = Map[Str, List[T]]\n\ntype C = fn(Int) -> json.Value  // later\n\nfn main(root: Root) {}\n";
  canonical "fmt-one-line-return"
    "fn f(o: Option[Int]) -> Int {\n    let n = o ?? { return 0 }\n    match o {\n        Some(_) => { return n },\n        None => { n = 1 },\n    }\n}\n";

  ok "fmt-use-pub"
    (Fmt.format ~file:"fu.tsr" "use geom // g\npub   fn f() -> Int { 1 }\npub struct S { a: Int }\n"
     = "use geom  // g\n\npub fn f() -> Int { 1 }\n\npub struct S {\n    a: Int,\n}\n")
    (Fmt.format ~file:"fu.tsr" "use geom // g\npub   fn f() -> Int { 1 }\npub struct S { a: Int }\n");

  (* the formatter on the examples and benchmarks too (with their comments) *)
  List.iter
    (fun dir ->
      if Sys.file_exists dir then
        Array.iter
          (fun f ->
            if Filename.check_suffix f ".tsr" then begin
              let path = Filename.concat dir f in
              let src = In_channel.with_open_bin path In_channel.input_all in
              match Parser.parse_program path src with
              | _ -> check_fmt ("file-" ^ f) src ()
              | exception Diag.Fatal _ -> ()
            end)
          (Sys.readdir dir))
    [ "../examples"; "../bench" ];
  (* and the examples, benchmarks and standard library are already formatted *)
  let unformatted =
    List.concat_map
      (fun dir ->
        if not (Sys.file_exists dir) then []
        else
          Sys.readdir dir |> Array.to_list |> List.sort compare
          |> List.filter (fun f -> Filename.check_suffix f ".tsr")
          |> List.filter_map (fun f ->
                 let path = Filename.concat dir f in
                 let src = In_channel.with_open_bin path In_channel.input_all in
                 match Fmt.format ~file:path src with
                 | out when out = src -> None
                 | _ -> Some path
                 | exception Diag.Fatal _ -> None))
      [ "../examples"; "../bench"; "../src/std" ]
  in
  ok "fmt-repo-clean" (unformatted = []) ("not formatted: " ^ String.concat ", " unformatted);
  ok "fmt-every-program" (!fmt_failures = [])
    (String.concat "\n\n" (List.map (fun (n, m) -> n ^ ": " ^ m) (List.rev !fmt_failures)));
  ok "fmt-checked-count" (Hashtbl.length fmt_checked > 90 || Sys.getenv_opt "TESSERA_TEST_ONLY" <> None) (string_of_int (Hashtbl.length fmt_checked));

  (* ---------- struct embedding ---------- *)
  let module J = Json in
  let embed_base =
    {|struct Person {
    name: Str,
}

fn greet(self: Person) -> Str {
    "hi, {self.name}"
}

fn hello(self: Person, to: Str) -> Str {
    "{self.name} says hello to {to}"
}

// calls greet on a Person: there is no overriding, so an outer struct's
// greet is never picked here
fn intro(self: Person) -> Str {
    "intro: " + self.greet()
}

struct User {
    Person,
    email: Str,
}

struct Admin {
    User,
    level: Int,
}

fn greet(self: Admin) -> Str {
    "admin {self.name} (level {self.level})"
}
|}
  in
  out "embed-promotion"
    (embed_base
     ^ {|
fn main(root: Root) {
    let out = root.stdout
    let u = User { Person: Person { name: "ana" }, email: "a@x" }
    let mut a = Admin { User: u, level: 3 }
    out.print("{a.name} {a.email} {a.level}")
    // the outer method wins; the embedded one is still there by name
    out.print(a.greet())
    out.print(a.User.greet())
    out.print(a.User.Person.greet())
    out.print(a.hello("bo"))
    out.print(a.intro())
    // assignment through a promoted field changes the embedded value
    a.name = "eva"
    a.User.email = "e@x"
    out.print("{a.User.Person.name} {a.email}")
    // a promoted method as a value
    let h = a.hello
    out.print(h("cy"))
    // the embedded value is an ordinary field: a copy
    let mut p = a.Person
    p.name = "zed"
    out.print("{p.name} {a.name}")
}
|})
    "ana a@x 3\nadmin ana (level 3)\nhi, ana\nhi, ana\nana says hello to bo\nintro: hi, ana\neva e@x\neva says hello to cy\nzed eva\n";
  out "embed-interfaces"
    (embed_base
     ^ {|
interface Greeter {
    fn greet(self) -> Str
}

interface Hello {
    fn hello(self, to: Str) -> Str
}

fn welcome[T: Greeter](g: T) -> Str {
    g.greet()
}

fn main(root: Root) {
    let out = root.stdout
    let a = Admin { User: User { Person: Person { name: "ana" }, email: "a@x" }, level: 3 }
    // User has greet only through Person; Admin has its own
    out.print(welcome(a.User))
    out.print(welcome(a))
    let gs: List[dyn Greeter] = [a, a.User, a.Person]
    for g in gs {
        out.print(g.greet())
    }
    let h: dyn Hello = a
    out.print(h.hello("bo"))
}
|})
    "hi, ana\nadmin ana (level 3)\nadmin ana (level 3)\nhi, ana\nhi, ana\nana says hello to bo\n";
  out "embed-depth"
    {|struct Deep {
    n: Int,
    d: Str,
}

fn which(self: Deep) -> Str { "deep" }

struct Mid {
    Deep,
}

struct Near {
    n: Int,
}

fn which(self: Near) -> Str { "near" }

// Near.n (depth 1) wins over Mid.Deep.n (depth 2); d is only at depth 2
struct Top {
    Near,
    Mid,
}

fn main(root: Root) {
    let t = Top { Near: Near { n: 1 }, Mid: Mid { Deep: Deep { n: 2, d: "x" } } }
    root.stdout.print("{t.n} {t.d} {t.Mid.n} {t.which()} {t.Mid.which()}")
}
|}
    "1 x 2 near deep\n";
  out "embed-generic"
    {|interface Getter {
    fn get(self) -> Int
}

struct Box[T] {
    item: T,
}

fn get[T](self: Box[T]) -> T {
    self.item
}

fn apply[T, U](self: Box[T], f: fn(T) -> U) -> U {
    f(self.item)
}

struct Labeled[T] {
    Box[T],
    label: Str,
}

struct Ints {
    Box[Int],
}

fn read(g: dyn Getter) -> Int {
    g.get()
}

fn main(root: Root) {
    let out = root.stdout
    let l = Labeled { Box: Box { item: 41 }, label: "n" }
    let s = Labeled { Box: Box { item: "s" }, label: "m" }
    out.print("{l.get()} {l.item} {l.label} {s.get()} {l.apply(|x| x + 1)} {s.apply(|x| x.len())}")
    out.print("{read(l)} {read(Ints { Box: Box { item: 7 } })}")
}
|}
    "41 41 n s 42 1\n41 7\n";
  let auth =
    "pub struct User {\n    name: Str,\n}\n\npub fn greet(self: User) -> Str {\n    \"hi {self.name}\"\n}\n\nfn secret(self: User) -> Str {\n    \"s\"\n}\n"
  in
  out "embed-modules" ~modules:[ ("auth", auth) ]
    {|use auth

interface Greeter {
    fn greet(self) -> Str
}

struct Admin {
    auth.User,
    level: Int,
}

fn main(root: Root) {
    let a = Admin { User: auth.User { name: "x" }, level: 1 }
    let g: dyn Greeter = a
    root.stdout.print("{a.greet()} {g.greet()} {a.User.name} {a.name}")
}
|}
    "hi x hi x x x\n";
  errs "embed-private-method" ~modules:[ ("auth", auth) ]
    "use auth\n\nstruct Admin {\n    auth.User,\n}\n\nfn main(root: Root) {\n    let a = Admin { User: auth.User { name: \"x\" } }\n    root.stdout.print(a.secret())\n}\n"
    [ "E0402" ];
  let two =
    {|struct A {
    x: Int,
}

fn id(self: A) -> Int { 1 }

struct B {
    x: Int,
}

fn id(self: B) -> Int { 2 }

interface Ided {
    fn id(self) -> Int
}

struct C {
    A,
    B,
}
|}
  in
  (* ambiguity is an error only where the name is used *)
  out "embed-ambiguous-unused" (two ^ "\nfn main(root: Root) {\n    let c = C { A: A { x: 1 }, B: B { x: 2 } }\n    root.stdout.print(\"{c.A.x + c.B.x} {c.A.id()}\")\n}\n") "3 1\n";
  errs "embed-ambiguous-field" (two ^ "\nfn main(root: Root) {\n    let c = C { A: A { x: 1 }, B: B { x: 2 } }\n    root.stdout.print(\"{c.x}\")\n}\n") [ "E0120" ];
  errs "embed-ambiguous-method" (two ^ "\nfn main(root: Root) {\n    let c = C { A: A { x: 1 }, B: B { x: 2 } }\n    root.stdout.print(\"{c.id()}\")\n}\n") [ "E0120" ];
  errs "embed-ambiguous-interface" (two ^ "\nfn main(root: Root) {\n    let c: dyn Ided = C { A: A { x: 1 }, B: B { x: 2 } }\n}\n") [ "E0111" ];
  errs "embed-not-struct" "enum E {\n    P,\n}\n\nstruct S[T] {\n    Int,\n    Option[Int],\n    E,\n    T,\n}\n\nfn main(root: Root) {}\n"
    [ "E0121"; "E0121"; "E0121"; "E0121" ];
  errs "embed-self" "struct S {\n    S,\n}\n\nfn main(root: Root) {}\n" [ "E0114" ];
  errs "embed-twice" "struct A {\n    x: Int,\n}\n\nstruct S {\n    A,\n    A,\n}\n\nfn main(root: Root) {}\n" [ "E0106" ];
  errs "embed-field-named-like-embed" "struct A {\n    x: Int,\n}\n\nstruct S {\n    A,\n    A: Int,\n}\n\nfn main(root: Root) {}\n" [ "E0106" ];
  (* a literal sets the embedded struct as a whole, not its promoted fields *)
  errs "embed-literal-promoted" (embed_base ^ "\nfn main(root: Root) {\n    let a = Admin { name: \"x\", level: 1 }\n}\n") [ "E0206"; "E0103" ];
  ok "embed-literal-promoted-note"
    (let _, ds, _ = compile "embed-literal-note" (embed_base ^ "\nfn main(root: Root) {\n    let a = Admin { name: \"x\", level: 1 }\n}\n") in
     List.exists (fun (d : Diag.t) -> List.exists (fun n -> Interp.find_sub n "User.Person" 0 <> None) d.notes) ds)
    "note names the embedded path";
  (* forwarders for promoted methods are not reported as functions of their own *)
  ok "embed-caps"
    (let _, _, infos = Modules.load_and_check "embed-caps.tsr" (embed_base ^ "\nfn main(root: Root) {}\n") in
     let names = List.map (fun (i : Check.fn_info) -> i.name) infos in
     List.mem "Admin::greet" names && not (List.mem "Admin::hello" names) && not (List.mem "User::greet" names))
    "infos";
  (* a lowercase bare name is still a field missing its type *)
  errs "embed-lowercase" "struct P {\n    x\n}\n\nfn main(root: Root) {}\n" [ "E0003" ];
  ok "embed-fmt"
    (Fmt.format ~file:"emb.tsr" "struct A { x: Int }\nstruct S { A, auth.User,   Box[Int], n: Int }\n"
     = "struct A {\n    x: Int,\n}\n\nstruct S {\n    A,\n    auth.User,\n    Box[Int],\n    n: Int,\n}\n")
    (Fmt.format ~file:"emb.tsr" "struct A { x: Int }\nstruct S { A, auth.User,   Box[Int], n: Int }\n");
  (* the language server sees through promotion *)
  let est = Lsp.create () in
  let euri = "file:///tmp/lsp-embed.tsr" in
  let edoc =
    embed_base
    ^ {|
fn main(root: Root) {
    let a = Admin { User: User { Person: Person { name: "ana" }, email: "a@x" }, level: 3 }
    root.stdout.print(a.hello(a.name))
}
|}
  in
  let epos text needle k =
    let off = Option.get (Interp.find_sub text needle 0) + k in
    let line = ref 0 and col = ref 0 in
    String.iteri (fun i c -> if i < off then if c = '\n' then (incr line; col := 0) else incr col) text;
    (!line, !col)
  in
  let ereq m needle k =
    let l, c = epos edoc needle k in
    let params = J.Obj [ ("textDocument", J.Obj [ ("uri", J.String euri) ]); ("position", J.Obj [ ("line", J.Int l); ("character", J.Int c) ]) ] in
    match Lsp.handle est (J.Obj [ ("jsonrpc", J.String "2.0"); ("id", J.Int 1); ("method", J.String m); ("params", params) ]) with
    | [ r ] -> J.member "result" r
    | _ -> J.Null
  in
  let enote m params = ignore (Lsp.handle est (J.Obj [ ("jsonrpc", J.String "2.0"); ("method", J.String m); ("params", params) ])) in
  enote "textDocument/didOpen" (J.Obj [ ("textDocument", J.Obj [ ("uri", J.String euri); ("text", J.String edoc) ]) ]);
  let edef needle k = match ereq "textDocument/definition" needle k with J.Null -> None | r -> Some (J.to_int (J.path r [ "range"; "start"; "line" ]), J.to_int (J.path r [ "range"; "start"; "character" ])) in
  ok "embed-lsp-def-method" (edef "a.hello(" 3 = Some (epos edoc "hello(self" 0)) "hello";
  ok "embed-lsp-def-field" (edef "a.name)" 3 = Some (epos edoc "name: Str" 0)) "name";
  let ehover needle k = match ereq "textDocument/hover" needle k with J.Null -> "" | r -> J.to_str (J.path r [ "contents"; "value" ]) in
  ok "embed-lsp-hover-method" (Interp.find_sub (ehover "a.hello(" 3) "fn hello(self: Person, to: Str) -> Str" 0 <> None) (ehover "a.hello(" 3);
  let ecomplete text needle k =
    enote "textDocument/didChange"
      (J.Obj [ ("textDocument", J.Obj [ ("uri", J.String euri) ]); ("contentChanges", J.List [ J.Obj [ ("text", J.String text) ] ]) ]);
    let l, c = epos text needle k in
    let params = J.Obj [ ("textDocument", J.Obj [ ("uri", J.String euri) ]); ("position", J.Obj [ ("line", J.Int l); ("character", J.Int c) ]) ] in
    match Lsp.handle est (J.Obj [ ("jsonrpc", J.String "2.0"); ("id", J.Int 2); ("method", J.String "textDocument/completion"); ("params", params) ]) with
    | [ r ] -> ( match J.member "result" r with J.List items -> List.map (fun i -> J.to_str (J.member "label" i)) items | _ -> [])
    | _ -> []
  in
  let ec = ecomplete (Interp.replace_all edoc "    root.stdout.print(a.hello(a.name))" "    root.stdout.print(a.hello(a.name))\n    a.") "    a.\n" 6 in
  ok "embed-lsp-complete"
    (List.for_all (fun x -> List.mem x ec) [ "User"; "level"; "email"; "name"; "greet"; "hello"; "intro" ])
    (String.concat "," ec);

  (* ---------- pure functions: calls kept across loops, repeated calls reused ---------- *)
  (* Both backends must print the same as without the optimizations (the
     interpreter keeps fewer calls); natively each also runs under ASan with
     the leak check. Covered: loops that run zero times around a call that
     would panic or never return, a call under a condition that holds only
     later, an earlier panic in the same iteration winning, arguments that
     change, kept values mutated, nested loops, return and break while a
     kept value is held, a caught panic, built-in methods, tasks, and an
     argument moved into the call. *)
  let pure_src = {v|struct Point {
    x: Int,
    name: Str,
}

fn table(n: Int) -> List[Int] {
    let mut t = []
    for i in 0..<n {
        t.push(i * i)
    }
    t
}

fn label(p: Point) -> Str {
    "{p.name}:{p.x}"
}

fn boom(d: Int) -> List[Int] {
    let q = 10 / d
    [q, q]
}

fn spin(n: Int) -> Int {
    let mut k = n
    while k != 0 {
        k = k + 1
    }
    k
}

fn find(words: List[Str], w: Str) -> Int {
    for i in 0..<10 {
        if table(4)[i % 4] == w.len() {
            return i
        }
    }
    -1
}

fn kept_then_panic(n: Int) -> Int {
    let mut total = 0
    let xs = [1, 2, 3]
    for i in 0..<n {
        total = total + table(5)[i % 5]
        if i == 2 {
            // the index panic comes first, as written
            total = total + xs[i + 5] + boom(0)[0]
        }
    }
    total
}

fn main(root: Root) {
    let out = root.stdout
    // zero iterations: the panicking and the endless call never run
    let mut n = 0
    while n < 0 {
        out.print("{boom(0)} {spin(1)}")
        n = n + 1
    }
    for i in 0..<0 {
        out.print("{boom(i)}")
    }
    // under a condition that holds only later
    let mut total = 0
    for i in 0..<6 {
        if i > 3 {
            total = total + boom(5)[0] + table(3).len()
        }
    }
    out.print("cond {total}")
    // arguments that change, and ones that do not
    let p = Point { x: 3, name: "p" }
    let mut s = ""
    for i in 0..<3 {
        s = s + label(p) + label(Point { x: i, name: "q" }) + " "
    }
    out.print(s)
    // kept results mutated in the loop: each iteration sees a fresh copy
    for i in 0..<3 {
        let mut t = table(3)
        t.push(i)
        out.print("{t}")
    }
    // nested loops: kept across the outer one, or per outer iteration
    let mut sum = 0
    for i in 0..<4 {
        for j in 0..<4 {
            sum = sum + table(4)[j] + table(i + 1).len()
        }
    }
    out.print("nested {sum}")
    // return and break while a kept value is held
    out.print("find {find(["ab"], "abcd")} {find(["a"], "abcde")}")
    let mut k = 0
    while true {
        let t = table(6)
        if k == 3 {
            break
        }
        k = k + t.len() - 5
    }
    out.print("k {k}")
    // a panic after a kept value is held, caught
    out.print("{catch(|| kept_then_panic(10))} {catch(|| kept_then_panic(2))}")
    // built-in methods
    let text = "a,b,c"
    let mut parts = 0
    for i in 0..<4 {
        parts = parts + text.split(",").len() + text.to_upper().len()
    }
    out.print("parts {parts}")
    // a kept value handed to tasks
    let mut acc = 0
    for i in 0..<3 {
        let rs = group g {
            g.go(|| table(3).len() + i)
        }
        acc = acc + rs[0]
    }
    out.print("tasks {acc}")
    // an argument made in the loop and moved into the call
    let mut m = 0
    for i in 0..<3 {
        let name = "p" + "q"
        m = m + label(Point { x: 1, name: name }).len()
    }
    out.print("moved {m}")
}
|v} in
  out "pure-kept-calls" pure_src "cond 10\np:3q:0 p:3q:1 p:3q:2 \n[0, 1, 4, 0]\n[0, 1, 4, 1]\n[0, 1, 4, 2]\nnested 96\nfind 2 -1\nk 3\nErr(\"index 7 is out of range for a list of length 3\") Ok(1)\nparts 32\ntasks 12\nmoved 12\n";
  let cse_src = {v|fn count(xs: List[Int], v: Int) -> Int {
    let mut n = 0
    for x in xs {
        if x == v {
            n = n + 1
        }
    }
    n
}

fn tag(s: Str) -> Str {
    "<{s}>"
}

fn guarded(out: Stdout, n: Int) -> Int {
    defer out.print("guarded done")
    let mut t = 0
    for i in 0..<n {
        t = t + count([1, 1, 2], 1)
    }
    t
}

fn main(root: Root) {
    let out = root.stdout
    let mut xs = [1, 2, 1]
    let a = count(xs, 1)
    let b = count(xs, 1)
    xs.push(1)
    let c = count(xs, 1)
    let mut v = 2
    let d = count(xs, v)
    v = 1
    let e = count(xs, v)
    out.print("{a} {b} {c} {d} {e}")
    let mut s = tag("x")
    let s2 = tag("x")
    s = s + "!"
    out.print("{s} {s2} {tag("x")}")
    if a > 1 {
        out.print("{count(xs, 1)} {count(xs, 1)}")
    }
    let mut w = 0
    while w < 3 {
        w = w + count(xs, 1) - count(xs, 1) + 1
        xs.push(1)
    }
    out.print("w {w} {count(xs, 1)} {guarded(out, 3)}")
}
|v} in
  out "pure-reused-calls" cse_src "2 2 3 1 3\n<x>! <x> <x>\n3 3\nguarded done\nw 3 6 6\n";
  (* natively the optimizations really happen: slots for kept calls, and
     main calls `count` six times, not eight *)
  (let c_of name src =
     let items, ds, _ = compile name src in
     if ds = [] then Native.c_source items else ""
   in
   let count_sub s sub =
     let n = String.length s and m = String.length sub in
     let rec go i acc = if i + m > n then acc else go (i + 1) (if String.sub s i m = sub then acc + 1 else acc) in
     go 0 0
   in
   let kept = count_sub (c_of "pure-kept-c" pure_src) "__builtin_expect(k" in
   ok "pure-kept-c" (kept >= 10) (sprintf "%d kept calls" kept);
   let c = c_of "pure-reused-c" cse_src in
   let main_body =
     let lines = String.split_on_char '\n' c in
     let rec from = function [] -> [] | l :: r -> if String.starts_with ~prefix:"static W_Unit wf_main(" l && String.ends_with ~suffix:"{" l then upto r else from r
     and upto = function [] | "}" :: _ -> [] | l :: r -> l :: upto r in
     String.concat "\n" (from lines)
   in
   let calls = count_sub main_body "wf_count(" in
   ok "pure-reused-c" (calls = 6) (sprintf "%d calls" calls));

  drain ();
  Printf.printf "%d/%d passed\n" (!total - !failed) !total;
  if !failed > 0 then exit 1
