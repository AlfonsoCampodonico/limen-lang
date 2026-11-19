open Limen

let usage =
  {|limen: a small language where I/O needs a capability

usage:
  limen run FILE [--native] [--grant net=HOST[:PORT]|*]... [--grant fs=DIR[:ro]] [--grant listen=HOST:PORT]... [--grant env=NAME|*]... [--grant exec=PROGRAM[ ARGS|ARGS...]|*]... [--grant foreign=LIB[,LIB...]|*]... [--seed N] [--no-sandbox] [-- ARGS...]
  limen build FILE [-o OUT] [--dev] [--emit-c]
  limen check FILE [--json]
  limen caps FILE [--json]
  limen explain CODE
  limen test FILE [--native] [--run TEXT] [--bench] [--bench-time 1s|100x] [--json]
  limen fmt FILE... [--write | --check]
  limen doc FILE|MODULE... [--std] [--all] [--html] [-o OUT]
  limen lsp                (language server on stdin/stdout)
  limen mcp                (MCP server for coding agents, on stdin/stdout)
  limen spec               (print the language specification)
  limen search [WORDS...]  (search the package index named by LIMEN_INDEX)
  limen add NAME[@VERSION] (add a package from the index to limen.toml, with the caps it declares)
  limen index entry [DIR]  (print the index entry for the package's current tag)

Stdout, Stderr, Stdin, Clock and Rand are always granted. Net, Fs, Listen, Env, Exec and Foreign exist only
when granted, by --grant or by the [grants] table of limen.toml. --seed makes Rand repeatable; arguments after -- are root.args(). The process runs in an operating-system sandbox
limited to those grants (macOS; Linux untested); --no-sandbox turns it off, for debugging.
|}

let die_usage msg =
  prerr_string (if msg = "" then usage else "limen: " ^ msg ^ "\n\n" ^ usage);
  exit 2

(* "1 error", "2 warnings" *)
let count n what = Printf.sprintf "%d %s%s" n what (if n = 1 then "" else "s")

(* The warnings [load] printed, which the closing count of [report] includes. *)
let warned = ref 0

(* Warnings go to stderr here, unless [keep_warnings]; the diagnostics
   returned are the errors, which stop the command. With warnings and no
   errors, their count closes them, as [report] closes errors. *)
let load ?(keep_warnings = false) file =
  let src =
    try In_channel.with_open_bin file In_channel.input_all
    with Sys_error m -> prerr_endline ("limen: " ^ m); exit 2
  in
  let items, ds, infos = Modules.load_and_check file src in
  if keep_warnings then (items, ds, infos)
  else begin
    let ws = Diag.warnings ds and errs = Diag.errors ds in
    List.iter (fun d -> prerr_string (Diag.render d); prerr_newline ()) ws;
    warned := List.length ws;
    if errs = [] && ws <> [] then prerr_endline (count !warned "warning");
    (items, errs, infos)
  end

(* The errors, then their count, with the warnings [load] printed: "2 errors,
   1 warning". *)
let report diags =
  List.iter (fun d -> prerr_string (Diag.render d); prerr_newline ()) diags;
  prerr_endline (count (List.length diags) "error" ^ if !warned > 0 then ", " ^ count !warned "warning" else "")

let parse_grants args =
  List.fold_left
    (fun (g : Interp.grants) a ->
      match String.index_opt a '=' with
      | Some i when String.sub a 0 i = "net" ->
        { g with net = g.net @ [ String.sub a (i + 1) (String.length a - i - 1) ] }
      | Some i when String.sub a 0 i = "fs" ->
        let v = String.sub a (i + 1) (String.length a - i - 1) in
        let dir, ro =
          if String.ends_with ~suffix:":ro" v then (String.sub v 0 (String.length v - 3), true) else (v, false)
        in
        let real = try Unix.realpath dir with Unix.Unix_error _ -> die_usage ("fs grant: no such directory " ^ dir) in
        if not (Sys.is_directory real) then die_usage ("fs grant: not a directory " ^ dir);
        { g with fs = Some (real, ro) }
      | Some i when String.sub a 0 i = "listen" ->
        { g with listen = g.listen @ [ String.sub a (i + 1) (String.length a - i - 1) ] }
      | Some i when String.sub a 0 i = "env" ->
        { g with env = g.env @ [ String.sub a (i + 1) (String.length a - i - 1) ] }
      | Some i when String.sub a 0 i = "exec" ->
        let v = String.sub a (i + 1) (String.length a - i - 1) in
        if Caps.exec_entry v = None then die_usage ("exec grant: " ^ Caps.exec_grant_help);
        { g with exec = g.exec @ [ v ] }
      | Some i when String.sub a 0 i = "foreign" ->
        (* one library, or several separated by commas: foreign=z,sqlite3 *)
        let libs = String.split_on_char ',' (String.sub a (i + 1) (String.length a - i - 1)) in
        if List.mem "" libs then die_usage "foreign grant: name C libraries, as in foreign=z or foreign=z,sqlite3";
        { g with foreign = g.foreign @ libs }
      | None when a = "no-sandbox" -> g
      | Some i when String.sub a 0 i = "seed" -> (
        match int_of_string_opt (String.sub a (i + 1) (String.length a - i - 1)) with
        | Some n -> { g with seed = Some n }
        | None -> die_usage "--seed needs an integer")
      | _ -> die_usage ("unknown grant `" ^ a ^ "`"))
    Interp.no_grants args

(* The foreign grant is full access; say so every time it is given. *)
let warn_foreign (g : Interp.grants) =
  if g.foreign <> [] then
    prerr_endline
      (Printf.sprintf "limen: warning: --grant foreign=%s is full access: C code can do anything this process can, past every other grant"
         (String.concat "," g.foreign))

(* The [grants] of the program's limen.toml, as `--grant` values. They come
   first, so flags add to them (and a --grant fs=... replaces the fs one). *)
let toml_grants file =
  match Package.load file with
  | Ok (Some p) -> List.map (fun (k, v) -> k ^ "=" ^ v) p.grants
  | Ok None -> []
  | Error d -> report [ d ]; exit 1

let rec split_grants = function
  | "--grant" :: g :: rest ->
    let gs, others = split_grants rest in
    (g :: gs, others)
  | "--seed" :: n :: rest ->
    let gs, others = split_grants rest in
    (("seed=" ^ n) :: gs, others)
  | "--no-sandbox" :: rest ->
    let gs, others = split_grants rest in
    ("no-sandbox" :: gs, others)
  | "--grant" :: [] -> die_usage "--grant needs a value"
  | x :: rest ->
    let gs, others = split_grants rest in
    (gs, x :: others)
  | [] -> ([], [])

(* limen test: run every `test "name" { ... }` block, each isolated. *)
let where_of (sp : Span.t) =
  if sp.file = "" then ""
  else
    let l, c = Source.line_col sp.file sp.lo in
    Printf.sprintf "%s:%d:%d" sp.file l c

(* Run the native test binary [exe] with [args] (plus an fs grant to a fresh
   directory when [takes_fs]). Returns the exit status, stderr and seconds. *)
let run_test_binary exe args takes_fs =
  let err = exe ^ ".err" in
  let fd = Unix.openfile err [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  (* a test that takes Fs gets a fresh directory, removed afterwards *)
  let dir = if takes_fs then Some (Caps.test_dir ()) else None in
  let grant = match dir with Some d -> [| "--grant"; "fs=" ^ d |] | None -> [||] in
  let start = Unix.gettimeofday () in
  let pid = Unix.create_process exe (Array.concat [ [| exe |]; args; grant ]) Unix.stdin Unix.stdout fd in
  let _, status = Unix.waitpid [] pid in
  let secs = Unix.gettimeofday () -. start in
  Option.iter Caps.remove_tree dir;
  Unix.close fd;
  (status, In_channel.with_open_bin err In_channel.input_all, secs)

(* A failed run's message and location, from the panic it printed. *)
let failure text =
  let lines = String.split_on_char '\n' text in
  let msg =
    match lines with
    | l :: _ when String.starts_with ~prefix:"panic: " l -> String.sub l 7 (String.length l - 7)
    | l :: _ when l <> "" -> l
    | _ -> "crashed"
  in
  let where =
    match lines with
    | _ :: w :: _ when String.starts_with ~prefix:"-->" (String.trim w) ->
      let w = String.trim w in
      String.trim (String.sub w 3 (String.length w - 3))
    | _ -> ""
  in
  (msg, where)

type bench_result = { iters : int; ns : int; allocs : int }

(* Build the test binary once; run the selected tests (and, with [bench],
   the selected benches) each in its own process. [tests] and [benches] are
   (index, name, takes_fs). *)
let run_native_tests items tests benches bench =
  let exe = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "limen-test-%d" (Unix.getpid ())) in
  (* benchmarks measure the optimised program; tests build fast *)
  let flags = if bench = None then Some Native.dev_flags else None in
  Lower.lines := bench = None;
  (match Native.build ?flags ~tests:true items exe with
   | Ok () -> ()
   | Error m -> prerr_string (Native.cc_failed m); exit 1);
  let test_results =
    List.map
      (fun (i, name, takes_fs) ->
        let status, text, secs = run_test_binary exe [| string_of_int i |] takes_fs in
        match status with
        | Unix.WEXITED 0 -> (name, Ok (), secs)
        | _ -> (name, Error (failure text), secs))
      tests
  in
  let bench_results =
    match bench with
    | None -> []
    | Some (budget_ns, fixed) ->
      List.map
        (fun (i, name, takes_fs) ->
          let args = [| "bench"; string_of_int i; string_of_int budget_ns; string_of_int fixed |] in
          let status, text, _ = run_test_binary exe args takes_fs in
          let report =
            List.find_map
              (fun l -> let p = "limen-bench: " in if String.starts_with ~prefix:p l then Some (String.sub l (String.length p) (String.length l - String.length p)) else None)
              (String.split_on_char '\n' text)
          in
          match (status, report) with
          | Unix.WEXITED 0, Some r -> (
            match List.map int_of_string_opt (String.split_on_char ' ' r) with
            | [ Some iters; Some ns; Some allocs ] -> (name, Ok { iters; ns; allocs })
            | _ -> (name, Error ("bad benchmark report: " ^ r, "")))
          | _ -> (name, Error (failure text)))
        benches
  in
  List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [ exe; exe ^ ".c"; exe ^ ".err" ];
  (test_results, bench_results)

(* `--bench-time`: `1s`, `500ms`, `2m`, or `100x` for an exact count.
   Returns (budget in nanoseconds, fixed count or 0). *)
let parse_bench_time s =
  let num k = match float_of_string_opt k with Some f when f > 0. -> f | _ -> die_usage ("bad --bench-time " ^ s) in
  let cut n = String.sub s 0 (String.length s - n) in
  if String.ends_with ~suffix:"x" s then
    match int_of_string_opt (cut 1) with Some n when n > 0 -> (0, n) | _ -> die_usage ("bad --bench-time " ^ s)
  else if String.ends_with ~suffix:"ms" s then (int_of_float (num (cut 2) *. 1e6), 0)
  else if String.ends_with ~suffix:"s" s then (int_of_float (num (cut 1) *. 1e9), 0)
  else if String.ends_with ~suffix:"m" s then (int_of_float (num (cut 1) *. 60e9), 0)
  else die_usage ("--bench-time takes a duration such as 1s or 200ms, or a count such as 100x, not " ^ s)

let fmt_secs s = if s < 1.0 then Printf.sprintf "%.0fms" (s *. 1000.) else Printf.sprintf "%.2fs" s

let ns_per_op b =
  let v = float_of_int b.ns /. float_of_int (max 1 b.iters) in
  if v < 10. then Printf.sprintf "%.2f" v else if v < 100. then Printf.sprintf "%.1f" v else Printf.sprintf "%.0f" v

type test_opts = { native : bool; bench : (int * int) option; pattern : string option; json : bool; coverage : bool }

let cmd_test file (o : test_opts) =
  Package.write_lock := true;
  let items, diags, _ = load file in
  if diags <> [] then begin
    if o.json then print_endline (Diag.list_to_json diags) else report diags;
    exit 1
  end;
  let selected n = match o.pattern with None -> true | Some p -> Interp_util.find_sub n p 0 <> None in
  (* tests and benches are numbered in order, as the test binary numbers them *)
  let numbered bench =
    List.mapi (fun i (n, d) -> (i, n, Caps.takes_fs d))
      (List.filter_map (function Ast.IFn ({ fn_test = Some n; fn_bench = b; _ } as d) when b = bench -> Some (n, d) | _ -> None) items)
    |> List.filter (fun (_, n, _) -> selected n)
  in
  let tests = numbered false and benches = if o.bench <> None then numbered true else [] in
  if tests = [] && benches = [] then begin
    if o.json then print_endline {|{"tests":[],"benches":[],"passed":0,"failed":0}|}
    else print_endline (if o.pattern = None then "no tests" else "no tests match");
    exit 0
  end;
  let start = Unix.gettimeofday () in
  (* a test's own output goes to stdout; with --json, it would break the JSON *)
  let out s = if not o.json then (print_string s; flush stdout) in
  if o.coverage && (o.native || o.bench <> None) then die_usage "--coverage runs the tests interpreted: leave out --native and --bench";
  if o.coverage then Interp.coverage := Some (Hashtbl.create 1024);
  let test_results, bench_results =
    if o.native || o.bench <> None then run_native_tests items tests benches o.bench
    else
      ( List.map
          (fun (n, r, t) -> (n, (match r with Ok () -> Ok () | Error (m, sp) -> Error (m, where_of sp)), t))
          (Interp.run_tests_timed ~select:selected ~out items),
        [] )
  in
  let total = Unix.gettimeofday () -. start in
  let failed = List.length (List.filter (fun (_, r, _) -> Result.is_error r) test_results)
               + List.length (List.filter (fun (_, r) -> Result.is_error r) bench_results) in
  let passed = List.length test_results + List.length bench_results - failed in
  if o.json then begin
    let open Json in
    let micro t = Float.round (t *. 1e6) /. 1e6 in
    let fail_fields (m, w) = [ ("ok", Bool false); ("message", String m) ] @ if w = "" then [] else [ ("where", String w) ] in
    let tests =
      List.map
        (fun (n, r, t) ->
          Obj ([ ("name", String n) ] @ (match r with Ok () -> [ ("ok", Bool true) ] | Error e -> fail_fields e) @ [ ("seconds", Float (micro t)) ]))
        test_results
    in
    let benches =
      List.map
        (fun (n, r) ->
          Obj
            (( "name", String n )
            :: (match r with
               | Ok b ->
                 [ ("ok", Bool true); ("iterations", Int b.iters); ("ns_per_op", Float (float_of_int b.ns /. float_of_int (max 1 b.iters)));
                   ("allocs_per_op", Int (b.allocs / max 1 b.iters)) ]
               | Error e -> fail_fields e)))
        bench_results
    in
    print_endline
      (to_string
         (Obj
            ([ ("tests", List tests); ("benches", List benches); ("passed", Int passed); ("failed", Int failed); ("seconds", Float (micro total)) ]
            @
            match !Interp.coverage with
            | None -> []
            | Some ran ->
              [ ( "coverage",
                  List
                    (List.map
                       (fun (f : Coverage.file) ->
                         Obj [ ("file", String f.path); ("lines", Int f.lines); ("ran", Int f.run); ("missed", List (List.map (fun l -> Int l) f.missed)) ])
                       (Coverage.report items ran)) ) ])))
  end
  else begin
    let fail name (m, w) =
      Printf.printf "FAIL  %s\n      %s\n" name m;
      if w <> "" then Printf.printf "      --> %s\n" w
    in
    List.iter
      (fun (name, r, t) -> match r with Ok () -> Printf.printf "ok    %s (%s)\n" name (fmt_secs t) | Error e -> fail name e)
      test_results;
    let width = List.fold_left (fun w (n, _) -> max w (String.length n)) 0 bench_results in
    List.iter
      (fun (name, r) ->
        match r with
        | Ok b ->
          Printf.printf "bench %-*s %12d %12s ns/op %8d allocs/op\n" width name b.iters (ns_per_op b) (b.allocs / max 1 b.iters)
        | Error e -> fail name e)
      bench_results;
    let count n what = Printf.sprintf "%d %s%s" n what (if n = 1 then "" else if what = "bench" then "es" else "s") in
    Printf.printf "%s%s, %d failed (%s)\n"
      (count (List.length test_results) "test")
      (if bench_results = [] then "" else ", " ^ count (List.length bench_results) "bench")
      failed (fmt_secs total);
    match !Interp.coverage with Some ran -> print_string (Coverage.to_text (Coverage.report items ran)) | None -> ()
  end;
  exit (if failed = 0 then 0 else 1)

let cmd_fmt files mode =
  let changed = ref [] in
  List.iter
    (fun file ->
      let src = try In_channel.with_open_bin file In_channel.input_all with Sys_error m -> prerr_endline ("limen: " ^ m); exit 2 in
      match Fmt.format ~file src with
      | exception Diag.Fatal d -> prerr_string (Diag.render d); exit 1
      | out -> (
        match mode with
        | `Print -> print_string out
        | `Write -> if out <> src then Out_channel.with_open_bin file (fun oc -> output_string oc out)
        | `Check -> if out <> src then changed := file :: !changed))
    files;
  if mode = `Check && !changed <> [] then begin
    List.iter (fun f -> prerr_endline ("not formatted: " ^ f)) (List.rev !changed);
    exit 1
  end

(* limen doc: Markdown (or with --html, one static page) from doc comments. *)
let cmd_doc args =
  let rec go srcs std all html out = function
    | "--std" :: r -> go srcs true all html out r
    | "--all" :: r -> go srcs std true html out r
    | "--html" :: r -> go srcs std all true out r
    | "-o" :: o :: r -> go srcs std all html (Some o) r
    | "-o" :: [] -> die_usage "-o needs a file"
    | a :: _ when String.length a > 1 && a.[0] = '-' -> die_usage ("unknown doc option " ^ a)
    | a :: r -> go (srcs @ [ a ]) std all html out r
    | [] -> (srcs, std, all, html, out)
  in
  let srcs, std, all, html, out = go [] false false false None args in
  let srcs = srcs @ if std then Doc.std_names () else [] in
  if srcs = [] then die_usage "doc needs a FILE, a standard library module name, or --std";
  let mods =
    List.map
      (fun a ->
        try Doc.load ~all a with
        | Sys_error m -> prerr_endline ("limen: " ^ m); exit 2
        | Diag.Fatal d -> prerr_string (Diag.render d); exit 1)
      srcs
  in
  let text = if html then Doc.html mods else Doc.markdown mods in
  match out with
  | Some o -> Out_channel.with_open_bin o (fun oc -> output_string oc text)
  | None -> print_string text

(* Every part of the program that calls C, so it can be reviewed: C code is
   outside every capability check. One line each. *)
let c_callers (items : Ast.item list) =
  let groups =
    List.fold_left
      (fun acc it ->
        match it with
        | Ast.IFn { fn_name; fn_foreign = Some fo; _ } ->
          let m = match String.index_opt fn_name '.' with Some k -> String.sub fn_name 0 k | None -> "" in
          let libs, trusted = Option.value ~default:([], fo.ftrusted) (List.assoc_opt m acc) in
          (m, ((if List.mem fo.flib libs || fo.flib = "limen" then libs else libs @ [ fo.flib ]), trusted)) :: List.remove_assoc m acc
        | _ -> acc)
      [] items
  in
  List.map
    (fun (m, (libs, trusted)) ->
      let who =
        if m = "" then "the program itself (needs --grant foreign)"
        else if trusted then Printf.sprintf "module `%s` (standard library, trusted by src/std/limen.toml)" m
        else
          match List.assoc_opt m !Modules.module_pkg with
          | Some dep -> Printf.sprintf "module `%s` (dependency `%s`, trusted in limen.toml [trust])" m dep
          | None -> Printf.sprintf "module `%s` (the program's own, needs --grant foreign)" m
      in
      Printf.sprintf "calls C: %s: %s" who (String.concat ", " libs))
    (List.sort compare groups)

let cmd_check file json =
  let items, diags, _ = load ~keep_warnings:json file in
  if json then print_endline (Diag.list_to_json diags)
  else if diags = [] then (print_endline "ok"; List.iter print_endline (c_callers items))
  else report diags;
  exit (if Diag.errors diags = [] then 0 else 1)

let cmd_caps file json =
  let _, diags, infos = load file in
  if diags <> [] then (report diags; exit 1);
  let caps_str (i : Check.fn_info) = List.map Types.cap_name i.caps in
  (* a capability held through a struct or enum says where: `Net (via Client.net)` *)
  let caps_via (i : Check.fn_info) = List.map (Check.cap_text i) i.caps in
  if json then begin
    let one (i : Check.fn_info) =
      Printf.sprintf {|{"name":%s,"caps":[%s],"via":{%s},"callbacks":%b,"shares":%b,"pure":%b,"pure_if":[%s],"libs":[%s]}|} (Diag.jstr i.name)
        (String.concat "," (List.map Diag.jstr (caps_str i)))
        (String.concat "," (List.map (fun (c, p) -> Diag.jstr (Types.cap_name c) ^ ":" ^ Diag.jstr p) i.via))
        i.callbacks i.shares
        (i.caps = [] && not i.callbacks && not i.shares)
        (String.concat "," (List.map Diag.jstr i.pure_if))
        (String.concat "," (List.map Diag.jstr (if List.mem Types.Foreign i.caps then i.libs else [])))
    in
    let emb (m, path, size) = Printf.sprintf {|{"module":%s,"path":%s,"bytes":%d}|} (Diag.jstr m) (Diag.jstr path) size in
    print_endline
      ("{\"functions\":[" ^ String.concat "," (List.map one infos) ^ "],\"embed_roots\":["
      ^ String.concat "," (List.map (fun (r, _) -> Diag.jstr r) !Modules.embed_roots)
      ^ "],\"embeds\":["
      ^ String.concat "," (List.map emb (List.rev !Modules.embedded))
      ^ "],\"grants\":["
      ^ String.concat "," (List.map Diag.jstr (toml_grants file)) ^ "]}")
  end
  else begin
    let width = List.fold_left (fun w (i : Check.fn_info) -> max w (String.length i.name)) 0 infos in
    List.iter
      (fun (i : Check.fn_info) ->
        let what =
          match (caps_via i, i.callbacks) with
          | [], _ -> Check.pure_text i
          | cs, cb -> String.concat ", " cs ^ if cb then " + callbacks" else ""
        in
        Printf.printf "%-*s  %s\n" width i.name what)
      infos;
    (* files read at compile time: changing one changes the program; the
       directories outside the package they may come from first *)
    List.iter (fun (r, _) -> Printf.printf "embed root %s\n" r) !Modules.embed_roots;
    List.iter
      (fun (m, path, size) -> Printf.printf "embeds %s (%d bytes)%s\n" path size (if m = "" then "" else " in module " ^ m))
      (List.rev !Modules.embedded);
    (* what limen.toml grants the program, narrowing included: `grant exec=git status|log` *)
    List.iter (fun g -> Printf.printf "grant %s\n" g) (toml_grants file)
  end

let checked file =
  let items, diags, _ = load file in
  if diags <> [] then (report diags; exit 1);
  if not (List.exists (function Ast.IFn d -> d.fn_name = "main" | _ -> false) items) then begin
    prerr_endline "error: no `main` function";
    exit 1
  end;
  items

let cmd_build file out dev emit_c =
  Package.write_lock := true;
  let items = checked file in
  let out = match out with Some o -> o | None -> Filename.remove_extension (Filename.basename file) in
  (* a development build marks Limen lines in its C *)
  Lower.lines := dev;
  if emit_c then Out_channel.with_open_bin (out ^ ".c") (fun oc -> output_string oc (Native.c_source items))
  else
    match Native.build ?flags:(if dev then Some Native.dev_flags else None) items out with
    | Ok () -> (try Sys.remove (out ^ ".c") with Sys_error _ -> ())
    | Error m ->
      prerr_string (Native.cc_failed m);
      exit 1

let cmd_run_native file grant_args prog_args =
  Package.write_lock := true;
  let grant_args = toml_grants file @ grant_args in
  warn_foreign (parse_grants grant_args);
  let items = checked file in
  let dir = Filename.get_temp_dir_name () in
  let exe = Filename.concat dir (Printf.sprintf "limen-%d" (Unix.getpid ())) in
  Lower.lines := true;
  (match Native.build ~flags:Native.dev_flags items exe with
   | Ok () -> ()
   | Error m -> prerr_string (Native.cc_failed m); exit 1);
  let args =
    List.concat_map
      (fun g ->
        if g = "no-sandbox" then [ "--no-sandbox" ]
        else if String.starts_with ~prefix:"seed=" g then [ "--seed"; String.sub g 5 (String.length g - 5) ]
        else [ "--grant"; g ])
      grant_args
    @ ("--" :: prog_args)
  in
  let pid = Unix.create_process exe (Array.of_list (exe :: args)) Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] pid in
  List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [ exe; exe ^ ".c" ];
  exit (match status with Unix.WEXITED n -> n | _ -> 1)

let cmd_run file grant_args prog_args =
  Package.write_lock := true;
  let grant_args = toml_grants file @ grant_args in
  let grants = { (parse_grants grant_args) with args = prog_args } in
  warn_foreign grants;
  let items, diags, _ = load file in
  if diags <> [] then (report diags; exit 1);
  let out s = print_string s; flush stdout in
  (* the operating-system sandbox, once foreign libraries are loaded *)
  let before_main () =
    if not (List.mem "no-sandbox" grant_args) then
      match Interp.sandbox grants with
      | Ok _ -> ()
      | Error m -> Printf.eprintf "limen: %s (run with --no-sandbox to go without it)\n" m; exit 1
  in
  match Interp.run ~before_main ~out ~grants items with
  | Ok (Interp.VEnum ("Result", "Err", [ e ])) ->
    Printf.eprintf "error: %s\n" (Interp.show e);
    exit 1
  | Ok (Interp.VInt n | Interp.VEnum ("Result", "Ok", [ Interp.VInt n ])) -> exit (Interp.exit_code n)
  | Ok _ -> exit 0
  | Error (m, sp) ->
    Printf.eprintf "panic: %s\n" m;
    if sp.Span.file <> "" then begin
      let l, c = Source.line_col sp.file sp.lo in
      Printf.eprintf "  --> %s:%d:%d\n" sp.file l c
    end;
    exit 1

(* The package index commands fail with one plain sentence. *)
let registry f =
  try f () with Registry.Failed m -> prerr_endline ("limen: " ^ m); exit 1

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [ "check"; file ] -> cmd_check file false
  | [ "check"; file; "--json" ] | [ "check"; "--json"; file ] -> cmd_check file true
  | [ "caps"; file ] -> cmd_caps file false
  | [ "caps"; file; "--json" ] | [ "caps"; "--json"; file ] -> cmd_caps file true
  | [ "lsp" ] -> Lsp.serve ()
  | [ "mcp" ] -> Mcp.serve ()
  | [ "spec" ] -> print_string Spec_text.text
  | "search" :: words -> registry (fun () -> Registry.search words)
  | [ "add"; spec ] -> registry (fun () -> Registry.add spec)
  | "add" :: _ -> die_usage "add takes one NAME or NAME@VERSION"
  | [ "index"; "entry" ] -> registry (fun () -> Registry.entry (Sys.getcwd ()))
  | [ "index"; "entry"; dir ] -> registry (fun () -> Registry.entry dir)
  | "index" :: _ -> die_usage "index takes `entry`, and optionally the package's directory"
  | "test" :: rest ->
    let rec go file (o : test_opts) = function
      | "--native" :: r -> go file { o with native = true } r
      | "--bench" :: r -> go file { o with bench = Some (Option.value o.bench ~default:(1_000_000_000, 0)) } r
      | "--bench-time" :: t :: r -> go file { o with bench = Some (parse_bench_time t) } r
      | "--run" :: p :: r -> go file { o with pattern = Some p } r
      | "--json" :: r -> go file { o with json = true } r
      | "--coverage" :: r -> go file { o with coverage = true } r
      | ("--bench-time" | "--run") :: [] -> die_usage "--bench-time and --run need a value"
      | a :: _ when String.length a > 1 && a.[0] = '-' -> die_usage ("unknown test option " ^ a)
      | f :: r when file = None -> go (Some f) o r
      | _ :: _ -> die_usage "test takes one FILE"
      | [] -> ( match file with Some f -> cmd_test f o | None -> die_usage "test needs a FILE")
    in
    go None { native = false; bench = None; pattern = None; json = false; coverage = false } rest
  | "doc" :: rest -> cmd_doc rest
  | "fmt" :: rest -> (
    let mode = if List.mem "--write" rest then `Write else if List.mem "--check" rest then `Check else `Print in
    match List.filter (fun a -> a <> "--write" && a <> "--check") rest with
    | [] -> die_usage "fmt needs at least one FILE"
    | files -> cmd_fmt files mode)
  | [ "explain"; code ] -> (
    match Diag.explain (String.uppercase_ascii code) with
    | Some t -> print_endline (String.uppercase_ascii code ^ ": " ^ t)
    | None -> die_usage ("unknown error code " ^ code))
  | "run" :: rest -> (
    (* arguments after `--` go to the program, as `root.args()` *)
    let rec cut acc = function "--" :: r -> (List.rev acc, r) | x :: r -> cut (x :: acc) r | [] -> (List.rev acc, []) in
    let rest, prog_args = cut [] rest in
    match split_grants rest with
    | gs, [ file ] -> cmd_run file gs prog_args
    | gs, ([ "--native"; file ] | [ file; "--native" ]) -> cmd_run_native file gs prog_args
    | gs, [ a; b; c ] when List.sort compare [ a; b; c ] |> List.filter (fun x -> x = "--native" || x = "--profile") |> List.length = 2 ->
      Cgen.profile := true;
      cmd_run_native (List.find (fun x -> x <> "--native" && x <> "--profile") [ a; b; c ]) gs prog_args
    | _ -> die_usage "run needs exactly one FILE")
  | "build" :: rest ->
    let rec go file out dev emit = function
      | "-o" :: o :: r -> go file (Some o) dev emit r
      | "--dev" :: r -> go file out true emit r
      | "--profile" :: r -> Cgen.profile := true; go file out dev emit r
      | "--emit-c" :: r -> go file out dev true r
      | f :: r when file = None -> go (Some f) out dev emit r
      | [] -> ( match file with Some f -> cmd_build f out dev emit | None -> die_usage "build needs a FILE")
      | _ -> die_usage "bad build arguments"
    in
    go None None false false rest
  | [] | [ ("-h" | "--help" | "help") ] -> print_string usage
  | _ -> die_usage ""
