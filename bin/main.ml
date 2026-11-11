open Vela

let usage =
  {|vela: a small language where I/O needs a capability

usage:
  vela run FILE [--native] [--grant net=HOST[:PORT]|*]... [--grant fs=DIR[:ro]] [--grant listen=HOST:PORT]... [--grant env=NAME|*]... [--grant exec=PROGRAM|*]... [--grant foreign=LIB|*]... [--seed N] [-- ARGS...]
  vela build FILE [-o OUT] [--emit-c]
  vela check FILE [--json]
  vela caps FILE [--json]
  vela explain CODE
  vela test FILE [--native]
  vela fmt FILE... [--write | --check]
  vela lsp                (language server on stdin/stdout)
  vela mcp                (MCP server for coding agents, on stdin/stdout)
  vela spec               (print the language specification)

Stdout, Stdin, Clock and Rand are always granted. Net, Fs, Listen, Env, Exec and Foreign exist only
when granted, by --grant or by the [grants] table of vela.toml. --seed makes Rand repeatable; arguments after -- are root.args().
|}

let die_usage msg =
  prerr_string (if msg = "" then usage else "vela: " ^ msg ^ "\n\n" ^ usage);
  exit 2

let load file =
  let src =
    try In_channel.with_open_bin file In_channel.input_all
    with Sys_error m -> prerr_endline ("vela: " ^ m); exit 2
  in
  Modules.load_and_check file src

let report diags =
  List.iter (fun d -> prerr_string (Diag.render d); prerr_newline ()) diags;
  let n = List.length diags in
  Printf.eprintf "%d error%s\n" n (if n = 1 then "" else "s")

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
        { g with exec = g.exec @ [ String.sub a (i + 1) (String.length a - i - 1) ] }
      | Some i when String.sub a 0 i = "foreign" ->
        { g with foreign = g.foreign @ [ String.sub a (i + 1) (String.length a - i - 1) ] }
      | Some i when String.sub a 0 i = "seed" -> (
        match int_of_string_opt (String.sub a (i + 1) (String.length a - i - 1)) with
        | Some n -> { g with seed = Some n }
        | None -> die_usage "--seed needs an integer")
      | _ -> die_usage ("unknown grant `" ^ a ^ "`"))
    Interp.no_grants args

(* The [grants] of the program's vela.toml, as `--grant` values. They come
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
  | "--grant" :: [] -> die_usage "--grant needs a value"
  | x :: rest ->
    let gs, others = split_grants rest in
    (gs, x :: others)
  | [] -> ([], [])

(* vela test: run every `test "name" { ... }` block, each isolated. *)
let where_of (sp : Span.t) =
  if sp.file = "" then ""
  else
    let l, c = Source.line_col sp.file sp.lo in
    Printf.sprintf "%s:%d:%d" sp.file l c

let run_tests_native items names =
  let exe = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "vela-test-%d" (Unix.getpid ())) in
  (match Native.build ~tests:true items exe with
   | Ok () -> ()
   | Error m -> prerr_string m; prerr_endline "vela: the C compiler failed (this is a bug in vela)"; exit 1);
  let results =
    List.mapi
      (fun i (name, takes_fs) ->
        let err = exe ^ ".err" in
        let fd = Unix.openfile err [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
        (* a test that takes Fs gets a fresh directory, removed afterwards *)
        let dir = if takes_fs then Some (Caps.test_dir ()) else None in
        let grant = match dir with Some d -> [| "--grant"; "fs=" ^ d |] | None -> [||] in
        let pid = Unix.create_process exe (Array.append [| exe; string_of_int i |] grant) Unix.stdin Unix.stdout fd in
        let _, status = Unix.waitpid [] pid in
        Option.iter Caps.remove_tree dir;
        Unix.close fd;
        let text = In_channel.with_open_bin err In_channel.input_all in
        match status with
        | Unix.WEXITED 0 -> (name, Ok ())
        | _ ->
          let lines = String.split_on_char '\n' text in
          let msg =
            match lines with
            | l :: _ when String.starts_with ~prefix:"panic: " l -> String.sub l 7 (String.length l - 7)
            | l :: _ when l <> "" -> l
            | _ -> "crashed"
          in
          let where =
            match lines with
            | _ :: w :: _ when String.starts_with ~prefix:"-->" (String.trim w) -> String.trim (String.sub (String.trim w) 3 (String.length (String.trim w) - 3))
            | _ -> ""
          in
          (name, Error (msg, where)))
      names
  in
  List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [ exe; exe ^ ".c"; exe ^ ".err" ];
  results

let cmd_test file native =
  let items, diags, _ = load file in
  if diags <> [] then (report diags; exit 1);
  let tests = List.filter_map (function Ast.IFn ({ fn_test = Some n; _ } as d) -> Some (n, Caps.takes_fs d) | _ -> None) items in
  if tests = [] then (print_endline "no tests"; exit 0);
  let results =
    if native then run_tests_native items tests
    else
      List.map
        (fun (n, r) -> (n, match r with Ok () -> Ok () | Error (m, sp) -> Error (m, where_of sp)))
        (Interp.run_tests ~out:(fun s -> print_string s; flush stdout) items)
  in
  let failed = ref 0 in
  List.iter
    (fun (name, r) ->
      match r with
      | Ok () -> Printf.printf "ok    %s\n" name
      | Error (m, w) ->
        incr failed;
        Printf.printf "FAIL  %s\n      %s\n" name m;
        if w <> "" then Printf.printf "      --> %s\n" w)
    results;
  Printf.printf "%d test%s, %d failed\n" (List.length results) (if List.length results = 1 then "" else "s") !failed;
  exit (if !failed = 0 then 0 else 1)

let cmd_fmt files mode =
  let changed = ref [] in
  List.iter
    (fun file ->
      let src = try In_channel.with_open_bin file In_channel.input_all with Sys_error m -> prerr_endline ("vela: " ^ m); exit 2 in
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

let cmd_check file json =
  let _, diags, _ = load file in
  if json then print_endline (Diag.list_to_json diags)
  else if diags = [] then print_endline "ok"
  else report diags;
  exit (if diags = [] then 0 else 1)

let cmd_caps file json =
  let _, diags, infos = load file in
  if diags <> [] then (report diags; exit 1);
  let caps_str (i : Check.fn_info) = List.map Types.cap_name i.caps in
  if json then begin
    let one (i : Check.fn_info) =
      Printf.sprintf {|{"name":%s,"caps":[%s],"callbacks":%b,"pure":%b}|} (Diag.jstr i.name)
        (String.concat "," (List.map Diag.jstr (caps_str i)))
        i.callbacks
        (i.caps = [] && not i.callbacks)
    in
    print_endline ("{\"functions\":[" ^ String.concat "," (List.map one infos) ^ "]}")
  end
  else begin
    let width = List.fold_left (fun w (i : Check.fn_info) -> max w (String.length i.name)) 0 infos in
    List.iter
      (fun (i : Check.fn_info) ->
        let what =
          match (caps_str i, i.callbacks) with
          | [], false -> "pure"
          | [], true -> "pure itself, runs callbacks"
          | cs, cb -> String.concat ", " cs ^ if cb then " + callbacks" else ""
        in
        Printf.printf "%-*s  %s\n" width i.name what)
      infos
  end

let checked file =
  let items, diags, _ = load file in
  if diags <> [] then (report diags; exit 1);
  if not (List.exists (function Ast.IFn d -> d.fn_name = "main" | _ -> false) items) then begin
    prerr_endline "error: no `main` function";
    exit 1
  end;
  items

let cmd_build file out emit_c =
  let items = checked file in
  let out = match out with Some o -> o | None -> Filename.remove_extension (Filename.basename file) in
  if emit_c then Out_channel.with_open_bin (out ^ ".c") (fun oc -> output_string oc (Native.c_source items))
  else
    match Native.build items out with
    | Ok () -> (try Sys.remove (out ^ ".c") with Sys_error _ -> ())
    | Error m ->
      prerr_string m;
      prerr_endline "vela: the C compiler failed (this is a bug in vela)";
      exit 1

let cmd_run_native file grant_args prog_args =
  let grant_args = toml_grants file @ grant_args in
  ignore (parse_grants grant_args);
  let items = checked file in
  let dir = Filename.get_temp_dir_name () in
  let exe = Filename.concat dir (Printf.sprintf "vela-%d" (Unix.getpid ())) in
  (match Native.build items exe with
   | Ok () -> ()
   | Error m -> prerr_string m; prerr_endline "vela: the C compiler failed (this is a bug in vela)"; exit 1);
  let args =
    List.concat_map
      (fun g -> if String.starts_with ~prefix:"seed=" g then [ "--seed"; String.sub g 5 (String.length g - 5) ] else [ "--grant"; g ])
      grant_args
    @ ("--" :: prog_args)
  in
  let pid = Unix.create_process exe (Array.of_list (exe :: args)) Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] pid in
  List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [ exe; exe ^ ".c" ];
  exit (match status with Unix.WEXITED n -> n | _ -> 1)

let cmd_run file grant_args prog_args =
  let grant_args = toml_grants file @ grant_args in
  let grants = { (parse_grants grant_args) with args = prog_args } in
  let items, diags, _ = load file in
  if diags <> [] then (report diags; exit 1);
  let out s = print_string s; flush stdout in
  match Interp.run ~out ~grants items with
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

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [ "check"; file ] -> cmd_check file false
  | [ "check"; file; "--json" ] | [ "check"; "--json"; file ] -> cmd_check file true
  | [ "caps"; file ] -> cmd_caps file false
  | [ "caps"; file; "--json" ] | [ "caps"; "--json"; file ] -> cmd_caps file true
  | [ "lsp" ] -> Lsp.serve ()
  | [ "mcp" ] -> Mcp.serve ()
  | [ "spec" ] -> print_string Spec_text.text
  | [ "test"; file ] -> cmd_test file false
  | [ "test"; file; "--native" ] | [ "test"; "--native"; file ] -> cmd_test file true
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
    | _ -> die_usage "run needs exactly one FILE")
  | "build" :: rest ->
    let rec go file out emit = function
      | "-o" :: o :: r -> go file (Some o) emit r
      | "--emit-c" :: r -> go file out true r
      | f :: r when file = None -> go (Some f) out emit r
      | [] -> ( match file with Some f -> cmd_build f out emit | None -> die_usage "build needs a FILE")
      | _ -> die_usage "bad build arguments"
    in
    go None None false rest
  | [] | [ ("-h" | "--help" | "help") ] -> print_string usage
  | _ -> die_usage ""
