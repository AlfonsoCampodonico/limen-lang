(* vela mcp: a Model Context Protocol server on stdin/stdout (one JSON-RPC
   message per line). Gives a coding agent the compiler's tools and the
   spec. Programs run with no grants: only Stdout, Clock and Rand are useful (stdin is empty), so code an
   agent just wrote cannot touch files or the network. *)
module J = Json

let text_result ?(is_error = false) s =
  J.Obj [ ("content", J.List [ J.Obj [ ("type", J.String "text"); ("text", J.String s) ] ]); ("isError", J.Bool is_error) ]

let str_prop desc = J.Obj [ ("type", J.String "string"); ("description", J.String desc) ]
let int_prop desc = J.Obj [ ("type", J.String "integer"); ("description", J.String desc) ]
let bool_prop desc = J.Obj [ ("type", J.String "boolean"); ("description", J.String desc) ]

let tool name desc props required =
  J.Obj
    [ ("name", J.String name); ("description", J.String desc);
      ("inputSchema",
       J.Obj [ ("type", J.String "object"); ("properties", J.Obj props); ("required", J.List (List.map (fun r -> J.String r) required)) ]) ]

let source_p = ("source", str_prop "Complete Vela program text")

let tools =
  [ tool "vela_spec" "The Vela language specification (SPEC.md). Read this before writing Vela." [] [];
    tool "vela_check" "Type-check a Vela program. Returns \"ok\" or diagnostics as JSON, with error codes, positions, notes and machine-applicable fixes." [ source_p ] [ "source" ];
    tool "vela_run"
      "Run a Vela program and return its exit status, stdout and stderr. The program gets no grants: only root.stdout, root.clock and root.rand work, and stdin is empty; file and network access are denied. Stops after timeout_seconds (default 10)."
      [ source_p; ("native", bool_prop "Compile to native code instead of interpreting (default false)");
        ("timeout_seconds", int_prop "Time limit, 1 to 60") ]
      [ "source" ];
    tool "vela_test"
      "Run the `test \"name\" { ... }` blocks in a Vela program and report each result. A test that declares `(fs: Fs)` gets a fresh, empty temporary directory. A table test `test \"add\" for (a, b, want) in [(1, 2, 3)] { ... }` runs once per case, named by the case. With bench, also runs the `bench \"name\" { ... }` blocks natively and reports ns/op and allocs/op."
      [ source_p; ("native", bool_prop "Run natively (default false)"); ("run", str_prop "Only tests and benches whose name contains this text");
        ("bench", bool_prop "Also run the benchmarks, natively, for 200ms each (default false)");
        ("json", bool_prop "Report as JSON: {tests: [{name, ok, message?, where?, seconds}], benches: [{name, ok, iterations, ns_per_op, allocs_per_op}], passed, failed, seconds}");
        ("timeout_seconds", int_prop "Time limit, 1 to 60") ]
      [ "source" ];
    tool "vela_format" "Format a Vela program in the canonical style." [ source_p ] [ "source" ];
    tool "vela_caps" "List each function and the capabilities in its signature (its authority), or `pure`." [ source_p ] [ "source" ];
    tool "vela_type_at" "The type (or signature) at a position in a Vela program. line and character are 0-based."
      [ source_p; ("line", int_prop "0-based line"); ("character", int_prop "0-based column") ]
      [ "source"; "line"; "character" ];
    tool "vela_references"
      "Every place the name at a position is defined or used: functions, methods, types, fields, variants and locals. Returns JSON {name, references: [{file, line, character, end_character, definition}]}, 0-based; a file other than input.vela is a standard library module."
      [ source_p; ("line", int_prop "0-based line"); ("character", int_prop "0-based column") ]
      [ "source"; "line"; "character" ];
    tool "vela_rename"
      "Rename the name at a position everywhere it is used and return the whole new program. Refused, with the reason, when unsafe: built-in or standard library names, interface methods, a name of the wrong case, or a new name that would clash with or capture another one."
      [ source_p; ("line", int_prop "0-based line"); ("character", int_prop "0-based column"); ("new_name", str_prop "The new name") ]
      [ "source"; "line"; "character"; "new_name" ];
    tool "vela_explain" "Explain a Vela error code such as E0302." [ ("code", str_prop "Error code") ] [ "code" ] ]

(* ---------- helpers ---------- *)

let file = "input.vela"

let analyze src =
  Modules.overlay := [];
  match Modules.load file src with
  | [], (_ :: _ as ds) -> (None, ds, [])
  | items, _ ->
    let ds, infos = Check.check_program items in
    (Some items, ds, infos)

let truncate s = if String.length s > 65536 then String.sub s 0 65536 ^ "\n[output truncated]" else s

(* Run `vela ARGS` on the source in a fresh directory, with a time limit. *)
let run_cli args src timeout =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "vela-mcp-%d-%d" (Unix.getpid ()) (Random.bits ())) in
  Unix.mkdir dir 0o700;
  let path = Filename.concat dir "main.vela" in
  Out_channel.with_open_bin path (fun oc -> output_string oc src);
  let out = Filename.concat dir "out" and err = Filename.concat dir "err" in
  let fo = Unix.openfile out [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let fe = Unix.openfile err [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let exe = Sys.executable_name in
  let argv = Array.of_list (exe :: args @ [ path ]) in
  let pid = Unix.create_process exe argv devnull fo fe in
  Unix.close fo; Unix.close fe; Unix.close devnull;
  let deadline = Unix.gettimeofday () +. float_of_int timeout in
  let rec wait () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
      if Unix.gettimeofday () > deadline then begin
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        ignore (Unix.waitpid [] pid);
        `Timeout
      end
      else (Unix.sleepf 0.01; wait ())
    | _, Unix.WEXITED n -> `Exit n
    | _, _ -> `Exit 1
  in
  let status = wait () in
  let o = truncate (In_channel.with_open_bin out In_channel.input_all) in
  let e = truncate (In_channel.with_open_bin err In_channel.input_all) in
  List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [ path; out; err; Filename.concat dir "main"; Filename.concat dir "main.c" ];
  (try Unix.rmdir dir with Unix.Unix_error _ -> ());
  (status, o, e)

let arg args k = J.member k args

let call name args =
  let src = J.to_str (arg args "source") in
  match name with
  | "vela_spec" -> text_result Spec_text.text
  | "vela_check" -> (
    match analyze src with
    | _, [], _ -> text_result "ok"
    | _, ds, _ -> text_result (Diag.list_to_json ds))
  | "vela_run" | "vela_test" ->
    let native = arg args "native" = J.Bool true in
    let timeout = match arg args "timeout_seconds" with J.Int n -> max 1 (min 60 n) | _ -> 10 in
    let base =
      if name = "vela_run" then [ "run" ]
      else
        [ "test" ]
        @ (match arg args "run" with J.String p -> [ "--run"; p ] | _ -> [])
        @ (if arg args "bench" = J.Bool true then [ "--bench-time"; "200ms" ] else [])
        @ if arg args "json" = J.Bool true then [ "--json" ] else []
    in
    let status, o, e = run_cli (base @ if native then [ "--native" ] else []) src timeout in
    let head = match status with `Exit n -> Printf.sprintf "exit %d" n | `Timeout -> Printf.sprintf "timed out after %d s" timeout in
    text_result ~is_error:(status <> `Exit 0) (Printf.sprintf "%s\nstdout:\n%s\nstderr:\n%s" head o e)
  | "vela_format" -> (
    match Fmt.format ~file src with
    | out -> text_result out
    | exception Diag.Fatal d -> text_result ~is_error:true (Diag.render d))
  | "vela_caps" -> (
    match analyze src with
    | _, [], infos ->
      text_result
        (String.concat "\n"
           (List.map
              (fun (i : Check.fn_info) ->
                let caps = List.map Types.cap_name i.caps in
                i.name ^ ": " ^ (match (caps, i.callbacks) with [], false -> "pure" | [], true -> "pure itself, runs callbacks" | cs, cb -> String.concat ", " cs ^ if cb then " + callbacks" else ""))
              infos))
    | _, ds, _ -> text_result ~is_error:true (Diag.list_to_json ds))
  | "vela_type_at" -> (
    match analyze src with
    | Some items, _, _ -> (
      let off = Lsp.offset_of src (J.to_int (arg args "line")) (J.to_int (arg args "character")) in
      match Lsp.smallest_at ~file off (Lsp.hover_items src items) with
      | Some (_, s) -> text_result s
      | None -> text_result "nothing typed at that position")
    | None, ds, _ -> text_result ~is_error:true (Diag.list_to_json ds))
  | "vela_references" -> (
    match analyze src with
    | Some items, _, _ -> (
      let off = Lsp.offset_of src (J.to_int (arg args "line")) (J.to_int (arg args "character")) in
      let occs = Refs.index items in
      match Refs.at occs file off with
      | None -> text_result ~is_error:true "no name at that position"
      | Some o ->
        let one (r : Refs.occ) =
          let text = Option.value ~default:"" (Source.text r.sp.file) in
          let pos = Lsp.pos_of text r.sp.lo in
          J.Obj
            [ ("file", J.String r.sp.file); ("line", J.member "line" pos); ("character", J.member "character" pos);
              ("end_character", J.Int (J.to_int (J.member "character" pos) + r.sp.hi - r.sp.lo)); ("definition", J.Bool r.def) ]
        in
        text_result (J.to_string (J.Obj [ ("name", J.String o.name); ("references", J.List (List.map one (Refs.references occs o.key))) ])))
    | None, ds, _ -> text_result ~is_error:true (Diag.list_to_json ds))
  | "vela_rename" -> (
    match analyze src with
    | Some items, _, _ -> (
      let off = Lsp.offset_of src (J.to_int (arg args "line")) (J.to_int (arg args "character")) in
      let load overrides =
        match analyze (Option.value ~default:src (List.assoc_opt file overrides)) with
        | Some items, ds, _ -> (items, ds)
        | None, ds, _ -> ([], ds)
      in
      match Refs.rename ~load items file off (J.to_str (arg args "new_name")) with
      | Error m -> text_result ~is_error:true m
      | Ok files -> text_result (Refs.apply_edits src (List.concat_map snd (List.filter (fun (f, _) -> f = file) files))))
    | None, ds, _ -> text_result ~is_error:true (Diag.list_to_json ds))
  | "vela_explain" -> (
    let code = String.uppercase_ascii (J.to_str (arg args "code")) in
    match Diag.explain code with Some t -> text_result (code ^ ": " ^ t) | None -> text_result ~is_error:true ("unknown error code " ^ code))
  | n -> text_result ~is_error:true ("unknown tool " ^ n)

(* ---------- protocol ---------- *)

let handle (msg : J.t) : J.t option =
  let id = J.member "id" msg in
  let params = J.member "params" msg in
  let reply r = if id = J.Null then None else Some (J.Obj [ ("jsonrpc", J.String "2.0"); ("id", id); ("result", r) ]) in
  match J.to_str (J.member "method" msg) with
  | "initialize" ->
    let version = match J.member "protocolVersion" params with J.String v -> v | _ -> "2025-06-18" in
    reply
      (J.Obj
         [ ("protocolVersion", J.String version);
           ("capabilities", J.Obj [ ("tools", J.Obj [ ("listChanged", J.Bool false) ]); ("resources", J.Obj []) ]);
           ("serverInfo", J.Obj [ ("name", J.String "vela"); ("version", J.String "0.4") ]);
           ("instructions",
            J.String "Vela compiler tools. Read the spec (vela_spec or the vela://spec resource) first, then use vela_check after every edit and vela_run or vela_test to try programs.") ])
  | "ping" -> reply (J.Obj [])
  | "tools/list" -> reply (J.Obj [ ("tools", J.List tools) ])
  | "tools/call" -> reply (call (J.to_str (J.member "name" params)) (J.member "arguments" params))
  | "resources/list" ->
    reply
      (J.Obj
         [ ("resources",
            J.List
              [ J.Obj
                  [ ("uri", J.String "vela://spec"); ("name", J.String "Vela language specification");
                    ("mimeType", J.String "text/markdown") ] ]) ])
  | "resources/read" ->
    if J.to_str (J.member "uri" params) = "vela://spec" then
      reply
        (J.Obj
           [ ("contents",
              J.List [ J.Obj [ ("uri", J.String "vela://spec"); ("mimeType", J.String "text/markdown"); ("text", J.String Spec_text.text) ] ]) ])
    else Some (J.Obj [ ("jsonrpc", J.String "2.0"); ("id", id); ("error", J.Obj [ ("code", J.Int (-32002)); ("message", J.String "resource not found") ]) ])
  | m ->
    if id = J.Null then None
    else Some (J.Obj [ ("jsonrpc", J.String "2.0"); ("id", id); ("error", J.Obj [ ("code", J.Int (-32601)); ("message", J.String ("method not supported: " ^ m)) ]) ])

let serve () =
  (* never run git from here: a git dependency is fetched by the CLI *)
  Package.offline := true;
  Random.self_init ();
  let rec loop () =
    match In_channel.input_line stdin with
    | None -> ()
    | Some line ->
      if String.trim line <> "" then begin
        match J.parse line with
        | msg -> (
          match handle msg with
          | Some r -> print_string (J.to_string r ^ "\n"); flush stdout
          | None -> ())
        | exception J.Error m -> prerr_endline ("vela mcp: bad JSON: " ^ m)
      end;
      loop ()
  in
  loop ();
  exit 0
