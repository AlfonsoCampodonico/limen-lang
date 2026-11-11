open Vela

let sprintf = Printf.sprintf
let total = ref 0
let failed = ref 0

(* VELA_TEST_ONLY=a,b runs only the program tests whose names start with
   one of those prefixes (for working on a feature; the full run ignores it). *)
let selected name =
  match Sys.getenv_opt "VELA_TEST_ONLY" with
  | None | Some "" -> true
  | Some ps -> List.exists (fun p -> String.starts_with ~prefix:p name) (String.split_on_char ',' ps)

let ok name cond msg =
  incr total;
  if not cond then begin
    incr failed;
    Printf.printf "FAIL %s: %s\n" name msg
  end

(* VELA_DUMP_DIR=dir writes every program into dir/accept (it parses) or
   dir/reject (lexer or parser error), for checking grammar/vela.gbnf. *)
(* Rejected by the parser but within the grammar's documented superset
   (keywords are not excluded from identifiers). *)
let grammar_superset = [ "tuple-let-refutable"; "select-bad-arm"; "group-until-bad" ]

let dump name src parsed =
  match Sys.getenv_opt "VELA_DUMP_DIR" with
  | None -> ()
  | Some _ when (not parsed) && List.mem name grammar_superset -> ()
  | Some dir ->
    let sub = Filename.concat dir (if parsed then "accept" else "reject") in
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    (try Unix.mkdir sub 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    Out_channel.with_open_bin (Filename.concat sub (name ^ ".vela")) (fun oc -> output_string oc src)

(* vela fmt on every program that parses: the result must parse to the same
   tree (ignoring positions), formatting again must not change it, and no
   comment may be lost. Checked once per program. *)
let fmt_checked : (string, unit) Hashtbl.t = Hashtbl.create 64
let fmt_failures = ref []

let check_fmt name src () =
  if not (Hashtbl.mem fmt_checked name) then begin
    Hashtbl.replace fmt_checked name ();
    let file = name ^ ".vela" in
    let comments_of s = (try ignore (Lexer.lex "c.vela" s) with Diag.Fatal _ -> ()); List.sort compare (List.map (fun (_, _, t) -> t) !Lexer.comments) in
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
  let file = name ^ ".vela" in
  let file =
    if modules = [] then file
    else begin
      let dir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "vela-mods-%d-%s" (Unix.getpid ()) name) in
      (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      List.iter (fun (m, s) -> Out_channel.with_open_bin (Filename.concat dir (m ^ ".vela")) (fun oc -> output_string oc s)) modules;
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
  if ds <> [] then Error ("diagnostics: " ^ String.concat ", " (List.map (fun (d : Diag.t) -> d.code ^ " " ^ d.message) ds))
  else
    let buf = Buffer.create 64 in
    match with_stdin feed (fun () -> Interp.run ~out:(Buffer.add_string buf) ~grants items) with
    | Ok v -> Ok (Buffer.contents buf, v)
    | Error (m, _) -> Error ("panic: " ^ m)

(* ---------- native backend: same program, compiled to C ---------- *)

let tmpdir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "vela-native-%d" (Unix.getpid ()))
let () = try Unix.mkdir tmpdir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
let native_enabled = Sys.getenv_opt "VELA_SKIP_NATIVE" = None

(* VELA_NATIVE_ONLY=a,b runs the native half only for tests whose name starts with a or b. *)
let native_for name =
  native_enabled
  && match Sys.getenv_opt "VELA_NATIVE_ONLY" with
     | None | Some "" -> true
     | Some l -> List.exists (fun p -> String.starts_with ~prefix:p name) (String.split_on_char ',' l)

(* AddressSanitizer catches use-after-free and double free in the reference
   counting; VELA_LEAKCHECK reports objects still alive at exit. *)
let sanitize = [ "-O1"; "-g"; "-fsanitize=address,undefined"; "-fno-sanitize-recover=all" ]

let read_file p = In_channel.with_open_bin p In_channel.input_all

(* With [feed], stdin comes from that shell command, and the program runs on
   one worker thread, so a task that blocked its thread would stall the rest. *)
let run_native ?(grants = Interp.no_grants) ?modules ?feed name src =
  let items, ds, _ = compile ?modules name src in
  if ds <> [] then Error "does not compile" else
  let exe = Filename.concat tmpdir name in
  match Native.build ~flags:sanitize items exe with
  | Error m -> Error ("C compile failed:\n" ^ m)
  | Ok () ->
    let args =
      List.concat_map (fun h -> [ "--grant"; "net=" ^ h ]) grants.Interp.net
      @ List.concat_map (fun h -> [ "--grant"; "listen=" ^ h ]) grants.Interp.listen
      @ List.concat_map (fun n -> [ "--grant"; "env=" ^ n ]) grants.Interp.env
      @ List.concat_map (fun n -> [ "--grant"; "exec=" ^ n ]) grants.Interp.exec
      @ (match grants.seed with Some s -> [ "--seed"; string_of_int s ] | None -> [])
      @ (match grants.fs with Some (d, ro) -> [ "--grant"; "fs=" ^ d ^ if ro then ":ro" else "" ] | None -> [])
      @ ("--" :: grants.args)
    in
    let o = exe ^ ".out" and e = exe ^ ".err" in
    let cmd =
      String.concat " " (List.map Filename.quote (exe :: args))
      ^ sprintf " >%s 2>%s" (Filename.quote o) (Filename.quote e)
    in
    let env = "ASAN_OPTIONS=detect_leaks=0 VELA_LEAKCHECK=1 " in
    let code =
      match feed with
      | None -> Sys.command (env ^ cmd)
      | Some f -> Sys.command (sprintf "(%s) | %sVELA_THREADS=1 %s" f env cmd)
    in
    Ok (code, read_file o, read_file e)

let check_native name ?grants ?modules ?feed src ~want_out ~want_err_prefix =
  if native_for name then
    match run_native ?grants ?modules ?feed name src with
    | Error m -> ok (name ^ "/native") false m
    | Ok (code, o, e) -> (
      ok (name ^ "/native") (o = want_out) (sprintf "stdout %S, want %S (stderr %S)" o want_out e);
      match want_err_prefix with
      | None ->
        ok (name ^ "/native-clean")
          (code = 0 && e = "vela-leakcheck: 0 live\n")
          (sprintf "exit %d, stderr %S" code e)
      | Some p -> ok (name ^ "/native-panic") (String.starts_with ~prefix:p e) (sprintf "stderr %S" e))

(* Concurrency: the same program again under ThreadSanitizer. *)
let tsan = [ "-O1"; "-g"; "-fsanitize=thread" ]

let check_tsan name ?(grants = Interp.no_grants) ?feed src want =
  if native_for name && selected name then begin
    let items, ds, _ = compile name src in
    let exe = Filename.concat tmpdir (name ^ "-tsan") in
    if ds <> [] then ok (name ^ "/tsan") false "does not compile"
    else
    match Native.build ~flags:tsan items exe with
    | Error m -> ok (name ^ "/tsan") false ("C compile failed:\n" ^ m)
    | Ok () ->
      let args =
        (match grants.Interp.fs with Some (d, ro) -> [ "--grant"; "fs=" ^ d ^ if ro then ":ro" else "" ] | None -> [])
        @ List.concat_map (fun h -> [ "--grant"; "net=" ^ h ]) grants.Interp.net
        @ List.concat_map (fun h -> [ "--grant"; "listen=" ^ h ]) grants.Interp.listen
        @ List.concat_map (fun h -> [ "--grant"; "exec=" ^ h ]) grants.Interp.exec
      in
      let o = exe ^ ".out" and e = exe ^ ".err" in
      let cmd =
        String.concat " " (List.map Filename.quote (exe :: args)) ^ sprintf " >%s 2>%s" (Filename.quote o) (Filename.quote e)
      in
      let env = "TSAN_OPTIONS=halt_on_error=1 VELA_THREADS=8 VELA_LEAKCHECK=1 " in
      let code = match feed with None -> Sys.command (env ^ cmd) | Some f -> Sys.command (sprintf "(%s) | %s%s" f env cmd) in
      let o = read_file o and e = read_file e in
      ok (name ^ "/tsan")
        (code = 0 && o = want && e = "vela-leakcheck: 0 live\n")
        (sprintf "exit %d, stdout %S, stderr %S" code o e)
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

let panics name src want =
  if selected name then begin
  (match run name src with
   | Error e -> ok name (e = "panic: " ^ want) (sprintf "got %S" e)
   | Ok (o, _) -> ok name false ("no panic, output " ^ o));
  check_native name src ~want_out:"" ~want_err_prefix:(Some ("panic: " ^ want ^ "\n"))
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
    match run_native ?grants name src with
    | Error m -> ok (name ^ "/native") false m
    | Ok (code, o, e) ->
      ok (name ^ "/native")
        (o = want_out && code = want_code
         && (String.ends_with ~suffix:"vela-leakcheck: 0 live\n" e || String.starts_with ~prefix:"error: " e))
        (sprintf "stdout %S exit %d stderr %S, want %S exit %d" o code e want_out want_code)

(* Apply every fix's edits and return the new source. *)
let apply_fixes src (ds : Diag.t list) =
  let edits = List.concat_map (fun (d : Diag.t) -> match d.fix with Some f -> f.edits | None -> []) ds in
  let edits = List.sort (fun (a : Diag.edit) b -> compare b.at.lo a.at.lo) edits in
  List.fold_left
    (fun s (e : Diag.edit) ->
      String.sub s 0 e.at.lo ^ e.replacement ^ String.sub s e.at.hi (String.length s - e.at.hi))
    src edits

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
    let errs = rs.filter(|r| r.is_err()).map(|r| r.map_err(|e| e.to_upper()))
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
    (main_wrap "    out.print(\"{root.exec.run(\"vela-no-such-program\", [])}\")")
    "Err(Failed(\"exec: cannot run `vela-no-such-program`: No such file or directory\"))\n";
  out "exec-signal" ~grants:sh
    (main_wrap "    out.print(\"{root.exec.run(\"sh\", [\"-c\", \"kill -9 $$\"]).map_err(|e| 0)}\")")
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
  (* the child sees PATH and only the variables the Env grant names *)
  let env_src = main_wrap {|    match root.exec.run("sh", ["-c", "echo $\{HOME:+home\} $\{PATH:+path\} $\{VELA_SECRET_TEST:-none\}"]) {
        Ok(r) => out.write(r.stdout),
        Err(e) => out.print("{e}"),
    }|} in
  Unix.putenv "VELA_SECRET_TEST" "s3cret";
  out "exec-env-filtered" ~grants:sh env_src "path none\n";
  out "exec-env-granted" ~grants:{ sh with env = [ "HOME"; "VELA_SECRET_TEST" ] } env_src "home path s3cret\n";
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
    out.print("{net.get("https://127.0.0.1:{port}/")}")
    Ok(())
}
|} in
  let http_want =
    "200 [\"content-length\"] None GET /a/b?x=1 HTTP/1.1\n200 [\"content-length\"] None GET /?q HTTP/1.1\n404 [\"content-type\", \"x-a\", \"content-length\"] Some(\"1, 2\") nope!\n200 [\"transfer-encoding\"] None Wikipedia\n301 [\"location\"] None until close\nFailed(\"http: incomplete response headers\")\nFailed(\"http: truncated body\")\nFailed(\"http: malformed header line\")\nFailed(\"http: bad chunk size\")\nFailed(\"http: more than 1000 response headers\")\nFailed(\"http: response headers larger than 64 KiB\")\n[11, 0]\nErr(Failed(\"https is not supported yet (vela has no TLS); use an http:// URL\"))\n"
  in
  let http_grants = { Interp.no_grants with listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1" ] } in
  out "net-get-http" ~grants:http_grants http_src http_want;
  check_tsan "net-get-http" ~grants:http_grants http_src http_want;
  let dir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "vela-test-%d" (Unix.getpid ())) in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (try Unix.mkdir (Filename.concat dir "sub") 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Out_channel.with_open_bin (Filename.concat dir "a.txt") (fun oc -> output_string oc "alpha");
  Out_channel.with_open_bin (Filename.concat dir "sub/b.txt") (fun oc -> output_string oc "beta");
  let real = Unix.realpath dir in
  let rw = { Interp.no_grants with fs = Some (real, false) } in
  let ro = { Interp.no_grants with fs = Some (real, true) } in
  out "fs-read" ~grants:ro (main_wrap "    out.print(\"{root.fs.read(\"a.txt\")}\")") "Ok(\"alpha\")\n";
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

  (* ---------- vela test ---------- *)
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
  let items, ds, _ = compile "vela-test" test_src in
  ok "vela-test-checks" (ds = []) (String.concat "," (codes ds));
  let want =
    [ ("adds", None); ("fails on a list", Some "assertion failed: [\"a\", \"b\"] != [\"a\", \"b\", \"c\"]");
      ("panics are failures", Some "index 5 is out of range for a list of length 1"); ("channels", None) ]
  in
  let got = List.map (fun (n, r) -> (n, match r with Ok () -> None | Error (m, _) -> Some m)) (Interp.run_tests ~out:ignore items) in
  ok "vela-test-interp" (got = want) (String.concat "; " (List.map (fun (n, m) -> n ^ "=" ^ Option.value ~default:"ok" m) got));
  if native_enabled then begin
    let exe = Filename.concat tmpdir "vela-test-native" in
    match Native.build ~flags:sanitize ~tests:true items exe with
    | Error m -> ok "vela-test-native" false m
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
      ok "vela-test-native" (got = want) (String.concat "; " (List.map (fun (n, m) -> n ^ "=" ^ Option.value ~default:"ok" m) got))
  end;
  errs "vela-test-duplicate" "test \"a\" {}\ntest \"a\" {}" [ "E0106" ];

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
  let items, ds, _ = compile "vela-test-caps" cap_tests in
  ok "vela-test-caps-checks" (ds = []) (String.concat "," (codes ds));
  let want =
    [ ("saves a file", None); ("each test starts empty", None); ("clock only", None);
      ("fails with a dir", Some "assertion failed: Ok([\"x\"]) != Ok([])") ]
  in
  let test_dirs () =
    List.filter (String.starts_with ~prefix:"vela-test-") (Array.to_list (Sys.readdir (Filename.get_temp_dir_name ())))
  in
  let before = test_dirs () in
  let got = List.map (fun (n, r) -> (n, match r with Ok () -> None | Error (m, _) -> Some m)) (Interp.run_tests ~out:ignore items) in
  ok "vela-test-caps-interp" (got = want) (String.concat "; " (List.map (fun (n, m) -> n ^ "=" ^ Option.value ~default:"ok" m) got));
  ok "vela-test-caps-cleanup" (List.for_all (fun d -> List.mem d before) (test_dirs ())) "test directories left behind";
  if native_for "vela-test-caps" then begin
    let exe = Filename.concat tmpdir "vela-test-caps" in
    match Native.build ~flags:sanitize ~tests:true items exe with
    | Error m -> ok "vela-test-caps-native" false m
    | Ok () ->
      List.iteri
        (fun i (n, w) ->
          let dir = Caps.test_dir () in
          let e = exe ^ ".err" in
          let code =
            Sys.command
              (sprintf "ASAN_OPTIONS=detect_leaks=0 VELA_LEAKCHECK=1 %s %d --grant fs=%s 2>%s" (Filename.quote exe) i (Filename.quote dir) (Filename.quote e))
          in
          Caps.remove_tree dir;
          let err = read_file e in
          match w with
          | None -> ok ("vela-test-caps-native/" ^ n) (code = 0 && err = "vela-leakcheck: 0 live\n") (sprintf "exit %d, stderr %S" code err)
          | Some m -> ok ("vela-test-caps-native/" ^ n) (code <> 0 && String.starts_with ~prefix:("panic: " ^ m) err) (sprintf "exit %d, stderr %S" code err))
        want
  end;
  errs "vela-test-bad-cap" "test \"x\" (n: Net) {}\ntest \"y\" (a: Fs, b: Fs) {}\ntest \"z\" (c: Clock, d: Int) {}" [ "E0306"; "E0306"; "E0306" ];

  (* ---------- language server ---------- *)
  let module J = Json in
  let st = Lsp.create () in
  let uri = "file:///tmp/lsp-test.vela" in
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
  ok "lsp-hover-let" (hover doc "total = " 0 = "```vela\nlet total: Int\n```") (hover doc "total = " 0);
  (* a call shows the function's declaration (and its doc comment, if any) *)
  ok "lsp-hover-call" (hover doc "dist(pt)" 0 = "```vela\nfn dist(p: Point) -> Int\n```") (hover doc "dist(pt)" 0);
  ok "lsp-hover-method" (hover doc "print(\"{" 0 = "```vela\nprint(Str) -> ()\n```") (hover doc "print(\"{" 0);
  ok "lsp-hover-fn" (hover doc "dist(p" 0 = "```vela\nfn dist(p: Point) -> Int\n```") (hover doc "dist(p" 0);
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
  let c4 = complete doc "    let n" 4 in
  ok "lsp-complete-general" (List.mem "total" c4 && List.mem "dist" c4 && List.mem "while" c4) (String.concat "," c4);
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
    let dir = if Sys.file_exists "../examples/stdlib.vela" then "../examples" else "examples" in
    In_channel.with_open_bin (Filename.concat dir f) In_channel.input_all
  in
  out "std-json-list-text" (example "stdlib.vela") "{\"name\":\"Ada\",\"tags\":[\"x\",\"yé😀\"],\"n\":-125,\"ok\":true,\"none\":null,\"nested\":{\"a\":[1,2,{}]}}\nAda\n-125.0\nexpected a value at byte 3\nexpected `:` at byte 5\nexpected a value at byte 0\nexpected a closing `\"` at byte 4\nexpected end of input at byte 4\nexpected a value at byte 0\n{\"q\":\"a\\\"b\\n\\u0001\",\"xs\":[1,null,false]}\na,b,c\n[0, 1, 2, 3, 4] 2 [1, 3] [1, 2, 3] [1, 2]\n007|ab..|---|2|[\"hi\", \"there\", \"you\"]\n";
  out "std-http"
    ~grants:{ Interp.no_grants with listen = [ "127.0.0.1:0" ]; net = [ "127.0.0.1" ] }
    (example "http_demo.vela")
    "served 4\n200 text/plain; charset=utf-8 hi GET\n500 text/plain; charset=utf-8 internal error\n404 text/plain; charset=utf-8 no /missing\n201 {\"got\":[1,{\"a\":true}]}\nchunked sent\n200 hello world\n";
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
  out "std-report-example" (example "report.vela")
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
  out "std-logscan-example" (example "logscan.vela")
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
  (* ---------- Rand, Env, args and the Fs additions ---------- *)
  let box = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "vela-box-%d" (Unix.getpid ())) in
  (try Unix.mkdir box 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let box = Unix.realpath box in
  Unix.putenv "VELA_TEST_VISIBLE" "yes";
  Unix.putenv "VELA_TEST_HIDDEN" "no";
  out "caps-rand-env-args-fs"
    ~grants:{ Interp.no_grants with seed = Some 42; env = [ "VELA_TEST_VISIBLE" ]; args = [ "one"; "two" ]; fs = Some (box, false) }
    (String.concat "\n"
       [ "fn main(root: Root) -> Result[(), IoError] {";
         "    let out = root.stdout";
         "    let r = root.rand";
         "    let xs = [r.int(1, 7), r.int(1, 7), r.int(1, 7), r.int(-5, -4), r.int(0, 1000000)]";
         "    out.print(\"{xs} {r.float() < 1.0}\")";
         "    out.print(\"{root.args()} {root.env.get(\"VELA_TEST_VISIBLE\")} {root.env.get(\"VELA_TEST_HIDDEN\")}\")";
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
  out "methods" (example "methods.vela") "8.0 12.0\ncircle at 8.0,12.0 with area 3.0\nrect with area 96.0\none 1 1\n10 25\n";
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
  out "interfaces" (example "interfaces.vela") "15.0 9.0\nsquare of area 4.0\nbox of area 1.0, box of area 1.0\n500 true\n";
  let shape = "interface Shape {\n    fn area(self) -> Float\n}\n\nstruct Blob {\n    n: Int,\n}\n\nfn area(self: Blob) -> Int {\n    self.n\n}\n\nfn total[T: Shape](xs: List[T]) -> Float {\n    xs.map(|s| s.area()).fold(0.0, |a, b| a + b)\n}\n\n" in
  errs "iface-wrong-signature" (shape ^ "fn main() {\n    let a = total([Blob { n: 1 }])\n}\n") [ "E0111" ];
  errs "iface-builtin-missing" (shape ^ "fn main() {\n    let a = total([1, 2])\n}\n") [ "E0111" ];
  errs "iface-unbounded-method" (shape ^ "fn loose[T](x: T) -> Float {\n    x.area()\n}\n\nfn main() {}\n") [ "E0105" ];
  errs "iface-as-type" (shape ^ "fn bad(s: Shape) {}\n\nfn main() {}\n") [ "E0102" ];
  errs "iface-unknown-bound" (shape ^ "fn bad[T: Shpe](x: T) {}\n\nfn main() {}\n") [ "E0102" ];
  errs "iface-forward-unbounded" (shape ^ "fn outer[T](xs: List[T]) -> Float {\n    total(xs)\n}\n\nfn main() {}\n") [ "E0111" ];
  errs "iface-bound-on-struct" "interface S {\n    fn a(self) -> Int\n}\n\nstruct B[T: S] {\n    x: T,\n}\n\nfn main() {}\n" [ "E0003" ];
  (* an interface from another module, satisfied by a type in this one *)
  let geo = "pub interface Area {\n    fn area(self) -> Int\n}\n\npub fn twice[T: Area](x: T) -> Int {\n    x.area() * 2\n}\n" in
  out "iface-across-modules" ~modules:[ ("geo", geo) ]
    "use geo\n\nstruct Sq {\n    s: Int,\n}\n\nfn area(self: Sq) -> Int {\n    self.s * self.s\n}\n\nfn show[T: geo.Area](x: T) -> Int {\n    geo.twice(x) + x.area()\n}\n\nfn main(root: Root) {\n    root.stdout.print(\"{show(Sq { s: 3 })}\")\n}\n"
    "27\n";

  (* ---------- 64-bit Int, identical in both backends ---------- *)
  out "int64" "fn main(root: Root) {\n    let out = root.stdout\n    let max = 9223372036854775807\n    let min = -max - 1\n    out.print(\"{max} {min} {max + 1} {min - 1} {max * 2} {min / -1} {min % -1}\")\n    out.print(\"{min.abs()} {(-7) / 2} {(-7) % 2} {7 % -2}\")\n    out.print(\"{4611686018427387904 * 2} {3037000500 * 3037000500}\")\n    out.print(\"{1.0e19.to_int()} {(-1.0e19).to_int()} {9.0e18.to_int()} {(0.0 / 0.0).to_int()}\")\n    out.print(\"{\"9223372036854775807\".parse_int()} {\"-9223372036854775808\".parse_int()} {\"9223372036854775808\".parse_int()}\")\n    out.print(\"{max.to_float()} {[1, 2, 3].get(max)} {\"abc\".slice(-max, max)} {[max, max].sum()}\")\n    match max {\n        9223372036854775807 => out.print(\"matched max\"),\n        _ => out.print(\"no\"),\n    }\n}\n"
    "9223372036854775807 -9223372036854775808 -9223372036854775808 9223372036854775807 -2 -9223372036854775808 0\n-9223372036854775808 -3 -1 1\n-9223372036854775808 -9223372036709301616\n9223372036854775807 -9223372036854775808 9000000000000000000 0\nSome(9223372036854775807) Some(-9223372036854775808) None\n9.22337203685478e+18 None abc -2\nmatched max\n";
  errs "int-literal-too-large" (main_wrap "    let x = 9223372036854775808") [ "E0005" ];

  (* ---------- select ---------- *)
  out "select" (example "select.vela") "nums done; words done; total 15 letters 3\nnothing ready | sent to c | sent to d | d Some(10)\nafter close: Some(7)\nErr(\"every arm of this `select` is turned off by its guard\")\n";
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
  errs "select-two-afters" (main_wrap (tchans ^ "    let x = select {\n        v = c.recv() => 1,\n        clock.after(1) => 2,\n        clock.after(2) => 3,\n    }")) [ "E0106" ];
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
    // tasks started after the deadline never run
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
|} "[Some(1), None, None, None, None, None, None, Some(7)]\ntrue\n[None]\n[Some(1), None]\nErr(\"boom\")\n";
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
            let mut i = 0
            while i < 10 {
                g.go(|| 1)
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

  (* ---------- tuples and maps ---------- *)
  out "tuples" (example "tuples.vela") "3 2 (2, 1)\n(1, \"one\", 1.5) 1 one 1.5 (2, \"a\")\n4 ((1, 2), (3, (4, 5)))\n1 3 4 5\na1 b2 c3 \nyes zero yes no 7\ntrue false 6\n";
  out "maps" (example "maps.vela") "{\"the\": 3, \"cat\": 1, \"and\": 2, \"hat\": 1, \"bat\": 1} 5 3 None\n[\"the\", \"cat\", \"and\", \"hat\", \"bat\"] [3, 1, 2, 1, 1]\nthe=3 and=2 \n{\"the\": 100, \"and\": 2, \"hat\": 1, \"bat\": 1, \"zebra\": 9}\n{\"the\": 3, \"cat\": 1, \"and\": 2, \"hat\": 1, \"bat\": 1}\ntrue false false\n{1: 1, 2: 4, 3: 9} true false 0\n1 {\"a\": 1}\nyes\n";
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
  out "dyn" (example "dyn.vela")
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
  (* packages: each dependency may only use the capabilities vela.toml grants it *)
  let pdir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "vela-pkg-%d" (Unix.getpid ())) in
  List.iter (fun d -> try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    [ pdir; Filename.concat pdir "app"; Filename.concat pdir "fetch" ];
  let put f s = Out_channel.with_open_bin (Filename.concat pdir f) (fun oc -> output_string oc s) in
  put "fetch/fetch.vela" "use util\n\npub fn get(net: Net, u: Str) -> Str {\n    match net.get(u) {\n        Ok(r) => util.first(r.body),\n        Err(_) => \"none\",\n    }\n}\n";
  put "fetch/util.vela" "pub fn first(s: Str) -> Str {\n    s.lines().first() ?? \"\"\n}\n\npub fn sneaky(fs: Fs) -> Bool {\n    fs.exists(\"x\")\n}\n";
  let app = "use fetch\n\nfn main(root: Root) {\n    root.stdout.print(fetch.get(root.net, \"http://h\"))\n}\n" in
  put "app/main.vela" app;
  let pkg_codes toml =
    put "app/vela.toml" toml;
    let path = Filename.concat pdir "app/main.vela" in
    let _, ds, _ = Modules.load_and_check path app in
    codes ds
  in
  ok "pkg-denied" (pkg_codes "[package]\nname = \"app\"\n\n[dependencies]\nfetch = { path = \"../fetch\" }\n" = [ "E0320"; "E0320" ]) "two functions over their grant";
  ok "pkg-partial" (pkg_codes "[package]\nname = \"app\"\n[dependencies]\nfetch = { path = \"../fetch\", caps = [\"net\"] }\n" = [ "E0320" ]) "util.sneaky takes Fs";
  ok "pkg-granted" (pkg_codes "[package]\nname = \"app\"\n[dependencies]\nfetch = { path = \"../fetch\", caps = [\"net\", \"fs\"] }  # both\n" = []) "granted";
  ok "pkg-bad-toml" (pkg_codes "[package]\nname = \"app\"\n[dependencies]\nfetch = { path = \"../fetch\", caps = [\"disk\"] }\n" = [ "E0321" ]) "bad cap";
  ok "pkg-missing-dir" (pkg_codes "[package]\nname = \"app\"\n[dependencies]\nother = { path = \"../nope\" }\n" = [ "E0401" ]) "fetch not found";
  Sys.remove (Filename.concat pdir "app/vela.toml");

  (* [grants] in vela.toml: the program's own grants, with --grant flags adding to them *)
  (* the CLI built next to this test binary, wherever it is run from *)
  let vela_exe = Filename.concat (Filename.dirname (Filename.dirname Sys.executable_name)) "bin/main.exe" in
  let vela_exe = if Filename.is_relative vela_exe then Filename.concat (Sys.getcwd ()) vela_exe else vela_exe in
  let vela_cli args =
    let o = Filename.concat pdir "cli.out" and e = Filename.concat pdir "cli.err" in
    let code =
      Sys.command
        (String.concat " " (List.map Filename.quote (vela_exe :: args)) ^ sprintf " >%s 2>%s" (Filename.quote o) (Filename.quote e))
    in
    (code, read_file o, read_file e)
  in
  List.iter (fun d -> try Unix.mkdir (Filename.concat pdir d) 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()) [ "grants"; "grants/data"; "envy" ];
  put "grants/data/a.txt" "hi";
  Unix.putenv "VELA_T_A" "a";
  Unix.putenv "VELA_T_B" "b";
  put "grants/vela.toml" "[package]\nname = \"g\"\n\n[grants]\nfs = \"data:ro\"   # read-only\nenv = [\"VELA_T_A\"]\n";
  put "grants/main.vela"
    "fn main(root: Root) {\n    root.stdout.print(root.fs.read(\"a.txt\") ?? \"denied\")\n    root.stdout.print(root.env.get(\"VELA_T_A\") ?? \"none\")\n    root.stdout.print(root.env.get(\"VELA_T_B\") ?? \"none\")\n    root.stdout.print(\"{root.fs.write(\"b.txt\", \"x\").is_ok()}\")\n}\n";
  let gmain = Filename.concat pdir "grants/main.vela" in
  (match Package.load gmain with
   | Ok (Some p) ->
     ok "grants-toml-parse"
       (p.grants = [ ("fs", Filename.concat (Filename.concat pdir "grants") "data" ^ ":ro"); ("env", "VELA_T_A") ])
       (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) p.grants))
   | _ -> ok "grants-toml-parse" false "no package");
  List.iter
    (fun (name, extra) ->
      let code, o, e = vela_cli ([ "run" ] @ extra @ [ gmain ]) in
      ok ("grants-toml-" ^ name) (code = 0 && o = "hi\na\nnone\nfalse\n") (sprintf "code %d, out %S, err %S" code o e);
      let code, o, e = vela_cli ([ "run" ] @ extra @ [ gmain; "--grant"; "env=VELA_T_B" ]) in
      ok ("grants-toml-flag-adds-" ^ name) (code = 0 && o = "hi\na\nb\nfalse\n") (sprintf "code %d, out %S, err %S" code o e))
    ([ ("interp", []) ] @ if native_enabled then [ ("native", [ "--native" ]) ] else []);
  (* a flag's fs replaces the toml's: here a writable one *)
  let code, o, e = vela_cli [ "run"; gmain; "--grant"; "fs=" ^ Filename.concat pdir "grants/data" ] in
  ok "grants-toml-fs-flag" (code = 0 && o = "hi\na\nnone\ntrue\n") (sprintf "code %d, out %S, err %S" code o e);
  let grant_err toml =
    put "grants/vela.toml" toml;
    let code, _, e = vela_cli [ "run"; gmain ] in
    code = 1 && Interp_util.find_sub e "E0321" 0 <> None
  in
  ok "grants-toml-always" (grant_err "[grants]\nstdout = \"x\"\n") "stdout is always granted";
  ok "grants-toml-unknown" (grant_err "[grants]\ndisk = \"x\"\n") "unknown grant";
  ok "grants-toml-two-fs" (grant_err "[grants]\nfs = [\"a\", \"b\"]\n") "one fs";
  ok "grants-toml-twice" (grant_err "[grants]\nnet = \"a\"\nnet = \"b\"\n") "listed twice";
  Sys.remove (Filename.concat pdir "grants/vela.toml");
  (* a dependency's own vela.toml never grants anything: only the program's does *)
  put "envy/envy.vela" "pub fn get(env: Env) -> Str {\n    env.get(\"VELA_T_A\") ?? \"none\"\n}\n";
  put "envy/vela.toml" "[package]\nname = \"envy\"\n\n[grants]\nenv = \"*\"\n";
  put "grants/vela.toml" "[package]\nname = \"g\"\n\n[dependencies]\nenvy = { path = \"../envy\", caps = [\"env\"] }\n";
  put "grants/dep.vela" "use envy\n\nfn main(root: Root) {\n    root.stdout.print(envy.get(root.env))\n}\n";
  let code, o, e = vela_cli [ "run"; Filename.concat pdir "grants/dep.vela" ] in
  ok "grants-dep-toml-ignored" (code = 0 && o = "none\n") (sprintf "code %d, out %S, err %S" code o e);
  Sys.remove (Filename.concat pdir "grants/vela.toml");

  (* git dependencies: a local file:// repository, fetched into a temporary
     cache, pinned by vela.lock *)
  let gdir = Filename.concat pdir "git" in
  List.iter (fun d -> try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()) [ gdir; Filename.concat gdir "repo"; Filename.concat gdir "app" ];
  Unix.putenv "VELA_CACHE" (Filename.concat gdir "cache");
  let repo = Filename.concat gdir "repo" in
  let git args = Sys.command (String.concat " " (List.map Filename.quote ([ "git"; "-C"; repo; "-c"; "user.name=t"; "-c"; "user.email=t@t" ] @ args)) ^ " >/dev/null 2>&1") in
  let rev_of r =
    let o = Filename.concat gdir "rev" in
    ignore (Sys.command (sprintf "git -C %s rev-parse %s >%s" (Filename.quote repo) (Filename.quote r) (Filename.quote o)));
    String.trim (read_file o)
  in
  let version v = Out_channel.with_open_bin (Filename.concat repo "greet.vela") (fun oc -> output_string oc (sprintf "pub fn hi() -> Str {\n    \"%s\"\n}\n" v)) in
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
  let gmain = Filename.concat gdir "app/main.vela" in
  put "git/app/main.vela" gapp;
  let lock = Filename.concat gdir "app/vela.lock" in
  let gtoml dep = put "git/app/vela.toml" (sprintf "[package]\nname = \"app\"\n\n[dependencies]\ngreet = { %s }\n" dep) in
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
  ok "git-lock-written" (lock_has (sprintf "greet %s tag:v1 %s blake2b:" url v1)) (if Sys.file_exists lock then read_file lock else "no vela.lock");
  if native_enabled then begin
    let code, o, e = vela_cli [ "run"; "--native"; gmain ] in
    ok "git-tag/native" (code = 0 && o = "v1\n") (sprintf "code %d, out %S, err %S" code o e)
  end;
  (* the tag moves upstream: the lock still pins the old commit *)
  ok "git-move-tag" (git [ "tag"; "-f"; "v1"; "v2" ] = 0) "git tag -f";
  grun "git-locked" "v1";
  (* the editor and agent servers do not fetch, but use what is cached *)
  Package.offline := true;
  grun "git-offline-cached" "v1";
  (* a changed cached copy no longer matches the lock *)
  let cached = Filename.concat (Package.commit_dir url v1) "greet.vela" in
  let orig = read_file cached in
  Out_channel.with_open_bin cached (fun oc -> output_string oc (orig ^ "// changed\n"));
  ok "git-hash-mismatch" (gcodes () = [ "E0323" ]) (String.concat "," (gcodes ()));
  Out_channel.with_open_bin cached (fun oc -> output_string oc orig);
  grun "git-hash-restored" "v1";
  (* a changed tag in vela.toml is fetched, except offline *)
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
  Out_channel.with_open_bin (Filename.concat repo "greet.vela") (fun oc ->
      output_string oc "pub fn hi() -> Str {\n    \"v3\"\n}\n\npub fn peek(fs: Fs) -> Bool {\n    fs.exists(\"x\")\n}\n");
  ok "git-v3" (git [ "commit"; "-qam"; "three" ] = 0 && git [ "tag"; "v3" ] = 0) "v3";
  gtoml (sprintf "git = %S, tag = \"v3\"" url);
  ok "git-caps-denied" (gcodes () = [ "E0320" ]) (String.concat "," (gcodes ()));
  gtoml (sprintf "git = %S, tag = \"v3\", caps = [\"fs\"]" url);
  grun "git-caps-granted" "v3";

  (* the standard library declares its capabilities in src/std/vela.toml and is checked against it *)
  let manifest = Lazy.force Package.std_manifest in
  let listed = List.sort compare (List.map (fun (d : Package.dep) -> d.dname) manifest.deps) in
  ok "std-manifest-complete" (listed = List.sort compare (List.map fst Std_text.modules)) (String.concat ", " listed);
  ok "std-manifest-http" (List.exists (fun (d : Package.dep) -> d.dname = "http" && d.caps = [ "net"; "listen" ]) manifest.deps) "http: net, listen";
  ok "std-manifest-pure" (List.for_all (fun (d : Package.dep) -> d.dname = "http" || d.caps = []) manifest.deps) "json, list, text are pure";
  let real_std = !Modules.std_modules in
  let std_errs name extra src =
    Modules.std_modules := extra @ real_std;
    let _, ds, _ = Modules.load_and_check (name ^ ".vela") src in
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
  (* checked without any vela.toml, and fine for the real std *)
  let _, ds, _ = Modules.load_and_check "std-http.vela" "use http\nuse json\n\nfn main(root: Root) {\n    root.stdout.print(http.reason(200))\n}\n" in
  ok "std-real-ok" (ds = []) (String.concat "; " (List.map (fun (d : Diag.t) -> d.code ^ " " ^ d.message) ds));

  (* find references and rename on real programs: renaming every function,
     struct, enum and variant of each example must be accepted (a missed
     reference would make the renamed program fail to check), and the result
     must check cleanly *)
  let refs_failures = ref [] and renamed = ref 0 in
  Array.iter
    (fun f ->
      if Filename.check_suffix f ".vela" then begin
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
    (Sys.readdir "../examples");
  ok "refs-rename-examples" (!refs_failures = [] && !renamed > 50) (sprintf "%d renamed; %s" !renamed (String.concat "\n" (List.rev !refs_failures)));

  (* vela doc: doc comments directly above items, fields, variants and interface methods *)
  let doc_src =
    "// Geometry helpers.\n//\n// Second paragraph.\n\n// A point.\npub struct Point {\n    // Distance from the left.\n    x: Float,\n    y: Float,\n}\n\n// not a doc: a blank line follows\n\n// How far from the origin.\npub fn norm(self: Point) -> Float {\n    self.x\n}\n\npub enum Shape {\n    // A round one.\n    Circle(Float),\n    Dot,\n}\n\n// Things with an area.\npub interface Area {\n    // The area, in square units.\n    fn area(self) -> Float\n}\n\n// Shouts.\npub fn loud(self: Str) -> Str {\n    self.upper()\n}\n\n// Private.\nfn hidden() -> Int {\n    1\n}\n\npub fn pick[T: Area](xs: List[T], i: Int) -> T {\n    xs[i]\n}\n"
  in
  let md = Doc.load_source "geom" "geom.vela" doc_src in
  let mdtext = Doc.markdown [ md ] in
  let has s = Interp_util.find_sub mdtext s 0 <> None in
  ok "doc-markdown"
    (has "# Module `geom`\n\nGeometry helpers.\n\nSecond paragraph.\n"
     && has "### struct `Point`\n\n```vela\npub struct Point {\n    x: Float,\n    y: Float,\n}\n```\n\nA point.\n\n- `x`: Distance from the left.\n"
     && has "#### Methods\n\n```vela\npub fn norm(self: Point) -> Float\n```\n\nHow far from the origin.\n"
     && has "- `Circle`: A round one.\n" && has "Things with an area.\n\n- `area`: The area, in square units.\n"
     && has "## Methods on `Str`\n\n```vela\npub fn loud(self: Str) -> Str\n```\n\nShouts.\n"
     && has "pub fn pick[T: Area](xs: List[T], i: Int) -> T\n"
     && (not (has "hidden")) && not (has "not a doc"))
    mdtext;
  let all = Doc.markdown [ Doc.load_source ~all:true "geom" "geom.vela" doc_src ] in
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

  ok "fmt-use-pub"
    (Fmt.format ~file:"fu.vela" "use geom // g\npub   fn f() -> Int { 1 }\npub struct S { a: Int }\n"
     = "use geom  // g\n\npub fn f() -> Int { 1 }\n\npub struct S {\n    a: Int,\n}\n")
    (Fmt.format ~file:"fu.vela" "use geom // g\npub   fn f() -> Int { 1 }\npub struct S { a: Int }\n");

  (* the formatter on the examples and benchmarks too (with their comments) *)
  List.iter
    (fun dir ->
      if Sys.file_exists dir then
        Array.iter
          (fun f ->
            if Filename.check_suffix f ".vela" then begin
              let path = Filename.concat dir f in
              let src = In_channel.with_open_bin path In_channel.input_all in
              match Parser.parse_program path src with
              | _ -> check_fmt ("file-" ^ f) src ()
              | exception Diag.Fatal _ -> ()
            end)
          (Sys.readdir dir))
    [ "../examples"; "../bench" ];
  ok "fmt-every-program" (!fmt_failures = [])
    (String.concat "\n\n" (List.map (fun (n, m) -> n ^ ": " ^ m) (List.rev !fmt_failures)));
  ok "fmt-checked-count" (Hashtbl.length fmt_checked > 90 || Sys.getenv_opt "VELA_ONLY" <> None) (string_of_int (Hashtbl.length fmt_checked));

  Printf.printf "%d/%d passed\n" (!total - !failed) !total;
  if !failed > 0 then exit 1
