open Vela

let sprintf = Printf.sprintf
let total = ref 0
let failed = ref 0

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
let grammar_superset = [ "tuple-let-refutable"; "select-bad-arm" ]

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

let run ?(grants = Interp.no_grants) ?modules name src =
  let items, ds, _ = compile ?modules name src in
  if ds <> [] then Error ("diagnostics: " ^ String.concat ", " (List.map (fun (d : Diag.t) -> d.code ^ " " ^ d.message) ds))
  else
    let buf = Buffer.create 64 in
    match Interp.run ~out:(Buffer.add_string buf) ~grants items with
    | Ok v -> Ok (Buffer.contents buf, v)
    | Error (m, _) -> Error ("panic: " ^ m)

(* ---------- native backend: same program, compiled to C ---------- *)

let tmpdir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "vela-native-%d" (Unix.getpid ()))
let () = try Unix.mkdir tmpdir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
let native_enabled = Sys.getenv_opt "VELA_SKIP_NATIVE" = None

(* AddressSanitizer catches use-after-free and double free in the reference
   counting; VELA_LEAKCHECK reports objects still alive at exit. *)
let sanitize = [ "-O1"; "-g"; "-fsanitize=address,undefined"; "-fno-sanitize-recover=all" ]

let read_file p = In_channel.with_open_bin p In_channel.input_all

let run_native ?(grants = Interp.no_grants) ?modules name src =
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
      @ (match grants.seed with Some s -> [ "--seed"; string_of_int s ] | None -> [])
      @ (match grants.fs with Some (d, ro) -> [ "--grant"; "fs=" ^ d ^ if ro then ":ro" else "" ] | None -> [])
      @ ("--" :: grants.args)
    in
    let o = exe ^ ".out" and e = exe ^ ".err" in
    let cmd =
      String.concat " " (List.map Filename.quote (exe :: args))
      ^ sprintf " >%s 2>%s" (Filename.quote o) (Filename.quote e)
    in
    let code = Sys.command ("ASAN_OPTIONS=detect_leaks=0 VELA_LEAKCHECK=1 " ^ cmd) in
    Ok (code, read_file o, read_file e)

let check_native name ?grants ?modules src ~want_out ~want_err_prefix =
  if native_enabled then
    match run_native ?grants ?modules name src with
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

let check_tsan name ?(grants = Interp.no_grants) src want =
  if native_enabled then begin
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
      in
      let o = exe ^ ".out" and e = exe ^ ".err" in
      let cmd =
        String.concat " " (List.map Filename.quote (exe :: args)) ^ sprintf " >%s 2>%s" (Filename.quote o) (Filename.quote e)
      in
      let code = Sys.command ("TSAN_OPTIONS=halt_on_error=1 VELA_THREADS=8 VELA_LEAKCHECK=1 " ^ cmd) in
      let o = read_file o and e = read_file e in
      ok (name ^ "/tsan")
        (code = 0 && o = want && e = "vela-leakcheck: 0 live\n")
        (sprintf "exit %d, stdout %S, stderr %S" code o e)
  end

let out name ?grants ?modules src want =
  (match run ?grants ?modules name src with
   | Ok (got, _) -> ok name (got = want) (sprintf "got %S, want %S" got want)
   | Error e -> ok name false e);
  check_native name ?grants ?modules src ~want_out:want ~want_err_prefix:None

let errs ?modules name src want =
  let _, ds, _ = compile ?modules name src in
  ok name (codes ds = want) (sprintf "got [%s], want [%s]" (String.concat " " (codes ds)) (String.concat " " want))

let panics name src want =
  (match run name src with
   | Error e -> ok name (e = "panic: " ^ want) (sprintf "got %S" e)
   | Ok (o, _) -> ok name false ("no panic, output " ^ o));
  check_native name src ~want_out:"" ~want_err_prefix:(Some ("panic: " ^ want ^ "\n"))

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
  panics "chan-capacity" (main_wrap "    let c: Chan[Int] = channel(0)") "channel capacity must be at least 1";
  errs "group-handle-assign" (main_wrap "    let r = group g { g = 1 }") [ "E0310" ];
  errs "chan-in-struct" "struct S { c: Chan[Int] }\nfn main(root: Root) {}" [ "E0305" ];
  panics "div-zero" (main_wrap "    let z = 0\n    out.print(\"{1 / z}\")") "division by zero";
  panics "index-oob" (main_wrap "    let xs = [1]\n    out.print(\"{xs[3]}\")") "index 3 is out of range for a list of length 1";
  panics "panic-fn" (main_wrap "    panic(\"boom\")") "boom";

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
  ok "lsp-hover-call" (hover doc "dist(pt)" 0 = "```vela\nfn(Point) -> Int\n```") (hover doc "dist(pt)" 0);
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
  errs "method-on-builtin" "fn double(self: Int) -> Int {\n    self * 2\n}\n\nfn main() {}\n" [ "E0110" ];
  errs "method-self-not-first" "struct P {\n    a: Int,\n}\n\nfn f(x: Int, self: P) -> Int {\n    x\n}\n\nfn main() {}\n" [ "E0110" ];
  errs "method-on-option" "fn f(self: Option[Int]) -> Int {\n    1\n}\n\nfn main() {}\n" [ "E0110" ];
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
  out "catch-unwinds-main-loop" "struct Req { path: Str, tags: List[Str] }\n\nfn handle(r: Req) -> Int {\n    let n = r.tags.len()\n    if r.path == \"/bad\" {\n        panic(\"bad path\")\n    }\n    n\n}\n\nfn main(root: Root) {\n    let mut i = 0\n    let mut errs = 0\n    while i < 100 {\n        let req = Req { path: if i % 2 == 0 { \"/bad\" } else { \"/ok\" }, tags: [\"a{i}\", \"b\"] }\n        match catch(|| handle(req)) {\n            Ok(_) => {},\n            Err(_) => { errs = errs + 1 },\n        }\n        i = i + 1\n    }\n    root.stdout.print(\"{errs}\")\n}\n" "50\n";
  check_tsan "group-body-panics-tsan" "fn slow(n: Int) -> Int {\n    let mut i = 0\n    let mut s = 0\n    while i < 20000 {\n        s = s + i % (n + 1)\n        i = i + 1\n    }\n    s\n}\n\nfn main(root: Root) {\n    let mut k = 0\n    let mut errs = 0\n    while k < 50 {\n        let r = catch(|| group g {\n            g.go(|| slow(1))\n            g.go(|| slow(2))\n            panic(\"boom {k}\")\n        })\n        if r.is_err() { errs = errs + 1 }\n        k = k + 1\n    }\n    root.stdout.print(\"{errs}\")\n}\n" "50\n";
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
  errs "map-entry-field" "struct P {\n    x: Int,\n}\n\nfn main() {\n    let mut m = map([(1, P { x: 1 })])\n    m[1].x = 2\n}\n" [ "E0209" ];
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
  put "fetch/fetch.vela" "use util\n\npub fn get(net: Net, u: Str) -> Str {\n    util.first(net.get(u) ?? \"none\")\n}\n";
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
  let vela_cli args =
    let o = Filename.concat pdir "cli.out" and e = Filename.concat pdir "cli.err" in
    let code =
      Sys.command
        (String.concat " " (List.map Filename.quote ("../bin/main.exe" :: args)) ^ sprintf " >%s 2>%s" (Filename.quote o) (Filename.quote e))
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
  ok "fmt-checked-count" (Hashtbl.length fmt_checked > 90) (string_of_int (Hashtbl.length fmt_checked));

  Printf.printf "%d/%d passed\n" (!total - !failed) !total;
  if !failed > 0 then exit 1
