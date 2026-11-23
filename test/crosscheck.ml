(* The cross-check: Limen programs that apply every built-in to edge cases and
   to seeded random inputs, one program per area. The test suite runs each
   through the interpreter and the native build and fails on the first line
   that differs, so drift between the built-ins written twice shows up as a
   named input with both answers.

   Every line reads "{area} {k} {input} ... -> {results}". Inputs are written
   so that any value survives the source: strings as bytes([..]).to_str(),
   UInts as (<bit pattern>).to_uint<w>(), floats as %.17g literals, NaN and
   the infinities as divisions. LIMEN_CROSSCHECK_SEED picks another seed. *)

let sprintf = Printf.sprintf

let seed =
  match Option.bind (Sys.getenv_opt "LIMEN_CROSSCHECK_SEED") int_of_string_opt with
  | Some s -> s
  | None -> 20261123

(* ---------- literals ---------- *)

let ilit n = if n = Int64.min_int then "(-9223372036854775807 - 1)" else sprintf "(%Ld)" n
let ulit w n = sprintf "%s.to_uint%d()" (ilit n) w

let flit x =
  if Float.is_nan x then "(0.0 / 0.0)"
  else if x = Float.infinity then "(1.0 / 0.0)"
  else if x = Float.neg_infinity then "(-1.0 / 0.0)"
  else if x = 0.0 && Float.sign_bit x then "(-0.0)"
  else
    let s = sprintf "%.17g" (Float.abs x) in
    let s = String.concat "" (String.split_on_char '+' s) in
    let s = if String.exists (fun c -> c = '.' || c = 'e') s then s else s ^ ".0" in
    if x < 0.0 then "(-" ^ s ^ ")" else s

let slit s = sprintf "bytes([%s]).to_str()" (String.concat ", " (List.map (fun c -> string_of_int (Char.code c)) (List.of_seq (String.to_seq s))))

(* A list literal, one item per line. *)
let lst items = "[\n        " ^ String.concat ",\n        " items ^ "\n    ]"

(* A long list as functions of [per] items each, joined by `+`: the C
   compiler takes far longer on one huge function (with the sanitizers, minutes)
   than on many small ones. Gives the functions and the expression. *)
let chunked ?(per = 20) name ty items =
  let rec groups acc cur n = function
    | [] -> List.rev (if cur = [] then acc else List.rev cur :: acc)
    | x :: rest -> if n = per then groups (List.rev cur :: acc) [ x ] 1 rest else groups acc (x :: cur) (n + 1) rest
  in
  let gs = groups [] [] 0 items in
  let defs = List.mapi (fun i g -> sprintf "fn %s_%d() -> List[%s] {\n    %s\n}\n\n" name i ty (lst g)) gs in
  (String.concat "" defs, if gs = [] then "[]" else String.concat " + " (List.mapi (fun i _ -> sprintf "%s_%d()" name i) gs))

let shuffle st l =
  let a = Array.of_list l in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int st (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  Array.to_list a

let rand64 st = Random.State.bits64 st

(* ---------- inputs ---------- *)

let long_digits n = String.make (n - 20) '0' ^ "12345678901234567890"

let str_edges =
  [ ""; "a"; "\000"; "\r"; "\r\n"; "\n\r"; "a\n\r"; "\xff"; "\xc0\x80"; "\xed\xa0\x80"; "\xf4\x90\x80\x80"; "\xe2\x82"; "caf\xc3\xa9";
    "\xf0\x9f\x98\x80"; " \t\x0c a \r\n"; "a,,b"; "aaa"; "+7"; "-0"; "9223372036854775807"; "9223372036854775808";
    "-9223372036854775808"; "1_000"; "0x10"; "1e5"; ".5"; "5."; "1e"; "e5"; "-"; "nan"; "inf"; long_digits 407; long_digits 408;
    long_digits 409 ]

let str_random st =
  let alphabet = [| "a"; ","; " "; "\n"; "\r"; "\t"; "\000"; "\xc3"; "\xa9"; "\xf0"; "\x9f"; "0"; "."; "e"; "-"; "+"; "" |] in
  let byte () =
    match alphabet.(Random.State.int st (Array.length alphabet)) with
    | "0" -> String.make 1 (Char.chr (48 + Random.State.int st 10))
    | "" -> String.make 1 (Char.chr (Random.State.int st 256))
    | c -> c
  in
  String.concat "" (List.init (Random.State.int st 41) (fun _ -> byte ()))

let code_points = [ -1L; 0L; 0x7fL; 0x80L; 0x7ffL; 0x800L; 0xd800L; 0xdfffL; 0xffffL; 0x10000L; 0x10ffffL; 0x110000L; Int64.max_int ]

let p2 k = Int64.shift_left 1L k

let int_edges =
  [ 0L; 1L; -1L; 2L; -2L; 63L; 64L; 65L; -64L; p2 31; p2 32; Int64.sub (p2 53) 1L; Int64.add (p2 53) 1L; Int64.max_int; Int64.min_int ]

let float_edges =
  [ 0.0; -0.0; Float.nan; Float.infinity; Float.neg_infinity; 5e-324; 2.2250738585072014e-308; 0.1; 1e-7; 1e15; 1e16 -. 2.0; 1e16;
    9.2e18; -9.2e18; 9.3e18; 9007199254740992.0; 9007199254740994.0; 123456789012345.6; 1e21; 1e22; 1.7976931348623157e308;
    -1.7976931348623157e308 ]

let rec finite_random st =
  let x = Int64.float_of_bits (rand64 st) in
  if Float.is_finite x then x else finite_random st

(* ---------- programs ---------- *)

let main body = "fn main(root: Root) {\n    let out = root.stdout\n" ^ body ^ "}\n"

let str_program st =
  let inputs = str_edges @ List.init 200 (fun _ -> str_random st) in
  let defs, xs = chunked ~per:10 "inputs" "Str" (List.map slit inputs) in
  defs
  ^ {|// 1 or 0, to keep the lines short
fn b(x: Bool) -> Int {
    if x { 1 } else { 0 }
}

// "=" for a result that is the input itself, likewise
fn r(s: Str, x: Str) -> Str {
    if x == s { "=" } else { "{[x]}" }
}

fn str_one(out: Stdout, k: Int, s: Str, t: Str) {
    let p = "str {k} {[s]}"
    let cs = s.chars()
    let bs = s.bytes()
    let sp = s.split("")
    out.print("{p} -> {s.len()} {s.is_empty()} {r(s, s.trim())} {r(s, s.to_upper())} {r(s, s.to_lower())} {s.parse_int()} {s.parse_float()} bytes {bs.sum()} {bs.min()} {bs.max()} chars {cs.map(|c| "{c.len()}").join("")} {b(cs.join("") == s)} replace {r(s, s.replace("", "x"))} {r(s, s.replace("a", ""))} {r(s, s.replace("aa", "b"))} plus {(s + t).len()} {b((s + t).starts_with(s))}{b((s + t).ends_with(t))}")
    out.print("{p} lines -> {s.lines()} split {sp.len()} {b(sp.all(|x| x.len() == 1))}{b(sp.join("") == s)} {s.split(",")} {s.split("\n")} {s.split("aa")} {s.split("\r\n")}")
    let needles = ["", "a", "aa", ",", "\r\n", bytes([195]).to_str(), t]
    let found = needles.map(|n| "{b(s.contains(n))}{b(s.starts_with(n))}{b(s.ends_with(n))}:{s.index_of(n) ?? -1}:{s.last_index_of(n) ?? -1}")
    let ends = [-1, 0, 1, s.len(), s.len() + 1, -9223372036854775807 - 1, 9223372036854775807]
    let mut lens: List[Int] = []
    for lo in ends {
        for hi in ends {
            lens.push(s.slice(lo, hi).len())
        }
    }
    let some = ends.map(|lo| r(s, s.slice(lo, s.len())))
    out.print("{p} find {[t]} -> {found} slice {lens.map(|n| "{n}").join(",")} {some.join(" ")}")
}

|}
  ^ main
      (sprintf
         {|    let xs = %s
    for (k, s) in xs.with_index() {
        str_one(out, k, s, xs[(k + 1) %% xs.len()])
    }
    for n in %s {
        out.print("str to_char {n} -> {[n.to_char()]}")
    }
|}
         xs
         (lst (List.map ilit code_points)))

let long_program =
  main
    {|    let mut parts: List[Str] = []
    for _ in 0..<20000 {
        parts.push("a\r\n")
    }
    let s = parts.join("")
    let ls = s.lines()
    let sp = s.split("\r")
    out.print("long len -> {s.len()}")
    out.print("long lines -> {ls.len()} {ls.map(|l| l.len()).sum()} {ls.first()} {ls.last()}")
    out.print("long split -> {sp.len()} {sp.map(|l| l.len()).sum()} {sp.first()} {sp.last()}")
    out.print("long chars -> {s.chars().len()}")
    out.print("long replace -> {s.replace("\r\n", "\n").len()}")
    out.print("long trim -> {s.trim().len()}")
|}

let int_program st =
  let small () = Int64.of_int (Random.State.int st 141 - 70) in
  let pick () = if Random.State.bool st then small () else rand64 st in
  let rand = List.init 200 (fun _ -> (rand64 st, pick ())) in
  let da, ra = chunked "ra" "Int" (List.map (fun (a, _) -> ilit a) rand) in
  let db, rb = chunked "rb" "Int" (List.map (fun (_, b) -> ilit b) rand) in
  da ^ db
  ^ {|fn int_one(out: Stdout, a: Int) {
    out.print("int {a} -> {a.abs()} {a.to_float()} {a.to_str()} {a.to_uint8()} {a.to_uint16()} {a.to_uint32()} {a.to_uint64()} {-a} {~a}")
}

fn int_pair(out: Stdout, a: Int, b: Int) {
    let d = if b != 0 { "{a / b} {a % b}" } else { "-" }
    let sh = if b >= 0 { "{a << b} {a >> b} {a.shift_right_unsigned(b)}" } else { "-" }
    out.print("int {a} {b} -> {a + b} {a - b} {a * b} {d} {sh} {a & b} {a | b} {a ^ b} {a.min(b)} {a.max(b)} {a < b} {a <= b} {a > b} {a >= b} {a == b} {a != b}")
}

|}
  ^ main
      (sprintf
         {|    let es = %s
    let ra = %s
    let rb = %s
    for a in es + ra {
        int_one(out, a)
    }
    for a in es {
        for b in es {
            int_pair(out, a, b)
        }
    }
    for i in 0..<ra.len() {
        int_pair(out, ra[i], rb[i])
    }
|}
         (lst (List.map ilit int_edges)) ra rb)

let uint_program st =
  let widths = [ 8; 16; 32; 64 ] in
  let mask w n = if w = 64 then n else Int64.logand n (Int64.sub (p2 w) 1L) in
  let edges w =
    let max = mask w (-1L) in
    [ 0L; 1L; max; Int64.sub max 1L; p2 (w - 1) ]
    @
    if w = 64 then [ Int64.sub Int64.min_int 1L; Int64.add Int64.min_int 1L; Int64.add Int64.min_int 1023L; Int64.add Int64.min_int 1024L; Int64.add Int64.min_int 1025L; -1025L ]
    else []
  in
  let fns w =
    sprintf
      {|fn u%d_one(out: Stdout, a: UInt%d) {
    out.print("u%d {a} -> {a.to_int()} {a.to_str()} {a.to_float()} {a.to_float().to_fixed(0)} {~a} {[-1, 0, 1, %d, %d].map(|n| (a.rotate_left(n), a.rotate_right(n)))}")
}

fn u%d_pair(out: Stdout, a: UInt%d, b: UInt%d) {
    let d = if b != (0).to_uint%d() { "{a / b} {a %% b}" } else { "-" }
    out.print("u%d {a} {b} -> {a + b} {a - b} {a * b} {d} {a << b} {a >> b} {a & b} {a | b} {a ^ b} {a.min(b)} {a.max(b)} {a < b} {a <= b} {a > b} {a >= b} {a == b} {a != b}")
}

|}
      w w w w (w + 1) w w w w w
  in
  let parts w =
    let e = edges w in
    let rand = List.init 50 (fun _ -> (mask w (rand64 st), mask w (if Random.State.bool st then Int64.of_int (Random.State.int st (w + 3)) else rand64 st))) in
    let da, ra = chunked (sprintf "u%d_ra" w) (sprintf "UInt%d" w) (List.map (fun (a, _) -> ulit w a) rand) in
    let db, rb = chunked (sprintf "u%d_rb" w) (sprintf "UInt%d" w) (List.map (fun (_, b) -> ulit w b) rand) in
    let call =
      sprintf {|    let u%d_es = %s
    let u%d_ra = %s
    let u%d_rb = %s
    for a in u%d_es + u%d_ra {
        u%d_one(out, a)
    }
    for a in u%d_es {
        for b in u%d_es {
            u%d_pair(out, a, b)
        }
    }
    for i in 0..<u%d_ra.len() {
        u%d_pair(out, u%d_ra[i], u%d_rb[i])
    }
|}
        w (lst (List.map (ulit w) e)) w ra w rb w w w w w w w w w w
    in
    (fns w ^ da ^ db, call)
  in
  let ps = List.map parts widths in
  String.concat "" (List.map fst ps) ^ main (String.concat "" (List.map snd ps))

let float_program st =
  let rand = List.init 200 (fun _ -> finite_random st) in
  let lit x = sprintf "(%s, \"%s\")" (flit x) (flit x) in
  let defs, rs = chunked "rs" "(Float, Str)" (List.map lit rand) in
  defs
  ^ {|fn float_one(out: Stdout, x: Float, src: Str) {
    out.print("float {src} -> {x.to_str()} {x} {[x]} {x.to_int()} {x.abs()} {x.sqrt()}")
    // "=" where to_fixed gives what the count before it gave
    let fx = [-1, 0, 1, 2, 15, 30, 31].map(|d| x.to_fixed(d))
    let shown = fx.with_index().map(|p| if p.0 > 0 && fx[p.0 - 1] == p.1 { "=" } else { p.1 })
    out.print("float {src} fixed -> {shown.join(" ")}")
}

fn float_pair(out: Stdout, a: (Float, Str), b: (Float, Str)) {
    let x = a.0
    let y = b.0
    out.print("float {a.1} {b.1} -> {x + y} {x - y} {x * y} {x / y} {x < y} {x <= y} {x > y} {x >= y} {x == y} {x != y}")
}

|}
  ^ main
      (sprintf {|    let es = %s
    let rs = %s
    for p in es + rs {
        float_one(out, p.0, p.1)
    }
    for a in es {
        for b in es {
            float_pair(out, a, b)
        }
    }
    for i in 0..<rs.len() {
        float_pair(out, rs[i], rs[(i + 1) %% rs.len()])
    }
|}
         (lst (List.map lit float_edges)) rs)

let order_program st =
  let fs = shuffle st float_edges and ss = shuffle st str_edges in
  (* the same number of each, for the tuples *)
  let n = min (List.length fs) (List.length ss) in
  let ss = List.filteri (fun i _ -> i < n) ss in
  let defs, sexp = chunked ~per:10 "ss" "Str" (List.map slit ss) in
  defs
  ^ main
    (sprintf
       {|    let fs = %s
    let ss = %s
    let bs = ss.map(|s| s.to_bytes())
    let ts = fs.zip(ss)
    out.print("order floats -> {fs.sort()}")
    out.print("order floats by index -> {fs.with_index().map(|p| (p.1, p.0)).sort().map(|p| p.1)}")
    out.print("order strs -> {ss.sort()}")
    out.print("order bytes -> {bs.sort()}")
    out.print("order tuples -> {ts.sort()}")
    out.print("order tuples by index -> {ts.with_index().map(|p| (p.1, p.0)).sort().map(|p| p.1)}")
    out.print("order lists -> {fs < fs.sort()} {fs.sort() < fs} {ss < ss.sort()} {bs < bs.sort()} {ts < ts.sort()} {ts.sort() < ts}")
    for i in 0..<fs.len() {
        for j in 0..<fs.len() {
            let a = fs[i]
            let b = fs[j]
            out.print("order float {i} {j} {a} {b} -> {a < b} {[a] < [b]} {[a] == [b]} {(a, ss[i]) < (b, ss[j])}")
        }
    }
    for i in 0..<ss.len() {
        let j = (i + 1) %% ss.len()
        out.print("order str {i} {j} {[ss[i]]} {[ss[j]]} -> {ss[i] < ss[j]} {bs[i] < bs[j]} {[ss[i]] < [ss[j]]} {ss[i] == ss[j]}")
    }
|}
       (lst (List.map flit fs))
       sexp)

let rand_program =
  main
    {|    let r = root.rand
    let spans = [(0, 1), (0, 2), (-5, 5), (0, 4611686018427387905), (-9223372036854775807 - 1, 9223372036854775807)]
    for sp in spans {
        for k in 0..<50 {
            out.print("rand int {sp.0} {sp.1} {k} -> {r.int(sp.0, sp.1)}")
        }
    }
    for k in 0..<50 {
        let f = r.float()
        out.print("rand float {k} -> {f} {(f * 9007199254740992.0).to_int()}")
    }
    for n in [0, 1, 7, 8, 9, 17] {
        for k in 0..<50 {
            out.print("rand bytes {n} {k} -> {r.bytes(n)}")
        }
    }
|}

let panic_program =
  main
    {|    let r = root.rand
    let xs = [1, 2, 3]
    let b = bytes([1, 2])
    let mut ms: Map[Str, Int] = map()
    ms["k"] = 1
    let mut mi: Map[Int, Int] = map()
    mi[1] = 1
    let zero = xs.len() - 3
    let neg = zero - 1
    out.print("panic list -1 -> {catch(|| xs[neg])}")
    out.print("panic list len -> {catch(|| xs[xs.len()])}")
    out.print("panic list max -> {catch(|| xs[9223372036854775807])}")
    out.print("panic bytes index -1 -> {catch(|| b[neg])}")
    out.print("panic bytes index len -> {catch(|| b[b.len()])}")
    out.print("panic map str -> {catch(|| ms["a\"b\n\0c"])}")
    out.print("panic map int -> {catch(|| mi[-7])}")
    out.print("panic bytes 256 -> {catch(|| bytes([1, 256]))}")
    out.print("panic bytes -1 -> {catch(|| bytes([0, -1]))}")
    out.print("panic int div -> {catch(|| 7 / zero)} {catch(|| 7 % zero)}")
    out.print("panic int shift -> {catch(|| 7 << neg)} {catch(|| 7 >> neg)} {catch(|| 7.shift_right_unsigned(neg))}")
    out.print("panic u8 div -> {catch(|| (7).to_uint8() / zero.to_uint8())} {catch(|| (7).to_uint8() % zero.to_uint8())}")
    out.print("panic u16 div -> {catch(|| (7).to_uint16() / zero.to_uint16())} {catch(|| (7).to_uint16() % zero.to_uint16())}")
    out.print("panic u32 div -> {catch(|| (7).to_uint32() / zero.to_uint32())} {catch(|| (7).to_uint32() % zero.to_uint32())}")
    out.print("panic u64 div -> {catch(|| (7).to_uint64() / zero.to_uint64())} {catch(|| (7).to_uint64() % zero.to_uint64())}")
    out.print("panic rand int -> {catch(|| r.int(3, 3))}")
    out.print("panic rand bytes -> {catch(|| r.bytes(-1))}")
|}

(* (test name, Limen source, --seed for Rand). The random inputs come from
   one stream, drawn program by program in this order. *)
let programs ~seed =
  let st = Random.State.make [| seed |] in
  let str = str_program st in
  let int = int_program st in
  let uint = uint_program st in
  let float = float_program st in
  let order = order_program st in
  [ ("xc-str", str, None);
    ("xc-long", long_program, None);
    ("xc-int", int, None);
    ("xc-uint", uint, None);
    ("xc-float", float, None);
    ("xc-order", order, None);
    ("xc-rand", rand_program, Some seed);
    ("xc-panic", panic_program, None) ]
