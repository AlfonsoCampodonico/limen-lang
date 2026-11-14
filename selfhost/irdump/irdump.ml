(* Print the IR the OCaml backend makes of FILE, as selfhost/lowered.tsr
   prints the Tessera one's, so the two can be compared: what
   Native.c_source hands to C generation (Lower.lower_program, then
   Pure.cse, then Own.program), with what C generation decides about it
   (Pure.analyse and Pure.plan, and per loop what Hoist finds, as
   Cgen.stmt asks). The program as `tessera build` compiles it comes under
   `== build` when it has a `main`, and as `tessera test` compiles it under
   `== test` when it has tests or benchmarks. A program that does not load
   or check prints its errors instead, one `ERROR CODE FILE LO HI` line
   each.

   The format: the entry points, the libraries and the pure functions
   (sorted by name), then each function, a header and one statement per
   line, indented by nesting. A variable is `name#id`, with its type where
   it is made; a span is `@file:lo-hi`. A loop shows the slots of the pure
   calls kept across it, the lists made unique before it, and whether it is
   a fill loop or versioned; a statement whose call is kept says which
   slot. *)
open Tessera
open Ir

let show = Types.show
let span (sp : Span.t) = Printf.sprintf "@%s:%d-%d" sp.file sp.lo sp.hi
let var (v : var) = Printf.sprintf "%s#%d" v.name v.id
let var_decl (v : var) = Printf.sprintf "%s: %s" (var v) (show v.ty)

let atom = function
  | V v -> var v
  | VDup v -> "dup " ^ var v
  | AInt n -> Int64.to_string n
  | AFloat x -> Printf.sprintf "%h" x
  | ABool b -> string_of_bool b
  | AUnit -> "()"
  | AStr s -> "\"" ^ String.escaped s ^ "\""

let atoms xs = String.concat ", " (List.map atom xs)
let types_of ts = String.concat ", " (List.map show ts)

let step = function
  | PField k -> Printf.sprintf ".%d" k
  | PIndex (a, sp) -> Printf.sprintf "[%s %s]" (atom a) (span sp)
  | PIndexU a -> Printf.sprintf "[%s unchecked]" (atom a)
  | PKey (a, sp) -> Printf.sprintf "{%s %s}" (atom a) (span sp)

let steps ss = String.concat "" (List.map step ss)

let rec cty (c : Ffi.cty) =
  match c with
  | Int -> "Int"
  | Int32 -> "Int32"
  | Float -> "Float"
  | Bool -> "Bool"
  | Str -> "Str"
  | Bytes -> "Bytes"
  | Handle -> "Handle"
  | Unit -> "Unit"
  | Struct (n, fs) -> Printf.sprintf "Struct(%s{%s})" n (String.concat "," (List.map (fun (f, t) -> f ^ ":" ^ cty t) fs))
  | Fn (ps, r) -> Printf.sprintf "Fn(%s)->%s" (String.concat "," (List.map cty ps)) (cty r)

let csig (s : Ffi.csig) =
  let fixed = match s.fixed with Some k -> string_of_int k | None -> "-" in
  let ps = String.concat ", " (List.map (fun (p : Ffi.param) -> p.name ^ ":" ^ cty p.cty ^ if p.out then " out" else "") s.params) in
  Printf.sprintf "%s %s trusted=%b fixed=%s (%s) -> %s" s.lib s.sym s.trusted fixed ps (cty s.ret)

let ipart = function
  | IStr s -> "\"" ^ String.escaped s ^ "\""
  | IVal (a, t) -> Printf.sprintf "val %s: %s" (atom a) (show t)
  | IValQ (a, t) -> Printf.sprintf "valq %s: %s" (atom a) (show t)

let sel = function
  | SRecv (c, t, g) -> Printf.sprintf "recv %s: %s if %s" (atom c) (show t) (atom g)
  | SSend (c, v, t, g) -> Printf.sprintf "send %s %s: %s if %s" (atom c) (atom v) (show t) (atom g)
  | SAfter (c, ms, g) -> Printf.sprintf "after %s %s if %s" (atom c) (atom ms) (atom g)

let rhs = function
  | Atom a -> atom a
  | Bin (op, t, a, b, sp) -> Printf.sprintf "bin %s %s %s, %s %s" (Astdump.op op) (show t) (atom a) (atom b) (span sp)
  | Neg (t, a) -> Printf.sprintf "neg %s %s" (show t) (atom a)
  | Not a -> "not " ^ atom a
  | Call (f, args) -> Printf.sprintf "call %s(%s)" f (atoms args)
  | CallClo (c, args) -> Printf.sprintf "callclo %s(%s)" (atom c) (atoms args)
  | Prim (n, t, args, sp) -> Printf.sprintf "prim %s %s(%s) %s" n (show t) (atoms args) (span sp)
  | MkStruct (t, xs) -> Printf.sprintf "struct %s(%s)" (show t) (atoms xs)
  | MkEnum (t, k, xs) -> Printf.sprintf "enum %s #%d(%s)" (show t) k (atoms xs)
  | MkList (t, xs) -> Printf.sprintf "list %s[%s]" (show t) (atoms xs)
  | MkClosure (f, t, xs) -> Printf.sprintf "closure %s %s(%s)" f (show t) (atoms xs)
  | Get (a, t, ss) -> Printf.sprintf "get %s: %s %s" (atom a) (show t) (steps ss)
  | Payload (a, t, tag, k) -> Printf.sprintf "payload %s: %s #%d.%d" (atom a) (show t) tag k
  | IsTag (a, t, tag) -> Printf.sprintf "istag %s: %s #%d" (atom a) (show t) tag
  | PatTest (a, t, p) -> Printf.sprintf "pattest %s: %s %s" (atom a) (show t) (Astdump.pat p)
  | Interp ps -> "interp[" ^ String.concat ", " (List.map ipart ps) ^ "]"
  | RootField (a, n) -> Printf.sprintf "rootfield %s.%s" (atom a) n
  | Panic (a, sp) -> Printf.sprintf "panic %s %s" (atom a) (span sp)
  | GroupNew (dl, il) ->
    let until = match dl with Some (c, ms) -> Printf.sprintf " until %s %s" (atom c) (atom ms) | None -> "" in
    Printf.sprintf "groupnew interleaved=%b%s" il until
  | Spawn (g, c, t, keep) -> Printf.sprintf "spawn %s %s: %s keep=%b" (atom g) (atom c) (show t) keep
  | GroupJoin (g, t, dl) -> Printf.sprintf "groupjoin %s: %s deadline=%b" (atom g) (show t) dl
  | ChanNew (t, cap, sp) -> Printf.sprintf "channew %s %s %s" (show t) (atom cap) (span sp)
  | ChanSend (c, v, sp) -> Printf.sprintf "chansend %s %s %s" (atom c) (atom v) (span sp)
  | MkDyn (t, key, a, ms) ->
    let entries = String.concat ", " (List.map (fun (f, ps, r) -> Printf.sprintf "%s(%s) -> %s" f (types_of ps) (show r)) ms) in
    Printf.sprintf "dyn %s %s %s [%s]" (show t) key (atom a) entries
  | Select (n, ops, dflt, sp) -> Printf.sprintf "select %d [%s] default=%b %s" n (String.concat "; " (List.map sel ops)) dflt (span sp)
  | SelGot (n, i, t) -> Printf.sprintf "selgot %d %d: %s" n i (show t)
  | Catch (c, t) -> Printf.sprintf "catch %s: %s" (atom c) (show t)
  | CallDyn (k, ps, rt, recv, args) -> Printf.sprintf "calldyn #%d (%s) -> %s %s(%s)" k (types_of ps) (show rt) (atom recv) (atoms args)
  | CallC (s, args, cbs, sp) ->
    let callbacks = String.concat "; " (List.map (fun (c, n, csp, f) -> Printf.sprintf "%s %s %s foreign=%b" c n (span csp) f) cbs) in
    Printf.sprintf "callc %s (%s) [%s] %s" (csig s) (atoms args) callbacks (span sp)

let mutation = function
  | MSet a -> "= " ^ atom a
  | MPush a -> "push " ^ atom a
  | MPut (k, v) -> Printf.sprintf "put %s %s" (atom k) (atom v)
  | MRemove k -> "remove " ^ atom k

(* The plan of the function being printed, and the lists unique in the
   loops around, as Cgen keeps them in c.plan and c.uniq. *)
let plan = ref (Pure.no_plan ())
let uniq = ref []

let kept (v : var) = match Hashtbl.find_opt !plan.kept v.id with Some (sl : Pure.slot) -> Printf.sprintf " kept k%d" sl.sid | None -> ""

let rec block ind ss = List.iter (stmt ind) ss

and stmt ind s =
  let line x = print_endline (ind ^ x) in
  let deeper = ind ^ "  " in
  match s with
  | Let (v, r) -> line (Printf.sprintf "let %s = %s%s" (var_decl v) (rhs r) (kept v))
  | Decl v -> line ("decl " ^ var_decl v)
  | Init (v, r) -> line (Printf.sprintf "init %s = %s%s" (var v) (rhs r) (kept v))
  | Set (v, r, release) -> line (Printf.sprintf "set %s = %s release=%b" (var v) (rhs r) !release)
  | Mutate (v, ss, m) -> line (Printf.sprintf "mutate %s%s %s" (var v) (steps ss) (mutation m))
  | If (c, a, b) ->
    line ("if " ^ atom c);
    block deeper a;
    if b <> [] then begin
      line "else";
      block deeper b
    end
  | Loop body ->
    let slots =
      match List.find_opt (fun (b, _) -> b == body) !plan.at_loop with
      | Some (_, slots) ->
        " slots[" ^ String.concat ", " (List.map (fun (sl : Pure.slot) -> Printf.sprintf "k%d: %s" sl.sid (show sl.sty)) slots) ^ "]"
      | None -> ""
    in
    let saved = !uniq in
    let hoisted = List.filter (fun (v : var) -> not (List.mem v.id saved) && not (Hoist.escapes v body)) (Hoist.mutated_lists body) in
    let unique = if hoisted = [] then "" else " unique[" ^ String.concat ", " (List.map var hoisted) ^ "]" in
    uniq := List.map (fun (v : var) -> v.id) hoisted @ saved;
    let how =
      match Hoist.fill_loop body with
      | Some (i, inclusive, n, v, x) ->
        Printf.sprintf " fill(%s %s %s push %s %s)" (var i) (if inclusive then "<=" else "<") (atom n) (var v) (atom x)
      | None -> (
        match Hoist.versionable body with
        | Some (m, inclusive, n, step, lists) ->
          Printf.sprintf " versioned(%s %s %s step %s lists %s)" (var m) (if inclusive then "<=" else "<") (atom n) (atom step)
            (String.concat ", " (List.map var lists))
        | None -> "")
    in
    line ("loop" ^ slots ^ unique ^ how);
    block deeper body;
    uniq := saved
  | Break -> line "break"
  | Continue -> line "continue"
  | Return a -> line ("return " ^ atom a)
  | Drop v -> line ("drop " ^ var v)
  | Guard (n, h, b) ->
    line (Printf.sprintf "guard %d" n);
    line "handler";
    block deeper h;
    line "body";
    block deeper b
  | GuardEnd n -> line (Printf.sprintf "guardend %d" n)
  | Scope (g, b) ->
    line ("scope " ^ atom g);
    block deeper b

let program (p : program) libs =
  Printf.printf "main %s root=%b ret=%s\n" p.main p.main_takes_root (show p.main_ret);
  List.iter (fun (c, fields, virt) -> Printf.printf "test %s [%s] virtual=%b\n" c (String.concat "," fields) virt) p.tests;
  List.iter (fun (c, fields) -> Printf.printf "bench %s [%s]\n" c (String.concat "," fields)) p.benches;
  Printf.printf "libs [%s]\n" (String.concat "," libs);
  let pure = Pure.analyse p in
  let names = List.sort compare (Hashtbl.fold (fun k _ acc -> k :: acc) pure []) in
  List.iter (fun n -> Printf.printf "pure %s heavy=%b\n" n (Hashtbl.find pure n).Pure.heavy) names;
  List.iter
    (fun (f : fn) ->
      plan := Pure.plan pure f;
      uniq := [];
      let caps = if f.captures = [] then "" else " captures(" ^ String.concat ", " (List.map var_decl f.captures) ^ ")" in
      Printf.printf "fn %s(%s) -> %s%s%s\n" f.fname (String.concat ", " (List.map var_decl f.params)) (show f.ret)
        (if f.is_closure then " closure" else "") caps;
      block "  " f.body)
    p.fns;
  plan := Pure.no_plan ()

(* The IR of the program, as Native.c_source makes it. The counters the
   OCaml compiler keeps in globals start again for each. *)
let dump ~tests items =
  Lower.var_counter := 0;
  Lower.guard_counter := 0;
  Pure.slot_counter := 0;
  let prog, _ = Lower.lower_program ~tests items in
  program (Own.program (Pure.cse prog)) (Ffi.libs items)

let error (d : Diag.t) = Printf.printf "ERROR %s %s %d %d\n" d.code d.span.file d.span.lo d.span.hi

let () =
  let path = Sys.argv.(1) in
  let src = In_channel.with_open_bin path In_channel.input_all in
  match Package.load path with
  | Error d -> error d
  | Ok pkg -> (
    let deps = match pkg with Some p -> List.map (fun (d : Package.dep) -> (d.dname, d.dir)) p.deps | None -> [] in
    let root = match pkg with Some p -> Filename.dirname p.file | None -> Filename.dirname path in
    match Modules.load ~deps ~root path src with
    | [], (_ :: _ as ds) -> List.iter error ds
    | items, _ ->
      let ds, infos = Check.check_program items in
      (* the capability checks, as Modules.load_and_check makes them *)
      let ds = Diag.errors ds in
      let ds = if ds = [] then Package.enforce ~std:true (Lazy.force Package.std_manifest) (List.map (fun m -> (m, m)) !Modules.std_loaded) infos else ds in
      let ds = match pkg with Some p when ds = [] -> Package.enforce p !Modules.module_pkg infos | _ -> ds in
      let ds = match pkg with Some p when ds = [] -> Package.check_foreign_lock p (Modules.c_users items) | _ -> ds in
      if ds <> [] then List.iter error ds
      else begin
        let fns = List.filter_map (function Ast.IFn d -> Some d | _ -> None) items in
        if List.exists (fun (d : Ast.fn_decl) -> d.fn_name = "main") fns then begin
          print_endline "== build";
          dump ~tests:false items
        end;
        if List.exists (fun (d : Ast.fn_decl) -> d.fn_test <> None) fns then begin
          print_endline "== test";
          dump ~tests:true items
        end
      end)
