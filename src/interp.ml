(* Tree-walking interpreter. Runs only programs that passed the checker.
   Values have value semantics: lists and structs are never shared mutably. *)
open Ast

let sprintf = Printf.sprintf

type cap =
  | CRoot
  | CNet of string list
  | CFs of Caps.fs
  | CStdout
  | CClock
  | CListen of string list
  | CListener of sock * int
  | CConn of sock
  | CRand
  | CStdin
  | CEnv of string list
  | CExec of string list
  | CForeign of string list (* libraries, or "*" *)

(* A socket that knows when it was closed, so a task waiting on it never
   touches a descriptor number that has been reused. *)
and sock = { fd : Unix.file_descr; mutable shut : bool; mutable reading : bool; mutable writing : bool; mutable tls : Tls.side }
(* reading / writing: a task is in a read (or accept) / a write that has had
   to wait; as natively, another that has to wait meanwhile fails. tls: a
   TLS connection's session, or a TLS listener's context. *)

(* Map keys: Int, Str, Bool, and tuples, structs and enums of them. *)
type mkey =
  | KInt of int64
  | KStr of string
  | KBytes of string
  | KBool of bool
  | KStruct of string * (string * mkey) list
  | KEnum of string * string * mkey list

(* Any total order will do (maps iterate in insertion order, kept apart),
   so keys are compared directly instead of through the generic compare. *)
let key_tag = function KInt _ -> 0 | KStr _ -> 1 | KBytes _ -> 2 | KBool _ -> 3 | KStruct _ -> 4 | KEnum _ -> 5

let rec compare_key a b =
  match (a, b) with
  | KInt x, KInt y -> Int64.compare x y
  | KStr x, KStr y | KBytes x, KBytes y -> String.compare x y
  | KBool x, KBool y -> Bool.compare x y
  | KStruct (n, fs), KStruct (m, gs) ->
    let c = String.compare n m in
    if c <> 0 then c else compare_keys (List.map snd fs) (List.map snd gs)
  | KEnum (e, v, xs), KEnum (f, w, ys) ->
    let c = String.compare e f in
    if c <> 0 then c else
      let c = String.compare v w in
      if c <> 0 then c else compare_keys xs ys
  | _ -> Int.compare (key_tag a) (key_tag b)

and compare_keys xs ys =
  match (xs, ys) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys -> ( match compare_key x y with 0 -> compare_keys xs ys | c -> c)

module KM = Map.Make (struct
  type t = mkey
  let compare = compare_key
end)

type value =
  | VInt of int64 (* 64-bit and wrapping, as in native code *)
  | VMap of vmap
  | VFloat of float
  | VBool of bool
  | VStr of string
  | VBytes of string
  | VUnit
  | VList of value array
  | VBuf of buf
      (* a list being built or updated in a variable: only ever in a
         variable's cell, never a value anywhere else (see settle) *)
  | VStruct of string * (string * value) list
  | VEnum of string * string * value list
  | VClosure of string list * expr * env
  | VFn of string
  | VCtor of string * string
  | VPanicFn
  | VCap of cap
  | VGroup of group
  | VChan of chan
  | VChanFn
  | VBuiltin of string (* assert, assert_eq *)

(* A channel as natively: a buffer of up to [cap] values (none when cap is
   0), and queues of tasks waiting to send and to receive. *)
and chan = { q : value Queue.t; cap : int; mutable closed : bool; rq : waiter Queue.t; sq : waiter Queue.t }

(* A waiting task. A `select` puts one on every channel it waits on, all
   sharing one claim; the first operation to happen sets [fired] to its
   index, and the other waiters are then stale. -1: still waiting; -2:
   given up (cancelled). [v] is the value to send, or the value received. *)
and claim = { mutable fired : int }
and waiter = { claim : claim; idx : int; mutable v : value; mutable ok : bool }

(* A task's cancel flag: set by its group's deadline, seen at its next wait. *)
and task = { mutable cancelled : bool; mutable scopes : group list; sink : sink option }
(* scopes: the groups whose bodies it is running under their deadline,
   innermost first; a passed deadline stops the body at its next wait too.
   sink: where its output goes, None for stdout itself. *)

(* The output of a task of an ordered group, held until the tasks started
   before it have finished and the group's body has ended (section 10). *)
and sink = { sgroup : group; held : Buffer.t; mutable sdone : bool }

(* How a task ended: its value, the panic that ended it, or cancellation. *)
and outcome = Done of value | Failed of string * Span.t | Stopped

(* The tasks of a group, newest first, and whether its deadline has passed. *)
and group = {
  mutable tasks : (task * outcome option ref) list;
  mutable kept : bool list;
  mutable gcancelled : bool;
  gparent : sink option;
  mutable ordered : bool;
  mutable body_done : bool;
  waiting : sink Queue.t;
}
(* kept: per task, newest first like tasks, whether its result goes in the
   group's list (a task that returns () is not kept). gparent: where the
   body's output goes, and the tasks' once released. ordered: the tasks'
   output follows start order (not `interleaved`, and not given up for
   memory or stdin). waiting: the sinks not released yet, in start order;
   once the body has ended, the first of them writes through. *)

(* A persistent map, so updates are cheap and values stay values. Each entry
   remembers when its key was first inserted, for insertion-order iteration. *)
and vmap = { m : (int * value) KM.t; next : int }

(* The names in scope, innermost scope first. A scope is a short list,
   newest binding first, searched in order: a function's scopes hold a
   handful of names, and comparing a few short strings is much cheaper than
   hashing every lookup. *)
and env = scope list

and scope = { mutable vars : (string * value ref) list }

(* A list a variable owns, with room to grow: `push` and `xs[i] = v` change
   it in place instead of copying the list each time. Elements 0 to len - 1
   are the list. *)
and buf = { mutable items : value array; mutable len : int }

exception Return of value
exception Break_
exception Continue_
exception Panic of string * Span.t

(* Ends the program, past every `catch` and task boundary: a panic in a
   callback from C, which cannot unwind through C's frames. *)
exception Fatal of string * Span.t

(* A cancelled task stopping at a wait. `catch` does not catch it. *)
exception Cancelled

type grants = {
  net : string list;
  fs : (string * bool) option; (* realpath, read-only *)
  listen : string list;
  env : string list; (* variable names, or "*" *)
  exec : string list; (* program names, or "*" *)
  foreign : string list; (* C libraries, or "*" *)
  seed : int option; (* makes Rand deterministic *)
  args : string list; (* command-line arguments after `--` *)
}

let no_grants = { net = []; fs = None; listen = []; env = []; exec = []; foreign = []; seed = None; args = [] }

(* Rand: splitmix64, the same generator as the native runtime, so a seed
   gives the same numbers in both backends. *)
let rand_state = ref 0L

let rand_next () =
  rand_state := Int64.add !rand_state 0x9E3779B97F4A7C15L;
  let z = !rand_state in
  let z = Int64.mul (Int64.logxor z (Int64.shift_right_logical z 30)) 0xBF58476D1CE4E5B9L in
  let z = Int64.mul (Int64.logxor z (Int64.shift_right_logical z 27)) 0x94D049BB133111EBL in
  Int64.logxor z (Int64.shift_right_logical z 31)

let seed_rand = function
  | Some s -> rand_state := Int64.of_int s
  | None -> rand_state := Int64.of_float (Unix.gettimeofday () *. 1e6) |> Int64.logxor (Int64.of_int (Unix.getpid ()))

type rt = {
  fns : (string, fn_decl) Hashtbl.t;
  structs : (string, struct_decl) Hashtbl.t;
  enums : (string, enum_decl) Hashtbl.t;
  ctors : (string, string) Hashtbl.t; (* variant -> enum, when unambiguous *)
  out : string -> unit;
  grants : grants;
}

(* ---------- printing and comparison ---------- *)

let fmt_float x =
  if Float.is_integer x && Float.abs x < 1e16 then sprintf "%.1f" x else sprintf "%.15g" x

let cap_name = function
  | CRoot -> "Root" | CNet _ -> "Net" | CFs _ -> "Fs" | CStdout -> "Stdout" | CClock -> "Clock"
  | CListen _ -> "Listen" | CListener _ -> "Listener" | CConn _ -> "Conn"
  | CRand -> "Rand" | CStdin -> "Stdin" | CEnv _ -> "Env" | CExec _ -> "Exec" | CForeign _ -> "Foreign"

let rec key_of = function
  | VInt n -> KInt n
  | VStr s -> KStr s
  | VBytes s -> KBytes s
  | VBool b -> KBool b
  | VStruct (n, fs) -> KStruct (n, List.map (fun (f, v) -> (f, key_of v)) fs)
  | VEnum (e, v, xs) -> KEnum (e, v, List.map key_of xs)
  | _ -> failwith "internal: map key"

let rec val_of_key = function
  | KInt n -> VInt n
  | KStr s -> VStr s
  | KBytes s -> VBytes s
  | KBool b -> VBool b
  | KStruct (n, fs) -> VStruct (n, List.map (fun (f, k) -> (f, val_of_key k)) fs)
  | KEnum (e, v, ks) -> VEnum (e, v, List.map val_of_key ks)
let empty_map = { m = KM.empty; next = 0 }

let map_entries mv =
  KM.bindings mv.m
  |> List.sort (fun (_, (a, _)) (_, (b, _)) -> compare a b)
  |> List.map (fun (k, (_, v)) -> (val_of_key k, v))

let map_put mv k v =
  let k = key_of k in
  match KM.find_opt k mv.m with
  | Some (o, _) -> { mv with m = KM.add k (o, v) mv.m }
  | None -> { m = KM.add k (mv.next, v) mv.m; next = mv.next + 1 }

let rec show ?(top = true) v =
  match v with
  | VInt n -> Int64.to_string n
  | VFloat x -> fmt_float x
  | VBool b -> string_of_bool b
  | VStr s -> if top then s else "\"" ^ String.escaped s ^ "\""
  | VBytes s -> "bytes([" ^ String.concat ", " (List.init (String.length s) (fun i -> string_of_int (Char.code s.[i]))) ^ "])"
  | VUnit -> "()"
  | VList a -> "[" ^ String.concat ", " (Array.to_list (Array.map (show ~top:false) a)) ^ "]"
  | VMap mv ->
    "{" ^ String.concat ", " (List.map (fun (k, v) -> show ~top:false k ^ ": " ^ show ~top:false v) (map_entries mv)) ^ "}"
  | VStruct (n, fs) when Ast.is_tuple_name n -> "(" ^ String.concat ", " (List.map (fun (_, x) -> show ~top:false x) fs) ^ ")"
  | VStruct (n, fs) ->
    n ^ " { " ^ String.concat ", " (List.map (fun (k, x) -> k ^ ": " ^ show ~top:false x) fs) ^ " }"
  | VEnum (_, vn, []) -> vn
  | VEnum (_, vn, args) -> vn ^ "(" ^ String.concat ", " (List.map (show ~top:false) args) ^ ")"
  | VClosure _ | VFn _ | VCtor _ | VPanicFn -> "<fn>"
  | VCap c -> "<" ^ cap_name c ^ ">"
  | VGroup _ -> "<group>"
  | VChan _ -> "<chan>"
  | VChanFn | VBuiltin _ -> "<fn>"

let rec equal a b =
  match (a, b) with
  | VInt x, VInt y -> x = y
  | VFloat x, VFloat y -> x = y
  | VBool x, VBool y -> x = y
  | VStr x, VStr y -> x = y
  | VBytes x, VBytes y -> x = y
  | VUnit, VUnit -> true
  | VList x, VList y -> Array.length x = Array.length y && Array.for_all2 equal x y
  | VStruct (n, xs), VStruct (m, ys) -> n = m && List.for_all2 (fun (_, x) (_, y) -> equal x y) xs ys
  | VEnum (e, v, xs), VEnum (f, w, ys) ->
    e = f && v = w && List.length xs = List.length ys && List.for_all2 equal xs ys
  | VMap a, VMap b ->
    (* the same entries, in any order *)
    KM.cardinal a.m = KM.cardinal b.m
    && KM.for_all (fun k (_, v) -> match KM.find_opt k b.m with Some (_, w) -> equal v w | None -> false) a.m
  | _ -> false

(* (enum, variant) -> position in the declaration, for ordering enum values *)
let variant_pos : (string * string, int) Hashtbl.t = Hashtbl.create 32

(* The order of `<` and `sort()`: item by item, as the native `_cmp` functions.
   Floats use a total order (NaN first) inside containers and in sort. *)
let rec compare_v a b =
  let rec lex xs ys =
    match (xs, ys) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys -> ( match compare_v x y with 0 -> lex xs ys | c -> c)
  in
  match (a, b) with
  | VInt x, VInt y -> compare x y
  | VFloat x, VFloat y -> compare x y
  | VStr x, VStr y | VBytes x, VBytes y -> compare x y
  | VBool x, VBool y -> compare x y
  | VList x, VList y -> lex (Array.to_list x) (Array.to_list y)
  | VStruct (_, xs), VStruct (_, ys) -> lex (List.map snd xs) (List.map snd ys)
  | VEnum (e, v, xs), VEnum (_, w, ys) ->
    let pos v = Option.value ~default:0 (Hashtbl.find_opt variant_pos (e, v)) in
    if v = w then lex xs ys else compare (pos v) (pos w)
  | _ -> 0

let as_bool = function VBool b -> b | _ -> failwith "internal: expected Bool"
let some x = VEnum ("Option", "Some", [ x ])
let none = VEnum ("Option", "None", [])
let ok x = VEnum ("Result", "Ok", [ x ])

let io_result f = function
  | Ok x -> ok (f x)
  | Error (Caps.Denied m) -> VEnum ("Result", "Err", [ VEnum ("IoError", "Denied", [ VStr m ]) ])
  | Error (Caps.Failed m) -> VEnum ("Result", "Err", [ VEnum ("IoError", "Failed", [ VStr m ]) ])

(* ---------- string helpers ---------- *)

let find_sub s sub from =
  let n = String.length s and m = String.length sub in
  let rec go i = if i + m > n then None else if String.sub s i m = sub then Some i else go (i + 1) in
  if m = 0 then Some from else go from

let split_on s sep =
  if sep = "" then List.init (String.length s) (fun i -> String.make 1 s.[i])
  else
    let rec go from acc =
      match find_sub s sep from with
      | Some i -> go (i + String.length sep) (String.sub s from (i - from) :: acc)
      | None -> List.rev (String.sub s from (String.length s - from) :: acc)
    in
    go 0 []

let replace_all s a b = if a = "" then s else String.concat b (split_on s a)

let lines s =
  let ls = String.split_on_char '\n' s in
  let ls = List.map (fun l -> if String.ends_with ~suffix:"\r" l then String.sub l 0 (String.length l - 1) else l) ls in
  match List.rev ls with "" :: rest -> List.rev rest | _ -> ls

(* Decimal only, optional sign, surrounding whitespace allowed (same as the C runtime). *)
(* A decimal float: digits with optional sign, point and exponent; no hex, nan or inf. *)
let parse_float s =
  let s = String.trim s in
  let ok_char c = (c >= '0' && c <= '9') || c = '.' || c = '-' || c = '+' || c = 'e' || c = 'E' in
  if s = "" || (not (String.for_all ok_char s)) || not (String.exists (fun c -> c >= '0' && c <= '9') s) then None
  else float_of_string_opt s

(* The UTF-8 encoding of a code point; U+FFFD for anything that is not one. *)
let utf8_of_code n =
  let n = if n < 0 || n > 0x10FFFF || (n >= 0xD800 && n <= 0xDFFF) then 0xFFFD else n in
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int n);
  Buffer.contents b

(* The length of the UTF-8 character at s.[i], or 1 for a byte that does not
   start a well-formed one (as the native w_utf8_len). *)
let utf8_len s i =
  let n = String.length s in
  let b = Char.code s.[i] in
  let byte j = Char.code s.[j] in
  let cont k lo hi =
    if i + k > n then 1
    else if byte (i + 1) < lo || byte (i + 1) > hi then 1
    else if List.for_all (fun j -> byte (i + j) >= 0x80 && byte (i + j) <= 0xBF) (List.init (k - 2) (fun j -> j + 2)) then k
    else 1
  in
  if b < 0x80 then 1
  else if b >= 0xC2 && b <= 0xDF then cont 2 0x80 0xBF
  else if b >= 0xE0 && b <= 0xEF then cont 3 (if b = 0xE0 then 0xA0 else 0x80) (if b = 0xED then 0x9F else 0xBF)
  else if b >= 0xF0 && b <= 0xF4 then cont 4 (if b = 0xF0 then 0x90 else 0x80) (if b = 0xF4 then 0x8F else 0xBF)
  else 1

let chars s =
  let rec go i acc = if i >= String.length s then List.rev acc else let k = utf8_len s i in go (i + k) (String.sub s i k :: acc) in
  go 0 []

let parse_decimal s =
  let s = String.trim s in
  let n = String.length s in
  let start = if n > 0 && (s.[0] = '-' || s.[0] = '+') then 1 else 0 in
  if n = start then None
  else if String.exists (fun c -> c < '0' || c > '9') (String.sub s start (n - start)) then None
  else Int64.of_string_opt s

let vint n = VInt (Int64.of_int n)

(* An Int used as a count or position, clamped to 0..n. *)
let clamp (k : int64) n = if k < 0L then 0 else if k > Int64.of_int n then n else Int64.to_int k

(* Float to Int as the native w_f2i: NaN is 0, out of range saturates. *)
let float_to_int x =
  if Float.is_nan x then 0L
  else if x >= 9.2e18 then Int64.max_int
  else if x <= -9.2e18 then Int64.min_int
  else Int64.of_float x

let strs l = VList (Array.of_list (List.map (fun s -> VStr s) l))

(* ---------- evaluation ---------- *)

let scope () = { vars = [] }
let bind s n v = s.vars <- (n, ref v) :: s.vars
let scope_of n v = { vars = [ (n, ref v) ] }

(* names are interned by the lexer, so a match is usually the same string;
   the length check skips most mismatches without comparing text *)
let same_name k n = k == n || (String.length k = String.length n && String.equal k n)

let rec lookup n = function
  | [] -> None
  | (k, v) :: r -> if same_name k n then Some v else lookup n r

let rec field_of name = function
  | [] -> raise Not_found
  | (k, v) :: r -> if same_name k name then v else field_of name r

let rec find_var env n =
  match env with
  | [] -> None
  | s :: r -> ( match lookup n s.vars with Some v -> Some v | None -> find_var r n)

(* A variable's value, ready to be used anywhere: a list it was building
   becomes an ordinary list (and stays one in the cell, so the next change
   copies it first: other holders of the value never see a change). *)
let settle r =
  match !r with
  | VBuf b ->
    let v = VList (if Array.length b.items = b.len then b.items else Array.sub b.items 0 b.len) in
    r := v;
    v
  | v -> v

(* The list a variable is building, read without settling it: for reads
   that cannot keep the list (an element, its length). *)
let peek env (e : Ast.expr) = match e.edesc with EVar n -> (match find_var env n with Some { contents = VBuf b } -> Some b | _ -> None) | _ -> None

(* The variable's cell as a list it can change in place, made so if needed. *)
let own r =
  match !r with
  | VBuf b -> b
  | VList arr ->
    let n = Array.length arr in
    let b = { items = Array.make (max 8 (2 * n)) VUnit; len = n } in
    Array.blit arr 0 b.items 0 n;
    r := VBuf b;
    b
  | _ -> failwith "internal: not a list"

(* Closures capture by value: copy every visible binding, the innermost of
   each name. *)
let snapshot env =
  let out = ref [] in
  List.iter (fun s -> List.iter (fun (k, v) -> if lookup k !out = None then out := (k, ref (settle v)) :: !out) s.vars) env;
  { vars = List.rev !out }

let binop op a b sp =
  match (op, a, b) with
  (* integers first, compared directly rather than through compare_v *)
  | Lt, VInt x, VInt y -> VBool (Int64.compare x y < 0)
  | Le, VInt x, VInt y -> VBool (Int64.compare x y <= 0)
  | Gt, VInt x, VInt y -> VBool (Int64.compare x y > 0)
  | Ge, VInt x, VInt y -> VBool (Int64.compare x y >= 0)
  | Eq, VInt x, VInt y -> VBool (Int64.equal x y)
  | Ne, VInt x, VInt y -> VBool (not (Int64.equal x y))
  | Add, VInt x, VInt y -> VInt (Int64.add x y)
  | Sub, VInt x, VInt y -> VInt (Int64.sub x y)
  | Mul, VInt x, VInt y -> VInt (Int64.mul x y)
  | (Div | Mod), VInt _, VInt 0L -> raise (Panic ("division by zero", sp))
  (* as the native w_div/w_mod: dividing by -1 wraps instead of trapping *)
  | Div, VInt x, VInt -1L -> VInt (Int64.neg x)
  | Mod, VInt _, VInt -1L -> VInt 0L
  | Div, VInt x, VInt y -> VInt (Int64.div x y)
  | Mod, VInt x, VInt y -> VInt (Int64.rem x y)
  | Add, VFloat x, VFloat y -> VFloat (x +. y)
  | Sub, VFloat x, VFloat y -> VFloat (x -. y)
  | Mul, VFloat x, VFloat y -> VFloat (x *. y)
  | Div, VFloat x, VFloat y -> VFloat (x /. y)
  | Add, VStr x, VStr y -> VStr (x ^ y)
  | Add, VBytes x, VBytes y -> VBytes (x ^ y)
  | Lt, VFloat x, VFloat y -> VBool (x < y)
  | Le, VFloat x, VFloat y -> VBool (x <= y)
  | Gt, VFloat x, VFloat y -> VBool (x > y)
  | Ge, VFloat x, VFloat y -> VBool (x >= y)
  | Range, VInt lo, VInt hi ->
    if hi <= lo then VList [||]
    else if Int64.sub hi lo < 0L || Int64.sub hi lo > Int64.of_int Sys.max_array_length then raise (Panic ("out of memory", sp))
    else VList (Array.init (Int64.to_int (Int64.sub hi lo)) (fun i -> VInt (Int64.add lo (Int64.of_int i))))
  | Eq, _, _ -> VBool (equal a b)
  | Ne, _, _ -> VBool (not (equal a b))
  | Lt, _, _ -> VBool (compare_v a b < 0)
  | Le, _, _ -> VBool (compare_v a b <= 0)
  | Gt, _, _ -> VBool (compare_v a b > 0)
  | Ge, _, _ -> VBool (compare_v a b >= 0)
  | _ -> failwith "internal: bad operands"

(* ---------- cooperative scheduler ----------
   Tasks are coroutines built on OCaml 5 effects. A task that cannot go on
   performs Block with the condition it waits for, optionally a socket to
   poll and a time to wake up at; the scheduler resumes the first task whose
   condition holds, and when none can go on it sleeps until a socket is
   ready or the earliest wake-up time. *)

type io_wait = (sock * bool) option (* socket, true = wait for writing *)
type _ Effect.t += Block : (unit -> bool) * io_wait * float option -> unit Effect.t

let runq : ((unit -> bool) * io_wait * float option * (unit -> unit)) Queue.t = Queue.create ()
let io_epoch = ref 0 (* advances after each round of waiting for I/O *)

let now () = Unix.gettimeofday ()

(* The task running now; main and tests run as tasks nobody cancels. *)
let current = ref { cancelled = false; scopes = []; sink = None }

(* Wait until [cond] holds. A cancelled task stops instead (raising
   Cancelled) unless [cancellable] is false, which is only for joining the
   tasks of a group that is being abandoned. As natively, an operation that
   can go on at once does not look at the cancel flag. *)
let stopped t = t.cancelled || List.exists (fun g -> g.gcancelled) t.scopes

let wait_until ?io ?at ?(cancellable = true) cond =
  if not (cond ()) then begin
    let t = !current in
    if cancellable && stopped t then raise Cancelled;
    let c = if cancellable then fun () -> cond () || stopped t else cond in
    Effect.perform (Block (c, io, at));
    if cancellable && not (cond ()) then raise Cancelled
  end

(* The POSIX number of an OCaml signal, for exit statuses (128 + n). OCaml
   numbers signals itself; Sys.signal_to_int only exists from 5.4. *)
let signal_number s =
  let table =
    [ (Sys.sighup, 1); (Sys.sigint, 2); (Sys.sigquit, 3); (Sys.sigill, 4); (Sys.sigabrt, 6); (Sys.sigfpe, 8);
      (Sys.sigkill, 9); (Sys.sigsegv, 11); (Sys.sigpipe, 13); (Sys.sigalrm, 14); (Sys.sigterm, 15) ]
  in
  match List.assoc_opt s table with Some n -> n | None -> if s > 0 then s else 0

let rec run_fiber t (f : unit -> unit) =
  Effect.Deep.match_with (fun () -> current := t; f ()) ()
    { retc = (fun () -> ());
      exnc = raise;
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Block (cond, io, at) ->
            Some
              (fun (k : (a, unit) Effect.Deep.continuation) ->
                Queue.push (cond, io, at, fun () -> current := t; Effect.Deep.continue k ()) runq)
          | _ -> None) }

let spawn_fiber t f = Queue.push ((fun () -> true), None, None, fun () -> run_fiber t f) runq

(* The deadline passed: cancel every task still running, and every task
   started from now on. *)
let cancel_group g =
  g.gcancelled <- true;
  List.iter (fun (t, r) -> if !r = None then t.cancelled <- true) g.tasks

let group_finished g = List.for_all (fun (_, r) -> !r <> None) g.tasks

(* ---------- output in start order ----------
   A task of an ordered group writes into its sink until it is the first
   unreleased task and the body has ended; then its held output goes to the
   group's parent and it writes through. Held output is limited to
   [hold_limit] bytes for the whole program: past it, the group that would
   hold more gives up order, releasing what it holds in start order. *)

let hold_limit = 4 * 1024 * 1024
let held_bytes = ref 0

(* ordered groups still running, newest (innermost) first *)
let live_groups : group list ref = ref []

let through k =
  let g = k.sgroup in
  (not g.ordered) || (g.body_done && match Queue.peek_opt g.waiting with Some f -> f == k | None -> false)

let rec put (out : string -> unit) sk s =
  match sk with
  | None -> out s
  | Some k ->
    if through k then put out k.sgroup.gparent s
    else if !held_bytes + String.length s > hold_limit then begin
      spill out k.sgroup;
      put out k.sgroup.gparent s
    end
    else begin
      Buffer.add_string k.held s;
      held_bytes := !held_bytes + String.length s
    end

and release out g k =
  if Buffer.length k.held > 0 then begin
    let s = Buffer.contents k.held in
    Buffer.reset k.held;
    held_bytes := !held_bytes - String.length s;
    put out g.gparent s
  end

(* give up order: release everything held, in start order *)
and spill out g =
  g.ordered <- false;
  Queue.iter (release out g) g.waiting;
  Queue.clear g.waiting

(* release what can go now: the finished tasks at the front, and the held
   output of the first unfinished one, which then writes through *)
let advance out g =
  if g.ordered && g.body_done then begin
    let rec go () =
      match Queue.peek_opt g.waiting with
      | Some k ->
        release out g k;
        if k.sdone then begin
          ignore (Queue.pop g.waiting);
          go ()
        end
      | None -> ()
    in
    go ()
  end

(* Reading stdin: a prompt the task printed must show, so every group that
   holds this task's output gives up order. *)
let rec unhold out = function
  | None -> ()
  | Some k ->
    if not (through k) then spill out k.sgroup;
    unhold out k.sgroup.gparent

(* The program is ending with a panic or a deadlock: release everything
   still held, innermost groups first, before the message. *)
let release_all out =
  List.iter (fun g -> if g.ordered then spill out g) !live_groups;
  live_groups := [];
  held_bytes := 0

(* ---------- channels ---------- *)

(* The next waiter that can still be woken; claim it for operation [w.idx]. *)
let rec pop_live q =
  match Queue.take_opt q with
  | None -> None
  | Some w when w.claim.fired = -1 -> w.claim.fired <- w.idx; Some w
  | Some _ -> pop_live q

let remove_waiter q w =
  let keep = Queue.fold (fun acc x -> if x == w then acc else x :: acc) [] q in
  Queue.clear q;
  List.iter (fun x -> Queue.push x q) (List.rev keep)

(* Try a send without waiting: handed to a waiting receiver, or buffered. *)
let try_send ch v =
  if ch.closed then `Closed
  else
    match pop_live ch.rq with
    | Some r -> r.v <- v; r.ok <- true; `Done
    | None -> if Queue.length ch.q < ch.cap then (Queue.push v ch.q; `Done) else `Blocked

(* Try a receive without waiting: from the buffer (then a waiting sender
   refills it), or straight from a waiting sender (unbuffered). *)
let try_recv ch =
  if not (Queue.is_empty ch.q) then begin
    let v = Queue.pop ch.q in
    (match pop_live ch.sq with Some s -> Queue.push s.v ch.q; s.ok <- true | None -> ());
    `Got v
  end
  else
    match pop_live ch.sq with
    | Some s -> s.ok <- true; `Got s.v
    | None -> if ch.closed then `Closed else `Blocked

(* Wait until a waker claims [cl], or until [at]. A cancelled task gives up:
   its waiters become stale and are taken off their queues. *)
let wait_claim ?at cl (queued : (waiter Queue.t * waiter) list) =
  (try wait_until ?at (fun () -> cl.fired <> -1 || match at with Some t -> now () >= t | None -> false)
   with Cancelled ->
     cl.fired <- -2;
     List.iter (fun (q, w) -> remove_waiter q w) queued;
     raise Cancelled);
  (* woken: take back the waiters nobody claimed *)
  List.iter (fun (q, w) -> if cl.fired <> w.idx then remove_waiter q w) queued

let chan_send ch v sp =
  match try_send ch v with
  | `Done -> ()
  | `Closed -> raise (Panic ("send on a closed channel", sp))
  | `Blocked ->
    let w = { claim = { fired = -1 }; idx = 0; v; ok = false } in
    Queue.push w ch.sq;
    wait_claim w.claim [ (ch.sq, w) ];
    if not w.ok then raise (Panic ("send on a closed channel", sp))

let chan_recv ch =
  match try_recv ch with
  | `Got v -> Some v
  | `Closed -> None
  | `Blocked ->
    let w = { claim = { fired = -1 }; idx = 0; v = VUnit; ok = false } in
    Queue.push w ch.rq;
    wait_claim w.claim [ (ch.rq, w) ];
    if w.ok then Some w.v else None

(* Waiting senders and receivers are woken with nothing: senders panic,
   receivers get None. *)
let chan_close ch =
  ch.closed <- true;
  let rec drain q = match pop_live q with Some w -> w.ok <- false; drain q | None -> () in
  drain ch.rq;
  drain ch.sq

(* Clock.sleep: wait without holding up other tasks. *)
let sleep_ms (ms : int64) =
  if ms > 0L then begin
    let at = now () +. (Int64.to_float (min ms 1_000_000_000_000L) /. 1000.) in
    wait_until ~at (fun () -> now () >= at)
  end

(* Run tasks until [finished ()]. *)
let rec schedule finished sp =
  if not (finished ()) then begin
    let n = Queue.length runq in
    let rec pick i =
      if i = n then false
      else
        let ((cond, _, _, k) as entry) = Queue.pop runq in
        if cond () then (k (); true) else (Queue.push entry runq; pick (i + 1))
    in
    if not (pick 0) then begin
      let waits =
        Queue.fold (fun acc (_, io, _, _) -> match io with Some ((s : sock), w) when not s.shut -> (s.fd, w) :: acc | _ -> acc) [] runq
      in
      let wake = Queue.fold (fun acc (_, _, at, _) -> match (at, acc) with Some t, Some u -> Some (Float.min t u) | Some t, None -> Some t | None, _ -> acc) None runq in
      if waits = [] && wake = None then raise (Panic ("all tasks are blocked (deadlock)", sp));
      let timeout = match wake with Some t -> Float.max 0. (t -. now ()) | None -> -1. in
      (* select() cannot watch descriptors past FD_SETSIZE; then just pause
         briefly. Either way, waiting tasks retry their operation. *)
      let small = List.for_all (fun (fd, _) -> (Obj.magic fd : int) < 1000) waits in
      (if small then
         let rd = List.filter_map (fun (fd, w) -> if w then None else Some fd) waits in
         let wr = List.filter_map (fun (fd, w) -> if w then Some fd else None) waits in
         ignore (try Unix.select rd wr [] timeout with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], []))
       else Unix.sleepf (if timeout < 0. then 0.001 else Float.min timeout 0.001));
      incr io_epoch
    end;
    schedule finished sp
  end

(* ---------- sockets ----------
   Non-blocking sockets; a task that would block waits in the scheduler. *)
module Sock = struct
  (* retry after the scheduler's next round of waiting for I/O, or as soon as the socket is closed *)
  let wait sk write =
    let e = !io_epoch in
    wait_until ~io:(sk, write) (fun () -> sk.shut || !io_epoch > e)

  (* Run [op] as this socket's reader (or writer): [op claim] calls [claim ()]
     before each wait (a writer also before its first byte), which is false
     if another task holds the slot; the slot is held from then until [op]
     returns. *)
  let one_at_a_time sk write op =
    let held = ref false in
    let claim () =
      if !held then true
      else if (if write then sk.writing else sk.reading) then false
      else begin
        if write then sk.writing <- true else sk.reading <- true;
        held := true;
        true
      end
    in
    let release () = if !held then (if write then sk.writing <- false else sk.reading <- false) in
    match op claim with
    | r -> release (); r
    | exception e -> release (); raise e

  let busy what = Error (Caps.Failed ("another task is already " ^ what))

  let failed e = Error (Caps.Failed (Unix.error_message e))
  let closed_err what = Error (Caps.Failed (what ^ " is closed"))

  (* "host:port" with a host of [A-Za-z0-9.-] and a port 0..65535, or
     "[ipv6]:port" with the host kept in its brackets *)
  let port_of p = if p <> "" && String.length p <= 5 && String.for_all (fun c -> c >= '0' && c <= '9') p && int_of_string p <= 65535 then Some (int_of_string p) else None

  let parse_addr addr =
    if String.length addr > 0 && addr.[0] = '[' then
      match String.index_opt addr ']' with
      | Some k when k >= 2 && k + 1 < String.length addr && addr.[k + 1] = ':' -> (
        let inner = String.sub addr 1 (k - 1) in
        match (Unix.inet_addr_of_string inner, port_of (String.sub addr (k + 2) (String.length addr - k - 2))) with
        | ip, Some p when Unix.domain_of_sockaddr (Unix.ADDR_INET (ip, 0)) = Unix.PF_INET6 -> Some (String.lowercase_ascii (String.sub addr 0 (k + 1)), p)
        | _ -> None
        | exception Failure _ -> None)
      | _ -> None
    else
    match String.split_on_char ':' addr with
    | [ h; p ]
      when h <> "" && String.for_all (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '.' || c = '-') h
           && p <> "" && String.length p <= 5 && String.for_all (fun c -> c >= '0' && c <= '9') p
           && int_of_string p <= 65535 ->
      Some (String.lowercase_ascii h, int_of_string p)
    | _ -> None

  (* The addresses to try for an outgoing connection, IPv4 and IPv6, in the
     resolver's order; a bracketed host is an IPv6 address. *)
  let lookup h port =
    let h = if h = "localhost" then "127.0.0.1" else if h.[0] = '[' then String.sub h 1 (String.length h - 2) else h in
    Unix.getaddrinfo h (string_of_int port) [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
    |> List.filter_map (fun a -> match a.Unix.ai_addr with Unix.ADDR_INET _ as sa -> Some (a.Unix.ai_family, sa) | _ -> None)

  let resolve h =
    if h = "localhost" then Some Unix.inet_addr_loopback
    else
      try Some (Unix.inet_addr_of_string h)
      with Failure _ -> ( try Some (Unix.gethostbyname h).Unix.h_addr_list.(0) with Not_found | Invalid_argument _ -> None)

  let listen allowed addr =
    match parse_addr addr with
    | None -> Error (Caps.Failed "address must be HOST:PORT")
    | Some (h, port) ->
      if not (Caps.permits allowed ~host:h ~port) then Error (Caps.Denied (Printf.sprintf "listen: %s:%d is not granted" h port))
      else if h.[0] = '[' then (
        (* IPv6, and IPv4 too through mapped addresses *)
        let ip = Unix.inet_addr_of_string (String.sub h 1 (String.length h - 2)) in
        let fd = Unix.socket Unix.PF_INET6 Unix.SOCK_STREAM 0 in
        try
          Unix.setsockopt fd Unix.SO_REUSEADDR true;
          Unix.setsockopt fd Unix.IPV6_ONLY false;
          Unix.bind fd (Unix.ADDR_INET (ip, port));
          Unix.listen fd 4096;
          Unix.set_nonblock fd;
          let p = match Unix.getsockname fd with Unix.ADDR_INET (_, p) -> p | _ -> port in
          Ok ({ fd; shut = false; reading = false; writing = false; tls = Tls.Plain }, p)
        with Unix.Unix_error (e, _, _) -> Unix.close fd; failed e)
      else (
        match resolve h with
        | None -> Error (Caps.Failed ("cannot resolve " ^ h))
        | Some ip -> (
          let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
          try
            Unix.setsockopt fd Unix.SO_REUSEADDR true;
            Unix.bind fd (Unix.ADDR_INET (ip, port));
            Unix.listen fd 4096;
            Unix.set_nonblock fd;
            let p = match Unix.getsockname fd with Unix.ADDR_INET (_, p) -> p | _ -> port in
            Ok ({ fd; shut = false; reading = false; writing = false; tls = Tls.Plain }, p)
          with Unix.Unix_error (e, _, _) -> Unix.close fd; failed e))

  let accept sk =
    one_at_a_time sk false (fun claim ->
        let rec go () =
          if sk.shut then closed_err "listener"
          else
            match Unix.accept ~cloexec:true sk.fd with
            | c, _ -> (
              Unix.set_nonblock c;
              let conn = { fd = c; shut = false; reading = false; writing = false; tls = Tls.Plain } in
              match sk.tls with
              | Tls.Server ctx -> (
                match Tls.server ctx c with
                | s -> conn.tls <- Tls.Session s; Ok conn
                | exception Failure m -> Unix.close c; Error (Caps.Failed m))
              | _ -> Ok conn)
            | exception Unix.Unix_error ((Unix.EINTR | Unix.ECONNABORTED), _, _) -> go ()
            | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
              if claim () then (wait sk false; go ()) else busy "accepting on this listener"
            | exception Unix.Unix_error (e, _, _) -> failed e
        in
        go ())

  let connect allowed addr =
    match parse_addr addr with
    | None -> Error (Caps.Failed "address must be HOST:PORT")
    | Some (h, port) -> (
      if not (Caps.permits allowed ~host:h ~port) then Error (Caps.Denied (Printf.sprintf "net: %s:%d is not granted" h port))
      else
        match lookup h port with
        | [] -> Error (Caps.Failed ("cannot resolve " ^ h))
        | addrs ->
          (* each address in turn; the last failure is the error *)
          let rec try_ = function
            | [] -> assert false
            | (dom, sa) :: rest -> (
              let sk = { fd = Unix.socket dom Unix.SOCK_STREAM 0; shut = false; reading = false; writing = false; tls = Tls.Plain } in
              Unix.set_nonblock sk.fd;
              (* waits can be woken by any socket, so retry until this one says it is connected *)
              let rec go () =
                match Unix.connect sk.fd sa with
                | () -> Ok sk
                | exception Unix.Unix_error (Unix.EISCONN, _, _) -> Ok sk
                | exception Unix.Unix_error ((Unix.EINPROGRESS | Unix.EALREADY | Unix.EAGAIN | Unix.EINTR), _, _) -> (
                  (match wait sk true with () -> () | exception e -> Unix.close sk.fd; raise e);
                  match Unix.getsockopt_error sk.fd with
                  | Some e -> Unix.close sk.fd; failed e
                  | None -> go ())
                | exception Unix.Unix_error (e, _, _) -> Unix.close sk.fd; failed e
              in
              match go () with
              | Error _ as e when rest = [] -> e
              | Error _ -> try_ rest
              | ok -> ok)
          in
          try_ addrs)

  (* One TLS read or write step: Ok n, or Error (Some write) when the socket
     must be ready that way first, or Error None with the message in [m]. *)
  let tls_step s op b off len =
    match Tls.step s op b off len with
    | n when n >= 0 -> `Done n
    | n -> `Wait (n = Tls.want_write)
    | exception Failure m -> `Failed m

  let read sk =
    one_at_a_time sk false (fun claim ->
        let b = Bytes.create 65536 in
        let rec go () =
          if sk.shut then closed_err "connection"
          else
            match sk.tls with
            | Tls.Session s -> (
              match tls_step s 0 b 0 65536 with
              | `Done n -> Ok (Bytes.sub_string b 0 n)
              | `Wait w -> if claim () then (wait sk w; go ()) else busy "reading this connection"
              | `Failed m -> Error (Caps.Failed m))
            | _ ->
            match Unix.read sk.fd b 0 65536 with
            | n -> Ok (Bytes.sub_string b 0 n)
            | exception Unix.Unix_error (Unix.EINTR, _, _) -> go ()
            | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
              if claim () then (wait sk false; go ()) else busy "reading this connection"
            | exception Unix.Unix_error (e, _, _) -> failed e
        in
        go ())

  let write sk s =
    one_at_a_time sk true (fun claim ->
        let rec go off =
          if sk.shut then closed_err "connection"
          else if off >= String.length s then Ok ()
          else
            match sk.tls with
            | Tls.Session t -> (
              match tls_step t 1 (Bytes.unsafe_of_string s) off (min (String.length s - off) 0x3fffffff) with
              | `Done n -> go (off + n)
              | `Wait w -> if claim () then (wait sk w; go off) else busy "writing this connection"
              | `Failed m -> Error (Caps.Failed m))
            | _ ->
            match Unix.single_write_substring sk.fd s off (String.length s - off) with
            | n -> go (off + n)
            | exception Unix.Unix_error (Unix.EINTR, _, _) -> go off
            | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
              if claim () then (wait sk true; go off) else busy "writing this connection"
            | exception Unix.Unix_error (e, _, _) -> failed e
        in
        (* a writer holds the slot from its first byte: a second writer that
           sent part of its data before finding the slot taken would mix
           the two into the stream *)
        if claim () then go 0 else busy "writing this connection")

  let close sk =
    if not sk.shut then begin
      sk.shut <- true;
      (match sk.tls with
       | Tls.Session s -> Tls.close s
       | Tls.Server c -> Tls.ctx_free c
       | Tls.Plain -> ());
      try Unix.close sk.fd with Unix.Unix_error _ -> ()
    end

  (* Net.connect_tls: connect, then run the client handshake, which checks
     the server's certificate against the host. *)
  let connect_tls allowed addr =
    match parse_addr addr with
    | None -> Error (Caps.Failed "address must be HOST:PORT")
    | Some (h, _) -> (
      match connect allowed addr with
      | Error e -> Error e
      | Ok sk -> (
        let h = if h.[0] = '[' then String.sub h 1 (String.length h - 2) else h in
        match Tls.client sk.fd h with
        | exception Failure m -> close sk; Error (Caps.Failed m)
        | s ->
          sk.tls <- Tls.Session s;
          let rec go () =
            match tls_step s 2 Bytes.empty 0 0 with
            | `Done _ -> Ok sk
            | `Wait w -> (match wait sk w with () -> go () | exception e -> close sk; raise e)
            | `Failed m -> close sk; Error (Caps.Failed m)
          in
          go ()))

  (* Listen.on_tls: a listener whose connections speak TLS with this
     certificate and key (PEM text). *)
  let listen_tls allowed addr cert key =
    match listen allowed addr with
    | Error e -> Error e
    | Ok (sk, p) -> (
      match Tls.server_ctx cert key with
      | c -> sk.tls <- Tls.Server c; Ok (sk, p)
      | exception Failure m -> close sk; Error (Caps.Failed m))

  (* Net.get: one plain HTTP/1.1 GET over a socket, read until the server
     closes. Redirects are returned, not followed. *)
  let http_get allowed url =
    match Caps.parse_url url with
    | Error m -> Error (Caps.Failed m)
    | Ok u ->
      if not (Caps.permits allowed ~host:u.host ~port:u.port) then
        Error (Caps.Denied (Printf.sprintf "net: %s:%d is not granted" u.host u.port))
      else
        match (if u.https then connect_tls else connect) allowed (Printf.sprintf "%s:%d" u.host u.port) with
        | Error e -> Error e
        | Ok sk ->
          let result =
            match write sk (Caps.http_request u) with
            | Error e -> Error e
            | Ok () ->
              let b = Buffer.create 4096 in
              let rec go () =
                match read sk with
                | Error (Caps.Failed m) when m = Caps.tls_cut -> Caps.parse_http ~cut:true (Buffer.contents b)
                | Error e -> Error e
                | Ok "" -> Caps.parse_http (Buffer.contents b)
                | Ok s ->
                  Buffer.add_string b s;
                  if Buffer.length b > Caps.max_response then Error (Caps.Failed "net: the response is larger than 64 MiB") else go ()
              in
              go ()
          in
          close sk;
          result
end

(* Foreign functions: C libraries loaded with dlopen, called through the
   generic stub in ffi_stubs.c. The native backend links the same libraries
   and calls each function through its prototype. *)
module Foreign = struct
  type carg = AInt of int64 | AFloat of float | APtr of string | ABlob of string | AOut

  external dl_open : string -> nativeint * string = "vela_ffi_open"
  external dl_sym : nativeint -> string -> nativeint = "vela_ffi_sym"
  external call_c : nativeint -> carg array -> float array -> int -> bool -> int * int64 array * int64 array = "vela_ffi_call"
  external call_var : nativeint -> int -> carg array -> float array -> int -> int * int64 array * int64 array = "vela_ffi_call_var"
  external abi_kind : unit -> int = "vela_ffi_abi"
  external cb_take : bool -> int = "vela_ffi_cb_take"
  external cb_free : bool -> int -> unit = "vela_ffi_cb_free"
  external cb_addr : bool -> int -> nativeint = "vela_ffi_cb_addr"
  external cb_fail : unit -> unit = "vela_ffi_cb_fail"
  external cstr : int64 -> string = "vela_ffi_cstr"
  external peek : int64 -> int -> string = "vela_ffi_peek"
  external sandbox_enter : string -> bool -> bool -> bool -> int = "vela_sandbox_enter"

  let permits allowed lib = List.mem "*" allowed || List.mem lib allowed
  let only allowed l = if (l = "*" && List.mem "*" allowed) || (l <> "*" && permits allowed l) then [ l ] else []

  let libs : (string, nativeint) Hashtbl.t = Hashtbl.create 4
  let syms : (string * string, nativeint) Hashtbl.t = Hashtbl.create 16

  (* lib<name>.dylib on macOS, lib<name>.so elsewhere; "c" is the program itself *)
  let load lib : (nativeint, string) result =
    match Hashtbl.find_opt libs lib with
    | Some h -> Ok h
    | None ->
      let try_open names =
        List.fold_left
          (fun acc n -> match acc with Ok _ -> acc | Error _ -> let h, e = dl_open n in if h <> 0n then Ok h else Error e)
          (Error "") names
      in
      let r = if lib = "c" then try_open [ "" ] else try_open [ "lib" ^ lib ^ ".dylib"; "lib" ^ lib ^ ".so" ] in
      (match r with Ok h -> Hashtbl.replace libs lib h | Error _ -> ());
      r

  (* Resolve the foreign functions of granted libraries before the program
     starts, so a missing library or symbol is an error up front, as a link
     error is natively. A library the grants do not cover is never loaded
     (loading runs its initialisers): calls into it panic before reaching it. *)
  let resolve granted (items : item list) : (unit, string * Span.t) result =
    List.fold_left
      (fun acc it ->
        match (acc, it) with
        | Ok (), IFn ({ fn_foreign = Some { flib = lib; fsym = sym; ftrusted }; _ } as d) when lib <> "vela" && (ftrusted || permits granted lib) -> (
          match load lib with
          | Error e -> Error (sprintf "foreign: cannot load library `%s`: %s" lib e, d.fn_span)
          | Ok h ->
            let p = dl_sym (if lib = "c" then 0n else h) sym in
            if p = 0n then Error (sprintf "foreign: library `%s` has no function `%s`" lib sym, d.fn_span)
            else (Hashtbl.replace syms (lib, sym) p; Ok ()))
        | _ -> acc)
      (Ok ()) items

  let handle v = VStruct ("Handle", [ ("addr", VInt v) ])

  let copy allowed p n sp =
    if allowed = [] then raise (Panic ("foreign: no library is granted, so C memory cannot be read", sp));
    if n < 0L then raise (Panic (sprintf "foreign.copy: negative length %Ld" n, sp));
    if p = 0L && n > 0L then raise (Panic ("foreign.copy: null pointer", sp));
    if n > Int64.of_int Sys.max_string_length then raise (Panic (sprintf "foreign.copy: length %Ld is too large" n, sp));
    VBytes (peek p (Int64.to_int n))

  (* The runtime's handle table (Ffi.runtime_fns): id -> pointer, uses in
     progress, closed. Ids are random so they cannot be guessed. *)
  type entry = { ptr : int64; mutable uses : int; mutable closed : bool }
  let handles : (int64, entry) Hashtbl.t = Hashtbl.create 8
  let id_rng = lazy (Random.State.make_self_init ())

  let runtime_helper sym args sp =
    let ptr_of = function
      | Some e -> e.ptr
      | None -> 0L
    in
    match (sym, args) with
    | "vela_copy", [ VStruct (_, [ (_, VInt p) ]); VInt n ] -> copy [ "vela" ] p n sp
    | "vela_handle_new", [ VStruct (_, [ (_, VInt p) ]) ] ->
      let rec fresh () =
        let id = Int64.logand (Random.State.int64 (Lazy.force id_rng) Int64.max_int) 0x3FFFFFFFFFFFFFFFL in
        if id = 0L || Hashtbl.mem handles id then fresh () else id
      in
      let id = fresh () in
      Hashtbl.replace handles id { ptr = p; uses = 0; closed = false };
      VInt id
    | "vela_handle_acquire", [ VInt id ] -> (
      match Hashtbl.find_opt handles id with
      | Some e when not e.closed -> e.uses <- e.uses + 1; handle e.ptr
      | _ -> handle 0L)
    | "vela_handle_release", [ VInt id ] -> (
      match Hashtbl.find_opt handles id with
      | Some e when e.uses > 0 ->
        e.uses <- e.uses - 1;
        if e.closed && e.uses = 0 then (Hashtbl.remove handles id; handle (ptr_of (Some e))) else handle 0L
      | _ -> handle 0L)
    | "vela_handle_close", [ VInt id ] -> (
      match Hashtbl.find_opt handles id with
      | Some e when not e.closed ->
        e.closed <- true;
        if e.uses = 0 then (Hashtbl.remove handles id; handle e.ptr) else handle 0L
      | _ -> handle 0L)
    | _ -> failwith ("internal: runtime helper " ^ sym)

  (* ---- the calling convention (see the comment at the top of ffi_stubs.c) ---- *)

  type abi = X64 | Arm64 | Apple

  let abi = lazy (match abi_kind () with 0 -> X64 | 1 -> Arm64 | _ -> Apple)

  let int32 n = Int64.of_int32 (Int64.to_int32 n)

  let int32_arg n what sp =
    if n < -2147483648L || n > 2147483647L then raise (Panic (sprintf "foreign: %Ld does not fit in Int32 (%s)" n what, sp));
    n

  (* A struct's bytes, at C's layout. *)
  let struct_bytes name (fs : (string * Ffi.cty) list) v sp =
    let offs, size = Ffi.layout fs in
    let b = Bytes.make size '\000' in
    let vals = match v with VStruct (_, vs) -> vs | _ -> failwith "internal: foreign struct" in
    List.iter2
      (fun (f, t) o ->
        match (t, List.assoc f vals) with
        | Ffi.Int, VInt n -> Bytes.set_int64_le b o n
        | Ffi.Int32, VInt n -> Bytes.set_int32_le b o (Int64.to_int32 (int32_arg n (sprintf "field `%s` of `%s`" f name) sp))
        | Ffi.Bool, VBool x -> Bytes.set_int32_le b o (if x then 1l else 0l)
        | Ffi.Float, VFloat x -> Bytes.set_int64_le b o (Int64.bits_of_float x)
        | Ffi.Handle, VStruct (_, [ (_, VInt n) ]) -> Bytes.set_int64_le b o n
        | _ -> failwith "internal: foreign struct field")
      fs offs;
    b

  (* A struct from C's bytes. *)
  let struct_of_bytes name (fs : (string * Ffi.cty) list) b =
    let offs, _ = Ffi.layout fs in
    VStruct
      ( name,
        List.map2
          (fun (f, t) o ->
            ( f,
              match t with
              | Ffi.Int32 -> VInt (Int64.of_int32 (Bytes.get_int32_le b o))
              | Ffi.Bool -> VBool (Bytes.get_int32_le b o <> 0l)
              | Ffi.Float -> VFloat (Int64.float_of_bits (Bytes.get_int64_le b o))
              | Ffi.Handle -> handle (Bytes.get_int64_le b o)
              | _ -> VInt (Bytes.get_int64_le b o) ))
          fs offs )

  let word b k = let w = Bytes.make 8 '\000' in Bytes.blit b (8 * k) w 0 (min 8 (Bytes.length b - (8 * k))); Bytes.get_int64_le w 0

  (* How a struct travels. x86-64: up to 16 bytes, each eightbyte in an
     integer register unless it is a Float alone (SSE); larger, in memory.
     arm64: 1 to 4 Floats in floating-point registers (a homogeneous
     aggregate); up to 16 bytes in integer registers; larger, copied and
     passed by pointer. *)
  type cls = Words of bool list (* per eightbyte: an SSE one *) | Hfa of int | Memory | Indirect

  let classify (fs : (string * Ffi.cty) list) =
    let offs, size = Ffi.layout fs in
    match Lazy.force abi with
    | X64 ->
      if size > 16 then Memory
      else
        Words
          (List.init ((size + 7) / 8) (fun k ->
               List.for_all2 (fun (_, t) o -> o / 8 <> k || t = Ffi.Float) fs offs))
    | Arm64 | Apple ->
      if List.length fs <= 4 && List.for_all (fun (_, t) -> t = Ffi.Float) fs then Hfa (List.length fs)
      else if size <= 16 then Words (List.init ((size + 7) / 8) (fun _ -> false))
      else Indirect

  (* An argument as the call stub sees it. *)
  type arg = Word of carg (* integer-like, 8 bytes *) | Small of int64 (* Int32 or Bool: a C int *) | Dbl of float | Agg of (string * Ffi.cty) list * Bytes.t

  (* Lay out a plain call's arguments: the stub's integer words (registers,
     then the stack image) and its 8 doubles. [sret]: the result comes back
     through memory, whose pointer takes x86-64's first integer register. *)
  let place ~sret (args : arg list) =
    let abi = Lazy.force abi in
    let gcap = match abi with X64 -> if sret then 5 else 6 | Arm64 | Apple -> 8 in
    let gp = Array.make gcap (AInt 0L) and ng = ref 0 in
    let fp = Array.make 8 0.0 and nf = ref 0 in
    let sbytes = Bytes.make 128 '\000' and sptr = Array.make 16 None and soff = ref 0 in
    let align a = soff := (!soff + a - 1) / a * a in
    let st_word c = align 8; sptr.(!soff / 8) <- Some c; soff := !soff + 8 in
    let st_bytes b =
      align 8;
      Bytes.blit b 0 sbytes !soff (Bytes.length b);
      soff := !soff + ((Bytes.length b + 7) / 8 * 8)
    in
    let gp_word c = if !ng < gcap then (gp.(!ng) <- c; incr ng) else st_word c in
    let dbl x = if !nf < 8 then (fp.(!nf) <- x; incr nf) else st_word (AInt (Int64.bits_of_float x)) in
    List.iter
      (function
        | Word c -> gp_word c
        | Small n ->
          if !ng < gcap then (gp.(!ng) <- AInt n; incr ng)
          else if abi = Apple then begin
            (* Apple packs a 4-byte argument on the stack at 4-byte alignment *)
            align 4;
            Bytes.set_int32_le sbytes !soff (Int64.to_int32 n);
            soff := !soff + 4
          end
          else st_word (AInt n)
        | Dbl x -> dbl x
        | Agg (fs, b) -> (
          match classify fs with
          | Memory -> st_bytes b
          | Indirect -> gp_word (ABlob (Bytes.to_string b))
          | Hfa n ->
            if !nf + n <= 8 then List.iteri (fun k _ -> dbl (Int64.float_of_bits (word b k))) (List.init n Fun.id)
            else (nf := 8; st_bytes b)
          | Words cls ->
            let ni = List.length (List.filter not cls) and nd = List.length (List.filter Fun.id cls) in
            if !ng + ni <= gcap && !nf + nd <= 8 then List.iteri (fun k sse -> if sse then dbl (Int64.float_of_bits (word b k)) else gp_word (AInt (word b k))) cls
            else begin
              if abi <> X64 then ng := gcap;
              st_bytes b
            end))
      args;
    let nslots = (!soff + 7) / 8 in
    if gcap + nslots > 24 then failwith "internal: foreign call too large";
    let stack = Array.init nslots (fun k -> match sptr.(k) with Some c -> c | None -> AInt (word sbytes k)) in
    (Array.append gp stack, fp)

  (* The stub result kind for a result type, and whether it comes back
     through memory. *)
  let ret_kind (t : Ffi.cty) =
    match t with
    | Ffi.Float -> (1, false)
    | Ffi.Struct (_, fs) -> (
      match classify fs with
      | Memory | Indirect -> (8, Lazy.force abi = X64)
      | Hfa 1 -> (1, false)
      | Hfa 2 -> (3, false)
      | Hfa 3 -> (6, false)
      | Hfa _ -> (7, false)
      | Words [ false ] -> (0, false)
      | Words [ true ] -> (1, false)
      | Words [ false; false ] -> (2, false)
      | Words [ true; true ] -> (3, false)
      | Words [ false; true ] -> (4, false)
      | Words _ -> (5, false))
    | _ -> (0, false)

  (* ---- callbacks: slots of the trampoline pools in ffi_stubs.c ---- *)

  (* slot key -> what to do when C calls it *)
  let cb_handlers : (int, int64 array -> float array -> int64) Hashtbl.t = Hashtbl.create 8

  (* the failure of a callback, raised again (as Fatal) once C is left *)
  let cb_failure : (string * Span.t) option ref = ref None

  let cb_wait = "a callback from C cannot wait (for a task, a channel, a timer or input)"

  let () =
    Callback.register "vela_ffi_callback" (fun key ints floats ->
        match Hashtbl.find_opt cb_handlers key with Some h -> h ints floats | None -> 0L)

  (* The handler for Vela function [name], with C parameters [ps] and result
     [r]: it converts the arguments, calls it, and converts the result. A
     panic, or waiting, ends the program: nothing unwinds through C. *)
  let cb_handler ~call ~cap (name, (cd : fn_decl)) ps r =
    fun ints floats ->
      try
        let ii = ref 0 and fi = ref 0 in
        let args =
          List.map
            (fun (t : Ffi.cty) ->
              match t with
              | Ffi.Float -> let x = floats.(!fi) in incr fi; VFloat x
              | _ -> (
                let n = ints.(!ii) in
                incr ii;
                match t with
                | Ffi.Int32 -> VInt (int32 n)
                | Ffi.Bool -> VBool (int32 n <> 0L)
                | Ffi.Handle -> handle n
                | Ffi.Str -> VStr (cstr n)
                | _ -> VInt n))
            ps
        in
        let args = if List.length cd.fn_params > List.length ps then cap :: args else args in
        match (r, call name args) with
        | Ffi.Int32, VInt n -> int32_arg n (sprintf "the result of callback `%s`" name) cd.fn_span
        | Ffi.Int, VInt n -> n
        | Ffi.Float, VFloat x -> Int64.bits_of_float x
        | Ffi.Bool, VBool b -> if b then 1L else 0L
        | Ffi.Handle, VStruct (_, [ (_, VInt n) ]) -> n
        | _ -> 0L
      with e ->
        let failure =
          match e with
          | Panic (m, s) -> ("in a callback from C: " ^ m, s)
          | Fatal (m, s) -> (m, s)
          | Stack_overflow -> ("in a callback from C: stack overflow", cd.fn_span)
          | Effect.Unhandled _ | Cancelled -> (cb_wait, cd.fn_span)
          | e -> ("internal: " ^ Printexc.to_string e, cd.fn_span)
        in
        if !cb_failure = None then cb_failure := Some failure;
        cb_fail ();
        0L

  (* [call name args] calls the top-level function [name], [fns] finds its
     declaration, and [structs] a struct's. *)
  let invoke ~call ~fns ~structs (d : fn_decl) args sp =
    let cs = match Ffi.of_decl ~structs d with Ok cs -> cs | Error _ -> failwith "internal: foreign signature" in
    let cap, args =
      if cs.trusted then (VUnit, args)
      else
        match args with
        | (VCap (CForeign allowed) as cap) :: rest ->
          if not (permits allowed cs.lib) then raise (Panic (Ffi.denied_msg cs.lib, sp));
          (cap, rest)
        | _ -> failwith "internal: foreign capability"
    in
    match (cs.lib, args) with
    | "vela", args -> runtime_helper cs.sym args sp
    | _ ->
    let taken = ref [] in
    let release () =
      List.iter (fun (pool, k) -> cb_free pool k; Hashtbl.remove cb_handlers ((if pool then 32 else 0) + k)) !taken;
      taken := []
    in
    Fun.protect ~finally:release @@ fun () ->
    let rec conv ps args =
      match (ps, args) with
      | [], _ -> []
      | (p : Ffi.param) :: ps, _ when p.out -> Word AOut :: conv ps args
      | p :: ps, a :: args ->
        let c =
          match (p.cty, a) with
          | Ffi.Int, VInt n -> Word (AInt n)
          | Ffi.Int32, VInt n -> Small (int32_arg n (sprintf "argument `%s` of `%s`" p.name cs.sym) sp)
          | Ffi.Float, VFloat x -> Dbl x
          | Ffi.Bool, VBool b -> Small (if b then 1L else 0L)
          | (Ffi.Str | Ffi.Bytes), (VStr s | VBytes s) -> Word (APtr s)
          | Ffi.Handle, VStruct (_, [ (_, VInt n) ]) -> Word (AInt n)
          | Ffi.Struct (n, fs), v -> Agg (fs, struct_bytes n fs v sp)
          | Ffi.Fn (cps, r), VFn m ->
            let pool = r = Ffi.Float in
            let k = cb_take pool in
            if k < 0 then raise (Panic ("foreign: too many callbacks in use at once (the interpreter has room for 32)", sp));
            taken := (pool, k) :: !taken;
            Hashtbl.replace cb_handlers ((if pool then 32 else 0) + k) (cb_handler ~call ~cap (m, fns m) cps r);
            Word (AInt (Int64.of_nativeint (cb_addr pool k)))
          | _ -> failwith "internal: foreign argument"
        in
        c :: conv ps args
      | _ :: _, [] -> failwith "internal: foreign arity"
    in
    let cargs = conv cs.params args in
    let fp = match Hashtbl.find_opt syms (cs.lib, cs.sym) with Some p -> p | None -> failwith "internal: unresolved foreign function" in
    let copy = !taken <> [] in
    let kind, sret = ret_kind cs.ret in
    let status, words, outs =
      match cs.fixed with
      | None ->
        let ints, floats = place ~sret cargs in
        call_c fp ints floats kind copy
      | Some k ->
        let word_of = function Word c -> c | Small n -> AInt n | Dbl x -> AInt (Int64.bits_of_float x) | Agg _ -> AInt 0L in
        let ints, floats =
          if Lazy.force abi = Apple then (List.map word_of cargs, [])
          else
            ( List.filter_map (function Dbl _ -> None | a -> Some (word_of a)) cargs,
              List.filter_map (function Dbl x -> Some x | _ -> None) cargs )
        in
        call_var fp (k + if copy then 256 else 0) (Array.of_list ints) (Array.of_list floats) kind
    in
    if status <> 0 then begin
      let m, s = Option.value ~default:("internal: a callback failed", sp) !cb_failure in
      cb_failure := None;
      raise (Fatal (m, s))
    end;
    let of_raw (t : Ffi.cty) n =
      match t with
      | Ffi.Int -> VInt n
      | Ffi.Int32 -> VInt (int32 n)
      | Ffi.Float -> VFloat (Int64.float_of_bits n)
      | Ffi.Bool -> VBool (int32 n <> 0L)
      | Ffi.Handle -> handle n
      | Ffi.Str -> VStr (cstr n)
      | _ -> VUnit
    in
    let ret =
      match cs.ret with
      | Ffi.Unit -> []
      | Ffi.Struct (n, fs) ->
        let b = Bytes.create (8 * Array.length words) in
        Array.iteri (fun k w -> Bytes.set_int64_le b (8 * k) w) words;
        [ struct_of_bytes n fs b ]
      | t -> [ of_raw t words.(0) ]
    in
    let out_tys = List.filter_map (fun (p : Ffi.param) -> if p.out then Some p.cty else None) cs.params in
    let outs = List.mapi (fun i t -> of_raw t outs.(i)) out_tys in
    match ret @ outs with
    | [] -> VUnit
    | [ v ] -> v
    | vs -> VStruct (Ast.tuple_name (List.length vs), List.mapi (fun i v -> (string_of_int i, v)) vs)

end

(* ---------- processes ----------
   Same rules as the native runtime: the program name must be granted exactly
   (or `*`), arguments go as a list with no shell, the child's environment is
   PATH plus the variables the Env grant names, stdin is /dev/null, stdout
   comes through a pipe this task waits on, stderr through an unlinked
   temporary file. A child killed by signal N exits with 128 + N. *)
module Proc = struct
  let permits allowed p = List.mem "*" allowed || List.mem p allowed
  let only allowed p = if (p = "*" && List.mem "*" allowed) || (p <> "*" && permits allowed p) then [ p ] else []

  let run ~env_grant allowed prog args : (int * string * string, Caps.ioerr) result =
    if String.contains prog '\000' then Error (Caps.Failed "exec: the program name contains a NUL byte")
    else if List.exists (fun a -> String.contains a '\000') args then Error (Caps.Failed "exec: an argument contains a NUL byte")
    else if not (permits allowed prog) then Error (Caps.Denied (sprintf "exec: `%s` is not granted" prog))
    else
      match Filename.temp_file "vela-exec-" "" with
      | exception Sys_error m -> Error (Caps.Failed m)
      | tmp -> (
        let errfd = Unix.openfile tmp [ Unix.O_RDWR; Unix.O_CLOEXEC ] 0o600 in
        Sys.remove tmp;
        let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
        let rd, wr = Unix.pipe ~cloexec:true () in
        let close_all fds = List.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ()) fds in
        let env =
          Array.of_list
            (List.filter
               (fun kv ->
                 match String.index_opt kv '=' with
                 | Some i -> let k = String.sub kv 0 i in k = "PATH" || List.mem "*" env_grant || List.mem k env_grant
                 | None -> false)
               (Array.to_list (Unix.environment ())))
        in
        match Unix.create_process_env prog (Array.of_list (prog :: args)) env devnull wr errfd with
        | exception Unix.Unix_error (e, _, _) ->
          close_all [ rd; wr; errfd; devnull ];
          Error (Caps.Failed (sprintf "exec: cannot run `%s`: %s" prog (Unix.error_message e)))
        | pid ->
          close_all [ wr; devnull ];
          Unix.set_nonblock rd;
          let sk = { fd = rd; shut = false; reading = false; writing = false; tls = Tls.Plain } in
          let b = Buffer.create 4096 and chunk = Bytes.create 65536 in
          let rec drain () =
            match Unix.read rd chunk 0 65536 with
            | 0 -> ()
            | n -> Buffer.add_subbytes b chunk 0 n; drain ()
            | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) -> Sock.wait sk false; drain ()
            | exception Unix.Unix_error _ -> ()
          in
          drain ();
          Unix.close rd;
          let rec reap () = try snd (Unix.waitpid [] pid) with Unix.Unix_error (Unix.EINTR, _, _) -> reap () in
          let code =
            match reap () with
            | Unix.WEXITED n -> n
            | Unix.WSIGNALED s | Unix.WSTOPPED s -> 128 + signal_number s
          in
          ignore (Unix.lseek errfd 0 Unix.SEEK_SET);
          let ic = Unix.in_channel_of_descr errfd in
          let err = In_channel.input_all ic in
          close_in ic;
          Ok (code, Buffer.contents b, err))
end

(* ---------- stdin ----------
   Read like a socket: a task waiting for input waits in the scheduler, and
   other tasks keep running. One task reads at a time (each gets whole
   lines); the others wait their turn. A regular file is read directly. *)
module Stdin = struct
  let buf = Buffer.create 4096
  let eof = ref false
  let busy = ref false
  let sk = { fd = Unix.stdin; shut = false; reading = false; writing = false; tls = Tls.Plain }
  let direct = lazy (try (Unix.fstat Unix.stdin).Unix.st_kind = Unix.S_REG with Unix.Unix_error _ -> true)

  let reset () = Buffer.clear buf; eof := false; busy := false

  (* run [f] as the reader; a task cancelled while it waits lets the next one in *)
  let exclusive f =
    wait_until (fun () -> not !busy);
    busy := true;
    match f () with
    | v -> busy := false; v
    | exception e -> busy := false; raise e

  let rec fill () =
    let ready () = Lazy.force direct || match Unix.select [ Unix.stdin ] [] [] 0. with r, _, _ -> r <> [] | exception Unix.Unix_error _ -> true in
    if not (ready ()) then (Sock.wait sk false; fill ())
    else
      let b = Bytes.create 65536 in
      match Unix.read Unix.stdin b 0 65536 with
      | 0 -> eof := true
      | n -> Buffer.add_subbytes buf b 0 n
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> fill ()
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> Sock.wait sk false; fill ()
      | exception Unix.Unix_error _ -> eof := true

  let take n =
    let s = Buffer.contents buf in
    Buffer.clear buf;
    Buffer.add_string buf (String.sub s n (String.length s - n))

  let read_line () =
    exclusive (fun () ->
        let rec go () =
          let s = Buffer.contents buf in
          match String.index_opt s '\n' with
          | Some i -> take (i + 1); Some (String.sub s 0 i)
          | None when !eof -> if s = "" then None else (take (String.length s); Some s)
          | None -> fill (); go ()
        in
        Option.map (fun l -> if String.ends_with ~suffix:"\r" l then String.sub l 0 (String.length l - 1) else l) (go ()))

  let read_all () =
    exclusive (fun () ->
        while not !eof do fill () done;
        let s = Buffer.contents buf in
        Buffer.clear buf;
        s)
end

let rec pmatch p v binds =
  match (p.pdesc, v) with
  | PWild, _ -> true
  | PBind n, _ -> bind binds n v; true
  | PInt n, VInt m -> Int64.equal n m
  | PStr s, VStr t -> s = t
  | PBool b, VBool c -> b = c
  | PVariant (_, vn, args), VEnum (_, vn', vals) ->
    vn = vn' && List.length args = List.length vals && List.for_all2 (fun p v -> pmatch p v binds) args vals
  | PTuple ps, VStruct (_, fs) when List.length ps = List.length fs ->
    List.for_all2 (fun p (_, v) -> pmatch p v binds) ps fs
  | _ -> false

let rec eval rt env e : value =
  match e.edesc with
  | EInt n -> VInt n
  | EFloat x -> VFloat x
  | EBool b -> VBool b
  | EUnit -> VUnit
  | EStr parts ->
    VStr (String.concat "" (List.map (function SPLit s -> s | SPExpr x -> show (eval rt env x)) parts))
  | EVar n -> (
    match e.ecallee with
    | Some d -> VFn d.fn_name
    | None ->
      let v = eval_var rt env n e.espan in
      (* a top-level function: remember which, so later calls skip the search *)
      (match v with VFn m when m == n || String.equal m n -> e.ecallee <- Hashtbl.find_opt rt.fns n | _ -> ());
      v)
  | EList es -> VList (Array.of_list (List.map (eval rt env) es))
  | EBin (And, a, b) -> if as_bool (eval rt env a) then eval rt env b else VBool false
  | EBin (Or, a, b) -> if as_bool (eval rt env a) then VBool true else eval rt env b
  | EBin (Else, a, b) -> (
    match eval rt env a with
    | VEnum (_, ("Some" | "Ok"), [ x ]) -> x
    | _ -> eval rt env b)
  | EBin (op, a, b) ->
    let x = eval rt env a in
    let y = eval rt env b in
    binop op x y e.espan
  | EUn (Neg, a) -> (
    match eval rt env a with VInt n -> VInt (Int64.neg n) | VFloat x -> VFloat (-.x) | _ -> failwith "internal: neg")
  | EUn (Not, a) -> VBool (not (as_bool (eval rt env a)))
  | ENamed _ -> failwith "internal: the checker puts named arguments in order"
  | ECall ({ ecallee = Some d; _ }, args) when d.fn_foreign = None ->
    (* a call to a known top-level function: no name to look up *)
    call_fn rt d (List.map (eval rt env) args)
  | ECall (c, args) ->
    let fv = eval rt env c in
    let avs = List.map (eval rt env) args in
    apply rt fv avs e.espan
  | EMethod (recv, "remove", _, [ arg ]) when (match eval rt env recv with VMap _ -> true | _ -> false) -> (
    match eval rt env recv with
    | VMap mv ->
      let k = key_of (eval rt env arg) in
      assign rt env recv (VMap { mv with m = KM.remove k mv.m });
      VUnit
    | _ -> failwith "internal: remove")
  | EMethod ({ edesc = EVar n; _ }, "push", _, [ arg ]) when (match find_var env n with Some { contents = VList _ | VBuf _ } -> true | _ -> false) ->
    (* in place, growing by doubling; the argument first, since it may read
       the list (which turns it back into an ordinary one) *)
    let v = eval rt env arg in
    let b = own (Option.get (find_var env n)) in
    if b.len = Array.length b.items then begin
      let bigger = Array.make (2 * b.len) VUnit in
      Array.blit b.items 0 bigger 0 b.len;
      b.items <- bigger
    end;
    b.items.(b.len) <- v;
    b.len <- b.len + 1;
    VUnit
  | EMethod (recv, "push", _, [ arg ]) -> (
    match eval rt env recv with
    | VList arr ->
      let v = eval rt env arg in
      assign rt env recv (VList (Array.append arr [| v |]));
      VUnit
    | _ -> failwith "internal: push")
  | EMethod ({ edesc = EVar en; _ }, name, _, args) when find_var env en = None && Hashtbl.mem rt.enums en ->
    apply rt (ctor_value rt en name) (List.map (eval rt env) args) e.espan
  | EMethod (recv, name, _, args) -> (
    match peek env recv with
    | Some b when name = "len" && args = [] -> VInt (Int64.of_int b.len)
    | Some b when name = "is_empty" && args = [] -> VBool (b.len = 0)
    | Some b ->
      (* the method sees a copy; it reads the whole list anyway *)
      let rv = VList (Array.sub b.items 0 b.len) in
      let avs = List.map (eval rt env) args in
      call_method rt rv name avs e.espan
    | None ->
      let rv = eval rt env recv in
      let avs = List.map (eval rt env) args in
      call_method rt rv name avs e.espan)
  | EField ({ edesc = EVar en; _ }, name, _) when find_var env en = None && Hashtbl.mem rt.enums en ->
    ctor_value rt en name
  | EField (recv, name, _) -> field rt (eval rt env recv) name
  | EIndex (a, i) when peek env a <> None -> (
    let b = Option.get (peek env a) in
    match eval rt env i with
    | VInt k ->
      if k < 0L || k >= Int64.of_int b.len then
        raise (Panic (sprintf "index %Ld is out of range for a list of length %d" k b.len, e.espan));
      b.items.(Int64.to_int k)
    | _ -> failwith "internal: index")
  | EIndex (a, i) -> (
    match (eval rt env a, eval rt env i) with
    | VMap mv, k -> (
      match KM.find_opt (key_of k) mv.m with
      | Some (_, v) -> v
      | None -> raise (Panic (sprintf "key %s is not in the map" (show ~top:false k), e.espan)))
    | VList arr, VInt k ->
      if k < 0L || k >= Int64.of_int (Array.length arr) then
        raise (Panic (sprintf "index %Ld is out of range for a list of length %d" k (Array.length arr), e.espan));
      arr.(Int64.to_int k)
    | VBytes b, VInt k ->
      if k < 0L || k >= Int64.of_int (String.length b) then
        raise (Panic (sprintf "index %Ld is out of range for bytes of length %d" k (String.length b), e.espan));
      vint (Char.code b.[Int64.to_int k])
    | _ -> failwith "internal: index")
  | ETry (x, ctx) -> (
    match eval rt env x with
    | VEnum (_, ("Ok" | "Some"), [ v ]) -> v
    | VEnum (_, ("Err" | "None"), _) as r when ctx = None -> raise (Return r)
    | VEnum (_, ("Err" | "None"), cause) -> (
      (* `x? "context"`: the error becomes `dyn Error` (values carry their
         type, so that changes nothing here), wrapped with the context *)
      let text = eval rt env (Option.get ctx) in
      let err = match cause with [ c ] -> VStruct ("WrappedError", [ ("context", text); ("inner", c) ]) | _ -> text in
      raise (Return (VEnum ("Result", "Err", [ err ]))))
    | _ -> failwith "internal: try")
  | EIf (c, b, els) -> (
    if as_bool (eval rt env c) then eval_block rt env b
    else match els with Some x -> eval rt env x | None -> VUnit)
  | EMatch (s, arms) ->
    let v = eval rt env s in
    let rec go = function
      | [] -> raise (Panic ("no `match` arm matched " ^ show ~top:false v, e.espan))
      | a :: rest ->
        let binds = scope () in
        if pmatch a.apat v binds && (match a.aguard with Some g -> as_bool (eval rt (binds :: env) g) | None -> true) then
          eval rt (binds :: env) a.abody
        else go rest
    in
    go arms
  | ESelect arms ->
    (* evaluate every channel and value first, in order; then run the first
       ready arm, or `_`, or wait until one is ready *)
    let ops =
      List.map
        (fun a ->
          let on = match a.sguard with Some g -> as_bool (eval rt env g) | None -> true in
          match a.sop with
          | SelRecv (b, ch) -> let c = eval rt env ch in if on then `Recv (b, c) else `Off
          | SelSend (ch, v) ->
            let c = eval rt env ch in
            let x = eval rt env v in
            if on then `Send (c, x) else `Off
          | SelAfter (clk, ms) ->
            ignore (eval rt env clk);
            let ms = match eval rt env ms with VInt n -> n | _ -> 0L in
            if on then `After ms else `Off
          | SelDefault -> `Default)
        arms
    in
    if (not (List.exists (function `Recv _ | `Send _ | `After _ | `Default -> true | `Off -> false) ops)) then
      raise (Panic ("every arm of this `select` is turned off by its guard", e.espan));
    let closed_send () = raise (Panic ("send on a closed channel", e.espan)) in
    (* a timeout counts from now, after every operand was evaluated *)
    let start = now () in
    let ops = List.mapi (fun i op -> (i, op)) ops in
    (* the first operation that can happen now, in order: (arm, received) *)
    let rec first = function
      | [] -> None
      | (i, `Recv (_, VChan ch)) :: rest -> (
        match try_recv ch with `Got v -> Some (i, Some v) | `Closed -> Some (i, None) | `Blocked -> first rest)
      | (i, `Send (VChan ch, x)) :: rest -> (
        match try_send ch x with `Done -> Some (i, None) | `Closed -> closed_send () | `Blocked -> first rest)
      | (i, `After ms) :: _ when ms <= 0L -> Some (i, None)
      | _ :: rest -> first rest
    in
    let i, got =
      match (first ops, List.find_opt (fun (_, op) -> op = `Default) ops) with
      | Some r, _ -> r
      | None, Some (d, _) -> (d, None)
      | None, None ->
        (* wait on every channel at once; the first to act claims the select *)
        let cl = { fired = -1 } in
        let queued =
          List.filter_map
            (fun (i, op) ->
              match op with
              | `Recv (_, VChan ch) -> let w = { claim = cl; idx = i; v = VUnit; ok = false } in Queue.push w ch.rq; Some (ch.rq, w)
              | `Send (VChan ch, x) -> let w = { claim = cl; idx = i; v = x; ok = false } in Queue.push w ch.sq; Some (ch.sq, w)
              | _ -> None)
            ops
        in
        (* the earliest timeout wins; on a tie, the first written *)
        let after =
          List.fold_left
            (fun best (i, op) ->
              match (op, best) with
              | `After ms, _ ->
                let at = start +. (Int64.to_float (min ms 1_000_000_000_000L) /. 1000.) in
                (match best with Some (_, b) when b <= at -> best | _ -> Some (i, at))
              | _ -> best)
            None ops
        in
        wait_claim ?at:(Option.map snd after) cl queued;
        if cl.fired = -1 then cl.fired <- fst (Option.get after); (* the timeout *)
        let i = cl.fired in
        match List.find_opt (fun (_, w) -> w.idx = i) queued with
        | Some (_, w) -> (
          match List.assoc i ops with
          | `Recv _ -> (i, if w.ok then Some w.v else None)
          | _ -> if w.ok then (i, None) else closed_send ())
        | None -> (i, None)
    in
    let binds = scope () in
    (match List.assoc i ops with
     | `Recv (b, _) -> Option.iter (fun (n, _) -> bind binds n (match got with Some v -> some v | None -> none)) b
     | _ -> ());
    eval rt (binds :: env) (List.nth arms i).sbody
  | EBlock b -> eval_block rt env b
  | EClosure (ps, body) -> VClosure (List.map (fun (n, _, _) -> n) ps, body, [ snapshot env ])
  | EStruct (name, _, fields) ->
    let decl = Hashtbl.find rt.structs name in
    let vals = List.map (fun (f, _, x) -> (f, eval rt env x)) fields in
    VStruct (name, List.map (fun (f, _, _) -> (f, field_of f vals)) decl.st_fields)
  | EDyn (x, _) -> eval rt env x (* values carry their type, so methods dispatch on it *)
  | EPipe _ -> failwith "pipes are expanded when modules load"
  | ESafe (base, x, rest, flat) -> (
    match eval rt env base with
    | VEnum (_, "Some", [ v ]) ->
      let r = eval rt (scope_of x v :: env) rest in
      if flat then r else some r
    | _ -> none)
  | EWith (base, fields) -> (
    (* the base first, then the new values; a plain variable is read last,
       as the native backend does to reuse it in place *)
    let early = match base.edesc with EVar _ -> None | _ -> Some (eval rt env base) in
    let vals = List.map (fun (fname, _, x) -> (fname, eval rt env x)) fields in
    match (match early with Some v -> v | None -> eval rt env base) with
    | VStruct (n, fs) -> VStruct (n, List.map (fun (k, v) -> (k, Option.value ~default:v (List.assoc_opt k vals))) fs)
    | _ -> failwith "internal: with on a non-struct")
  | EGroup (name, interleaved, until, b) ->
    (* tasks are coroutines; the group waits until every one has a result *)
    let t = !current in
    let g =
      { tasks = []; kept = []; gcancelled = false; gparent = t.sink; ordered = not interleaved; body_done = false;
        waiting = Queue.create () }
    in
    if g.ordered then live_groups := g :: !live_groups;
    let finish () = live_groups := List.filter (fun o -> o != g) !live_groups in
    (* the body has ended: the tasks' output can start to go out *)
    let body_ended () =
      if not g.body_done then begin
        g.body_done <- true;
        advance rt.out g
      end
    in
    let joined = ref false in
    (match until with
     | None -> ()
     | Some (clk, ms) ->
       ignore (eval rt env clk);
       let ms = match eval rt env ms with VInt n -> n | _ -> 0L in
       if ms <= 0L then g.gcancelled <- true
       else begin
         (* the deadline: a coroutine of its own that cancels the tasks when the time comes *)
         let at = now () +. (Int64.to_float (min ms 1_000_000_000_000L) /. 1000.) in
         spawn_fiber { cancelled = false; scopes = []; sink = None } (fun () ->
             wait_until ~at ~cancellable:false (fun () -> !joined || now () >= at);
             if not !joined then cancel_group g)
       end);
    let sc = scope_of name (VGroup g) in
    (try
       (match until with
        | None -> ignore (eval_block rt (sc :: env) b)
        | Some _ -> (
          (* the deadline stops the body at its next wait; then the join goes on *)
          t.scopes <- g :: t.scopes;
          let leave () = t.scopes <- List.tl t.scopes in
          try ignore (eval_block rt (sc :: env) b); leave () with
          | Cancelled when g.gcancelled && (not t.cancelled) && not (List.exists (fun o -> o.gcancelled) (List.tl t.scopes)) -> leave ()
          | e -> leave (); raise e));
       body_ended ();
       wait_until (fun () -> group_finished g)
     with e ->
       (* abandoned (a panic, or this task was cancelled): a cancellation
          reaches the group's tasks too; either way wait for them *)
       body_ended ();
       if stopped !current then cancel_group g;
       wait_until ~cancellable:false (fun () -> group_finished g);
       joined := true;
       finish ();
       raise e);
    joined := true;
    finish ();
    let results = List.rev_map snd g.tasks in
    (* a panic ended only its task; the group passes the first one on *)
    List.iter (fun r -> match !r with Some (Failed (m, s)) -> raise (Panic (m, s)) | _ -> ()) results;
    let result r =
      match (!r, until) with
      | Some (Done v), None -> v
      | Some (Done v), Some _ -> some v
      | _, Some _ -> none (* cancelled *)
      | _ -> VUnit
    in
    let kept = List.rev g.kept in
    VList (Array.of_list (List.concat (List.map2 (fun keep r -> if keep then [ result r ] else []) kept results)))

and eval_var rt env n sp =
  match find_var env n with
  | Some r -> settle r
  | None -> (
    if Hashtbl.mem rt.fns n then VFn n
    else
      match Hashtbl.find_opt rt.ctors n with
      | Some en -> ctor_value rt en n
      | None ->
        if n = "panic" then VPanicFn
        else if n = "channel" then VChanFn
        else if n = "assert" || n = "assert_eq" || n = "map" || n = "catch" || n = "bytes" || n = "keep" then VBuiltin n
        else raise (Panic ("internal: unbound name " ^ n, sp)))

and ctor_value rt en vn =
  let e = Hashtbl.find rt.enums en in
  match List.find (fun (v, _, _) -> v = vn) e.en_variants with
  | _, _, [] -> VEnum (en, vn, [])
  | _ -> VCtor (en, vn)

and field rt v name =
  match (v, name) with
  | VStruct (_, fs), _ -> field_of name fs
  | VCap CRoot, "net" -> VCap (CNet rt.grants.net)
  | VCap CRoot, "fs" -> (
    match rt.grants.fs with
    | Some (root, ro) -> VCap (CFs { Caps.root = Some root; ro })
    | None -> VCap (CFs { Caps.root = None; ro = true }))
  | VCap CRoot, "stdout" -> VCap CStdout
  | VCap CRoot, "clock" -> VCap CClock
  | VCap CRoot, "listen" -> VCap (CListen rt.grants.listen)
  | VCap CRoot, "rand" -> VCap CRand
  | VCap CRoot, "stdin" -> VCap CStdin
  | VCap CRoot, "env" -> VCap (CEnv rt.grants.env)
  | VCap CRoot, "exec" -> VCap (CExec rt.grants.exec)
  | VCap CRoot, "foreign" -> VCap (CForeign rt.grants.foreign)
  | _ -> failwith ("internal: field " ^ name)

and apply rt fv args sp =
  match fv with
  | VFn n when String.length n > 5 && (String.sub n 0 6 = "List::" || String.sub n 0 5 = "Map::")
               && (match Hashtbl.find_opt rt.fns n with Some d -> d.fn_span.file = Prelude.methods_file | None -> false) ->
    vela_method rt n args sp
  | VFn n ->
    let d = Hashtbl.find rt.fns n in
    if d.fn_foreign <> None then
      Foreign.invoke d args sp
        ~call:(fun m args -> call_fn rt (Hashtbl.find rt.fns m) args)
        ~fns:(Hashtbl.find rt.fns) ~structs:(Hashtbl.find_opt rt.structs)
    else call_fn rt d args
  | VClosure (ps, body, cenv) -> (
    let sc = scope () in
    List.iter2 (fun p a -> bind sc p a) ps args;
    try eval rt (sc :: cenv) body with Return v -> v)
  | VCtor (en, vn) -> VEnum (en, vn, args)
  | VBuiltin "assert" -> (
    match args with
    | [ VBool true ] -> VUnit
    | _ -> raise (Panic ("assertion failed", sp)))
  | VBuiltin "assert_eq" -> (
    match args with
    | [ a; b ] when equal a b -> VUnit
    | [ a; b ] -> raise (Panic (sprintf "assertion failed: %s != %s" (show ~top:false a) (show ~top:false b), sp))
    | _ -> raise (Panic ("assertion failed", sp)))
  | VBuiltin "keep" -> VUnit
  | VBuiltin "catch" -> (
    match args with
    | [ f ] -> ( try ok (apply rt f [] sp) with Panic (m, _) -> VEnum ("Result", "Err", [ VStr m ]))
    | _ -> failwith "internal: catch")
  | VBuiltin "bytes" -> (
    match args with
    | [ VList xs ] ->
      VBytes
        (String.init (Array.length xs) (fun i ->
             match xs.(i) with
             | VInt n when n >= 0L && n <= 255L -> Char.chr (Int64.to_int n)
             | VInt n -> raise (Panic (sprintf "byte %Ld is out of range 0 to 255" n, sp))
             | _ -> failwith "internal: bytes"))
    | _ -> failwith "internal: bytes")
  | VBuiltin "map" -> (
    match args with
    | [] -> VMap empty_map
    | [ VList pairs ] ->
      VMap
        (Array.fold_left
           (fun mv p -> match p with VStruct (_, [ (_, k); (_, v) ]) -> map_put mv k v | _ -> mv)
           empty_map pairs)
    | _ -> failwith "internal: map")
  | VChanFn -> (
    match args with
    | [ VInt n ] when n >= 0L ->
      VChan
        { q = Queue.create (); cap = (if n > Int64.of_int max_int then max_int else Int64.to_int n); closed = false;
          rq = Queue.create (); sq = Queue.create () }
    | _ -> raise (Panic ("channel capacity cannot be negative", sp)))
  | VPanicFn -> (
    match args with [ VStr m ] -> raise (Panic (m, sp)) | _ -> raise (Panic ("panic", sp)))
  | _ -> failwith "internal: not callable"

and call_fn rt d args =
  let sc = scope () in
  List.iter2 (fun p a -> bind sc p.prm_name a) d.fn_params args;
  try eval_block rt [ sc ] d.fn_body with Return v -> v

and eval_block rt env b =
  (* only a `let` directly in the block binds into its own scope; a block
     without one shares the enclosing scope, and lookups stay short *)
  let env = if List.exists (fun s -> match s.sdesc with SLet _ | SLetPat _ -> true | _ -> false) b.stmts then scope () :: env else env in
  run_stmts rt env b.stmts b.tail

(* A `defer` runs the rest of its block, then its own body however that rest
   ends: a value, return, break, continue, a panic or a cancellation. The body
   sees the names in scope at the defer (later `let`s do not shadow them) and
   their values when it runs. A panic in the body replaces whatever was
   leaving the block. *)
and run_stmts rt env ss tail =
  match ss with
  | [] -> ( match tail with Some e -> eval rt env e | None -> VUnit)
  | { sdesc = SDefer d; _ } :: rest -> (
    (* the scopes as they are now: later lets are not seen, but the values
       of the names already there are, as they are when it runs *)
    let at_defer = List.map (fun s -> { vars = s.vars }) env in
    let run_defer () = ignore (eval_block rt at_defer d) in
    match run_stmts rt env rest tail with
    | v -> run_defer (); v
    | exception e -> run_defer (); raise e)
  | s :: rest -> exec rt env s; run_stmts rt env rest tail

and exec rt env s =
  match s.sdesc with
  | SLet (_, n, _, _, e) ->
    let v = eval rt env e in
    bind (List.hd env) n v
  | SAssign (lhs, rhs) -> assign rt env lhs (eval rt env rhs)
  | SExpr e -> ignore (eval rt env e)
  | SReturn eo -> raise (Return (match eo with Some e -> eval rt env e | None -> VUnit))
  | SWhile (c, b) -> (
    try
      while as_bool (eval rt env c) do
        try ignore (eval_block rt env b) with Continue_ -> ()
      done
    with Break_ -> ())
  | SFor (n, _, { edesc = EBin (Range, lo, hi); _ }, b) -> (
    let lo = match eval rt env lo with VInt x -> x | _ -> failwith "internal: range" in
    let hi = match eval rt env hi with VInt x -> x | _ -> failwith "internal: range" in
    let i = ref lo in
    try
      while !i < hi do
        let sc = scope_of n (VInt !i) in
        i := Int64.add !i 1L;
        try ignore (eval_block rt (sc :: env) b) with Continue_ -> ()
      done
    with Break_ -> ())
  | SFor (n, _, e, b) -> (
    match eval rt env e with
    | VList arr -> (
      try
        Array.iter
          (fun v ->
            let sc = scope_of n v in
            try ignore (eval_block rt (sc :: env) b) with Continue_ -> ())
          arr
      with Break_ -> ())
    | _ -> failwith "internal: for")
  | SBreak -> raise Break_
  | SContinue -> raise Continue_
  | SDefer _ -> failwith "internal: defer outside a block"

(* Value semantics: assigning into a field or element rebuilds the container. *)
and assign rt env lhs v =
  match lhs.edesc with
  | EVar n -> ( match find_var env n with Some r -> r := v | None -> failwith "internal: assign")
  | EField (r, f, _) -> (
    match eval rt env r with
    | VStruct (sn, fs) -> assign rt env r (VStruct (sn, List.map (fun (k, x) -> if same_name k f then (k, v) else (k, x)) fs))
    | _ -> failwith "internal: assign field")
  | EIndex ({ edesc = EVar n; _ }, i) when (match find_var env n with Some { contents = VList _ | VBuf _ } -> true | _ -> false) -> (
    (* in place, in a list this variable owns *)
    match eval rt env i with
    | VInt k ->
      let b = own (Option.get (find_var env n)) in
      if k < 0L || k >= Int64.of_int b.len then
        raise (Panic (sprintf "index %Ld is out of range for a list of length %d" k b.len, lhs.espan));
      b.items.(Int64.to_int k) <- v
    | _ -> failwith "internal: assign index")
  | EIndex (r, i) -> (
    match (eval rt env r, eval rt env i) with
    | VMap mv, k -> assign rt env r (VMap (map_put mv k v))
    | VList arr, VInt k ->
      if k < 0L || k >= Int64.of_int (Array.length arr) then
        raise (Panic (sprintf "index %Ld is out of range for a list of length %d" k (Array.length arr), lhs.espan));
      let a = Array.copy arr in
      a.(Int64.to_int k) <- v;
      assign rt env r (VList a)
    | _ -> failwith "internal: assign index")
  | _ -> failwith "internal: assign target"

(* The built-in methods written in Vela (std/methods.vela), done directly:
   pushing onto a list copies it here, so the Vela loops would be quadratic.
   Same results as the Vela source, which the native backend compiles. *)
and vela_method rt n args sp =
  let call f xs = apply rt f xs sp in
  let lt a b = as_bool (binop Lt a b sp) in
  let tup a b = VStruct (Ast.tuple_name 2, [ ("0", a); ("1", b) ]) in
  let lst l = VList (Array.of_list l) in
  let pick better a key =
    if Array.length a = 0 then none
    else begin
      let best = ref a.(0) and bk = ref (key a.(0)) in
      for i = 1 to Array.length a - 1 do
        let k = key a.(i) in
        if better k !bk then (best := a.(i); bk := k)
      done;
      some !best
    end
  in
  (* entries grouped by key in order of first appearance *)
  let grouped a key init add fin =
    let tbl = Hashtbl.create 16 and order = ref [] in
    Array.iter
      (fun x ->
        let k = key x in
        let kk = key_of k in
        match Hashtbl.find_opt tbl kk with
        | Some (k, acc) -> Hashtbl.replace tbl kk (k, add acc x)
        | None -> Hashtbl.replace tbl kk (k, init x); order := kk :: !order)
      a;
    VMap (List.fold_left (fun mv kk -> let k, acc = Hashtbl.find tbl kk in map_put mv k (fin acc)) empty_map (List.rev !order))
  in
  let distinct a key =
    let seen = Hashtbl.create 16 in
    lst (List.filter (fun x -> let k = key_of (key x) in if Hashtbl.mem seen k then false else (Hashtbl.replace seen k (); true)) (Array.to_list a))
  in
  let sized a k f = if k < 1L then VList [||] else f (Int64.to_int (Int64.min k (Int64.of_int max_int))) in
  match (n, args) with
  | "List::any", [ VList a; f ] -> VBool (Array.exists (fun x -> as_bool (call f [ x ])) a)
  | "List::all", [ VList a; f ] -> VBool (Array.for_all (fun x -> as_bool (call f [ x ])) a)
  | "List::count", [ VList a; f ] -> vint (Array.fold_left (fun n x -> if as_bool (call f [ x ]) then n + 1 else n) 0 a)
  | "List::find", [ VList a; f ] -> ( match Array.find_opt (fun x -> as_bool (call f [ x ])) a with Some x -> some x | None -> none)
  | "List::sum_by", [ VList a; f ] ->
    VInt (Array.fold_left (fun s x -> match call f [ x ] with VInt n -> Int64.add s n | _ -> s) 0L a)
  | "List::min", [ VList a ] -> pick lt a Fun.id
  | "List::max", [ VList a ] -> pick (fun x y -> lt y x) a Fun.id
  | "List::min_by", [ VList a; f ] -> pick lt a (fun x -> call f [ x ])
  | "List::max_by", [ VList a; f ] -> pick (fun x y -> lt y x) a (fun x -> call f [ x ])
  | "List::flat_map", [ VList a; f ] ->
    VList (Array.concat (List.map (fun x -> match call f [ x ] with VList b -> b | _ -> [||]) (Array.to_list a)))
  | "List::take_while", [ VList a; f ] ->
    let i = ref 0 in
    while !i < Array.length a && as_bool (call f [ a.(!i) ]) do incr i done;
    VList (Array.sub a 0 !i)
  | "List::skip_while", [ VList a; f ] ->
    let i = ref 0 in
    while !i < Array.length a && as_bool (call f [ a.(!i) ]) do incr i done;
    VList (Array.sub a !i (Array.length a - !i))
  | "List::with_index", [ VList a ] -> VList (Array.mapi (fun i x -> tup (vint i) x) a)
  | "List::zip", [ VList a; VList b ] -> VList (Array.init (min (Array.length a) (Array.length b)) (fun i -> tup a.(i) b.(i)))
  | "List::distinct", [ VList a ] -> distinct a Fun.id
  | "List::distinct_by", [ VList a; f ] -> distinct a (fun x -> call f [ x ])
  | "List::sort_by", [ VList a; f ] ->
    let keyed = Array.map (fun x -> (call f [ x ], x)) a in
    Array.stable_sort (fun (k1, _) (k2, _) -> compare_v k1 k2) keyed;
    VList (Array.map snd keyed)
  | "List::partition", [ VList a; f ] ->
    let yes, no = List.partition (fun x -> as_bool (call f [ x ])) (Array.to_list a) in
    tup (lst yes) (lst no)
  | "List::chunks", [ VList a; VInt k ] ->
    sized a k (fun k ->
        let len = Array.length a in
        VList (Array.init ((len + k - 1) / k) (fun i -> VList (Array.sub a (i * k) (min k (len - (i * k)))))))
  | "List::windows", [ VList a; VInt k ] ->
    sized a k (fun k -> VList (Array.init (max 0 (Array.length a - k + 1)) (fun i -> VList (Array.sub a i k))))
  | "List::chunk_by", [ VList a; f ] ->
    let runs = ref [] and cur = ref [] and ck = ref VUnit in
    Array.iteri
      (fun i x ->
        let k = call f [ x ] in
        if i > 0 && not (as_bool (binop Eq k !ck sp)) then (runs := lst (List.rev !cur) :: !runs; cur := []);
        cur := x :: !cur;
        ck := k)
      a;
    if !cur <> [] then runs := lst (List.rev !cur) :: !runs;
    lst (List.rev !runs)
  | "List::group_by", [ VList a; f ] -> grouped a (fun x -> call f [ x ]) (fun x -> [ x ]) (fun l x -> x :: l) (fun l -> lst (List.rev l))
  | "List::count_by", [ VList a; f ] -> grouped a (fun x -> call f [ x ]) (fun _ -> 1) (fun n _ -> n + 1) vint
  | "List::tally", [ VList a ] -> grouped a Fun.id (fun _ -> 1) (fun n _ -> n + 1) vint
  | "Map::map_values", [ VMap mv; f ] ->
    VMap (List.fold_left (fun m (k, v) -> map_put m k (call f [ v ])) empty_map (map_entries mv))
  | "Map::filter", [ VMap mv; f ] ->
    VMap (List.fold_left (fun m (k, v) -> if as_bool (call f [ k; v ]) then map_put m k v else m) empty_map (map_entries mv))
  | "Map::merge", [ VMap mv; VMap other ] -> VMap (List.fold_left (fun m (k, v) -> map_put m k v) mv (map_entries other))
  | _ -> call_fn rt (Hashtbl.find rt.fns n) args

and call_method rt v name args sp =
  let call f xs = apply rt f xs sp in
  let user_method = match v with VStruct (n, _) | VEnum (n, _, _) -> Hashtbl.find_opt rt.fns (n ^ "::" ^ name) | _ -> None in
  match user_method with
  | Some d -> call_fn rt d (v :: args)
  | None ->
  match (v, name, args) with
  | VStr _, "message", [] -> call_fn rt (Hashtbl.find rt.fns "$Str::message") [ v ]
  | VEnum ("IoError", _, _), "message", [] -> call_fn rt (Hashtbl.find rt.fns "$IoError::message") [ v ]
  | _, "cause", [] -> none (* `cause` is optional in `Error` *)
  | VMap mv, "len", [] -> vint (KM.cardinal mv.m)
  | VMap mv, "is_empty", [] -> VBool (KM.is_empty mv.m)
  | VMap mv, "get", [ k ] -> ( match KM.find_opt (key_of k) mv.m with Some (_, v) -> some v | None -> none)
  | VMap mv, "contains", [ k ] -> VBool (KM.mem (key_of k) mv.m)
  | VMap mv, "keys", [] -> VList (Array.of_list (List.map fst (map_entries mv)))
  | VMap mv, "values", [] -> VList (Array.of_list (List.map snd (map_entries mv)))
  | VMap mv, "entries", [] ->
    VList (Array.of_list (List.map (fun (k, v) -> VStruct (Ast.tuple_name 2, [ ("0", k); ("1", v) ])) (map_entries mv)))
  | VStr s, "len", [] -> vint (String.length s)
  | VStr s, "is_empty", [] -> VBool (s = "")
  | VStr s, "lines", [] -> strs (lines s)
  | VStr s, "split", [ VStr sep ] -> strs (split_on s sep)
  | VStr s, "trim", [] -> VStr (String.trim s)
  | VStr s, "contains", [ VStr t ] -> VBool (find_sub s t 0 <> None)
  | VStr s, "starts_with", [ VStr t ] -> VBool (String.starts_with ~prefix:t s)
  | VStr s, "ends_with", [ VStr t ] -> VBool (String.ends_with ~suffix:t s)
  | VStr s, "to_upper", [] -> VStr (String.uppercase_ascii s)
  | VStr s, "to_lower", [] -> VStr (String.lowercase_ascii s)
  | VStr s, "replace", [ VStr a; VStr b ] -> VStr (replace_all s a b)
  | VStr s, "parse_int", [] -> ( match parse_decimal s with Some n -> some (VInt n) | None -> none)
  | VStr s, "parse_float", [] -> ( match parse_float s with Some x -> some (VFloat x) | None -> none)
  | VStr s, "slice", [ VInt lo; VInt hi ] ->
    let n = String.length s in
    let lo = clamp lo n in
    let hi = max lo (clamp hi n) in
    VStr (String.sub s lo (hi - lo))
  | VStr s, "index_of", [ VStr t ] -> ( match find_sub s t 0 with Some i -> some (vint i) | None -> none)
  | VStr s, "chars", [] -> strs (chars s)
  | VStr s, "to_bytes", [] -> VBytes s
  | VBytes b, "len", [] -> vint (String.length b)
  | VBytes b, "is_empty", [] -> VBool (b = "")
  | VBytes b, "get", [ VInt i ] -> if i >= 0L && i < Int64.of_int (String.length b) then some (vint (Char.code b.[Int64.to_int i])) else none
  | VBytes b, "slice", [ VInt lo; VInt hi ] ->
    let n = String.length b in
    let lo = clamp lo n in
    let hi = max lo (clamp hi n) in
    VBytes (String.sub b lo (hi - lo))
  | VBytes b, "to_list", [] -> VList (Array.init (String.length b) (fun i -> vint (Char.code b.[i])))
  | VBytes b, "to_str", [] -> VStr b
  | VStr s, "bytes", [] -> VList (Array.init (String.length s) (fun i -> vint (Char.code s.[i])))
  | VInt n, "to_char", [] -> VStr (utf8_of_code (if n < 0L || n > 0x10FFFFL then -1 else Int64.to_int n))
  | VInt a, "min", [ VInt b ] -> VInt (min a b)
  | VInt a, "max", [ VInt b ] -> VInt (max a b)
  | VInt n, "to_str", [] -> VStr (Int64.to_string n)
  | VInt n, "to_float", [] -> VFloat (Int64.to_float n)
  | VInt n, "abs", [] -> VInt (if n < 0L then Int64.neg n else n)
  | VFloat x, "to_str", [] -> VStr (fmt_float x)
  | VFloat x, "to_int", [] -> VInt (float_to_int x)
  | VFloat x, "abs", [] -> VFloat (Float.abs x)
  | VFloat x, "sqrt", [] -> VFloat (Float.sqrt x)
  | VFloat x, "to_fixed", [ VInt d ] ->
    let d = Int64.to_int (Int64.max 0L (Int64.min 30L d)) in
    VStr
      (if Float.is_nan x then "nan"
       else if x = Float.infinity then "inf"
       else if x = Float.neg_infinity then "-inf"
       else sprintf "%.*f" d x)
  | VBool b, "to_str", [] -> VStr (string_of_bool b)
  | VList a, "len", [] -> vint (Array.length a)
  | VList a, "to_list", [] -> VList a
  | VList a, "is_empty", [] -> VBool (Array.length a = 0)
  | VList a, "get", [ VInt i ] -> if i >= 0L && i < Int64.of_int (Array.length a) then some a.(Int64.to_int i) else none
  | VList a, "first", [] -> if Array.length a > 0 then some a.(0) else none
  | VList a, "last", [] -> if Array.length a > 0 then some a.(Array.length a - 1) else none
  | VList a, "contains", [ x ] -> VBool (Array.exists (equal x) a)
  | VList a, "reverse", [] ->
    let n = Array.length a in
    VList (Array.init n (fun i -> a.(n - 1 - i)))
  | VList a, "sort", [] ->
    let c = Array.copy a in
    Array.stable_sort compare_v c;
    VList c
  | VList a, "take", [ VInt k ] -> VList (Array.sub a 0 (clamp k (Array.length a)))
  | VList a, "skip", [ VInt k ] ->
    let k = clamp k (Array.length a) in
    VList (Array.sub a k (Array.length a - k))
  | VList a, "filter", [ f ] -> VList (Array.of_list (List.filter (fun x -> as_bool (call f [ x ])) (Array.to_list a)))
  | VList a, "map", [ f ] -> VList (Array.map (fun x -> call f [ x ]) a)
  | VList a, "fold", [ init; f ] -> Array.fold_left (fun acc x -> call f [ acc; x ]) init a
  | VList a, "join", [ VStr sep ] -> VStr (String.concat sep (List.map (fun x -> show x) (Array.to_list a)))
  | VList a, "sum", [] -> VInt (Array.fold_left (fun acc x -> match x with VInt n -> Int64.add acc n | _ -> acc) 0L a)
  | VEnum ("Option", vn, _), "is_some", [] -> VBool (vn = "Some")
  | VEnum ("Option", vn, _), "is_none", [] -> VBool (vn = "None")
  | VEnum ("Option", "Some", [ x ]), "map", [ f ] -> some (call f [ x ])
  | VEnum ("Option", _, _), "map", [ _ ] -> none
  | VEnum ("Result", vn, _), "is_ok", [] -> VBool (vn = "Ok")
  | VEnum ("Result", vn, _), "is_err", [] -> VBool (vn = "Err")
  | VEnum ("Result", "Err", [ e ]), "map_error", [ f ] -> VEnum ("Result", "Err", [ call f [ e ] ])
  | (VEnum ("Result", _, _) as r), "map_error", [ _ ] -> r
  | VGroup g, (("go" | "#go") as go), [ c ] ->
    (* an ordered group holds the task's output in a sink of its own *)
    let sink =
      if g.ordered && not g.gcancelled then begin
        let k = { sgroup = g; held = Buffer.create 0; sdone = false } in
        Queue.push k g.waiting;
        Some k
      end
      else g.gparent
    in
    let t = { cancelled = g.gcancelled; scopes = []; sink } in
    let res = ref (if g.gcancelled then Some Stopped else None) in
    g.tasks <- (t, res) :: g.tasks;
    g.kept <- (go = "go") :: g.kept;
    (* a task cancelled before it started never runs *)
    if not g.gcancelled then
      spawn_fiber t (fun () ->
          res :=
            Some
              (if t.cancelled then Stopped
               else try Done (apply rt c [] sp) with Panic (m, s) -> Failed (m, s) | Cancelled -> Stopped);
          (* finished: its output goes out once the tasks before it have *)
          match sink with
          | Some k when k.sgroup == g ->
            k.sdone <- true;
            advance rt.out g
          | _ -> ());
    VUnit
  | VChan ch, "send", [ v ] -> chan_send ch v sp; VUnit
  | VChan ch, "recv", [] -> ( match chan_recv ch with Some v -> some v | None -> none)
  | VChan ch, "close", [] ->
    if ch.closed then raise (Panic ("close of a closed channel", sp));
    chan_close ch;
    VUnit
  | VCap CRand, "int", [ VInt lo; VInt hi ] ->
    if hi <= lo then raise (Panic (sprintf "rand.int: empty range %Ld..%Ld" lo hi, sp));
    let span = Int64.sub hi lo in
    VInt (Int64.add lo (Int64.unsigned_rem (rand_next ()) span))
  | VCap c, _, _ -> cap_call rt c name args sp
  | _ -> raise (Panic (sprintf "internal: no method `%s` on %s" name (show ~top:false v), sp))

and sock_result : 'a. ('a, Caps.ioerr) result -> ('a -> value) -> value = fun r f -> io_result f r

and cap_call rt c name args sp =
  match (c, name, args) with
  | CStdout, "print", [ VStr s ] -> put rt.out !current.sink (s ^ "\n"); VUnit
  | CStdout, "write", [ VStr s ] -> put rt.out !current.sink s; VUnit
  | CClock, "now_ms", [] -> VInt (Int64.of_float (Unix.gettimeofday () *. 1000.))
  | CClock, "sleep", [ VInt ms ] -> sleep_ms ms; VUnit
  | CRand, "float", [] -> VFloat (Int64.to_float (Int64.shift_right_logical (rand_next ()) 11) *. 0x1p-53)
  | CStdin, "read_line", [] -> (
    unhold rt.out !current.sink;
    match Stdin.read_line () with Some l -> some (VStr l) | None -> none)
  | CStdin, "read_all", [] ->
    unhold rt.out !current.sink;
    VStr (Stdin.read_all ())
  | CEnv names, "get", [ VStr n ] ->
    if List.mem "*" names || List.mem n names then match Sys.getenv_opt n with Some v -> some (VStr v) | None -> none
    else none
  | CRoot, "args", [] -> VList (Array.of_list (List.map (fun a -> VStr a) rt.grants.args))
  | CFs fs, "append", [ VStr p; VStr data ] -> io_result (fun () -> VUnit) (Caps.append fs p data)
  | CFs fs, "list", [ VStr p ] -> io_result (fun l -> VList (Array.of_list (List.map (fun s -> VStr s) l))) (Caps.list fs p)
  | CFs fs, "remove", [ VStr p ] -> io_result (fun () -> VUnit) (Caps.remove fs p)
  | CFs fs, "make_dir", [ VStr p ] -> io_result (fun () -> VUnit) (Caps.make_dir fs p)
  | CFs fs, "is_dir", [ VStr p ] -> VBool (Caps.is_dir fs p)
  | CNet allowed, "only", [ VStr h ] -> VCap (CNet (Caps.attenuate allowed h))
  | CNet allowed, "get", [ VStr url ] ->
    io_result
      (fun (status, hs, body) ->
        let headers = List.fold_left (fun m (k, v) -> map_put m (VStr k) (VStr v)) empty_map hs in
        VStruct ("HttpResponse", [ ("status", vint status); ("headers", VMap headers); ("body", VStr body) ]))
      (Sock.http_get allowed url)
  | CFs fs, "under", [ VStr p ] -> VCap (CFs (Caps.under fs p))
  | CFs fs, "read_only", [] -> VCap (CFs { fs with ro = true })
  | CFs fs, "read", [ VStr p ] -> io_result (fun s -> VStr s) (Caps.read fs p)
  | CFs fs, "write", [ VStr p; VStr data ] -> io_result (fun () -> VUnit) (Caps.write fs p data)
  | CFs fs, "exists", [ VStr p ] -> VBool (Caps.exists fs p)
  | CFs fs, "full_path", [ VStr p ] -> io_result (fun s -> VStr s) (Caps.full_path fs p)
  | CFs fs, "is_read_only", [] -> VBool fs.ro
  | CNet allowed, "connect", [ VStr addr ] -> sock_result (Sock.connect allowed addr) (fun fd -> VCap (CConn fd))
  | CNet allowed, "connect_tls", [ VStr addr ] -> sock_result (Sock.connect_tls allowed addr) (fun fd -> VCap (CConn fd))
  | CListen allowed, "on_tls", [ VStr addr; VStr cert; VStr key ] ->
    sock_result (Sock.listen_tls allowed addr cert key) (fun (fd, port) -> VCap (CListener (fd, port)))
  | CListen allowed, "on", [ VStr addr ] -> sock_result (Sock.listen allowed addr) (fun (fd, port) -> VCap (CListener (fd, port)))
  | CListener (fd, _), "accept", [] -> sock_result (Sock.accept fd) (fun c -> VCap (CConn c))
  | CListener (_, port), "port", [] -> vint port
  | (CListener (fd, _) | CConn fd), "close", [] -> Sock.close fd; VUnit
  | CConn fd, "read", [] -> sock_result (Sock.read fd) (fun s -> VStr s)
  | CConn fd, "write", [ VStr s ] -> sock_result (Sock.write fd s) (fun () -> VUnit)
  | CForeign allowed, "only", [ VStr l ] -> VCap (CForeign (Foreign.only allowed l))
  | CForeign allowed, "allows", [ VStr l ] -> VBool (Foreign.permits allowed l)
  | CForeign allowed, "copy", [ VStruct (_, [ (_, VInt p) ]); VInt n ] -> Foreign.copy allowed p n sp
  | CExec allowed, "only", [ VStr p ] -> VCap (CExec (Proc.only allowed p))
  | CExec allowed, "run", [ VStr p; VList args ] ->
    let args = Array.to_list (Array.map (function VStr a -> a | _ -> failwith "internal: exec argument") args) in
    io_result
      (fun (code, o, e) -> VStruct ("ProcessOutput", [ ("code", vint code); ("stdout", VStr o); ("stderr", VStr e) ]))
      (Proc.run ~env_grant:rt.grants.env allowed p args)
  | _ -> failwith ("internal: capability method " ^ name)

(* The status a process exits with when `main` returns [n]: the low 8 bits,
   as the operating system keeps them (the native backend gets the same from
   returning n from C's main). *)
let exit_code n = Int64.to_int (Int64.logand n 255L)

(* Run `main`. Returns its value, or the panic message and location. *)
(* Restrict this process to [grants] (vela_sandbox.h). Ok true when the
   operating system enforces it, Ok false when it has no sandbox here. *)
let sandbox (g : grants) : (bool, string) result =
  let root, ro = match g.fs with Some (r, ro) -> (r, ro) | None -> ("", false) in
  match Foreign.sandbox_enter root ro (g.net <> [] || g.listen <> []) (g.exec <> []) with
  | 0 -> Ok true
  | _ -> Ok false
  | exception Failure m -> Error m

let run ?(before_main = fun () -> ()) ~out ~grants (user_items : item list) : (value, string * Span.t) result =
  seed_rand grants.seed;
  let rt =
    { fns = Hashtbl.create 16; structs = Hashtbl.create 16; enums = Hashtbl.create 16;
      ctors = Hashtbl.create 16; out; grants }
  in
  let owners = Hashtbl.create 16 in
  List.iter
    (function
      | IIface _ | IType _ -> ()
      | IFn d -> Hashtbl.replace rt.fns d.fn_name d
      | IStruct s -> Hashtbl.replace rt.structs s.st_name s
      | IEnum e ->
        Hashtbl.replace rt.enums e.en_name e;
        List.iteri (fun i (v, _, _) -> Hashtbl.replace variant_pos (e.en_name, v) i) e.en_variants;
        List.iter
          (fun (v, _, _) -> Hashtbl.replace owners v (e.en_name :: Option.value ~default:[] (Hashtbl.find_opt owners v)))
          e.en_variants)
    (Prelude.items () @ user_items);
  Hashtbl.iter (fun v ens -> match ens with [ en ] -> Hashtbl.replace rt.ctors v en | _ -> ()) owners;
  match Hashtbl.find_opt rt.fns "main" with
  | None -> Error ("no `main` function", Span.dummy)
  | Some d -> (
    match Foreign.resolve grants.foreign user_items with
    | Error e -> Error e
    | Ok () ->
    before_main ();
    let args = match d.fn_params with [] -> [] | _ -> [ VCap CRoot ] in
    (* main is a coroutine too, so it can wait on groups and channels *)
    Queue.clear runq;
    live_groups := [];
    held_bytes := 0;
    Stdin.reset ();
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    let result = ref None in
    try
      spawn_fiber { cancelled = false; scopes = []; sink = None } (fun () -> result := Some (call_fn rt d args));
      schedule (fun () -> !result <> None) d.fn_span;
      Queue.clear runq;
      (* a `dyn Error` from main is reported by its message *)
      match (Option.get !result, d.fn_ret) with
      | ( VEnum ("Result", "Err", [ e ]),
          Some { tdesc = TyName ("Result", [ _; { tdesc = TyName ("dyn", [ { tdesc = TyName ("Error", []); _ } ]); _ } ]); _ } ) ->
        let m = ref None in
        spawn_fiber { cancelled = false; scopes = []; sink = None } (fun () -> m := Some (call_method rt e "message" [] d.fn_span));
        schedule (fun () -> !m <> None) d.fn_span;
        Queue.clear runq;
        Ok (VEnum ("Result", "Err", [ Option.get !m ]))
      | r, _ -> Ok r
    with
    | Panic (m, sp) | Fatal (m, sp) -> Queue.clear runq; release_all rt.out; Error (m, sp)
    | Stack_overflow -> Queue.clear runq; release_all rt.out; Error ("stack overflow", d.fn_span))

let make_rt ~out ~grants (user_items : item list) =
  let rt =
    { fns = Hashtbl.create 16; structs = Hashtbl.create 16; enums = Hashtbl.create 16;
      ctors = Hashtbl.create 16; out; grants }
  in
  let owners = Hashtbl.create 16 in
  List.iter
    (function
      | IIface _ | IType _ -> ()
      | IFn d -> Hashtbl.replace rt.fns d.fn_name d
      | IStruct s -> Hashtbl.replace rt.structs s.st_name s
      | IEnum e ->
        Hashtbl.replace rt.enums e.en_name e;
        List.iteri (fun i (v, _, _) -> Hashtbl.replace variant_pos (e.en_name, v) i) e.en_variants;
        List.iter
          (fun (v, _, _) -> Hashtbl.replace owners v (e.en_name :: Option.value ~default:[] (Hashtbl.find_opt owners v)))
          e.en_variants)
    (Prelude.items () @ user_items);
  Hashtbl.iter (fun v ens -> match ens with [ en ] -> Hashtbl.replace rt.ctors v en | _ -> ()) owners;
  rt

(* Run every `test` block in its own coroutine scheduler; a panic fails
   only that test. Returns (name, Ok () | Error (message, location), seconds)
   for the tests [select] picks. *)
let run_tests_timed ?(select = fun _ -> true) ~out (user_items : item list) :
    (string * (unit, string * Span.t) result * float) list =
  let rt = make_rt ~out ~grants:no_grants user_items in
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let resolved = Foreign.resolve no_grants.foreign user_items in
  List.filter_map
    (function
      | IFn ({ fn_test = Some name; fn_bench = false; _ } as d) when select name ->
        let start = Unix.gettimeofday () in
        Queue.clear runq;
        let finished = ref false in
        (* capabilities the test asked for: a fresh directory as Fs, the real Clock *)
        let dir = if Caps.takes_fs d then Some (Caps.test_dir ()) else None in
        let args =
          List.map
            (fun (p : param) ->
              match (p.prm_ty.tdesc, dir) with
              | TyName ("Fs", _), Some dir -> VCap (CFs { Caps.root = Some dir; ro = false })
              | _ -> VCap CClock)
            d.fn_params
        in
        let r =
          match resolved with
          | Error e -> Error e
          | Ok () ->
          try
            spawn_fiber { cancelled = false; scopes = []; sink = None } (fun () -> ignore (call_fn rt d args); finished := true);
            schedule (fun () -> !finished) d.fn_span;
            Ok ()
          with
          | Panic (m, sp) | Fatal (m, sp) -> release_all out; Error (m, sp)
          | Stack_overflow -> release_all out; Error ("stack overflow", d.fn_span)
        in
        Queue.clear runq;
        live_groups := [];
        Option.iter Caps.remove_tree dir;
        Some (name, r, Unix.gettimeofday () -. start)
      | _ -> None)
    user_items

let run_tests ~out user_items = List.map (fun (n, r, _) -> (n, r)) (run_tests_timed ~out user_items)
