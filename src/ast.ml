type span = Span.t

type ty_expr = { tdesc : ty_desc; tspan : span }
and ty_desc =
  | TyName of string * ty_expr list
  | TyUnit
  | TyFn of ty_expr list * ty_expr
  | TyInt of int (* a size, as in Array[Int, 3] *)

type binop =
  | Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or | Else (* `??` *) | Range (* `a..<b` *)
  | BitAnd (* `&` *) | BitOr (* `|` *) | BitXor (* `^` *) | Shl (* `<<` *) | Shr (* `>>`, arithmetic *)
type unop = Neg | Not | BitNot (* `~` *)

type pat = { pdesc : pat_desc; pspan : span }
and pat_desc =
  | PWild
  | PBind of string
  | PInt of int64
  | PStr of string
  | PBool of bool
  | PVariant of string option * string * pat list (* qualifier, variant, payload *)
  | PTuple of pat list
  | PList of pat list (* `[a, b, ..rest]` *)
  | PRest of string option
      (* `..`, any remaining parts, as an item of a tuple, variant or list
         pattern; `..name` (lists only) binds them as a list *)
  | POr of pat list (* `A | B`: two or more alternatives, none of them a POr *)
  | PStruct of string * span * (string * span * pat) list * bool
      (* `Point { x, y: 0, .. }`: the struct (`geo.Point` until modules load)
         and its span, each field named with its pattern and the field's
         span (`x` alone is `x: x`, a PBind with the field's span), and
         whether `..` stands for the fields not named *)
  | PAs of pat * string * span (* `p as name`: matches [p], and binds the whole value to [name] too *)

let is_rest (p : pat) = match p.pdesc with PRest _ -> true | _ -> false

(* The items of a tuple or variant pattern for a value with [n] parts: a
   `..` among them stands for as many `_` as make up the count. The checker
   reports a pattern with too many items; elsewhere [n] is always enough. *)
let fill_rest (ps : pat list) n =
  if not (List.exists is_rest ps) then ps
  else
    let k = List.length ps - 1 in
    List.concat_map (fun p -> if is_rest p then List.init (max 0 (n - k)) (fun _ -> { p with pdesc = PWild }) else [ p ]) ps

(* A list pattern's items before the `..`, the `..` itself (None when there
   is none, Some name when it binds the rest), and the items after it. *)
let list_parts (ps : pat list) =
  let rec go before = function
    | [] -> (List.rev before, None, [])
    | { pdesc = PRest r; _ } :: after -> (List.rev before, Some r, after)
    | p :: rest -> go (p :: before) rest
  in
  go [] ps

(* Does [p] match every value of its type? Names, `_`, tuples of those,
   `[..rest]`, and or-patterns with such an alternative. *)
let rec irrefutable (p : pat) =
  match p.pdesc with
  | PWild | PBind _ | PRest _ -> true
  | PTuple ps -> List.for_all irrefutable ps
  | PList [ { pdesc = PRest _; _ } ] -> true
  | POr ps -> List.exists irrefutable ps
  | PStruct (_, _, fs, _) -> List.for_all (fun (_, _, q) -> irrefutable q) fs
  | PAs (q, _, _) -> irrefutable q
  | _ -> false

(* The alternatives of an or-pattern, or the pattern alone. *)
let alternatives (p : pat) = match p.pdesc with POr ps -> ps | _ -> [ p ]

(* The pattern without the `as name` around it. *)
let rec unnamed (p : pat) = match p.pdesc with PAs (q, _, _) -> unnamed q | _ -> p

(* The items of a struct pattern in the order of the struct's [fields]: each
   named field's pattern, `_` for the others. *)
let struct_items (fs : (string * span * pat) list) (fields : string list) (sp : span) =
  List.map (fun n -> match List.find_opt (fun (m, _, _) -> m = n) fs with Some (_, _, q) -> q | None -> { pdesc = PWild; pspan = sp }) fields

(* The bounds on type parameters: each parameter with the interfaces it must
   satisfy, written as types so module names and aliases resolve as in any
   type: `Shape`, `geo.Area`, `Container[Int]`. *)
type bounds = (string * ty_expr list) list

(* [ety] and [einst] are filled in by the checker: the expression's type, and
   for a reference to a generic function, the type arguments it was used at. *)
type expr = {
  mutable edesc : expr_desc; (* the checker wraps a value in EDyn where a `dyn` is expected *)
  espan : span;
  mutable ety : Types.ty;
  mutable einst : (string * Types.ty) list;
  mutable ecallee : fn_decl option;
      (* the interpreter's cache: for a name that refers to a top-level
         function, that function, found the first time the name is evaluated.
         Scoping is lexical, so it is the same every time. *)
}
and expr_desc =
  | EInt of int64
  | EFloat of float
  | EBool of bool
  | EUnit
  | EStr of str_part list
  | EVar of string
  | EList of expr list
  | EBin of binop * expr * expr
  | EUn of unop * expr
  | ECall of expr * expr list
  | EMethod of expr * string * span * expr list
  | EField of expr * string * span
  | EIndex of expr * expr
  | ETry of expr * expr option (* `x?`, or `x? "context"` *)
  | EIf of expr * block * expr option
  | EMatch of expr * arm list
  | EBlock of block
  | EClosure of (string * ty_expr option * span) list * expr
  | EStruct of string * span * (string * span * expr) list
  | EGroup of string * bool * (expr * expr) option * block
      (* `group g interleaved until clock.after(ms) { ... }`: whether its tasks'
         output interleaves instead of following start order, and the
         deadline's clock and milliseconds *)
  | EDyn of expr * string (* inserted by the checker: box a value as `dyn Interface` *)
  | ESelect of sel_arm list
  | EPipe of expr * expr (* `x |> f(a)`: becomes `f(x, a)` when modules load *)
  | EWith of expr * (string * span * expr) list (* `{ p with x: 1 }`: a copy of a struct with fields replaced *)
  | ESafe of expr * string * expr * bool
      (* `a?.b.c`: the Option [a]; a name for the value inside it; the rest of
         the chain, `name.b.c`, run on that value; and (set by the checker)
         whether that rest is an Option already, so its result is not wrapped
         again. The name ("safe$N") cannot be written in source. *)
  | ENamed of string * span * expr
      (* `name: value`, a call argument naming its parameter; only between the
         parser and the checker, which puts arguments in parameter order *)
  | EMut of expr
      (* `mut place`, a call argument for a `mut` parameter: the callee
         changes the caller's place. Its span runs from `mut` to the end of
         the place. *)

and str_part = SPLit of string | SPExpr of expr

(* `select { v = c.recv() => ..., c.send(x) => ..., _ => ... }` *)
and sel_arm = { sop : sel_op; sguard : expr option (* `if cond`: skip the arm when false *); sbody : expr; sspan_ : span }
and sel_op =
  | SelRecv of (string * span) option * expr (* binding (None: `_ =` or no binding), channel *)
  | SelSend of expr * expr (* channel, value *)
  | SelAfter of expr * expr (* `clock.after(ms)`: clock, milliseconds *)
  | SelDefault
and arm = { apat : pat; aguard : expr option (* `if cond`: the arm applies only when true *); abody : expr }

(* A function's parameter; [prm_default] is its default value, a constant
   (`sep: Str = ", "`), used when a call leaves the argument out. [prm_mut]:
   written `mut name: Type`, so the callee changes the caller's place. *)
and param = { prm_name : string; prm_span : span; prm_ty : ty_expr; prm_default : expr option; prm_mut : bool }
and block = { stmts : stmt list; tail : expr option; bspan : span }
and stmt = { sdesc : stmt_desc; sspan : span }
and stmt_desc =
  | SLet of bool * string * span * ty_expr option * expr
  | SAssign of expr * expr
  | SExpr of expr
  | SReturn of expr option
  | SWhile of expr * block
  | SFor of string * span * expr * block
  | SLetPat of pat * expr (* `let (a, b) = e`; expanded into plain lets when modules load *)
  | SForPat of pat * expr * block (* `for (k, v) in xs { ... }`; expanded likewise *)
  | SBreak
  | SContinue
  | SDefer of block (* runs when the enclosing block is left, in reverse order of the defers *)

and fn_decl = {
  fn_name : string;
  fn_span : span;
  fn_gen : string list;
  fn_params : param list;
  fn_ret : ty_expr option;
  fn_body : block;
  fn_test : string option; (* Some name: a `test "name" { ... }` or `bench "name" { ... }` block *)
  fn_bench : bool; (* a `bench` block: run by `limen test --bench`, timed, never as a test *)
  fn_virtual : bool; (* a test that takes `clock: virtual Clock`: it runs on virtual time *)
  fn_table : (pat * expr) option;
      (* a table test, `for PAT in [case, ...]` (the expression is that list): one isolated run per
         case. Expanded when modules load into one test per case, each keeping its own case here. *)
  fn_pub : bool;
  fn_bounds : bounds; (* type parameter -> interfaces it must satisfy *)
  fn_foreign : foreign option; (* declared in a `foreign "lib" { ... }` block; no body *)
}

(* A C function: its library, its symbol, and whether it is trusted. Only
   the foreign blocks of standard library modules that src/std/limen.toml
   names in [trust] are trusted: their calls take no Foreign capability.
   [fsym] is the function's own name unless the declaration names another
   (`fn print_int(...) -> Int32 = "printf"`). [fvariadic]: for a variadic
   function (`...` among its parameters), how many parameters come before
   the `...`. [fblocking]: declared `blocking fn`, so natively a call runs
   on a thread of the blocking-call pool while the task waits (SPEC 9). *)
and foreign = { flib : string; fsym : string; ftrusted : bool; fvariadic : int option; fblocking : bool }

type struct_decl = {
  st_name : string;
  st_span : span;
  st_gen : string list;
  st_fields : (string * span * ty_expr) list;
  st_bounds : bounds; (* `struct Sorted[T: Ordered]`: checked wherever the type is instantiated *)
  st_embeds : string list;
      (* the fields written as a bare type, `struct Admin { User, level: Int }`:
         an embedded struct, stored as a field named after its type (`User`),
         whose fields and methods the checker promotes onto the outer struct *)
  st_pub : bool;
  st_foreign : (string * ty_expr list) option;
      (* declared in a `foreign "lib" { struct ... }` block: a C struct passed
         by value. The library, and the field types as written (Int32 among
         them); the loader turns Int32 fields into Int for the rest of Limen. *)
}

type enum_decl = {
  en_name : string;
  en_span : span;
  en_gen : string list;
  en_variants : (string * span * ty_expr list) list;
  en_bounds : bounds;
  en_pub : bool;
}

(* `interface Shape { fn area(self) -> Float }`: the methods a type needs.
   Satisfied structurally, by any type that has those methods. A generic
   interface, `interface Container[T] { ... }`, is named with its arguments
   wherever it is used: `[C: Container[Int]]`, `dyn Container[Str]`. *)
type iface_decl = {
  if_name : string;
  if_span : span;
  if_gen : string list;
  if_methods : (string * span * param list * ty_expr option) list; (* `self` is left out of the params *)
  if_pub : bool;
}

(* `type Seen = Map[Str, Int]`, `type Pair[T] = (T, T)`: another name for a
   type, the same type wherever it is written. Expanded when modules load,
   so the checker and both backends never see one. *)
type alias_decl = { ty_name : string; ty_span : span; ty_gen : string list; ty_body : ty_expr; ty_pub : bool }

type item = IFn of fn_decl | IStruct of struct_decl | IEnum of enum_decl | IIface of iface_decl | IType of alias_decl

(* Tuples are built-in generic structs named `#tupleN` with fields `0`, `1`, ...;
   the name cannot be written in source. *)
let tuple_name n = "#tuple" ^ string_of_int n
let is_tuple_name s = String.length s > 6 && String.sub s 0 6 = "#tuple"

(* `x |> rhs`: the call it stands for, with [x] as the first argument.
   `f(a)` and `r.m(a)` get it in front of their arguments, `f` and `r.m`
   become one-argument calls, and `rhs?` pipes into the call inside the `?`.
   None: the right side is not something a value can be piped into. *)
let rec pipe_call (x : expr) (rhs : expr) : expr_desc option =
  match rhs.edesc with
  | ECall (f, args) -> Some (ECall (f, x :: args))
  | EMethod (r, m, msp, args) -> Some (EMethod (r, m, msp, x :: args))
  | EField (r, m, msp) when not (String.length m > 0 && m.[0] >= '0' && m.[0] <= '9') -> Some (EMethod (r, m, msp, [ x ]))
  | EVar _ | EClosure _ -> Some (ECall (rhs, [ x ]))
  | ETry (inner, ctx) -> (
    match pipe_call x inner with
    | Some d -> Some (ETry ({ inner with edesc = d; espan = Span.merge x.espan inner.espan }, ctx))
    | None -> None)
  | _ -> None

(* A parsed file: its `use` declarations and its items. *)
type file = { uses : (string * span) list; items : item list }
