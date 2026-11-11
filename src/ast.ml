type span = Span.t

type ty_expr = { tdesc : ty_desc; tspan : span }
and ty_desc =
  | TyName of string * ty_expr list
  | TyUnit
  | TyFn of ty_expr list * ty_expr
  | TyInt of int (* a size, as in Array[Int, 3] *)

type binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or
type unop = Neg | Not

type pat = { pdesc : pat_desc; pspan : span }
and pat_desc =
  | PWild
  | PBind of string
  | PInt of int64
  | PStr of string
  | PBool of bool
  | PVariant of string option * string * pat list (* qualifier, variant, payload *)

(* [ety] and [einst] are filled in by the checker: the expression's type, and
   for a reference to a generic function, the type arguments it was used at. *)
type expr = {
  mutable edesc : expr_desc; (* the checker wraps a value in EDyn where a `dyn` is expected *)
  espan : span;
  mutable ety : Types.ty;
  mutable einst : (string * Types.ty) list;
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
  | ETry of expr
  | EIf of expr * block * expr option
  | EMatch of expr * arm list
  | EBlock of block
  | EClosure of (string * ty_expr option * span) list * expr
  | EStruct of string * span * (string * span * expr) list
  | EGroup of string * block
  | EDyn of expr * string (* inserted by the checker: box a value as `dyn Interface` *)

and str_part = SPLit of string | SPExpr of expr
and arm = { apat : pat; abody : expr }
and block = { stmts : stmt list; tail : expr option; bspan : span }
and stmt = { sdesc : stmt_desc; sspan : span }
and stmt_desc =
  | SLet of bool * string * span * ty_expr option * expr
  | SAssign of expr * expr
  | SExpr of expr
  | SReturn of expr option
  | SWhile of expr * block
  | SFor of string * span * expr * block
  | SBreak
  | SContinue

type param = { prm_name : string; prm_span : span; prm_ty : ty_expr }

type fn_decl = {
  fn_name : string;
  fn_span : span;
  fn_gen : string list;
  fn_params : param list;
  fn_ret : ty_expr option;
  fn_body : block;
  fn_test : string option; (* Some name: a `test "name" { ... }` block *)
  fn_pub : bool;
  fn_bounds : (string * (string * span) list) list; (* type parameter -> interfaces it must satisfy *)
}

type struct_decl = {
  st_name : string;
  st_span : span;
  st_gen : string list;
  st_fields : (string * span * ty_expr) list;
  st_pub : bool;
}

type enum_decl = {
  en_name : string;
  en_span : span;
  en_gen : string list;
  en_variants : (string * span * ty_expr list) list;
  en_pub : bool;
}

(* `interface Shape { fn area(self) -> Float }`: the methods a type needs.
   Satisfied structurally, by any type that has those methods. *)
type iface_decl = {
  if_name : string;
  if_span : span;
  if_methods : (string * span * param list * ty_expr option) list; (* `self` is left out of the params *)
  if_pub : bool;
}

type item = IFn of fn_decl | IStruct of struct_decl | IEnum of enum_decl | IIface of iface_decl

(* A parsed file: its `use` declarations and its items. *)
type file = { uses : (string * span) list; items : item list }
