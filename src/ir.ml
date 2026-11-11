(* A small structured IR in A-normal form. Every type is concrete (no
   generics, no unknowns). Every variable owns its value; the ownership pass
   (Own) decides where to retain (VDup) and where to release (Drop). *)
open Types

type var = { id : int; name : string; ty : ty }

type atom =
  | V of var (* use of a variable; a move when consumed *)
  | VDup of var (* consumed while still live elsewhere: retain first *)
  | AInt of int64
  | AFloat of float
  | ABool of bool
  | AUnit
  | AStr of string

(* A step into a value: struct field (by declaration index) or list element. *)
type step =
  | PField of int
  | PIndex of atom * Span.t
  | PIndexU of atom (* index already proven in range (compiler-generated loops) *)

type rhs =
  | Atom of atom (* consumed *)
  | Bin of Ast.binop * ty * atom * atom * Span.t (* operand type; borrowed *)
  | Neg of ty * atom
  | Not of atom
  | Call of string * atom list (* consumed *)
  | CallClo of atom * atom list (* closure borrowed, args consumed *)
  | Prim of string * ty * atom list * Span.t (* builtin method on receiver type; borrowed *)
  | MkStruct of ty * atom list (* consumed *)
  | MkEnum of ty * int * atom list (* consumed *)
  | MkList of ty * atom list (* consumed *)
  | MkClosure of string * ty * atom list (* lifted function, captures consumed *)
  | Get of atom * ty * step list (* read through steps; borrowed, result retained *)
  | Payload of atom * ty * int * int (* enum value, enum type, tag, field *)
  | IsTag of atom * ty * int
  | PatTest of atom * ty * Ast.pat
  | Interp of ipart list
  | RootField of atom * string
  | Panic of atom * Span.t
  | GroupNew
  | Spawn of atom * atom * ty (* group (borrowed), task closure (consumed), result type *)
  | GroupJoin of atom * ty (* wait for every task; result list type *)
  | ChanNew of ty * atom * Span.t (* channel type, capacity *)
  | ChanSend of atom * atom * Span.t (* channel (borrowed), value (consumed) *)
  | MkDyn of ty * string * atom * (string * ty list * ty) list
      (* box a value of a concrete type as `dyn I` (consumed); one entry per
         interface method: the C function implementing it, its parameter
         types after self, and its result type *)
  | CallDyn of int * ty list * ty * atom * atom list (* method index, params, result, receiver (borrowed), args (consumed) *)

and ipart = IStr of string | IVal of atom * ty | IValQ of atom * ty (* printed as inside a container *)

type stmt =
  | Let of var * rhs
  | Decl of var (* declared, initialised later by Init in each branch *)
  | Init of var * rhs
  | Set of var * rhs * bool ref (* reassign; the flag says whether to release the old value *)
  | Mutate of var * step list * mutation
  | If of atom * stmt list * stmt list
  | Loop of stmt list
  | Break
  | Continue
  | Return of atom
  | Drop of var

and mutation = MSet of atom | MPush of atom

type fn = {
  fname : string;
  params : var list;
  captures : var list; (* non-empty only for lifted closures *)
  is_closure : bool;
  ret : ty;
  body : stmt list;
}

type program = { fns : fn list; main : string; main_takes_root : bool; main_ret : ty; tests : string list (* C names, in order *) }

(* Which atoms of an rhs are consumed (ownership moves) and which are only read. *)
let consumed = function
  | Atom a -> [ a ]
  | Call (_, args) -> args
  | CallClo (_, args) -> args
  | MkStruct (_, xs) | MkEnum (_, _, xs) | MkList (_, xs) | MkClosure (_, _, xs) -> xs
  | Spawn (_, c, _) -> [ c ]
  | ChanSend (_, v, _) -> [ v ]
  | MkDyn (_, _, a, _) -> [ a ]
  | CallDyn (_, _, _, _, args) -> args
  | _ -> []

let borrowed = function
  | Atom _ | Call _ | MkStruct _ | MkEnum _ | MkList _ | MkClosure _ -> []
  | CallClo (c, _) -> [ c ]
  | CallDyn (_, _, _, r, _) -> [ r ]
  | MkDyn _ -> []
  | Bin (_, _, a, b, _) -> [ a; b ]
  | Neg (_, a) | Not a -> [ a ]
  | Prim (_, _, xs, _) -> xs
  | Get (a, _, steps) -> a :: List.filter_map (function PIndex (i, _) | PIndexU i -> Some i | PField _ -> None) steps
  | Payload (a, _, _, _) | IsTag (a, _, _) | PatTest (a, _, _) -> [ a ]
  | Interp ps -> List.filter_map (function IVal (a, _) | IValQ (a, _) -> Some a | IStr _ -> None) ps
  | RootField (a, _) -> [ a ]
  | Panic (a, _) -> [ a ]
  | Spawn (g, _, _) | GroupJoin (g, _) -> [ g ]
  | GroupNew -> []
  | ChanNew (_, cap, _) -> [ cap ]
  | ChanSend (c, _, _) -> [ c ]

let atom_var = function V v | VDup v -> Some v | _ -> None
