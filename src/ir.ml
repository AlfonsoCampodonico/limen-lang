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
  | PKey of atom * Span.t (* map entry by key; panics when the key is missing *)

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
  | GroupNew of (atom * atom) option (* `until clock.after(ms)`: clock, milliseconds *)
  | Spawn of atom * atom * ty * bool
      (* group (borrowed), task closure (consumed), result type, whether its result goes in the group's list *)
  | GroupJoin of atom * ty * bool
      (* wait for every task; result list type; with a deadline (each result an Option) *)
  | ChanNew of ty * atom * Span.t (* channel type, capacity *)
  | ChanSend of atom * atom * Span.t (* channel (borrowed), value (consumed) *)
  | MkDyn of ty * string * atom * (string * ty list * ty) list
      (* box a value of a concrete type as `dyn I` (consumed); one entry per
         interface method: the C function implementing it, its parameter
         types after self, and its result type *)
  | Select of int * sel_op list * bool * Span.t
      (* select number, operations, has a `_` arm; result: the index of the
         operation done, or -1 for `_`. Channels borrowed, send values consumed. *)
  | SelGot of int * int * ty (* the Option a receive in select [n], operation [i], produced *)
  | Catch of atom * ty (* call a closure (borrowed) under a panic boundary; ty is its result type *)
  | CallDyn of int * ty list * ty * atom * atom list (* method index, params, result, receiver (borrowed), args (consumed) *)
  | CallC of Ffi.csig * atom list * (string * string * Span.t * bool) list * Span.t
      (* a foreign C function; the Foreign capability first, then the
         non-Out arguments that are not callbacks; all borrowed. Then the
         callbacks, in order: each top-level function's C name, its Vela
         name, where it is declared, and whether it takes the call's
         Foreign first. *)

and sel_op =
  | SRecv of atom * ty * atom (* channel, element type, enabled (its guard) *)
  | SSend of atom * atom * ty * atom (* channel, value, element type, enabled *)
  | SAfter of atom * atom * atom (* clock, milliseconds, enabled: ready once that time has passed *)

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
  | Guard of int * stmt list * stmt list
      (* a `defer`: guard number, the deferred body to run when a panic or a
         cancellation leaves the protected statements, and those statements.
         Normal exits run the body themselves, after a GuardEnd. *)
  | GuardEnd of int (* leave guard [n] normally: pop its panic boundary *)
  | Scope of atom * stmt list
      (* the body of a group with a deadline: the deadline stops it at its
         next wait, and the code after it (the join) goes on *)

and mutation =
  | MSet of atom
  | MPush of atom
  | MPut of atom * atom (* map: key, value *)
  | MRemove of atom (* map: key *)

let mut_atoms = function MSet a | MPush a | MRemove a -> [ a ] | MPut (k, v) -> [ k; v ]

type fn = {
  fname : string;
  params : var list;
  captures : var list; (* non-empty only for lifted closures *)
  is_closure : bool;
  ret : ty;
  body : stmt list;
}

type program = { fns : fn list; main : string; main_takes_root : bool; main_ret : ty; tests : (string * string list) list (* C names, in order, and the root fields each takes *);
  benches : (string * string list) list (* likewise, for `bench` blocks *) }

(* Which atoms of an rhs are consumed (ownership moves) and which are only read. *)
let consumed = function
  | Atom a -> [ a ]
  | Call (_, args) -> args
  | CallClo (_, args) -> args
  | MkStruct (_, xs) | MkEnum (_, _, xs) | MkList (_, xs) | MkClosure (_, _, xs) -> xs
  | Spawn (_, c, _, _) -> [ c ]
  | ChanSend (_, v, _) -> [ v ]
  | MkDyn (_, _, a, _) -> [ a ]
  | Select (_, ops, _, _) -> List.filter_map (function SSend (_, v, _, _) -> Some v | SRecv _ | SAfter _ -> None) ops
  | CallDyn (_, _, _, _, args) -> args
  | _ -> []

let borrowed = function
  | Atom _ | Call _ | MkStruct _ | MkEnum _ | MkList _ | MkClosure _ -> []
  | CallClo (c, _) -> [ c ]
  | CallDyn (_, _, _, r, _) -> [ r ]
  | Catch (c, _) -> [ c ]
  | Select (_, ops, _, _) ->
    List.concat_map (function SSend (c, _, _, g) | SRecv (c, _, g) -> [ c; g ] | SAfter (c, ms, g) -> [ c; ms; g ]) ops
  | SelGot _ -> []
  | MkDyn _ -> []
  | Bin (_, _, a, b, _) -> [ a; b ]
  | Neg (_, a) | Not a -> [ a ]
  | Prim (_, _, xs, _) -> xs
  | CallC (_, xs, _, _) -> xs
  | Get (a, _, steps) -> a :: List.filter_map (function PIndex (i, _) | PIndexU i | PKey (i, _) -> Some i | PField _ -> None) steps
  | Payload (a, _, _, _) | IsTag (a, _, _) | PatTest (a, _, _) -> [ a ]
  | Interp ps -> List.filter_map (function IVal (a, _) | IValQ (a, _) -> Some a | IStr _ -> None) ps
  | RootField (a, _) -> [ a ]
  | Panic (a, _) -> [ a ]
  | Spawn (g, _, _, _) | GroupJoin (g, _, _) -> [ g ]
  | GroupNew None -> []
  | GroupNew (Some (c, ms)) -> [ c; ms ]
  | ChanNew (_, cap, _) -> [ cap ]
  | ChanSend (c, _, _) -> [ c ]

let atom_var = function V v | VDup v -> Some v | _ -> None
