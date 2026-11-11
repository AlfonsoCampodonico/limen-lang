(* A byte range in a source file. *)
type t = { file : string; lo : int; hi : int }

let dummy = { file = ""; lo = 0; hi = 0 }
let merge a b = { a with hi = max a.hi b.hi }
let at_start s = { s with hi = s.lo }
