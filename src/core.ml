(* The built-ins written once in C (runtime/limen_core.h, through the stubs
   in core_stubs.c), so the interpreter and native code give the same
   answers. *)

(* [s]'s lines by SPEC.md's rule for `lines()` (lc_lines_next). *)
external lines : string -> string array = "limen_core_lines"
