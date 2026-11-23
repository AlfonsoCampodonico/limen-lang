(* The built-ins written once in C (runtime/limen_core.h, through the stubs
   in core_stubs.c), so the interpreter and native code give the same
   answers. *)

(* [s]'s lines by SPEC.md's rule for `lines()` (lc_lines_next). *)
external lines : string -> string array = "limen_core_lines"

(* Float to Int (lc_f2i): NaN is 0, out of range saturates. *)
external f2i : (float [@unboxed]) -> (int64 [@unboxed]) = "limen_core_f2i_byte" "limen_core_f2i" [@@noalloc]

(* A UInt's 64 bits to the nearest Float, ties to even (lc_u2f). *)
external u2f : (int64 [@unboxed]) -> (float [@unboxed]) = "limen_core_u2f_byte" "limen_core_u2f" [@@noalloc]
