(* The built-ins written once in C (runtime/limen_core.h and limen_core_inline.h,
   through the stubs in core_stubs.c), so the interpreter and native code give the same
   answers. *)

(* [s]'s lines by SPEC.md's rule for `lines()` (lc_lines_next). *)
external lines : string -> string array = "limen_core_lines"

(* Float to Int (lc_f2i): NaN is 0, out of range saturates. *)
external f2i : (float [@unboxed]) -> (int64 [@unboxed]) = "limen_core_f2i_byte" "limen_core_f2i" [@@noalloc]

(* A UInt's 64 bits to the nearest Float, ties to even (lc_u2f). *)
external u2f : (int64 [@unboxed]) -> (float [@unboxed]) = "limen_core_u2f_byte" "limen_core_u2f" [@@noalloc]

(* [s] split on [sep] (lc_split_next): one piece per byte for an empty [sep],
   else the pieces between non-overlapping occurrences, always at least one. *)
external split : string -> string -> string array = "limen_core_split"

(* [s] without leading and trailing space, \t, \n, \r and \f (lc_trim). *)
external trim : string -> string = "limen_core_trim"

(* [s]'s UTF-8 characters, a byte that starts none on its own (lc_utf8_len). *)
external chars : string -> string array = "limen_core_chars"

(* The UTF-8 encoding of a code point, U+FFFD for anything that is not one
   (lc_utf8_encode). *)
external of_code : int64 -> string = "limen_core_of_code"

(* A decimal Int with an optional sign and surrounding spaces (lc_parse_int). *)
external parse_int : string -> int64 option = "limen_core_parse_int"

(* A Float as Float.to_str prints it (lc_fmt_float). *)
external fmt_float : float -> string = "limen_core_fmt_float"

(* nan, inf, -inf, or [d] decimals clamped to 0..30 (lc_to_fixed). *)
external to_fixed : float -> int64 -> string = "limen_core_to_fixed"

(* A decimal float with optional sign, point, exponent and surrounding
   spaces; no hex, nan or inf (lc_parse_float). *)
external parse_float : string -> float option = "limen_core_parse_float"
