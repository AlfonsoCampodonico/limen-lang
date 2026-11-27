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

(* Float's total order (lc_float_cmp): NaN first and equal to itself, -0.0
   equal to 0.0; -1, 0 or 1. *)
external float_cmp : (float [@unboxed]) -> (float [@unboxed]) -> (int [@untagged])
  = "limen_core_float_cmp_byte" "limen_core_float_cmp"
[@@noalloc]

(* Str and Bytes order (lc_bytes_cmp): byte by byte, then length; -1, 0 or 1. *)
external str_cmp : string -> string -> int = "limen_core_str_cmp" [@@noalloc]

(* [s] as it prints inside a container, quotes included (lc_escape). *)
external escaped : string -> string = "limen_core_escaped"

(* Rand with --seed: splitmix64. The state advances by [golden] in
   interp.ml; [mix64] turns it into the output (lc_mix64). *)
external golden_c : unit -> (int64 [@unboxed]) = "limen_core_golden_byte" "limen_core_golden" [@@noalloc]

let golden = golden_c ()

external mix64 : (int64 [@unboxed]) -> (int64 [@unboxed]) = "limen_core_mix64_byte" "limen_core_mix64" [@@noalloc]

(* 2^64 mod span, below which an unseeded Rand.int rejects a draw (lc_rand_skip). *)
external rand_skip : (int64 [@unboxed]) -> (int64 [@unboxed]) = "limen_core_rand_skip_byte" "limen_core_rand_skip" [@@noalloc]

(* A draw's top 53 bits as a Float in [0, 1) (lc_rand_unit). *)
external rand_unit : (int64 [@unboxed]) -> (float [@unboxed]) = "limen_core_rand_unit_byte" "limen_core_rand_unit" [@@noalloc]

external rand_le_c : bytes -> (int [@untagged]) -> (int [@untagged]) -> (int64 [@unboxed]) -> unit
  = "limen_core_rand_le_byte" "limen_core_rand_le"
[@@noalloc]

(* [rand_le b off k z]: z's low [k] <= 8 bytes, low first, into b at [off]
   (lc_rand_le); Invalid_argument when they don't fit. *)
let rand_le b off k z =
  if off < 0 || k < 0 || k > 8 || off + k > Bytes.length b then invalid_arg "Core.rand_le";
  rand_le_c b off k z

(* Panic messages for both backends; each adds the location itself. *)
external msg_list_index : int64 -> int -> string = "limen_core_msg_list_index"
external msg_bytes_index : int64 -> int -> string = "limen_core_msg_bytes_index"
external msg_byte_range : int64 -> string = "limen_core_msg_byte_range"

(* "key <k> is not in the map", [k] being the key as the backend prints it. *)
external msg_key : string -> string = "limen_core_msg_key"
