/* The interpreter's way to the built-ins written once in C
   (runtime/limen_core.h and limen_core_inline.h), declared to OCaml in core.ml.

   GC safety: an allocation can run the minor GC, which moves small strings,
   so no stub keeps a char * into an OCaml string across one. Stubs keep
   offsets and read String_val again after every caml_alloc*. A list of
   strings comes back as a string array, built in two passes: count, then
   allocate and fill. */
#define _GNU_SOURCE
#include "runtime/limen_core_inline.h"
#include "runtime/limen_core.h"
#include <string.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value limen_core_lines(value s) {
  CAMLparam1(s);
  CAMLlocal2(out, piece);
  size_t n = caml_string_length(s);
  size_t pos = 0, start, len, count = 0;
  while (lc_lines_next(String_val(s), n, &pos, &start, &len)) count++;
  out = caml_alloc(count, 0);
  pos = 0;
  for (size_t i = 0; i < count; i++) {
    lc_lines_next(String_val(s), n, &pos, &start, &len);
    /* not caml_alloc_initialized_string: its source pointer would be read
       before the allocation that may move s */
    piece = caml_alloc_string(len);
    memcpy(Bytes_val(piece), String_val(s) + start, len);
    Store_field(out, i, piece);
  }
  CAMLreturn(out);
}

/* Copies s[start..start+len) into a new OCaml string. s is a root the
   caller registered; it is read again after the allocation, which may move it. */
static value core_sub(value s, size_t start, size_t len) {
  CAMLparam1(s);
  CAMLlocal1(piece);
  piece = caml_alloc_string(len);
  memcpy(Bytes_val(piece), String_val(s) + start, len);
  CAMLreturn(piece);
}

value limen_core_split(value s, value sep) {
  CAMLparam2(s, sep);
  CAMLlocal2(out, piece);
  size_t n = caml_string_length(s), m = caml_string_length(sep);
  size_t pos = 0, start, len, count = 0;
  while (lc_split_next(String_val(s), n, String_val(sep), m, &pos, &start, &len)) count++;
  out = caml_alloc(count, 0);
  pos = 0;
  for (size_t i = 0; i < count; i++) {
    /* String_val read afresh each time: the last piece may have moved them */
    lc_split_next(String_val(s), n, String_val(sep), m, &pos, &start, &len);
    piece = core_sub(s, start, len);
    Store_field(out, i, piece);
  }
  CAMLreturn(out);
}

value limen_core_trim(value s) {
  CAMLparam1(s);
  size_t a, b;
  lc_trim(String_val(s), caml_string_length(s), &a, &b);
  CAMLreturn(core_sub(s, a, b - a));
}

value limen_core_chars(value s) {
  CAMLparam1(s);
  CAMLlocal2(out, piece);
  size_t n = caml_string_length(s), count = 0;
  for (size_t i = 0; i < n; count++) i += lc_utf8_len((const unsigned char *)String_val(s), i, n);
  out = caml_alloc(count, 0);
  size_t i = 0;
  for (size_t c = 0; c < count; c++) {
    size_t k = lc_utf8_len((const unsigned char *)String_val(s), i, n);
    piece = core_sub(s, i, k);
    Store_field(out, c, piece);
    i += k;
  }
  CAMLreturn(out);
}

value limen_core_of_code(value cp) {
  CAMLparam1(cp);
  char b[4];
  size_t k = lc_utf8_encode(Int64_val(cp), b);
  /* b is on the C stack, so the GC cannot move it */
  CAMLreturn(caml_alloc_initialized_string(k, b));
}

value limen_core_parse_int(value s) {
  CAMLparam1(s);
  CAMLlocal2(out, boxed);
  int64_t v;
  if (!lc_parse_int(String_val(s), caml_string_length(s), &v)) CAMLreturn(Val_none);
  boxed = caml_copy_int64(v);
  out = caml_alloc_some(boxed);
  CAMLreturn(out);
}

/* Numeric stubs: no allocation, unboxed in native code, boxed _byte variants
   for bytecode. The int64 of u2f is the UInt's 64 bits. */
double limen_core_u2f(int64_t x) { return lc_u2f((uint64_t)x); }
value limen_core_u2f_byte(value x) { return caml_copy_double(lc_u2f((uint64_t)Int64_val(x))); }
int64_t limen_core_f2i(double x) { return lc_f2i(x); }
value limen_core_f2i_byte(value x) { return caml_copy_int64(lc_f2i(Double_val(x))); }

/* Rand: splitmix64's output, the skip below which a draw is rejected, a
   draw as a Float, and a seeded draw's low bytes written into a buffer
   (the caller checks off + k fits). The state stays in interp.ml. */
int64_t limen_core_mix64(int64_t z) { return (int64_t)lc_mix64((uint64_t)z); }
value limen_core_mix64_byte(value z) { return caml_copy_int64((int64_t)lc_mix64((uint64_t)Int64_val(z))); }
int64_t limen_core_rand_skip(int64_t span) { return (int64_t)lc_rand_skip((uint64_t)span); }
value limen_core_rand_skip_byte(value span) { return caml_copy_int64((int64_t)lc_rand_skip((uint64_t)Int64_val(span))); }
double limen_core_rand_unit(int64_t z) { return lc_rand_unit((uint64_t)z); }
value limen_core_rand_unit_byte(value z) { return caml_copy_double(lc_rand_unit((uint64_t)Int64_val(z))); }
value limen_core_rand_le(value b, intnat off, intnat k, int64_t z) {
  lc_rand_le((uint64_t)z, Bytes_val(b) + off, (size_t)k);
  return Val_unit;
}
value limen_core_rand_le_byte(value b, value off, value k, value z) {
  return limen_core_rand_le(b, Long_val(off), Long_val(k), Int64_val(z));
}

value limen_core_fmt_float(value x) {
  CAMLparam1(x);
  char t[LC_FLOAT_MAX];
  size_t k = lc_fmt_float(t, sizeof t, Double_val(x));
  /* t is on the C stack, so the GC cannot move it */
  CAMLreturn(caml_alloc_initialized_string(k, t));
}

value limen_core_to_fixed(value x, value d) {
  CAMLparam2(x, d);
  char t[LC_FIXED_MAX];
  size_t k = lc_to_fixed(t, sizeof t, Double_val(x), Int64_val(d));
  CAMLreturn(caml_alloc_initialized_string(k, t));
}

value limen_core_parse_float(value s) {
  CAMLparam1(s);
  CAMLlocal2(out, boxed);
  double x;
  lc_float_parse r = lc_parse_float(String_val(s), caml_string_length(s), &x);
  /* nothing allocated on the OCaml heap yet, so raising here is safe */
  if (r == LC_FLOAT_OOM) caml_raise_out_of_memory();
  if (r == LC_FLOAT_BAD) CAMLreturn(Val_none);
  boxed = caml_copy_double(x);
  out = caml_alloc_some(boxed);
  CAMLreturn(out);
}

/* Order stubs: no allocation, so no roots; called noalloc. */
intnat limen_core_float_cmp(double a, double b) { return lc_float_cmp(a, b); }
value limen_core_float_cmp_byte(value a, value b) { return Val_int(lc_float_cmp(Double_val(a), Double_val(b))); }
value limen_core_str_cmp(value a, value b) {
  return Val_int(lc_bytes_cmp(String_val(a), caml_string_length(a), String_val(b), caml_string_length(b)));
}

value limen_core_escaped(value s) {
  CAMLparam1(s);
  CAMLlocal1(out);
  size_t n = caml_string_length(s);
  size_t k = lc_escaped_len(String_val(s), n);
  out = caml_alloc_string(k);
  /* String_val read after the allocation, which may have moved s */
  lc_escape(String_val(s), n, (char *)Bytes_val(out));
  CAMLreturn(out);
}

/* Panic messages. The numeric ones fit a stack buffer; msg_key measures with
   a NULL buffer, allocates, then writes into the result (the NUL lands in the
   string's own padding byte). */
value limen_core_msg_list_index(value i, value len) {
  CAMLparam2(i, len);
  char t[LC_MSG_MAX];
  size_t k = lc_msg_list_index(t, sizeof t, Int64_val(i), Long_val(len));
  CAMLreturn(caml_alloc_initialized_string(k, t));
}
value limen_core_msg_bytes_index(value i, value len) {
  CAMLparam2(i, len);
  char t[LC_MSG_MAX];
  size_t k = lc_msg_bytes_index(t, sizeof t, Int64_val(i), Long_val(len));
  CAMLreturn(caml_alloc_initialized_string(k, t));
}
value limen_core_msg_byte_range(value v) {
  CAMLparam1(v);
  char t[LC_MSG_MAX];
  size_t k = lc_msg_byte_range(t, sizeof t, Int64_val(v));
  CAMLreturn(caml_alloc_initialized_string(k, t));
}
value limen_core_msg_key(value key) {
  CAMLparam1(key);
  CAMLlocal1(out);
  size_t kn = caml_string_length(key);
  size_t n = lc_msg_key(NULL, 0, String_val(key), kn);
  out = caml_alloc_string(n);
  /* String_val read after the allocation, which may have moved key */
  lc_msg_key((char *)Bytes_val(out), n + 1, String_val(key), kn);
  CAMLreturn(out);
}
