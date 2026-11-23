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
