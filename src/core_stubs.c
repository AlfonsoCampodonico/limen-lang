/* The interpreter's way to the built-ins written once in C
   (runtime/limen_core.h), declared to OCaml in core.ml.

   GC safety: an allocation can run the minor GC, which moves small strings,
   so no stub keeps a char * into an OCaml string across one. Stubs keep
   offsets and read String_val again after every caml_alloc*. A list of
   strings comes back as a string array, built in two passes: count, then
   allocate and fill. */
#define _GNU_SOURCE
#include "runtime/limen_core.h"
#include <string.h>
#include <caml/alloc.h>
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
