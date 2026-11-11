/* The interpreter's way into C: load a library, find a symbol, and call it.

   Calls go through one generic function-pointer type that takes 8 integer
   and 8 double arguments. On the 64-bit ABIs Vela supports (x86-64 System V
   and arm64, Apple's included) integer and floating-point arguments are
   assigned to their own register sequences, and up to 8 doubles all travel
   in registers, so a callee with at most 8 integer-like and 8 double
   parameters reads exactly the arguments it declares, in order; the extra
   ones are ignored. This does not hold for variadic functions, which the
   checker never lets through. The native backend calls each function
   through its exact prototype instead. */
#include <dlfcn.h>
#include <stdint.h>
#include <string.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* vela_ffi_open : string -> (nativeint, string) result as (handle, error) */
value vela_ffi_open(value name) {
  CAMLparam1(name);
  CAMLlocal1(r);
  const char *n = String_val(name);
  void *h = strcmp(n, "") == 0 ? dlopen(NULL, RTLD_NOW) : dlopen(n, RTLD_NOW | RTLD_LOCAL);
  const char *err = h ? "" : dlerror();
  r = caml_alloc_tuple(2);
  Store_field(r, 0, caml_copy_nativeint((intnat)h));
  Store_field(r, 1, caml_copy_string(err ? err : "unknown error"));
  CAMLreturn(r);
}

/* vela_ffi_sym : nativeint -> string -> nativeint (0 when missing) */
value vela_ffi_sym(value lib, value name) {
  CAMLparam2(lib, name);
  void *h = (void *)Nativeint_val(lib);
  void *p = dlsym(h ? h : RTLD_DEFAULT, String_val(name));
  CAMLreturn(caml_copy_nativeint((intnat)p));
}

typedef int64_t (*vela_fi)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t,
                           double, double, double, double, double, double, double, double);
typedef double (*vela_ff)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t,
                          double, double, double, double, double, double, double, double);

/* vela_ffi_call : nativeint -> arg array -> int (result kind) -> int64 * float * int64 array

   arg is the OCaml variant
     | AInt of int64      (tag 0)
     | AFloat of float    (tag 1)
     | APtr of string     (tag 2: a pointer to the string's bytes, NUL-terminated by OCaml)
     | AOut               (constant 0: a pointer to a fresh zeroed 8-byte slot)
   Result kind: 0 integer-like (the int64 register), 1 double.
   The out slots come back in order, as raw 8-byte values. No OCaml allocation
   happens between reading a string's address and the call, so the GC cannot
   move it meanwhile. */
value vela_ffi_call(value fn, value args, value kind) {
  CAMLparam3(fn, args, kind);
  CAMLlocal2(r, outs);
  int64_t iv[8] = { 0 };
  double fv[8] = { 0 };
  int64_t slots[8] = { 0 };
  int ni = 0, nf = 0, no = 0;
  mlsize_t n = Wosize_val(args);
  for (mlsize_t k = 0; k < n; k++) {
    value a = Field(args, k);
    if (Is_long(a)) {
      if (ni < 8) iv[ni++] = (int64_t)(intptr_t)&slots[no++];
    } else
      switch (Tag_val(a)) {
      case 0: if (ni < 8) iv[ni++] = Int64_val(Field(a, 0)); break;
      case 1: if (nf < 8) fv[nf++] = Double_val(Field(a, 0)); break;
      case 2: if (ni < 8) iv[ni++] = (int64_t)(intptr_t)String_val(Field(a, 0)); break;
      }
  }
  void *f = (void *)Nativeint_val(fn);
  int64_t ri = 0;
  double rf = 0;
  if (Int_val(kind) == 1)
    rf = ((vela_ff)f)(iv[0], iv[1], iv[2], iv[3], iv[4], iv[5], iv[6], iv[7], fv[0], fv[1], fv[2], fv[3], fv[4], fv[5], fv[6], fv[7]);
  else
    ri = ((vela_fi)f)(iv[0], iv[1], iv[2], iv[3], iv[4], iv[5], iv[6], iv[7], fv[0], fv[1], fv[2], fv[3], fv[4], fv[5], fv[6], fv[7]);
  outs = caml_alloc(no, 0);
  for (int k = 0; k < no; k++) Store_field(outs, k, caml_copy_int64(slots[k]));
  r = caml_alloc_tuple(3);
  Store_field(r, 0, caml_copy_int64(ri));
  Store_field(r, 1, caml_copy_double(rf));
  Store_field(r, 2, outs);
  CAMLreturn(r);
}

/* vela_ffi_cstr : int64 -> string (the NUL-terminated string at the address; "" for NULL) */
value vela_ffi_cstr(value p) {
  CAMLparam1(p);
  const char *s = (const char *)(intptr_t)Int64_val(p);
  CAMLreturn(caml_copy_string(s ? s : ""));
}

/* vela_ffi_peek : int64 -> int -> string (n bytes at the address) */
value vela_ffi_peek(value p, value n) {
  CAMLparam2(p, n);
  CAMLlocal1(r);
  const char *s = (const char *)(intptr_t)Int64_val(p);
  intnat len = Long_val(n);
  r = caml_alloc_string((mlsize_t)len);
  if (len > 0) memcpy((char *)Bytes_val(r), s, (size_t)len);
  CAMLreturn(r);
}
