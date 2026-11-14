/* The interpreter's way into C: load a library, find a symbol, and call it,
   without libffi. The native backend calls each function through its exact
   prototype instead, and lets the C compiler do the ABI.

   Plain calls. Every call goes through one of a few function-pointer types
   that take 24 integer and 8 double arguments (tessera_gi and friends). On the
   64-bit ABIs Tessera supports (x86-64 System V, arm64 AAPCS64, and Apple's
   arm64 variant) integer and floating-point arguments are assigned to their
   own register sequences, and arguments that do not fit in registers go on
   the stack in order. So the integer arguments of the stub are the callee's
   integer registers (6 on x86-64, 5 when a hidden struct-result pointer
   takes the first, 8 on arm64) followed by an image of its stack arguments,
   8 bytes per stub argument, and its 8 doubles are the callee's floating
   point registers. The interpreter (Foreign in interp.ml) lays out each
   call that way for the ABI tessera_ffi_abi reports: which register or stack
   bytes each argument goes to, how a struct by value is split (x86-64:
   eightbytes classified INTEGER or SSE, memory beyond 16 bytes; arm64:
   homogeneous Float aggregates in FP registers, other structs of up to 16
   bytes in integer registers, larger ones copied to memory and passed by
   pointer; Apple packs 4-byte arguments on the stack at 4-byte alignment).
   A callee reads exactly the registers and stack bytes it declares; the
   rest of the stub's arguments are ignored. The result comes back through a
   stub whose result type the C compiler returns in the same registers:
   int64_t (rax / x0), double (xmm0 / d0), pairs of them, 3 or 4 doubles
   (an arm64 floating-point aggregate), or a 128-byte struct, which the C
   compiler returns through memory (the hidden pointer in rdi on x86-64, x8
   on arm64), as it does the callee's larger structs.

   Variadic calls (tessera_ffi_call_var). On x86-64 and on arm64 Linux a
   variadic argument travels like any other, and va_arg reads the integer
   and floating-point registers by class, so one stub declared
   `int64_t (int64_t, ...)` passes the integer-like arguments (fixed, then
   variadic) and then 8 doubles; the C compiler sets %al on x86-64. On
   Apple arm64 every variadic argument goes on the stack in an 8-byte slot,
   so the stub must have exactly as many fixed parameters as the callee:
   one per count, 1 to 6, all integer-like (the checker allows nothing
   else before `...`), then 8 slots holding the variadic arguments' bit
   patterns in order.

   Callbacks (trampolines). C calls a Tessera function through one of a fixed
   pool of trampolines that take 8 integer and 8 double arguments, the same
   way round: a callee's parameters arrive in those registers (and the stack
   slots after them, on x86-64). One pool returns int64_t, the other double.
   A trampoline calls the interpreter (caml_callback) with its slot number.
   A slot is taken for the duration of one foreign call. A panic in the
   callback cannot unwind through C's frames: the interpreter records it, and
   the trampoline jumps (longjmp) back to the foreign call that is running,
   skipping only C frames, which then ends the program. */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <pthread.h>
#include <setjmp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* tessera_ffi_open : string -> (nativeint, string) result as (handle, error) */
value tessera_ffi_open(value name) {
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

/* tessera_ffi_sym : nativeint -> string -> nativeint (0 when missing) */
value tessera_ffi_sym(value lib, value name) {
  CAMLparam2(lib, name);
  void *h = (void *)Nativeint_val(lib);
  void *p = dlsym(h ? h : RTLD_DEFAULT, String_val(name));
  CAMLreturn(caml_copy_nativeint((intnat)p));
}

/* tessera_ffi_abi : unit -> int (0 x86-64 System V, 1 arm64 AAPCS64, 2 Apple arm64) */
value tessera_ffi_abi(value unit) {
  (void)unit;
#if defined(__aarch64__) && defined(__APPLE__)
  return Val_int(2);
#elif defined(__aarch64__)
  return Val_int(1);
#elif defined(__x86_64__)
  return Val_int(0);
#else
#error "foreign calls in the interpreter support x86-64 and arm64"
#endif
}

#define NI 24 /* integer words: registers, then the stack image */
#define I24 int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, \
            int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t
#define D8 double, double, double, double, double, double, double, double
#define AI24 iv[0], iv[1], iv[2], iv[3], iv[4], iv[5], iv[6], iv[7], iv[8], iv[9], iv[10], iv[11], iv[12], iv[13], iv[14], iv[15], \
             iv[16], iv[17], iv[18], iv[19], iv[20], iv[21], iv[22], iv[23]
#define AD8 fv[0], fv[1], fv[2], fv[3], fv[4], fv[5], fv[6], fv[7]

typedef struct { int64_t a, b; } tessera_ii;
typedef struct { double a, b; } tessera_dd;
typedef struct { int64_t a; double b; } tessera_id;
typedef struct { double a; int64_t b; } tessera_di;
typedef struct { double a, b, c; } tessera_d3;
typedef struct { double a, b, c, d; } tessera_d4;
typedef struct { int64_t w[16]; } tessera_big;
typedef int64_t (*tessera_gi)(I24, D8);
typedef double (*tessera_gd)(I24, D8);
typedef tessera_ii (*tessera_gii)(I24, D8);
typedef tessera_dd (*tessera_gdd)(I24, D8);
typedef tessera_id (*tessera_gid)(I24, D8);
typedef tessera_di (*tessera_gdi)(I24, D8);
typedef tessera_d3 (*tessera_gd3)(I24, D8);
typedef tessera_d4 (*tessera_gd4)(I24, D8);
typedef tessera_big (*tessera_gbig)(I24, D8);

/* The innermost foreign call a failed callback jumps back to, and whether
   the interpreter has reported a failure (tessera_ffi_cb_fail). The
   interpreter runs on one thread. */
static jmp_buf *tessera_cb_jmp;
static int tessera_cb_failed;

/* An argument word. arg is the OCaml variant
     | AInt of int64      (tag 0)
     | AFloat of float    (tag 1: its bit pattern)
     | APtr of string     (tag 2: a pointer to the string's bytes, NUL-terminated by OCaml)
     | ABlob of string    (tag 3: a pointer to a copy of the bytes, freed after the call)
     | AOut               (constant 0: a pointer to a fresh zeroed 8-byte slot)
   With [copy] (a callback may run, and the OCaml heap may move while it
   does), APtr points to a copy too. Otherwise no OCaml allocation happens
   between reading a string's address and the call, so the GC cannot move it
   meanwhile. */
static int64_t tessera_word(value a, int copy, int64_t *slots, int *no, void **owned, int *nown) {
  if (Is_long(a)) return (int64_t)(intptr_t)&slots[(*no)++];
  switch (Tag_val(a)) {
  case 0: return Int64_val(Field(a, 0));
  case 1: { double d = Double_val(Field(a, 0)); int64_t x; memcpy(&x, &d, 8); return x; }
  case 2:
    if (!copy) return (int64_t)(intptr_t)String_val(Field(a, 0));
    /* fall through */
  default: {
    mlsize_t n = caml_string_length(Field(a, 0));
    char *p = malloc(n + 1);
    if (!p) { fputs("panic: out of memory\n", stderr); exit(1); }
    memcpy(p, String_val(Field(a, 0)), n + 1);
    owned[(*nown)++] = p;
    return (int64_t)(intptr_t)p;
  }
  }
}

static value tessera_result(int status, int64_t *w, int nw, int64_t *slots, int no) {
  CAMLparam0();
  CAMLlocal4(r, words, outs, x);
  words = caml_alloc(nw, 0);
  for (int k = 0; k < nw; k++) { x = caml_copy_int64(w[k]); Store_field(words, k, x); }
  outs = caml_alloc(no, 0);
  for (int k = 0; k < no; k++) { x = caml_copy_int64(slots[k]); Store_field(outs, k, x); }
  r = caml_alloc_tuple(3);
  Store_field(r, 0, Val_int(status));
  Store_field(r, 1, words);
  Store_field(r, 2, outs);
  CAMLreturn(r);
}

/* tessera_ffi_call : nativeint -> arg array -> float array -> int -> bool -> int * int64 array * int64 array

   (function, integer words, doubles, result kind, copy) -> (status, result
   words, out slots). Result kinds: 0 int64_t, 1 double, 2 two int64_t, 3
   two doubles, 4 int64_t then double, 5 double then int64_t, 6 three
   doubles, 7 four doubles, 8 a struct returned through memory (16 words).
   The words are the result's bytes in memory order. Status 1: a callback
   failed and the call was abandoned. */
value tessera_ffi_call(value fn, value ints, value floats, value kind, value copy) {
  CAMLparam5(fn, ints, floats, kind, copy);
  int64_t iv[NI] = { 0 };
  double fv[8] = { 0 };
  int64_t slots[NI] = { 0 };
  void *owned[NI];
  int no = 0, nown = 0;
  int cp = Bool_val(copy);
  mlsize_t n = Wosize_val(ints);
  for (mlsize_t k = 0; k < n && k < NI; k++) iv[k] = tessera_word(Field(ints, k), cp, slots, &no, owned, &nown);
  mlsize_t nf = Wosize_val(floats) / Double_wosize;
  for (mlsize_t k = 0; k < nf && k < 8; k++) fv[k] = Double_flat_field(floats, k);
  void *f = (void *)Nativeint_val(fn);
  int64_t w[16] = { 0 };
  int nw = 1;
  volatile int status = 0;
  jmp_buf jb;
  jmp_buf *prev = tessera_cb_jmp;
  if (setjmp(jb) == 0) {
    tessera_cb_jmp = &jb;
    switch (Int_val(kind)) {
    case 1: { double d = ((tessera_gd)f)(AI24, AD8); memcpy(w, &d, 8); break; }
    case 2: { tessera_ii x = ((tessera_gii)f)(AI24, AD8); memcpy(w, &x, 16); nw = 2; break; }
    case 3: { tessera_dd x = ((tessera_gdd)f)(AI24, AD8); memcpy(w, &x, 16); nw = 2; break; }
    case 4: { tessera_id x = ((tessera_gid)f)(AI24, AD8); memcpy(w, &x, 16); nw = 2; break; }
    case 5: { tessera_di x = ((tessera_gdi)f)(AI24, AD8); memcpy(w, &x, 16); nw = 2; break; }
    case 6: { tessera_d3 x = ((tessera_gd3)f)(AI24, AD8); memcpy(w, &x, 24); nw = 3; break; }
    case 7: { tessera_d4 x = ((tessera_gd4)f)(AI24, AD8); memcpy(w, &x, 32); nw = 4; break; }
    case 8: { tessera_big x = ((tessera_gbig)f)(AI24, AD8); memcpy(w, &x, sizeof x); nw = 16; break; }
    default: w[0] = ((tessera_gi)f)(AI24, AD8); break;
    }
  } else
    status = 1;
  tessera_cb_jmp = prev;
  for (int k = 0; k < nown; k++) free(owned[k]);
  CAMLreturn(tessera_result(status, w, status ? 0 : nw, slots, status ? 0 : no));
}

/* tessera_ffi_call_var : nativeint -> int -> arg array -> float array -> int -> int * int64 array * int64 array

   (function, fixed + 256 * copy, integer words, doubles, result kind 0 or
   1). Apple arm64: the words are the fixed arguments, then the variadic
   ones' bit patterns in order (doubles included). Elsewhere: the
   integer-like arguments, fixed then variadic, and the doubles apart. */
#define V8(k) iv[k], iv[k + 1], iv[k + 2], iv[k + 3], iv[k + 4], iv[k + 5], iv[k + 6], iv[k + 7]
#define VCALL(RT, out)                                                                                                         \
  switch (fixed) {                                                                                                             \
  case 1: out = ((RT (*)(int64_t, ...))f)(iv[0], V8(1)); break;                                                               \
  case 2: out = ((RT (*)(int64_t, int64_t, ...))f)(iv[0], iv[1], V8(2)); break;                                               \
  case 3: out = ((RT (*)(int64_t, int64_t, int64_t, ...))f)(iv[0], iv[1], iv[2], V8(3)); break;                               \
  case 4: out = ((RT (*)(int64_t, int64_t, int64_t, int64_t, ...))f)(iv[0], iv[1], iv[2], iv[3], V8(4)); break;               \
  case 5: out = ((RT (*)(int64_t, int64_t, int64_t, int64_t, int64_t, ...))f)(iv[0], iv[1], iv[2], iv[3], iv[4], V8(5)); break; \
  default:                                                                                                                     \
    out = ((RT (*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, ...))f)(iv[0], iv[1], iv[2], iv[3], iv[4], iv[5], V8(6)); \
    break;                                                                                                                     \
  }
value tessera_ffi_call_var(value fn, value flags, value ints, value floats, value kind) {
  CAMLparam5(fn, flags, ints, floats, kind);
  int64_t iv[NI] = { 0 };
  double fv[8] = { 0 };
  int64_t slots[NI] = { 0 };
  void *owned[NI];
  int no = 0, nown = 0;
  int fixed = Int_val(flags) & 255, cp = Int_val(flags) >> 8;
  mlsize_t n = Wosize_val(ints);
  for (mlsize_t k = 0; k < n && k < NI; k++) iv[k] = tessera_word(Field(ints, k), cp, slots, &no, owned, &nown);
  mlsize_t nf = Wosize_val(floats) / Double_wosize;
  for (mlsize_t k = 0; k < nf && k < 8; k++) fv[k] = Double_flat_field(floats, k);
  void *f = (void *)Nativeint_val(fn);
  int64_t w[1] = { 0 };
  volatile int status = 0;
  jmp_buf jb;
  jmp_buf *prev = tessera_cb_jmp;
  if (setjmp(jb) == 0) {
    tessera_cb_jmp = &jb;
#if defined(__aarch64__) && defined(__APPLE__)
    if (Int_val(kind) == 1) { double d = 0; VCALL(double, d); memcpy(w, &d, 8); }
    else { VCALL(int64_t, w[0]); }
#else
    (void)fixed;
    if (Int_val(kind) == 1) {
      double d = ((double (*)(int64_t, ...))f)(iv[0], iv[1], iv[2], iv[3], iv[4], iv[5], iv[6], iv[7], iv[8], iv[9], iv[10], iv[11],
                                               iv[12], iv[13], AD8);
      memcpy(w, &d, 8);
    } else
      w[0] = ((int64_t (*)(int64_t, ...))f)(iv[0], iv[1], iv[2], iv[3], iv[4], iv[5], iv[6], iv[7], iv[8], iv[9], iv[10], iv[11], iv[12],
                                            iv[13], AD8);
#endif
  } else
    status = 1;
  tessera_cb_jmp = prev;
  for (int k = 0; k < nown; k++) free(owned[k]);
  CAMLreturn(tessera_result(status, w, status ? 0 : 1, slots, status ? 0 : no));
}

/* ---- callbacks ---- */

#define NCB 32
static int tessera_cb_used[2][NCB];
static pthread_t tessera_cb_thread;

static int64_t tessera_cb_call(int pool, int slot, int64_t *a, double *d) {
  CAMLparam0();
  CAMLlocal4(ia, fa, x, r);
  static const value *cb = NULL;
  if (!cb) cb = caml_named_value("tessera_ffi_callback");
  ia = caml_alloc(8, 0);
  for (int k = 0; k < 8; k++) { x = caml_copy_int64(a[k]); Store_field(ia, k, x); }
  fa = caml_alloc(8 * Double_wosize, Double_array_tag);
  for (int k = 0; k < 8; k++) Store_double_flat_field(fa, k, d[k]);
  r = caml_callback3_exn(*cb, Val_int(pool * NCB + slot), ia, fa);
  if (Is_exception_result(r)) { tessera_cb_failed = 1; CAMLreturnT(int64_t, 0); }
  CAMLreturnT(int64_t, Int64_val(r));
}

static int64_t tessera_cb_run(int pool, int slot, int64_t *a, double *d) {
  if (!tessera_cb_used[pool][slot] || !pthread_equal(pthread_self(), tessera_cb_thread)) {
    fputs("panic: a callback from C ran after its foreign call returned, or on another thread\n", stderr);
    fflush(stderr);
    _exit(1);
  }
  int64_t r = tessera_cb_call(pool, slot, a, d);
  if (tessera_cb_failed) {
    tessera_cb_failed = 0;
    if (tessera_cb_jmp) longjmp(*tessera_cb_jmp, 1);
  }
  return r;
}

#define TRAMP(n)                                                                                                            \
  static int64_t tessera_cbi_##n(int64_t a0, int64_t a1, int64_t a2, int64_t a3, int64_t a4, int64_t a5, int64_t a6, int64_t a7, \
                              double d0, double d1, double d2, double d3, double d4, double d5, double d6, double d7) {     \
    int64_t a[8] = { a0, a1, a2, a3, a4, a5, a6, a7 };                                                                     \
    double d[8] = { d0, d1, d2, d3, d4, d5, d6, d7 };                                                                      \
    return tessera_cb_run(0, n, a, d);                                                                                        \
  }                                                                                                                         \
  static double tessera_cbd_##n(int64_t a0, int64_t a1, int64_t a2, int64_t a3, int64_t a4, int64_t a5, int64_t a6, int64_t a7, \
                             double d0, double d1, double d2, double d3, double d4, double d5, double d6, double d7) {      \
    int64_t a[8] = { a0, a1, a2, a3, a4, a5, a6, a7 };                                                                     \
    double d[8] = { d0, d1, d2, d3, d4, d5, d6, d7 };                                                                      \
    int64_t r = tessera_cb_run(1, n, a, d);                                                                                   \
    double x;                                                                                                               \
    memcpy(&x, &r, 8);                                                                                                      \
    return x;                                                                                                               \
  }
TRAMP(0) TRAMP(1) TRAMP(2) TRAMP(3) TRAMP(4) TRAMP(5) TRAMP(6) TRAMP(7) TRAMP(8) TRAMP(9) TRAMP(10) TRAMP(11) TRAMP(12) TRAMP(13)
TRAMP(14) TRAMP(15) TRAMP(16) TRAMP(17) TRAMP(18) TRAMP(19) TRAMP(20) TRAMP(21) TRAMP(22) TRAMP(23) TRAMP(24) TRAMP(25) TRAMP(26)
TRAMP(27) TRAMP(28) TRAMP(29) TRAMP(30) TRAMP(31)
#define TL(p)                                                                                                                       \
  { (void *)tessera_cb##p##_0, (void *)tessera_cb##p##_1, (void *)tessera_cb##p##_2, (void *)tessera_cb##p##_3, (void *)tessera_cb##p##_4,       \
    (void *)tessera_cb##p##_5, (void *)tessera_cb##p##_6, (void *)tessera_cb##p##_7, (void *)tessera_cb##p##_8, (void *)tessera_cb##p##_9,       \
    (void *)tessera_cb##p##_10, (void *)tessera_cb##p##_11, (void *)tessera_cb##p##_12, (void *)tessera_cb##p##_13, (void *)tessera_cb##p##_14,  \
    (void *)tessera_cb##p##_15, (void *)tessera_cb##p##_16, (void *)tessera_cb##p##_17, (void *)tessera_cb##p##_18, (void *)tessera_cb##p##_19,  \
    (void *)tessera_cb##p##_20, (void *)tessera_cb##p##_21, (void *)tessera_cb##p##_22, (void *)tessera_cb##p##_23, (void *)tessera_cb##p##_24,  \
    (void *)tessera_cb##p##_25, (void *)tessera_cb##p##_26, (void *)tessera_cb##p##_27, (void *)tessera_cb##p##_28, (void *)tessera_cb##p##_29,  \
    (void *)tessera_cb##p##_30, (void *)tessera_cb##p##_31 }
static void *const tessera_cb_addrs[2][NCB] = { TL(i), TL(d) };

/* tessera_ffi_cb_take : bool (double result) -> int: a free slot, now taken, or -1 */
value tessera_ffi_cb_take(value dbl) {
  int p = Bool_val(dbl);
  for (int k = 0; k < NCB; k++)
    if (!tessera_cb_used[p][k]) {
      tessera_cb_used[p][k] = 1;
      tessera_cb_thread = pthread_self();
      return Val_int(k);
    }
  return Val_int(-1);
}

/* tessera_ffi_cb_free : bool -> int -> unit */
value tessera_ffi_cb_free(value dbl, value k) {
  tessera_cb_used[Bool_val(dbl)][Int_val(k)] = 0;
  return Val_unit;
}

/* tessera_ffi_cb_addr : bool -> int -> nativeint (the slot's trampoline) */
value tessera_ffi_cb_addr(value dbl, value k) {
  CAMLparam2(dbl, k);
  CAMLreturn(caml_copy_nativeint((intnat)tessera_cb_addrs[Bool_val(dbl)][Int_val(k)]));
}

/* tessera_ffi_cb_fail : unit -> unit: the callback running now failed; its
   trampoline abandons the foreign call */
value tessera_ffi_cb_fail(value unit) {
  (void)unit;
  tessera_cb_failed = 1;
  return Val_unit;
}

/* tessera_ffi_cstr : int64 -> string (the NUL-terminated string at the address; "" for NULL) */
value tessera_ffi_cstr(value p) {
  CAMLparam1(p);
  const char *s = (const char *)(intptr_t)Int64_val(p);
  CAMLreturn(caml_copy_string(s ? s : ""));
}

/* tessera_ffi_peek : int64 -> int -> string (n bytes at the address) */
value tessera_ffi_peek(value p, value n) {
  CAMLparam2(p, n);
  CAMLlocal1(r);
  const char *s = (const char *)(intptr_t)Int64_val(p);
  intnat len = Long_val(n);
  r = caml_alloc_string((mlsize_t)len);
  if (len > 0) memcpy((char *)Bytes_val(r), s, (size_t)len);
  CAMLreturn(r);
}

/* The interpreter's way into the operating-system sandbox (tessera_sandbox.h),
   entered by `tessera run` once the program is loaded and its granted foreign
   libraries are resolved, before main starts.
   tessera_sandbox_enter : string -> bool -> bool -> bool -> int
   (fs root or "", read-only, net, exec) -> 0 enforced, 1 unsupported here;
   raises Failure with the reason if it could not be entered. */
#include <caml/fail.h>
#include "runtime/tessera_sandbox.h"
value tessera_sandbox_enter(value root, value ro, value net, value exec) {
  CAMLparam4(root, ro, net, exec);
  WSandboxSpec s = { caml_string_length(root) ? String_val(root) : NULL, Bool_val(ro), Bool_val(net), Bool_val(exec) };
  char err[512];
  int rc = w_sandbox_enter(&s, err, sizeof err);
  if (rc < 0) caml_failwith(err);
  CAMLreturn(Val_int(rc));
}
