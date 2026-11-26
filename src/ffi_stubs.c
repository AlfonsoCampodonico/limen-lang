/* The interpreter's way into C: load a library, find a symbol, and call it,
   without libffi. The native backend calls each function through its exact
   prototype instead, and lets the C compiler do the ABI.

   Plain calls. Every call goes through one of a few function-pointer types
   that take 24 integer and 8 double arguments (limen_gi and friends). On the
   64-bit ABIs Limen supports (x86-64 System V, arm64 AAPCS64, and Apple's
   arm64 variant) integer and floating-point arguments are assigned to their
   own register sequences, and arguments that do not fit in registers go on
   the stack in order. So the integer arguments of the stub are the callee's
   integer registers (6 on x86-64, 5 when a hidden struct-result pointer
   takes the first, 8 on arm64) followed by an image of its stack arguments,
   8 bytes per stub argument, and its 8 doubles are the callee's floating
   point registers. The interpreter (Foreign in interp.ml) lays out each
   call that way for the ABI limen_ffi_abi reports: which register or stack
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

   Variadic calls (limen_ffi_call_var). On x86-64 and on arm64 Linux a
   variadic argument travels like any other, and va_arg reads the integer
   and floating-point registers by class, so one stub declared
   `int64_t (int64_t, ...)` passes the integer-like arguments (fixed, then
   variadic) and then 8 doubles; the C compiler sets %al on x86-64. On
   Apple arm64 every variadic argument goes on the stack in an 8-byte slot,
   so the stub must have exactly as many fixed parameters as the callee:
   one per count, 1 to 6, all integer-like (the checker allows nothing
   else before `...`), then 8 slots holding the variadic arguments' bit
   patterns in order.

   Callbacks (trampolines). C calls a Limen function through one of a fixed
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
#include <errno.h>
#include <fcntl.h>
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

/* limen_ffi_open : string -> (nativeint, string) result as (handle, error) */
value limen_ffi_open(value name) {
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

/* limen_ffi_sym : nativeint -> string -> nativeint (0 when missing) */
value limen_ffi_sym(value lib, value name) {
  CAMLparam2(lib, name);
  void *h = (void *)Nativeint_val(lib);
  void *p = dlsym(h ? h : RTLD_DEFAULT, String_val(name));
  CAMLreturn(caml_copy_nativeint((intnat)p));
}

/* limen_ffi_abi : unit -> int (0 x86-64 System V, 1 arm64 AAPCS64, 2 Apple arm64) */
value limen_ffi_abi(value unit) {
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

typedef struct { int64_t a, b; } limen_ii;
typedef struct { double a, b; } limen_dd;
typedef struct { int64_t a; double b; } limen_id;
typedef struct { double a; int64_t b; } limen_di;
typedef struct { double a, b, c; } limen_d3;
typedef struct { double a, b, c, d; } limen_d4;
typedef struct { int64_t w[16]; } limen_big;
typedef int64_t (*limen_gi)(I24, D8);
typedef double (*limen_gd)(I24, D8);
typedef limen_ii (*limen_gii)(I24, D8);
typedef limen_dd (*limen_gdd)(I24, D8);
typedef limen_id (*limen_gid)(I24, D8);
typedef limen_di (*limen_gdi)(I24, D8);
typedef limen_d3 (*limen_gd3)(I24, D8);
typedef limen_d4 (*limen_gd4)(I24, D8);
typedef limen_big (*limen_gbig)(I24, D8);

/* The innermost foreign call a failed callback jumps back to, and whether
   the interpreter has reported a failure (limen_ffi_cb_fail). The
   interpreter runs on one thread. */
static jmp_buf *limen_cb_jmp;
static int limen_cb_failed;

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
static int64_t limen_word(value a, int copy, int64_t *slots, int *no, void **owned, int *nown) {
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

static value limen_result(int status, int64_t *w, int nw, int64_t *slots, int no) {
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

/* limen_ffi_call : nativeint -> arg array -> float array -> int -> bool -> int * int64 array * int64 array

   (function, integer words, doubles, result kind, copy) -> (status, result
   words, out slots). Result kinds: 0 int64_t, 1 double, 2 two int64_t, 3
   two doubles, 4 int64_t then double, 5 double then int64_t, 6 three
   doubles, 7 four doubles, 8 a struct returned through memory (16 words).
   The words are the result's bytes in memory order. Status 1: a callback
   failed and the call was abandoned. */
value limen_ffi_call(value fn, value ints, value floats, value kind, value copy) {
  CAMLparam5(fn, ints, floats, kind, copy);
  int64_t iv[NI] = { 0 };
  double fv[8] = { 0 };
  int64_t slots[NI] = { 0 };
  void *owned[NI];
  int no = 0, nown = 0;
  int cp = Bool_val(copy);
  mlsize_t n = Wosize_val(ints);
  for (mlsize_t k = 0; k < n && k < NI; k++) iv[k] = limen_word(Field(ints, k), cp, slots, &no, owned, &nown);
  mlsize_t nf = Wosize_val(floats) / Double_wosize;
  for (mlsize_t k = 0; k < nf && k < 8; k++) fv[k] = Double_flat_field(floats, k);
  void *f = (void *)Nativeint_val(fn);
  int64_t w[16] = { 0 };
  int nw = 1;
  volatile int status = 0;
  jmp_buf jb;
  jmp_buf *prev = limen_cb_jmp;
  if (setjmp(jb) == 0) {
    limen_cb_jmp = &jb;
    switch (Int_val(kind)) {
    case 1: { double d = ((limen_gd)f)(AI24, AD8); memcpy(w, &d, 8); break; }
    case 2: { limen_ii x = ((limen_gii)f)(AI24, AD8); memcpy(w, &x, 16); nw = 2; break; }
    case 3: { limen_dd x = ((limen_gdd)f)(AI24, AD8); memcpy(w, &x, 16); nw = 2; break; }
    case 4: { limen_id x = ((limen_gid)f)(AI24, AD8); memcpy(w, &x, 16); nw = 2; break; }
    case 5: { limen_di x = ((limen_gdi)f)(AI24, AD8); memcpy(w, &x, 16); nw = 2; break; }
    case 6: { limen_d3 x = ((limen_gd3)f)(AI24, AD8); memcpy(w, &x, 24); nw = 3; break; }
    case 7: { limen_d4 x = ((limen_gd4)f)(AI24, AD8); memcpy(w, &x, 32); nw = 4; break; }
    case 8: { limen_big x = ((limen_gbig)f)(AI24, AD8); memcpy(w, &x, sizeof x); nw = 16; break; }
    default: w[0] = ((limen_gi)f)(AI24, AD8); break;
    }
  } else
    status = 1;
  limen_cb_jmp = prev;
  for (int k = 0; k < nown; k++) free(owned[k]);
  CAMLreturn(limen_result(status, w, status ? 0 : nw, slots, status ? 0 : no));
}

/* limen_ffi_call_var : nativeint -> int -> arg array -> float array -> int -> int * int64 array * int64 array

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
value limen_ffi_call_var(value fn, value flags, value ints, value floats, value kind) {
  CAMLparam5(fn, flags, ints, floats, kind);
  int64_t iv[NI] = { 0 };
  double fv[8] = { 0 };
  int64_t slots[NI] = { 0 };
  void *owned[NI];
  int no = 0, nown = 0;
  int fixed = Int_val(flags) & 255, cp = Int_val(flags) >> 8;
  mlsize_t n = Wosize_val(ints);
  for (mlsize_t k = 0; k < n && k < NI; k++) iv[k] = limen_word(Field(ints, k), cp, slots, &no, owned, &nown);
  mlsize_t nf = Wosize_val(floats) / Double_wosize;
  for (mlsize_t k = 0; k < nf && k < 8; k++) fv[k] = Double_flat_field(floats, k);
  void *f = (void *)Nativeint_val(fn);
  int64_t w[1] = { 0 };
  volatile int status = 0;
  jmp_buf jb;
  jmp_buf *prev = limen_cb_jmp;
  if (setjmp(jb) == 0) {
    limen_cb_jmp = &jb;
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
  limen_cb_jmp = prev;
  for (int k = 0; k < nown; k++) free(owned[k]);
  CAMLreturn(limen_result(status, w, status ? 0 : 1, slots, status ? 0 : no));
}

/* ---- the blocking-call pool ----
   Blocking foreign calls and SQL statements run on a pool of threads, as
   natively (limen_rt.c, w_bl_submit): the first job starts one thread per
   core (at least 4), and a job that finds no thread idle starts another, up
   to LIMEN_BLOCKING_THREADS (64 by default); past that, jobs wait in the
   queue. A job starts with a limen_pjob header; its run function owns it
   from then on (frees it, or hands it back through its pipe). Jobs are
   submitted by the thread holding OCaml's runtime lock; pool threads never
   touch an OCaml value. The threads stay for the life of the process. */
typedef struct limen_pjob {
  struct limen_pjob *next;
  void (*run)(struct limen_pjob *);
} limen_pjob;

static pthread_mutex_t limen_pool_mu = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t limen_pool_cv = PTHREAD_COND_INITIALIZER;
static limen_pjob *limen_pool_head, *limen_pool_tail;
static int limen_pool_threads, limen_pool_idle, limen_pool_queued, limen_pool_max;

static void *limen_pool_thread(void *arg) {
  (void)arg;
  pthread_mutex_lock(&limen_pool_mu);
  for (;;) {
    while (!limen_pool_head) {
      limen_pool_idle++;
      pthread_cond_wait(&limen_pool_cv, &limen_pool_mu);
      limen_pool_idle--;
    }
    limen_pjob *b = limen_pool_head;
    limen_pool_head = b->next;
    if (!limen_pool_head) limen_pool_tail = NULL;
    limen_pool_queued--;
    pthread_mutex_unlock(&limen_pool_mu);
    b->run(b);
    pthread_mutex_lock(&limen_pool_mu);
  }
  return NULL;
}

/* Start a pool thread (with limen_pool_mu held); 0 if none could start. */
static int limen_pool_grow(void) {
  pthread_t th;
  pthread_attr_t at;
  pthread_attr_init(&at);
  pthread_attr_setdetachstate(&at, PTHREAD_CREATE_DETACHED);
  pthread_attr_setstacksize(&at, (size_t)4 << 20); /* C libraries expect a thread's usual stack */
  int ok = pthread_create(&th, &at, limen_pool_thread, NULL) == 0;
  pthread_attr_destroy(&at);
  if (ok) limen_pool_threads++;
  return ok;
}

/* Queue a job; 0 (and the job not queued) if the pool has no thread and
   none could start. */
static int limen_pool_submit(limen_pjob *b) {
  pthread_mutex_lock(&limen_pool_mu);
  if (!limen_pool_max) {
    const char *e = getenv("LIMEN_BLOCKING_THREADS");
    limen_pool_max = e && atoi(e) > 0 ? atoi(e) : 64;
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    if (n < 4) n = 4;
    if (n > limen_pool_max) n = limen_pool_max;
    while (limen_pool_threads < n && limen_pool_grow()) {}
  }
  /* a thread signalled but not yet awake still counts as idle, so the
     queue's length, not the signal, says whether another thread is needed */
  if (limen_pool_queued + 1 > limen_pool_idle && limen_pool_threads < limen_pool_max) limen_pool_grow();
  if (limen_pool_threads == 0) { pthread_mutex_unlock(&limen_pool_mu); return 0; }
  b->next = NULL;
  if (limen_pool_tail) limen_pool_tail->next = b; else limen_pool_head = b;
  limen_pool_tail = b;
  limen_pool_queued++;
  if (limen_pool_idle > 0) pthread_cond_signal(&limen_pool_cv);
  pthread_mutex_unlock(&limen_pool_mu);
  return 1;
}

/* ---- blocking calls ----
   A function declared `blocking fn` runs on the blocking-call pool, as
   natively, so the interpreter's other tasks go on while C works.
   limen_ffi_job_start copies the arguments (strings included: the OCaml
   heap may move meanwhile), queues the job and returns it with the read end
   of a pipe the pool thread writes to when the call has returned; the task
   waits on that descriptor as on a socket. A cancelled task abandons the
   job (limen_ffi_job_abandon): the pool thread frees it when C returns, or
   at once, without making the call, if it was still in the queue.
   Blocking functions take no callbacks. */
typedef struct {
  limen_pjob hd;
  void *f; int fixed; int var; int kind;
  int64_t iv[NI]; double fv[8]; int64_t slots[NI]; int no;
  void *owned[NI]; int nown;
  int64_t w[16]; int nw;
  int fds[2];
  int state; /* 0 running, 1 done, 2 abandoned (under mu) */
  pthread_mutex_t mu;
} limen_job;

static void limen_job_free(limen_job *j) {
  for (int k = 0; k < j->nown; k++) free(j->owned[k]);
  close(j->fds[0]);
  close(j->fds[1]);
  pthread_mutex_destroy(&j->mu);
  free(j);
}

static void limen_job_run(limen_pjob *p) {
  limen_job *j = (limen_job *)p;
  pthread_mutex_lock(&j->mu);
  int abandoned = j->state == 2;
  pthread_mutex_unlock(&j->mu);
  if (abandoned) { limen_job_free(j); return; }
  int64_t *iv = j->iv;
  double *fv = j->fv;
  void *f = j->f;
  int64_t *w = j->w;
  j->nw = 1;
  if (j->var) {
    int fixed = j->fixed;
#if defined(__aarch64__) && defined(__APPLE__)
    if (j->kind == 1) { double d = 0; VCALL(double, d); memcpy(w, &d, 8); }
    else { VCALL(int64_t, w[0]); }
#else
    (void)fixed;
    if (j->kind == 1) {
      double d = ((double (*)(int64_t, ...))f)(iv[0], iv[1], iv[2], iv[3], iv[4], iv[5], iv[6], iv[7], iv[8], iv[9], iv[10], iv[11],
                                               iv[12], iv[13], AD8);
      memcpy(w, &d, 8);
    } else
      w[0] = ((int64_t (*)(int64_t, ...))f)(iv[0], iv[1], iv[2], iv[3], iv[4], iv[5], iv[6], iv[7], iv[8], iv[9], iv[10], iv[11], iv[12],
                                            iv[13], AD8);
#endif
  } else {
    switch (j->kind) {
    case 1: { double d = ((limen_gd)f)(AI24, AD8); memcpy(w, &d, 8); break; }
    case 2: { limen_ii x = ((limen_gii)f)(AI24, AD8); memcpy(w, &x, 16); j->nw = 2; break; }
    case 3: { limen_dd x = ((limen_gdd)f)(AI24, AD8); memcpy(w, &x, 16); j->nw = 2; break; }
    case 4: { limen_id x = ((limen_gid)f)(AI24, AD8); memcpy(w, &x, 16); j->nw = 2; break; }
    case 5: { limen_di x = ((limen_gdi)f)(AI24, AD8); memcpy(w, &x, 16); j->nw = 2; break; }
    case 6: { limen_d3 x = ((limen_gd3)f)(AI24, AD8); memcpy(w, &x, 24); j->nw = 3; break; }
    case 7: { limen_d4 x = ((limen_gd4)f)(AI24, AD8); memcpy(w, &x, 32); j->nw = 4; break; }
    case 8: { limen_big x = ((limen_gbig)f)(AI24, AD8); memcpy(w, &x, sizeof x); j->nw = 16; break; }
    default: w[0] = ((limen_gi)f)(AI24, AD8); break;
    }
  }
  pthread_mutex_lock(&j->mu);
  if (j->state == 2) {
    pthread_mutex_unlock(&j->mu);
    limen_job_free(j);
    return;
  }
  /* written before done shows and under the lock: once a cancelled task
     sees done it frees the job (closing the pipe), so the thread must not
     touch it after letting go (the pipe is empty, so this does not block) */
  ssize_t k;
  do k = write(j->fds[1], "x", 1); while (k < 0 && errno == EINTR);
  j->state = 1;
  pthread_mutex_unlock(&j->mu);
}

/* limen_ffi_job_start : nativeint -> int -> arg array -> float array -> int -> nativeint * int
   (function, fixed + 256 * variadic, integer words, doubles, result kind)
   -> (job, descriptor to wait on); a job of 0 if the pool has no thread. */
value limen_ffi_job_start(value fn, value flags, value ints, value floats, value kind) {
  CAMLparam5(fn, flags, ints, floats, kind);
  CAMLlocal2(r, jv);
  limen_job *j = calloc(1, sizeof *j);
  if (!j) { fputs("panic: out of memory\n", stderr); exit(1); }
  j->f = (void *)Nativeint_val(fn);
  j->fixed = Int_val(flags) & 255;
  j->var = Int_val(flags) >> 8;
  j->kind = Int_val(kind);
  mlsize_t n = Wosize_val(ints);
  for (mlsize_t k = 0; k < n && k < NI; k++) j->iv[k] = limen_word(Field(ints, k), 1, j->slots, &j->no, j->owned, &j->nown);
  mlsize_t nf = Wosize_val(floats) / Double_wosize;
  for (mlsize_t k = 0; k < nf && k < 8; k++) j->fv[k] = Double_flat_field(floats, k);
  /* out-parameters point at the job's slots, which outlive the call */
  pthread_mutex_init(&j->mu, NULL);
  j->hd.run = limen_job_run;
  int ok = pipe(j->fds) == 0;
  if (ok) {
    ok = limen_pool_submit(&j->hd);
    if (!ok) { close(j->fds[0]); close(j->fds[1]); }
  }
  if (!ok) {
    for (int k = 0; k < j->nown; k++) free(j->owned[k]);
    pthread_mutex_destroy(&j->mu);
    free(j);
    j = NULL;
  }
  jv = caml_copy_nativeint((intnat)j);
  r = caml_alloc_tuple(2);
  Store_field(r, 0, jv);
  Store_field(r, 1, Val_int(j ? j->fds[0] : -1));
  CAMLreturn(r);
}

/* limen_ffi_job_done : nativeint -> bool */
value limen_ffi_job_done(value jv) {
  limen_job *j = (limen_job *)Nativeint_val(jv);
  pthread_mutex_lock(&j->mu);
  int d = j->state == 1;
  pthread_mutex_unlock(&j->mu);
  return Val_bool(d);
}

/* limen_ffi_job_finish : nativeint -> int * int64 array * int64 array
   The results of a job that is done, as limen_ffi_call gives them; frees it. */
value limen_ffi_job_finish(value jv) {
  CAMLparam1(jv);
  CAMLlocal1(r);
  limen_job *j = (limen_job *)Nativeint_val(jv);
  r = limen_result(0, j->w, j->nw, j->slots, j->no);
  limen_job_free(j);
  CAMLreturn(r);
}

/* limen_ffi_job_abandon : nativeint -> unit
   Its task was cancelled: the pool thread frees the job once C returns, or
   without making the call if it has not started it. */
value limen_ffi_job_abandon(value jv) {
  limen_job *j = (limen_job *)Nativeint_val(jv);
  pthread_mutex_lock(&j->mu);
  int done = j->state == 1;
  if (!done) j->state = 2;
  pthread_mutex_unlock(&j->mu);
  if (done) limen_job_free(j);
  return Val_unit;
}

/* ---- callbacks ---- */

#define NCB 32
static int limen_cb_used[2][NCB];
static pthread_t limen_cb_thread;

static int64_t limen_cb_call(int pool, int slot, int64_t *a, double *d) {
  CAMLparam0();
  CAMLlocal4(ia, fa, x, r);
  static const value *cb = NULL;
  if (!cb) cb = caml_named_value("limen_ffi_callback");
  ia = caml_alloc(8, 0);
  for (int k = 0; k < 8; k++) { x = caml_copy_int64(a[k]); Store_field(ia, k, x); }
  fa = caml_alloc(8 * Double_wosize, Double_array_tag);
  for (int k = 0; k < 8; k++) Store_double_flat_field(fa, k, d[k]);
  r = caml_callback3_exn(*cb, Val_int(pool * NCB + slot), ia, fa);
  if (Is_exception_result(r)) { limen_cb_failed = 1; CAMLreturnT(int64_t, 0); }
  CAMLreturnT(int64_t, Int64_val(r));
}

static int64_t limen_cb_run(int pool, int slot, int64_t *a, double *d) {
  if (!limen_cb_used[pool][slot] || !pthread_equal(pthread_self(), limen_cb_thread)) {
    fputs("panic: a callback from C ran after its foreign call returned, or on another thread\n", stderr);
    fflush(stderr);
    _exit(1);
  }
  int64_t r = limen_cb_call(pool, slot, a, d);
  if (limen_cb_failed) {
    limen_cb_failed = 0;
    if (limen_cb_jmp) longjmp(*limen_cb_jmp, 1);
  }
  return r;
}

#define TRAMP(n)                                                                                                            \
  static int64_t limen_cbi_##n(int64_t a0, int64_t a1, int64_t a2, int64_t a3, int64_t a4, int64_t a5, int64_t a6, int64_t a7, \
                              double d0, double d1, double d2, double d3, double d4, double d5, double d6, double d7) {     \
    int64_t a[8] = { a0, a1, a2, a3, a4, a5, a6, a7 };                                                                     \
    double d[8] = { d0, d1, d2, d3, d4, d5, d6, d7 };                                                                      \
    return limen_cb_run(0, n, a, d);                                                                                        \
  }                                                                                                                         \
  static double limen_cbd_##n(int64_t a0, int64_t a1, int64_t a2, int64_t a3, int64_t a4, int64_t a5, int64_t a6, int64_t a7, \
                             double d0, double d1, double d2, double d3, double d4, double d5, double d6, double d7) {      \
    int64_t a[8] = { a0, a1, a2, a3, a4, a5, a6, a7 };                                                                     \
    double d[8] = { d0, d1, d2, d3, d4, d5, d6, d7 };                                                                      \
    int64_t r = limen_cb_run(1, n, a, d);                                                                                   \
    double x;                                                                                                               \
    memcpy(&x, &r, 8);                                                                                                      \
    return x;                                                                                                               \
  }
TRAMP(0) TRAMP(1) TRAMP(2) TRAMP(3) TRAMP(4) TRAMP(5) TRAMP(6) TRAMP(7) TRAMP(8) TRAMP(9) TRAMP(10) TRAMP(11) TRAMP(12) TRAMP(13)
TRAMP(14) TRAMP(15) TRAMP(16) TRAMP(17) TRAMP(18) TRAMP(19) TRAMP(20) TRAMP(21) TRAMP(22) TRAMP(23) TRAMP(24) TRAMP(25) TRAMP(26)
TRAMP(27) TRAMP(28) TRAMP(29) TRAMP(30) TRAMP(31)
#define TL(p)                                                                                                                       \
  { (void *)limen_cb##p##_0, (void *)limen_cb##p##_1, (void *)limen_cb##p##_2, (void *)limen_cb##p##_3, (void *)limen_cb##p##_4,       \
    (void *)limen_cb##p##_5, (void *)limen_cb##p##_6, (void *)limen_cb##p##_7, (void *)limen_cb##p##_8, (void *)limen_cb##p##_9,       \
    (void *)limen_cb##p##_10, (void *)limen_cb##p##_11, (void *)limen_cb##p##_12, (void *)limen_cb##p##_13, (void *)limen_cb##p##_14,  \
    (void *)limen_cb##p##_15, (void *)limen_cb##p##_16, (void *)limen_cb##p##_17, (void *)limen_cb##p##_18, (void *)limen_cb##p##_19,  \
    (void *)limen_cb##p##_20, (void *)limen_cb##p##_21, (void *)limen_cb##p##_22, (void *)limen_cb##p##_23, (void *)limen_cb##p##_24,  \
    (void *)limen_cb##p##_25, (void *)limen_cb##p##_26, (void *)limen_cb##p##_27, (void *)limen_cb##p##_28, (void *)limen_cb##p##_29,  \
    (void *)limen_cb##p##_30, (void *)limen_cb##p##_31 }
static void *const limen_cb_addrs[2][NCB] = { TL(i), TL(d) };

/* limen_ffi_cb_take : bool (double result) -> int: a free slot, now taken, or -1 */
value limen_ffi_cb_take(value dbl) {
  int p = Bool_val(dbl);
  for (int k = 0; k < NCB; k++)
    if (!limen_cb_used[p][k]) {
      limen_cb_used[p][k] = 1;
      limen_cb_thread = pthread_self();
      return Val_int(k);
    }
  return Val_int(-1);
}

/* limen_ffi_cb_free : bool -> int -> unit */
value limen_ffi_cb_free(value dbl, value k) {
  limen_cb_used[Bool_val(dbl)][Int_val(k)] = 0;
  return Val_unit;
}

/* limen_ffi_cb_addr : bool -> int -> nativeint (the slot's trampoline) */
value limen_ffi_cb_addr(value dbl, value k) {
  CAMLparam2(dbl, k);
  CAMLreturn(caml_copy_nativeint((intnat)limen_cb_addrs[Bool_val(dbl)][Int_val(k)]));
}

/* limen_ffi_cb_fail : unit -> unit: the callback running now failed; its
   trampoline abandons the foreign call */
value limen_ffi_cb_fail(value unit) {
  (void)unit;
  limen_cb_failed = 1;
  return Val_unit;
}

/* ---- kept callbacks ----
   C may keep a kept callback's pointer (`Kept[fn(...)]`) and call it at any
   time, from any thread, so such a call never enters OCaml: it copies its
   arguments, and the strings among them (the slot's mask names which
   integer words those are), onto a queue and writes a byte to a pipe. The
   interpreter's scheduler watches the pipe and turns each call into a value
   on the slot's channel (Interp.Foreign.kept_drain). A slot stays on for the
   rest of the program, since C may still hold its pointer. */
typedef struct LKept {
  struct LKept *next;
  int slot;
  int64_t a[8];
  double d[8];
  char *s[8];
} LKept;
static pthread_mutex_t limen_kept_mu = PTHREAD_MUTEX_INITIALIZER;
static LKept *limen_kept_head, *limen_kept_tail;
static int limen_kept_mask[NCB], limen_kept_on[NCB];
static int limen_kept_pipe[2] = { -1, -1 };

static void limen_kept_run(int slot, int64_t *a, double *d) {
  pthread_mutex_lock(&limen_kept_mu);
  int on = limen_kept_on[slot], mask = limen_kept_mask[slot];
  pthread_mutex_unlock(&limen_kept_mu);
  if (!on) return;
  LKept *e = (LKept *)calloc(1, sizeof *e);
  if (!e) return;
  e->slot = slot;
  memcpy(e->a, a, sizeof e->a);
  memcpy(e->d, d, sizeof e->d);
  for (int k = 0; k < 8; k++)
    if (mask & (1 << k)) e->s[k] = strdup(a[k] ? (const char *)(intptr_t)a[k] : "");
  pthread_mutex_lock(&limen_kept_mu);
  if (limen_kept_tail) limen_kept_tail->next = e; else limen_kept_head = e;
  limen_kept_tail = e;
  pthread_mutex_unlock(&limen_kept_mu);
  char b = 1;
  if (write(limen_kept_pipe[1], &b, 1) < 0) { /* full: the reader is behind and will drain anyway */ }
}

#define KTRAMP(n)                                                                                                         \
  static void limen_kt_##n(int64_t a0, int64_t a1, int64_t a2, int64_t a3, int64_t a4, int64_t a5, int64_t a6, int64_t a7, \
                           double d0, double d1, double d2, double d3, double d4, double d5, double d6, double d7) {     \
    int64_t a[8] = { a0, a1, a2, a3, a4, a5, a6, a7 };                                                                   \
    double d[8] = { d0, d1, d2, d3, d4, d5, d6, d7 };                                                                    \
    limen_kept_run(n, a, d);                                                                                              \
  }
KTRAMP(0) KTRAMP(1) KTRAMP(2) KTRAMP(3) KTRAMP(4) KTRAMP(5) KTRAMP(6) KTRAMP(7) KTRAMP(8) KTRAMP(9) KTRAMP(10) KTRAMP(11)
KTRAMP(12) KTRAMP(13) KTRAMP(14) KTRAMP(15) KTRAMP(16) KTRAMP(17) KTRAMP(18) KTRAMP(19) KTRAMP(20) KTRAMP(21) KTRAMP(22)
KTRAMP(23) KTRAMP(24) KTRAMP(25) KTRAMP(26) KTRAMP(27) KTRAMP(28) KTRAMP(29) KTRAMP(30) KTRAMP(31)
static void *const limen_kt_addrs[NCB] = {
  (void *)limen_kt_0, (void *)limen_kt_1, (void *)limen_kt_2, (void *)limen_kt_3, (void *)limen_kt_4, (void *)limen_kt_5,
  (void *)limen_kt_6, (void *)limen_kt_7, (void *)limen_kt_8, (void *)limen_kt_9, (void *)limen_kt_10, (void *)limen_kt_11,
  (void *)limen_kt_12, (void *)limen_kt_13, (void *)limen_kt_14, (void *)limen_kt_15, (void *)limen_kt_16, (void *)limen_kt_17,
  (void *)limen_kt_18, (void *)limen_kt_19, (void *)limen_kt_20, (void *)limen_kt_21, (void *)limen_kt_22, (void *)limen_kt_23,
  (void *)limen_kt_24, (void *)limen_kt_25, (void *)limen_kt_26, (void *)limen_kt_27, (void *)limen_kt_28, (void *)limen_kt_29,
  (void *)limen_kt_30, (void *)limen_kt_31 };

/* limen_ffi_kept_open : int -> int -> nativeint: turn slot [k] on, its
   string words [mask]; its trampoline */
value limen_ffi_kept_open(value k, value mask) {
  CAMLparam2(k, mask);
  pthread_mutex_lock(&limen_kept_mu);
  if (limen_kept_pipe[0] < 0 && pipe(limen_kept_pipe) == 0) {
    for (int i = 0; i < 2; i++) {
      fcntl(limen_kept_pipe[i], F_SETFL, fcntl(limen_kept_pipe[i], F_GETFL) | O_NONBLOCK);
      fcntl(limen_kept_pipe[i], F_SETFD, FD_CLOEXEC);
    }
  }
  limen_kept_mask[Int_val(k)] = Int_val(mask);
  limen_kept_on[Int_val(k)] = 1;
  pthread_mutex_unlock(&limen_kept_mu);
  CAMLreturn(caml_copy_nativeint((intnat)limen_kt_addrs[Int_val(k)]));
}

/* limen_ffi_kept_fd : unit -> int: the pipe the scheduler watches (-1 before any slot) */
value limen_ffi_kept_fd(value unit) {
  (void)unit;
  return Val_int(limen_kept_pipe[0]);
}

/* limen_ffi_kept_take : unit -> (int * int64 array * float array * string array) list:
   every call queued so far, oldest first */
value limen_ffi_kept_take(value unit) {
  CAMLparam1(unit);
  CAMLlocal5(list, cell, item, ia, fa);
  CAMLlocal2(sa, x);
  char buf[256];
  if (limen_kept_pipe[0] >= 0) while (read(limen_kept_pipe[0], buf, sizeof buf) > 0) {}
  pthread_mutex_lock(&limen_kept_mu);
  LKept *e = limen_kept_head;
  limen_kept_head = limen_kept_tail = NULL;
  pthread_mutex_unlock(&limen_kept_mu);
  /* reversed, so consing below gives the oldest first */
  LKept *rev = NULL;
  while (e) { LKept *n = e->next; e->next = rev; rev = e; e = n; }
  list = Val_emptylist;
  while (rev) {
    LKept *n = rev->next;
    ia = caml_alloc(8, 0);
    for (int k = 0; k < 8; k++) { x = caml_copy_int64(rev->a[k]); Store_field(ia, k, x); }
    fa = caml_alloc(8 * Double_wosize, Double_array_tag);
    for (int k = 0; k < 8; k++) Store_double_flat_field(fa, k, rev->d[k]);
    sa = caml_alloc(8, 0);
    for (int k = 0; k < 8; k++) { x = caml_copy_string(rev->s[k] ? rev->s[k] : ""); Store_field(sa, k, x); free(rev->s[k]); }
    item = caml_alloc_tuple(4);
    Store_field(item, 0, Val_int(rev->slot));
    Store_field(item, 1, ia);
    Store_field(item, 2, fa);
    Store_field(item, 3, sa);
    cell = caml_alloc(2, 0);
    Store_field(cell, 0, item);
    Store_field(cell, 1, list);
    list = cell;
    free(rev);
    rev = n;
  }
  CAMLreturn(list);
}

/* limen_ffi_cstr : int64 -> string (the NUL-terminated string at the address; "" for NULL) */
value limen_ffi_cstr(value p) {
  CAMLparam1(p);
  const char *s = (const char *)(intptr_t)Int64_val(p);
  CAMLreturn(caml_copy_string(s ? s : ""));
}

/* limen_ffi_peek : int64 -> int -> string (n bytes at the address) */
value limen_ffi_peek(value p, value n) {
  CAMLparam2(p, n);
  CAMLlocal1(r);
  const char *s = (const char *)(intptr_t)Int64_val(p);
  intnat len = Long_val(n);
  r = caml_alloc_string((mlsize_t)len);
  if (len > 0) memcpy((char *)Bytes_val(r), s, (size_t)len);
  CAMLreturn(r);
}

/* The interpreter's way into the operating-system sandbox (limen_sandbox.h),
   entered by `limen run` once the program is loaded and its granted foreign
   libraries are resolved, before main starts.
   limen_sandbox_enter : string -> bool -> bool -> string -> int
   (fs root or "", read-only, net, the programs Exec names one per line or
   "") -> 0 enforced, 1 unsupported here; raises Failure with the reason if
   it could not be entered. */
#include <caml/fail.h>
#include "runtime/limen_sandbox.h"
#include "runtime/limen_core_inline.h"
#include "runtime/limen_core.h"
value limen_sandbox_enter(value root, value ro, value net, value exec) {
  CAMLparam4(root, ro, net, exec);
  int has_exec = caml_string_length(exec) > 0;
  WSandboxSpec s = { caml_string_length(root) ? String_val(root) : NULL, Bool_val(ro), Bool_val(net), has_exec, has_exec ? String_val(exec) : NULL };
  char err[512];
  int rc = w_sandbox_enter(&s, err, sizeof err);
  if (rc < 0) caml_failwith(err);
  CAMLreturn(Val_int(rc));
}

/* A file's modification time in whole milliseconds, as the native runtime's
   Fs.info gives it: OCaml's Unix.stat has it only as a float, which cannot
   always give the same millisecond back (lc_mtime_ms). Follows links; -1
   when the file cannot be read.
   limen_mtime_ms : string -> int */
value limen_mtime_ms(value path) {
  CAMLparam1(path);
  struct stat st;
  if (stat(String_val(path), &st) != 0) CAMLreturn(Val_long(-1));
  CAMLreturn(Val_long(lc_mtime_ms(&st)));
}

/* Fill a buffer from the operating system's cryptographically secure
   generator (lc_os_random, the native runtime's Rand without --seed too).
   Returns false if the generators gave fewer bytes.
   limen_os_random : bytes -> bool */
value limen_os_random(value buf) {
  return Val_bool(lc_os_random(Bytes_val(buf), caml_string_length(buf)) == 0);
}

/* Fs for the interpreter: the same walk through directory descriptors as
   the native runtime (w_fs_at and the functions after it in
   limen_sandbox.h), so both backends reach the same files and say the same
   things. `top` is the granted directory, `root` the capability's.
   limen_fs_op : int -> string -> string -> string -> string -> int * string
   op: 0 read, 1 write, 2 append, 3 list (names separated by NUL bytes),
   4 remove, 5 make_dir, 6 rename (a to b), 7 full_path, 8 under (kind 0
   and the new root, or kind 1: not inside), 9 read_at, 10 copy (a to b),
   11 touch, 12 set_executable (b is "1" to set, "0" to clear), 13
   read_link. b is the data for write and append. The int is 0 for Ok (the string is the result), 1 for Denied and
   2 for Failed (the string is the message). */
#include <dirent.h>
static value limen_fs_pair(int kind, const char *s, size_t n) {
  CAMLparam0();
  CAMLlocal2(r, str);
  str = caml_alloc_initialized_string(n, s);
  r = caml_alloc_tuple(2);
  Store_field(r, 0, Val_int(kind));
  Store_field(r, 1, str);
  CAMLreturn(r);
}
value limen_fs_op(value op, value top, value root, value a, value b) {
  CAMLparam5(op, top, root, a, b);
  CAMLlocal1(r);
  const char *t = String_val(top), *ro = String_val(root);
  const char *ap = String_val(a);
  size_t an = caml_string_length(a);
  WFsErr e;
  e.kind = 2; e.msg[0] = 0;
  switch (Int_val(op)) {
    case 0: {
      int fd = w_fs_open(t, ro, ap, an, W_FS_READ, &e);
      if (fd < 0) break;
      size_t cap = 65536, n = 0;
      char *buf = (char *)w_sb_alloc(cap);
      for (;;) {
        if (n == cap) { cap *= 2; buf = (char *)realloc(buf, cap); if (!buf) w_sb_oom(); }
        ssize_t k = read(fd, buf + n, cap - n);
        if (k < 0 && errno == EINTR) continue;
        if (k <= 0) break;
        n += (size_t)k;
      }
      close(fd);
      r = limen_fs_pair(0, buf, n);
      free(buf);
      CAMLreturn(r);
    }
    case 1: case 2: {
      int fd = w_fs_open(t, ro, ap, an, Int_val(op) == 1 ? W_FS_WRITE : W_FS_APPEND, &e);
      if (fd < 0) break;
      /* the data is copied: the OCaml string may move while write waits */
      size_t n = caml_string_length(b);
      char *d = (char *)w_sb_alloc(n);
      memcpy(d, String_val(b), n);
      int err = w_fs_write_all(fd, d, n);
      free(d);
      close(fd);
      if (err) { e.kind = 2; snprintf(e.msg, sizeof e.msg, "fs: `%.*s`: %s", (int)an, ap, strerror(err)); break; }
      CAMLreturn(limen_fs_pair(0, "", 0));
    }
    case 3: {
      DIR *d = w_fs_opendir(t, ro, ap, an, &e);
      if (!d) break;
      size_t cap = 256, n = 0;
      char *buf = (char *)w_sb_alloc(cap);
      struct dirent *de;
      while ((de = readdir(d))) {
        if (strcmp(de->d_name, ".") == 0 || strcmp(de->d_name, "..") == 0) continue;
        size_t l = strlen(de->d_name);
        while (n + l + 1 > cap) { cap *= 2; buf = (char *)realloc(buf, cap); if (!buf) w_sb_oom(); }
        memcpy(buf + n, de->d_name, l + 1);
        n += l + 1;
      }
      closedir(d);
      r = limen_fs_pair(0, buf, n);
      free(buf);
      CAMLreturn(r);
    }
    case 9: {
      /* read_at: b is "OFFSET COUNT" */
      long long off = -1, cnt = -1;
      if (sscanf(String_val(b), "%lld %lld", &off, &cnt) != 2 || off < 0 || cnt < 0) {
        e.kind = 2; snprintf(e.msg, sizeof e.msg, "fs: read_at needs an offset and a count of at least 0"); break;
      }
      if (cnt > (64LL << 20)) { e.kind = 2; snprintf(e.msg, sizeof e.msg, "fs: read_at reads at most 64 MB at a time"); break; }
      int fd = w_fs_open(t, ro, ap, an, W_FS_READ, &e);
      if (fd < 0) break;
      char *buf = (char *)w_sb_alloc(cnt > 0 ? (size_t)cnt : 1);
      long long got = 0;
      int err = 0;
      while (got < cnt) {
        ssize_t k = pread(fd, buf + got, (size_t)(cnt - got), (off_t)(off + got));
        if (k < 0 && errno == EINTR) continue;
        if (k < 0) { err = errno; break; }
        if (k == 0) break;
        got += k;
      }
      close(fd);
      if (err) { free(buf); e.kind = 2; snprintf(e.msg, sizeof e.msg, "fs: `%.*s`: %s", (int)an, ap, strerror(err)); break; }
      r = limen_fs_pair(0, buf, (size_t)got);
      free(buf);
      CAMLreturn(r);
    }
    case 4: if (w_fs_remove(t, ro, ap, an, &e) == 0) CAMLreturn(limen_fs_pair(0, "", 0)); break;
    case 5: if (w_fs_mkdir(t, ro, ap, an, &e) == 0) CAMLreturn(limen_fs_pair(0, "", 0)); break;
    case 6:
      if (w_fs_rename(t, ro, ap, an, String_val(b), caml_string_length(b), &e) == 0) CAMLreturn(limen_fs_pair(0, "", 0));
      break;
    case 7: {
      char *p = w_fs_full_path(t, ro, ap, an, &e);
      if (!p) break;
      r = limen_fs_pair(0, p, strlen(p));
      free(p);
      CAMLreturn(r);
    }
    case 10:
      if (w_fs_copy(t, ro, ap, an, String_val(b), caml_string_length(b), &e) == 0) CAMLreturn(limen_fs_pair(0, "", 0));
      break;
    case 11: if (w_fs_touch(t, ro, ap, an, &e) == 0) CAMLreturn(limen_fs_pair(0, "", 0)); break;
    case 12:
      if (w_fs_set_executable(t, ro, ap, an, String_val(b)[0] == '1', &e) == 0) CAMLreturn(limen_fs_pair(0, "", 0));
      break;
    case 13: {
      size_t n;
      char *p = w_fs_read_link(t, ro, ap, an, &n, &e);
      if (!p) break;
      r = limen_fs_pair(0, p, n);
      free(p);
      CAMLreturn(r);
    }
    case 8: {
      char *p = w_fs_under(t, ro, ap, an);
      if (!p) CAMLreturn(limen_fs_pair(1, "", 0));
      r = limen_fs_pair(0, p, strlen(p));
      free(p);
      CAMLreturn(r);
    }
  }
  CAMLreturn(limen_fs_pair(e.kind, e.msg, strlen(e.msg)));
}

/* What a path names, following links, as Fs.info, exists and is_dir see it.
   limen_fs_stat : string -> string -> string -> int * string * int * int * bool * bool
   (kind and message as limen_fs_op, size, modification time in
   milliseconds, is a file, is a directory) */
value limen_fs_stat(value top, value root, value rel) {
  CAMLparam3(top, root, rel);
  CAMLlocal2(r, m);
  WFsErr e;
  struct stat st;
  int rc = w_fs_stat(String_val(top), String_val(root), String_val(rel), caml_string_length(rel), &st, &e);
  r = caml_alloc_tuple(6);
  if (rc != 0) {
    m = caml_copy_string(e.msg);
    Store_field(r, 0, Val_int(e.kind)); Store_field(r, 1, m);
    Store_field(r, 2, Val_long(0)); Store_field(r, 3, Val_long(0)); Store_field(r, 4, Val_false); Store_field(r, 5, Val_false);
    CAMLreturn(r);
  }
  int64_t ms = lc_mtime_ms(&st);
  m = caml_copy_string("");
  Store_field(r, 0, Val_int(0)); Store_field(r, 1, m);
  Store_field(r, 2, Val_long((intnat)st.st_size)); Store_field(r, 3, Val_long((intnat)ms));
  Store_field(r, 4, Val_bool(S_ISREG(st.st_mode))); Store_field(r, 5, Val_bool(S_ISDIR(st.st_mode)));
  CAMLreturn(r);
}

/* ---- SQL statements (the std sql module's limen_sql_run) ----
   One statement runs on the blocking-call pool, through w_sql_core
   (runtime/limen_sql.h, shared with the native runtime), while its task
   waits on the pipe like a socket. The text and parameters are copied, so
   the OCaml heap can move. A cancelled task sets `stop`, which makes the
   statement give up, and still waits for the pool thread (limen_sql_finish),
   so the connection is never used after the task let it go. A statement
   always runs, even one stopped while still in the queue: w_sql_core gives
   up at once and reports it as it does any stopped statement. */
#include "runtime/limen_sql.h"

typedef struct {
  limen_pjob hd;
  void *api[W_SQL_NAPI];
  void *db;
  char *text, *params;
  int64_t tlen, plen;
  int mode, stop;
  WSqlBuf out;
  int fds[2];
  int state; /* 0 running, 1 done (under mu) */
  pthread_mutex_t mu;
} limen_sql_job;

static void limen_sql_job_run(limen_pjob *p) {
  limen_sql_job *j = (limen_sql_job *)p;
  w_sql_core(j->api, j->db, j->text, j->tlen, j->params, j->plen, j->mode, &j->stop, &j->out);
  pthread_mutex_lock(&j->mu);
  ssize_t k;
  do k = write(j->fds[1], "x", 1); while (k < 0 && errno == EINTR);
  j->state = 1;
  pthread_mutex_unlock(&j->mu);
}

static char *limen_sql_copy(value s) {
  mlsize_t n = caml_string_length(s);
  char *p = malloc(n ? n : 1);
  if (!p) { fputs("panic: out of memory\n", stderr); exit(1); }
  memcpy(p, String_val(s), n);
  return p;
}

static void limen_sql_free(limen_sql_job *j) {
  free(j->text);
  free(j->params);
  free(j->out.p);
  close(j->fds[0]);
  close(j->fds[1]);
  pthread_mutex_destroy(&j->mu);
  free(j);
}

/* limen_sql_start : nativeint array -> nativeint -> string -> string -> int -> nativeint * int
   (SQLite's functions in W_SQL_* order, the connection, the text, the
   encoded parameters, the mode) -> (job, descriptor to wait on); a job of 0
   if the pool has no thread. */
value limen_sql_start(value api, value db, value text, value params, value mode) {
  CAMLparam5(api, db, text, params, mode);
  CAMLlocal2(r, jv);
  limen_sql_job *j = calloc(1, sizeof *j);
  if (!j) { fputs("panic: out of memory\n", stderr); exit(1); }
  for (int k = 0; k < W_SQL_NAPI && k < (int)Wosize_val(api); k++) j->api[k] = (void *)Nativeint_val(Field(api, k));
  j->db = (void *)Nativeint_val(db);
  j->text = limen_sql_copy(text); j->tlen = (int64_t)caml_string_length(text);
  j->params = limen_sql_copy(params); j->plen = (int64_t)caml_string_length(params);
  j->mode = Int_val(mode);
  pthread_mutex_init(&j->mu, NULL);
  j->hd.run = limen_sql_job_run;
  int ok = pipe(j->fds) == 0;
  if (ok) {
    ok = limen_pool_submit(&j->hd);
    if (!ok) { close(j->fds[0]); close(j->fds[1]); }
  }
  if (!ok) {
    free(j->text); free(j->params);
    pthread_mutex_destroy(&j->mu);
    free(j);
    j = NULL;
  }
  jv = caml_copy_nativeint((intnat)j);
  r = caml_alloc_tuple(2);
  Store_field(r, 0, jv);
  Store_field(r, 1, Val_int(j ? j->fds[0] : -1));
  CAMLreturn(r);
}

/* limen_sql_done : nativeint -> bool */
value limen_sql_done(value jv) {
  limen_sql_job *j = (limen_sql_job *)Nativeint_val(jv);
  pthread_mutex_lock(&j->mu);
  int d = j->state == 1;
  pthread_mutex_unlock(&j->mu);
  return Val_bool(d);
}

/* limen_sql_stop : nativeint -> unit (the statement gives up soon) */
value limen_sql_stop(value jv) {
  limen_sql_job *j = (limen_sql_job *)Nativeint_val(jv);
  __atomic_store_n(&j->stop, 1, __ATOMIC_RELEASE);
  return Val_unit;
}

/* limen_sql_finish : nativeint -> string
   The result of a job that is done (see limen_sql.h); frees it. */
value limen_sql_finish(value jv) {
  CAMLparam1(jv);
  CAMLlocal1(r);
  limen_sql_job *j = (limen_sql_job *)Nativeint_val(jv);
  r = caml_alloc_initialized_string((mlsize_t)j->out.len, j->out.p ? j->out.p : "");
  limen_sql_free(j);
  CAMLreturn(r);
}

/* limen_sql_open : nativeint array -> string -> string -> int -> string
   (SQLite's functions in W_SQL_* order, the Fs root, the database's name,
   sqlite3_open_v2's flags) -> the result of w_sql_open_core (limen_sql.h).
   The interpreter's Fs keeps its granted directory in its value
   (caps.ml), out of this stub's reach, so the walk starts from the granted
   directory the sandbox code keeps (w_fs_top_path). That is the Fs's own:
   sql.open walks to the root (fs.full_path(".")) just before, which keeps
   that directory, and the interpreter runs one granted directory at a time
   (limen_sandbox.h). A root outside the kept directory is refused. */
value limen_sql_open(value api, value root, value name, value flags) {
  CAMLparam4(api, root, name, flags);
  CAMLlocal1(r);
  void *fns[W_SQL_NAPI] = { 0 };
  for (int k = 0; k < W_SQL_NAPI && k < (int)Wosize_val(api); k++) fns[k] = (void *)Nativeint_val(Field(api, k));
  mlsize_t rl = caml_string_length(root), nl = caml_string_length(name);
  char *ro = malloc(rl + 1), *nm = malloc(nl + 1);
  if (!ro || !nm) { fputs("panic: out of memory\n", stderr); exit(1); }
  memcpy(ro, String_val(root), rl); ro[rl] = 0;
  memcpy(nm, String_val(name), nl); nm[nl] = 0;
  pthread_mutex_lock(&w_fs_top_mu);
  char *top = w_fs_top_path ? w_sb_strndup(w_fs_top_path, strlen(w_fs_top_path)) : NULL;
  pthread_mutex_unlock(&w_fs_top_mu);
  WSqlBuf out = { 0 };
  if (!top || !w_fs_inside(top, ro))
    w_sql_open_result(&out, W_SQL_OPEN_DENIED, 0, NULL, "fs: not granted (run with --grant fs=DIR)");
  else
    w_sql_open_core((const void *const *)fns, top, ro, nm, (size_t)nl, Int_val(flags), &out);
  free(top); free(ro); free(nm);
  r = caml_alloc_initialized_string((mlsize_t)out.len, out.p ? out.p : "");
  free(out.p);
  CAMLreturn(r);
}
