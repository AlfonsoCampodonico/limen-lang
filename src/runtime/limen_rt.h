/* Limen C runtime: the declarations. Its behaviour mirrors the interpreter
   (src/interp.ml and src/caps.ml).

   The runtime is two files. This one holds what generated code needs to see:
   includes, macros, types, the small functions it inlines, and a prototype
   or extern declaration for everything else. limen_rt.c holds the rest of
   the definitions. A native build compiles limen_sandbox.h, this file and
   limen_rt.c as one object, once per C compiler and set of flags, and keeps
   it in the build cache; each piece of a program includes only this file.
   `limen build --emit-c` writes all three ahead of the program, as one file
   that needs nothing else.

   So: a function the program calls goes in limen_rt.c, marked W_RT, with
   its prototype here; one small and hot enough to be worth inlining into
   generated code is `static inline` here. Variables are defined in
   limen_rt.c and declared `extern` here.

   W_RT is `static` when everything is one file, so the C compiler leaves
   out what a program doesn't use, and empty when the runtime is an object
   of its own, so the program's pieces can call it. */
#ifndef W_RT
#define W_RT static
#endif
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <unistd.h>
#include <spawn.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/time.h>
#include <time.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/resource.h>
#include <pthread.h>
#include <setjmp.h>
#include <unwind.h>

extern char **environ;
typedef uint8_t W_Unit;

/* Counted allocations: every runtime object (strings, lists, closures,
   capabilities, boxed enums) goes through w_alloc/w_free, so tests can check
   that reference counting frees everything (LIMEN_LEAKCHECK=1). */
extern int64_t w_live;
extern int64_t w_allocs;  /* allocations ever made: `limen test --bench` reports them per operation */

/* Every allocation has a header. While a panic boundary (a `catch` or a
   task) is active, each object the task allocates is linked into the
   boundary's list until it is freed or shared. Code a panic abandons can
   only have left unshared objects nothing outside refers to (closures
   capture by value, and anything another task can see is shared first), so
   on a panic the boundary frees everything still on its list, by kind:
   lists their data, maps their tables, sockets their descriptors. */
typedef struct WHdr { struct WHdr *prev, *next; int64_t kind; void (*kids)(void *); } WHdr;
/* kids: drops what the object refers to, without freeing the object; used
   when a panic abandons it. */
enum { WK_BUF, WK_PLAIN, WK_LIST, WK_MAP, WK_CAP, WK_CHAN, WK_GROUP };
typedef struct WCatch {
  jmp_buf jb; struct WCatch *prev; char *msg; char *where; WHdr objs;
  int unwinding;                   /* a panic is unwinding to this boundary */
  int cancel;                      /* it is the task's cancellation, not a panic (msg is NULL) */
  struct _Unwind_Exception uexc;   /* in the boundary's frame, which unwinding does not touch */
} WCatch;

/* An owned local: its value, whether it still owns it, and how to drop it.
   Generated code gives every local that owns memory one of these with
   __attribute__((cleanup)), so unwinding after a panic drops it. */
typedef struct { void *v; uint8_t *o; void (*d)(void *); } WCl;
static inline void w_cl_run(WCl *c) { if (*c->o) { *c->o = 0; c->d(c->v); } }
W_RT WCatch **w_catch_slot(void);  /* the current fiber's innermost boundary, or NULL outside fibers */
W_RT void w_mark_unwinding(void);  /* the current fiber's cancellation is unwinding it */
W_RT void w_update_waits(void);  /* a wait refused in update's function: panic */

static inline void w_hdr_unlink(WHdr *h) {
  if (h->prev) { h->prev->next = h->next; h->next->prev = h->prev; h->prev = h->next = NULL; }
}
static inline void w_hdr_link(WHdr *list, WHdr *h) {
  h->next = list->next; h->prev = list; list->next->prev = h; list->next = h;
}
W_RT void *w_alloc_k(size_t n, int kind);
static inline void w_set_kids(void *o, void (*k)(void *)) { ((WHdr *)o - 1)->kids = k; }
W_RT void *w_alloc(size_t n);
W_RT void w_free(void *p);
/* Free an object whose count has reached zero: drop what it refers to
   (its kids function), then free it, by kind. Dropping a child that dies
   too does not recurse: the child is queued and the outermost call frees
   the queue, so a list of a million `Option[Node]`s, or a list of lists as
   deep, is freed with constant stack. Dropping never waits, so the queue
   can be the thread's. */
typedef struct { int64_t rc, len, cap; void *data; } WListAny;
typedef struct { int64_t rc, len, n, cap, icap; void *keys, *vals, *live, *idx; } WMapAny;
extern __thread WHdr *w_rel_queue;
extern __thread int w_rel_busy;
W_RT void w_release(void *o);
W_RT void w_catch_init(WCatch *c);
W_RT void w_catch_end(WCatch *c);
W_RT void w_catch_release(WCatch *c);  /* after a panic: free what the abandoned code left */
W_RT void *w_brealloc(void *p, size_t n);

/* Reference counts. rc > 0: owned by one thread, plain arithmetic.
   rc < 0: shared between threads (count is -rc), atomic arithmetic.
   rc == 0: immortal (string literals). An object is switched to shared,
   together with everything it references, before a task can see it. */
static inline void w_rc_inc(int64_t *rc) {
  int64_t r = __atomic_load_n(rc, __ATOMIC_RELAXED);
  if (r > 0) *rc = r + 1;
  else if (r < 0) __atomic_fetch_sub(rc, 1, __ATOMIC_RELAXED);
}
/* True when this was the last reference. */
static inline bool w_rc_dec(int64_t *rc) {
  int64_t r = __atomic_load_n(rc, __ATOMIC_RELAXED);
  if (r > 0) { *rc = r - 1; return r == 1; }
  if (r < 0) return __atomic_fetch_add(rc, 1, __ATOMIC_ACQ_REL) == -1;
  return false;
}
static inline bool w_rc_unique(int64_t *rc) { return __atomic_load_n(rc, __ATOMIC_RELAXED) == 1; }
/* Switch to shared; returns true if it was owned (so children need sharing too). */
static inline bool w_rc_share(int64_t *rc) {
  int64_t r = __atomic_load_n(rc, __ATOMIC_RELAXED);
  if (r > 0) {
    /* another task may hold it now, so it is no longer this boundary's to free
       (every shared object has its count first, right after the header) */
    w_hdr_unlink((WHdr *)rc - 1);
    __atomic_store_n(rc, -r, __ATOMIC_RELAXED);
    return true;
  }
  return false;
}
W_RT void *w_raw(size_t n);
__attribute__((noreturn)) W_RT void w_oom(void);
W_RT size_t w_bytes(int64_t count, size_t size);
W_RT void *w_realloc(void *p, size_t n);
W_RT char *w_strdup(const char *c);
W_RT char *w_strndup(const char *c, size_t n);
W_RT void w_leakcheck(void);

/* A panic boundary: `catch(|| ...)`, and every task. A panic inside one
   jumps back to it; memory and OS handles the abandoned code owned are not
   freed.

   Invariant for jumping out safely: the runtime only panics where no runtime
   structure is half-updated and no lock is held. Range, division and map-key
   checks happen before any write; channel send/close release the channel
   lock first. A failure that cannot meet this (the deadlock detector) calls
   w_fatal_c instead. Keep it that way when adding a w_panic_c call. */


W_RT void w_out_release_all(void);
__attribute__((noreturn)) W_RT void w_fatal_c(const char *msg, const char *where);
W_RT _Unwind_Reason_Code w_unwind_stop(int version, _Unwind_Action actions, _Unwind_Exception_Class cls, struct _Unwind_Exception *exc, struct _Unwind_Context *ctx, void *param);
W_RT void w_catch_land(WCatch **g);
__attribute__((noreturn)) W_RT void w_unwind_to(WCatch **slot, WCatch *c);
__attribute__((noreturn)) W_RT void w_panic_c(const char *msg, const char *where);
__attribute__((noreturn)) W_RT void w_cancel_unwind(void);
__attribute__((noreturn)) W_RT void w_unreachable(void);

/* ---------- strings ---------- */

typedef struct WStr { int64_t rc; int64_t len; char *data; } WStr; /* rc == 0: immortal literal */
W_RT WStr *Str_new(int64_t len);
W_RT WStr *Str_from(const char *p, int64_t len);
static inline WStr *Str_dup(WStr *s) { w_rc_inc(&s->rc); return s; }
static inline void Str_drop(WStr *s) { if (w_rc_dec(&s->rc)) w_free(s); }
static inline void Str_share(WStr *s) { w_rc_share(&s->rc); }
static inline bool Str_eq(WStr *a, WStr *b) { return a->len == b->len && memcmp(a->data, b->data, (size_t)a->len) == 0; }
W_RT int Str_cmp(WStr *a, WStr *b);
W_RT WStr *Str_concat(WStr *a, WStr *b);

/* ---------- buffers and printing ---------- */

typedef struct { char *p; int64_t len, cap; } WBuf;
W_RT void buf_init(WBuf *b);
W_RT void buf_putn(WBuf *b, const char *s, int64_t n);
W_RT void buf_puts(WBuf *b, const char *s);
W_RT WStr *buf_take(WBuf *b);
W_RT void fmt_float(char *out, size_t n, double x);
W_RT void buf_escaped(WBuf *b, WStr *s);

/* Deep values without deep recursion. Sharing, comparing and showing a
   value recurse into the objects it holds (enums that contain themselves,
   lists, maps, `dyn`, closures) only W_DEEP objects deep; past that, the
   object is put on a work stack of the thread's, and the outermost call
   finishes the stack before it returns, so a list of a million `Cons`es
   takes no more native stack than a short one. Like dropping (w_release),
   none of these waits or runs Limen code, so the stacks can be the
   thread's.
   - w_share(o, kids): kids shares what o holds; o's count is shared already.
   - w_eq(f, a, b, i): f compares two objects, its i saying where to resume
     (a list's next element); an object put on the stack counts as equal for
     now and is compared later, in the order the recursion would have taken,
     and the first difference ends the whole comparison. A map finding a key
     needs a real answer, so it holds the stack (w_eq_hold) while it looks.
   - w_show(b, f, v, i): f writes v to b. An object put on the stack is
     written later; the text written after it in the meantime is moved to a
     second stack and written back after it, so the output is the same.
   - w_cmp(f, a, b, i): f orders two objects (negative, zero, positive),
     resuming at i as w_eq's does. An object put on the stack counts as
     equal for now and is ordered later, in the order the recursion would
     have taken. A difference found after something was put on the stack in
     the same step comes later in that order, so it goes on the stack too, as
     an answer (w_cmp_res), and the first difference off the stack decides.
   - w_hash(f, o): f hashes o from part i on, starting from h. A hash needs
     its parts' hashes, so when a part goes on the stack, every call waiting
     for it goes on the stack too (w_hash_wait), with the hash so far, and
     each resumes once the part below it is done. The hash is the same as
     the recursion's. Parts are passed by address, so a struct's address
     must outlive the walk: the public X_hash always starts a fresh one. */
#define W_DEEP 32
typedef bool (*WEqFn)(void *, void *, int64_t);
typedef void (*WShowFn)(WBuf *, void *, int64_t);
extern __thread int w_sh_busy, w_sh_depth;
extern __thread int w_eq_busy, w_eq_depth;
extern __thread int64_t w_eq_n;
extern __thread WBuf *w_show_buf;
extern __thread int w_show_depth;
extern __thread int64_t w_show_n;
W_RT void w_share_outer(void *o, void (*kids)(void *));
W_RT void w_share_push(void *o, void (*kids)(void *));
W_RT bool w_eq_outer(WEqFn f, void *a, void *b, int64_t i);
W_RT void w_eq_push(WEqFn f, void *a, void *b, int64_t i);
W_RT void w_show_outer(WBuf *b, WShowFn f, void *v, int64_t i);
W_RT void w_show_push(WBuf *b, WShowFn f, void *v, int64_t i);
static inline void w_share(void *o, void (*kids)(void *)) {
  if (__builtin_expect(w_sh_busy && w_sh_depth < W_DEEP, 1)) { w_sh_depth++; kids(o); w_sh_depth--; }
  else if (!w_sh_busy) w_share_outer(o, kids);
  else w_share_push(o, kids);
}
static inline bool w_eq(WEqFn f, void *a, void *b, int64_t i) {
  if (__builtin_expect(w_eq_busy && w_eq_depth < W_DEEP, 1)) { w_eq_depth++; bool r = f(a, b, i); w_eq_depth--; return r; }
  if (!w_eq_busy) return w_eq_outer(f, a, b, i);
  w_eq_push(f, a, b, i);
  return true;
}
static inline int w_eq_hold(void) { int h = w_eq_busy; w_eq_busy = 0; return h; }
static inline void w_eq_resume(int h) { w_eq_busy = h; }
static inline void w_show(WBuf *b, WShowFn f, void *v, int64_t i) {
  if (__builtin_expect(w_show_buf == b && w_show_depth < W_DEEP, 1)) { w_show_depth++; f(b, v, i); w_show_depth--; }
  else if (w_show_buf != b) w_show_outer(b, f, v, i);
  else w_show_push(b, f, v, i);
}
typedef int (*WCmpFn)(void *, void *, int64_t);
typedef uint64_t (*WHashFn)(void *, int64_t, uint64_t);
extern __thread int w_cmp_busy, w_cmp_depth;
extern __thread int64_t w_cmp_n, w_cmp_base;
extern __thread int w_hash_busy, w_hash_depth, w_hash_susp;
W_RT int w_cmp_outer(WCmpFn f, void *a, void *b, int64_t i);
W_RT void w_cmp_push(WCmpFn f, void *a, void *b, int64_t i);
W_RT uint64_t w_hash_outer(WHashFn f, void *o);
W_RT void w_hash_push(WHashFn f, void *o);
W_RT void w_hash_wait(WHashFn f, void *o, int64_t i, uint64_t h);
static inline int w_cmp(WCmpFn f, void *a, void *b, int64_t i) {
  if (__builtin_expect(w_cmp_busy && w_cmp_depth < W_DEEP, 1)) { w_cmp_depth++; int r = f(a, b, i); w_cmp_depth--; return r; }
  if (!w_cmp_busy) return w_cmp_outer(f, a, b, i);
  w_cmp_push(f, a, b, i);
  return 0;
}
/* r, a difference just found: the answer now, or on the stack if something
   before it in the order is still to come */
static inline int w_cmp_res(int r) {
  if (__builtin_expect(w_cmp_n == w_cmp_base, 1)) return r;
  w_cmp_push(NULL, NULL, NULL, r);
  return 0;
}
#define W_HASH_SEED 0x2545F4914F6CDD1DULL
static inline uint64_t w_hash(WHashFn f, void *o) {
  if (__builtin_expect(w_hash_busy && w_hash_depth < W_DEEP, 1)) { w_hash_depth++; uint64_t h = f(o, 0, W_HASH_SEED); w_hash_depth--; return h; }
  if (!w_hash_busy) return w_hash_outer(f, o);
  w_hash_push(f, o);
  return 0;
}

#define Int_dup(x) (x)
#define Int_drop(x) ((void)0)
#define Int_eq(a, b) ((a) == (b))
#define Int_cmp(a, b) ((a) < (b) ? -1 : ((a) > (b) ? 1 : 0))
#define Float_dup(x) (x)
#define Float_drop(x) ((void)0)
#define Float_eq(a, b) ((a) == (b))
#define Bool_dup(x) (x)
#define Bool_drop(x) ((void)0)
#define Bool_eq(a, b) ((a) == (b))
#define Bool_cmp(a, b) ((int)(a) - (int)(b))
#define Unit_dup(x) (x)
#define Unit_drop(x) ((void)0)
#define Unit_eq(a, b) true
#define Unit_cmp(a, b) 0
#define Int_share(x) ((void)0)
#define UInt8_dup(x) (x)
#define UInt16_dup(x) (x)
#define UInt32_dup(x) (x)
#define UInt64_dup(x) (x)
#define UInt8_drop(x) ((void)0)
#define UInt16_drop(x) ((void)0)
#define UInt32_drop(x) ((void)0)
#define UInt64_drop(x) ((void)0)
#define UInt8_share(x) ((void)0)
#define UInt16_share(x) ((void)0)
#define UInt32_share(x) ((void)0)
#define UInt64_share(x) ((void)0)
#define UInt8_eq(a, b) ((a) == (b))
#define UInt16_eq(a, b) ((a) == (b))
#define UInt32_eq(a, b) ((a) == (b))
#define UInt64_eq(a, b) ((a) == (b))
#define UInt8_cmp(a, b) ((a) < (b) ? -1 : ((a) > (b) ? 1 : 0))
#define UInt16_cmp(a, b) ((a) < (b) ? -1 : ((a) > (b) ? 1 : 0))
#define UInt32_cmp(a, b) ((a) < (b) ? -1 : ((a) > (b) ? 1 : 0))
#define UInt64_cmp(a, b) ((a) < (b) ? -1 : ((a) > (b) ? 1 : 0))
#define UInt8_show(b, x, t) UInt_show(b, (uint64_t)(x), t)
#define UInt16_show(b, x, t) UInt_show(b, (uint64_t)(x), t)
#define UInt32_show(b, x, t) UInt_show(b, (uint64_t)(x), t)
#define UInt64_show(b, x, t) UInt_show(b, (uint64_t)(x), t)
#define Float_share(x) ((void)0)
#define Bool_share(x) ((void)0)
#define Unit_share(x) ((void)0)
W_RT int Float_cmp(double a, double b);
W_RT void Int_show(WBuf *b, int64_t x, bool top);
W_RT void UInt_show(WBuf *b, uint64_t x, bool top);
W_RT void Float_show(WBuf *b, double x, bool top);
W_RT void Bool_show(WBuf *b, bool x, bool top);
W_RT void Unit_show(WBuf *b, W_Unit x, bool top);
W_RT void Str_show(WBuf *b, WStr *s, bool top);

/* ---------- arithmetic ---------- */

static inline int64_t w_div(int64_t a, int64_t b, const char *where) {
  if (b == 0) w_panic_c("division by zero", where);
  if (b == -1) return (int64_t)(0 - (uint64_t)a);
  return a / b;
}
static inline int64_t w_mod(int64_t a, int64_t b, const char *where) {
  if (b == 0) w_panic_c("division by zero", where);
  if (b == -1) return 0;
  return a % b;
}
/* Shifts: a negative count panics, a count of 64 or more shifts every bit
   out (no undefined behaviour in C). `>>` keeps the sign; w_ushr is
   Int.shift_right_unsigned, which shifts in zeros. */
static inline int64_t w_shl(int64_t a, int64_t n, const char *where) {
  if (n < 0) w_panic_c("negative shift count", where);
  return n >= 64 ? 0 : (int64_t)((uint64_t)a << n);
}
static inline int64_t w_shr(int64_t a, int64_t n, const char *where) {
  if (n < 0) w_panic_c("negative shift count", where);
  return a >> (n >= 64 ? 63 : n);
}
static inline int64_t w_ushr(int64_t a, int64_t n, const char *where) {
  if (n < 0) w_panic_c("negative shift count", where);
  return n >= 64 ? 0 : (int64_t)((uint64_t)a >> n);
}
/* UInt8 to UInt64: C's unsigned types, which wrap; the operators cast
   back to the type, since C promotes the narrow ones to int. Division by
   zero panics, and a shift of the type's width or more shifts every bit
   out. */
static inline uint64_t w_udiv(uint64_t a, uint64_t b, const char *where) {
  if (b == 0) w_panic_c("division by zero", where);
  return a / b;
}
static inline uint64_t w_umod(uint64_t a, uint64_t b, const char *where) {
  if (b == 0) w_panic_c("division by zero", where);
  return a % b;
}
static inline uint64_t w_ushl_n(uint64_t a, uint64_t n, int bits) { return n >= (uint64_t)bits ? 0 : a << n; }
static inline uint64_t w_ushr_n(uint64_t a, uint64_t n, int bits) { return n >= (uint64_t)bits ? 0 : a >> n; }
static inline uint64_t w_urot(uint64_t x, int64_t k, int bits, int left) {
  int64_t r = k % bits; if (r < 0) r += bits;
  if (!left) r = (bits - r) % bits;
  if (r == 0) return x;
  uint64_t m = bits == 64 ? ~(uint64_t)0 : (((uint64_t)1 << bits) - 1);
  return ((x << r) | (x >> (bits - r))) & m;
}
static inline double w_u2f(uint64_t x) { return (double)x; }
static inline int64_t w_f2i(double x) {
  if (isnan(x)) return 0;
  if (x >= 9.2e18) return INT64_MAX;
  if (x <= -9.2e18) return INT64_MIN;
  return (int64_t)x;
}
static inline int64_t w_abs(int64_t x) { return x < 0 ? (int64_t)(0 - (uint64_t)x) : x; }
__attribute__((noreturn)) W_RT void w_panic_index(int64_t i, int64_t len, const char *where);

/* Bounds check for fixed-size arrays; with a constant size and a loop guard
   the C compiler removes it. */
static inline int64_t w_idx(int64_t i, int64_t n, const char *where) {
  if ((uint64_t)i >= (uint64_t)n) w_panic_index(i, n, where);
  return i;
}

/* ---------- lists ---------- */

#define LIMEN_LIST_STRUCT(L, T) struct L { int64_t rc; int64_t len; int64_t cap; T *data; };

#define LIMEN_LIST_FUNCS(L, T, P)                                                              \
  static void L##_kids(void *o) { L *l = (L *)o; for (int64_t i = 0; i < l->len; i++) P##_drop(l->data[i]); } \
  static L *L##_new(int64_t cap) {                                                            \
    L *l = (L *)w_alloc_k(sizeof(L), WK_LIST);                                                \
    w_set_kids(l, L##_kids);                                                                  \
    l->rc = 1; l->len = 0; l->cap = cap > 4 ? cap : 4;                                        \
    l->data = (T *)w_alloc(w_bytes(l->cap, sizeof(T)));                                       \
    return l;                                                                                 \
  }                                                                                           \
  static inline L *L##_dup(L *l) { w_rc_inc(&l->rc); return l; }                              \
  static void L##_drop(L *l) { if (w_rc_dec(&l->rc)) w_release(l); }                         \
  __attribute__((noinline, cold)) static void L##_copy(L **p) {                              \
    L *l = *p;                                                                                \
    L *c = L##_new(l->len);                                                                   \
    for (int64_t i = 0; i < l->len; i++) c->data[i] = P##_dup(l->data[i]);                   \
    c->len = l->len;                                                                          \
    *p = c;                                                                                   \
    L##_drop(l);                                                                              \
  }                                                                                           \
  static void L##_share_kids(void *o) { L *l = (L *)o; for (int64_t i = 0; i < l->len; i++) P##_share(l->data[i]); } \
  static void L##_share(L *l) { if (w_rc_share(&l->rc)) w_share(l, L##_share_kids); }        \
  static inline void L##_unique(L **p) { if (__builtin_expect(!w_rc_unique(&(*p)->rc), 0)) L##_copy(p); } \
  __attribute__((noinline)) static void L##_grow(L *l) {                                     \
    if (l->cap > INT64_MAX / 2) w_oom();                                                      \
    l->cap *= 2; l->data = (T *)w_brealloc(l->data, w_bytes(l->cap, sizeof(T)));              \
  }                                                                                           \
  static inline void L##_push_u(L **p, T v) {                                                 \
    L *l = *p;                                                                                \
    if (__builtin_expect(l->len == l->cap, 0)) L##_grow(l);                                   \
    l->data[l->len++] = v;                                                                    \
  }                                                                                           \
  static inline void L##_push(L **p, T v) { L##_unique(p); L##_push_u(p, v); }                \
  static void L##_reserve(L **p, int64_t cap) {                                               \
    L##_unique(p);                                                                            \
    L *l = *p;                                                                                \
    if (l->cap < cap) { l->data = (T *)w_brealloc(l->data, w_bytes(cap, sizeof(T))); l->cap = cap; } \
  }                                                                                           \
  static inline T *L##_at(L *l, int64_t i, const char *where) {                               \
    if (i < 0 || i >= l->len) w_panic_index(i, l->len, where);                                \
    return &l->data[i];                                                                       \
  }                                                                                           \
  /* from element i on; once an element is left on the stack, the rest of   \
     the list goes on after it */                                                             \
  static bool L##_eq_k(void *pa, void *pb, int64_t i) {                                       \
    L *a = (L *)pa, *b = (L *)pb;                                                             \
    for (; i < a->len; i++) {                                                                 \
      int64_t n = w_eq_n;                                                                     \
      if (!P##_eq(a->data[i], b->data[i])) return false;                                      \
      if (w_eq_n != n) { if (i + 1 < a->len) w_eq_push(L##_eq_k, a, b, i + 1); return true; } \
    }                                                                                         \
    return true;                                                                              \
  }                                                                                           \
  static bool L##_eq(L *a, L *b) { return a->len == b->len && w_eq(L##_eq_k, a, b, 0); }     \
  static void L##_show_k(WBuf *b, void *o, int64_t i) {                                       \
    L *l = (L *)o;                                                                            \
    if (i == 0) buf_puts(b, "[");                                                             \
    for (; i < l->len; i++) {                                                                 \
      if (i) buf_puts(b, ", ");                                                               \
      int64_t n = w_show_n;                                                                   \
      P##_show(b, l->data[i], false);                                                         \
      if (w_show_n != n && i + 1 < l->len) { w_show_push(b, L##_show_k, l, i + 1); return; }  \
    }                                                                                         \
    buf_puts(b, "]");                                                                         \
  }                                                                                           \
  static void L##_show(WBuf *b, L *l, bool top) { (void)top; w_show(b, L##_show_k, l, 0); }  \
  static L *L##_slice(L *l, int64_t from, int64_t to) {                                       \
    if (from < 0) from = 0; if (to > l->len) to = l->len; if (to < from) to = from;           \
    L *c = L##_new(to - from);                                                                \
    for (int64_t i = from; i < to; i++) c->data[c->len++] = P##_dup(l->data[i]);             \
    return c;                                                                                 \
  }                                                                                           \
  static L *L##_concat(L *a, L *b) {                                                          \
    L *c = L##_new(a->len + b->len);                                                          \
    for (int64_t i = 0; i < a->len; i++) c->data[c->len++] = P##_dup(a->data[i]);           \
    for (int64_t i = 0; i < b->len; i++) c->data[c->len++] = P##_dup(b->data[i]);           \
    return c;                                                                                 \
  }                                                                                           \
  static L *L##_reverse(L *l) {                                                               \
    L *c = L##_new(l->len);                                                                   \
    for (int64_t i = l->len - 1; i >= 0; i--) c->data[c->len++] = P##_dup(l->data[i]);      \
    return c;                                                                                 \
  }                                                                                           \
  static bool L##_contains(L *l, T x) {                                                       \
    for (int64_t i = 0; i < l->len; i++) if (P##_eq(l->data[i], x)) return true;             \
    return false;                                                                             \
  }

/* Stable merge sort, by the element type's P##_cmp order. */
#define LIMEN_LIST_SORT(L, T, P)                                                               \
  static L *L##_sort(L *l) {                                                                  \
    L *c = L##_slice(l, 0, l->len);                                                           \
    int64_t n = c->len;                                                                       \
    T *tmp = (T *)w_raw(sizeof(T) * (size_t)(n ? n : 1));                                     \
    for (int64_t w = 1; w < n; w *= 2) {                                                      \
      for (int64_t lo = 0; lo < n; lo += 2 * w) {                                             \
        int64_t mid = lo + w < n ? lo + w : n, hi = lo + 2 * w < n ? lo + 2 * w : n;          \
        int64_t i = lo, j = mid, k = lo;                                                      \
        while (i < mid && j < hi) tmp[k++] = P##_cmp(c->data[j], c->data[i]) < 0 ? c->data[j++] : c->data[i++]; \
        while (i < mid) tmp[k++] = c->data[i++];                                              \
        while (j < hi) tmp[k++] = c->data[j++];                                               \
      }                                                                                       \
      memcpy(c->data, tmp, sizeof(T) * (size_t)n);                                            \
    }                                                                                         \
    free(tmp);                                                                                \
    return c;                                                                                 \
  }

/* ---------- maps: insertion-ordered hash tables with copy-on-write ----------
   Entries live in arrays in insertion order; removed entries stay as holes
   (live = 0) until the next rehash compacts them. idx is an open-addressing
   table of entry positions, at most half full. */

/* A random seed per process, so nobody can choose keys that collide (hash
   flooding). Iteration follows insertion order, so output never depends on it. */
/* SipHash-1-3 with a 128-bit key from /dev/urandom: a keyed hash, so keys
   cannot be chosen to collide without knowing the key. */
extern uint64_t g_hash_k0, g_hash_k1;
W_RT void w_seed_hash(void);
#define W_ROTL(x, b) (uint64_t)(((x) << (b)) | ((x) >> (64 - (b))))
#define W_SIPROUND                                                          \
  do {                                                                      \
    v0 += v1; v1 = W_ROTL(v1, 13); v1 ^= v0; v0 = W_ROTL(v0, 32);           \
    v2 += v3; v3 = W_ROTL(v3, 16); v3 ^= v2;                                \
    v0 += v3; v3 = W_ROTL(v3, 21); v3 ^= v0;                                \
    v2 += v1; v1 = W_ROTL(v1, 17); v1 ^= v2; v2 = W_ROTL(v2, 32);           \
  } while (0)
W_RT uint64_t w_siphash13(const unsigned char *in, size_t len);
static inline uint64_t Int_hash(int64_t x) { return w_siphash13((const unsigned char *)&x, sizeof x); }
static inline uint64_t UInt_hash_(uint64_t x) { return w_siphash13((const unsigned char *)&x, sizeof x); }
#define UInt8_hash(x) UInt_hash_((uint64_t)(x))
#define UInt16_hash(x) UInt_hash_((uint64_t)(x))
#define UInt32_hash(x) UInt_hash_((uint64_t)(x))
#define UInt64_hash(x) UInt_hash_((uint64_t)(x))
static inline uint64_t Unit_hash(W_Unit u) { (void)u; return 0; }
static inline uint64_t Bool_hash(bool b) { return b ? 0x51ED27F1ULL : 0x2545F491ULL; }
/* Combine the hashes of a struct's fields (boost's hash_combine). */
static inline uint64_t w_hash_mix(uint64_t h, uint64_t x) { return h ^ (x + 0x9E3779B97F4A7C15ULL + (h << 6) + (h >> 2)); }
static inline uint64_t Str_hash(WStr *s) { return w_siphash13((const unsigned char *)s->data, (size_t)s->len); }
__attribute__((noreturn)) W_RT void w_panic_key(WBuf *key, const char *where);

#define LIMEN_MAP_STRUCT(M, K, V) \
  struct M { int64_t rc; int64_t len, n, cap, icap; K *keys; V *vals; uint8_t *live; int64_t *idx; };

#define LIMEN_MAP_FUNCS(M, K, KN, V, VN)                                                       \
  static void M##_kids(void *o) {                                                             \
    M *m = (M *)o;                                                                            \
    for (int64_t i = 0; i < m->n; i++) if (m->live[i]) { KN##_drop(m->keys[i]); VN##_drop(m->vals[i]); } \
  }                                                                                           \
  static M *M##_new(void) {                                                                   \
    M *m = (M *)w_alloc_k(sizeof(M), WK_MAP);                                                 \
    w_set_kids(m, M##_kids);                                                                  \
    m->rc = 1; m->len = 0; m->n = 0; m->cap = 8; m->icap = 16;                                \
    m->keys = (K *)w_alloc(w_bytes(m->cap, sizeof(K)));                                       \
    m->vals = (V *)w_alloc(w_bytes(m->cap, sizeof(V)));                                       \
    m->live = (uint8_t *)w_alloc((size_t)m->cap);                                             \
    m->idx = (int64_t *)w_alloc(w_bytes(m->icap, sizeof(int64_t)));                           \
    for (int64_t i = 0; i < m->icap; i++) m->idx[i] = -1;                                     \
    return m;                                                                                 \
  }                                                                                           \
  static inline M *M##_dup(M *m) { w_rc_inc(&m->rc); return m; }                              \
  static void M##_drop(M *m) { if (w_rc_dec(&m->rc)) w_release(m); }                         \
  static void M##_share_kids(void *o) {                                                       \
    M *m = (M *)o;                                                                            \
    for (int64_t i = 0; i < m->n; i++) if (m->live[i]) { KN##_share(m->keys[i]); VN##_share(m->vals[i]); } \
  }                                                                                           \
  static void M##_share(M *m) { if (w_rc_share(&m->rc)) w_share(m, M##_share_kids); }        \
  static int64_t M##_find(M *m, K k) {                                                        \
    uint64_t mask = (uint64_t)m->icap - 1;                                                    \
    for (uint64_t h = KN##_hash(k) & mask;; h = (h + 1) & mask) {                             \
      int64_t j = m->idx[h];                                                                  \
      if (j < 0) return -1;                                                                   \
      if (m->live[j] && KN##_eq(m->keys[j], k)) return j;                                    \
    }                                                                                         \
  }                                                                                           \
  /* compact the holes away and rebuild idx, sized for cap entries */                         \
  static void M##_rehash(M *m, int64_t cap) {                                                 \
    int64_t w = 0;                                                                            \
    for (int64_t i = 0; i < m->n; i++)                                                        \
      if (m->live[i]) { m->keys[w] = m->keys[i]; m->vals[w] = m->vals[i]; m->live[w] = 1; w++; } \
    m->n = w;                                                                                 \
    if (cap != m->cap) {                                                                      \
      m->keys = (K *)w_brealloc(m->keys, w_bytes(cap, sizeof(K)));                            \
      m->vals = (V *)w_brealloc(m->vals, w_bytes(cap, sizeof(V)));                            \
      m->live = (uint8_t *)w_brealloc(m->live, (size_t)cap);                                  \
      m->cap = cap;                                                                           \
    }                                                                                         \
    int64_t icap = 16;                                                                        \
    while (icap < 2 * cap) icap *= 2;                                                         \
    w_free(m->idx);                                                                           \
    m->idx = (int64_t *)w_alloc(w_bytes(icap, sizeof(int64_t)));                              \
    m->icap = icap;                                                                           \
    for (int64_t i = 0; i < icap; i++) m->idx[i] = -1;                                        \
    uint64_t mask = (uint64_t)icap - 1;                                                       \
    for (int64_t j = 0; j < m->n; j++) {                                                      \
      uint64_t h = KN##_hash(m->keys[j]) & mask;                                              \
      while (m->idx[h] >= 0) h = (h + 1) & mask;                                              \
      m->idx[h] = j;                                                                          \
    }                                                                                         \
  }                                                                                           \
  static void M##_insert_new(M *m, K k, V v) {                                                \
    if (m->n == m->cap) {                                                                     \
      if (m->cap > INT64_MAX / 4) w_oom();                                                    \
      M##_rehash(m, m->len * 2 < m->cap ? m->cap : m->cap * 2);                               \
    }                                                                                         \
    int64_t j = m->n++;                                                                       \
    m->keys[j] = k; m->vals[j] = v; m->live[j] = 1; m->len++;                                 \
    uint64_t mask = (uint64_t)m->icap - 1;                                                    \
    uint64_t h = KN##_hash(k) & mask;                                                         \
    while (m->idx[h] >= 0) h = (h + 1) & mask;                                                \
    m->idx[h] = j;                                                                            \
  }                                                                                           \
  __attribute__((noinline, cold)) static void M##_copy(M **p) {                              \
    M *m = *p;                                                                                \
    M *c = M##_new();                                                                         \
    for (int64_t i = 0; i < m->n; i++)                                                        \
      if (m->live[i]) M##_insert_new(c, KN##_dup(m->keys[i]), VN##_dup(m->vals[i]));          \
    *p = c;                                                                                   \
    M##_drop(m);                                                                              \
  }                                                                                           \
  static inline void M##_unique(M **p) { if (__builtin_expect(!w_rc_unique(&(*p)->rc), 0)) M##_copy(p); } \
  /* m[k] = v: consumes k and v */                                                            \
  static void M##_put(M **p, K k, V v) {                                                      \
    M##_unique(p);                                                                            \
    M *m = *p;                                                                                \
    int64_t j = M##_find(m, k);                                                               \
    if (j >= 0) { KN##_drop(k); VN##_drop(m->vals[j]); m->vals[j] = v; return; }             \
    M##_insert_new(m, k, v);                                                                  \
  }                                                                                           \
  /* consumes k */                                                                            \
  static void M##_remove(M **p, K k) {                                                        \
    M##_unique(p);                                                                            \
    M *m = *p;                                                                                \
    int64_t j = M##_find(m, k);                                                               \
    if (j >= 0) { KN##_drop(m->keys[j]); VN##_drop(m->vals[j]); m->live[j] = 0; m->len--; }   \
    KN##_drop(k);                                                                             \
  }                                                                                           \
  static inline V *M##_get(M *m, K k) { int64_t j = M##_find(m, k); return j < 0 ? NULL : &m->vals[j]; } \
  static bool M##_eq_k(void *pa, void *pb, int64_t from) {                                    \
    M *a = (M *)pa, *b = (M *)pb;                                                             \
    (void)from;                                                                               \
    for (int64_t i = 0; i < a->n; i++)                                                        \
      if (a->live[i]) {                                                                       \
        /* a key lookup needs a real answer: its own == starts a fresh walk */                \
        int h = w_eq_hold(); V *w = M##_get(b, a->keys[i]); w_eq_resume(h);                   \
        if (!w || !VN##_eq(a->vals[i], *w)) return false;                                     \
      }                                                                                       \
    return true;                                                                              \
  }                                                                                           \
  static bool M##_eq(M *a, M *b) { return a->len == b->len && w_eq(M##_eq_k, a, b, 0); }     \
  static void M##_show_k(WBuf *b, void *o, int64_t from) {                                    \
    M *m = (M *)o;                                                                            \
    (void)from;                                                                               \
    buf_puts(b, "{");                                                                         \
    bool first = true;                                                                        \
    for (int64_t i = 0; i < m->n; i++)                                                        \
      if (m->live[i]) {                                                                       \
        if (!first) buf_puts(b, ", ");                                                        \
        first = false;                                                                        \
        KN##_show(b, m->keys[i], false); buf_puts(b, ": "); VN##_show(b, m->vals[i], false);  \
      }                                                                                       \
    buf_puts(b, "}");                                                                         \
  }                                                                                           \
  static void M##_show(WBuf *b, M *m, bool top) { (void)top; w_show(b, M##_show_k, m, 0); }  \
  static V *M##_slot(M *m, K k, const char *where) {                                         \
    V *v = M##_get(m, k);                                                                     \
    if (!v) { WBuf kb; buf_init(&kb); KN##_show(&kb, k, false); w_panic_key(&kb, where); }   \
    return v;                                                                                 \
  }                                                                                           \
  static V M##_at(M *m, K k, const char *where) { return VN##_dup(*M##_slot(m, k, where)); }

typedef struct L_Int L_Int;
typedef struct L_Str L_Str;
LIMEN_LIST_STRUCT(L_Int, int64_t)
LIMEN_LIST_STRUCT(L_Str, WStr *)
LIMEN_LIST_FUNCS(L_Int, int64_t, Int)
LIMEN_LIST_FUNCS(L_Str, WStr *, Str)
LIMEN_LIST_SORT(L_Int, int64_t, Int)
LIMEN_LIST_SORT(L_Str, WStr *, Str)
W_RT int64_t L_Int_sum(L_Int *l);
W_RT WStr *L_Str_join(L_Str *l, WStr *sep);
W_RT int64_t Str_find(WStr *s, WStr *sub, int64_t from);
W_RT int64_t Str_rfind(WStr *s, WStr *sub);
W_RT L_Str *Str_split(WStr *s, WStr *sep);
W_RT L_Str *Str_lines(WStr *s);
W_RT bool is_ws(char c);
W_RT WStr *Str_trim(WStr *s);
W_RT WStr *Str_map_case(WStr *s, bool upper);
W_RT WStr *Str_replace(WStr *s, WStr *a, WStr *b);
W_RT bool Str_starts(WStr *s, WStr *p);
W_RT bool Str_ends(WStr *s, WStr *p);
W_RT bool Str_parse_int(WStr *s, int64_t *out);
W_RT WStr *Str_slice(WStr *s, int64_t lo, int64_t hi);
W_RT L_Int *Str_bytes(WStr *s);
/* Bytes: a WStr holding binary data; only printing differs from Str. */
#define Bytes_dup Str_dup
#define Bytes_drop Str_drop
#define Bytes_share Str_share
#define Bytes_eq Str_eq
#define Bytes_cmp Str_cmp
#define Bytes_hash Str_hash
W_RT void Bytes_show(WBuf *b, WStr *s, bool top);
W_RT WStr *Bytes_from_list(L_Int *l, const char *where);
W_RT int64_t Bytes_at(WStr *s, int64_t i, const char *where);
W_RT int64_t w_utf8_len(const unsigned char *s, int64_t i, int64_t n);
W_RT L_Str *Str_chars(WStr *s);
W_RT L_Int *L_Int_range(int64_t lo, int64_t hi);
W_RT WStr *Int_to_char(int64_t n);
W_RT WStr *Float_to_fixed(double x, int64_t d);
W_RT bool Str_parse_float(WStr *s, double *out);

/* ---------- closures ---------- */

typedef struct WClo WClo;
struct WClo { int64_t rc; void *fn; void (*drop)(WClo *); void (*share)(WClo *); };
static inline WClo *Clo_dup(WClo *c) { w_rc_inc(&c->rc); return c; }
W_RT void Clo_drop(WClo *c);
W_RT void Clo_share(WClo *c);
static inline bool Clo_eq(WClo *a, WClo *b) { return a == b; }
W_RT void Clo_show(WBuf *b, WClo *c, bool top);

/* `dyn I`: a boxed value with a vtable generated per (type, interface):
   drop, share and show for the value, then one entry per method. */
typedef struct WDyn WDyn;
typedef struct { void (*drop)(WDyn *); void (*share)(WDyn *); void (*show)(WBuf *, WDyn *, bool); } WVtHead;
typedef struct { WVtHead h; void *m[]; } WVt;
struct WDyn { int64_t rc; const WVt *vt; };
static inline WDyn *Dyn_dup(WDyn *d) { w_rc_inc(&d->rc); return d; }
W_RT void Dyn_drop(WDyn *d);
W_RT void Dyn_share(WDyn *d);
static inline bool Dyn_eq(WDyn *a, WDyn *b) { return a == b; }
W_RT void Dyn_show(WBuf *b, WDyn *d, bool top);
W_RT void w_dyn_kids(void *o);

/* ---------- capabilities ---------- */

enum { CAP_ROOT, CAP_NET, CAP_FS, CAP_STDOUT, CAP_CLOCK, CAP_LISTEN, CAP_LISTENER, CAP_CONN, CAP_RAND, CAP_STDIN, CAP_ENV, CAP_EXEC, CAP_FOREIGN, CAP_STDERR, CAP_REVOKER };
typedef struct WCap WCap;
/* A revocation switch (`x.revocable()`): once `off`, every capability made
   from it, narrowed from those or obtained through them, is withdrawn.
   `parent` is the switch of the capability it was made from, if any. */
typedef struct WRevoke { int64_t rc; int off; struct WRevoke *parent; } WRevoke;
struct WCap {
  int64_t rc; int kind;
  char **hosts; int nhosts;   /* Net: allowlist entries */
  char *root; bool ro;        /* Fs: realpath of root, NULL = not granted */
  int fd; int port;           /* Listener, Conn: the socket */
  int64_t inflight; int closing; /* operations in progress; close requested */
  int reading, writing;          /* a task is in a read (or accept) / a write that has had to wait */
  void *tls;                     /* Listener: its SSL_CTX; Conn: its VTls (NULL for plain TCP) */
  WRevoke *rv;                   /* the switch that can withdraw it (Revoker: its own), or NULL */
  WCap *rnext, *rprev;           /* Listener, Conn with a switch: in w_revocable_socks, closed at revoke */
};
W_RT WCap *cap_new(int kind);
W_RT void w_sock_release_fd(int fd);
W_RT void w_tls_release(WCap *c);
static inline WCap *Cap_dup(WCap *c) { w_rc_inc(&c->rc); return c; }
static inline void Cap_share(WCap *c) { w_rc_share(&c->rc); }
W_RT void w_rv_release(WCap *c);
W_RT void Cap_release(WCap *c);
W_RT void Cap_drop(WCap *c);
static inline bool Cap_eq(WCap *a, WCap *b) { return a == b; }
W_RT void Cap_show(WBuf *b, WCap *c, bool top);

/* ---------- revocation ----------
   `x.revocable()` gives a copy of x tied to a new switch (whose parent is
   x's, so revoking x's withdraws it too) and a Revoker holding the switch.
   Narrowing keeps the switch, and so do the Listeners and Conns obtained
   through it; those sit in w_revocable_socks, and revoking closes the ones
   under it as close() does, waking the tasks that wait on them. */
extern pthread_mutex_t w_rv_lock;
extern WCap *w_revocable_socks;
W_RT bool w_rv_off(WRevoke *r);
W_RT bool w_revoked(WCap *c);
W_RT bool w_rv_under(WRevoke *r, WRevoke *top);
W_RT void w_rv_drop(WRevoke *r);
W_RT void Cap_sock_close(WCap *c);
W_RT void w_rv_release(WCap *c);
W_RT void w_rv_inherit(WCap *to, WCap *from);
W_RT WCap *Cap_copy_kind(WCap *x);
W_RT WCap *Cap_revocable(WCap *x, WCap **revoker);
W_RT bool Cap_is_revoked(WCap *k);
W_RT void Cap_revoke(WCap *k);

extern char **g_hosts;
extern int g_nhosts;
extern char *g_fs_root;
extern bool g_fs_ro;
extern char **g_listen;
extern int g_nlisten;
extern char **g_env;
extern int g_nenv;
extern char **g_exec;
extern int g_nexec;
extern char **g_foreign;
extern int g_nforeign;
extern bool g_no_sandbox;
extern char **g_args;
extern int g_nargs;
extern uint64_t g_rand_state;
extern bool g_rand_seeded;
W_RT char *w_exec_entry(const char *g);
W_RT void w_usage(const char *m);
W_RT void w_init(int argc, char **argv);
W_RT WCap *w_root(void);
W_RT WCap *Cap_root_field(WCap *root, const char *f);

/* Rand. Without --seed every number comes from the operating system's
   cryptographically secure generator (arc4random_buf on macOS, getrandom on
   Linux, /dev/urandom where that is missing), drawn into a buffer per
   thread so a number costs a copy, not a system call; `int` rejects the
   draws that would make some values likelier than others. With --seed they
   come from splitmix64, the interpreter's sequence too: its state only ever
   advances by a constant, so an atomic add keeps it correct across
   threads. */
#if defined(__linux__)
#include <sys/random.h>
#endif
W_RT uint64_t w_rand_next(void);
W_RT void w_os_random(unsigned char *p, int64_t n, const char *where);
#define W_RAND_BUF 4096
extern __thread unsigned char w_rand_buf[W_RAND_BUF];
extern __thread int w_rand_left;
W_RT uint64_t w_rand_u64(const char *where);
W_RT int64_t Cap_rand_int(int64_t lo, int64_t hi, const char *where);
W_RT double Cap_rand_float(void);
W_RT WStr *Cap_rand_bytes(int64_t n, const char *where);
W_RT L_Str *Cap_args(void);
W_RT WStr *Cap_env_get(WCap *env, WStr *name, bool *ok);

/* I/O results: kind 0 = ok, 1 = denied, 2 = failed. */
typedef struct { int kind; WStr *val; WStr *msg; void *ptr; } WIo;
W_RT WIo io_ok(WStr *v);
W_RT WIo io_ok_ptr(void *p);
W_RT WIo io_err(int kind, const char *m);
W_RT void lower_ascii(char *s);
W_RT void parse_entry(const char *e, char *host, size_t hn, int *port);
W_RT bool net_permits(WCap *n, const char *host, int port);
W_RT WCap *Cap_net_only(WCap *n, WStr *req_s);
W_RT int parse_url(const char *u, char *host, size_t hn, int *port, int *is_https, const char **path, char *err, size_t en);
W_RT WStr *read_fd(int fd);

/* Fs: every operation walks its path with the functions in
   limen_sandbox.h (w_fs_at and the ones after it), which the interpreter
   uses too, from the granted directory g_fs_root down to the capability's
   root and on to the file, through directory descriptors. */
#define W_FS_NOT_GRANTED "fs: not granted (run with --grant fs=DIR)"
#define W_FS_TOP (g_fs_root ? g_fs_root : "/")
W_RT WCap *Cap_fs_under(WCap *fs, WStr *rel);
W_RT WCap *Cap_fs_read_only(WCap *fs);
W_RT bool Cap_fs_is_read_only(WCap *fs);
W_RT WIo Cap_fs_read(WCap *fs, WStr *rel);
W_RT WIo w_fs_put(WCap *fs, WStr *rel, WStr *data, int mode);
W_RT WIo Cap_fs_write(WCap *fs, WStr *rel, WStr *data);
W_RT WIo Cap_fs_append(WCap *fs, WStr *rel, WStr *data);
W_RT WIo Cap_fs_full_path(WCap *fs, WStr *rel);
W_RT bool Cap_fs_exists(WCap *fs, WStr *rel);
W_RT int w_cmp_cstr(const void *a, const void *b);
W_RT WIo Cap_fs_list(WCap *fs, WStr *rel);
W_RT bool Cap_fs_is_dir(WCap *fs, WStr *rel);
W_RT WIo Cap_fs_remove(WCap *fs, WStr *rel);
W_RT WIo Cap_fs_make_dir(WCap *fs, WStr *rel);
W_RT WIo Cap_fs_rename(WCap *fs, WStr *from, WStr *to);
/* Fs.info: size, modification time in milliseconds and kind, following
   links. */
typedef struct { int64_t size; int64_t mtime_ms; bool is_file; bool is_dir; } WFileInfo;
W_RT WIo Cap_fs_info(WCap *fs, WStr *rel);
/* Virtual time, for a test that takes `clock: virtual Clock`: the clock
   starts at 2026-01-01T00:00:00Z and moves only when every task is blocked
   (see w_tm_advance). w_vnow_ns is how far it has moved. */
#define W_VIRTUAL_START_MS ((int64_t)1767225600000)
extern int w_virtual;
extern int64_t w_vnow_ns;
W_RT void w_virtual_start(void);
W_RT int64_t Cap_now_ms(void);

/* ---------- fibers and the work-stealing scheduler ----------
   Every Limen task, and main itself, runs as a fiber: a small mmap'd stack
   plus saved registers. Worker threads (one per core, LIMEN_THREADS to
   override) each keep a local run queue; an idle worker steals from the
   others. A fiber that must wait (for a group, a channel or a socket)
   parks: it switches back to its worker's scheduler, which then runs the
   park's "after" action (unlock a mutex, register a socket). Doing that
   after the switch means a waker can never resume a fiber that is still
   running. */

/* GCC before 14 has no __has_feature; it says so with __SANITIZE_*__. Without
   this, a gcc build never told the sanitizers about fiber switches: TSan's
   shadow stack underflowed and it faulted in its own code, then deadlocked
   in our fault handler's sigaction (the CI hang on arm64 Linux). */
#if defined(__SANITIZE_ADDRESS__)
#define W_ASAN 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define W_ASAN 1
#endif
#endif
#if defined(__SANITIZE_THREAD__)
#define W_TSAN 1
#elif defined(__has_feature)
#if __has_feature(thread_sanitizer)
#define W_TSAN 1
#endif
#endif
#ifdef W_ASAN
#include <sanitizer/common_interface_defs.h>
#endif
#ifdef W_TSAN
#include <sanitizer/tsan_interface.h>
#endif
#include <sys/mman.h>
#include <sched.h>

typedef struct WFiber WFiber;
typedef struct WOut WOut;
/* A group body under its deadline (`group g until ...`): the deadline stops
   the body at its next wait, as it does the group's tasks. stop points at
   the group's cancelled flag. */
typedef struct WScope { struct WScope *prev; int *stop; struct WGroup *g; } WScope;
struct WFiber {
  void *sp;
  char *map; size_t map_size;          /* mmap'd stack, NULL for a worker's own context */
  void (*entry)(void *); void *arg;
  WFiber *next;
  int park_state;                       /* W_RUNNING, W_PARKING, W_PARKED or W_NOTIFIED */
  WCatch *catch_top;                    /* innermost panic boundary */
  int cancelled;                        /* its group cancelled it: stop at the next wait */
  int wlock;                            /* guards wait */
  struct WSel *wait;                    /* the token of the wait it is parked in, if cancellable */
  WScope *scopes;                       /* deadline scopes it is in, innermost first (under wlock) */
  const char *in_cb;                    /* in a callback from C: where the callback is declared */
  int unwinding;                        /* its cancellation is unwinding it: checkpoints let defers finish */
  int32_t prof;                         /* `--profile`: the function it was in when it switched out */
  int in_update;                        /* running the function of a Shared's update: it must not wait */
  const char *upd_where;                /* where that update was called */
  WOut *out;                            /* where its output goes (NULL: stdout); see Cap_print */
  struct WCap *cb_foreign;              /* in a foreign call: its Foreign, for callbacks that take one */
#ifdef W_ASAN
  const void *asan_bottom; size_t asan_size;
#endif
#ifdef W_TSAN
  void *tsan;
#endif
};
enum { W_RUNNING = 0, W_PARKING = 1, W_PARKED = 2, W_NOTIFIED = 3 };

void w_ctx_switch(void **save, void *to);
void w_ctx_tramp(void);
void w_fiber_main(WFiber *f);

#if defined(__aarch64__) || defined(__arm64__)
#ifdef __APPLE__
#define W_ASM_SYM(s) "_" #s
#else
#define W_ASM_SYM(s) #s
#endif
#define W_FRAME_WORDS 20
W_RT void w_frame_init(void **frame, WFiber *f);
#elif defined(__x86_64__)
#ifdef __APPLE__
#define W_ASM_SYM(s) "_" #s
#else
#define W_ASM_SYM(s) #s
#endif
/* six saved registers and the return address. Odd on purpose: after
   w_ctx_switch's ret into w_ctx_tramp, %rsp must be 16-byte aligned, as
   before any call, so w_fiber_main starts with the stack the ABI promises
   (with 8 it was 8 bytes off, and aligned SSE stores on the stack faulted) */
#define W_FRAME_WORDS 7
W_RT void w_frame_init(void **frame, WFiber *f);
#else
#error "Limen fibers need arm64 or x86_64"
#endif

typedef struct WWorker {
  int id;
  WFiber sched;                         /* this thread's own context */
  WFiber *current;
  void (*after)(void *); void *after_arg;
  pthread_mutex_t mu;                   /* local run queue: owner pops the tail, thieves take the head */
  WFiber **q; int64_t head, tail, cap;
} WWorker;

#define W_MAX_WORKERS 256
extern WWorker *w_workers[W_MAX_WORKERS];
extern int w_nworkers;
extern __thread WWorker *w_self_tls;
extern pthread_mutex_t w_global_mu;  /* global queue, for wakeups from non-workers */
extern WFiber *w_global_head, *w_global_tail;
extern int64_t w_queued;  /* runnable fibers in all queues */
extern pthread_mutex_t w_idle_mu;
extern pthread_cond_t w_idle_cv;
extern int64_t w_nidle;
extern int64_t w_io_waiters;  /* fibers parked on sockets */
extern int64_t w_tm_count;  /* pending timers (sleeps, timeouts, deadlines) */
extern int w_main_done;
W_RT int w_tm_advance(void);
extern size_t w_stack_size;  /* reserved lazily, so 10k tasks are cheap */
#define W_GUARD_BYTES (64 * 1024)                                 /* larger than any runtime stack frame */
extern int64_t w_fibers_alive;  /* created and not yet freed */
__attribute__((noinline)) W_RT WWorker *w_self(void);
W_RT void w_switch(WFiber *from, WFiber *to, int from_exiting);
W_RT WFiber *w_fiber_new(void (*entry)(void *), void *arg, size_t size);
W_RT void w_fiber_free(void *p);
W_RT void w_wake_one(void);
W_RT void w_ready(WFiber *f);
W_RT WFiber *w_take(WWorker *w, int own);
W_RT WFiber *w_find_work(WWorker *w);
W_RT void w_sched_loop(WWorker *w, int is_main);

/* ---- stack overflow ----
   Every fiber's stack has a guard region at its low end (w_fiber_new), so
   running past the stack faults there. A handler for SIGSEGV and SIGBUS, on
   an alternate signal stack of each worker thread (the fiber's own stack
   is the one that ran out), recognises a fault in the current fiber's guard
   region and ends the program as an unhandled panic does: `panic: stack
   overflow` on stderr and exit status 1. A stack overflow cannot be caught.
   Any other fault goes to the handler that was there before (the default,
   or a sanitizer's). */
extern WFiber *w_main_fiber;
extern struct sigaction w_old_segv, w_old_bus;
W_RT void w_sig_puts(const char *m);
W_RT void w_sig_putu(uint64_t v);
W_RT void w_stack_fault(int sig, siginfo_t *si, void *uc);
#define W_ALTSTACK_BYTES (64 * 1024)
W_RT void w_altstack(void);
W_RT void w_stack_fault_install(void);
W_RT WWorker *w_worker_new(void);
W_RT void *w_worker_thread(void *arg);

extern pthread_once_t w_pool_once;
W_RT void w_pool_start(void);

/* Park the current fiber; [after] runs on the scheduler once it is off its stack. */
#define W_CB_WAIT "a callback from C cannot wait (for a task, a channel, a timer or input)"
W_RT void w_park(void (*after)(void *), void *arg);
W_RT WFiber *w_current(void);
__attribute__((noinline)) W_RT WCatch **w_catch_slot(void);
W_RT void w_mark_unwinding(void);
W_RT void w_prepare_park(void);
W_RT void w_park_commit(void *p);
W_RT void w_park_now(void);
W_RT void w_wake(WFiber *f);
void w_fiber_main(WFiber *f);
W_RT void w_run(void (*entry)(void *), void *arg);
W_RT void w_main_finished(void);

/* ---------- timers ----------
   Sleeps, `select` timeouts and group deadlines. A timer thread keeps a
   min-heap of deadlines on the monotonic clock and sleeps until the
   earliest; a sleeping task parks its fiber, so no worker thread waits.

   A timer either wakes a fiber or calls a function. Waking goes through a
   WSel token that every waker of that wait must claim first (done 0 -> 1),
   so a fiber is woken once even when a timer races a channel. A function
   runs on the timer thread with the timer lock held; removing the timer
   under that lock is therefore enough to know it will not run. */

typedef struct WSel { int done; int fired; } WSel;
#define W_CANCEL (-2)                   /* fired: the task was cancelled */

/* ---- cancellation ----
   A group cancels its tasks at its deadline. A cancelled task stops at its
   next wait: one it is parked in is woken at once, a later one does not
   park; either way the task unwinds as for a panic, freeing what it owns.
   Every cancellable wait publishes its token in the fiber (under wlock),
   so the canceller can claim it like any other waker. Waits that clean up
   after a cancellation (joining a group being unwound) are not cancellable. */
static inline void w_flock(WFiber *f) { while (__atomic_exchange_n(&f->wlock, 1, __ATOMIC_ACQUIRE)) sched_yield(); }
static inline void w_funlock(WFiber *f) { __atomic_store_n(&f->wlock, 0, __ATOMIC_RELEASE); }
W_RT int w_scopes_stopped(WScope *sc);
W_RT int w_fiber_stopped(WFiber *f);
W_RT int w_is_cancelled(void);
W_RT int w_wait_begin(WSel *tok);
W_RT void w_update_waits(void);
W_RT void w_wait_end(void);
W_RT void w_interrupt_fiber(WFiber *f, int task);
W_RT void w_cancel_fiber(WFiber *f);

/* ---- checkpoints ----
   A cancelled task also stops where it does not wait: generated code checks
   at the top of every loop iteration and on entry to every function. The
   check is one relaxed load of w_stopping, the number of groups that are
   cancelling (their deadline passed, or the code running them is being
   abandoned) and have not been joined yet; only while it is not zero does a
   check look at the current fiber. So code outside any cancelled group, the
   main task's included, pays a load and a branch the CPU predicts. A
   callback from C never stops at a checkpoint (nothing unwinds through C's
   frames); it stops at the first checkpoint after the foreign call. Once a
   cancellation is unwinding the fiber, checkpoints let the deferred code it
   runs finish (a wait in it still stops it); a group body's own deadline,
   once landed, makes them look again (w_scope_land). */
extern int64_t w_stopping;
__attribute__((noinline, cold)) W_RT void w_checkpoint(void);
#define W_CHECK() do { if (__builtin_expect(__atomic_load_n(&w_stopping, __ATOMIC_RELAXED) != 0, 0)) w_checkpoint(); } while (0)
static inline int w_tok_claim(WSel *tok, int idx) {
  int zero = 0;
  if (!__atomic_compare_exchange_n(&tok->done, &zero, 1, 0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) return 0;
  __atomic_store_n(&tok->fired, idx, __ATOMIC_RELEASE);
  return 1;
}
static inline int w_tok_fired(WSel *tok) { return __atomic_load_n(&tok->fired, __ATOMIC_ACQUIRE); }

typedef struct WTimer {
  int64_t at;                           /* monotonic deadline, ns */
  int64_t seq;                          /* orders timers due at the same time: the one set first */
  int64_t pos;                          /* index in the heap, -1 when not in it */
  WFiber *f; WSel *sel; int idx;        /* claim sel (fired = idx), then wake f */
  void (*fire)(void *); void *arg;      /* or call fire(arg) */
} WTimer;

extern pthread_mutex_t w_tm_mu;
extern pthread_cond_t w_tm_cv;
extern WTimer **w_tm_heap;
extern int64_t w_tm_n, w_tm_cap, w_tm_seq;
extern pthread_once_t w_tm_once;
W_RT int64_t w_mono_ns(void);
W_RT int64_t w_deadline_ms(int64_t ms);
W_RT void w_tm_swap(int64_t i, int64_t j);
W_RT int w_tm_before(WTimer *a, WTimer *b);
W_RT void w_tm_up(int64_t i);
W_RT void w_tm_down(int64_t i);
W_RT void w_tm_unlink(WTimer *t);
W_RT void *w_timer_thread(void *arg);
W_RT void w_tm_start(void);
W_RT void w_tm_add(WTimer *t);
W_RT int w_tm_advance(void);
W_RT void w_tm_remove(WTimer *t);
W_RT int w_nap(int64_t ms);
W_RT void Cap_sleep(int64_t ms);

/* ---------- group / go ---------- */

typedef struct WGroup WGroup;
typedef struct WTask WTask;
/* The output of a task of an ordered group, held until the tasks started
   before it have finished and the group's body has ended. Under w_out_mu. */
struct WOut {
  WGroup *g;                            /* the group whose task writes here */
  WOut *next;                           /* g's next task not released yet, in start order */
  char *buf; size_t len, cap;           /* what it holds */
  int done;                             /* the task has finished */
};
struct WTask {
  WClo *fn; void (*run)(WTask *); WGroup *g; char *pmsg; char *pwhere;
  WFiber *f;                            /* its fiber while it runs (under g->mu) */
  int done, cancelled;                  /* finished; ended by cancellation (no result) */
  bool keep;                            /* its result goes in the group's list (not a () task) */
  bool linked;                          /* its output is in the group's start-order list */
  int64_t idx;                          /* its place in g->tasks (under g->mu) */
  WOut out;                             /* its output, in an ordered group */
  max_align_t slot[];
};
struct WGroup {
  int64_t rc; pthread_mutex_t mu; int64_t pending;
  WFiber *waiter; WSel *wtok;           /* the task joining it, and its wait token */
  int64_t n, cap; WTask **tasks;        /* in spawn order; grown under mu. A finished task
                                           nobody needs any more (no result, no panic, no held
                                           output) is freed at once and leaves NULL here, so a
                                           group that runs for ever (a server) stays small */
  int64_t holes;                        /* how many of tasks are NULL */
  int cancelled;                        /* its deadline passed: tasks started now never run */
  int stopping;                         /* 1: counted in w_stopping; 2: counted and uncounted (under mu) */
  WFiber *body;                         /* the fiber running its body under the deadline, if any */
  WTimer deadline;                      /* `group g until clock.after(ms)` (pos -1 when none) */
  /* output, under w_out_mu: */
  WOut *parent;                         /* where the body's output goes (NULL: stdout) */
  int ordered;                          /* tasks' output in start order: not `interleaved`, not given up */
  int body_done;                        /* the body has ended, so the first task writes through */
  WOut *first, *last;                   /* the tasks not released yet, in start order */
  WGroup *live_next, *live_prev; int live;   /* in w_live_groups */
};

/* ---------- output in start order ----------
   Output that tasks of an ordered group print is held in their WOut until
   the group's body has ended and every task started before them has
   finished; the first unfinished task then writes through to the group's
   parent. So a group's output reads as if its body ran first and then each
   task, one after another, in start order. Held output is limited to
   W_HOLD_LIMIT bytes for the whole program: past it, the group that would
   hold more gives up order, releasing what it holds in start order. Every
   write happens under w_out_mu, so a print is never torn. The interpreter
   does the same (interp.ml, "output in start order"). */
#define W_HOLD_LIMIT ((size_t)4 << 20)
extern pthread_mutex_t w_out_mu;
extern __thread int w_out_locked;  /* this thread holds w_out_mu (never across a switch) */
extern size_t w_held;  /* bytes held in every WOut */
extern WGroup *w_live_groups;  /* ordered groups still running, newest (innermost) first */
W_RT void w_out_lock(void);
W_RT void w_out_unlock(void);
W_RT WOut *w_cur_out(void);
W_RT void w_live_remove(WGroup *g);
W_RT int w_out_through(WOut *o);
W_RT void w_out_spill(WGroup *g);
W_RT void w_out_put(WOut *o, const char *p, size_t n);
W_RT void w_out_release(WOut *o);
W_RT void w_out_spill(WGroup *g);
W_RT void w_out_advance(WGroup *g);
W_RT void w_out_release_all(void);
W_RT void Cap_print(WStr *s, bool nl);
W_RT void Cap_eprint(WStr *s, bool nl);
W_RT void w_out_unhold(void);

W_RT void w_group_cancel(WGroup *g);
W_RT void w_group_deadline(void *p);
W_RT WGroup *w_group_new(int ordered);
W_RT WGroup *w_group_new_until(int64_t ms, int ordered);
W_RT void w_group_cancel(WGroup *g);
W_RT void w_group_cleanup(WGroup *g);
W_RT void w_group_release_tasks(WGroup *g);
W_RT void w_scope_push(WScope *sc, WGroup *g);
W_RT void w_scope_pop(WScope *sc);
W_RT void w_scope_land(WCatch *c, WScope *sc);
W_RT void w_task_entry(void *p);
W_RT void w_spawn(WGroup *g, WClo *fn, void (*run)(WTask *), size_t slot, bool keep);
W_RT void w_group_wait(WGroup *g, int cancellable);
W_RT void w_group_cleanup(WGroup *g);
W_RT void w_group_release_tasks(WGroup *g);
W_RT void w_group_reraise(WGroup *g);
static inline WGroup *Group_dup(WGroup *g) { g->rc++; return g; }
W_RT void Group_drop(WGroup *g);
static inline void Group_share(WGroup *g) { (void)g; }

/* ---------- channels ----------
   A bounded queue between tasks. send blocks while the channel is full,
   recv blocks while it is empty; after close, recv drains what is left and
   then returns nothing, and send panics. Values are switched to shared
   before they enter, because the receiver may be on another thread.
   With capacity 0 there is no buffer: a sender waits until a receiver takes
   its value from it (or a waiting receiver is handed it), so send and recv
   meet. */

/* A waiting task. A `select` puts one on every channel it waits on, all
   pointing to one WSel; the first channel to act claims it (done 0 -> 1) and
   the others then skip those waiters as stale. */
typedef struct WChWait { WFiber *f; void *val; int ok; struct WChWait *next; WSel *sel; int idx; } WChWait;
typedef struct WChan {
  int64_t rc; pthread_mutex_t mu;
  int64_t cap, len, head; size_t esz; char *buf; int closed;
  WChWait *sq, *sq_t, *rq, *rq_t;                       /* waiting senders and receivers */
  void (*edrop)(void *); void (*eshare)(void *);
} WChan;
W_RT WChan *w_chan_new(int64_t cap, size_t esz, void (*edrop)(void *), void (*eshare)(void *), const char *where);
static inline WChan *Chan_dup(WChan *c) { w_rc_inc(&c->rc); return c; }
W_RT void Chan_drop(WChan *c);
static inline void Chan_share(WChan *c) { w_rc_share(&c->rc); }
static inline bool Chan_eq(WChan *a, WChan *b) { return a == b; }
W_RT void Chan_show(WBuf *b, WChan *c, bool top);
W_RT void w_chq_push(WChWait **h, WChWait **t, WChWait *w);
W_RT WChWait *w_chq_pop(WChWait **h, WChWait **t);
W_RT WChWait *w_chq_pop_live(WChWait **h, WChWait **t);
W_RT void w_chq_remove(WChWait **h, WChWait **t, WChWait *w);
W_RT void w_chan_send(WChan *c, void *v, const char *where);
W_RT bool w_chan_recv(WChan *c, void *out);
W_RT void w_chan_close(WChan *c, const char *where);

/* ---------- releasing what a panic abandoned ---------- */


#define W_DYING (INT64_MAX / 4)
W_RT void w_catch_release(WCatch *c);

/* ---------- select ----------
   ops[i] is a receive (send 0; val: where to store the value), a send
   (send 1; val: the value) or a timeout (send 2, no channel; ready ms
   milliseconds after the select starts). Returns the index of the
   operation done, with ops[i].ok set (a receive: 1 if a value came, 0 if
   the channel is closed and empty), or -1 when nothing was ready and the
   select has a `_` arm. The first ready operation in order wins. */
enum { W_SEL_RECV = 0, W_SEL_SEND = 1, W_SEL_AFTER = 2 };
typedef struct WSelOp { WChan *c; int send; void *val; int ok; int on; int64_t ms; } WSelOp;
W_RT int w_sel_try(WSelOp *op, WFiber **wake);
W_RT int w_chan_cmp(const void *a, const void *b);
W_RT int w_sel_lock(WSelOp *ops, int n, WChan **cs);
W_RT void w_sel_unlock(WChan **cs, int k);
W_RT void w_sel_drop_sends(WSelOp *ops, int n);
W_RT int w_select(WSelOp *ops, int n, int has_default, const char *where);

/* ---------- sockets ----------
   Sockets are non-blocking. A task that would block parks on the poller
   thread (kqueue on macOS and the BSDs, epoll on Linux), which wakes it
   when the socket is ready. One task at a time may wait on a given socket
   in a given direction. */

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#include <sys/event.h>
#define W_KQUEUE 1
#else
#include <sys/epoll.h>
#endif

extern int w_poll_fd;
extern pthread_once_t w_poll_once;

/* Per-descriptor waiters: one reader slot and one writer slot. Each
   descriptor is registered once, edge-triggered, for both directions; an
   event with nobody waiting sets a ready flag, so a task that registers
   just after the event still sees it. */
#define W_MAX_FD 65536
typedef struct { WFiber *r, *w; WSel *rt, *wt; uint8_t rready, wready, registered; } WFdState;
extern WFdState w_fds[W_MAX_FD];
#define W_FD_LOCKS 64
extern pthread_mutex_t w_fd_mu[W_FD_LOCKS];
W_RT pthread_mutex_t *w_fd_lock(int fd);
W_RT void w_fd_event(int fd, int readable, int writable);
W_RT void *w_poller(void *arg);
W_RT void w_poll_start(void);
W_RT int w_wait_fd(int fd, int write);
W_RT int w_wait_fd_until(int fd, int write, int64_t at);
W_RT void w_fd_wake_all(int fd);
W_RT void w_sock_release_fd(int fd);
W_RT int w_sock_enter(WCap *c);
W_RT void w_sock_really_close(WCap *c);
W_RT void w_sock_leave(WCap *c);
W_RT int w_sock_closing(WCap *c);
W_RT void w_tls_close_notify(WCap *c);
W_RT void Cap_sock_close(WCap *c);
W_RT void w_nonblock(int fd);
W_RT int w_parse_addr(WStr *a, char *host, size_t hn, int *port);
W_RT int w_resolve(const char *h, struct in_addr *out);
/* Name lookups for outgoing connections, IPv4 and IPv6. getaddrinfo
   blocks, so a lookup runs on a thread of its own and the task parks on a
   pipe the thread writes to when it is done. A cancelled task leaves at
   once; the thread then frees everything itself when the lookup returns. */
typedef struct {
  char host[256]; struct addrinfo *res; int rc; int fds[2];
  int state;   /* 0 looking up, 1 done, 2 abandoned by its task */
  pthread_mutex_t mu;
} WDns;
W_RT void *w_dns_thread(void *p);
W_RT struct addrinfo *w_lookup(const char *h, int port, char *err, size_t en);
W_RT WIo w_errno_io(void);
W_RT WIo Cap_listen_on(WCap *l, WStr *addr);
W_RT int w_sock_claim(int *slot, int *held);
W_RT void w_sock_unclaim(int *slot, int held);

W_RT int w_tls_accept(WCap *l, WCap *conn, WIo *err);
W_RT WIo Cap_accept(WCap *l);
W_RT int w_connect_one(struct addrinfo *a, char *err, size_t en);
W_RT WIo Cap_net_connect(WCap *n, WStr *addr);
/* One read or write on a connection, plain or TLS: the bytes moved (0 at
   the end of a read stream), or -1 if the socket must first be ready in
   direction *dir (0 read, 1 write; TLS may need to write while reading and
   the other way round), or -2 with a message in err. */
W_RT int w_tls_step(WCap *c, int wr, char *buf, size_t n, int *dir, char *err, size_t en);
W_RT ssize_t w_conn_step(WCap *c, int fd, int wr, char *buf, size_t n, int *dir, char *err, size_t en);
W_RT int w_conn_wait(int fd, int own, int dir);
W_RT int w_conn_wait_until(int fd, int own, int dir, int64_t at);
W_RT WIo Cap_conn_read(WCap *c);
W_RT WIo Cap_conn_write(WCap *c, WStr *s);
W_RT WIo Cap_conn_read_until(WCap *c, int64_t at, int64_t ms);
W_RT WIo Cap_conn_write_until(WCap *c, WStr *s, int64_t at, int64_t ms);
W_RT WIo Cap_conn_read_within(WCap *c, WCap *clock, int64_t ms);
W_RT WIo Cap_conn_write_within(WCap *c, WCap *clock, int64_t ms, WStr *s);

/* ---------- TLS ----------
   On the core in limen_tls.h, pasted ahead of this header (with LIMEN_TLS
   defined) when a program can use TLS and OpenSSL was found. A TLS Conn is
   a plain Conn whose reads and writes go through its session; waits park
   the task on the poller exactly as for plain sockets. A server session
   does its handshake inside the first read or write, so accept() never
   waits for a slow client. */
W_RT void w_tls_release(WCap *c);
W_RT void w_tls_close_notify(WCap *c);
W_RT int w_tls_step(WCap *c, int wr, char *buf, size_t n, int *dir, char *err, size_t en);
W_RT int w_tls_accept(WCap *l, WCap *conn, WIo *r);
W_RT WIo Cap_net_connect_tls(WCap *n, WStr *addr);
W_RT WIo Cap_conn_start_tls(WCap *c, WStr *host);
W_RT WIo Cap_fs_read_at(WCap *fs, WStr *rel, int64_t off, int64_t n);
W_RT WStr *w_crypto(int64_t op, WStr *a, WStr *b, WStr *c, WStr *d);
/* `--profile`: a sampling CPU profile. Every function counts its calls and
   says it is the thread's current one (W_PROF, at its start, restored as it
   leaves); a timer samples the current function every millisecond of CPU,
   and the report goes to stderr at exit. */
W_RT int32_t w_prof_enter(int32_t id);
W_RT void w_prof_leave(int32_t *prev);
W_RT void w_prof_start(const char *const *names, int64_t *calls, int64_t *samples, int32_t n);
#define W_PROF(id) int32_t w_prof_prev_ __attribute__((cleanup(w_prof_leave))) = w_prof_enter(id)
W_RT WIo Cap_listen_on_tls(WCap *l, WStr *addr, WStr *cert, WStr *key);

/* ---------- stdin ----------
   Read through the poller like a socket: a task waiting for input parks,
   and no worker thread blocks. Stdin keeps its blocking mode (it is shared
   with the parent shell), so a task reads only once poll() says input is
   there. A regular file never makes a reader wait, so it is read directly,
   and so is anything the poller cannot watch. One task reads at a time,
   each getting whole lines: the others queue on w_in_lock, a channel
   holding one token. */
#include <poll.h>
extern WChan *w_in_lock;
extern char *w_in_buf;
extern int64_t w_in_len, w_in_cap;
extern int w_in_eof, w_in_direct;
extern pthread_once_t w_in_once;
W_RT void w_in_init(void);
W_RT void w_in_acquire(void);
W_RT void w_in_release(void);
W_RT int w_in_fill(void);
W_RT void w_in_consume(int64_t n);
W_RT WStr *Cap_stdin_line(bool *ok);
W_RT WStr *Cap_stdin_all(void);

/* ---------- Foreign: calls into C libraries ----------
   The allowlist holds library names as granted (`sqlite3`) or `*`. Every
   call of a foreign function checks that its library is on the list of the
   Foreign value it was given; the call itself goes through the function's
   exact prototype (see cgen), to a symbol the linker resolved from -l<lib>. */
#define W_STR2_(x) #x
#define W_STR_(x) W_STR2_(x)
#define W_CSYM(s) W_STR_(__USER_LABEL_PREFIX__) s
W_RT bool Cap_foreign_allows(WCap *x, WStr *lib_s);
W_RT WCap *Cap_foreign_only(WCap *x, WStr *lib_s);
W_RT void w_foreign_check(WCap *x, const char *lib, const char *where);
W_RT int32_t w_ffi_i32(int64_t v, const char *arg, const char *where);
W_RT WStr *w_ffi_str(const char *p);
#define W_KEPT 32
W_RT int64_t w_kept_slot(WChan *c, const char *where);
W_RT void w_kept_send(int64_t i, void *v);
W_RT void w_kept_release(void);
/* ---------- blocking C calls ----------
   A foreign function declared `blocking fn` (and each statement of the std
   sql module, w_sql_run) runs on a thread of the blocking-call pool,
   not on the worker thread that runs the task: the task parks until the
   call returns, so a slow C call holds up only its own task. The pool
   starts a thread whenever a call finds none idle, up to
   LIMEN_BLOCKING_THREADS (64 by default); past that, calls queue.

   Generated code (cgen.ml, "blocking calls") makes a job: a WBlock header,
   then the C arguments, the out-parameters and the result; vbj<n>_run makes
   the call with them. A Str or Bytes argument is lent to C by keeping a
   reference to it in the job (w_block_keep), so it stays alive however the
   task ends; a Str result is copied on the pool thread at once, while C's
   string is still valid. The job's variable has a cleanup (w_block_end), so
   it is freed however the task leaves it, a panic in an argument included.
   A cancelled task stops waiting and unwinds at once; the call goes on in
   the background and the pool thread frees the job when it returns,
   dropping what it kept and the result (a Handle the call returned is
   lost, as C memory nobody frees). A stoppable job (an SQL statement) is
   told to stop instead and waited for, so it never outlives the task. Once a cancellation is
   unwinding the task (deferred code), a blocking call is waited for to the
   end, as checkpoints let that code finish. */
typedef struct WBlock {
  void (*run)(struct WBlock *);
  struct WBlock *next;
  WFiber *f; WSel tok;
  int state;                            /* 0 queued or running, 1 done, 2 abandoned by its task (under w_bl_mu) */
  int pooled;                           /* the pool holds it: from submission until the task takes it back */
  int nkeep; WStr *keep[16];            /* the Str and Bytes arguments lent to C */
  WStr *res;                            /* a Str result, copied on the pool thread */
  int stoppable;                        /* the call watches `stop`: a cancelled task sets it and waits for the call to end */
  int stop;
} WBlock;
extern pthread_mutex_t w_bl_mu;
extern pthread_cond_t w_bl_cv;
extern WBlock *w_bl_head, *w_bl_tail;
extern int w_bl_threads, w_bl_idle, w_bl_spinning, w_bl_max;
/* A call that returns quickly costs a round trip between threads. So that
   it costs no sleep and wakeup on either side, a pool thread that has
   finished a call watches the queue for W_BL_SPIN_NS before it sleeps, and
   a task that has handed over a call watches for its end as long before it
   parks: a loop of short blocking calls then never waits for a thread to
   wake. */
#define W_BL_SPIN_NS 50000
static inline void w_cpu_relax(void) {
#if defined(__x86_64__)
  __builtin_ia32_pause();
#elif defined(__aarch64__) || defined(__arm64__)
  __asm__ volatile("yield");
#endif
}
W_RT int64_t w_real_ns(void);
W_RT void *w_block_new(size_t n, void (*run)(WBlock *));
W_RT const void *w_block_keep(WBlock *b, WStr *s);
W_RT void w_block_free(WBlock *b);
W_RT void w_block_end(void *p);
W_RT WStr *w_block_str(WBlock *b);
W_RT void *w_bl_thread(void *arg);
W_RT void w_bl_submit(WBlock *b);
W_RT void w_block_wait(WBlock *b);
/* The std sql module's helpers (Ffi.runtime_fns): one SQL statement as one
   blocking call (limen_sql.h), and reading what it returns. */
W_RT WStr *w_sql_run(const void *const *api, int64_t db, WStr *text, WStr *params, int64_t mode);
W_RT WStr *w_sql_open(const void *const *api, WStr *root, WStr *name, int64_t flags);
W_RT int64_t w_bytes_int(WStr *b, int64_t at, const char *where);
W_RT double w_bytes_float(WStr *b, int64_t at, const char *where);
W_RT int64_t w_float_bits(double x);
/* ---------- Atomic and Shared ----------
   The shared mutable state tasks may hold besides channels. Both are shared,
   never copied: a task that captures one works on the same one.

   Atomic: an Int read and changed with the processor's atomic operations
   (sequentially consistent), so `hits.add(1)` from many tasks at once loses
   no update.

   Shared[T]: a value behind a lock. get() copies it out (a snapshot: values
   are copy-on-write, so it is a reference count, and a later set or update
   does not change it), set(v) replaces it, update(f) runs f on it and
   stores the result, holding the lock throughout, so updates from many
   tasks apply one at a time. The value inside is always switched to shared
   (its reference counts atomic), since any task may read it.

   The lock (WLock) never blocks a worker thread for long: a task that finds
   it held spins briefly, then parks; the holder hands it straight to the
   first parked task when it lets go. The holder never waits (update's
   function must not, below), so a held lock is always about to be let go,
   and waiting for one is not cancellable and never a deadlock.

   update's function must not wait: anything that would park the task while
   it holds the lock (a channel, a timer, a group, a socket, a blocking C
   call) and any use of a Shared (this one would deadlock, another could)
   panics with "update must not wait" instead, where the update was called.
   The fiber's in_update says it holds one; w_wait_begin refuses to start a
   wait then, and w_cancel_unwind turns that refusal into the panic. A
   cancellation or a panic in the function unwinds through the update's
   cleanup (w_update_end), which lets go of the lock and leaves the value as
   it was. */
typedef struct WAtomic { int64_t rc; int64_t v; } WAtomic;
W_RT WAtomic *w_atomic_new(int64_t v);
static inline WAtomic *Atomic_dup(WAtomic *a) { w_rc_inc(&a->rc); return a; }
static inline void Atomic_drop(WAtomic *a) { if (w_rc_dec(&a->rc)) w_free(a); }
static inline void Atomic_share(WAtomic *a) { w_rc_share(&a->rc); }
static inline bool Atomic_eq(WAtomic *a, WAtomic *b) { return a == b; }
W_RT void Atomic_show(WBuf *b, WAtomic *a, bool top);
static inline int64_t w_atomic_get(WAtomic *a) { return __atomic_load_n(&a->v, __ATOMIC_SEQ_CST); }
static inline void w_atomic_set(WAtomic *a, int64_t v) { __atomic_store_n(&a->v, v, __ATOMIC_SEQ_CST); }
/* the new value; Int arithmetic wraps, as everywhere */
static inline int64_t w_atomic_add(WAtomic *a, int64_t n) {
  return (int64_t)__atomic_add_fetch((uint64_t *)&a->v, (uint64_t)n, __ATOMIC_SEQ_CST);
}
/* the old value */
static inline int64_t w_atomic_swap(WAtomic *a, int64_t v) { return __atomic_exchange_n(&a->v, v, __ATOMIC_SEQ_CST); }
static inline bool w_atomic_cas(WAtomic *a, int64_t old, int64_t v) {
  return __atomic_compare_exchange_n(&a->v, &old, v, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

/* A task waiting for a WLock, on its own stack. */
typedef struct WLockWait { WFiber *f; struct WLockWait *next; } WLockWait;
typedef struct {
  int held;                             /* 1 while a task holds it */
  int nwait;                            /* tasks parked or about to park (under mu) */
  pthread_mutex_t mu;                   /* guards the queue */
  WLockWait *head, *tail;
} WLock;
W_RT void w_lock_init(WLock *l);
W_RT void w_lock_acquire(WLock *l);
W_RT void w_lock_release(WLock *l);

typedef struct WShared {
  int64_t rc;
  WLock lk;
  void (*edrop)(void *); void (*eshare)(void *);
  max_align_t v[];                      /* the value */
} WShared;
W_RT void w_shared_kids(void *o);
W_RT WShared *w_shared_new(size_t esz, void (*edrop)(void *), void (*eshare)(void *));
static inline WShared *Shared_dup(WShared *s) { w_rc_inc(&s->rc); return s; }
W_RT void Shared_drop(WShared *s);
static inline void Shared_share(WShared *s) { w_rc_share(&s->rc); }
static inline bool Shared_eq(WShared *a, WShared *b) { return a == b; }
W_RT void Shared_show(WBuf *b, WShared *s, bool top);
W_RT void w_shared_lock(WShared *s, const char *where);
static inline void w_shared_unlock(WShared *s) { w_lock_release(&s->lk); }
W_RT WShared *w_update_begin(WShared *s, const char *where);
W_RT void w_update_end(void *p);
W_RT WFiber *w_cb_fiber(const char *where);
__attribute__((noreturn)) W_RT void w_cb_fatal(WCatch *c, const char *where);
W_RT WStr *w_ffi_copy(int64_t p, int64_t n, const char *where);
/* The handle table for trusted standard library modules (Ffi.runtime_fns):
   a Limen value holds a random id, never the C pointer, so it cannot be
   forged, and a pointer is handed back for freeing only once no use of it is
   in progress. */
typedef struct { int64_t id, ptr; int64_t uses; bool closed; } WHandleEntry;
extern WHandleEntry *w_handles;
extern int64_t w_nhandles, w_caphandles;
extern pthread_mutex_t w_handles_mu;
W_RT int64_t w_handle_random(void);
W_RT WHandleEntry *w_handle_find(int64_t id);
W_RT void w_handle_remove(WHandleEntry *e);
W_RT int64_t w_handle_new(int64_t ptr);
W_RT int64_t w_handle_acquire(int64_t id);
W_RT int64_t w_handle_release(int64_t id);
W_RT int64_t w_handle_close(int64_t id);
W_RT WStr *Cap_foreign_copy(WCap *x, int64_t p, int64_t n, const char *where);
W_RT bool exec_prog_is(const char *e, const char *prog);
W_RT bool exec_prefix_matches(const char *p, size_t pn, char **args, int64_t nargs);
W_RT bool exec_permits(WCap *x, const char *prog, char **args, int64_t nargs);
W_RT bool exec_covers(WCap *x, const char *prog, const char *q);
W_RT WCap *exec_with(WCap *x, char *entry);
W_RT WCap *Cap_exec_only(WCap *x, WStr *prog_s);
W_RT WCap *Cap_exec_only_args(WCap *x, WStr *prog_s, L_Str *qs);
typedef struct { int64_t code; WStr *out; WStr *err; } WProc;
W_RT WIo Cap_exec_run(WCap *x, WStr *prog_s, L_Str *args);

/* ---------- Net.get: plain HTTP/1.1 over a socket ----------
   The same rules as Caps.parse_http in the interpreter: status line
   `HTTP/1.x NNN ...`; header names lowercased, names and values trimmed of
   spaces and tabs, a repeated header's values joined with ", "; the body
   chunked if Transfer-Encoding mentions chunked, else Content-Length bytes
   if given, else everything until the server closes. Redirects are
   returned, not followed. https goes over Net.connect_tls. */
typedef struct { int64_t status; int n; WStr **names; WStr **values; WStr *body; } WHttp;
#define W_MAX_RESPONSE ((int64_t)64 * 1024 * 1024)
W_RT int64_t w_find_bytes(const char *d, int64_t n, const char *pat, int64_t from);
W_RT void w_trim_sp(const char *d, int64_t *a, int64_t *b);
W_RT void w_http_free(WHttp *h);
W_RT WIo w_http_fail(WHttp *h, const char *m);
W_RT WIo w_http_parse(const char *d, int64_t n, const char *cut);
W_RT WIo Cap_net_get(WCap *n, WStr *url_s);
