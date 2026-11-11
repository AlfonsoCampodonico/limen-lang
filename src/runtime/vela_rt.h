/* Vela C runtime. Included verbatim at the top of every generated program.
   Its behaviour mirrors the interpreter (src/interp.ml and src/caps.ml). */
#define _GNU_SOURCE
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
   that reference counting frees everything (VELA_LEAKCHECK=1). */
static int64_t w_live;

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
  struct _Unwind_Exception uexc;   /* in the boundary's frame, which unwinding does not touch */
} WCatch;

/* An owned local: its value, whether it still owns it, and how to drop it.
   Generated code gives every local that owns memory one of these with
   __attribute__((cleanup)), so unwinding after a panic drops it. */
typedef struct { void *v; uint8_t *o; void (*d)(void *); } WCl;
static inline void w_cl_run(WCl *c) { if (*c->o) { *c->o = 0; c->d(c->v); } }
static WCatch **w_catch_slot(void); /* the current fiber's innermost boundary, or NULL outside fibers */

static inline void w_hdr_unlink(WHdr *h) {
  if (h->prev) { h->prev->next = h->next; h->next->prev = h->prev; h->prev = h->next = NULL; }
}
static inline void w_hdr_link(WHdr *list, WHdr *h) {
  h->next = list->next; h->prev = list; list->next->prev = h; list->next = h;
}
static void *w_alloc_k(size_t n, int kind) {
  WHdr *h = (WHdr *)malloc(sizeof(WHdr) + (n ? n : 1));
  if (!h) { fputs("panic: out of memory\n", stderr); exit(1); }
  h->prev = h->next = NULL; h->kind = kind; h->kids = NULL;
  __atomic_fetch_add(&w_live, 1, __ATOMIC_RELAXED);
  if (kind != WK_BUF) {
    WCatch **slot = w_catch_slot();
    if (slot && *slot) w_hdr_link(&(*slot)->objs, h);
  }
  return h + 1;
}
static inline void w_set_kids(void *o, void (*k)(void *)) { ((WHdr *)o - 1)->kids = k; }
/* a buffer owned by an object (list data, map tables): never on a list */
static void *w_alloc(size_t n) { return w_alloc_k(n, WK_BUF); }
static void w_free(void *p) {
  if (!p) return;
  WHdr *h = (WHdr *)p - 1;
  w_hdr_unlink(h);
  __atomic_fetch_sub(&w_live, 1, __ATOMIC_RELAXED);
  free(h);
}
/* Start a boundary's list; its parent is the current innermost boundary. */
static void w_catch_init(WCatch *c) {
  c->msg = NULL; c->where = NULL; c->unwinding = 0;
  c->objs.prev = c->objs.next = &c->objs;
  WCatch **slot = w_catch_slot();
  c->prev = slot ? *slot : NULL;
}
/* Normal exit: what the boundary allocated and still lives now belongs to
   the enclosing boundary, or to nobody. */
static void w_catch_end(WCatch *c) {
  if (c->objs.next == &c->objs) return;
  if (c->prev) {
    WHdr *first = c->objs.next, *last = c->objs.prev, *p = &c->prev->objs;
    last->next = p->next; p->next->prev = last; p->next = first; first->prev = p;
  } else {
    WHdr *h = c->objs.next;
    while (h != &c->objs) { WHdr *n = h->next; h->prev = h->next = NULL; h = n; }
  }
  c->objs.prev = c->objs.next = &c->objs;
}
static void w_catch_release(WCatch *c); /* after a panic: free what the abandoned code left */

/* realloc for a buffer from w_alloc */
static void *w_brealloc(void *p, size_t n) {
  WHdr *h = (WHdr *)realloc((WHdr *)p - 1, sizeof(WHdr) + (n ? n : 1));
  if (!h) { fputs("panic: out of memory\n", stderr); exit(1); }
  return h + 1;
}

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
static void *w_raw(size_t n) {
  void *p = malloc(n ? n : 1);
  if (!p) { fputs("panic: out of memory\n", stderr); exit(1); }
  return p;
}
__attribute__((noreturn)) static void w_oom(void) { fputs("panic: out of memory\n", stderr); exit(1); }
/* count * size with overflow check */
static size_t w_bytes(int64_t count, size_t size) {
  if (count < 0 || (size && (uint64_t)count > SIZE_MAX / size)) w_oom();
  return (size_t)count * size;
}
static void *w_realloc(void *p, size_t n) {
  void *q = realloc(p, n ? n : 1);
  if (!q) w_oom();
  return q;
}
static void w_leakcheck(void) {
  if (getenv("VELA_LEAKCHECK")) fprintf(stderr, "vela-leakcheck: %lld live\n", (long long)__atomic_load_n(&w_live, __ATOMIC_ACQUIRE));
}

/* A panic boundary: `catch(|| ...)`, and every task. A panic inside one
   jumps back to it; memory and OS handles the abandoned code owned are not
   freed.

   Invariant for jumping out safely: the runtime only panics where no runtime
   structure is half-updated and no lock is held. Range, division and map-key
   checks happen before any write; channel send/close release the channel
   lock first. A failure that cannot meet this (the deadlock detector) calls
   w_fatal_c instead. Keep it that way when adding a w_panic_c call. */


/* Errors no boundary can handle: report and exit. */
__attribute__((noreturn)) static void w_fatal_c(const char *msg, const char *where) {
  fflush(stdout);
  fprintf(stderr, "panic: %s\n  --> %s\n", msg, where);
  exit(1);
}
/* Unwinding runs every frame's cleanups. The boundary's frame has a guard
   variable declared after its other locals, so its cleanup runs first
   there and jumps back into the boundary before that frame's own locals
   are touched. The stop function only catches a failed unwind. */
static _Unwind_Reason_Code w_unwind_stop(int version, _Unwind_Action actions, _Unwind_Exception_Class cls,
                                         struct _Unwind_Exception *exc, struct _Unwind_Context *ctx, void *param) {
  (void)version; (void)cls; (void)exc; (void)ctx;
  if (actions & _UA_END_OF_STACK) _longjmp(((WCatch *)param)->jb, 1);
  return _URC_NO_REASON;
}
static void w_catch_land(WCatch **g) { if ((*g)->unwinding) _longjmp((*g)->jb, 1); }
__attribute__((noreturn)) static void w_panic_c(const char *msg, const char *where) {
  WCatch **slot = w_catch_slot();
  if (slot && *slot) {
    WCatch *c = *slot;
    c->msg = strdup(msg);
    c->where = strdup(where);
    *slot = c->prev;
    /* unwind to the boundary, running the cleanups of every frame on the way,
       so owned locals are dropped; fall back to a plain jump if unwinding fails */
    c->unwinding = 1;
    memset(&c->uexc, 0, sizeof c->uexc);
    c->uexc.exception_class = 0x56454c4150414e43ULL; /* "VELAPANC" */
    _Unwind_ForcedUnwind(&c->uexc, w_unwind_stop, c);
    _longjmp(c->jb, 1);
  }
  w_fatal_c(msg, where);
}
__attribute__((noreturn)) static void w_unreachable(void) { w_panic_c("internal: unreachable", "?"); }

/* ---------- strings ---------- */

typedef struct WStr { int64_t rc; int64_t len; char *data; } WStr; /* rc == 0: immortal literal */

static WStr *Str_new(int64_t len) {
  if (len < 0 || (uint64_t)len > SIZE_MAX - sizeof(WStr) - 1) w_oom();
  WStr *s = (WStr *)w_alloc_k(sizeof(WStr) + (size_t)len + 1, WK_PLAIN);
  s->rc = 1; s->len = len; s->data = (char *)(s + 1); s->data[len] = 0;
  return s;
}
static WStr *Str_from(const char *p, int64_t len) { WStr *s = Str_new(len); memcpy(s->data, p, (size_t)len); return s; }
static inline WStr *Str_dup(WStr *s) { w_rc_inc(&s->rc); return s; }
static inline void Str_drop(WStr *s) { if (w_rc_dec(&s->rc)) w_free(s); }
static inline void Str_share(WStr *s) { w_rc_share(&s->rc); }
static inline bool Str_eq(WStr *a, WStr *b) { return a->len == b->len && memcmp(a->data, b->data, (size_t)a->len) == 0; }
static int Str_cmp(WStr *a, WStr *b) {
  int64_t n = a->len < b->len ? a->len : b->len;
  int c = memcmp(a->data, b->data, (size_t)n);
  if (c) return c < 0 ? -1 : 1;
  return a->len < b->len ? -1 : (a->len > b->len ? 1 : 0);
}
static WStr *Str_concat(WStr *a, WStr *b) {
  WStr *s = Str_new(a->len + b->len);
  memcpy(s->data, a->data, (size_t)a->len); memcpy(s->data + a->len, b->data, (size_t)b->len);
  return s;
}

/* ---------- buffers and printing ---------- */

typedef struct { char *p; int64_t len, cap; } WBuf;
static void buf_init(WBuf *b) { b->cap = 64; b->len = 0; b->p = (char *)w_raw(64); }
static void buf_putn(WBuf *b, const char *s, int64_t n) {
  if (n < 0 || b->len > INT64_MAX - n) w_oom();
  if (b->len + n > b->cap) { while (b->len + n > b->cap) { if (b->cap > INT64_MAX / 2) w_oom(); b->cap *= 2; } b->p = (char *)w_realloc(b->p, (size_t)b->cap); }
  memcpy(b->p + b->len, s, (size_t)n); b->len += n;
}
static void buf_puts(WBuf *b, const char *s) { buf_putn(b, s, (int64_t)strlen(s)); }
static WStr *buf_take(WBuf *b) { WStr *s = Str_from(b->p, b->len); free(b->p); return s; }

static void fmt_float(char *out, size_t n, double x) {
  if (x == floor(x) && fabs(x) < 1e16) snprintf(out, n, "%.1f", x);
  else snprintf(out, n, "%.15g", x);
}

/* Same escaping as OCaml's String.escaped, so output matches the interpreter. */
static void buf_escaped(WBuf *b, WStr *s) {
  char tmp[8];
  buf_puts(b, "\"");
  for (int64_t i = 0; i < s->len; i++) {
    unsigned char c = (unsigned char)s->data[i];
    switch (c) {
      case '"': buf_puts(b, "\\\""); break;
      case '\\': buf_puts(b, "\\\\"); break;
      case '\n': buf_puts(b, "\\n"); break;
      case '\t': buf_puts(b, "\\t"); break;
      case '\r': buf_puts(b, "\\r"); break;
      case '\b': buf_puts(b, "\\b"); break;
      default:
        if (c >= 32 && c <= 126) buf_putn(b, (const char *)&c, 1);
        else { snprintf(tmp, sizeof tmp, "\\%03u", c); buf_puts(b, tmp); }
    }
  }
  buf_puts(b, "\"");
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
#define Float_share(x) ((void)0)
#define Bool_share(x) ((void)0)
#define Unit_share(x) ((void)0)

/* Total order with nan first, like OCaml's compare (used by sort). */
static int Float_cmp(double a, double b) {
  if (isnan(a)) return isnan(b) ? 0 : -1;
  if (isnan(b)) return 1;
  return a < b ? -1 : (a > b ? 1 : 0);
}

static void Int_show(WBuf *b, int64_t x, bool top) { (void)top; char t[32]; snprintf(t, sizeof t, "%lld", (long long)x); buf_puts(b, t); }
static void Float_show(WBuf *b, double x, bool top) { (void)top; char t[64]; fmt_float(t, sizeof t, x); buf_puts(b, t); }
static void Bool_show(WBuf *b, bool x, bool top) { (void)top; buf_puts(b, x ? "true" : "false"); }
static void Unit_show(WBuf *b, W_Unit x, bool top) { (void)x; (void)top; buf_puts(b, "()"); }
static void Str_show(WBuf *b, WStr *s, bool top) { if (top) buf_putn(b, s->data, s->len); else buf_escaped(b, s); }

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
static inline int64_t w_f2i(double x) {
  if (isnan(x)) return 0;
  if (x >= 9.2e18) return INT64_MAX;
  if (x <= -9.2e18) return INT64_MIN;
  return (int64_t)x;
}
static inline int64_t w_abs(int64_t x) { return x < 0 ? (int64_t)(0 - (uint64_t)x) : x; }

__attribute__((noreturn)) static void w_panic_index(int64_t i, int64_t len, const char *where) {
  char m[128];
  snprintf(m, sizeof m, "index %lld is out of range for a list of length %lld", (long long)i, (long long)len);
  w_panic_c(m, where);
}

/* Bounds check for fixed-size arrays; with a constant size and a loop guard
   the C compiler removes it. */
static inline int64_t w_idx(int64_t i, int64_t n, const char *where) {
  if ((uint64_t)i >= (uint64_t)n) w_panic_index(i, n, where);
  return i;
}

/* ---------- lists ---------- */

#define VELA_LIST_STRUCT(L, T) struct L { int64_t rc; int64_t len; int64_t cap; T *data; };

#define VELA_LIST_FUNCS(L, T, P)                                                              \
  static void L##_kids(void *o) { L *l = (L *)o; for (int64_t i = 0; i < l->len; i++) P##_drop(l->data[i]); } \
  static L *L##_new(int64_t cap) {                                                            \
    L *l = (L *)w_alloc_k(sizeof(L), WK_LIST);                                                \
    w_set_kids(l, L##_kids);                                                                  \
    l->rc = 1; l->len = 0; l->cap = cap > 4 ? cap : 4;                                        \
    l->data = (T *)w_alloc(w_bytes(l->cap, sizeof(T)));                                       \
    return l;                                                                                 \
  }                                                                                           \
  static inline L *L##_dup(L *l) { w_rc_inc(&l->rc); return l; }                              \
  static void L##_drop(L *l) {                                                                \
    if (w_rc_dec(&l->rc)) {                                                                   \
      for (int64_t i = 0; i < l->len; i++) P##_drop(l->data[i]);                              \
      w_free(l->data); w_free(l);                                                             \
    }                                                                                         \
  }                                                                                           \
  __attribute__((noinline, cold)) static void L##_copy(L **p) {                              \
    L *l = *p;                                                                                \
    L *c = L##_new(l->len);                                                                   \
    for (int64_t i = 0; i < l->len; i++) c->data[i] = P##_dup(l->data[i]);                   \
    c->len = l->len;                                                                          \
    *p = c;                                                                                   \
    L##_drop(l);                                                                              \
  }                                                                                           \
  static void L##_share(L *l) {                                                               \
    if (w_rc_share(&l->rc)) for (int64_t i = 0; i < l->len; i++) P##_share(l->data[i]);       \
  }                                                                                           \
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
  static bool L##_eq(L *a, L *b) {                                                            \
    if (a->len != b->len) return false;                                                       \
    for (int64_t i = 0; i < a->len; i++) if (!P##_eq(a->data[i], b->data[i])) return false;  \
    return true;                                                                              \
  }                                                                                           \
  static void L##_show(WBuf *b, L *l, bool top) {                                             \
    (void)top; buf_puts(b, "[");                                                              \
    for (int64_t i = 0; i < l->len; i++) { if (i) buf_puts(b, ", "); P##_show(b, l->data[i], false); } \
    buf_puts(b, "]");                                                                         \
  }                                                                                           \
  static L *L##_slice(L *l, int64_t from, int64_t to) {                                       \
    if (from < 0) from = 0; if (to > l->len) to = l->len; if (to < from) to = from;           \
    L *c = L##_new(to - from);                                                                \
    for (int64_t i = from; i < to; i++) c->data[c->len++] = P##_dup(l->data[i]);             \
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
#define VELA_LIST_SORT(L, T, P)                                                               \
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
static uint64_t g_hash_k0, g_hash_k1;
static void w_seed_hash(void) {
  uint64_t k[2] = { 0, 0 };
  int fd = open("/dev/urandom", O_RDONLY);
  if (fd >= 0) { if (read(fd, k, sizeof k) != (ssize_t)sizeof k) { k[0] = 0; k[1] = 0; } close(fd); }
  if (k[0] == 0 && k[1] == 0) {
    struct timeval tv; gettimeofday(&tv, NULL);
    k[0] = (uint64_t)tv.tv_usec * 0x9E3779B97F4A7C15ULL ^ (uint64_t)getpid();
    k[1] = (uint64_t)tv.tv_sec * 0xBF58476D1CE4E5B9ULL ^ (uint64_t)(uintptr_t)&k;
  }
  g_hash_k0 = k[0]; g_hash_k1 = k[1];
}
#define W_ROTL(x, b) (uint64_t)(((x) << (b)) | ((x) >> (64 - (b))))
#define W_SIPROUND                                                          \
  do {                                                                      \
    v0 += v1; v1 = W_ROTL(v1, 13); v1 ^= v0; v0 = W_ROTL(v0, 32);           \
    v2 += v3; v3 = W_ROTL(v3, 16); v3 ^= v2;                                \
    v0 += v3; v3 = W_ROTL(v3, 21); v3 ^= v0;                                \
    v2 += v1; v1 = W_ROTL(v1, 17); v1 ^= v2; v2 = W_ROTL(v2, 32);           \
  } while (0)
static uint64_t w_siphash13(const unsigned char *in, size_t len) {
  uint64_t v0 = 0x736f6d6570736575ULL ^ g_hash_k0, v1 = 0x646f72616e646f6dULL ^ g_hash_k1;
  uint64_t v2 = 0x6c7967656e657261ULL ^ g_hash_k0, v3 = 0x7465646279746573ULL ^ g_hash_k1;
  const unsigned char *end = in + (len - len % 8);
  for (; in != end; in += 8) {
    uint64_t m;
    memcpy(&m, in, 8);
    v3 ^= m; W_SIPROUND; v0 ^= m;
  }
  uint64_t b = (uint64_t)len << 56;
  switch (len & 7) {
    case 7: b |= (uint64_t)in[6] << 48; /* fallthrough */
    case 6: b |= (uint64_t)in[5] << 40; /* fallthrough */
    case 5: b |= (uint64_t)in[4] << 32; /* fallthrough */
    case 4: b |= (uint64_t)in[3] << 24; /* fallthrough */
    case 3: b |= (uint64_t)in[2] << 16; /* fallthrough */
    case 2: b |= (uint64_t)in[1] << 8; /* fallthrough */
    case 1: b |= (uint64_t)in[0]; break;
    default: break;
  }
  v3 ^= b; W_SIPROUND; v0 ^= b;
  v2 ^= 0xff;
  W_SIPROUND; W_SIPROUND; W_SIPROUND;
  return v0 ^ v1 ^ v2 ^ v3;
}
static inline uint64_t Int_hash(int64_t x) { return w_siphash13((const unsigned char *)&x, sizeof x); }
static inline uint64_t Unit_hash(W_Unit u) { (void)u; return 0; }
static inline uint64_t Bool_hash(bool b) { return b ? 0x51ED27F1ULL : 0x2545F491ULL; }
/* Combine the hashes of a struct's fields (boost's hash_combine). */
static inline uint64_t w_hash_mix(uint64_t h, uint64_t x) { return h ^ (x + 0x9E3779B97F4A7C15ULL + (h << 6) + (h >> 2)); }
static inline uint64_t Str_hash(WStr *s) { return w_siphash13((const unsigned char *)s->data, (size_t)s->len); }
__attribute__((noreturn)) static void w_panic_key(WBuf *key, const char *where) {
  WBuf m; buf_init(&m);
  buf_puts(&m, "key "); buf_putn(&m, key->p, key->len); buf_puts(&m, " is not in the map");
  buf_putn(&m, "", 1);
  w_panic_c(m.p, where);
}

#define VELA_MAP_STRUCT(M, K, V) \
  struct M { int64_t rc; int64_t len, n, cap, icap; K *keys; V *vals; uint8_t *live; int64_t *idx; };

#define VELA_MAP_FUNCS(M, K, KN, V, VN)                                                       \
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
  static void M##_drop(M *m) {                                                                \
    if (w_rc_dec(&m->rc)) {                                                                   \
      for (int64_t i = 0; i < m->n; i++) if (m->live[i]) { KN##_drop(m->keys[i]); VN##_drop(m->vals[i]); } \
      w_free(m->keys); w_free(m->vals); w_free(m->live); w_free(m->idx); w_free(m);           \
    }                                                                                         \
  }                                                                                           \
  static void M##_share(M *m) {                                                               \
    if (w_rc_share(&m->rc))                                                                   \
      for (int64_t i = 0; i < m->n; i++) if (m->live[i]) { KN##_share(m->keys[i]); VN##_share(m->vals[i]); } \
  }                                                                                           \
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
  static bool M##_eq(M *a, M *b) {                                                            \
    if (a->len != b->len) return false;                                                       \
    for (int64_t i = 0; i < a->n; i++)                                                        \
      if (a->live[i]) { V *w = M##_get(b, a->keys[i]); if (!w || !VN##_eq(a->vals[i], *w)) return false; } \
    return true;                                                                              \
  }                                                                                           \
  static void M##_show(WBuf *b, M *m, bool top) {                                             \
    (void)top;                                                                                \
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
  static V *M##_slot(M *m, K k, const char *where) {                                         \
    V *v = M##_get(m, k);                                                                     \
    if (!v) { WBuf kb; buf_init(&kb); KN##_show(&kb, k, false); w_panic_key(&kb, where); }   \
    return v;                                                                                 \
  }                                                                                           \
  static V M##_at(M *m, K k, const char *where) { return VN##_dup(*M##_slot(m, k, where)); }

typedef struct L_Int L_Int;
typedef struct L_Str L_Str;
VELA_LIST_STRUCT(L_Int, int64_t)
VELA_LIST_STRUCT(L_Str, WStr *)
VELA_LIST_FUNCS(L_Int, int64_t, Int)
VELA_LIST_FUNCS(L_Str, WStr *, Str)
VELA_LIST_SORT(L_Int, int64_t, Int)
VELA_LIST_SORT(L_Str, WStr *, Str)

static int64_t L_Int_sum(L_Int *l) { int64_t s = 0; for (int64_t i = 0; i < l->len; i++) s += l->data[i]; return s; }
static WStr *L_Str_join(L_Str *l, WStr *sep) {
  WBuf b; buf_init(&b);
  for (int64_t i = 0; i < l->len; i++) { if (i) buf_putn(&b, sep->data, sep->len); buf_putn(&b, l->data[i]->data, l->data[i]->len); }
  return buf_take(&b);
}

/* ---------- string methods ---------- */

static int64_t Str_find(WStr *s, WStr *sub, int64_t from) {
  if (sub->len == 0) return from;
  for (int64_t i = from; i + sub->len <= s->len; i++)
    if (memcmp(s->data + i, sub->data, (size_t)sub->len) == 0) return i;
  return -1;
}
static L_Str *Str_split(WStr *s, WStr *sep) {
  L_Str *out = L_Str_new(4);
  if (sep->len == 0) {
    for (int64_t i = 0; i < s->len; i++) L_Str_push(&out, Str_from(s->data + i, 1));
    return out;
  }
  int64_t from = 0, i;
  while ((i = Str_find(s, sep, from)) >= 0) { L_Str_push(&out, Str_from(s->data + from, i - from)); from = i + sep->len; }
  L_Str_push(&out, Str_from(s->data + from, s->len - from));
  return out;
}
static L_Str *Str_lines(WStr *s) {
  L_Str *out = L_Str_new(4);
  int64_t from = 0;
  for (int64_t i = 0; i <= s->len; i++) {
    if (i == s->len || s->data[i] == '\n') {
      int64_t end = i;
      if (end > from && s->data[end - 1] == '\r') end--;
      if (!(i == s->len && from == s->len)) L_Str_push(&out, Str_from(s->data + from, end - from));
      from = i + 1;
    }
  }
  return out;
}
static bool is_ws(char c) { return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'; }
static WStr *Str_trim(WStr *s) {
  int64_t a = 0, b = s->len;
  while (a < b && is_ws(s->data[a])) a++;
  while (b > a && is_ws(s->data[b - 1])) b--;
  return Str_from(s->data + a, b - a);
}
static WStr *Str_map_case(WStr *s, bool upper) {
  WStr *r = Str_from(s->data, s->len);
  for (int64_t i = 0; i < r->len; i++) {
    char c = r->data[i];
    if (upper && c >= 'a' && c <= 'z') r->data[i] = (char)(c - 32);
    if (!upper && c >= 'A' && c <= 'Z') r->data[i] = (char)(c + 32);
  }
  return r;
}
static WStr *Str_replace(WStr *s, WStr *a, WStr *b) {
  if (a->len == 0) return Str_dup(s);
  L_Str *parts = Str_split(s, a);
  WStr *r = L_Str_join(parts, b);
  L_Str_drop(parts);
  return r;
}
static bool Str_starts(WStr *s, WStr *p) { return p->len <= s->len && memcmp(s->data, p->data, (size_t)p->len) == 0; }
static bool Str_ends(WStr *s, WStr *p) { return p->len <= s->len && memcmp(s->data + s->len - p->len, p->data, (size_t)p->len) == 0; }
/* Decimal integer with optional sign, surrounding whitespace allowed. */
static bool Str_parse_int(WStr *s, int64_t *out) {
  int64_t a = 0, b = s->len;
  while (a < b && is_ws(s->data[a])) a++;
  while (b > a && is_ws(s->data[b - 1])) b--;
  if (a == b) return false;
  bool neg = false;
  if (s->data[a] == '-' || s->data[a] == '+') { neg = s->data[a] == '-'; a++; }
  if (a == b) return false;
  uint64_t v = 0;
  for (int64_t i = a; i < b; i++) {
    char c = s->data[i];
    if (c < '0' || c > '9') return false;
    if (v > (UINT64_MAX - (uint64_t)(c - '0')) / 10) return false;
    v = v * 10 + (uint64_t)(c - '0');
  }
  if (!neg && v > (uint64_t)INT64_MAX) return false;
  if (neg && v > (uint64_t)INT64_MAX + 1) return false;
  *out = neg ? (int64_t)(0 - v) : (int64_t)v;
  return true;
}

/* Byte offsets, clamped to the string. */
static WStr *Str_slice(WStr *s, int64_t lo, int64_t hi) {
  if (lo < 0) lo = 0;
  if (lo > s->len) lo = s->len;
  if (hi > s->len) hi = s->len;
  if (hi < lo) hi = lo;
  return Str_from(s->data + lo, hi - lo);
}
static L_Int *Str_bytes(WStr *s) {
  L_Int *out = L_Int_new(s->len);
  for (int64_t i = 0; i < s->len; i++) L_Int_push(&out, (int64_t)(unsigned char)s->data[i]);
  return out;
}
/* The length of the UTF-8 character at s[i], or 1 for a byte that does not
   start a well-formed one (as Interp.utf8_len). */
static int64_t w_utf8_len(const unsigned char *s, int64_t i, int64_t n) {
  unsigned char b = s[i];
  int64_t k;
  unsigned char lo = 0x80, hi = 0xBF;
  if (b < 0x80) return 1;
  else if (b >= 0xC2 && b <= 0xDF) k = 2;
  else if (b >= 0xE0 && b <= 0xEF) { k = 3; if (b == 0xE0) lo = 0xA0; if (b == 0xED) hi = 0x9F; }
  else if (b >= 0xF0 && b <= 0xF4) { k = 4; if (b == 0xF0) lo = 0x90; if (b == 0xF4) hi = 0x8F; }
  else return 1;
  if (i + k > n) return 1;
  if (s[i + 1] < lo || s[i + 1] > hi) return 1;
  for (int64_t j = 2; j < k; j++) if (s[i + j] < 0x80 || s[i + j] > 0xBF) return 1;
  return k;
}
/* The characters (UTF-8 code points) of a string, each as a string. */
static L_Str *Str_chars(WStr *s) {
  L_Str *out = L_Str_new(s->len);
  for (int64_t i = 0; i < s->len;) {
    int64_t k = w_utf8_len((const unsigned char *)s->data, i, s->len);
    L_Str_push(&out, Str_from(s->data + i, k));
    i += k;
  }
  return out;
}
/* lo..<hi as a list */
static L_Int *L_Int_range(int64_t lo, int64_t hi) {
  int64_t n = 0;
  if (hi > lo && __builtin_sub_overflow(hi, lo, &n)) w_oom();
  L_Int *out = L_Int_new(n);
  for (int64_t i = 0; i < n; i++) out->data[i] = lo + i;
  out->len = n;
  return out;
}
/* The UTF-8 encoding of a code point; U+FFFD for anything that is not one. */
static WStr *Int_to_char(int64_t n) {
  if (n < 0 || n > 0x10FFFF || (n >= 0xD800 && n <= 0xDFFF)) n = 0xFFFD;
  char b[4];
  int k;
  if (n < 0x80) { b[0] = (char)n; k = 1; }
  else if (n < 0x800) { b[0] = (char)(0xC0 | (n >> 6)); b[1] = (char)(0x80 | (n & 0x3F)); k = 2; }
  else if (n < 0x10000) { b[0] = (char)(0xE0 | (n >> 12)); b[1] = (char)(0x80 | ((n >> 6) & 0x3F)); b[2] = (char)(0x80 | (n & 0x3F)); k = 3; }
  else { b[0] = (char)(0xF0 | (n >> 18)); b[1] = (char)(0x80 | ((n >> 12) & 0x3F)); b[2] = (char)(0x80 | ((n >> 6) & 0x3F)); b[3] = (char)(0x80 | (n & 0x3F)); k = 4; }
  return Str_from(b, k);
}
/* A decimal float: digits with optional sign, point and exponent; no hex, nan or inf. */
static bool Str_parse_float(WStr *s, double *out) {
  int64_t a = 0, b = s->len;
  while (a < b && is_ws(s->data[a])) a++;
  while (b > a && is_ws(s->data[b - 1])) b--;
  if (a == b || b - a > 400) return false;
  bool digit = false;
  for (int64_t i = a; i < b; i++) {
    char c = s->data[i];
    if (c >= '0' && c <= '9') digit = true;
    else if (!(c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E')) return false;
  }
  if (!digit) return false;
  char t[408];
  memcpy(t, s->data + a, (size_t)(b - a));
  t[b - a] = 0;
  char *end;
  double x = strtod(t, &end);
  if (end != t + (b - a)) return false;
  *out = x;
  return true;
}

/* ---------- closures ---------- */

typedef struct WClo WClo;
struct WClo { int64_t rc; void *fn; void (*drop)(WClo *); void (*share)(WClo *); };
static inline WClo *Clo_dup(WClo *c) { w_rc_inc(&c->rc); return c; }
static void Clo_drop(WClo *c) { if (w_rc_dec(&c->rc)) { if (c->drop) c->drop(c); w_free(c); } }
static void Clo_share(WClo *c) { if (w_rc_share(&c->rc) && c->share) c->share(c); }
static inline bool Clo_eq(WClo *a, WClo *b) { return a == b; }
static void Clo_show(WBuf *b, WClo *c, bool top) { (void)c; (void)top; buf_puts(b, "<fn>"); }

/* `dyn I`: a boxed value with a vtable generated per (type, interface):
   drop, share and show for the value, then one entry per method. */
typedef struct WDyn WDyn;
typedef struct { void (*drop)(WDyn *); void (*share)(WDyn *); void (*show)(WBuf *, WDyn *, bool); } WVtHead;
typedef struct { WVtHead h; void *m[]; } WVt;
struct WDyn { int64_t rc; const WVt *vt; };
static inline WDyn *Dyn_dup(WDyn *d) { w_rc_inc(&d->rc); return d; }
static void Dyn_drop(WDyn *d) { if (w_rc_dec(&d->rc)) { d->vt->h.drop(d); w_free(d); } }
static void Dyn_share(WDyn *d) { if (w_rc_share(&d->rc)) d->vt->h.share(d); }
static inline bool Dyn_eq(WDyn *a, WDyn *b) { return a == b; }
static void Dyn_show(WBuf *b, WDyn *d, bool top) { d->vt->h.show(b, d, top); }
static void w_dyn_kids(void *o) { WDyn *d = (WDyn *)o; d->vt->h.drop(d); }

/* ---------- capabilities ---------- */

enum { CAP_ROOT, CAP_NET, CAP_FS, CAP_STDOUT, CAP_CLOCK, CAP_LISTEN, CAP_LISTENER, CAP_CONN, CAP_RAND, CAP_STDIN, CAP_ENV };
typedef struct WCap WCap;
struct WCap {
  int64_t rc; int kind;
  char **hosts; int nhosts;   /* Net: allowlist entries */
  char *root; bool ro;        /* Fs: realpath of root, NULL = not granted */
  int fd; int port;           /* Listener, Conn: the socket */
  int64_t inflight; int closing; /* operations in progress; close requested */
};
static WCap *cap_new(int kind) { WCap *c = (WCap *)w_alloc_k(sizeof(WCap), WK_CAP); memset(c, 0, sizeof *c); c->rc = 1; c->kind = kind; c->fd = -1; return c; }
static void w_sock_release_fd(int fd);
static inline WCap *Cap_dup(WCap *c) { w_rc_inc(&c->rc); return c; }
static inline void Cap_share(WCap *c) { w_rc_share(&c->rc); }
static void Cap_release(WCap *c) { for (int i = 0; i < c->nhosts; i++) free(c->hosts[i]); free(c->hosts); free(c->root); if (c->fd >= 0) w_sock_release_fd(c->fd); w_free(c); }
static void Cap_drop(WCap *c) { if (w_rc_dec(&c->rc)) Cap_release(c); }
static inline bool Cap_eq(WCap *a, WCap *b) { return a == b; }
static void Cap_show(WBuf *b, WCap *c, bool top) {
  (void)top;
  static const char *names[] = { "<Root>", "<Net>", "<Fs>", "<Stdout>", "<Clock>", "<Listen>", "<Listener>", "<Conn>", "<Rand>", "<Stdin>", "<Env>" };
  buf_puts(b, names[c->kind]);
}

static char **g_hosts; static int g_nhosts; static char *g_fs_root; static bool g_fs_ro;
static char **g_listen; static int g_nlisten;
static char **g_env; static int g_nenv;
static char **g_args; static int g_nargs;
static uint64_t g_rand_state;

static void w_usage(const char *m) {
  fprintf(stderr, "%s\nusage: PROGRAM [--grant net=HOST[:PORT]|*]... [--grant fs=DIR[:ro]]\n", m);
  exit(2);
}
static void w_init(int argc, char **argv) {
  signal(SIGPIPE, SIG_IGN);   /* a peer closing a socket is an error value, not a crash */
  w_seed_hash();
  {
    struct rlimit rl;
    if (getrlimit(RLIMIT_NOFILE, &rl) == 0) {
      rlim_t want = rl.rlim_max < (rlim_t)65536 ? rl.rlim_max : (rlim_t)65536;
      if (rl.rlim_cur < want) { rl.rlim_cur = want; setrlimit(RLIMIT_NOFILE, &rl); }
    }
  }
  {
    struct timeval tv; gettimeofday(&tv, NULL);
    g_rand_state = (uint64_t)tv.tv_sec * 1000000u + (uint64_t)tv.tv_usec;
    g_rand_state ^= (uint64_t)getpid();
  }
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--") == 0) { g_args = argv + i + 1; g_nargs = argc - i - 1; break; }
    if (strcmp(argv[i], "--seed") == 0 && i + 1 < argc) { g_rand_state = (uint64_t)strtoll(argv[++i], NULL, 10); continue; }
    if (strcmp(argv[i], "--grant") != 0 || i + 1 >= argc) w_usage("unknown argument");
    const char *g = argv[++i];
    if (strncmp(g, "net=", 4) == 0) {
      g_hosts = (char **)w_realloc(g_hosts, sizeof(char *) * (size_t)(g_nhosts + 1));
      g_hosts[g_nhosts++] = strdup(g + 4);
    } else if (strncmp(g, "fs=", 3) == 0) {
      char *v = strdup(g + 3);
      size_t n = strlen(v);
      if (n > 3 && strcmp(v + n - 3, ":ro") == 0) { v[n - 3] = 0; g_fs_ro = true; }
      char *r = realpath(v, NULL);
      struct stat st;
      if (!r || stat(r, &st) != 0 || !S_ISDIR(st.st_mode)) w_usage("fs grant: no such directory");
      free(v);
      g_fs_root = r;
    } else if (strncmp(g, "env=", 4) == 0) {
      g_env = (char **)w_realloc(g_env, sizeof(char *) * (size_t)(g_nenv + 1));
      g_env[g_nenv++] = strdup(g + 4);
    } else if (strncmp(g, "listen=", 7) == 0) {
      g_listen = (char **)w_realloc(g_listen, sizeof(char *) * (size_t)(g_nlisten + 1));
      g_listen[g_nlisten++] = strdup(g + 7);
    } else w_usage("unknown grant");
  }
}
static WCap *w_root(void) { return cap_new(CAP_ROOT); }
static WCap *Cap_root_field(WCap *root, const char *f) {
  (void)root;
  if (strcmp(f, "net") == 0) {
    WCap *c = cap_new(CAP_NET);
    c->hosts = (char **)w_raw(sizeof(char *) * (size_t)(g_nhosts + 1));
    for (int i = 0; i < g_nhosts; i++) c->hosts[i] = strdup(g_hosts[i]);
    c->nhosts = g_nhosts;
    return c;
  }
  if (strcmp(f, "fs") == 0) {
    WCap *c = cap_new(CAP_FS);
    c->root = g_fs_root ? strdup(g_fs_root) : NULL;
    c->ro = g_fs_root ? g_fs_ro : true;
    return c;
  }
  if (strcmp(f, "stdout") == 0) return cap_new(CAP_STDOUT);
  if (strcmp(f, "listen") == 0) {
    WCap *c = cap_new(CAP_LISTEN);
    c->hosts = (char **)w_raw(sizeof(char *) * (size_t)(g_nlisten + 1));
    for (int i = 0; i < g_nlisten; i++) c->hosts[i] = strdup(g_listen[i]);
    c->nhosts = g_nlisten;
    return c;
  }
  if (strcmp(f, "rand") == 0) return cap_new(CAP_RAND);
  if (strcmp(f, "stdin") == 0) return cap_new(CAP_STDIN);
  if (strcmp(f, "env") == 0) {
    WCap *c = cap_new(CAP_ENV);
    c->hosts = (char **)w_raw(sizeof(char *) * (size_t)(g_nenv + 1));
    for (int i = 0; i < g_nenv; i++) c->hosts[i] = strdup(g_env[i]);
    c->nhosts = g_nenv;
    return c;
  }
  return cap_new(CAP_CLOCK);
}

/* Rand: splitmix64. The state only ever advances by a constant, so an atomic
   add keeps it correct across threads, and a seed gives the interpreter's
   sequence too. */
static uint64_t w_rand_next(void) {
  uint64_t z = __atomic_add_fetch(&g_rand_state, 0x9E3779B97F4A7C15ULL, __ATOMIC_RELAXED);
  z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
  z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
  return z ^ (z >> 31);
}
static int64_t Cap_rand_int(int64_t lo, int64_t hi, const char *where) {
  if (hi <= lo) {
    char m[96];
    snprintf(m, sizeof m, "rand.int: empty range %lld..%lld", (long long)lo, (long long)hi);
    w_panic_c(m, where);
  }
  uint64_t span = (uint64_t)hi - (uint64_t)lo;
  return (int64_t)((uint64_t)lo + w_rand_next() % span);
}
static double Cap_rand_float(void) { return (double)(w_rand_next() >> 11) * 0x1.0p-53; }
static L_Str *Cap_args(void) {
  L_Str *out = L_Str_new(g_nargs);
  for (int i = 0; i < g_nargs; i++) L_Str_push(&out, Str_from(g_args[i], (int64_t)strlen(g_args[i])));
  return out;
}
/* Stdin blocks the worker thread while it waits; reading is meant for
   programs that read their input up front. */
static WStr *Cap_stdin_line(bool *ok) {
  char *line = NULL; size_t cap = 0;
  ssize_t n = getline(&line, &cap, stdin);
  if (n < 0) { free(line); *ok = false; return NULL; }
  if (n > 0 && line[n - 1] == '\n') n--;
  if (n > 0 && line[n - 1] == '\r') n--;
  WStr *s = Str_from(line, (int64_t)n);
  free(line);
  *ok = true;
  return s;
}
static WStr *Cap_stdin_all(void) {
  WBuf b; buf_init(&b);
  char t[65536]; size_t k;
  while ((k = fread(t, 1, sizeof t, stdin)) > 0) buf_putn(&b, t, (int64_t)k);
  return buf_take(&b);
}
static WStr *Cap_env_get(WCap *env, WStr *name, bool *ok) {
  char *n = strndup(name->data, (size_t)name->len);
  bool allowed = false;
  for (int i = 0; i < env->nhosts; i++) if (strcmp(env->hosts[i], "*") == 0 || strcmp(env->hosts[i], n) == 0) allowed = true;
  const char *v = allowed ? getenv(n) : NULL;
  free(n);
  if (!v) { *ok = false; return NULL; }
  *ok = true;
  return Str_from(v, (int64_t)strlen(v));
}

/* I/O results: kind 0 = ok, 1 = denied, 2 = failed. */
typedef struct { int kind; WStr *val; WStr *msg; void *ptr; } WIo;
static WIo io_ok(WStr *v) { WIo r = { 0, v, NULL, NULL }; return r; }
static WIo io_ok_ptr(void *p) { WIo r = { 0, NULL, NULL, p }; return r; }
static WIo io_err(int kind, const char *m) { WIo r = { kind, NULL, Str_from(m, (int64_t)strlen(m)), NULL }; return r; }

static void lower_ascii(char *s) { for (; *s; s++) if (*s >= 'A' && *s <= 'Z') *s = (char)(*s + 32); }

/* Parse "host", "host:port" or "*". port -1 = any. */
static void parse_entry(const char *e, char *host, size_t hn, int *port) {
  const char *c = strrchr(e, ':');
  char *end;
  if (c && c[1]) {
    long p = strtol(c + 1, &end, 10);
    if (*end == 0) { snprintf(host, hn, "%.*s", (int)(c - e), e); *port = (int)p; lower_ascii(host); return; }
  }
  snprintf(host, hn, "%s", e); *port = -1; lower_ascii(host);
}
static bool net_permits(WCap *n, const char *host, int port) {
  for (int i = 0; i < n->nhosts; i++) {
    if (strcmp(n->hosts[i], "*") == 0) return true;
    char h[512]; int p;
    parse_entry(n->hosts[i], h, sizeof h, &p);
    if (strcmp(h, host) == 0 && (p < 0 || p == port)) return true;
  }
  return false;
}
static WCap *Cap_net_only(WCap *n, WStr *req_s) {
  char *req = strndup(req_s->data, (size_t)req_s->len);
  bool covered = false;
  if (strcmp(req, "*") == 0) {
    for (int i = 0; i < n->nhosts; i++) if (strcmp(n->hosts[i], "*") == 0) covered = true;
  } else {
    char h[512]; int p;
    parse_entry(req, h, sizeof h, &p);
    if (p >= 0) covered = net_permits(n, h, p);
    else
      for (int i = 0; i < n->nhosts; i++) {
        char h2[512]; int p2;
        if (strcmp(n->hosts[i], "*") == 0) { covered = true; break; }
        parse_entry(n->hosts[i], h2, sizeof h2, &p2);
        if (p2 < 0 && strcmp(h2, h) == 0) { covered = true; break; }
      }
  }
  WCap *c = cap_new(CAP_NET);
  c->hosts = (char **)w_raw(sizeof(char *));
  if (covered) { c->hosts[0] = req; c->nhosts = 1; } else free(req);
  return c;
}

/* Strict URL check, so the host we allow is the host curl connects to:
   http or https only; no userinfo (`@`); host made of [A-Za-z0-9.-];
   port plain digits in 1..65535; no spaces or control characters anywhere.
   Mirrors Caps.parse_url in the interpreter. */
static int parse_url(const char *u, char *host, size_t hn, int *port, char *err, size_t en) {
  for (const unsigned char *k = (const unsigned char *)u; *k; k++)
    if (*k <= 0x20 || *k >= 0x7f) { snprintf(err, en, "URL contains spaces or non-ASCII characters"); return 0; }
  const char *c = strstr(u, "://");
  if (!c) { snprintf(err, en, "URL has no scheme"); return 0; }
  size_t sl = (size_t)(c - u);
  int https;
  if (sl == 4 && strncasecmp(u, "http", 4) == 0) https = 0;
  else if (sl == 5 && strncasecmp(u, "https", 5) == 0) https = 1;
  else { snprintf(err, en, "scheme `%.*s` is not allowed, only http and https", (int)(sl > 32 ? 32 : sl), u); return 0; }
  const char *auth = c + 3;
  size_t alen = strcspn(auth, "/?#");
  const char *colon = NULL;
  for (size_t i = 0; i < alen; i++) {
    char ch = auth[i];
    if (ch == ':') { if (colon) { snprintf(err, en, "malformed host in URL"); return 0; } colon = auth + i; }
    else if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == '.' || ch == '-')) {
      snprintf(err, en, "URL host may only contain letters, digits, `.` and `-`"); return 0;
    }
  }
  size_t hl = colon ? (size_t)(colon - auth) : alen;
  if (hl == 0) { snprintf(err, en, "URL has no host"); return 0; }
  if (hl >= hn) { snprintf(err, en, "URL host is too long"); return 0; }
  *port = https ? 443 : 80;
  if (colon) {
    const char *ps = colon + 1, *pe = auth + alen;
    if (ps == pe || pe - ps > 5) { snprintf(err, en, "bad port in URL"); return 0; }
    int p = 0;
    for (const char *k = ps; k < pe; k++) p = p * 10 + (*k - '0');   /* digits only: checked above, ':' excluded */
    for (const char *k = ps; k < pe; k++) if (*k < '0' || *k > '9') { snprintf(err, en, "bad port in URL"); return 0; }
    if (p < 1 || p > 65535) { snprintf(err, en, "bad port in URL"); return 0; }
    *port = p;
  }
  memcpy(host, auth, hl); host[hl] = 0; lower_ascii(host);
  return 1;
}

static WStr *read_fd(int fd) {
  WBuf b; buf_init(&b);
  char *tmp = (char *)w_raw(65536);   /* on the heap: task stacks are small */
  ssize_t n;
  while ((n = read(fd, tmp, 65536)) > 0) buf_putn(&b, tmp, n);
  free(tmp);
  return buf_take(&b);
}

static WIo Cap_net_get(WCap *n, WStr *url_s) {
  char *url = strndup(url_s->data, (size_t)url_s->len);
  char host[512], err[256]; int port;
  if (!parse_url(url, host, sizeof host, &port, err, sizeof err)) { free(url); return io_err(2, err); }
  if (!net_permits(n, host, port)) {
    char m[700]; snprintf(m, sizeof m, "net: %s:%d is not granted", host, port);
    free(url); return io_err(1, m);
  }
  int out[2], er[2];
  if (pipe(out) || pipe(er)) { free(url); return io_err(2, "pipe failed"); }
  posix_spawn_file_actions_t fa;
  posix_spawn_file_actions_init(&fa);
  posix_spawn_file_actions_adddup2(&fa, out[1], 1);
  posix_spawn_file_actions_adddup2(&fa, er[1], 2);
  posix_spawn_file_actions_addclose(&fa, out[0]);
  posix_spawn_file_actions_addclose(&fa, er[0]);
  char *argv[] = { "curl", "-sS", "--fail", "--max-time", "20", "--proto", "=http,https", "--", url, NULL };
  pid_t pid;
  int rc = posix_spawnp(&pid, "curl", &fa, NULL, argv, environ);
  posix_spawn_file_actions_destroy(&fa);
  close(out[1]); close(er[1]);
  if (rc != 0) { close(out[0]); close(er[0]); free(url); return io_err(2, "could not run curl"); }
  WStr *body = read_fd(out[0]);
  WStr *emsg = read_fd(er[0]);
  close(out[0]); close(er[0]);
  int status; waitpid(pid, &status, 0);
  free(url);
  if (WIFEXITED(status) && WEXITSTATUS(status) == 0) { Str_drop(emsg); return io_ok(body); }
  Str_drop(body);
  WStr *t = Str_trim(emsg); Str_drop(emsg);
  WIo r = { 2, NULL, t, NULL };
  return r;
}

/* Normalise `rel` under `root`; returns malloc'd path or NULL with err set. */
static char *fs_normalize(const char *root, const char *rel, const char **err) {
  if (rel[0] == '/') { *err = "absolute paths are not allowed; paths are relative to the granted directory"; return NULL; }
  char **parts = (char **)w_raw(sizeof(char *) * (strlen(rel) + 1));
  int np = 0;
  char *copy = strdup(rel), *save = NULL;
  for (char *p = strtok_r(copy, "/", &save); p; p = strtok_r(NULL, "/", &save)) {
    if (strcmp(p, ".") == 0) continue;
    if (strcmp(p, "..") == 0) {
      if (np == 0) { free(copy); free(parts); *err = "path escapes the granted directory"; return NULL; }
      np--; continue;
    }
    parts[np++] = p;
  }
  WBuf b; buf_init(&b);
  buf_puts(&b, root);
  for (int i = 0; i < np; i++) { buf_puts(&b, "/"); buf_puts(&b, parts[i]); }
  buf_putn(&b, "", 1);
  free(copy); free(parts);
  return b.p;
}
static bool fs_inside(const char *root, const char *p) {
  size_t n = strlen(root);
  if (strcmp(root, "/") == 0) return p[0] == '/';
  return strncmp(root, p, n) == 0 && (p[n] == 0 || p[n] == '/');
}
static char *dirname_of(const char *p) {
  char *d = strdup(p);
  char *s = strrchr(d, '/');
  if (s && s != d) *s = 0; else if (s) s[1] = 0;
  return d;
}
/* Resolve; on error returns NULL and fills kind/msg. */
static char *fs_resolve(WCap *fs, WStr *rel_s, bool for_write, int *kind, char *msg, size_t mn) {
  if (!fs->root) { *kind = 1; snprintf(msg, mn, "fs: not granted (run with --grant fs=DIR)"); return NULL; }
  char *rel = strndup(rel_s->data, (size_t)rel_s->len);
  const char *err = NULL;
  char *p = fs_normalize(fs->root, rel, &err);
  if (!p) { *kind = 1; snprintf(msg, mn, "fs: %s", err); free(rel); return NULL; }
  struct stat st;
  bool exists = stat(p, &st) == 0 || lstat(p, &st) == 0;
  char *target = (for_write && !exists) ? dirname_of(p) : strdup(p);
  char *real = realpath(target, NULL);
  free(target);
  if (real && fs_inside(fs->root, real)) { free(real); free(rel); return p; }
  if (real) { *kind = 1; snprintf(msg, mn, "fs: path resolves outside the granted directory"); }
  else if (for_write) { *kind = 2; snprintf(msg, mn, "fs: directory of `%s` does not exist", rel); }
  else { *kind = 2; snprintf(msg, mn, "fs: `%s` does not exist", rel); }
  free(real); free(p); free(rel);
  return NULL;
}
static WCap *Cap_fs_under(WCap *fs, WStr *rel_s) {
  WCap *c = cap_new(CAP_FS);
  c->ro = fs->ro;
  if (!fs->root) return c;
  char *rel = strndup(rel_s->data, (size_t)rel_s->len);
  const char *err = NULL;
  char *p = fs_normalize(fs->root, rel, &err);
  free(rel);
  if (!p) return c;
  char *real = realpath(p, NULL);
  if (real) { if (fs_inside(fs->root, real)) c->root = real; else free(real); free(p); }
  else c->root = p;
  return c;
}
static WCap *Cap_fs_read_only(WCap *fs) {
  WCap *c = cap_new(CAP_FS);
  c->root = fs->root ? strdup(fs->root) : NULL;
  c->ro = true;
  return c;
}
static WIo Cap_fs_read(WCap *fs, WStr *rel) {
  int kind; char msg[1024];
  char *p = fs_resolve(fs, rel, false, &kind, msg, sizeof msg);
  if (!p) return io_err(kind, msg);
  FILE *f = fopen(p, "rb");
  if (!f) { snprintf(msg, sizeof msg, "%s: %s", p, strerror(errno)); free(p); return io_err(2, msg); }
  WStr *s = read_fd(fileno(f));
  fclose(f); free(p);
  return io_ok(s);
}
static WIo Cap_fs_write(WCap *fs, WStr *rel, WStr *data) {
  if (fs->root && fs->ro) return io_err(1, "fs: this capability is read-only");
  int kind; char msg[1024];
  char *p = fs_resolve(fs, rel, true, &kind, msg, sizeof msg);
  if (!p) return io_err(kind, msg);
  FILE *f = fopen(p, "wb");
  if (!f) { snprintf(msg, sizeof msg, "%s: %s", p, strerror(errno)); free(p); return io_err(2, msg); }
  fwrite(data->data, 1, (size_t)data->len, f);
  fclose(f); free(p);
  return io_ok(NULL);
}
static bool Cap_fs_exists(WCap *fs, WStr *rel) {
  int kind; char msg[1024];
  char *p = fs_resolve(fs, rel, false, &kind, msg, sizeof msg);
  if (!p) return false;
  struct stat st;
  bool e = stat(p, &st) == 0;
  free(p);
  return e;
}
static WIo Cap_fs_append(WCap *fs, WStr *rel, WStr *data) {
  if (fs->root && fs->ro) return io_err(1, "fs: this capability is read-only");
  int kind; char msg[1024];
  char *p = fs_resolve(fs, rel, true, &kind, msg, sizeof msg);
  if (!p) return io_err(kind, msg);
  FILE *f = fopen(p, "ab");
  if (!f) { snprintf(msg, sizeof msg, "%s: %s", p, strerror(errno)); free(p); return io_err(2, msg); }
  fwrite(data->data, 1, (size_t)data->len, f);
  fclose(f); free(p);
  return io_ok(NULL);
}
static int w_cmp_cstr(const void *a, const void *b) { return strcmp(*(char *const *)a, *(char *const *)b); }
static WIo Cap_fs_list(WCap *fs, WStr *rel) {
  int kind; char msg[1024];
  char *p = fs_resolve(fs, rel, false, &kind, msg, sizeof msg);
  if (!p) return io_err(kind, msg);
  DIR *d = opendir(p);
  free(p);
  if (!d) { snprintf(msg, sizeof msg, "fs: `%.*s` is not a directory", (int)rel->len, rel->data); return io_err(2, msg); }
  char **names = NULL; int n = 0;
  struct dirent *e;
  while ((e = readdir(d))) {
    if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0) continue;
    names = (char **)w_realloc(names, sizeof(char *) * (size_t)(n + 1));
    names[n++] = strdup(e->d_name);
  }
  closedir(d);
  qsort(names, (size_t)n, sizeof(char *), w_cmp_cstr);
  L_Str *out = L_Str_new(n);
  for (int i = 0; i < n; i++) { L_Str_push(&out, Str_from(names[i], (int64_t)strlen(names[i]))); free(names[i]); }
  free(names);
  return io_ok_ptr(out);
}
static bool Cap_fs_is_dir(WCap *fs, WStr *rel) {
  int kind; char msg[1024];
  char *p = fs_resolve(fs, rel, false, &kind, msg, sizeof msg);
  if (!p) return false;
  struct stat st;
  bool r = stat(p, &st) == 0 && S_ISDIR(st.st_mode);
  free(p);
  return r;
}
static WIo Cap_fs_remove(WCap *fs, WStr *rel) {
  if (fs->root && fs->ro) return io_err(1, "fs: this capability is read-only");
  int kind; char msg[1024];
  char *p = fs_resolve(fs, rel, false, &kind, msg, sizeof msg);
  if (!p) return io_err(kind, msg);
  if (strcmp(p, fs->root) == 0) { free(p); return io_err(1, "fs: cannot remove the granted directory itself"); }
  struct stat st;
  int rc = (lstat(p, &st) == 0 && S_ISDIR(st.st_mode)) ? rmdir(p) : unlink(p);
  free(p);
  if (rc != 0) { snprintf(msg, sizeof msg, "fs: `%.*s`: %s", (int)rel->len, rel->data, strerror(errno)); return io_err(2, msg); }
  return io_ok(NULL);
}
static WIo Cap_fs_make_dir(WCap *fs, WStr *rel) {
  if (fs->root && fs->ro) return io_err(1, "fs: this capability is read-only");
  int kind; char msg[1024];
  char *p = fs_resolve(fs, rel, true, &kind, msg, sizeof msg);
  if (!p) return io_err(kind, msg);
  int rc = mkdir(p, 0755);
  free(p);
  if (rc != 0) { snprintf(msg, sizeof msg, "fs: `%.*s`: %s", (int)rel->len, rel->data, strerror(errno)); return io_err(2, msg); }
  return io_ok(NULL);
}
static void Cap_print(WStr *s, bool nl) {
  flockfile(stdout);
  fwrite(s->data, 1, (size_t)s->len, stdout);
  if (nl) fputc('\n', stdout);
  funlockfile(stdout);
}
static int64_t Cap_now_ms(void) { struct timeval tv; gettimeofday(&tv, NULL); return (int64_t)tv.tv_sec * 1000 + tv.tv_usec / 1000; }

/* ---------- fibers and the work-stealing scheduler ----------
   Every Vela task, and main itself, runs as a fiber: a small mmap'd stack
   plus saved registers. Worker threads (one per core, VELA_THREADS to
   override) each keep a local run queue; an idle worker steals from the
   others. A fiber that must wait (for a group, a channel or a socket)
   parks: it switches back to its worker's scheduler, which then runs the
   park's "after" action (unlock a mutex, register a socket). Doing that
   after the switch means a waker can never resume a fiber that is still
   running. */

#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#define W_ASAN 1
#include <sanitizer/common_interface_defs.h>
#endif
#if __has_feature(thread_sanitizer)
#define W_TSAN 1
#include <sanitizer/tsan_interface.h>
#endif
#endif
#include <sys/mman.h>
#include <sched.h>

typedef struct WFiber WFiber;
struct WFiber {
  void *sp;
  char *map; size_t map_size;          /* mmap'd stack, NULL for a worker's own context */
  void (*entry)(void *); void *arg;
  WFiber *next;
  int park_state;                       /* W_RUNNING, W_PARKING, W_PARKED or W_NOTIFIED */
  WCatch *catch_top;                    /* innermost panic boundary */
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
__asm__(
  ".text\n.globl " W_ASM_SYM(w_ctx_switch) "\n.p2align 2\n" W_ASM_SYM(w_ctx_switch) ":\n"
  "  sub sp, sp, #160\n"
  "  stp x19, x20, [sp, #0]\n  stp x21, x22, [sp, #16]\n  stp x23, x24, [sp, #32]\n"
  "  stp x25, x26, [sp, #48]\n  stp x27, x28, [sp, #64]\n  stp x29, x30, [sp, #80]\n"
  "  stp d8, d9, [sp, #96]\n  stp d10, d11, [sp, #112]\n  stp d12, d13, [sp, #128]\n  stp d14, d15, [sp, #144]\n"
  "  mov x9, sp\n  str x9, [x0]\n  mov sp, x1\n"
  "  ldp x19, x20, [sp, #0]\n  ldp x21, x22, [sp, #16]\n  ldp x23, x24, [sp, #32]\n"
  "  ldp x25, x26, [sp, #48]\n  ldp x27, x28, [sp, #64]\n  ldp x29, x30, [sp, #80]\n"
  "  ldp d8, d9, [sp, #96]\n  ldp d10, d11, [sp, #112]\n  ldp d12, d13, [sp, #128]\n  ldp d14, d15, [sp, #144]\n"
  "  add sp, sp, #160\n  ret\n"
  ".globl " W_ASM_SYM(w_ctx_tramp) "\n.p2align 2\n" W_ASM_SYM(w_ctx_tramp) ":\n"
  "  mov x0, x19\n  bl " W_ASM_SYM(w_fiber_main) "\n  brk #0\n");
#define W_FRAME_WORDS 20
static void w_frame_init(void **frame, WFiber *f) {
  memset(frame, 0, sizeof(void *) * W_FRAME_WORDS);
  frame[0] = f;                         /* x19 -> argument of the trampoline */
  frame[11] = (void *)w_ctx_tramp;      /* x30 */
}
#elif defined(__x86_64__)
#ifdef __APPLE__
#define W_ASM_SYM(s) "_" #s
#else
#define W_ASM_SYM(s) #s
#endif
__asm__(
  ".text\n.globl " W_ASM_SYM(w_ctx_switch) "\n" W_ASM_SYM(w_ctx_switch) ":\n"
  "  pushq %rbp\n  pushq %rbx\n  pushq %r12\n  pushq %r13\n  pushq %r14\n  pushq %r15\n"
  "  movq %rsp, (%rdi)\n  movq %rsi, %rsp\n"
  "  popq %r15\n  popq %r14\n  popq %r13\n  popq %r12\n  popq %rbx\n  popq %rbp\n  ret\n"
  ".globl " W_ASM_SYM(w_ctx_tramp) "\n" W_ASM_SYM(w_ctx_tramp) ":\n"
  "  movq %r12, %rdi\n  call " W_ASM_SYM(w_fiber_main) "\n  ud2\n");
#define W_FRAME_WORDS 8
static void w_frame_init(void **frame, WFiber *f) {
  memset(frame, 0, sizeof(void *) * W_FRAME_WORDS);
  frame[3] = f;                         /* r12 */
  frame[6] = (void *)w_ctx_tramp;       /* return address */
}
#else
#error "Vela fibers need arm64 or x86_64"
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
static WWorker *w_workers[W_MAX_WORKERS];
static int w_nworkers;
static __thread WWorker *w_self_tls;
static pthread_mutex_t w_global_mu = PTHREAD_MUTEX_INITIALIZER;   /* global queue, for wakeups from non-workers */
static WFiber *w_global_head, *w_global_tail;
static int64_t w_queued;                                          /* runnable fibers in all queues */
static pthread_mutex_t w_idle_mu = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t w_idle_cv = PTHREAD_COND_INITIALIZER;
static int64_t w_nidle;
static int64_t w_io_waiters;                                      /* fibers parked on sockets */
static int w_main_done;
static size_t w_stack_size = 1024 * 1024;                         /* reserved lazily, so 10k tasks are cheap */
#define W_GUARD_BYTES (64 * 1024)                                 /* larger than any runtime stack frame */
static int64_t w_fibers_alive;                                    /* created and not yet freed */

/* A fiber may resume on another thread, so the worker is never cached
   across a switch: always read it through this function. */
__attribute__((noinline)) static WWorker *w_self(void) {
  WWorker *w = w_self_tls;
  __asm__ volatile("" ::: "memory");
  return w;
}

static void w_switch(WFiber *from, WFiber *to, int from_exiting) {
#ifdef W_ASAN
  void *fake = NULL;
  __sanitizer_start_switch_fiber(from_exiting ? NULL : &fake, to->asan_bottom, to->asan_size);
#else
  (void)from_exiting;
#endif
#ifdef W_TSAN
  __tsan_switch_to_fiber(to->tsan, 0);
#endif
  w_ctx_switch(&from->sp, to->sp);
#ifdef W_ASAN
  {
    WWorker *w = w_self();
    if (from == &w->sched) __sanitizer_finish_switch_fiber(fake, NULL, NULL);
    else __sanitizer_finish_switch_fiber(fake, &w->sched.asan_bottom, &w->sched.asan_size);
  }
#endif
}

static WFiber *w_fiber_new(void (*entry)(void *), void *arg, size_t size) {
  long page = sysconf(_SC_PAGESIZE);
  size = (size + (size_t)page - 1) / (size_t)page * (size_t)page;
  size_t guard = (W_GUARD_BYTES + (size_t)page - 1) / (size_t)page * (size_t)page;
  size_t total = size + guard;          /* guard region at the low end */
  char *map = (char *)mmap(NULL, total, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
  if (map == MAP_FAILED) { fputs("panic: cannot allocate a task stack\n", stderr); exit(1); }
  if (mprotect(map, guard, PROT_NONE) != 0) { fputs("panic: cannot protect a task stack\n", stderr); exit(1); }
  WFiber *f = (WFiber *)w_alloc(sizeof(WFiber));
  memset(f, 0, sizeof *f);
  f->map = map; f->map_size = total; f->entry = entry; f->arg = arg;
  __atomic_fetch_add(&w_fibers_alive, 1, __ATOMIC_RELAXED);
  char *top = map + total;
  top = (char *)((uintptr_t)top & ~(uintptr_t)15);
  void **frame = (void **)(top - sizeof(void *) * W_FRAME_WORDS);
  w_frame_init(frame, f);
  f->sp = frame;
#ifdef W_ASAN
  f->asan_bottom = map + guard; f->asan_size = size;
#endif
#ifdef W_TSAN
  f->tsan = __tsan_create_fiber(0);
#endif
  return f;
}

static void w_fiber_free(void *p) {
  WFiber *f = (WFiber *)p;
#ifdef W_TSAN
  __tsan_destroy_fiber(f->tsan);
#endif
  munmap(f->map, f->map_size);
  w_free(f);
  __atomic_fetch_sub(&w_fibers_alive, 1, __ATOMIC_RELEASE);
}

/* ---- run queues ---- */

static void w_wake_one(void) {
  if (__atomic_load_n(&w_nidle, __ATOMIC_SEQ_CST) > 0) {
    pthread_mutex_lock(&w_idle_mu);
    pthread_cond_signal(&w_idle_cv);
    pthread_mutex_unlock(&w_idle_mu);
  }
}

/* Make a fiber runnable: on the caller's own queue if it is a worker,
   otherwise on the global queue. */
static void w_ready(WFiber *f) {
  WWorker *w = w_self();
  f->next = NULL;
  if (w) {
    pthread_mutex_lock(&w->mu);
    if (w->tail - w->head == w->cap) {
      WFiber **q = (WFiber **)w_raw(sizeof(WFiber *) * (size_t)w->cap * 2);
      for (int64_t i = w->head; i < w->tail; i++) q[i - w->head] = w->q[i % w->cap];
      free(w->q); w->q = q; w->tail -= w->head; w->head = 0; w->cap *= 2;
    }
    w->q[w->tail % w->cap] = f; w->tail++;
    pthread_mutex_unlock(&w->mu);
  } else {
    pthread_mutex_lock(&w_global_mu);
    if (w_global_tail) w_global_tail->next = f; else __atomic_store_n(&w_global_head, f, __ATOMIC_RELEASE);
    w_global_tail = f;
    pthread_mutex_unlock(&w_global_mu);
  }
  __atomic_fetch_add(&w_queued, 1, __ATOMIC_SEQ_CST);
  w_wake_one();
}

static WFiber *w_take(WWorker *w, int own) {
  WFiber *f = NULL;
  pthread_mutex_lock(&w->mu);
  if (w->tail > w->head) {
    if (own) { w->tail--; f = w->q[w->tail % w->cap]; }   /* newest first: good locality for fork/join */
    else { f = w->q[w->head % w->cap]; w->head++; }       /* thieves take the oldest */
  }
  pthread_mutex_unlock(&w->mu);
  if (f) __atomic_fetch_sub(&w_queued, 1, __ATOMIC_SEQ_CST);
  return f;
}

static WFiber *w_find_work(WWorker *w) {
  WFiber *f = w_take(w, 1);
  if (f) return f;
  if (__atomic_load_n(&w_global_head, __ATOMIC_RELAXED)) {
    pthread_mutex_lock(&w_global_mu);
    f = w_global_head;
    if (f) { __atomic_store_n(&w_global_head, f->next, __ATOMIC_RELEASE); if (!f->next) w_global_tail = NULL; }
    pthread_mutex_unlock(&w_global_mu);
    if (f) { __atomic_fetch_sub(&w_queued, 1, __ATOMIC_SEQ_CST); return f; }
  }
  int n = __atomic_load_n(&w_nworkers, __ATOMIC_ACQUIRE);
  for (int k = 1; k < n; k++) {
    WWorker *v = w_workers[(w->id + k) % n];
    if (v && (f = w_take(v, 0))) return f;
  }
  return NULL;
}

static void w_sched_loop(WWorker *w, int is_main) {
  for (;;) {
    if (is_main && __atomic_load_n(&w_main_done, __ATOMIC_ACQUIRE)) return;
    WFiber *f = w_find_work(w);
    if (!f) {
      pthread_mutex_lock(&w_idle_mu);
      int64_t idle = __atomic_add_fetch(&w_nidle, 1, __ATOMIC_SEQ_CST);
      if (__atomic_load_n(&w_queued, __ATOMIC_SEQ_CST) == 0 && !__atomic_load_n(&w_main_done, __ATOMIC_ACQUIRE)) {
        /* Every worker idle, nothing runnable, nothing waiting on I/O: no
           task can ever wake another. Confirm after a short wait, then
           report the deadlock instead of hanging. */
        if (idle == __atomic_load_n(&w_nworkers, __ATOMIC_ACQUIRE) && __atomic_load_n(&w_io_waiters, __ATOMIC_SEQ_CST) == 0) {
          struct timeval tv; gettimeofday(&tv, NULL);
          struct timespec ts = { tv.tv_sec, (long)tv.tv_usec * 1000 + 100000000L };
          if (ts.tv_nsec >= 1000000000L) { ts.tv_sec++; ts.tv_nsec -= 1000000000L; }
          int rc = pthread_cond_timedwait(&w_idle_cv, &w_idle_mu, &ts);
          if (rc == ETIMEDOUT && __atomic_load_n(&w_queued, __ATOMIC_SEQ_CST) == 0
              && __atomic_load_n(&w_nidle, __ATOMIC_SEQ_CST) == __atomic_load_n(&w_nworkers, __ATOMIC_ACQUIRE)
              && __atomic_load_n(&w_io_waiters, __ATOMIC_SEQ_CST) == 0 && !__atomic_load_n(&w_main_done, __ATOMIC_ACQUIRE))
            w_fatal_c("all tasks are blocked (deadlock)", "every task is waiting");
        } else
          pthread_cond_wait(&w_idle_cv, &w_idle_mu);
      }
      __atomic_fetch_sub(&w_nidle, 1, __ATOMIC_SEQ_CST);
      pthread_mutex_unlock(&w_idle_mu);
      continue;
    }
    w->current = f;
    w_switch(&w->sched, f, 0);
    w->current = NULL;
    if (w->after) {
      void (*a)(void *) = w->after;
      w->after = NULL;
      a(w->after_arg);
    }
  }
}

static WWorker *w_worker_new(void) {
  WWorker *w = (WWorker *)w_raw(sizeof(WWorker));
  memset(w, 0, sizeof *w);
  pthread_mutex_init(&w->mu, NULL);
  w->cap = 64;
  w->q = (WFiber **)w_raw(sizeof(WFiber *) * (size_t)w->cap);
#ifdef W_TSAN
  w->sched.tsan = __tsan_get_current_fiber();
#endif
  w->id = __atomic_fetch_add(&w_nworkers, 0, __ATOMIC_SEQ_CST);
  w_workers[w->id] = w;
  __atomic_fetch_add(&w_nworkers, 1, __ATOMIC_RELEASE);
  w_self_tls = w;
  return w;
}

static void *w_worker_thread(void *arg) {
  (void)arg;
  pthread_mutex_lock(&w_global_mu);
  WWorker *w = w_worker_new();
  pthread_mutex_unlock(&w_global_mu);
  w_sched_loop(w, 0);
  return NULL;
}

static pthread_once_t w_pool_once = PTHREAD_ONCE_INIT;
static void w_pool_start(void) {
  long n = sysconf(_SC_NPROCESSORS_ONLN);
  const char *e = getenv("VELA_THREADS");
  if (e) n = atol(e);
  if (n > W_MAX_WORKERS) n = W_MAX_WORKERS;
  for (long i = 1; i < n; i++) {
    pthread_t th;
    pthread_create(&th, NULL, w_worker_thread, NULL);
    pthread_detach(th);
  }
}

/* Park the current fiber; [after] runs on the scheduler once it is off its stack. */
static void w_park(void (*after)(void *), void *arg) {
  WWorker *w = w_self();
  WFiber *f = w->current;
  w->after = after; w->after_arg = arg;
  w_switch(f, &w->sched, 0);
}

/* Waiting protocol. The fiber calls w_prepare_park(), registers itself
   wherever a waker will find it (under that structure's own lock), releases
   the lock, then calls w_park_now(). A waker calls w_wake(f). Whichever of
   "the fiber is fully parked" and "the wakeup" comes second makes the fiber
   runnable, so no wakeup is lost and no fiber is resumed while it still runs.
   Locks are never held across a switch. */
static WFiber *w_current(void) { return w_self()->current; }
/* Through w_self(), never the thread-local directly: a fiber can resume on
   another worker thread, and an inlined TLS address would be stale. */
__attribute__((noinline)) static WCatch **w_catch_slot(void) {
  WWorker *w = w_self();
  if (!w || !w->current) return NULL;
  return &w->current->catch_top;
}
static void w_prepare_park(void) { __atomic_store_n(&w_current()->park_state, W_PARKING, __ATOMIC_RELEASE); }
static void w_park_commit(void *p) {
  WFiber *f = (WFiber *)p;
  int expect = W_PARKING;
  if (!__atomic_compare_exchange_n(&f->park_state, &expect, W_PARKED, 0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) {
    __atomic_store_n(&f->park_state, W_RUNNING, __ATOMIC_RELEASE);   /* woken before it finished parking */
    w_ready(f);
  }
}
static void w_park_now(void) { WFiber *f = w_current(); w_park(w_park_commit, f); }
static void w_wake(WFiber *f) {
  int old = __atomic_exchange_n(&f->park_state, W_NOTIFIED, __ATOMIC_ACQ_REL);
  if (old == W_PARKED) {
    __atomic_store_n(&f->park_state, W_RUNNING, __ATOMIC_RELEASE);
    w_ready(f);
  }
}

void w_fiber_main(WFiber *f) {
#ifdef W_ASAN
  {
    WWorker *w = w_self();
    __sanitizer_finish_switch_fiber(NULL, &w->sched.asan_bottom, &w->sched.asan_size);
  }
#endif
  f->entry(f->arg);
  WWorker *w = w_self();
  w->after = w_fiber_free; w->after_arg = f;
  w_switch(f, &w->sched, 1);
  __builtin_unreachable();
}

/* Run [entry] as the main fiber; return when it finishes. */
static void w_run(void (*entry)(void *), void *arg) {
  const char *e = getenv("VELA_STACK_KB");
  if (e && atol(e) > 0) w_stack_size = (size_t)atol(e) * 1024;
  pthread_mutex_lock(&w_global_mu);
  WWorker *w = w_worker_new();
  pthread_mutex_unlock(&w_global_mu);
  WFiber *m = w_fiber_new(entry, arg, (size_t)256 << 20); /* main gets a large, lazily committed stack */
  w_ready(m);
  w_sched_loop(w, 1);
  /* main has returned, so every task has finished; wait for the workers to
     finish freeing the last fibers they switched off */
  while (__atomic_load_n(&w_fibers_alive, __ATOMIC_ACQUIRE) != 0) sched_yield();
}
static void w_main_finished(void) {
  pthread_mutex_lock(&w_idle_mu);
  __atomic_store_n(&w_main_done, 1, __ATOMIC_RELEASE);
  pthread_cond_broadcast(&w_idle_cv);
  pthread_mutex_unlock(&w_idle_mu);
}

/* ---------- group / go ---------- */

typedef struct WGroup WGroup;
typedef struct WTask WTask;
struct WTask { WClo *fn; void (*run)(WTask *); WGroup *g; char *pmsg; char *pwhere; max_align_t slot[]; };
struct WGroup { int64_t rc; pthread_mutex_t mu; int64_t pending; WFiber *waiter; int64_t n, cap; WTask **tasks; };

static WGroup *w_group_new(void) {
  WGroup *g = (WGroup *)w_alloc_k(sizeof(WGroup), WK_GROUP);
  g->rc = 1; g->pending = 0; g->waiter = NULL; g->n = 0; g->cap = 8;
  pthread_mutex_init(&g->mu, NULL);
  g->tasks = (WTask **)w_raw(sizeof(WTask *) * (size_t)g->cap);
  return g;
}
static void w_task_entry(void *p) {
  WTask *volatile t = (WTask *)p;
  WGroup *g = t->g;
  /* a panic ends only this task; the group passes it on when it finishes */
  WCatch c;
  w_catch_init(&c);
  c.prev = NULL;
  WCatch **slot = w_catch_slot();
  if (_setjmp(c.jb) == 0) {
    WCatch *guard __attribute__((cleanup(w_catch_land))) = &c;
    *slot = &c;
    t->run(t);
    *slot = NULL;
    /* the result was shared by run(); anything else this task allocated and
       that is still alive is garbage (a panic it caught may have left some) */
    w_catch_release(&c);
  } else {
    w_catch_release(&c);
    Clo_drop(t->fn);   /* run() did not get to drop it */
    t->pmsg = c.msg;
    t->pwhere = c.where;
  }
  pthread_mutex_lock(&g->mu);
  WFiber *wake = NULL;
  if (--g->pending == 0) { wake = g->waiter; g->waiter = NULL; }
  pthread_mutex_unlock(&g->mu);
  if (wake) w_wake(wake);
}
/* The closure and everything it references become shared before any other
   thread can see them. */
static void w_spawn(WGroup *g, WClo *fn, void (*run)(WTask *), size_t slot) {
  pthread_once(&w_pool_once, w_pool_start);
  Clo_share(fn);
  WTask *t = (WTask *)w_alloc(sizeof(WTask) + slot);
  t->fn = fn; t->run = run; t->g = g; t->pmsg = NULL; t->pwhere = NULL;
  if (g->n == g->cap) { g->cap *= 2; g->tasks = (WTask **)w_realloc(g->tasks, w_bytes(g->cap, sizeof(WTask *))); }
  g->tasks[g->n++] = t;
  pthread_mutex_lock(&g->mu);
  g->pending++;
  pthread_mutex_unlock(&g->mu);
  w_ready(w_fiber_new(w_task_entry, t, w_stack_size));
}
static void w_group_wait(WGroup *g) {
  pthread_mutex_lock(&g->mu);
  if (g->pending == 0) { pthread_mutex_unlock(&g->mu); return; }
  w_prepare_park();
  g->waiter = w_current();
  pthread_mutex_unlock(&g->mu);
  w_park_now();
  /* resumed by the last task; w_wake's atomics order its writes before ours */
}
static void w_group_release_tasks(WGroup *g) {
  for (int64_t i = 0; i < g->n; i++) { free(g->tasks[i]->pmsg); free(g->tasks[i]->pwhere); w_free(g->tasks[i]); }
  g->n = 0;
}
/* After every task finished: pass on the first panic, in spawn order. */
static void w_group_reraise(WGroup *g) {
  for (int64_t i = 0; i < g->n; i++)
    if (g->tasks[i]->pmsg) {
      char m[1024], w[512];
      snprintf(m, sizeof m, "%s", g->tasks[i]->pmsg);
      snprintf(w, sizeof w, "%s", g->tasks[i]->pwhere);
      w_group_release_tasks(g);
      w_panic_c(m, w);
    }
}
static inline WGroup *Group_dup(WGroup *g) { g->rc++; return g; }
/* The last reference can go before the join when the group's body panics:
   its tasks may still be running, so wait for them before freeing. */
static void Group_drop(WGroup *g) {
  if (--g->rc == 0) {
    w_group_wait(g);
    w_group_release_tasks(g);
    pthread_mutex_destroy(&g->mu); free(g->tasks); w_free(g);
  }
}
static inline void Group_share(WGroup *g) { (void)g; }

/* ---------- channels ----------
   A bounded queue between tasks. send blocks while the channel is full,
   recv blocks while it is empty; after close, recv drains what is left and
   then returns nothing, and send panics. Values are switched to shared
   before they enter, because the receiver may be on another thread. */

/* A waiting task. A `select` puts one on every channel it waits on, all
   pointing to one WSel; the first channel to act claims it (done 0 -> 1) and
   the others then skip those waiters as stale. */
typedef struct WSel { int done; int fired; } WSel;
typedef struct WChWait { WFiber *f; void *val; int ok; struct WChWait *next; WSel *sel; int idx; } WChWait;
typedef struct WChan {
  int64_t rc; pthread_mutex_t mu;
  int64_t cap, len, head; size_t esz; char *buf; int closed;
  WChWait *sq, *sq_t, *rq, *rq_t;                       /* waiting senders and receivers */
  void (*edrop)(void *); void (*eshare)(void *);
} WChan;

static WChan *w_chan_new(int64_t cap, size_t esz, void (*edrop)(void *), void (*eshare)(void *), const char *where) {
  if (cap < 1) w_panic_c("channel capacity must be at least 1", where);
  WChan *c = (WChan *)w_alloc_k(sizeof(WChan), WK_CHAN);
  memset(c, 0, sizeof *c);
  c->rc = 1; c->cap = cap; c->esz = esz ? esz : 1;
  c->buf = (char *)w_raw(w_bytes(cap, c->esz));
  c->edrop = edrop; c->eshare = eshare;
  pthread_mutex_init(&c->mu, NULL);
  return c;
}
static inline WChan *Chan_dup(WChan *c) { w_rc_inc(&c->rc); return c; }
static void Chan_drop(WChan *c) {
  if (!w_rc_dec(&c->rc)) return;
  if (c->edrop) for (int64_t i = 0; i < c->len; i++) c->edrop(c->buf + ((c->head + i) % c->cap) * c->esz);
  free(c->buf);
  pthread_mutex_destroy(&c->mu);
  w_free(c);
}
static inline void Chan_share(WChan *c) { w_rc_share(&c->rc); }
static inline bool Chan_eq(WChan *a, WChan *b) { return a == b; }
static void Chan_show(WBuf *b, WChan *c, bool top) { (void)c; (void)top; buf_puts(b, "<chan>"); }

static void w_chq_push(WChWait **h, WChWait **t, WChWait *w) { w->next = NULL; if (*t) (*t)->next = w; else *h = w; *t = w; }
static WChWait *w_chq_pop(WChWait **h, WChWait **t) { WChWait *w = *h; if (w) { *h = w->next; if (!*h) *t = NULL; } return w; }
/* The next waiter that can still be woken: a plain one, or a select this
   call manages to claim. Stale select waiters are dropped. */
static WChWait *w_chq_pop_live(WChWait **h, WChWait **t) {
  WChWait *w;
  while ((w = w_chq_pop(h, t))) {
    if (!w->sel) return w;
    int zero = 0;
    if (__atomic_compare_exchange_n(&w->sel->done, &zero, 1, 0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) {
      w->sel->fired = w->idx;
      return w;
    }
  }
  return NULL;
}
static void w_chq_remove(WChWait **h, WChWait **t, WChWait *w) {
  WChWait *prev = NULL;
  for (WChWait *p = *h; p; prev = p, p = p->next)
    if (p == w) {
      if (prev) prev->next = p->next; else *h = p->next;
      if (*t == p) *t = prev;
      return;
    }
}

static void w_chan_send(WChan *c, void *v, const char *where) {
  if (c->eshare) c->eshare(v);
  pthread_mutex_lock(&c->mu);
  if (c->closed) { pthread_mutex_unlock(&c->mu); w_panic_c("send on a closed channel", where); }
  WChWait *r = w_chq_pop_live(&c->rq, &c->rq_t);
  if (r) {                                       /* a receiver is waiting: hand the value over */
    memcpy(r->val, v, c->esz); r->ok = 1;
    pthread_mutex_unlock(&c->mu);
    w_wake(r->f);
    return;
  }
  if (c->len < c->cap) {
    memcpy(c->buf + ((c->head + c->len) % c->cap) * c->esz, v, c->esz);
    c->len++;
    pthread_mutex_unlock(&c->mu);
    return;
  }
  WChWait me = { w_current(), v, 0, NULL, NULL, 0 };
  w_prepare_park();
  w_chq_push(&c->sq, &c->sq_t, &me);
  pthread_mutex_unlock(&c->mu);
  w_park_now();
  if (!me.ok) w_panic_c("send on a closed channel", where);
}

static bool w_chan_recv(WChan *c, void *out) {
  pthread_mutex_lock(&c->mu);
  if (c->len > 0) {
    memcpy(out, c->buf + c->head * c->esz, c->esz);
    c->head = (c->head + 1) % c->cap; c->len--;
    WChWait *s = w_chq_pop_live(&c->sq, &c->sq_t);
    if (s) {                                     /* a sender was waiting for room */
      memcpy(c->buf + ((c->head + c->len) % c->cap) * c->esz, s->val, c->esz);
      c->len++; s->ok = 1;
      pthread_mutex_unlock(&c->mu);
      w_wake(s->f);
      return true;
    }
    pthread_mutex_unlock(&c->mu);
    return true;
  }
  if (c->closed) { pthread_mutex_unlock(&c->mu); return false; }
  WChWait me = { w_current(), out, 0, NULL, NULL, 0 };
  w_prepare_park();
  w_chq_push(&c->rq, &c->rq_t, &me);
  pthread_mutex_unlock(&c->mu);
  w_park_now();
  return me.ok;
}

static void w_chan_close(WChan *c, const char *where) {
  pthread_mutex_lock(&c->mu);
  if (c->closed) { pthread_mutex_unlock(&c->mu); w_panic_c("close of a closed channel", where); }
  c->closed = 1;
  /* claim every waiter now, under the lock; wake them after it */
  WChWait *wake = NULL, *w;
  while ((w = w_chq_pop_live(&c->rq, &c->rq_t))) { w->ok = 0; w->next = wake; wake = w; }
  while ((w = w_chq_pop_live(&c->sq, &c->sq_t))) { w->ok = 0; w->next = wake; wake = w; }
  pthread_mutex_unlock(&c->mu);
  while (wake) { WChWait *n = wake->next; w_wake(wake->f); wake = n; }
}

/* ---------- releasing what a panic abandoned ---------- */

typedef struct { int64_t rc, len, cap; void *data; } WListAny;
typedef struct { int64_t rc, len, n, cap, icap; void *keys, *vals, *live, *idx; } WMapAny;

#define W_DYING (INT64_MAX / 4)

static void w_catch_release(WCatch *c) {
  WHdr *end = &c->objs;
  /* 1. nothing abandoned may be freed by the decrements below */
  for (WHdr *h = end->next; h != end; h = h->next) *(int64_t *)(h + 1) = W_DYING;
  /* 2. give back the references they hold, the normal way: outside objects
     get their counts back (and may be freed), abandoned ones stay */
  for (WHdr *h = end->next; h != end; h = h->next) {
    void *o = h + 1;
    if (h->kids) h->kids(o);
    if (h->kind == WK_CHAN) {
      WChan *ch = (WChan *)o;
      if (ch->edrop) for (int64_t i = 0; i < ch->len; i++) ch->edrop(ch->buf + ((ch->head + i) % ch->cap) * ch->esz);
    }
    if (h->kind == WK_GROUP) w_group_wait((WGroup *)o); /* its tasks still run: let them finish */
  }
  /* 3. free them */
  while (end->next != end) {
    WHdr *h = end->next;
    w_hdr_unlink(h);
    void *o = h + 1;
    switch (h->kind) {
      case WK_LIST: w_free(((WListAny *)o)->data); w_free(o); break;
      case WK_MAP: {
        WMapAny *m = (WMapAny *)o;
        w_free(m->keys); w_free(m->vals); w_free(m->live); w_free(m->idx); w_free(o);
        break;
      }
      case WK_CAP: Cap_release((WCap *)o); break;
      case WK_CHAN: {
        WChan *ch = (WChan *)o;
        free(ch->buf);
        pthread_mutex_destroy(&ch->mu);
        w_free(o);
        break;
      }
      case WK_GROUP: {
        WGroup *g = (WGroup *)o;
        w_group_release_tasks(g);
        pthread_mutex_destroy(&g->mu);
        free(g->tasks);
        w_free(o);
        break;
      }
      default: w_free(o); break;
    }
  }
}

/* ---------- select ----------
   ops[i] is a receive (val: where to store the value) or a send (val: the
   value). Returns the index of the operation done, with ops[i].ok set (a
   receive: 1 if a value came, 0 if the channel is closed and empty), or -1
   when nothing was ready and the select has a `_` arm. The first ready
   operation in order wins. */
typedef struct WSelOp { WChan *c; int send; void *val; int ok; int on; } WSelOp;

/* Try one operation with its channel locked. Returns 1 if done; *wake gets
   a fiber to wake after unlocking. A send on a closed channel returns -1. */
static int w_sel_try(WSelOp *op, WFiber **wake) {
  WChan *c = op->c;
  if (op->send) {
    if (c->closed) return -1;
    WChWait *r = w_chq_pop_live(&c->rq, &c->rq_t);
    if (r) { memcpy(r->val, op->val, c->esz); r->ok = 1; *wake = r->f; op->ok = 1; return 1; }
    if (c->len < c->cap) {
      memcpy(c->buf + ((c->head + c->len) % c->cap) * c->esz, op->val, c->esz);
      c->len++; op->ok = 1;
      return 1;
    }
    return 0;
  }
  if (c->len > 0) {
    memcpy(op->val, c->buf + c->head * c->esz, c->esz);
    c->head = (c->head + 1) % c->cap; c->len--;
    WChWait *s = w_chq_pop_live(&c->sq, &c->sq_t);
    if (s) {
      memcpy(c->buf + ((c->head + c->len) % c->cap) * c->esz, s->val, c->esz);
      c->len++; s->ok = 1; *wake = s->f;
    }
    op->ok = 1;
    return 1;
  }
  if (c->closed) { op->ok = 0; return 1; }
  return 0;
}
static int w_chan_cmp(const void *a, const void *b) {
  uintptr_t x = (uintptr_t)*(WChan *const *)a, y = (uintptr_t)*(WChan *const *)b;
  return x < y ? -1 : x > y;
}
/* every distinct channel of the select, locked in address order */
static int w_sel_lock(WSelOp *ops, int n, WChan **cs) {
  int m = 0;
  for (int i = 0; i < n; i++) if (ops[i].on) cs[m++] = ops[i].c;
  qsort(cs, (size_t)m, sizeof(WChan *), w_chan_cmp);
  int k = 0;
  for (int i = 0; i < m; i++) if (k == 0 || cs[k - 1] != cs[i]) cs[k++] = cs[i];
  for (int i = 0; i < k; i++) pthread_mutex_lock(&cs[i]->mu);
  return k;
}
static void w_sel_unlock(WChan **cs, int k) { for (int i = k - 1; i >= 0; i--) pthread_mutex_unlock(&cs[i]->mu); }

static int w_select(WSelOp *ops, int n, int has_default, const char *where) {
  int any = 0;
  for (int i = 0; i < n; i++) if (ops[i].on) any = 1;
  if (!any && !has_default) w_panic_c("every arm of this `select` is turned off by its guard", where);
  for (int i = 0; i < n; i++) if (ops[i].on && ops[i].send && ops[i].c->eshare) ops[i].c->eshare(ops[i].val);
  /* fast path: one channel at a time, in order */
  for (int i = 0; i < n; i++) {
    if (!ops[i].on) continue;
    WFiber *wake = NULL;
    pthread_mutex_lock(&ops[i].c->mu);
    int r = w_sel_try(&ops[i], &wake);
    pthread_mutex_unlock(&ops[i].c->mu);
    if (r < 0) w_panic_c("send on a closed channel", where);
    if (r) { if (wake) w_wake(wake); return i; }
  }
  if (has_default) return -1;
  /* slow path: lock them all, try once more, else wait on every one */
  WChan **cs = (WChan **)w_raw(sizeof(WChan *) * (size_t)n);
  WChWait *ws = (WChWait *)w_raw(sizeof(WChWait) * (size_t)n);
  int k = w_sel_lock(ops, n, cs);
  for (int i = 0; i < n; i++) {
    if (!ops[i].on) continue;
    WFiber *wake = NULL;
    int r = w_sel_try(&ops[i], &wake);
    if (r != 0) {
      w_sel_unlock(cs, k);
      free(cs); free(ws);
      if (r < 0) w_panic_c("send on a closed channel", where);
      if (wake) w_wake(wake);
      return i;
    }
  }
  WSel sel = { 0, -1 };
  WFiber *me = w_current();
  w_prepare_park();
  for (int i = 0; i < n; i++) {
    if (!ops[i].on) continue;
    ws[i].f = me; ws[i].val = ops[i].val; ws[i].ok = 0; ws[i].sel = &sel; ws[i].idx = i;
    if (ops[i].send) w_chq_push(&ops[i].c->sq, &ops[i].c->sq_t, &ws[i]);
    else w_chq_push(&ops[i].c->rq, &ops[i].c->rq_t, &ws[i]);
  }
  w_sel_unlock(cs, k);
  w_park_now();
  /* woken by the channel that claimed us; take our other waiters back */
  k = w_sel_lock(ops, n, cs);
  for (int i = 0; i < n; i++) {
    if (!ops[i].on) continue;
    if (ops[i].send) w_chq_remove(&ops[i].c->sq, &ops[i].c->sq_t, &ws[i]);
    else w_chq_remove(&ops[i].c->rq, &ops[i].c->rq_t, &ws[i]);
  }
  w_sel_unlock(cs, k);
  int i = __atomic_load_n(&sel.fired, __ATOMIC_ACQUIRE);
  ops[i].ok = ws[i].ok;
  free(cs); free(ws);
  if (ops[i].send && !ops[i].ok) w_panic_c("send on a closed channel", where);
  return i;
}

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

static int w_poll_fd = -1;
static pthread_once_t w_poll_once = PTHREAD_ONCE_INIT;

/* Per-descriptor waiters: one reader slot and one writer slot. Each
   descriptor is registered once, edge-triggered, for both directions; an
   event with nobody waiting sets a ready flag, so a task that registers
   just after the event still sees it. */
#define W_MAX_FD 65536
typedef struct { WFiber *r, *w; uint8_t rready, wready, registered; } WFdState;
static WFdState w_fds[W_MAX_FD];
#define W_FD_LOCKS 64
static pthread_mutex_t w_fd_mu[W_FD_LOCKS];
static pthread_mutex_t *w_fd_lock(int fd) { return &w_fd_mu[fd % W_FD_LOCKS]; }

static void w_fd_event(int fd, int readable, int writable) {
  if (fd < 0 || fd >= W_MAX_FD) return;
  WFdState *st = &w_fds[fd];
  WFiber *wr = NULL, *ww = NULL;
  pthread_mutex_lock(w_fd_lock(fd));
  if (readable) { if (st->r) { wr = st->r; st->r = NULL; } else st->rready = 1; }
  if (writable) { if (st->w) { ww = st->w; st->w = NULL; } else st->wready = 1; }
  pthread_mutex_unlock(w_fd_lock(fd));
  if (wr) w_wake(wr);
  if (ww) w_wake(ww);
}

static void *w_poller(void *arg) {
  (void)arg;
  for (;;) {
#ifdef W_KQUEUE
    struct kevent evs[256];
    int n = kevent(w_poll_fd, NULL, 0, evs, 256, NULL);
    for (int i = 0; i < n; i++) {
      int fd = (int)evs[i].ident;
      int bad = (evs[i].flags & (EV_EOF | EV_ERROR)) != 0;
      w_fd_event(fd, evs[i].filter == EVFILT_READ || bad, evs[i].filter == EVFILT_WRITE || bad);
    }
#else
    struct epoll_event evs[256];
    int n = epoll_wait(w_poll_fd, evs, 256, -1);
    for (int i = 0; i < n; i++) {
      uint32_t e = evs[i].events;
      w_fd_event(evs[i].data.fd, (e & (EPOLLIN | EPOLLRDHUP | EPOLLHUP | EPOLLERR)) != 0, (e & (EPOLLOUT | EPOLLHUP | EPOLLERR)) != 0);
    }
#endif
  }
  return NULL;
}
static void w_poll_start(void) {
  for (int i = 0; i < W_FD_LOCKS; i++) pthread_mutex_init(&w_fd_mu[i], NULL);
#ifdef W_KQUEUE
  w_poll_fd = kqueue();
#else
  w_poll_fd = epoll_create1(EPOLL_CLOEXEC);
#endif
  if (w_poll_fd < 0) { fputs("panic: cannot create the socket poller\n", stderr); exit(1); }
  pthread_t th;
  pthread_create(&th, NULL, w_poller, NULL);
  pthread_detach(th);
}

/* Park until fd may be ready in that direction. Returns -1 if another task
   is already waiting on it in that direction (or fd is out of range). */
static int w_wait_fd(int fd, int write) {
  if (fd < 0 || fd >= W_MAX_FD) return -1;
  pthread_once(&w_poll_once, w_poll_start);
  WFdState *st = &w_fds[fd];
  pthread_mutex_lock(w_fd_lock(fd));
  if (!st->registered) {
#ifdef W_KQUEUE
    struct kevent ev[2];
    EV_SET(&ev[0], (uintptr_t)fd, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, NULL);
    EV_SET(&ev[1], (uintptr_t)fd, EVFILT_WRITE, EV_ADD | EV_CLEAR, 0, 0, NULL);
    kevent(w_poll_fd, ev, 2, NULL, 0, NULL);
#else
    struct epoll_event ev;
    ev.events = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLET;
    ev.data.fd = fd;
    epoll_ctl(w_poll_fd, EPOLL_CTL_ADD, fd, &ev);
#endif
    st->registered = 1;
  }
  uint8_t *ready = write ? &st->wready : &st->rready;
  WFiber **slot = write ? &st->w : &st->r;
  if (*ready) { *ready = 0; pthread_mutex_unlock(w_fd_lock(fd)); return 0; }
  if (*slot) { pthread_mutex_unlock(w_fd_lock(fd)); return -1; }
  __atomic_fetch_add(&w_io_waiters, 1, __ATOMIC_SEQ_CST);
  w_prepare_park();
  *slot = w_current();
  pthread_mutex_unlock(w_fd_lock(fd));
  w_park_now();
  __atomic_fetch_sub(&w_io_waiters, 1, __ATOMIC_SEQ_CST);
  return 0;
}

/* Wake whoever waits on fd (used when a socket is closed under them). */
static void w_fd_wake_all(int fd) { w_fd_event(fd, 1, 1); }

/* Forget fd's waiter state and close it, so a new socket that reuses the
   number starts clean. */
static void w_sock_release_fd(int fd) {
  if (fd >= 0 && fd < W_MAX_FD && __atomic_load_n(&w_poll_fd, __ATOMIC_ACQUIRE) >= 0) {
    pthread_mutex_lock(w_fd_lock(fd));
    memset(&w_fds[fd], 0, sizeof(WFdState));
    pthread_mutex_unlock(w_fd_lock(fd));
  }
  if (fd >= 0) close(fd);
}

/* Socket operations are bracketed by enter/leave. close() shuts the socket
   down at once (waking blocked tasks, which then see `closing`), but the
   descriptor is only released when no operation is in progress, so its
   number cannot be reused while a task still holds it. */
static int w_sock_enter(WCap *c) {
  __atomic_fetch_add(&c->inflight, 1, __ATOMIC_SEQ_CST);
  if (__atomic_load_n(&c->closing, __ATOMIC_SEQ_CST)) { __atomic_fetch_sub(&c->inflight, 1, __ATOMIC_SEQ_CST); return -1; }
  return __atomic_load_n(&c->fd, __ATOMIC_SEQ_CST);
}
static void w_sock_really_close(WCap *c) {
  int fd = __atomic_exchange_n(&c->fd, -1, __ATOMIC_SEQ_CST);
  if (fd >= 0) w_sock_release_fd(fd);
}
static void w_sock_leave(WCap *c) {
  if (__atomic_sub_fetch(&c->inflight, 1, __ATOMIC_SEQ_CST) == 0 && __atomic_load_n(&c->closing, __ATOMIC_SEQ_CST))
    w_sock_really_close(c);
}
static int w_sock_closing(WCap *c) { return __atomic_load_n(&c->closing, __ATOMIC_SEQ_CST); }
static void Cap_sock_close(WCap *c) {
  if (__atomic_exchange_n(&c->closing, 1, __ATOMIC_SEQ_CST)) return;
  int fd = __atomic_load_n(&c->fd, __ATOMIC_SEQ_CST);
  if (fd >= 0) {
    shutdown(fd, SHUT_RDWR);
    if (fd < W_MAX_FD && __atomic_load_n(&w_poll_fd, __ATOMIC_ACQUIRE) >= 0) w_fd_wake_all(fd);
  }
  if (__atomic_load_n(&c->inflight, __ATOMIC_SEQ_CST) == 0) w_sock_really_close(c);
}

static void w_nonblock(int fd) {
  fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
  fcntl(fd, F_SETFD, FD_CLOEXEC);
#ifdef SO_NOSIGPIPE
  int one = 1;
  setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof one);
#endif
}

/* "host:port", host of [A-Za-z0-9.-], port 0..65535 (same rules as the interpreter). */
static int w_parse_addr(WStr *a, char *host, size_t hn, int *port) {
  int64_t colon = -1;
  for (int64_t i = 0; i < a->len; i++) if (a->data[i] == ':') { if (colon >= 0) return 0; colon = i; }
  if (colon <= 0 || colon >= a->len - 1 || a->len - colon - 1 > 5 || (size_t)colon >= hn) return 0;
  for (int64_t i = 0; i < colon; i++) {
    char ch = a->data[i];
    if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == '.' || ch == '-')) return 0;
  }
  int p = 0;
  for (int64_t i = colon + 1; i < a->len; i++) {
    if (a->data[i] < '0' || a->data[i] > '9') return 0;
    p = p * 10 + (a->data[i] - '0');
  }
  if (p > 65535) return 0;
  memcpy(host, a->data, (size_t)colon); host[colon] = 0; lower_ascii(host);
  *port = p;
  return 1;
}
static int w_resolve(const char *h, struct in_addr *out) {
  if (strcmp(h, "localhost") == 0) { out->s_addr = htonl(INADDR_LOOPBACK); return 1; }
  if (inet_pton(AF_INET, h, out) == 1) return 1;
  struct addrinfo hints, *res = NULL;
  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_INET; hints.ai_socktype = SOCK_STREAM;
  if (getaddrinfo(h, NULL, &hints, &res) != 0 || !res) return 0;
  *out = ((struct sockaddr_in *)res->ai_addr)->sin_addr;
  freeaddrinfo(res);
  return 1;
}
static WIo w_errno_io(void) { return io_err(2, strerror(errno)); }

static WIo Cap_listen_on(WCap *l, WStr *addr) {
  char h[256]; int port;
  if (!w_parse_addr(addr, h, sizeof h, &port)) return io_err(2, "address must be HOST:PORT");
  if (!net_permits(l, h, port)) { char m[400]; snprintf(m, sizeof m, "listen: %s:%d is not granted", h, port); return io_err(1, m); }
  struct sockaddr_in sa; memset(&sa, 0, sizeof sa);
  sa.sin_family = AF_INET; sa.sin_port = htons((uint16_t)port);
  if (!w_resolve(h, &sa.sin_addr)) { char m[400]; snprintf(m, sizeof m, "cannot resolve %s", h); return io_err(2, m); }
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  if (fd < 0) return w_errno_io();
  int one = 1;
  setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
  if (bind(fd, (struct sockaddr *)&sa, sizeof sa) < 0 || listen(fd, 4096) < 0) { WIo r = w_errno_io(); close(fd); return r; }
  w_nonblock(fd);
  socklen_t sl = sizeof sa;
  getsockname(fd, (struct sockaddr *)&sa, &sl);
  WCap *c = cap_new(CAP_LISTENER);
  c->fd = fd; c->port = ntohs(sa.sin_port);
  return io_ok_ptr(c);
}
static WIo Cap_accept(WCap *l) {
  int fd = w_sock_enter(l);
  if (fd < 0) return io_err(2, "listener is closed");
  for (;;) {
    int c = accept(fd, NULL, NULL);
    if (c >= 0) {
      w_nonblock(c);
      int one = 1;
      setsockopt(c, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one);
      WCap *conn = cap_new(CAP_CONN);
      conn->fd = c;
      w_sock_leave(l);
      return io_ok_ptr(conn);
    }
    if (errno == EINTR || errno == ECONNABORTED) continue;
    if ((errno == EAGAIN || errno == EWOULDBLOCK) && !w_sock_closing(l)) {
      if (w_wait_fd(fd, 0) < 0) { w_sock_leave(l); return io_err(2, "another task is already accepting on this listener"); }
      if (w_sock_closing(l)) { w_sock_leave(l); return io_err(2, "listener is closed"); }
      continue;
    }
    WIo r = w_sock_closing(l) ? io_err(2, "listener is closed") : w_errno_io();
    w_sock_leave(l);
    return r;
  }
}
static WIo Cap_net_connect(WCap *n, WStr *addr) {
  char h[256]; int port;
  if (!w_parse_addr(addr, h, sizeof h, &port)) return io_err(2, "address must be HOST:PORT");
  if (!net_permits(n, h, port)) { char m[400]; snprintf(m, sizeof m, "net: %s:%d is not granted", h, port); return io_err(1, m); }
  struct sockaddr_in sa; memset(&sa, 0, sizeof sa);
  sa.sin_family = AF_INET; sa.sin_port = htons((uint16_t)port);
  if (!w_resolve(h, &sa.sin_addr)) { char m[400]; snprintf(m, sizeof m, "cannot resolve %s", h); return io_err(2, m); }
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  if (fd < 0) return w_errno_io();
  w_nonblock(fd);
  if (connect(fd, (struct sockaddr *)&sa, sizeof sa) < 0) {
    if (errno != EINPROGRESS) { WIo r = w_errno_io(); close(fd); return r; }
    for (;;) {
      if (w_wait_fd(fd, 1) < 0) { w_sock_release_fd(fd); return io_err(2, "cannot wait on socket"); }
      int err = 0; socklen_t el = sizeof err;
      getsockopt(fd, SOL_SOCKET, SO_ERROR, &err, &el);
      if (err) { w_sock_release_fd(fd); return io_err(2, strerror(err)); }
      struct sockaddr_in peer; socklen_t pl = sizeof peer;
      if (getpeername(fd, (struct sockaddr *)&peer, &pl) == 0) break;   /* connected */
      if (errno != ENOTCONN) { WIo r = w_errno_io(); w_sock_release_fd(fd); return r; }
    }
  }
  WCap *c = cap_new(CAP_CONN);
  c->fd = fd;
  return io_ok_ptr(c);
}
static WIo Cap_conn_read(WCap *c) {
  int fd = w_sock_enter(c);
  if (fd < 0) return io_err(2, "connection is closed");
  char *buf = (char *)w_raw(65536);
  for (;;) {
    ssize_t n = read(fd, buf, 65536);
    if (n >= 0 && !w_sock_closing(c)) { WStr *s = Str_from(buf, n); free(buf); w_sock_leave(c); return io_ok(s); }
    if (n < 0 && errno == EINTR) continue;
    if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK) && !w_sock_closing(c)) {
      if (w_wait_fd(fd, 0) < 0) { free(buf); w_sock_leave(c); return io_err(2, "another task is already reading this connection"); }
      continue;
    }
    WIo r = w_sock_closing(c) ? io_err(2, "connection is closed") : w_errno_io();
    free(buf);
    w_sock_leave(c);
    return r;
  }
}
static WIo Cap_conn_write(WCap *c, WStr *s) {
  int fd = w_sock_enter(c);
  if (fd < 0) return io_err(2, "connection is closed");
  int64_t off = 0;
  while (off < s->len) {
    if (w_sock_closing(c)) { w_sock_leave(c); return io_err(2, "connection is closed"); }
    ssize_t n = write(fd, s->data + off, (size_t)(s->len - off));
    if (n >= 0) { off += n; continue; }
    if (errno == EINTR) continue;
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      if (w_wait_fd(fd, 1) < 0) { w_sock_leave(c); return io_err(2, "another task is already writing this connection"); }
      continue;
    }
    WIo r = w_errno_io();
    w_sock_leave(c);
    return r;
  }
  w_sock_leave(c);
  return io_ok(NULL);
}
