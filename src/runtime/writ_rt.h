/* Writ C runtime. Included verbatim at the top of every generated program.
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
#include <sys/time.h>
#include <errno.h>
#include <pthread.h>

extern char **environ;
typedef uint8_t W_Unit;

/* Counted allocations: every runtime object (strings, lists, closures,
   capabilities, boxed enums) goes through w_alloc/w_free, so tests can check
   that reference counting frees everything (WRIT_LEAKCHECK=1). */
static int64_t w_live;
static void *w_alloc(size_t n) {
  void *p = malloc(n ? n : 1);
  if (!p) { fputs("panic: out of memory\n", stderr); exit(1); }
  __atomic_fetch_add(&w_live, 1, __ATOMIC_RELAXED);
  return p;
}
static void w_free(void *p) { if (p) { __atomic_fetch_sub(&w_live, 1, __ATOMIC_RELAXED); free(p); } }

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
  if (r > 0) { __atomic_store_n(rc, -r, __ATOMIC_RELAXED); return true; }
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
  if (getenv("WRIT_LEAKCHECK")) fprintf(stderr, "writ-leakcheck: %lld live\n", (long long)__atomic_load_n(&w_live, __ATOMIC_ACQUIRE));
}

__attribute__((noreturn)) static void w_panic_c(const char *msg, const char *where) {
  fflush(stdout);
  fprintf(stderr, "panic: %s\n  --> %s\n", msg, where);
  exit(1);
}
__attribute__((noreturn)) static void w_unreachable(void) { w_panic_c("internal: unreachable", "?"); }

/* ---------- strings ---------- */

typedef struct WStr { int64_t rc; int64_t len; char *data; } WStr; /* rc == 0: immortal literal */

static WStr *Str_new(int64_t len) {
  if (len < 0 || (uint64_t)len > SIZE_MAX - sizeof(WStr) - 1) w_oom();
  WStr *s = (WStr *)w_alloc(sizeof(WStr) + (size_t)len + 1);
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
#define Unit_dup(x) (x)
#define Unit_drop(x) ((void)0)
#define Unit_eq(a, b) true
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

#define WRIT_LIST_STRUCT(L, T) struct L { int64_t rc; int64_t len; int64_t cap; T *data; };

#define WRIT_LIST_FUNCS(L, T, P)                                                              \
  static L *L##_new(int64_t cap) {                                                            \
    L *l = (L *)w_alloc(sizeof(L));                                                           \
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
    l->cap *= 2; l->data = (T *)w_realloc(l->data, w_bytes(l->cap, sizeof(T)));               \
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
    if (l->cap < cap) { l->data = (T *)w_realloc(l->data, w_bytes(cap, sizeof(T))); l->cap = cap; } \
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

/* Stable merge sort, for lists of Int, Float and Str. */
#define WRIT_LIST_SORT(L, T, P)                                                               \
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

typedef struct L_Int L_Int;
typedef struct L_Str L_Str;
WRIT_LIST_STRUCT(L_Int, int64_t)
WRIT_LIST_STRUCT(L_Str, WStr *)
WRIT_LIST_FUNCS(L_Int, int64_t, Int)
WRIT_LIST_FUNCS(L_Str, WStr *, Str)
WRIT_LIST_SORT(L_Int, int64_t, Int)
WRIT_LIST_SORT(L_Str, WStr *, Str)

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

/* ---------- closures ---------- */

typedef struct WClo WClo;
struct WClo { int64_t rc; void *fn; void (*drop)(WClo *); void (*share)(WClo *); };
static inline WClo *Clo_dup(WClo *c) { w_rc_inc(&c->rc); return c; }
static void Clo_drop(WClo *c) { if (w_rc_dec(&c->rc)) { if (c->drop) c->drop(c); w_free(c); } }
static void Clo_share(WClo *c) { if (w_rc_share(&c->rc) && c->share) c->share(c); }
static inline bool Clo_eq(WClo *a, WClo *b) { return a == b; }
static void Clo_show(WBuf *b, WClo *c, bool top) { (void)c; (void)top; buf_puts(b, "<fn>"); }

/* ---------- capabilities ---------- */

enum { CAP_ROOT, CAP_NET, CAP_FS, CAP_STDOUT, CAP_CLOCK };
typedef struct WCap WCap;
struct WCap {
  int64_t rc; int kind;
  char **hosts; int nhosts;   /* Net: allowlist entries */
  char *root; bool ro;        /* Fs: realpath of root, NULL = not granted */
};
static WCap *cap_new(int kind) { WCap *c = (WCap *)w_alloc(sizeof(WCap)); memset(c, 0, sizeof *c); c->rc = 1; c->kind = kind; return c; }
static inline WCap *Cap_dup(WCap *c) { w_rc_inc(&c->rc); return c; }
static inline void Cap_share(WCap *c) { w_rc_share(&c->rc); }
static void Cap_drop(WCap *c) {
  if (w_rc_dec(&c->rc)) { for (int i = 0; i < c->nhosts; i++) free(c->hosts[i]); free(c->hosts); free(c->root); w_free(c); }
}
static inline bool Cap_eq(WCap *a, WCap *b) { return a == b; }
static void Cap_show(WBuf *b, WCap *c, bool top) {
  (void)top;
  static const char *names[] = { "<Root>", "<Net>", "<Fs>", "<Stdout>", "<Clock>" };
  buf_puts(b, names[c->kind]);
}

static char **g_hosts; static int g_nhosts; static char *g_fs_root; static bool g_fs_ro;

static void w_usage(const char *m) {
  fprintf(stderr, "%s\nusage: PROGRAM [--grant net=HOST[:PORT]|*]... [--grant fs=DIR[:ro]]\n", m);
  exit(2);
}
static void w_init(int argc, char **argv) {
  for (int i = 1; i < argc; i++) {
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
  return cap_new(CAP_CLOCK);
}

/* I/O results: kind 0 = ok, 1 = denied, 2 = failed. */
typedef struct { int kind; WStr *val; WStr *msg; } WIo;
static WIo io_ok(WStr *v) { WIo r = { 0, v, NULL }; return r; }
static WIo io_err(int kind, const char *m) { WIo r = { kind, NULL, Str_from(m, (int64_t)strlen(m)) }; return r; }

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
  char tmp[65536]; ssize_t n;
  while ((n = read(fd, tmp, sizeof tmp)) > 0) buf_putn(&b, tmp, n);
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
  WIo r = { 2, NULL, t };
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
static void Cap_print(WStr *s, bool nl) {
  flockfile(stdout);
  fwrite(s->data, 1, (size_t)s->len, stdout);
  if (nl) fputc('\n', stdout);
  funlockfile(stdout);
}
static int64_t Cap_now_ms(void) { struct timeval tv; gettimeofday(&tv, NULL); return (int64_t)tv.tv_sec * 1000 + tv.tv_usec / 1000; }

/* ---------- fibers and the work-stealing scheduler ----------
   Every Writ task, and main itself, runs as a fiber: a small mmap'd stack
   plus saved registers. Worker threads (one per core, WRIT_THREADS to
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
#error "Writ fibers need arm64 or x86_64"
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
static int w_main_done;
static size_t w_stack_size = 256 * 1024;
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
  size_t total = size + (size_t)page;   /* plus a guard page at the low end */
  char *map = (char *)mmap(NULL, total, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
  if (map == MAP_FAILED) { fputs("panic: cannot allocate a task stack\n", stderr); exit(1); }
  mprotect(map, (size_t)page, PROT_NONE);
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
  f->asan_bottom = map + page; f->asan_size = size;
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
    if (w_global_tail) w_global_tail->next = f; else w_global_head = f;
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
    if (f) { w_global_head = f->next; if (!w_global_head) w_global_tail = NULL; }
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
      __atomic_fetch_add(&w_nidle, 1, __ATOMIC_SEQ_CST);
      if (__atomic_load_n(&w_queued, __ATOMIC_SEQ_CST) == 0 && !(is_main && __atomic_load_n(&w_main_done, __ATOMIC_ACQUIRE)))
        pthread_cond_wait(&w_idle_cv, &w_idle_mu);
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
  const char *e = getenv("WRIT_THREADS");
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
  const char *e = getenv("WRIT_STACK_KB");
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
struct WTask { WClo *fn; void (*run)(WTask *); WGroup *g; max_align_t slot[]; };
struct WGroup { int64_t rc; pthread_mutex_t mu; int64_t pending; WFiber *waiter; int64_t n, cap; WTask **tasks; };

static WGroup *w_group_new(void) {
  WGroup *g = (WGroup *)w_alloc(sizeof(WGroup));
  g->rc = 1; g->pending = 0; g->waiter = NULL; g->n = 0; g->cap = 8;
  pthread_mutex_init(&g->mu, NULL);
  g->tasks = (WTask **)w_raw(sizeof(WTask *) * (size_t)g->cap);
  return g;
}
static void w_task_entry(void *p) {
  WTask *t = (WTask *)p;
  WGroup *g = t->g;
  t->run(t);
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
  t->fn = fn; t->run = run; t->g = g;
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
  for (int64_t i = 0; i < g->n; i++) w_free(g->tasks[i]);
  g->n = 0;
}
static inline WGroup *Group_dup(WGroup *g) { g->rc++; return g; }
static void Group_drop(WGroup *g) { if (--g->rc == 0) { pthread_mutex_destroy(&g->mu); free(g->tasks); w_free(g); } }
static inline void Group_share(WGroup *g) { (void)g; }
