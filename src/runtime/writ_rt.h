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
static void w_leakcheck(void) {
  if (getenv("WRIT_LEAKCHECK")) fprintf(stderr, "writ-leakcheck: %lld live\n", (long long)w_live);
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
  if (b->len + n > b->cap) { while (b->len + n > b->cap) b->cap *= 2; b->p = (char *)realloc(b->p, (size_t)b->cap); }
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
    l->data = (T *)w_alloc(sizeof(T) * (size_t)l->cap);                                       \
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
    l->cap *= 2; l->data = (T *)realloc(l->data, sizeof(T) * (size_t)l->cap);                 \
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
    if (l->cap < cap) { l->cap = cap; l->data = (T *)realloc(l->data, sizeof(T) * (size_t)cap); } \
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
      g_hosts = (char **)realloc(g_hosts, sizeof(char *) * (size_t)(g_nhosts + 1));
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

static int parse_url(const char *u, char *host, size_t hn, int *port, char *err, size_t en) {
  const char *c = strchr(u, ':');
  if (!c) { snprintf(err, en, "URL has no scheme"); return 0; }
  if (strncmp(c, "://", 3) != 0) { snprintf(err, en, "malformed URL"); return 0; }
  char scheme[32];
  snprintf(scheme, sizeof scheme, "%.*s", (int)(c - u), u);
  lower_ascii(scheme);
  if (strcmp(scheme, "http") != 0 && strcmp(scheme, "https") != 0) {
    snprintf(err, en, "scheme `%s` is not allowed, only http and https", scheme); return 0;
  }
  const char *rest = c + 3;
  size_t stop = strcspn(rest, "/?#");
  char hp[512];
  snprintf(hp, sizeof hp, "%.*s", (int)stop, rest);
  char *at = strrchr(hp, '@');
  char *h = at ? at + 1 : hp;
  char *colon = strrchr(h, ':');
  *port = strcmp(scheme, "https") == 0 ? 443 : 80;
  if (colon) {
    char *end; long p = strtol(colon + 1, &end, 10);
    if (*end != 0 || colon[1] == 0) { snprintf(err, en, "bad port in URL"); return 0; }
    *port = (int)p; *colon = 0;
  } else if (*h == 0) { snprintf(err, en, "URL has no host"); return 0; }
  snprintf(host, hn, "%s", h); lower_ascii(host);
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

/* ---------- structured concurrency: group / go ----------
   A fixed pool of worker threads (one per core, WRIT_THREADS to override)
   takes tasks from one queue. A thread waiting for its group runs queued
   tasks itself instead of sleeping, so nested groups cannot deadlock. */

typedef struct WGroup WGroup;
typedef struct WTask WTask;
struct WTask { WClo *fn; void (*run)(WTask *); WGroup *g; WTask *next; max_align_t slot[]; };
struct WGroup { int64_t rc; int64_t pending; int64_t n, cap; WTask **tasks; };

static pthread_mutex_t wq_mu = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t wq_cv = PTHREAD_COND_INITIALIZER;
static WTask *wq_head, *wq_tail;
static pthread_once_t w_pool_once = PTHREAD_ONCE_INIT;

static WTask *wq_pop_locked(void) {
  WTask *t = wq_head;
  if (t) { wq_head = t->next; if (!wq_head) wq_tail = NULL; }
  return t;
}
static void w_task_done(WTask *t) {
  if (__atomic_fetch_sub(&t->g->pending, 1, __ATOMIC_ACQ_REL) == 1) {
    pthread_mutex_lock(&wq_mu);
    pthread_cond_broadcast(&wq_cv);
    pthread_mutex_unlock(&wq_mu);
  }
}
static void *w_worker(void *arg) {
  (void)arg;
  for (;;) {
    pthread_mutex_lock(&wq_mu);
    WTask *t;
    while (!(t = wq_pop_locked())) pthread_cond_wait(&wq_cv, &wq_mu);
    pthread_mutex_unlock(&wq_mu);
    t->run(t);
    w_task_done(t);
  }
  return NULL;
}
static void w_pool_start(void) {
  long n = sysconf(_SC_NPROCESSORS_ONLN);
  const char *e = getenv("WRIT_THREADS");
  if (e) n = atol(e);
  for (long i = 1; i < n; i++) {
    pthread_t th;
    pthread_attr_t at;
    pthread_attr_init(&at);
    pthread_attr_setstacksize(&at, 64 << 20);
    pthread_create(&th, &at, w_worker, NULL);
    pthread_detach(th);
    pthread_attr_destroy(&at);
  }
}
static WGroup *w_group_new(void) {
  pthread_once(&w_pool_once, w_pool_start);
  WGroup *g = (WGroup *)w_alloc(sizeof(WGroup));
  g->rc = 1; g->pending = 0; g->n = 0; g->cap = 8;
  g->tasks = (WTask **)w_raw(sizeof(WTask *) * (size_t)g->cap);
  return g;
}
/* The closure and everything it references become shared before any other
   thread can see them. */
static void w_spawn(WGroup *g, WClo *fn, void (*run)(WTask *), size_t slot) {
  Clo_share(fn);
  WTask *t = (WTask *)w_alloc(sizeof(WTask) + slot);
  t->fn = fn; t->run = run; t->g = g; t->next = NULL;
  if (g->n == g->cap) { g->cap *= 2; g->tasks = (WTask **)realloc(g->tasks, sizeof(WTask *) * (size_t)g->cap); }
  g->tasks[g->n++] = t;
  __atomic_fetch_add(&g->pending, 1, __ATOMIC_RELAXED);
  pthread_mutex_lock(&wq_mu);
  if (wq_tail) wq_tail->next = t; else wq_head = t;
  wq_tail = t;
  pthread_cond_signal(&wq_cv);
  pthread_mutex_unlock(&wq_mu);
}
static void w_group_wait(WGroup *g) {
  for (;;) {
    if (__atomic_load_n(&g->pending, __ATOMIC_ACQUIRE) == 0) return;
    pthread_mutex_lock(&wq_mu);
    WTask *t = wq_pop_locked();
    if (!t) {
      if (__atomic_load_n(&g->pending, __ATOMIC_ACQUIRE) != 0) pthread_cond_wait(&wq_cv, &wq_mu);
      pthread_mutex_unlock(&wq_mu);
      continue;
    }
    pthread_mutex_unlock(&wq_mu);
    t->run(t);
    w_task_done(t);
  }
}
static void w_group_release_tasks(WGroup *g) {
  for (int64_t i = 0; i < g->n; i++) w_free(g->tasks[i]);
  g->n = 0;
}
static inline WGroup *Group_dup(WGroup *g) { g->rc++; return g; }
static void Group_drop(WGroup *g) { if (--g->rc == 0) { free(g->tasks); w_free(g); } }
static inline void Group_share(WGroup *g) { (void)g; }
