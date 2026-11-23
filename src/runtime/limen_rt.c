/* Limen C runtime: the definitions, after limen_rt.h (see the note at the
   top of it). Functions are W_RT: static in a one-file build, callable from
   the program's pieces when the runtime is an object. */
int64_t w_live;
int64_t w_allocs;
W_RT void *w_alloc_k(size_t n, int kind) {
  WHdr *h = (WHdr *)malloc(sizeof(WHdr) + (n ? n : 1));
  if (!h) { fputs("panic: out of memory\n", stderr); exit(1); }
  h->prev = h->next = NULL; h->kind = kind; h->kids = NULL;
  __atomic_fetch_add(&w_live, 1, __ATOMIC_RELAXED);
  __atomic_fetch_add(&w_allocs, 1, __ATOMIC_RELAXED);
  if (kind != WK_BUF) {
    WCatch **slot = w_catch_slot();
    if (slot && *slot) w_hdr_link(&(*slot)->objs, h);
  }
  return h + 1;
}
/* a buffer owned by an object (list data, map tables): never on a list */
W_RT void *w_alloc(size_t n) { return w_alloc_k(n, WK_BUF); }
W_RT void w_free(void *p) {
  if (!p) return;
  WHdr *h = (WHdr *)p - 1;
  w_hdr_unlink(h);
  __atomic_fetch_sub(&w_live, 1, __ATOMIC_RELAXED);
  free(h);
}
__thread WHdr *w_rel_queue;
__thread int w_rel_busy;
W_RT void w_release(void *o) {
  WHdr *h = (WHdr *)o - 1;
  w_hdr_unlink(h);
  h->next = w_rel_queue;
  w_rel_queue = h;
  if (w_rel_busy) return;
  w_rel_busy = 1;
  while ((h = w_rel_queue)) {
    w_rel_queue = h->next;
    h->next = NULL;
    void *p = h + 1;
    if (h->kids) h->kids(p);
    if (h->kind == WK_LIST) w_free(((WListAny *)p)->data);
    else if (h->kind == WK_MAP) {
      WMapAny *m = (WMapAny *)p;
      w_free(m->keys); w_free(m->vals); w_free(m->live); w_free(m->idx);
    }
    w_free(p);
  }
  w_rel_busy = 0;
}
/* The work stacks of w_share, w_eq, w_show, w_cmp and w_hash (limen_rt.h).
   Each grows as needed and is given back when a large one empties, or when
   the thread ends: they hang off one block of the thread's, which is also
   the value of a pthread key, so the key's destructor frees them without
   reading thread-local variables (not safe in a destructor on Darwin). */
typedef struct { void (*kids)(void *); void *o; } WShareItem;
typedef struct { WEqFn f; void *a, *b; int64_t i; } WEqItem;
/* f == NULL: i bytes of text, on top of the text stack */
typedef struct { WShowFn f; void *v; int64_t i, pos; } WShowItem;
/* f == NULL: an answer found ahead of time, i (w_cmp_res) */
typedef struct { WCmpFn f; void *a, *b; int64_t i; } WCmpItem;
/* A fresh part, or (wait) a call waiting at part i, with h the hash before
   that part's */
typedef struct { WHashFn f; void *o; int64_t i; uint64_t h; int wait; } WHashItem;
typedef struct {
  WShareItem *sh; WEqItem *eq; WShowItem *show, *show_tmp; char *text; WCmpItem *cmp; WHashItem *hash;
  int64_t sh_cap, eq_cap, show_cap, show_tmp_cap, text_cap, cmp_cap, hash_cap;
} WStacks;
static __thread WStacks *w_stk;
static pthread_key_t w_stacks_key;
static pthread_once_t w_stacks_once = PTHREAD_ONCE_INIT;
/* w_stk still points at the block after this has freed it; nothing reads
   it then, since the thread is ending and runs no more Limen code */
static void w_stacks_free(void *p) {
  WStacks *s = (WStacks *)p;
  free(s->sh); free(s->eq); free(s->show); free(s->show_tmp); free(s->text); free(s->cmp); free(s->hash);
  free(s);
}
static void w_stacks_key_new(void) { pthread_key_create(&w_stacks_key, w_stacks_free); }
__attribute__((noinline)) static WStacks *w_stk_new(void) {
  WStacks *s = (WStacks *)calloc(1, sizeof *s);
  if (!s) w_oom();
  pthread_once(&w_stacks_once, w_stacks_key_new);
  pthread_setspecific(w_stacks_key, s);
  w_stk = s;
  return s;
}
static inline WStacks *w_stk_get(void) { return __builtin_expect(w_stk != NULL, 1) ? w_stk : w_stk_new(); }
#define w_sh_st (w_stk_get()->sh)
#define w_sh_cap (w_stk_get()->sh_cap)
#define w_eq_st (w_stk_get()->eq)
#define w_eq_cap (w_stk_get()->eq_cap)
#define w_show_st (w_stk_get()->show)
#define w_show_cap (w_stk_get()->show_cap)
#define w_show_tmp (w_stk_get()->show_tmp)
#define w_show_tmp_cap (w_stk_get()->show_tmp_cap)
#define w_text (w_stk_get()->text)
#define w_text_cap (w_stk_get()->text_cap)
#define w_cmp_st (w_stk_get()->cmp)
#define w_cmp_cap (w_stk_get()->cmp_cap)
#define w_hash_st (w_stk_get()->hash)
#define w_hash_cap (w_stk_get()->hash_cap)
static void *w_stack_grow(void *p, int64_t *cap, int64_t need, size_t size) {
  if (need <= *cap) return p;
  int64_t c = *cap ? *cap : 64;
  while (c < need) { if (c > INT64_MAX / 2) w_oom(); c *= 2; }
  *cap = c;
  return w_realloc(p, w_bytes(c, size));
}
#define W_STACK_KEEP 4096
__thread int w_sh_busy, w_sh_depth;
static __thread int64_t w_sh_n;
W_RT void w_share_push(void *o, void (*kids)(void *)) {
  w_sh_st = (WShareItem *)w_stack_grow(w_sh_st, &w_sh_cap, w_sh_n + 1, sizeof(WShareItem));
  w_sh_st[w_sh_n++] = (WShareItem){ kids, o };
}
W_RT void w_share_outer(void *o, void (*kids)(void *)) {
  w_sh_busy = 1; w_sh_depth = 0;
  kids(o);
  while (w_sh_n > 0) { WShareItem it = w_sh_st[--w_sh_n]; it.kids(it.o); }
  if (w_sh_cap > W_STACK_KEEP) { free(w_sh_st); w_sh_st = NULL; w_sh_cap = 0; }
  w_sh_busy = 0;
}
__thread int w_eq_busy, w_eq_depth;
__thread int64_t w_eq_n;
W_RT void w_eq_push(WEqFn f, void *a, void *b, int64_t i) {
  w_eq_st = (WEqItem *)w_stack_grow(w_eq_st, &w_eq_cap, w_eq_n + 1, sizeof(WEqItem));
  w_eq_st[w_eq_n++] = (WEqItem){ f, a, b, i };
}
/* f, then reverse what it left on the stack, so the first is compared first */
static bool w_eq_step(WEqFn f, void *a, void *b, int64_t i) {
  int64_t mark = w_eq_n;
  bool ok = f(a, b, i);
  for (int64_t lo = mark, hi = w_eq_n - 1; lo < hi; lo++, hi--) { WEqItem t = w_eq_st[lo]; w_eq_st[lo] = w_eq_st[hi]; w_eq_st[hi] = t; }
  return ok;
}
W_RT bool w_eq_outer(WEqFn f, void *a, void *b, int64_t i) {
  int depth = w_eq_depth;
  int64_t base = w_eq_n;
  w_eq_busy = 1; w_eq_depth = 0;
  bool ok = w_eq_step(f, a, b, i);
  while (ok && w_eq_n > base) { WEqItem it = w_eq_st[--w_eq_n]; ok = w_eq_step(it.f, it.a, it.b, it.i); }
  w_eq_n = base;
  if (base == 0 && w_eq_cap > W_STACK_KEEP) { free(w_eq_st); w_eq_st = NULL; w_eq_cap = 0; }
  w_eq_busy = 0; w_eq_depth = depth;
  return ok;
}
__thread WBuf *w_show_buf;
__thread int w_show_depth;
__thread int64_t w_show_n;
static __thread int64_t w_text_n;
W_RT void w_show_push(WBuf *b, WShowFn f, void *v, int64_t i) {
  w_show_st = (WShowItem *)w_stack_grow(w_show_st, &w_show_cap, w_show_n + 1, sizeof(WShowItem));
  w_show_st[w_show_n++] = (WShowItem){ f, v, i, b->len };
}
/* f, then cut the text written after the first object it left on the stack
   into pieces, one after each such object, and order the stack so that
   popping it writes each object and then its piece */
static void w_show_step(WBuf *b, WShowFn f, void *v, int64_t i) {
  int64_t mark = w_show_n;
  f(b, v, i);
  int64_t k = w_show_n - mark;
  if (k == 0) return;
  w_show_tmp = (WShowItem *)w_stack_grow(w_show_tmp, &w_show_tmp_cap, k, sizeof(WShowItem));
  memcpy(w_show_tmp, w_show_st + mark, (size_t)k * sizeof(WShowItem));
  w_show_st = (WShowItem *)w_stack_grow(w_show_st, &w_show_cap, mark + 2 * k, sizeof(WShowItem));
  w_show_n = mark;
  int64_t end = b->len;
  for (int64_t j = k - 1; j >= 0; j--) {
    int64_t p = w_show_tmp[j].pos, len = end - p;
    if (len > 0) {
      w_text = (char *)w_stack_grow(w_text, &w_text_cap, w_text_n + len, 1);
      memcpy(w_text + w_text_n, b->p + p, (size_t)len);
      w_text_n += len;
      w_show_st[w_show_n++] = (WShowItem){ NULL, NULL, len, 0 };
    }
    w_show_st[w_show_n++] = w_show_tmp[j];
    end = p;
  }
  b->len = w_show_tmp[0].pos;
  if (w_show_tmp_cap > W_STACK_KEEP) { free(w_show_tmp); w_show_tmp = NULL; w_show_tmp_cap = 0; }
}
W_RT void w_show_outer(WBuf *b, WShowFn f, void *v, int64_t i) {
  WBuf *buf = w_show_buf;
  int depth = w_show_depth;
  int64_t base = w_show_n;
  w_show_buf = b; w_show_depth = 0;
  w_show_step(b, f, v, i);
  while (w_show_n > base) {
    WShowItem it = w_show_st[--w_show_n];
    if (it.f) w_show_step(b, it.f, it.v, it.i);
    else { w_text_n -= it.i; buf_putn(b, w_text + w_text_n, it.i); }
  }
  if (base == 0) {
    if (w_show_cap > W_STACK_KEEP) { free(w_show_st); w_show_st = NULL; w_show_cap = 0; }
    if (w_text_cap > 16 * W_STACK_KEEP) { free(w_text); w_text = NULL; w_text_cap = 0; }
  }
  w_show_buf = buf; w_show_depth = depth;
}
__thread int w_cmp_busy, w_cmp_depth;
__thread int64_t w_cmp_n, w_cmp_base;
W_RT void w_cmp_push(WCmpFn f, void *a, void *b, int64_t i) {
  w_cmp_st = (WCmpItem *)w_stack_grow(w_cmp_st, &w_cmp_cap, w_cmp_n + 1, sizeof(WCmpItem));
  w_cmp_st[w_cmp_n++] = (WCmpItem){ f, a, b, i };
}
/* as w_eq_step; f answers only if it left nothing on the stack */
static int w_cmp_step(WCmpItem it) {
  if (!it.f) return (int)it.i;
  int64_t mark = w_cmp_n;
  w_cmp_base = mark;
  int r = it.f(it.a, it.b, it.i);
  for (int64_t lo = mark, hi = w_cmp_n - 1; lo < hi; lo++, hi--) { WCmpItem t = w_cmp_st[lo]; w_cmp_st[lo] = w_cmp_st[hi]; w_cmp_st[hi] = t; }
  return r;
}
W_RT int w_cmp_outer(WCmpFn f, void *a, void *b, int64_t i) {
  int depth = w_cmp_depth, busy = w_cmp_busy;
  int64_t base = w_cmp_n, sbase = w_cmp_base;
  w_cmp_busy = 1; w_cmp_depth = 0;
  int r = w_cmp_step((WCmpItem){ f, a, b, i });
  while (r == 0 && w_cmp_n > base) r = w_cmp_step(w_cmp_st[--w_cmp_n]);
  w_cmp_n = base; w_cmp_base = sbase;
  if (base == 0 && w_cmp_cap > W_STACK_KEEP) { free(w_cmp_st); w_cmp_st = NULL; w_cmp_cap = 0; }
  w_cmp_busy = busy; w_cmp_depth = depth;
  return r;
}
__thread int w_hash_busy, w_hash_depth, w_hash_susp;
static __thread int64_t w_hash_n;
W_RT void w_hash_push(WHashFn f, void *o) {
  w_hash_st = (WHashItem *)w_stack_grow(w_hash_st, &w_hash_cap, w_hash_n + 1, sizeof(WHashItem));
  w_hash_st[w_hash_n++] = (WHashItem){ f, o, 0, W_HASH_SEED, 0 };
  w_hash_susp = 1;
}
W_RT void w_hash_wait(WHashFn f, void *o, int64_t i, uint64_t h) {
  w_hash_st = (WHashItem *)w_stack_grow(w_hash_st, &w_hash_cap, w_hash_n + 1, sizeof(WHashItem));
  w_hash_st[w_hash_n++] = (WHashItem){ f, o, i, h, 1 };
}
/* f from part i; if a part went on the stack, the part is pushed first and
   then each waiting call, innermost first, so reverse them: the part runs
   next, and each waiting call resumes after the one it waits for */
static uint64_t w_hash_step(WHashFn f, void *o, int64_t i, uint64_t h) {
  int64_t mark = w_hash_n;
  w_hash_susp = 0;
  uint64_t r = f(o, i, h);
  if (!w_hash_susp) return r;
  w_hash_susp = 0;
  for (int64_t lo = mark, hi = w_hash_n - 1; lo < hi; lo++, hi--) { WHashItem t = w_hash_st[lo]; w_hash_st[lo] = w_hash_st[hi]; w_hash_st[hi] = t; }
  return 0;
}
W_RT uint64_t w_hash_outer(WHashFn f, void *o) {
  int depth = w_hash_depth, busy = w_hash_busy, susp = w_hash_susp;
  int64_t base = w_hash_n;
  w_hash_busy = 1; w_hash_depth = 0;
  uint64_t h = w_hash_step(f, o, 0, W_HASH_SEED);
  while (w_hash_n > base) {
    WHashItem it = w_hash_st[--w_hash_n];
    if (it.wait) { it.h = w_hash_mix(it.h, h); it.i++; }
    h = w_hash_step(it.f, it.o, it.i, it.h);
  }
  if (base == 0 && w_hash_cap > W_STACK_KEEP) { free(w_hash_st); w_hash_st = NULL; w_hash_cap = 0; }
  w_hash_busy = busy; w_hash_depth = depth; w_hash_susp = susp;
  return h;
}
#undef w_sh_st
#undef w_sh_cap
#undef w_eq_st
#undef w_eq_cap
#undef w_show_st
#undef w_show_cap
#undef w_show_tmp
#undef w_show_tmp_cap
#undef w_text
#undef w_text_cap
#undef w_cmp_st
#undef w_cmp_cap
#undef w_hash_st
#undef w_hash_cap
/* Start a boundary's list; its parent is the current innermost boundary. */
W_RT void w_catch_init(WCatch *c) {
  c->msg = NULL; c->where = NULL; c->unwinding = 0; c->cancel = 0;
  c->objs.prev = c->objs.next = &c->objs;
  WCatch **slot = w_catch_slot();
  c->prev = slot ? *slot : NULL;
}
/* Normal exit: what the boundary allocated and still lives now belongs to
   the enclosing boundary, or to nobody. */
W_RT void w_catch_end(WCatch *c) {
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

/* realloc for a buffer from w_alloc */
W_RT void *w_brealloc(void *p, size_t n) {
  WHdr *h = (WHdr *)realloc((WHdr *)p - 1, sizeof(WHdr) + (n ? n : 1));
  if (!h) { fputs("panic: out of memory\n", stderr); exit(1); }
  return h + 1;
}
W_RT void *w_raw(size_t n) {
  void *p = malloc(n ? n : 1);
  if (!p) { fputs("panic: out of memory\n", stderr); exit(1); }
  return p;
}
__attribute__((noreturn)) W_RT void w_oom(void) { fputs("panic: out of memory\n", stderr); exit(1); }
/* count * size with overflow check */
W_RT size_t w_bytes(int64_t count, size_t size) {
  if (count < 0 || (size && (uint64_t)count > SIZE_MAX / size)) w_oom();
  return (size_t)count * size;
}
W_RT void *w_realloc(void *p, size_t n) {
  void *q = realloc(p, n ? n : 1);
  if (!q) w_oom();
  return q;
}
W_RT char *w_strdup(const char *c) { char *p = strdup(c); if (!p) w_oom(); return p; }
W_RT char *w_strndup(const char *c, size_t n) { char *p = strndup(c, n); if (!p) w_oom(); return p; }
W_RT void w_leakcheck(void) {
  w_kept_release();
  if (getenv("LIMEN_LEAKCHECK")) fprintf(stderr, "limen-leakcheck: %lld live\n", (long long)__atomic_load_n(&w_live, __ATOMIC_ACQUIRE));
}
/* Errors no boundary can handle: report and exit, after the output tasks
   still hold. */
__attribute__((noreturn)) W_RT void w_fatal_c(const char *msg, const char *where) {
  w_out_release_all();
  fflush(stdout);
  fprintf(stderr, "panic: %s\n  --> %s\n", msg, where);
  exit(1);
}
/* Unwinding runs every frame's cleanups. The boundary's frame has a guard
   variable declared after its other locals, so its cleanup runs first
   there and jumps back into the boundary before that frame's own locals
   are touched. The stop function only catches a failed unwind. */
W_RT _Unwind_Reason_Code w_unwind_stop(int version, _Unwind_Action actions, _Unwind_Exception_Class cls,
                                         struct _Unwind_Exception *exc, struct _Unwind_Context *ctx, void *param) {
  (void)version; (void)cls; (void)exc; (void)ctx;
  if (actions & _UA_END_OF_STACK) _longjmp(((WCatch *)param)->jb, 1);
  return _URC_NO_REASON;
}
W_RT void w_catch_land(WCatch **g) { if ((*g)->unwinding) _longjmp((*g)->jb, 1); }
/* Unwind to the innermost boundary, running the cleanups of every frame on
   the way, so owned locals are dropped; fall back to a plain jump if
   unwinding fails. */
__attribute__((noreturn)) W_RT void w_unwind_to(WCatch **slot, WCatch *c) {
  *slot = c->prev;
  c->unwinding = 1;
  memset(&c->uexc, 0, sizeof c->uexc);
  c->uexc.exception_class = 0x56454c4150414e43ULL; /* "LIMENPANC" */
  _Unwind_ForcedUnwind(&c->uexc, w_unwind_stop, c);
  _longjmp(c->jb, 1);
}
__attribute__((noreturn)) W_RT void w_panic_c(const char *msg, const char *where) {
  WCatch **slot = w_catch_slot();
  if (slot && *slot) {
    WCatch *c = *slot;
    c->msg = w_strdup(msg);
    c->where = w_strdup(where);
    w_unwind_to(slot, c);
  }
  w_fatal_c(msg, where);
}
/* A cancelled task stops at a wait: unwind like a panic. A `catch` on the
   way frees what it owns and passes the cancellation on (see Catch in
   cgen.ml), so only the task's own boundary ends it. Tasks always have a
   boundary, and only tasks are cancelled. */
__attribute__((noreturn)) W_RT void w_cancel_unwind(void) {
  WCatch **slot = w_catch_slot();
  w_update_waits();
  if (!slot || !*slot) w_fatal_c("internal: cancelled outside a task", "?");
  w_mark_unwinding();
  (*slot)->cancel = 1;
  w_unwind_to(slot, *slot);
}
__attribute__((noreturn)) W_RT void w_unreachable(void) { w_panic_c("internal: unreachable", "?"); }

W_RT WStr *Str_new(int64_t len) {
  if (len < 0 || (uint64_t)len > SIZE_MAX - sizeof(WStr) - 1) w_oom();
  WStr *s = (WStr *)w_alloc_k(sizeof(WStr) + (size_t)len + 1, WK_PLAIN);
  s->rc = 1; s->len = len; s->data = (char *)(s + 1); s->data[len] = 0;
  return s;
}
W_RT WStr *Str_from(const char *p, int64_t len) { WStr *s = Str_new(len); memcpy(s->data, p, (size_t)len); return s; }
W_RT int Str_cmp(WStr *a, WStr *b) {
  int64_t n = a->len < b->len ? a->len : b->len;
  int c = memcmp(a->data, b->data, (size_t)n);
  if (c) return c < 0 ? -1 : 1;
  return a->len < b->len ? -1 : (a->len > b->len ? 1 : 0);
}
W_RT WStr *Str_concat(WStr *a, WStr *b) {
  WStr *s = Str_new(a->len + b->len);
  memcpy(s->data, a->data, (size_t)a->len); memcpy(s->data + a->len, b->data, (size_t)b->len);
  return s;
}
W_RT void buf_init(WBuf *b) { b->cap = 64; b->len = 0; b->p = (char *)w_raw(64); }
W_RT void buf_putn(WBuf *b, const char *s, int64_t n) {
  if (n < 0 || b->len > INT64_MAX - n) w_oom();
  if (b->len + n > b->cap) { while (b->len + n > b->cap) { if (b->cap > INT64_MAX / 2) w_oom(); b->cap *= 2; } b->p = (char *)w_realloc(b->p, (size_t)b->cap); }
  memcpy(b->p + b->len, s, (size_t)n); b->len += n;
}
W_RT void buf_puts(WBuf *b, const char *s) { buf_putn(b, s, (int64_t)strlen(s)); }
W_RT WStr *buf_take(WBuf *b) { WStr *s = Str_from(b->p, b->len); free(b->p); return s; }

W_RT void fmt_float(char *out, size_t n, double x) {
  if (x == floor(x) && fabs(x) < 1e16) snprintf(out, n, "%.1f", x);
  else snprintf(out, n, "%.15g", x);
}

/* Same escaping as OCaml's String.escaped, so output matches the interpreter. */
W_RT void buf_escaped(WBuf *b, WStr *s) {
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

/* Total order with nan first, like OCaml's compare (used by sort). */
W_RT int Float_cmp(double a, double b) {
  if (isnan(a)) return isnan(b) ? 0 : -1;
  if (isnan(b)) return 1;
  return a < b ? -1 : (a > b ? 1 : 0);
}

W_RT void Int_show(WBuf *b, int64_t x, bool top) { (void)top; char t[32]; snprintf(t, sizeof t, "%lld", (long long)x); buf_puts(b, t); }
W_RT void UInt_show(WBuf *b, uint64_t x, bool top) { (void)top; char t[32]; snprintf(t, sizeof t, "%llu", (unsigned long long)x); buf_puts(b, t); }
W_RT void Float_show(WBuf *b, double x, bool top) { (void)top; char t[64]; fmt_float(t, sizeof t, x); buf_puts(b, t); }
W_RT void Bool_show(WBuf *b, bool x, bool top) { (void)top; buf_puts(b, x ? "true" : "false"); }
W_RT void Unit_show(WBuf *b, W_Unit x, bool top) { (void)x; (void)top; buf_puts(b, "()"); }
W_RT void Str_show(WBuf *b, WStr *s, bool top) { if (top) buf_putn(b, s->data, s->len); else buf_escaped(b, s); }

__attribute__((noreturn)) W_RT void w_panic_index(int64_t i, int64_t len, const char *where) {
  char m[128];
  snprintf(m, sizeof m, "index %lld is out of range for a list of length %lld", (long long)i, (long long)len);
  w_panic_c(m, where);
}
uint64_t g_hash_k0, g_hash_k1;
W_RT void w_seed_hash(void) {
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
W_RT uint64_t w_siphash13(const unsigned char *in, size_t len) {
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
__attribute__((noreturn)) W_RT void w_panic_key(WBuf *key, const char *where) {
  WBuf m; buf_init(&m);
  buf_puts(&m, "key "); buf_putn(&m, key->p, key->len); buf_puts(&m, " is not in the map");
  buf_putn(&m, "", 1);
  w_panic_c(m.p, where);
}

W_RT int64_t L_Int_sum(L_Int *l) { int64_t s = 0; for (int64_t i = 0; i < l->len; i++) s += l->data[i]; return s; }
W_RT WStr *L_Str_join(L_Str *l, WStr *sep) {
  WBuf b; buf_init(&b);
  for (int64_t i = 0; i < l->len; i++) { if (i) buf_putn(&b, sep->data, sep->len); buf_putn(&b, l->data[i]->data, l->data[i]->len); }
  return buf_take(&b);
}

/* ---------- string methods ---------- */

W_RT int64_t Str_find(WStr *s, WStr *sub, int64_t from) {
  if (sub->len == 0) return from;
  for (int64_t i = from; i + sub->len <= s->len; i++)
    if (memcmp(s->data + i, sub->data, (size_t)sub->len) == 0) return i;
  return -1;
}
/* the last occurrence; an empty sub is found at the end */
W_RT int64_t Str_rfind(WStr *s, WStr *sub) {
  for (int64_t i = s->len - sub->len; i >= 0; i--)
    if (memcmp(s->data + i, sub->data, (size_t)sub->len) == 0) return i;
  return -1;
}
W_RT L_Str *Str_split(WStr *s, WStr *sep) {
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
W_RT L_Str *Str_lines(WStr *s) {
  L_Str *out = L_Str_new(4);
  size_t pos = 0, start, len;
  while (lc_lines_next(s->data, (size_t)s->len, &pos, &start, &len)) L_Str_push(&out, Str_from(s->data + start, (int64_t)len));
  return out;
}
W_RT bool is_ws(char c) { return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'; }
W_RT WStr *Str_trim(WStr *s) {
  int64_t a = 0, b = s->len;
  while (a < b && is_ws(s->data[a])) a++;
  while (b > a && is_ws(s->data[b - 1])) b--;
  return Str_from(s->data + a, b - a);
}
W_RT WStr *Str_map_case(WStr *s, bool upper) {
  WStr *r = Str_from(s->data, s->len);
  for (int64_t i = 0; i < r->len; i++) {
    char c = r->data[i];
    if (upper && c >= 'a' && c <= 'z') r->data[i] = (char)(c - 32);
    if (!upper && c >= 'A' && c <= 'Z') r->data[i] = (char)(c + 32);
  }
  return r;
}
W_RT WStr *Str_replace(WStr *s, WStr *a, WStr *b) {
  if (a->len == 0) return Str_dup(s);
  L_Str *parts = Str_split(s, a);
  WStr *r = L_Str_join(parts, b);
  L_Str_drop(parts);
  return r;
}
W_RT bool Str_starts(WStr *s, WStr *p) { return p->len <= s->len && memcmp(s->data, p->data, (size_t)p->len) == 0; }
W_RT bool Str_ends(WStr *s, WStr *p) { return p->len <= s->len && memcmp(s->data + s->len - p->len, p->data, (size_t)p->len) == 0; }
/* Decimal integer with optional sign, surrounding whitespace allowed. */
W_RT bool Str_parse_int(WStr *s, int64_t *out) {
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
W_RT WStr *Str_slice(WStr *s, int64_t lo, int64_t hi) {
  if (lo < 0) lo = 0;
  if (lo > s->len) lo = s->len;
  if (hi > s->len) hi = s->len;
  if (hi < lo) hi = lo;
  return Str_from(s->data + lo, hi - lo);
}
W_RT L_Int *Str_bytes(WStr *s) {
  L_Int *out = L_Int_new(s->len);
  for (int64_t i = 0; i < s->len; i++) L_Int_push(&out, (int64_t)(unsigned char)s->data[i]);
  return out;
}
W_RT void Bytes_show(WBuf *b, WStr *s, bool top) {
  (void)top;
  char tmp[8];
  buf_puts(b, "bytes([");
  for (int64_t i = 0; i < s->len; i++) {
    snprintf(tmp, sizeof tmp, i ? ", %u" : "%u", (unsigned)(unsigned char)s->data[i]);
    buf_puts(b, tmp);
  }
  buf_puts(b, "])");
}
W_RT WStr *Bytes_from_list(L_Int *l, const char *where) {
  WStr *s = Str_new(l->len);
  for (int64_t i = 0; i < l->len; i++) {
    int64_t v = l->data[i];
    if (v < 0 || v > 255) {
      char m[96];
      snprintf(m, sizeof m, "byte %lld is out of range 0 to 255", (long long)v);
      Str_drop(s);
      w_panic_c(m, where);
    }
    s->data[i] = (char)v;
  }
  return s;
}
W_RT int64_t Bytes_at(WStr *s, int64_t i, const char *where) {
  if (i < 0 || i >= s->len) {
    char m[128];
    snprintf(m, sizeof m, "index %lld is out of range for bytes of length %lld", (long long)i, (long long)s->len);
    w_panic_c(m, where);
  }
  return (int64_t)(unsigned char)s->data[i];
}
/* The length of the UTF-8 character at s[i], or 1 for a byte that does not
   start a well-formed one (as Interp.utf8_len). */
W_RT int64_t w_utf8_len(const unsigned char *s, int64_t i, int64_t n) {
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
W_RT L_Str *Str_chars(WStr *s) {
  L_Str *out = L_Str_new(s->len);
  for (int64_t i = 0; i < s->len;) {
    int64_t k = w_utf8_len((const unsigned char *)s->data, i, s->len);
    L_Str_push(&out, Str_from(s->data + i, k));
    i += k;
  }
  return out;
}
/* lo..<hi as a list */
W_RT L_Int *L_Int_range(int64_t lo, int64_t hi) {
  int64_t n = 0;
  if (hi > lo && __builtin_sub_overflow(hi, lo, &n)) w_oom();
  L_Int *out = L_Int_new(n);
  for (int64_t i = 0; i < n; i++) out->data[i] = lo + i;
  out->len = n;
  return out;
}
/* The UTF-8 encoding of a code point; U+FFFD for anything that is not one. */
W_RT WStr *Int_to_char(int64_t n) {
  if (n < 0 || n > 0x10FFFF || (n >= 0xD800 && n <= 0xDFFF)) n = 0xFFFD;
  char b[4];
  int k;
  if (n < 0x80) { b[0] = (char)n; k = 1; }
  else if (n < 0x800) { b[0] = (char)(0xC0 | (n >> 6)); b[1] = (char)(0x80 | (n & 0x3F)); k = 2; }
  else if (n < 0x10000) { b[0] = (char)(0xE0 | (n >> 12)); b[1] = (char)(0x80 | ((n >> 6) & 0x3F)); b[2] = (char)(0x80 | (n & 0x3F)); k = 3; }
  else { b[0] = (char)(0xF0 | (n >> 18)); b[1] = (char)(0x80 | ((n >> 12) & 0x3F)); b[2] = (char)(0x80 | ((n >> 6) & 0x3F)); b[3] = (char)(0x80 | (n & 0x3F)); k = 4; }
  return Str_from(b, k);
}
/* Fixed-point text with [d] decimals (clamped to 0..30), rounded as printf
   does; the interpreter uses the same printf. */
W_RT WStr *Float_to_fixed(double x, int64_t d) {
  if (isnan(x)) return Str_from("nan", 3);
  if (isinf(x)) return x > 0 ? Str_from("inf", 3) : Str_from("-inf", 4);
  if (d < 0) d = 0;
  if (d > 30) d = 30;
  int n = snprintf(NULL, 0, "%.*f", (int)d, x);
  WStr *s = Str_new(n);
  char *b = (char *)w_raw((size_t)n + 1);
  snprintf(b, (size_t)n + 1, "%.*f", (int)d, x);
  memcpy(s->data, b, (size_t)n);
  free(b);
  return s;
}
/* A decimal float: digits with optional sign, point and exponent; no hex, nan or inf. */
W_RT bool Str_parse_float(WStr *s, double *out) {
  int64_t a = 0, b = s->len;
  while (a < b && is_ws(s->data[a])) a++;
  while (b > a && is_ws(s->data[b - 1])) b--;
  if (a == b) return false;
  bool digit = false;
  for (int64_t i = a; i < b; i++) {
    char c = s->data[i];
    if (c >= '0' && c <= '9') digit = true;
    else if (!(c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E')) return false;
  }
  if (!digit) return false;
  /* any length, as the interpreter reads it */
  char small[408];
  char *t = b - a < (int64_t)sizeof small ? small : (char *)w_raw((size_t)(b - a) + 1);
  memcpy(t, s->data + a, (size_t)(b - a));
  t[b - a] = 0;
  char *end;
  double x = strtod(t, &end);
  bool ok = end == t + (b - a);
  if (t != small) free(t);
  if (!ok) return false;
  *out = x;
  return true;
}
W_RT void Clo_drop(WClo *c) {
  if (!w_rc_dec(&c->rc)) return;
  if (((WHdr *)c - 1)->kids == (void (*)(void *))c->drop) { w_release(c); return; }  /* captures that hold closures */
  if (c->drop) c->drop(c);
  w_free(c);
}
W_RT void Clo_share(WClo *c) { if (w_rc_share(&c->rc) && c->share) w_share(c, (void (*)(void *))c->share); }
W_RT void Clo_show(WBuf *b, WClo *c, bool top) { (void)c; (void)top; buf_puts(b, "<fn>"); }
/* through w_release (its kids are w_dyn_kids), so a long chain of `dyn`
   values is freed without recursing */
W_RT void Dyn_drop(WDyn *d) { if (w_rc_dec(&d->rc)) w_release(d); }
W_RT void Dyn_share(WDyn *d) { if (w_rc_share(&d->rc)) w_share(d, (void (*)(void *))d->vt->h.share); }
static void w_dyn_show_k(WBuf *b, void *o, int64_t top) { WDyn *d = (WDyn *)o; d->vt->h.show(b, d, top != 0); }
W_RT void Dyn_show(WBuf *b, WDyn *d, bool top) { w_show(b, w_dyn_show_k, d, top); }
W_RT void w_dyn_kids(void *o) { WDyn *d = (WDyn *)o; d->vt->h.drop(d); }
W_RT WCap *cap_new(int kind) { WCap *c = (WCap *)w_alloc_k(sizeof(WCap), WK_CAP); memset(c, 0, sizeof *c); c->rc = 1; c->kind = kind; c->fd = -1; return c; }
W_RT void Cap_release(WCap *c) { if (c->rv) w_rv_release(c); for (int i = 0; i < c->nhosts; i++) free(c->hosts[i]); free(c->hosts); free(c->root); if (c->tls) w_tls_release(c); if (c->fd >= 0) w_sock_release_fd(c->fd); w_free(c); }
W_RT void Cap_drop(WCap *c) { if (w_rc_dec(&c->rc)) Cap_release(c); }
W_RT void Cap_show(WBuf *b, WCap *c, bool top) {
  (void)top;
  static const char *names[] = { "<Root>", "<Net>", "<Fs>", "<Stdout>", "<Clock>", "<Listen>", "<Listener>", "<Conn>", "<Rand>", "<Stdin>", "<Env>", "<Exec>", "<Foreign>", "<Stderr>", "<Revoker>" };
  buf_puts(b, names[c->kind]);
}
pthread_mutex_t w_rv_lock = PTHREAD_MUTEX_INITIALIZER;
WCap *w_revocable_socks;
W_RT bool w_rv_off(WRevoke *r) {
  for (; r; r = r->parent) if (__atomic_load_n(&r->off, __ATOMIC_ACQUIRE)) return true;
  return false;
}
W_RT bool w_revoked(WCap *c) { return c->rv && w_rv_off(c->rv); }
W_RT bool w_rv_under(WRevoke *r, WRevoke *top) {
  for (; r; r = r->parent) if (r == top) return true;
  return false;
}
W_RT void w_rv_drop(WRevoke *r) {
  while (r && w_rc_dec(&r->rc)) { WRevoke *p = r->parent; free(r); r = p; }
}
W_RT void w_rv_release(WCap *c) {
  if (c->kind == CAP_LISTENER || c->kind == CAP_CONN) {
    pthread_mutex_lock(&w_rv_lock);
    if (c->rprev) c->rprev->rnext = c->rnext; else if (w_revocable_socks == c) w_revocable_socks = c->rnext;
    if (c->rnext) c->rnext->rprev = c->rprev;
    pthread_mutex_unlock(&w_rv_lock);
  }
  w_rv_drop(c->rv);
  c->rv = NULL;
}
/* `to` was made from `from`: it keeps from's switch; a socket is closed at
   once if that switch is already off */
W_RT void w_rv_inherit(WCap *to, WCap *from) {
  if (!from->rv) return;
  w_rc_inc(&from->rv->rc);
  to->rv = from->rv;
  if (to->kind != CAP_LISTENER && to->kind != CAP_CONN) return;
  pthread_mutex_lock(&w_rv_lock);
  to->rnext = w_revocable_socks;
  if (w_revocable_socks) w_revocable_socks->rprev = to;
  w_revocable_socks = to;
  bool off = w_rv_off(to->rv);
  pthread_mutex_unlock(&w_rv_lock);
  if (off) Cap_sock_close(to);
}
W_RT WCap *Cap_copy_kind(WCap *x) {
  WCap *c = cap_new(x->kind);
  c->hosts = (char **)w_raw(sizeof(char *) * (size_t)(x->nhosts + 1));
  for (int i = 0; i < x->nhosts; i++) c->hosts[i] = w_strdup(x->hosts[i]);
  c->nhosts = x->nhosts;
  c->root = x->root ? w_strdup(x->root) : NULL;
  c->ro = x->ro;
  return c;
}
/* `x.revocable()`: the copy, and through *revoker its switch */
W_RT WCap *Cap_revocable(WCap *x, WCap **revoker) {
  WRevoke *r = (WRevoke *)w_raw(sizeof(WRevoke));
  /* two holders, the copy and the Revoker; counted atomically (negative),
     since tasks on other threads may hold either */
  r->rc = -2; r->off = 0; r->parent = x->rv;
  if (x->rv) w_rc_inc(&x->rv->rc);
  WCap *c = Cap_copy_kind(x);
  c->rv = r;
  WCap *k = cap_new(CAP_REVOKER);
  k->rv = r;
  *revoker = k;
  return c;
}
W_RT bool Cap_is_revoked(WCap *k) { return w_revoked(k); }
W_RT void Cap_revoke(WCap *k) {
  WRevoke *r = k->rv;
  if (!r || __atomic_exchange_n(&r->off, 1, __ATOMIC_ACQ_REL)) return;
  pthread_mutex_lock(&w_rv_lock);
  for (WCap *c = w_revocable_socks; c; c = c->rnext) if (w_rv_under(c->rv, r)) Cap_sock_close(c);
  pthread_mutex_unlock(&w_rv_lock);
}
char **g_hosts;
int g_nhosts;
char *g_fs_root;
bool g_fs_ro;
char **g_listen;
int g_nlisten;
char **g_env;
int g_nenv;
char **g_exec;
int g_nexec;
char **g_foreign;
int g_nforeign;
bool g_no_sandbox;
char **g_args;
int g_nargs;
uint64_t g_rand_state;
bool g_rand_seeded;

/* An Exec grant, `git` or `git status|remote -v`, as an allowlist entry:
   the program, then a newline before each argument prefix, its words
   separated by single spaces (`git\nstatus\nremote -v`). NULL if malformed:
   an empty prefix, or prefixes for `*`. */
W_RT char *w_exec_entry(const char *g) {
  const char *sp = strchr(g, ' ');
  if (!sp) return w_strdup(g);
  size_t pn = (size_t)(sp - g);
  if (pn == 0 || (pn == 1 && g[0] == '*')) return NULL;
  size_t n = strlen(g);
  char *e = (char *)w_raw(n + 2);
  memcpy(e, g, pn);
  size_t o = pn;
  const char *p = sp + 1;
  for (;;) {
    const char *bar = strchr(p, '|');
    const char *end = bar ? bar : p + strlen(p);
    e[o++] = '\n';
    size_t start = o;
    for (const char *c = p; c < end;) {
      while (c < end && *c == ' ') c++;
      const char *w = c;
      while (c < end && *c != ' ') c++;
      if (c > w) { if (o > start) e[o++] = ' '; memcpy(e + o, w, (size_t)(c - w)); o += (size_t)(c - w); }
    }
    if (o == start || memchr(e + start, '\n', o - start)) { free(e); return NULL; }
    if (!bar) break;
    p = bar + 1;
  }
  e[o] = 0;
  return e;
}

W_RT void w_usage(const char *m) {
  fprintf(stderr, "%s\nusage: PROGRAM [--grant KIND=VALUE]... [--seed N] [--no-sandbox] [-- ARGS...]\n", m);
  exit(2);
}
W_RT void w_init(int argc, char **argv) {
  signal(SIGPIPE, SIG_IGN);   /* a peer closing a socket is an error value, not a crash */
  w_seed_hash();
  {
    struct rlimit rl;
    if (getrlimit(RLIMIT_NOFILE, &rl) == 0) {
      rlim_t want = rl.rlim_max < (rlim_t)65536 ? rl.rlim_max : (rlim_t)65536;
      if (rl.rlim_cur < want) { rl.rlim_cur = want; setrlimit(RLIMIT_NOFILE, &rl); }
    }
  }
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--") == 0) { g_args = argv + i + 1; g_nargs = argc - i - 1; break; }
    if (strcmp(argv[i], "--seed") == 0 && i + 1 < argc) { g_rand_state = (uint64_t)strtoll(argv[++i], NULL, 10); g_rand_seeded = true; continue; }
    if (strcmp(argv[i], "--no-sandbox") == 0) { g_no_sandbox = true; continue; }
    if (strcmp(argv[i], "--grant") != 0 || i + 1 >= argc) w_usage("unknown argument");
    const char *g = argv[++i];
    if (strncmp(g, "net=", 4) == 0) {
      g_hosts = (char **)w_realloc(g_hosts, sizeof(char *) * (size_t)(g_nhosts + 1));
      g_hosts[g_nhosts++] = w_strdup(g + 4);
    } else if (strncmp(g, "fs=", 3) == 0) {
      char *v = w_strdup(g + 3);
      size_t n = strlen(v);
      if (n > 3 && strcmp(v + n - 3, ":ro") == 0) { v[n - 3] = 0; g_fs_ro = true; }
      char *r = realpath(v, NULL);
      struct stat st;
      if (!r || stat(r, &st) != 0 || !S_ISDIR(st.st_mode)) w_usage("fs grant: no such directory");
      free(v);
      g_fs_root = r;
    } else if (strncmp(g, "env=", 4) == 0) {
      g_env = (char **)w_realloc(g_env, sizeof(char *) * (size_t)(g_nenv + 1));
      g_env[g_nenv++] = w_strdup(g + 4);
    } else if (strncmp(g, "exec=", 5) == 0) {
      g_exec = (char **)w_realloc(g_exec, sizeof(char *) * (size_t)(g_nexec + 1));
      g_exec[g_nexec] = w_exec_entry(g + 5);
      if (!g_exec[g_nexec++]) w_usage("exec grant: write PROGRAM, or PROGRAM ARGS|ARGS... with no empty prefix and not for *");
    } else if (strncmp(g, "foreign=", 8) == 0) {
      /* one library, or several separated by commas: foreign=z,sqlite3 */
      for (const char *p = g + 8;;) {
        const char *comma = strchr(p, ',');
        size_t n = comma ? (size_t)(comma - p) : strlen(p);
        if (n == 0) w_usage("foreign grant: name C libraries, as in foreign=z or foreign=z,sqlite3");
        g_foreign = (char **)w_realloc(g_foreign, sizeof(char *) * (size_t)(g_nforeign + 1));
        g_foreign[g_nforeign++] = w_strndup(p, n);
        if (!comma) break;
        p = comma + 1;
      }
    } else if (strncmp(g, "listen=", 7) == 0) {
      g_listen = (char **)w_realloc(g_listen, sizeof(char *) * (size_t)(g_nlisten + 1));
      g_listen[g_nlisten++] = w_strdup(g + 7);
    } else w_usage("unknown grant");
  }
  /* restrict the whole process to the grants (limen_sandbox.h), before any
     thread starts; --no-sandbox turns it off, for debugging */
  if (!g_no_sandbox) {
    /* the programs Exec may start, for the sandbox: each entry's name */
    WBuf pb; buf_init(&pb);
    for (int i = 0; i < g_nexec; i++) {
      const char *nl = strchr(g_exec[i], '\n');
      if (i > 0) buf_puts(&pb, "\n");
      buf_putn(&pb, g_exec[i], nl ? (int64_t)(nl - g_exec[i]) : (int64_t)strlen(g_exec[i]));
    }
    buf_putn(&pb, "", 1);
    WSandboxSpec sb = { g_fs_root, g_fs_root ? (int)g_fs_ro : 0, g_nhosts > 0 || g_nlisten > 0, g_nexec > 0, pb.p };
    char err[512];
    if (w_sandbox_enter(&sb, err, sizeof err) < 0) {
      fprintf(stderr, "limen: %s (run with --no-sandbox to go without it)\n", err);
      exit(1);
    }
    free(pb.p);
  }
}
W_RT WCap *w_root(void) { return cap_new(CAP_ROOT); }
W_RT WCap *Cap_root_field(WCap *root, const char *f) {
  (void)root;
  if (strcmp(f, "net") == 0) {
    WCap *c = cap_new(CAP_NET);
    c->hosts = (char **)w_raw(sizeof(char *) * (size_t)(g_nhosts + 1));
    for (int i = 0; i < g_nhosts; i++) c->hosts[i] = w_strdup(g_hosts[i]);
    c->nhosts = g_nhosts;
    return c;
  }
  if (strcmp(f, "fs") == 0) {
    WCap *c = cap_new(CAP_FS);
    c->root = g_fs_root ? w_strdup(g_fs_root) : NULL;
    c->ro = g_fs_root ? g_fs_ro : true;
    return c;
  }
  if (strcmp(f, "stdout") == 0) return cap_new(CAP_STDOUT);
  if (strcmp(f, "stderr") == 0) return cap_new(CAP_STDERR);
  if (strcmp(f, "listen") == 0) {
    WCap *c = cap_new(CAP_LISTEN);
    c->hosts = (char **)w_raw(sizeof(char *) * (size_t)(g_nlisten + 1));
    for (int i = 0; i < g_nlisten; i++) c->hosts[i] = w_strdup(g_listen[i]);
    c->nhosts = g_nlisten;
    return c;
  }
  if (strcmp(f, "exec") == 0) {
    WCap *c = cap_new(CAP_EXEC);
    c->hosts = (char **)w_raw(sizeof(char *) * (size_t)(g_nexec + 1));
    for (int i = 0; i < g_nexec; i++) c->hosts[i] = w_strdup(g_exec[i]);
    c->nhosts = g_nexec;
    return c;
  }
  if (strcmp(f, "foreign") == 0) {
    WCap *c = cap_new(CAP_FOREIGN);
    c->hosts = (char **)w_raw(sizeof(char *) * (size_t)(g_nforeign + 1));
    for (int i = 0; i < g_nforeign; i++) c->hosts[i] = w_strdup(g_foreign[i]);
    c->nhosts = g_nforeign;
    return c;
  }
  if (strcmp(f, "rand") == 0) return cap_new(CAP_RAND);
  if (strcmp(f, "stdin") == 0) return cap_new(CAP_STDIN);
  if (strcmp(f, "env") == 0) {
    WCap *c = cap_new(CAP_ENV);
    c->hosts = (char **)w_raw(sizeof(char *) * (size_t)(g_nenv + 1));
    for (int i = 0; i < g_nenv; i++) c->hosts[i] = w_strdup(g_env[i]);
    c->nhosts = g_nenv;
    return c;
  }
  return cap_new(CAP_CLOCK);
}
W_RT uint64_t w_rand_next(void) {
  uint64_t z = __atomic_add_fetch(&g_rand_state, 0x9E3779B97F4A7C15ULL, __ATOMIC_RELAXED);
  z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
  z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
  return z ^ (z >> 31);
}
/* `n` bytes from the operating system's secure generator. */
W_RT void w_os_random(unsigned char *p, int64_t n, const char *where) {
#if defined(__APPLE__)
  (void)where;
  arc4random_buf(p, (size_t)n);
#else
  int64_t got = 0;
#if defined(__linux__)
  while (got < n) {
    ssize_t r = getrandom(p + got, (size_t)(n - got), 0);
    if (r < 0 && errno == EINTR) continue;
    if (r <= 0) break;
    got += r;
  }
#endif
  if (got < n) {
    int fd = open("/dev/urandom", O_RDONLY | O_CLOEXEC);
    while (fd >= 0 && got < n) {
      ssize_t r = read(fd, p + got, (size_t)(n - got));
      if (r < 0 && errno == EINTR) continue;
      if (r <= 0) break;
      got += r;
    }
    if (fd >= 0) close(fd);
  }
  if (got < n) w_panic_c("rand: cannot read the system's secure random generator", where);
#endif
}
__thread unsigned char w_rand_buf[W_RAND_BUF];
__thread int w_rand_left;
W_RT uint64_t w_rand_u64(const char *where) {
  if (g_rand_seeded) return w_rand_next();
  if (w_rand_left < 8) { w_os_random(w_rand_buf, W_RAND_BUF, where); w_rand_left = W_RAND_BUF; }
  uint64_t z;
  memcpy(&z, w_rand_buf + (W_RAND_BUF - w_rand_left), 8);
  w_rand_left -= 8;
  return z;
}
W_RT int64_t Cap_rand_int(int64_t lo, int64_t hi, const char *where) {
  if (hi <= lo) {
    char m[96];
    snprintf(m, sizeof m, "rand.int: empty range %lld..%lld", (long long)lo, (long long)hi);
    w_panic_c(m, where);
  }
  uint64_t span = (uint64_t)hi - (uint64_t)lo;
  if (g_rand_seeded) return (int64_t)((uint64_t)lo + w_rand_next() % span);
  /* the draws below 2^64 mod span would make the low values likelier */
  uint64_t skip = -span % span, z;
  do z = w_rand_u64(where); while (z < skip);
  return (int64_t)((uint64_t)lo + z % span);
}
W_RT double Cap_rand_float(void) { return (double)(w_rand_u64("rand.float") >> 11) * 0x1.0p-53; }
/* Rand.bytes: `n` bytes from the operating system's secure generator, for
   keys, tokens and UUIDs. With --seed they come from splitmix64 instead, so
   a seeded run repeats: each output gives eight bytes, low byte first, the
   last output's unused bytes dropped, as in the interpreter. */
W_RT WStr *Cap_rand_bytes(int64_t n, const char *where) {
  if (n < 0) { char m[96]; snprintf(m, sizeof m, "rand.bytes: negative length %lld", (long long)n); w_panic_c(m, where); }
  WStr *s = Str_new(n);
  unsigned char *p = (unsigned char *)s->data;
  if (g_rand_seeded) {
    for (int64_t i = 0; i < n; i += 8) {
      uint64_t z = w_rand_next();
      for (int64_t j = 0; j < 8 && i + j < n; j++) p[i + j] = (unsigned char)(z >> (8 * j));
    }
    return s;
  }
  w_os_random(p, n, where);
  return s;
}
W_RT L_Str *Cap_args(void) {
  L_Str *out = L_Str_new(g_nargs);
  for (int i = 0; i < g_nargs; i++) L_Str_push(&out, Str_from(g_args[i], (int64_t)strlen(g_args[i])));
  return out;
}
W_RT WStr *Cap_env_get(WCap *env, WStr *name, bool *ok) {
  char *n = w_strndup(name->data, (size_t)name->len);
  bool allowed = false;
  for (int i = 0; i < env->nhosts; i++) if (strcmp(env->hosts[i], "*") == 0 || strcmp(env->hosts[i], n) == 0) allowed = true;
  if (w_revoked(env)) allowed = false;
  const char *v = allowed ? getenv(n) : NULL;
  free(n);
  if (!v) { *ok = false; return NULL; }
  *ok = true;
  return Str_from(v, (int64_t)strlen(v));
}
W_RT WIo io_ok(WStr *v) { WIo r = { 0, v, NULL, NULL }; return r; }
W_RT WIo io_ok_ptr(void *p) { WIo r = { 0, NULL, NULL, p }; return r; }
W_RT WIo io_err(int kind, const char *m) { WIo r = { kind, NULL, Str_from(m, (int64_t)strlen(m)), NULL }; return r; }

W_RT void lower_ascii(char *s) { for (; *s; s++) if (*s >= 'A' && *s <= 'Z') *s = (char)(*s + 32); }

/* Parse "host", "host:port" or "*". port -1 = any. */
W_RT void parse_entry(const char *e, char *host, size_t hn, int *port) {
  const char *c = strrchr(e, ':');
  char *end;
  if (c && c[1]) {
    long p = strtol(c + 1, &end, 10);
    if (*end == 0) { snprintf(host, hn, "%.*s", (int)(c - e), e); *port = (int)p; lower_ascii(host); return; }
  }
  snprintf(host, hn, "%s", e); *port = -1; lower_ascii(host);
}
W_RT bool net_permits(WCap *n, const char *host, int port) {
  for (int i = 0; i < n->nhosts; i++) {
    if (strcmp(n->hosts[i], "*") == 0) return true;
    char h[512]; int p;
    parse_entry(n->hosts[i], h, sizeof h, &p);
    if (strcmp(h, host) == 0 && (p < 0 || p == port)) return true;
  }
  return false;
}
W_RT WCap *Cap_net_only(WCap *n, WStr *req_s) {
  char *req = w_strndup(req_s->data, (size_t)req_s->len);
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
  w_rv_inherit(c, n);
  c->hosts = (char **)w_raw(sizeof(char *));
  if (covered) { c->hosts[0] = req; c->nhosts = 1; } else free(req);
  return c;
}

/* Strict URL check, so the host we allow is the host we connect to:
   http or https only; no userinfo (`@`); host made of [A-Za-z0-9.-];
   port plain digits in 1..65535; no spaces or control characters anywhere.
   Mirrors Caps.parse_url in the interpreter. */
W_RT int parse_url(const char *u, char *host, size_t hn, int *port, int *is_https, const char **path, char *err, size_t en) {
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
  size_t from = 0;
  if (alen > 0 && auth[0] == '[') {   /* [ipv6], kept with its brackets */
    const char *rb = memchr(auth, ']', alen);
    char inner[64]; struct in6_addr probe;
    size_t il = rb ? (size_t)(rb - auth - 1) : 0;
    if (!rb || il == 0 || il >= sizeof inner) { snprintf(err, en, "malformed IPv6 address in URL"); return 0; }
    memcpy(inner, auth + 1, il); inner[il] = 0;
    if (inet_pton(AF_INET6, inner, &probe) != 1) { snprintf(err, en, "malformed IPv6 address in URL"); return 0; }
    from = il + 2;
    if (from < alen && auth[from] != ':') { snprintf(err, en, "malformed host in URL"); return 0; }
  }
  for (size_t i = from; i < alen; i++) {
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
  *is_https = https;
  *path = auth + alen;
  return 1;
}

W_RT WStr *read_fd(int fd) {
  WBuf b; buf_init(&b);
  char *tmp = (char *)w_raw(65536);   /* on the heap: task stacks are small */
  ssize_t n;
  while ((n = read(fd, tmp, 65536)) > 0) buf_putn(&b, tmp, n);
  free(tmp);
  return buf_take(&b);
}
W_RT WIo w_fs_fail(WFsErr *e);
W_RT WIo w_fs_fail(WFsErr *e) { return io_err(e->kind, e->msg); }
W_RT WCap *Cap_fs_under(WCap *fs, WStr *rel) {
  WCap *c = cap_new(CAP_FS);
  w_rv_inherit(c, fs);
  c->ro = fs->ro;
  if (!fs->root) return c;
  c->root = w_fs_under(W_FS_TOP, fs->root, rel->data, (size_t)rel->len);
  return c;
}
W_RT WCap *Cap_fs_read_only(WCap *fs) {
  WCap *c = cap_new(CAP_FS);
  w_rv_inherit(c, fs);
  c->root = fs->root ? w_strdup(fs->root) : NULL;
  c->ro = true;
  return c;
}
W_RT bool Cap_fs_is_read_only(WCap *fs) { return fs->ro || w_revoked(fs); }
W_RT WIo Cap_fs_read(WCap *fs, WStr *rel) {
  if (w_revoked(fs)) return io_err(1, "fs: revoked");
  if (!fs->root) return io_err(1, W_FS_NOT_GRANTED);
  WFsErr e;
  int fd = w_fs_open(W_FS_TOP, fs->root, rel->data, (size_t)rel->len, W_FS_READ, &e);
  if (fd < 0) return w_fs_fail(&e);
  WStr *s = read_fd(fd);
  close(fd);
  return io_ok(s);
}
/* Fs.read_at(path, offset, n): up to n bytes from offset, fewer at the end
   of the file and none past it; for reading a file larger than memory a
   piece at a time. The file is opened as for read, each time, so no open
   file outlives the call. */
#define W_READ_AT_MAX ((int64_t)64 << 20)
W_RT WIo Cap_fs_read_at(WCap *fs, WStr *rel, int64_t off, int64_t n) {
  if (w_revoked(fs)) return io_err(1, "fs: revoked");
  if (!fs->root) return io_err(1, W_FS_NOT_GRANTED);
  if (off < 0 || n < 0) return io_err(2, "fs: read_at needs an offset and a count of at least 0");
  if (n > W_READ_AT_MAX) return io_err(2, "fs: read_at reads at most 64 MB at a time");
  WFsErr e;
  int fd = w_fs_open(W_FS_TOP, fs->root, rel->data, (size_t)rel->len, W_FS_READ, &e);
  if (fd < 0) return w_fs_fail(&e);
  WStr *s = Str_new(n);
  int64_t got = 0;
  while (got < n) {
    ssize_t k = pread(fd, s->data + got, (size_t)(n - got), (off_t)(off + got));
    if (k < 0 && errno == EINTR) continue;
    if (k < 0) { int err = errno; close(fd); Str_drop(s); char m[1300]; snprintf(m, sizeof m, "fs: `%.*s`: %s", (int)rel->len, rel->data, strerror(err)); return io_err(2, m); }
    if (k == 0) break;
    got += k;
  }
  close(fd);
  s->len = got;
  s->data[got] = 0;
  return io_ok(s);
}
W_RT WIo w_fs_put(WCap *fs, WStr *rel, WStr *data, int mode) {
  if (w_revoked(fs)) return io_err(1, "fs: revoked");
  if (fs->root && fs->ro) return io_err(1, "fs: this capability is read-only");
  if (!fs->root) return io_err(1, W_FS_NOT_GRANTED);
  WFsErr e;
  int fd = w_fs_open(W_FS_TOP, fs->root, rel->data, (size_t)rel->len, mode, &e);
  if (fd < 0) return w_fs_fail(&e);
  int err = w_fs_write_all(fd, data->data, (size_t)data->len);
  close(fd);
  if (err) { char msg[1300]; snprintf(msg, sizeof msg, "fs: `%.*s`: %s", (int)rel->len, rel->data, strerror(err)); return io_err(2, msg); }
  return io_ok(NULL);
}
W_RT WIo Cap_fs_write(WCap *fs, WStr *rel, WStr *data) { return w_fs_put(fs, rel, data, W_FS_WRITE); }
W_RT WIo Cap_fs_append(WCap *fs, WStr *rel, WStr *data) { return w_fs_put(fs, rel, data, W_FS_APPEND); }
/* The absolute path of `rel`, checked as for writing: for handing to C. */
W_RT WIo Cap_fs_full_path(WCap *fs, WStr *rel) {
  if (w_revoked(fs)) return io_err(1, "fs: revoked");
  if (!fs->root) return io_err(1, W_FS_NOT_GRANTED);
  WFsErr e;
  char *p = w_fs_full_path(W_FS_TOP, fs->root, rel->data, (size_t)rel->len, &e);
  if (!p) return w_fs_fail(&e);
  WStr *s = Str_from(p, (int64_t)strlen(p));
  free(p);
  return io_ok(s);
}
W_RT bool Cap_fs_exists(WCap *fs, WStr *rel) {
  if (w_revoked(fs) || !fs->root) return false;
  WFsErr e; struct stat st;
  return w_fs_stat(W_FS_TOP, fs->root, rel->data, (size_t)rel->len, &st, &e) == 0;
}
W_RT int w_cmp_cstr(const void *a, const void *b) { return strcmp(*(char *const *)a, *(char *const *)b); }
W_RT WIo Cap_fs_list(WCap *fs, WStr *rel) {
  if (w_revoked(fs)) return io_err(1, "fs: revoked");
  if (!fs->root) return io_err(1, W_FS_NOT_GRANTED);
  WFsErr e;
  DIR *d = w_fs_opendir(W_FS_TOP, fs->root, rel->data, (size_t)rel->len, &e);
  if (!d) return w_fs_fail(&e);
  char **names = NULL; int n = 0;
  struct dirent *de;
  while ((de = readdir(d))) {
    if (strcmp(de->d_name, ".") == 0 || strcmp(de->d_name, "..") == 0) continue;
    names = (char **)w_realloc(names, sizeof(char *) * (size_t)(n + 1));
    names[n++] = w_strdup(de->d_name);
  }
  closedir(d);
  if (n > 1) qsort(names, (size_t)n, sizeof(char *), w_cmp_cstr); /* never qsort(NULL, 0): glibc marks it nonnull */
  L_Str *out = L_Str_new(n);
  for (int i = 0; i < n; i++) { L_Str_push(&out, Str_from(names[i], (int64_t)strlen(names[i]))); free(names[i]); }
  free(names);
  return io_ok_ptr(out);
}
W_RT bool Cap_fs_is_dir(WCap *fs, WStr *rel) {
  if (w_revoked(fs) || !fs->root) return false;
  WFsErr e; struct stat st;
  return w_fs_stat(W_FS_TOP, fs->root, rel->data, (size_t)rel->len, &st, &e) == 0 && S_ISDIR(st.st_mode);
}
W_RT WIo Cap_fs_remove(WCap *fs, WStr *rel) {
  if (w_revoked(fs)) return io_err(1, "fs: revoked");
  if (fs->root && fs->ro) return io_err(1, "fs: this capability is read-only");
  if (!fs->root) return io_err(1, W_FS_NOT_GRANTED);
  WFsErr e;
  if (w_fs_remove(W_FS_TOP, fs->root, rel->data, (size_t)rel->len, &e) != 0) return w_fs_fail(&e);
  return io_ok(NULL);
}
W_RT WIo Cap_fs_make_dir(WCap *fs, WStr *rel) {
  if (w_revoked(fs)) return io_err(1, "fs: revoked");
  if (fs->root && fs->ro) return io_err(1, "fs: this capability is read-only");
  if (!fs->root) return io_err(1, W_FS_NOT_GRANTED);
  WFsErr e;
  if (w_fs_mkdir(W_FS_TOP, fs->root, rel->data, (size_t)rel->len, &e) != 0) return w_fs_fail(&e);
  return io_ok(NULL);
}
/* Move `from` to `to` in one step, replacing what is at `to` (rename(2));
   never the granted directory itself. */
W_RT WIo Cap_fs_rename(WCap *fs, WStr *from, WStr *to) {
  if (w_revoked(fs)) return io_err(1, "fs: revoked");
  if (fs->root && fs->ro) return io_err(1, "fs: this capability is read-only");
  if (!fs->root) return io_err(1, W_FS_NOT_GRANTED);
  WFsErr e;
  if (w_fs_rename(W_FS_TOP, fs->root, from->data, (size_t)from->len, to->data, (size_t)to->len, &e) != 0) return w_fs_fail(&e);
  return io_ok(NULL);
}
W_RT WIo Cap_fs_info(WCap *fs, WStr *rel) {
  if (w_revoked(fs)) return io_err(1, "fs: revoked");
  if (!fs->root) return io_err(1, W_FS_NOT_GRANTED);
  WFsErr e; struct stat st;
  if (w_fs_stat(W_FS_TOP, fs->root, rel->data, (size_t)rel->len, &st, &e) != 0) return w_fs_fail(&e);
  WFileInfo *i = (WFileInfo *)w_raw(sizeof(WFileInfo));
  i->size = (int64_t)st.st_size;
#ifdef __APPLE__
  i->mtime_ms = (int64_t)st.st_mtimespec.tv_sec * 1000 + st.st_mtimespec.tv_nsec / 1000000;
#else
  i->mtime_ms = (int64_t)st.st_mtim.tv_sec * 1000 + st.st_mtim.tv_nsec / 1000000;
#endif
  i->is_file = S_ISREG(st.st_mode);
  i->is_dir = S_ISDIR(st.st_mode);
  return io_ok_ptr(i);
}
int w_virtual;
int64_t w_vnow_ns;
W_RT void w_virtual_start(void) { w_virtual = 1; }
W_RT int64_t Cap_now_ms(void) {
  if (w_virtual) return W_VIRTUAL_START_MS + __atomic_load_n(&w_vnow_ns, __ATOMIC_ACQUIRE) / 1000000;
  struct timeval tv; gettimeofday(&tv, NULL); return (int64_t)tv.tv_sec * 1000 + tv.tv_usec / 1000;
}
#if defined(__aarch64__) || defined(__arm64__)
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
W_RT void w_frame_init(void **frame, WFiber *f) {
  memset(frame, 0, sizeof(void *) * W_FRAME_WORDS);
  frame[0] = f;                         /* x19 -> argument of the trampoline */
  frame[11] = (void *)w_ctx_tramp;      /* x30 */
}
#elif defined(__x86_64__)
__asm__(
  ".text\n.globl " W_ASM_SYM(w_ctx_switch) "\n" W_ASM_SYM(w_ctx_switch) ":\n"
  "  pushq %rbp\n  pushq %rbx\n  pushq %r12\n  pushq %r13\n  pushq %r14\n  pushq %r15\n"
  "  movq %rsp, (%rdi)\n  movq %rsi, %rsp\n"
  "  popq %r15\n  popq %r14\n  popq %r13\n  popq %r12\n  popq %rbx\n  popq %rbp\n  ret\n"
  ".globl " W_ASM_SYM(w_ctx_tramp) "\n" W_ASM_SYM(w_ctx_tramp) ":\n"
  "  movq %r12, %rdi\n  call " W_ASM_SYM(w_fiber_main) "\n  ud2\n");
W_RT void w_frame_init(void **frame, WFiber *f) {
  memset(frame, 0, sizeof(void *) * W_FRAME_WORDS);
  frame[3] = f;                         /* r12 */
  frame[6] = (void *)w_ctx_tramp;       /* return address */
}
#endif
WWorker *w_workers[W_MAX_WORKERS];
int w_nworkers;
__thread WWorker *w_self_tls;
pthread_mutex_t w_global_mu = PTHREAD_MUTEX_INITIALIZER;
WFiber *w_global_head, *w_global_tail;
int64_t w_queued;
pthread_mutex_t w_idle_mu = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t w_idle_cv = PTHREAD_COND_INITIALIZER;
int64_t w_nidle;
int64_t w_io_waiters;
int64_t w_tm_count;
int w_main_done;
size_t w_stack_size = 1024 * 1024;
int64_t w_fibers_alive;

/* A fiber may resume on another thread, so the worker is never cached
   across a switch: always read it through this function. */
__attribute__((noinline)) W_RT WWorker *w_self(void) {
  WWorker *w = w_self_tls;
  __asm__ volatile("" ::: "memory");
  return w;
}

/* ---------- `--profile` ---------- */

static __thread int32_t w_prof_cur;
static const char *const *w_prof_names;
static int64_t *w_prof_calls, *w_prof_samples;
static int32_t w_prof_n;
static int64_t w_prof_t0;

W_RT int32_t w_prof_enter(int32_t id) {
  int32_t prev = w_prof_cur;
  if (w_prof_calls) __atomic_add_fetch(&w_prof_calls[id], 1, __ATOMIC_RELAXED);
  w_prof_cur = id;
  return prev;
}
W_RT void w_prof_leave(int32_t *prev) { w_prof_cur = *prev; }
static void w_prof_tick(int sig) {
  (void)sig;
  int32_t c = w_prof_cur;
  if (c >= 0 && c < w_prof_n) __atomic_add_fetch(&w_prof_samples[c], 1, __ATOMIC_RELAXED);
}
static int w_prof_cmp(const void *a, const void *b) {
  int64_t x = w_prof_samples[*(const int32_t *)a], y = w_prof_samples[*(const int32_t *)b];
  return x < y ? 1 : x > y ? -1 : *(const int32_t *)a - *(const int32_t *)b;
}
static void w_prof_report(void) {
  struct itimerval off; memset(&off, 0, sizeof off);
  setitimer(ITIMER_PROF, &off, NULL);
  int64_t total = 0;
  for (int32_t i = 0; i < w_prof_n; i++) total += w_prof_samples[i];
  int32_t *order = malloc(sizeof(int32_t) * (size_t)w_prof_n);
  if (!order) return;
  for (int32_t i = 0; i < w_prof_n; i++) order[i] = i;
  qsort(order, (size_t)w_prof_n, sizeof(int32_t), w_prof_cmp);
  /* the CPU the process used, shared out by the samples: the timer's
     period is a request the system rounds */
  struct rusage ru; getrusage(RUSAGE_SELF, &ru);
  double cpu = (double)ru.ru_utime.tv_sec + (double)ru.ru_utime.tv_usec / 1e6 + (double)ru.ru_stime.tv_sec + (double)ru.ru_stime.tv_usec / 1e6;
  double wall = (double)(w_mono_ns() - w_prof_t0) / 1e9;
  fprintf(stderr, "\nprofile: %.2f s of CPU in %.2f s, %lld samples\n", cpu, wall, (long long)total);
  fprintf(stderr, "  %9s %6s %12s  %s\n", "own CPU", "", "calls", "function");
  int shown = 0;
  for (int32_t k = 0; k < w_prof_n && shown < 25; k++) {
    int32_t i = order[k];
    if (w_prof_samples[i] == 0 && w_prof_calls[i] == 0) continue;
    if (w_prof_samples[i] == 0 && shown >= 10) continue;
    double share = total ? (double)w_prof_samples[i] / (double)total : 0.0;
    fprintf(stderr, "  %7.3f s %5.1f%% %12lld  %s\n", share * cpu, 100.0 * share, (long long)w_prof_calls[i], w_prof_names[i]);
    shown++;
  }
  free(order);
}
/* Called at the start of main in a program built with --profile. Index 0
   is "runtime and C": time outside any Limen function. */
W_RT void w_prof_start(const char *const *names, int64_t *calls, int64_t *samples, int32_t n) {
  w_prof_names = names; w_prof_calls = calls; w_prof_samples = samples; w_prof_n = n;
  w_prof_t0 = w_mono_ns();
  struct sigaction sa; memset(&sa, 0, sizeof sa);
  sa.sa_handler = w_prof_tick; sa.sa_flags = SA_RESTART;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGPROF, &sa, NULL);
  struct itimerval t; memset(&t, 0, sizeof t);
  t.it_interval.tv_usec = 1000; t.it_value.tv_usec = 1000;
  setitimer(ITIMER_PROF, &t, NULL);
  atexit(w_prof_report);
}

W_RT void w_switch(WFiber *from, WFiber *to, int from_exiting) {
  /* with --profile, the function each task is in goes with it (w_prof_calls
     is set before any other thread starts, and never again) */
  int prof = w_prof_calls != NULL;
  if (prof) from->prof = w_prof_cur;
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
  if (prof) w_prof_cur = from->prof;
#ifdef W_ASAN
  {
    WWorker *w = w_self();
    if (from == &w->sched) __sanitizer_finish_switch_fiber(fake, NULL, NULL);
    else __sanitizer_finish_switch_fiber(fake, &w->sched.asan_bottom, &w->sched.asan_size);
  }
#endif
}

W_RT WFiber *w_fiber_new(void (*entry)(void *), void *arg, size_t size) {
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

W_RT void w_fiber_free(void *p) {
  WFiber *f = (WFiber *)p;
#ifdef W_TSAN
  __tsan_destroy_fiber(f->tsan);
#endif
  munmap(f->map, f->map_size);
  w_free(f);
  __atomic_fetch_sub(&w_fibers_alive, 1, __ATOMIC_RELEASE);
}

/* ---- run queues ---- */

W_RT void w_wake_one(void) {
  if (__atomic_load_n(&w_nidle, __ATOMIC_SEQ_CST) > 0) {
    pthread_mutex_lock(&w_idle_mu);
    pthread_cond_signal(&w_idle_cv);
    pthread_mutex_unlock(&w_idle_mu);
  }
}

/* Make a fiber runnable: on the caller's own queue if it is a worker,
   otherwise on the global queue. */
W_RT void w_ready(WFiber *f) {
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

W_RT WFiber *w_take(WWorker *w, int own) {
  WFiber *f = NULL;
  pthread_mutex_lock(&w->mu);
  if (w->tail > w->head) {
    if (own && !w_virtual) { w->tail--; f = w->q[w->tail % w->cap]; }   /* newest first: good locality for fork/join */
    else { f = w->q[w->head % w->cap]; w->head++; }       /* thieves take the oldest; on virtual time, everyone */
  }
  pthread_mutex_unlock(&w->mu);
  if (f) __atomic_fetch_sub(&w_queued, 1, __ATOMIC_SEQ_CST);
  return f;
}

W_RT WFiber *w_find_work(WWorker *w) {
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

W_RT void w_sched_loop(WWorker *w, int is_main) {
  for (;;) {
    if (is_main && __atomic_load_n(&w_main_done, __ATOMIC_ACQUIRE)) return;
    WFiber *f = w_find_work(w);
    if (!f) {
      /* nothing to run here: write out what was printed before going idle,
         so output is not held while the program waits */
      fflush(stdout);
      pthread_mutex_lock(&w_idle_mu);
      int64_t idle = __atomic_add_fetch(&w_nidle, 1, __ATOMIC_SEQ_CST);
      if (__atomic_load_n(&w_queued, __ATOMIC_SEQ_CST) == 0 && !__atomic_load_n(&w_main_done, __ATOMIC_ACQUIRE)) {
        /* Every worker idle, nothing runnable, nothing waiting on I/O or a
           timer: no task can ever wake another. Confirm after a short wait,
           then report the deadlock instead of hanging. */
        int quiet = idle == __atomic_load_n(&w_nworkers, __ATOMIC_ACQUIRE) && __atomic_load_n(&w_io_waiters, __ATOMIC_SEQ_CST) == 0;
        if (quiet && w_virtual && __atomic_load_n(&w_tm_count, __ATOMIC_SEQ_CST) > 0) {
          /* virtual time: every task is blocked and none on I/O, so the
             clock jumps to the next timer. This worker is the last to go
             idle; it stops being idle to ring the timer, so no other can
             ring one until everything is blocked again. */
          __atomic_fetch_sub(&w_nidle, 1, __ATOMIC_SEQ_CST);
          pthread_mutex_unlock(&w_idle_mu);
          w_tm_advance();
          continue;
        }
        if (quiet && __atomic_load_n(&w_tm_count, __ATOMIC_SEQ_CST) == 0) {
          struct timeval tv; gettimeofday(&tv, NULL);
          struct timespec ts = { tv.tv_sec, (long)tv.tv_usec * 1000 + 100000000L };
          if (ts.tv_nsec >= 1000000000L) { ts.tv_sec++; ts.tv_nsec -= 1000000000L; }
          int rc = pthread_cond_timedwait(&w_idle_cv, &w_idle_mu, &ts);
          if (rc == ETIMEDOUT && __atomic_load_n(&w_queued, __ATOMIC_SEQ_CST) == 0
              && __atomic_load_n(&w_nidle, __ATOMIC_SEQ_CST) == __atomic_load_n(&w_nworkers, __ATOMIC_ACQUIRE)
              && __atomic_load_n(&w_io_waiters, __ATOMIC_SEQ_CST) == 0 && __atomic_load_n(&w_tm_count, __ATOMIC_SEQ_CST) == 0
              && !__atomic_load_n(&w_main_done, __ATOMIC_ACQUIRE))
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
WFiber *w_main_fiber;
struct sigaction w_old_segv, w_old_bus;
W_RT void w_sig_puts(const char *m) { size_t n = strlen(m); while (n > 0) { ssize_t k = write(2, m, n); if (k <= 0) break; m += k; n -= (size_t)k; } }
W_RT void w_sig_putu(uint64_t v) {
  char b[24]; int i = 23; b[i] = 0;
  do { b[--i] = (char)('0' + v % 10); v /= 10; } while (v);
  w_sig_puts(b + i);
}
W_RT void w_stack_fault(int sig, siginfo_t *si, void *uc) {
  (void)uc;
  WWorker *w = w_self_tls;
  WFiber *f = w ? w->current : NULL;
  long page = sysconf(_SC_PAGESIZE);
  size_t guard = (W_GUARD_BYTES + (size_t)page - 1) / (size_t)page * (size_t)page;
  char *a = (char *)si->si_addr;
  if (f && f->map && a >= f->map && a < f->map + guard) {
    w_sig_puts("panic: stack overflow\n  --> ");
    if (f == w_main_fiber) w_sig_puts("main, whose stack is ");
    else w_sig_puts("a task, whose stack is ");
    w_sig_putu((uint64_t)(f->map_size - guard) / 1024);
    w_sig_puts(" KB");
    if (f != w_main_fiber) w_sig_puts(" (LIMEN_STACK_KB sets it)");
    w_sig_puts("\n");
    _exit(1);
  }
  /* not ours: put the previous handler back and let the fault happen again */
  sigaction(sig, sig == SIGBUS ? &w_old_bus : &w_old_segv, NULL);
}
W_RT void w_altstack(void) {
  stack_t ss;
  size_t n = W_ALTSTACK_BYTES + (size_t)SIGSTKSZ;
  ss.ss_sp = mmap(NULL, n, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
  if (ss.ss_sp == MAP_FAILED) return;
  ss.ss_size = n; ss.ss_flags = 0;
  if (sigaltstack(&ss, NULL) != 0) munmap(ss.ss_sp, n);
}
W_RT void w_stack_fault_install(void) {
  struct sigaction sa;
  memset(&sa, 0, sizeof sa);
  sa.sa_sigaction = w_stack_fault;
  sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGSEGV, &sa, &w_old_segv);
  sigaction(SIGBUS, &sa, &w_old_bus);
}

W_RT WWorker *w_worker_new(void) {
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
  w_altstack();
  return w;
}

W_RT void *w_worker_thread(void *arg) {
  (void)arg;
  pthread_mutex_lock(&w_global_mu);
  WWorker *w = w_worker_new();
  pthread_mutex_unlock(&w_global_mu);
  w_sched_loop(w, 0);
  return NULL;
}
pthread_once_t w_pool_once = PTHREAD_ONCE_INIT;
W_RT void w_pool_start(void) {
  long n = sysconf(_SC_NPROCESSORS_ONLN);
  const char *e = getenv("LIMEN_THREADS");
  if (e) n = atol(e);
  if (n > W_MAX_WORKERS) n = W_MAX_WORKERS;
  if (w_virtual) n = 1;   /* virtual time: tasks take turns on one thread, in start order, the same every run */
  for (long i = 1; i < n; i++) {
    pthread_t th;
    pthread_create(&th, NULL, w_worker_thread, NULL);
    pthread_detach(th);
  }
}
W_RT void w_park(void (*after)(void *), void *arg) {
  WWorker *w = w_self();
  WFiber *f = w->current;
  /* C's frames are on this fiber's stack: it must not move to another thread */
  if (f->in_cb) w_fatal_c(W_CB_WAIT, f->in_cb);
  w->after = after; w->after_arg = arg;
  w_switch(f, &w->sched, 0);
}

/* Waiting protocol. The fiber calls w_prepare_park(), registers itself
   wherever a waker will find it (under that structure's own lock), releases
   the lock, then calls w_park_now(). A waker calls w_wake(f). Whichever of
   "the fiber is fully parked" and "the wakeup" comes second makes the fiber
   runnable, so no wakeup is lost and no fiber is resumed while it still runs.
   Locks are never held across a switch. */
W_RT WFiber *w_current(void) { return w_self()->current; }
/* Through w_self(), never the thread-local directly: a fiber can resume on
   another worker thread, and an inlined TLS address would be stale. */
__attribute__((noinline)) W_RT WCatch **w_catch_slot(void) {
  WWorker *w = w_self();
  if (!w || !w->current) return NULL;
  return &w->current->catch_top;
}
W_RT void w_mark_unwinding(void) { w_self()->current->unwinding = 1; }
W_RT void w_prepare_park(void) { __atomic_store_n(&w_current()->park_state, W_PARKING, __ATOMIC_RELEASE); }
W_RT void w_park_commit(void *p) {
  WFiber *f = (WFiber *)p;
  int expect = W_PARKING;
  if (!__atomic_compare_exchange_n(&f->park_state, &expect, W_PARKED, 0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) {
    __atomic_store_n(&f->park_state, W_RUNNING, __ATOMIC_RELEASE);   /* woken before it finished parking */
    w_ready(f);
  }
}
W_RT void w_park_now(void) { WFiber *f = w_current(); w_park(w_park_commit, f); }
W_RT void w_wake(WFiber *f) {
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
W_RT void w_run(void (*entry)(void *), void *arg) {
  const char *e = getenv("LIMEN_STACK_KB");
  if (e && atol(e) > 0) w_stack_size = (size_t)atol(e) * 1024;
  w_stack_fault_install();
  pthread_mutex_lock(&w_global_mu);
  WWorker *w = w_worker_new();
  pthread_mutex_unlock(&w_global_mu);
  WFiber *m = w_fiber_new(entry, arg, (size_t)1 << 30); /* main gets a large, lazily committed stack */
  w_main_fiber = m;
  w_ready(m);
  w_sched_loop(w, 1);
  /* main has returned, so every task has finished; wait for the workers to
     finish freeing the last fibers they switched off */
  while (__atomic_load_n(&w_fibers_alive, __ATOMIC_ACQUIRE) != 0) sched_yield();
}
W_RT void w_main_finished(void) {
  pthread_mutex_lock(&w_idle_mu);
  __atomic_store_n(&w_main_done, 1, __ATOMIC_RELEASE);
  pthread_cond_broadcast(&w_idle_cv);
  pthread_mutex_unlock(&w_idle_mu);
}
/* cancelled as a task, or inside a group body whose deadline passed */
W_RT int w_scopes_stopped(WScope *sc) {
  for (; sc; sc = sc->prev) if (__atomic_load_n(sc->stop, __ATOMIC_ACQUIRE)) return 1;
  return 0;
}
W_RT int w_fiber_stopped(WFiber *f) { return __atomic_load_n(&f->cancelled, __ATOMIC_ACQUIRE) || w_scopes_stopped(f->scopes); }
W_RT int w_is_cancelled(void) {
  WWorker *w = w_self();
  if (!w || !w->current) return 0;
  w_flock(w->current);
  int r = w_fiber_stopped(w->current);
  w_funlock(w->current);
  return r;
}
/* Start a wait (like w_prepare_park). Returns 0, without preparing to
   park, if the task is cancelled: the caller undoes what it did, releases
   its locks and calls w_cancel_unwind(). */
W_RT int w_wait_begin(WSel *tok) {
  WFiber *f = w_current();
  tok->done = 0; tok->fired = -1;
  if (f->in_update) return 0;   /* update's function must not wait: w_cancel_unwind panics */
  w_flock(f);
  if (w_fiber_stopped(f)) { w_funlock(f); return 0; }
  w_prepare_park();
  f->wait = tok;
  w_funlock(f);
  return 1;
}
/* The wait w_wait_begin refused was in the function of a Shared's update,
   which must not wait: unless the task is cancelled anyway, that is a
   panic, where the update was called (the caller has let go of its locks). */
W_RT void w_update_waits(void) {
  WWorker *w = w_self();
  if (w && w->current && w->current->in_update && !w_is_cancelled()) w_panic_c("update must not wait", w->current->upd_where);
}
/* After waking: the token is no longer the canceller's to claim. */
W_RT void w_wait_end(void) { WFiber *f = w_current(); w_flock(f); f->wait = NULL; w_funlock(f); }
/* Wake f from a cancellable wait, if it is in one: its stop flag (the task's
   or a scope's) is already set, so it unwinds. */
W_RT void w_interrupt_fiber(WFiber *f, int task) {
  int wake = 0;
  w_flock(f);
  if (task) __atomic_store_n(&f->cancelled, 1, __ATOMIC_RELEASE);
  if (f->wait) {
    int zero = 0;
    if (__atomic_compare_exchange_n(&f->wait->done, &zero, 1, 0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) {
      __atomic_store_n(&f->wait->fired, W_CANCEL, __ATOMIC_RELEASE);
      wake = 1;
    }
  }
  w_funlock(f);
  if (wake) w_wake(f);
}
W_RT void w_cancel_fiber(WFiber *f) { w_interrupt_fiber(f, 1); }
int64_t w_stopping;
__attribute__((noinline, cold)) W_RT void w_checkpoint(void) {
  WWorker *w = w_self();
  if (!w || !w->current || w->current->in_cb || w->current->unwinding) return;
  if (w_is_cancelled()) w_cancel_unwind();
}
pthread_mutex_t w_tm_mu = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t w_tm_cv = PTHREAD_COND_INITIALIZER;
WTimer **w_tm_heap;
int64_t w_tm_n, w_tm_cap, w_tm_seq;
pthread_once_t w_tm_once = PTHREAD_ONCE_INIT;

W_RT int64_t w_mono_ns(void) {
  if (w_virtual) return __atomic_load_n(&w_vnow_ns, __ATOMIC_ACQUIRE);
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (int64_t)ts.tv_sec * 1000000000 + ts.tv_nsec;
}
/* now + ms, saturating: a deadline far in the future never overflows */
W_RT int64_t w_deadline_ms(int64_t ms) {
  if (ms > (int64_t)1000000000000) ms = (int64_t)1000000000000;   /* about 31 years */
  return w_mono_ns() + ms * 1000000;
}

W_RT void w_tm_swap(int64_t i, int64_t j) {
  WTimer *t = w_tm_heap[i]; w_tm_heap[i] = w_tm_heap[j]; w_tm_heap[j] = t;
  w_tm_heap[i]->pos = i; w_tm_heap[j]->pos = j;
}
W_RT int w_tm_before(WTimer *a, WTimer *b) { return a->at < b->at || (a->at == b->at && a->seq < b->seq); }
W_RT void w_tm_up(int64_t i) {
  while (i > 0 && w_tm_before(w_tm_heap[i], w_tm_heap[(i - 1) / 2])) { w_tm_swap(i, (i - 1) / 2); i = (i - 1) / 2; }
}
W_RT void w_tm_down(int64_t i) {
  for (;;) {
    int64_t l = 2 * i + 1, r = l + 1, m = i;
    if (l < w_tm_n && w_tm_before(w_tm_heap[l], w_tm_heap[m])) m = l;
    if (r < w_tm_n && w_tm_before(w_tm_heap[r], w_tm_heap[m])) m = r;
    if (m == i) return;
    w_tm_swap(i, m); i = m;
  }
}
/* with w_tm_mu held */
W_RT void w_tm_unlink(WTimer *t) {
  int64_t i = t->pos;
  if (i < 0) return;
  w_tm_n--;
  if (i != w_tm_n) { w_tm_heap[i] = w_tm_heap[w_tm_n]; w_tm_heap[i]->pos = i; w_tm_down(i); w_tm_up(i); }
  t->pos = -1;
  __atomic_store_n(&w_tm_count, w_tm_n, __ATOMIC_SEQ_CST);
}

W_RT void *w_timer_thread(void *arg) {
  (void)arg;
  pthread_mutex_lock(&w_tm_mu);
  for (;;) {
    if (w_tm_n == 0) { pthread_cond_wait(&w_tm_cv, &w_tm_mu); continue; }
    WTimer *t = w_tm_heap[0];
    int64_t now = w_mono_ns();
    if (t->at > now) {
      /* the condition variable takes wall-clock time; recheck on waking */
      int64_t d = t->at - now;
      struct timeval tv; gettimeofday(&tv, NULL);
      int64_t ns = (int64_t)tv.tv_usec * 1000 + d % 1000000000;
      struct timespec ts = { tv.tv_sec + (time_t)(d / 1000000000) + (time_t)(ns / 1000000000), (long)(ns % 1000000000) };
      pthread_cond_timedwait(&w_tm_cv, &w_tm_mu, &ts);
      continue;
    }
    w_tm_unlink(t);
    if (t->fire) { t->fire(t->arg); continue; }
    WFiber *f = t->f;
    int claimed = w_tok_claim(t->sel, t->idx);
    /* t lives on the waiting fiber's stack: do not touch it after unlocking */
    pthread_mutex_unlock(&w_tm_mu);
    if (claimed) w_wake(f);
    pthread_mutex_lock(&w_tm_mu);
  }
  return NULL;
}
W_RT void w_tm_start(void) {
  pthread_t th;
  pthread_create(&th, NULL, w_timer_thread, NULL);
  pthread_detach(th);
}
W_RT void w_tm_add(WTimer *t) {
  if (!w_virtual) pthread_once(&w_tm_once, w_tm_start);   /* on virtual time, idle workers ring the timers */
  pthread_mutex_lock(&w_tm_mu);
  t->seq = w_tm_seq++;
  if (w_tm_n == w_tm_cap) {
    w_tm_cap = w_tm_cap ? w_tm_cap * 2 : 64;
    w_tm_heap = (WTimer **)w_realloc(w_tm_heap, sizeof(WTimer *) * (size_t)w_tm_cap);
  }
  t->pos = w_tm_n; w_tm_heap[w_tm_n++] = t;
  w_tm_up(t->pos);
  __atomic_store_n(&w_tm_count, w_tm_n, __ATOMIC_SEQ_CST);
  if (t->pos == 0) pthread_cond_signal(&w_tm_cv);   /* a new earliest deadline */
  pthread_mutex_unlock(&w_tm_mu);
}
/* Virtual time: ring the earliest timer (of two due together, the one set
   first), moving the clock to it, as the timer thread would. Only that
   timer: the next one, even at the same time, waits until every task is
   blocked again, so what each woken task does happens in the same order on
   every run. Returns 0 if no timer was pending. */
W_RT int w_tm_advance(void) {
  pthread_mutex_lock(&w_tm_mu);
  if (w_tm_n == 0) { pthread_mutex_unlock(&w_tm_mu); return 0; }
  WTimer *t = w_tm_heap[0];
  w_tm_unlink(t);
  if (t->at > __atomic_load_n(&w_vnow_ns, __ATOMIC_ACQUIRE)) __atomic_store_n(&w_vnow_ns, t->at, __ATOMIC_RELEASE);
  if (t->fire) t->fire(t->arg);
  else {
    WFiber *f = t->f;
    int claimed = w_tok_claim(t->sel, t->idx);
    pthread_mutex_unlock(&w_tm_mu);
    if (claimed) w_wake(f);
    return 1;
  }
  pthread_mutex_unlock(&w_tm_mu);
  return 1;
}

/* Take a timer out if it has not fired. Once this returns, a fire function
   is not running and will not run. */
W_RT void w_tm_remove(WTimer *t) {
  pthread_mutex_lock(&w_tm_mu);
  w_tm_unlink(t);
  pthread_mutex_unlock(&w_tm_mu);
}

/* Park until ms milliseconds have passed. Returns -2 if the task was
   cancelled (the caller cleans up, then calls w_cancel_unwind()). */
W_RT int w_nap(int64_t ms) {
  if (ms <= 0) return 0;
  WSel tok;
  WTimer t;
  memset(&t, 0, sizeof t);
  t.at = w_deadline_ms(ms); t.pos = -1; t.f = w_current(); t.sel = &tok; t.idx = 0;
  if (!w_wait_begin(&tok)) return -2;
  w_tm_add(&t);
  w_park_now();
  w_wait_end();
  if (w_tok_fired(&tok) == W_CANCEL) { w_tm_remove(&t); return -2; }
  return 0;
}

/* Clock.sleep(ms): park until the time has passed. */
W_RT void Cap_sleep(int64_t ms) { if (w_nap(ms) < 0) w_cancel_unwind(); }
pthread_mutex_t w_out_mu = PTHREAD_MUTEX_INITIALIZER;
__thread int w_out_locked;
size_t w_held;
WGroup *w_live_groups;
W_RT void w_out_lock(void) { pthread_mutex_lock(&w_out_mu); w_out_locked = 1; }
W_RT void w_out_unlock(void) { w_out_locked = 0; pthread_mutex_unlock(&w_out_mu); }
W_RT WOut *w_cur_out(void) { WWorker *w = w_self(); return w && w->current ? w->current->out : NULL; }
W_RT void w_live_remove(WGroup *g) {
  if (!g->live) return;
  if (g->live_prev) g->live_prev->live_next = g->live_next; else w_live_groups = g->live_next;
  if (g->live_next) g->live_next->live_prev = g->live_prev;
  g->live = 0;
}
W_RT int w_out_through(WOut *o) { WGroup *g = o->g; return !g->ordered || (g->body_done && g->first == o); }
W_RT void w_out_put(WOut *o, const char *p, size_t n) {
  while (o) {
    WGroup *g = o->g;
    if (w_out_through(o)) { o = g->parent; continue; }
    if (w_held + n > W_HOLD_LIMIT) { w_out_spill(g); o = g->parent; continue; }
    if (o->len + n > o->cap) {
      size_t c = o->cap ? o->cap : 256;
      while (c < o->len + n) c *= 2;
      char *b = (char *)realloc(o->buf, c);
      if (!b) w_oom();
      o->buf = b; o->cap = c;
    }
    memcpy(o->buf + o->len, p, n);
    o->len += n; w_held += n;
    return;
  }
  fwrite(p, 1, n, stdout);
}
W_RT void w_out_release(WOut *o) {
  if (!o->buf) return;
  char *b = o->buf; size_t n = o->len;
  o->buf = NULL; o->len = o->cap = 0;
  w_held -= n;
  w_out_put(o->g->parent, b, n);
  free(b);
}
/* Give up order: release everything held, in start order. */
W_RT void w_out_spill(WGroup *g) {
  g->ordered = 0;
  for (WOut *o = g->first; o; o = o->next) w_out_release(o);
  g->first = g->last = NULL;
  w_live_remove(g);
}
/* Release what can go now: the finished tasks at the front, and the held
   output of the first unfinished one, which then writes through. */
W_RT void w_out_advance(WGroup *g) {
  if (!g->ordered || !g->body_done) return;
  WOut *o;
  while ((o = g->first)) {
    w_out_release(o);
    if (!o->done) break;
    g->first = o->next;
    if (!g->first) g->last = NULL;
  }
}
/* The program is ending with a panic or a deadlock: release everything
   still held, innermost groups first, before the message. */
W_RT void w_out_release_all(void) {
  int mine = w_out_locked;
  if (!mine) w_out_lock();
  while (w_live_groups) w_out_spill(w_live_groups);
  if (!mine) w_out_unlock();
}
/* Buffered, so printing many lines costs what it does in C. The buffer is
   written out whenever the program waits (see w_sched_loop), before stdin
   is read, and at exit: a long-running program's output reaches a file or
   pipe as soon as it has printed and gone back to waiting. */
W_RT void Cap_print(WStr *s, bool nl) {
  WOut *o = w_cur_out();
  w_out_lock();
  w_out_put(o, s->data, (size_t)s->len);
  if (nl) w_out_put(o, "\n", 1);
  w_out_unlock();
}
/* Stderr is written at once, past the order groups keep for stdout: it is
   for messages about the run, which should show when they happen. What
   stdout has buffered goes first, so where both reach the same place a
   message follows the output printed before it. */
W_RT void Cap_eprint(WStr *s, bool nl) {
  w_out_lock();
  fflush(stdout);
  fwrite(s->data, 1, (size_t)s->len, stderr);
  if (nl) fputc('\n', stderr);
  w_out_unlock();
}
/* Reading stdin: a prompt this task printed must show, so every group that
   holds its output gives up order. */
W_RT void w_out_unhold(void) {
  WOut *o = w_cur_out();
  w_out_lock();
  while (o) {
    WGroup *g = o->g;
    if (!w_out_through(o)) w_out_spill(g);
    o = g->parent;
  }
  w_out_unlock();
}
W_RT void w_group_deadline(void *p) { w_group_cancel((WGroup *)p); }
/* ordered: 0 for `group g interleaved` */
W_RT WGroup *w_group_new(int ordered) {
  WGroup *g = (WGroup *)w_alloc_k(sizeof(WGroup), WK_GROUP);
  memset(g, 0, sizeof *g);
  g->rc = 1; g->cap = 8;
  g->deadline.pos = -1;
  pthread_mutex_init(&g->mu, NULL);
  g->tasks = (WTask **)w_raw(sizeof(WTask *) * (size_t)g->cap);
  g->parent = w_cur_out();
  if (ordered) {
    w_out_lock();
    g->ordered = 1; g->live = 1;
    g->live_next = w_live_groups;
    if (w_live_groups) w_live_groups->live_prev = g;
    w_live_groups = g;
    w_out_unlock();
  }
  return g;
}
/* A group with a deadline ms milliseconds from now. */
W_RT WGroup *w_group_new_until(int64_t ms, int ordered) {
  WGroup *g = w_group_new(ordered);
  if (ms <= 0) { g->cancelled = 1; g->stopping = 1; __atomic_fetch_add(&w_stopping, 1, __ATOMIC_RELAXED); return g; }
  g->deadline.at = w_deadline_ms(ms); g->deadline.fire = w_group_deadline; g->deadline.arg = g;
  w_tm_add(&g->deadline);
  return g;
}
/* Cancel every task still running, and every task started from now on. */
W_RT void w_group_cancel(WGroup *g) {
  pthread_mutex_lock(&g->mu);
  __atomic_store_n(&g->cancelled, 1, __ATOMIC_RELEASE);
  if (!g->stopping) { g->stopping = 1; __atomic_fetch_add(&w_stopping, 1, __ATOMIC_RELAXED); }
  for (int64_t i = 0; i < g->n; i++) if (g->tasks[i] && !g->tasks[i]->done && g->tasks[i]->f) w_cancel_fiber(g->tasks[i]->f);
  if (g->body) w_interrupt_fiber(g->body, 0);   /* the body stops at its next wait too */
  pthread_mutex_unlock(&g->mu);
}
/* Enter and leave a group body under its deadline. */
W_RT void w_scope_push(WScope *sc, WGroup *g) {
  WFiber *f = w_current();
  sc->g = g; sc->stop = &g->cancelled;
  pthread_mutex_lock(&g->mu);
  g->body = f;
  pthread_mutex_unlock(&g->mu);
  w_flock(f);
  sc->prev = f->scopes; f->scopes = sc;
  w_funlock(f);
}
W_RT void w_scope_pop(WScope *sc) {
  WFiber *f = w_current();
  if (f->scopes != sc) return;   /* already left (a stop passed on) */
  pthread_mutex_lock(&sc->g->mu);
  sc->g->body = NULL;
  pthread_mutex_unlock(&sc->g->mu);
  w_flock(f);
  f->scopes = sc->prev;
  w_funlock(f);
}
/* The body unwound to its boundary c. Its own deadline stopping it ends the
   body: groups it started and left are cancelled and joined, what else it
   made is handed on, and the join goes on. A panic, the task's own
   cancellation or an outer deadline passes on. */
W_RT void w_scope_land(WCatch *c, WScope *sc) {
  WFiber *f = w_current();
  int mine = c->cancel && __atomic_load_n(sc->stop, __ATOMIC_ACQUIRE) && !__atomic_load_n(&f->cancelled, __ATOMIC_ACQUIRE) && !w_scopes_stopped(sc->prev);
  if (mine) {
    f->unwinding = 0;   /* the body has stopped; the code after the group runs as usual */
    for (;;) {
      WHdr *h = c->objs.next;
      while (h != &c->objs && h->kind != WK_GROUP) h = h->next;
      if (h == &c->objs) break;
      WGroup *g = (WGroup *)(h + 1);
      w_group_cleanup(g);   /* stopped: cancels its tasks, then waits for them */
      w_group_release_tasks(g);
      pthread_mutex_destroy(&g->mu); free(g->tasks); w_free(g);
    }
    w_catch_end(c);
    return;
  }
  w_catch_end(c);
  w_scope_pop(sc);
  if (c->cancel) w_cancel_unwind();
  char m[1024], w[512];
  snprintf(m, sizeof m, "%s", c->msg); snprintf(w, sizeof w, "%s", c->where);
  free(c->msg); free(c->where);
  w_panic_c(m, w);
}
W_RT void w_task_entry(void *p) {
  WTask *volatile t = (WTask *)p;
  WGroup *g = t->g;
  /* a panic ends only this task; the group passes it on when it finishes */
  WCatch c;
  w_catch_init(&c);
  c.prev = NULL;
  WCatch **slot = w_catch_slot();
  if (w_is_cancelled()) {
    Clo_drop(t->fn);   /* cancelled before it started: it never runs */
    t->cancelled = 1;
  } else if (_setjmp(c.jb) == 0) {
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
    if (c.cancel) t->cancelled = 1;
    else { t->pmsg = c.msg; t->pwhere = c.where; }
  }
  /* finished: its output goes out once the tasks before it have */
  w_out_lock();
  t->out.done = 1;
  w_out_advance(g);
  w_out_unlock();
  pthread_mutex_lock(&g->mu);
  WFiber *wake = NULL;
  t->done = 1; t->f = NULL;
  /* nothing will read it again: free it now, not when the group ends */
  int gone = !t->keep && !t->linked && !t->pmsg;
  if (gone) { g->tasks[t->idx] = NULL; g->holes++; }
  if (--g->pending == 0 && g->waiter) {
    if (w_tok_claim(g->wtok, 0)) wake = g->waiter;
    g->waiter = NULL; g->wtok = NULL;
  }
  pthread_mutex_unlock(&g->mu);
  if (gone) w_free(t);
  if (wake) w_wake(wake);
}
/* The closure and everything it references become shared before any other
   thread can see them. */
/* keep: the task's result goes in the group's list (false for a task that returns ()) */
W_RT void w_spawn(WGroup *g, WClo *fn, void (*run)(WTask *), size_t slot, bool keep) {
  pthread_once(&w_pool_once, w_pool_start);
  Clo_share(fn);
  WTask *t = (WTask *)w_alloc(sizeof(WTask) + slot);
  t->fn = fn; t->run = run; t->g = g; t->pmsg = NULL; t->pwhere = NULL; t->f = NULL; t->done = 0; t->cancelled = 0; t->keep = keep; t->linked = 0;
  memset(&t->out, 0, sizeof t->out);
  t->out.g = g;
  WFiber *f = w_fiber_new(w_task_entry, t, w_stack_size);
  /* an ordered group holds the task's output in its WOut */
  w_out_lock();
  if (g->ordered) {
    t->linked = 1;
    if (g->last) g->last->next = &t->out; else g->first = &t->out;
    g->last = &t->out;
    f->out = &t->out;
  } else f->out = g->parent;
  w_out_unlock();
  pthread_mutex_lock(&g->mu);
  if (g->n == g->cap && g->holes * 2 >= g->n) {
    /* mostly freed tasks: close the gaps, keeping spawn order */
    int64_t k = 0;
    for (int64_t i = 0; i < g->n; i++) if (g->tasks[i]) { g->tasks[k] = g->tasks[i]; g->tasks[k]->idx = k; k++; }
    g->n = k; g->holes = 0;
  }
  if (g->n == g->cap) { g->cap *= 2; g->tasks = (WTask **)w_realloc(g->tasks, w_bytes(g->cap, sizeof(WTask *))); }
  t->idx = g->n;
  g->tasks[g->n++] = t;
  if (g->cancelled) {   /* past the deadline: the task never runs */
    t->done = 1; t->cancelled = 1;
    pthread_mutex_unlock(&g->mu);
    w_out_lock();
    t->out.done = 1;
    w_out_advance(g);
    w_out_unlock();
    w_fiber_free(f);
    Clo_drop(fn);
    return;
  }
  t->f = f;
  g->pending++;
  pthread_mutex_unlock(&g->mu);
  w_ready(f);
}
/* Wait for every task, then take the deadline off the timer thread (after
   this, nothing else touches the group). A cancelled joiner stops waiting
   and unwinds; the boundary it unwinds to then cancels the group's tasks
   and joins it again, not cancellably (w_group_cleanup). */
W_RT void w_group_wait(WGroup *g, int cancellable) {
  /* the body has ended: the tasks' output can start to go out */
  w_out_lock();
  if (!g->body_done) { g->body_done = 1; w_out_advance(g); }
  w_out_unlock();
  pthread_mutex_lock(&g->mu);
  if (g->pending > 0) {
    WSel tok;
    if (cancellable) {
      if (!w_wait_begin(&tok)) { pthread_mutex_unlock(&g->mu); w_cancel_unwind(); }
    } else {
      tok.done = 0; tok.fired = -1;
      w_prepare_park();
    }
    g->waiter = w_current(); g->wtok = &tok;
    pthread_mutex_unlock(&g->mu);
    w_park_now();
    /* resumed by the last task (w_wake's atomics order its writes before
       ours) or by cancellation */
    if (cancellable) {
      w_wait_end();
      if (w_tok_fired(&tok) == W_CANCEL) {
        pthread_mutex_lock(&g->mu);
        if (g->wtok == &tok) { g->waiter = NULL; g->wtok = NULL; }
        pthread_mutex_unlock(&g->mu);
        w_cancel_unwind();
      }
    }
  } else pthread_mutex_unlock(&g->mu);
  if (g->deadline.fire) w_tm_remove(&g->deadline);
  /* every task has stopped and the deadline cannot fire any more: the
     group no longer makes checkpoints look */
  pthread_mutex_lock(&g->mu);
  if (g->stopping == 1) { g->stopping = 2; __atomic_fetch_sub(&w_stopping, 1, __ATOMIC_RELAXED); }
  pthread_mutex_unlock(&g->mu);
  /* every task has finished, so all their output has gone out */
  w_out_lock();
  w_live_remove(g);
  w_out_unlock();
}
/* Joining a group whose code is being abandoned (a panic, or the task
   running it was cancelled): a cancellation reaches its tasks too. */
W_RT void w_group_cleanup(WGroup *g) {
  if (w_is_cancelled()) w_group_cancel(g);
  w_group_wait(g, 0);
}
W_RT void w_group_release_tasks(WGroup *g) {
  for (int64_t i = 0; i < g->n; i++) if (g->tasks[i]) { free(g->tasks[i]->pmsg); free(g->tasks[i]->pwhere); free(g->tasks[i]->out.buf); w_free(g->tasks[i]); }
  g->n = 0; g->holes = 0;
}
/* After every task finished: pass on the first panic, in spawn order. */
W_RT void w_group_reraise(WGroup *g) {
  for (int64_t i = 0; i < g->n; i++)
    if (g->tasks[i] && g->tasks[i]->pmsg) {
      char m[1024], w[512];
      snprintf(m, sizeof m, "%s", g->tasks[i]->pmsg);
      snprintf(w, sizeof w, "%s", g->tasks[i]->pwhere);
      w_group_release_tasks(g);
      w_panic_c(m, w);
    }
}
/* The last reference can go before the join when the group's body panics:
   its tasks may still be running, so wait for them before freeing. */
W_RT void Group_drop(WGroup *g) {
  if (--g->rc == 0) {
    w_group_cleanup(g);
    w_group_release_tasks(g);
    pthread_mutex_destroy(&g->mu); free(g->tasks); w_free(g);
  }
}

W_RT WChan *w_chan_new(int64_t cap, size_t esz, void (*edrop)(void *), void (*eshare)(void *), const char *where) {
  if (cap < 0) w_panic_c("channel capacity cannot be negative", where);
  WChan *c = (WChan *)w_alloc_k(sizeof(WChan), WK_CHAN);
  memset(c, 0, sizeof *c);
  c->rc = 1; c->cap = cap; c->esz = esz ? esz : 1;
  c->buf = (char *)w_raw(w_bytes(cap, c->esz));
  c->edrop = edrop; c->eshare = eshare;
  pthread_mutex_init(&c->mu, NULL);
  return c;
}
W_RT void Chan_drop(WChan *c) {
  if (!w_rc_dec(&c->rc)) return;
  if (c->edrop) for (int64_t i = 0; i < c->len; i++) c->edrop(c->buf + ((c->head + i) % c->cap) * c->esz);
  free(c->buf);
  pthread_mutex_destroy(&c->mu);
  w_free(c);
}
W_RT void Chan_show(WBuf *b, WChan *c, bool top) { (void)c; (void)top; buf_puts(b, "<chan>"); }

W_RT void w_chq_push(WChWait **h, WChWait **t, WChWait *w) { w->next = NULL; if (*t) (*t)->next = w; else *h = w; *t = w; }
W_RT WChWait *w_chq_pop(WChWait **h, WChWait **t) { WChWait *w = *h; if (w) { *h = w->next; if (!*h) *t = NULL; } return w; }
/* The next waiter that can still be woken: a plain one, or a select this
   call manages to claim. Stale select waiters are dropped. */
W_RT WChWait *w_chq_pop_live(WChWait **h, WChWait **t) {
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
W_RT void w_chq_remove(WChWait **h, WChWait **t, WChWait *w) {
  WChWait *prev = NULL;
  for (WChWait *p = *h; p; prev = p, p = p->next)
    if (p == w) {
      if (prev) prev->next = p->next; else *h = p->next;
      if (*t == p) *t = prev;
      return;
    }
}

W_RT void w_chan_send(WChan *c, void *v, const char *where) {
  if (c->eshare) c->eshare(v);
  pthread_mutex_lock(&c->mu);
  if (c->closed) { pthread_mutex_unlock(&c->mu); if (c->edrop) c->edrop(v); w_panic_c("send on a closed channel", where); }
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
  WSel tok;
  WChWait me = { w_current(), v, 0, NULL, &tok, 0 };
  if (!w_wait_begin(&tok)) { pthread_mutex_unlock(&c->mu); if (c->edrop) c->edrop(v); w_cancel_unwind(); }
  w_chq_push(&c->sq, &c->sq_t, &me);
  pthread_mutex_unlock(&c->mu);
  w_park_now();
  w_wait_end();
  if (w_tok_fired(&tok) == W_CANCEL) {
    pthread_mutex_lock(&c->mu);
    w_chq_remove(&c->sq, &c->sq_t, &me);
    pthread_mutex_unlock(&c->mu);
    if (c->edrop) c->edrop(v);
    w_cancel_unwind();
  }
  if (!me.ok) { if (c->edrop) c->edrop(v); w_panic_c("send on a closed channel", where); }
}

W_RT bool w_chan_recv(WChan *c, void *out) {
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
  WChWait *s = w_chq_pop_live(&c->sq, &c->sq_t);
  if (s) {                                       /* unbuffered: take it from a waiting sender */
    memcpy(out, s->val, c->esz); s->ok = 1;
    pthread_mutex_unlock(&c->mu);
    w_wake(s->f);
    return true;
  }
  if (c->closed) { pthread_mutex_unlock(&c->mu); return false; }
  WSel tok;
  WChWait me = { w_current(), out, 0, NULL, &tok, 0 };
  if (!w_wait_begin(&tok)) { pthread_mutex_unlock(&c->mu); w_cancel_unwind(); }
  w_chq_push(&c->rq, &c->rq_t, &me);
  pthread_mutex_unlock(&c->mu);
  w_park_now();
  w_wait_end();
  if (w_tok_fired(&tok) == W_CANCEL) {
    pthread_mutex_lock(&c->mu);
    w_chq_remove(&c->rq, &c->rq_t, &me);
    pthread_mutex_unlock(&c->mu);
    w_cancel_unwind();
  }
  return me.ok;
}

W_RT void w_chan_close(WChan *c, const char *where) {
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

W_RT void w_catch_release(WCatch *c) {
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
    if (h->kind == WK_GROUP) w_group_cleanup((WGroup *)o); /* its tasks still run: let them finish */
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

/* Try one operation with its channel locked. Returns 1 if done; *wake gets
   a fiber to wake after unlocking. A send on a closed channel returns -1. */
W_RT int w_sel_try(WSelOp *op, WFiber **wake) {
  if (op->send == W_SEL_AFTER) return op->ms <= 0;
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
  WChWait *s = w_chq_pop_live(&c->sq, &c->sq_t);
  if (s) {                                       /* unbuffered: take it from a waiting sender */
    memcpy(op->val, s->val, c->esz); s->ok = 1; *wake = s->f; op->ok = 1;
    return 1;
  }
  if (c->closed) { op->ok = 0; return 1; }
  return 0;
}
W_RT int w_chan_cmp(const void *a, const void *b) {
  uintptr_t x = (uintptr_t)*(WChan *const *)a, y = (uintptr_t)*(WChan *const *)b;
  return x < y ? -1 : x > y;
}
/* every distinct channel of the select, locked in address order */
W_RT int w_sel_lock(WSelOp *ops, int n, WChan **cs) {
  int m = 0;
  for (int i = 0; i < n; i++) if (ops[i].on && ops[i].c) cs[m++] = ops[i].c;
  qsort(cs, (size_t)m, sizeof(WChan *), w_chan_cmp);
  int k = 0;
  for (int i = 0; i < m; i++) if (k == 0 || cs[k - 1] != cs[i]) cs[k++] = cs[i];
  for (int i = 0; i < k; i++) pthread_mutex_lock(&cs[i]->mu);
  return k;
}
W_RT void w_sel_unlock(WChan **cs, int k) { for (int i = k - 1; i >= 0; i--) pthread_mutex_unlock(&cs[i]->mu); }

/* A select that is cancelled sends nothing: its values are released. */
W_RT void w_sel_drop_sends(WSelOp *ops, int n) {
  for (int i = 0; i < n; i++) if (ops[i].on && ops[i].send == W_SEL_SEND && ops[i].c->edrop) ops[i].c->edrop(ops[i].val);
}
W_RT int w_select(WSelOp *ops, int n, int has_default, const char *where) {
  int any = 0;
  for (int i = 0; i < n; i++) if (ops[i].on) any = 1;
  if (!any && !has_default) w_panic_c("every arm of this `select` is turned off by its guard", where);
  int64_t start = 0;
  for (int i = 0; i < n; i++) {
    if (ops[i].on && ops[i].send == W_SEL_SEND && ops[i].c->eshare) ops[i].c->eshare(ops[i].val);
    if (ops[i].on && ops[i].send == W_SEL_AFTER && ops[i].ms > 0 && !start) start = w_mono_ns();
  }
  /* fast path: one channel at a time, in order */
  for (int i = 0; i < n; i++) {
    if (!ops[i].on) continue;
    WFiber *wake = NULL;
    if (ops[i].c) pthread_mutex_lock(&ops[i].c->mu);
    int r = w_sel_try(&ops[i], &wake);
    if (ops[i].c) pthread_mutex_unlock(&ops[i].c->mu);
    if (r < 0) { w_sel_drop_sends(ops, n); w_panic_c("send on a closed channel", where); }
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
      if (r < 0) { w_sel_drop_sends(ops, n); w_panic_c("send on a closed channel", where); }
      if (wake) w_wake(wake);
      return i;
    }
  }
  WSel sel;
  WFiber *me = w_current();
  WTimer tm;
  memset(&tm, 0, sizeof tm);
  tm.pos = -1;
  if (!w_wait_begin(&sel)) {
    w_sel_unlock(cs, k);
    free(cs); free(ws);
    w_sel_drop_sends(ops, n);
    w_cancel_unwind();
  }
  for (int i = 0; i < n; i++) {
    if (!ops[i].on) continue;
    if (ops[i].send == W_SEL_AFTER) {   /* the earliest timeout wins; on a tie, the first written */
      int64_t ms = ops[i].ms > (int64_t)1000000000000 ? (int64_t)1000000000000 : ops[i].ms;
      int64_t at = start + ms * 1000000;
      if (!tm.f || at < tm.at) { tm.at = at; tm.f = me; tm.sel = &sel; tm.idx = i; }
      continue;
    }
    ws[i].f = me; ws[i].val = ops[i].val; ws[i].ok = 0; ws[i].sel = &sel; ws[i].idx = i;
    if (ops[i].send) w_chq_push(&ops[i].c->sq, &ops[i].c->sq_t, &ws[i]);
    else w_chq_push(&ops[i].c->rq, &ops[i].c->rq_t, &ws[i]);
  }
  w_sel_unlock(cs, k);
  if (tm.f) w_tm_add(&tm);
  w_park_now();
  /* woken by the channel or timer that claimed us; take our other waiters back */
  if (tm.f) w_tm_remove(&tm);
  k = w_sel_lock(ops, n, cs);
  for (int i = 0; i < n; i++) {
    if (!ops[i].on || ops[i].send == W_SEL_AFTER) continue;
    if (ops[i].send) w_chq_remove(&ops[i].c->sq, &ops[i].c->sq_t, &ws[i]);
    else w_chq_remove(&ops[i].c->rq, &ops[i].c->rq_t, &ws[i]);
  }
  w_sel_unlock(cs, k);
  w_wait_end();
  int i = w_tok_fired(&sel);
  if (i == W_CANCEL) { free(cs); free(ws); w_sel_drop_sends(ops, n); w_cancel_unwind(); }
  ops[i].ok = ops[i].send == W_SEL_AFTER ? 1 : ws[i].ok;
  free(cs); free(ws);
  if (ops[i].send && !ops[i].ok) { w_sel_drop_sends(ops, n); w_panic_c("send on a closed channel", where); }
  return i;
}
int w_poll_fd = -1;
pthread_once_t w_poll_once = PTHREAD_ONCE_INIT;
WFdState w_fds[W_MAX_FD];
pthread_mutex_t w_fd_mu[W_FD_LOCKS];
W_RT pthread_mutex_t *w_fd_lock(int fd) { return &w_fd_mu[fd % W_FD_LOCKS]; }

W_RT void w_fd_event(int fd, int readable, int writable) {
  if (fd < 0 || fd >= W_MAX_FD) return;
  WFdState *st = &w_fds[fd];
  WFiber *wr = NULL, *ww = NULL;
  pthread_mutex_lock(w_fd_lock(fd));
  /* a waiter whose token was claimed by its cancellation removes itself */
  if (readable) { if (st->r) { if (w_tok_claim(st->rt, 0)) wr = st->r; st->r = NULL; st->rt = NULL; } else st->rready = 1; }
  if (writable) { if (st->w) { if (w_tok_claim(st->wt, 0)) ww = st->w; st->w = NULL; st->wt = NULL; } else st->wready = 1; }
  pthread_mutex_unlock(w_fd_lock(fd));
  if (wr) w_wake(wr);
  if (ww) w_wake(ww);
}

W_RT void *w_poller(void *arg) {
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
W_RT void w_poll_start(void) {
  for (int i = 0; i < W_FD_LOCKS; i++) pthread_mutex_init(&w_fd_mu[i], NULL);
#ifdef W_KQUEUE
  int pfd = kqueue();
#else
  int pfd = epoll_create1(EPOLL_CLOEXEC);
#endif
  if (pfd < 0) { fputs("panic: cannot create the socket poller\n", stderr); exit(1); }
  /* published atomically: closing a socket reads it without pthread_once */
  __atomic_store_n(&w_poll_fd, pfd, __ATOMIC_RELEASE);
  pthread_t th;
  pthread_create(&th, NULL, w_poller, NULL);
  pthread_detach(th);
}

/* Park until fd may be ready in that direction. Returns -1 if another task
   is already waiting on it in that direction (or fd is out of range), -2
   if the task was cancelled (the caller cleans up, then calls
   w_cancel_unwind()), and -3 if the poller cannot watch fd. */
W_RT int w_wait_fd(int fd, int write) { return w_wait_fd_until(fd, write, 0); }
/* w_wait_fd with a deadline `at` (monotonic ns, 0 for none): -4 when it
   passes first. A timer claims the wait as the poller would; the timed-out
   task then takes itself off the descriptor, as a cancelled one does. */
W_RT int w_wait_fd_until(int fd, int write, int64_t at) {
  if (fd < 0 || fd >= W_MAX_FD) return -1;
  pthread_once(&w_poll_once, w_poll_start);
  WFdState *st = &w_fds[fd];
  pthread_mutex_lock(w_fd_lock(fd));
  if (!st->registered) {
    int bad;
#ifdef W_KQUEUE
    /* one change per call, so a descriptor that cannot be written (stdin)
       still gets its read filter */
    struct kevent ev;
    EV_SET(&ev, (uintptr_t)fd, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, NULL);
    bad = kevent(w_poll_fd, &ev, 1, NULL, 0, NULL) < 0;
    EV_SET(&ev, (uintptr_t)fd, EVFILT_WRITE, EV_ADD | EV_CLEAR, 0, 0, NULL);
    kevent(w_poll_fd, &ev, 1, NULL, 0, NULL);
#else
    struct epoll_event ev;
    ev.events = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLET;
    ev.data.fd = fd;
    bad = epoll_ctl(w_poll_fd, EPOLL_CTL_ADD, fd, &ev) < 0;
#endif
    if (bad) { pthread_mutex_unlock(w_fd_lock(fd)); return -3; }   /* the poller cannot watch it */
    st->registered = 1;
  }
  uint8_t *ready = write ? &st->wready : &st->rready;
  WFiber **slot = write ? &st->w : &st->r;
  WSel **tslot = write ? &st->wt : &st->rt;
  if (*ready) { *ready = 0; pthread_mutex_unlock(w_fd_lock(fd)); return 0; }
  if (*slot) { pthread_mutex_unlock(w_fd_lock(fd)); return -1; }
  if (at && w_mono_ns() >= at) { pthread_mutex_unlock(w_fd_lock(fd)); return -4; }
  WSel tok;
  WTimer t;
  if (!w_wait_begin(&tok)) { pthread_mutex_unlock(w_fd_lock(fd)); return -2; }
  __atomic_fetch_add(&w_io_waiters, 1, __ATOMIC_SEQ_CST);
  *slot = w_current(); *tslot = &tok;
  pthread_mutex_unlock(w_fd_lock(fd));
  if (at) {
    memset(&t, 0, sizeof t);
    t.at = at; t.pos = -1; t.f = w_current(); t.sel = &tok; t.idx = 1;
    w_tm_add(&t);
  }
  w_park_now();
  w_wait_end();
  __atomic_fetch_sub(&w_io_waiters, 1, __ATOMIC_SEQ_CST);
  if (at) w_tm_remove(&t);
  int fired = w_tok_fired(&tok);
  if (fired == W_CANCEL || fired == 1) {
    pthread_mutex_lock(w_fd_lock(fd));
    if (*tslot == &tok) { *slot = NULL; *tslot = NULL; }
    pthread_mutex_unlock(w_fd_lock(fd));
    return fired == 1 ? -4 : -2;
  }
  return 0;
}

/* Wake whoever waits on fd (used when a socket is closed under them). */
W_RT void w_fd_wake_all(int fd) { w_fd_event(fd, 1, 1); }

/* Forget fd's waiter state and close it, so a new socket that reuses the
   number starts clean. */
W_RT void w_sock_release_fd(int fd) {
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
W_RT int w_sock_enter(WCap *c) {
  __atomic_fetch_add(&c->inflight, 1, __ATOMIC_SEQ_CST);
  if (__atomic_load_n(&c->closing, __ATOMIC_SEQ_CST)) { __atomic_fetch_sub(&c->inflight, 1, __ATOMIC_SEQ_CST); return -1; }
  return __atomic_load_n(&c->fd, __ATOMIC_SEQ_CST);
}
W_RT void w_sock_really_close(WCap *c) {
  int fd = __atomic_exchange_n(&c->fd, -1, __ATOMIC_SEQ_CST);
  if (fd >= 0) w_sock_release_fd(fd);
}
W_RT void w_sock_leave(WCap *c) {
  if (__atomic_sub_fetch(&c->inflight, 1, __ATOMIC_SEQ_CST) == 0 && __atomic_load_n(&c->closing, __ATOMIC_SEQ_CST))
    w_sock_really_close(c);
}
W_RT int w_sock_closing(WCap *c) { return __atomic_load_n(&c->closing, __ATOMIC_SEQ_CST); }
W_RT void Cap_sock_close(WCap *c) {
  if (__atomic_exchange_n(&c->closing, 1, __ATOMIC_SEQ_CST)) return;
  int fd = __atomic_load_n(&c->fd, __ATOMIC_SEQ_CST);
  if (fd >= 0) {
    if (c->tls && c->kind == CAP_CONN) w_tls_close_notify(c);
    shutdown(fd, SHUT_RDWR);
    if (fd < W_MAX_FD && __atomic_load_n(&w_poll_fd, __ATOMIC_ACQUIRE) >= 0) w_fd_wake_all(fd);
  }
  if (__atomic_load_n(&c->inflight, __ATOMIC_SEQ_CST) == 0) w_sock_really_close(c);
}

W_RT void w_nonblock(int fd) {
  fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
  fcntl(fd, F_SETFD, FD_CLOEXEC);
#ifdef SO_NOSIGPIPE
  int one = 1;
  setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof one);
#endif
}

/* "host:port", host of [A-Za-z0-9.-], port 0..65535 (same rules as the
   interpreter); or "[ipv6]:port", the host kept with its brackets. */
W_RT int w_parse_addr(WStr *a, char *host, size_t hn, int *port) {
  if (a->len > 0 && a->data[0] == '[') {
    int64_t close = -1;
    for (int64_t i = 1; i < a->len; i++) if (a->data[i] == ']') { close = i; break; }
    if (close < 2 || close + 2 >= a->len || a->data[close + 1] != ':' || (size_t)close + 2 > hn || a->len - close - 2 > 5) return 0;
    char inner[64];
    if (close - 1 >= (int64_t)sizeof inner) return 0;
    memcpy(inner, a->data + 1, (size_t)(close - 1)); inner[close - 1] = 0;
    struct in6_addr probe;
    if (inet_pton(AF_INET6, inner, &probe) != 1) return 0;
    int p = 0;
    for (int64_t i = close + 2; i < a->len; i++) {
      if (a->data[i] < '0' || a->data[i] > '9') return 0;
      p = p * 10 + (a->data[i] - '0');
    }
    if (p > 65535) return 0;
    memcpy(host, a->data, (size_t)close + 1); host[close + 1] = 0; lower_ascii(host);
    *port = p;
    return 1;
  }
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
W_RT int w_resolve(const char *h, struct in_addr *out) {
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
W_RT void *w_dns_thread(void *p) {
  WDns *d = (WDns *)p;
  struct addrinfo hints; memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC; hints.ai_socktype = SOCK_STREAM; hints.ai_flags = AI_ADDRCONFIG;
  d->rc = getaddrinfo(d->host, NULL, &hints, &d->res);
  pthread_mutex_lock(&d->mu);
  if (d->state == 2) {
    pthread_mutex_unlock(&d->mu);
    if (d->rc == 0 && d->res) freeaddrinfo(d->res);
    close(d->fds[1]); pthread_mutex_destroy(&d->mu); free(d);
    return NULL;
  }
  d->state = 1;
  pthread_mutex_unlock(&d->mu);
  ssize_t k; do k = write(d->fds[1], "x", 1); while (k < 0 && errno == EINTR);
  return NULL;
}
/* The addresses for host (a name, an IPv4 address or a bracketed IPv6
   one) and port, in the order to try them; NULL with a message in err if
   there are none. A cancelled task unwinds. Free with freeaddrinfo. */
W_RT struct addrinfo *w_lookup(const char *h, int port, char *err, size_t en) {
  char hb[256];
  snprintf(hb, sizeof hb, "%s", strcmp(h, "localhost") == 0 ? "127.0.0.1" : h);
  if (hb[0] == '[') { size_t l = strlen(hb); memmove(hb, hb + 1, l); hb[l - 2] = 0; }
  char ps[8]; snprintf(ps, sizeof ps, "%d", port);
  struct addrinfo hints, *res = NULL;
  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC; hints.ai_socktype = SOCK_STREAM; hints.ai_flags = AI_NUMERICHOST | AI_NUMERICSERV;
  if (getaddrinfo(hb, ps, &hints, &res) == 0 && res) return res;   /* an address: no lookup */
  WDns *d = (WDns *)calloc(1, sizeof(WDns));
  if (!d) w_oom();
  snprintf(d->host, sizeof d->host, "%s", hb);
  pthread_mutex_init(&d->mu, NULL);
  if (pipe(d->fds) != 0) { snprintf(err, en, "cannot resolve %s: %s", h, strerror(errno)); pthread_mutex_destroy(&d->mu); free(d); return NULL; }
  w_nonblock(d->fds[0]);
  pthread_t th; pthread_attr_t at;
  pthread_attr_init(&at); pthread_attr_setdetachstate(&at, PTHREAD_CREATE_DETACHED);
  if (pthread_create(&th, &at, w_dns_thread, d) != 0) {
    pthread_attr_destroy(&at);
    close(d->fds[0]); close(d->fds[1]); pthread_mutex_destroy(&d->mu); free(d);
    snprintf(err, en, "cannot resolve %s: no thread for the lookup", h); return NULL;
  }
  pthread_attr_destroy(&at);
  for (;;) {
    char b;
    ssize_t k = read(d->fds[0], &b, 1);
    if (k == 1) break;
    if (k < 0 && errno == EINTR) continue;
    int wr = w_wait_fd(d->fds[0], 0);
    if (wr == -2 || wr == -1) {
      pthread_mutex_lock(&d->mu);
      int done = d->state == 1;
      if (!done) d->state = 2;
      pthread_mutex_unlock(&d->mu);
      w_sock_release_fd(d->fds[0]);
      if (done) {
        if (d->rc == 0 && d->res) freeaddrinfo(d->res);
        close(d->fds[1]); pthread_mutex_destroy(&d->mu); free(d);
      }
      if (wr == -2) w_cancel_unwind();
      snprintf(err, en, "cannot resolve %s: cannot wait for the lookup", h); return NULL;
    }
  }
  w_sock_release_fd(d->fds[0]); close(d->fds[1]);
  int rc = d->rc; res = d->res;
  pthread_mutex_destroy(&d->mu); free(d);
  if (rc != 0 || !res) { snprintf(err, en, "cannot resolve %s", h); return NULL; }
  /* port into every address */
  for (struct addrinfo *a = res; a; a = a->ai_next) {
    if (a->ai_family == AF_INET) ((struct sockaddr_in *)a->ai_addr)->sin_port = htons((uint16_t)port);
    else if (a->ai_family == AF_INET6) ((struct sockaddr_in6 *)a->ai_addr)->sin6_port = htons((uint16_t)port);
  }
  return res;
}
W_RT WIo w_errno_io(void) { return io_err(2, strerror(errno)); }

W_RT WIo Cap_listen_on(WCap *l, WStr *addr) {
  if (w_revoked(l)) return io_err(1, "listen: revoked");
  char h[256]; int port;
  if (!w_parse_addr(addr, h, sizeof h, &port)) return io_err(2, "address must be HOST:PORT");
  if (!net_permits(l, h, port)) { char m[400]; snprintf(m, sizeof m, "listen: %s:%d is not granted", h, port); return io_err(1, m); }
  if (h[0] == '[') {
    /* IPv6, and IPv4 too through mapped addresses: `[::]` serves both, as
       platforms that probe over IPv6 and forward over IPv4 need */
    struct sockaddr_in6 s6; memset(&s6, 0, sizeof s6);
    s6.sin6_family = AF_INET6; s6.sin6_port = htons((uint16_t)port);
    h[strlen(h) - 1] = 0;
    inet_pton(AF_INET6, h + 1, &s6.sin6_addr);
    int fd = socket(AF_INET6, SOCK_STREAM, 0);
    if (fd < 0) return w_errno_io();
    int one = 1, zero = 0;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    setsockopt(fd, IPPROTO_IPV6, IPV6_V6ONLY, &zero, sizeof zero);
    if (bind(fd, (struct sockaddr *)&s6, sizeof s6) < 0 || listen(fd, 4096) < 0) { WIo r = w_errno_io(); close(fd); return r; }
    w_nonblock(fd);
    socklen_t sl = sizeof s6;
    getsockname(fd, (struct sockaddr *)&s6, &sl);
    WCap *c = cap_new(CAP_LISTENER);
    c->fd = fd; c->port = ntohs(s6.sin6_port);
    w_rv_inherit(c, l);
    return io_ok_ptr(c);
  }
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
  w_rv_inherit(c, l);
  return io_ok_ptr(c);
}
/* One task at a time may wait to read (or accept) and one to write: an
   operation takes the slot when it first has to wait and keeps it until it
   returns, and another that has to wait meanwhile fails. The interpreter
   does the same, so both backends give the same error. */
W_RT int w_sock_claim(int *slot, int *held) {
  if (*held) return 1;
  int zero = 0;
  if (!__atomic_compare_exchange_n(slot, &zero, 1, 0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) return 0;
  *held = 1;
  return 1;
}
W_RT void w_sock_unclaim(int *slot, int held) { if (held) __atomic_store_n(slot, 0, __ATOMIC_RELEASE); }
W_RT WIo Cap_accept(WCap *l) {
  int fd = w_sock_enter(l);
  if (fd < 0) return io_err(2, "listener is closed");
  int held = 0, cancelled = 0;
  WIo r;
  for (;;) {
    int c = accept(fd, NULL, NULL);
    if (c >= 0) {
      w_nonblock(c);
      int one = 1;
      setsockopt(c, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one);
      WCap *conn = cap_new(CAP_CONN);
      conn->fd = c;
      w_rv_inherit(conn, l);
      if (l->tls && !w_tls_accept(l, conn, &r)) break;
      r = io_ok_ptr(conn);
      break;
    }
    if (errno == EINTR || errno == ECONNABORTED) continue;
    if ((errno == EAGAIN || errno == EWOULDBLOCK) && !w_sock_closing(l)) {
      if (!w_sock_claim(&l->reading, &held)) { r = io_err(2, "another task is already accepting on this listener"); break; }
      int wr = w_wait_fd(fd, 0);
      if (wr == -2) { cancelled = 1; break; }
      if (wr < 0) { r = io_err(2, "another task is already accepting on this listener"); break; }
      if (w_sock_closing(l)) { r = io_err(2, "listener is closed"); break; }
      continue;
    }
    r = w_sock_closing(l) ? io_err(2, "listener is closed") : w_errno_io();
    break;
  }
  w_sock_unclaim(&l->reading, held);
  w_sock_leave(l);
  if (cancelled) w_cancel_unwind();
  return r;
}
/* One connection attempt: the connected fd, -1 with err set, or it unwinds
   if the task is cancelled. */
W_RT int w_connect_one(struct addrinfo *a, char *err, size_t en) {
  int fd = socket(a->ai_family, SOCK_STREAM, 0);
  if (fd < 0) { snprintf(err, en, "%s", strerror(errno)); return -1; }
  w_nonblock(fd);
  if (connect(fd, a->ai_addr, a->ai_addrlen) == 0) return fd;
  if (errno != EINPROGRESS) { snprintf(err, en, "%s", strerror(errno)); close(fd); return -1; }
  for (;;) {
    int wr = w_wait_fd(fd, 1);
    if (wr == -2) { w_sock_release_fd(fd); return -2; }
    if (wr < 0) { w_sock_release_fd(fd); snprintf(err, en, "cannot wait on socket"); return -1; }
    int e = 0; socklen_t el = sizeof e;
    getsockopt(fd, SOL_SOCKET, SO_ERROR, &e, &el);
    if (e) { w_sock_release_fd(fd); snprintf(err, en, "%s", strerror(e)); return -1; }
    struct sockaddr_storage peer; socklen_t pl = sizeof peer;
    if (getpeername(fd, (struct sockaddr *)&peer, &pl) == 0) return fd;   /* connected */
    if (errno != ENOTCONN) { snprintf(err, en, "%s", strerror(errno)); w_sock_release_fd(fd); return -1; }
  }
}
W_RT WIo Cap_net_connect(WCap *n, WStr *addr) {
  if (w_revoked(n)) return io_err(1, "net: revoked");
  char h[256]; int port;
  if (!w_parse_addr(addr, h, sizeof h, &port)) return io_err(2, "address must be HOST:PORT");
  if (!net_permits(n, h, port)) { char m[400]; snprintf(m, sizeof m, "net: %s:%d is not granted", h, port); return io_err(1, m); }
  char err[400];
  struct addrinfo *res = w_lookup(h, port, err, sizeof err);
  if (!res) return io_err(2, err);
  /* each address in turn, as the resolver ordered them (IPv6 first where
     the host has a route for it); the last failure is the error */
  int fd = -1;
  for (struct addrinfo *a = res; a && fd < 0; a = a->ai_next) {
    if (a->ai_family != AF_INET && a->ai_family != AF_INET6) continue;
    fd = w_connect_one(a, err, sizeof err);
    if (fd == -2) { freeaddrinfo(res); w_cancel_unwind(); }
  }
  freeaddrinfo(res);
  if (fd < 0) return io_err(2, err);
  WCap *c = cap_new(CAP_CONN);
  c->fd = fd;
  w_rv_inherit(c, n);
  return io_ok_ptr(c);
}
W_RT ssize_t w_conn_step(WCap *c, int fd, int wr, char *buf, size_t n, int *dir, char *err, size_t en) {
  if (c->tls) return w_tls_step(c, wr, buf, n, dir, err, en);
  for (;;) {
    ssize_t k = wr ? write(fd, buf, n) : read(fd, buf, n);
    if (k >= 0) return k;
    if (errno == EINTR) continue;
    if (errno == EAGAIN || errno == EWOULDBLOCK) { *dir = wr; return -1; }
    snprintf(err, en, "%s", strerror(errno));
    return -2;
  }
}
/* Park a connection's reader (or writer) until the socket is ready in
   direction dir: 0 when it is, -1 if another task holds that side, -2 if
   cancelled. When TLS has to wait on the other side, which that side's
   task may already be waiting on, it naps briefly instead. */
W_RT int w_conn_wait(int fd, int own, int dir) { return w_conn_wait_until(fd, own, dir, 0); }
/* The same with a deadline (0 for none): -4 once it has passed. */
W_RT int w_conn_wait_until(int fd, int own, int dir, int64_t at) {
  int wr = w_wait_fd_until(fd, dir, at);
  if (wr == -1 && dir != own) {
    if (at && w_mono_ns() >= at) return -4;
    return w_nap(1);
  }
  return wr == -2 ? -2 : wr == -4 ? -4 : wr < 0 ? -1 : 0;
}
W_RT WIo Cap_conn_read(WCap *c) { return Cap_conn_read_until(c, 0, 0); }
W_RT WIo Cap_conn_write(WCap *c, WStr *s) { return Cap_conn_write_until(c, s, 0, 0); }
static WIo w_timed_out(int64_t ms) {
  char m[64];
  snprintf(m, sizeof m, "timed out after %lld ms", (long long)ms);
  return io_err(2, m);
}
/* Conn.read_within(clock, ms) and write_within(clock, ms, s): as read and
   write, but an error once ms milliseconds have passed while waiting. */
W_RT WIo Cap_conn_read_within(WCap *c, WCap *clock, int64_t ms) {
  (void)clock;
  return Cap_conn_read_until(c, w_deadline_ms(ms < 0 ? 0 : ms), ms);
}
W_RT WIo Cap_conn_write_within(WCap *c, WCap *clock, int64_t ms, WStr *s) {
  (void)clock;
  return Cap_conn_write_until(c, s, w_deadline_ms(ms < 0 ? 0 : ms), ms);
}
W_RT WIo Cap_conn_read_until(WCap *c, int64_t at, int64_t ms) {
  int fd = w_sock_enter(c);
  if (fd < 0) return io_err(2, "connection is closed");
  char *buf = (char *)w_raw(65536);
  int held = 0, cancelled = 0;
  WIo r;
  char err[300];
  for (;;) {
    int dir = 0;
    ssize_t n = w_conn_step(c, fd, 0, buf, 65536, &dir, err, sizeof err);
    if (n >= 0 && !w_sock_closing(c)) { r = io_ok(Str_from(buf, n)); break; }
    if (n == -1 && !w_sock_closing(c)) {
      if (!w_sock_claim(&c->reading, &held)) { r = io_err(2, "another task is already reading this connection"); break; }
      int wr = w_conn_wait_until(fd, 0, dir, at);
      if (wr == -2) { cancelled = 1; break; }
      if (wr == -4) { r = w_timed_out(ms); break; }
      if (wr < 0) { r = io_err(2, "another task is already reading this connection"); break; }
      continue;
    }
    r = w_sock_closing(c) ? io_err(2, "connection is closed") : io_err(2, err);
    break;
  }
  free(buf);
  w_sock_unclaim(&c->reading, held);
  w_sock_leave(c);
  if (cancelled) w_cancel_unwind();
  return r;
}
W_RT WIo Cap_conn_write_until(WCap *c, WStr *s, int64_t at, int64_t ms) {
  int fd = w_sock_enter(c);
  if (fd < 0) return io_err(2, "connection is closed");
  int64_t off = 0;
  int held = 0, cancelled = 0;
  WIo r = io_ok(NULL);
  char err[300];
  /* a writer holds the slot from its first byte: a second writer that sent
     part of its data before finding the slot taken would mix the two */
  if (!w_sock_claim(&c->writing, &held)) { w_sock_leave(c); return io_err(2, "another task is already writing this connection"); }
  while (off < s->len) {
    if (w_sock_closing(c)) { r = io_err(2, "connection is closed"); break; }
    int dir = 1;
    ssize_t n = w_conn_step(c, fd, 1, s->data + off, (size_t)(s->len - off), &dir, err, sizeof err);
    if (n >= 0) { off += n; continue; }
    if (n == -1) {
      if (!w_sock_claim(&c->writing, &held)) { r = io_err(2, "another task is already writing this connection"); break; }
      int wr = w_conn_wait_until(fd, 1, dir, at);
      if (wr == -2) { cancelled = 1; break; }
      if (wr == -4) { r = w_timed_out(ms); break; }
      if (wr < 0) { r = io_err(2, "another task is already writing this connection"); break; }
      continue;
    }
    r = io_err(2, err);
    break;
  }
  w_sock_unclaim(&c->writing, held);
  w_sock_leave(c);
  if (cancelled) w_cancel_unwind();
  return r;
}
#ifdef LIMEN_TLS
W_RT void w_tls_release(WCap *c) {
  if (c->kind == CAP_LISTENER) SSL_CTX_free((SSL_CTX *)c->tls);
  else vtls_free((VTls *)c->tls);
  c->tls = NULL;
}
W_RT void w_tls_close_notify(WCap *c) { vtls_close_notify((VTls *)c->tls); }
W_RT int w_tls_step(WCap *c, int wr, char *buf, size_t n, int *dir, char *err, size_t en) {
  int k = vtls_step((VTls *)c->tls, wr ? VTLS_WRITE : VTLS_READ, buf, n > (size_t)INT_MAX ? INT_MAX : (int)n, err, en);
  if (k >= 0) return k;
  if (k == VTLS_WANT_READ) { *dir = 0; return -1; }
  if (k == VTLS_WANT_WRITE) { *dir = 1; return -1; }
  return -2;
}
W_RT int w_tls_accept(WCap *l, WCap *conn, WIo *r) {
  char err[300];
  conn->tls = vtls_new((SSL_CTX *)l->tls, conn->fd, 1, err, sizeof err);
  if (conn->tls) return 1;
  Cap_sock_close(conn); Cap_drop(conn);
  *r = io_err(2, err);
  return 0;
}
W_RT WIo Cap_net_connect_tls(WCap *n, WStr *addr) {
  char h[256]; int port;
  if (!w_parse_addr(addr, h, sizeof h, &port)) return io_err(2, "address must be HOST:PORT");
  WIo r = Cap_net_connect(n, addr);
  if (r.kind != 0) return r;
  WCap *c = (WCap *)r.ptr;
  char err[300];
  if (h[0] == '[') { size_t l = strlen(h); memmove(h, h + 1, l); h[l - 2] = 0; }
  VTls *t = vtls_client(c->fd, h, err, sizeof err);
  if (!t) { Cap_sock_close(c); Cap_drop(c); return io_err(2, err); }
  c->tls = t;
  for (;;) {
    int k = vtls_step(t, VTLS_HANDSHAKE, NULL, 0, err, sizeof err);
    if (k == 0) return io_ok_ptr(c);
    if (k == VTLS_ERROR) { Cap_sock_close(c); Cap_drop(c); return io_err(2, err); }
    int wr = w_wait_fd(c->fd, k == VTLS_WANT_WRITE);
    if (wr == -2) { Cap_sock_close(c); Cap_drop(c); w_cancel_unwind(); }
    if (wr < 0) { Cap_sock_close(c); Cap_drop(c); return io_err(2, "cannot wait on socket"); }
  }
}
/* The crypto module's runtime helper (vcrypto in limen_tls.h): the result
   after a status byte, 0 for success, 1 for an error message. */
W_RT WStr *w_crypto(int64_t op, WStr *a, WStr *b, WStr *c, WStr *d) {
  unsigned char *out = NULL; size_t n = 0; char err[300];
  int r = vcrypto((int)op, (const unsigned char *)a->data, (size_t)a->len, (const unsigned char *)b->data, (size_t)b->len,
                  (const unsigned char *)c->data, (size_t)c->len, (const unsigned char *)d->data, (size_t)d->len, &out, &n, err, sizeof err);
  if (r != 0) { size_t m = strlen(err); WStr *s = Str_new((int64_t)m + 1); s->data[0] = 1; memcpy(s->data + 1, err, m); return s; }
  WStr *s = Str_new((int64_t)n + 1);
  s->data[0] = 0;
  if (n) memcpy(s->data + 1, out, n);
  OPENSSL_cleanse(out, n);
  free(out);
  return s;
}
/* Conn.start_tls(host): a client TLS handshake on a connection that is
   already open, for protocols that start TLS inside plain TCP (Postgres's
   SSLRequest, Redis behind a proxy, SMTP's STARTTLS). The server's
   certificate is checked against host, as in connect_tls. The handshake
   holds both the reading and the writing slot, so no other task's bytes
   can land in the middle of it; a failed handshake closes the
   connection, which can no longer be trusted. */
W_RT WIo Cap_conn_start_tls(WCap *c, WStr *host) {
  int fd = w_sock_enter(c);
  if (fd < 0) return io_err(2, "connection is closed");
  if (c->tls) { w_sock_leave(c); return io_err(2, "tls: the connection already speaks TLS"); }
  if (host->len <= 0 || host->len >= 256 || memchr(host->data, 0, (size_t)host->len)) { w_sock_leave(c); return io_err(2, "tls: start_tls needs the server's host name"); }
  int rh = 0, wh = 0;
  if (!w_sock_claim(&c->reading, &rh) || !w_sock_claim(&c->writing, &wh)) {
    w_sock_unclaim(&c->reading, rh); w_sock_unclaim(&c->writing, wh); w_sock_leave(c);
    return io_err(2, "another task is using this connection");
  }
  char h[256], err[300];
  memcpy(h, host->data, (size_t)host->len); h[host->len] = 0;
  WIo r = io_ok(NULL);
  int cancelled = 0;
  VTls *t = vtls_client(fd, h, err, sizeof err);
  if (!t) r = io_err(2, err);
  else {
    c->tls = t;
    for (;;) {
      if (w_sock_closing(c)) { r = io_err(2, "connection is closed"); break; }
      int k = vtls_step(t, VTLS_HANDSHAKE, NULL, 0, err, sizeof err);
      if (k == 0) break;
      if (k == VTLS_ERROR) { r = io_err(2, err); break; }
      int wr = w_wait_fd(fd, k == VTLS_WANT_WRITE);
      if (wr == -2) { cancelled = 1; break; }
      if (wr < 0) { r = io_err(2, "cannot wait on socket"); break; }
    }
  }
  w_sock_unclaim(&c->reading, rh); w_sock_unclaim(&c->writing, wh);
  w_sock_leave(c);
  if (r.kind != 0 || cancelled) Cap_sock_close(c);
  if (cancelled) w_cancel_unwind();
  return r;
}
W_RT WIo Cap_listen_on_tls(WCap *l, WStr *addr, WStr *cert, WStr *key) {
  WIo r = Cap_listen_on(l, addr);
  if (r.kind != 0) return r;
  char err[300];
  SSL_CTX *ctx = vtls_server_ctx(cert->data, (size_t)cert->len, key->data, (size_t)key->len, err, sizeof err);
  WCap *c = (WCap *)r.ptr;
  if (!ctx) { Cap_sock_close(c); Cap_drop(c); return io_err(2, err); }
  c->tls = ctx;
  return r;
}
#else
#define W_NO_TLS "tls: this program was built without TLS (OpenSSL was not found when it was compiled)"
W_RT void w_tls_release(WCap *c) { c->tls = NULL; }
W_RT void w_tls_close_notify(WCap *c) { (void)c; }
W_RT int w_tls_step(WCap *c, int wr, char *buf, size_t n, int *dir, char *err, size_t en) {
  (void)c; (void)wr; (void)buf; (void)n; (void)dir; snprintf(err, en, W_NO_TLS); return -2;
}
W_RT int w_tls_accept(WCap *l, WCap *conn, WIo *r) { (void)l; (void)conn; (void)r; return 1; }
W_RT WIo Cap_net_connect_tls(WCap *n, WStr *addr) {
  if (w_revoked(n)) return io_err(1, "net: revoked");
  char h[256]; int port;
  if (!w_parse_addr(addr, h, sizeof h, &port)) return io_err(2, "address must be HOST:PORT");
  if (!net_permits(n, h, port)) { char m[400]; snprintf(m, sizeof m, "net: %s:%d is not granted", h, port); return io_err(1, m); }
  return io_err(2, W_NO_TLS);
}
W_RT WIo Cap_conn_start_tls(WCap *c, WStr *host) { (void)c; (void)host; return io_err(2, W_NO_TLS); }
W_RT WStr *w_crypto(int64_t op, WStr *a, WStr *b, WStr *c, WStr *d) {
  (void)op; (void)a; (void)b; (void)c; (void)d;
  const char *m = "crypto: this program was built without OpenSSL";
  WStr *s = Str_new((int64_t)strlen(m) + 1); s->data[0] = 1; memcpy(s->data + 1, m, strlen(m)); return s;
}
W_RT WIo Cap_listen_on_tls(WCap *l, WStr *addr, WStr *cert, WStr *key) {
  (void)cert; (void)key;
  if (w_revoked(l)) return io_err(1, "listen: revoked");
  char h[256]; int port;
  if (!w_parse_addr(addr, h, sizeof h, &port)) return io_err(2, "address must be HOST:PORT");
  if (!net_permits(l, h, port)) { char m[400]; snprintf(m, sizeof m, "listen: %s:%d is not granted", h, port); return io_err(1, m); }
  return io_err(2, W_NO_TLS);
}
#endif
WChan *w_in_lock;
char *w_in_buf;
int64_t w_in_len, w_in_cap;
int w_in_eof, w_in_direct;
pthread_once_t w_in_once = PTHREAD_ONCE_INIT;
W_RT void w_in_init(void) {
  /* built by hand, not counted by the leak check: it lives as long as the program */
  WChan *c = (WChan *)w_raw(sizeof(WChan));
  memset(c, 0, sizeof *c);
  c->cap = 1; c->esz = sizeof(int64_t); c->buf = (char *)w_raw(sizeof(int64_t)); c->len = 1;
  memset(c->buf, 0, sizeof(int64_t));
  pthread_mutex_init(&c->mu, NULL);
  w_in_lock = c;
  struct stat st;
  w_in_direct = fstat(0, &st) == 0 && S_ISREG(st.st_mode);
}
W_RT void w_in_acquire(void) {
  pthread_once(&w_in_once, w_in_init);
  int64_t tok;
  w_chan_recv(w_in_lock, &tok);   /* cancellable, before anything is held */
}
W_RT void w_in_release(void) { int64_t tok = 0; w_chan_send(w_in_lock, &tok, "stdin"); }
/* Read more input into the buffer. Returns 1, 0 at the end of input, or -2
   if the task was cancelled while it waited. */
W_RT int w_in_fill(void) {
  char *tmp = (char *)w_raw(65536);
  for (;;) {
    if (!w_in_direct) {
      struct pollfd p = { 0, POLLIN, 0 };
      int r = poll(&p, 1, 0);
      if (r < 0 && errno == EINTR) continue;
      if (r == 0) {
        int wr = w_wait_fd(0, 0);
        if (wr == -2) { free(tmp); return -2; }
        if (wr < 0) w_in_direct = 1;
        continue;
      }
      if (r < 0 || (p.revents & POLLNVAL)) w_in_direct = 1;
    }
    if (w_in_direct) fflush(stdout); /* a prompt shows before a read that blocks */
    ssize_t n = read(0, tmp, 65536);
    if (n < 0 && errno == EINTR) continue;
    if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {   /* inherited a non-blocking stdin */
      struct pollfd p = { 0, POLLIN, 0 };
      if (w_in_direct) poll(&p, 1, -1);
      else { int wr = w_wait_fd(0, 0); if (wr == -2) { free(tmp); return -2; } if (wr < 0) w_in_direct = 1; }
      continue;
    }
    if (n <= 0) { free(tmp); w_in_eof = 1; return 0; }
    if (w_in_len + n > w_in_cap) {
      while (w_in_len + n > w_in_cap) w_in_cap = w_in_cap ? w_in_cap * 2 : 65536;
      w_in_buf = (char *)w_realloc(w_in_buf, (size_t)w_in_cap);
    }
    memcpy(w_in_buf + w_in_len, tmp, (size_t)n);
    w_in_len += n;
    free(tmp);
    return 1;
  }
}
W_RT void w_in_consume(int64_t n) { memmove(w_in_buf, w_in_buf + n, (size_t)(w_in_len - n)); w_in_len -= n; }
W_RT WStr *Cap_stdin_line(bool *ok) {
  w_out_unhold();
  w_in_acquire();
  int64_t from = 0;
  for (;;) {
    char *nl = w_in_len > from ? (char *)memchr(w_in_buf + from, '\n', (size_t)(w_in_len - from)) : NULL;
    if (nl || w_in_eof) {
      int64_t end = nl ? nl - w_in_buf : w_in_len;
      if (!nl && end == 0) { w_in_release(); *ok = false; return NULL; }
      int64_t n = end;
      if (n > 0 && w_in_buf[n - 1] == '\r') n--;
      WStr *s = Str_from(w_in_buf, n);
      w_in_consume(nl ? end + 1 : end);
      w_in_release();
      *ok = true;
      return s;
    }
    from = w_in_len;
    if (w_in_fill() == -2) { w_in_release(); w_cancel_unwind(); }
  }
}
W_RT WStr *Cap_stdin_all(void) {
  w_out_unhold();
  w_in_acquire();
  while (!w_in_eof) if (w_in_fill() == -2) { w_in_release(); w_cancel_unwind(); }
  WStr *s = Str_from(w_in_buf ? w_in_buf : "", w_in_len);
  w_in_len = 0;
  w_in_release();
  return s;
}
W_RT bool Cap_foreign_allows(WCap *x, WStr *lib_s) {
  if (memchr(lib_s->data, 0, (size_t)lib_s->len) || w_revoked(x)) return false;
  for (int i = 0; i < x->nhosts; i++)
    if (strcmp(x->hosts[i], "*") == 0 || ((size_t)lib_s->len == strlen(x->hosts[i]) && memcmp(x->hosts[i], lib_s->data, (size_t)lib_s->len) == 0)) return true;
  return false;
}
W_RT WCap *Cap_foreign_only(WCap *x, WStr *lib_s) {
  char *lib = w_strndup(lib_s->data, (size_t)lib_s->len);
  bool covered;
  if (strcmp(lib, "*") == 0) { covered = false; for (int i = 0; i < x->nhosts; i++) if (strcmp(x->hosts[i], "*") == 0) covered = true; }
  else covered = Cap_foreign_allows(x, lib_s);
  WCap *c = cap_new(CAP_FOREIGN);
  w_rv_inherit(c, x);
  c->hosts = (char **)w_raw(sizeof(char *));
  if (covered) { c->hosts[0] = lib; c->nhosts = 1; } else free(lib);
  return c;
}
W_RT void w_foreign_check(WCap *x, const char *lib, const char *where) {
  if (w_revoked(x)) w_panic_c("foreign: revoked", where);
  for (int i = 0; i < x->nhosts; i++) if (strcmp(x->hosts[i], "*") == 0 || strcmp(x->hosts[i], lib) == 0) return;
  char msg[512];
  snprintf(msg, sizeof msg, "foreign: library `%s` is not granted (grant it with --grant foreign=%s)", lib, lib);
  w_panic_c(msg, where);
}
W_RT int32_t w_ffi_i32(int64_t v, const char *arg, const char *where) {
  if (v < INT32_MIN || v > INT32_MAX) {
    char msg[256]; snprintf(msg, sizeof msg, "foreign: %lld does not fit in Int32 (%s)", (long long)v, arg);
    w_panic_c(msg, where);
  }
  return (int32_t)v;
}
W_RT WStr *w_ffi_str(const char *p) { return p ? Str_from(p, (int64_t)strlen(p)) : Str_new(0); }
/* Kept callbacks (`Kept[fn(...)]`): C may keep the function pointer it was
   given and call it at any time, from any thread. The pointer is one of
   W_KEPT trampolines the program has for that C signature, and its number
   is a slot here holding the channel it was made from; each call builds
   the value and sends it with w_kept_send. A slot stays taken for the rest
   of the program (C may still hold the pointer), so the same channel always
   gets the same slot, and a program has W_KEPT channels at most. */
WChan *w_kept_ch[W_KEPT];
pthread_mutex_t w_kept_mu = PTHREAD_MUTEX_INITIALIZER;
W_RT int64_t w_kept_slot(WChan *c, const char *where) {
  pthread_mutex_lock(&w_kept_mu);
  for (int i = 0; i < W_KEPT; i++)
    if (w_kept_ch[i] == c) { pthread_mutex_unlock(&w_kept_mu); return i; }
  for (int i = 0; i < W_KEPT; i++)
    if (!w_kept_ch[i]) {
      Chan_share(c);
      w_kept_ch[i] = Chan_dup(c);
      /* C may send at any time, so a task waiting on it is not deadlocked */
      __atomic_fetch_add(&w_io_waiters, 1, __ATOMIC_SEQ_CST);
      pthread_mutex_unlock(&w_kept_mu);
      return i;
    }
  pthread_mutex_unlock(&w_kept_mu);
  char msg[160];
  snprintf(msg, sizeof msg, "foreign: C keeps %d channels already, the most a program can give it", W_KEPT);
  w_panic_c(msg, where);
}
/* Send [v] (owned) on slot [i]'s channel from wherever C called: never
   waits, since the caller may be a thread that is not a task. A waiting
   receiver gets it at once; otherwise it is queued, the channel growing
   when full. On a closed channel, or once the program is ending, it is
   dropped. */
W_RT void w_kept_send(int64_t i, void *v) {
  pthread_mutex_lock(&w_kept_mu);
  WChan *c = w_kept_ch[i];
  if (c) Chan_dup(c);
  pthread_mutex_unlock(&w_kept_mu);
  if (!c) return;
  if (c->eshare) c->eshare(v);
  pthread_mutex_lock(&c->mu);
  if (c->closed) {
    pthread_mutex_unlock(&c->mu);
    if (c->edrop) c->edrop(v);
    Chan_drop(c);
    return;
  }
  WChWait *r = w_chq_pop_live(&c->rq, &c->rq_t);
  if (r) {
    memcpy(r->val, v, c->esz); r->ok = 1;
    pthread_mutex_unlock(&c->mu);
    w_wake(r->f);
    Chan_drop(c);
    return;
  }
  if (c->len == c->cap) {
    int64_t cap = c->cap ? c->cap * 2 : 16;
    char *buf = (char *)w_raw(w_bytes(cap, c->esz));
    for (int64_t k = 0; k < c->len; k++) memcpy(buf + k * c->esz, c->buf + ((c->head + k) % c->cap) * c->esz, c->esz);
    free(c->buf);
    c->buf = buf; c->cap = cap; c->head = 0;
  }
  memcpy(c->buf + ((c->head + c->len) % c->cap) * c->esz, v, c->esz);
  c->len++;
  pthread_mutex_unlock(&c->mu);
  Chan_drop(c);
}
/* The program is ending: let go of the kept channels (a late call from C
   finds its slot empty and is dropped). */
W_RT void w_kept_release(void) {
  pthread_mutex_lock(&w_kept_mu);
  for (int i = 0; i < W_KEPT; i++)
    if (w_kept_ch[i]) { Chan_drop(w_kept_ch[i]); w_kept_ch[i] = NULL; }
  pthread_mutex_unlock(&w_kept_mu);
}
pthread_mutex_t w_bl_mu = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t w_bl_cv = PTHREAD_COND_INITIALIZER;
WBlock *w_bl_head, *w_bl_tail;
int w_bl_threads, w_bl_idle, w_bl_spinning, w_bl_max;
W_RT int64_t w_real_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (int64_t)ts.tv_sec * 1000000000 + ts.tv_nsec;
}
W_RT void *w_block_new(size_t n, void (*run)(WBlock *)) {
  WBlock *b = (WBlock *)calloc(1, n);
  if (!b) w_oom();
  b->run = run;
  return b;
}
W_RT const void *w_block_keep(WBlock *b, WStr *s) {
  Str_share(s);
  b->keep[b->nkeep++] = Str_dup(s);
  return s->data;
}
W_RT void w_block_free(WBlock *b) {
  for (int i = 0; i < b->nkeep; i++) Str_drop(b->keep[i]);
  if (b->res) Str_drop(b->res);
  free(b);
}
/* The job's variable goes out of scope, normally or unwinding (generated
   code declares it with this cleanup): freed, unless a cancelled task leaves
   it while C still runs it, in which case the pool frees it when C returns.
   That hand-over is decided here, as the task's last use of the job: once
   state says abandoned, the pool thread may free it at any moment. pooled
   is only ever set and read by the task, so it needs no lock. */
W_RT void w_block_end(void *p) {
  WBlock *b = *(WBlock **)p;
  if (!b) return;
  if (b->pooled) {
    pthread_mutex_lock(&w_bl_mu);
    int running = b->state == 0;
    if (running) b->state = 2;
    pthread_mutex_unlock(&w_bl_mu);
    if (running) return;
  }
  w_block_free(b);
}
/* The Str result, now the task's (on its boundary's list, like anything it allocates). */
W_RT WStr *w_block_str(WBlock *b) {
  WStr *s = b->res;
  b->res = NULL;
  WCatch **slot = w_catch_slot();
  if (slot && *slot) w_hdr_link(&(*slot)->objs, (WHdr *)s - 1);
  return s;
}
W_RT void *w_bl_thread(void *arg) {
  (void)arg;
  pthread_mutex_lock(&w_bl_mu);
  for (;;) {
    if (!w_bl_head) {
      /* watch the queue for a while before sleeping */
      w_bl_spinning++;
      pthread_mutex_unlock(&w_bl_mu);
      int64_t until = w_real_ns() + W_BL_SPIN_NS;
      for (int i = 1; !__atomic_load_n(&w_bl_head, __ATOMIC_ACQUIRE); i++) {
        w_cpu_relax();
        if ((i & 63) == 0 && w_real_ns() > until) break;
      }
      pthread_mutex_lock(&w_bl_mu);
      w_bl_spinning--;
    }
    while (!w_bl_head) { w_bl_idle++; pthread_cond_wait(&w_bl_cv, &w_bl_mu); w_bl_idle--; }
    WBlock *b = w_bl_head;
    __atomic_store_n(&w_bl_head, b->next, __ATOMIC_RELAXED);
    if (!w_bl_head) w_bl_tail = NULL;
    pthread_mutex_unlock(&w_bl_mu);
    b->run(b);
    pthread_mutex_lock(&w_bl_mu);
    if (b->state == 2) { pthread_mutex_unlock(&w_bl_mu); w_block_free(b); pthread_mutex_lock(&w_bl_mu); continue; }
    b->state = 1;
    WFiber *f = b->f;
    int claimed = w_tok_claim(&b->tok, 0);
    pthread_mutex_unlock(&w_bl_mu);
    if (claimed) w_wake(f);
    pthread_mutex_lock(&w_bl_mu);
  }
  return NULL;
}
/* Hand the job to the pool (with w_bl_mu held). */
W_RT void w_bl_submit(WBlock *b) {
  if (!w_bl_max) {
    const char *e = getenv("LIMEN_BLOCKING_THREADS");
    w_bl_max = e && atoi(e) > 0 ? atoi(e) : 64;
  }
  b->next = NULL;
  b->pooled = 1;
  if (w_bl_tail) w_bl_tail->next = b; else __atomic_store_n(&w_bl_head, b, __ATOMIC_RELEASE);
  w_bl_tail = b;
  if (w_bl_spinning > 0) { /* a watching thread takes it */ }
  else if (w_bl_idle > 0) pthread_cond_signal(&w_bl_cv);
  else if (w_bl_threads < w_bl_max) {
    pthread_t th; pthread_attr_t at;
    pthread_attr_init(&at);
    pthread_attr_setdetachstate(&at, PTHREAD_CREATE_DETACHED);
    pthread_attr_setstacksize(&at, (size_t)4 << 20);   /* C libraries expect a thread's usual stack */
    if (pthread_create(&th, &at, w_bl_thread, NULL) == 0) w_bl_threads++;
    else if (w_bl_threads == 0) w_fatal_c("cannot start a thread for a blocking C call", "?");
    pthread_attr_destroy(&at);
  }
}
/* Run the job on the pool and wait for it. A cancelled task abandons it and
   unwinds; the job is then the pool's to free. */
W_RT void w_block_wait(WBlock *b) {
  WFiber *f = w_current();
  /* in a callback from C the task cannot park (C's frames are on its
     stack): the call runs here, as C is already blocking this thread */
  if (f->in_cb) { b->run(b); return; }
  int cancellable = !f->unwinding;
  b->f = f;
  pthread_mutex_lock(&w_bl_mu);
  if (cancellable) {
    if (!w_wait_begin(&b->tok)) { pthread_mutex_unlock(&w_bl_mu); w_cancel_unwind(); }
  } else {
    b->tok.done = 0; b->tok.fired = -1;
    w_prepare_park();
  }
  w_bl_submit(b);
  __atomic_fetch_add(&w_io_waiters, 1, __ATOMIC_SEQ_CST);   /* the pool will wake it: not a deadlock */
  pthread_mutex_unlock(&w_bl_mu);
  /* Watch for the call's end before parking. A waker (the pool thread, or
     the canceller) leaves the fiber W_NOTIFIED as the last thing it does to
     it, so once that shows, nothing touches the fiber any more and it goes
     on without parking. */
  int64_t until = w_real_ns() + W_BL_SPIN_NS;
  int quick = 0;
  for (int i = 1;; i++) {
    if (__atomic_load_n(&f->park_state, __ATOMIC_ACQUIRE) == W_NOTIFIED) {
      __atomic_store_n(&f->park_state, W_RUNNING, __ATOMIC_RELEASE);
      quick = 1;
      break;
    }
    w_cpu_relax();
    if ((i & 63) == 0 && w_real_ns() > until) break;
  }
  if (!quick) w_park_now();
  __atomic_fetch_sub(&w_io_waiters, 1, __ATOMIC_SEQ_CST);
  if (cancellable) w_wait_end();
  /* cancelled: the job stays pooled and w_block_end, as the task unwinds,
     either frees it (C has returned) or leaves it to the pool */
  if (w_tok_fired(&b->tok) == W_CANCEL) {
    /* a call that can be told to stop is told, and waited for (not
       cancellably), so it never outlives the task: what it was lent and
       what it works on (an SQL connection) are then never used after the
       task let go */
    if (b->stoppable) {
      pthread_mutex_lock(&w_bl_mu);
      if (b->state == 0) {
        __atomic_store_n(&b->stop, 1, __ATOMIC_RELEASE);
        b->tok.done = 0; b->tok.fired = -1;
        w_prepare_park();
        __atomic_fetch_add(&w_io_waiters, 1, __ATOMIC_SEQ_CST);
        pthread_mutex_unlock(&w_bl_mu);
        w_park_now();
        __atomic_fetch_sub(&w_io_waiters, 1, __ATOMIC_SEQ_CST);
      } else pthread_mutex_unlock(&w_bl_mu);
    }
    w_cancel_unwind();
  }
  b->pooled = 0;
}
/* ---------- SQL statements ----------
   The std sql module runs each statement as one blocking call
   (limen_sql_run): w_sql_core (limen_sql.h) on the pool, with the SQLite
   functions the generated code passes in. A cancelled task stops the
   statement and waits for it. */
typedef struct { WBlock h; void *api[W_SQL_NAPI]; void *db; const char *text, *params; int64_t tlen, plen; int mode; } WSqlJob;
static void w_sql_job_run(WBlock *b) {
  WSqlJob *j = (WSqlJob *)b;
  WSqlBuf o = { 0 };
  w_sql_core(j->api, j->db, j->text, j->tlen, j->params, j->plen, j->mode, &b->stop, &o);
  b->res = Str_from(o.p, o.len);
  free(o.p);
}
W_RT WStr *w_sql_run(const void *const *api, int64_t db, WStr *text, WStr *params, int64_t mode) {
  WSqlJob *j __attribute__((cleanup(w_block_end))) = (WSqlJob *)w_block_new(sizeof(WSqlJob), w_sql_job_run);
  memcpy(j->api, api, sizeof j->api);
  j->db = (void *)(intptr_t)db;
  j->text = text->data; j->tlen = text->len;
  j->params = params->data; j->plen = params->len;
  j->mode = (int)mode;
  j->h.stoppable = 1;
  w_block_wait(&j->h);
  return w_block_str(&j->h);
}
/* Opening a database (limen_sql_open): w_sql_open_core (limen_sql.h) walks
   from the granted directory to it and opens it, on the calling thread, as
   the module's sqlite3_open_v2 did. */
W_RT WStr *w_sql_open(const void *const *api, WStr *root, WStr *name, int64_t flags) {
  char *r = (char *)malloc((size_t)root->len + 1), *n = (char *)malloc((size_t)name->len + 1);
  if (!r || !n) { fputs("panic: out of memory\n", stderr); exit(1); }
  memcpy(r, root->data, (size_t)root->len); r[root->len] = 0;
  memcpy(n, name->data, (size_t)name->len); n[name->len] = 0;
  WSqlBuf o = { 0 };
  w_sql_open_core(api, W_FS_TOP, r, n, (size_t)name->len, (int)flags, &o);
  free(r); free(n);
  WStr *s = Str_from(o.p, o.len);
  free(o.p);
  return s;
}
/* The int64 or double at byte `at` of Bytes (the machine's order), and a
   Float's bits: for the sql module's results and parameters. */
W_RT int64_t w_bytes_int(WStr *b, int64_t at, const char *where) {
  if (at < 0 || at > b->len - 8) w_panic_c("limen_bytes_int: no 8 bytes there", where);
  int64_t v;
  memcpy(&v, b->data + at, 8);
  return v;
}
W_RT double w_bytes_float(WStr *b, int64_t at, const char *where) {
  if (at < 0 || at > b->len - 8) w_panic_c("limen_bytes_float: no 8 bytes there", where);
  double v;
  memcpy(&v, b->data + at, 8);
  return v;
}
W_RT int64_t w_float_bits(double x) { int64_t v; memcpy(&v, &x, 8); return v; }
W_RT WAtomic *w_atomic_new(int64_t v) {
  WAtomic *a = (WAtomic *)w_alloc_k(sizeof(WAtomic), WK_PLAIN);
  a->rc = 1;
  __atomic_store_n(&a->v, v, __ATOMIC_SEQ_CST);
  return a;
}
W_RT void Atomic_show(WBuf *b, WAtomic *a, bool top) { (void)a; (void)top; buf_puts(b, "<atomic>"); }
W_RT void w_lock_init(WLock *l) { memset(l, 0, sizeof *l); pthread_mutex_init(&l->mu, NULL); }
W_RT void w_lock_acquire(WLock *l) {
  int zero = 0;
  /* spin briefly: the holder is running and lets go soon */
  for (int i = 0; i < 200; i++) {
    zero = 0;
    if (__atomic_compare_exchange_n(&l->held, &zero, 1, 0, __ATOMIC_ACQUIRE, __ATOMIC_RELAXED)) return;
    w_cpu_relax();
  }
  pthread_mutex_lock(&l->mu);
  __atomic_fetch_add(&l->nwait, 1, __ATOMIC_SEQ_CST);
  zero = 0;
  if (__atomic_compare_exchange_n(&l->held, &zero, 1, 0, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED)) {
    __atomic_fetch_sub(&l->nwait, 1, __ATOMIC_SEQ_CST);
    pthread_mutex_unlock(&l->mu);
    return;
  }
  /* park; the releaser hands the lock over (held stays 1) and wakes us */
  WLockWait me = { w_current(), NULL };
  if (l->tail) l->tail->next = &me; else l->head = &me;
  l->tail = &me;
  w_prepare_park();
  pthread_mutex_unlock(&l->mu);
  w_park_now();
}
W_RT void w_lock_release(WLock *l) {
  __atomic_store_n(&l->held, 0, __ATOMIC_SEQ_CST);
  if (__atomic_load_n(&l->nwait, __ATOMIC_SEQ_CST) == 0) return;
  WFiber *wake = NULL;
  pthread_mutex_lock(&l->mu);
  int zero = 0;
  /* take it back for the first waiter, unless a spinner got it first */
  if (l->head && __atomic_compare_exchange_n(&l->held, &zero, 1, 0, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED)) {
    WLockWait *w = l->head;
    l->head = w->next;
    if (!l->head) l->tail = NULL;
    __atomic_fetch_sub(&l->nwait, 1, __ATOMIC_SEQ_CST);
    wake = w->f;
  }
  pthread_mutex_unlock(&l->mu);
  if (wake) w_wake(wake);
}
/* what a panic that abandons a Shared gives back: its value */
W_RT void w_shared_kids(void *o) {
  WShared *s = (WShared *)o;
  if (s->edrop) s->edrop(s->v);
  pthread_mutex_destroy(&s->lk.mu);
}
W_RT WShared *w_shared_new(size_t esz, void (*edrop)(void *), void (*eshare)(void *)) {
  WShared *s = (WShared *)w_alloc_k(sizeof(WShared) + (esz ? esz : 1), WK_PLAIN);
  s->rc = 1; s->edrop = edrop; s->eshare = eshare;
  w_lock_init(&s->lk);
  w_set_kids(s, w_shared_kids);
  return s;
}
/* through w_release, as a `dyn` value is */
W_RT void Shared_drop(WShared *s) { if (w_rc_dec(&s->rc)) w_release(s); }
W_RT void Shared_show(WBuf *b, WShared *s, bool top) { (void)s; (void)top; buf_puts(b, "<shared>"); }
/* get and set: the lock for a moment. Inside an update's function, any use
   of a Shared panics (it could deadlock). */
W_RT void w_shared_lock(WShared *s, const char *where) {
  WFiber *f = w_current();
  if (f->in_update) w_panic_c("update must not wait: its function uses a Shared value", where);
  w_lock_acquire(&s->lk);
}
/* update: the lock while the function runs; the generated code declares
   the result of w_update_begin with the cleanup w_update_end and clears it
   once the new value is stored, so a panic or a cancellation in the
   function lets go of the lock and leaves the value unchanged. */
W_RT WShared *w_update_begin(WShared *s, const char *where) {
  w_shared_lock(s, where);
  WFiber *f = w_current();
  f->in_update = 1;
  f->upd_where = where;
  return s;
}
W_RT void w_update_end(void *p) {
  WShared *s = *(WShared **)p;
  if (!s) return;
  w_current()->in_update = 0;
  w_shared_unlock(s);
}
/* Callbacks from C (see ffi_callback_def in cgen.ml). C may call one only
   during its foreign call, on the same thread: that is the calling task's
   fiber, which the callback runs on. A thread Limen did not start has none. */
W_RT WFiber *w_cb_fiber(const char *where) {
  WWorker *w = w_self();
  if (!w || !w->current) w_fatal_c("a callback from C ran on a thread Limen did not start (C may call it only during the foreign call, on its thread)", where);
  return w->current;
}
/* A panic in a callback cannot unwind through C's frames: it ends the program. */
__attribute__((noreturn)) W_RT void w_cb_fatal(WCatch *c, const char *where) {
  if (c->cancel || !c->msg) w_fatal_c(W_CB_WAIT, where);
  size_t n = strlen(c->msg) + 32;
  char *m = (char *)w_raw(n);
  snprintf(m, n, "in a callback from C: %s", c->msg);
  w_fatal_c(m, c->where ? c->where : where);
}
W_RT WStr *w_ffi_copy(int64_t p, int64_t n, const char *where) {
  if (n < 0) { char m[128]; snprintf(m, sizeof m, "foreign.copy: negative length %lld", (long long)n); w_panic_c(m, where); }
  if (p == 0 && n > 0) w_panic_c("foreign.copy: null pointer", where);
  return Str_from((const char *)(intptr_t)p, n);
}
WHandleEntry *w_handles;
int64_t w_nhandles, w_caphandles;
pthread_mutex_t w_handles_mu = PTHREAD_MUTEX_INITIALIZER;
W_RT int64_t w_handle_random(void) {
  uint64_t r = 0;
#if defined(__APPLE__)
  arc4random_buf(&r, sizeof r);
#else
  FILE *f = fopen("/dev/urandom", "rb");
  if (!f || fread(&r, sizeof r, 1, f) != 1) { struct timeval tv; gettimeofday(&tv, NULL); r ^= (uint64_t)tv.tv_usec * 0x9E3779B97F4A7C15ull ^ (uint64_t)getpid(); }
  if (f) fclose(f);
#endif
  return (int64_t)(r & 0x3FFFFFFFFFFFFFFFull);
}
W_RT WHandleEntry *w_handle_find(int64_t id) {
  for (int64_t i = 0; i < w_nhandles; i++) if (w_handles[i].id == id) return &w_handles[i];
  return NULL;
}
W_RT void w_handle_remove(WHandleEntry *e) { *e = w_handles[--w_nhandles]; }
W_RT int64_t w_handle_new(int64_t ptr) {
  pthread_mutex_lock(&w_handles_mu);
  int64_t id;
  do id = w_handle_random(); while (id == 0 || w_handle_find(id));
  if (w_nhandles == w_caphandles) {
    w_caphandles = w_caphandles ? w_caphandles * 2 : 8;
    w_handles = (WHandleEntry *)realloc(w_handles, sizeof(WHandleEntry) * (size_t)w_caphandles);
    if (!w_handles) w_oom();
  }
  w_handles[w_nhandles++] = (WHandleEntry){ id, ptr, 0, false };
  pthread_mutex_unlock(&w_handles_mu);
  return id;
}
W_RT int64_t w_handle_acquire(int64_t id) {
  pthread_mutex_lock(&w_handles_mu);
  WHandleEntry *e = w_handle_find(id);
  int64_t p = 0;
  if (e && !e->closed) { e->uses++; p = e->ptr; }
  pthread_mutex_unlock(&w_handles_mu);
  return p;
}
W_RT int64_t w_handle_release(int64_t id) {
  pthread_mutex_lock(&w_handles_mu);
  WHandleEntry *e = w_handle_find(id);
  int64_t p = 0;
  if (e && e->uses > 0) {
    e->uses--;
    if (e->closed && e->uses == 0) { p = e->ptr; w_handle_remove(e); }
  }
  pthread_mutex_unlock(&w_handles_mu);
  return p;
}
W_RT int64_t w_handle_close(int64_t id) {
  pthread_mutex_lock(&w_handles_mu);
  WHandleEntry *e = w_handle_find(id);
  int64_t p = 0;
  if (e && !e->closed) {
    e->closed = true;
    if (e->uses == 0) { p = e->ptr; w_handle_remove(e); }
  }
  pthread_mutex_unlock(&w_handles_mu);
  return p;
}
W_RT WStr *Cap_foreign_copy(WCap *x, int64_t p, int64_t n, const char *where) {
  if (w_revoked(x)) w_panic_c("foreign: revoked", where);
  if (x->nhosts == 0) w_panic_c("foreign: no library is granted, so C memory cannot be read", where);
  if (n < 0) { char m[128]; snprintf(m, sizeof m, "foreign.copy: negative length %lld", (long long)n); w_panic_c(m, where); }
  if (p == 0 && n > 0) w_panic_c("foreign.copy: null pointer", where);
  return Str_from((const char *)(intptr_t)p, n);
}

/* ---------- Exec: run another program ----------
   The allowlist holds program names exactly as granted (`git`, `/usr/bin/env`)
   or `*`, each followed by the argument prefixes it is limited to, if any:
   `git\nstatus\nremote -v` (w_exec_entry) allows `git status ...` and
   `git remote -v ...`, matched word for word from the first argument.
   Arguments go to the program as a list; no shell is involved. The
   child gets /dev/null as stdin, our working directory, and only the
   environment variables granted with `--grant env=` (plus PATH), so Exec
   cannot read past the Env grant. Its
   stdout comes through a pipe the task waits on without blocking a worker;
   its stderr goes to an unlinked temporary file, so a child that fills one
   stream while we wait on the other cannot deadlock. */
W_RT bool exec_prog_is(const char *e, const char *prog) {
  const char *nl = strchr(e, '\n');
  size_t n = nl ? (size_t)(nl - e) : strlen(e);
  return (n == 1 && e[0] == '*') || (strlen(prog) == n && memcmp(e, prog, n) == 0);
}
/* Do the words of prefix [p, p + pn) (separated by single spaces) equal
   the first arguments? An empty prefix allows any. */
W_RT bool exec_prefix_matches(const char *p, size_t pn, char **args, int64_t nargs) {
  int64_t i = 0;
  size_t at = 0;
  while (at < pn) {
    size_t end = at;
    while (end < pn && p[end] != ' ') end++;
    if (i >= nargs || strlen(args[i]) != end - at || memcmp(args[i], p + at, end - at) != 0) return false;
    i++;
    at = end + 1;
  }
  return true;
}
/* Is `prog` granted, with these arguments (nargs < 0: with some)? */
W_RT bool exec_permits(WCap *x, const char *prog, char **args, int64_t nargs) {
  for (int i = 0; i < x->nhosts; i++) {
    const char *e = x->hosts[i];
    if (!exec_prog_is(e, prog)) continue;
    const char *nl = strchr(e, '\n');
    if (!nl || nargs < 0) return true;
    while (nl) {
      const char *p = nl + 1;
      nl = strchr(p, '\n');
      if (exec_prefix_matches(p, nl ? (size_t)(nl - p) : strlen(p), args, nargs)) return true;
    }
  }
  return false;
}
/* Is the prefix `q` (normalized words) covered by what `x` grants `prog`? */
W_RT bool exec_covers(WCap *x, const char *prog, const char *q) {
  for (int i = 0; i < x->nhosts; i++) {
    const char *e = x->hosts[i];
    if (!exec_prog_is(e, prog)) continue;
    const char *nl = strchr(e, '\n');
    if (!nl) return true;
    while (nl) {
      const char *p = nl + 1;
      nl = strchr(p, '\n');
      size_t pn = nl ? (size_t)(nl - p) : strlen(p);
      if (pn == 0 || (strlen(q) >= pn && memcmp(q, p, pn) == 0 && (q[pn] == 0 || q[pn] == ' '))) return true;
    }
  }
  return false;
}
W_RT WCap *exec_with(WCap *x, char *entry) {
  WCap *c = cap_new(CAP_EXEC);
  w_rv_inherit(c, x);
  c->hosts = (char **)w_raw(sizeof(char *));
  if (entry) { c->hosts[0] = entry; c->nhosts = 1; }
  return c;
}
W_RT WCap *Cap_exec_only(WCap *x, WStr *prog_s) {
  char *prog = w_strndup(prog_s->data, (size_t)prog_s->len);
  if ((size_t)prog_s->len != strlen(prog) || strchr(prog, '\n')) { free(prog); return exec_with(x, NULL); }
  if (strcmp(prog, "*") == 0) {
    for (int i = 0; i < x->nhosts; i++) if (strcmp(x->hosts[i], "*") == 0) return exec_with(x, prog);
    free(prog);
    return exec_with(x, NULL);
  }
  /* the program with every prefix granted for it, or with none if one
     entry allows any arguments */
  WBuf b; buf_init(&b);
  bool found = false;
  for (int i = 0; i < x->nhosts; i++) {
    const char *e = x->hosts[i];
    if (!exec_prog_is(e, prog)) continue;
    const char *nl = strchr(e, '\n');
    if (!nl) { free(b.p); return exec_with(x, prog); }
    found = true;
    buf_puts(&b, nl);
  }
  if (!found) { free(b.p); free(prog); return exec_with(x, NULL); }
  WStr *rest = buf_take(&b);
  char *entry = (char *)w_raw(strlen(prog) + (size_t)rest->len + 1);
  memcpy(entry, prog, strlen(prog));
  memcpy(entry + strlen(prog), rest->data, (size_t)rest->len);
  entry[strlen(prog) + (size_t)rest->len] = 0;
  Str_drop(rest);
  free(prog);
  return exec_with(x, entry);
}
/* `exec.only_args(program, prefixes)`: the prefixes `x` covers for it */
W_RT WCap *Cap_exec_only_args(WCap *x, WStr *prog_s, L_Str *qs) {
  char *prog = w_strndup(prog_s->data, (size_t)prog_s->len);
  if ((size_t)prog_s->len != strlen(prog) || strchr(prog, '\n') || strcmp(prog, "*") == 0) { free(prog); return exec_with(x, NULL); }
  WBuf b; buf_init(&b);
  buf_puts(&b, prog);
  bool any = false;
  for (int64_t i = 0; i < qs->len; i++) {
    WStr *q = qs->data[i];
    if (memchr(q->data, 0, (size_t)q->len) || memchr(q->data, '\n', (size_t)q->len)) continue;
    /* its words, separated by single spaces */
    WBuf w; buf_init(&w);
    int64_t at = 0;
    while (at < q->len) {
      while (at < q->len && q->data[at] == ' ') at++;
      int64_t end = at;
      while (end < q->len && q->data[end] != ' ') end++;
      if (end > at) { if (w.len > 0) buf_puts(&w, " "); buf_putn(&w, q->data + at, end - at); }
      at = end;
    }
    WStr *ws = buf_take(&w);
    char *norm = w_strndup(ws->data, (size_t)ws->len);
    Str_drop(ws);
    if (exec_covers(x, prog, norm)) { buf_puts(&b, "\n"); buf_puts(&b, norm); any = true; }
    free(norm);
  }
  WStr *e = buf_take(&b);
  char *entry = any ? w_strndup(e->data, (size_t)e->len) : NULL;
  Str_drop(e);
  free(prog);
  return exec_with(x, entry);
}
W_RT WIo Cap_exec_run(WCap *x, WStr *prog_s, L_Str *args) {
  if (w_revoked(x)) return io_err(1, "exec: revoked");
  if (memchr(prog_s->data, 0, (size_t)prog_s->len)) return io_err(2, "exec: the program name contains a NUL byte");
  for (int64_t i = 0; i < args->len; i++)
    if (memchr(args->data[i]->data, 0, (size_t)args->data[i]->len)) return io_err(2, "exec: an argument contains a NUL byte");
  char *prog = w_strndup(prog_s->data, (size_t)prog_s->len);
  char **argv = (char **)w_raw(sizeof(char *) * (size_t)(args->len + 2));
  argv[0] = prog;
  for (int64_t i = 0; i < args->len; i++) argv[i + 1] = w_strndup(args->data[i]->data, (size_t)args->data[i]->len);
  argv[args->len + 1] = NULL;
  if (!exec_permits(x, prog, argv + 1, args->len)) {
    WBuf b; buf_init(&b);
    buf_puts(&b, "exec: `"); buf_puts(&b, prog);
    buf_puts(&b, exec_permits(x, prog, NULL, -1) ? "` is not granted with these arguments" : "` is not granted");
    for (int64_t i = 0; i <= args->len; i++) free(argv[i]);
    free(argv);
    WIo r = { 1, NULL, buf_take(&b), NULL };
    return r;
  }
  const char *tmp = getenv("TMPDIR");
  char tpl[1024];
  snprintf(tpl, sizeof tpl, "%s/limen-exec-XXXXXX", tmp && tmp[0] ? tmp : "/tmp");
  /* Descriptors are close-on-exec from birth where the system allows, so a
     program spawned at the same moment by another worker cannot inherit our
     pipe and hold it open; on macOS the spawn closes everything else anyway. */
#ifdef __APPLE__
  int errfd = mkstemp(tpl);
#else
  int errfd = mkostemp(tpl, O_CLOEXEC);
#endif
  if (errfd >= 0) unlink(tpl);
  int out[2] = { -1, -1 };
  char **envp = NULL; int nenvp = 0;
  WIo res;
#ifdef __APPLE__
  if (errfd < 0 || pipe(out) != 0) { res = w_errno_io(); goto done; }
  fcntl(errfd, F_SETFD, FD_CLOEXEC);
  fcntl(out[0], F_SETFD, FD_CLOEXEC);
  fcntl(out[1], F_SETFD, FD_CLOEXEC);
#else
  if (errfd < 0 || pipe2(out, O_CLOEXEC) != 0) { res = w_errno_io(); goto done; }
#endif
  /* the child's environment: PATH and the variables the Env grant names */
  for (char **e = environ; *e; e++) nenvp++;
  envp = (char **)w_raw(sizeof(char *) * (size_t)(nenvp + 1));
  nenvp = 0;
  for (char **e = environ; *e; e++) {
    const char *eq = strchr(*e, '=');
    if (!eq) continue;
    size_t kl = (size_t)(eq - *e);
    bool keep = kl == 4 && strncmp(*e, "PATH", 4) == 0;
    for (int i = 0; i < g_nenv && !keep; i++)
      keep = strcmp(g_env[i], "*") == 0 || (strlen(g_env[i]) == kl && strncmp(g_env[i], *e, kl) == 0);
    if (keep) envp[nenvp++] = *e;
  }
  envp[nenvp] = NULL;
  posix_spawn_file_actions_t fa;
  posix_spawn_file_actions_init(&fa);
  posix_spawn_file_actions_addopen(&fa, 0, "/dev/null", O_RDONLY, 0);
  posix_spawn_file_actions_adddup2(&fa, out[1], 1);
  posix_spawn_file_actions_adddup2(&fa, errfd, 2);
  posix_spawnattr_t sa;
  posix_spawnattr_init(&sa);
#ifdef POSIX_SPAWN_CLOEXEC_DEFAULT
  posix_spawnattr_setflags(&sa, POSIX_SPAWN_CLOEXEC_DEFAULT);
#endif
  pid_t pid;
  int rc = posix_spawnp(&pid, prog, &fa, &sa, argv, envp);
  posix_spawnattr_destroy(&sa);
  posix_spawn_file_actions_destroy(&fa);
  close(out[1]); out[1] = -1;
  if (rc != 0) {
    WBuf b; buf_init(&b);
    buf_puts(&b, "exec: cannot run `"); buf_puts(&b, prog); buf_puts(&b, "`: "); buf_puts(&b, strerror(rc));
    res = (WIo){ 2, NULL, buf_take(&b), NULL };
    goto done;
  }
  {
    WBuf ob; buf_init(&ob);
    char *chunk = (char *)w_raw(65536);   /* on the heap: task stacks are small */
    fcntl(out[0], F_SETFL, fcntl(out[0], F_GETFL) | O_NONBLOCK);
    for (;;) {
      ssize_t n = read(out[0], chunk, 65536);
      if (n > 0) { buf_putn(&ob, chunk, n); continue; }
      if (n == 0) break;
      if (errno == EINTR) continue;
      if (errno == EAGAIN || errno == EWOULDBLOCK) {
        if (w_wait_fd(out[0], 0) < 0) fcntl(out[0], F_SETFL, fcntl(out[0], F_GETFL) & ~O_NONBLOCK);
        continue;
      }
      break;
    }
    free(chunk);
    w_sock_release_fd(out[0]); out[0] = -1;
    int status = 0;
    while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {}
    lseek(errfd, 0, SEEK_SET);
    WStr *e = read_fd(errfd);
    WProc *p = (WProc *)w_raw(sizeof(WProc));
    p->code = WIFEXITED(status) ? WEXITSTATUS(status) : WIFSIGNALED(status) ? 128 + WTERMSIG(status) : 1;
    p->out = buf_take(&ob);
    p->err = e;
    res = io_ok_ptr(p);
  }
done:
  if (out[0] >= 0) close(out[0]);
  if (out[1] >= 0) close(out[1]);
  if (errfd >= 0) close(errfd);
  free(envp);
  for (int64_t i = 0; i <= args->len; i++) free(argv[i]);
  free(argv);
  return res;
}

W_RT int64_t w_find_bytes(const char *d, int64_t n, const char *pat, int64_t from) {
  int64_t m = (int64_t)strlen(pat);
  for (int64_t i = from; i + m <= n; i++) if (memcmp(d + i, pat, (size_t)m) == 0) return i;
  return -1;
}
W_RT void w_trim_sp(const char *d, int64_t *a, int64_t *b) {
  while (*a < *b && (d[*a] == ' ' || d[*a] == '\t')) (*a)++;
  while (*b > *a && (d[*b - 1] == ' ' || d[*b - 1] == '\t')) (*b)--;
}
W_RT void w_http_free(WHttp *h) {
  for (int i = 0; i < h->n; i++) { Str_drop(h->names[i]); Str_drop(h->values[i]); }
  free(h->names); free(h->values);
  if (h->body) Str_drop(h->body);
  free(h);
}
W_RT WIo w_http_fail(WHttp *h, const char *m) { w_http_free(h); return io_err(2, m); }

/* cut: the TLS stream ended without a close message (this is its error
   text), so a body that runs until the close may be incomplete and is
   refused; one framed by Content-Length or chunks is checked as usual. */
W_RT WIo w_http_parse(const char *d, int64_t n, const char *cut) {
  /* bounded, so a hostile server cannot make the header merging quadratic */
  int64_t e = w_find_bytes(d, n < 65536 + 4 ? n : 65536 + 4, "\r\n\r\n", 0);
  if (e > 65536 || (e < 0 && n > 65536)) return io_err(2, "http: response headers larger than 64 KiB");
  if (e < 0) return io_err(2, "http: incomplete response headers");
  int64_t lines = 1;
  for (int64_t k = 0; k < e; k++) if (d[k] == '\n') lines++;
  if (lines > 1000 + 1) return io_err(2, "http: more than 1000 response headers");
  WHttp *h = (WHttp *)w_raw(sizeof(WHttp));
  memset(h, 0, sizeof *h);
  /* lines of the head, split on \n with a trailing \r removed */
  int64_t pos = 0; bool first = true;
  while (pos <= e) {
    int64_t nl = w_find_bytes(d, e, "\n", pos);
    int64_t end = nl < 0 ? e : nl;
    int64_t le = end;
    if (le > pos && d[le - 1] == '\r') le--;
    const char *l = d + pos; int64_t ll = le - pos;
    if (first) {
      bool ok = ll >= 12 && memcmp(l, "HTTP/1.", 7) == 0 && l[8] == ' ' && (ll == 12 || l[12] == ' ');
      for (int k = 9; ok && k < 12; k++) ok = l[k] >= '0' && l[k] <= '9';
      if (!ok) return w_http_fail(h, "http: malformed status line");
      h->status = (l[9] - '0') * 100 + (l[10] - '0') * 10 + (l[11] - '0');
      first = false;
    } else {
      const char *colon = memchr(l, ':', (size_t)ll);
      if (!colon) return w_http_fail(h, "http: malformed header line");
      int64_t na = 0, nb = colon - l, va = nb + 1, vb = ll;
      w_trim_sp(l, &na, &nb); w_trim_sp(l, &va, &vb);
      WStr *name = Str_from(l + na, nb - na);
      for (int64_t k = 0; k < name->len; k++) if (name->data[k] >= 'A' && name->data[k] <= 'Z') name->data[k] = (char)(name->data[k] + 32);
      int found = -1;
      for (int k = 0; k < h->n; k++) if (h->names[k]->len == name->len && memcmp(h->names[k]->data, name->data, (size_t)name->len) == 0) found = k;
      if (found >= 0) {
        WBuf b; buf_init(&b);
        buf_putn(&b, h->values[found]->data, h->values[found]->len);
        buf_puts(&b, ", ");
        buf_putn(&b, l + va, vb - va);
        Str_drop(h->values[found]);
        h->values[found] = buf_take(&b);
        Str_drop(name);
      } else {
        h->names = (WStr **)w_realloc(h->names, sizeof(WStr *) * (size_t)(h->n + 1));
        h->values = (WStr **)w_realloc(h->values, sizeof(WStr *) * (size_t)(h->n + 1));
        h->names[h->n] = name;
        h->values[h->n] = Str_from(l + va, vb - va);
        h->n++;
      }
    }
    if (nl < 0) break;
    pos = nl + 1;
  }
  const char *raw = d + e + 4; int64_t rn = n - e - 4;
  WStr *te = NULL, *cl = NULL;
  for (int k = 0; k < h->n; k++) {
    if (h->names[k]->len == 17 && memcmp(h->names[k]->data, "transfer-encoding", 17) == 0) te = h->values[k];
    if (h->names[k]->len == 14 && memcmp(h->names[k]->data, "content-length", 14) == 0) cl = h->values[k];
  }
  bool chunked = false;
  if (te) {
    char *low = w_strndup(te->data, (size_t)te->len);
    lower_ascii(low);
    chunked = w_find_bytes(low, (int64_t)strlen(low), "chunked", 0) >= 0;
    free(low);
  }
  if (chunked) {
    WBuf b; buf_init(&b);
    int64_t p = 0;
    for (;;) {
      int64_t eol = w_find_bytes(raw, rn, "\r\n", p);
      if (eol < 0) { free(b.p); return w_http_fail(h, "http: truncated chunked body"); }
      int64_t sa = p, sb = eol;
      const char *semi = memchr(raw + p, ';', (size_t)(eol - p));
      if (semi) sb = semi - raw;
      w_trim_sp(raw, &sa, &sb);
      int64_t size = 0; bool ok = sb > sa && sb - sa <= 15;
      for (int64_t k = sa; ok && k < sb; k++) {
        char c = raw[k];
        int v = c >= '0' && c <= '9' ? c - '0' : c >= 'a' && c <= 'f' ? c - 87 : c >= 'A' && c <= 'F' ? c - 55 : -1;
        if (v < 0) ok = false; else size = size * 16 + v;
      }
      if (!ok) { free(b.p); return w_http_fail(h, "http: bad chunk size"); }
      if (size == 0) break;
      int64_t start = eol + 2;
      if (start + size > rn) { free(b.p); return w_http_fail(h, "http: truncated chunked body"); }
      buf_putn(&b, raw + start, size);
      p = start + size + 2;
    }
    h->body = buf_take(&b);
  } else if (cl) {
    int64_t a = 0, b = cl->len;
    w_trim_sp(cl->data, &a, &b);
    bool ok = b > a && b - a <= 15;
    int64_t len = 0;
    for (int64_t k = a; ok && k < b; k++) { if (cl->data[k] < '0' || cl->data[k] > '9') ok = false; else len = len * 10 + (cl->data[k] - '0'); }
    if (!ok) return w_http_fail(h, "http: bad Content-Length");
    if (rn < len) return w_http_fail(h, "http: truncated body");
    h->body = Str_from(raw, len);
  } else if (cut) return w_http_fail(h, cut);
  else h->body = Str_from(raw, rn);
  return io_ok_ptr(h);
}

W_RT WIo Cap_net_get(WCap *n, WStr *url_s) {
  if (w_revoked(n)) return io_err(1, "net: revoked");
  if (memchr(url_s->data, 0, (size_t)url_s->len)) return io_err(2, "URL contains spaces or non-ASCII characters");
  char *url = w_strndup(url_s->data, (size_t)url_s->len);
  char host[512], err[256]; int port, https; const char *path;
  if (!parse_url(url, host, sizeof host, &port, &https, &path, err, sizeof err)) { free(url); return io_err(2, err); }
  if (!net_permits(n, host, port)) {
    char m[700]; snprintf(m, sizeof m, "net: %s:%d is not granted", host, port);
    free(url); return io_err(1, m);
  }
  /* the request: the path without any #fragment, `/` if empty, `/?q` for a bare query */
  WBuf req; buf_init(&req);
  buf_puts(&req, "GET ");
  size_t pl = strcspn(path, "#");
  if (pl == 0 || path[0] == '?') buf_puts(&req, "/");
  buf_putn(&req, path, (int64_t)pl);
  buf_puts(&req, " HTTP/1.1\r\nHost: ");
  buf_puts(&req, host);
  if (port != (https ? 443 : 80)) { char pb[16]; snprintf(pb, sizeof pb, ":%d", port); buf_puts(&req, pb); }
  buf_puts(&req, "\r\nUser-Agent: limen\r\nAccept: */*\r\nConnection: close\r\n\r\n");
  WStr *reqs = buf_take(&req);
  char addr[600]; snprintf(addr, sizeof addr, "%s:%d", host, port);
  free(url);
  WStr *addrs = Str_from(addr, (int64_t)strlen(addr));
  WIo c = https ? Cap_net_connect_tls(n, addrs) : Cap_net_connect(n, addrs);
  Str_drop(addrs);
  if (c.kind != 0) { Str_drop(reqs); return c; }
  WCap *conn = (WCap *)c.ptr;
  WIo r = Cap_conn_write(conn, reqs);
  Str_drop(reqs);
  if (r.kind == 0) {
    WBuf b; buf_init(&b);
    for (;;) {
      WIo k = Cap_conn_read(conn);
#ifdef LIMEN_TLS
      if (k.kind != 0 && https && k.msg->len == (int64_t)strlen(VTLS_CUT_MSG) && memcmp(k.msg->data, VTLS_CUT_MSG, strlen(VTLS_CUT_MSG)) == 0) {
        r = w_http_parse(b.p, b.len, VTLS_CUT_MSG);
        Str_drop(k.msg);
        break;
      }
#endif
      if (k.kind != 0) { r = k; break; }
      int64_t got = k.val->len;
      buf_putn(&b, k.val->data, got);
      Str_drop(k.val);
      if (got == 0) { r = w_http_parse(b.p, b.len, NULL); break; }
      if (b.len > W_MAX_RESPONSE) { r = io_err(2, "net: the response is larger than 64 MiB"); break; }
    }
    free(b.p);
  }
  Cap_sock_close(conn);
  Cap_drop(conn);
  return r;
}
