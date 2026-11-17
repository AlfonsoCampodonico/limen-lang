/* One SQL statement run as a single blocking call, for the std sql module.
   Shared by the native runtime (w_sql_run in limen_rt.c, on the
   blocking-call pool) and the interpreter (ffi_stubs.c, on a thread of its
   own).

   A call into SQLite that can take long must not run on a worker thread,
   but handing each call to another thread costs a round trip (about half a
   microsecond), which made a query cost one per row. So the module hands
   over the whole statement instead: w_sql_core prepares it, binds the
   parameters, steps through every row, copies the columns into a buffer and
   finalizes it, and the module turns the buffer into rows. A statement is
   then one round trip whatever it returns.

   SQLite's functions come in a table (w_sql_api), filled by the caller from
   the symbols the program links (natively) or the interpreter loaded, so
   this code needs no sqlite3.h and no link of its own.

   Stopping: the caller sets *stop when the task that runs the statement is
   cancelled. w_sql_core looks at it before each step, and a progress handler
   on the connection looks at it every W_SQL_PROGRESS virtual machine
   instructions while a step runs, making SQLite abandon the statement as
   sqlite3_interrupt would (SQLITE_INTERRUPT). Unlike sqlite3_interrupt,
   which stops whatever the connection is running, the handler finds the
   stop flag of the job running on its own thread (w_sql_stop), so on a
   connection shared by tasks it stops only the cancelled task's statement,
   and it cannot miss a statement that is just starting. A busy handler
   (w_sql_busy) does the same for a wait on another connection's lock. Both
   are installed on the connection by every job.

   The parameters come encoded by the module: the count, `;`, then each one:
   `n` (NULL), `i<decimal>;`, `f<decimal bits of the double>;`, `t<len>;<bytes>`
   or `b<len>;<bytes>`.

   The result, in the machine's byte order (it never leaves the process):
     what, rc, n, changes   four int64: what went wrong (W_SQL_*), SQLite's
                            result code, a number for the message (the
                            parameter count wanted, or the parameter that
                            failed to bind), the rows changed
   then, if what is W_SQL_OK:
     ncols, each name as int64 length + bytes, then each row's columns:
     a type byte (0 NULL, 1 integer, 2 float, 3 text, 4 blob), then an
     int64 or a double, or an int64 length and the bytes
   otherwise:
     SQLite's message for the connection, as int64 length + bytes. */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

enum {
  W_SQL_PREPARE, W_SQL_STEP, W_SQL_FINALIZE, W_SQL_PARAM_COUNT, W_SQL_BIND_NULL, W_SQL_BIND_INT64, W_SQL_BIND_DOUBLE,
  W_SQL_BIND_TEXT, W_SQL_BIND_BLOB, W_SQL_COLUMN_COUNT, W_SQL_COLUMN_NAME, W_SQL_COLUMN_TYPE, W_SQL_COLUMN_INT64,
  W_SQL_COLUMN_DOUBLE, W_SQL_COLUMN_TEXT, W_SQL_COLUMN_BLOB, W_SQL_COLUMN_BYTES, W_SQL_ERRMSG, W_SQL_CHANGES64,
  W_SQL_PROGRESS_HANDLER, W_SQL_BUSY_HANDLER, W_SQL_NAPI
};
/* the symbols, in the order of the enum (cgen.ml and interp.ml fill the table from Ffi.sql_syms) */

/* what: */
enum { W_SQL_OK, W_SQL_EPREPARE, W_SQL_ENONE, W_SQL_EALONE, W_SQL_ECOUNT, W_SQL_EBIND, W_SQL_ESTEP, W_SQL_ESTOPPED };
/* mode: */
enum { W_SQL_ALL, W_SQL_ONCE, W_SQL_ROWS };   /* every statement of the text, rows ignored; one statement, one step; one statement, every row */

#define W_SQL_PROGRESS 1000

typedef struct { char *p; int64_t len, cap; } WSqlBuf;

static void w_sqlbuf_put(WSqlBuf *b, const void *d, int64_t n) {
  if (b->len + n > b->cap) {
    int64_t cap = b->cap ? b->cap : 256;
    while (cap < b->len + n) cap *= 2;
    char *p = (char *)realloc(b->p, (size_t)cap);
    if (!p) { fputs("panic: out of memory\n", stderr); exit(1); }
    b->p = p; b->cap = cap;
  }
  if (n > 0) memcpy(b->p + b->len, d, (size_t)n);
  b->len += n;
}
static void w_sqlbuf_int(WSqlBuf *b, int64_t v) { w_sqlbuf_put(b, &v, 8); }
static void w_sqlbuf_bytes(WSqlBuf *b, const void *d, int64_t n) { w_sqlbuf_int(b, n); w_sqlbuf_put(b, d, n); }
static void w_sqlbuf_set(WSqlBuf *b, int64_t at, int64_t v) { memcpy(b->p + at, &v, 8); }

static __thread const int *w_sql_stop;
static int w_sql_progress(void *arg) {
  (void)arg;
  return w_sql_stop && __atomic_load_n(w_sql_stop, __ATOMIC_ACQUIRE);
}

/* Waiting for another connection's lock: up to W_SQL_BUSY_MS, a
   millisecond at a time, and not at all once the statement is told to stop
   (SQLite's own busy timeout would sleep through that). */
#define W_SQL_BUSY_MS 5000
static int w_sql_busy(void *arg, int n) {
  (void)arg;
  if (n >= W_SQL_BUSY_MS || w_sql_progress(NULL)) return 0;
  usleep(1000);
  return !w_sql_progress(NULL);
}

/* Parse a decimal number of the parameter encoding, ending at `;`. */
static int w_sql_num(const char **p, const char *end, int64_t *v) {
  const char *s = *p;
  int neg = 0;
  uint64_t n = 0;
  if (s < end && *s == '-') { neg = 1; s++; }
  if (s >= end || *s < '0' || *s > '9') return 0;
  while (s < end && *s >= '0' && *s <= '9') n = n * 10 + (uint64_t)(*s++ - '0');
  if (s >= end || *s != ';') return 0;
  *p = s + 1;
  *v = neg ? (int64_t)(0 - n) : (int64_t)n;
  return 1;
}

static int w_sql_ws(char c) { return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'; }

/* Run the text on db as [mode] says, into [out] (empty). */
static void w_sql_core(void *const *api, void *db, const char *text, int64_t tlen, const char *params, int64_t plen, int mode,
                       const int *stop, WSqlBuf *out) {
  typedef int (*prepare_f)(void *, const char *, int, void **, const char **);
  typedef int (*st_f)(void *);
  typedef int (*st_i_f)(void *, int);
  typedef const char *(*db_s_f)(void *);
  int (*step)(void *) = (st_f)api[W_SQL_STEP];
  int (*finalize)(void *) = (st_f)api[W_SQL_FINALIZE];
  int (*column_type)(void *, int) = (st_i_f)api[W_SQL_COLUMN_TYPE];
  int64_t (*column_int64)(void *, int) = (int64_t (*)(void *, int))api[W_SQL_COLUMN_INT64];
  double (*column_double)(void *, int) = (double (*)(void *, int))api[W_SQL_COLUMN_DOUBLE];
  const void *(*column_text)(void *, int) = (const void *(*)(void *, int))api[W_SQL_COLUMN_TEXT];
  const void *(*column_blob)(void *, int) = (const void *(*)(void *, int))api[W_SQL_COLUMN_BLOB];
  int (*column_bytes)(void *, int) = (st_i_f)api[W_SQL_COLUMN_BYTES];
  const char *(*errmsg)(void *) = (db_s_f)api[W_SQL_ERRMSG];
  int what = W_SQL_OK, rc = 0;
  int64_t num = 0;
  void *st = NULL;
  for (int k = 0; k < 4; k++) w_sqlbuf_int(out, 0);
  const int *outer = w_sql_stop;
  w_sql_stop = stop;
  if (__atomic_load_n(stop, __ATOMIC_ACQUIRE)) { what = W_SQL_ESTOPPED; goto done; }
  ((void (*)(void *, int, int (*)(void *), void *))api[W_SQL_PROGRESS_HANDLER])(db, W_SQL_PROGRESS, w_sql_progress, NULL);
  ((int (*)(void *, int (*)(void *, int), void *))api[W_SQL_BUSY_HANDLER])(db, w_sql_busy, NULL);
  const char *rest = text, *end = text + tlen;
  for (;;) {
    const char *tail = NULL;
    st = NULL;
    rc = ((prepare_f)api[W_SQL_PREPARE])(db, rest, (int)(end - rest), &st, &tail);
    if (rc != 0) { what = W_SQL_EPREPARE; goto done; }
    if (!tail || tail < rest || tail > end) tail = end;
    if (mode == W_SQL_ALL) {
      if (!st) {
        if (tail == rest) break;   /* nothing more */
        rest = tail;
        continue;
      }
    } else {
      if (!st) { what = W_SQL_ENONE; goto done; }
      /* exactly one statement: anything after it but blanks and one `;` is a second one */
      const char *a = tail, *b = end;
      while (a < b && w_sql_ws(*a)) a++;
      while (b > a && w_sql_ws(b[-1])) b--;
      if (!(a == b || (b - a == 1 && *a == ';'))) { what = W_SQL_EALONE; goto done; }
      const char *p = params, *pend = params + plen;
      int64_t given = 0;
      if (plen > 0 && !w_sql_num(&p, pend, &given)) given = -1;
      int want = ((st_f)api[W_SQL_PARAM_COUNT])(st);
      if (given != want) { what = W_SQL_ECOUNT; num = want; goto done; }
      for (int i = 1; i <= want; i++) {
        int64_t v = 0;
        char tag = p < pend ? *p++ : 0;
        int ok = tag == 'n' || w_sql_num(&p, pend, &v);
        if (ok && (tag == 't' || tag == 'b') && (v < 0 || v > pend - p || v > 0x7fffffff)) ok = 0;
        if (!ok) { rc = 21; what = W_SQL_EBIND; num = i; goto done; }   /* SQLITE_MISUSE: the encoding is wrong */
        switch (tag) {
        case 'n': rc = ((st_i_f)api[W_SQL_BIND_NULL])(st, i); break;
        case 'i': rc = ((int (*)(void *, int, int64_t))api[W_SQL_BIND_INT64])(st, i, v); break;
        case 'f': { double d; memcpy(&d, &v, 8); rc = ((int (*)(void *, int, double))api[W_SQL_BIND_DOUBLE])(st, i, d); break; }
        case 't':
          rc = ((int (*)(void *, int, const char *, int, void (*)(void *)))api[W_SQL_BIND_TEXT])(st, i, p, (int)v, (void (*)(void *))(intptr_t)-1);
          p += v;
          break;
        case 'b':
          rc = ((int (*)(void *, int, const void *, int, void (*)(void *)))api[W_SQL_BIND_BLOB])(st, i, p, (int)v, (void (*)(void *))(intptr_t)-1);
          p += v;
          break;
        default: rc = 21; break;
        }
        if (rc != 0) { what = W_SQL_EBIND; num = i; goto done; }
      }
    }
    if (mode == W_SQL_ROWS) {
      int n = ((st_f)api[W_SQL_COLUMN_COUNT])(st);
      w_sqlbuf_int(out, n);
      for (int i = 0; i < n; i++) {
        const char *name = ((const char *(*)(void *, int))api[W_SQL_COLUMN_NAME])(st, i);
        w_sqlbuf_bytes(out, name ? name : "", name ? (int64_t)strlen(name) : 0);
      }
      while (!__atomic_load_n(stop, __ATOMIC_ACQUIRE) && (rc = step(st)) == 100) {
        for (int i = 0; i < n; i++) {
          unsigned char t = (unsigned char)column_type(st, i);
          switch (t) {
          case 1: { w_sqlbuf_put(out, &t, 1); w_sqlbuf_int(out, column_int64(st, i)); break; }
          case 2: { double d = column_double(st, i); w_sqlbuf_put(out, &t, 1); w_sqlbuf_put(out, &d, 8); break; }
          case 3: case 4: {
            const void *p = t == 3 ? column_text(st, i) : column_blob(st, i);
            int64_t len = column_bytes(st, i);
            w_sqlbuf_put(out, &t, 1);
            w_sqlbuf_bytes(out, p, p ? len : 0);
            break;
          }
          default: t = 0; w_sqlbuf_put(out, &t, 1); break;
          }
        }
      }
      if (__atomic_load_n(stop, __ATOMIC_ACQUIRE)) { what = W_SQL_ESTOPPED; goto done; }
      if (rc != 101) { what = W_SQL_ESTEP; goto done; }
      break;
    }
    if (mode == W_SQL_ONCE) {
      if (!__atomic_load_n(stop, __ATOMIC_ACQUIRE)) rc = step(st);
      if (__atomic_load_n(stop, __ATOMIC_ACQUIRE)) { what = W_SQL_ESTOPPED; goto done; }
      if (rc != 100 && rc != 101) { what = W_SQL_ESTEP; goto done; }
      break;
    }
    /* W_SQL_ALL: run the statement to its end, then the next one */
    while (!__atomic_load_n(stop, __ATOMIC_ACQUIRE) && (rc = step(st)) == 100) {}
    if (__atomic_load_n(stop, __ATOMIC_ACQUIRE)) { what = W_SQL_ESTOPPED; goto done; }
    if (rc != 101) { what = W_SQL_ESTEP; goto done; }
    finalize(st);
    st = NULL;
    rest = tail;
  }
done:
  if (what != W_SQL_OK) {
    /* the message before finalize, which would leave its own */
    const char *m = errmsg(db);
    out->len = 32;
    w_sqlbuf_bytes(out, m ? m : "", m ? (int64_t)strlen(m) : 0);
  }
  if (st) finalize(st);
  w_sqlbuf_set(out, 0, what);
  w_sqlbuf_set(out, 8, rc);
  w_sqlbuf_set(out, 16, num);
  if (what == W_SQL_OK) w_sqlbuf_set(out, 24, ((int64_t (*)(void *))api[W_SQL_CHANGES64])(db));
  w_sql_stop = outer;
}
