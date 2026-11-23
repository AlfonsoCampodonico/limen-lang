/* A small C library for the foreign-function tests: structs by value in
   the shapes each calling convention treats differently, and callbacks.
   The test suite builds it as liblimentest (a shared library). */
#include <stdint.h>
#include <string.h>

typedef struct { int32_t a, b; } P2;               /* 8 bytes: one integer register */
typedef struct { int32_t a, b, c; } T3;            /* 12 bytes: two integer registers */
typedef struct { double x, y; } V2;                /* two doubles: SSE, SSE / an FP aggregate */
typedef struct { double x, y, z; } V3;             /* memory on x86-64, an FP aggregate on arm64 */
typedef struct { double a, b, c, d; } V4;
typedef struct { int64_t n; double x; } NX;        /* INTEGER then SSE on x86-64 */
typedef struct { double x; int32_t k; } XK;        /* SSE then INTEGER on x86-64 */
typedef struct { int32_t a; int64_t b; int32_t on; void *h; double x; } Big; /* 32 bytes: memory, or by pointer */

P2 vt_p2_swap(P2 p) { P2 r = { p.b, p.a }; return r; }
T3 vt_t3_rot(T3 t) { T3 r = { t.b, t.c, t.a }; return r; }
V2 vt_v2_add(V2 a, V2 b) { V2 r = { a.x + b.x, a.y + b.y }; return r; }
V3 vt_v3_scale(V3 v, double k) { V3 r = { v.x * k, v.y * k, v.z * k }; return r; }
V4 vt_v4(double x) { V4 r = { x, x * 2, x * 3, x * 4 }; return r; }
NX vt_nx(NX a, int64_t m) { NX r = { a.n * m, a.x * (double)m }; return r; }
XK vt_xk(XK a) { XK r = { a.x + 1, a.k * 2 }; return r; }
Big vt_big(Big b, int32_t d) {
  Big r = { b.a + d, b.b * d, !b.on, (char *)b.h + d, b.x / d };
  return r;
}

/* x86-64: the struct no longer fits in registers and goes on the stack */
int64_t vt_tight(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, T3 t) {
  return a + b * 10 + c * 100 + d * 1000 + e * 10000 + t.a * 100000 + t.b * 1000000 + (int64_t)t.c * 10000000;
}
/* the stack on every ABI: arm64 runs out of integer registers for NX */
double vt_spill(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f, int64_t g, NX n, double z) {
  return (double)(a + b + c + d + e + f + g) + (double)n.n * 100 + n.x * 1000 + z * 10000;
}
/* two ints on the stack: 4 bytes apart on Apple arm64, 8 elsewhere */
int64_t vt_pack(int64_t a, int64_t b, int64_t c, int64_t d, XK p, XK q, int32_t h1, int32_t h2) {
  return a + b + c + d + p.k + q.k + (int64_t)h1 * 1000 + (int64_t)h2 * 100000 + (int64_t)(p.x + q.x) * 7;
}
int32_t vt_bool(int32_t on) { return on ? 7 : 3; }

/* callbacks */
double vt_apply(double (*f)(double, int64_t), double x, int64_t n) { return f(x, n) + f(x + 1, n); }
int64_t vt_each(int64_t (*f)(const char *, int64_t), int64_t n) {
  static const char *names[] = { "zero", "one", "two", "three" };
  int64_t s = 0;
  for (int64_t i = 0; i < n; i++) s += f(names[i % 4], i);
  return s;
}
int64_t vt_call8(int64_t (*f)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)) {
  return f(1, 2, 3, 4, 5, 6, 7, 8);
}
int64_t vt_mixed(int64_t (*f)(int32_t, double, int32_t, double, void *)) { return f(-3, 0.5, 4, 0.25, (void *)8); }
static int64_t vt_total;
void vt_note(int64_t n) { vt_total += n; }
int64_t vt_noted(void) { int64_t t = vt_total; vt_total = 0; return t; }
void vt_times(void (*f)(int64_t), int64_t n) { for (int64_t i = 1; i <= n; i++) f(i); }

/* blocking calls: slow on purpose, and reading or writing what they were
   lent only after the wait, when a cancelled task has long unwound */
#include <unistd.h>
int64_t vt_slow_len(const char *s, int64_t ms) { usleep((useconds_t)(ms * 1000)); return (int64_t)strlen(s); }
int64_t vt_slow_out(int64_t ms, int64_t *out) { usleep((useconds_t)(ms * 1000)); *out = ms * 2; return ms; }
const char *vt_slow_name(int64_t ms) { usleep((useconds_t)(ms * 1000)); return "slow"; }

/* For the sql tests: whether a lock is held on byte `offset` of `path`, as
   another process sees it (a process never sees its own): 1, 0, or -1. */
#include <errno.h>
#include <fcntl.h>
#include <sys/wait.h>
int32_t vt_lock_held(const char *path, int64_t offset) {
  pid_t pid = fork();
  if (pid == 0) {
    int fd = open(path, O_RDONLY);
    struct flock fl;
    memset(&fl, 0, sizeof fl);
    fl.l_type = F_WRLCK; fl.l_whence = SEEK_SET; fl.l_start = (off_t)offset; fl.l_len = 1;
    if (fd < 0 || fcntl(fd, F_GETLK, &fl) != 0) _exit(2);
    _exit(fl.l_type == F_UNLCK ? 0 : 1);
  }
  int st;
  if (pid < 0 || waitpid(pid, &st, 0) != pid || !WIFEXITED(st) || WEXITSTATUS(st) == 2) return -1;
  return WEXITSTATUS(st);
}

/* For the sql tests: make `link` a symbolic link to `target`; 0, or errno. */
int32_t vt_symlink(const char *target, const char *link) {
  return symlink(target, link) == 0 ? 0 : errno;
}
