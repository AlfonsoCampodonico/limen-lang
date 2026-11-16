/* The operating-system sandbox: at startup, the whole process (and every
   program it starts) is restricted to what was granted, so code that is
   outside Limen's own checks (C called through `foreign`, or a bug in the
   runtime) cannot reach past the grants either. Shared by the native runtime
   (compiled into every program, see w_init) and the interpreter (ffi_stubs.c).

   What the kernel enforces, from the grants:
   - files: writing only under the Fs directory (unless it is read-only), and
     with Exec, the temporary directory (Exec keeps a child's stderr there);
     reading file contents only under the Fs directory, the temporary
     directory with Exec, and system locations (libraries, /etc,
     certificates, devices);
   - network: none at all unless Net or Listen is granted, and that includes
     Unix sockets, which reach other programs (Linux allows socketpair);
   - signals: only to this process, and with Exec to the programs it starts;
   - /proc (Linux): only this process's own entries, unless Exec is granted;
   - programs: starting another program only if Exec is granted, and then
     (unless it is granted `*`) only the granted programs' files and programs
     in system locations, never one the program could have written under the
     Fs or temporary directory, even when that directory is inside a system
     location (on Linux that whole system location then stops starting
     programs) or holds one (`fs=/`): there only the OS's own program
     directories still start programs, when this process cannot act as root
     and no one but root can change them, the directories above them or
     anything inside them (see w_sb_sealed). A program started through Exec
     inherits the same restrictions; it may also list (not read) the
     directories above the current one, as shells do.
   macOS also refuses Apple events and LaunchServices opens, so the process
   cannot have another application act for it. It still reaches the system's
   Mach services, as every process does; seccomp on Linux refuses x32 system
   calls and io_uring without Net/Listen.
   What stays in Limen, not the kernel: which hosts and ports Net and Listen
   allow, which of the programs in system locations Exec allows and with
   which arguments, which environment variables Env shows,
   Fs's refusal of `..` and absolute paths (the kernel only sees the result).

   macOS: a sandbox profile (sandbox_init). Linux: Landlock for files (kernel
   5.13 or later) and seccomp for network, exec and signals; the Linux
   code is tested in a container (dev/linux/run.sh). Elsewhere, or on a Linux kernel without Landlock, files are not
   restricted by the kernel.

   w_sandbox_enter returns 0 when restrictions are in place, 1 when this
   system has no sandbox, -1 with a message in err when it failed. Must be
   called while the process has a single thread. */
/* first in every program (the runtime follows it), so before any header */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <pthread.h>
#if defined(__linux__)
#include <sys/syscall.h>
#endif

/* Out of memory: the runtime's message (w_oom), for code that runs before
   it or in the interpreter. */
__attribute__((noreturn)) static void w_sb_oom(void) { fputs("panic: out of memory\n", stderr); exit(1); }
static void *w_sb_alloc(size_t n) { void *p = malloc(n ? n : 1); if (!p) w_sb_oom(); return p; }
static char *w_sb_strndup(const char *s, size_t n) { char *p = strndup(s, n); if (!p) w_sb_oom(); return p; }

typedef struct {
  const char *fs_root; /* realpath of the Fs grant, or NULL */
  int fs_ro;
  int net;             /* Net or Listen granted */
  int exec;            /* Exec granted */
  const char *exec_progs; /* the programs it names, one per line (`*` for any), or NULL */
} WSandboxSpec;

/* The programs Exec may start, as files: each granted name resolved as
   Exec resolves it (a name without `/` through PATH) to its real path.
   NULL when `*` is granted: any program. Free with w_sb_free_progs. */
static char **w_sb_progs(const WSandboxSpec *s, int *any) {
  *any = 0;
  if (!s->exec_progs) { *any = 1; return NULL; }
  size_t n = 1;
  for (const char *c = s->exec_progs; *c; c++) if (*c == '\n') n++;
  char **out = (char **)calloc(n + 1, sizeof(char *));
  if (!out) w_sb_oom();
  size_t k = 0;
  const char *p = s->exec_progs;
  while (*p || p == s->exec_progs) {
    const char *nl = strchr(p, '\n');
    size_t len = nl ? (size_t)(nl - p) : strlen(p);
    char *name = w_sb_strndup(p, len);
    if (strcmp(name, "*") == 0) { *any = 1; free(name); for (size_t i = 0; i < k; i++) free(out[i]); free(out); return NULL; }
    char *real = NULL;
    if (strchr(name, '/')) real = realpath(name, NULL);
    else if (name[0]) {
      const char *path = getenv("PATH");
      if (!path || !path[0]) path = "/usr/bin:/bin";
      for (const char *d = path; !real;) {
        const char *colon = strchr(d, ':');
        size_t dn = colon ? (size_t)(colon - d) : strlen(d);
        char buf[4096];
        if (snprintf(buf, sizeof buf, "%.*s/%s", (int)dn, dn ? d : ".", name) < (int)sizeof buf && access(buf, X_OK) == 0) real = realpath(buf, NULL);
        if (!colon) break;
        d = colon + 1;
      }
    }
    free(name);
    if (real) out[k++] = real;
    if (!nl) break;
    p = nl + 1;
  }
  return out;
}
static void w_sb_free_progs(char **ps) { if (ps) { for (char **p = ps; *p; p++) free(*p); free(ps); } }

/* a directory's real path, or NULL; free() it */
static char *w_sb_real(const char *p) { return p && p[0] ? realpath(p, NULL) : NULL; }

static char *w_sb_tmpdir(void) {
  const char *t = getenv("TMPDIR");
  char *r = w_sb_real(t && t[0] ? t : "/tmp");
  if (r) {
    size_t n = strlen(r);
    while (n > 1 && r[n - 1] == '/') r[--n] = 0;
  }
  return r;
}

/* whether the real path a is dir or lies inside it */
static int w_sb_within(const char *a, const char *dir) {
  if (!a || !dir) return 0;
  size_t n = strlen(dir);
  return strncmp(a, dir, n) == 0 && (a[n] == 0 || a[n] == '/' || (n == 1 && dir[0] == '/'));
}

/* System directories that start programs even inside a writable Fs
   directory (`fs=/`, say), since nothing this process does can change
   them: the OS's own, for a process that is not root and cannot write what
   it does not own.

   A directory is sealed only when no one but root can change what lies at
   its path: it and every directory above it up to `/` (on the path as named,
   where a symbolic link must be root's too, and on its real path) are owned
   by root, writable by neither group nor others, and not writable by this
   process (an ACL could allow that). When a writable directory holds it,
   the program could also write anything inside it that is writable, so then,
   unless its file system is mounted read-only, everything inside it is
   checked the same way (files, directories and links, without following
   links or leaving its file system), up to W_SB_WALK_MAX entries. Whatever
   cannot be checked is not sealed. */
#ifndef W_SB_WALK_MAX
#define W_SB_WALK_MAX 50000
#endif

/* whether this process is root or can act as root on files: write, chmod
   or chown what it does not own. Linux: any root uid, or the capabilities
   that get past file permissions in its permitted set (from which it can
   raise them); unknown counts as privileged */
static int w_sb_privileged(void) {
  if (getuid() == 0 || geteuid() == 0) return 1;
  /* started set-user-ID or set-group-ID, or running with ids that differ:
     a saved id may be root's (macOS has no portable way to read it) */
  if (getuid() != geteuid() || getgid() != getegid()) return 1;
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
  if (issetugid()) return 1;
#endif
#if defined(__linux__)
  uid_t r, e, sv;
  if (getresuid(&r, &e, &sv) != 0 || sv == 0 || r != e || e != sv) return 1;
  gid_t gr, ge, gs;
  if (getresgid(&gr, &ge, &gs) != 0 || gr != ge || ge != gs) return 1;
  FILE *f = fopen("/proc/self/status", "r");
  if (!f) return 1;
  char line[256];
  int found = 0;
  unsigned long long prm = 0;
  while (fgets(line, sizeof line, f)) {
    if (strncmp(line, "CapPrm:", 7) == 0) {
      char *end;
      errno = 0;
      prm = strtoull(line + 7, &end, 16);
      found = errno == 0 && end != line + 7;
      break;
    }
  }
  fclose(f);
  /* any permitted capability counts: many of them lead to the others
     (CAP_SETUID, CAP_SETPCAP, CAP_SYS_PTRACE, ...) */
  if (!found || prm != 0) return 1;
#endif
  return 0;
}

/* an entry (as lstat saw it) that only root can change: root's, and for
   anything but a symbolic link, writable by neither group nor others nor
   this process */
static int w_sb_root_only(int dfd, const char *name, const struct stat *st) {
  if (st->st_uid != 0) return 0;
  if (S_ISLNK(st->st_mode)) return 1;
  if (st->st_mode & 022) return 0;
  if (faccessat(dfd, name, W_OK, AT_EACCESS) == 0) return 0;
  return errno == EACCES || errno == EROFS || errno == EPERM;
}

/* whether the absolute path p and every directory above it are root-only;
   p must be a directory or a symbolic link */
static int w_sb_chain(const char *p) {
  char buf[PATH_MAX];
  size_t n = p ? strlen(p) : 0;
  if (n == 0 || p[0] != '/' || n >= sizeof buf) return 0;
  memcpy(buf, p, n + 1);
  struct stat st;
  if (lstat("/", &st) != 0 || !w_sb_root_only(AT_FDCWD, "/", &st)) return 0;
  for (size_t i = 1; i <= n; i++) {
    if ((buf[i] != '/' && buf[i] != 0) || buf[i - 1] == '/') continue;
    char c = buf[i];
    buf[i] = 0;
    int ok = lstat(buf, &st) == 0 && (S_ISDIR(st.st_mode) || S_ISLNK(st.st_mode)) && w_sb_root_only(AT_FDCWD, buf, &st);
    buf[i] = c;
    if (!ok) return 0;
  }
  return 1;
}

/* everything inside the open directory dfd (closed here) root-only, on
   device dev; budget counts entries down */
static int w_sb_walk(int dfd, dev_t dev, long *budget, int depth) {
  DIR *d = fdopendir(dfd);
  if (!d) { close(dfd); return 0; }
  int ok = 1;
  for (;;) {
    errno = 0;
    struct dirent *e = readdir(d);
    if (!e) { if (errno) ok = 0; break; }
    if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0) continue;
    struct stat st;
    if (--*budget < 0 || fstatat(dirfd(d), e->d_name, &st, AT_SYMLINK_NOFOLLOW) != 0 || st.st_dev != dev ||
        !w_sb_root_only(dirfd(d), e->d_name, &st)) { ok = 0; break; }
    if (S_ISDIR(st.st_mode)) {
      int c = depth < 64 ? openat(dirfd(d), e->d_name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC) : -1;
      if (c < 0 || !w_sb_walk(c, dev, budget, depth + 1)) { ok = 0; break; }
    }
  }
  closedir(d);
  return ok;
}

/* whether nothing inside the real directory p can change but by root:
   on macOS, it lies on the read-only sealed system volume, which this
   process cannot remount; otherwise everything inside it is root-only (on
   Linux a read-only mount proves nothing: it can be a bind mount of a
   writable directory, so the tree is always walked) */
static int w_sb_tree(const char *p) {
  int fd = open(p, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
  struct stat st;
  struct statvfs vfs;
  if (fd < 0) return 0;
  if (fstat(fd, &st) != 0 || fstatvfs(fd, &vfs) != 0) { close(fd); return 0; }
#if defined(__APPLE__)
  if (vfs.f_flag & ST_RDONLY) { close(fd); return 1; }
#endif
  long budget = W_SB_WALK_MAX;
  return w_sb_walk(fd, st.st_dev, &budget, 0);
}

/* Returns the sealed directory's real path (free it), or NULL. rw: the
   real paths this process can write (either may be NULL). */
static char *w_sb_sealed(const char *dir, char *const rw[2]) {
  if (w_sb_privileged() || !w_sb_chain(dir)) return NULL;
  char *real = w_sb_real(dir);
  struct stat st;
  if (!real || !w_sb_chain(real) || lstat(real, &st) != 0 || !S_ISDIR(st.st_mode) || w_sb_within(rw[0], real) ||
      w_sb_within(rw[1], real) || ((w_sb_within(real, rw[0]) || w_sb_within(real, rw[1])) && !w_sb_tree(real))) {
    free(real);
    return NULL;
  }
  return real;
}

#if defined(__APPLE__)

int sandbox_init(const char *profile, uint64_t flags, char **errorbuf);
void sandbox_free_error(char *errorbuf);

typedef struct { char *p; size_t len, cap; } WSbBuf;
static void w_sb_put(WSbBuf *b, const char *s) {
  size_t n = strlen(s);
  if (b->len + n + 1 > b->cap) {
    b->cap = (b->len + n + 1) * 2;
    b->p = (char *)realloc(b->p, b->cap);
    if (!b->p) { fputs("panic: out of memory\n", stderr); exit(1); }
  }
  memcpy(b->p + b->len, s, n + 1);
  b->len += n;
}
/* a string literal of the profile language, escaped */
static void w_sb_str(WSbBuf *b, const char *s) {
  w_sb_put(b, "\"");
  for (; *s; s++) {
    char c[3] = { 0, 0, 0 };
    if (*s == '"' || *s == '\\') { c[0] = '\\'; c[1] = *s; } else c[0] = *s;
    w_sb_put(b, c);
  }
  w_sb_put(b, "\"");
}
static void w_sb_subpath(WSbBuf *b, const char *p) { if (p) { w_sb_put(b, " (subpath "); w_sb_str(b, p); w_sb_put(b, ")"); } }

static int w_sandbox_enter(const WSandboxSpec *s, char *err, size_t en) {
  char *tmp = s->exec ? w_sb_tmpdir() : NULL;
  /* OpenSSL's trust store may be named by the environment */
  char *cert_file = w_sb_real(getenv("SSL_CERT_FILE"));
  char *cert_dir = w_sb_real(getenv("SSL_CERT_DIR"));
  WSbBuf b = { NULL, 0, 0 };
  w_sb_put(&b, "(version 1)\n(allow default)\n");
  /* writing: the Fs directory (unless read-only), the temp directory with Exec, and devices */
  w_sb_put(&b, "(deny file-write* (require-not (require-any (literal \"/dev/null\") (literal \"/dev/tty\") (literal \"/dev/dtracehelper\") (regex #\"^/dev/fd/\")");
  if (!s->fs_ro) w_sb_subpath(&b, s->fs_root);
  w_sb_subpath(&b, tmp);
  w_sb_put(&b, ")))\n");
  /* reading contents: system locations, the Fs directory, the temp directory with Exec */
  w_sb_put(&b, "(deny file-read-data (require-not (require-any (literal \"/\")");
  const char *sys[] = { "/usr", "/System", "/Library", "/bin", "/sbin", "/private/etc", "/private/var/db", "/private/var/select",
                        "/dev", "/opt/homebrew", "/opt/local", "/Applications/Xcode.app", NULL };
  for (int i = 0; sys[i]; i++) w_sb_subpath(&b, sys[i]);
  w_sb_subpath(&b, s->fs_root);
  w_sb_subpath(&b, tmp);
  w_sb_subpath(&b, cert_dir);
  if (cert_file) { w_sb_put(&b, " (literal "); w_sb_str(&b, cert_file); w_sb_put(&b, ")"); }
  if (s->exec) {
    /* a started program runs in our directory, and a shell lists the
       directories above it to learn its path: those directories themselves
       (not what is in them) */
    char *cwd = getcwd(NULL, 0);
    char *real = w_sb_real(cwd);
    free(cwd);
    if (real) {
      for (char *q = real + 1;; q++) {
        if (*q == '/' || *q == 0) {
          char c = *q;
          *q = 0;
          w_sb_put(&b, " (literal "); w_sb_str(&b, real); w_sb_put(&b, ")");
          *q = c;
          if (!c) break;
        }
      }
      free(real);
    }
  }
  w_sb_put(&b, ")))\n");
  /* no reaching other applications, which have their own authority */
  w_sb_put(&b, "(deny appleevent-send)\n(deny lsopen)\n");
  /* signals: this process, and with Exec the programs it started */
  w_sb_put(&b, "(deny signal)\n(allow signal (target self))\n");
  if (s->exec) w_sb_put(&b, "(allow signal (target children))\n");
  if (!s->net) w_sb_put(&b, "(deny network*)\n");
  if (!s->exec) w_sb_put(&b, "(deny process-exec*)\n(deny process-fork)\n");
  else {
    /* starting programs: the granted ones, and those in system locations
       (which the granted ones start in turn); nothing the program could
       have written, unless it is granted by name */
    int any;
    char **progs = w_sb_progs(s, &any);
    if (!any) {
      w_sb_put(&b, "(deny process-exec* (require-not (require-any");
      const char *bins[] = { "/usr", "/bin", "/sbin", "/System", "/Library", "/opt/homebrew", "/opt/local", "/Applications/Xcode.app", NULL };
      for (int i = 0; bins[i]; i++) w_sb_subpath(&b, bins[i]);
      for (char **p = progs; *p; p++) { w_sb_put(&b, " (literal "); w_sb_str(&b, *p); w_sb_put(&b, ")"); }
      w_sb_put(&b, ")))\n");
      /* the system locations include ones a user can write to (Homebrew's
         prefix, /usr/local), which may hold the Fs or temporary directory:
         nothing under a directory this process can write is started, the
         last matching rule winning, except a granted program by name */
      char *rw[2] = { s->fs_ro ? NULL : (char *)s->fs_root, tmp };
      /* (a deny without a path would refuse everything) */
      if (rw[0] || rw[1]) {
        w_sb_put(&b, "(deny process-exec* (require-any");
        w_sb_subpath(&b, rw[0]);
        w_sb_subpath(&b, rw[1]);
        w_sb_put(&b, "))\n");
        /* the system volume's program directories (SIP keeps them even
           from root) and the compilers of the Command Line Tools; not all of
           /System, nor Xcode in /Applications, which admins can rename */
        const char *sealed[] = { "/bin", "/sbin", "/usr/bin", "/usr/sbin", "/usr/lib", "/usr/libexec",
                                 "/Library/Developer/CommandLineTools/usr/bin", "/Library/Developer/CommandLineTools/usr/libexec", NULL };
        for (int i = 0; sealed[i]; i++) {
          char *real = w_sb_sealed(sealed[i], rw);
          if (real) { w_sb_put(&b, "(allow process-exec* (subpath "); w_sb_str(&b, real); w_sb_put(&b, "))\n"); }
          free(real);
        }
      }
      for (char **p = progs; *p; p++) { w_sb_put(&b, "(allow process-exec* (literal "); w_sb_str(&b, *p); w_sb_put(&b, "))\n"); }
    }
    w_sb_free_progs(progs);
  }
  free(tmp); free(cert_file); free(cert_dir);
  char *e = NULL;
  int rc = sandbox_init(b.p, 0, &e);
  free(b.p);
  if (rc != 0) {
    snprintf(err, en, "cannot enter the sandbox: %s", e ? e : "unknown error");
    if (e) sandbox_free_error(e);
    return -1;
  }
  return 0;
}

#elif defined(__linux__)

/* Landlock restricts files (and, from ABI 6, abstract Unix sockets and
   signals); a seccomp filter refuses sockets without Net/Listen, execve
   without Exec, and signals to other processes without Exec. */
#include <fcntl.h>
#include <stddef.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <sys/socket.h>
#include <linux/filter.h>
#include <linux/seccomp.h>
#include <linux/audit.h>
#ifndef O_PATH
#define O_PATH 010000000 /* glibc hides it without _GNU_SOURCE */
#endif

#ifndef __NR_landlock_create_ruleset
#define __NR_landlock_create_ruleset 444
#endif
#ifndef __NR_landlock_add_rule
#define __NR_landlock_add_rule 445
#endif
#ifndef __NR_landlock_restrict_self
#define __NR_landlock_restrict_self 446
#endif
#define W_LL_EXECUTE (1ULL << 0)
#define W_LL_WRITE_FILE (1ULL << 1)
#define W_LL_READ_FILE (1ULL << 2)
#define W_LL_READ_DIR (1ULL << 3)
#define W_LL_ABI1_ALL ((1ULL << 13) - 1)
#define W_LL_REFER (1ULL << 13)
#define W_LL_TRUNCATE (1ULL << 14)
#define W_LL_FILE_ONLY (W_LL_EXECUTE | W_LL_WRITE_FILE | W_LL_READ_FILE | W_LL_TRUNCATE)

struct w_ll_ruleset_attr { uint64_t handled_access_fs; uint64_t handled_access_net; uint64_t scoped; };
#define W_LL_NET_BIND_TCP (1ULL << 0)
#define W_LL_NET_CONNECT_TCP (1ULL << 1)
#define W_LL_SCOPE_ABSTRACT_UNIX (1ULL << 0)
#define W_LL_SCOPE_SIGNAL (1ULL << 1)
struct w_ll_path_beneath { uint64_t allowed_access; int32_t parent_fd; } __attribute__((packed));

static int w_ll_allow(int ruleset, const char *path, uint64_t access, uint64_t handled) {
  if (!path) return 0;
  int fd = open(path, O_PATH | O_CLOEXEC);
  if (fd < 0) return 0; /* a missing system directory is fine */
  struct stat st;
  if (fstat(fd, &st) == 0 && !S_ISDIR(st.st_mode)) access &= W_LL_FILE_ONLY;
  struct w_ll_path_beneath pb = { access & handled, fd };
  long rc = syscall(__NR_landlock_add_rule, ruleset, 1 /* LANDLOCK_RULE_PATH_BENEATH */, &pb, 0);
  close(fd);
  return rc == 0 ? 0 : -1;
}

/* whether a is b, or one lies inside the other (real paths) */
static int w_sb_overlap(const char *a, const char *b) { return w_sb_within(a, b) || w_sb_within(b, a); }

static int w_landlock(const WSandboxSpec *s, char *err, size_t en) {
  long abi = syscall(__NR_landlock_create_ruleset, NULL, 0, 1 /* LANDLOCK_CREATE_RULESET_VERSION */);
  if (abi < 1) return 1; /* no Landlock in this kernel */
  uint64_t handled = W_LL_ABI1_ALL | (abi >= 2 ? W_LL_REFER : 0) | (abi >= 3 ? W_LL_TRUNCATE : 0);
  /* ABI 4 and later also handle TCP: with no rules, no bind and no connect.
     ABI 6 and later scope abstract Unix sockets and signals: only processes
     in this sandbox (this one, and with Exec what it starts) can be reached.
     Pathname Unix sockets are refused by seccomp below, without Net/Listen. */
  struct w_ll_ruleset_attr attr = {
    handled,
    (abi >= 4 && !s->net) ? (W_LL_NET_BIND_TCP | W_LL_NET_CONNECT_TCP) : 0,
    abi >= 6 ? (W_LL_SCOPE_SIGNAL | (s->net ? 0 : W_LL_SCOPE_ABSTRACT_UNIX)) : 0,
  };
  size_t attr_size = abi >= 6 ? sizeof attr : abi >= 4 ? 2 * sizeof(uint64_t) : sizeof attr.handled_access_fs;
  int rs = (int)syscall(__NR_landlock_create_ruleset, &attr, attr_size, 0);
  if (rs < 0) { snprintf(err, en, "cannot create a Landlock ruleset: %s", strerror(errno)); return -1; }
  uint64_t rd = W_LL_READ_FILE | W_LL_READ_DIR | (s->exec ? W_LL_EXECUTE : 0);
  /* starting programs: from system locations and the granted programs'
     files, not from what the program could have written, unless Exec is
     granted `*` */
  int exec_any = 0;
  char **progs = s->exec ? w_sb_progs(s, &exec_any) : NULL;
  uint64_t no_exec = exec_any ? 0 : W_LL_EXECUTE;
  /* the directories this process can write: never executable. Landlock
     only adds rights down a path, so a system location that holds one of
     them (or lies inside one) loses execute as a whole: only the granted
     programs' files can then be started from it */
  char *rw[2] = { s->fs_ro ? NULL : w_sb_real(s->fs_root), s->exec ? w_sb_tmpdir() : NULL };
  const char *sys[] = { "/usr", "/lib", "/lib64", "/lib32", "/bin", "/sbin", "/etc", "/sys", "/opt", NULL };
  int bad = 0;
  for (int i = 0; sys[i]; i++) {
    uint64_t acc = rd;
    char *real = no_exec ? w_sb_real(sys[i]) : NULL;
    for (int j = 0; real && j < 2; j++) if (w_sb_overlap(real, rw[j])) acc &= ~no_exec;
    free(real);
    bad |= w_ll_allow(rs, sys[i], acc, handled);
  }
  /* but the OS's own program directories stay executable when no grant
     can make them writable */
  if (no_exec) {
    const char *sealed[] = { "/usr/bin", "/usr/sbin", "/usr/lib", "/usr/libexec", "/usr/lib64", "/usr/lib32",
                             "/bin", "/sbin", "/lib", "/lib64", "/lib32", NULL };
    for (int i = 0; sealed[i]; i++) {
      char *real = w_sb_sealed(sealed[i], rw);
      bad |= w_ll_allow(rs, real, rd, handled);
      free(real);
    }
  }
  /* /proc: only this process's own entries, so other processes' command
     lines and state stay hidden. /proc/self resolves to this process's
     directory now; a program started through Exec has its own, so with Exec
     all of /proc stays readable (Landlock still keeps ptrace-guarded entries
     such as another process's environ and memory out of reach). */
  bad |= w_ll_allow(rs, s->exec ? "/proc" : "/proc/self", rd, handled);
  bad |= w_ll_allow(rs, "/dev", W_LL_READ_FILE | W_LL_READ_DIR, handled);
  bad |= w_ll_allow(rs, "/dev/null", W_LL_READ_FILE | W_LL_WRITE_FILE | W_LL_TRUNCATE, handled);
  bad |= w_ll_allow(rs, "/dev/tty", W_LL_READ_FILE | W_LL_WRITE_FILE, handled);
  if (s->fs_root) bad |= w_ll_allow(rs, s->fs_root, s->fs_ro ? (W_LL_READ_FILE | W_LL_READ_DIR) : (handled & ~no_exec), handled);
  if (progs) for (char **p = progs; *p; p++) bad |= w_ll_allow(rs, *p, W_LL_READ_FILE | W_LL_EXECUTE, handled);
  w_sb_free_progs(progs);
  /* OpenSSL's trust store may be named by the environment */
  {
    char *cf = w_sb_real(getenv("SSL_CERT_FILE")), *cd = w_sb_real(getenv("SSL_CERT_DIR"));
    bad |= w_ll_allow(rs, cf, W_LL_READ_FILE, handled);
    bad |= w_ll_allow(rs, cd, W_LL_READ_FILE | W_LL_READ_DIR, handled);
    free(cf); free(cd);
  }
  bad |= w_ll_allow(rs, rw[1], handled & ~no_exec, handled);
  free(rw[0]); free(rw[1]);
  if (bad) { close(rs); snprintf(err, en, "cannot add a Landlock rule: %s", strerror(errno)); return -1; }
  if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0 || syscall(__NR_landlock_restrict_self, rs, 0) != 0) {
    close(rs);
    snprintf(err, en, "cannot enforce the Landlock ruleset: %s", strerror(errno));
    return -1;
  }
  close(rs);
  return 0;
}

#if defined(__x86_64__)
#define W_SB_ARCH AUDIT_ARCH_X86_64
#elif defined(__aarch64__)
#define W_SB_ARCH AUDIT_ARCH_AARCH64
#endif

static int w_seccomp(const WSandboxSpec *s, char *err, size_t en) {
#ifndef W_SB_ARCH
  (void)s; (void)err; (void)en;
  return 1;
#else
  if (s->net && s->exec) return 0;
  struct sock_filter f[64];
  pid_t self = getpid();
  int n = 0;
#define W_SB(code, k, jt, jf) (f[n++] = (struct sock_filter){ (unsigned short)(code), (unsigned char)(jt), (unsigned char)(jf), (unsigned int)(k) })
  /* another architecture's system calls: refuse them all */
  W_SB(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, arch), 0, 0);
  W_SB(BPF_JMP | BPF_JEQ | BPF_K, W_SB_ARCH, 1, 0);
  W_SB(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EPERM, 0, 0);
  W_SB(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, nr), 0, 0);
#if defined(__x86_64__)
  /* x32 system calls (numbers with bit 30 set) would get past the checks below */
  W_SB(BPF_JMP | BPF_JGE | BPF_K, 0x40000000, 0, 1);
  W_SB(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EPERM, 0, 0);
#endif
  if (!s->exec) {
    W_SB(BPF_JMP | BPF_JEQ | BPF_K, __NR_execve, 0, 1);
    W_SB(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EPERM, 0, 0);
#ifdef __NR_execveat
    W_SB(BPF_JMP | BPF_JEQ | BPF_K, __NR_execveat, 0, 1);
    W_SB(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EPERM, 0, 0);
#endif
  }
  if (!s->exec) {
    /* signals only to this process (a kernel with Landlock ABI 6 also scopes
       them; this covers older ones): kill and the queueing calls name the
       target process first, tkill and pidfd signals are refused */
    const long to_pid[] = { __NR_kill, __NR_tgkill, __NR_rt_sigqueueinfo, __NR_rt_tgsigqueueinfo, -1 };
    for (int i = 0; to_pid[i] >= 0; i++) {
      W_SB(BPF_JMP | BPF_JEQ | BPF_K, to_pid[i], 0, 4);
      W_SB(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, args[0]), 0, 0);
      W_SB(BPF_JMP | BPF_JEQ | BPF_K, (unsigned int)self, 1, 0);
      W_SB(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EPERM, 0, 0);
      W_SB(BPF_RET | BPF_K, SECCOMP_RET_ALLOW, 0, 0);
    }
    W_SB(BPF_JMP | BPF_JEQ | BPF_K, __NR_tkill, 0, 1);
    W_SB(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EPERM, 0, 0);
#ifdef __NR_pidfd_send_signal
    W_SB(BPF_JMP | BPF_JEQ | BPF_K, __NR_pidfd_send_signal, 0, 1);
    W_SB(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EPERM, 0, 0);
#endif
  }
  if (!s->net) {
    /* io_uring can open sockets without the socket system call */
#ifdef __NR_io_uring_setup
    W_SB(BPF_JMP | BPF_JEQ | BPF_K, __NR_io_uring_setup, 0, 1);
    W_SB(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EPERM, 0, 0);
#endif
    /* no socket() at all: a Unix socket could connect to another program
       (a Docker daemon, an ssh-agent) and act with its authority. A
       socketpair connects only to itself, so Unix ones stay allowed. */
    W_SB(BPF_JMP | BPF_JEQ | BPF_K, __NR_socket, 0, 1);
    W_SB(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EACCES, 0, 0);
    W_SB(BPF_JMP | BPF_JEQ | BPF_K, __NR_socketpair, 0, 4);
    W_SB(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, args[0]), 0, 0);
    W_SB(BPF_JMP | BPF_JEQ | BPF_K, AF_UNIX, 1, 0);
    W_SB(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EACCES, 0, 0);
    W_SB(BPF_RET | BPF_K, SECCOMP_RET_ALLOW, 0, 0);
  }
  W_SB(BPF_RET | BPF_K, SECCOMP_RET_ALLOW, 0, 0);
#undef W_SB
  struct sock_fprog prog = { (unsigned short)n, f };
  if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) {
    snprintf(err, en, "cannot set no_new_privs: %s", strerror(errno));
    return -1;
  }
  if (prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &prog) != 0) {
    if (errno == EINVAL) return 1; /* no seccomp filters here (an emulator, say) */
    snprintf(err, en, "cannot install the seccomp filter: %s", strerror(errno));
    return -1;
  }
  return 0;
#endif
}

static int w_sandbox_enter(const WSandboxSpec *s, char *err, size_t en) {
  int a = w_landlock(s, err, en);
  if (a < 0) return -1;
  int b = w_seccomp(s, err, en);
  if (b < 0) return -1;
  return (a == 0 && b == 0) ? 0 : 1;
}

#else

static int w_sandbox_enter(const WSandboxSpec *s, char *err, size_t en) {
  (void)s; (void)err; (void)en;
  return 1;
}

#endif

/* ---------- Fs paths ----------
   Every Fs operation, in both backends (the interpreter calls these through
   ffi_stubs.c), reaches its file through directory descriptors, never by
   handing the kernel a path that could change between a check and its use.
   The capability's root is opened first: the granted directory (`top`) by
   its path, then each directory below it that `under` chose, never through a
   symbolic link. From there the path is walked one name at a time with
   openat(..., O_NOFOLLOW): a symbolic link is never followed by the kernel
   but read here (readlinkat) and its target walked the same way, from the
   directory that holds it (or from the root, for an absolute target inside
   it), and `..` goes back to the directory it came from. A link that leads
   out of the root is refused, whatever happens to the directories while the
   walk goes on: swapping a directory for a link between two steps only
   changes which file inside the root is reached.

   The result is the directory that holds the entry (a descriptor and its
   real path) and the entry's name in it, "." for the directory itself. With
   W_FS_FOLLOW a final link is followed too (read, write, list, info); without
   it the operation acts on the link itself (remove, rename, make_dir).
   Errors: kind 1 is Denied, 2 Failed, with the message in e->msg. */

enum { W_FS_FOLLOW = 1, W_FS_CREATE = 2, W_FS_EXIST = 4 };
#define W_FS_MAX_LINKS 40
typedef struct { int kind; char msg[1200]; } WFsErr;
typedef struct {
  int dfd;      /* the directory holding the entry */
  char *name;   /* the entry's name there; "." for that directory itself */
  char *dir;    /* the directory's real path */
  char *shown;  /* root/path, for messages */
  int is_root;  /* the path names the root itself, before following links */
  int own;      /* dfd is to be closed (not the kept granted directory) */
} WFsAt;

static void w_fs_at_free(WFsAt *at) {
  if (at->dfd >= 0 && at->own) close(at->dfd);
  free(at->name); free(at->dir); free(at->shown);
  at->dfd = -1; at->name = at->dir = at->shown = NULL;
}

#if defined(__linux__)
#define W_FS_DIRFLAGS (O_PATH | O_DIRECTORY | O_CLOEXEC)   /* search permission is enough */
#else
#define W_FS_DIRFLAGS (O_RDONLY | O_DIRECTORY | O_CLOEXEC)
#endif

static int w_fs_inside(const char *root, const char *p) {
  size_t n = strlen(root);
  if (strcmp(root, "/") == 0) return p[0] == '/';
  return strncmp(root, p, n) == 0 && (p[n] == 0 || p[n] == '/');
}

/* `rel` as names separated by single slashes ("" for the root), with `.`
   dropped and `..` taken back; NULL with e set when it is absolute, climbs
   out or holds a NUL byte. */
static char *w_fs_norm(const char *rel, size_t len, WFsErr *e) {
  if (memchr(rel, 0, len)) { e->kind = 2; snprintf(e->msg, sizeof e->msg, "fs: the path contains a NUL byte"); return NULL; }
  if (len > 0 && rel[0] == '/') { e->kind = 1; snprintf(e->msg, sizeof e->msg, "fs: absolute paths are not allowed; paths are relative to the granted directory"); return NULL; }
  char *out = (char *)w_sb_alloc(len + 1);
  size_t o = 0, i = 0;
  while (i < len) {
    while (i < len && rel[i] == '/') i++;
    size_t j = i;
    while (j < len && rel[j] != '/') j++;
    size_t n = j - i;
    if (n == 0 || (n == 1 && rel[i] == '.')) { i = j; continue; }
    if (n == 2 && rel[i] == '.' && rel[i + 1] == '.') {
      if (o == 0) { free(out); e->kind = 1; snprintf(e->msg, sizeof e->msg, "fs: path escapes the granted directory"); return NULL; }
      while (o > 0 && out[o - 1] != '/') o--;
      if (o > 0) o--;
      i = j; continue;
    }
    if (o > 0) out[o++] = '/';
    memcpy(out + o, rel + i, n); o += n;
    i = j;
  }
  out[o] = 0;
  return out;
}

/* The granted directory, opened once and kept: the grant is that
   directory. A different `top` (the interpreter's tests each have their
   own; it runs them one at a time) replaces it. Never closed by callers. */
static pthread_mutex_t w_fs_top_mu = PTHREAD_MUTEX_INITIALIZER;
static char *w_fs_top_path;
static int w_fs_top_cached = -1;
static int w_fs_top_fd(const char *top) {
  pthread_mutex_lock(&w_fs_top_mu);
  if (!w_fs_top_path || strcmp(w_fs_top_path, top) != 0) {
    int fd = open(top, W_FS_DIRFLAGS);
    if (fd < 0) { int err = errno; pthread_mutex_unlock(&w_fs_top_mu); errno = err; return -1; }
    if (w_fs_top_cached >= 0) close(w_fs_top_cached);
    free(w_fs_top_path);
    w_fs_top_path = w_sb_strndup(top, strlen(top));
    w_fs_top_cached = fd;
  }
  int fd = w_fs_top_cached;
  pthread_mutex_unlock(&w_fs_top_mu);
  return fd;
}

/* One step for a path of several names with no symbolic link anywhere in
   it (or -1, and the caller walks it name by name): O_NOFOLLOW_ANY on
   macOS, openat2 with RESOLVE_NO_SYMLINKS on Linux. */
#if defined(__linux__) && !defined(SYS_openat2)
#define SYS_openat2 437
#endif
static int w_fs_open_plain(int dfd, const char *path, int flags) {
#if defined(O_NOFOLLOW_ANY)
  return openat(dfd, path, flags | O_NOFOLLOW_ANY);
#elif defined(__linux__)
  static int none;
  if (__atomic_load_n(&none, __ATOMIC_RELAXED)) return -1;
  struct { uint64_t flags, mode, resolve; } how = { (uint64_t)flags, 0, 0x04 | 0x08 /* RESOLVE_NO_SYMLINKS | RESOLVE_BENEATH */ };
  long fd = syscall(SYS_openat2, dfd, path, &how, sizeof how);
  if (fd < 0 && (errno == ENOSYS || errno == EPERM)) __atomic_store_n(&none, 1, __ATOMIC_RELAXED);
  return fd < 0 ? -1 : (int)fd;
#else
  (void)dfd; (void)path; (void)flags;
  return -1;
#endif
}

/* The root's directory: the granted directory, then each name of `root`
   below it without following links. -1 with errno set; EXDEV when a link
   (or a root not under top) is in the way. *borrowed: it is the kept
   descriptor of the granted directory, not to be closed. */
static int w_fs_root_fd(const char *top, const char *root, int *borrowed) {
  size_t tn = strlen(top);
  *borrowed = 0;
  if (!w_fs_inside(top, root)) { errno = EXDEV; return -1; }
  int tfd = w_fs_top_fd(top);
  if (tfd < 0) return -1;
  const char *p = root + (strcmp(top, "/") == 0 ? 0 : tn);
  while (*p == '/') p++;
  if (!*p) { *borrowed = 1; return tfd; }
  int fd = w_fs_open_plain(tfd, p, W_FS_DIRFLAGS);
  if (fd >= 0) return fd;
  fd = tfd;
  char name[NAME_MAX + 1];
  while (*p) {
    while (*p == '/') p++;
    if (!*p) break;
    const char *q = p;
    while (*q && *q != '/') q++;
    if ((size_t)(q - p) > NAME_MAX) { if (fd != tfd) close(fd); errno = ENAMETOOLONG; return -1; }
    memcpy(name, p, (size_t)(q - p)); name[q - p] = 0;
    p = q;
    int nfd = openat(fd, name, W_FS_DIRFLAGS | O_NOFOLLOW);
    if (nfd < 0) {
      int err = errno;
      char lb[8];
      if (readlinkat(fd, name, lb, sizeof lb) >= 0) err = EXDEV;
      if (fd != tfd) close(fd);
      errno = err;
      return -1;
    }
    if (fd != tfd) close(fd);
    fd = nfd;
  }
  return fd;
}

static void w_fs_missing(WFsErr *e, int how, const char *rel, size_t len) {
  e->kind = 2;
  if (how & W_FS_CREATE) snprintf(e->msg, sizeof e->msg, "fs: directory of `%.*s` does not exist", (int)len, rel);
  else snprintf(e->msg, sizeof e->msg, "fs: `%.*s` does not exist", (int)len, rel);
}
static void w_fs_outside(WFsErr *e) { e->kind = 1; snprintf(e->msg, sizeof e->msg, "fs: path resolves outside the granted directory"); }

/* Walk `rel` (len bytes) from the root; see above. 0, or -1 with e set. */
static int w_fs_at(const char *top, const char *root, const char *rel, size_t len, int how, WFsAt *at, WFsErr *e) {
  at->dfd = -1; at->name = at->dir = at->shown = NULL; at->is_root = 0; at->own = 0;
  char *norm = w_fs_norm(rel, len, e);
  if (!norm) return -1;
  size_t rn = strlen(root);
  {
    size_t sn = rn + strlen(norm) + 2;
    at->shown = (char *)w_sb_alloc(sn);
    snprintf(at->shown, sn, norm[0] ? "%s%s%s" : "%s", root, strcmp(root, "/") == 0 ? "" : "/", norm);
  }
  at->is_root = norm[0] == 0;
  int borrowed;
  int bottom = w_fs_root_fd(top, root, &borrowed);
  if (bottom < 0) {
    if (errno == EXDEV) w_fs_outside(e); else w_fs_missing(e, how, rel, len);
    free(norm); w_fs_at_free(at);
    return -1;
  }
  /* the directories walked so far, from the root; and the real path of the last */
  int cap = 16, depth = 0;
  int *fds = (int *)w_sb_alloc(sizeof(int) * (size_t)cap);
  size_t *plen = (size_t *)w_sb_alloc(sizeof(size_t) * (size_t)cap);
  fds[0] = bottom; plen[0] = rn;
  size_t pcap = rn + 256;
  char *phys = (char *)w_sb_alloc(pcap);
  memcpy(phys, root, rn + 1);
  /* what is left to walk; a link's target goes in front of it */
  char *rest = norm;
  size_t at_i = 0;
  char *fast = NULL;   /* the directories the step below opened at once, while that level is in use */
  {
    /* the directories on the way in one step, when none is a link */
    char *slash = strrchr(norm, '/');
    if (slash) {
      *slash = 0;
      int fd = w_fs_open_plain(bottom, norm, W_FS_DIRFLAGS);
      if (fd >= 0) {
        size_t dn = (size_t)(slash - norm);
        if (rn + 1 + dn + 1 > pcap) { pcap = rn + dn + 256; phys = (char *)realloc(phys, pcap); if (!phys) w_sb_oom(); }
        size_t cur = rn;
        if (!(cur == 1 && phys[0] == '/')) phys[cur++] = '/';
        memcpy(phys + cur, norm, dn + 1);
        depth = 1; fds[1] = fd; plen[1] = cur + dn;
        at_i = dn + 1;
        fast = w_sb_strndup(norm, dn);
      }
      *slash = '/';
    }
  }
  int links = 0, ok = 0;
  char *link = (char *)w_sb_alloc(PATH_MAX + 1);
  char name[NAME_MAX + 1];
  for (;;) {
    while (rest[at_i] == '/') at_i++;
    if (!rest[at_i]) { at->name = w_sb_strndup(".", 1); ok = 1; break; }
    size_t j = at_i;
    while (rest[j] && rest[j] != '/') j++;
    size_t n = j - at_i;
    if (n > NAME_MAX) { e->kind = 2; snprintf(e->msg, sizeof e->msg, "fs: `%.*s`: %s", (int)len, rel, strerror(ENAMETOOLONG)); break; }
    memcpy(name, rest + at_i, n); name[n] = 0;
    size_t k = j;
    while (rest[k] == '/') k++;
    int last = rest[k] == 0;
    at_i = j;
    if (strcmp(name, ".") == 0) continue;
    if (strcmp(name, "..") == 0) {
      if (depth == 0) { w_fs_outside(e); break; }
      close(fds[depth]);
      depth--;
      phys[plen[depth]] = 0;
      continue;
    }
    ssize_t ln = -1;
    if (!last) {
      int nfd = openat(fds[depth], name, W_FS_DIRFLAGS | O_NOFOLLOW);
      if (nfd >= 0) {
        if (depth + 1 >= cap) {
          cap *= 2;
          fds = (int *)realloc(fds, sizeof(int) * (size_t)cap);
          plen = (size_t *)realloc(plen, sizeof(size_t) * (size_t)cap);
          if (!fds || !plen) w_sb_oom();
        }
        size_t cur = plen[depth], need = cur + 1 + n + 1;
        if (need > pcap) { pcap = need * 2; phys = (char *)realloc(phys, pcap); if (!phys) w_sb_oom(); }
        if (!(cur == 1 && phys[0] == '/')) phys[cur++] = '/';
        memcpy(phys + cur, name, n + 1);
        depth++;
        fds[depth] = nfd; plen[depth] = cur + n;
        continue;
      }
      ln = readlinkat(fds[depth], name, link, PATH_MAX);
      if (ln < 0) { w_fs_missing(e, how, rel, len); break; }
    } else {
      if (how & W_FS_EXIST) {
        struct stat st;
        if (fstatat(fds[depth], name, &st, AT_SYMLINK_NOFOLLOW) != 0) { w_fs_missing(e, 0, rel, len); break; }
        if (S_ISLNK(st.st_mode) && (how & W_FS_FOLLOW)) {
          ln = readlinkat(fds[depth], name, link, PATH_MAX);
          if (ln < 0) {
            /* it stopped being a link just now: look again */
            if (++links > W_FS_MAX_LINKS) { w_fs_missing(e, how, rel, len); break; }
            at_i -= n;
            continue;
          }
        }
      } else if (how & W_FS_FOLLOW) ln = readlinkat(fds[depth], name, link, PATH_MAX);
      if (ln < 0) { at->name = w_sb_strndup(name, n); ok = 1; break; }
    }
    /* a link: walk its target, then what was left */
    if (++links > W_FS_MAX_LINKS || ln == 0) { w_fs_missing(e, how, rel, len); break; }
    link[ln] = 0;
    const char *t = link;
    char *real = NULL;
    if (t[0] == '/') {
      if (!w_fs_inside(root, t)) {
        /* a path to the root through other links (/tmp for /private/tmp):
           only to find which name inside the root it means */
        real = realpath(t, NULL);
        if (!real || !w_fs_inside(root, real)) { free(real); w_fs_outside(e); break; }
        t = real;
      }
      t += strcmp(root, "/") == 0 ? 0 : rn;
      while (depth > 0) close(fds[depth--]);
      phys[rn] = 0;
    }
    const char *pre = "";
    if (fast) {
      /* the link is in a level opened as several names at once, where `..`
         could not go back one name: walk those names again one by one */
      if (link[0] != '/') pre = fast;
      if (depth == 1) { close(fds[1]); depth = 0; phys[rn] = 0; }
    }
    size_t pl = strlen(pre), tl = strlen(t), rl = strlen(rest + at_i);
    char *nr = (char *)w_sb_alloc(pl + 1 + tl + 1 + rl + 1);
    memcpy(nr, pre, pl); nr[pl] = '/';
    memcpy(nr + pl + 1, t, tl); nr[pl + 1 + tl] = '/';
    memcpy(nr + pl + 1 + tl + 1, rest + at_i, rl + 1);
    free(fast); fast = NULL;
    free(real);
    free(rest);
    rest = nr; at_i = 0;
  }
  free(link); free(rest); free(fast);
  if (ok) {
    at->dfd = fds[depth];
    at->own = depth > 0 || !borrowed;
    at->dir = w_sb_strndup(phys, strlen(phys));
    depth--;
  }
  for (int i = depth; i >= 0; i--) if (i > 0 || !borrowed) close(fds[i]);
  free(fds); free(plen); free(phys);
  if (!ok) { w_fs_at_free(at); return -1; }
  return 0;
}

static void w_fs_sys(WFsErr *e, const char *rel, size_t len, int err) {
  e->kind = 2;
  snprintf(e->msg, sizeof e->msg, "fs: `%.*s`: %s", (int)len, rel, strerror(err));
}

/* Open a file for reading (W_FS_READ), writing from the start or appending;
   the descriptor, or -1 with e set. A directory is not read. */
enum { W_FS_READ, W_FS_WRITE, W_FS_APPEND };
static int w_fs_open(const char *top, const char *root, const char *rel, size_t len, int mode, WFsErr *e) {
  int how = mode == W_FS_READ ? W_FS_FOLLOW : (W_FS_FOLLOW | W_FS_CREATE);
  int flags = mode == W_FS_READ ? O_RDONLY : mode == W_FS_WRITE ? (O_WRONLY | O_CREAT | O_TRUNC) : (O_WRONLY | O_CREAT | O_APPEND);
  for (int tries = 0;; tries++) {
    WFsAt at;
    if (w_fs_at(top, root, rel, len, how, &at, e) != 0) return -1;
    int fd = openat(at.dfd, at.name, flags | O_NOFOLLOW | O_CLOEXEC | O_NOCTTY, 0666);
    int err = errno;
    if (fd < 0 && err == ELOOP && tries < 8) { w_fs_at_free(&at); continue; } /* became a link: walk again */
    if (fd >= 0 && mode == W_FS_READ) {
      struct stat st;
      if (fstat(fd, &st) == 0 && S_ISDIR(st.st_mode)) { close(fd); fd = -1; err = EISDIR; }
    }
    if (fd < 0) {
      e->kind = 2;
      if (err == ENOENT && mode == W_FS_READ) w_fs_missing(e, 0, rel, len);
      else snprintf(e->msg, sizeof e->msg, "%s: %s", at.shown, strerror(err));
    }
    w_fs_at_free(&at);
    return fd;
  }
}
/* write all of data; 0, or an errno */
static int w_fs_write_all(int fd, const char *data, size_t n) {
  while (n > 0) {
    ssize_t w = write(fd, data, n);
    if (w < 0 && errno == EINTR) continue;
    if (w <= 0) return w < 0 ? errno : EIO;
    data += w; n -= (size_t)w;
  }
  return 0;
}

/* A directory, open for listing; NULL with e set. */
static DIR *w_fs_opendir(const char *top, const char *root, const char *rel, size_t len, WFsErr *e) {
  for (int tries = 0;; tries++) {
    WFsAt at;
    if (w_fs_at(top, root, rel, len, W_FS_FOLLOW | W_FS_EXIST, &at, e) != 0) return NULL;
    int fd = openat(at.dfd, at.name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    int err = errno;
    w_fs_at_free(&at);
    if (fd < 0 && err == ELOOP && tries < 8) continue;
    DIR *d = fd >= 0 ? fdopendir(fd) : NULL;
    if (!d) {
      if (fd >= 0) close(fd);
      e->kind = 2;
      snprintf(e->msg, sizeof e->msg, "fs: `%.*s` is not a directory", (int)len, rel);
    }
    return d;
  }
}

/* What a path names, following links; -1 with e set when it does not exist. */
static int w_fs_stat(const char *top, const char *root, const char *rel, size_t len, struct stat *st, WFsErr *e) {
  for (int tries = 0;; tries++) {
    WFsAt at;
    if (w_fs_at(top, root, rel, len, W_FS_FOLLOW | W_FS_EXIST, &at, e) != 0) return -1;
    int rc = fstatat(at.dfd, at.name, st, AT_SYMLINK_NOFOLLOW);
    int err = errno;
    w_fs_at_free(&at);
    if (rc == 0 && S_ISLNK(st->st_mode) && tries < 8) continue;
    if (rc != 0) { w_fs_sys(e, rel, len, err); return -1; }
    return 0;
  }
}

/* The entry a path names, not following a final link, after checking the
   path as the operation always has (a link out of the root is refused even
   though only the link itself is touched). */
static int w_fs_entry(const char *top, const char *root, const char *rel, size_t len, int check, WFsAt *at, WFsErr *e) {
  WFsAt c;
  if (w_fs_at(top, root, rel, len, check, &c, e) != 0) return -1;
  w_fs_at_free(&c);
  return w_fs_at(top, root, rel, len, check & W_FS_CREATE, at, e);
}

/* A file or an empty directory; never the root. */
static int w_fs_remove(const char *top, const char *root, const char *rel, size_t len, WFsErr *e) {
  WFsAt at;
  if (w_fs_entry(top, root, rel, len, W_FS_FOLLOW | W_FS_EXIST, &at, e) != 0) return -1;
  if (at.is_root) { w_fs_at_free(&at); e->kind = 1; snprintf(e->msg, sizeof e->msg, "fs: cannot remove the granted directory itself"); return -1; }
  struct stat st;
  int rc = fstatat(at.dfd, at.name, &st, AT_SYMLINK_NOFOLLOW);
  if (rc == 0) rc = unlinkat(at.dfd, at.name, S_ISDIR(st.st_mode) ? AT_REMOVEDIR : 0);
  int err = errno;
  w_fs_at_free(&at);
  if (rc != 0) { w_fs_sys(e, rel, len, err); return -1; }
  return 0;
}

static int w_fs_mkdir(const char *top, const char *root, const char *rel, size_t len, WFsErr *e) {
  WFsAt at;
  if (w_fs_entry(top, root, rel, len, W_FS_FOLLOW | W_FS_CREATE, &at, e) != 0) return -1;
  int rc = mkdirat(at.dfd, at.name, 0755);
  int err = errno;
  w_fs_at_free(&at);
  if (rc != 0) { w_fs_sys(e, rel, len, err); return -1; }
  return 0;
}

/* Move `from` to `to` in one step, replacing what is at `to`; never the root. */
static int w_fs_rename(const char *top, const char *root, const char *from, size_t fl, const char *to, size_t tl, WFsErr *e) {
  WFsAt a, b;
  if (w_fs_entry(top, root, from, fl, W_FS_FOLLOW | W_FS_EXIST, &a, e) != 0) return -1;
  if (w_fs_entry(top, root, to, tl, W_FS_FOLLOW | W_FS_CREATE, &b, e) != 0) { w_fs_at_free(&a); return -1; }
  int rc = -1, err = 0;
  if (a.is_root || b.is_root) { e->kind = 1; snprintf(e->msg, sizeof e->msg, "fs: cannot rename the granted directory itself"); }
  else {
    rc = renameat(a.dfd, a.name, b.dfd, b.name);
    err = errno;
    if (rc != 0) w_fs_sys(e, from, fl, err);
  }
  w_fs_at_free(&a); w_fs_at_free(&b);
  return rc == 0 ? 0 : -1;
}

/* The absolute path of `rel`, checked as for writing (its directory must
   exist), for handing to C: the real directory and the last name, so no
   link on the way can be swapped later. malloc'd, or NULL with e set. */
static char *w_fs_full_path(const char *top, const char *root, const char *rel, size_t len, WFsErr *e) {
  WFsAt at;
  if (w_fs_entry(top, root, rel, len, W_FS_FOLLOW | W_FS_CREATE, &at, e) != 0) return NULL;
  char *p;
  if (at.is_root) p = w_sb_strndup(root, strlen(root));
  else {
    size_t n = strlen(at.dir) + strlen(at.name) + 2;
    p = (char *)w_sb_alloc(n);
    snprintf(p, n, "%s%s%s", at.dir, strcmp(at.dir, "/") == 0 ? "" : "/", at.name);
  }
  w_fs_at_free(&at);
  return p;
}

/* The root of `under(rel)`: the real path it names now, or (when it does
   not exist yet) the path itself; NULL when it is not inside the root. */
static char *w_fs_under(const char *top, const char *root, const char *rel, size_t len) {
  WFsErr e;
  WFsAt at;
  if (w_fs_at(top, root, rel, len, W_FS_FOLLOW, &at, &e) != 0) {
    if (e.kind == 1 || memchr(rel, 0, len)) return NULL;
    char *norm = w_fs_norm(rel, len, &e);
    if (!norm) return NULL;
    size_t n = strlen(root) + strlen(norm) + 2;
    char *p = (char *)w_sb_alloc(n);
    snprintf(p, n, norm[0] ? "%s%s%s" : "%s", root, strcmp(root, "/") == 0 ? "" : "/", norm);
    free(norm);
    return p;
  }
  char *p;
  if (strcmp(at.name, ".") == 0) p = w_sb_strndup(at.dir, strlen(at.dir));
  else {
    size_t n = strlen(at.dir) + strlen(at.name) + 2;
    p = (char *)w_sb_alloc(n);
    snprintf(p, n, "%s%s%s", at.dir, strcmp(at.dir, "/") == 0 ? "" : "/", at.name);
  }
  w_fs_at_free(&at);
  return p;
}
