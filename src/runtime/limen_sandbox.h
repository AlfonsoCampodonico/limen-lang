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
   - programs: starting another program only if Exec is granted. A program
     started through Exec inherits the same restrictions; it may also list
     (not read) the directories above the current one, as shells do.
   macOS also refuses Apple events and LaunchServices opens, so the process
   cannot have another application act for it. It still reaches the system's
   Mach services, as every process does; seccomp on Linux refuses x32 system
   calls and io_uring without Net/Listen.
   What stays in Limen, not the kernel: which hosts and ports Net and Listen
   allow, which programs Exec allows, which environment variables Env shows,
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
#include <sys/stat.h>

typedef struct {
  const char *fs_root; /* realpath of the Fs grant, or NULL */
  int fs_ro;
  int net;             /* Net or Listen granted */
  int exec;            /* Exec granted */
} WSandboxSpec;

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
  const char *sys[] = { "/usr", "/lib", "/lib64", "/lib32", "/bin", "/sbin", "/etc", "/sys", "/opt", NULL };
  int bad = 0;
  for (int i = 0; sys[i]; i++) bad |= w_ll_allow(rs, sys[i], rd, handled);
  /* /proc: only this process's own entries, so other processes' command
     lines and state stay hidden. /proc/self resolves to this process's
     directory now; a program started through Exec has its own, so with Exec
     all of /proc stays readable (Landlock still keeps ptrace-guarded entries
     such as another process's environ and memory out of reach). */
  bad |= w_ll_allow(rs, s->exec ? "/proc" : "/proc/self", rd, handled);
  bad |= w_ll_allow(rs, "/dev", W_LL_READ_FILE | W_LL_READ_DIR, handled);
  bad |= w_ll_allow(rs, "/dev/null", W_LL_READ_FILE | W_LL_WRITE_FILE | W_LL_TRUNCATE, handled);
  bad |= w_ll_allow(rs, "/dev/tty", W_LL_READ_FILE | W_LL_WRITE_FILE, handled);
  if (s->fs_root) bad |= w_ll_allow(rs, s->fs_root, s->fs_ro ? (W_LL_READ_FILE | W_LL_READ_DIR) : handled, handled);
  /* OpenSSL's trust store may be named by the environment */
  {
    char *cf = w_sb_real(getenv("SSL_CERT_FILE")), *cd = w_sb_real(getenv("SSL_CERT_DIR"));
    bad |= w_ll_allow(rs, cf, W_LL_READ_FILE, handled);
    bad |= w_ll_allow(rs, cd, W_LL_READ_FILE | W_LL_READ_DIR, handled);
    free(cf); free(cd);
  }
  if (s->exec) {
    char *tmp = w_sb_tmpdir();
    bad |= w_ll_allow(rs, tmp, handled, handled);
    free(tmp);
  }
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
