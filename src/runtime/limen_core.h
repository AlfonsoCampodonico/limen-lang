/* Limen C runtime: the built-ins written once. The native runtime
   (limen_rt.c) and the interpreter (src/core_stubs.c, declared to OCaml in
   src/core.ml) both call these functions, so the two backends cannot give
   different answers.

   What belongs here: a built-in whose answer depends on bytes, Unicode,
   parsing, float formatting, integer edge cases, scalar order, random
   numbers or a panic message, where two separate definitions could drift
   apart. Simple built-ins with nothing to get wrong stay written twice.

   The plain-data rule: these functions take plain data only, a
   (const char *, size_t) pair, int64_t, uint64_t, double or a buffer the
   caller supplies. They know nothing of the runtime's WStr or of OCaml
   values, hold no state, and nothing they allocate outlives the call. They
   use lengths, never strlen, so a NUL byte is a byte like any other. Each is
   LC_FN (static inline, so one a program does not use never warns) and
   named lc_.

   The runtime text pastes this file between limen_sql.h and limen_rt.c;
   the pieces of a split build never include it, as they call the runtime's
   Str_ and Float_ functions, never these. */
#ifndef LIMEN_CORE_H
#define LIMEN_CORE_H
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#if defined(__linux__)
#include <sys/random.h>
#endif

#define LC_FN static inline

/* Str.lines: the next line of s[0..n). *pos starts at 0; each call puts one
   line's start and length in *start and *len and returns true, or returns
   false when there are none left. A line ends at a \n or at the end, and
   one trailing \r is dropped from it; a last line left empty (by a final
   \n, or by a final \r stripped) is not a line (SPEC.md, `lines()`). */
LC_FN bool lc_lines_next(const char *s, size_t n, size_t *pos, size_t *start, size_t *len) {
  size_t from = *pos;
  if (from > n) return false;
  size_t end = from;
  while (end < n && s[end] != '\n') end++;
  size_t stop = end;
  if (stop > from && s[stop - 1] == '\r') stop--;
  if (end == n && stop == from) return false;
  *start = from;
  *len = stop - from;
  *pos = end + 1;
  return true;
}

/* Str.split: the next piece of s[0..n) split on sep[0..m), walked as
   lc_lines_next. An empty sep gives one piece per byte (none for an empty
   s); otherwise the pieces between non-overlapping occurrences of sep, left
   to right, and always at least one. *pos is SIZE_MAX after the last piece. */
LC_FN bool lc_split_next(const char *s, size_t n, const char *sep, size_t m, size_t *pos, size_t *start, size_t *len) {
  size_t from = *pos;
  if (from == SIZE_MAX) return false;
  if (m == 0) {
    if (from >= n) { *pos = SIZE_MAX; return false; }
    *start = from;
    *len = 1;
    *pos = from + 1;
    return true;
  }
  for (size_t i = from; m <= n && i <= n - m; i++)
    if (memcmp(s + i, sep, m) == 0) {
      *start = from;
      *len = i - from;
      *pos = i + m;
      return true;
    }
  *start = from;
  *len = n - from;
  *pos = SIZE_MAX;
  return true;
}

/* A byte Str.trim and Str.parse_int skip: space, \t, \n, \r or \f. */
LC_FN bool lc_is_space(char c) { return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'; }

/* Str.trim: the bounds [*a, *b) of s[0..n) without its leading and
   trailing spaces (lc_is_space). */
LC_FN void lc_trim(const char *s, size_t n, size_t *a, size_t *b) {
  size_t i = 0, j = n;
  while (i < j && lc_is_space(s[i])) i++;
  while (j > i && lc_is_space(s[j - 1])) j--;
  *a = i;
  *b = j;
}

/* The length of the UTF-8 character at s[i] (i < n), or 1 for a byte that
   does not start a well-formed one. Str.chars walks a string with it. */
LC_FN size_t lc_utf8_len(const unsigned char *s, size_t i, size_t n) {
  unsigned char b = s[i];
  size_t k;
  unsigned char lo = 0x80, hi = 0xBF;
  if (b < 0x80) return 1;
  else if (b >= 0xC2 && b <= 0xDF) k = 2;
  else if (b >= 0xE0 && b <= 0xEF) { k = 3; if (b == 0xE0) lo = 0xA0; if (b == 0xED) hi = 0x9F; }
  else if (b >= 0xF0 && b <= 0xF4) { k = 4; if (b == 0xF0) lo = 0x90; if (b == 0xF4) hi = 0x8F; }
  else return 1;
  if (k > n - i) return 1;
  if (s[i + 1] < lo || s[i + 1] > hi) return 1;
  for (size_t j = 2; j < k; j++) if (s[i + j] < 0x80 || s[i + j] > 0xBF) return 1;
  return k;
}

/* Int.to_char: the UTF-8 encoding of code point cp in out, returning its
   length; U+FFFD for a negative value, a surrogate or past 0x10FFFF. */
LC_FN size_t lc_utf8_encode(int64_t cp, char out[4]) {
  if (cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF)) cp = 0xFFFD;
  if (cp < 0x80) { out[0] = (char)cp; return 1; }
  if (cp < 0x800) { out[0] = (char)(0xC0 | (cp >> 6)); out[1] = (char)(0x80 | (cp & 0x3F)); return 2; }
  if (cp < 0x10000) {
    out[0] = (char)(0xE0 | (cp >> 12)); out[1] = (char)(0x80 | ((cp >> 6) & 0x3F)); out[2] = (char)(0x80 | (cp & 0x3F));
    return 3;
  }
  out[0] = (char)(0xF0 | (cp >> 18)); out[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
  out[2] = (char)(0x80 | ((cp >> 6) & 0x3F)); out[3] = (char)(0x80 | (cp & 0x3F));
  return 4;
}

/* Str.parse_int: a decimal integer with an optional sign, surrounding
   spaces (lc_trim) allowed, in range for Int. */
LC_FN bool lc_parse_int(const char *s, size_t n, int64_t *out) {
  size_t a, b;
  lc_trim(s, n, &a, &b);
  if (a == b) return false;
  bool neg = false;
  if (s[a] == '-' || s[a] == '+') { neg = s[a] == '-'; a++; }
  if (a == b) return false;
  uint64_t v = 0;
  for (size_t i = a; i < b; i++) {
    char c = s[i];
    if (c < '0' || c > '9') return false;
    if (v > (UINT64_MAX - (uint64_t)(c - '0')) / 10) return false;
    v = v * 10 + (uint64_t)(c - '0');
  }
  if (!neg && v > (uint64_t)INT64_MAX) return false;
  if (neg && v > (uint64_t)INT64_MAX + 1) return false;
  *out = neg ? (int64_t)(0 - v) : (int64_t)v;
  return true;
}

/* The length snprintf wrote into a buffer of cap bytes, given the k it
   returned: never more than cap - 1, so a caller cannot read past the end. */
LC_FN size_t lc_written(int k, size_t cap) {
  if (k < 0 || cap == 0) return 0;
  return (size_t)k < cap ? (size_t)k : cap - 1;
}

/* Float.to_str and a Float in interpolation: nan for any NaN (no sign),
   inf or -inf, one decimal for a whole number
   under 1e16, else 15 significant digits. LC_FLOAT_MAX bytes always hold
   it (the longest, such as "-1.23456789012346e-308", is 22 bytes and the
   NUL). Returns the length written, which a smaller cap cuts short. */
#define LC_FLOAT_MAX 32
LC_FN size_t lc_fmt_float(char *out, size_t cap, double x) {
  int k;
  if (isnan(x)) k = snprintf(out, cap, "nan"); /* %.15g would print "-nan" for a sign-bit NaN on glibc */
  else if (x == floor(x) && fabs(x) < 1e16) k = snprintf(out, cap, "%.1f", x);
  else k = snprintf(out, cap, "%.15g", x);
  return lc_written(k, cap);
}

/* Float.to_fixed: nan, inf or -inf, else d decimals (clamped to 0..30)
   rounded as printf does. LC_FIXED_MAX bytes always hold it: -DBL_MAX with
   30 decimals is 341 bytes. Returns the length written, which a smaller
   cap cuts short. */
#define LC_FIXED_MAX 352
LC_FN size_t lc_to_fixed(char *out, size_t cap, double x, int64_t d) {
  int k;
  if (isnan(x)) k = snprintf(out, cap, "nan");
  else if (isinf(x)) k = snprintf(out, cap, x > 0 ? "inf" : "-inf");
  else {
    if (d < 0) d = 0;
    if (d > 30) d = 30;
    k = snprintf(out, cap, "%.*f", (int)d, x);
  }
  return lc_written(k, cap);
}

/* Str.parse_float: a decimal float, digits with an optional sign, point and
   exponent, surrounding spaces (lc_trim) allowed; no hex, nan or inf. Any
   length: strtod needs a NUL-terminated copy, on the stack up to 407 bytes
   and from malloc past that. LC_FLOAT_OOM when that malloc fails, which
   the caller turns into its out-of-memory failure, never into None. */
typedef enum { LC_FLOAT_OK, LC_FLOAT_BAD, LC_FLOAT_OOM } lc_float_parse;
LC_FN lc_float_parse lc_parse_float(const char *s, size_t n, double *out) {
  size_t a, b;
  lc_trim(s, n, &a, &b);
  if (a == b) return LC_FLOAT_BAD;
  bool digit = false;
  for (size_t i = a; i < b; i++) {
    char c = s[i];
    if (c >= '0' && c <= '9') digit = true;
    else if (!(c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E')) return LC_FLOAT_BAD;
  }
  if (!digit) return LC_FLOAT_BAD;
  size_t len = b - a;
  char small[408];
  char *t = len < sizeof small ? small : (char *)malloc(len + 1);
  if (!t) return LC_FLOAT_OOM;
  memcpy(t, s + a, len);
  t[len] = 0;
  char *end;
  double x = strtod(t, &end);
  bool ok = end == t + len;
  if (t != small) free(t);
  if (!ok) return LC_FLOAT_BAD;
  *out = x;
  return LC_FLOAT_OK;
}

/* Float's total order, for sort and for Floats inside containers: NaN
   first and equal to itself, -0.0 equal to 0.0. -1, 0 or 1. */
LC_FN int lc_float_cmp(double a, double b) {
  if (a != a) return b != b ? 0 : -1;
  if (b != b) return 1;
  return a < b ? -1 : (a > b ? 1 : 0);
}

/* Str and Bytes order: byte by byte as unsigned, then the shorter first.
   -1, 0 or 1. */
LC_FN int lc_bytes_cmp(const char *a, size_t an, const char *b, size_t bn) {
  int c = memcmp(a, b, an < bn ? an : bn);
  if (c) return c < 0 ? -1 : 1;
  return an < bn ? -1 : (an > bn ? 1 : 0);
}

/* A Str inside a container (SPEC.md, How values print): quoted, with ",
   \, newline, tab, \r and \b escaped and any other byte outside 32..126
   as \ddd, three decimal digits. lc_escaped_len is its length, quotes
   included; lc_escape writes exactly that many bytes to out, no NUL. */
LC_FN size_t lc_escaped_len(const char *s, size_t n) {
  size_t k = 2;
  for (size_t i = 0; i < n; i++) {
    unsigned char c = (unsigned char)s[i];
    if (c == '"' || c == '\\' || c == '\n' || c == '\t' || c == '\r' || c == '\b') k += 2;
    else if (c >= 32 && c <= 126) k += 1;
    else k += 4;
  }
  return k;
}
LC_FN void lc_escape(const char *s, size_t n, char *out) {
  size_t k = 0;
  out[k++] = '"';
  for (size_t i = 0; i < n; i++) {
    unsigned char c = (unsigned char)s[i];
    char e = 0;
    switch (c) {
      case '"': e = '"'; break;
      case '\\': e = '\\'; break;
      case '\n': e = 'n'; break;
      case '\t': e = 't'; break;
      case '\r': e = 'r'; break;
      case '\b': e = 'b'; break;
      default: break;
    }
    if (e) { out[k++] = '\\'; out[k++] = e; }
    else if (c >= 32 && c <= 126) out[k++] = (char)c;
    else {
      out[k++] = '\\';
      out[k++] = (char)('0' + c / 100);
      out[k++] = (char)('0' + c / 10 % 10);
      out[k++] = (char)('0' + c % 10);
    }
  }
  out[k] = '"';
}

/* Rand with --seed: splitmix64. The state stays with each backend (an
   atomic add natively, a ref in the interpreter) and only ever advances by
   LC_GOLDEN; lc_mix64 turns the advanced state into the output. */
#define LC_GOLDEN 0x9E3779B97F4A7C15ULL
LC_FN uint64_t lc_mix64(uint64_t z) {
  z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
  z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
  return z ^ (z >> 31);
}

/* Rand.int without --seed rejects the draws below this, 2^64 mod span, so
   every value of the span is as likely as the others. */
LC_FN uint64_t lc_rand_skip(uint64_t span) { return -span % span; }

/* Rand.float: the top 53 bits of a draw, in [0, 1). */
LC_FN double lc_rand_unit(uint64_t z) { return (double)(z >> 11) * 0x1.0p-53; }

/* Rand.bytes with --seed: the low k <= 8 bytes of a draw, low byte first. */
LC_FN void lc_rand_le(uint64_t z, unsigned char *p, size_t k) {
  for (size_t j = 0; j < k && j < 8; j++) p[j] = (unsigned char)(z >> (8 * j));
}

/* n bytes from the operating system's cryptographically secure generator:
   arc4random_buf on macOS, getrandom on Linux, /dev/urandom where that is
   missing. 0, or -1 if the generators gave fewer bytes. */
LC_FN int lc_os_random(unsigned char *p, size_t n) {
#if defined(__APPLE__) || defined(__wasi__)
  /* WASI (the WebAssembly build): wasi-libc's arc4random_buf reads random_get */
  arc4random_buf(p, n);
  return 0;
#else
  size_t got = 0;
#if defined(__linux__)
  while (got < n) {
    ssize_t r = getrandom(p + got, n - got, 0);
    if (r < 0 && errno == EINTR) continue;
    if (r <= 0) break;
    got += (size_t)r;
  }
#endif
  if (got < n) {
    int fd = open("/dev/urandom", O_RDONLY | O_CLOEXEC);
    while (fd >= 0 && got < n) {
      ssize_t r = read(fd, p + got, n - got);
      if (r < 0 && errno == EINTR) continue;
      if (r <= 0) break;
      got += (size_t)r;
    }
    if (fd >= 0) close(fd);
  }
  return got == n ? 0 : -1;
#endif
}

/* Fs.info's mtime: a file's modification time in whole milliseconds. */
LC_FN int64_t lc_mtime_ms(const struct stat *st) {
#ifdef __APPLE__
  return (int64_t)st->st_mtimespec.tv_sec * 1000 + st->st_mtimespec.tv_nsec / 1000000;
#else
  return (int64_t)st->st_mtim.tv_sec * 1000 + st->st_mtim.tv_nsec / 1000000;
#endif
}

/* Panic messages, written once for both backends; each backend raises the
   panic and adds the location. snprintf-style: they write at most cap bytes
   (a NUL included, so cap 0 and a NULL out are fine for measuring) and
   return the full length. LC_MSG_MAX holds the numeric ones. */
#define LC_MSG_MAX 128
LC_FN size_t lc_msg_list_index(char *out, size_t cap, int64_t i, int64_t len) {
  int k = snprintf(out, cap, "index %lld is out of range for a list of length %lld", (long long)i, (long long)len);
  return k < 0 ? 0 : (size_t)k;
}
LC_FN size_t lc_msg_bytes_index(char *out, size_t cap, int64_t i, int64_t len) {
  int k = snprintf(out, cap, "index %lld is out of range for bytes of length %lld", (long long)i, (long long)len);
  return k < 0 ? 0 : (size_t)k;
}
LC_FN size_t lc_msg_byte_range(char *out, size_t cap, int64_t v) {
  int k = snprintf(out, cap, "byte %lld is out of range 0 to 255", (long long)v);
  return k < 0 ? 0 : (size_t)k;
}
/* A missing map key; key is the text the backend prints it as. */
LC_FN size_t lc_msg_key(char *out, size_t cap, const char *key, size_t kn) {
  static const char pre[] = "key ", post[] = " is not in the map";
  size_t n = 0;
  if (out && cap > 0) {
    size_t room = cap - 1;
    size_t a = sizeof pre - 1, b = kn, c = sizeof post - 1;
    size_t t;
    t = a < room ? a : room; memcpy(out, pre, t); n = t; room -= t;
    t = b < room ? b : room; if (t) memcpy(out + n, key, t); n += t; room -= t;
    t = c < room ? c : room; memcpy(out + n, post, t); n += t;
    out[n] = 0;
  }
  return sizeof pre - 1 + kn + sizeof post - 1;
}

#endif
