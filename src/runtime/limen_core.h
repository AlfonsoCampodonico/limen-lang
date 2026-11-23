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
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

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

#endif
