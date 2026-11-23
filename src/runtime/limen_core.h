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

#endif
