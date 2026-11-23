/* Limen C runtime: the built-ins written once that generated code inlines.
   Like limen_core.h, both the native runtime and the interpreter
   (src/core_stubs.c) use these, so the two backends cannot disagree. They
   live apart because the runtime's declarations (limen_rt.h) wrap them, and
   so this file comes ahead of limen_rt.h, in the runtime text and in the
   header each piece of a split build includes. It needs only <stdint.h>
   and <math.h>. */
#ifndef LIMEN_CORE_INLINE_H
#define LIMEN_CORE_INLINE_H
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <math.h>
#include <stdint.h>

#ifndef LC_FN
#define LC_FN static inline
#endif

/* Float to Int: NaN is 0, out of range saturates, else truncate. */
LC_FN int64_t lc_f2i(double x) {
  if (isnan(x)) return 0;
  if (x >= 9.2e18) return INT64_MAX;
  if (x <= -9.2e18) return INT64_MIN;
  return (int64_t)x;
}

/* A UInt (up to 64 bits) to Float: the nearest Float, ties to even (IEEE 754). */
LC_FN double lc_u2f(uint64_t x) { return (double)x; }

#endif
