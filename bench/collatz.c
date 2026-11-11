#include <stdio.h>
#include <stdint.h>

static int64_t chain(int64_t n) {
  int64_t x = n, steps = 0;
  while (x != 1) { x = (x % 2 == 0) ? x / 2 : 3 * x + 1; steps++; }
  return steps;
}

int main(void) {
  int64_t top = 0;
  for (int64_t n = 1; n < 5000000; n++) { int64_t c = chain(n); if (c > top) top = c; }
  printf("%lld\n", (long long)top);
  return 0;
}
