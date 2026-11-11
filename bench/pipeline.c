/* Same computation as pipeline.vela, written the way C would: one fused loop. */
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

int main(void) {
  int64_t n = 3000000;
  int64_t *xs = malloc(sizeof(int64_t) * (size_t)n);
  for (int64_t i = 0; i < n; i++) xs[i] = i;
  int64_t total = 0;
  for (int64_t round = 0; round < 30; round++)
    for (int64_t i = 0; i < n; i++)
      if (xs[i] % 2 == 0) total += (xs[i] * xs[i] + round) % 1000;
  printf("%lld\n", (long long)total);
  free(xs);
  return 0;
}
