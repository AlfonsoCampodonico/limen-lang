#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

int main(void) {
  int64_t n = 100000000;
  bool *is_prime = malloc((size_t)(n + 1));
  for (int64_t i = 0; i <= n; i++) is_prime[i] = true;
  is_prime[0] = is_prime[1] = false;
  for (int64_t p = 2; p * p <= n; p++)
    if (is_prime[p])
      for (int64_t m = p * p; m <= n; m += p) is_prime[m] = false;
  int64_t count = 0;
  for (int64_t i = 0; i <= n; i++) count += is_prime[i];
  printf("%lld\n", (long long)count);
  free(is_prime);
  return 0;
}
