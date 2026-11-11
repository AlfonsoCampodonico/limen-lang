/* Hand-parallelised C: chunks of 100k handed out through an atomic counter. */
#include <stdio.h>
#include <stdint.h>
#include <pthread.h>
#include <unistd.h>

static int64_t next_lo = 1, best_all = 0;

static int64_t chain(int64_t n) {
  int64_t x = n, steps = 0;
  while (x != 1) { x = (x % 2 == 0) ? x / 2 : 3 * x + 1; steps++; }
  return steps;
}

static void *worker(void *arg) {
  (void)arg;
  int64_t lo, top = 0;
  while ((lo = __atomic_fetch_add(&next_lo, 100000, __ATOMIC_RELAXED)) < 5000000) {
    int64_t hi = lo + 100000 < 5000000 ? lo + 100000 : 5000000;
    for (int64_t n = lo; n < hi; n++) { int64_t c = chain(n); if (c > top) top = c; }
  }
  int64_t cur = __atomic_load_n(&best_all, __ATOMIC_RELAXED);
  while (top > cur && !__atomic_compare_exchange_n(&best_all, &cur, top, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED)) {}
  return NULL;
}

int main(void) {
  long n = sysconf(_SC_NPROCESSORS_ONLN);
  pthread_t th[256];
  for (long i = 0; i < n; i++) pthread_create(&th[i], NULL, worker, NULL);
  for (long i = 0; i < n; i++) pthread_join(th[i], NULL);
  printf("%lld\n", (long long)best_all);
  return 0;
}
