/* Hand-parallelised C: rows handed out through an atomic counter. */
#include <stdio.h>
#include <stdint.h>
#include <pthread.h>
#include <unistd.h>

static int64_t size = 4000, next_row = 0, total = 0;

static int64_t row_count(int64_t y, int64_t iters) {
  double ci = 2.0 * (double)y / (double)size - 1.0;
  int64_t count = 0;
  for (int64_t x = 0; x < size; x++) {
    double cr = 2.0 * (double)x / (double)size - 1.5, zr = 0, zi = 0;
    int inside = 1;
    for (int64_t i = 0; i < iters; i++) {
      double zr2 = zr * zr, zi2 = zi * zi;
      if (zr2 + zi2 > 4.0) { inside = 0; break; }
      zi = 2.0 * zr * zi + ci;
      zr = zr2 - zi2 + cr;
    }
    count += inside;
  }
  return count;
}

static void *worker(void *arg) {
  (void)arg;
  int64_t local = 0, y;
  while ((y = __atomic_fetch_add(&next_row, 1, __ATOMIC_RELAXED)) < size) local += row_count(y, 500);
  __atomic_fetch_add(&total, local, __ATOMIC_RELAXED);
  return NULL;
}

int main(void) {
  long n = sysconf(_SC_NPROCESSORS_ONLN);
  pthread_t th[256];
  for (long i = 0; i < n; i++) pthread_create(&th[i], NULL, worker, NULL);
  for (long i = 0; i < n; i++) pthread_join(th[i], NULL);
  printf("%lld\n", (long long)total);
  return 0;
}
