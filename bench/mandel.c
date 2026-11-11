#include <stdio.h>
#include <stdint.h>

static int64_t row_count(int64_t y, int64_t size, int64_t iters) {
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

int main(void) {
  int64_t size = 4000, total = 0;
  for (int64_t y = 0; y < size; y++) total += row_count(y, size, 500);
  printf("%lld\n", (long long)total);
  return 0;
}
