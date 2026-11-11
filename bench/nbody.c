#include <stdio.h>
#include <math.h>

typedef struct { double x, y, z, vx, vy, vz, mass; } Body;

#define PI 3.141592653589793
#define SOLAR (4.0 * PI * PI)
#define DAYS 365.24

static Body body(double x, double y, double z, double vx, double vy, double vz, double mass) {
  Body b = { x, y, z, vx * DAYS, vy * DAYS, vz * DAYS, mass * SOLAR };
  return b;
}

static void advance(Body *bs, int n, double dt) {
  for (int i = 0; i < n; i++)
    for (int j = i + 1; j < n; j++) {
      double dx = bs[i].x - bs[j].x, dy = bs[i].y - bs[j].y, dz = bs[i].z - bs[j].z;
      double d2 = dx * dx + dy * dy + dz * dz;
      double mag = dt / (d2 * sqrt(d2));
      double mi = bs[i].mass * mag, mj = bs[j].mass * mag;
      bs[i].vx -= dx * mj; bs[i].vy -= dy * mj; bs[i].vz -= dz * mj;
      bs[j].vx += dx * mi; bs[j].vy += dy * mi; bs[j].vz += dz * mi;
    }
  for (int i = 0; i < n; i++) { bs[i].x += dt * bs[i].vx; bs[i].y += dt * bs[i].vy; bs[i].z += dt * bs[i].vz; }
}

static double energy(Body *bs, int n) {
  double e = 0.0;
  for (int i = 0; i < n; i++) {
    Body b = bs[i];
    e = e + 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz);
    for (int j = i + 1; j < n; j++) {
      double dx = b.x - bs[j].x, dy = b.y - bs[j].y, dz = b.z - bs[j].z;
      e = e - b.mass * bs[j].mass / sqrt(dx * dx + dy * dy + dz * dz);
    }
  }
  return e;
}

int main(void) {
  Body bs[5] = {
    body(0, 0, 0, 0, 0, 0, 1),
    body(4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01, 1.66007664274403694e-03,
         7.69901118419740425e-03, -6.90460016972063023e-05, 9.54791938424326609e-04),
    body(8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01, -2.76742510726862411e-03,
         4.99852801234917238e-03, 2.30417297573763929e-05, 2.85885980666130812e-04),
    body(1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01, 2.96460137564761618e-03,
         2.37847173959480950e-03, -2.96589568540237556e-05, 4.36624404335156298e-05),
    body(1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01, 2.68067772490389322e-03,
         1.62824170038242295e-03, -9.51592254519715870e-05, 5.15138902046611451e-05),
  };
  double px = 0, py = 0, pz = 0;
  for (int i = 0; i < 5; i++) { px += bs[i].vx * bs[i].mass; py += bs[i].vy * bs[i].mass; pz += bs[i].vz * bs[i].mass; }
  bs[0].vx = 0.0 - px / SOLAR; bs[0].vy = 0.0 - py / SOLAR; bs[0].vz = 0.0 - pz / SOLAR;
  printf("%.15g\n", energy(bs, 5));
  for (int s = 0; s < 15000000; s++) advance(bs, 5, 0.01);
  printf("%.15g\n", energy(bs, 5));
  return 0;
}
