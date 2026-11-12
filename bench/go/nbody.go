// The classic n-body simulation over a slice of structs, updated in place.
package main

import (
	"fmt"
	"math"
)

type Body struct {
	x, y, z, vx, vy, vz, mass float64
}

const solar = 4.0 * 3.141592653589793 * 3.141592653589793
const days = 365.24

func body(x, y, z, vx, vy, vz, mass float64) Body {
	return Body{x, y, z, vx * days, vy * days, vz * days, mass * solar}
}

func advance(bs []Body, dt float64) {
	n := len(bs)
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			dx := bs[i].x - bs[j].x
			dy := bs[i].y - bs[j].y
			dz := bs[i].z - bs[j].z
			d2 := dx*dx + dy*dy + dz*dz
			mag := dt / (d2 * math.Sqrt(d2))
			mi := bs[i].mass * mag
			mj := bs[j].mass * mag
			bs[i].vx -= dx * mj
			bs[i].vy -= dy * mj
			bs[i].vz -= dz * mj
			bs[j].vx += dx * mi
			bs[j].vy += dy * mi
			bs[j].vz += dz * mi
		}
	}
	for i := range bs {
		bs[i].x += dt * bs[i].vx
		bs[i].y += dt * bs[i].vy
		bs[i].z += dt * bs[i].vz
	}
}

func energy(bs []Body) float64 {
	e := 0.0
	for i, b := range bs {
		e += 0.5 * b.mass * (b.vx*b.vx + b.vy*b.vy + b.vz*b.vz)
		for j := i + 1; j < len(bs); j++ {
			dx := b.x - bs[j].x
			dy := b.y - bs[j].y
			dz := b.z - bs[j].z
			e -= b.mass * bs[j].mass / math.Sqrt(dx*dx+dy*dy+dz*dz)
		}
	}
	return e
}

func main() {
	bs := []Body{
		body(0, 0, 0, 0, 0, 0, 1),
		body(4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01, 1.66007664274403694e-03, 7.69901118419740425e-03, -6.90460016972063023e-05, 9.54791938424326609e-04),
		body(8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01, -2.76742510726862411e-03, 4.99852801234917238e-03, 2.30417297573763929e-05, 2.85885980666130812e-04),
		body(1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01, 2.96460137564761618e-03, 2.37847173959480950e-03, -2.96589568540237556e-05, 4.36624404335156298e-05),
		body(1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01, 2.68067772490389322e-03, 1.62824170038242295e-03, -9.51592254519715870e-05, 5.15138902046611451e-05),
	}
	var px, py, pz float64
	for _, b := range bs {
		px += b.vx * b.mass
		py += b.vy * b.mass
		pz += b.vz * b.mass
	}
	bs[0].vx = 0 - px/solar
	bs[0].vy = 0 - py/solar
	bs[0].vz = 0 - pz/solar
	fmt.Println(energy(bs))
	for step := 0; step < 15000000; step++ {
		advance(bs, 0.01)
	}
	fmt.Println(energy(bs))
}
