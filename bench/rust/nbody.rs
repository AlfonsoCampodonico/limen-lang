// The classic n-body simulation over a Vec of structs, updated in place.
#[derive(Clone, Copy)]
struct Body {
    x: f64,
    y: f64,
    z: f64,
    vx: f64,
    vy: f64,
    vz: f64,
    mass: f64,
}

const SOLAR: f64 = 4.0 * 3.141592653589793 * 3.141592653589793;
const DAYS: f64 = 365.24;

fn body(x: f64, y: f64, z: f64, vx: f64, vy: f64, vz: f64, mass: f64) -> Body {
    Body { x, y, z, vx: vx * DAYS, vy: vy * DAYS, vz: vz * DAYS, mass: mass * SOLAR }
}

fn advance(bs: &mut [Body], dt: f64) {
    let n = bs.len();
    for i in 0..n {
        for j in i + 1..n {
            let dx = bs[i].x - bs[j].x;
            let dy = bs[i].y - bs[j].y;
            let dz = bs[i].z - bs[j].z;
            let d2 = dx * dx + dy * dy + dz * dz;
            let mag = dt / (d2 * d2.sqrt());
            let mi = bs[i].mass * mag;
            let mj = bs[j].mass * mag;
            bs[i].vx -= dx * mj;
            bs[i].vy -= dy * mj;
            bs[i].vz -= dz * mj;
            bs[j].vx += dx * mi;
            bs[j].vy += dy * mi;
            bs[j].vz += dz * mi;
        }
    }
    for b in bs.iter_mut() {
        b.x += dt * b.vx;
        b.y += dt * b.vy;
        b.z += dt * b.vz;
    }
}

fn energy(bs: &[Body]) -> f64 {
    let mut e = 0.0;
    for i in 0..bs.len() {
        let b = bs[i];
        e += 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz);
        for j in i + 1..bs.len() {
            let dx = b.x - bs[j].x;
            let dy = b.y - bs[j].y;
            let dz = b.z - bs[j].z;
            e -= b.mass * bs[j].mass / (dx * dx + dy * dy + dz * dz).sqrt();
        }
    }
    e
}

fn main() {
    let mut bs = vec![
        body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),
        body(4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01, 1.66007664274403694e-03, 7.69901118419740425e-03, -6.90460016972063023e-05, 9.54791938424326609e-04),
        body(8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01, -2.76742510726862411e-03, 4.99852801234917238e-03, 2.30417297573763929e-05, 2.85885980666130812e-04),
        body(1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01, 2.96460137564761618e-03, 2.37847173959480950e-03, -2.96589568540237556e-05, 4.36624404335156298e-05),
        body(1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01, 2.68067772490389322e-03, 1.62824170038242295e-03, -9.51592254519715870e-05, 5.15138902046611451e-05),
    ];
    let (mut px, mut py, mut pz) = (0.0, 0.0, 0.0);
    for b in &bs {
        px += b.vx * b.mass;
        py += b.vy * b.mass;
        pz += b.vz * b.mass;
    }
    bs[0].vx = 0.0 - px / SOLAR;
    bs[0].vy = 0.0 - py / SOLAR;
    bs[0].vz = 0.0 - pz / SOLAR;
    println!("{}", energy(&bs));
    for _ in 0..15_000_000 {
        advance(&mut bs, 0.01);
    }
    println!("{}", energy(&bs));
}
