// Points of a 4000x4000 grid inside the Mandelbrot set (500 iterations),
// rows shared out over one thread per core.
use std::sync::atomic::{AtomicUsize, Ordering};

fn row_count(y: usize, size: usize, iters: usize) -> usize {
    let ci = 2.0 * y as f64 / size as f64 - 1.0;
    let mut count = 0;
    for x in 0..size {
        let cr = 2.0 * x as f64 / size as f64 - 1.5;
        let (mut zr, mut zi) = (0.0f64, 0.0f64);
        let mut inside = true;
        for _ in 0..iters {
            let zr2 = zr * zr;
            let zi2 = zi * zi;
            if zr2 + zi2 > 4.0 {
                inside = false;
                break;
            }
            zi = 2.0 * zr * zi + ci;
            zr = zr2 - zi2 + cr;
        }
        if inside {
            count += 1;
        }
    }
    count
}

fn main() {
    let size = 4000;
    let next = AtomicUsize::new(0);
    let threads = std::thread::available_parallelism().map_or(1, |n| n.get());
    let total: usize = std::thread::scope(|s| {
        let workers: Vec<_> = (0..threads)
            .map(|_| {
                s.spawn(|| {
                    let mut sum = 0;
                    loop {
                        let y = next.fetch_add(1, Ordering::Relaxed);
                        if y >= size {
                            return sum;
                        }
                        sum += row_count(y, size, 500);
                    }
                })
            })
            .collect();
        workers.into_iter().map(|w| w.join().unwrap()).sum()
    });
    println!("{}", total);
}
