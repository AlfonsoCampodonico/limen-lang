// Longest Collatz chain for a starting value below 5 million, in 50 chunks
// of 100k numbers, one thread each.
fn chain(n: i64) -> i64 {
    let mut x = n;
    let mut steps = 0;
    while x != 1 {
        x = if x % 2 == 0 { x / 2 } else { 3 * x + 1 };
        steps += 1;
    }
    steps
}

fn best(lo: i64, hi: i64) -> i64 {
    (lo..hi).map(chain).max().unwrap_or(0)
}

fn main() {
    let (limit, chunk) = (5_000_000i64, 100_000i64);
    let top = std::thread::scope(|s| {
        let tasks: Vec<_> = (1..limit)
            .step_by(chunk as usize)
            .map(|lo| s.spawn(move || best(lo, (lo + chunk).min(limit))))
            .collect();
        tasks.into_iter().map(|t| t.join().unwrap()).max().unwrap_or(0)
    });
    println!("{}", top);
}
