// filter / map / fold with closures over a 3M-element Vec, 30 rounds.
// Each stage builds a new Vec, as the Limen version does.
fn main() {
    let mut xs = Vec::new();
    for i in 0..3_000_000i64 {
        xs.push(i);
    }
    let mut total: i64 = 0;
    for round in 0..30i64 {
        let k = round;
        let evens: Vec<i64> = xs.iter().copied().filter(|x| x % 2 == 0).collect();
        let mapped: Vec<i64> = evens.iter().map(|x| (x * x + k) % 1000).collect();
        total += mapped.iter().fold(0, |a, b| a + b);
    }
    println!("{}", total);
}
