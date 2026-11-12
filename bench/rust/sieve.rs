// Primes up to 100 million: a 100M-element Vec built with push, then
// mutated in place by index.
fn main() {
    let n: usize = 100_000_000;
    let mut is_prime = Vec::new();
    for _ in 0..=n {
        is_prime.push(true);
    }
    is_prime[0] = false;
    is_prime[1] = false;
    let mut p = 2;
    while p * p <= n {
        if is_prime[p] {
            let mut m = p * p;
            while m <= n {
                is_prime[m] = false;
                m += p;
            }
        }
        p += 1;
    }
    println!("{}", is_prime.iter().filter(|&&b| b).count());
}
