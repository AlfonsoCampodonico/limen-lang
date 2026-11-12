// Primes up to 100 million: a 100M-element slice built with append, then
// mutated in place by index.
package main

import "fmt"

func main() {
	n := 100000000
	var isPrime []bool
	for i := 0; i <= n; i++ {
		isPrime = append(isPrime, true)
	}
	isPrime[0] = false
	isPrime[1] = false
	for p := 2; p*p <= n; p++ {
		if isPrime[p] {
			for m := p * p; m <= n; m += p {
				isPrime[m] = false
			}
		}
	}
	count := 0
	for _, b := range isPrime {
		if b {
			count++
		}
	}
	fmt.Println(count)
}
