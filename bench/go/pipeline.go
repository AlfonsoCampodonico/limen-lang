// filter / map / fold with closures over a 3M-element slice, 30 rounds.
// Each stage builds a new slice, as the Limen version does.
package main

import "fmt"

func filter(xs []int, keep func(int) bool) []int {
	var out []int
	for _, x := range xs {
		if keep(x) {
			out = append(out, x)
		}
	}
	return out
}

func mapInts(xs []int, f func(int) int) []int {
	out := make([]int, 0, len(xs))
	for _, x := range xs {
		out = append(out, f(x))
	}
	return out
}

func fold(xs []int, init int, f func(int, int) int) int {
	acc := init
	for _, x := range xs {
		acc = f(acc, x)
	}
	return acc
}

func main() {
	var xs []int
	for i := 0; i < 3000000; i++ {
		xs = append(xs, i)
	}
	total := 0
	for round := 0; round < 30; round++ {
		k := round
		evens := filter(xs, func(x int) bool { return x%2 == 0 })
		mapped := mapInts(evens, func(x int) int { return (x*x + k) % 1000 })
		total += fold(mapped, 0, func(a, b int) int { return a + b })
	}
	fmt.Println(total)
}
