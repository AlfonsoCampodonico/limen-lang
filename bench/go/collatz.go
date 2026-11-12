// Longest Collatz chain for a starting value below 5 million.
// 50 goroutines of 100k numbers each.
package main

import (
	"fmt"
	"sync"
)

func chain(n int) int {
	x, steps := n, 0
	for x != 1 {
		if x%2 == 0 {
			x /= 2
		} else {
			x = 3*x + 1
		}
		steps++
	}
	return steps
}

func best(lo, hi int) int {
	top := 0
	for n := lo; n < hi; n++ {
		if c := chain(n); c > top {
			top = c
		}
	}
	return top
}

func main() {
	limit, chunk := 5000000, 100000
	var tops []int
	var mu sync.Mutex
	var wg sync.WaitGroup
	for lo := 1; lo < limit; lo += chunk {
		hi := min(lo+chunk, limit)
		wg.Add(1)
		go func(a, b int) {
			defer wg.Done()
			t := best(a, b)
			mu.Lock()
			tops = append(tops, t)
			mu.Unlock()
		}(lo, hi)
	}
	wg.Wait()
	top := 0
	for _, t := range tops {
		top = max(top, t)
	}
	fmt.Println(top)
}
