// Points of a 4000x4000 grid inside the Mandelbrot set (500 iterations).
// One goroutine per row, as Limen starts one task per row.
package main

import (
	"fmt"
	"sync"
)

func rowCount(y, size, iters int) int {
	ci := 2.0*float64(y)/float64(size) - 1.0
	count := 0
	for x := 0; x < size; x++ {
		cr := 2.0*float64(x)/float64(size) - 1.5
		zr, zi := 0.0, 0.0
		inside := true
		for i := 0; i < iters; i++ {
			zr2 := zr * zr
			zi2 := zi * zi
			if zr2+zi2 > 4.0 {
				inside = false
				break
			}
			zi = 2.0*zr*zi + ci
			zr = zr2 - zi2 + cr
		}
		if inside {
			count++
		}
	}
	return count
}

func main() {
	size := 4000
	counts := make([]int, size)
	var wg sync.WaitGroup
	for y := 0; y < size; y++ {
		wg.Add(1)
		go func(row int) {
			defer wg.Done()
			counts[row] = rowCount(row, size, 500)
		}(y)
	}
	wg.Wait()
	total := 0
	for _, c := range counts {
		total += c
	}
	fmt.Println(total)
}
