// A million lines of output, with fmt.Println as written in most programs.
package main

import "fmt"

func main() {
	for i := 0; i < 1000000; i++ {
		fmt.Println("line", i)
	}
}
