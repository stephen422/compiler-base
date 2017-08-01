package main

import (
	"fmt"
	"os"
	"io/ioutil"
)

type Source []byte

func (s Source) print() {
	fmt.Print(string(s))
}

func main() {
	var src Source
	src, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		panic(err)
	}
	src.print()
	fmt.Println("Read finished!")
}
