package main

import (
	"fmt"
	"os"
	"compiler-base-go/lexer"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "usage: %v FILE\n", os.Args[0])
		os.Exit(1)
	}

	l := lexer.New(os.Args[1])
	for {
		tok, lit := l.Lex()
		if tok == lexer.EOF {
			break
		}
		fmt.Printf("[%v]\n", lit)
	}
}
