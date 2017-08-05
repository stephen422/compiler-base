package main

import (
	"fmt"
	"os"
	"io/ioutil"
)

type Lexer struct {
	filename string
	src []byte

	// Current character offset
	off int
}

func New(filename string) *Lexer {
	src, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	return &Lexer{filename, src, 0}
}

func (l *Lexer) skipWhitespace() {
	for l.src[l.off] == ' ' || l.src[l.off] == '\n' || l.src[l.off] == '\t' {
		l.off++
	}
}

func (l *Lexer) debug() {
	fmt.Printf("Look: [%v] (%v/%v)\n", string(l.src[l.off]), l.off, len(l.src))
}

func main() {
	if (len(os.Args) < 2) {
		fmt.Fprintf(os.Stderr, "usage: %v FILE\n", os.Args[0])
		os.Exit(1)
	}

	l := New(os.Args[1])
	fmt.Println("Read finished!")
	l.debug()
	l.skipWhitespace()
	l.debug()
}
