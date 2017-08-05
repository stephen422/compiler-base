package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"unicode/utf8"
)

const EOF = -1

type Lexer struct {
	filename string
	src      []byte
	look     rune // Lookahead character
	off      int  // Lookahead character offset
	rdOff    int  // File reading offset (one character after look)
}

func New(filename string) *Lexer {
	src, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	l := Lexer{filename, src, 0, 0, 0}
	l.next()
	return &l
}

func (l *Lexer) next() {
	if l.rdOff < len(l.src) {
		r, w := rune(l.src[l.rdOff]), 1
		if r >= utf8.RuneSelf {
			r, w := utf8.DecodeRune(l.src[l.rdOff:])
			if r == utf8.RuneError && w == 1 {
				fmt.Fprintf(os.Stderr, "error: invalid UTF-8 character in byte %v\n", l.off)
				os.Exit(1)
			}
		}
		l.off = l.rdOff
		l.look = r
		l.rdOff += w
	} else {
		l.off = len(l.src)
		l.look = EOF
	}
}

func (l *Lexer) LexIdent() {
}

func isAlnum(r rune) {
}

func (l *Lexer) skipWhitespace() {
	for l.look == ' ' || l.look == '\n' || l.look == '\t' {
		l.next()
	}
}

func (l *Lexer) debug() {
	fmt.Print("Look: ")
	if l.look == EOF {
		fmt.Println("EOF")
	} else {
		fmt.Printf("[%v] (%v/%v)\n", string(l.look), l.off, len(l.src))
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "usage: %v FILE\n", os.Args[0])
		os.Exit(1)
	}

	l := New(os.Args[1])
	fmt.Println("Read finished!")
	l.debug()
	l.skipWhitespace()
	l.debug()
}
