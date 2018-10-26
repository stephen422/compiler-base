package lexer

import (
	"fmt"
	"io/ioutil"
	"os"
	"unicode"
	"unicode/utf8"
)

type Token int

const (
	IDENT Token = iota
	INTEGER
	DECIMAL
	STRING

	EOF = -1
)

// var keywords map[string]Token = {
// 	"fn": FUN
// }

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
		fmt.Fprintln(os.Stderr, err)
	}

	l := Lexer{filename, src, 0, 0, 0}
	l.next()
	l.skipWhitespace() // at the file beginning
	return &l
}

func (l *Lexer) next() {
	if l.rdOff < len(l.src) {
		r, w := rune(l.src[l.rdOff]), 1
		if r >= utf8.RuneSelf {
			r, w := utf8.DecodeRune(l.src[l.rdOff:])
			if r == utf8.RuneError && w == 1 {
				fmt.Fprintf(os.Stderr, "error: invalid UTF-8 character at byte %v\n", l.off)
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

func (l *Lexer) lexIdent() string {
	off := l.off
	for isLetter(l.look) || isDigit(l.look) {
		l.next()
	}
	return string(l.src[off:l.off])
}

func (l *Lexer) lexString() string {
	off := l.off
	l.next() // beginning quote
	for ; l.look != '"'; l.next() {
		if l.look == '\\' {
			l.next()
		}
	}
	l.next() // ending quote
	return string(l.src[off:l.off])
}

func (l *Lexer) lexNumber() (tok Token, lit string) {
	off := l.off
	l.skipNumbers()
	tok = INTEGER
	// Decimal
	if l.look == '.' {
		l.next()
		l.skipNumbers()
		tok = DECIMAL
	}
	lit = string(l.src[off:l.off])
	return
}

func (l *Lexer) skipNumbers() {
	for isDigit(l.look) {
		l.next()
	}
}

func isLetter(r rune) bool {
	return 'a' <= r && r <= 'z' || 'A' <= r && r <= 'Z' || r == '_' || r >= utf8.RuneSelf && unicode.IsLetter(r)
}

func isDigit(r rune) bool {
	return '0' <= r && r <= '9'
}

func (l *Lexer) skipWhitespace() {
	for l.look == ' ' || l.look == '\n' || l.look == '\t' {
		l.next()
	}
}

func (l *Lexer) Lex() (tok Token, lit string) {
	if isLetter(l.look) {
		tok = IDENT
		lit = l.lexIdent()
	} else if isDigit(l.look) {
		tok, lit = l.lexNumber()
	} else if l.look == '"' {
		tok = STRING
		lit = l.lexString()
	} else {
		fmt.Fprintf(os.Stderr, "unrecognized token: [%c]\n", l.look)
		os.Exit(1)
	}
	l.skipWhitespace()
	return
}

func (l *Lexer) debug() {
	fmt.Print("Look: ")
	if l.look == EOF {
		fmt.Println("EOF")
	} else {
		fmt.Printf("[%v] (%v/%v)\n", string(l.look), l.off, len(l.src))
	}
}
