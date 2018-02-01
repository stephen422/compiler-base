#ifndef LEX_H
#define LEX_H

typedef struct Lexer {
	char *filename;
	long srcSize;
	char *src;

	char *look;	// lookahead character
	int off;	// lookahead character offset
	int rdOff;	// file reading offset (one character after look)
} Lexer;

int lexer_init(Lexer *l, const char *filename);
void lexer_free(Lexer *l);

static void next(Lexer *l);

#endif
