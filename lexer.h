#ifndef LEX_H
#define LEX_H

typedef struct Lexer {
	char *filename;
	long src_len;
	char *src;

	char ch;	/* lookahead character */
	int off;	/* lookahead character offset */
	int rd_off;	/* file reading offset (one character after look) */
	int line_off;	/* current line offset */
} Lexer;

int lexer_init(Lexer *l, const char *filename);
void lexer_next(Lexer *l, int *tok);
void lexer_free(Lexer *l);

#endif
