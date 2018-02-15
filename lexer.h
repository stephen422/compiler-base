#ifndef LEX_H
#define LEX_H

typedef enum token {
	TOK_EOF,
	TOK_NUM,
	TOK_IDENT,
	TOK_STRING,
	TOK_DOT,
	TOK_HASH,
	TOK_LT,
	TOK_ERR
} Token;

typedef struct Lexer {
	char *filename;
	char *src;
	long src_len;

	char ch;	/* lookahead character */
	long off;	/* lookahead character offset */
	long rd_off;	/* file reading offset (one character after look) */
	long line_off;	/* current line offset */
} Lexer;

int lexer_init(Lexer *l, const char *filename);
Token lexer_next(Lexer *l, char **lit, int *lit_len);
void lexer_free(Lexer *l);

#endif
