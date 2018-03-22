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

typedef struct Strbuf {
    char *s;
    long len;
} Strbuf;

#define strbuf_reset(sb) ((sb)->len = 0)

typedef struct Lexer {
    char *filename;
    char *src;
    long src_len;

    char ch;       // lookahead character
    long off;      // lookahead character offset
    long rd_off;   // file reading offset (one character after lookahead)
    long line_off; // current line offset
    Strbuf sb;     // current token in string
    long lit;      // start of the last token literal
} Lexer;

int lexer_init(Lexer *l, const char *filename);
int lexer_next(Lexer *l);
void lexer_free(Lexer *l);

#endif
