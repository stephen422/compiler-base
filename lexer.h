#ifndef LEX_H
#define LEX_H

#include <stdlib.h>

typedef enum TokenType {
    TOK_EOF,
    TOK_NUM,
    TOK_IDENT,
    TOK_STRING,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACE,
    TOK_RBRACE,
    TOK_LBRACKET,
    TOK_RBRACKET,
    TOK_LT,
    TOK_GT,
    TOK_DOT,
    TOK_COMMA,
    TOK_COLON,
    TOK_SEMICOLON,
    TOK_QUOTE,
    TOK_DOUBLEQUOTE,
    TOK_BACKTICK,
    TOK_EQUALS,
    TOK_PLUS,
    TOK_MINUS,
    TOK_STAR,
    TOK_AMPERSAND,
    TOK_CARET,
    TOK_TILDE,
    TOK_SLASH,
    TOK_BACKSLASH,
    TOK_PIPE,
    TOK_BANG,
    TOK_QUESTION,
    TOK_AT,
    TOK_HASH,
    TOK_DOLLAR,
    TOK_PERCENT,
    TOK_DASH,
    TOK_ARROW,

    // Keywords
    TOK_KEYWORDS,

    TOK_ASSIGN,
    TOK_FN,
    TOK_LET,
    TOK_VAR,
    TOK_RETURN,
    TOK_INT,
    TOK_IF,
    TOK_ELSE,
    TOK_FOR,

    TOK_ERR,
    NUM_TOKENTYPES
} TokenType;

static char *token_names[NUM_TOKENTYPES] = {
    [TOK_IDENT] = "identifier",
    [TOK_EQUALS] = "=",
    [TOK_BANG] = "!",
    [TOK_ERR] = "unknown"
};

typedef struct {
    TokenType type;
    size_t start;   // start position in the source
    size_t end;     // end position in the source
} token_t;

typedef struct {
    token_t token;  // currently lexed token
    char ch;        // lookahead character
    long off;       // lookahead character offset
    long line_off;  // current line offset
    long start;     // start of the last token literal
    char *filename; // source filename
    char *src;      // buffer holding source file contents
    long srclen;    // length of src excluding \0
} lexer_t;

void token_free(token_t *t);
void print_token(const lexer_t *l, const token_t *t);

int lexer_init(lexer_t *l, const char *filename);
int lexer_next(lexer_t *l);
void lexer_free(lexer_t *l);

#endif
