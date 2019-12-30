/* vim: set ft=c: */
#ifndef LEX_H
#define LEX_H

#include <stdlib.h>

typedef enum TokenType {
    TOK_EOF,
    TOK_NEWLINE,
    TOK_COMMENT,
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

char *token_names[NUM_TOKENTYPES];

struct token_map {
	const char *text;
	TokenType type;
};

typedef struct {
    size_t start; // start position in the source
    size_t end;   // end position in the source
} SrcRange;

typedef struct Token {
    TokenType type;
    SrcRange range;
} Token;

typedef struct SrcLoc {
    int line;
    int col;
} SrcLoc;

typedef struct Lexer {
    Token tok;         // currently lexed token
    char ch;           // lookahead character
    long off;          // lookahead character offset
    long rd_off;       // next read character offset
    long line_off;     // current line offset
    size_t *line_offs; // byte offsets of '\n's
    int lines;         // number of lines
    long start;        // start of the last token literal
    char *filename;    // source filename
    char *src;         // source text
    long srclen;       // length of src excluding \0
} Lexer;

void token_free(Token *t);
void print_token(Lexer *l, const Token t);
int is_keyword(Token tok);
SrcLoc locate(Lexer *l, size_t pos);
int lexer_init(Lexer *l, const char *filename);
int lexer_next(Lexer *l);
void lexer_free(Lexer *l);

#endif
