#ifndef LEX_H
#define LEX_H

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
    TOK_DASH,
    TOK_ARROW,

    // Keywords
    TOK_FN,
    TOK_LET,
    TOK_VAR,
    TOK_RETURN,
    TOK_INT,
    TOK_IF,
    TOK_ELSE,
    TOK_FOR,

    TOK_ERR
} TokenType;

typedef struct Token {
    TokenType type;
    long pos;
    char *lit;
    long litlen;
} Token;

typedef struct Strbuf {
    char *s;
    long len;
} Strbuf;

#define strbuf_reset(sb) ((sb)->len = 0)

typedef struct Lexer {
    char ch;        // lookahead character
    long off;       // lookahead character offset
    long line_off;  // current line offset
    Strbuf sb;      // current token string
    long start;       // start of the last token literal
    char *filename; // source filename
    char *src;      // buffer holding source file contents
    long srclen;    // length of src excluding \0
} Lexer;

void token_free(Token *t);

int lexer_init(Lexer *l, const char *filename);
Token *lexer_next(Lexer *l);
void lexer_free(Lexer *l);

#endif
