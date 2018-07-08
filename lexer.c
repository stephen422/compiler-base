#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "lexer.h"

struct SymbolMap {
    const char *text;
    TokenType type;
} symbols[] = {
    {"->", TOK_ARROW},
    {"/", TOK_SLASH},
    {"(", TOK_LPAREN},
    {")", TOK_RPAREN},
    {"{", TOK_LBRACE},
    {"}", TOK_RBRACE},
    {"[", TOK_LBRACKET},
    {"]", TOK_RBRACKET},
    {"<", TOK_LT},
    {">", TOK_GT},
    {".", TOK_DOT},
    {",", TOK_COMMA},
    {":", TOK_COLON},
    {";", TOK_SEMICOLON},
    {"'", TOK_QUOTE},
    {"\"", TOK_DOUBLEQUOTE},
    {"=", TOK_EQUALS},
    {"+", TOK_PLUS},
    {"-", TOK_MINUS},
    {"*", TOK_STAR},
    {"&", TOK_AMPERSAND},
    {"^", TOK_CARET},
    {"~", TOK_TILDE},
    {"\\", TOK_BACKSLASH},
    {"!", TOK_BANG},
    {"?", TOK_QUESTION},
    {"#", TOK_HASH},
    {"-", TOK_DASH},
    {NULL, 0}
};

static char *readfile(const char *filename, long *filesize) {
    FILE *f = fopen(filename, "r");
    char *s;
    if (!f) {
        fprintf(stderr, "error: %s: %s\n", filename, strerror(errno));
        exit(EXIT_FAILURE);
    }
    fseek(f, 0, SEEK_END);
    *filesize = ftell(f);
    rewind(f);

    s = malloc(*filesize + 1);
    if (!s) {
        fprintf(stderr, "out of memory\n");
        exit(EXIT_FAILURE);
    }
    fread(s, *filesize, 1, f);
    s[*filesize] = '\0';
    fclose(f);
    return s;
}

static void step(Lexer *l) {
    if (l->off + 1 < l->srclen) {
        l->off++;
        l->ch = l->src[l->off];
    } else {
        l->off = l->srclen;
        l->ch = 0;
    }
}

// NOTE: more strict?
static void consume(Lexer *l, long n) {
    for (long i = 0; i < n; i++)
	step(l);
}

static char lookn(Lexer *l, long n) {
    if (l->off + n < l->srclen)
        return l->src[l->off + n];
    return '\0';
}

int lexer_init(Lexer *l, const char *filename) {
    l->filename = malloc(256);
    l->filename = memcpy(l->filename, filename, 255);
    l->filename[255] = '\0';
    l->src = readfile(filename, &l->srclen);
    l->off = 0;
    l->ch = 0;
    if (l->srclen > 0)
	l->ch = l->src[0];
    l->start = 0;
    memset(&l->sb, 0, sizeof(l->sb));
    return 1;
}

void lexer_free(Lexer *l) {
    free(l->filename);
    free(l->src);
    free(l->sb.s);
}

static Token *make_token(Lexer *l, TokenType type) {
    Token *t = malloc(sizeof(Token));
    memset(t, 0, sizeof(Token));
    t->type = type;
    t->pos = l->start;
    return t;
}

static Token *make_token_with_text(Lexer *l, TokenType type) {
    Token *t = make_token(l, type);
    t->litlen = l->off - l->start;
    t->lit = malloc(t->litlen + 1);
    memcpy(t->lit, l->src + l->start, t->litlen);
    t->lit[t->litlen] = '\0';
    return t;
}

void token_free(Token *t) {
    if (t->lit)
	free(t->lit);
    free(t);
}

#if 0
static void skip_whitespace(Lexer *l) {
    while (l->ch == ' ' || l->ch == '\t' || l->ch == '\n')
        step(l);
}
#endif

static void skip_numbers(Lexer *l) {
    while (isdigit(l->ch))
        step(l);
}

static Token *lex_ident(Lexer *l) {
    l->start = l->off;
    while (isalnum(l->ch))
        step(l);
    return make_token_with_text(l, TOK_IDENT);
}

static Token *lex_number(Lexer *l) {
    l->start = l->off;
    skip_numbers(l);
    if (l->ch == '.') {
        step(l);
        skip_numbers(l);
    }
    return make_token_with_text(l, TOK_NUM);
}

// NOTE: taken from ponyc
static Token *lex_symbol(Lexer *l) {
    // NOTE: look_n()s into a temp buffer?
    for (const struct SymbolMap *m = &symbols[0]; m->text != NULL; m++) {
        for (int i = 0; m->text[i] == '\0' || m->text[i] == lookn(l, i); i++) {
            if (m->text[i] == '\0') {
		consume(l, i);
                return make_token(l, m->type);
            }
        }
    }
    step(l);
    return make_token(l, TOK_ERR);
}

Token *lexer_next(Lexer *l) {
    strbuf_reset(&l->sb);

    for (;;) {
	l->start = l->off;

        switch (l->ch) {
            case 0: {
                return make_token(l, TOK_EOF);
            }
            case '\n': {
                l->line_off = l->off;
                step(l);
                break;
            }
            case ' ': case '\t': {
                step(l);
                break;
	    }
            case '/': {
                step(l);
                if (l->ch != '/')
                    return make_token(l, TOK_SLASH);
                // ignore comment
                while (l->ch != '\n')
                    step(l);
                break;
            }
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9': {
                return lex_number(l);
	    }
            default: {
                if (isalpha(l->ch)) {
                    return lex_ident(l);
                } else { // symbols
                    return lex_symbol(l);
                }
            }
        }
    }
}
