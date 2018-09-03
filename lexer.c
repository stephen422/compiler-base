#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "lexer.h"

struct TokenMap {
    const char *text;
    TokenType type;
};

static struct TokenMap symbols[] = {
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
    {"`", TOK_BACKTICK},
    {"=", TOK_EQUALS},
    {"+", TOK_PLUS},
    {"-", TOK_MINUS},
    {"*", TOK_STAR},
    {"&", TOK_AMPERSAND},
    {"^", TOK_CARET},
    {"~", TOK_TILDE},
    {"\\", TOK_BACKSLASH},
    {"|", TOK_PIPE},
    {"!", TOK_BANG},
    {"?", TOK_QUESTION},
    {"@", TOK_AT},
    {"#", TOK_HASH},
    {"$", TOK_DOLLAR},
    {"%", TOK_PERCENT},
    {"-", TOK_DASH},
    {NULL, 0} // terminator
};

static struct TokenMap keywords[] = {
    {"assign", TOK_ASSIGN},
    {"fn", TOK_FN},
    {"let", TOK_LET},
    {"var", TOK_VAR},
    {"return", TOK_RETURN},
    {"int", TOK_INT},
    {"if", TOK_IF},
    {"else", TOK_ELSE},
    {"for", TOK_FOR},
    {NULL, 0} // terminator
};

static char *readfile(const char *filename, long *filesize)
{
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

int lexer_init(Lexer *l, const char *filename)
{
    l->filename = malloc(256);
    l->filename = memcpy(l->filename, filename, 255);
    l->filename[255] = '\0';
    l->src = readfile(filename, &l->srclen);
    l->off = 0;
    l->start = 0;
    l->ch = l->src[0];
    memset(&l->sb, 0, sizeof(l->sb));
    return 1;
}

void lexer_free(Lexer *l)
{
    free(l->filename);
    free(l->src);
    free(l->sb.s);
}

static void step(Lexer *l)
{
    if (l->off + 1 < l->srclen) {
        l->off++;
        l->ch = l->src[l->off];
    } else {
        l->off = l->srclen;
        l->ch = 0;
    }
}

// NOTE: more strict?
static void consume(Lexer *l, long n)
{
    for (long i = 0; i < n; i++)
        step(l);
}

static char lookn(Lexer *l, long n)
{
    if (l->off + n < l->srclen)
        return l->src[l->off + n];
    return '\0';
}

static Token *make_token(Lexer *l, TokenType type)
{
    Token *t = malloc(sizeof(Token));
    memset(t, 0, sizeof(Token));
    t->type = type;
    t->pos = l->start;
    return t;
}

static Token *make_token_with_text(Lexer *l, TokenType type)
{
    Token *t = make_token(l, type);
    t->litlen = l->off - l->start;
    t->lit = malloc(t->litlen + 1);
    memcpy(t->lit, l->src + l->start, t->litlen);
    t->lit[t->litlen] = '\0';
    return t;
}

void token_free(Token *t)
{
    if (t->lit)
        free(t->lit);
    free(t);
}

static Token *lex_ident(Lexer *l)
{
    while (isalnum(l->ch) || l->ch == '_')
        step(l);

    // Known keywords
    for (const struct TokenMap *m = &keywords[0]; m->text != NULL; m++) {
        const char *c = m->text;
        const char *s = l->src + l->start;
        for (; *c != '\0' && s < l->src + l->off && *c == *s; c++, s++)
            /* nothing */;
        if (*c == '\0')
            return make_token(l, m->type);
    }
    return make_token_with_text(l, TOK_IDENT);
}

static void skip_numbers(Lexer *l)
{
    while (isdigit(l->ch))
        step(l);
}

static Token *lex_number(Lexer *l)
{
    skip_numbers(l);
    if (l->ch == '.') {
        step(l);
        skip_numbers(l);
    }
    return make_token_with_text(l, TOK_NUM);
}

// NOTE: taken from ponyc
static Token *lex_symbol(Lexer *l)
{
    for (const struct TokenMap *m = &symbols[0]; m->text != NULL; m++) {
        for (int i = 0; m->text[i] == '\0' || m->text[i] == lookn(l, i); i++) {
            if (m->text[i] == '\0') {
                consume(l, i);
                return make_token(l, m->type);
            }
        }
    }
    fprintf(stderr, "lex error: unknown symbol '%c'\n", l->ch);
    step(l);
    return make_token(l, TOK_ERR);
}

Token *lexer_next(Lexer *l)
{
    strbuf_reset(&l->sb);

    for (;;) {
        l->start = l->off;

        switch (l->ch) {
        case 0: {
            return make_token(l, TOK_EOF);
        }
        case '\n': case '\r': {
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
            if (isalpha(l->ch) || l->ch == '_') {
                return lex_ident(l);
            } else { // symbols
                return lex_symbol(l);
            }
        }
        }
    }
}

void print_token(const Token *tok)
{
    switch (tok->type) {
    case TOK_IDENT:
    case TOK_NUM : {
        printf((tok->type == TOK_IDENT) ? "ident" : "num");
        if (tok->lit)
            printf(" [%s]\n", tok->lit);
        else
            printf("\n");
        break;
    }
    case TOK_ERR: {
        printf("error\n");
        break;
    }
    default: {
        if (tok->type >= TOK_KEYWORDS)
            printf("keyword (type %d)\n", tok->type - TOK_KEYWORDS - 1);
        else
            printf("symbol (type %d)\n", tok->type);
        break;
    }
    }
}
