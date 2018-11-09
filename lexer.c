#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "lexer.h"

struct token_map {
    const char *text;
    TokenType type;
};

char *token_names[NUM_TOKENTYPES] = {
    [TOK_IDENT] = "identifier",
    [TOK_LPAREN] = "(",
    [TOK_RPAREN] = ")",
    [TOK_DOT] = ".",
    [TOK_COLON] = ":",
    [TOK_SEMICOLON] = ";",
    [TOK_EQUALS] = "=",
    [TOK_BANG] = "!",
    [TOK_LET] = "let",
    [TOK_VAR] = "var",
    [TOK_ERR] = "unknown"
};

static struct token_map symbols[] = {
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

static struct token_map keywords[] = {
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

int lexer_init(lexer_t *l, const char *filename)
{
    l->filename = malloc(256);
    l->filename = memcpy(l->filename, filename, 255);
    l->filename[255] = '\0';
    l->src = readfile(filename, &l->srclen);
    l->off = 0;
    l->start = 0;
    l->ch = l->src[0];
    return 1;
}

void lexer_free(lexer_t *l)
{
    free(l->filename);
    free(l->src);
}

static void step(lexer_t *l)
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
static void consume(lexer_t *l, long n)
{
    for (long i = 0; i < n; i++)
        step(l);
}

static char lookn(lexer_t *l, long n)
{
    if (l->off + n < l->srclen)
        return l->src[l->off + n];
    return '\0';
}

static void make_token(lexer_t *l, TokenType type)
{
    memset(&l->token, 0, sizeof(token_t));
    l->token.type = type;
    l->token.start = l->start;
    l->token.end = l->off;
}

void token_free(token_t *t)
{
    free(t);
}

static void lex_ident(lexer_t *l)
{
    while (isalnum(l->ch) || l->ch == '_')
        step(l);

    for (const struct token_map *m = &keywords[0]; m->text != NULL; m++) {
        const char *c = m->text;
        const char *s = l->src + l->start;
        for (; *c != '\0' && s < l->src + l->off && *c == *s; c++, s++)
            /* nothing */;
        if (*c == '\0') {
            // This is a keyword.
            make_token(l, m->type);
            return;
        }
    }
    // This is an identifier.
    make_token(l, TOK_IDENT);
}

static void skip_numbers(lexer_t *l)
{
    while (isdigit(l->ch))
        step(l);
}

static void lex_number(lexer_t *l)
{
    skip_numbers(l);
    if (l->ch == '.') {
        step(l);
        skip_numbers(l);
    }
    make_token(l, TOK_NUM);
}

// NOTE: taken from ponyc
static void lex_symbol(lexer_t *l)
{
    for (const struct token_map *m = &symbols[0]; m->text != NULL; m++) {
        for (int i = 0; m->text[i] == '\0' || m->text[i] == lookn(l, i); i++) {
            if (m->text[i] == '\0') {
                consume(l, i);
                make_token(l, m->type);
                return;
            }
        }
    }
    fprintf(stderr, "lex error: unknown symbol '%c'\n", l->ch);
    step(l);
    make_token(l, TOK_ERR);
}

// Return EOF if reached source EOF.
int lexer_next(lexer_t *l)
{
    for (;;) {
        l->start = l->off;

        switch (l->ch) {
        case 0: {
            make_token(l, TOK_EOF);
            return EOF;
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
            if (l->ch != '/') {
                make_token(l, TOK_SLASH);
                return 0;
            }
            // ignore comment
            while (l->ch != '\n')
                step(l);
            break;
        }
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9': {
            lex_number(l);
            return 0;
        }
        default: {
            if (isalpha(l->ch) || l->ch == '_') {
                lex_ident(l);
            } else {
                lex_symbol(l);
            }
            return 0;
        }
        }
    }

    return 0;
}

#if 0
void locate_line_col(lexer_t *l, size_t pos, int *line, int *col)
{
    // Search linearly for line that contains this position.
    int line;
    for (line = 0; static_cast<size_t>(line) < line_off.size(); line++) {
        if (pos < line_off[line])
            break;
    }
    int col = pos - line_off[line - 1] + 1;
    return {line, col};
}
#endif

void print_token(lexer_t *lex, const token_t *tok)
{
    switch (tok->type) {
    case TOK_IDENT:
    case TOK_NUM : {
        printf((tok->type == TOK_IDENT) ? "ident" : "num");
        printf(" [%.*s]\n", (int)(tok->end - tok->start), lex->src + tok->start);
        break;
    }
    case TOK_ERR: {
        printf("error\n");
        break;
    }
    default: {
        if (tok->type >= TOK_KEYWORDS)
            printf("%s\n", token_names[tok->type]);
        else
            printf("symbol (type %d)\n", tok->type);
        break;
    }
    }
}
