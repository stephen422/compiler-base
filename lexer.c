#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "lexer.h"

static char *readfile(const char *filename, long *filesize)
{
    FILE *f = fopen(filename, "r");
    char *s;

    if (!f) {
        fprintf(stderr, "%s: %s\n", filename, strerror(errno));
        return 0;
    }

    fseek(f, 0, SEEK_END);
    *filesize = ftell(f);
    rewind(f);

    s = malloc(*filesize + 1);
    if (!s) {
        fprintf(stderr, "%s\n", strerror(errno));
        return 0;
    }
    fread(s, *filesize, 1, f);
    s[*filesize] = '\0';
    fclose(f);
    return s;
}

static void step(Lexer *l)
{
    if (l->rd_off < l->src_len) {
        l->off = l->rd_off;
        if (l->ch == '\n')
            l->line_off = l->off;
        l->ch = l->src[l->rd_off];
        l->rd_off++;
    } else {
        l->off = l->src_len;
        if (l->ch == '\n')
            l->line_off = l->off;
        l->ch = 0;
    }
}

int lexer_init(Lexer *l, const char *filename)
{
    l->filename = calloc(256, 1);
    l->filename = strncpy((char *)l->filename, filename, 255);
    l->src = readfile(filename, &l->src_len);
    l->ch = 0;
    l->off = 0;
    l->rd_off = 0;

    /* Start the engine */
    step(l);
    return 1;
}

void lexer_free(Lexer *l)
{
    free(l->filename);
    free(l->src);
}

static void skip_whitespace(Lexer *l)
{
    while (l->ch == ' ' || l->ch == '\t' || l->ch == '\n')
        step(l);
}

static char *make_lit(Lexer *l, long start, long end, int *lit_len)
{
    char *s;

    s = malloc(end - start + 1);
    if (!s) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    memcpy(s, l->src + start, end - start);
    s[end - start] = '\0';
    *lit_len = end - start;

    return s;
}

static void lex_ident(Lexer *l, char **lit, int *lit_len)
{
    long off = l->off;
    while (isalnum(l->ch))
        step(l);
    *lit = make_lit(l, off, l->off, lit_len);
}

Token lexer_next(Lexer *l, char **lit, int *lit_len)
{
    Token tok;

    /*
     * Skip whitespace at the beginning of the lex.
     * This could be done at the end of the lex, but that requires additional
     * operation for sources that contains whitespace at the start.
     */
    skip_whitespace(l);

    switch (l->ch) {
        case 0:
            tok = TOK_EOF;
            break;
        case '0': case '1': case '2':
        case '3': case '4': case '5':
        case '6': case '7': case '8':
        case '9':
            tok = TOK_NUM;
            break;
        case '#':
            tok = TOK_HASH;
            break;
        case '<':
            tok = TOK_LT;
            break;
        default: {
            if (isalpha(l->ch)) {
                lex_ident(l, lit, lit_len);
                tok = TOK_IDENT;
            } else {
                tok = TOK_ERR;
            }
            break;
        }
    }
    step(l);

    return tok;
}
