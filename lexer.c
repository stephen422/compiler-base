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
    memset(&l->sb, 0, sizeof(l->sb));
    step(l);
    return 1;
}

void lexer_free(Lexer *l)
{
    free(l->filename);
    free(l->src);
    free(l->sb.s);
}

static void skip_whitespace(Lexer *l)
{
    while (l->ch == ' ' || l->ch == '\t' || l->ch == '\n')
        step(l);
}

static void save_lit(Lexer *l, long start, long end)
{
    long len = end - start;
    if (len > l->sb.len) {
        char *s = realloc(l->sb.s, len + 1);
        if (!s) {
            fprintf(stderr, "out of memory\n");
            exit(1);
        }
        l->sb.s = s;
    }
    memcpy(l->sb.s, l->src + start, len);
    l->sb.s[len] = '\0';
    l->sb.len = len;
}

static void lex_ident(Lexer *l)
{
    long off = l->off;
    while (isalnum(l->ch))
        step(l);
    save_lit(l, off, l->off);
}

int lexer_next(Lexer *l)
{
    skip_whitespace(l);
    strbuf_reset(&l->sb);
    switch (l->ch) {
        case 0: {
            return TOK_EOF;
        }
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9': {
            step(l);
            return TOK_NUM;
        }
        default: {
            if (isalpha(l->ch)) {
                lex_ident(l);
                return TOK_IDENT;
            } else { // single-char tokens
                int c = l->ch;
                step(l);
                return c;
            }
        }
    }
}
