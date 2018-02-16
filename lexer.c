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

static void step(Lexer *l)
{
    if (l->rd_off < l->src_len) {
        l->off = l->rd_off;
        l->ch = l->src[l->rd_off];
        l->rd_off++;
    } else {
        l->off = l->src_len;
        l->ch = 0;
    }
}

int lexer_init(Lexer *l, const char *filename)
{
    l->filename = malloc(256);
    l->filename = memcpy(l->filename, filename, 255);
    l->filename[255] = '\0';
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

static void save_lit(Lexer *l, long start, long end)
{
    long len = end - start;
    if (len > l->sb.len) {
        char *s = realloc(l->sb.s, len + 1);
        if (!s) {
            fprintf(stderr, "out of memory\n");
            exit(EXIT_FAILURE);
        }
        l->sb.s = s;
    }
    memcpy(l->sb.s, l->src + start, len);
    l->sb.s[len] = '\0';
    l->sb.len = len;
}

static void skip_whitespace(Lexer *l)
{
    while (l->ch == ' ' || l->ch == '\t' || l->ch == '\n')
        step(l);
}

static void skip_numbers(Lexer *l) {
    while (isdigit(l->ch))
        step(l);
}

static void lex_ident(Lexer *l)
{
    long off = l->off;
    while (isalnum(l->ch))
        step(l);
    save_lit(l, off, l->off);
}

static void lex_number(Lexer *l)
{
    long off = l->off;
    skip_numbers(l);
    if (l->ch == '.') {
        step(l);
        skip_numbers(l);
    }
    save_lit(l, off, l->off);
}

int lexer_next(Lexer *l)
{
    strbuf_reset(&l->sb);

    for (;;) {
        switch (l->ch) {
            case 0: {
                return TOK_EOF;
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
            case '-': {
                step(l);
                if (l->ch != '-')
                    return '-';
                // comment
                while (l->ch != '\n')
                    step(l);
                break;
            }
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9': {
                lex_number(l);
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
}
