#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "lexer.h"

static char *readfile(const char *filename, long *filesize)
{
	FILE *f = fopen(filename, "rb");
	if (!f) {
		fprintf(stderr, "%s: %s\n", filename, strerror(errno));
		return 0;
	}

	fseek(f, 0, SEEK_END);
	*filesize = ftell(f);
	rewind(f);

	char *s = malloc(*filesize + 1);
	if (!s) {
		fprintf(stderr, "%s\n", strerror(errno));
		return 0;
	}
	fread(s, *filesize, 1, f);
	s[*filesize] = '\0';
	fclose(f);
	return s;
}

static void step(Lexer *l);

int lexer_init(Lexer *l, const char *filename)
{
	l->filename = calloc(256, 1);
	l->filename = strncpy((char *)l->filename, filename, 255);
	l->src = readfile(filename, &l->src_len);
	l->ch = 0;
	l->off = 0;
	l->rd_off = 0;
	step(l);
	return 1;
}

void lexer_free(Lexer *l)
{
	free(l->filename);
	free(l->src);
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

void lexer_next(Lexer *l, int *tok)
{
	/*
	 * Skip whitespace at the beginning of the lex.
	 * This could be done at the end of the lex, but that requires additional
	 * operation for sources that contains whitespace at the start.
	 */
	/* skip_whitespace(); */

	switch (l->ch) {
	case 0:
		*tok = 0;
		break;
	default:
		*tok = 1;
		break;
	}
	step(l);
}
