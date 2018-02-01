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

static void next(Lexer *l);

int lexer_init(Lexer *l, const char *filename)
{
	l->filename = calloc(256, 1);
	l->filename = strncpy((char *)l->filename, filename, 255);
	l->src = readfile(filename, &l->src_size);
	l->ch = 0;
	l->off = 0;
	l->rd_off = 0;

	for (;;) {
		next(l);
		printf("%d: %c\n", l->off, l->ch);
	}
	return 1;
}

void lexer_free(Lexer *l)
{
	free(l->filename);
	free(l->src);
}

static void next(Lexer *l)
{
	if (l->rd_off < l->src_size) {
		l->off = l->rd_off;
		if (l->ch == '\n')
			l->line_off = l->off;
		l->ch = l->src[l->rd_off];
		l->rd_off++;
	} else {
		l->off = l->src_size;
		if (l->ch == '\n')
			l->line_off = l->off;
		l->ch = 0;
	}
}
