#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "lexer.h"

static char *readFile(const char *filename, long *filesize) {
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
	fclose(f);
	return s;
}

int lexerInit(Lexer *l, const char *filename) {
	l->filename = calloc(256, 1);
	l->filename = strncpy((char *)l->filename, filename, 255);
	l->src = readFile(filename, &l->srcSize);
	l->look = NULL;
	l->off = 0;
	l->rdOff = 0;
	return 1;
}

void lexerFree(Lexer *l) {
	free(l->filename);
	free(l->src);
}
