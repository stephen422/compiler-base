#include <stdio.h>
#include "lexer.h"

int main(int argc, char **argv) {
	Lexer lex;

	if (argc < 2) {
		fprintf(stderr, "usage: %s FILE\n", argv[0]);
		return 1;
	}

	lexer_init(&lex, argv[1]);
	printf("%s\n", lex.src);
	printf("filesize: %ld\n", lex.src_size);
	lexer_free(&lex);
	return 0;
}
