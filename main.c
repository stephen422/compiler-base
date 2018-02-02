#include <stdio.h>
#include "lexer.h"

int main(int argc, char **argv) {
	Lexer lex;
	int tok = 1;

	if (argc < 2) {
		fprintf(stderr, "usage: %s FILE\n", argv[0]);
		return 1;
	}

	lexer_init(&lex, argv[1]);
	printf("%s\n", lex.src);
	printf("filesize: %ld\n", lex.src_len);

	while (tok) {
		lexer_next(&lex, &tok);
	}

	printf("%d\n", lex.off);
	lexer_free(&lex);
	return 0;
}
