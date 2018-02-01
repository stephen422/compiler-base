#include <stdio.h>
#include "lexer.h"

int main(int argc, char **argv) {
	Lexer lex;
	lexer_init(&lex, argv[1]);
	printf("%s\n", lex.src);
	printf("filesize: %ld\n", lex.srcSize);
	lexer_free(&lex);
	return 0;
}
