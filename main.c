#include <stdio.h>
#include "lexer.h"

int main(int argc, char **argv) {
	Lexer lex;
	lexerInit(&lex, argv[1]);
	printf("%s\n", lex.src);
	printf("filesize: %ld\n", lex.srcSize);
	lexerFree(&lex);
	return 0;
}
