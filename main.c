#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

int main(int argc, char **argv) {
    Lexer lex;
    int tok = 1;

    if (argc < 2) {
        fprintf(stderr, "usage: %s FILE\n", argv[0]);
        return 1;
    }

    lexer_init(&lex, argv[1]);
    while (tok != TOK_EOF) {
        if (tok == TOK_ERR) {
            fprintf(stderr, "Unknown token: %c\n", lex.ch);
            break;
        }
        tok = lexer_next(&lex);
    }
    printf("%ld\n", lex.off);
    lexer_free(&lex);
    return 0;
}
