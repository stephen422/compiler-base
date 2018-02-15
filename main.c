#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

int main(int argc, char **argv) {
    Lexer lex;
    Token tok = 1;
    char *lit;
    int lit_len;

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
        tok = lexer_next(&lex, &lit, &lit_len);
    }

    printf("%s (len: %d)\n", lit, lit_len);
    printf("%ld\n", lex.off);

    free(lit);
    lexer_free(&lex);
    return 0;
}
