#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

int main(int argc, char **argv) {
    Lexer lex;
    int tok;

    if (argc < 2) {
        fprintf(stderr, "usage: %s FILE\n", argv[0]);
        return 1;
    }

    lexer_init(&lex, argv[1]);
    tok = lexer_next(&lex);
    for (; tok != TOK_EOF; tok = lexer_next(&lex)) {
        switch (tok) {
            case TOK_IDENT:
            case TOK_NUM:
                printf("[%s]\n", lex.sb.s);
                break;
            default:
                printf("[%c]\n", tok);
                break;
        }
    }
    printf("%ld\n", lex.off);
    lexer_free(&lex);
    return 0;
}
