#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

int main(int argc, char **argv) {
    Lexer lex;
    Token *tok;

    if (argc < 2) {
        fprintf(stderr, "usage: %s FILE\n", argv[0]);
        return 1;
    }

    lexer_init(&lex, argv[1]);

    while ((tok = lexer_next(&lex))->type != TOK_EOF) {
        printf("pos %ld/%ld: ", tok->pos, lex.srclen);
        switch (tok->type) {
        case TOK_IDENT:
        case TOK_NUM : {
            printf((tok->type == TOK_IDENT) ? "ident" : "num");
            if (tok->lit)
                printf(" [%s]\n", tok->lit);
            else
                printf("\n");
            break;
        }
        case TOK_ERR: {
            printf("Lex error occurred\n");
        }
        default: {
            if (tok->type >= TOK_FN)
                printf("keyword (type %d)\n", tok->type - TOK_FN);
            else
                printf("symbol (type %d)\n", tok->type);
            break;
        }
        }
        token_free(tok);
    }
    token_free(tok);

    printf("file size: %ld\n", lex.off);
    lexer_free(&lex);
    return 0;
}
