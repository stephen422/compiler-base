#include <stdio.h>
#include <stdlib.h>
#include "parser.h"
#include "lexer.h"

void test_lexer(const char *filename)
{
    lexer_t lex;
    token_t *tok;

    lexer_init(&lex, filename);

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
            exit(1);
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
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "usage: %s FILE\n", argv[0]);
        return 1;
    }

    // test_lexer(argv[1]);

    parser_t p;
    parser_init(&p, argv[1]);
    parse(&p);
    parser_free(&p);
}
