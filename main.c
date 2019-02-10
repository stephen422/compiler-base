#include <stdio.h>
#include <stdlib.h>
#include "sema.h"
#include "parser.h"
#include "lexer.h"

void test_lexer(const char *filename)
{
    Lexer lex;
    lexer_init(&lex, filename);

    while (lexer_next(&lex) == 0) {
        printf("pos %ld/%ld: ", lex.token.range.start, lex.srclen);
        print_token(&lex, lex.token);
    }

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

    Parser p;
    parser_init(&p, argv[1]);
    AstNode *program = parse(&p);
    traverse(program);
    parser_free(&p);
}
