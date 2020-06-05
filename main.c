#include <stdio.h>
#include <stdlib.h>
#include "sema.h"
#include "parser.h"
#include "lexer.h"

void test_lexer(const char *filename)
{
    Lexer lex;
    lexerInit(&lex, filename);

    while (lexerNext(&lex) == 0) {
        printf("pos %ld/%ld: ", lex.tok.range.start, lex.srclen);
        tokenPrint(&lex, lex.tok);
    }

    printf("file size: %ld\n", lex.off);
    lexerCleanup(&lex);
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "usage: %s FILE\n", argv[0]);
        return 1;
    }

    // test_lexer(argv[1]);

    Parser p;
    parserInit(&p, argv[1]);
    Node *root = parse(&p);
    parser_verify(&p);
    // printAst(&p, root);
    // sema(&p.nametable, root);
    parserCleanup(&p);
    return 0;
}
