#include "sema.h"
#include "parser.h"
#include "lexer.h"
#include <cstring>
#include <fstream>
#include <iostream>

using namespace cmp;

void test_lexer(Lexer &lexer) {
    Token token;

    while ((token = lexer.lex()).type != TokenType::eos) {
        if (token.type == TokenType::none) {
            std::cerr << "lex error: [" << token.text << "]: Unrecognized token type\n";
            break;
        }
        std::cout << token << std::endl;
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        std::cerr << "error: no filename specified\n";
        return 1;
    }

    Source src{Path{argv[1]}};
    Semantics sema{src};
    Lexer lexer{src};

#if 0
    test_lexer(lexer);
#else
    Parser p{lexer};
    auto ast = p.parse();
    ast->print();
    ast->traverse(sema);
    sema.symtab.print();
#endif
    return 0;
}
