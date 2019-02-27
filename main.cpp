#include "sema.h"
#include "parser.h"
#include "lexer.h"
#include <cstring>
#include <fstream>
#include <iostream>

using namespace cmp;

void test_lexer(Lexer &lexer) {
    Token token;

    while ((token = lexer.lex()).kind != TokenKind::eos) {
        if (token.kind == TokenKind::none) {
            std::cerr << "lex error: [" << token.text << "]: Unrecognized token kind\n";
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
    Lexer lexer{src};

#if 0
    test_lexer(lexer);
#else
    Parser p{lexer};
    auto ast = p.parse();
    Semantics sema{src, ast.name_table};
    std::cout << "After parsing:\n";
    ast.root->print();
    semantic_analyze(sema, ast);
    std::cout << "==== Declaration table ====\n";
    sema.decl_table.print();
    std::cout << "==== Type table ====\n";
    sema.type_table.print();
#endif
    return 0;
}
