#include "parser.h"
#include "lexer.h"
#include <cstring>
#include <fstream>
#include <iostream>

using namespace comp;

extern void use_read(std::ifstream& in);

void test_lexer(const char *path) {
    Source src{Path{path}};
    Lexer lexer{src};
    Token token;

    while ((token = lexer.lex()).type != TokenType::eos) {
        auto token = lexer.lex();
        //std::cout << static_cast<char>(token.type) << std::endl;
        if (token.type == TokenType::none) {
            std::cerr << "lex error: [" << token.lit << "]: Unrecognized token type\n";
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

    // test_lexer(argv[1]);

    // Source src{Path{argv[1]}};
    Source src{"hi there"};
    Lexer lexer{src};
    Parser p{lexer};
    AstNode::OwnPtr ast;
    while ((ast = p.parse())) {
        ast->print();
        if (ast->type == NodeType::expr) {
            std::cout << "flatten: " << static_cast<Expr *>(ast.get())->flatten() << std::endl;
        }
    }
    return 0;
}
