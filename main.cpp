#include "parser.h"
#include "lexer.h"
#include <cstring>
#include <fstream>
#include <iostream>

extern void use_read(std::ifstream& in);

void test_lexer(const char *path)
{
    Source src{path};
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

int main(int argc, char** argv)
{
    if (argc < 2) {
        std::cerr << "No filename specified\n";
        return 1;
    }

    test_lexer(argv[1]);

    // Source src{argv[1]};
    // Lexer lexer{src};
    // Parser p{lexer};
    // p.parse();

    std::cout << "sizeof token: " << sizeof(Token) << std::endl;
    std::cout << "sizeof parser: " << sizeof(Parser) << std::endl;
    return 0;
}
