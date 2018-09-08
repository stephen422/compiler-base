#include "lexer.h"
#include "parse.h"
#include <cstring>
#include <fstream>
#include <iostream>

extern void use_read(std::ifstream& in);

int main(int argc, char** argv) {
    if (argc < 2) {
        std::cerr << "No filename specified\n";
        return 1;
    }

    Source src{argv[1]};
    Lexer lexer{src};

    while (true) {
        auto token = lexer.lex();
        //std::cout << static_cast<char>(token.type) << std::endl;
        if (token.type == TokenType::none) {
            std::cerr << "lex error: [" << token.lit << "]: Unrecognized token type\n";
            break;
        }
        if (token.type == TokenType::eos)
            break;

        std::cout << token << std::endl;
    }

    //Parser p{lexer};
    //p.parse();
    return 0;
}
