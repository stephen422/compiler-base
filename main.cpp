#include "lexer.hpp"
#include "parse.hpp"
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
        std::cout << token << std::endl;

        if (token.type == TokenType::eos)
            break;
    }

    //Parser p{lexer};
    //p.parse();
    std::cout << static_cast<int>(TokenType::ident) << std::endl;
    return 0;
}
