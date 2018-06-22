#include "lexer.hh"
#include "parse.hh"
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
    std::cout << token << std::endl;

    if (std::holds_alternative<Eos>(token))
      break;
  }

  //Parser p{lexer};
  //p.parse();
  return 0;
}
