#include "Lexer.h"

void Lexer::Lex() {
  std::cout << "File size: " << src.buffer().size() << std::endl;
  std::cout << "Dump of buffer:\n";
  std::cout << src.buffer().data();
}
