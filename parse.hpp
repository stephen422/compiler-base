#ifndef PARSE_H
#define PARSE_H

#include "lexer.hpp"

class Parser {
public:
  Parser(Lexer &l) : lexer(l) {}

  void parse();

private:
  Lexer &lexer;
};

#endif
