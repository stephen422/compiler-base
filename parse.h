#ifndef PARSE_H
#define PARSE_H

#include "lexer.h"

class Parser {
public:
  Parser(Lexer &l) : lexer(l) {}

  void parse();

private:
  Lexer &lexer;
};

#endif
