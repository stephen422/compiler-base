#ifndef PARSE_H
#define PARSE_H

#include "lexer.hh"

class Parser {
public:
  Parser(Lexer &l) : lexer(l) {}

  void parse();

private:
  Lexer &lexer;
};

#endif
