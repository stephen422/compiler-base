#include "Lexer.h"
#include <cctype>

const Ident Lexer::lex_ident() {
  auto isalpha = [](char c) { return std::isalnum(c) || c == '_'; };
  auto end = std::find_if_not(look, eos(), isalpha);
  auto token = std::string(look, end);
  look = end;
  return Ident{token};
}

const Number Lexer::lex_numeric() {
  auto end = std::find_if_not(look, eos(), isdigit);
  auto token = std::string(look, end);
  look = end;
  return Number{token};
}

Token Lexer::lex() {
  auto total = std::cend(sv) - std::cbegin(sv);
  std::cout << "Current pos: " << look - std::cbegin(sv) << " / " << total
            << std::endl;

  // Skip whitespace at the beginning of the lex.
  // This could be done at the end of the lex, but that requires additional
  // operation for sources that contains whitespace at the start.
  skip_whitespace();

  std::cout << "Skipped pos: " << look - std::cbegin(sv) << " / " << total
            << std::endl;

  // Identifier starts with an alphabet or an underscore.
  if (look == eos()) {
    std::cout << "eos\n";
    return Eof{};
  } else if (std::isalpha(*look) || *look == '_') {
    std::cout << "ident: [" << lex_ident().s << "]\n";
  } else if (std::isdigit(*look)) {
    std::cout << "numeric: [" << lex_numeric().s << "]\n";
  } else {
    // std::cerr << "lex error: [" << *look << "]: Unrecognized token type\n";
    std::cout << eos() - look << " chars left to eos\n";
    throw std::string{"Unrecognized token type"};
  }

  return Ident{"token"};
}

void Lexer::skip_whitespace() {
  look = std::find_if_not(look, eos(), isspace);
}
