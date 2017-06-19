#include "Lexer.h"
#include <cctype>

const Ident Lexer::lex_ident() {
  auto isalpha = [](char c) { return std::isalnum(c) || c == '_'; };
  auto end = std::find_if_not(look, eos(), isalpha);
  auto token = std::string(look, end);
  look = end;
  return Ident{token};
}

const Number Lexer::lex_number() {
  auto end = std::find_if_not(look, eos(), isdigit);
  auto token = std::string(look, end);
  look = end;
  return Number{token};
}

template <typename T> const Token Lexer::lex_single() {
  look++;
  return T{};
}

Token Lexer::lex() {
  auto total = std::cend(sv) - std::cbegin(sv);

  // Skip whitespace at the beginning of the lex.
  // This could be done at the end of the lex, but that requires additional
  // operation for sources that contains whitespace at the start.
  skip_whitespace();

  std::cout << "pos " << look - std::cbegin(sv) << "/" << total << ": ";

  // Identifier starts with an alphabet or an underscore.
  if (look == eos()) {
    std::cout << "eos\n";
    return Eos{};
  } else if (std::isalpha(*look) || *look == '_') {
    auto tok = lex_ident();
    std::cout << "ident [" << tok.s << "]\n";
    return tok;
  } else if (std::isdigit(*look)) {
    auto tok = lex_number();
    std::cout << "number [" << tok.s << "]\n";
    return tok;
  } else if (*look == '.') {
    std::cout << "dot\n";
    return lex_single<Dot>();
  } else if (*look == ',') {
    std::cout << "comma\n";
    return lex_single<Comma>();
  } else {
    // std::cerr << "lex error: [" << *look << "]: Unrecognized token type\n";
    std::cout << eos() - look << " chars left to eos\n";
    throw std::string{"Unrecognized token type"};
  }

  return Ident{"token"};
}

void Lexer::skip_whitespace() { look = std::find_if_not(look, eos(), isspace); }
