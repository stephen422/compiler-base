#include "Lexer.h"
#include <cctype>

const std::string Lexer::lex_ident() {
  auto isalpha = [](char c) { return std::isalnum(c) || c == '_'; };
  auto end = std::find_if_not(look, eos(), isalpha);
  auto token = std::string(look, end);
  look = end;
  return token;
}

std::string Lexer::lex() {
  std::cout << "File size: " << src.buffer().size() << std::endl;
  std::cout << "Current pos: " << look - std::cbegin(sv) << std::endl;

  std::cout << "[" << *look << "]: ";

  // Skip whitespace at the beginning of the lex.
  // This could be done at the end of the lex, but that requires additional
  // operation for sources that contains whitespace at the start.
  skip_whitespace();

  // Identifier starts with an alphabet or an underscore.
  if (std::isalpha(*look) || *look == '_') {
    std::cout << "ident: [" << lex_ident() << "]\n";
  } else if (look == eos()) {
    std::cout << "End of file\n";
    return "EOF";
  } else {
    std::cout << "not ident\n";
    look++;
  }

  std::cout << "Current pos: " << look - std::cbegin(sv) << std::endl;

  return "token";
}

void Lexer::skip_whitespace() {
  look = std::find_if_not(look, eos(), isspace);
}
