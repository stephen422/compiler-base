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

const String Lexer::lex_string() {
  auto end = look + 1;
  for (; end != eos(); end = std::find_if(end, eos(), [](char c) { return (c == '\\') || (c == '"'); })) {
    if (*end == '"')
      break;
    // Skip the escaped character
    end += 2;
  }
  // Range is end-exclusive
  end++;

  auto token = std::string(look, end);
  look = end;
  return String{token};
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
  } else if (*look == '(') {
    std::cout << "lparen\n";
    return lex_single<Lparen>();
  } else if (*look == ')') {
    std::cout << "rparen\n";
    return lex_single<Rparen>();
  } else if (*look == '{') {
    std::cout << "lbrace\n";
    return lex_single<Lbrace>();
  } else if (*look == '}') {
    std::cout << "rbrace\n";
    return lex_single<Rbrace>();
  } else if (*look == '[') {
    std::cout << "lbrace\n";
    return lex_single<Lbrace>();
  } else if (*look == ']') {
    std::cout << "rbrace\n";
    return lex_single<Rbrace>();
  } else if (*look == '<') {
    std::cout << "lesserthan\n";
    return lex_single<Lesserthan>();
  } else if (*look == '>') {
    std::cout << "greaterthan\n";
    return lex_single<Greaterthan>();
  } else if (*look == '.') {
    std::cout << "dot\n";
    return lex_single<Dot>();
  } else if (*look == ',') {
    std::cout << "comma\n";
    return lex_single<Comma>();
  } else if (*look == ':') {
    std::cout << "colon\n";
    return lex_single<Colon>();
  } else if (*look == ';') {
    std::cout << "semicolon\n";
    return lex_single<Semicolon>();
  } else if (*look == '"') {
    auto tok = lex_string();
    std::cout << "string [" << tok.s << "]\n";
    return tok;
  } else if (*look == '\'') {
    std::cout << "quote\n";
    return lex_single<Quote>();
  } else if (*look == '=') {
    std::cout << "equals\n";
    return lex_single<Equals>();
  } else if (*look == '+') {
    std::cout << "plus\n";
    return lex_single<Plus>();
  } else if (*look == '-') {
    std::cout << "minus\n";
    return lex_single<Minus>();
  } else if (*look == '*') {
    std::cout << "star\n";
    return lex_single<Star>();
  } else if (*look == '&') {
    std::cout << "ampersand\n";
    return lex_single<Ampersand>();
  } else if (*look == '^') {
    std::cout << "caret\n";
    return lex_single<Caret>();
  } else if (*look == '/') {
    std::cout << "slash\n";
    return lex_single<Slash>();
  } else if (*look == '\\') {
    std::cout << "backslash\n";
    return lex_single<Backslash>();
  } else if (*look == '!') {
    std::cout << "bang\n";
    return lex_single<Bang>();
  } else if (*look == '?') {
    std::cout << "question\n";
    return lex_single<Question>();
  } else if (*look == '#') {
    std::cout << "hash\n";
    return lex_single<Hash>();
  } else if (*look == '|') {
    std::cout << "bar\n";
    return lex_single<Bar>();
  } else {
    std::cerr << "lex error: [" << *look << "]: Unrecognized token type\n";
    std::cout << eos() - look << " chars left to eos\n";
    throw std::string{"Unrecognized token type"};
  }

  return Ident{"token"};
}

void Lexer::skip_whitespace() { look = std::find_if_not(look, eos(), isspace); }
