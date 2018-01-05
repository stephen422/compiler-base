#include "lexer.hh"
#include <cctype>

template <typename T>
void print_literal(std::ostream& os, const Token& token) {
  auto& tok = std::get<T>(token);
  os << " [" << tok.lit << "]";
}

std::ostream& operator<<(std::ostream& os, const Token& token) {
#define IS_TYPE(a, t) std::holds_alternative<t>(a)
  if (IS_TYPE(token, Ident)) {
    os << "ident";
    print_literal<Ident>(os, token);
  } else if (IS_TYPE(token, Number)) {
    os << "number";
    print_literal<Number>(os, token);
  } else if (IS_TYPE(token, String)) {
    os << "string";
    print_literal<String>(os, token);
  } else if (IS_TYPE(token, Lparen)) {
    os << "lparen";
  } else if (IS_TYPE(token, Rparen)) {
    os << "rparen";
  } else if (IS_TYPE(token, Lbrace)) {
    os << "lbrace";
  } else if (IS_TYPE(token, Rbrace)) {
    os << "rbrace";
  } else if (IS_TYPE(token, Lesserthan)) {
    os << "lesserthan";
  } else if (IS_TYPE(token, Greaterthan)) {
    os << "greaterthan";
  } else if (IS_TYPE(token, Dot)) {
    os << "dot";
  } else if (IS_TYPE(token, Comma)) {
    os << "comma";
  } else if (IS_TYPE(token, Colon)) {
    os << "colon";
  } else if (IS_TYPE(token, Semicolon)) {
    os << "semicolon";
  } else if (IS_TYPE(token, Quote)) {
    os << "quote";
  } else if (IS_TYPE(token, Equals)) {
    os << "equals";
  } else if (IS_TYPE(token, Plus)) {
    os << "plus";
  } else if (IS_TYPE(token, Minus)) {
    os << "minus";
  } else if (IS_TYPE(token, Star)) {
    os << "star";
  } else if (IS_TYPE(token, Ampersand)) {
    os << "ampersand";
  } else if (IS_TYPE(token, Caret)) {
    os << "caret";
  } else if (IS_TYPE(token, Tilde)) {
    os << "tilde";
  } else if (IS_TYPE(token, Slash)) {
    os << "slash";
  } else if (IS_TYPE(token, Backslash)) {
    os << "backslash";
  } else if (IS_TYPE(token, Bang)) {
    os << "bang";
  } else if (IS_TYPE(token, Question)) {
    os << "question";
  } else if (IS_TYPE(token, Hash)) {
    os << "hash";
  } else if (IS_TYPE(token, Bar)) {
    os << "bar";
  } else if (IS_TYPE(token, Eos)) {
    os << "eos";
  } else {
    os << "unrecognized token";
  }
  return os;
}

const Ident Lexer::lex_ident() {
  auto isalpha = [](char c) { return std::isalnum(c) || c == '_'; };
  auto end = std::find_if_not(look, eos(), isalpha);
  auto s = std::string(look, end);
  look = end;
  return Ident{s};
}

const Number Lexer::lex_number() {
  auto end = std::find_if_not(look, eos(), isdigit);
  auto s = std::string(look, end);
  look = end;
  return Number{s};
}

const String Lexer::lex_string() {
  auto end = look + 1;
  for (; end != eos(); end = std::find_if(end, eos(), [](char c) {
                         return (c == '\\') || (c == '"');
                       })) {
    if (*end == '"')
      break;
    // Skip the escaped character '\x'
    end += 2;
  }
  // Range is end-exclusive
  end++;

  auto s = std::string(look, end);
  look = end;
  return String{s};
}

const Comment Lexer::lex_comment() {
  auto end = std::find(look, eos(), '\n');
  auto s = std::string(look, end);
  look = end;
  return Comment{s};
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
    return Eos{};
  } else if (std::isalpha(*look) || *look == '_') {
    auto tok = lex_ident();
    return tok;
  } else if (std::isdigit(*look)) {
    auto tok = lex_number();
    return tok;
  } else if (*look == '(') {
    return lex_single<Lparen>();
  } else if (*look == ')') {
    return lex_single<Rparen>();
  } else if (*look == '{') {
    return lex_single<Lbrace>();
  } else if (*look == '}') {
    return lex_single<Rbrace>();
  } else if (*look == '[') {
    return lex_single<Lbrace>();
  } else if (*look == ']') {
    return lex_single<Rbrace>();
  } else if (*look == '<') {
    return lex_single<Lesserthan>();
  } else if (*look == '>') {
    return lex_single<Greaterthan>();
  } else if (*look == '.') {
    return lex_single<Dot>();
  } else if (*look == ',') {
    return lex_single<Comma>();
  } else if (*look == ':') {
    return lex_single<Colon>();
  } else if (*look == ';') {
    return lex_single<Semicolon>();
  } else if (*look == '"') {
    auto tok = lex_string();
    return tok;
  } else if (*look == '\'') {
    return lex_single<Quote>();
  } else if (*look == '=') {
    return lex_single<Equals>();
  } else if (*look == '+') {
    return lex_single<Plus>();
  } else if (*look == '-') {
    return lex_single<Minus>();
  } else if (*look == '*') {
    return lex_single<Star>();
  } else if (*look == '/') {
    // Slashes may be either divides or comments
    if (look + 1 != eos() && *(look + 1) == '/') {
      auto tok = lex_comment();
      return tok;
    }
    return lex_single<Slash>();
  } else if (*look == '&') {
    return lex_single<Ampersand>();
  } else if (*look == '^') {
    return lex_single<Caret>();
  } else if (*look == '~') {
    return lex_single<Tilde>();
  } else if (*look == '/') {
    return lex_single<Slash>();
  } else if (*look == '\\') {
    return lex_single<Backslash>();
  } else if (*look == '!') {
    return lex_single<Bang>();
  } else if (*look == '?') {
    return lex_single<Question>();
  } else if (*look == '#') {
    return lex_single<Hash>();
  } else if (*look == '|') {
    return lex_single<Bar>();
  } else {
    std::cerr << "lex error: [" << *look << "]: Unrecognized token type\n";
    throw std::string{"Unrecognized token type"};
  }

  return Ident{"token"};
}

Token Lexer::peek() {
  auto save = look;
  auto token = lex();
  look = save;
  return token;
}

void Lexer::skip_whitespace() { look = std::find_if_not(look, eos(), isspace); }
