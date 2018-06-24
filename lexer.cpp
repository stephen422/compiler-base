#include "lexer.hpp"
#include <cctype>

template <typename T>
void print_literal(std::ostream& os, const Token& token) {
    auto& tok = std::get<T>(token);
    os << " [" << tok.lit << "]";
}

template <typename T>
bool is_type(const Token& token) {
  return std::holds_alternative<T>(token);
}
std::ostream& operator<<(std::ostream& os, const Token& token) {
  if (is_type<Ident>(token)) {
    os << "ident";
    print_literal<Ident>(os, token);
  } else if (is_type<Number>(token)) {
    os << "number";
    print_literal<Number>(os, token);
  } else if (is_type<String>(token)) {
    os << "string";
    print_literal<String>(os, token);
  } else if (is_type<Lparen>(token)) {
    os << "lparen";
  } else if (is_type<Rparen>(token)) {
    os << "rparen";
  } else if (is_type<Lbrace>(token)) {
    os << "lbrace";
  } else if (is_type<Rbrace>(token)) {
    os << "rbrace";
  } else if (is_type<Lesserthan>(token)) {
    os << "lesserthan";
  } else if (is_type<Greaterthan>(token)) {
    os << "greaterthan";
  } else if (is_type<Dot>(token)) {
    os << "dot";
  } else if (is_type<Comma>(token)) {
    os << "comma";
  } else if (is_type<Colon>(token)) {
    os << "colon";
  } else if (is_type<Semicolon>(token)) {
    os << "semicolon";
  } else if (is_type<Quote>(token)) {
    os << "quote";
  } else if (is_type<Equals>(token)) {
    os << "equals";
  } else if (is_type<Plus>(token)) {
    os << "plus";
  } else if (is_type<Minus>(token)) {
    os << "minus";
  } else if (is_type<Star>(token)) {
    os << "star";
  } else if (is_type<Ampersand>(token)) {
    os << "ampersand";
  } else if (is_type<Caret>(token)) {
    os << "caret";
  } else if (is_type<Tilde>(token)) {
    os << "tilde";
  } else if (is_type<Slash>(token)) {
    os << "slash";
  } else if (is_type<Backslash>(token)) {
    os << "backslash";
  } else if (is_type<Bang>(token)) {
    os << "bang";
  } else if (is_type<Question>(token)) {
    os << "question";
  } else if (is_type<Hash>(token)) {
    os << "hash";
  } else if (is_type<Bar>(token)) {
    os << "bar";
  } else if (is_type<Eos>(token)) {
    os << "eos";
  } else {
    os << "unrecognized token";
  }
  return os;
}

Token_ Lexer::lex_ident() {
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
  while (end != eos()) {
    end = std::find_if(end, eos(),
                       [](char c) { return ((c == '\\') || (c == '"')); });
    if (*end == '"') {
      break;
    } else {
      // Skip the escaped character '\x'
      end += 2;
    }
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

Token_ Lexer::lex_symbol() {
    // There may be mismatches
    auto type = static_cast<Token_::Type>(this->pos());
    this->look++;
    return Token_{type, this->pos()};
}

Token_ Lexer::lex() {
    auto total = std::cend(sv) - std::cbegin(sv);

    // Skip whitespace at the beginning of the lex.
    // This could be done at the end of the lex, but that requires additional
    // operation for sources that contains whitespace at the start.
    skip_whitespace();

    std::cout << "pos " << look - std::cbegin(sv) << "/" << total << ": ";

    // Identifier starts with an alphabet or an underscore.
    if (look == eos()) {
        return Token_{Token_::Type::Eos, this->pos()};
    } else if (std::isalpha(*look) || *look == '_') {
        // auto tok = lex_ident();
        look++;
        return Token_{Token_::Type::Ident, this->pos()};
    } else if (std::isdigit(*look)) {
        // auto tok = lex_number();
        look++;
        return Token_{Token_::Type::Number, this->pos()};
    } else if (*look == '"') {
        // auto tok = lex_string();
        look++;
        return Token_{Token_::Type::String, this->pos()};
    } else if (*look == '/') {
        // Slashes may be either divides or comments
        if (look + 1 != eos() && *(look + 1) == '/') {
            // auto tok = lex_comment();
        }
        look++;
        return Token_{Token_::Type::Ident, this->pos(), "BAD"};
    } else {
        return lex_symbol();
        // std::cerr << "lex error: [" << *look << "]: Unrecognized token type\n";
        // throw std::string{"Unrecognized token type"};
    }

    return Token_{Token_::Type::Ident, this->pos(), "BAD"};
}

Token_ Lexer::peek() {
  auto save = look;
  auto token = lex();
  look = save;
  return token;
}

void Lexer::skip_whitespace() { look = std::find_if_not(look, eos(), isspace); }
