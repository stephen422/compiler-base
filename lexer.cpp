#include "lexer.h"
#include <cctype>

template <typename T>
void print_literal(std::ostream& os, const Token_& token) {
    os << " [" << token.lit << "]";
}

std::ostream& operator<<(std::ostream& os, const Token_& token) {
    os << "[" << token.lit << "]";
    return os;
}

Token_ Lexer::lex_ident() {
    auto isalpha = [](char c) { return std::isalnum(c) || c == '_'; };
    auto end = std::find_if_not(look, eos(), isalpha);
    std::string lit{look, end};
    Token_ token{TokenType::ident, pos(), lit};
    look = end;
    return token;
}

Token_ Lexer::lex_number() {
    auto end = std::find_if_not(look, eos(), isdigit);
    std::string lit{look, end};
    Token_ token{TokenType::number, pos(), lit};
    look = end;
    return token;
}

Token_ Lexer::lex_string() {
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

    std::string lit{look, end};
    Token_ token{TokenType::string, pos(), lit};
    look = end;
    return token;
}

Token_ Lexer::lex_comment() {
    auto end = std::find(look, eos(), '\n');
    std::string lit{look, end};
    Token_ token{TokenType::comment, pos(), lit};
    look = end;
    return token;
}

Token_ Lexer::lex_symbol() {
    for (auto &[type, lit] : token_map) {
        std::string_view sv{look, lit.length()};
        if (sv == lit) {
            Token_ token{type, pos(), lit};
            look += lit.length();
            return token;
        }
    }
    return Token_{TokenType::none, pos(), std::string{*look}};
}

Token_ Lexer::lex() {
    skip_whitespace();

    // debug
    auto total = std::cend(sv) - std::cbegin(sv);
    std::cout << "pos " << look - std::cbegin(sv) << "/" << total << ": ";

    // Identifier starts with an alphabet or an underscore.
    if (look == eos()) {
        return Token_{TokenType::eos, pos()};
    } else if (std::isalpha(*look) || *look == '_') {
        return lex_ident();
    } else if (std::isdigit(*look)) {
        return lex_number();
    }

    /*
    else if (*look == '"') {
        auto tok = lex_string();
        return tok;
    } else if (*look == '/') {
        // Slashes may be either divides or comments
        if (look + 1 != eos() && *(look + 1) == '/') {
            auto tok = lex_comment();
            return tok;
        }
    } else {
        return lex_symbol();
        // std::cerr << "lex error: [" << *look << "]: Unrecognized token type\n";
        // throw std::string{"Unrecognized token type"};
    }
    */

    // Otherwise, it's a literal or a symbol.
    auto sym = lex_symbol();
    return sym;
}

Token_ Lexer::peek() {
  auto save = look;
  auto token = lex();
  look = save;
  return token;
}

void Lexer::skip_whitespace() { look = std::find_if_not(look, eos(), isspace); }