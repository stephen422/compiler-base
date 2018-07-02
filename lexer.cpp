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

    switch (*look) {
    case 0:
        if (look == eos())
            return Token_{TokenType::eos, pos()};
        throw std::string{"Unexpected null character"};
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        return lex_number();
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
    case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
    case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
    case 'V': case 'W': case 'X': case 'Y': case 'Z':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z':
    case '_':
        return lex_ident();
    case '"':
        return lex_string();
    default:
        return lex_symbol();
    }

    // Identifier starts with an alphabet or an underscore.
    // if (std::isalpha(*look) || *look == '_') {
    //     return lex_ident();
    // } else if (std::isdigit(*look)) {
    //     return lex_number();
    // }

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
    // auto sym = lex_symbol();

    // switch (sym.type) {
    // case TokenType::doublequote: {
    //     look--;
    //     return lex_string();
    // }
    // case TokenType::slash: {
    //     if (lex_symbol().type == TokenType::slash) {
    //         look -= 2;
    //         return lex_comment();
    //     } else {
    //         // FIXME no look--?
    //         return sym;
    //     }
    // }
    // default: { break; }
    // }
    // return sym;
}

Token_ Lexer::peek() {
  auto save = look;
  auto token = lex();
  look = save;
  return token;
}

void Lexer::skip_whitespace() { look = std::find_if_not(look, eos(), isspace); }
