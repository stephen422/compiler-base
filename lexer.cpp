#include "lexer.hpp"
#include <cctype>

template <typename T>
void print_literal(std::ostream& os, const Token_& token) {
    os << " [" << token.lit << "]";
}

std::ostream& operator<<(std::ostream& os, const Token_& token) {
    /*
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
    */
    os << "something (" << token.lit << ")";
    return os;
}

Token_ Lexer::lex_ident() {
    auto isalpha = [](char c) { return std::isalnum(c) || c == '_'; };
    auto end = std::find_if_not(look, eos(), isalpha);
    std::string lit{look, end};
    look = end;
    return Token_{TokenType::ident, pos(), lit};
}

Token_ Lexer::lex_number() {
    auto end = std::find_if_not(look, eos(), isdigit);
    std::string lit{look, end};
    look = end;
    return Token_{TokenType::number, pos(), lit};
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
    look = end;
    return Token_{TokenType::string, pos(), lit};
}

Token_ Lexer::lex_comment() {
    auto end = std::find(look, eos(), '\n');
    std::string lit{look, end};
    look = end;
    return Token_{TokenType::comment, pos(), lit};
}

Token_ Lexer::lex_symbol() {
    // There may be mismatches
    auto type = static_cast<TokenType>(*look);
    look++;
    return Token_{type, pos()};
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
        auto tok = lex_ident();
        return tok;
    } else if (std::isdigit(*look)) {
        auto tok = lex_number();
        return tok;
    } else if (*look == '"') {
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

    return Token_{TokenType::ident, pos(), "BAD"};
}

Token_ Lexer::peek() {
  auto save = look;
  auto token = lex();
  look = save;
  return token;
}

void Lexer::skip_whitespace() { look = std::find_if_not(look, eos(), isspace); }
