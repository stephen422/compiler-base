#include "lexer.h"
#include <cctype>

namespace comp {

std::string tokentype_to_string(TokenType type) {
    for (auto &p : symbol_map) {
        if (p.second == type) {
            return p.first;
        }
    }
    for (auto &p : keyword_map) {
        if (p.second == type) {
            return p.first;
        }
    }
    return "";
}

std::ostream &operator<<(std::ostream &os, const Token &token) {
    os << "[" << token.text << "]";
    return os;
}

void Token::print() {
    throw std::runtime_error("unimplemented");
}

void Lexer::step() {
    if (look < eos()) {
        // Register newline first
        if (*look == '\n') {
            line_off.push_back(pos());
        }
        look++;
    } else {
        look = eos();
    }
}

Token Lexer::lex_ident() {
    skip_while([](char c) { return isalnum(c) || c == '_'; });

    // Keyword lookup
    for (auto &p : keyword_map) {
        auto text = p.first;
        auto type = p.second;

        // If the leftover source text is shorter than the keyword, skip it.
        if (static_cast<size_t>(eos() - curr) < text.length()) {
            continue;
        }

        StringView sv{curr, text.length()};
        if (sv == text) {
            return make_token_with_literal(type);
        }
    }
    // No keyword match; it's an identifier
    return make_token_with_literal(TokenType::ident);
}

Token Lexer::lex_number() {
    skip_while(isdigit);
    return make_token_with_literal(TokenType::number);
}

Token Lexer::lex_string() {
    step(); // skip opening "
    while (look < eos()) {
        skip_while([](char c) { return !(c == '\\' || c == '"'); });
        if (*look == '"') {
            step(); // skip closing "
            break;
        } else {
            // skip the escaped character '\x'
            step();
            step();
        }
    }
    return make_token_with_literal(TokenType::string);
}

Token Lexer::lex_comment() {
    skip_while([](char c) { return c != '\n'; });
    return make_token_with_literal(TokenType::comment);
}

Token Lexer::lex_symbol() {
    for (auto &p : symbol_map) {
        auto text = p.first;
        auto type = p.second;

        // If the leftover source text is shorter than the keyword, skip it.
        if (static_cast<size_t>(eos() - curr) < text.length())
            continue;

        StringView sv{curr, text.length()};
        if (sv == text) {
            look = curr + text.length();
            return make_token_with_literal(type);
        }
    }
    // Match fail
    error("unrecognzied token");
    return make_token(TokenType::none);
}

Lexer::char_iterator Lexer::lookn(long n) const {
    if (look + n < eos())
        return look + n;
    return eos();
}

Token Lexer::make_token(TokenType type) {
    return Token{type, pos()};
}

Token Lexer::make_token_with_literal(TokenType type) {
    StringView text{curr, static_cast<size_t>(look - curr)};
    return Token{type, pos(), text};
}

Token Lexer::lex() {
    skip_whitespace();

    if (curr == eos())
        return Token{TokenType::eos, pos()};

    Token tok;
    switch (*curr) {
    case 0:
        // TODO emit a warning
        std::cerr << "unexpected null in source\n";
        break;
    case '"':
        tok = lex_string();
        break;
    case '/':
        if (*lookn(1) == '/') {
            tok = lex_comment();
        } else {
            tok = lex_symbol(); // divide
        }
        break;
    default:
        if (std::isalpha(*curr) || *curr == '_') {
            tok = lex_ident();
        } else if (std::isdigit(*curr)) {
            tok = lex_number();
        } else {
            tok = lex_symbol();
        }
        break;
    }

    // Advance lexed position
    curr = look;

    return tok;
}

Token Lexer::peek() {
    auto save = curr;
    auto token = lex();
    curr = save;
    return token;
}

template <typename F>
void Lexer::skip_while(F &&lambda) {
    while (look < eos() && lambda(*look)) {
        step();
    }
}

void Lexer::skip_whitespace() {
    skip_while(isspace);
    curr = look;
}

void Lexer::error(const std::string &msg) {
    auto loc = src.locate(pos());
    std::cout << src.filename << ":" << loc.first << ":" << loc.second << ": ";
    std::cout << "lex error: " << msg << std::endl;
    exit(1);
}

} // namespace comp
