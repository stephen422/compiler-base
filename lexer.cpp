#include "lexer.h"
#include <cctype>

std::ostream &operator<<(std::ostream &os, const Token &token) {
    os << "[" << token.lit << "]";
    return os;
}

void Token::print() {
    throw std::runtime_error("unimplemented");
}

void Lexer::step() {
    if (look < eos()) {
        // Register newline first
        if (*look == '\n')
            line_off.push_back(pos());
        look++;
    } else {
        look = sv.end();
    }
}

Token Lexer::lex_ident() {
    skip_while([](char c) { return isalnum(c) || c == '_'; });
    // Keyword lookup
    for (auto &p : keyword_map) {
        auto lit = p.first;
        auto type = p.second;
        StringView sv{curr, lit.length()};
        if (sv == lit) {
            return make_token_with_literal(type);
        }
    }
    // Match fail
    return make_token_with_literal(TokenType::ident);
}

Token Lexer::lex_number() {
    skip_while(isdigit);
    return make_token_with_literal(TokenType::number);
}

Token Lexer::lex_string() {
    look = curr + 1; // skip "
    while (look != eos()) {
        skip_while([](char c) { return !(c == '\\' || c == '"'); });
        if (*look == '"') {
            // skip " and end
            step();
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
        auto lit = p.first;
        auto type = p.second;
        StringView sv{curr, lit.length()};
        if (sv == lit) {
            look = curr + lit.length();
            return make_token_with_literal(type);
        }
    }
    // Match fail
    step();
    error("unrecognzied token");
    return make_token(TokenType::none);
}

Lexer::char_iterator Lexer::lookn(long n) const {
    if (curr + n < eos())
        return curr + n;
    return eos();
}

Token Lexer::make_token(TokenType type) {
    return Token{type, pos()};
}

Token Lexer::make_token_with_literal(TokenType type) {
    StringView lit{curr, static_cast<size_t>(look - curr)};
    return Token{type, pos(), lit};
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
    while (look < eos() && lambda(*look))
        step();
}

void Lexer::skip_whitespace() {
    skip_while(isspace);
    curr = look;
}

void Lexer::error(const std::string &msg) {
    std::cout << "error: " << msg << ": ";

    if (line_off.empty()) {
        std::cout << "oops!\n";
        exit(1);
    }

    // Search linearly for the current line.
    // line number is 0-based.
    int line;
    for (line = 0; line < static_cast<int>(line_off.size()); line++) {
        if (line_off[line] >= pos())
            break;
    }
    int col = pos() - (line > 0 ? line_off[line - 1] : 0);
    std::cout << (line + 1) << "," << col << std::endl;
    exit(1);
}
