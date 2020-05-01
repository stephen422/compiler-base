#include "lexer.h"
#include "fmt/core.h"
#include <cctype>

namespace cmp {

std::string tokenTypeToString(Tok kind) {
    if (kind == Tok::newline)
        return "\\n";
    for (auto &p : symbol_map) {
        if (p.second == kind)
            return std::string{p.first};
    }
    for (auto &p : keyword_map) {
        if (p.second == kind)
            return std::string{p.first};
    }
    return "";
}

bool isIdentOrKeyword(const Token tok) {
  return tok.kind == Tok::ident ||
         (tok.kind > Tok::KWSTART && tok.kind < Tok::KWEND);
}

bool Token::isAny(const std::vector<Tok> &kinds) const {
  for (auto cand : kinds)
    if (kind == cand)
      return true;
  return false;
}

std::string Token::str() const {
    if (kind == Tok::newline)
        return std::string{"\\n"};
    return std::string{text};
}

// Advances 'look', not 'curr'. 'curr' is used as a manual marking position for
// each token start.
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

Token Lexer::lex_ident_or_keyword() {
    skip_while([](char c) { return isalnum(c) || c == '_'; });

    // Keyword lookup
    for (auto &p : keyword_map) {
        auto text = p.first;
        auto kind = p.second;

        // If the remaining source text is shorter than the keyword, skip it.
        if (static_cast<size_t>(eos() - curr) < text.length()) {
            continue;
        }

        std::string_view sv{curr, static_cast<size_t>(look - curr)};
        if (sv == text) {
            return make_token_with_literal(kind);
        }
    }
    // No keyword match; it's an identifier
    num_ident++;
    return make_token_with_literal(Tok::ident);
}

Token Lexer::lex_number() {
    skip_while(isdigit);
    return make_token_with_literal(Tok::number);
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
    return make_token_with_literal(Tok::string);
}

Token Lexer::lex_comment() {
    skip_while([](char c) { return c != '\n'; });
    auto tok = make_token_with_literal(Tok::comment);
    return tok;
}

Token Lexer::lex_symbol() {
    for (auto &p : symbol_map) {
        auto text = p.first;
        auto kind = p.second;

        // If the leftover source text is shorter than the keyword, skip it.
        if (static_cast<size_t>(eos() - curr) < text.length())
            continue;

        std::string_view sv{curr, text.length()};
        if (sv == text) {
            look = curr + text.length();
            return make_token_with_literal(kind);
        }
    }
    // Match fail
    error("unrecognized token");
    return make_token(Tok::none);
}

const char *Lexer::lookn(long n) const {
    if (look + n < eos()) {
        return look + n;
    }
    return eos();
}

Token Lexer::make_token(Tok kind) {
    return Token{kind, pos()};
}

Token Lexer::make_token_with_literal(Tok kind) {
    std::string_view text{curr, static_cast<size_t>(look - curr)};
    return Token{kind, pos(), text};
}

Token Lexer::lex() {
    skip_whitespace();

    if (curr == eos())
        return Token{Tok::eos, pos()};

    Token tok;
    switch (*curr) {
    case 0:
        // TODO emit a warning
        fmt::print(stderr, "unexpected null in source\n");
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
            tok = lex_ident_or_keyword();
        } else if (std::isdigit(*curr)) {
            tok = lex_number();
        } else {
            tok = lex_symbol();
        }
        break;
    }

    // Advance token start position
    curr = look;

    return tok;
}

std::vector<Token> Lexer::lex_all() {
    std::vector<Token> v;
    Token tok;
    while ((tok = lex()).kind != Tok::eos) {
        v.push_back(tok);
    }
    v.push_back(tok); // terminate with eos
    return v;
}

Token Lexer::peek() {
    auto save = curr;
    auto token = lex();
    curr = save;
    return token;
}

template <typename F> void Lexer::skip_while(F &&lambda) {
    while (look < eos() && lambda(*look)) {
        step();
    }
}

void Lexer::skip_whitespace() {
    // Newline is significant because the language doesn't have semicolons.
    skip_while([](char c) { return isspace(c) && c != '\n'; });
    curr = look;
}

void Lexer::error(const std::string &msg) {
    auto loc = src.locate(pos());
    std::cout << loc.filename << ":" << loc.line << ":" << loc.col << ": ";
    std::cout << "lex error: " << msg << std::endl;
    exit(1);
}

} // namespace cmp
