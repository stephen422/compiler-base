#ifndef LEXER_H
#define LEXER_H

#include "source.h"
#include <algorithm>
#include <string>
#include <string_view>

enum class TokenType {
    eos,
    lparen,
    rparen,
    lbrace,
    rbrace,
    lbracket,
    rbracket,
    lesserthan,
    greaterthan,
    dot,
    comma,
    colon,
    semicolon,
    doublequote,
    quote,
    equals,
    plus,
    minus,
    star,
    ampersand,
    caret,
    tilde,
    slash,
    backslash,
    bang,
    question,
    hash,
    dash,
    ident,
    number,
    string,
    character,
    comment,
    none // not found
};

static const std::pair<TokenType, std::string> token_map[] {
    {TokenType::doublequote, "\""},
    {TokenType::quote, "'"},
    {TokenType::slash, "/"},
    {TokenType::lparen, "("},
    {TokenType::rparen, ")"},
    {TokenType::lbrace, "{"},
    {TokenType::rbrace, "}"},
    {TokenType::lbracket, "["},
    {TokenType::rbracket, "]"},
    {TokenType::lesserthan, "<"},
    {TokenType::greaterthan, ">"},
    {TokenType::dot, "."},
    {TokenType::comma, ","},
    {TokenType::colon, ":"},
    {TokenType::semicolon, ";"},
    {TokenType::equals, "="},
    {TokenType::plus, "+"},
    {TokenType::minus, "-"},
    {TokenType::star, "*"},
    {TokenType::ampersand, "&"},
    {TokenType::caret, "^"},
    {TokenType::tilde, "~"},
    {TokenType::backslash, "\\"},
    {TokenType::bang, "!"},
    {TokenType::question, "?"},
    {TokenType::hash, "#"},
    {TokenType::dash, "-"},
};

struct Token {
    TokenType type;
    size_t pos;
    std::string lit;

    Token(TokenType type, size_t pos) : type(type), pos(pos) {}
    Token(TokenType type, size_t pos, std::string lit)
        : type(type), pos(pos), lit(std::move(lit)) {}
};

std::ostream& operator<<(std::ostream& os, const Token& token);

enum class Keywords {
    fn,
    int_
};

/// Represents a lexer state machine.
/// Assumes that the associated Source outlives it.
class Lexer {
public:
    /// Make a lexer for the given source.
    Lexer(Source& s)
        : src(s), sv(src.buf.data(), src.buf.size()),
        look(std::cbegin(sv)) {}

    /// Lex the current token and advance to the next one.
    Token lex();

    /// Peek the next token without consuming it.
    Token peek();

private:
    Source &src;
    std::string_view sv;
    std::string_view::iterator look;

    Token lex_ident();
    Token lex_number();
    Token lex_string();
    Token lex_comment();
    Token lex_symbol();

    size_t pos() const { return this->look - std::cbegin(sv); }
    std::string_view::iterator eos() const { return std::cend(sv); }

    /// Move current pos to the first non-whitespace char.
    void skip_whitespace();
};

#endif
