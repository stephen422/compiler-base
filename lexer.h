#ifndef LEXER_H
#define LEXER_H

#include "source.h"
#include <algorithm>
#include <string>
#include <string_view>

enum class TokenType {
    eos = 0,
    lparen = '(',
    rparen = ')',
    lbrace = '{',
    rbrace = '}',
    lbracket = '[',
    rbracket = ']',
    lesserthan = '<',
    greaterthan = '>',
    dot = '.',
    comma = ',',
    colon = ':',
    semicolon = ';',
    doublequote = '"',
    quote = '\'',
    equals = '=',
    plus = '+',
    minus = '-',
    star = '*',
    ampersand = '&',
    caret = '^',
    tilde = '~',
    slash = '/',
    backslash = '\\',
    bang = '!',
    question = '?',
    hash = '#',
    dash = '-',
    ident = 256, // skip ASCII
    number,
    string,
    character,
    comment,
    none // not found
};

// TODO: map maybe better?
const std::vector<std::pair<TokenType, std::string>> token_map {
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

struct Token_ {
    TokenType type;
    size_t pos;
    std::string lit;

    Token_(TokenType type, size_t pos) : type(type), pos(pos) {}
    Token_(TokenType type, size_t pos, std::string lit)
        : type(type), pos(pos), lit(std::move(lit)) {}
};

std::ostream& operator<<(std::ostream& os, const Token_& token);

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
    Token_ lex();

    /// Peek the next token without consuming it.
    Token_ peek();

private:
    Source &src;
    std::string_view sv;
    std::string_view::iterator look;

    Token_ lex_ident();
    Token_ lex_number();
    Token_ lex_string();
    Token_ lex_comment();
    Token_ lex_symbol();

    size_t pos() const { return this->look - std::cbegin(sv); }
    std::string_view::iterator eos() const { return std::cend(sv); }

    /// Move current pos to the first non-whitespace char.
    void skip_whitespace();
};

#endif
