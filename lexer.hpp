#ifndef LEXER_H
#define LEXER_H

#include "source.hpp"
#include <algorithm>
#include <string>
#include <string_view>
#include <variant>

enum class TokenType {
    Eos = 0,
    Lparen = '(',
    Rparen = ')',
    Lbrace = '{',
    Rbrace = '}',
    Lbracket = '[',
    Rbracket = ']',
    Lesserthan = '<',
    Greaterthan = '>',
    Dot = '.',
    Comma = ',',
    Colon = ':',
    Semicolon = ';',
    Doublequote = '"',
    Quote = '\'',
    Equals = '=',
    Plus = '+',
    Minus = '-',
    Star = '*',
    Ampersand = '&',
    Caret = '^',
    Tilde = '~',
    Slash = '/',
    Backslash = '\\',
    Bang = '!',
    Question = '?',
    Hash = '#',
    Bar = '-',
    Ident = 256, // skip ASCII
    Number,
    String,
    Char,
    Comment,
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
/// This lexer assumes that the source data will outlive it.
class Lexer {
public:
    /// Make a lexer for the given file.
    Lexer(Source& src)
        : src(src), sv(src.buf.data(), src.buf.size()),
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
