#ifndef LEXER_H
#define LEXER_H

#include "source.h"
#include <algorithm>
#include <map>
#include <string>
#include <string_view>

enum class TokenType {
    eos,
    arrow,
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
    pipe,
    bang,
    question,
    hash,
    dash,
    ident,
    number,
    string,
    character,
    comment,
    kw_timescale,
    kw_include,
    kw_define,
    kw_parameter,
    kw_module,
    kw_endmodule,
    kw_input,
    kw_output,
    kw_inout,
    kw_reg,
    kw_wire,
    kw_assign,
    kw_always,
    kw_posedge,
    kw_negedge,
    kw_if,
    kw_else,
    kw_begin,
    kw_end,
    none // not found
};

// This is under linear search, so it is better to place more frequently used
// symbols at the top.
static const std::pair<TokenType, std::string> symbol_map[] {
    {TokenType::doublequote, "\""},
    {TokenType::arrow, "->"},
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
    {TokenType::pipe, "|"},
    {TokenType::bang, "!"},
    {TokenType::question, "?"},
    {TokenType::hash, "#"},
    {TokenType::dash, "-"},
};

static const std::map<std::string, TokenType> keyword_map {
    {"timescale", TokenType::kw_timescale},
    {"include", TokenType::kw_include},
    {"define", TokenType::kw_define},
    {"parameter", TokenType::kw_parameter},
    {"module", TokenType::kw_module},
    {"endmodule", TokenType::kw_endmodule},
    {"input", TokenType::kw_input},
    {"output", TokenType::kw_output},
    {"inout", TokenType::kw_inout},
    {"reg", TokenType::kw_reg},
    {"wire", TokenType::kw_wire},
    {"assign", TokenType::kw_assign},
    {"always", TokenType::kw_always},
    {"posedge", TokenType::kw_posedge},
    {"negedge", TokenType::kw_negedge},
    {"if", TokenType::kw_if},
    {"else", TokenType::kw_else},
    {"begin", TokenType::kw_begin},
    {"end", TokenType::kw_end},
};

struct Token {
    TokenType type;
    size_t pos;
    std::string lit;

    Token() : type(TokenType::none), pos(0) {}
    Token(TokenType type, size_t pos) : type(type), pos(pos) {}
    Token(TokenType type, size_t pos, const std::string &lit)
        : type(type), pos(pos), lit(std::move(lit)) {}
};

std::ostream& operator<<(std::ostream& os, const Token& token);

/// Represents a lexer state machine.
/// Assumes that the associated Source outlives it.
class Lexer {
    using char_iterator = std::string_view::iterator;

public:
    /// Make a lexer for the given source.
    Lexer(Source& s)
        : src(s), sv(src.buf.data(), src.buf.size()),
        look(std::cbegin(sv)), curr(std::cbegin(sv)) {}

    /// Lex the current token and advance to the next one.
    Token lex();

    /// Peek the next token without consuming it.
    Token peek();

private:
    Source &src;
    std::string_view sv;
    char_iterator look; // lookahead character
    char_iterator curr; // start of the current token

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
