// -*- C++ -*-
#ifndef LEXER_H
#define LEXER_H

#include "source.h"
#include "string_view.h"
#include <algorithm>
#include <map>
#include <string>
#include <string_view>

namespace cmp {

enum class TokenKind {
    eos,
    newline,
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
    kw_fn,
    kw_let,
    kw_var,
    kw_if,
    kw_else,
    kw_int,
    kw_i64,
    kw_float,
    kw_return,
    none // not found
};

// This is under linear search, so it is better to place more frequently used
// symbols at the top.
static const std::pair<StringView, TokenKind> symbol_map[] {
    {"\"", TokenKind::doublequote},
    {"\n", TokenKind::newline},
    {"->", TokenKind::arrow},
    {"'", TokenKind::quote},
    {"(", TokenKind::lparen},
    {")", TokenKind::rparen},
    {"{", TokenKind::lbrace},
    {"}", TokenKind::rbrace},
    {"[", TokenKind::lbracket},
    {"]", TokenKind::rbracket},
    {"<", TokenKind::lesserthan},
    {">", TokenKind::greaterthan},
    {".", TokenKind::dot},
    {",", TokenKind::comma},
    {":", TokenKind::colon},
    {";", TokenKind::semicolon},
    {"=", TokenKind::equals},
    {"+", TokenKind::plus},
    {"-", TokenKind::minus},
    {"*", TokenKind::star},
    {"&", TokenKind::ampersand},
    {"^", TokenKind::caret},
    {"~", TokenKind::tilde},
    {"/", TokenKind::slash},
    {"\\", TokenKind::backslash},
    {"|", TokenKind::pipe},
    {"!", TokenKind::bang},
    {"?", TokenKind::question},
    {"#", TokenKind::hash},
    {"-", TokenKind::dash},
    {"comment", TokenKind::comment},
};

static const std::pair<StringView, TokenKind> keyword_map[] {
    {"fn", TokenKind::kw_fn},
    {"let", TokenKind::kw_let},
    {"var", TokenKind::kw_var},
    {"if", TokenKind::kw_if},
    {"else", TokenKind::kw_else},
    {"int", TokenKind::kw_int},
    {"i64", TokenKind::kw_i64},
    {"return", TokenKind::kw_return},
};

std::string tokentype_to_string(TokenKind kind);

// Token contains the kind, a view of the text data, and the source position of
// a token.
class Token {
public:
    TokenKind kind;
    size_t pos;
    StringView text;

    Token() : kind(TokenKind::none), pos(0), text() {}
    Token(TokenKind kind, size_t pos) : kind(kind), pos(pos), text() {}
    Token(TokenKind kind, size_t pos, StringView text)
        : kind(kind), pos(pos), text(text) {}
    void print();
};

std::ostream& operator<<(std::ostream& os, const Token& token);

/// Represents a lexer state machine.
/// Assumes that the associated Source outlives it.
class Lexer {
public:
    using char_iterator = StringView::iterator;

    /// Make a lexer for the given source.
    Lexer(Source &s)
        : src(s), sv(src.buf.data(), src.buf.size()), look(std::cbegin(sv)),
          curr(std::cbegin(sv)) {}

    /// Lex the current token and advance to the next one.
    Token lex();
    /// Completely lex the source text and return array of tokens.
    std::vector<Token> lex_all();

    /// Peek the next token without consuming it.
    Token peek();

    // Source object associated to this lexer.
    Source &src;

    StringView sv;                // view into the source buffer
    char_iterator look;           // lookahead position
    char_iterator curr;           // start of the current token
    std::vector<size_t> line_off; // offsets of each newline
    size_t num_ident = 0;         // number of identifiers found

    // Lex functions for a single token kind.
    Token lex_ident();
    Token lex_number();
    Token lex_string();
    Token lex_comment();
    Token lex_symbol();

    // Advance lex position by one character.
    void step();
    char_iterator lookn(long n) const;
    char_iterator eos() const {
        // Account for '\0' at the end.
        return std::cend(sv) - 1;
    }
    size_t pos() const { return curr - std::cbegin(sv); }
    Token make_token(TokenKind kind);
    Token make_token_with_literal(TokenKind kind);
    template <typename F> void skip_while(F &&lambda);
    void skip_whitespace();
    void error(const std::string &msg);
};

} // namespace cmp

#endif
