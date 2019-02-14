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

enum class TokenType {
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
    kw_return,
    none // not found
};

// This is under linear search, so it is better to place more frequently used
// symbols at the top.
static const std::pair<StringView, TokenType> symbol_map[] {
    {"\"", TokenType::doublequote},
    {"\n", TokenType::newline},
    {"->", TokenType::arrow},
    {"'", TokenType::quote},
    {"(", TokenType::lparen},
    {")", TokenType::rparen},
    {"{", TokenType::lbrace},
    {"}", TokenType::rbrace},
    {"[", TokenType::lbracket},
    {"]", TokenType::rbracket},
    {"<", TokenType::lesserthan},
    {">", TokenType::greaterthan},
    {".", TokenType::dot},
    {",", TokenType::comma},
    {":", TokenType::colon},
    {";", TokenType::semicolon},
    {"=", TokenType::equals},
    {"+", TokenType::plus},
    {"-", TokenType::minus},
    {"*", TokenType::star},
    {"&", TokenType::ampersand},
    {"^", TokenType::caret},
    {"~", TokenType::tilde},
    {"/", TokenType::slash},
    {"\\", TokenType::backslash},
    {"|", TokenType::pipe},
    {"!", TokenType::bang},
    {"?", TokenType::question},
    {"#", TokenType::hash},
    {"-", TokenType::dash},
    {"comment", TokenType::comment},
};

static const std::pair<StringView, TokenType> keyword_map[] {
    {"fn", TokenType::kw_fn},
    {"let", TokenType::kw_let},
    {"var", TokenType::kw_var},
    {"if", TokenType::kw_if},
    {"else", TokenType::kw_else},
    {"return", TokenType::kw_return},
};

std::string tokentype_to_string(TokenType type);

// Token contains the type, a view of the text data, and the source position of
// a token.
class Token {
public:
    TokenType type;
    size_t pos;
    StringView text;

    Token() : type(TokenType::none), pos(0), text() {}
    Token(TokenType type, size_t pos) : type(type), pos(pos), text() {}
    Token(TokenType type, size_t pos, StringView text)
        : type(type), pos(pos), text(text) {}
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

private:
    StringView sv;                // view into the source buffer
    char_iterator look;           // lookahead position
    char_iterator curr;           // start of the current token
    std::vector<size_t> line_off; // offsets of each newline

    // Lex functions for a single token type.
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
    Token make_token(TokenType type);
    Token make_token_with_literal(TokenType type);
    template <typename F> void skip_while(F &&lambda);
    void skip_whitespace();
    void error(const std::string &msg);
};

} // namespace cmp

#endif
