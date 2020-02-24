// -*- C++ -*-
#ifndef LEXER_H
#define LEXER_H

#include "source.h"
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
    KWSTART,
    kw_func,
    kw_struct,
    kw_let,
    kw_var,
    kw_mut,
    kw_if,
    kw_else,
    kw_int,
    kw_i64,
    kw_float,
    kw_return,
    kw_error,
    KWEND,
    none // not found
};

// This is under linear search, so it is better to place more frequently used
// symbols at the top.
const std::pair<std::string_view, TokenKind> symbol_map[] {
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

const std::pair<std::string_view, TokenKind> keyword_map[] {
    {"func", TokenKind::kw_func},
    {"struct", TokenKind::kw_struct},
    {"let", TokenKind::kw_let},
    {"var", TokenKind::kw_var},
    {"mut", TokenKind::kw_mut},
    {"if", TokenKind::kw_if},
    {"else", TokenKind::kw_else},
    {"int", TokenKind::kw_int},
    {"i64", TokenKind::kw_i64},
    {"return", TokenKind::kw_return},
    {"error", TokenKind::kw_error},
};

std::string tokenTypeToString(TokenKind kind);

// Token contains the kind, a view of the text data, and the source position of
// a token.
struct Token {
    TokenKind kind;
    size_t pos;
    std::string_view text;

    Token() : kind(TokenKind::none), pos(0), text() {}
    Token(TokenKind kind, size_t pos) : kind(kind), pos(pos), text() {}
    Token(TokenKind kind, size_t pos, std::string_view text)
        : kind(kind), pos(pos), text(text) {}
    std::string str() const;
};

bool is_identifier_or_keyword(const Token tok);

/// Represents a lexer state machine.
/// Assumes that the associated Source outlives it.
class Lexer {
public:
    const Source &src;            // source fed to this lexer
    std::string_view sv;          // view into the source buffer
    const char *look;             // lookahead position
    const char *curr;             // start of the current token
    std::vector<size_t> line_off; // offsets of each newline
    size_t num_ident = 0;         // number of identifiers found

    Lexer(const Source &s)
        : src(s), sv(src.buf.data(), src.buf.size()), look(std::cbegin(sv)),
          curr(std::cbegin(sv)) {}

    /// Lex the current token and advance to the next one.
    Token lex();
    /// Lex all of the source text and return the array of tokens.
    std::vector<Token> lex_all();
    /// Peek the next token without consuming it.
    Token peek();
    const Source &source() const { return src; }

private:
    Token lex_ident_or_keyword();
    Token lex_number();
    Token lex_string();
    Token lex_comment();
    Token lex_symbol();

    // Advance lex position by one character.
    void step();
    const char *lookn(long n) const;
    const char *eos() const {
        // account for '\0' at the end
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
