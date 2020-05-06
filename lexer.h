// -*- C++ -*-
#ifndef LEXER_H
#define LEXER_H

#include "source.h"
#include <algorithm>
#include <map>
#include <string>
#include <string_view>

namespace cmp {

enum class Tok {
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
    kw_enum,
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
constexpr std::pair<std::string_view, Tok> symbol_map[] {
    {"\"", Tok::doublequote},
    {"\n", Tok::newline},
    {"->", Tok::arrow},
    {"'", Tok::quote},
    {"(", Tok::lparen},
    {")", Tok::rparen},
    {"{", Tok::lbrace},
    {"}", Tok::rbrace},
    {"[", Tok::lbracket},
    {"]", Tok::rbracket},
    {"<", Tok::lesserthan},
    {">", Tok::greaterthan},
    {".", Tok::dot},
    {",", Tok::comma},
    {":", Tok::colon},
    {";", Tok::semicolon},
    {"=", Tok::equals},
    {"+", Tok::plus},
    {"-", Tok::minus},
    {"*", Tok::star},
    {"&", Tok::ampersand},
    {"^", Tok::caret},
    {"~", Tok::tilde},
    {"/", Tok::slash},
    {"\\", Tok::backslash},
    {"|", Tok::pipe},
    {"!", Tok::bang},
    {"?", Tok::question},
    {"#", Tok::hash},
    {"-", Tok::dash},
    {"comment", Tok::comment},
};

constexpr std::pair<std::string_view, Tok> keyword_map[] {
    {"func", Tok::kw_func},
    {"struct", Tok::kw_struct},
    {"enum", Tok::kw_enum},
    {"let", Tok::kw_let},
    {"var", Tok::kw_var},
    {"mut", Tok::kw_mut},
    {"if", Tok::kw_if},
    {"else", Tok::kw_else},
    {"int", Tok::kw_int},
    {"i64", Tok::kw_i64},
    {"return", Tok::kw_return},
    {"error", Tok::kw_error},
};

std::string tokenTypeToString(Tok kind);

// Token contains the kind, a view of the text data, and the position in the
// source of a token.
struct Token {
    Tok kind;
    size_t pos;
    std::string_view text;

    Token() : kind(Tok::none), pos(0), text() {}
    Token(Tok kind, size_t pos) : kind(kind), pos(pos), text() {}
    Token(Tok kind, size_t pos, std::string_view text)
        : kind(kind), pos(pos), text(text) {}
    bool is_any(std::initializer_list<Tok> &kinds) const;
    std::string str() const;
};

bool isIdentOrKeyword(const Token tok);

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
    Token make_token(Tok kind);
    Token make_token_with_literal(Tok kind);
    template <typename F> void skip_while(F &&lambda);
    void skip_whitespace();
    void error(const std::string &msg);
};

} // namespace cmp

#endif
