#ifndef LEXER_H
#define LEXER_H

#include "source.hh"
#include <algorithm>
#include <string>
#include <string_view>
#include <variant>

struct Ident {
  const std::string lit;
};

struct Number {
  const std::string lit;
};

struct String {
  const std::string lit;
};

struct Character {
  const std::string lit;
};

struct Comment {
  const std::string lit;
};

struct Lparen {};
struct Rparen {};
struct Lbrace {};
struct Rbrace {};
struct Lbracket {};
struct Rbracket {};
struct Lesserthan {};
struct Greaterthan {};
struct Dot {};
struct Comma {};
struct Colon {};
struct Semicolon {};
struct Doublequote {};
struct Quote {};
struct Equals {};
struct Plus {};
struct Minus {};
struct Star {};
struct Ampersand {};
struct Caret {};
struct Tilde {};
struct Slash {};
struct Backslash {};
struct Bang {};
struct Question {};
struct Hash {};
struct Bar {};
struct Eos {};

using Token =
    std::variant<Ident, Number, String, Character, Comment, Lparen, Rparen,
                 Lbrace, Rbrace, Lbracket, Rbracket, Lesserthan, Greaterthan,
                 Dot, Comma, Colon, Semicolon, Doublequote, Quote, Equals, Plus,
                 Minus, Star, Ampersand, Caret, Tilde, Slash, Backslash, Bang,
                 Question, Hash, Bar, Eos>;

std::ostream& operator<<(std::ostream& os, const Token& token);

enum class Keywords {
  fn,
  int_
};

/// Represents a lexer state machine.
/// This lexer assumes that the source data will outlive it.
class Lexer {
  /// The source being lexed.
  /// Should outlive this lexer.
  Source& src;
  std::string_view sv;

  // The lookahead pos.
  std::string_view::iterator look;

  /// Returns the end pos of the source.
  std::string_view::iterator eos() const { return std::cend(sv); }

  /// Move current pos to the first non-whitespace char.
  void skip_whitespace();

public:
  /// Make a lexer for the given file.
  Lexer(Source& src)
      : src(src), sv(src.buffer().data(), src.buffer().size()),
        look(std::cbegin(sv)) {}

  /// Make a Lexer from the given memory buffer.  Assumes the buffer
  /// would outlive the lexer.
  Lexer(std::string_view sv);

  /// Lex the current token and advance to the next one.
  Token lex();

  /// Peek the next token without consuming it.
  Token peek();

  /// Lex the next identifier.
  const Ident lex_ident();

  /// Lex the next number literal.
  const Number lex_number();

  /// Lex the next string literal.
  const String lex_string();

  /// Lex the next single-line comment.
  const Comment lex_comment();

  /// Lex any token that consists of a single character, such as
  /// '.', ';', '(', ')', etc.
  template <typename T> const Token lex_single();
};

#endif