// -*- C++ -*-

#include "Source.h"
#include <algorithm>
#include <string>
#include <string_view>
#include <variant>

struct Ident {
  const std::string s;
};

struct Number {
  const std::string s;
};

struct String {
  const std::string s;
};

struct Lparen {};
struct Rparen {};
struct Lbrace {};
struct Rbrace {};
struct Dot {};
struct Comma {};
struct Colon {};
struct Semicolon {};
struct Eos {};

using Token = std::variant<Ident, Number, String, Lparen, Rparen, Lbrace,
                           Rbrace, Dot, Comma, Colon, Semicolon, Eos>;

/// Represents a lexer state machine.
/// This lexer assumes that the source data will outlive it.
class Lexer {
  /// The source being lexed.
  /// Should outlive this lexer.
  Source &src;
  std::string_view sv;

  // The lookahead pos.
  std::string_view::iterator look;

  /// Returns the end pos of the source.
  std::string_view::iterator eos() const { return std::cend(sv); }

  /// Lex the next identifier.
  const Ident lex_ident();

  /// Lex the next number literal.
  const Number lex_number();

  /// Lex any token that consists of a single character, such as
  /// '.', ';', '(', ')', etc.
  template <typename T> const Token lex_single();

  /// Move current pos to the first non-whitespace char.
  void skip_whitespace();

public:
  /// Make a lexer for the given file.
  Lexer(Source &src)
      : src(src), sv(src.buffer().data(), src.buffer().size()),
        look(std::cbegin(sv)) {}

  /// Make a Lexer from the given memory buffer.  Assumes the buffer
  /// would outlive the lexer.
  Lexer(std::string_view sv);

  /// Lex the current token and advance to the next one.
  Token lex();
};
