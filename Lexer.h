// -*- C++ -*-

#include "Source.h"
#include <algorithm>
#include <string>
#include <string_view>
#include <variant>

struct Ident {
  const std::string name;
};

struct Number {
  const std::string value;
};

struct Eof {};

using Token = std::variant<Ident, Number, Eof>;

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

  /// Lex the next numeric literal.
  const Number lex_numeric();

  /// Move current pos to the first non-whitespace char.
  void skip_whitespace();

public:
  /// Make a lexer for the given file.
  Lexer(Source &src)
      : src(src), sv(src.buffer().data()), look(std::cbegin(sv)) {}

  /// Make a Lexer from the given memory buffer.  Assumes the buffer
  /// would outlive the lexer.
  Lexer(std::string_view sv);

  Token lex();
};
