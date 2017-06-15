// -*- C++ -*-

#include "Source.h"
#include <string>
#include <string_view>
#include <algorithm>

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
  std::string_view::iterator end() const { return std::cend(sv); }

public:
  /// Make a lexer for the given file.
  Lexer(Source &src)
      : src(src), sv(src.buffer().data()), look(std::cbegin(sv)) {}

  /// Make a Lexer from the given memory buffer.  Assumes the buffer
  /// would outlive the lexer.
  Lexer(std::string_view sv);

  void Lex();
};
