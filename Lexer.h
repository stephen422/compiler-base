// -*- C++ -*-

#include "Source.h"
#include <string>
#include <string_view>

/// Represents a lexer state machine.
/// This lexer assumes that the source data will outlive it.
class Lexer {
  /// The source being lexed.
  /// Should outlive this lexer.
  Source &src;
  std::string_view sv;

public:
  /// Make a lexer for the given file.
  Lexer(Source &src) : src(src) {}

  /// Make a Lexer from the given memory buffer.  Assumes the buffer
  /// would outlive the lexer.
  Lexer(std::string_view sv);

  void Lex();
};
