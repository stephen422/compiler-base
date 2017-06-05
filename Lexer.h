#include "Source.h"
#include <string>
#include <string_view>

/// Represents a Lexer state machine.
/// This doesn't handle file reading,
class Lexer {
  Source src;
  std::string_view sv;

public:
  /// Make a lexer for the given file.
  Lexer(const std::string &path);

  /// Make a Lexer from the given memory buffer.  Assumes the buffer
  /// would outlive the lexer.
  Lexer(std::string_view sv);
};
