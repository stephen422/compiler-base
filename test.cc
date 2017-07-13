#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include "Lexer.h"

TEST_CASE( "String lexing", "[lex_string]" ) {
  Source src{"\"Hello, there!\""};
  Lexer lexer{src};
  auto tok = lexer.lex_string();
  REQUIRE ( tok.s == "\"Hello, there!\"" );
}
