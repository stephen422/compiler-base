#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include "Lexer.h"

TEST_CASE( "String lexing", "[lex_string]" ) {
  SECTION("no escape chars") {
    Source src{"\"Hello, there!\""};
    Lexer lexer{src};
    auto tok = lexer.lex_string();
    REQUIRE( tok.s == "\"Hello, there!\"" );
  }
  SECTION("with escape chars") {
    // Hairy stuff...
    Source src{"\"Hello, \\\"Bartleby!\\\"\""};
    Lexer lexer{src};
    auto tok = lexer.lex_string();
    REQUIRE( tok.s == "\"Hello, \\\"Bartleby!\\\"\"" );
  }
  SECTION("only escape chars") {
    // Hairy stuff...
    Source src{"\"\\\"\\n\\\"\\t\\\"\""};
    Lexer lexer{src};
    auto tok = lexer.lex_string();
    REQUIRE( tok.s == "\"\\\"\\n\\\"\\t\\\"\"" );
  }
}

TEST_CASE( "Comment lexing", "[lex_comment]" ) {
  Source src{"// Hello there\n// General Kenobi"};
  Lexer lexer{src};
  auto tok = lexer.lex_comment();
  REQUIRE( tok.s == "// Hello there" );
}
