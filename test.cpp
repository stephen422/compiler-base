#define CATCH_CONFIG_MAIN
#include "parser.h"
#include "catch.hpp"

using namespace comp;

TEST_CASE("Parsing", "[parse]") {
    Source src{"a + b + c"};
    Lexer l{src};
    Parser p{l};
    p.parse();
}

#if 0
TEST_CASE("String lexing", "[lex_string]") {
  SECTION("no escape chars") {
    Source src{"\"Hello, there!\""};
    Lexer lexer{src};
    auto tok = lexer.lex_string();
    REQUIRE(tok.s == "\"Hello, there!\"");
  }
  SECTION("with escape chars") {
    // Hairy stuff...
    Source src{"\"Hello, \\\"Bartleby!\\\"\""};
    Lexer lexer{src};
    auto tok = lexer.lex_string();
    REQUIRE(tok.s == "\"Hello, \\\"Bartleby!\\\"\"");
  }
  SECTION("only escape chars") {
    // Hairy stuff...
    Source src{"\"\\\"\\n\\\"\\t\\\"\""};
    Lexer lexer{src};
    auto tok = lexer.lex_string();
    REQUIRE(tok.s == "\"\\\"\\n\\\"\\t\\\"\"");
  }
  SECTION("meets EOS") {
    Source src{"\"Hello,"};
    Lexer lexer{src};
    auto tok = lexer.lex_string();
    REQUIRE(tok.s == "\"Hello,");
  }
}

TEST_CASE("Comment lexing", "[lex_comment]") {
  Source src{"// Hello there\n// General Kenobi"};
  Lexer lexer{src};
  auto tok = lexer.lex_comment();
  REQUIRE(tok.s == "// Hello there");
}
#endif
