#define CATCH_CONFIG_MAIN
#include "driver.h"
#include "parser.h"
#include "catch.hpp"

using namespace cmp;

TEST_CASE("String lexing", "[lex_string]") {
  SECTION("no escape chars") {
    Source s{"\"Hello, there!\""};
    Lexer l{s};
    auto tok = l.lex();
    REQUIRE(tok.text == "\"Hello, there!\"");
  }
  SECTION("with escape chars") {
    Source s{"\"Hello, \\\"Bartleby!\\\"\""};
    Lexer l{s};
    auto tok = l.lex();
    REQUIRE(tok.text == "\"Hello, \\\"Bartleby!\\\"\"");
  }
  SECTION("only escape chars") {
    Source s{"\"\\\"\\n\\\"\\t\\\"\""};
    Lexer l{s};
    auto tok = l.lex();
    REQUIRE(tok.text == "\"\\\"\\n\\\"\\t\\\"\"");
  }
  SECTION("meets EOS") {
    Source s{"\"Hello,"};
    Lexer l{s};
    auto tok = l.lex();
    REQUIRE(tok.text == "\"Hello,\n");
  }
}

TEST_CASE("Comment lexing", "[lex_comment]") {
  Source s{"// Hello there\n// General Kenobi"};
  Lexer l{s};
  auto tok = l.lex();
  REQUIRE(tok.text == "// Hello there");
}

TEST_CASE("Parsing") {
  auto d = Driver::from_path(Path{"../test_parser.txt"});
  d.compile();
  REQUIRE(d.verify());
}

TEST_CASE("Name binding") {
  auto d = Driver::from_path(Path{"../test_namebind.txt"});
  d.compile();
  REQUIRE(d.verify());
}

TEST_CASE("Type checking") {
  auto d = Driver::from_path(Path{"../test_typeck.txt"});
  d.compile();
  REQUIRE(d.verify());
}
