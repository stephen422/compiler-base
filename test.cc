#include "catch.hpp"
#include "driver.h"
#include "parser.h"

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
    SECTION("basic") {
        auto d = Driver::from_path(Path{"../test_parser.txt"});
        d.compile();
        REQUIRE(d.verify());
    }
    SECTION("struct") {
        auto d = Driver::from_path(Path{"../test_struct.txt"});
        d.compile();
        REQUIRE(d.verify());
    }
    SECTION("enum") {
      auto d = Driver::from_path(Path{"../test_enum.txt"});
      d.compile();
      REQUIRE(d.verify());
    }
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

TEST_CASE("Borrow checking") {
    auto d = Driver::from_path(Path{"../test_borrowck.txt"});
    d.compile();
    REQUIRE(d.verify());
}

TEST_CASE("Return checking") {
    auto d = Driver::from_path(Path{"../test_returnck.txt"});
    d.compile();
    REQUIRE(d.verify());
}

TEST_CASE("Codegen") {
    auto d = Driver::from_path(Path{"../test_codegen.txt"});
    d.compile();
    REQUIRE(d.verify());
}
