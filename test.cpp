#define CATCH_CONFIG_MAIN
#include "parser.h"
#include "catch.hpp"

using namespace comp;

template <typename T>
constexpr T *as(const AstNodePtr &p) {
    return static_cast<T *>(p.get());
}

TEST_CASE("Expression parsing", "[parse_expr]") {
    SECTION("left associativity") {
        Source src{"a + b + c + d + e"};
        Lexer l{src};
        Parser p{l};
        auto e = p.parse();
        REQUIRE(as<Expr>(e)->flatten() == "((((a+b)+c)+d)+e)");
    }
    SECTION("operator precedence") {
        Source src{"a * b + c / d * e"};
        Lexer l{src};
        Parser p{l};
        auto e = p.parse();
        REQUIRE(as<Expr>(e)->flatten() == "((a*b)+((c/d)*e))");
    }
    SECTION("variable declaration") {
        Source src{"let a;"};
        Lexer l{src};
        Parser p{l};
        auto e = p.parse();
        REQUIRE(as<VarDecl>(e)->id.lit == "a");
        REQUIRE(as<VarDecl>(e)->mut == false);
    }
}

TEST_CASE("String lexing", "[lex_string]") {
    SECTION("no escape chars") {
        Source src{"\"Hello, there!\""};
        Lexer lexer{src};
        auto tok = lexer.lex();
        REQUIRE(tok.lit == "\"Hello, there!\"");
    }
    SECTION("with escape chars") {
        Source src{"\"Hello, \\\"Bartleby!\\\"\""};
        Lexer lexer{src};
        auto tok = lexer.lex();
        REQUIRE(tok.lit == "\"Hello, \\\"Bartleby!\\\"\"");
    }
    SECTION("only escape chars") {
        Source src{"\"\\\"\\n\\\"\\t\\\"\""};
        Lexer lexer{src};
        auto tok = lexer.lex();
        REQUIRE(tok.lit == "\"\\\"\\n\\\"\\t\\\"\"");
    }
    SECTION("meets EOS") {
        Source src{"\"Hello,"};
        Lexer lexer{src};
        auto tok = lexer.lex();
        REQUIRE(tok.lit == "\"Hello,");
    }
}

TEST_CASE("Comment lexing", "[lex_comment]") {
    Source src{"// Hello there\n// General Kenobi"};
    Lexer lexer{src};
    auto tok = lexer.lex();
    REQUIRE(tok.lit == "// Hello there");
}
