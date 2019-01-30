#define CATCH_CONFIG_MAIN
#include "parser.h"
#include "catch.hpp"

using namespace cmp;

template <typename T>
constexpr T *as(const AstNodePtr &p) {
    return static_cast<T *>(p.get());
}

TEST_CASE("Expression parsing", "[parse_expr]") {
    SECTION("left associativity") {
        Source s{"a + b + c + d + e"};
        Lexer l{s};
        Parser p{l};
        auto e = p.parse();
        REQUIRE(e->as<Expr>()->flatten() == "((((a+b)+c)+d)+e)");
    }
    SECTION("operator precedence") {
        Source s{"a * b + c / d * e"};
        Lexer l{s};
        Parser p{l};
        auto e = p.parse();
        REQUIRE(e->as<Expr>()->flatten() == "((a*b)+((c/d)*e))");
    }
    SECTION("variable declaration") {
        Source s{"let a;"};
        Lexer l{s};
        Parser p{l};
        auto e = p.parse();
        REQUIRE(e->as<VarDecl>()->id.text == "a");
        REQUIRE(e->as<VarDecl>()->mut == false);
    }
}

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
        std::cout << "Source length: " << s.length() << std::endl;
        auto tok = l.lex();
        REQUIRE(tok.text == "\"Hello,");
    }
}

TEST_CASE("Comment lexing", "[lex_comment]") {
    Source s{"// Hello there\n// General Kenobi"};
    Lexer l{s};
    auto tok = l.lex();
    REQUIRE(tok.text == "// Hello there");
}
