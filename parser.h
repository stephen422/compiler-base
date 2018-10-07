// -*- C++ -*-
#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"

namespace comp {

class Parser {
public:
    Parser(Lexer &lexer) : lexer(lexer), tok(lexer.lex()) {}

    AstNodePtr parse();

private:
    // Parse an expression.
    ExprPtr parse_expr();
    ExprPtr parse_unary_expr();
    ExprPtr parse_binary_or_unary_expr();

    // Parse an identifier atom.
    ExprPtr parse_ident();

    // Parse a literal atom.
    ExprPtr parse_literal();

    // Parse a, b, c, ...
    AstNodePtr parse_list();

    // Parse [msb:lsb].
    AstNodePtr parse_range();

    // Parse wire declaration.
    AstNodePtr parse_netdecl();

    // Parse continuous assignments.
    AstNodePtr parse_assign();

    // Get the next token from the lexer.
    void next();

    void expect(TokenType type);
    void expect_semi();
    void error(const std::string &msg);

public:
    Lexer &lexer;
    Token tok; // lookahead token
};

} // namespace comp

#endif
