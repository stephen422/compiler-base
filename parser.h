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

    // Parse a unary expression.
    ExprPtr parse_unary_expr();

    // Extend a unary expression into binary if possible, by parsing any
    // attached RHS.  Returns the owning pointer to the newly constructed binary
    // expression.
    //
    // After the call, the owning pointer to lhs is moved away and invalidated
    // in case the binary extension is successful.
    ExprPtr parse_binary_expr_rhs(ExprPtr &lhs, int precedence = 0);

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

    // Get the precedence of an operator.
    int get_precedence(const Token &op) const;

    void expect(TokenType type);
    void expect_semi();
    void error(const std::string &msg);

    Lexer &lexer;
    Token tok; // lookahead token

    // Current operator precedence when parsing an expression.
    // int precedence = 0;
};

} // namespace comp

#endif
