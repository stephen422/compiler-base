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
    // Parse a whole file.
    FilePtr parse_file();

    // Parse a toplevel statement.
    ToplevelPtr parse_toplevel();

    // Parse a statement.
    StmtPtr parse_stmt();

    // Parse a return statement.
    NodePtr<ReturnStmt> parse_return_stmt();

    NodePtr<CompoundStmt> parse_compound_stmt();

    // Parse a declaration.
    DeclPtr parse_decl();

    // Parse a variable declaration.
    DeclPtr parse_var_decl();

    // Parse a function declaration.
    FunctionPtr parse_function();

    // Parse an expression.
    ExprPtr parse_expr();

    // Parse a unary expression.
    ExprPtr parse_unary_expr();

    // Extend a unary expression into binary if possible, by parsing any
    // attached RHS.  Returns the owning pointer to the newly constructed binary
    // expression.
    //
    // After the call, 'lhs' is invalidated by being moved away.  Subsequent
    // code should use the returned pointer instead.
    ExprPtr parse_binary_expr_rhs(ExprPtr &lhs, int precedence = 0);

    // Parse a literal expression.
    ExprPtr parse_literal_expr();

    // Get the next token from the lexer.
    void next();

    // Get the precedence of an operator.
    int get_precedence(const Token &op) const;

    void expect(TokenType type);
    void expect_semi();
    void error(const std::string &msg);

    // Figure out the current location (line, col) in the source.
    std::pair<int, int> locate() const {
        return lexer.src.locate(tok.pos);
    }

    Lexer &lexer;
    Token tok; // lookahead token

    // Current operator precedence when parsing an expression.
    // int precedence = 0;
};

} // namespace comp

#endif
