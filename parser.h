// -*- C++ -*-
#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include <memory>

enum class NodeType {
    expr,
    net_decl,
    assign,
    list,
    range,
    atom,
};

template <typename T>
class Ast {
    std::unique_ptr<T> ptr;
};

class AST {
public:
    // Owning pointer to an AST node.
    using NodePtr = std::unique_ptr<AST>;

    AST(NodeType type) : type(type) {}
    AST(NodeType type, const Token &tok) : type(type), tok(std::move(tok)) {}

    void add(NodePtr child);
    void print();

    NodeType type;
    Token tok;
    std::vector<NodePtr> children;
    // Non-owning pointer to the next sibling
    // AST *sibling;
};

AST::NodePtr make_ast(NodeType type);
AST::NodePtr make_ast(NodeType type, const Token &tok);

class Expr : public AST {
public:
    enum ExprType {
        literal,
        unary,
        binary
    } type;

    Expr(ExprType type) : AST(NodeType::expr), type(type) {}

    void print();
};

using ExprPtr = std::unique_ptr<Expr>;

class BinaryExpr : public Expr {
public:
    std::unique_ptr<Expr> lhs;
    std::unique_ptr<Expr> rhs;
};

class Parser {
public:
    Parser(Lexer &lexer) : lexer(lexer), tok(lexer.lex()) {}

    ExprPtr parse();

private:
    // Parse an expression.
    ExprPtr parse_unary_expr();
    ExprPtr parse_binary_expr();

    // Parse an identifier atom.
    ExprPtr parse_ident();

    // Parse a literal atom.
    ExprPtr parse_literal();

    // Parse a, b, c, ...
    AST::NodePtr parse_list();

    // Parse [msb:lsb].
    AST::NodePtr parse_range();

    // Parse wire declaration.
    AST::NodePtr parse_netdecl();

    // Parse continuous assignments.
    AST::NodePtr parse_assign();

    // Get the next token from the lexer.
    void next();

    void expect(TokenType type);
    void expect_semi();
    void error(const std::string &msg);

public:
    Lexer &lexer;
    Token tok; // lookahead token
};

#endif
