// -*- C++ -*-
#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include <memory>

enum class NodeType {
    net_decl,
    assign,
    list,
    range,
    atom,
};

struct AST {
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

class Expr {
public:
    enum {
        unary,
        binary
    } type;
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

    AST::NodePtr parse();

private:
    // Parse an expression.
    std::unique_ptr<Expr> parse_unary_expr();
    std::unique_ptr<Expr> parse_binary_expr();

    // Parse an identifier atom.
    AST::NodePtr parse_ident();

    // Parse a literal atom.
    AST::NodePtr parse_literal();

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
