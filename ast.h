// -*- C++ -*-
#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <iostream>
#include <memory>

namespace comp {

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

class AstNode {
public:
    // Owning pointer to an AST node.
    using OwnPtr = std::unique_ptr<AstNode>;

    AstNode() : AstNode(NodeType::atom) {}
    AstNode(NodeType type) : type(type) {}
    AstNode(NodeType type, const Token &tok) : type(type), tok(std::move(tok)) {}
    virtual ~AstNode() = default;

    void add(OwnPtr child);
    virtual void print();

    // Convenience ostream for omitting explicit indeinting in the AST printing
    // code.
    std::ostream& out() const {
        std::cout << std::string(depth, ' ');
        return std::cout;
    }

    NodeType type;
    Token tok;

    // Indentation of the current node when dumping AST.
    // Since all nodes share one indentation level variable, this should be
    // declared static.
    static int depth;
    std::vector<OwnPtr> children;
};

using AstNodePtr = AstNode::OwnPtr;

// A little RAII trick to handle indentation when printing a node at a deeper level.
// Instantiating this is pretty much equivalent to doing
// "depth++; defer(depth--);" in Go.
class PrintScope : public AstNode {
public:
    PrintScope() { depth++; }
    ~PrintScope() { depth--; }
};

AstNode::OwnPtr make_ast(NodeType type);
AstNode::OwnPtr make_ast(NodeType type, const Token &tok);

class Expr : public AstNode {
public:
    enum ExprType {
        literal,
        unary,
        binary
    } type;

    Expr(ExprType type) : AstNode(NodeType::expr), type(type) {}
    virtual ~Expr() = default;

    virtual void print();
};
using ExprPtr = std::unique_ptr<Expr>;

class LiteralExpr : public Expr {
public:
    LiteralExpr(const Token &lit) : Expr(ExprType::literal), lit(lit) {}
    void print();

    Token lit;
};

class BinaryExpr : public Expr {
public:
    BinaryExpr(ExprPtr &lhs, Token op, ExprPtr &rhs)
        : Expr(ExprType::binary), lhs(std::move(lhs)), op(op),
          rhs(std::move(rhs)) {}
    void print();

    ExprPtr lhs;
    Token op;
    ExprPtr rhs;
};

} // namespace cp

#endif
