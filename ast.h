// -*- C++ -*-
#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <iostream>
#include <memory>

namespace comp {

enum class NodeType {
    stmt,
    decl,
    expr,
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
    virtual void print() const {}

    // Convenience ostream for AST dumping code.
    // Handles indentation, tree drawing, etc.
    std::ostream &out() const {
        if (depth > 0) {
            std::cout << std::string(depth - 2, ' ');
            std::cout << "`-";
        }
        return std::cout;
    }

    NodeType type;
    Token tok;

    // Indentation of the current node when dumping AST.
    // Since all nodes share one indentation level variable, this should be
    // declared static.
    static int depth;

    std::vector<OwnPtr> children; // FIXME
};

using AstNodePtr = AstNode::OwnPtr;
AstNodePtr make_ast(NodeType type);
AstNodePtr make_ast(NodeType type, const Token &tok);

// A little RAII trick to handle indentation when printing a node at a deeper
// level.  This is equivalent to doing "depth++; defer(depth--);" in Go.
class PrintScope : public AstNode {
public:
    PrintScope() { depth += 2; }
    ~PrintScope() { depth -= 2; }
};

// ===-------===
//   Statement
// ===-------===

enum class StmtType {
};

class Stmt : public AstNode {
public:
};

// ===----------===
//   Declarations
// ===----------===

enum class DeclType {
    variable,
};

class Decl : public AstNode {
public:
    Decl(DeclType type) : AstNode(NodeType::decl), type(type) {}

    DeclType type;
};
using DeclPtr = std::unique_ptr<Decl>;

class VarDecl : public Decl {
public:
    VarDecl(const Token &id, bool mut)
        : Decl(DeclType::variable), id(id), mut(mut) {}
    void print() const override;

    Token id;
    // Is this a "var" declaration?
    bool mut;
};

// ===---------===
//   Expressions
// ===---------===

enum class ExprType {
    literal,
    unary,
    binary,
};

class Expr : public AstNode {
public:
    Expr(ExprType type) : AstNode(NodeType::expr), type(type) {}
    virtual std::string flatten() const = 0;

    ExprType type;
};
using ExprPtr = std::unique_ptr<Expr>;

class LiteralExpr : public Expr {
public:
    LiteralExpr(const Token &lit) : Expr(ExprType::literal), lit(lit) {}
    void print() const override;
    std::string flatten() const override;

    Token lit;
};

class BinaryExpr : public Expr {
public:
    BinaryExpr(ExprPtr &lhs, Token op, ExprPtr &rhs)
        : Expr(ExprType::binary), lhs(std::move(lhs)), op(op),
          rhs(std::move(rhs)) {}
    void print() const override;
    std::string flatten() const override;

    ExprPtr lhs;
    Token op;
    ExprPtr rhs;
};

} // namespace comp

#endif
