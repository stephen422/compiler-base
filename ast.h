// -*- C++ -*-
#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <iostream>
#include <memory>

namespace comp {

enum class AstNodeType {
    stmt,
    decl,
    expr,
};

class AstNode {
public:
    // Owning pointer to an AST node.
    using OwnPtr = std::unique_ptr<AstNode>;

    // TODO stmt by default
    AstNode(): AstNode(AstNodeType::stmt) {}
    AstNode(AstNodeType type): type(type) {}
    virtual ~AstNode() = default;

    virtual void print() const {}
    template <typename T> constexpr T *as() {
        return static_cast<T *>(this);
    }

    // Convenience ostream for AST dumping code.
    // Handles indentation, tree drawing, etc.
    std::ostream &out() const {
        if (depth > 0) {
            std::cout << std::string(depth - 2, ' ');
            std::cout << "`-";
        }
        return std::cout;
    }

    AstNodeType type;

    // Indentation of the current node when dumping AST.
    // Since all nodes share one indentation level variable, this should be
    // declared static.
    static int depth;

    // RAII trick to handle indentation when printing a node with deeper
    // indentation.
    class PrintScope {
    public:
        PrintScope() { depth += 2; }
        ~PrintScope() { depth -= 2; }
    };
};

using AstNodePtr = AstNode::OwnPtr;

// ===-------===
//   Statement
// ===-------===

enum class StmtType {
};

class Stmt : public AstNode {
public:
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
    Expr(ExprType type) : AstNode(AstNodeType::expr), type(type) {}
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

// ===----------===
//   Declarations
// ===----------===

enum class DeclType {
    variable,
    function,
};

// A declaration.
class Decl : public AstNode {
public:
    Decl(DeclType type) : AstNode(AstNodeType::decl), type(type) {}

    DeclType type;
};
using DeclPtr = std::unique_ptr<Decl>;

// Variable declaration.
class VarDecl : public Decl {
public:
    VarDecl(const Token &id, ExprPtr &rhs, bool mut)
        : Decl(DeclType::variable), id(id), rhs(std::move(rhs)), mut(mut) {}
    void print() const override;

    Token id;
    ExprPtr rhs;
    // Is this a "var" declaration?
    bool mut;
};

// Function declaration.
class FuncDecl : public Decl {
public:
    FuncDecl(const Token &name) : Decl(DeclType::function), name(name) {}
    Token name;
};

} // namespace comp

#endif
