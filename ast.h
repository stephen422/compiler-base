// -*- C++ -*-
#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <iostream>
#include <memory>

namespace cmp {

enum class AstType {
    none, // FIXME necessary?
    file,
    toplevel,
    decl_stmt,
    expr_stmt,
    assign_stmt,
    return_stmt,
    compound_stmt,
    var_decl,
    literal_expr,
    binary_expr,
    function,
};

class AstNode;
class File;
class Toplevel;
class Stmt;
class Expr;
class Decl;
class Function;

// Owning pointers to each AST node.
using AstNodePtr = std::unique_ptr<AstNode>;
using FilePtr = std::unique_ptr<File>;
using ToplevelPtr = std::unique_ptr<Toplevel>;
using StmtPtr = std::unique_ptr<Stmt>;
using ExprPtr = std::unique_ptr<Expr>;
using DeclPtr = std::unique_ptr<Decl>;
using FunctionPtr = std::unique_ptr<Function>;

template <typename T>
using NodePtr = std::unique_ptr<T>;

template<typename T, typename... Args>
NodePtr<T> make_node(Args&&... args) {
    return std::make_unique<T>(std::forward<Args>(args)...);
}

class AstNode {
public:
    AstNode(): AstNode(AstType::none) {}
    AstNode(AstType type): type(type) {}
    virtual ~AstNode() = default;

    virtual void print() const = 0;

    // AST traversal.
    virtual void traverse() const = 0;

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

    AstType type;

    // Indentation of the current node when dumping AST.
    // Since all nodes share one indentation level variable, this should be
    // declared static.
    static int depth;

    // RAII trick to handle indentation.
    class PrintScope {
    public:
        PrintScope() { depth += 2; }
        ~PrintScope() { depth -= 2; }
    };
};

// ========
//   File
// ========

// File is simply a group of Toplevels.
class File : public AstNode {
public:
    File() : AstNode(AstType::file) {}
    void print() const override;
    void traverse() const override;

    std::vector<ToplevelPtr> toplevels;
};

// =============
//   Statement
// =============

class Stmt : public AstNode {
public:
    Stmt(AstType type) : AstNode(type) {}
};

class DeclStmt : public Stmt {
public:
    DeclStmt(DeclPtr decl) : Stmt(AstType::decl_stmt), decl(std::move(decl)) {}
    void print() const override;
    void traverse() const override;

    DeclPtr decl;
};

class ExprStmt : public Stmt {
public:
    ExprStmt(ExprPtr expr) : Stmt(AstType::expr_stmt), expr(std::move(expr)) {}
    void print() const override;
    void traverse() const override;

    ExprPtr expr;
};

class AssignStmt : public Stmt {
public:
    AssignStmt(const Token &lhs, ExprPtr rhs) : Stmt(AstType::assign_stmt), lhs(lhs), rhs(std::move(rhs)) {}
    void print() const override;
    void traverse() const override;

    // TODO LHS could be multiple tokens.
    Token lhs;
    ExprPtr rhs;
};

class ReturnStmt : public Stmt {
public:
    ReturnStmt(ExprPtr expr) : Stmt(AstType::return_stmt), expr(std::move(expr)) {}
    void print() const override;
    void traverse() const override;

    ExprPtr expr;
};

class CompoundStmt : public Stmt {
public:
    CompoundStmt() : Stmt(AstType::compound_stmt) {}
    void print() const override;
    void traverse() const override;

    std::vector<StmtPtr> stmts;
};

// ===============
//   Expressions
// ===============

class Expr : public AstNode {
public:
    Expr(AstType type) : AstNode(type) {}
    virtual std::string flatten() const = 0;
};

class LiteralExpr : public Expr {
public:
    LiteralExpr(const Token &lit) : Expr(AstType::literal_expr), lit(lit) {}
    void print() const override;
    void traverse() const override;
    std::string flatten() const override;

    Token lit;
};

class BinaryExpr : public Expr {
public:
    BinaryExpr(ExprPtr lhs, Token op, ExprPtr rhs)
        : Expr(AstType::binary_expr), lhs(std::move(lhs)), op(op),
          rhs(std::move(rhs)) {}
    void print() const override;
    void traverse() const override;
    std::string flatten() const override;

    ExprPtr lhs;
    Token op;
    ExprPtr rhs;
};

// ================
//   Declarations
// ================

// A declaration.
class Decl : public AstNode {
public:
    Decl(AstType type) : AstNode(type) {}
};

// Variable declaration.
class VarDecl : public Decl {
public:
    VarDecl(const Token &id, ExprPtr expr, bool mut)
        : Decl(AstType::var_decl), id(id), assign_expr(std::move(expr)), mut(mut) {}
    void print() const override;
    void traverse() const override;

    Token id;
    ExprPtr assign_expr;
    // Is this a "var" declaration?
    bool mut;
};

// ============
//   Toplevel
// ============

class Toplevel : public AstNode {
public:
    Toplevel(AstType type) : AstNode(type) {}
};

// Function definition.  There is no separate 'function declaration': functions
// should always be defined whenever they are declared.
class Function : public Toplevel {
public:
    Function(const Token &id) : Toplevel(AstType::function), id(id) {}
    void print() const override;
    void traverse() const override {
        body->traverse();
    }

    Token id;
    // Compound statement body
    NodePtr<CompoundStmt> body;
    Token return_type;
};

} // namespace cmp

#endif
