// -*- C++ -*-
#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <iostream>
#include <memory>

namespace cmp {

enum class AstNodeType {
    none, // FIXME necessary?
    file,
    toplevel,
    stmt,
    decl,
    expr,
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
    AstNode(): AstNode(AstNodeType::none) {}
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
    File() : AstNode(AstNodeType::file) {}
    void print() const override;

    std::vector<ToplevelPtr> toplevels;
};

// ============
//   Toplevel
// ============

enum class ToplevelType {
    function,
};

class Toplevel : public AstNode {
public:
    Toplevel(ToplevelType type) : AstNode(AstNodeType::toplevel), type(type) {}

    ToplevelType type;
};


// =============
//   Statement
// =============

enum class StmtType {
    decl,
    expr,
    return_,
    compound
};

class Stmt : public AstNode {
public:
    Stmt(StmtType type) : AstNode(AstNodeType::stmt), type(type) {}

    StmtType type;
};

class DeclStmt : public Stmt {
public:
    DeclStmt(DeclPtr decl) : Stmt(StmtType::decl), decl(std::move(decl)) {}
    void print() const override;

    DeclPtr decl;
};

class ExprStmt : public Stmt {
public:
    ExprStmt(ExprPtr expr) : Stmt(StmtType::expr), expr(std::move(expr)) {}
    void print() const override;

    ExprPtr expr;
};

class ReturnStmt : public Stmt {
public:
    ReturnStmt(ExprPtr expr) : Stmt(StmtType::return_), expr(std::move(expr)) {}
    void print() const override;

    ExprPtr expr;
};

class CompoundStmt : public Stmt {
public:
    CompoundStmt() : Stmt(StmtType::compound) {}
    void print() const override;

    std::vector<StmtPtr> stmts;
};

// ===============
//   Expressions
// ===============

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

class LiteralExpr : public Expr {
public:
    LiteralExpr(const Token &lit) : Expr(ExprType::literal), lit(lit) {}
    void print() const override;
    std::string flatten() const override;

    Token lit;
};

class BinaryExpr : public Expr {
public:
    BinaryExpr(ExprPtr lhs, Token op, ExprPtr rhs)
        : Expr(ExprType::binary), lhs(std::move(lhs)), op(op),
          rhs(std::move(rhs)) {}
    void print() const override;
    std::string flatten() const override;

    ExprPtr lhs;
    Token op;
    ExprPtr rhs;
};

// ================
//   Declarations
// ================

enum class DeclType {
    var,
    func,
};

// A declaration.
class Decl : public AstNode {
public:
    Decl(DeclType type) : AstNode(AstNodeType::decl), type(type) {}

    DeclType type;
};

// Variable declaration.
class VarDecl : public Decl {
public:
    VarDecl(const Token &id, ExprPtr expr, bool mut)
        : Decl(DeclType::var), id(id), assign_expr(std::move(expr)), mut(mut) {}
    void print() const override;

    Token id;
    ExprPtr assign_expr;
    // Is this a "var" declaration?
    bool mut;
};

// Function definition.  There is no separate 'function declaration': functions
// should always be defined whenever they are declared.
class Function : public Toplevel {
public:
    Function(const Token &id) : Toplevel(ToplevelType::function), id(id) {}
    void print() const override;

    Token id;
    // Compound statement body
    NodePtr<CompoundStmt> body;
    Token return_type;
};

} // namespace cmp

#endif
