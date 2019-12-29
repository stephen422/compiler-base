#ifndef AST_H
#define AST_H

#include "sema.h"
#include "lexer.h"
#include <iostream>
#include <memory>
#include <unordered_map>

namespace cmp {

// Note about header dependency: AST depends on Sema because it is traversed
// multiple times in the course of compilation, and therefore it has to be able
// to retain semantic informations for the next traverse, as class members of
// AstNodes.

enum class AstKind {
    none, // FIXME necessary?
    file,
    toplevel,
    decl_stmt,
    expr_stmt,
    assign_stmt,
    return_stmt,
    compound_stmt,
    bad_stmt,
    var_decl,
    param_decl,
    struct_decl,
    func_decl,
    bad_decl,
    literal_expr,
    integer_literal,
    ref_expr,
    type_expr,
    unary_expr,
    binary_expr,
    bad_expr,
    error_beacon,
};

class AstNode;
class File;
class Toplevel;
class Stmt;
class Expr;
class Decl;
class FuncDecl;

std::pair<size_t, size_t> get_ast_range(std::initializer_list<AstNode *> nodes);

// Ast is aggregate type that contains all information necessary for semantic
// analysis of an AST: namely, the root node and the name table.
class Ast {
public:
    Ast(AstNode *r, NameTable &nt) : root(r), name_table(nt) {}
    AstNode *root;
    NameTable &name_table;
};

class AstNode {
public:
    AstNode() {}
    AstNode(AstKind kind) : kind(kind) {}
    virtual ~AstNode() = default;

    // AST printing.
    virtual void print() const = 0;
    // AST traversal.
    // TODO: AST is traversed at least twice, i.e. once for semantic analysis
    // and once for IR generation.  So there should be a generic way to
    // traverse it; maybe pass in a lambda that does work for a single node?
    virtual void traverse(Semantics &sema) {
        (void)sema; // squelch unused warning
    }

    // Convenience ostream for AST printing.
    // Handles indentation, tree drawing, etc.
    std::ostream &out() const {
        if (depth > 0) {
            std::cout << std::string(depth - 2, ' ');
            std::cout << "`-";
        }
        return std::cout;
    }

    // RAII trick to handle indentation.
    class PrintScope {
    public:
        PrintScope() { depth += 2; }
        ~PrintScope() { depth -= 2; }
    };

    AstKind kind = AstKind::none; // node kind
    size_t start_pos = 0;          // start pos of this AST in the source text
    size_t end_pos = 0;            // end pos of this AST in the source text
    // Indentation of the current node when dumping AST.
    // static because all nodes share this.
    static int depth;
};

// ========
//   File
// ========

// File is simply a group of Toplevels.
class File : public AstNode {
public:
    File() : AstNode(AstKind::file) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    std::vector<AstNode *> toplevels;
};

// =============
//   Statement
// =============

class Stmt : public AstNode {
public:
    Stmt(AstKind kind) : AstNode(kind) {}
};

class DeclStmt : public Stmt {
public:
    DeclStmt(Decl *decl) : Stmt(AstKind::decl_stmt), decl(std::move(decl)) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    Decl *decl;
};

class ExprStmt : public Stmt {
public:
    ExprStmt(Expr *expr) : Stmt(AstKind::expr_stmt), expr(std::move(expr)) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    Expr *expr;
};

// Assignment statement, e.g. a[0] = func().
// Non-single-token expressions can come at the RHS as long as they are lvalues,
// but this is not easily determined at the parsing stage.  As such, parse both
// LHS and RHS as generic Exprs, and check the assignability at the semantic
// stage.
class AssignStmt : public Stmt {
public:
    AssignStmt(Expr *lhs, Expr *rhs) : Stmt(AstKind::assign_stmt), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    Expr *lhs;
    Expr *rhs;
};

class ReturnStmt : public Stmt {
public:
    ReturnStmt(Expr *expr) : Stmt(AstKind::return_stmt), expr(std::move(expr)) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    Expr *expr;
};

class CompoundStmt : public Stmt {
public:
    CompoundStmt() : Stmt(AstKind::compound_stmt) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    std::vector<Stmt *> stmts;
};

class BadStmt : public Stmt {
public:
    BadStmt() : Stmt(AstKind::bad_stmt) {}
    void print() const override;
};


// ==============
//   Expression
// ==============

class Type;

class Expr : public AstNode {
public:
    Expr(AstKind kind) : AstNode(kind) {}

    // This value will be propagated by post-order tree traversal, starting
    // from DeclRefExpr or literal expressions.
    Type *type = nullptr;
};

class UnaryExpr : public Expr {
public:
    enum UnaryKind {
        DeclRef,
        Literal,
        Paren,
        Address,
        Deref,
        // TODO:
        Plus,
        Minus,
    };

    UnaryExpr(UnaryKind k, Expr *oper)
        : Expr(AstKind::unary_expr), unary_kind(k), operand(std::move(oper)) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    UnaryKind unary_kind;
    Expr *operand;
};

class IntegerLiteral : public UnaryExpr {
public:
    IntegerLiteral(int64_t v) : UnaryExpr(Literal, nullptr), value(v) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    int64_t value;
};

class DeclRefExpr : public UnaryExpr {
public:
    DeclRefExpr() : UnaryExpr(DeclRef, nullptr) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    // The integer value of this pointer serves as a unique ID to be used for
    // indexing the symbol table.
    Name *name = nullptr;
};

// XXX: can I call this an expression?
class TypeExpr : public Expr {
public:
    TypeExpr() : Expr(AstKind::type_expr) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    Name *name = nullptr;          // name of the type
    bool mut = false;              // mutable?
    bool ref = false;              // is this a reference type?
    Expr *subexpr = nullptr;       // 'T' part of '&T'
    // This is Expr mainly so that BadExpr can be stored here.
};

class BinaryExpr : public Expr {
public:
    BinaryExpr(Expr *lhs_, Token op_, Expr *rhs_)
        : Expr(AstKind::binary_expr), lhs(std::move(lhs_)), op(op_),
          rhs(std::move(rhs_)) {
        auto pair = get_ast_range({lhs, rhs});
        start_pos = pair.first;
        end_pos = pair.second;
    }
    void print() const override;
    void traverse(Semantics &sema) override;

    Expr *lhs;
    Token op;
    Expr *rhs;
};

class BadExpr : public Expr {
public:
    BadExpr() : Expr(AstKind::bad_expr) {}
    void print() const override;
};

// ================
//   Declarations
// ================

class VarDecl;
class StructDecl;
class FuncDecl;
class ErrorDecl;

// class Visitor {
// public:
//     virtual void visit(VarDecl &fd) = 0;
//     virtual void visit(StructDecl &fd) = 0;
//     virtual void visit(FuncDecl &fd) = 0;
//     virtual void visit(ErrorDecl &fd) = 0;
// };

// A declaration.
class Decl : public AstNode {
public:
    Decl(AstKind kind) : AstNode(kind) {}
};

// Variable declaration.
class VarDecl : public Decl {
public:
    VarDecl(Name *n, Expr *t, Expr *expr)
        : Decl(AstKind::var_decl), name(n), typeExpr(std::move(t)),
          assignExpr(std::move(expr)) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    // The value of this pointer serves as a unique integer ID to be used for
    // indexing the symbol table.
    Name *name = nullptr;       // name of the variable
    Expr *typeExpr = nullptr;   // type node of the variable.
                                // If null, it will be inferred later
    Expr *assignExpr = nullptr; // initial assignment value
};

// Struct declaration.
class StructDecl : public Decl {
public:
    StructDecl(Name *n, std::vector<Decl *> m)
        : Decl(AstKind::struct_decl), name(n), members(std::move(m)) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    Name *name = nullptr;            // name of the struct
    std::vector<Decl *> members; // member variables
};

// Function declaration.  There is no separate function definition: functions
// should always be defined whenever they are declared.
class FuncDecl : public Decl {
public:
    FuncDecl(Name *n) : Decl(AstKind::func_decl), name(n) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    Name *name = nullptr;          // name of the function
    std::vector<Decl *> params;    // list of parameters
    CompoundStmt *body = nullptr;  // body statements
    Expr *retTypeExpr = nullptr;   // return type expression
};

class BadDecl : public Decl {
public:
    BadDecl() : Decl(AstKind::bad_decl) {}
    void print() const override;
};

void test(Semantics &sema);

} // namespace cmp

#endif
