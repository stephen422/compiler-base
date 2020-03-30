#ifndef AST_H
#define AST_H

#include "sema.h"
#include "lexer.h"
#include <iostream>
#include <memory>
#include <unordered_map>

namespace cmp {

enum class AstKind {
    none, // FIXME necessary?
    file,
    toplevel,
    decl_stmt,
    expr_stmt,
    assign_stmt,
    return_stmt,
    if_stmt,
    compound_stmt,
    bad_stmt,
    var_decl,
    struct_decl,
    member_decl,
    func_decl,
    bad_decl,
    ref_expr,
    type_expr,
    unary_expr,
    integer_literal,
    string_literal,
    decl_ref_expr,
    func_call_expr,
    paren_expr,
    address_expr,
    deref_expr,
    plus_expr, // TODO
    minus_expr, // TODO
    binary_expr,
    member_expr,
    bad_expr,
    error_beacon,
};

struct AstNode;
struct File;
struct Toplevel;
struct Stmt;
struct Expr;
struct DeclNode;
struct FuncDeclNode;

std::pair<size_t, size_t> get_ast_range(std::initializer_list<AstNode *> nodes);

// Ast is a struct that contains all necessary information for semantic
// analysis of an AST: namely, the root node and the name table.
struct Ast {
  AstNode *root;
  NameTable &name_table;
};

struct Sema;

struct AstNode {
    AstNode() {}
    AstNode(AstKind kind) : kind(kind) {}
    virtual ~AstNode() = default;

    // AST printing.
    virtual void print() const = 0;
    // AST traversal.
    // TODO: AST is traversed at least twice, i.e. once for semantic analysis
    // and once for IR generation.  So there should be a generic way to
    // traverse it; maybe pass in a lambda that does work for a single node?
    virtual void walk(Sema &sema) {
        (void)sema; // squelch unused warning
    }
    // Name binding pass. Handles variable/function/struct declaration,
    // redefinition/undeclared-use checks, number of function arguments checks,
    // etc.
    virtual bool name_bind_pre(Sema *sema)
    {
        (void)sema;
        return true;
    }
    virtual bool name_bind_post(Sema *sema)
    {
        (void)sema;
        return true;
    }

    // Convenience ostream for AST printing.
    // Handles indentation, tree drawing, etc.
    std::ostream &out() const {
        if (indent > 0) {
            std::cout << std::string(indent - 2, ' ');
            std::cout << "`-";
        }
        return std::cout;
    }

    // RAII trick to handle indentation.
    static int indent;
    struct PrintScope {
        PrintScope() { indent += 2; }
        ~PrintScope() { indent -= 2; }
    };

    AstKind kind = AstKind::none; // node kind
    size_t pos = 0;               // start pos of this AST in the source text
};

// These are free-standing functions that simply do the virtual call into the
// polymorphic compiler pass functions.
inline bool name_bind_pre(Sema *sema, AstNode *node) {
    return node->name_bind_pre(sema);
}
inline bool name_bind_post(Sema *sema, AstNode *node) {
    return node->name_bind_post(sema);
}

// ========
//   File
// ========

// File is simply a group of Toplevels.
struct File : public AstNode {
    File() : AstNode(AstKind::file) {}
    void print() const override;
    void walk(Sema &sema) override;

    std::vector<AstNode *> toplevels;
};

// =============
//   Statement
// =============

struct Stmt : public AstNode {
    Stmt(AstKind kind) : AstNode(kind) {}
};

struct DeclStmt : public Stmt {
    DeclStmt(DeclNode *decl) : Stmt(AstKind::decl_stmt), decl(std::move(decl)) {}
    void print() const override;
    void walk(Sema &sema) override;

    DeclNode *decl;
};

struct ExprStmt : public Stmt {
    ExprStmt(Expr *expr) : Stmt(AstKind::expr_stmt), expr(std::move(expr)) {}
    void print() const override;
    void walk(Sema &sema) override;

    Expr *expr;
};

// Assignment statement, e.g. a[0] = func().
// Non-single-token expressions can come at the RHS as long as they are lvalues,
// but this is not easily determined at the parsing stage.  As such, parse both
// LHS and RHS as generic Exprs, and check the assignability at the semantic
// stage.
struct AssignStmt : public Stmt {
    AssignStmt(Expr *lhs, Expr *rhs) : Stmt(AstKind::assign_stmt), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    void print() const override;
    void walk(Sema &sema) override;

    Expr *lhs;
    Expr *rhs;
};

struct ReturnStmt : public Stmt {
    ReturnStmt(Expr *e) : Stmt(AstKind::return_stmt), expr(e) {}
    void print() const override;
    void walk(Sema &sema) override;

    Expr *expr;
};

struct CompoundStmt : public Stmt {
    CompoundStmt() : Stmt(AstKind::compound_stmt) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_pre(Sema *sema) override;
    bool name_bind_post(Sema *sema) override;

    std::vector<Stmt *> stmts;
};

struct IfStmt : public Stmt {
    IfStmt(Expr *e, CompoundStmt *c) : Stmt(AstKind::if_stmt), cond(e), cstmt(c) {}
    void print() const override;
    // bool name_bind_pre(Sema *sema) override;

    Expr *cond;
    CompoundStmt *cstmt;
};

struct BadStmt : public Stmt {
    BadStmt() : Stmt(AstKind::bad_stmt) {}
    void print() const override;
};


// ==============
//   Expression
// ==============

struct Type;

struct Expr : public AstNode {
    Expr(AstKind kind) : AstNode(kind) {}

    // This value will be filled in the type checking phase.
    Type *type = nullptr;
};

struct UnaryExpr : public Expr {
    Expr *operand;

    UnaryExpr(AstKind k, Expr *oper)
        : Expr(k), operand(std::move(oper)) {}
    void print() const override;
    void walk(Sema &sema) override;
};

struct IntegerLiteral : public UnaryExpr {
    int64_t value;

    IntegerLiteral(int64_t v)
        : UnaryExpr(AstKind::integer_literal, nullptr), value(v) {}
    void print() const override;
    void walk(Sema &sema) override;
};

struct StringLiteral : public UnaryExpr {
    const std::string_view value;

    StringLiteral(std::string_view sv)
        : UnaryExpr(AstKind::string_literal, nullptr), value(sv) {}
    void print() const override;
    void walk(Sema &sema) override;
};

// A unary expression that references a declaration object, e.g. a variable or
// a function.
struct DeclRefExpr : public UnaryExpr {
    Name *name = nullptr;
    Decl *decl = nullptr;

    DeclRefExpr(Name *name)
        : UnaryExpr(AstKind::decl_ref_expr, nullptr), name(name) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_post(Sema *sema) override;
};

struct FuncCallExpr : public UnaryExpr {
    Name *func_name = nullptr;
    FuncDecl *func_decl = nullptr;
    std::vector<Expr *> args;

    FuncCallExpr(Name *name, const std::vector<Expr *> &args)
        : UnaryExpr(AstKind::func_call_expr, nullptr), func_name(name),
          args(args) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_pre(Sema *sema) override;
    bool name_bind_post(Sema *sema) override;
};

struct ParenExpr : public UnaryExpr {
    // Some unary expressions, such as Paren, can have an associated Decl (e.g.
    // (a).m).  We thus have to carry a Decl* here.
    Decl *decl = nullptr;

    ParenExpr(Expr *inside_expr)
        : UnaryExpr(AstKind::paren_expr, inside_expr) {}
    void print() const override;
};

// 'struct.mem'
struct MemberExpr : public Expr {
    Expr *struct_expr = nullptr; // 'struct' part
    Name *member_name = nullptr; // 'mem' part
    Decl *decl = nullptr;        // decl of 'mem'

    MemberExpr(Expr *e, Name *m)
        : Expr(AstKind::member_expr), struct_expr(e), member_name(m) {}
    void print() const override;

    // XXX: disabled, check comment in sema.cc.
    // bool name_bind_post(Sema &sema) override;
};

// XXX: can I call this an expression?
struct TypeExpr : public Expr {
    Name *name = nullptr;    // name of the type
    Type *type = nullptr;    // type declaration
    bool mut = false;        // mutable?
    bool ref = false;        // is this a reference type?
    Expr *subexpr = nullptr; // 'T' part of '&T'.  It is Expr mainly so that
                             // BadExpr can be stored here.

    TypeExpr() : Expr(AstKind::type_expr) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_post(Sema *sema) override;
};

struct BinaryExpr : public Expr {
    Expr *lhs;
    Token op;
    Expr *rhs;

    BinaryExpr(Expr *lhs_, Token op_, Expr *rhs_)
        : Expr(AstKind::binary_expr), lhs(std::move(lhs_)), op(op_),
          rhs(std::move(rhs_)) {
        auto pair = get_ast_range({lhs, rhs});
        pos = pair.first;
    }
    void print() const override;
    void walk(Sema &sema) override;
};

struct BadExpr : public Expr {
    BadExpr() : Expr(AstKind::bad_expr) {}
    void print() const override;
};

// ================
//   Declarations
// ================

struct VarDeclNode;
struct StructDeclNode;
struct FuncDeclNode;

// class Visitor {
// public:
//     virtual void visit(VarDecl &fd) = 0;
//     virtual void visit(StructDecl &fd) = 0;
//     virtual void visit(FuncDecl &fd) = 0;
//     virtual void visit(ErrorDecl &fd) = 0;
// };

// A declaration.
struct DeclNode : public AstNode {
    DeclNode(AstKind kind) : AstNode(kind) {}
};

// Variable declaration.
struct VarDeclNode : public DeclNode {
    // The value of this pointer serves as a unique integer ID to be used for
    // indexing the symbol table.
    Name *name = nullptr;        // name of the variable
    VarDecl *var_decl = nullptr; // decl info
    enum Kind { local, struct_, func } kind = local;
    Expr *type_expr = nullptr;   // type node of the variable
                                 // (inferred later if null)
    Expr *assign_expr = nullptr; // initial assignment value

    VarDeclNode(Name *n, Kind k, Expr *t, Expr *expr)
        : DeclNode(AstKind::var_decl), name(n), kind(k), type_expr(t),
          assign_expr(std::move(expr))
    {
    }
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_post(Sema *sema) override;
};

// Struct declaration.
struct StructDeclNode : public DeclNode {
    StructDeclNode(Name *n, std::vector<DeclNode *> m)
        : DeclNode(AstKind::struct_decl), name(n), members(m) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_pre(Sema *sema) override;
    bool name_bind_post(Sema *sema) override;

    Name *name = nullptr;              // name of the struct
    TypeDecl *struct_decl = nullptr; // decl info
    std::vector<DeclNode *> members;   // member variables
};

// Function declaration.  There is no separate function definition: functions
// should always be defined whenever they are declared.
struct FuncDeclNode : public DeclNode {
    FuncDeclNode(Name *n) : DeclNode(AstKind::func_decl), name(n) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_pre(Sema *sema) override;
    bool name_bind_post(Sema *sema) override;

    Name *name = nullptr;          // name of the function
    FuncDecl *func_decl = nullptr; // decl info
    std::vector<DeclNode *> args;  // list of parameters
    CompoundStmt *body = nullptr;  // body statements
    Expr *ret_type_expr = nullptr; // return type expression
};

struct BadDeclNode : public DeclNode {
    BadDeclNode() : DeclNode(AstKind::bad_decl) {}
    void print() const override;
};

void test(Sema &sema);

} // namespace cmp

#endif
