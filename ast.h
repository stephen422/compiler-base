#ifndef AST_H
#define AST_H

#include "sema.h"
#include "lexer.h"
#include <iostream>
#include <memory>
#include <unordered_map>

namespace cmp {

enum class AstKind {
    file,
    stmt,
    decl,
    expr,
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
    virtual bool name_bind_pre(Sema &sema)
    {
        (void)sema;
        return true;
    }
    virtual bool name_bind_post(Sema &sema)
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

    AstKind kind;   // node kind
    size_t pos = 0; // start pos of this AST in the source text
};

// These are free-standing functions that simply do the virtual call into the
// polymorphic compiler pass functions.
inline bool name_bind_pre(Sema &sema, AstNode *node) {
    return node->name_bind_pre(sema);
}
inline bool name_bind_post(Sema &sema, AstNode *node) {
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

enum class StmtKind {
    decl,
    expr,
    assign,
    return_,
    compound,
    if_,
    bad,
};

struct Stmt : public AstNode {
    Stmt(StmtKind s) : AstNode(AstKind::stmt), stmt_kind(s) {}

    StmtKind stmt_kind;
};

struct DeclStmt : public Stmt {
    DeclStmt(DeclNode *d) : Stmt(StmtKind::decl), decl(d) {}
    void print() const override;
    void walk(Sema &sema) override;

    DeclNode *decl;
};

struct ExprStmt : public Stmt {
    ExprStmt(Expr *e) : Stmt(StmtKind::expr), expr(e) {}
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
    AssignStmt(Expr *l, Expr *r) : Stmt(StmtKind::assign), lhs(l), rhs(r) {}
    void print() const override;
    void walk(Sema &sema) override;

    Expr *lhs;
    Expr *rhs;
};

struct ReturnStmt : public Stmt {
    ReturnStmt(Expr *e) : Stmt(StmtKind::return_), expr(e) {}
    void print() const override;
    void walk(Sema &sema) override;

    Expr *expr;
};

struct CompoundStmt : public Stmt {
    CompoundStmt() : Stmt(StmtKind::compound) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_pre(Sema &sema) override;
    bool name_bind_post(Sema &sema) override;

    std::vector<Stmt *> stmts;
};

struct IfStmt : public Stmt {
    IfStmt(Expr *e, CompoundStmt *ct, IfStmt *ei, CompoundStmt *cf)
        : Stmt(StmtKind::if_), cond(e), cstmt_true(ct), elseif(ei),
          cstmt_false(cf) {}
    void print() const override;

    Expr *cond;               // conditional expr
    CompoundStmt *cstmt_true; // body for true cond
    // Views 'else if' clauses as a separate if statement for the false case.
    // 'elseif' and 'cstmt_false' cannot be non-null at the same time.
    IfStmt *elseif = nullptr;
    CompoundStmt *cstmt_false = nullptr;
};

struct BadStmt : public Stmt {
    BadStmt() : Stmt(StmtKind::bad) {}
    void print() const override;
};


// ==============
//   Expression
// ==============

struct Type;

enum class ExprKind {
    unary,
    binary,
    member,
    type,
    bad,
};

struct Expr : public AstNode {
    Expr(ExprKind e) : AstNode(AstKind::expr), expr_kind(e) {}

    ExprKind expr_kind;
    Type *type = nullptr;
};

enum class UnaryExprKind {
    integer_literal,
    string_literal,
    decl_ref,
    func_call,
    paren,
    address,
    deref,
    plus, // TODO
    minus, // TODO
};

struct UnaryExpr : public Expr {
    UnaryExprKind unary_kind;
    Expr *operand;

    UnaryExpr(UnaryExprKind k, Expr *oper)
        : Expr(ExprKind::unary), unary_kind(k), operand(oper) {}
    void print() const override;
    void walk(Sema &sema) override;
};

struct IntegerLiteral : public UnaryExpr {
    int64_t value;

    IntegerLiteral(int64_t v)
        : UnaryExpr(UnaryExprKind::integer_literal, nullptr), value(v) {}
    void print() const override;
    void walk(Sema &sema) override;
};

struct StringLiteral : public UnaryExpr {
    const std::string_view value;

    StringLiteral(std::string_view sv)
        : UnaryExpr(UnaryExprKind::string_literal, nullptr), value(sv) {}
    void print() const override;
    void walk(Sema &sema) override;
};

// A unary expression that references a declaration object, e.g. a variable or
// a function.
struct DeclRefExpr : public UnaryExpr {
    Name *name = nullptr;
    Decl *decl = nullptr;

    DeclRefExpr(Name *name)
        : UnaryExpr(UnaryExprKind::decl_ref, nullptr), name(name) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_post(Sema &sema) override;
};

struct FuncCallExpr : public UnaryExpr {
    Name *func_name = nullptr;
    FuncDecl *func_decl = nullptr;
    std::vector<Expr *> args;

    FuncCallExpr(Name *name, const std::vector<Expr *> &args)
        : UnaryExpr(UnaryExprKind::func_call, nullptr), func_name(name),
          args(args) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_pre(Sema &sema) override;
    bool name_bind_post(Sema &sema) override;
};

struct ParenExpr : public UnaryExpr {
    // Some unary expressions, such as Paren, can have an associated Decl (e.g.
    // (a).m).  We thus have to carry a Decl* here.
    Decl *decl = nullptr;

    ParenExpr(Expr *inside_expr)
        : UnaryExpr(UnaryExprKind::paren, inside_expr) {}
    void print() const override;
};

struct BinaryExpr : public Expr {
    Expr *lhs;
    Token op;
    Expr *rhs;

    BinaryExpr(Expr *lhs_, Token op_, Expr *rhs_)
        : Expr(ExprKind::binary),
          lhs(std::move(lhs_)), op(op_), rhs(std::move(rhs_)) {
        auto pair = get_ast_range({lhs, rhs});
        pos = pair.first;
    }
    void print() const override;
    void walk(Sema &sema) override;
};

// 'struct.mem'
struct MemberExpr : public Expr {
    Expr *struct_expr = nullptr; // 'struct' part
    Name *member_name = nullptr; // 'mem' part
    Decl *decl = nullptr;        // decl of 'mem'

    MemberExpr(Expr *e, Name *m)
        : Expr(ExprKind::member), struct_expr(e),
          member_name(m) {}
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

    TypeExpr() : Expr(ExprKind::type) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_post(Sema &sema) override;
};

struct BadExpr : public Expr {
    BadExpr() : Expr(ExprKind::bad) {}
    void print() const override;
};

// ================
//   Declarations
// ================

enum class DeclNodeKind {
    var,
    struct_,
    func,
    bad,
};

// A declaration node.
struct DeclNode : public AstNode {
    DeclNodeKind decl_kind;

    DeclNode(DeclNodeKind d) : AstNode(AstKind::decl), decl_kind(d) {}
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
        : DeclNode(DeclNodeKind::var), name(n), kind(k),
          type_expr(t), assign_expr(std::move(expr)) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_post(Sema &sema) override;
};

// Struct declaration.
struct StructDeclNode : public DeclNode {
    StructDeclNode(Name *n, std::vector<DeclNode *> m)
        : DeclNode(DeclNodeKind::struct_), name(n),
          members(m) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_pre(Sema &sema) override;
    bool name_bind_post(Sema &sema) override;

    Name *name = nullptr;              // name of the struct
    TypeDecl *struct_decl = nullptr; // decl info
    std::vector<DeclNode *> members;   // member variables
};

// Function declaration.  There is no separate function definition: functions
// should always be defined whenever they are declared.
struct FuncDeclNode : public DeclNode {
    FuncDeclNode(Name *n)
        : DeclNode(DeclNodeKind::func), name(n) {}
    void print() const override;
    void walk(Sema &sema) override;
    bool name_bind_pre(Sema &sema) override;
    bool name_bind_post(Sema &sema) override;

    Name *name = nullptr;          // name of the function
    FuncDecl *func_decl = nullptr; // decl info
    std::vector<DeclNode *> args;  // list of parameters
    CompoundStmt *body = nullptr;  // body statements
    Expr *ret_type_expr = nullptr; // return type expression
};

struct BadDeclNode : public DeclNode {
    BadDeclNode() : DeclNode(DeclNodeKind::bad) {}
    void print() const override;
};

// AST visitor

struct AstVisitor {
    void visit_file(const File *f);
    void visit_toplevel(const AstNode *a);

    void visit_stmt(const Stmt *s);
    void visit_decl_stmt(const DeclStmt *ds);
    void visit_expr_stmt(const ExprStmt *es);
    void visit_assign_stmt(const AssignStmt *as);
    void visit_return_stmt(const ReturnStmt *rs);
    void visit_compound_stmt(const CompoundStmt *cs);
    void visit_if_stmt(const IfStmt *is);
    void visit_bad_stmt(const BadStmt *bs);

    void visit_expr(const Expr *e);
    void visit_decl(const DeclNode *d);
    void visit_struct_decl(const StructDeclNode *s);
    void visit_func_decl(const FuncDeclNode *f);
    void visit_var_decl(const VarDeclNode *vd);
};

void walk_file(AstVisitor &v, const File *f);
void walk_decl_stmt(AstVisitor &v, const DeclStmt *ds);
void walk_expr_stmt(AstVisitor &v, const ExprStmt *es);
void walk_assign_stmt(AstVisitor &v, const AssignStmt *as);
void walk_return_stmt(AstVisitor &v, const ReturnStmt *rs);
void walk_compound_stmt(AstVisitor &v, const CompoundStmt *cs);
void walk_if_stmt(AstVisitor &v, const IfStmt *is);
void walk_var_decl(AstVisitor &v, const VarDeclNode *var);
void walk_struct_decl(AstVisitor &v, const StructDeclNode *s);
void walk_func_decl(AstVisitor &v, const FuncDeclNode *f);

} // namespace cmp

#endif
