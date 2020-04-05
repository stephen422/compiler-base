#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <cassert>
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

// Forward-declare stuff so that we don't have to include sema.h.
struct AstNode;
struct File;
struct Toplevel;
struct Stmt;
struct Expr;
struct DeclNode;
struct FuncDeclNode;
struct Type;
struct Decl;
struct FuncDecl;
struct VarDecl;
struct StructDecl;

// 'Name' corresponds to a single unique identifier string in the source text.
// There may be multiple occurrences of a string in the source text, but only
// one instance of the matching Name can reside in the name table.
struct Name {
    std::string text;

    Name(const std::string &s) : text(s) {}
    std::string str() const { return text; }
};

// 'NameTable' is a hash table of Names queried by their string value.  It
// serves to reduce the number of string hashing operation, since we can look
// up the symbol table using Name instead of raw char * throughout the semantic
// analysis.
struct NameTable {
    Name *get_or_add(const std::string &s) {
        Name *found = get(s);
        if (found) {
            return found;
        }
        auto pair = map.insert({s, {s}});
        return &pair.first->second;
    }
    Name *get(const std::string &s) {
        auto found = map.find(s);
        if (found == map.end()) {
            return nullptr;
        } else {
            return &found->second;
        }
    }
    std::unordered_map<std::string, Name> map;
};

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
};

// XXX: can I call this an expression?
enum class TypeExprKind {
    value,
    ref,
    array,
};
struct TypeExpr : public Expr {
    TypeExprKind kind;
    // Name of the type. TODO: should this contain '&' and '[]'?
    Name *name = nullptr;
    // Decl object that represents this type.  Null if the type is not
    // canonical, e.g. reference or an array.
    Decl *decl = nullptr;
    // Type object.
    Type *type = nullptr; // type declaration
    // Is this type mutable?
    bool mut = false;
    // 'T' part of '&T'.  It is Expr rather than TypeExpr mainly so that it can
    // store BadExpr.  XXX dirty.
    Expr *subexpr = nullptr;

    // TODO: incomplete.
    TypeExpr(TypeExprKind k, Name *n, Expr *se)
        : Expr(ExprKind::type), kind(k), name(n), subexpr(se) {}
    void print() const override;
    void walk(Sema &sema) override;
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
    VarDecl *var_decl = nullptr; // decl of the variable
    enum Kind { local, struct_, func } kind = local;
    // TypeExpr of the variable.  Declared as Expr to accommodate for BadExpr.
    Expr *type_expr = nullptr;
    Expr *assign_expr = nullptr; // initial assignment value

    VarDeclNode(Name *n, Kind k, Expr *t, Expr *expr)
        : DeclNode(DeclNodeKind::var), name(n), kind(k), type_expr(t),
          assign_expr(std::move(expr)) {}
    void print() const override;
    void walk(Sema &sema) override;
};

// Struct declaration.
struct StructDeclNode : public DeclNode {
    Name *name = nullptr;              // name of the struct
    StructDecl *struct_decl = nullptr; // decl info
    std::vector<DeclNode *> members;   // member variables

    StructDeclNode(Name *n, std::vector<DeclNode *> m)
        : DeclNode(DeclNodeKind::struct_), name(n),
          members(m) {}
    void print() const override;
    void walk(Sema &sema) override;
};

// Function declaration.  There is no separate function definition: functions
// should always be defined whenever they are declared.
struct FuncDeclNode : public DeclNode {
    FuncDeclNode(Name *n)
        : DeclNode(DeclNodeKind::func), name(n) {}
    void print() const override;
    void walk(Sema &sema) override;

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

// AST visitor.
//
// Custom visitors, e.g. name binders or type checkers, are expected to inherit
// from this class and override the node visitor functions as necessary.
//
// Curiously-recurring template pattern (CRTP) is used to enable calling the
// visitor function of the derived class from the default visitor defined in
// this class.  See commit log b7e6113.
template <typename Derived>
class AstVisitor {
    // 'Derived this'. By calling visitors and walkers through the return
    // pointer of this function, the base class can access the derived visitor
    // implementations.
    constexpr Derived *dis() { return static_cast<Derived *>(this); }

public:
    void visit_file(File *f);
    void visit_toplevel(AstNode *a);

    void visit_stmt(Stmt *s);
    void visit_decl_stmt(DeclStmt *ds);
    void visit_expr_stmt(ExprStmt *es);
    void visit_assign_stmt(AssignStmt *as);
    void visit_return_stmt(ReturnStmt *rs);
    void visit_compound_stmt(CompoundStmt *cs);
    void visit_if_stmt(IfStmt *is);

    void visit_expr(Expr *e);
    void visit_unary_expr(UnaryExpr *u);
    void visit_integer_literal(IntegerLiteral *i);
    void visit_string_literal(StringLiteral *s);
    void visit_decl_ref_expr(DeclRefExpr *d);
    void visit_func_call_expr(FuncCallExpr *f);
    void visit_binary_expr(BinaryExpr *b);
    void visit_member_expr(MemberExpr *m);
    void visit_type_expr(TypeExpr *t);

    void visit_decl(DeclNode *d);
    void visit_var_decl(VarDeclNode *v);
    void visit_struct_decl(StructDeclNode *s);
    void visit_func_decl(FuncDeclNode *f);
};

//
// AST visitor functions.
//
// These are to be overridden by the specialized visitor classes. They provide
// a default behavior of simply traversing the node, acting nothing upon them.
//

template <typename Derived>
void AstVisitor<Derived>::visit_file(File *f) {
    walk_file(*dis(), f);
}
template <typename Derived>
void AstVisitor<Derived>::visit_toplevel(AstNode *a) {
    switch (a->kind) {
    case AstKind::stmt:
        dis()->visit_stmt(static_cast<Stmt *>(a));
        break;
    case AstKind::decl:
        dis()->visit_decl(static_cast<DeclNode *>(a));
        break;
    default:
        fmt::print("AstKind: {}\n", a->kind);
        assert(false && "not a toplevel node");
    }
}
template <typename Derived>
void AstVisitor<Derived>::visit_stmt(Stmt *s) {
    switch (s->stmt_kind) {
    case StmtKind::decl:
        dis()->visit_decl_stmt(static_cast<DeclStmt *>(s));
        break;
    case StmtKind::expr:
        dis()->visit_expr_stmt(static_cast<ExprStmt *>(s));
        break;
    case StmtKind::assign:
        dis()->visit_assign_stmt(static_cast<AssignStmt *>(s));
        break;
    case StmtKind::return_:
        dis()->visit_return_stmt(static_cast<ReturnStmt *>(s));
        break;
    case StmtKind::compound:
        dis()->visit_compound_stmt(static_cast<CompoundStmt *>(s));
        break;
    case StmtKind::if_:
        dis()->visit_if_stmt(static_cast<IfStmt *>(s));
        break;
    case StmtKind::bad:
        // do nothing
        break;
    default:
        assert(false);
    }
}
template <typename Derived>
void AstVisitor<Derived>::visit_decl_stmt(DeclStmt *ds) {
    walk_decl_stmt(*dis(), ds);
}
template <typename Derived>
void AstVisitor<Derived>::visit_expr_stmt(ExprStmt *es) {
    walk_expr_stmt(*dis(), es);
}
template <typename Derived>
void AstVisitor<Derived>::visit_assign_stmt(AssignStmt *as) {
    walk_assign_stmt(*dis(), as);
}
template <typename Derived>
void AstVisitor<Derived>::visit_return_stmt(ReturnStmt *rs) {
    walk_return_stmt(*dis(), rs);
}
template <typename Derived>
void AstVisitor<Derived>::visit_compound_stmt(CompoundStmt *cs) {
    walk_compound_stmt(*dis(), cs);
}
template <typename Derived>
void AstVisitor<Derived>::visit_if_stmt(IfStmt *is) {
    walk_if_stmt(*dis(), is);
}
template <typename Derived>
void AstVisitor<Derived>::visit_decl(DeclNode *d) {
    switch (d->decl_kind) {
    case DeclNodeKind::var:
        dis()->visit_var_decl(static_cast<VarDeclNode *>(d));
        break;
    case DeclNodeKind::struct_:
        dis()->visit_struct_decl(static_cast<StructDeclNode *>(d));
        break;
    case DeclNodeKind::func:
        dis()->visit_func_decl(static_cast<FuncDeclNode *>(d));
        break;
    case DeclNodeKind::bad:
        // do nothing
        break;
    default:
        assert(false);
    }
}
template <typename Derived>
void AstVisitor<Derived>::visit_var_decl(VarDeclNode *v) {
    walk_var_decl(*dis(), v);
}
template <typename Derived>
void AstVisitor<Derived>::visit_struct_decl(StructDeclNode *s) {
    walk_struct_decl(*dis(), s);
}
template <typename Derived>
void AstVisitor<Derived>::visit_func_decl(FuncDeclNode *f) {
    walk_func_decl(*dis(), f);
}
template <typename Derived>
void AstVisitor<Derived>::visit_expr(Expr *e) {
    // Rather than calling walk_expr() here, we do a switch-case, because the
    // visiting logic in a specialized visitor is likely to be different for
    // each type of Expr and thus be implemented using a switch-case anyway.
    switch (e->expr_kind) {
    case ExprKind::unary:
        dis()->visit_unary_expr(static_cast<UnaryExpr *>(e));
        break;
    case ExprKind::binary:
        dis()->visit_binary_expr(static_cast<BinaryExpr *>(e));
        break;
    case ExprKind::member:
        dis()->visit_member_expr(static_cast<MemberExpr *>(e));
        break;
    case ExprKind::type:
        dis()->visit_type_expr(static_cast<TypeExpr *>(e));
        break;
    case ExprKind::bad:
        // do nothing
        break;
    default:
        assert(false);
        break;
    }
}
template <typename Derived>
void AstVisitor<Derived>::visit_unary_expr(UnaryExpr *u) {
    switch (u->unary_kind) {
    case UnaryExprKind::integer_literal:
        dis()->visit_integer_literal(static_cast<IntegerLiteral *>(u));
        break;
    case UnaryExprKind::string_literal:
        dis()->visit_string_literal(static_cast<StringLiteral *>(u));
        // do nothing
        break;
    case UnaryExprKind::decl_ref:
        dis()->visit_decl_ref_expr(static_cast<DeclRefExpr *>(u));
        break;
    case UnaryExprKind::func_call:
        dis()->visit_func_call_expr(static_cast<FuncCallExpr *>(u));
        break;
    case UnaryExprKind::paren:
        // do nothing
        break;
    case UnaryExprKind::address:
        // do nothing
        break;
    case UnaryExprKind::deref:
        // do nothing
        break;
    default:
        assert(false);
        break;
    }
}
template <typename Derived>
void AstVisitor<Derived>::visit_integer_literal(IntegerLiteral *i) {
    // nothing to walk
}
template <typename Derived>
void AstVisitor<Derived>::visit_string_literal(StringLiteral *s) {
    // nothing to walk
}
template <typename Derived>
void AstVisitor<Derived>::visit_decl_ref_expr(DeclRefExpr *d) {
    // nothing to walk
}
template <typename Derived>
void AstVisitor<Derived>::visit_func_call_expr(FuncCallExpr *f) {
    walk_func_call_expr(*dis(), f);
}
template <typename Derived>
void AstVisitor<Derived>::visit_binary_expr(BinaryExpr *b) {
    walk_binary_expr(*dis(), b);
}
template <typename Derived>
void AstVisitor<Derived>::visit_member_expr(MemberExpr *m) {
    walk_member_expr(*dis(), m);
}
template <typename Derived>
void AstVisitor<Derived>::visit_type_expr(TypeExpr *t) {
    walk_type_expr(*dis(), t);
}

//
// AST walker functions.
//
// These functions exist to separate the traversal logic from the actual work
// done on each node in the 'visit_...' functions.  This way, the visitor
// functions only have to worry about whether to do work before or after
// walking the subnodes, by simply positioning the walker function in the right
// place.
//
// Polymorphism is required because they should be able to call different
// derived visitor methods.
//
// Note: The functions here combined can be seen as what the 'accept()'
// function do in the visitor pattern. However, whereas the accept() function
// is declared as virtual in the pattern proper, these are not polymorphic.
// TODO: Document why.
//

template <typename Visitor>
void walk_file(Visitor &v, File *f) {
    for (auto a : f->toplevels) {
        v.visit_toplevel(a);
    }
}
template <typename Visitor>
void walk_var_decl(Visitor &v, VarDeclNode *var) {
    if (var->assign_expr) {
        v.visit_expr(var->assign_expr);
    } else if (var->type_expr) {
        // XXX again, Type'Expr'?
        v.visit_type_expr(static_cast<TypeExpr *>(var->type_expr));
    } else {
        assert(false && "unreachable");
    }
}
template <typename Visitor>
void walk_struct_decl(Visitor &v, StructDeclNode *s) {
    for (auto d : s->members) {
        v.visit_decl(d);
    }
}
template <typename Visitor>
void walk_func_decl(Visitor &v, FuncDeclNode *f) {
    if (f->ret_type_expr) {
        v.visit_expr(f->ret_type_expr);
    }
    for (auto arg : f->args) {
        v.visit_decl(arg);
    }
    v.visit_compound_stmt(f->body);
}
template <typename Visitor>
void walk_decl_stmt(Visitor &v, DeclStmt *ds) {
    v.visit_decl(ds->decl);
}
template <typename Visitor>
void walk_expr_stmt(Visitor &v, ExprStmt *es) {
    v.visit_expr(es->expr);
}
template <typename Visitor>
void walk_assign_stmt(Visitor &v, AssignStmt *as) {
    v.visit_expr(as->rhs);
    v.visit_expr(as->lhs);
}
template <typename Visitor>
void walk_return_stmt(Visitor &v, ReturnStmt *rs) {
    v.visit_expr(rs->expr);
}
template <typename Visitor>
void walk_compound_stmt(Visitor &v, CompoundStmt *cs) {
    for (auto s : cs->stmts) {
        v.visit_stmt(s);
    }
}
template <typename Visitor>
void walk_if_stmt(Visitor &v, IfStmt *is) {
    v.visit_expr(is->cond);
    v.visit_compound_stmt(is->cstmt_true);
    if (is->elseif) {
        v.visit_if_stmt(is->elseif);
    } else if (is->cstmt_false) {
        v.visit_compound_stmt(is->cstmt_false);
    }
}
template <typename Visitor>
void walk_func_call_expr(Visitor &v, FuncCallExpr *f) {
    for (auto arg : f->args) {
        v.visit_expr(arg);
    }
}
template <typename Visitor>
void walk_binary_expr(Visitor &v, BinaryExpr *b) {
    v.visit_expr(b->lhs);
    v.visit_expr(b->rhs);
}
template <typename Visitor>
void walk_member_expr(Visitor &v, MemberExpr *m) {
    v.visit_expr(m->struct_expr);
}
template <typename Visitor>
void walk_type_expr(Visitor &v, TypeExpr *t) {
    if (t->subexpr) {
        v.visit_expr(t->subexpr);
    }
}

} // namespace cmp

#endif
