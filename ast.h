#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <iostream>
#include <memory>
#include <unordered_map>

namespace cmp {

// A Name corresponds to a single unique identifier string in the source text.
// There may be multiple occurrences of a string in the source text, but only
// one instance of the matching Name can reside in the name table.
struct Name {
    std::string text;

    Name(const std::string &s) : text(s) {}
    std::string toString() const;
};

// A NameTable is a hash table of Names queried by their string value.  It
// serves to reduce the number of string hashing operation, since we can look
// up the symbol table using Name instead of raw char * throughout the semantic
// analysis.
class NameTable {
public:
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
    struct_decl,
    member_decl,
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

struct AstNode;
struct File;
struct Toplevel;
struct Stmt;
struct Expr;
struct DeclNode;
struct FuncDecl;

std::pair<size_t, size_t> get_ast_range(std::initializer_list<AstNode *> nodes);

// Ast is a struct that contains all necessary information for semantic
// analysis of an AST: namely, the root node and the name table.
struct Ast {
  AstNode *root;
  NameTable &name_table;
};

struct Sema;
struct Decl;

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
    // Name binding pass.
    virtual void nameBindPre(Sema &sema) { (void)sema; }
    virtual void nameBindPost(Sema &sema) { (void)sema; }

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
    struct PrintScope {
        PrintScope() { depth += 2; }
        ~PrintScope() { depth -= 2; }
    };

    AstKind kind = AstKind::none; // node kind
    size_t pos = 0;               // start pos of this AST in the source text
    // Indentation of the current node when dumping AST.
    // static because all nodes share this.
    static int depth;
};

// These are free-standing functions that simply do the virtual call into the
// polymorphic compiler pass functions.
inline void nameBindPre(Sema &sema, AstNode *node) { node->nameBindPre(sema); }
inline void nameBindPost(Sema &sema, AstNode *node) { node->nameBindPost(sema); }

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
    ReturnStmt(Expr *expr) : Stmt(AstKind::return_stmt), expr(std::move(expr)) {}
    void print() const override;
    void walk(Sema &sema) override;

    Expr *expr;
};

struct CompoundStmt : public Stmt {
    CompoundStmt() : Stmt(AstKind::compound_stmt) {}
    void print() const override;
    void walk(Sema &sema) override;
    void nameBindPre(Sema &sema) override;
    void nameBindPost(Sema &sema) override;

    std::vector<Stmt *> stmts;
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
    enum UnaryKind {
        Literal,
        DeclRef,
        FuncCall,
        Paren,
        Address,
        Deref,
        // TODO:
        Plus,
        Minus,
    } unary_kind;
    Expr *operand;

    UnaryExpr(UnaryKind k, Expr *oper)
        : Expr(AstKind::unary_expr), unary_kind(k), operand(std::move(oper)) {}
    void print() const override;
    void walk(Sema &sema) override;
};

struct IntegerLiteral : public UnaryExpr {
    int64_t value;

    IntegerLiteral(int64_t v) : UnaryExpr(Literal, nullptr), value(v) {}
    void print() const override;
    void walk(Sema &sema) override;
};

struct StringLiteral : public UnaryExpr {
    const std::string_view value;

    StringLiteral(std::string_view sv) : UnaryExpr(Literal, nullptr), value(sv) {}
    void print() const override;
    void walk(Sema &sema) override;
};

struct DeclRefExpr : public UnaryExpr {
    // The integer value of this pointer serves as a unique ID to be used for
    // indexing the symbol table in the name binding phase.
    Name *name = nullptr;
    Decl *decl = nullptr;

    DeclRefExpr(Name *name) : UnaryExpr(DeclRef, nullptr), name(name) {}
    void print() const override;
    void walk(Sema &sema) override;
    void nameBindPost(Sema &sema) override;
};

struct FuncCallExpr : public UnaryExpr {
    Name *name = nullptr;
    std::vector<Expr *> args;

    FuncCallExpr(Name *name, const std::vector<Expr *> &args)
        : UnaryExpr(FuncCall, nullptr), name(name), args(args) {}
    void print() const override;
    void walk(Sema &sema) override;
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
    void nameBindPost(Sema &sema) override;
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

struct VarDecl;
struct StructDecl;
struct FuncDecl;

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
struct VarDecl : public DeclNode {
    VarDecl(Name *n, bool mem, Expr *t, Expr *expr)
        : DeclNode(AstKind::var_decl), name(n), is_member(mem), type_expr(t),
          assign_expr(std::move(expr)) {}
    void print() const override;
    void walk(Sema &sema) override;
    void nameBindPost(Sema &sema) override;

    // The value of this pointer serves as a unique integer ID to be used for
    // indexing the symbol table.
    Name *name = nullptr;        // name of the variable
    bool is_member = false;      // member of a struct?
    Expr *type_expr = nullptr;   // type node of the variable
                                 // (inferred later if null)
    Expr *assign_expr = nullptr; // initial assignment value
};

// Struct declaration.
struct StructDecl : public DeclNode {
    StructDecl(Name *n, std::vector<DeclNode *> m)
        : DeclNode(AstKind::struct_decl), name(n), members(m) {}
    void print() const override;
    void walk(Sema &sema) override;
    void nameBindPost(Sema &sema) override;

    Name *name = nullptr;            // name of the struct
    std::vector<DeclNode *> members; // member variables
};

// Struct member declaration.
struct MemberDecl : public DeclNode {
    MemberDecl(Name *n, Expr *t, Expr *expr)
        : DeclNode(AstKind::member_decl), name(n), type_expr(t),
          assign_expr(expr) {}
    void print() const override;
    void nameBindPost(Sema &sema) override;

    Name *name = nullptr;        // name of the member
    Expr *type_expr = nullptr;   // type node of the member.
                                 // If null, it will be inferred later.
    Expr *assign_expr = nullptr; // initial assignment value
};

// Function declaration.  There is no separate function definition: functions
// should always be defined whenever they are declared.
struct FuncDecl : public DeclNode {
    FuncDecl(Name *n) : DeclNode(AstKind::func_decl), name(n) {}
    void print() const override;
    void walk(Sema &sema) override;

    Name *name = nullptr;          // name of the function
    std::vector<DeclNode *> params;    // list of parameters
    CompoundStmt *body = nullptr;  // body statements
    Expr *ret_type_expr = nullptr; // return type expression
};

struct BadDecl : public DeclNode {
    BadDecl() : DeclNode(AstKind::bad_decl) {}
    void print() const override;
};

void test(Sema &sema);

} // namespace cmp

#endif
