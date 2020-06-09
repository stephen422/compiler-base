#ifndef AST_H
#define AST_H

#include "types.h"
#include "lexer.h"
#include <cassert>
#include <unordered_map>
#include <variant> // FIXME

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

enum class Pass {
  none,
  parse,
  namebind,
  typecheck,
  // returncheck?
  codegen,
};

struct AstNode {
  AstKind kind;               // node kind
  size_t pos = 0;             // start pos of this AST in the source text
  Pass progress = Pass::none; // latest pass that this node got through
  bool failed = false;

  AstNode() {}
  AstNode(AstKind kind) : kind(kind) {}
  virtual ~AstNode() = default;

  // Casts to the *pointer* of the given type.  Not checked.
  template <typename T> T *as() { return static_cast<T *>(this); }
  template <typename T> const T *as() const {
    return static_cast<const T *>(this);
  }

  // AST printing.
  virtual void print() const = 0;
  // Convenience ostream for AST printing.
  // Handles indentation, tree drawing, etc.
  std::ostream &out() const;

  // RAII trick to handle indentation.
  static int indent;
  struct PrintScope {
    PrintScope() { indent += 2; }
    ~PrintScope() { indent -= 2; }
  };
};

// File is simply a group of Toplevels.
struct File : public AstNode {
    File() : AstNode(AstKind::file) {}
    void print() const override;

    std::vector<AstNode *> toplevels;
};

//
// Statements
// ==========

enum class StmtKind {
    decl,
    expr,
    assign,
    return_,
    compound,
    if_,
    builtin,
    bad,
};

struct Stmt : public AstNode {
    Stmt(StmtKind s) : AstNode(AstKind::stmt), kind(s) {}

    StmtKind kind;
};

struct DeclStmt : public Stmt {
    DeclStmt(DeclNode *d) : Stmt(StmtKind::decl), decl(d) {}
    void print() const override;

    DeclNode *decl;
};

struct ExprStmt : public Stmt {
    ExprStmt(Expr *e) : Stmt(StmtKind::expr), expr(e) {}
    void print() const override;

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

    Expr *lhs;
    Expr *rhs;
};

struct ReturnStmt : public Stmt {
    ReturnStmt(Expr *e) : Stmt(StmtKind::return_), expr(e) {}
    void print() const override;

    Expr *expr;
};

struct CompoundStmt : public Stmt {
    CompoundStmt() : Stmt(StmtKind::compound) {}
    void print() const override;

    std::vector<Stmt *> stmts;
};

struct IfStmt : public Stmt {
    Expr *cond;               // conditional expr
    CompoundStmt *if_body; // body for true cond
    // Views 'else if' clauses as a separate if statement for the false case.
    // 'elseif' and 'cstmt_false' cannot be non-null at the same time.
    IfStmt *else_if = nullptr;
    CompoundStmt *else_body = nullptr;

    IfStmt(Expr *e, CompoundStmt *is, IfStmt *ei, CompoundStmt *es)
        : Stmt(StmtKind::if_), cond(e), if_body(is), else_if(ei),
        else_body(es) {}
    void print() const override;
};

struct BuiltinStmt : public Stmt {
  std::string_view text;

  BuiltinStmt(std::string_view sv)
      : Stmt(StmtKind::builtin), text(sv) {}
  void print() const override;
};

struct BadStmt : public Stmt {
    BadStmt() : Stmt(StmtKind::bad) {}
    void print() const override;
};


// Expressions
// ===========

enum class ExprKind {
    integer_literal,
    string_literal,
    decl_ref,
    func_call,
    struct_def,
    member,
    unary,
    binary,
    type,
    bad,
};

struct Type;

struct Expr : public AstNode {
    ExprKind kind;
    // Type of the expression.
    // For expressions that have a Decl, e.g. DeclRefExpr and MemberExpr, their
    // types are stored in decl->type.  For these cases, the value of this
    // pointer should be maintained the same as decl->type, so that expr->type
    // becomes the unified way to retrieve the type of an expression.
    Type *type = nullptr;
    // TODO: I think this function is somehow related to lvalue determination.
    std::optional<Decl> decl() const;

    Expr(ExprKind e) : AstNode(AstKind::expr), kind(e) {}
};

enum class UnaryExprKind {
    paren,
    ref,
    deref,
    plus, // TODO
    minus, // TODO
};

struct IntegerLiteral : public Expr {
    int64_t value;

    IntegerLiteral(int64_t v) : Expr(ExprKind::integer_literal), value(v) {}
    void print() const override;
};

struct StringLiteral : public Expr {
    std::string_view value;

    StringLiteral(std::string_view sv)
        : Expr(ExprKind::string_literal), value(sv) {}
    void print() const override;
};

// A unary expression that references a declaration object, e.g. a variable or
// a function.
struct DeclRefExpr : public Expr {
    Name *name = nullptr;
    Decl decl;

    DeclRefExpr(Name *n) : Expr(ExprKind::decl_ref), name(n) {}
    void print() const override;
};

struct FuncCallExpr : public Expr {
    Name *func_name = nullptr;
    std::vector<Expr *> args;
    FuncDecl *func_decl = nullptr;

    FuncCallExpr(Name *name, const std::vector<Expr *> &args)
        : Expr(ExprKind::func_call), func_name(name),
          args(args) {}
    void print() const override;
};

// '.memb = expr' part in Struct { ... }.
struct StructFieldDesignator {
    Name *name = nullptr;
    VarDecl *decl = nullptr;
    Expr *expr = nullptr;
};

// 'Struct { .m1 = .e1, .m2 = e2, ... }'
struct StructDefExpr : public Expr {
    Expr *name_expr; // either a DeclRefExpr or a MemberExpr
    std::vector<StructFieldDesignator> desigs;

    StructDefExpr(Expr *e, const std::vector<StructFieldDesignator> &ds)
        : Expr(ExprKind::struct_def), name_expr(e), desigs(ds) {}
    void print() const override;
};

// 'struct.mem'
struct MemberExpr : public Expr {
    Expr *lhs_expr = nullptr;    // 'struct' part
    Name *member_name = nullptr; // 'mem' part
    Decl decl;                   // decl of 'mem'

    MemberExpr(Expr *e, Name *m)
        : Expr(ExprKind::member), lhs_expr(e), member_name(m) {}
    void print() const override;
};

struct UnaryExpr : public Expr {
    UnaryExprKind kind;
    Expr *operand;
    VarDecl *var_decl = nullptr;

    UnaryExpr(UnaryExprKind k, Expr *oper)
        : Expr(ExprKind::unary), kind(k), operand(oper) {}
    void print() const override;
};

struct ParenExpr : public UnaryExpr {
    // The 'inside' expression is stored in UnaryExpr::operand.

    ParenExpr(Expr *inside_expr)
        : UnaryExpr(UnaryExprKind::paren, inside_expr) {}
    void print() const override;
    // ParenExprs may also have an associated Decl (e.g. (a).m).
    std::optional<Decl> decl() const { return operand->decl(); }
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
};

// XXX: can I call this an expression?
enum class TypeExprKind {
  none,
  value,
  ref,
  array,
};

struct TypeExpr : public Expr {
    TypeExprKind kind = TypeExprKind::none;
    // Name of the type. TODO: should this contain '&' and '[]'?
    Name *name = nullptr;
    // Decl object that represents this type.
    Decl decl;
    // Is this type mutable?
    bool mut = false;
    // 'T' part of '&T'.  It is Expr rather than TypeExpr mainly so that it can
    // store BadExpr.  XXX dirty.
    Expr *subexpr = nullptr;

    // TODO: incomplete.
    TypeExpr(TypeExprKind k, Name *n, bool m, Expr *se)
        : Expr(ExprKind::type), kind(k), name(n), mut(m), subexpr(se) {}
    void print() const override;
};

struct BadExpr : public Expr {
    BadExpr() : Expr(ExprKind::bad) {}
    void print() const override;
};

// Declarations
// ============

enum class DeclNodeKind {
    var,
    func,
    struct_,
    enum_variant,
    enum_,
    bad,
};

// A declaration node.
struct DeclNode : public AstNode {
    DeclNodeKind kind;

    DeclNode(DeclNodeKind d) : AstNode(AstKind::decl), kind(d) {}
    // TODO: Unlike Expr, there is no Decl * here.  Do we need it?
};

enum class VarDeclNodeKind {
  local,
  struct_,
  param,
};

// Variable declaration.
struct VarDeclNode : public DeclNode {
    // The value of this pointer serves as a unique integer ID to be used for
    // indexing the symbol table.
    Name *name = nullptr;        // name of the variable
    VarDecl *var_decl = nullptr; // decl of the variable
    VarDeclNodeKind kind = VarDeclNodeKind::local;
    // TypeExpr of the variable.  Declared as Expr to accommodate for BadExpr.
    Expr *type_expr = nullptr;
    Expr *assign_expr = nullptr; // initial assignment value
    bool mut = false;            // mutability of the variable

    VarDeclNode(Name *n, VarDeclNodeKind k, Expr *t, Expr *expr)
        : DeclNode(DeclNodeKind::var), name(n), kind(k), type_expr(t),
          assign_expr(std::move(expr)) {}
    void print() const override;
};

// Function declaration.  There is no separate function definition: functions
// should always be defined whenever they are declared.
struct FuncDeclNode : public DeclNode {
  Name *name = nullptr;            // name of the function
  FuncDecl *func_decl = nullptr;   // decl object
  std::vector<VarDeclNode *> args; // list of parameters
  CompoundStmt *body = nullptr;    // body statements
  Expr *ret_type_expr = nullptr;     // return type expression

  FuncDeclNode(Name *n) : DeclNode(DeclNodeKind::func), name(n) {}
  void print() const override;
};

// Struct declaration.
struct StructDeclNode : public DeclNode {
  Name *name = nullptr;               // name of the struct
  std::vector<VarDeclNode *> members; // member variables
  StructDecl *struct_decl = nullptr;    // decl object

  StructDeclNode(Name *n, std::vector<VarDeclNode *> m)
      : DeclNode(DeclNodeKind::struct_), name(n), members(m) {}
  void print() const override;
};

// A variant type in an enum.
struct EnumVariantDeclNode : public DeclNode {
  Name *name = nullptr;             // name of the variant
  std::vector<Expr *> fields;   // type of the fields
  StructDecl *struct_decl = nullptr; // decl object

  EnumVariantDeclNode(Name *n, std::vector<Expr *> f)
      : DeclNode(DeclNodeKind::enum_variant), name(n), fields(f) {}
  void print() const override;
};

// Enum declaration.
struct EnumDeclNode : public DeclNode {
  Name *name = nullptr;                        // name of the struct
  std::vector<EnumVariantDeclNode *> variants; // variants
  EnumDecl *enum_decl = nullptr;               // decl object

  EnumDeclNode(Name *n, std::vector<EnumVariantDeclNode *> m)
      : DeclNode(DeclNodeKind::enum_), name(n), variants(m) {}
  void print() const override;
};

struct BadDeclNode : public DeclNode {
    BadDeclNode() : DeclNode(DeclNodeKind::bad) {}
    void print() const override;
};

} // namespace cmp

#endif
