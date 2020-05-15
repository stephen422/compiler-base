#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <cassert>
#include <iostream>
#include <memory>
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
struct Toplevel;
struct Stmt;
struct Expr;
struct DeclNode;
struct FuncDeclNode;
struct Type;
struct VarDecl;
struct StructDecl;
struct EnumDecl;
struct FuncDecl;
using Decl = std::variant<VarDecl *, StructDecl *, EnumDecl *, FuncDecl *>;

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
    AstKind kind;   // node kind
    size_t pos = 0; // start pos of this AST in the source text

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
};

// ========
//   File
// ========

// File is simply a group of Toplevels.
struct File : public AstNode {
    File() : AstNode(AstKind::file) {}
    void print() const override;

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


// ==============
//   Expression
// ==============

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
    address,
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

enum class DeclRefKind {
  undecided,
  var,
  struct_,
  enum_,
};

// A unary expression that references a declaration object, e.g. a variable or
// a function.
struct DeclRefExpr : public Expr {
  DeclRefKind kind;
  Name *name = nullptr;
  union {
    VarDecl *var_decl = nullptr;
    StructDecl *struct_decl;
    EnumDecl *enum_decl;
  };

  DeclRefExpr(DeclRefKind k, Name *n)
      : Expr(ExprKind::decl_ref), kind(k), name(n) {}
  void print() const override;
};

struct FuncCallExpr : public Expr {
    Name *func_name = nullptr;
    std::vector<Expr *> args;
    // Decl for the function name, 'func'.
    FuncDecl *func_decl = nullptr;

    FuncCallExpr(Name *name, const std::vector<Expr *> &args)
        : Expr(ExprKind::func_call), func_name(name),
          args(args) {}
    void print() const override;
};

// '.memb = expr' part in Struct { ... }.
struct StructFieldDesignator {
    Name *name;
    Expr *expr;
};

// 'Struct { .m1 = .e1, .m2 = e2, ... }'
struct StructDefExpr : public Expr {
    Expr *name_expr; // could be DeclRefs or MemberExprs
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
    UnaryExprKind unaryKind;
    Expr *operand;
    VarDecl *varDecl = nullptr;

    UnaryExpr(UnaryExprKind k, Expr *oper)
        : Expr(ExprKind::unary), unaryKind(k), operand(oper) {}
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
    TypeExpr(TypeExprKind k, Name *n, Expr *se)
        : Expr(ExprKind::type), kind(k), name(n), subexpr(se) {}
    void print() const override;
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

// AST visitor.
//
// Custom visitors, e.g. name binders or type checkers, are expected to inherit
// from this class and override the node visitor functions as necessary.
//
// Curiously-recurring template pattern (CRTP) is used to enable calling the
// visitor function of the derived class from the that of the parent class
// defined here.  See commit log b7e6113.
template <typename Derived, typename RetTy, typename... Args> class AstVisitor {
  // 'Derived this'. By calling visitors and walkers through the return
  // pointer of this function, the base class can access the derived visitor
  // implementations.
  constexpr Derived *dis() { return static_cast<Derived *>(this); }

public:
  //
  // AST visitor functions.
  //
  // These are to be overridden by the specialized visitor classes. They
  // provide a default behavior of simply traversing the node, acting nothing
  // upon them.
  //

  RetTy visitFile(File *f, Args... args) {
    walk_file(*dis(), f, args...);
    return RetTy();
  }
  RetTy visitToplevel(AstNode *a, Args... args) {
    switch (a->kind) {
    case AstKind::stmt:
      return dis()->visitStmt(static_cast<Stmt *>(a), args...);
      break;
    case AstKind::decl:
      return dis()->visitDecl(static_cast<DeclNode *>(a), args...);
      break;
    default:
      assert(false && "not a toplevel node");
    }
    return RetTy();
  }
  RetTy visitStmt(Stmt *s, Args... args) {
    switch (s->kind) {
    case StmtKind::decl:
      return dis()->visitDeclStmt(static_cast<DeclStmt *>(s), args...);
      break;
    case StmtKind::expr:
      return dis()->visitExprStmt(static_cast<ExprStmt *>(s), args...);
      break;
    case StmtKind::assign:
      return dis()->visitAssignStmt(static_cast<AssignStmt *>(s), args...);
      break;
    case StmtKind::return_:
      return dis()->visitReturnStmt(static_cast<ReturnStmt *>(s), args...);
      break;
    case StmtKind::compound:
      return dis()->visitCompoundStmt(static_cast<CompoundStmt *>(s), args...);
      break;
    case StmtKind::if_:
      return dis()->visitIfStmt(static_cast<IfStmt *>(s), args...);
      break;
    case StmtKind::builtin:
      return dis()->visitBuiltinStmt(static_cast<BuiltinStmt *>(s), args...);
      break;
    case StmtKind::bad:
      // do nothing
      break;
    default:
      assert(false);
    }
    return RetTy();
  }
  RetTy visitDeclStmt(DeclStmt *ds, Args... args) {
    walk_decl_stmt(*dis(), ds, args...);
    return RetTy();
  }
  RetTy visitExprStmt(ExprStmt *es, Args... args) {
    walk_expr_stmt(*dis(), es, args...);
    return RetTy();
  }
  RetTy visitAssignStmt(AssignStmt *as, Args... args) {
    walk_assign_stmt(*dis(), as, args...);
    return RetTy();
  }
  RetTy visitReturnStmt(ReturnStmt *rs, Args... args) {
    walk_return_stmt(*dis(), rs, args...);
    return RetTy();
  }
  RetTy visitCompoundStmt(CompoundStmt *cs, Args... args) {
    walk_compound_stmt(*dis(), cs, args...);
    return RetTy();
  }
  RetTy visitIfStmt(IfStmt *is, Args... args) {
    walk_if_stmt(*dis(), is, args...);
    return RetTy();
  }
  RetTy visitBuiltinStmt(BuiltinStmt *bs, Args... args) {
    // nothing to walk
    return RetTy();
  }
  RetTy visitDecl(DeclNode *d, Args... args) {
    switch (d->kind) {
    case DeclNodeKind::var:
      return dis()->visitVarDecl(static_cast<VarDeclNode *>(d), args...);
      break;
    case DeclNodeKind::func:
      return dis()->visitFuncDecl(static_cast<FuncDeclNode *>(d), args...);
      break;
    case DeclNodeKind::struct_:
      return dis()->visitStructDecl(static_cast<StructDeclNode *>(d), args...);
      break;
    case DeclNodeKind::enum_variant:
      return dis()->visitEnumVariantDecl(static_cast<EnumVariantDeclNode *>(d),
                                         args...);
      break;
    case DeclNodeKind::enum_:
      return dis()->visitEnumDecl(static_cast<EnumDeclNode *>(d), args...);
      break;
    case DeclNodeKind::bad:
      // do nothing
      break;
    default:
      assert(false);
    }
    return RetTy();
  }
  RetTy visitVarDecl(VarDeclNode *v, Args... args) {
    walk_var_decl(*dis(), v, args...);
    return RetTy();
  }
  RetTy visitFuncDecl(FuncDeclNode *f, Args... args) {
    walk_func_decl(*dis(), f, args...);
    return RetTy();
  }
  RetTy visitStructDecl(StructDeclNode *s, Args... args) {
    walk_struct_decl(*dis(), s, args...);
    return RetTy();
  }
  RetTy visitEnumVariantDecl(EnumVariantDeclNode *e, Args... args) {
    walk_enum_variant_decl(*dis(), e, args...);
    return RetTy();
  }
  RetTy visitEnumDecl(EnumDeclNode *e, Args... args) {
    walk_enum_decl(*dis(), e, args...);
    return RetTy();
  }
  RetTy visitExpr(Expr *e, Args... args) {
    // Rather than calling walk_expr() here, we do a switch-case, because the
    // visiting logic in a specialized visitor is likely to be different for
    // each type of Expr and thus a similar switch-case will be used anyway.
    switch (e->kind) {
    case ExprKind::integer_literal:
      return dis()->visitIntegerLiteral(static_cast<IntegerLiteral *>(e),
                                        args...);
      break;
    case ExprKind::string_literal:
      return dis()->visitStringLiteral(static_cast<StringLiteral *>(e),
                                       args...);
      // do nothing
      break;
    case ExprKind::decl_ref:
      return dis()->visitDeclRefExpr(static_cast<DeclRefExpr *>(e), args...);
      break;
    case ExprKind::func_call:
      return dis()->visitFuncCallExpr(static_cast<FuncCallExpr *>(e), args...);
      break;
    case ExprKind::struct_def:
      return dis()->visitStructDefExpr(static_cast<StructDefExpr *>(e),
                                       args...);
      break;
    case ExprKind::member:
      return dis()->visitMemberExpr(static_cast<MemberExpr *>(e), args...);
      break;
    case ExprKind::unary:
      return dis()->visitUnaryExpr(static_cast<UnaryExpr *>(e), args...);
      break;
    case ExprKind::binary:
      return dis()->visitBinaryExpr(static_cast<BinaryExpr *>(e), args...);
      break;
    case ExprKind::type:
      return dis()->visitTypeExpr(static_cast<TypeExpr *>(e), args...);
      break;
    case ExprKind::bad:
      // do nothing
      break;
    default:
      assert(false);
      break;
    }
    return RetTy();
  }
  RetTy visitIntegerLiteral(IntegerLiteral *i, Args... args) {
    // nothing to walk
    return RetTy();
  }
  RetTy visitStringLiteral(StringLiteral *s, Args... args) {
    // nothing to walk
    return RetTy();
  }
  RetTy visitDeclRefExpr(DeclRefExpr *d, Args... args) {
    // nothing to walk
    return RetTy();
  }
  RetTy visitFuncCallExpr(FuncCallExpr *f, Args... args) {
    walk_func_call_expr(*dis(), f, args...);
    return RetTy();
  }
  RetTy visitStructDefExpr(StructDefExpr *s, Args... args) {
    walk_struct_def_expr(*dis(), s, args...);
    return RetTy();
  }
  RetTy visitMemberExpr(MemberExpr *m, Args... args) {
    walk_member_expr(*dis(), m, args...);
    return RetTy();
  }
  RetTy visitUnaryExpr(UnaryExpr *u, Args... args) {
    switch (u->unaryKind) {
    case UnaryExprKind::paren:
      return dis()->visitParenExpr(static_cast<ParenExpr *>(u), args...);
      break;
    case UnaryExprKind::address:
      return dis()->visitExpr(u->operand, args...);
      break;
    case UnaryExprKind::deref:
      return dis()->visitExpr(u->operand, args...);
      break;
    default:
      assert(false);
      break;
    }
    return RetTy();
  }
  RetTy visitParenExpr(ParenExpr *p, Args... args) {
    walk_paren_expr(*dis(), p, args...);
    return RetTy();
  }
  RetTy visitBinaryExpr(BinaryExpr *b, Args... args) {
    walk_binary_expr(*dis(), b, args...);
    return RetTy();
  }
  RetTy visitTypeExpr(TypeExpr *t, Args... args) {
    walk_type_expr(*dis(), t, args...);
    return RetTy();
  }
};

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
// Note: The functions here combined can be regarded as what the 'accept()'
// function do in the visitor pattern. However, whereas the accept() function
// is declared as virtual in the pattern proper, these need not be polymorphic.
// TODO: Document why.
//

template <typename Visitor, typename... Args>
void walk_file(Visitor &v, File *f, Args... args) {
  for (auto a : f->toplevels) {
    v.visitToplevel(a, args...);
  }
}
template <typename Visitor, typename... Args>
void walk_var_decl(Visitor &v, VarDeclNode *var, Args... args) {
  if (var->assign_expr) {
    v.visitExpr(var->assign_expr, args...);
  } else if (var->type_expr) {
    v.visitTypeExpr(static_cast<TypeExpr *>(var->type_expr), args...);
  } else {
    assert(false && "unreachable");
  }
}
template <typename Visitor, typename... Args>
void walk_func_decl(Visitor &v, FuncDeclNode *f, Args... args) {
  if (f->ret_type_expr)
    v.visitExpr(f->ret_type_expr, args...);
  for (auto arg : f->args)
    v.visitDecl(arg, args...);
  v.visitCompoundStmt(f->body, args...);
}
template <typename Visitor, typename... Args>
void walk_struct_decl(Visitor &v, StructDeclNode *s, Args... args) {
  for (auto d : s->members) {
    v.visitDecl(d, args...);
  }
}
template <typename Visitor, typename... Args>
void walk_enum_variant_decl(Visitor &v, EnumVariantDeclNode *e, Args... args) {
  for (auto f : e->fields)
    v.visitExpr(f, args...);
}
template <typename Visitor, typename... Args>
void walk_enum_decl(Visitor &v, EnumDeclNode *e, Args... args) {
  for (auto var : e->variants)
    v.visitEnumVariantDecl(var, args...);
}
template <typename Visitor, typename... Args>
void walk_decl_stmt(Visitor &v, DeclStmt *ds, Args... args) {
  v.visitDecl(ds->decl, args...);
}
template <typename Visitor, typename... Args>
void walk_expr_stmt(Visitor &v, ExprStmt *es, Args... args) {
  v.visitExpr(es->expr, args...);
}
template <typename Visitor, typename... Args>
void walk_assign_stmt(Visitor &v, AssignStmt *as, Args... args) {
  v.visitExpr(as->rhs, args...);
  v.visitExpr(as->lhs, args...);
}
template <typename Visitor, typename... Args>
void walk_return_stmt(Visitor &v, ReturnStmt *rs, Args... args) {
  v.visitExpr(rs->expr, args..., args...);
}
template <typename Visitor, typename... Args>
void walk_compound_stmt(Visitor &v, CompoundStmt *cs, Args... args) {
  for (auto s : cs->stmts) {
    v.visitStmt(s, args...);
  }
}
template <typename Visitor, typename... Args>
void walk_if_stmt(Visitor &v, IfStmt *is, Args... args) {
  v.visitExpr(is->cond, args...);
  v.visitCompoundStmt(is->if_body, args...);
  if (is->else_if)
    v.visitIfStmt(is->else_if, args...);
  else if (is->else_body)
    v.visitCompoundStmt(is->else_body, args...);
}
template <typename Visitor, typename... Args>
void walk_func_call_expr(Visitor &v, FuncCallExpr *f, Args... args) {
  for (auto arg : f->args)
    v.visitExpr(arg, args...);
}
template <typename Visitor, typename... Args>
void walk_struct_def_expr(Visitor &v, StructDefExpr *s, Args... args) {
    v.visitExpr(s->name_expr, args...);
    for (auto d : s->desigs)
        v.visitExpr(d.expr, args...);
}
template <typename Visitor, typename... Args>
void walk_member_expr(Visitor &v, MemberExpr *m, Args... args) {
    v.visitExpr(m->lhs_expr, args...);
}
template <typename Visitor, typename... Args>
void walk_paren_expr(Visitor &v, ParenExpr *p, Args... args) {
    v.visitExpr(p->operand, args...);
}
template <typename Visitor, typename... Args>
void walk_binary_expr(Visitor &v, BinaryExpr *b, Args... args) {
    v.visitExpr(b->lhs, args...);
    v.visitExpr(b->rhs, args...);
}
template <typename Visitor, typename... Args>
void walk_type_expr(Visitor &v, TypeExpr *t, Args... args) {
    if (t->subexpr)
        v.visitExpr(t->subexpr, args...);
}

} // namespace cmp

#endif
