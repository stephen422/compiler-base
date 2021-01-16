#ifndef CMP_AST_H
#define CMP_AST_H

#include "lexer.h"
#include "types.h"
#include <type_traits>

namespace cmp {

// Forward-declare stuff so that we don't have to include sema.h.
struct AstNode;
struct File;
struct Stmt;
struct Expr;
class Decl;
struct VarDecl;
struct FuncDecl;
struct StructDecl;
struct EnumVariantDecl;
struct EnumDecl;
struct ExternDecl;
struct BadDecl;
class Lifetime;

std::pair<size_t, size_t> get_ast_range(std::initializer_list<AstNode *> nodes);

enum class AstKind {
    file,
    stmt,
    decl,
    expr,
};

struct AstNode {
    const AstKind kind = AstKind::decl; // node kind
    size_t pos = 0;             // start pos of this AST in the source text
    size_t endpos = 0;          // end pos of this AST in the source text

    AstNode() {}
    AstNode(AstKind kind) : kind(kind) {}
    virtual ~AstNode() = default;

    // Casts to the *pointer* of the given type.  Not checked.
    template <typename T> T *as() { return static_cast<T *>(this); }
    template <typename T> const T *as() const {
        return static_cast<const T *>(this);
    }

    // Get the text representation of this node.
    std::string_view text(const Source &source) const;

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

    const StmtKind kind;
};

struct DeclStmt : public Stmt {
    DeclStmt(Decl *d) : Stmt(StmtKind::decl), decl(d) {}

    Decl *decl;
};

struct ExprStmt : public Stmt {
    ExprStmt(Expr *e) : Stmt(StmtKind::expr), expr(e) {}

    Expr *expr;
};

// Move or copy assignment statement, e.g. 'a <- b' or 'a[0] = func()'.
// Non-single-token expressions can come at the RHS as long as they are lvalues,
// but this is not easily determined at the parsing stage.  As such, parse both
// LHS and RHS as generic Exprs, and check the assignability at the semantic
// stage.
struct AssignStmt : public Stmt {
    AssignStmt(Expr *l, Expr *r, bool m)
        : Stmt(StmtKind::assign), lhs(l), rhs(r), move(m) {}

    Expr *lhs;
    Expr *rhs;
    bool move;
};

struct ReturnStmt : public Stmt {
    ReturnStmt(Expr *e) : Stmt(StmtKind::return_), expr(e) {}

    Expr *expr;
};

struct CompoundStmt : public Stmt {
    CompoundStmt() : Stmt(StmtKind::compound) {}

    std::vector<Stmt *> stmts;
};

struct IfStmt : public Stmt {
    Expr *cond;            // conditional expr
    CompoundStmt *if_body; // body for true cond
    // Views 'else if' clauses as a separate if statement for the false case.
    // 'elseif' and 'cstmt_false' cannot be non-null at the same time.
    IfStmt *else_if = nullptr;
    CompoundStmt *else_body = nullptr;

    IfStmt(Expr *e, CompoundStmt *is, IfStmt *ei, CompoundStmt *es)
        : Stmt(StmtKind::if_), cond(e), if_body(is), else_if(ei),
          else_body(es) {}
};

struct BuiltinStmt : public Stmt {
    std::string_view text;

    BuiltinStmt(std::string_view sv) : Stmt(StmtKind::builtin), text(sv) {}
};

struct BadStmt : public Stmt {
    BadStmt() : Stmt(StmtKind::bad) {}
};

// Expressions
// ===========

enum class ExprKind {
    integer_literal,
    string_literal,
    decl_ref,
    call,
    struct_def,
    cast,
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
    //
    // For expressions that have a Decl, e.g. DeclRefExpr and MemberExpr, their
    // types are stored in decl->type.  For these cases, the value of this
    // pointer should be maintained the same as decl->type, so that expr->type
    // becomes the unified way to retrieve the type of an expression.
    Type *type = nullptr;

    Expr(ExprKind e) : AstNode(AstKind::expr), kind(e), type(nullptr) {}
};

struct IntegerLiteral : public Expr {
    int64_t value;

    IntegerLiteral(int64_t v) : Expr(ExprKind::integer_literal), value(v) {}
};

struct StringLiteral : public Expr {
    std::string_view value;

    StringLiteral(std::string_view sv)
        : Expr(ExprKind::string_literal), value(sv) {}
};

// A unary expression that references a declaration object, e.g. a variable or
// a function.
struct DeclRefExpr : public Expr {
    Name *name = nullptr;
    Decl *decl = nullptr;

    DeclRefExpr(Name *n) : Expr(ExprKind::decl_ref), name(n) {}
};

enum class CallExprKind {
    func,
};

// Also includes typecasts.
struct CallExpr : public Expr {
    CallExprKind kind;
    Name *func_name = nullptr;
    std::vector<Expr *> args;
    // Decl of the called function or the destination type.
    Decl *callee_decl = nullptr;

    CallExpr(CallExprKind kind, Name *name, const std::vector<Expr *> &args)
        : Expr(ExprKind::call), kind(kind), func_name(name), args(args) {}
};

// '.memb = expr' part in Struct { ... }.
struct StructFieldDesignator {
    Name *name = nullptr;
    VarDecl *decl = nullptr;
    Expr *initexpr = nullptr;
};

// 'Struct { .m1 = .e1, .m2 = e2, ... }'
struct StructDefExpr : public Expr {
    Expr *name_expr; // either a DeclRefExpr or a MemberExpr
    std::vector<StructFieldDesignator> desigs;

    StructDefExpr(Expr *e, const std::vector<StructFieldDesignator> &ds)
        : Expr(ExprKind::struct_def), name_expr(e), desigs(ds) {}
};

// 'struct.mem'
struct MemberExpr : public Expr {
    Expr *struct_expr = nullptr; // 'struct' part
    Name *member_name = nullptr; // 'mem' part

    // MemberExprs may or may not have an associated decl object, depending on
    // 'struct_expr' being l-value or r-value.
    std::optional<Decl *> decl;

    MemberExpr(Expr *e, Name *m)
        : Expr(ExprKind::member), struct_expr(e), member_name(m) {}
};

// '[type](expr)'
struct CastExpr : public Expr {
    Expr *type_expr = nullptr;
    Expr *target_expr = nullptr;

    CastExpr(Expr *type, Expr *target)
        : Expr(ExprKind::cast), type_expr(type), target_expr(target) {}
};

enum class UnaryExprKind {
    paren,
    ref,
    var_ref,
    deref,
    plus,  // TODO
    minus, // TODO
};

struct UnaryExpr : public Expr {
    const UnaryExprKind kind;
    Expr *operand;
    VarDecl *var_decl = nullptr;

    UnaryExpr(UnaryExprKind k, Expr *oper)
        : Expr(ExprKind::unary), kind(k), operand(oper) {}
};

struct ParenExpr : public UnaryExpr {
    // The 'inside' expression is stored in UnaryExpr::operand.

    ParenExpr(Expr *inside_expr)
        : UnaryExpr(UnaryExprKind::paren, inside_expr) {}
};

struct BinaryExpr : public Expr {
    Expr *lhs;
    Token op;
    Expr *rhs;

    BinaryExpr(Expr *lhs_, Token op_, Expr *rhs_)
        : Expr(ExprKind::binary), lhs(std::move(lhs_)), op(op_),
          rhs(std::move(rhs_)) {
        auto pair = get_ast_range({lhs, rhs});
        pos = pair.first;
    }
};

struct TypeExpr : public Expr {
    TypeKind kind = TypeKind::value;
    // Name of the type. TODO: should this contain '&' and '[]'?
    Name *name = nullptr;
    // Decl object that represents this type.
    Decl *decl = nullptr;
    // Is this type mutable?
    bool mut = false;
    // Name of the explicit lifetime annotation.
    Name *lifetime_annot = nullptr;
    // 'T' part of '&T'.  It is Expr rather than TypeExpr mainly so that it can
    // store BadExpr.  XXX dirty.
    Expr *subexpr = nullptr;

    // TODO: incomplete.
    TypeExpr(TypeKind k, Name *n, bool m, Name *lt, Expr *se)
        : Expr(ExprKind::type), kind(k), name(n), mut(m), lifetime_annot(lt),
          subexpr(se) {}
};

struct BadExpr : public Expr {
    BadExpr() : Expr(ExprKind::bad) {}
};

// Declarations
// ============

enum class DeclKind {
    var,
    func,
    struct_,
    enum_variant,
    enum_,
    extern_,
    bad,
};

template <typename T> bool decl_is(const DeclKind kind) {
    assert(false && "unhandled DeclNode type");
}
template <> inline bool decl_is<VarDecl>(const DeclKind kind) {
    return kind == DeclKind::var;
}
template <> inline bool decl_is<FuncDecl>(const DeclKind kind) {
    return kind == DeclKind::func;
}
template <> inline bool decl_is<StructDecl>(const DeclKind kind) {
    return kind == DeclKind::struct_;
}
template <> inline bool decl_is<EnumVariantDecl>(const DeclKind kind) {
    return kind == DeclKind::enum_variant;
}
template <> inline bool decl_is<EnumDecl>(const DeclKind kind) {
    return kind == DeclKind::enum_;
}
template <> inline bool decl_is<ExternDecl>(const DeclKind kind) {
    return kind == DeclKind::extern_;
}
template <> inline bool decl_is<BadDecl>(const DeclKind kind) {
    return kind == DeclKind::bad;
}

// A declaration node.
//
// All Decl derived types has a pointer field called 'name'. The value of this
// pointer serves as a unique integer ID used as the key the symbol table.
class Decl : public AstNode {
public:
    const DeclKind kind;

    Decl(DeclKind d) : AstNode(AstKind::decl), kind(d) {}

    template <typename T> bool is() const { return decl_is<T>(kind); }

    std::optional<Type *> typemaybe() const;

    Name *name() const;
};

enum class VarDeclKind {
    local,
    struct_,
    param,
};

// Variable declaration.
struct VarDecl : public Decl {
    Name *name = nullptr;
    Type *type = nullptr;

    // Whether this VarDecl has been declared as a local variable, as a field
    // inside a struct, or as a parameter for a function.
    // TODO: use separate types for each, e.g. FieldDecl.
    const VarDeclKind kind = VarDeclKind::local;

    // TypeExpr of the variable.  Declared as Expr to accommodate for BadExpr.
    // TODO: Ugly.
    Expr *type_expr = nullptr;

    // Assignment expression specified at the point of declaration, if any.
    Expr *assign_expr = nullptr;

    // Mutability of the variable.
    bool mut = false;

    // Whether this variable has been moved out.
    bool moved = false;

    // Whether this variable is a function-local variable.
    bool local = false;

    // Whether this variable has been borrowed.  Used for borrow checking.
    // TODO: Deprecate in favor of borrow_table.
    bool borrowed = false;

    // Lifetime of this variable.
    Lifetime *lifetime = nullptr;

    // [References] Lifetime of the value that this reference borrowed from.
    Lifetime *borrowee_lifetime = nullptr;

    // Decls for each of the values that are associated to this value.
    // For example, if this value is a struct type, these would be VarDecls for
    // each of its field.  The values are keyed using Names and are subject
    // under linear search.
    //
    // Note that these are different from the 'fields' field of StructDecl:
    // while the latter is simply a description of the struct declaration, the
    // former corresponds to the singular memory entity that each field of this
    // particular struct instance symbolizes.
    std::vector<std::pair<Name *, VarDecl *>> children;
    VarDecl *parent = nullptr;

    VarDecl(Name *n, VarDeclKind k, Expr *t, Expr *expr)
        : Decl(DeclKind::var), name(n), kind(k), type_expr(t),
          assign_expr(expr) {}
    VarDecl(Name *n, Type *t, bool m)
        : Decl(DeclKind::var), name(n), type(t), mut(m) {}
};

// Function declaration.  There is no separate function definition: functions
// should always be defined whenever they are declared.
struct FuncDecl : public Decl {
    Name *name = nullptr;         // name of the function
    Type *rettype = nullptr;      // return type of the function
    std::vector<VarDecl *> args;  // list of parameters
    CompoundStmt *body = nullptr; // body statements
    Expr *rettypeexpr = nullptr;  // return type expression
    Name *ret_lifetime_annot =
        nullptr; // lifetime annotation of the return value

    // "Bogus" lifetime that represents the scope of the function body.
    Lifetime *scope_lifetime = nullptr;

    FuncDecl(Name *n) : Decl(DeclKind::func), name(n) {}
    size_t args_count() const { return args.size(); }
};

// Struct declaration.
struct StructDecl : public Decl {
    Name *name = nullptr;          // name of the struct
    Type *type = nullptr;          // type of the struct
    std::vector<VarDecl *> fields; // member variables

    StructDecl(Name *n, std::vector<VarDecl *> m)
        : Decl(DeclKind::struct_), name(n), fields(m) {}
};

// A variant type in an enum.
struct EnumVariantDecl : public Decl {
    Name *name = nullptr;       // name of the variant
    Type *type = nullptr;       // type of the variant
    std::vector<Expr *> fields; // type of the fields

    EnumVariantDecl(Name *n, std::vector<Expr *> f)
        : Decl(DeclKind::enum_variant), name(n), fields(f) {}
};

// Enum declaration.
struct EnumDecl : public Decl {
    Name *name = nullptr;                    // name of the enum
    Type *type = nullptr;                    // type of the enum
    std::vector<EnumVariantDecl *> variants; // variants

    EnumDecl(Name *n, std::vector<EnumVariantDecl *> m)
        : Decl(DeclKind::enum_), name(n), variants(m) {}
};

// Extern declaration.
struct ExternDecl : public Decl {
    Decl *decl;

    ExternDecl(Decl *d) : Decl(DeclKind::extern_), decl(d) {}
};

struct BadDecl : public Decl {
    BadDecl() : Decl(DeclKind::bad) {}
};

} // namespace cmp

#endif
