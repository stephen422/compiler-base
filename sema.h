#ifndef SEMA_H
#define SEMA_H

#include "ast.h"
#include "error.h"
#include "fmt/core.h"
#include <assert.h>
#include <variant>
#include <vector>

namespace cmp {

// Declaration of a variable. Includes struct field variables.
struct VarDecl {
    Name *name = nullptr;
    Type *type = nullptr;
    // Decl of the var that this var references to.  Used for borrow checking.
    VarDecl *borrowee = nullptr;
    // number of occasions that this variable was borrowed
    int borrow_count = 0;

    VarDecl(Name *n) : name(n) {}
};

// Declaration of a type, e.g. struct or enum.
struct StructDecl {
    Name *name = nullptr;
    Type *type = nullptr;
    std::vector<VarDecl *> fields;

    StructDecl(Name *n) : name(n) {}
};

// Declaration of an enum.
struct EnumDecl {
    Name *name = nullptr;
    Type *type = nullptr;
    std::vector<StructDecl *> variants;

    EnumDecl(Name *n) : name(n) {}
};

// Declaration of a function.
struct FuncDecl {
    Name *name = nullptr;
    Type *ret_ty = nullptr;
    std::vector<VarDecl *> args;

    FuncDecl(Name *n) : name(n) {}
    size_t args_count() const { return args.size(); }
    // XXX: might false-report before typecheck is completed
    bool is_void(Sema &sema) const;
};

// 'Decl' represents declaration of a variable, a function, or a type.
// All Decls are stored in a global pool.  Scoped Decl tables act on the
// references of these pooled Decls to determine undeclared-use or
// redefinition.
// TODO: Clarify the definition. Should type names have a Decl too?  What is
// the 'type' member of a StructDecl?
using Decl = std::variant<VarDecl *, StructDecl *, EnumDecl *, FuncDecl *>;
using DeclMemBlock = std::variant<VarDecl, StructDecl, EnumDecl, FuncDecl>;

template <typename T> bool decl_is(const Decl decl) {
    return std::holds_alternative<T *>(decl);
}
template <typename T> T *decl_as(const Decl decl) {
    if (std::holds_alternative<T *>(decl))
        return std::get<T *>(decl);
    else
        return nullptr;
}
std::optional<Type *> decl_get_type(const Decl decl);

// 'Type' represents a type, whether it be built-in, user-defined, or a
// reference to another type.  It exists separately from the AST node TypeExpr.
// Similar to Names, Types are designed to be comparable by simply comparing
// the value of raw Type pointers.
//
// TODO: switch to union/variant?
enum class TypeKind {
    value, // built-in, struct
    ref,
    array,
};

struct Type {
    TypeKind kind;
    // Name of the type. TODO: include & or [] in the name?
    Name *name = nullptr;
    // Is this a builtin type?
    bool builtin = false;
    union {
        // Back-reference to the decl object that defines this value type.
        Decl type_decl{};
        // The type that this type refers to.  If it is a non-reference type,
        // this should be null.
        Type *base_type;
    };

    Type(Name *n) : kind(TypeKind::value), name(n), builtin(true) {}
    Type(TypeKind k, Name *n, Type *bt) : kind(k), name(n), base_type(bt) {}
    Type(TypeKind k, Name *n, Decl td) : kind(k), name(n), type_decl(td) {}

    const char *str() const { return name->str(); }

    bool is_struct() const {
        // TODO: should base_type be null too?
        return kind == TypeKind::value &&
               std::holds_alternative<StructDecl *>(type_decl);
    }
    bool is_enum() const {
        // TODO: should base_type be null too?
        return kind == TypeKind::value &&
               std::holds_alternative<EnumDecl *>(type_decl);
    }
    bool is_member_accessible() const { return is_struct() || is_enum(); }

    StructDecl *get_struct_decl() {
        assert(kind == TypeKind::value);
        assert(std::holds_alternative<StructDecl *>(type_decl));
        return std::get<StructDecl *>(type_decl);
    }
    EnumDecl *get_enum_decl() {
        assert(kind == TypeKind::value);
        assert(std::holds_alternative<EnumDecl *>(type_decl));
        return std::get<EnumDecl *>(type_decl);
    }
};

struct Context {
    // Current enclosing decls.
    std::vector<FuncDecl *> func_decl_stack;
    std::vector<StructDecl *> struct_decl_stack;
    std::vector<EnumDecl *> enum_decl_stack;
    // Builtin types.
    // voidType exists to differentiate the type of FuncCallExprs whose
    // function no return values, from expressions that failed to typecheck.
    Type *void_type = nullptr;
    Type *int_type = nullptr;
    Type *char_type = nullptr;
    Type *string_type = nullptr;
};

// Scoped symbol table.
constexpr int SYMBOL_TABLE_BUCKET_COUNT = 512;
template <typename T> struct ScopedTable {
    struct Symbol {
        Name *name;              // name of this symbol
        T value;                 // semantic value of this symbol
        Symbol *next = nullptr;  // next symbol in the hash table bucket
        Symbol *cross = nullptr; // next symbol in the same scope
        int scope_level = 0;

        Symbol(Name *n, const T &v) : name(n), value(v) {}
    };

    ScopedTable();
    ~ScopedTable();
    T *insert(Name *name, const T &value);
    Symbol *find(Name *name) const;
    void print() const;

    // Start a new scope.
    void scope_open();
    // Close current cope.
    void scope_close();

    std::array<Symbol *, SYMBOL_TABLE_BUCKET_COUNT> keys;
    std::vector<Symbol *> scope_stack = {};
    int curr_scope_level = 0;
};

#include "scoped_table.h"

class Parser;

struct BasicBlock {
  std::vector<Stmt *> stmts;
  std::vector<BasicBlock *> pred;
  std::vector<BasicBlock *> succ;
  bool walked = false;

  // Indicates whether it is guaranteed that a return statement is seen on
  // every possible control flow that leads to this basic block.
  bool returned_so_far = false;

  // True if this basic block contains a return statement.
  bool returns() const;

  // Walk and enumerate all children nodes and itself in post-order.
  // Used to implement the the reverse post-order traversal.
  void enumerate_post_order(std::vector<BasicBlock *> &walkList);
};

// Stores all of the semantic information necessary for semantic analysis
// phase.
struct Sema {
    const Source &source;  // source text
    NameTable &name_table; // name table

    // Memory pointer pools.
    std::vector<DeclMemBlock *> decl_pool;
    std::vector<Type *> type_pool;
    std::vector<BasicBlock *> basic_block_pool;

    // Declarations visible at the current scope.
    ScopedTable<Decl> decl_table;
    // XXX: needed?
    ScopedTable<Type *> type_table;
    // Live variables at the current scope.
    ScopedTable<VarDecl *> live_list;

    Context context;
    std::vector<Error> &errors;  // error list
    std::vector<Error> &beacons; // error beacon list

    Sema(const Source &s, NameTable &n, std::vector<Error> &es,
         std::vector<Error> &bs)
        : source(s), name_table(n), errors(es), beacons(bs) {}
    Sema(Parser &p);
    Sema(const Sema &) = delete;
    Sema(Sema &&) = delete;
    ~Sema();

    void scope_open();
    void scope_close();

    void error(size_t pos, const char *fmt, ...);

    // Allocator function for Decls and Types.
    template <typename T> T *make_decl(Name *name) {
        DeclMemBlock *mem_block = new DeclMemBlock(T{name});
        auto typed_decl = &std::get<T>(*mem_block);
        typed_decl->name = name;
        decl_pool.push_back(mem_block);
        return typed_decl;
    }
    BasicBlock *make_basic_block() {
        BasicBlock *bb = new BasicBlock;
        basic_block_pool.push_back(bb);
        return bb;
    }
};

void setup_builtin_types(Sema &s);

// Get a reference type of a given type.
// Constructs the type if it wasn't in the type table beforehand.
Type *getReferenceType(Sema &sema, Type *type);

struct Ast;
struct AstNode;

void walkAST(Sema &sema, AstNode *node, bool (*pre_fn)(Sema &sema, AstNode *),
              bool (*post_fn)(Sema &sema, AstNode *));

template <typename T> T *declare(Sema &sema, Name *name, size_t pos);

// Name binding pass.
// Name binding basically is a pass that simply links each Name to a Decl.
// It handles variable/function/struct declaration, redefinition/undeclared-uses
// checks, number of function arguments checks, etc.
// TODO: doc more.
class NameBinder : public AstVisitor<NameBinder> {
  Sema &sema;

public:
  NameBinder(Sema &s) : sema{s} {}
  bool success() const { return sema.errors.empty(); }

  void visitCompoundStmt(CompoundStmt *cs);
  void visitDeclRefExpr(DeclRefExpr *d);
  void visitFuncCallExpr(FuncCallExpr *f);
  void visitTypeExpr(TypeExpr *t);
  void visitVarDecl(VarDeclNode *v);
  void visitFuncDecl(FuncDeclNode *f);
  void visitStructDecl(StructDeclNode *s);
  void visitEnumVariantDecl(EnumVariantDeclNode *e);
  void visitEnumDecl(EnumDeclNode *e);
};

// Type checking pass.
class TypeChecker : public AstVisitor<TypeChecker, Type *> {
  Sema &sema;

public:
  TypeChecker(Sema &s) : sema{s} {}
  bool success() const { return sema.errors.empty(); }

  Type *visitAssignStmt(AssignStmt *as);
  Type *visitReturnStmt(ReturnStmt *rs);

  Type *visitIntegerLiteral(IntegerLiteral *i);
  Type *visitStringLiteral(StringLiteral *s);
  Type *visitDeclRefExpr(DeclRefExpr *d);
  Type *visitFuncCallExpr(FuncCallExpr *f);
  Type *visitStructDefExpr(StructDefExpr *s);
  Type *visitMemberExpr(MemberExpr *m);
  Type *visitUnaryExpr(UnaryExpr *u);
  Type *visitParenExpr(ParenExpr *p);
  Type *visitBinaryExpr(BinaryExpr *b);
  Type *visitTypeExpr(TypeExpr *t);

  Type *visitVarDecl(VarDeclNode *v);
  Type *visitFuncDecl(FuncDeclNode *f);
  Type *visitStructDecl(StructDeclNode *s);
  Type *visitEnumVariantDecl(EnumVariantDeclNode *e);
  Type *visitEnumDecl(EnumDeclNode *e);
};

class ReturnChecker
    : public AstVisitor<ReturnChecker, BasicBlock *, BasicBlock *> {
  Sema &sema;

public:
  ReturnChecker(Sema &s) : sema{s} {}
  bool success() const { return sema.errors.empty(); }

  BasicBlock *visitStmt(Stmt *s, BasicBlock *bb);
  BasicBlock *visitCompoundStmt(CompoundStmt *cs, BasicBlock *bb);
  BasicBlock *visitIfStmt(IfStmt *is, BasicBlock *bb);

  BasicBlock *visitFuncDecl(FuncDeclNode *f, BasicBlock *bb);
};

class BorrowChecker : public AstVisitor<BorrowChecker, void> {
  Sema &sema;

public:
  BorrowChecker(Sema &s) : sema{s} {}
  bool success() const { return sema.errors.empty(); }

  void visitCompoundStmt(CompoundStmt *cs);
  void visitAssignStmt(AssignStmt *as);

  void visitDeclRefExpr(DeclRefExpr *d);

  void visitVarDecl(VarDeclNode *v);
};

class CodeGenerator : public AstVisitor<CodeGenerator, void> {
  Sema &sema;
  int indent = 0;
  FILE *file = nullptr;

  template <typename... Args> void emit(Args &&... args) {
    fmt::print(file, "{:{}}", "", indent);
    fmt::print(file, std::forward<Args>(args)...);
  }
  template <typename... Args> void emitCont(Args &&... args) {
    fmt::print(file, std::forward<Args>(args)...);
  }
  void emitIndent() { fmt::print(file, "{:{}}", "", indent); };

  struct IndentBlock {
    CodeGenerator &c;
    IndentBlock(CodeGenerator &c) : c{c} { c.indent += 2; }
    ~IndentBlock() { c.indent -= 2; }
  };

  std::string cStringify(const Type *t);

public:
  CodeGenerator(Sema &s) : sema{s} { file = fopen("test_codegen.c", "w"); }
  ~CodeGenerator() { fclose(file); }
  bool success() const { return sema.errors.empty(); }

  void visitFile(File *f);

  void visitIntegerLiteral(IntegerLiteral *i);
  void visitStringLiteral(StringLiteral *s);
  void visitDeclRefExpr(DeclRefExpr *d);
  void visitFuncCallExpr(FuncCallExpr *f);
  void visitStructDefExpr(StructDefExpr *s);
  void visitMemberExpr(MemberExpr *m);
  void visitUnaryExpr(UnaryExpr *u);
  void visitParenExpr(ParenExpr *p);
  void visitBinaryExpr(BinaryExpr *b);

  void visitExprStmt(ExprStmt *e);
  void visitAssignStmt(AssignStmt *a);
  void visitReturnStmt(ReturnStmt *r);
  void visitIfStmt(IfStmt *i);
  void visitBuiltinStmt(BuiltinStmt *b);

  void visitVarDecl(VarDeclNode *v);
  void visitFuncDecl(FuncDeclNode *f);
  void visitStructDecl(StructDeclNode *s);
};

} // namespace cmp

#endif
