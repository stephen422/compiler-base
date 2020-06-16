#ifndef SEMA_H
#define SEMA_H

#include "ast_visitor.h"
#include "error.h"
#include "fmt/core.h"

namespace cmp {

inline void unreachable() { assert(false && "unreachable"); }

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
  Symbol *find(const Name *name) const;
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

// Maps a VarDecl to its borrow count in the current scope.
// To be stored inside a ScopedTable.
struct BorrowMap {
    VarDecl *decl = nullptr;
    // number of occasions that this variable was borrowed
    int borrow_count = 0;
};

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

class Parser;

// Stores all of the semantic information necessary for semantic analysis
// phase.
struct Sema {
  const Source &source;  // source text
  NameTable name_table; // name table

  // Memory pools.  Currently maintains simply a list of malloc()ed pointers for
  // batch freeing.
  std::vector<std::unique_ptr<AstNode>> node_pool;
  std::vector<Type *> type_pool;
  std::vector<BasicBlock *> basic_block_pool;

  // Declarations visible at the current scope.
  ScopedTable<Decl *> decl_table;
  // XXX: needed?
  ScopedTable<Type *> type_table;
  // Live variables at the current scope.
  ScopedTable<VarDecl *> live_list;
  // TODO: doc
  ScopedTable<BorrowMap> borrow_table;

  // TODO.
  Context context;

  // List of generated errors.
  std::vector<Error> &errors;
  // List of error beacons found in the source text.
  std::vector<Error> &beacons;

  Sema(const Source &s, std::vector<Error> &e, std::vector<Error> &b)
      : source(s), errors(e), beacons(b) {}
  Sema(const Sema &) = delete;
  Sema(Sema &&) = delete;
  ~Sema();

  void scope_open();
  void scope_close();

  void error(size_t pos, const char *fmt, ...);

  template <typename T, typename... Args> T *make_node(Args &&... args) {
    node_pool.emplace_back(new T{std::forward<Args>(args)...});
    return static_cast<T *>(node_pool.back().get());
  }
  template <typename T, typename... Args>
  T *make_node_pos(size_t pos, Args &&... args) {
    auto node = make_node<T>(std::forward<Args>(args)...);
    node->pos = pos;
    return node;
  }
  BasicBlock *makeBasicBlock() {
    BasicBlock *bb = new BasicBlock;
    basic_block_pool.push_back(bb);
    return bb;
  }
};

void setup_builtin_types(Sema &s);

// Name binding pass.
// Name binding basically is a pass that simply links each Name to a Decl.
// It handles variable/function/struct declaration, redefinition/undeclared-uses
// checks, number of function arguments checks, etc.
// TODO: doc more.
class NameBinding : public AstVisitor<NameBinding> {
  Sema &sema;

public:
  NameBinding(Sema &s) : sema{s} {}
  bool success() const { return sema.errors.empty(); }

  void visitCompoundStmt(CompoundStmt *cs);
  void visitAssignStmt(AssignStmt *as);
  void visitDeclRefExpr(DeclRefExpr *d);
  void visitFuncCallExpr(FuncCallExpr *f);
  void visitTypeExpr(TypeExpr *t);
  void visitVarDecl(VarDecl *v);
  void visitFuncDecl(FuncDecl *f);
  void visitStructDecl(StructDecl *s);
  void visitEnumVariantDecl(EnumVariantDecl *e);
  void visitEnumDecl(EnumDecl *e);
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

  Type *visitVarDecl(VarDecl *v);
  Type *visitFuncDecl(FuncDecl *f);
  Type *visitStructDecl(StructDecl *s);
  Type *visitEnumVariantDecl(EnumVariantDecl *e);
  Type *visitEnumDecl(EnumDecl *e);
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

  BasicBlock *visitFuncDecl(FuncDecl *f, BasicBlock *bb);
};

class BorrowChecker : public AstVisitor<BorrowChecker, void> {
  Sema &sema;

  // Shows whether the current expression being visited is inside a return
  // statement, so that the borrow checker knows to check if this expression
  // borrows from a value declared in the local scope.
  bool in_return_stmt;

public:
  BorrowChecker(Sema &s) : sema{s} {}
  bool success() const { return sema.errors.empty(); }

  void visitCompoundStmt(CompoundStmt *cs);
  void visitAssignStmt(AssignStmt *as);
  void visitReturnStmt(ReturnStmt *rs);

  void visitDeclRefExpr(DeclRefExpr *d);
  void visitFuncCallExpr(FuncCallExpr *f);
  void visitStructDefExpr(StructDefExpr *s);
  void visitUnaryExpr(UnaryExpr *u);

  void visitVarDecl(VarDecl *v);
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

  void visitVarDecl(VarDecl *v);
  void visitFuncDecl(FuncDecl *f);
  void visitStructDecl(StructDecl *s);
};

} // namespace cmp

#endif
