#ifndef SEMA_H
#define SEMA_H

#include "ast.h"
#include "error.h"
#include "fmt/core.h"
#include "fmt/format.h"
#include <assert.h>
#include <unordered_map>
#include <variant>
#include <vector>

namespace cmp {

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
    // The type that this type refers to.  If it is a non-reference type, this
    // should be null.
    Type *base_type = nullptr;
    // Back-reference to the StructDecl whose type is this object.
    StructDecl *struct_decl = nullptr;
    // Is this type an l-value type, i.e. can a variable of this type come to
    // the LHS of an assignment?
    bool isLValue = true;

    // Built-in types.
    Type(Name *n) : Type(TypeKind::value, n, nullptr, nullptr, true) {}
    Type(TypeKind k, Name *n, Type *bt, StructDecl *s, bool lval)
        : kind(k), name(n), base_type(bt), struct_decl(s), isLValue(lval) {}
    std::string str() const;

    bool is_struct() const {
        // TODO: should base_type be null too?
        return struct_decl != nullptr;
    }
};

// Declaration of a variable. Includes struct field variables.
struct VarDecl {
    Name *name = nullptr;
    Type *type = nullptr;

    VarDecl(Name *n) : name(n) {}
};

// Declaration of a struct.
struct StructDecl {
    Name *name = nullptr;
    Type *type = nullptr;
    std::vector<VarDecl *> fields;

    StructDecl(Name *n) : name(n) {}
};

// Declaration of a function.
struct FuncDecl {
    Name *name = nullptr;
    Type *return_type = nullptr;
    std::vector<VarDecl *> args;

    FuncDecl(Name *n) : name(n) {}
    size_t args_count() const { return args.size(); }
    // XXX: might false-report before typecheck is completed
    bool is_void() const { return return_type == nullptr; }
};

// 'Decl' represents declaration of a variable, a function, or a type.
// All Decls are stored in a global pool.  Scoped Decl tables act on the
// references of these pooled Decls to determine undeclared-use or
// redefinition.
// TODO: Clarify the definition. Should type names have a Decl too?  What is
// the 'type' member of a StructDecl?
using Decl = std::variant<VarDecl *, StructDecl *, FuncDecl *>;

using std::get;
template <typename T> bool decl_is(const Decl &decl) {
    return std::holds_alternative<T>(decl);
}

struct Context {
    // Current enclosing struct decl.
    std::vector<StructDecl *> struct_decl_stack;
    std::vector<FuncDecl *> func_decl_stack;
    // Return type of this function.
    Type *ret_type = nullptr;
    // Seen one or more return statement in this function.
    bool seen_return = false;
};

// Scoped symbol table.
constexpr int SYMBOL_TABLE_BUCKET_COUNT = 512;
template <typename T> struct ScopedTable {
    struct Symbol {
        Symbol(Name *n, const T &v) : name(n), value(v) {}

        Name *name;              // name of this symbol
        T value;                 // semantic value of this symbol
        Symbol *next = nullptr;  // next symbol in the hash table bucket
        Symbol *cross = nullptr; // next symbol in the same scope
        int scope_level = 0;
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
    int scope_level = 0;
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
    bool returnedSoFar = false;

    // True if this basic block contains a return statement.
    bool returns() const;

    // Walk and enumerate all children nodes and itself in post-order.
    // Used to implement the the reverse post-order traversal.
    void enumeratePostOrder(std::vector<BasicBlock *> &walkList);
};

// Stores all of the semantic information necessary for semantic analysis
// phase.
struct Sema {
    using DeclMemBlock = std::variant<VarDecl, StructDecl, FuncDecl>;

    const Source &source;                  // source text
    NameTable &names;                      // name table
    std::vector<DeclMemBlock *> decl_pool;
    std::vector<Type *> type_pool;
    std::vector<BasicBlock *> bb_pool;
    ScopedTable<Decl> decl_table;          // scoped declaration table
    ScopedTable<Type *> type_table;        // scoped type table
    Context context;
    std::vector<Error> &errors;  // error list
    std::vector<Error> &beacons; // error beacon list
    Type *int_type = nullptr;
    Type *char_type = nullptr;

    Sema(const Source &s, NameTable &n, std::vector<Error> &es,
         std::vector<Error> &bs)
        : source(s), names(n), errors(es), beacons(bs) {}
    Sema(Parser &p);
    Sema(const Sema &) = delete;
    Sema(Sema &&) = delete;
    ~Sema();
    void error(size_t pos, const std::string &msg);
    void scope_open();
    void scope_close();
    void report() const;
    bool verify() const;

    // Allocator function for Decls and Types.
    template <typename T, typename... Args> T *make_decl(T &&t) {
        DeclMemBlock *mem_block = new DeclMemBlock(t);
        decl_pool.push_back(mem_block);
        auto ptr = &std::get<T>(*decl_pool.back());
        return ptr;
    }
    template <typename... Args> Type *make_type(Args &&... args) {
        Type *t = new Type{std::forward<Args>(args)...};
        type_pool.push_back(t);
        return t;
    }
    BasicBlock *make_basic_block() {
        BasicBlock *bb = new BasicBlock;
        bb_pool.push_back(bb);
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

// Name binding pass.
// Name binding pass basically links a Name to a Decl.
// It handles variable/function/struct declaration, redefinition/undeclared-uses
// checks, number of function arguments checks, etc.
// TODO: doc more.
class NameBinder : public AstVisitor<NameBinder, void> {
    Sema &sema;

public:
    NameBinder(Sema &s) : sema{s} {}
    bool success() const { return sema.errors.empty(); }

    void visitCompoundStmt(CompoundStmt *cs);
    void visitDeclRefExpr(DeclRefExpr *d);
    void visitFuncCallExpr(FuncCallExpr *f);
    void visitTypeExpr(TypeExpr *t);
    void visitVarDecl(VarDeclNode *v);
    void visitStructDecl(StructDeclNode *s);
    void visitFuncDecl(FuncDeclNode *f);
};

// Type checking pass.
class TypeChecker : public AstVisitor<TypeChecker, void> {
    Sema &sema;

public:
    TypeChecker(Sema &s) : sema{s} {}
    bool success() const { return sema.errors.empty(); }

    void visitAssignStmt(AssignStmt *as);
    void visitReturnStmt(ReturnStmt *rs);

    void visitIntegerLiteral(IntegerLiteral *i);
    void visitStringLiteral(StringLiteral *s);
    void visitDeclRefExpr(DeclRefExpr *d);
    void visitFuncCallExpr(FuncCallExpr *f);
    void visitMemberExpr(MemberExpr *m);
    void visitUnaryExpr(UnaryExpr *u);
    void visitParenExpr(ParenExpr *p);
    void visitBinaryExpr(BinaryExpr *b);
    void visitTypeExpr(TypeExpr *t);

    void visitVarDecl(VarDeclNode *v);
    void visitStructDecl(StructDeclNode *s);
    void visitFuncDecl(FuncDeclNode *f);
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

class CodeGenerator : public AstVisitor<CodeGenerator, void> {
  Sema &sema;
  int indent = 0;

  template <typename... Args> void emit(Args &&... args) {
    fmt::print("{:{}}", "", indent);
    fmt::print(std::forward<Args>(args)...);
  }
  template <typename... Args> void emitCont(Args &&... args) {
    fmt::print(std::forward<Args>(args)...);
  }

  struct IndentBlock {
    CodeGenerator &c;
    IndentBlock(CodeGenerator &c) : c{c} { c.indent += 2; }
    ~IndentBlock() { c.indent -= 2; }
  };

public:
  CodeGenerator(Sema &s) : sema{s} {}
  bool success() const { return sema.errors.empty(); }

  void visitIntegerLiteral(IntegerLiteral *i);
  void visitTypeExpr(TypeExpr *t);
  void visitVarDecl(VarDeclNode *v);
  void visitFuncDecl(FuncDeclNode *f);
};

} // namespace cmp

#endif
