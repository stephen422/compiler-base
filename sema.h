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

    // Built-in types.
    Type(Name *n) : Type(TypeKind::value, n, nullptr, nullptr) {}
    Type(TypeKind k, Name *n, Type *v, StructDecl *s)
        : kind(k), name(n), base_type(v), struct_decl(s) {}
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
};

// 'Decl' represents declaration of a variable, a function, or a type.
// All Decls are stored in a global pool.  Scoped Decl tables act on the
// references of these pooled Decls to determine undeclared-use or
// redefinition.
// TODO: Clarify the definition. Should type names have a Decl too?  What is
// the 'type' member of a StructDecl?
// using Decl = std::variant<VarDecl, StructDecl>;
enum DeclKind { DECL_VAR, DECL_TYPE, DECL_FUNC };
struct Decl {
    DeclKind kind;
    union {
        VarDecl var_decl;
        StructDecl struct_decl;
        FuncDecl func_decl;
    };

    Decl(const VarDecl &v) : kind(DECL_VAR), var_decl(v) {}
    Decl(const StructDecl &s) : kind(DECL_TYPE), struct_decl(s) {}
    Decl(const FuncDecl &f) : kind(DECL_FUNC), func_decl(f) {}
    ~Decl() {
        switch (kind) {
        case DECL_VAR:
            var_decl.~VarDecl();
            break;
        case DECL_TYPE:
            struct_decl.~StructDecl();
            break;
        case DECL_FUNC:
            func_decl.~FuncDecl();
            break;
        default:
            assert(false);
        }
    }
    std::string str() const;

    // 'Unsafe' getter functions. Conveniently return pointers so that the
    // convention of using '->' to access Decls is maintained.
    VarDecl *get_var_decl() {
        assert(kind == DECL_VAR);
        return &var_decl;
    }
    StructDecl *get_struct_decl() {
        assert(kind == DECL_TYPE);
        return &struct_decl;
    }
    FuncDecl *get_func_decl() {
        assert(kind == DECL_FUNC);
        return &func_decl;
    }
};

// Convenience cast function. Note that this works on a *pointer* to the Decl.
// template <typename T> T *decl_cast(Decl *d) { return &std::get<T>(*d); }

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

// Stores all of the semantic information necessary for semantic analysis
// phase.
struct Sema {
    const Source &source;           // source text
    NameTable &names;               // name table
    std::vector<Decl *> decl_pool;  // memory pool for decls
    std::vector<Type *> type_pool;  // memory pool for types
    ScopedTable<Decl *> decl_table; // scoped declaration table
    ScopedTable<Type> type_table;   // scoped type table
    Context context;
    std::vector<Error> errors;  // error list
    std::vector<Error> beacons; // error beacon list
    Type *int_type = nullptr;
    Type *char_type = nullptr;

    Sema(const Source &s, NameTable &n) : source(s), names(n) {}
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
    template <typename T> Decl *make_decl(const T &d) {
        Decl *decl = new Decl{d};
        decl_pool.push_back(decl);
        return decl;
    }
    template <typename... Args> Type *make_type(Args &&... args) {
        Type *t = new Type{std::forward<Args>(args)...};
        type_pool.push_back(t);
        return t;
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

// Name binding pass. Handles variable/function/struct declaration,
// redefinition/undeclared-use checks, number of function arguments checks,
// etc.
// Name binding pass can be thought of bascially linking a Name to a Decl.
// TODO: doc more.
class NameBinder : public AstVisitor<NameBinder> {
    Sema &sema;

public:
    NameBinder(Sema &s) : sema{s} {}

    void visit_compound_stmt(CompoundStmt *cs);
    void visit_decl_ref_expr(DeclRefExpr *d);
    void visit_func_call_expr(FuncCallExpr *f);
    void visit_type_expr(TypeExpr *t);
    void visit_var_decl(VarDeclNode *v);
    void visit_struct_decl(StructDeclNode *s);
    void visit_func_decl(FuncDeclNode *f);
};

// Type checking pass.
class TypeChecker : public AstVisitor<TypeChecker> {
    Sema &sema;

public:
    TypeChecker(Sema &s) : sema{s} {}

    void visit_assign_stmt(AssignStmt *as);
    void visit_integer_literal(IntegerLiteral *i);
    void visit_string_literal(StringLiteral *s);
    void visit_decl_ref_expr(DeclRefExpr *d);
    void visit_member_expr(MemberExpr *m);
    void visit_type_expr(TypeExpr *t);
    void visit_var_decl(VarDeclNode *v);
    void visit_struct_decl(StructDeclNode *s);
};

} // namespace cmp

#endif
