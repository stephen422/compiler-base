#ifndef SEMA_H
#define SEMA_H

#include "error.h"
#include "fmt/core.h"
#include "fmt/format.h"
#include <variant>
#include <vector>
#include <assert.h>

namespace cmp {

class Source;

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

struct Sema;

// 'Type' represents a type, whether it be built-in, user-defined, or a
// reference to another type.  Type exists separately from the AST node
// TypeExpr so that type comparisons can be made by simply comparing raw Type
// pointers.
//
// TODO: switch to union?
struct Type {
    enum class Kind { value, ref, array } kind;
    Name *name = nullptr;       // name of this type
    Type *target_type = nullptr; // the type this reference refers to

    Type(Name *n) : kind(Kind::value), name(n) {}
    Type(Kind k, Name *n, Type *v) : kind(k), name(n), target_type(v) {}
    std::string str() const;
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
};

// 'Decl' represents declaration of a variable, a function, or a type.
// All Decls are stored in a global pool.  Scoped Decl tables act on the
// references of these pooled Decls to determine undeclared-use or
// redefinition.
// TODO: Clarify the definition. Should type names have a Decl too?  What is
// the 'type' member of a TypeDecl?
// using Decl = std::variant<VarDecl, StructDecl>;
enum DeclKind { DECL_VAR, DECL_STRUCT, DECL_FUNC };
struct Decl {
    DeclKind kind;
    union {
        VarDecl var_decl;
        StructDecl struct_decl;
        FuncDecl func_decl;
    };

    Decl(const VarDecl &v) : kind(DECL_VAR), var_decl(v) {}
    Decl(const StructDecl &s) : kind(DECL_STRUCT), struct_decl(s) {}
    Decl(const FuncDecl &f) : kind(DECL_FUNC), func_decl(f) {}
    ~Decl() {
        switch (kind) {
        case DECL_VAR:
            var_decl.~VarDecl();
            break;
        case DECL_STRUCT:
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
};

// Convenience cast function. Note that this works on a *pointer* to the Decl.
// template <typename T> T *decl_cast(Decl *d) { return &std::get<T>(*d); }
// Allocator function.
Decl *make_decl(Sema *sema, const VarDecl &var_decl);
Decl *make_decl(Sema *sema, const StructDecl &struct_decl);
Decl *make_decl(Sema *sema, const FuncDecl &func_decl);

struct Context {
    std::vector<StructDecl *> struct_decl_stack; // current enclosing struct decl
    // Return type of this function
    Type *ret_type = nullptr;
    // Seen one or more return statement in this function
    bool seen_return = false;
};

// Scoped symbol table.
const int SYMBOL_TABLE_BUCKET_COUNT = 512;
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
    ScopedTable<Decl *> decl_table; // scoped declaration table
    ScopedTable<Type> type_table;   // scoped type table
    Context context;
    std::vector<Error> errors;  // error list
    std::vector<Error> beacons; // error beacon list
    Type *int_type = nullptr;
    Type *char_type = nullptr;

    Sema(const Source &src_, NameTable &nt);
    Sema(Parser &p);
    Sema(const Sema &) = delete;
    Sema(Sema &&) = delete;
    ~Sema();
    void error(size_t pos, const std::string &msg);
    void scope_open();
    void scope_close();
    void report() const;
    bool verify() const;
};

// Get a reference type of a given type.
// Constructs the type if it wasn't in the type table beforehand.
Type *getReferenceType(Sema &sema, Type *type);

struct Ast;
struct AstNode;

// Do a semantic analysis on the given AST.
void walk_ast(Sema *sema, AstNode *node, void (*pre_fn)(Sema *sema, AstNode *),
              void (*post_fn)(Sema *sema, AstNode *));
void sema(Sema &sema, Ast &ast);

} // namespace cmp

// template <> struct fmt::formatter<std::variant<int, double>> {
//     constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }
// 
//     template <typename FormatContext>
//     auto format(const std::variant<int, double> &d, FormatContext &ctx) {
//         return format_to(ctx.out(), "this is a decl");
//     }
// };

#endif
