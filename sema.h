#ifndef SEMA_H
#define SEMA_H

#include "parser.h"
#include <array>
#include <iostream>
#include <unordered_map>
#include <vector>

namespace cmp {

class Source;

// Represents a type, whether it be built-in, user-defined, or a reference to
// another type.  Type exists separately from the AST node TypeExpr so that
// type comparisons can be made by simply comparing raw Type pointers.
//
// TODO: switch to union?
struct Type {
    enum class Kind { value, ref, array } kind;
    Name *name = nullptr;       // name of this type
    Type *target_type = nullptr; // the type this reference refers to

    Type(Name *n) : kind(Kind::value), name(n) {}
    Type(Kind k, Name *n, Type *v) : kind(k), name(n), target_type(v) {}
    std::string toString() const;
};

// Represents declaration of a variable or a function.
struct Declaration {
    Name *name = nullptr;
    Type *type = nullptr;

    std::string toString() const;
};

struct Context {
    // Return type of this function
    Type *retType = nullptr;
    // Seen one or more return statement in this function
    bool seenReturn = false;
};

constexpr int symbol_table_key_size = 512;

// Scoped symbol table.
template <typename T> class ScopedTable {
public:
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
    T *insert(std::pair<Name *, const T &> pair);
    Symbol *find(Name *name) const;
    void print() const;

    // Start a new scope.
    void scopeOpen();
    // Close current cope.
    void scopeClose();

    std::array<Symbol *, symbol_table_key_size> keys;
    std::vector<Symbol *> scope_stack = {};
    int scope_level = 0;
};

#include "scoped_table.h"

// Stores all of the semantic information necessary for semantic analysis
// phase.
struct Sema {
    const Source &source;                // source text
    NameTable &names;                    // name table
    ScopedTable<Declaration> decl_table; // declaration table
    ScopedTable<Type> type_table;        // type table
    std::vector<Context> context_table;  // semantic analysis context table
    std::vector<Error> errors;           // error list
    std::vector<Error> beacons;          // error beacon list
    Type *int_type = nullptr;
    Type *char_type = nullptr;

    Sema(const Source &src_, NameTable &nt);
    Sema(Parser &p);
    Sema(const Sema &) = delete;
    Sema(Sema &&) = delete;
    void error(size_t pos, const std::string &msg);
    void scope_open();
    void scope_close();
    Context &getContext() { return context_table.back(); }
    void report() const;
    bool verify() const;
};

// Get a reference type of a given type.
// Constructs the type if it wasn't in the type table beforehand.
Type *getReferenceType(Sema &sema, Type *type);

struct Ast;

// Do a semantic analysis on the given AST.
void sema(Sema &sema, Ast &ast);

} // namespace cmp

#endif
