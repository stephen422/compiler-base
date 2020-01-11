#ifndef SEMA_H
#define SEMA_H

#include <array>
#include <iostream>
#include <unordered_map>
#include <vector>

namespace cmp {

class Source;

// A Name corresponds to a single unique identifier string in the source text.
// There may be multiple occurrences of a string in the source text, but only
// one instance of the matching Name can reside in the name table.
class Name {
public:
    Name(const std::string &s) : text(s) {}
    std::string text;
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

// Represents a type, whether it be built-in, user-defined, or a reference to
// another type.  This exists separately from the AST node TypeExpr so that
// type comparisons can be made by simply comparing raw Type pointers.
class Type {
public:
    Type() {}
    Type(Name *n) : name(n) {}
    Type(Name *n, Type *v, bool r) : name(n), value_type(v), ref(r) {}
    std::string to_string() const;

    Name *name = nullptr;       // name of this type
    Type *value_type = nullptr; // the type this reference refers to
    bool ref = false;           // is this a reference type?
    int scope_level = 0;        // scope that this was declared
};

// Represents declaration of a variable or a function.
class Declaration {
public:
    Declaration(Name *n, Type &t) : name(n), type(t) {}
    std::string to_string() const;

    Name *name = nullptr;
    Type &type;
    int scope_level = 0;        // scope that this was declared
};

class Context {
public:
    // Return type of this function
    Type *retType = nullptr;
    // Seen one or more return statement in this function
    bool seenReturn = false;
};

constexpr int symbol_table_key_size = 512;

template<typename T>
class ScopedTable {
public:
    ScopedTable();
    ~ScopedTable();
    T *insert(std::pair<Name *, const T &> pair);
    T *find(Name *name) const;
    void print() const;

    // Start a new scope.
    void scopeOpen();
    // Close current cope.
    void scopeClose();
    // Get current scope level.
    int get_scope_level() { return scope_level; }

private:
    class Symbol {
    public:
        Symbol(Name *n, const T &v) : name(n), value(v) {}

        Name *name; // name of this symbol
        T value;    // semantic value of this symbol
        Symbol *next =
            nullptr; // pointer to next symbol in the hash table bucket
        Symbol *cross = nullptr; // pointer to next symbol in the same scope
        int scope_level;
    };

    std::array<Symbol *, symbol_table_key_size> keys;
    std::vector<Symbol *> scope_stack = {};
    int scope_level = 0;
};

#include "scoped_table.h"

// Stores all of the semantic information necessary for semantic analysis
// phase.
class Semantics {
public:
    Semantics(Source &src_, NameTable &nt);
    Semantics(const Semantics &) = delete;
    void error(size_t pos, const std::string &msg);

    // Access functions for the statically allocated built-in type objects.
    Type *get_int_type() { return int_type; }
    Type *get_i64_type() { return i64_type; }

    void scopeOpen() {
        decl_table.scopeOpen();
        type_table.scopeOpen();
        context_table.push_back(Context{});
    }

    void scopeClose() {
        decl_table.scopeClose();
        type_table.scopeClose();
        context_table.pop_back();
    }

    Context &getContext() { return context_table.back(); }

    Source &src;                        // source text
    NameTable &names;                   // name table
    ScopedTable<Declaration> decl_table; // declaration table
    ScopedTable<Type> type_table;        // type table
    std::vector<Context> context_table;  // semantic analysis context table

private:
    Type *int_type = nullptr;
    Type *i64_type = nullptr;
};

// Get a reference type of a given type.
// Constructs the type if it wasn't in the type table beforehand.
Type *get_reference_type(Semantics &sema, Type *type);

struct Ast;

// Do a semantic analysis on the given AST.
void semantic_analyze(Semantics &sema, Ast &ast);

} // namespace cmp

#endif
