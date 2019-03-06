#ifndef SEMA_H
#define SEMA_H

#include <array>
#include <iostream>
#include <unordered_map>

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
    Name *find_or_insert(const std::string &s) {
        Name *found = find(s);
        if (found) {
            return found;
        }
        auto pair = map.insert({s, {s}});
        return &pair.first->second;
    }
    Name *find(const std::string &s) {
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
// another type.  Types are usually stored and passed by value, so their size
// should be small.
class Type {
public:
    Type() {}
    Type(Name *n) : name(n) {}
    Type(Name *n, Type *v, bool r) : name(n), value_type(v), ref(r) {}
    std::string to_string() const;

    Name *name = nullptr;       // name of this type
    Type *value_type = nullptr; // the type this reference refers to
    bool ref = false;           // is this a reference type?

    Type *get_reference();
};

// Represents declaration of a variable or a function.
class Declaration {
public:
    std::string to_string() const;

    Name *name = nullptr;
    Type &type;
};

template <typename T>
class Symbol {
public:
    Symbol(Name *n, const T &v) : name(n), value(v) {}

    Name *name;   // name of this symbol
    T value;      // semantic value of this symbol
    Symbol *next; // pointer to next symbol in the hash table bucket
};

constexpr int symbol_table_key_size = 512;

template<typename T>
class SymbolTable {
public:
    SymbolTable();
    ~SymbolTable();
    T *insert(std::pair<Name *, const T &> pair);
    T *find(Name *name) const;
    void print() const;

private:
    std::array<Symbol<T> *, symbol_table_key_size> keys;
};

#include "symbol_table.cpp"

// Stores all of the semantic information necessary for semantic analysis
// phase.
class Semantics {
public:
    Semantics(Source &src_, NameTable &nt) : src(src_), name_table(nt) {}
    void error(size_t pos, const std::string &msg);

    Type *int_type = nullptr;
    Type *i64_type = nullptr;
    Source &src;                         // source text
    NameTable &name_table;               // name table
    SymbolTable<Declaration> decl_table; // declaration table
    SymbolTable<Type> type_table;        // type table
};

class Ast;

// Do a semantic analysis on the given AST.
void semantic_analyze(Semantics &sema, Ast &ast);

} // namespace cmp

#endif
