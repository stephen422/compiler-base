#ifndef SEMA_H
#define SEMA_H

#include "ast.h"
#include <unordered_map>

namespace cmp {

class Source;

// Represents a type, whether it be built-in or user-defined.
class Type {
public:
    Type(Name *n) : name(n) {}
    Name *name = nullptr;
    void print() const;
};

// Represents declaration of a variable or a function.
class Declaration {
public:
    Name *name = nullptr;
    Type &type;
    void print() const;
};

template <typename T>
class Symbol {
public:
    Symbol(Name *n, const T &v) : name(n), value(v) {}

    Name *name;   // name of this symbol
    T value;     // semantic value of this symbol
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

    Type *int_type = nullptr;            // integer type object
    Source &src;                         // source text
    NameTable &name_table;               // name table
    SymbolTable<Declaration> decl_table; // declaration table
    SymbolTable<Type> type_table;        // type table
};

// Do a semantic analysis on the given AST.
void semantic_analyze(Semantics &sema, Ast &ast);

} // namespace cmp

#endif
