#ifndef SEMA_H
#define SEMA_H

#include "ast.h"
#include <unordered_map>

namespace cmp {

class Source;

class Declaration {
public:
    Name &id;
};

class Symbol {
public:
    Symbol(Name *name, Declaration &decl) : name(name), decl(decl){};

    Name *name;
    Declaration &decl;
    Symbol *next;
};

constexpr int symbol_table_key_size = 512;

class SymbolTable {
public:
    SymbolTable();
    ~SymbolTable();
    void push(const Symbol sym);
    Declaration *find(Name *name) const;
    void print() const;

private:
    std::array<Symbol *, symbol_table_key_size> keys;
};

// Represents a type, whether it be built-in or user-defined.
class Type {
public:
    Name *name;
};

// Stores all of the semantic information necessary for semantic analysis
// phase.
class Semantics {
public:
    Semantics(Source &src_, NameTable &nt) : src(src_), name_table(nt) {}
    void error(size_t pos, const std::string &msg);

    Source &src;                                 // source text
    NameTable &name_table;                       // name table
    SymbolTable decl_table;                      // declaration table
    std::unordered_map<Name *, Type> type_table; // type table
};

// Do a semantic analysis on the given AST.
void semantic_analyze(Semantics &sema, Ast ast);

} // namespace cmp

#endif
