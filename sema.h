#ifndef SEMA_H
#define SEMA_H

#include <map>

namespace cmp {

// A Name corresponds to any single unique identifier string in the source
// text.  There may be multiple occurrences of the string in the source text,
// but one instance of the matching Name exists in the course of compilation.
// A NameTable is a hash table of Names queried by their raw string.  It serves
// to reduce the number of string hashing operation, since we can look up the
// symbol table using Name instead of raw char * throughout the semantic
// analysis.
class Name {
public:
    Name(const std::string &s) : text(s) {}
    std::string text;
};


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

} // namespace cmp

#endif
