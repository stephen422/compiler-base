#ifndef SEMA_H
#define SEMA_H

#include "ast.h"
#include <unordered_map>

namespace cmp {

class Source;

// Represents a type, whether it be built-in or user-defined.
class Type {
public:
    Name *name;
};

class Declaration {
public:
    Name &id;
    Type &type;
};

template <typename T>
class Symbol {
public:
    Symbol(Name *n, T &v) : name(n), value(v) {}

    Name *name;   // name of this symbol
    T &value;     // semantic value of this symbol
    Symbol *next; // pointer to next symbol in the hash table bucket
};

constexpr int symbol_table_key_size = 512;

template<typename T>
class SymbolTable {
public:
    SymbolTable();
    ~SymbolTable();
    void insert(const Symbol<T> sym);
    T *find(Name *name) const;
    void print() const;

private:
    std::array<Symbol<T> *, symbol_table_key_size> keys;
};

// ref: https://stackoverflow.com/a/12996028
static inline uint64_t hash(const void *p) {
    uint64_t x = (uint64_t)p;
    x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
    x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
    x = x ^ (x >> 31);
    return x;
}

template <typename T>
SymbolTable<T>::SymbolTable() {
    for (int i = 0; i < symbol_table_key_size; i++) {
        keys[i] = nullptr;
    }
}

template <typename T>
SymbolTable<T>::~SymbolTable() {
    for (int i = 0; i < symbol_table_key_size; i++) {
        auto *p = keys[i];
        if (!p) {
            continue;
        }
        while (p) {
            auto *next = p->next;
            delete p;
            p = next;
        }
    }
}

template <typename T>
void SymbolTable<T>::insert(const Symbol<T> sym) {
    int index = hash(sym.name) % symbol_table_key_size;
    auto **p = &keys[index];
    auto *head = new Symbol<T>(sym);
    head->next = *p;
    *p = head;
}

template <typename T>
T *SymbolTable<T>::find(Name *name) const {
    int index = hash(name) % symbol_table_key_size;
    for (auto *s = keys[index]; s; s = s->next) {
        if (s->name == name) {
            return &s->value;
        }
    }
    return nullptr;
}

template <typename T>
void SymbolTable<T>::print() const {
    std::cout << "==== Symbol table ====\n";
    for (int i = 0; i < symbol_table_key_size; i++) {
        auto *p = keys[i];
        if (!p) {
            continue;
        }
        std::cout << "[" << i << "]";
        while (p) {
            std::cout << " {" << p->name->text << "}";
            p = p->next;
        }
        std::cout << std::endl;
    }
}

// Stores all of the semantic information necessary for semantic analysis
// phase.
class Semantics {
public:
    Semantics(Source &src_, NameTable &nt) : src(src_), name_table(nt) {}
    void error(size_t pos, const std::string &msg);

    Source &src;                                 // source text
    NameTable &name_table;                       // name table
    SymbolTable<Declaration> decl_table;                      // declaration table
    SymbolTable<Type> type_table; // type table
};

// Do a semantic analysis on the given AST.
void semantic_analyze(Semantics &sema, Ast ast);

} // namespace cmp

#endif
