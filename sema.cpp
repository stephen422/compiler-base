#include "sema.h"
#include <iostream>

namespace cmp {

// ref: https://stackoverflow.com/a/12996028
static uint64_t hash(const void *p) {
    uint64_t x = (uint64_t)p;
    x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
    x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
    x = x ^ (x >> 31);
    return x;
}

SymbolTable::SymbolTable() {
    for (int i = 0; i < symbol_table_key_size; i++) {
        keys[i] = nullptr;
    }
}

SymbolTable::~SymbolTable() {
    for (int i = 0; i < symbol_table_key_size; i++) {
        Symbol *p = keys[i];
        if (!p) {
            continue;
        }
        while (p) {
            Symbol *next = p->next;
            delete p;
            p = next;
        }
    }
}

void SymbolTable::push(const Symbol sym) {
    int index = hash(sym.name) % symbol_table_key_size;
    Symbol **p = &keys[index];
    Symbol *head = new Symbol(sym);
    head->next = *p;
    *p = head;
}

Declaration *SymbolTable::find(Name *name) const {
    int index = hash(name) % symbol_table_key_size;
    for (Symbol *s = keys[index]; s; s = s->next) {
        if (s->name == name) {
            return &s->decl;
        }
    }
    return nullptr;
}

void SymbolTable::print() const {
    std::cout << "==== Symbol table ====\n";
    for (int i = 0; i < symbol_table_key_size; i++) {
        Symbol *p = keys[i];
        if (!p) {
            continue;
        }
        std::cout << "[" << std::dec << i << "]";
        while (p) {
            std::cout << " {" << p->name->text << "}";
            p = p->next;
        }
        std::cout << std::endl;
    }
}

} // namespace cmp
