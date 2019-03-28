#ifndef SEMA_H
#define SEMA_H

#include "parser.h"

#define HASHTABLE_SIZE 512

typedef struct {
} Func;

typedef struct {
    int mutable;
} Var;

typedef struct {
    int type;
    union {
        Var *var;
        Func *func;
    };
} Decl;

typedef struct Symbol Symbol;
typedef struct Symbol {
    char *name;
    Decl decl;
    Symbol *next;  // link to symbol in the same bucket
    Symbol *cross; // cross link to symbol in the same scope
} Symbol;

// Symbol table.
//
// Find Decl that matches the given Id.
typedef Symbol **SymbolTable;

typedef struct {
    Parser parser;
    SymbolTable symbol_table;
} CmpState;

void cmpstate_init(CmpState *cs, const char *filename);
void cmpstate_cleanup(CmpState *cs);

// Traverse the AST starting from 'node' as the root node.
void traverse(Node *node);

#endif
