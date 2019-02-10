#ifndef SEMA_H
#define SEMA_H

#include "parser.h"

// An Id corresponds to any single unique string name in the source txt.
typedef struct {
    char *text;
} Id;

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

typedef struct {
    Id *name;
    Decl decl;
} SymbolMap;

// Symbol table.
//
// Find a declaration by its ID.
typedef struct {
    // Symbol syms[];
} SymbolTable;

void traverse(AstNode *node);

#endif
