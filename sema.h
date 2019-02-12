#ifndef SEMA_H
#define SEMA_H

#include "parser.h"

#define HASHTABLE_SIZE 512

// An Id corresponds to any single unique string name in the source text.
// There may be multiple occurrences of the string in the source text, but
// there is only one instance of the matching Id in the course of compilation.
// This reduces the number of string hashing operation, since we can look up
// the symbol table using Id instead of raw char * throughout the semantic
// analysis phase.
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
    SymbolTable symbol_table;
} Compiler;

// Traverse the AST starting from 'node' as the root node.
void traverse(AstNode *node);

#endif
