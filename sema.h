#ifndef SEMA_H
#define SEMA_H

#include "parser.h"

#define HASHTABLE_SIZE 512

typedef struct Func {
} Func;

typedef struct Var {
	int mut;
} Var;

typedef struct Type {
} Type;

typedef struct Decl {
	Name *name;
	Type *type;
} Decl;

typedef struct Symbol {
	Name *name;
	void *value;
	struct Symbol *next;  // link to symbol in the same bucket
	struct Symbol *cross; // cross link to symbol in the same scope
} Symbol;

// A Map, or a symbol table.
// This type is intended to be small.
typedef Symbol **Map;

typedef struct CmpState {
	Parser parser;
	Map declmap;
	Map typemap;
} CmpState;

void cmpstate_init(CmpState *cs, const char *filename);
void cmpstate_cleanup(CmpState *cs);

// Traverse the AST starting from 'node' as the root node.
void traverse(Node *node);
void sema(Node *node);

#endif
