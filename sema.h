/* vim: set ft=c: */
#ifndef SEMA_H
#define SEMA_H

#include "ast.h"
#include <stdlib.h>

#define HASHTABLE_SIZE 512

void fatal(const char *fmt, ...);

// A Name corresponds to any single unique identifier string in the source
// text.  There may be multiple occurrences of the string in the source text,
// but only one matching Name object is created in the course of compilation.
typedef struct Name Name;
struct Name {
	char *text;
	Name *next;
};

/* A NameTable is a hash table of Names queried by their raw string.  It serves
 * to reduce the number of string hashing operation, since we can lookup the
 * symbol table using Name instead of raw char * throughout the semantic
 * analysis.
 */
#define NAMETABLE_SIZE 512

typedef struct NameTable {
	Name *keys[NAMETABLE_SIZE];
	Name *name_buf; // memory buffer that stores Names
} NameTable;

Name *push_name(NameTable *nt, const char *s, size_t len);
Name *push_refname(NameTable *nt, const Name *name);
Name *get_name(NameTable *nt, const char *s, size_t len);

typedef struct Func {
} Func;

typedef struct Var {
	int mut;
} Var;

enum TypeKind {
    T_REF,
    T_ALIAS,
    T_CANON,
};

// Type represents every type present in a program.
// Each type has a one-to-one correspondence with each Type object, and
// therefore the equality check between two types can be done by directly
// comparing Type * pointers.
//
// All Types have a pointer to their canonical type.  Canonical type is the
// 'atom' type that is actually being associated with.  For example, the
// canonical type of a pointer type is the type that it points to, and an array
// type the type of its elements.
//
// Canonical types has their `kind` value as T_CANON, and `canon_type` as NULL.
typedef struct Type {
	enum TypeKind kind;
	Name *name;
	struct Type *canon_type;
} Type;

typedef struct Decl {
	Name *name;
	int scope;
	Type *type;
} Decl;

// A Symbol owns its value, i.e. it should always be safe to do free(value) for
// each symbol.
typedef struct Symbol {
	Name *name;
	int scope;
	void *value;
	struct Symbol *next;  // link to symbol in the same bucket
	struct Symbol *cross; // cross link to symbol in the same scope
} Symbol;

// Map is a generic scoped hash table data structure used for all kinds of
// symbols in the program, such as declarations or types.
typedef struct Map {
	Symbol *buckets[HASHTABLE_SIZE];
	Symbol **scopes;
	int n_scope; /* innermost scope level */
} Map;

typedef struct Context {
    NameTable *nt;
    Map declmap;
    Map typemap;
    Type *i32_type;
    Type *i64_type;
} Context;

typedef struct Node Node;

void sema(NameTable *nt, Node *root);

#endif
