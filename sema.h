#ifndef SEMA_H
#define SEMA_H

#define HASHTABLE_SIZE 512

// A Name corresponds to any single unique identifier string in the source
// text.  There may be multiple occurrences of the string in the source text,
// but only one matching Name object is created in the course of compilation.
typedef struct Name Name;
struct Name {
	char *text;
	Name *next;
};

// A NameTable is a hash table of Names queried by their raw string.  It serves
// to reduce the number of string hashing operation, since we can lookup the
// symbol table using Name instead of raw char * throughout the semantic
// analysis.
#define NAMETABLE_SIZE 512

typedef struct NameTable NameTable;
struct NameTable {
	Name *keys[NAMETABLE_SIZE];
	Name *name_buf; // memory buffer that stores Names
};

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

typedef struct Node Node;

// Traverse the AST starting from 'node' as the root node.
void traverse(Node *node);
void sema(Node *node);

#endif
