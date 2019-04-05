#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

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

typedef enum NodeType {
	ND_FILE,
	ND_TOKEN,
	ND_PARAMDECL,
	ND_VARDECL,
	ND_TYPEEXPR,
	ND_REFEXPR,
	ND_LITEXPR,
	ND_DEREFEXPR,
	ND_BINEXPR,
	ND_EXPRSTMT,
	ND_DECLSTMT,
	ND_ASSIGNSTMT,
	ND_RETURNSTMT,
	ND_COMPOUNDSTMT,
	ND_FUNCDECL,
} NodeType;

typedef struct Node Node;
typedef struct Node {
	NodeType type;
	Token token;

	Name *name;
	Node *lhs;
	Node *op;
	Node *rhs;
	Node **nodes; // compoundstmt, file

	// decl
	Node *typeexpr; // var type
	Node *expr;
	int ref;
	int mutable;

	Node *stmt_expr; // exprstmt
	Node *decl;	  // declstmt

	// funcdecl
	Node *body;
	Node **paramdecls;
	Node *rettypeexpr;
} Node;

typedef struct {
	SrcLoc loc;
	char *msg;
} Error;

typedef struct {
	Lexer lexer;	      // lexer driven by this parser
	Token *token_cache;   // lookahead tokens cache
	int cache_index;      // current lookahead position in token_cache
	Error *errors;	      // list of possible parse errors
	Node **nodep_buf;     // pointers to the allocated nodes
	NameTable name_table; // name table
} Parser;

void parser_push_name(Parser *p, Token tok);
Name *parser_get_name(Parser *p, Token tok);
void parser_init(Parser *p, const char *filename);
void parser_cleanup(Parser *p);
Node *parse(Parser *p);
void print_ast(Parser *p, const Node *node);

#endif
