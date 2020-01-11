/* vim: set ft=c: */
#ifndef AST_H
#define AST_H

#include "lexer.h"

typedef struct Name Name;
typedef struct NameTable NameTable;
typedef struct Type Type;

typedef enum NodeKind {
    ND_NONE,
    ND_TOKEN,
    ND_TYPEEXPR,
    ND_BADTYPEEXPR,
    ND_LITEXPR,   // 3
    ND_IDEXPR,    // a
    ND_REFEXPR,   // &a
    ND_DEREFEXPR, // *a
    ND_BINEXPR,
    ND_EXPRSTMT,
    ND_PARAMDECL,
    ND_VARDECL,
    ND_DECLSTMT,
    ND_ASSIGNSTMT,
    ND_RETURNSTMT,
    ND_COMPOUNDSTMT,
    ND_FUNCDECL,
    ND_FILE,
} NodeKind;

typedef struct Node Node;
typedef struct Node {
    NodeKind kind;
    Token token;

    Name *name;
    Type *type;
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
    Node *decl;      // declstmt

    // funcdecl
    Node *body;
    Node **paramdecls;
    Node *rettypeexpr;
} Node;

/* ASTContext wraps the root node of AST with metadata such as the name table.
 * It does not own any of the metadata, and should consist only of pointers to
 * the structures.  It is intended to be passed around by value.
 */
typedef struct ASTContext {
	NameTable *nametable;
	Node *root;
} ASTContext;

#endif
