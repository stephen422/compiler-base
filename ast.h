/* vim: set ft=c: */
#ifndef AST_H
#define AST_H

#include "lexer.h"

typedef struct Name Name;
typedef struct NameTable NameTable;
typedef struct Type Type;
typedef struct Expr Expr;
typedef struct TypeExpr TypeExpr;
typedef struct DeclNode DeclNode;
typedef struct Stmt Stmt;
typedef struct Node Node;
typedef enum NodeKind NodeKind;

enum NodeKind {
    ND_NONE,
    ND_TOKEN,
    ND_TYPEEXPR,
    ND_BADTYPEEXPR,
    ND_LITEXPR,
    ND_IDEXPR,
    ND_REFEXPR,
    ND_DEREFEXPR,
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
};

struct Expr {
    Name *name;
    Node *target; // ref, deref
    Token op;
    Node *lhs;
    Node *rhs;
    Node **args;
};

struct TypeExpr {
    Name *name;
    Node *base;
    int ref;
};

struct DeclNode {
    Name *name;
    Node *typeexpr;
    Node *expr; // init expr for VarDecl
    int ref;
    int mutable;
};

struct Stmt {
    Node *stmt_expr;
    Node *decl;
    Node *nodes; // compoundstmt
};

struct Node {
    NodeKind kind;
    Token token;

    Type *type;

    Node **nodes; // file

    union {
        Expr e;
        DeclNode d;
        Stmt s;
        TypeExpr t;
    };

    // funcdecl
    Node *body;
    Node **paramdecls;
    Node *rettypeexpr;
};

/* ASTContext wraps the root node of AST with metadata such as the name table.
 * It does not own any of the metadata, and should consist only of pointers to
 * the structures.  It is intended to be passed around by value.
 */
typedef struct ASTContext {
	NameTable *nametable;
	Node *root;
} ASTContext;

#endif
