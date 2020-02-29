/* vim: set ft=c: */
#ifndef AST_H
#define AST_H

#include "lexer.h"

typedef struct Name Name;
typedef struct NameTable NameTable;
typedef struct Type Type;
typedef struct AstBase AstBase;
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

struct AstBase {
    NodeKind kind;
};

struct Expr {
    Name *name;
    Type *type;
    Expr *target; // ref, deref
    Token op;
    Expr *lhs;
    Expr *rhs;
    Node **args;
};

struct TypeExpr {
    Type *type;
    Name *name;
    TypeExpr *subtype;
    int ref;
};

struct DeclNode {
    Name *name;
    Type *type;
    TypeExpr *typeexpr;
    Expr *expr; // init expr for VarDecl
    int ref;
    int mutable;
};

struct Stmt {
    Expr *stmt_expr;
    Node *decl;
    Node *nodes; // compoundstmt
};

struct Node {
    AstBase base;

    union {
        Expr e;
        DeclNode d;
        Stmt s;
        TypeExpr t;
    };

    Token token;

    Node **nodes; // file

    // funcdecl
    Node *body;
    Node **paramdecls;
    TypeExpr *rettypeexpr;
};

static const Node _n;
#define astbase(pn)  \
    ((AstBase *)((char *)(pn) - ((char *)(&_n.e) - (char *)(&_n))))
/* ASTContext wraps the root node of AST with metadata such as the name table.
 * It does not own any of the metadata, and should consist only of pointers to
 * the structures.  It is intended to be passed around by value.
 */
typedef struct ASTContext {
	NameTable *nametable;
	Node *root;
} ASTContext;

#endif
