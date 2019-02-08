#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

typedef enum NodeType {
    ND_TOKEN,
    ND_VARDECL,
    ND_TYPE,
    ND_LITEXPR,
    ND_BINEXPR,
    ND_EXPRSTMT,
    ND_DECLSTMT,
    ND_COMPOUNDSTMT,
} NodeType ;

typedef struct AstNode AstNode;
typedef struct AstNode {
    NodeType type;
    union {
        // Literal/token-only expression
        Token token;
        // Binary expression
        struct {
            AstNode *lhs;
            AstNode *op;
            AstNode *rhs;
        };
        // Variable declaration
        struct {
            AstNode *decltype;
            AstNode *name;
            AstNode *expr;
            int mutable;
        };
        // Expression statement
        AstNode *stmt_expr;
        // Declaration statement
        AstNode *decl;
        // Compound statement
        AstNode **compound_stmt;
    };
} AstNode;

typedef struct {
    Lexer lexer;
    Token *tok; // lookahead token
    AstNode **nodep_buf; // pointers to the allocated nodes
} Parser;

void parser_init(Parser *p, const char *filename);
void parser_free(Parser *p);
void parse(Parser *p);

#endif
