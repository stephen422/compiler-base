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
    ND_RETURNSTMT,
    ND_COMPOUNDSTMT,
    ND_FUNCTION,
} NodeType ;

typedef struct AstNode AstNode;
typedef struct AstNode {
    NodeType type;
    // Literal/token-only expression
    Token token;
    // Binary expression
    AstNode *lhs;
    AstNode *op;
    AstNode *rhs;
    // Variable declaration
    AstNode *decltype;
    AstNode *expr;
    int mutable;
    // Expression statement
    AstNode *stmt_expr;
    // Declaration statement
    AstNode *decl;
    // Compound statement
    AstNode **stmt_buf;
    AstNode *body;
    // Function name (FIXME: unnecessary)
    Token name;
    // Function return type (TODO: type node)
    Token return_type;
} AstNode;

typedef struct {
    Lexer lexer;
    Token *lookahead_cache;   // lookahead tokens cache
    int cache_index;
    AstNode **nodep_buf; // pointers to the allocated nodes
} Parser;

void parser_init(Parser *p, const char *filename);
void parser_free(Parser *p);
AstNode *parse(Parser *p);

#endif
