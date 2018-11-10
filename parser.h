#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

enum node_type {
    ND_TOKEN,
    ND_STMT,
    ND_VARDECL,
    ND_TYPE,
    ND_LITEXPR,
    ND_BINEXPR,
};

typedef struct node {
    enum node_type type;
    union {
        // Literal/token-only expression
        token_t token; // TODO: replace with semantic value
        // Binary expression
        struct {
            struct node *lhs;
            struct node *op;
            struct node *rhs;
        };
        // Variable declaration
        struct {
            struct node *decltype;
            struct node *name;
            struct node *expr;
            int mutable;
        };
    };
} node_t;

typedef struct {
    lexer_t lexer;
    token_t *tok; // lookahead token
} parser_t;

void parser_init(parser_t *p, const char *filename);
void parser_free(parser_t *p);
void parse(parser_t *p);

#endif
