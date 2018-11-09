#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

enum node_type {
    ND_ASSIGN,
    ND_STMT,
    ND_ATOM,
    ND_BINEXPR,
};

typedef struct node {
    enum node_type type;
    token_t token;
    struct node *child;
    struct node *sibling;

    union {
        // Binary expression
        struct {
            struct node *lhs;
            struct node *op;
            struct node *rhs;
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
