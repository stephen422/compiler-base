#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

enum node_type {
    ND_ASSIGN,
    ND_STMT,
    ND_ATOM,
    ND_BINEXPR,
};

typedef struct ast_t ast_t;

typedef struct {
    ast_t *lhs;
    ast_t *rhs;
} assign_stmt;

typedef struct {
    enum {ASSIGN_STMT} type;
    union {
        assign_stmt assign;
    } u;
} stmt;

struct ast_t {
    enum node_type type;
    token_t token;
    ast_t *child;
    ast_t *sibling;
};

typedef struct {
    lexer_t lexer;
    token_t *tok; // lookahead token
} parser_t;

void parser_init(parser_t *p, const char *filename);
void parser_free(parser_t *p);
void parse(parser_t *p);

#endif
