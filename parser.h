#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

typedef enum {
    ND_ASSIGN,
} NodeType;

typedef struct ast_t ast_t;
struct ast_t {
    NodeType type;
    Token t;
    ast_t *child;
    int n_child;
};

typedef struct {
    Lexer lexer;
    Token *tok; // lookahead token
} parser_t;

void parser_init(parser_t *p, const char *filename);
void parser_free(parser_t *p);
void parse(parser_t *p);

#endif
