#include "parser.h"
#include <stdio.h>
#include <stdlib.h>

static void ast_add(ast_t *parent, ast_t *child)
{
    ast_t *c = parent->child;
    if (!c) {
        parent->child = child;
        return;
    }
    while (c->sibling)
        c = c->sibling;
    c->sibling = child;
}

static ast_t *make_node(enum node_type t, token_t *tok)
{
    ast_t *node = malloc(sizeof(ast_t));
    if (!node) {
        fprintf(stderr, "alloc error\n");
        exit(1);
    }
    node->type = t;
    if (tok)
        node->token = *tok;
    node->child = NULL;
    node->sibling = NULL;
    return node;
}

static void free_node(ast_t *node)
{
    ast_t *c = node->child;
    while (c) {
        ast_t *next = c->sibling;
        free_node(c);
        c = next;
    }
    free(node);
}

static void print_node(const ast_t *node)
{
    ast_t *c = node->child;
    for (int i = 0; c; i++) {
        printf("Child %d: (not implemented)\n", i);
        c = c->sibling;
    }
}

static token_t *look(parser_t *p)
{
    return &p->lexer.token;
}

void parser_init(parser_t *p, const char *filename)
{
    lexer_init(&p->lexer, filename);
    lexer_next(&p->lexer);
    p->tok = NULL;
}

void parser_free(parser_t *p)
{
    lexer_free(&p->lexer);
}

static void next(parser_t *p)
{
    lexer_next(&p->lexer);
}

static void expect(parser_t *p, TokenType t)
{
    if (look(p)->type != t) {
        fprintf(stderr, "parse error: expected %s, got %s (%d)\n", token_names[t], token_names[look(p)->type], look(p)->type);
        exit(1); // FIXME
    }
    // place the next token to the lookahead
    next(p);
}

ast_t *parse_assign(parser_t *p)
{
    ast_t *node = make_node(ND_ASSIGN, NULL);
    next(p); // "assign"

    ast_add(node, make_node(ND_ATOM, look(p)));
    next(p);

    expect(p, TOK_EQUALS);

    ast_add(node, make_node(ND_ATOM, look(p)));
    next(p);

    return node;
}

void parse(parser_t *p)
{
    for (; look(p)->type != TOK_EOF; next(p)) {
        if (look(p)->type == TOK_ASSIGN) {
            printf("Found assign!\n");
            ast_t *node = parse_assign(p);
            print_node(node);
            free_node(node);
            printf("Over!\n");
        }
    }
}
