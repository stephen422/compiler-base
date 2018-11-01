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
    node->tok = tok;
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
    if (node->tok)
        token_free(node->tok);
    free(node);
}

static void print_node(const ast_t *node)
{
    ast_t *c = node->child;
    for (int i = 0; c; i++) {
        printf("Child %d: ", i);
        print_token(c->tok);
        c = c->sibling;
    }
}

void parser_init(parser_t *p, const char *filename)
{
    lexer_init(&p->lexer, filename);
    p->tok = lexer_next(&p->lexer);
}

void parser_free(parser_t *p)
{
    token_free(p->tok);
    lexer_free(&p->lexer);
}

static void next(parser_t *p)
{
    if (p->tok)
        token_free(p->tok);
    p->tok = lexer_next(&p->lexer);
}

static void expect(parser_t *p, TokenType t)
{
    if (p->tok->type != t) {
        fprintf(stderr, "parse error: expected %s, got %s (%d)\n", token_names[t], token_names[p->tok->type], p->tok->type);
        exit(1); // FIXME
    }
    // place the next token to the lookahead
    next(p);
}

ast_t *parse_assign(parser_t *p)
{
    ast_t *node = make_node(ND_ASSIGN, NULL);
    next(p); // "assign"

    ast_add(node, make_node(ND_ATOM, p->tok));
    p->tok = NULL;
    next(p);

    expect(p, TOK_EQUALS);

    ast_add(node, make_node(ND_ATOM, p->tok));
    p->tok = NULL;
    next(p);

    return node;
}

void parse(parser_t *p)
{
    for (; p->tok->type != TOK_EOF; next(p)) {
        if (p->tok->type == TOK_ASSIGN) {
            printf("Found assign!\n");
            ast_t *node = parse_assign(p);
            print_node(node);
            free_node(node);
            printf("Over!\n");
        }
    }
}
