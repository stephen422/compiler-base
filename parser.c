#include "parser.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

static ast_t *parse_expr(parser_t *p);

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
    if (!node) {
        printf("(null)\n");
        return;
    }

    switch (node->type) {
    case ND_ATOM:
        switch (node->token.type) {
        case TOK_IDENT:
            printf("[Identifier]\n");
            break;
        case TOK_NUM:
            printf("[Number]\n");
            break;
        default:
            printf("[unknown]\n");
            break;
        }
        break;
    default:
        printf("print_node: unrecognized node type\n");
        break;
    }
    ast_t *c = node->child;
    for (int i = 0; c; i++) {
        printf("Child %d\n", i);
        c = c->sibling;
    }
}

static token_t *look(parser_t *p)
{
    return &p->lexer.token;
}

static void error(parser_t *p, const char *fmt, ...)
{
    va_list args;
    fprintf(stderr, "%s:%ld: parse error: ", p->lexer.filename, look(p)->start);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
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
        fprintf(stderr, "parse error: expected %s, got %s\n", token_names[t], token_names[look(p)->type]);
        exit(1); // FIXME
    }
    // Make progress
    next(p);
}

static ast_t *parse_literal_expr(parser_t *p)
{
    ast_t *expr = make_node(ND_ATOM, look(p));
    next(p);
    return expr;
}

static ast_t *parse_unary_expr(parser_t *p)
{
    ast_t *expr = NULL;

    switch (look(p)->type) {
    case TOK_NUM:
    case TOK_IDENT:
        expr = parse_literal_expr(p);
        break;
    case TOK_LPAREN:
        expect(p, TOK_LPAREN);
        expr = parse_expr(p);
        expect(p, TOK_RPAREN);
        break;
    default:
        error(p, "expected a unary expression\n");
        break;
    }
    return expr;
}

static ast_t *parse_expr(parser_t *p)
{
    return parse_unary_expr(p);
}

void parse(parser_t *p)
{
    // for (; look(p)->type != TOK_EOF; next(p)) {
    //     if (look(p)->type == TOK_ASSIGN) {
    //         ast_t *node = parse_assign(p);
    //         print_node(node);
    //         free_node(node);
    //     }
    // }

    ast_t *node;

    switch (look(p)->type) {
    default:
        node = parse_expr(p);
        print_node(node);
        free_node(node);
        break;
    }
}
