#include "parser.h"
#include <stdio.h>
#include <stdlib.h>

static ast_t *make_node(NodeType t)
{
    ast_t *node = malloc(sizeof(ast_t));
    if (!node)
        fprintf(stderr, "malloc error\n");
    node->type = t;
    return node;
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

static void parser_next(parser_t *p)
{
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
    parser_next(p);
}

ast_t *parse_assign(parser_t *p)
{
    ast_t *node = make_node(ND_ASSIGN);
    parser_next(p); // "assign"
    expect(p, TOK_IDENT);
    expect(p, TOK_EQUALS);
    expect(p, TOK_IDENT);
    return node;
}

void parse(parser_t *p)
{
    for (; p->tok->type != TOK_EOF; parser_next(p)) {
        if (p->tok->type == TOK_ASSIGN) {
            printf("Found assign!\n");
            ast_t *node = parse_assign(p);
            free(node);
            printf("Over!\n");
        }
    }
}
