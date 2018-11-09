#include "parser.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

static struct node *parse_expr(parser_t *p);

static struct node *make_node(enum node_type t, token_t *tok)
{
    struct node *node = malloc(sizeof(struct node));
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

static struct node *make_binexpr(struct node *lhs, struct node *op, struct node *rhs)
{
    struct node *node = make_node(ND_BINEXPR, NULL);
    node->lhs = lhs;
    node->op = op;
    node->rhs = rhs;
    return node;
}

static void free_node(struct node *node)
{
    struct node *c = node->child;
    switch(node->type) {
    case ND_BINEXPR:
        free_node(node->lhs);
        free_node(node->op);
        free_node(node->rhs);
        break;
    }
    free(node);
}

static void iprintf(int indent, const char *fmt, ...) {
    for (; indent > 0; indent--)
        putchar(' ');
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
}

static void print_node_indent(const struct node *node, int indent)
{
    if (!node) {
        iprintf(indent, "(null)\n");
        return;
    }

    switch (node->type) {
    case ND_ATOM:
        switch (node->token.type) {
        case TOK_IDENT:
            iprintf(indent, "[Identifier]\n");
            break;
        case TOK_NUM:
            iprintf(indent, "[Number]\n");
            break;
        default:
            iprintf(indent, "[unknown]\n");
            break;
        }
        break;
    case ND_BINEXPR:
        iprintf(indent, "[BinaryExpr]\n");
        indent += 2;
        indent -= 2;
        break;
    default:
        printf("print_node: unrecognized node type\n");
        break;
    }

    // struct node *c = node->child;
    // for (int i = 0; c; i++) {
    //     printf("Child %d\n", i);
    //     c = c->sibling;
    // }
}

static void print_node(const struct node *node)
{
    print_node_indent(node, 0);
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
        error(p, "expected %s, got %s\n", token_names[t], token_names[look(p)->type]);
        exit(1);
    }
    // Make progress
    next(p);
}

static struct node *parse_literal_expr(parser_t *p)
{
    struct node *expr = make_node(ND_ATOM, look(p));
    next(p);
    return expr;
}

static struct node *parse_unary_expr(parser_t *p)
{
    struct node *expr = NULL;

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

static int get_precedence(const token_t *op)
{
    switch (op->type) {
    case TOK_STAR:
    case TOK_SLASH:
        return 1;
    case TOK_PLUS:
    case TOK_MINUS:
        return 0;
    default:
        return -1; // not an operator
    }
}

// Parse (op binary)* part of the production
//
//     binary_expr: unary (op binary)* .
//
// Return the pointer to the node respresenting the reduced binary expression.
// May be the same as 'lhs' if nothing was reduced.
static struct node *parse_binary_expr_rhs(parser_t *p, struct node *lhs, int precedence)
{
    while (1) {
        int this_prec = get_precedence(look(p));

        // If the upcoming op has lower precedence, the subexpression of the
        // precedence level that we are currently parsing in is finished.
        // This is equivalent to reducing on a shift/reduce conflict in
        // bottom-up parsing.
        if (this_prec < precedence)
            break;

        struct node *op = make_node(ND_ATOM, look(p));
        next(p);

        // Parse the next term.  We do not know yet if this term should bind to
        // LHS or RHS; e.g. "a * b + c" or "a + b * c".  To know this, we should
        // look ahead for the operator that follows this term.
        struct node *rhs = parse_unary_expr(p);
        int next_prec = get_precedence(look(p));

        // If the next operator is indeed higher-level, evaluate the RHS as a
        // whole subexpression with elevated minimum precedence. Else, just
        // treat it as a unary expression.  This is equivalent to shifting on a
        // shift/reduce conflict in bottom-up parsing.
        //
        // If this_prec == next_prec, don't shift, but reduce it with lhs.
        // This implies left associativity.
        if (this_prec < next_prec)
            rhs = parse_binary_expr_rhs(p, rhs, precedence + 1);
        lhs = make_binexpr(lhs, op, rhs);
    }
    return lhs;
}

static struct node *parse_expr(parser_t *p)
{
    struct node *expr = parse_unary_expr(p);
    expr = parse_binary_expr_rhs(p, expr, 0);
    return expr;
}

void parse(parser_t *p)
{
    // for (; look(p)->type != TOK_EOF; next(p)) {
    //     if (look(p)->type == TOK_ASSIGN) {
    //         struct node *node = parse_assign(p);
    //         print_node(node);
    //         free_node(node);
    //     }
    // }

    struct node *node;

    switch (look(p)->type) {
    default:
        node = parse_expr(p);
        expect(p, TOK_SEMICOLON);
        print_node(node);
        free_node(node);
        break;
    }
}
