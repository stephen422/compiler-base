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

static struct node *make_vardecl(struct node *decltype, int mutable, struct node *name, struct node *expr)
{
    struct node *node = make_node(ND_VARDECL, NULL);
    node->decltype = decltype;
    node->mutable = mutable;
    node->name = name;
    node->expr = expr;
    return node;
}

static void free_node(struct node *node)
{
    if (!node)
        return;

    switch (node->type) {
    case ND_BINEXPR:
        free_node(node->lhs);
        free_node(node->op);
        free_node(node->rhs);
        break;
    case ND_VARDECL:
        free_node(node->decltype);
        free_node(node->name);
        free_node(node->expr);
        break;
    default:
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

static void print_node_indent(parser_t *p, const struct node *node, int indent)
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
        print_node_indent(p, node->lhs, indent);
        print_node_indent(p, node->op, indent);
        print_node_indent(p, node->rhs, indent);
        indent -= 2;
        break;
    case ND_VARDECL:
        iprintf(indent, "[VarDecl]\n");
        indent += 2;
        iprintf(indent, "[Mutable: %d]\n", node->mutable);
        print_node_indent(p, node->name, indent);
        print_node_indent(p, node->expr, indent);
        indent -= 2;
        break;
    default:
        fprintf(stderr, "%s: unrecognized node type\n", __func__);
        break;
    }
}

static void print_node(parser_t *p, const struct node *node)
{
    print_node_indent(p, node, 0);
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
    fprintf(stderr, "\n");
    abort();
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

static struct node *parse_var_decl(parser_t *p)
{
    struct node *decltype = NULL;
    struct node *expr = NULL;

    int mut = look(p)->type == TOK_VAR;
    next(p);
    struct node *name = make_node(ND_ATOM, look(p));
    next(p);

    // Type specification
    if (look(p)->type == TOK_COLON) {
        next(p);
        decltype = make_node(ND_TYPE, look(p));
        next(p);
    }
    // Assignment expression
    if (look(p)->type == TOK_EQUALS) {
        next(p);
        expr = parse_expr(p);
    }
    // If both type and assignment expression is not specified, it's an
    // error.
    if (decltype == NULL && expr == NULL)
        error(p, "declarations should at least specify its type or its value.");

    return make_vardecl(decltype, mut, name, expr);
}

static struct node *parse_decl(parser_t *p)
{
    struct node *decl;

    switch (look(p)->type) {
    case TOK_LET:
    case TOK_VAR:
        decl = parse_var_decl(p);
        break;
    default:
        error(p, "unknown declaration type");
        decl = NULL;
        break;
    }
    expect(p, TOK_SEMICOLON);
    return decl;
}

void parse(parser_t *p)
{
    struct node *node;

    while (look(p) != TOK_EOF) {
        switch (look(p)->type) {
        case TOK_LET:
        case TOK_VAR:
            node = parse_decl(p);
            break;
        default:
            node = parse_expr(p);
            break;
        }
        print_node(p, node);
        free_node(node);
    }
}
