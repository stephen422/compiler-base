#include "parser.h"
#include "stretchy_buffer.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

static AstNode *parse_expr(Parser *p);

static AstNode *make_node(Parser *p, NodeType t, token_t *tok)
{
    // TODO: maybe store all nodes in a contiguous buffer for better cache
    // utilization?  Should take care about node pointers going stale though
    AstNode *node = calloc(1, sizeof(AstNode));
    if (!node) {
        fprintf(stderr, "alloc error\n");
        exit(1);
    }
    node->type = t;
    if (tok) {
        node->token = *tok;
    }
    sb_push(p->nodep_buf, node);
    return node;
}

static AstNode *make_binexpr(Parser *p, AstNode *lhs, AstNode *op, AstNode *rhs)
{
    AstNode *node = make_node(p, ND_BINEXPR, NULL);
    node->lhs = lhs;
    node->op = op;
    node->rhs = rhs;
    return node;
}

static AstNode *make_vardecl(Parser *p, AstNode *decltype, int mutable, AstNode *name, AstNode *expr)
{
    AstNode *node = make_node(p, ND_VARDECL, NULL);
    node->decltype = decltype;
    node->mutable = mutable;
    node->name = name;
    node->expr = expr;
    return node;
}

static void free_node(AstNode *node)
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

static void print_node_indent(Parser *p, const AstNode *node, int indent)
{
    if (!node) {
        iprintf(indent, "(null)\n");
        return;
    }

    iprintf(indent, "");

    switch (node->type) {
    case ND_TOKEN:
        print_token(&p->lexer, &node->token);
        break;
    case ND_LITEXPR:
        printf("[LiteralExpr] ");
        switch (node->token.type) {
        case TOK_IDENT:
            print_token(&p->lexer, &node->token);
            break;
        case TOK_NUM:
            print_token(&p->lexer, &node->token);
            break;
        default:
            print_token(&p->lexer, &node->token);
            break;
        }
        break;
    case ND_BINEXPR:
        printf("[BinaryExpr]\n");
        indent += 2;
        print_node_indent(p, node->lhs, indent);
        print_node_indent(p, node->op, indent);
        print_node_indent(p, node->rhs, indent);
        indent -= 2;
        break;
    case ND_VARDECL:
        printf("[VarDecl]\n");
        indent += 2;
        iprintf(indent, "mutable: %d\n", node->mutable);
        print_node_indent(p, node->name, indent);
        print_node_indent(p, node->expr, indent);
        indent -= 2;
        break;
    default:
        fprintf(stderr, "%s: unrecognized node type\n", __func__);
        break;
    }
}

static void print_node(Parser *p, const AstNode *node)
{
    print_node_indent(p, node, 0);
}

static token_t *look(Parser *p)
{
    return &p->lexer.token;
}

static void error(Parser *p, const char *fmt, ...)
{
    SourceLoc loc = locate_line_col(&p->lexer, look(p)->start);
    va_list args;
    fprintf(stderr, "%s:%d:%d: parse error: ", p->lexer.filename, loc.line, loc.col);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    exit(1);
}

void parser_init(Parser *p, const char *filename)
{
    *p = (const Parser) {0};
    lexer_init(&p->lexer, filename);
    lexer_next(&p->lexer);
}

void parser_free(Parser *p)
{
    // Free all nodes.

    lexer_free(&p->lexer);
}

static void next(Parser *p)
{
    lexer_next(&p->lexer);
}

static void expect(Parser *p, TokenType t)
{
    if (look(p)->type != t) {
        error(p, "expected %s, got %s\n", token_names[t], token_names[look(p)->type]);
        exit(1);
    }
    // Make progress
    next(p);
}

static AstNode *parse_literal_expr(Parser *p)
{
    AstNode *expr = make_node(p, ND_LITEXPR, look(p));
    next(p);
    return expr;
}

static AstNode *parse_unary_expr(Parser *p)
{
    AstNode *expr = NULL;

    switch (look(p)->type) {
    case TOK_IDENT:
        expr = make_node(p, ND_TOKEN, look(p));
        next(p);
        break;
    case TOK_NUM:
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
static AstNode *parse_binary_expr_rhs(Parser *p, AstNode *lhs, int precedence)
{
    while (1) {
        int this_prec = get_precedence(look(p));

        // If the upcoming op has lower precedence, the subexpression of the
        // precedence level that we are currently parsing in is finished.
        // This is equivalent to reducing on a shift/reduce conflict in
        // bottom-up parsing.
        if (this_prec < precedence)
            break;

        AstNode *op = make_node(p, ND_TOKEN, look(p));
        next(p);

        // Parse the next term.  We do not know yet if this term should bind to
        // LHS or RHS; e.g. "a * b + c" or "a + b * c".  To know this, we should
        // look ahead for the operator that follows this term.
        AstNode *rhs = parse_unary_expr(p);
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
        lhs = make_binexpr(p, lhs, op, rhs);
    }
    return lhs;
}

static AstNode *parse_expr(Parser *p)
{
    AstNode *expr = parse_unary_expr(p);
    expr = parse_binary_expr_rhs(p, expr, 0);
    return expr;
}

static AstNode *parse_var_decl(Parser *p)
{
    AstNode *decltype = NULL;
    AstNode *expr = NULL;

    int mut = look(p)->type == TOK_VAR;
    next(p);
    AstNode *name = make_node(p, ND_TOKEN, look(p)); /* FIXME RefExpr? */
    next(p);

    // Type specification
    if (look(p)->type == TOK_COLON) {
        next(p);
        decltype = make_node(p, ND_TYPE, look(p));
        next(p);
    }
    // Assignment expression
    if (look(p)->type == TOK_EQUALS) {
        next(p);
        expr = parse_expr(p);
    }
    // At least one of the declaration type and assignment expression
    // should be specified.
    if (decltype == NULL && expr == NULL)
        error(p, "declarations should at least specify its type or its value.");

    return make_vardecl(p, decltype, mut, name, expr);
}

static AstNode *parse_decl(Parser *p)
{
    AstNode *decl;

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

void parse(Parser *p)
{
    AstNode *node;

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
