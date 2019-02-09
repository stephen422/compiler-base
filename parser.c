#include "parser.h"
#include "stretchy_buffer.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

static Token look(Parser *p);
static AstNode *parse_expr(Parser *p);
static AstNode *parse_decl(Parser *p);

static AstNode *make_node(Parser *p, NodeType t, Token tok)
{
    // TODO: maybe store all nodes in a contiguous buffer for better cache
    // utilization?  Should take care about node pointers going stale though
    AstNode *node = calloc(1, sizeof(AstNode));
    if (!node) {
        fprintf(stderr, "alloc error\n");
        exit(1);
    }
    node->type = t;
    node->token = tok;
    sb_push(p->nodep_buf, node);
    return node;
}

static AstNode *make_expr_stmt(Parser *p, AstNode *expr)
{
    AstNode *node = make_node(p, ND_EXPRSTMT, look(p));
    node->expr = expr;
    return node;
}

static AstNode *make_decl_stmt(Parser *p, AstNode *decl) {
    AstNode *node = make_node(p, ND_DECLSTMT, look(p));
    node->decl = decl;
    return node;
}

static AstNode *make_compound_stmt(Parser *p)
{
    AstNode *node = make_node(p, ND_COMPOUNDSTMT, look(p));
    return node;
}

static AstNode *make_binexpr(Parser *p, AstNode *lhs, AstNode *op, AstNode *rhs)
{
    AstNode *node = make_node(p, ND_BINEXPR, look(p));
    node->lhs = lhs;
    node->op = op;
    node->rhs = rhs;
    return node;
}

static AstNode *make_vardecl(Parser *p, AstNode *decltype, int mutable, Token name, AstNode *expr)
{
    AstNode *node = make_node(p, ND_VARDECL, look(p));
    node->decltype = decltype;
    node->mutable = mutable;
    node->name = name;
    node->expr = expr;
    return node;
}

static AstNode *make_function(Parser *p, Token name)
{
    AstNode *node = make_node(p, ND_FUNCTION, look(p));
    node->name = name;
    return node;
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
    case ND_DECLSTMT:
        printf("[DeclStmt]\n");
        indent += 2;
        print_node_indent(p, node->decl, indent);
        indent -= 2;
        break;
    case ND_EXPRSTMT:
        printf("[ExprStmt]\n");
        indent += 2;
        print_node_indent(p, node->expr, indent);
        indent -= 2;
        break;
    case ND_COMPOUNDSTMT:
        printf("[CompoundStmt]\n");
        indent += 2;
        for (int i = 0; i < sb_count(node->stmt_buf); i++) {
            print_node_indent(p, node->stmt_buf[i], indent);
        }
        indent -= 2;
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
        printf("[VarDecl] ");
        print_token(&p->lexer, &node->name);
        indent += 2;
        iprintf(indent, "mutable: %d\n", node->mutable);
        print_node_indent(p, node->expr, indent);
        indent -= 2;
        break;
    case ND_FUNCTION:
        printf("[Function] ");
        print_token(&p->lexer, &node->name);
        indent += 2;
        print_node_indent(p, node->body, indent);
        indent -= 2;
        break;
    default:
        fprintf(stderr, "%s: unrecognized node type %d\n", __func__, node->type);
        break;
    }
}

static void print_node(Parser *p, const AstNode *node)
{
    print_node_indent(p, node, 0);
}

static Token look(Parser *p)
{
    return p->lookahead_cache[p->cache_index];
}

static void error(Parser *p, const char *fmt, ...)
{
    SourceLoc loc = locate_line_col(&p->lexer, look(p).start);
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
    sb_push(p->lookahead_cache, p->lexer.token);
}

void parser_free(Parser *p)
{
    // Free all nodes.
    for (int i = 0; i < sb_count(p->nodep_buf); i++) {
        AstNode *node = p->nodep_buf[i];
        if (node) {
            if (node->stmt_buf) {
                sb_free(node->stmt_buf);
            }
            free(node);
        }
    }
    sb_free(p->nodep_buf);
    sb_free(p->lookahead_cache);
    lexer_free(&p->lexer);
}

static void next(Parser *p)
{
    if (p->cache_index >= sb_count(p->lookahead_cache) - 1) {
        lexer_next(&p->lexer);
        sb_push(p->lookahead_cache, p->lexer.token);
    }
    p->cache_index++;
}

static void expect(Parser *p, TokenType t)
{
    if (look(p).type != t) {
        error(p, "expected '%s', got '%s'\n", token_names[t], token_names[look(p).type]);
        exit(1);
    }
    // Make progress in expect()!
    next(p);
}

static void save_state(Parser *p)
{
    // Clear cache except the current lookahead.
    p->lookahead_cache[0] = p->lookahead_cache[p->cache_index];
    stb__sbn(p->lookahead_cache) = 1;
    p->cache_index = 0;
}

static void revert_state(Parser *p)
{
    p->cache_index = 0;
}

static AstNode *parse_stmt(Parser *p)
{
    AstNode *stmt, *node = NULL;

    save_state(p);

    // Try all possible productions and use the first successful one.
    // We use lookahead (LL(k)) to revert state if a production fails.
    // (See "recursive descent with backtracking":
    // https://en.wikipedia.org/wiki/Recursive_descent_parser)
    if (look(p).type == TOK_EOF) {
        return NULL;
    }
    if (look(p).type == TOK_SEMICOLON) {
        next(p);
        return parse_stmt(p); // FIXME stack overflow
    }

    node = parse_decl(p);
    if (node) {
        stmt = make_decl_stmt(p, node);
        expect(p, TOK_SEMICOLON);
        return stmt;
    }
    revert_state(p);

    node = parse_expr(p);
    if (node) {
        stmt = make_expr_stmt(p, node);
        expect(p, TOK_SEMICOLON);
        return stmt;
    }
    revert_state(p);

    // By now, no production succeeded and node is NULL.
    return NULL;
}

static AstNode *parse_compound_stmt(Parser *p)
{
    expect(p, TOK_LBRACE);
    AstNode *compound = make_compound_stmt(p);
    AstNode *stmt;
    while ((stmt = parse_stmt(p)) != NULL) {
        sb_push(compound->stmt_buf, stmt);
    }
    expect(p, TOK_RBRACE);
    return compound;
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

    switch (look(p).type) {
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
        expr = NULL;
        break;
    }
    return expr;
}

static int get_precedence(const Token op)
{
    switch (op.type) {
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

// Parse (op binary)* part of the production.
//
// BinaryExpr:
//     UnaryExpr (op BinaryExpr)*
//
// Return the pointer to the node respresenting the reduced binary expression.
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

// Parse an expression.
//
// Expr:
//     Id CallParam?
//     UnaryExpr
//     BinaryExpr
//
// CallParam:
//     (Param)
//
// This grammar requires two or more lookahead, because a single token
// lookahead would not tell us whether it is a single-ID expression or a call
// expression.
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

    int mut = look(p).type == TOK_VAR;
    next(p);
    Token name = look(p);
    next(p);

    // Type specification
    if (look(p).type == TOK_COLON) {
        next(p);
        decltype = make_node(p, ND_TYPE, look(p));
        next(p);
    }
    // Assignment expression
    if (look(p).type == TOK_EQUALS) {
        next(p);
        expr = parse_expr(p);
    }
    // At least one of the declaration type and assignment expression
    // should be specified.
    if (decltype == NULL && expr == NULL) {
        error(p, "declarations should at least specify its type or its value.");
    }

    return make_vardecl(p, decltype, mut, name, expr);
}

// Parse a declaration.
//
// Decl:
//     VarDecl
//     Function
static AstNode *parse_decl(Parser *p)
{
    AstNode *decl;

    switch (look(p).type) {
    case TOK_LET:
    case TOK_VAR:
        decl = parse_var_decl(p);
        break;
    default:
        decl = NULL;
        break;
    }
    return decl;
}

static AstNode *parse_function(Parser *p)
{
    expect(p, TOK_FN);

    Token name = look(p);
    AstNode *func = make_function(p, name);
    next(p);

    expect(p, TOK_LPAREN);
    // TODO: Argument list (foo(...))
    expect(p, TOK_RPAREN);

    // Return type (-> ...)
    expect(p, TOK_ARROW);
    func->return_type = look(p);
    next(p);

    // Function body
    func->body = parse_compound_stmt(p);

    return func;
}

void parse(Parser *p)
{
    printf("sizeof(AstNode)=%zu\n", sizeof(AstNode));
    printf("sizeof(Token)=%zu\n", sizeof(Token));
    AstNode *node;
    node = parse_function(p);
    print_node(p, node);
}
