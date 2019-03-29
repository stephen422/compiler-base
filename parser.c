#include "parser.h"
#include "stretchy_buffer.h"
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

static Token look(Parser *p);
static Node *parse_expr(Parser *p);
static Node *parse_decl(Parser *p);
static Name *push_name(Parser *p, Token tok);
static Name *get_name(Parser *p, Token tok);

static Node *make_node(Parser *p, NodeType t, Token tok)
{
    // TODO: maybe store all nodes in a contiguous buffer for better locality?
    // Should be careful about node pointers going stale though
    Node *node = calloc(1, sizeof(Node));
    if (!node) {
        fprintf(stderr, "alloc error\n");
        exit(1);
    }
    node->type = t;
    node->token = tok;
    sb_push(p->nodep_buf, node);
    return node;
}

static Node *make_expr_stmt(Parser *p, Node *expr)
{
    Node *node = make_node(p, ND_EXPRSTMT, look(p));
    node->expr = expr;
    return node;
}

static Node *make_decl_stmt(Parser *p, Node *decl) {
    Node *node = make_node(p, ND_DECLSTMT, look(p));
    node->decl = decl;
    return node;
}

static Node *make_return_stmt(Parser *p, Node *expr) {
    Node *node = make_node(p, ND_RETURNSTMT, look(p));
    node->expr = expr;
    return node;
}

static Node *make_compound_stmt(Parser *p)
{
    Node *node = make_node(p, ND_COMPOUNDSTMT, look(p));
    return node;
}

static Node *make_binexpr(Parser *p, Node *lhs, Node *op, Node *rhs)
{
    Node *node = make_node(p, ND_BINEXPR, look(p));
    node->lhs = lhs;
    node->op = op;
    node->rhs = rhs;
    return node;
}

static Node *make_ref_expr(Parser *p, Name *name)
{
    Node *node = make_node(p, ND_REFEXPR, look(p));
    node->name = name;
    return node;
}

static Node *make_vardecl(Parser *p, Node *decltype, int mutable, Name *name, Node *expr)
{
    Node *node = make_node(p, ND_VARDECL, look(p));
    node->decltype = decltype;
    node->mutable = mutable;
    node->name = name;
    node->expr = expr;
    return node;
}

static Node *make_function(Parser *p, Name *name)
{
    Node *node = make_node(p, ND_FUNCTION, look(p));
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

static void print_ast_indent(Parser *p, const Node *node, int indent)
{
    if (!node) {
        iprintf(indent, "(null)\n");
        return;
    }

    iprintf(indent, "");

    switch (node->type) {
    case ND_TOKEN:
        print_token(&p->lexer, node->token);
        break;
    case ND_DECLSTMT:
        printf("[DeclStmt]\n");
        indent += 2;
        print_ast_indent(p, node->decl, indent);
        indent -= 2;
        break;
    case ND_EXPRSTMT:
        printf("[ExprStmt]\n");
        indent += 2;
        print_ast_indent(p, node->expr, indent);
        indent -= 2;
        break;
    case ND_RETURNSTMT:
        printf("[ReturnStmt]\n");
        indent += 2;
        print_ast_indent(p, node->expr, indent);
        indent -= 2;
        break;
    case ND_COMPOUNDSTMT:
        printf("[CompoundStmt]\n");
        indent += 2;
        for (int i = 0; i < sb_count(node->stmt_buf); i++) {
            print_ast_indent(p, node->stmt_buf[i], indent);
        }
        indent -= 2;
        break;
    case ND_LITEXPR:
        printf("[LiteralExpr] ");
        switch (node->token.type) {
        case TOK_IDENT:
            print_token(&p->lexer, node->token);
            break;
        case TOK_NUM:
            print_token(&p->lexer, node->token);
            break;
        default:
            print_token(&p->lexer, node->token);
            break;
        }
        break;
    case ND_BINEXPR:
        printf("[BinaryExpr]\n");
        indent += 2;
        print_ast_indent(p, node->lhs, indent);
        print_ast_indent(p, node->op, indent);
        print_ast_indent(p, node->rhs, indent);
        indent -= 2;
        break;
    case ND_VARDECL:
        printf("[VarDecl] '%s'\n", node->name->text);
        indent += 2;
        iprintf(indent, "mutable: %d\n", node->mutable);
        print_ast_indent(p, node->expr, indent);
        indent -= 2;
        break;
    case ND_FUNCTION:
        printf("[Function] '%s'\n", node->name->text);
        indent += 2;
        print_ast_indent(p, node->body, indent);
        indent -= 2;
        break;
    default:
        fprintf(stderr, "%s: unrecognized node type %d\n", __func__, node->type);
        break;
    }
}

static void print_ast(Parser *p, const Node *node)
{
    print_ast_indent(p, node, 0);
}

static Token look(Parser *p)
{
    return p->lookahead_cache[p->cache_index];
}

static void error(Parser *p, const char *fmt, ...)
{
    SrcLoc loc = locate_line_col(&p->lexer, look(p).range.start);
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

static void name_table_cleanup(Parser *p)
{
    for (int i = 0; i < NAMETABLE_SIZE; i++) {
        Name *n = p->name_table.keys[i];
        while (n) {
            Name *next = n->next;
            free(n->text);
            free(n);
            n = next;
        }
    }
}

void parser_cleanup(Parser *p)
{
    // Free all nodes.
    for (int i = 0; i < sb_count(p->nodep_buf); i++) {
        Node *node = p->nodep_buf[i];
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
    name_table_cleanup(p);
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
    // make progress
    next(p);
}

static void save_state(Parser *p)
{
    // clear cache except the current lookahead
    p->lookahead_cache[0] = p->lookahead_cache[p->cache_index];
    stb__sbn(p->lookahead_cache) = 1;
    p->cache_index = 0;
}

static void revert_state(Parser *p)
{
    p->cache_index = 0;
}

static Node *parse_return_stmt(Parser *p)
{
    expect(p, TOK_RETURN);
    Node *expr = parse_expr(p);
    if (!expr) {
        error(p, "expected expression");
    }
    return make_return_stmt(p, expr);
}

static Node *parse_stmt(Parser *p)
{
    Node *stmt, *node = NULL;

    save_state(p);

    // Try all possible productions and use the first successful one.
    // We use lookahead (LL(k)) to revert state if a production fails.
    // (See "recursive descent with backtracking":
    // https://en.wikipedia.org/wiki/Recursive_descent_parser)
    switch (look(p).type) {
    case TOK_EOF:
        return NULL;
    case TOK_NEWLINE:
        next(p);
        return parse_stmt(p); // FIXME stack overflow
    case TOK_RETURN:
        stmt = parse_return_stmt(p);
        expect(p, TOK_NEWLINE);
        return stmt;
    default:
        break;
    }

    node = parse_decl(p);
    if (node) {
        stmt = make_decl_stmt(p, node);
        expect(p, TOK_NEWLINE);
        return stmt;
    }
    revert_state(p);

    node = parse_expr(p);
    if (node) {
        stmt = make_expr_stmt(p, node);
        expect(p, TOK_NEWLINE);
        return stmt;
    }
    revert_state(p);

    // no production has succeeded and node is NULL
    return NULL;
}

static Node *parse_compound_stmt(Parser *p)
{
    expect(p, TOK_LBRACE);
    Node *compound = make_compound_stmt(p);
    Node *stmt;
    while ((stmt = parse_stmt(p)) != NULL) {
        sb_push(compound->stmt_buf, stmt);
    }
    expect(p, TOK_RBRACE);
    return compound;
}

static Node *parse_literal_expr(Parser *p)
{
    Node *expr = make_node(p, ND_LITEXPR, look(p));
    next(p);
    return expr;
}

static Node *parse_ref_expr(Parser *p)
{
    Name *name = get_name(p, look(p));
    if (!name) {
        name = push_name(p, look(p));
    }
    next(p);
    return make_ref_expr(p, name);
}

static Node *parse_unary_expr(Parser *p)
{
    Node *expr = NULL;
    Name *name;

    switch (look(p).type) {
    case TOK_IDENT:
        expr = make_node(p, ND_TOKEN, look(p));
        name = get_name(p, look(p));
        if (!name) {
            printf("First seen name, pushing ");
            print_token(&p->lexer, look(p));
            name = push_name(p, look(p));
        } else {
            printf("Found name ");
            print_token(&p->lexer, look(p));
        }
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
static Node *parse_binary_expr_rhs(Parser *p, Node *lhs, int precedence)
{
    while (1) {
        int this_prec = get_precedence(look(p));

        // If the upcoming op has lower precedence, the subexpression of the
        // precedence level that we are currently parsing in is finished.
        // This is equivalent to reducing on a shift/reduce conflict in
        // bottom-up parsing.
        if (this_prec < precedence)
            break;

        Node *op = make_node(p, ND_TOKEN, look(p));
        next(p);

        // Parse the next term.  We do not know yet if this term should bind to
        // LHS or RHS; e.g. "a * b + c" or "a + b * c".  To know this, we should
        // look ahead for the operator that follows this term.
        Node *rhs = parse_unary_expr(p);
        if (!rhs) {
            error(p, "expected expression");
        }
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
static Node *parse_expr(Parser *p)
{
    Node *expr = parse_unary_expr(p);
    expr = parse_binary_expr_rhs(p, expr, 0);
    return expr;
}

static Node *parse_var_decl(Parser *p)
{
    Node *decltype = NULL;
    Node *assign = NULL;

    int mut = look(p).type == TOK_VAR;
    next(p);
    Name *name = get_name(p, look(p));
    if (!name) {
        name = push_name(p, look(p));
    }
    next(p);

    if (!mut) {
        // 'let' declarations require assignment expression
        expect(p, TOK_EQUALS);
        assign = parse_expr(p);
    } else if (look(p).type == TOK_EQUALS) {
        // type inference from assignment expression
        next(p);
        assign = parse_expr(p);
    } else if (look(p).type == TOK_COLON) {
        // explicit type
        next(p);
        decltype = make_node(p, ND_TYPE, look(p));
        next(p);
    }

    if (decltype == NULL && assign == NULL) {
        error(p, "declarations should at least specify its type or its value.");
    }

    return make_vardecl(p, decltype, mut, name, assign);
}

// Parse a declaration.
//
// Decl:
//     VarDecl
//     Function
static Node *parse_decl(Parser *p)
{
    Node *decl;

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

static Node *parse_function(Parser *p)
{
    expect(p, TOK_FN);

    Name *name = get_name(p, look(p));
    if (!name) {
        name = push_name(p, look(p));
    }
    Node *func = make_function(p, name);
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

Node *parse(Parser *p)
{
    printf("sizeof(Node)=%zu\n", sizeof(Node));
    printf("sizeof(Token)=%zu\n", sizeof(Token));
    Node *node = parse_function(p);
    print_ast(p, node);
    return node;
}

static uint64_t hash(const char *text, int len)
{
    uint64_t hash = 5381;
    for (int i = 0; i < len; i++) {
        int c = text[i];
        hash = ((hash << 5) + hash) + c; // hash * 33 + c
        len--;
    }
    return hash;
}

static char *copy_source_range(Parser *p, SrcRange range)
{
    int len = range.end - range.start;
    char *s = calloc(len + 1, sizeof(char));
    memcpy(s, p->lexer.src + range.start, len);
    return s;
}

static Name *push_name(Parser *p, Token tok)
{
    Name *name = calloc(1, sizeof(Name));
    name->text = copy_source_range(p, tok.range);

    size_t len = tok.range.end - tok.range.start;
    int index = hash(name->text, len) % NAMETABLE_SIZE;
    Name **keyp = &p->name_table.keys[index];
    name->next = *keyp;
    *keyp = name;
    return *keyp;
}

static Name *get_name(Parser *p, Token tok)
{
    char *src_text = p->lexer.src + tok.range.start;
    size_t len = tok.range.end - tok.range.start;
    int index = hash(src_text, len) % NAMETABLE_SIZE;
    for (Name *n = p->name_table.keys[index]; n; n = n->next) {
        if (strlen(n->text) == len && !strncmp(n->text, src_text, len)) {
            return n;
        }
    }
    return NULL;
}
