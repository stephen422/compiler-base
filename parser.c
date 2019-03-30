#include "parser.h"
#include "stretchy_buffer.h"
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

static Token look(Parser *p);
static Node *parse_expr(Parser *p);
static int is_decl_start(Parser *p);
static Node *parse_decl(Parser *p);
static Name *get_or_push_name(Parser *p, Token tok);

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

static Node *make_file(Parser *p, Node **nodes)
{
    Node *node =make_node(p, ND_FILE, look(p));
    node->nodes = nodes;
    return node;
}

static Node *make_exprstmt(Parser *p, Node *expr)
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

static Node *make_assignstmt(Parser *p, Node *lhs, Node *rhs)
{
    Node *node = make_node(p, ND_ASSIGNSTMT, look(p));
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

static Node *make_retstmt(Parser *p, Node *expr) {
    Node *node = make_node(p, ND_RETURNSTMT, look(p));
    node->expr = expr;
    return node;
}

static Node *make_compoundstmt(Parser *p)
{
    Node *node = make_node(p, ND_COMPOUNDSTMT, look(p));
    return node;
}

static Node *make_unaryexpr(Parser *p, NodeType t, Node *expr)
{
    Node *node = make_node(p, t, look(p));
    node->expr = expr;
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

static Node *make_typeexpr(Parser *p, Name *name)
{
    Node *node = make_node(p, ND_TYPEEXPR, look(p));
    node->name = name;
    return node;
}

static Node *make_refexpr(Parser *p, Name *name)
{
    Node *node = make_node(p, ND_REFEXPR, look(p));
    node->name = name;
    return node;
}

static Node *make_vardecl(Parser *p, Node *typeexpr, int mutable, Name *name, Node *expr)
{
    Node *node = make_node(p, ND_VARDECL, look(p));
    node->typeexpr = typeexpr;
    node->mutable = mutable;
    node->name = name;
    node->expr = expr;
    return node;
}

static Node *make_paramdecl(Parser *p, Name *name, Node *typeexpr)
{
    Node *node = make_node(p, ND_PARAMDECL, look(p));
    node->name = name;
    node->typeexpr = typeexpr;
    return node;
}

static Node *make_funcdecl(Parser *p, Name *name)
{
    Node *node = make_node(p, ND_FUNCDECL, look(p));
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
    case ND_FILE:
        printf("[File]\n");
        indent += 2;
        for (int i = 0; i < sb_count(node->nodes); i++) {
            print_ast_indent(p, node->nodes[i], indent);
        }
        break;
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
    case ND_ASSIGNSTMT:
        printf("[AssignStmt]\n");
        indent += 2;
        print_ast_indent(p, node->lhs, indent);
        print_ast_indent(p, node->rhs, indent);
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
        for (int i = 0; i < sb_count(node->nodes); i++) {
            print_ast_indent(p, node->nodes[i], indent);
        }
        indent -= 2;
        break;
    case ND_REFEXPR:
        printf("[RefExpr] '%s'\n", node->name->text);
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
    case ND_DEREFEXPR:
        printf("[DerefExpr]\n");
        indent += 2;
        print_ast_indent(p, node->expr, indent);
        indent -= 2;
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
        printf("[VarDecl] '%s' %s\n", node->name->text,
               node->typeexpr ? node->typeexpr->name->text : "(null)");
        indent += 2;
        iprintf(indent, "mutable: %d\n", node->mutable);
        print_ast_indent(p, node->expr, indent);
        indent -= 2;
        break;
    case ND_PARAMDECL:
        printf("[ParamDecl] '%s'\n", node->name->text);
        break;
    case ND_FUNCDECL:
        printf("[FuncDecl] '%s'\n", node->name->text);
        indent += 2;
        for (int i = 0; i < sb_count(node->paramdecls); i++) {
            print_ast_indent(p, node->paramdecls[i], indent);
        }
        print_ast_indent(p, node->body, indent);
        indent -= 2;
        break;
    default:
        fprintf(stderr, "%s: unrecognized node type %d\n", __func__, node->type);
        break;
    }
}

void print_ast(Parser *p, const Node *node)
{
    print_ast_indent(p, node, 0);
}

static Token look(Parser *p)
{
    return p->token_cache[p->cache_index];
}

void parser_init(Parser *p, const char *filename)
{
    *p = (const Parser) {0};
    lexer_init(&p->lexer, filename);
    lexer_next(&p->lexer);
    sb_push(p->token_cache, p->lexer.token);
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
            if (node->nodes) {
                sb_free(node->nodes);
            }
            if (node->paramdecls) {
                sb_free(node->paramdecls);
            }
            free(node);
        }
    }
    sb_free(p->nodep_buf);
    sb_free(p->token_cache);
    lexer_free(&p->lexer);
    name_table_cleanup(p);
}

static void next(Parser *p)
{
    if (p->cache_index >= sb_count(p->token_cache) - 1) {
        lexer_next(&p->lexer);
        sb_push(p->token_cache, p->lexer.token);
    }
    p->cache_index++;
}

static void add_error(Parser *p, SrcLoc loc, const char *msg)
{
    Error error = {.loc = loc};
    error.msg = calloc(strlen(msg) + 1, 1);
    strcpy(error.msg, msg);
    sb_push(p->errors, error);
}

static void clear_error(Parser *p)
{
    sb_len(p->errors) = 0;
}

static void error(Parser *p, const char *fmt, ...)
{
    static char msg[1024];
    va_list args;

    va_start(args, fmt);
    vsnprintf(msg, sizeof(msg), fmt, args);
    va_end(args);

    SrcLoc loc = locate_line_col(&p->lexer, look(p).range.start);
    add_error(p, loc, msg);
}

void parser_report_errors(Parser *p)
{
    for (int i = 0; i < sb_len(p->errors); i++) {
        SrcLoc loc = p->errors[i].loc;
        const char *msg = p->errors[i].msg;
        fprintf(stderr, "%s:%d:%d: parse error: %s\n", p->lexer.filename, loc.line, loc.col, msg);
    }
    exit(1);
}

static void error_expected(Parser *p, TokenType t)
{
    error(p, "expected '%s', got '%s'", token_names[t], token_names[look(p).type]);
}

static Token expect(Parser *p, TokenType t)
{
    Token tok = look(p);
    if (look(p).type != t) {
        error_expected(p, t);
    }
    // make progress
    next(p);
    return tok;
}

static int success(Parser *p)
{
    return p->errors == NULL || sb_len(p->errors) == 0;
}

static int get_pos(Parser *p)
{
    return p->cache_index;
}

static void restore_pos(Parser *p, int pos)
{
    p->cache_index = pos;
}

// Assignment statements start with an expression, so it cannot be determined
// whether a statement is an expression or an assignment until the LHS is fully
// parsed.
static Node *parse_assignstmt_or_exprstmt(Parser *p, Node *expr)
{
    if (look(p).type == TOK_EQUALS) {
        next(p);
        Node *rhs = parse_expr(p);
        return make_assignstmt(p, expr, rhs);
    } else {
        return make_exprstmt(p, expr);
    }
}

static Node *parse_returnstmt(Parser *p)
{
    expect(p, TOK_RETURN);
    Node *expr = parse_expr(p);
    if (!expr) {
        error(p, "expected expression");
    }
    expect(p, TOK_NEWLINE);
    return make_retstmt(p, expr);
}

static void skip_invisibles(Parser *p)
{
    while (look(p).type == TOK_NEWLINE) {
        next(p);
    }
}

static Node *parse_stmt(Parser *p)
{
    Node *stmt;

    skip_invisibles(p);

    // try all possible productions and use the first successful one
    switch (look(p).type) {
    case TOK_EOF:
    case TOK_RBRACE: // compoundstmt end
        return NULL;
    case TOK_RETURN:
        stmt = parse_returnstmt(p);
        return stmt;
    default:
        break;
    }

    if (is_decl_start(p)) {
        Node *decl = parse_decl(p);
        stmt = make_decl_stmt(p, decl);
        expect(p, TOK_NEWLINE);
        return stmt;
    }

    // all productions below start with an expression
    Node *expr = parse_expr(p);
    if (expr) {
        return parse_assignstmt_or_exprstmt(p, expr);
    }

    // no production has succeeded
    return NULL;
}

static Node *parse_compoundstmt(Parser *p)
{
    expect(p, TOK_LBRACE);
    Node *compound = make_compoundstmt(p);
    Node *stmt;
    while ((stmt = parse_stmt(p)) != NULL) {
        if (!success(p)) {
            parser_report_errors(p);
        }
        sb_push(compound->nodes, stmt);
    }
    expect(p, TOK_RBRACE);
    return compound;
}

static Node *parse_litexpr(Parser *p)
{
    Node *expr = make_node(p, ND_LITEXPR, look(p));
    next(p);
    return expr;
}

static int is_typename(Token tok)
{
    switch (tok.type) {
    case TOK_INT:
    case TOK_IDENT:
        return 1;
    default:
        return 0;
    }
}

static Node *parse_typeexpr(Parser *p)
{
    if (!is_typename(look(p))) {
        error(p, "invalid type name '%s'", token_names[look(p).type]);
        return NULL;
    }
    Name *name = get_or_push_name(p, look(p));
    next(p);
    return make_typeexpr(p, name);
}

static Node *parse_refexpr(Parser *p)
{
    Name *name = get_or_push_name(p, look(p));
    next(p);
    return make_refexpr(p, name);
}

static Node *parse_unaryexpr(Parser *p)
{
    Node *expr = NULL;

    switch (look(p).type) {
    case TOK_IDENT:
        expr = parse_refexpr(p);
        break;
    case TOK_NUM:
        expr = parse_litexpr(p);
        break;
    case TOK_STAR:
        next(p);
        expr = parse_unaryexpr(p);
        expr = make_unaryexpr(p, ND_DEREFEXPR, expr);
        break;
    case TOK_LPAREN:
        expect(p, TOK_LPAREN);
        expr = parse_expr(p);
        expect(p, TOK_RPAREN);
        break;
    default:
        error(p, "expected an expression");
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
static Node *parse_binexpr_rhs(Parser *p, Node *lhs, int precedence)
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
        Node *rhs = parse_unaryexpr(p);
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
            rhs = parse_binexpr_rhs(p, rhs, precedence + 1);
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
    Node *expr = parse_unaryexpr(p);
    expr = parse_binexpr_rhs(p, expr, 0);
    return expr;
}

static int is_paramdecl_start(Parser *p)
{
    return look(p).type == TOK_IDENT;
}

static Node *parse_paramdecl(Parser *p)
{
    Token tok = expect(p, TOK_IDENT);
    Name *name = get_or_push_name(p, tok);

    if (look(p).type != TOK_COLON) {
        error_expected(p, TOK_COLON);
        return NULL;
    }
    next(p);
    Node *typeexpr = parse_typeexpr(p);

    return make_paramdecl(p, name, typeexpr);
}

static Node **parse_paramdecllist(Parser *p)
{
    Node **list = NULL;
    // assumes enclosed in parentheses
    while (look(p).type != TOK_RPAREN) {
        Node *node = parse_paramdecl(p);
        if (!success(p)) {
            return NULL;
        }
        sb_push(list, node);
        if (look(p).type == TOK_COMMA) {
            next(p);
        }
    }
    return list;
}

static Node *parse_vardecl(Parser *p)
{
    int mut = (look(p).type == TOK_VAR);
    next(p);

    Token tok = expect(p, TOK_IDENT);
    Name *name = get_or_push_name(p, tok);

    if (look(p).type == TOK_COLON) {
        next(p);
        // 'let' cannot be used with explicit type specfication
        if (!mut) {
            error(p, "initial value required");
        }
        Node *typeexpr = parse_typeexpr(p);
        return make_vardecl(p, typeexpr, mut, name, NULL);
    } else {
        expect(p, TOK_EQUALS);
        Node *assign = parse_expr(p);
        return make_vardecl(p, NULL, mut, name, assign);
    }

}

// Declarations have clear indicator tokens.
static int is_decl_start(Parser *p)
{
    switch (look(p).type) {
    case TOK_LET:
    case TOK_VAR:
        return 1;
    default:
        return 0;
    }
}

// Parse a declaration.
//
// Decl:
//     VarDecl
//     FuncDecl
static Node *parse_decl(Parser *p)
{
    Node *decl;

    switch (look(p).type) {
    case TOK_LET:
    case TOK_VAR:
        decl = parse_vardecl(p);
        break;
    default:
        error(p, "not a start of declaration");
        decl = NULL;
        break;
    }
    return decl;
}

static Node *parse_funcdecl(Parser *p)
{
    if (look(p).type != TOK_FN) {
        return NULL;
    }
    next(p);

    Name *name = get_or_push_name(p, look(p));
    Node *func = make_funcdecl(p, name);
    next(p);

    // parameter list
    expect(p, TOK_LPAREN);
    func->paramdecls = parse_paramdecllist(p);
    expect(p, TOK_RPAREN);

    // return type
    expect(p, TOK_ARROW);
    func->return_type = look(p);
    next(p);

    func->body = parse_compoundstmt(p);

    return func;
}

Node *parse(Parser *p)
{
    Node **nodes = NULL;
    Node *func;
    skip_invisibles(p);
    while ((func = parse_funcdecl(p))) {
        sb_push(nodes, func);
        skip_invisibles(p);
    }
    return make_file(p, nodes);
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

static Name *get_or_push_name(Parser *p, Token tok)
{
    Name *name = get_name(p, tok);
    if (!name) {
        name = push_name(p, tok);
    }
    return name;
}
