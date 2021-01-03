#include "parser.h"
#include "sds.h"
#include "stretchy_buffer.h"
#include <assert.h>
#include <regex.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

static Node *parse_expr(Parser *p);
static int is_decl_start(Parser *p);
static Node *parse_decl(Parser *p);

static Name *push_name_from_token(Parser *p, Token tok) {
    return push_name(&p->nametable, p->lexer.src + tok.range.start,
                     tok.range.end - tok.range.start);
}

// FIXME: 'tok' is not really used.
static Node *make_node(Parser *p, NodeKind k, Token tok)
{
    // TODO: maybe store all nodes in a contiguous buffer for better locality?
    // Should be careful about node pointers going stale though
    Node *node = calloc(1, sizeof(Node));
    if (!node) {
        fprintf(stderr, "alloc error\n");
        exit(1);
    }
    node->kind = k;
    node->token = tok;
    sb_push(p->nodep_buf, node);
    return node;
}

static Node *makeFile(Parser *p, Node **nodes)
{
	Node *node = make_node(p, ND_FILE, p->tok);
	node->nodes = nodes;
	return node;
}

static Node *makeExprStmt(Parser *p, Node *expr)
{
	Node *node = make_node(p, ND_EXPRSTMT, p->tok);
	node->s.stmt_expr = expr;
	return node;
}

static Node *makeDecl_stmt(Parser *p, Node *decl) {
	Node *node = make_node(p, ND_DECLSTMT, p->tok);
	node->s.decl = decl;
	return node;
}

static Node *makeAssignStmt(Parser *p, Node *lhs, Node *rhs)
{
	Node *node = make_node(p, ND_ASSIGNSTMT, p->tok);
	node->e.lhs = lhs;
	node->e.rhs = rhs;
	return node;
}

static Node *makeRetstmt(Parser *p, Node *expr) {
	Node *node = make_node(p, ND_RETURNSTMT, p->tok);
	node->s.stmt_expr = expr;
	return node;
}

static Node *makeCompoundstmt(Parser *p)
{
	Node *node = make_node(p, ND_COMPOUNDSTMT, p->tok);
	return node;
}

static Node *make_unaryexpr(Parser *p, NodeKind t, Node *expr)
{
	Node *node = make_node(p, t, p->tok);
	node->e.target = expr;
	return node;
}

static Node *make_binexpr(Parser *p, Node *lhs, Token op, Node *rhs)
{
	Node *node = make_node(p, ND_BINEXPR, p->tok);
	node->e.lhs = lhs;
	node->e.op = op;
	node->e.rhs = rhs;
	return node;
}

static Node *makeTypeExpr(Parser *p, Name *name, int ref, Node *base)
{
	Node *node = make_node(p, ND_TYPEEXPR, p->tok);
	node->t.name = name;
	node->t.ref = ref;
	node->t.base = base;
	return node;
}

static Node *make_badtypeexpr(Parser *p)
{
	Node *node = make_node(p, ND_BADTYPEEXPR, p->tok);
	return node;
}

static Node *make_idexpr(Parser *p, Name *name)
{
	Node *node = make_node(p, ND_IDEXPR, p->tok);
	node->e.name = name;
	return node;
}

static Node *make_vardecl(Parser *p, Node *typeexpr, int mutable, Name *name, Node *expr)
{
	Node *node = make_node(p, ND_VARDECL, p->tok);
	node->d.typeexpr = typeexpr;
	node->d.mutable = mutable;
	node->d.name = name;
	node->d.expr = expr;
	return node;
}

static Node *make_paramdecl(Parser *p, Name *name, Node *typeexpr)
{
	Node *node = make_node(p, ND_PARAMDECL, p->tok);
	node->d.name = name;
	node->d.typeexpr = typeexpr;
	return node;
}

static Node *make_funcdecl(Parser *p, Name *name)
{
	Node *node = make_node(p, ND_FUNCDECL, p->tok);
	node->d.name = name;
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

// Print current parsing position. Mainly used for debugging.
static void printLocation(Parser *p)
{
    SrcLoc loc = lexer_locate(&p->lexer, p->tok.range.start);
    printf("loc: %s:%d:%d\n", loc.filename, loc.line, loc.col);
}

static void print_ast_indent(Parser *p, const Node *node, int indent)
{
	if (!node) {
		iprintf(indent, "(null)\n");
		return;
	}

	iprintf(indent, "");

	switch (node->kind) {
	case ND_FILE:
		printf("[File]\n");
		indent += 2;
		for (int i = 0; i < sb_count(node->nodes); i++) {
			print_ast_indent(p, node->nodes[i], indent);
		}
		break;
	case ND_TOKEN:
		tokenPrint(&p->lexer, node->token);
		break;
	case ND_DECLSTMT:
		printf("[DeclStmt]\n");
		indent += 2;
		print_ast_indent(p, node->s.decl, indent);
		indent -= 2;
		break;
	case ND_EXPRSTMT:
		printf("[ExprStmt]\n");
		indent += 2;
		print_ast_indent(p, node->s.stmt_expr, indent);
		indent -= 2;
		break;
	case ND_ASSIGNSTMT:
		printf("[AssignStmt]\n");
		indent += 2;
		print_ast_indent(p, node->e.lhs, indent);
		print_ast_indent(p, node->e.rhs, indent);
		indent -= 2;
		break;
	case ND_RETURNSTMT:
		printf("[ReturnStmt]\n");
		indent += 2;
		print_ast_indent(p, node->s.stmt_expr, indent);
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
	case ND_IDEXPR:
		printf("[IdExpr] '%s'\n", node->e.name->text);
		break;
	case ND_LITEXPR:
		printf("[LiteralExpr] ");
		switch (node->token.type) {
		case TOK_IDENT:
			tokenPrint(&p->lexer, node->token);
			break;
		case TOK_NUM:
			tokenPrint(&p->lexer, node->token);
			break;
		default:
			tokenPrint(&p->lexer, node->token);
			break;
		}
		break;
        case ND_REFEXPR:
		printf("[RefExpr]\n");
		indent += 2;
		print_ast_indent(p, node->e.target, indent);
		indent -= 2;
                break;
	case ND_DEREFEXPR:
		printf("[DerefExpr]\n");
		indent += 2;
		print_ast_indent(p, node->e.target, indent);
		indent -= 2;
		break;
	case ND_BINEXPR:
		printf("[BinaryExpr]\n");
		indent += 2;
		print_ast_indent(p, node->e.lhs, indent);
                tokenPrint(&p->lexer, node->e.op);
		print_ast_indent(p, node->e.rhs, indent);
		indent -= 2;
		break;
	case ND_VARDECL:
		printf("[VarDecl] '%s' %s\n", node->d.name->text,
			   node->d.typeexpr ? node->d.typeexpr->t.name->text : "(null)");
		indent += 2;
		iprintf(indent, "mutable: %d\n", node->d.mutable);
		print_ast_indent(p, node->d.expr, indent);
		indent -= 2;
		break;
	case ND_PARAMDECL:
		printf("[ParamDecl] '%s'\n", node->d.name->text);
		break;
	case ND_FUNCDECL:
		printf("[FuncDecl] '%s'\n", node->d.name->text);
		indent += 2;
		for (int i = 0; i < sb_count(node->paramdecls); i++) {
			print_ast_indent(p, node->paramdecls[i], indent);
		}
		print_ast_indent(p, node->body, indent);
		indent -= 2;
		break;
	default:
		fprintf(stderr, "%s: unrecognized node type %d\n", __func__, node->kind);
		break;
	}
}

void printAst(Parser *p, const Node *node)
{
	print_ast_indent(p, node, 0);
}

// Initialize a Parser that parses the given filename.
void parserInit(Parser *p, const char *filename)
{
    memset(p, 0, sizeof(Parser));
    lexerInit(&p->lexer, filename);
    lexerNext(&p->lexer);
    p->tok = p->lexer.tok;
}

// Initialize a Parser for a predefined string.
void parserInitText(Parser *p, const char *text, size_t len)
{
    memset(p, 0, sizeof(Parser));
    lexerInitText(&p->lexer, text, len);
    lexerNext(&p->lexer);
    p->tok = p->lexer.tok;
}

static void nametable_cleanup(NameTable *nt)
{
    for (int i = 0; i < NAMETABLE_SIZE; i++) {
        Name *n = nt->keys[i];
        while (n) {
            Name *next = n->next;
            free(n->text);
            free(n);
            n = next;
        }
    }
}

void parserCleanup(Parser *p)
{
	// Free all nodes.
	// Some nodes have children nodes, reap them as well.
	for (int i = 0; i < sb_count(p->nodep_buf); i++) {
		Node *node = p->nodep_buf[i];
		if (node) {
			if (node->nodes)
				sb_free(node->nodes);
			if (node->paramdecls)
				sb_free(node->paramdecls);
			free(node);
		}
	}

        for (int i = 0; i < sb_count(p->errors); i++)
            if (p->errors[i].msg)
                free(p->errors[i].msg);
        sb_free(p->errors);

        for (int i = 0; i < sb_count(p->beacons); i++)
            if (p->beacons[i].msg)
                free(p->beacons[i].msg);
        sb_free(p->beacons);

	lexerCleanup(&p->lexer);
	sb_free(p->nodep_buf);
	nametable_cleanup(&p->nametable);
}

static SrcLoc locate(Parser *p)
{
    return lexer_locate(&p->lexer, p->tok.range.start);
}

// In the process, if an error beacon is found in the comment, add the error to
// the parser error list so that it can be compared to the actual errors later
// in the verifying phase.
static void next(Parser *p)
{
    if (p->lexer.tok.type != TOK_EOF) {
        lexerNext(&p->lexer);
        p->tok = p->lexer.tok;

        if (p->tok.type == TOK_COMMENT) {
            char comment_buf[TOKLEN];
            tokenstr(&p->lexer, p->tok, comment_buf, sizeof(comment_buf));

            const char *marker =  "// ERROR: ";
            char *found = strstr(comment_buf, marker);
            if (found == comment_buf) {
                char *regex_start = found + strlen(marker);
                char *msg = malloc(strlen(regex_start) + 1);
                memcpy(msg, regex_start, strlen(regex_start) + 1);
                Error e = {.loc = lexer_locate(&p->lexer, p->tok.range.start),
                           .msg = msg};
                sb_push(p->beacons, e);
            }
        }

        // Push keywords that we come by into the name table.
        if (is_keyword(p->tok)) {
            push_name_from_token(p, p->tok);
        }
    }
}

static void add_error(Parser *p, SrcLoc loc, const char *msg)
{
    int len = strlen(msg);
    char *msgcpy = malloc(len + 1);
    memcpy(msgcpy, msg, len + 1);
    Error error = {.loc = loc, .msg = msgcpy};
    sb_push(p->errors, error);
}

static void error(Parser *p, const char *fmt, ...)
{
	static char msg[1024];
	va_list args;

	va_start(args, fmt);
	vsnprintf(msg, sizeof(msg), fmt, args);
	va_end(args);

	SrcLoc loc = lexer_locate(&p->lexer, p->tok.range.start);
	add_error(p, loc, msg);
}

static void error_print(const Error e) {
  printf("%s:%d:%d: error: %s\n", e.loc.filename, e.loc.line, e.loc.col, e.msg);
}

void parserReportErrors(const Parser *p)
{
    for (int i = 0; i < sb_len(p->errors); i++) {
        SrcLoc loc = p->errors[i].loc;
        const char *msg = p->errors[i].msg;
        fprintf(stderr, "%s:%d:%d: parse error: %s\n", p->lexer.filename,
                loc.line, loc.col, msg);
    }
    exit(1);
}

// Emit general 'expected ..., got ...' message, with the 'expected' part
// customized.
static void error_expected(Parser *p, const char *s) {
    char buf[TOKLEN];

    tokenstr(&p->lexer, p->tok, buf, sizeof(buf));
    // if it's a comment, don't print the actual comment, as it can mess with
    // beacon regex matching.
    error(p, "expected %s, got '%s'", s,
          p->tok.type == TOK_COMMENT ? "comment" : buf);
}

static Token expect(Parser *p, TokenType t) {
    Token tok = p->tok;
    if (p->tok.type != t) {
        sds quoted = sdscatprintf(sdsempty(), "'%s'", token_names[t]);
        error_expected(p, quoted);
        sdsfree(quoted);
    }
    // make progress
    next(p);
    return tok;
}

static int is_eof(const Parser *p) {
    return p->tok.type == TOK_EOF;
}

static int isEndOfLine(const Parser *p) {
    return p->tok.type == TOK_NEWLINE || p->tok.type == TOK_COMMENT;
}

static void expectEndOfLine(Parser *p) {
    if (!isEndOfLine(p)) expect(p, TOK_NEWLINE);
}

static void skip_newlines(Parser *p) {
    while (p->tok.type == TOK_NEWLINE || p->tok.type == TOK_COMMENT) {
        next(p);
    }
}

static void skip_until(Parser *p, TokenType type) {
    while (!is_eof(p) && p->tok.type != type) {
        next(p);
    }
}

static void skip_until_end_of_line(Parser *p) {
    while (!is_eof(p) && p->tok.type != TOK_NEWLINE) {
        next(p);
    }
}

// Assignment statements start with an expression, so we cannot easily
// predetermine whether a statement is just an expression or an assignment
// until we see the '='.  We therefore first parse the expression (LHS) and
// then call this to transform that node into an assignment if needed.
static Node *parseAssignOrExprStmt(Parser *p, Node *expr)
{
    Node *stmt = NULL;
    if (p->tok.type == TOK_EQUALS) {
        next(p);
        Node *rhs = parse_expr(p);
        stmt = makeAssignStmt(p, expr, rhs);
    } else {
        stmt = makeExprStmt(p, expr);
    }
    expectEndOfLine(p);
    skip_until_end_of_line(p);
    return stmt;
}

static Node *parseReturnStmt(Parser *p) {
    expect(p, TOK_RETURN);

    Node *expr = parse_expr(p);
    if (!expr)
        error_expected(p, "expression");

    expectEndOfLine(p);

    return makeRetstmt(p, expr);
}

static Node *parse_stmt(Parser *p) {
    Node *stmt;

    skip_newlines(p);

    // try all possible productions and use the first successful one
    switch (p->tok.type) {
    case TOK_EOF:
    case TOK_RBRACE: // compoundstmt end
        return NULL;
    case TOK_RETURN:
        stmt = parseReturnStmt(p);
        return stmt;
    default:
        break;
    }

    if (is_decl_start(p)) {
        Node *decl = parse_decl(p);
        stmt = makeDecl_stmt(p, decl);
        expectEndOfLine(p);
        return stmt;
    }

    // all productions from now on start with an expression
    Node *expr = parse_expr(p);
    if (expr) return parseAssignOrExprStmt(p, expr);

    // no production has succeeded
    // TODO: unreachable?
    return NULL;
}

static Node *parseCompoundStmt(Parser *p)
{
    expect(p, TOK_LBRACE);

    Node *compound = makeCompoundstmt(p);
    Node *stmt;
    while ((stmt = parse_stmt(p)) != NULL) {
        sb_push(compound->nodes, stmt);
    }

    expect(p, TOK_RBRACE);

    return compound;
}

static Node *parse_litexpr(Parser *p)
{
	Node *expr = make_node(p, ND_LITEXPR, p->tok);
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

static Node *parse_typeexpr(Parser *p) {
    Node *subtype;
    Name *name;
    int ref;

    if (p->tok.type == TOK_STAR) {
        next(p);
        ref = 1;

        subtype = parse_typeexpr(p);
        if (subtype->kind == ND_BADTYPEEXPR) return subtype;

        assert(subtype->t.name);
        name = push_refname(&p->nametable, subtype->t.name);
    } else {
        ref = 0;
        subtype = NULL;

        if (p->tok.type != TOK_IDENT && !is_keyword(p->tok)) {
            error_expected(p, "type name");
            return make_badtypeexpr(p);
        } else if (!is_typename(p->tok)) {
            char buf[TOKLEN];

            tokenstr(&p->lexer, p->tok, buf, sizeof(buf));
            error(p, "invalid type name '%s'", buf);

            return make_badtypeexpr(p);
        }

        name = push_name_from_token(p, p->tok);
        next(p);
    }

    return makeTypeExpr(p, name, ref, subtype);
}

static Node *parse_idexpr(Parser *p)
{
	Name *name = push_name_from_token(p, p->tok);
	next(p);
	return make_idexpr(p, name);
}

// After parsing an identifier, if it turns out to be a function call (i.e.
// left paren is seen), parse the function call. Else, leave it as-is.
static Node *parse_funccall_maybe(Parser *p, Node *expr)
{
    if (p->tok.type != TOK_LPAREN) {
        return expr;
    }

    expect(p, TOK_LPAREN);

    while (p->tok.type != TOK_RPAREN) {
        parse_expr(p);
        if (p->tok.type == TOK_COMMA) {
            next(p);
        }
    }

    expect(p, TOK_RPAREN);

    return expr;
}

static Node *parse_unaryexpr(Parser *p) {
    Node *expr = NULL;

    switch (p->tok.type) {
    case TOK_IDENT:
        expr = parse_idexpr(p);
        expr = parse_funccall_maybe(p, expr);
        break;
    case TOK_NUM:
        expr = parse_litexpr(p);
        break;
    case TOK_AMPERSAND:
        next(p);
        expr = parse_unaryexpr(p);
        expr = make_unaryexpr(p, ND_REFEXPR, expr);
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
//	 UnaryExpr (op BinaryExpr)*
//
// Return the pointer to the node respresenting the reduced binary expression.
static Node *parse_binexpr_rhs(Parser *p, Node *lhs, int precedence) {
    while (1) {
        int this_prec = get_precedence(p->tok);

        // If the upcoming op has lower precedence, the subexpression of the
        // precedence level that we are currently parsing in is finished.
        // This is equivalent to reducing on a shift/reduce conflict in
        // bottom-up parsing.
        if (this_prec < precedence)
            break;

        Token op = p->tok;
        next(p);

        // Parse the next term.  We do not know yet if this term should bind to
        // LHS or RHS; e.g. "a * b + c" or "a + b * c".  To know this, we should
        // look ahead for the operator that follows this term.
        Node *rhs = parse_unaryexpr(p);
        if (!rhs)
            error(p, "expected expression");
        int next_prec = get_precedence(p->tok);

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
//	 Id CallParam?
//	 UnaryExpr
//	 BinaryExpr
//
// CallParam:
//	 (Param)
//
// This grammar requires two or more lookahead, because a single token
// lookahead would not tell us whether it is a single-ID expression or a call
// expression.
static Node *parse_expr(Parser *p) {
    Node *expr = parse_unaryexpr(p);
    expr = parse_binexpr_rhs(p, expr, 0);
    return expr;
}

// Parse a 'ident: type' decl form that appears on struct declarations or
// function argument lists.
static Node *parse_paramdecl(Parser *p) {
    Token tok = expect(p, TOK_IDENT);
    Name *name = push_name_from_token(p, tok);

    if (p->tok.type != TOK_COLON) expect(p, TOK_COLON);
    next(p);
    Node *typeexpr = parse_typeexpr(p);

    return make_paramdecl(p, name, typeexpr);
}

static Node **parse_paramdecllist(Parser *p) {
    Node **list = NULL;

    // assumes enclosed in parentheses
    for (;;) {
        printf("right here\n");

        skip_newlines(p);
        if (p->tok.type == TOK_RPAREN || p->tok.type == TOK_RBRACE) break;

        Node *node = parse_paramdecl(p);
        sb_push(list, node);
        if (p->tok.type == TOK_COMMA) next(p);
    }

    return list;
}

static Node *parse_vardecl(Parser *p)
{
    int mut = (p->tok.type == TOK_VAR);
    next(p);

    Token tok = expect(p, TOK_IDENT);
    Name *name = push_name_from_token(p, tok);

    if (p->tok.type == TOK_COLON) {
        next(p);
        if (!mut)
            error(p, "initial value required");
        Node *typeexpr = parse_typeexpr(p);
        return make_vardecl(p, typeexpr, mut, name, NULL);
    } else if (p->tok.type == TOK_EQUALS) {
        next(p);
        Node *assign = parse_expr(p);
        return make_vardecl(p, NULL, mut, name, assign);
    } else {
        error_expected(p, "':' or '='");
        skip_until_end_of_line(p);
        Node *typeexpr = make_badtypeexpr(p);
        return make_vardecl(p, typeexpr, mut, name, NULL);
    }
}

// Declarations have clear indicator tokens.
static int is_decl_start(Parser *p) {
    switch (p->tok.type) {
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
//	 VarDecl
//	 FuncDecl
static Node *parse_decl(Parser *p)
{
    Node *decl;

    switch (p->tok.type) {
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

static Node *parseFuncDecl(Parser *p) {
    expect(p, TOK_FN);

    Name *name = push_name_from_token(p, p->tok);
    Node *func = make_funcdecl(p, name);
    next(p);

    // argument list
    expect(p, TOK_LPAREN);
    func->paramdecls = parse_paramdecllist(p);
    expect(p, TOK_RPAREN);

    // return type
    func->rettypeexpr = NULL;
    if (p->tok.type == TOK_ARROW) {
        expect(p, TOK_ARROW);
        func->rettypeexpr = parse_typeexpr(p);
    }

    if (p->tok.type != TOK_LBRACE) {
        error_expected(p, "'->' or '{'");
        skip_until(p, TOK_LBRACE);
    }

    func->body = parseCompoundStmt(p);
    // TODO: func->pos = 

    return func;
}

static Node *parse_toplevel(Parser *p)
{
    skip_newlines(p);

    switch (p->tok.type) {
    case TOK_FN:
        return parseFuncDecl(p);
    // case TokenKind::kw_struct:
    //     return parse_struct_decl();
    default:
        assert(0 && "unreachable");
        return NULL;
    }
}

void parser_verify(const Parser *p) {
    int success = 1;
    printf("\033[0;32mtest\033[0m %s:\n", p->lexer.filename);

    int i = 0, j = 0;
    while (i < sb_count(p->errors) && j < sb_count(p->beacons)) {
        if (p->errors[i].loc.line == p->beacons[j].loc.line) {
            sds rs = sdsnew(p->beacons[j].msg);
            sdstrim(rs, "\"");
            regex_t preg;
            if (regcomp(&preg, rs, REG_NOSUB) == 0) {
                int match = regexec(&preg, p->errors[i].msg, 0, NULL, 0);
                if (match != 0) {
                    success = 0;
                    printf("< ");
                    error_print(p->errors[i]);
                    printf("> ");
                    error_print(p->beacons[j]);
                }
                regfree(&preg);
            } else {
                fatal("invalid regex in beacon: %s", p->beacons[j].msg);
            }
            sdsfree(rs);
            i++;
            j++;
        } else if (p->errors[i].loc.line < p->beacons[j].loc.line) {
            success = 0;
            printf("< ");
            error_print(p->errors[i]);
            i++;
        } else {
            success = 0;
            printf("> ");
            error_print(p->beacons[j]);
            j++;
        }
    }
    for (; i < sb_count(p->errors); i++) {
        success = 0;
        printf("< ");
        error_print(p->errors[i]);
    }
    for (; j < sb_count(p->beacons); j++) {
        success = 0;
        printf("> ");
        error_print(p->beacons[j]);
    }

    printf("%s %s\n",
           success ? "\033[0;32msuccess\033[0m" : "\033[0;31mfail\033[0m",
           p->lexer.filename);
}

// Parse a single error beacon ([error: "regex"]).
Error parse_beacon(Parser *p) {
    char *msg;

    expect(p, TOK_LBRACKET);
    expect(p, TOK_ERROR);
    expect(p, TOK_COLON);

    char buf[TOKLEN];
    tokenstr(&p->lexer, p->tok, buf, sizeof(buf));
    size_t len = strlen(buf);
    msg = malloc(len + 1);
    memcpy(msg, buf, len + 1);

    next(p);

    expect(p, TOK_RBRACKET);

    return (Error){
        .loc = {0}, // overrided
        .msg = msg,
    };
}

Node *parse(Parser *p)
{
    Node **nodes = NULL;

    while (p->tok.type != TOK_EOF) {
        Node *func = parse_toplevel(p);
        sb_push(nodes, func);
        skip_newlines(p);
    }

    return makeFile(p, nodes);
}
