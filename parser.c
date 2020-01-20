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

static Node *parseExpr(Parser *p);
static int isDeclStart(Parser *p);
static Node *parseDecl(Parser *p);

static Name *push_name_from_token(Parser *p, Token tok)
{
	return push_name(&p->nametable, p->lexer.src + tok.range.start, tok.range.end - tok.range.start);
}

static Node *makeNode(Parser *p, NodeKind k, Token tok)
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
	Node *node = makeNode(p, ND_FILE, p->tok);
	node->nodes = nodes;
	return node;
}

static Node *makeExprStmt(Parser *p, Node *expr)
{
	Node *node = makeNode(p, ND_EXPRSTMT, p->tok);
	node->expr = expr;
	return node;
}

static Node *makeDecl_stmt(Parser *p, Node *decl) {
	Node *node = makeNode(p, ND_DECLSTMT, p->tok);
	node->decl = decl;
	return node;
}

static Node *makeAssignStmt(Parser *p, Node *lhs, Node *rhs)
{
	Node *node = makeNode(p, ND_ASSIGNSTMT, p->tok);
	node->lhs = lhs;
	node->rhs = rhs;
	return node;
}

static Node *makeRetstmt(Parser *p, Node *expr) {
	Node *node = makeNode(p, ND_RETURNSTMT, p->tok);
	node->expr = expr;
	return node;
}

static Node *makeCompoundstmt(Parser *p)
{
	Node *node = makeNode(p, ND_COMPOUNDSTMT, p->tok);
	return node;
}

static Node *makeUnaryexpr(Parser *p, NodeKind t, Node *expr)
{
	Node *node = makeNode(p, t, p->tok);
	node->expr = expr;
	return node;
}

static Node *makeBinexpr(Parser *p, Node *lhs, Node *op, Node *rhs)
{
	Node *node = makeNode(p, ND_BINEXPR, p->tok);
	node->lhs = lhs;
	node->op = op;
	node->rhs = rhs;
	return node;
}

static Node *makeTypeExpr(Parser *p, Name *name, int ref, Node *canon)
{
	Node *node = makeNode(p, ND_TYPEEXPR, p->tok);
	node->name = name;
	node->ref = ref;
	node->typeexpr = canon;
	return node;
}

static Node *makeBadTypeExpr(Parser *p)
{
	Node *node = makeNode(p, ND_BADTYPEEXPR, p->tok);
	return node;
}

static Node *makeIdexpr(Parser *p, Name *name)
{
	Node *node = makeNode(p, ND_IDEXPR, p->tok);
	node->name = name;
	return node;
}

static Node *makeVarDecl(Parser *p, Node *typeexpr, int mutable, Name *name, Node *expr)
{
	Node *node = makeNode(p, ND_VARDECL, p->tok);
	node->typeexpr = typeexpr;
	node->mutable = mutable;
	node->name = name;
	node->expr = expr;
	return node;
}

static Node *makeParamdecl(Parser *p, Name *name, Node *typeexpr)
{
	Node *node = makeNode(p, ND_PARAMDECL, p->tok);
	node->name = name;
	node->typeexpr = typeexpr;
	return node;
}

static Node *makeFuncdecl(Parser *p, Name *name)
{
	Node *node = makeNode(p, ND_FUNCDECL, p->tok);
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

// Print current parsing position. Mainly used for debugging.
static void printLocation(Parser *p)
{
    sds s = srcLocString(locate(&p->lexer, p->tok.range.start));
    printf("loc: %s\n", s);
    sdsfree(s);
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
	case ND_IDEXPR:
		printf("[IdExpr] '%s'\n", node->name->text);
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
		print_ast_indent(p, node->expr, indent);
		indent -= 2;
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

// In the process, if an error beacon is found in the comment, add the error to
// the parser error list so that it can be compared to the actual errors later
// in the verifying phase.
static void next(Parser *p) {
    if (p->lexer.tok.type != TOK_EOF) {
        lexerNext(&p->lexer);
        p->tok = p->lexer.tok;

        if (p->tok.type == TOK_COMMENT) {
            sds text = tokenString(&p->lexer, p->tok);
            char *found = strstr(text, "[error:");
            if (found) {
                Parser p0;
                parserInitText(&p0, found, strlen(found));
                Error e = parseErrorBeacon(&p0);
                parserCleanup(&p0);

                // override loc
                e.loc = locate(&p->lexer, p->tok.range.start);
                sb_push(p->beacons, e);
            }
            sdsfree(text);
        }

        // Push keywords that we come by into the name table.
        if (isKeyword(p->tok))
            push_name_from_token(p, p->tok);
    }
}

static void addError(Parser *p, SrcLoc loc, const char *msg)
{
    Error error = {.loc = loc, .msg = strdup(msg)};
    sb_push(p->errors, error);
}

static void error(Parser *p, const char *fmt, ...)
{
	static char msg[1024];
	va_list args;

	va_start(args, fmt);
	vsnprintf(msg, sizeof(msg), fmt, args);
	va_end(args);

	SrcLoc loc = locate(&p->lexer, p->tok.range.start);
	addError(p, loc, msg);
}

static sds errorString(const Error e)
{
    sds s = srcLocString(e.loc);
    s = sdscatprintf(s, ": parse error: %s", e.msg);
    return s;
}

void parserReportErrors(const Parser *p) {
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
static void errorExpected(Parser *p, const char *s) {
    sds ts = tokenString(&p->lexer, p->tok);
    // if it's a comment, don't print the actual comment, as it can mess with
    // beacon regex matching.
    error(p, "expected %s, got '%s'", s,
          p->tok.type == TOK_COMMENT ? "comment" : ts);
    sdsfree(ts);
}

static Token expect(Parser *p, TokenType t) {
    Token tok = p->tok;
    if (p->tok.type != t) {
        sds quoted = sdscatprintf(sdsempty(), "'%s'", token_names[t]);
        errorExpected(p, quoted);
        sdsfree(quoted);
    }
    // make progress
    next(p);
    return tok;
}

void expectEndOfLine(Parser *p) {
    if (p->tok.type != TOK_NEWLINE && p->tok.type != TOK_COMMENT)
        // FIXME error message
        expect(p, TOK_NEWLINE);
    // make progress
    next(p);
}

// Assignment statements start with an expression, so we cannot easily
// predetermine whether a statement is just an expression or an assignment
// until we see the '='.  We therefore first parse the expression (LHS) and
// then call this to transform that node into an assignment if needed.
static Node *parseAssignOrExprStmt(Parser *p, Node *expr) {
    if (p->tok.type == TOK_EQUALS) {
        next(p);
        Node *rhs = parseExpr(p);
        return makeAssignStmt(p, expr, rhs);
    } else {
        return makeExprStmt(p, expr);
    }
}

static Node *parse_returnstmt(Parser *p)
{
	expect(p, TOK_RETURN);

	Node *expr = parseExpr(p);
	if (!expr)
		error(p, "expected expression");

        expectEndOfLine(p);

	return makeRetstmt(p, expr);
}

static void skipInvisibles(Parser *p)
{
    while (p->tok.type == TOK_NEWLINE || p->tok.type == TOK_COMMENT)
        next(p);
}

static void skipToEndOfLine(Parser *p)
{
    while (p->tok.type != TOK_NEWLINE)
        next(p);
}

static Node *parse_stmt(Parser *p)
{
    Node *stmt;

    skipInvisibles(p);

    // try all possible productions and use the first successful one
    switch (p->tok.type) {
    case TOK_EOF:
    case TOK_RBRACE: // compoundstmt end
        return NULL;
    case TOK_RETURN:
        stmt = parse_returnstmt(p);
        return stmt;
    default:
        break;
    }

    if (isDeclStart(p)) {
        Node *decl = parseDecl(p);
        stmt = makeDecl_stmt(p, decl);
        expectEndOfLine(p);
        return stmt;
    }

    // all productions from now on start with an expression
    Node *expr = parseExpr(p);
    if (expr)
        return parseAssignOrExprStmt(p, expr);

    // no production has succeeded
    // TODO: unreachable?
    return NULL;
}

static Node *parseCompoundStmt(Parser *p)
{
    expect(p, TOK_LBRACE);

    Node *compound = makeCompoundstmt(p);
    Node *stmt;
    while ((stmt = parse_stmt(p)) != NULL)
        sb_push(compound->nodes, stmt);

    expect(p, TOK_RBRACE);

    return compound;
}

static Node *parse_litexpr(Parser *p)
{
	Node *expr = makeNode(p, ND_LITEXPR, p->tok);
	next(p);
	return expr;
}

static int isTypeName(Token tok)
{
    switch (tok.type) {
    case TOK_INT:
    case TOK_IDENT:
        return 1;
    default:
        return 0;
    }
}

static Node *parseTypeExpr(Parser *p) {
    Node *subexpr;
    Name *name;
    int ref;

    if (p->tok.type == TOK_STAR) {
        next(p);
        ref = 1;
        subexpr = parseTypeExpr(p);
        name = push_refname(&p->nametable, subexpr->name);
    } else {
        ref = 0;
        subexpr = NULL;

        if (p->tok.type != TOK_IDENT && !isKeyword(p->tok)) {
            errorExpected(p, "type name");
        } else if (!isTypeName(p->tok)) {
            sds ts = tokenString(&p->lexer, p->tok);
            error(p, "invalid type name '%s'", ts);
            sdsfree(ts);
        }

        name = push_name_from_token(p, p->tok);
        next(p);
    }

    return makeTypeExpr(p, name, ref, subexpr);
}

static Node *parse_idexpr(Parser *p)
{
	Name *name = push_name_from_token(p, p->tok);
	next(p);
	return makeIdexpr(p, name);
}

static Node *parseUnaryExpr(Parser *p) {
    Node *expr = NULL;

    switch (p->tok.type) {
    case TOK_IDENT:
        expr = parse_idexpr(p);
        break;
    case TOK_NUM:
        expr = parse_litexpr(p);
        break;
    case TOK_AMPERSAND:
        next(p);
        expr = parseUnaryExpr(p);
        expr = makeUnaryexpr(p, ND_REFEXPR, expr);
        break;
    case TOK_STAR:
        next(p);
        expr = parseUnaryExpr(p);
        expr = makeUnaryexpr(p, ND_DEREFEXPR, expr);
        break;
    case TOK_LPAREN:
        expect(p, TOK_LPAREN);
        expr = parseExpr(p);
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
static Node *parse_binexpr_rhs(Parser *p, Node *lhs, int precedence)
{
	while (1) {
		int this_prec = get_precedence(p->tok);

		// If the upcoming op has lower precedence, the subexpression of the
		// precedence level that we are currently parsing in is finished.
		// This is equivalent to reducing on a shift/reduce conflict in
		// bottom-up parsing.
		if (this_prec < precedence)
			break;

		Node *op = makeNode(p, ND_TOKEN, p->tok);
		next(p);

		// Parse the next term.  We do not know yet if this term should bind to
		// LHS or RHS; e.g. "a * b + c" or "a + b * c".  To know this, we should
		// look ahead for the operator that follows this term.
		Node *rhs = parseUnaryExpr(p);
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
		lhs = makeBinexpr(p, lhs, op, rhs);
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
static Node *parseExpr(Parser *p) {
    Node *expr = parseUnaryExpr(p);
    expr = parse_binexpr_rhs(p, expr, 0);
    return expr;
}

static Node *parse_paramdecl(Parser *p)
{
    Token tok = expect(p, TOK_IDENT);
    Name *name = push_name_from_token(p, tok);

    if (p->tok.type != TOK_COLON)
        expect(p, TOK_COLON);
    next(p);
    Node *typeexpr = parseTypeExpr(p);

    return makeParamdecl(p, name, typeexpr);
}

static Node **parse_paramdecllist(Parser *p)
{
    Node **list = NULL;

    // assumes enclosed in parentheses
    while (p->tok.type != TOK_RPAREN) {
        Node *node = parse_paramdecl(p);
        sb_push(list, node);
        if (p->tok.type == TOK_COMMA)
            next(p);
    }

    return list;
}

static Node *parseVarDecl(Parser *p)
{
    int mut = (p->tok.type == TOK_VAR);
    next(p);

    Token tok = expect(p, TOK_IDENT);
    Name *name = push_name_from_token(p, tok);

    if (p->tok.type == TOK_COLON) {
        next(p);
        if (!mut)
            error(p, "initial value required");
        Node *typeexpr = parseTypeExpr(p);
        return makeVarDecl(p, typeexpr, mut, name, NULL);
    } else if (p->tok.type == TOK_EQUALS) {
        next(p);
        Node *assign = parseExpr(p);
        return makeVarDecl(p, NULL, mut, name, assign);
    } else {
        errorExpected(p, "':' or '='");
        skipToEndOfLine(p);
        Node *typeexpr = makeBadTypeExpr(p);
        return makeVarDecl(p, typeexpr, mut, name, NULL);
    }
}

// Declarations have clear indicator tokens.
static int isDeclStart(Parser *p) {
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
static Node *parseDecl(Parser *p)
{
	Node *decl;

	switch (p->tok.type) {
	case TOK_LET:
	case TOK_VAR:
		decl = parseVarDecl(p);
		break;
	default:
		error(p, "not a start of declaration");
		decl = NULL;
		break;
	}
	return decl;
}

static Node *parseFuncDecl(Parser *p)
{
    expect(p, TOK_FN);

    Name *name = push_name_from_token(p, p->tok);
    Node *func = makeFuncdecl(p, name);
    next(p);

    // parameter list
    expect(p, TOK_LPAREN);
    func->paramdecls = parse_paramdecllist(p);
    expect(p, TOK_RPAREN);

    // return type
    if (p->tok.type == TOK_ARROW) {
        expect(p, TOK_ARROW);
        func->rettypeexpr = parseTypeExpr(p);
    } else {
        func->rettypeexpr = NULL;
    }

    func->body = parseCompoundStmt(p);

    return func;
}

static Node *parseTopLevel(Parser *p)
{
    skipInvisibles(p);

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

void parserVerify(const Parser *p) {
    int success = 1;
    printf("\033[0;32mtest\033[0m %s:\n", p->lexer.filename);

    int i = 0, j = 0;
    while (i < sb_count(p->errors) || j < sb_count(p->beacons)) {
        int bothInRange = i < sb_count(p->errors) && j < sb_count(p->beacons);
        if (bothInRange && p->errors[i].loc.line == p->beacons[j].loc.line) {
            success = 0;
            sds rs = sdsnew(p->beacons[j].msg);
            sdstrim(rs, "\"");
            regex_t preg;
            if (regcomp(&preg, rs, REG_NOSUB) == 0) {
                int match = regexec(&preg, p->errors[i].msg, 0, NULL, 0);
                if (match != 0) {
                    sds es = errorString(p->errors[i]);
                    sds bs = errorString(p->beacons[j]);
                    printf("< %s\n> %s\n", es, bs);
                    sdsfree(es);
                    sdsfree(bs);
                }
                regfree(&preg);
            } else {
                fatal("invalid regex in beacon: %s", p->beacons[j].msg);
            }
            sdsfree(rs);
            i++;
            j++;
        } else if ((bothInRange &&
                    p->errors[i].loc.line < p->beacons[j].loc.line) ||
                   i < sb_count(p->errors)) {
            success = 0;
            sds es = errorString(p->errors[i]);
            printf("< %s\n", es);
            sdsfree(es);
            i++;
        } else if ((bothInRange &&
                    p->errors[i].loc.line > p->beacons[j].loc.line) ||
                   j < sb_count(p->beacons)) {
            success = 0;
            sds bs = errorString(p->beacons[j]);
            printf("> %s\n", bs);
            sdsfree(bs);
            j++;
        }
    }

    printf("%s %s\n", success ? "\033[0;32msuccess\033[0m" : "\033[0;31mfail\033[0m", p->lexer.filename);
}

// Parse a single error beacon ([error: "regex"]).
Error parseErrorBeacon(Parser *p) {
    expect(p, TOK_LBRACKET);
    expect(p, TOK_ERROR);
    expect(p, TOK_COLON);

    sds s = tokenString(&p->lexer, p->tok);
    char *msg = strdup(s);
    sdsfree(s);

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
        Node *func = parseTopLevel(p);
        sb_push(nodes, func);
        skipInvisibles(p);
    }

    return makeFile(p, nodes);
}
