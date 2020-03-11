#include "parser.h"
#include "fmt/core.h"
#include <cassert>
#include <regex>

namespace cmp {

template <typename T> using Res = ParserResult<T>;

Parser::Parser(Lexer &&l) : lexer{std::move(l)} {
    tok = lexer.lex();
    // insert keywords in name table
    for (auto m : keyword_map)
        names.get_or_add(std::string{m.first});
}

// Construct directly from a Source for convenience.  Note that 'src' should
// live longer than the Parser.
Parser::Parser(const Source &src) : Parser(Lexer{src}) {}

void Parser::error(const std::string &msg) {
    errors.push_back({locate(), msg});
}

void Parser::errorExpected(const std::string &msg) {
    std::string s = fmt::format("expected {}, found '{}'", msg, tok.str());
    error(s);
}

void Parser::next() {
    if (tok.kind == TokenKind::eos)
        return;

    tok = lexer.lex();

    // If an error beacon is found in a comment, add the error to the parser
    // error list so that it can be compared to the actual errors later in the
    // verifying phase.
    if (tok.kind == TokenKind::comment) {
        std::string_view marker{"[error:"};
        auto found = tok.text.find(marker);
        if (found != std::string_view::npos) {
            auto bracket = tok.text.substr(found);
            Source s{std::string{bracket}};
            Parser p{s};
            auto v = p.parseErrorBeacon();
            // override location
            for (auto &e : v) {
                e.loc = locate();
                beacons.push_back(e);
            }
        }
    }
}

bool Parser::expect(TokenKind kind, const std::string &msg = "") {
    if (tok.kind != kind) {
        std::string s = msg;
        if (msg.empty())
            s = fmt::format("expected '{}', found '{}'",
                            tokenTypeToString(kind), tok.text);
        error(s);
        return false;
    }
    next();
    return true;
}

bool Parser::is_end_of_stmt() const {
    return tok.kind == TokenKind::newline || tok.kind == TokenKind::comment;
}

bool Parser::is_eos() {
    skip_newlines();
    return tok.kind == TokenKind::eos;
}

// Parse a statement.
//
// Stmt:
//     Decl
//     Expr
Stmt *Parser::parse_stmt() {
    Stmt *stmt = nullptr;

    if (tok.kind == TokenKind::kw_return) {
        stmt = parse_return_stmt();
    } else if (isStartOfDecl()) {
        stmt = parse_decl_stmt();
    } else {
        stmt = parse_expr_or_assign_stmt();
    }
    skip_newlines();

    return stmt;
}

Stmt *Parser::parse_return_stmt() {
    auto pos = tok.pos;

    expect(TokenKind::kw_return);

    // optional
    Expr *expr = nullptr;
    if (!is_end_of_stmt()) {
        expr = parse_expr();
    }
    if (!is_end_of_stmt()) {
        skip_until_end_of_line();
        return makeNodeWithPos<BadStmt>(pos);
    }
    return makeNodeWithPos<ReturnStmt>(pos, expr);
}

// let a = ...
DeclStmt *Parser::parse_decl_stmt() {
    auto decl = parseDecl();
    if (!is_end_of_stmt()) {
        if (decl->kind != AstKind::bad_decl)
            expect(TokenKind::newline);
        // try to recover
        skip_until_end_of_line();
    }
    return makeNode<DeclStmt>(decl);
}

Stmt *Parser::parse_expr_or_assign_stmt() {
    auto pos = tok.pos;

    auto lhs = parse_expr();
    // ExprStmt: expression ends with a newline
    if (is_end_of_stmt()) {
        skip_until_end_of_line();
        return makeNode<ExprStmt>(lhs);
    }

    // AssignStmt: expression is followed by equals
    // (anything else is treated as an error)
    if (!expect(TokenKind::equals)) {
        skip_until_end_of_line();
        return makeNodeWithPos<BadStmt>(pos);
    }

    // At this point, it becomes certain that this is an assignment statement,
    // and so we can safely unwrap for RHS.
    auto rhs = parse_expr();
    return makeNodeWithPos<AssignStmt>(pos, lhs, rhs);
}

// Compound statement is a scoped block that consists of multiple statements.
// There is no restriction in order such as variable declarations should come
// first, etc.
//
// CompoundStmt:
//     { Stmt* }
CompoundStmt *Parser::parse_compound_stmt() {
    expect(TokenKind::lbrace);
    auto compound = makeNode<CompoundStmt>();

    while (true) {
        skip_newlines();
        if (tok.kind == TokenKind::rbrace)
            break;
        auto stmt = parse_stmt();
        compound->stmts.push_back(stmt);
    }

    expect(TokenKind::rbrace);
    return compound;
}

// Doesn't include 'let' or 'var'.
DeclNode *Parser::parseVarDecl(VarDeclNode::Kind kind) {
    auto pos = tok.pos;

    Name *name = names.get_or_add(std::string{tok.text});
    next();

    DeclNode *v = nullptr;
    // '=' comes either first, or after the ': type' part.
    if (tok.kind == TokenKind::colon) {
        next();
        auto type_expr = parseTypeExpr();
        v = makeNodeWithPos<VarDeclNode>(pos, name, kind, type_expr, nullptr);
    }
    if (tok.kind == TokenKind::equals) {
        next();
        auto assign_expr = parse_expr();
        if (v)
            static_cast<VarDeclNode *>(v)->assign_expr = assign_expr;
        else
            v = makeNodeWithPos<VarDeclNode>(pos, name, kind, nullptr, assign_expr);
    }
    if (!v) {
        errorExpected("'=' or ':' after var name");
        v = makeNodeWithPos<BadDeclNode>(pos);
    }
    return v;
}

// Parses function argument lists and struct declaration member lists (if
// 'isStruct' == true).
// Doesn't parse the enclosing parentheses or braces.
std::vector<DeclNode *> Parser::parseVarDeclList(VarDeclNode::Kind kind) {
    std::vector<DeclNode *> decls;

    while (true) {
        DeclNode *decl = nullptr;
        skip_newlines();
        if (tok.kind != TokenKind::ident)
            break;

        decl = parseVarDecl(kind);
        decls.push_back(decl);

        if (decl->kind == AstKind::bad_decl) {
            // Determining where each decl ends in a list is a little tricky.
            // Here, we stop for any token that is either (1) separator tokens,
            // i.e. comma, newline, or (2) used to enclose a decl list, i.e.
            // parentheses and braces.  This works for both function argument
            // lists and struct member lists.
            skip_until_any({TokenKind::comma, TokenKind::newline,
                          TokenKind::rparen, TokenKind::rbrace});
        }
        if (tok.kind == TokenKind::comma) {
            next();
        }
    }
    skip_newlines();

    return decls;
}

StructDeclNode *Parser::parseStructDecl() {
    auto pos = tok.pos;

    expect(TokenKind::kw_struct);

    if (tok.kind != TokenKind::ident)
        errorExpected("an identifier");
    Name *name = names.get_or_add(std::string{tok.text});
    next();

    if (!expect(TokenKind::lbrace))
        skip_until_end_of_line();
    auto fields = parseVarDeclList(VarDeclNode::struct_);
    expect(TokenKind::rbrace, "unterminated struct declaration");
    // TODO: recover

    return makeNodeWithPos<StructDeclNode>(pos, name, fields);
}

FuncDeclNode *Parser::parseFuncDecl() {
    auto pos = tok.pos;

    expect(TokenKind::kw_func);

    Name *name = names.get_or_add(std::string{tok.text});
    auto func = makeNode<FuncDeclNode>(name);
    func->pos = tok.pos;
    next();

    // argument list
    expect(TokenKind::lparen);
    func->args = parseVarDeclList(VarDeclNode::func);
    expect(TokenKind::rparen);

    // return type (-> ...)
    if (tok.kind == TokenKind::arrow) {
        next();
        func->ret_type_expr = parseTypeExpr();
    }
    if (tok.kind != TokenKind::lbrace) {
        errorExpected("'->' or '{'");
        skip_until(TokenKind::lbrace);
    }
    // function body
    func->body = parse_compound_stmt();
    func->pos = pos;

    return func;
}

bool Parser::isStartOfDecl() const {
    switch (tok.kind) {
    case TokenKind::kw_let:
    case TokenKind::kw_var:
        return true;
    default:
        return false;
    }
}

// 'let a = ...'
DeclNode *Parser::parseDecl() {
    switch (tok.kind) {
    case TokenKind::kw_let:
        next();
        return parseVarDecl(VarDeclNode::local);
    // TODO: 'var'
    default:
        assert(false && "not a start of a declaration");
    }
    // unreachable
    return nullptr;
}

UnaryExpr *Parser::parseLiteralExpr() {
    UnaryExpr *expr = nullptr;
    // TODO Literals other than integers?
    switch (tok.kind) {
    case TokenKind::number: {
        std::string s{tok.text};
        int value = std::stoi(s);
        expr = makeNode<IntegerLiteral>(value);
        break;
    }
    case TokenKind::string:
        expr = makeNode<StringLiteral>(tok.text);
        break;
    default:
        assert(false && "non-integer literals not implemented");
    }
    expr->pos = tok.pos;

    next();

    return expr;
}

// Upon seeing an expression that starts with an identifier, we don't know
// whether it is just a variable, a function call, or struct initialization
// without lookahead ('a' vs 'a()' vs 'a {...}'). Rather than using lookahead,
// parse the both kinds in one go in this function.
//
// TODO: maybe name it parse_ident_start_exprs?
// TODO: add struct declaration here, e.g. Car {}
Expr *Parser::parseFuncCallOrDeclRefExpr() {
    auto pos = tok.pos;
    assert(tok.kind == TokenKind::ident);
    auto name = names.get_or_add(std::string{tok.text});
    next();

    if (tok.kind == TokenKind::lparen) {
        expect(TokenKind::lparen);
        std::vector<Expr *> args;
        while (tok.kind != TokenKind::rparen) {
            args.push_back(parse_expr());
            if (tok.kind == TokenKind::comma)
                next();
        }
        expect(TokenKind::rparen);
        return makeNodeWithPos<FuncCallExpr>(pos, name, args);
    } else {
        return makeNodeWithPos<DeclRefExpr>(pos, name);
    }
}

bool Parser::isStartOfTypeExpr() const {
    return tok.kind == TokenKind::ampersand || is_identifier_or_keyword(tok);
}

// Parse a type expression.
// A type expression is simply every stream of tokens in the source that can
// represent a type.
//
// TypeExpr:
//     '&' TypeExpr
//     'mut'? ident
Expr *Parser::parseTypeExpr() {
    auto typeexpr = makeNode<TypeExpr>();

    typeexpr->pos = tok.pos;

    // Encode each type into a unique Name, so that they are easy to find in
    // the type table in the semantic analysis phase.
    std::string text;
    if (tok.kind == TokenKind::ampersand) {
        next();
        typeexpr->ref = true;
        typeexpr->subexpr = parseTypeExpr();
        if (typeexpr->subexpr->kind == AstKind::type_expr)
            text = "&" + static_cast<TypeExpr *>(typeexpr->subexpr)->name->text;
    } else if (is_identifier_or_keyword(tok)) {
        if (tok.kind == TokenKind::kw_mut) {
            expect(TokenKind::kw_mut);
            typeexpr->mut = true;
        }
        if (!is_identifier_or_keyword(tok)) {
            // FIXME: type name? expression?
            errorExpected("type name");
            return makeNodeWithPos<BadExpr>(typeexpr->pos);
        }
        typeexpr->ref = false;
        typeexpr->subexpr = nullptr;
        text = tok.text;
        next();
    } else {
        errorExpected("type expression");
        return makeNodeWithPos<BadExpr>(typeexpr->pos);
    }

    typeexpr->name = names.get_or_add(text);

    return typeexpr;
}

Expr *Parser::parseUnaryExpr() {
    auto pos = tok.pos;

    fmt::print("{}: token=[{}], kind={}\n", __func__, tok.str(), tok.kind);
    switch (tok.kind) {
    case TokenKind::number:
    case TokenKind::string:
        return parseLiteralExpr();
    case TokenKind::ident: {
        // TODO: do proper op precedence parsing
        auto expr = parseFuncCallOrDeclRefExpr();
        return parseMemberExprMaybe(expr);
    }
    case TokenKind::star: {
        next();
        auto expr = parseUnaryExpr();
        return makeNodeWithPos<UnaryExpr>(pos, AstKind::deref_expr, expr);
    }
    case TokenKind::ampersand: {
        next();
        auto expr = parseUnaryExpr();
        return makeNodeWithPos<UnaryExpr>(pos, AstKind::address_expr, expr);
    }
    case TokenKind::lparen: {
        expect(TokenKind::lparen);
        auto inside_expr = parse_expr();
        expect(TokenKind::rparen);
        return makeNodeWithPos<ParenExpr>(pos, inside_expr);
    }
    // TODO: prefix (++), postfix, sign (+/-)
    default:
        // Because all expressions start with a unary expression, failing here
        // means no other expression could be matched either, so just do a
        // really generic report.
        errorExpected("an expression");
        return makeNodeWithPos<BadExpr>(pos);
    }
}

static int binary_op_precedence(const Token &op) {
    switch (op.kind) {
    case TokenKind::star:
    case TokenKind::slash:
        return 1;
    case TokenKind::plus:
    case TokenKind::minus:
        return 0;
    default:
        // not an operator
        return -1;
    }
}

// Extend a unary expression into binary if possible, by parsing any attached
// RHS. There may be more than one terms in the RHS, all of which is consumed
// by this function. The parsing goes on as long as operators with higher than
// or equal to 'precedence' are seen. Giving this parameter 0 means to parse
// the whole chain of binary expressions.
Expr *Parser::parseBinaryExprRhs(Expr *lhs, int precedence = 0) {
    Expr *root = lhs;

    while (true) {
        int this_prec = binary_op_precedence(tok);

        // If the upcoming op has lower precedence, finish this subexpression.
        // It will be treated as a single term when this function is re-called
        // with lower precedence.
        if (this_prec < precedence) {
            return root;
        }

        Token op = tok;
        next();

        // Parse the second term.
        Expr *rhs = parseUnaryExpr();

        // We do not know if this term should associate to left or right;
        // e.g. "(a * b) + c" or "a + (b * c)".  We should look ahead for the
        // next operator that follows this term.
        int next_prec = binary_op_precedence(tok);

        // If the next operator has higher precedence ("a + b * c"), evaluate
        // the RHS as a single subexpression with elevated minimum precedence.
        // Else ("a * b + c"), just treat it as a unary expression.
        if (this_prec < next_prec) {
            rhs = parseBinaryExprRhs(rhs, precedence + 1);
        }

        // Create a new root with the old root as its LHS, and the recursion
        // result as RHS.  This implements left associativity.
        root = makeNode<BinaryExpr>(root, op, rhs);
    }

    return root;
}

// If this expression is a member expression with a dot (.) operator, parse as
// such. If not, just pass along the original expression. This function is
// called after the operand expression part is fully parsed.
Expr *Parser::parseMemberExprMaybe(Expr *expr) {
    Expr *result = expr;

    while (tok.kind == TokenKind::dot) {
        expect(TokenKind::dot);

        Name *member_name = names.get_or_add(std::string{tok.text});
        next();

        result = makeNodeWithPos<MemberExpr>(result->pos, result, member_name);
    }

    return result;
}

Expr *Parser::parse_expr() {
    auto unary = parseUnaryExpr();
    if (!unary)
        return nullptr;
    auto binary = parseBinaryExprRhs(unary);
    return parseMemberExprMaybe(binary);
}

std::vector<Error> Parser::parseErrorBeacon() {
    expect(TokenKind::lbracket);
    expect(TokenKind::kw_error);
    expect(TokenKind::colon);

    std::vector<Error> v;
    v.push_back({locate(), std::string{tok.text}});
    next();

    expect(TokenKind::rbracket);
    return v;
}

// See cmp::verify().
bool Parser::verify() const {
    return cmp::verify(lexer.source().filename, errors, beacons);
}

void Parser::skip_until(TokenKind kind) {
  while (tok.kind != kind)
    next();
}

void Parser::skip_until_any(const std::vector<TokenKind> &kinds) {
  while (true) {
    for (auto kind : kinds) {
      if (tok.kind == kind)
        return;
    }
    next();
  }
}

void Parser::skip_until_end_of_line() {
    while (!is_end_of_stmt())
        next();
}

// The language is newline-aware, but newlines are mostly meaningless unless
// they are at the end of a statement or a declaration.  In those cases we use
// this to skip over them.
void Parser::skip_newlines() {
    while (tok.kind == TokenKind::newline || tok.kind == TokenKind::comment)
        next();
}

AstNode *Parser::parseToplevel() {
    skip_newlines();

    switch (tok.kind) {
    case TokenKind::kw_func:
        return parseFuncDecl();
    case TokenKind::kw_struct:
        return parseStructDecl();
    default:
        assert(false && "unreachable");
    }
}

File *Parser::parseFile() {
    auto file = makeNode<File>();
    // FIXME
    while (!is_eos()) {
        auto toplevel = parseToplevel();
        file->toplevels.push_back(toplevel);
    }
    return file;
}

Ast Parser::parse() {
    ast = parseFile();
    return Ast{ast, names};
}

// Report errors to stdout.
void Parser::report() const {
  for (auto e : errors)
    fmt::print("{}\n", e.str());
}

} // namespace cmp