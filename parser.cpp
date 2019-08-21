#include "parser.h"
#include <utility>
#include <sstream>
#include <cassert>

namespace cmp {

using std::move;

Parser::Parser(Lexer &lexer) : lexer{lexer}, tok{} {
    // Insert keywords in name table
    for (auto m : keyword_map) {
        names.getOrAdd(m.first);
    }
    tokens = lexer.lexAll();
}

void Parser::next() {
    if (!tokens[lookIndex].is(TokenKind::eos)) {
        lookIndex++;
    }
}

const Token Parser::look() const {
    return tokens[lookIndex];
}

void Parser::expect(TokenKind kind, const std::string &msg = "") {
    if (!look().is(kind)) {
        std::stringstream ss;
        if (msg.empty())
            ss << "expected '" << tokentype_to_string(kind) << "', got '"
               << tokentype_to_string(look().kind) << "'";
        else
            ss << msg;
        throw ParseError{ss.str()};
    }
    next();
}

void Parser::expectEndOfStmt() {
    if (!isEndOfStmt())
        throw ParseError{"expected end of statement"};
    skipNewlines();
}

bool Parser::isEndOfStmt() const {
    return look().is(TokenKind::newline) || look().is(TokenKind::comment);
}

bool Parser::isEos() {
    skipNewlines();
    return look().is(TokenKind::eos);
}

// Parse a statement.
//
// Stmt:
//     Decl ;
//     Expr ;
//     ;
P<Stmt> Parser::parseStmt() {
    skipNewlines();

    if (look().is(TokenKind::kw_return))
        return parseReturnStmt();
    else if (isStartOfDecl())
        return parseDeclStmt();
    else
        return parseExprOrAssignStmt();
}

P<ReturnStmt> Parser::parseReturnStmt() {
    auto startPos = look().pos;

    expect(TokenKind::kw_return);
    P<Expr> expr = nullptr;
    if (!isEndOfStmt())
        expr = parseExpr();
    expectEndOfStmt();
    return make_node_with_pos<ReturnStmt>(startPos, look().pos, move(expr));
}

P<DeclStmt> Parser::parseDeclStmt() {
    auto decl = parseDecl();
    expectEndOfStmt();
    return make_node<DeclStmt>(move(decl));
}

P<Stmt> Parser::parseExprOrAssignStmt() {
    auto startPos = look().pos;

    auto lhs = parseExpr();
    // ExprStmt: expression ends with a newline
    if (isEndOfStmt()) {
        expect(TokenKind::newline);
        return make_node<ExprStmt>(move(lhs));
    }

    // AssignStmt: expression is followed by equals
    // (anything else is treated as an error)
    expect(TokenKind::equals);

    // At this point, it becomes certain that this is an assignment statement,
    // and so we can safely unwrap for RHS.
    auto rhs = parseExpr();
    return make_node_with_pos<AssignStmt>(startPos, look().pos, move(lhs),
                                          move(rhs));
}

// Compound statement is a scoped block that consists of multiple statements.
// There is no restriction in order such as variable declarations should come
// first, etc.
//
// CompoundStmt:
//     { Stmt* }
P<CompoundStmt> Parser::parseCompoundStmt() {
    expect(TokenKind::lbrace);
    auto compound = make_node<CompoundStmt>();

    try {
        while (true) {
            skipNewlines();
            if (look().is(TokenKind::rbrace))
                break;
            auto stmt = parseStmt();
            compound->stmts.push_back(move(stmt));
        }
    } catch (const ParseError &e) {
        auto loc = locate();
        std::cerr << loc.filename << ":" << loc.line << ":" << loc.col << ": ";
        std::cerr << "parse error: " << e.what() << std::endl;
        exit(EXIT_FAILURE);
    }

    expect(TokenKind::rbrace);
    return compound;
}

P<VarDecl> Parser::parseVarDecl() {
    auto startPos = look().pos;

    Name *name = names.getOrAdd(look().text);
    next();

    if (look().is(TokenKind::colon)) {
        next();
        auto typeexpr = parseTypeExpr();
        return make_node_with_pos<VarDecl>(startPos, typeexpr->endPos, name,
                                           move(typeexpr), nullptr, false);
    } else {
        expect(TokenKind::equals);
        auto assignexpr = parseExpr();
        return make_node_with_pos<VarDecl>(startPos, assignexpr->endPos, name,
                                           nullptr, move(assignexpr), false);
    }
}

// This doesn't include the enclosing parentheses or braces.
std::vector<P<VarDecl>> Parser::parseVarDeclList() {
    std::vector<P<VarDecl>> decls;

    while (true) {
        skipNewlines();

        if (!look().is(TokenKind::ident))
            break;

        decls.push_back(parseVarDecl());

        if (!look().is(TokenKind::comma))
            break;
        next();
    }

    return decls;
}

P<StructDecl> Parser::parseStructDecl() {
    expect(TokenKind::kw_struct);

    Name *name = names.getOrAdd(look().text);
    next();

    expect(TokenKind::lbrace);
    auto members = parseVarDeclList();
    expect(TokenKind::rbrace);

    return make_node<StructDecl>(name, std::move(members));
}

P<FuncDecl> Parser::parseFuncDecl() {
    expect(TokenKind::kw_fn);

    Name *name = names.getOrAdd(look().text);
    auto func = make_node<FuncDecl>(name);
    func->startPos = look().pos;
    next();

    // Argument list
    expect(TokenKind::lparen);
    func->params = parseVarDeclList();
    expect(TokenKind::rparen);

    // Return type (-> ...)
    if (look().is(TokenKind::arrow)) {
        next();
        func->retTypeExpr = parseTypeExpr();
    }

    // Function body
    func->body = parseCompoundStmt();
    func->endPos = look().pos;

    return func;
}

bool Parser::isStartOfDecl() const {
    switch (look().kind) {
    case TokenKind::kw_let:
    case TokenKind::kw_var:
        return true;
    default:
        return false;
    }
}

P<Decl> Parser::parseDecl() {
    switch (look().kind) {
    case TokenKind::kw_let: {
        next();
        return parseVarDecl();
    }
    default:
        throw ParseError{"not a start of a declaration"};
    }
}

P<UnaryExpr> Parser::parseLiteralExpr() {
    P<UnaryExpr> expr = nullptr;
    // TODO Literals other than integers?
    switch (look().kind) {
    case TokenKind::number: {
        std::string s{look().text};
        int value = std::stoi(s);
        expr = make_node<IntegerLiteral>(value);
        break;
    }
    default:
        throw ParseError{"non-integer literals not implemented"};
    }
    expr->startPos = look().pos;
    expr->endPos = look().pos + look().text.length();

    next();

    return expr;
}

P<DeclRefExpr> Parser::parseDeclRefExpr() {
    auto ref_expr = make_node<DeclRefExpr>();

    ref_expr->startPos = look().pos;
    ref_expr->endPos = look().pos + look().text.length();

    std::string text = look().text;
    ref_expr->name = names.getOrAdd(text);

    next();

    return ref_expr;
}

P<TypeExpr> Parser::parseTypeExpr() {
    auto type_expr = make_node<TypeExpr>();

    type_expr->startPos = look().pos;

    // We encode each type into a unique Name, so that they are easy to find in
    // the type table in the semantic analysis phase.
    std::string text;
    if (look().is(TokenKind::ampersand)) {
        next();
        type_expr->ref = true;
        type_expr->subexpr = parseTypeExpr();
        text = "&" + type_expr->subexpr->name->text;
    }
    else if (look().is_identifier_or_keyword()) {
        type_expr->ref = false;
        type_expr->subexpr = nullptr;
        text = look().text;
        next();
    } else {
        throw ParseError{"expected type name"};
    }

    type_expr->name = names.getOrAdd(text);
    type_expr->endPos = look().pos;

    return type_expr;
}

P<Expr> Parser::parseUnaryExpr() {
    auto startPos = look().pos;

    switch (look().kind) {
    case TokenKind::number:
    case TokenKind::string:
        return parseLiteralExpr();
    case TokenKind::ident:
        return parseDeclRefExpr();
    case TokenKind::star: {
        next();
        auto expr = parseUnaryExpr();
        return make_node_with_pos<UnaryExpr>(startPos, look().pos, UnaryExpr::Deref, move(expr));
    }
    case TokenKind::ampersand: {
        next();
        auto expr = parseUnaryExpr();
        return make_node_with_pos<UnaryExpr>(startPos, look().pos, UnaryExpr::Address, move(expr));
    }
    case TokenKind::lparen: {
        expect(TokenKind::lparen);
        auto expr = parseExpr();
        expect(TokenKind::rparen);
        // TODO: check unwrap
        return make_node_with_pos<UnaryExpr>(startPos, look().pos, UnaryExpr::Paren, move(expr));
    }
    default:
        // Because all expressions start with a unary expression, failing here
        // means no other expression could be matched either, so just do a
        // really generic report.
        throw ParseError{std::to_string(startPos) + ": expected an expression"};
    }
}

int Parser::getPrecedence(const Token &op) const {
    switch (op.kind) {
    case TokenKind::star:
    case TokenKind::slash:
        return 1;
    case TokenKind::plus:
    case TokenKind::minus:
        return 0;
    default:
        // Not an operator
        return -1;
    }
}

// Extend a unary expression into binary if possible, by parsing any attached
// RHS.  Returns result that owns the node of the newly constructed binary
// expression.
//
// After the call, 'lhs' is invalidated by being moved away.  Subsequent code
// should use the wrapped node in the return value instead.
P<Expr> Parser::parseBinaryExprRhs(ExprPtr lhs, int precedence) {
    ExprPtr root = move(lhs);

    while (true) {
        int this_prec = getPrecedence(look());

        // If the upcoming op has lower precedence, finish this subexpression.
        // It will be treated as a single term when this function is re-called
        // with lower precedence.
        if (this_prec < precedence)
            return root;

        Token op = look();
        next();

        // Parse the second term.
        ExprPtr rhs = parseUnaryExpr();
        // We do not know if this term should associate to left or right;
        // e.g. "(a * b) + c" or "a + (b * c)".  We should look ahead for the
        // next operator that follows this term.
        int next_prec = getPrecedence(look());

        // If the next operator is indeed higher-level ("a + (b * c)"),
        // evaluate the RHS as a single subexpression with elevated minimum
        // precedence. Else ("(a * b) + c"), just treat it as a unary
        // expression.
        if (this_prec < next_prec)
            rhs = parseBinaryExprRhs(move(rhs), precedence + 1);

        // Create a new root with the old root as its LHS, and the recursion
        // result as RHS.  This implements left associativity.
        root = make_node<BinaryExpr>(move(root), op, move(rhs));
    }

    return root;
}

P<Expr> Parser::parseExpr() {
    auto unary = parseUnaryExpr();
    return parseBinaryExprRhs(move(unary));
}

// The language is newline-aware, but newlines are mostly meaningless unless
// they are at the end of a statement or a declaration.  In those cases we use
// this to skip over them.
// @Cleanup: what about comments?
void Parser::skipNewlines() {
    while (look().is(TokenKind::newline) || look().is(TokenKind::comment)) {
        next();
    }
}

P<AstNode> Parser::parseToplevel() {
    skipNewlines();

    switch (look().kind) {
    case TokenKind::kw_fn:
        return parseFuncDecl();
    case TokenKind::kw_struct:
        return parseStructDecl();
    default:
        throw ParseError{"unreachable"};
    }
}

P<File> Parser::parseFile() {
    auto file = make_node<File>();
    // FIXME
    while (!isEos()) {
        auto toplevel = parseToplevel();
        file->toplevels.push_back(move(toplevel));
    }
    return file;
}

Ast Parser::parse() {
    P<File> file;
    try {
        file = parseFile();
    } catch (const ParseError &e) {
        auto loc = locate();
        std::cerr << loc.filename << ":" << loc.line << ":" << loc.col << ": ";
        std::cerr << "parse error: " << e.what() << std::endl;
        exit(EXIT_FAILURE);
    }
    return Ast{move(file), names};
}

} // namespace cmp
