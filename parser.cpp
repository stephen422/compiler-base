#include "parser.h"
#include "fmt/core.h"
#include <cassert>
#include <sstream>
#include <utility>

namespace cmp {

template <typename T> using Res = ParserResult<T>;

// Report this error to stderr.
void ParserError::report() const {
    fmt::print(stderr, "{}:{}:{}: parse error: {}\n", loc.filename, loc.line,
               loc.col, message);
}

template <typename T>
T *ParserResult<T>::unwrap() {
    if (!success()) {
        error().report();
        exit(EXIT_FAILURE);
    }
    return ptr();
}

Parser::Parser(Lexer &lexer) : lexer{lexer}, tok{} {
    // Insert keywords in name table
    for (auto m : keyword_map) {
        names.get_or_add(std::string{m.first});
    }
    tokens = lexer.lexAll();
}

Parser::~Parser() {
    for (auto ptr : nodes) {
        delete ptr;
    }
}

void Parser::next() {
    if (!tokens[look_index].is(TokenKind::eos)) {
        look_index++;
    }
}

const Token Parser::look() const { return tokens[look_index]; }

Res<AstNode> Parser::expect(TokenKind kind, const std::string &msg = "") {
    if (!look().is(kind)) {
        std::string s = msg;
        if (msg.empty()) {
            s = fmt::format("expected '{}', got '{}'",
                            tokentype_to_string(kind),
                            tokentype_to_string(look().kind));
        }
        return error(s);
    }
    next();
    return {static_cast<AstNode *>(nullptr)};
}

bool Parser::expect_end_of_stmt() {
    if (!is_end_of_stmt()) {
        return false;
    }
    skip_newlines();
    return true;
}

bool Parser::is_end_of_stmt() const {
    return look().is(TokenKind::newline) || look().is(TokenKind::comment);
}

bool Parser::is_eos() {
    skip_newlines();
    return look().is(TokenKind::eos);
}

// Parse a statement.
//
// Stmt:
//     Decl
//     Expr
StmtResult Parser::parse_stmt() {
    StmtResult stmt;

    if (look().is(TokenKind::kw_return)) {
        stmt = parse_return_stmt();
    } else if (is_start_of_decl()) {
        stmt = parse_decl_stmt();
    } else {
        stmt = parse_expr_or_assign_stmt();
    }
    skip_newlines();

    return stmt;
}

ReturnStmt *Parser::parse_return_stmt() {
    auto startPos = look().pos;

    expect(TokenKind::kw_return);

    // optional
    Expr *expr = nullptr;
    if (!is_end_of_stmt()) {
        expr = parse_expr().unwrap();
    }
    if (!expect_end_of_stmt()) {
        assert(false);
    }
    return make_node_with_pos<ReturnStmt>(startPos, look().pos, expr);
}

// let a = ...
DeclStmt *Parser::parse_decl_stmt() {
    auto decl = parse_decl();
    if (!expect_end_of_stmt()) {
        return nullptr;
    }
    return make_node<DeclStmt>(decl.unwrap());
}

StmtResult Parser::parse_expr_or_assign_stmt() {
    auto startPos = look().pos;

    auto lhs = parse_expr().unwrap();
    // ExprStmt: expression ends with a newline
    if (is_end_of_stmt()) {
        expect(TokenKind::newline);
        return make_node<ExprStmt>(lhs);
    }

    // AssignStmt: expression is followed by equals
    // (anything else is treated as an error)
    if (auto e = expect(TokenKind::equals); !e.success()) {
        return e;
    }

    // At this point, it becomes certain that this is an assignment statement,
    // and so we can safely unwrap for RHS.
    auto rhs = parse_expr().unwrap();
    return make_node_with_pos<AssignStmt>(startPos, look().pos, lhs,
                                          rhs);
}

// Compound statement is a scoped block that consists of multiple statements.
// There is no restriction in order such as variable declarations should come
// first, etc.
//
// CompoundStmt:
//     { Stmt* }
CompoundStmt *Parser::parse_compound_stmt() {
    expect(TokenKind::lbrace);
    auto compound = make_node<CompoundStmt>();

    while (true) {
        skip_newlines();
        if (look().is(TokenKind::rbrace))
            break;
        auto stmt = parse_stmt();
        if (!stmt.success()) {
            // per-stmt error check
            stmt.unwrap();
        }
        compound->stmts.push_back(stmt.unwrap());
    }

    expect(TokenKind::rbrace);
    return compound;
}

VarDecl *Parser::parse_var_decl() {
    auto startPos = look().pos;

    Name *name = names.get_or_add(std::string{look().text});
    next();

    if (look().is(TokenKind::colon)) {
        // a: type
        next();
        auto typeexpr = parse_type_expr();
        return make_node_with_pos<VarDecl>(startPos, typeexpr->endPos, name,
                                           typeexpr, nullptr);
    } else {
        // a = expr
        expect(TokenKind::equals);
        auto assignexpr = parse_expr().unwrap();
        return make_node_with_pos<VarDecl>(startPos, assignexpr->endPos, name,
                                           nullptr, assignexpr);
    }
}

// This doesn't include the enclosing parentheses or braces.
std::vector<VarDecl *> Parser::parse_var_decl_list() {
    std::vector<VarDecl *> decls;

    while (true) {
        skip_newlines();
        if (!look().is(TokenKind::ident)) {
            break;
        }
        decls.push_back(parse_var_decl());
        if (!look().is(TokenKind::comma)) {
            break;
        }
        next();
    }
    skip_newlines();

    return decls;
}

Res<StructDecl> Parser::parse_struct_decl() {
    expect(TokenKind::kw_struct);

    Name *name = names.get_or_add(std::string{look().text});
    next();

    if (auto e = expect(TokenKind::lbrace); !e.success()) {
        return e;
    }

    auto members = parse_var_decl_list();

    if (auto e =
            expect(TokenKind::rbrace, "unterminated struct member declaration");
        !e.success()) {
        return e;
    }

    return make_node<StructDecl>(name, members);
}

FuncDecl *Parser::parse_func_decl() {
    expect(TokenKind::kw_fn);

    Name *name = names.get_or_add(std::string{look().text});
    auto func = make_node<FuncDecl>(name);
    func->startPos = look().pos;
    next();

    // Argument list
    expect(TokenKind::lparen);
    func->params = parse_var_decl_list();
    expect(TokenKind::rparen);

    // Return type (-> ...)
    if (look().is(TokenKind::arrow)) {
        next();
        func->retTypeExpr = parse_type_expr();
    }

    // Function body
    func->body = parse_compound_stmt();
    func->endPos = look().pos;

    return func;
}

BadStmt *Parser::stmt_error(const std::string &msg) {
    return make_node<BadStmt>(msg);
}

BadDecl *Parser::decl_error(const std::string &msg) {
    return make_node<BadDecl>(msg);
}

BadExpr *Parser::expr_error(const std::string &msg) {
    return make_node<BadExpr>(msg);
}

bool Parser::is_start_of_decl() const {
    switch (look().kind) {
    case TokenKind::kw_let:
    case TokenKind::kw_var:
        return true;
    default:
        return false;
    }
}

DeclResult Parser::parse_decl() {
    switch (look().kind) {
    case TokenKind::kw_let: {
        next();
        return parse_var_decl();
    }
    default:
        return error("not a start of a declaration");
    }
}

UnaryExpr *Parser::parse_literal_expr() {
    UnaryExpr *expr = nullptr;
    // TODO Literals other than integers?
    switch (look().kind) {
    case TokenKind::number: {
        std::string s{look().text};
        int value = std::stoi(s);
        expr = make_node<IntegerLiteral>(value);
        break;
    }
    default:
        assert(false && "non-integer literals not implemented");
    }
    expr->startPos = look().pos;
    expr->endPos = look().pos + look().text.length();

    next();

    return expr;
}

DeclRefExpr *Parser::parse_declref_expr() {
    auto ref_expr = make_node<DeclRefExpr>();

    ref_expr->startPos = look().pos;
    ref_expr->endPos = look().pos + look().text.length();

    std::string text{look().text};
    ref_expr->name = names.get_or_add(text);

    next();

    return ref_expr;
}

TypeExpr *Parser::parse_type_expr() {
    auto typeExpr = make_node<TypeExpr>();

    typeExpr->startPos = look().pos;

    // Mutable type?
    if (look().is(TokenKind::quote)) {
        typeExpr->mut = true;
        next();
    }

    // Encode each type into a unique Name, so that they are easy to find in
    // the type table in the semantic analysis phase.
    std::string text;
    if (look().is(TokenKind::ampersand)) {
        next();
        typeExpr->ref = true;
        typeExpr->subexpr = parse_type_expr();
        text = "&" + typeExpr->subexpr->name->text;
    }
    else if (look().is_identifier_or_keyword()) {
        typeExpr->ref = false;
        typeExpr->subexpr = nullptr;
        text = look().text;
        next();
    } else {
        assert(false && "expected type name");
    }

    typeExpr->name = names.get_or_add(text);
    typeExpr->endPos = look().pos;

    return typeExpr;
}

ExprResult Parser::parse_unary_expr() {
    auto startPos = look().pos;

    switch (look().kind) {
    case TokenKind::number:
    case TokenKind::string:
        return parse_literal_expr();
    case TokenKind::ident:
        return parse_declref_expr();
    case TokenKind::star: {
        next();
        auto expr = parse_unary_expr().unwrap();
        return make_node_with_pos<UnaryExpr>(startPos, look().pos, UnaryExpr::Deref, expr);
    }
    case TokenKind::ampersand: {
        next();
        auto expr = parse_unary_expr().unwrap();
        return make_node_with_pos<UnaryExpr>(startPos, look().pos, UnaryExpr::Address, expr);
    }
    case TokenKind::lparen: {
        expect(TokenKind::lparen);
        auto expr = parse_expr().unwrap();
        expect(TokenKind::rparen);
        // TODO: check unwrap
        return make_node_with_pos<UnaryExpr>(startPos, look().pos, UnaryExpr::Paren, expr);
    }
    default:
        // Because all expressions start with a unary expression, failing here
        // means no other expression could be matched either, so just do a
        // really generic report.
        return error(fmt::format("{}: expected an expression", startPos));
    }
}

static int op_precedence(const Token &op) {
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
Expr *Parser::parse_binary_expr_rhs(Expr *lhs, int precedence) {
    Expr *root = lhs;

    while (true) {
        int this_prec = op_precedence(look());

        // If the upcoming op has lower precedence, finish this subexpression.
        // It will be treated as a single term when this function is re-called
        // with lower precedence.
        if (this_prec < precedence) {
            return root;
        }

        Token op = look();
        next();

        // Parse the second term.
        Expr *rhs = parse_unary_expr().unwrap();

        // We do not know if this term should associate to left or right;
        // e.g. "(a * b) + c" or "a + (b * c)".  We should look ahead for the
        // next operator that follows this term.
        int next_prec = op_precedence(look());

        // If the next operator has higher precedence ("a + b * c"), evaluate
        // the RHS as a single subexpression with elevated minimum precedence.
        // Else ("a * b + c"), just treat it as a unary expression.
        if (this_prec < next_prec) {
            rhs = parse_binary_expr_rhs(rhs, precedence + 1);
        }

        // Create a new root with the old root as its LHS, and the recursion
        // result as RHS.  This implements left associativity.
        root = make_node<BinaryExpr>(root, op, rhs);
    }

    return root;
}

ExprResult Parser::parse_expr() {
    auto unary = parse_unary_expr();
    if (!unary.success()) {
        return unary;
    }
    return parse_binary_expr_rhs(unary.unwrap());
}

// The language is newline-aware, but newlines are mostly meaningless unless
// they are at the end of a statement or a declaration.  In those cases we use
// this to skip over them.
// @Cleanup: what about comments?
void Parser::skip_newlines() {
    while (look().is(TokenKind::newline) || look().is(TokenKind::comment)) {
        if (look().is(TokenKind::comment)) {
            fmt::print("I saw a comment: {}\n", look().text);
        }
        next();
    }
}

AstNode *Parser::parse_toplevel() {
    skip_newlines();

    switch (look().kind) {
    case TokenKind::kw_fn:
        return parse_func_decl();
    case TokenKind::kw_struct: {
        auto decl = parse_struct_decl();
        return decl.unwrap();
    }
    default:
        assert(false && "unreachable");
    }
}

File *Parser::parse_file() {
    auto file = make_node<File>();
    // FIXME
    while (!is_eos()) {
        auto toplevel = parse_toplevel();
        file->toplevels.push_back(toplevel);
    }
    return file;
}

Ast Parser::parse() {
    File *file = parse_file();
    return Ast{file, names};
}

} // namespace cmp
