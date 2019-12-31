#include "parser.h"
#include "fmt/core.h"
#include <cassert>
#include <regex>

namespace cmp {

template <typename T> using Res = ParserResult<T>;

// Report this error to stderr.
void ParseError::print() const {
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

Parser::Parser(Lexer &lexer) : lexer{lexer} {
    tok = lexer.lex();
    // insert keywords in name table
    for (auto m : keyword_map)
        names.get_or_add(std::string{m.first});
}

Parser::~Parser() {
    for (auto ptr : nodes) {
        delete ptr;
    }
}

// In the course of this, if an error beacon is found in the comment, add the
// error to the parser error list so that it can be compared to the actual
// errors later in the testing phase.
void Parser::next() {
    if (!tok.is(TokenKind::eos)) {
        tok = lexer.lex();
        if (tok.is(TokenKind::comment)) {
            std::string_view marker{"[error:"};
            auto found = tok.text.find(marker);
            if (found != std::string_view::npos) {
                auto bracket = tok.text.substr(found);
                Source s{std::string{bracket}};
                Lexer l{s};
                Parser p{l};
                auto v = p.parse_error_beacon();
                // override location
                for (auto &e : v) {
                    e.loc = locate();
                    beacons.push_back(e);
                }
            }
        }
    }
}

bool Parser::expect(TokenKind kind, const std::string &msg = "") {
    if (!tok.is(kind)) {
        std::string s = msg;
        if (msg.empty()) {
            s = fmt::format("expected '{}', found '{}'",
                            tokentype_to_string(kind),
                            tokentype_to_string(tok.kind));
        }
        add_error(s);
        return false;
    }
    next();
    return true;
}

bool Parser::expect_end_of_stmt() {
    if (!is_end_of_stmt()) {
        return false;
    }
    skip_newlines();
    return true;
}

bool Parser::is_end_of_stmt() const {
    return tok.is(TokenKind::newline) || tok.is(TokenKind::comment);
}

bool Parser::is_eos() {
    skip_newlines();
    return tok.is(TokenKind::eos);
}

// Parse a statement.
//
// Stmt:
//     Decl
//     Expr
Stmt *Parser::parse_stmt() {
    Stmt *stmt = nullptr;

    if (tok.is(TokenKind::kw_return)) {
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
    auto start_pos = tok.pos;

    expect(TokenKind::kw_return);

    // optional
    Expr *expr = nullptr;
    if (!is_end_of_stmt())
        expr = parse_expr();
    if (!expect_end_of_stmt())
        assert(false);
    return make_node_with_pos<ReturnStmt>(start_pos, tok.pos, expr);
}

// let a = ...
DeclStmt *Parser::parse_decl_stmt() {
    auto decl = parse_decl();
    if (!expect_end_of_stmt()) {
        if (decl->kind == AstKind::bad_decl) {
            // try to recover
            skip_until_end_of_line();
        } else {
            // TODO: errorExpected for 'found '''
            add_error("expected end of declaration");
            skip_until_end_of_line();
        }
    }
    return make_node<DeclStmt>(decl);
}

Stmt *Parser::parse_expr_or_assign_stmt() {
    auto start_pos = tok.pos;

    auto lhs = parse_expr();
    // ExprStmt: expression ends with a newline
    if (is_end_of_stmt()) {
        skip_until_end_of_line();
        return make_node<ExprStmt>(lhs);
    }

    // AssignStmt: expression is followed by equals
    // (anything else is treated as an error)
    if (!expect(TokenKind::equals)) {
        skip_until_end_of_line();
        return make_node_with_pos<BadStmt>(start_pos, tok.pos);
    }

    // At this point, it becomes certain that this is an assignment statement,
    // and so we can safely unwrap for RHS.
    auto rhs = parse_expr();
    return make_node_with_pos<AssignStmt>(start_pos, tok.pos, lhs,
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
        if (tok.is(TokenKind::rbrace))
            break;
        auto stmt = parse_stmt();
        compound->stmts.push_back(stmt);
    }

    expect(TokenKind::rbrace);
    return compound;
}

Decl *Parser::parse_var_decl() {
    auto start_pos = tok.pos;

    Name *name = names.get_or_add(std::string{tok.text});
    next();

    if (tok.is(TokenKind::equals)) {
        // a = expr
        expect(TokenKind::equals);
        auto assignexpr = parse_expr();
        return make_node_with_pos<VarDecl>(start_pos, assignexpr->end_pos, name,
                                           nullptr, assignexpr);
    } else if (tok.is(TokenKind::colon)) {
        // a: type
        expect(TokenKind::colon);
        auto typeexpr = parse_type_expr();
        return make_node_with_pos<VarDecl>(start_pos, typeexpr->end_pos, name,
                                           typeexpr, nullptr);
    } else {
        add_error("expected '=' or ':' after var name");
        return make_node_with_pos<BadDecl>(start_pos, tok.pos);
    }
}

// This doesn't include enclosing parentheses or braces.
std::vector<Decl *> Parser::parse_var_decl_list() {
    std::vector<Decl *> decls;

    while (true) {
        Decl *decl = nullptr;
        skip_newlines();
        if (!tok.is(TokenKind::ident))
            break;

        decl = parse_var_decl();
        decls.push_back(decl);

        if (decl->kind == AstKind::bad_decl) {
            // Determining where each decl ends is a little tricky.
            // We could test for every tokens that are either (1) separator
            // tokens, i.e. comma, newline, or (2) used to enclose a decl list,
            // i.e. parentheses and braces.
            skip_until({TokenKind::comma, TokenKind::newline, TokenKind::rparen,
                        TokenKind::rbrace});
        }
        if (tok.is(TokenKind::comma)) {
            next();
        }
    }
    skip_newlines();

    return decls;
}

StructDecl *Parser::parse_struct_decl() {
    expect(TokenKind::kw_struct);

    // TODO check ident
    Name *name = names.get_or_add(std::string{tok.text});
    next();

    if (!expect(TokenKind::lbrace))
        skip_until_end_of_line();

    auto fields = parse_var_decl_list();

    expect(TokenKind::rbrace, "unterminated struct declaration");

    return make_node<StructDecl>(name, fields);
}

FuncDecl *Parser::parse_func_decl() {
    auto start_pos = tok.pos;

    expect(TokenKind::kw_fn);

    Name *name = names.get_or_add(std::string{tok.text});
    auto func = make_node<FuncDecl>(name);
    func->start_pos = tok.pos;
    next();

    // argument list
    expect(TokenKind::lparen);
    func->params = parse_var_decl_list();
    expect(TokenKind::rparen);

    // return type (-> ...)
    if (tok.is(TokenKind::arrow)) {
        next();
        func->retTypeExpr = parse_type_expr();
    }
    if (!tok.is(TokenKind::lbrace)) {
        add_error("expected '->' or '{'");
        skip_until(TokenKind::lbrace);
    }
    // function body
    func->body = parse_compound_stmt();
    func->start_pos = start_pos;
    func->end_pos = tok.pos;

    return func;
}

bool Parser::is_start_of_decl() const {
    switch (tok.kind) {
    case TokenKind::kw_let:
    case TokenKind::kw_var:
        return true;
    default:
        return false;
    }
}

// 'let a = ...'
Decl *Parser::parse_decl() {
    switch (tok.kind) {
    case TokenKind::kw_let:
        next();
        return parse_var_decl();
    default:
        assert(false && "not a start of a declaration");
    }
    // unreachable
    return nullptr;
}

UnaryExpr *Parser::parse_literal_expr() {
    UnaryExpr *expr = nullptr;
    // TODO Literals other than integers?
    switch (tok.kind) {
    case TokenKind::number: {
        std::string s{tok.text};
        int value = std::stoi(s);
        expr = make_node<IntegerLiteral>(value);
        break;
    }
    default:
        assert(false && "non-integer literals not implemented");
    }
    expr->start_pos = tok.pos;
    expr->end_pos = tok.pos + tok.text.length();

    next();

    return expr;
}

DeclRefExpr *Parser::parse_declref_expr() {
    auto ref_expr = make_node<DeclRefExpr>();

    ref_expr->start_pos = tok.pos;
    ref_expr->end_pos = tok.pos + tok.text.length();

    std::string text{tok.text};
    ref_expr->name = names.get_or_add(text);

    next();

    return ref_expr;
}

bool Parser::is_start_of_typeexpr() const {
    return tok.is(TokenKind::quote) || tok.is(TokenKind::ampersand) ||
           tok.is_identifier_or_keyword();
}

Expr *Parser::parse_type_expr() {
    auto typeExpr = make_node<TypeExpr>();

    assert(is_start_of_typeexpr());
    typeExpr->start_pos = tok.pos;

    // Mutable type?
    if (tok.is(TokenKind::quote)) {
        typeExpr->mut = true;
        next();
    }

    // Encode each type into a unique Name, so that they are easy to find in
    // the type table in the semantic analysis phase.
    std::string text;
    if (tok.is(TokenKind::ampersand)) {
        next();
        typeExpr->ref = true;
        typeExpr->subexpr = parse_type_expr();
        text = "&" + static_cast<TypeExpr *>(typeExpr->subexpr)->name->text;
    } else if (tok.is_identifier_or_keyword()) {
        typeExpr->ref = false;
        typeExpr->subexpr = nullptr;
        text = tok.text;
        next();
    } else {
        add_error("expected type name");
        return make_node_with_pos<BadExpr>(typeExpr->start_pos, tok.pos);
    }

    typeExpr->name = names.get_or_add(text);
    typeExpr->end_pos = tok.pos;

    return typeExpr;
}

Expr *Parser::parse_unary_expr() {
    auto start_pos = tok.pos;

    switch (tok.kind) {
    case TokenKind::number:
    case TokenKind::string:
        return parse_literal_expr();
    case TokenKind::ident:
        return parse_declref_expr();
    case TokenKind::star: {
        next();
        auto expr = parse_unary_expr();
        return make_node_with_pos<UnaryExpr>(start_pos, tok.pos, UnaryExpr::Deref, expr);
    }
    case TokenKind::ampersand: {
        next();
        auto expr = parse_unary_expr();
        return make_node_with_pos<UnaryExpr>(start_pos, tok.pos, UnaryExpr::Address, expr);
    }
    case TokenKind::lparen: {
        expect(TokenKind::lparen);
        auto expr = parse_expr();
        expect(TokenKind::rparen);
        return make_node_with_pos<UnaryExpr>(start_pos, tok.pos, UnaryExpr::Paren, expr);
    }
    default:
        // Because all expressions start with a unary expression, failing here
        // means no other expression could be matched either, so just do a
        // really generic report.
        add_error("expected an expression");
        return make_node_with_pos<BadExpr>(start_pos, tok.pos);
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
        int this_prec = op_precedence(tok);

        // If the upcoming op has lower precedence, finish this subexpression.
        // It will be treated as a single term when this function is re-called
        // with lower precedence.
        if (this_prec < precedence) {
            return root;
        }

        Token op = tok;
        next();

        // Parse the second term.
        Expr *rhs = parse_unary_expr();

        // We do not know if this term should associate to left or right;
        // e.g. "(a * b) + c" or "a + (b * c)".  We should look ahead for the
        // next operator that follows this term.
        int next_prec = op_precedence(tok);

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

Expr *Parser::parse_expr() {
    auto unary = parse_unary_expr();
    if (!unary)
        return unary;
    return parse_binary_expr_rhs(unary);
}

std::vector<ParseError> Parser::parse_error_beacon() {
    expect(TokenKind::lbracket);
    expect(TokenKind::kw_error);
    expect(TokenKind::colon);

    std::vector<ParseError> v;
    v.push_back({locate(), std::string{tok.text}});
    next();

    expect(TokenKind::rbracket);
    return v;
}

// Verify errors against the error beacons embedded in the source text.
void Parser::verify() const {
    bool success = true;
    fmt::print("TEST {}:\n", lexer.source().filename);

    size_t i = 0, j = 0;
    while (i < errors.size() && j < beacons.size()) {
        auto error = errors[i];
        auto beacon = beacons[j];
        if (error.loc.line == beacon.loc.line) {
            std::string stripped{std::cbegin(beacon.message) + 1,
                                 std::cend(beacon.message) - 1};
            std::regex regex{stripped};
            if (!std::regex_search(error.message, regex)) {
                success = false;
                fmt::print("< {}\n> {}\n", error, beacon);
            }
            i++;
            j++;
        } else if (error.loc.line < beacon.loc.line) {
            success = false;
            fmt::print("< {}\n", error);
            i++;
        } else {
            success = false;
            fmt::print("> {}\n", beacon);
            j++;
        }
    }
    for (; i < errors.size(); i++) {
        success = false;
        fmt::print("< {}\n", errors[i]);
    }
    for (; j < beacons.size(); j++) {
        success = false;
        fmt::print("> {}\n", beacons[j]);
    }

    fmt::print("{} {}\n", success ? "SUCCESS" : "FAIL",
               lexer.source().filename);
}

void Parser::skip_until(TokenKind kind) {
    while (!tok.is(kind))
        next();
}

void Parser::skip_until(const std::vector<TokenKind> &kinds) {
    while (true) {
        for (auto kind : kinds) {
            if (tok.is(kind))
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
    while (tok.is(TokenKind::newline) || tok.is(TokenKind::comment))
        next();
}

AstNode *Parser::parse_toplevel() {
    skip_newlines();

    switch (tok.kind) {
    case TokenKind::kw_fn:
        return parse_func_decl();
    case TokenKind::kw_struct:
        return parse_struct_decl();
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

// Report errors to stdout.
void Parser::report() const {
    for (auto e : errors) {
        e.print();
    }
}

} // namespace cmp
