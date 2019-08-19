#include "parser.h"
#include <utility>
#include <sstream>
#include <cassert>

namespace cmp {

Parser::Parser(Lexer &lexer) : lexer{lexer}, tok{} {
    // Insert keywords in name table
    for (auto m : keyword_map) {
        name_table.find_or_insert(m.first);
    }
    tokens = lexer.lex_all();
}

void Parser::next() {
    if (tokens[lookahead_pos].kind != TokenKind::eos) {
        lookahead_pos++;
    }
}

const Token Parser::look() const {
    return tokens[lookahead_pos];
}

void Parser::expect(TokenKind kind, const std::string &msg = "") {
    if (look().kind != kind) {
        std::stringstream ss;
        if (msg.empty())
            ss << "(" << look().pos << ") expected '" << tokentype_to_string(kind) << "', got '"
               << tokentype_to_string(look().kind) << "'";
        else
            ss << msg;
        throw ParseError{ss.str()};
    }
    next();
}

void Parser::expect_end_of_stmt() {
    if (!is_end_of_stmt())
        throw ParseError{"expected end of statement"};
    skip_newlines();
}

bool Parser::is_end_of_stmt() const {
    return look().kind == TokenKind::newline ||
           look().kind == TokenKind::comment;
}

bool Parser::is_eos() {
    skip_newlines();
    return look().kind == TokenKind::eos;
}

// Parse a statement.
//
// Stmt:
//     Decl ;
//     Expr ;
//     ;
P<Stmt> Parser::parse_stmt() {
    skip_newlines();

    // Try all possible productions and use the first successful one.
    // We use lookahead (LL(k)) to revert state if a production fails.
    // (See "recursive descent with backtracking":
    // https://en.wikipedia.org/wiki/Recursive_descent_parser)

    if (look().kind == TokenKind::kw_return)
        return parse_return_stmt();

    if (is_start_of_decl())
        return parse_decl_stmt();

    // Only possible production left
    return parse_expr_or_assign_stmt();
}

P<Stmt> Parser::parse_expr_or_assign_stmt() {
    auto start_pos = look().pos;

    auto lhs = parse_expr();
    // ExprStmt: expression ends with a newline
    if (is_end_of_stmt()) {
        expect(TokenKind::newline);
        return make_node<ExprStmt>(std::move(lhs));
    }

    // AssignStmt: expression is followed by equals
    // (anything else is treated as an error)
    expect(TokenKind::equals);

    // At this point, it becomes certain that this is an assignment statement,
    // and so we can safely unwrap for RHS.
    auto rhs = parse_expr();
    return make_node_with_pos<AssignStmt>(start_pos, look().pos, std::move(lhs), std::move(rhs));
}

P<ReturnStmt> Parser::parse_return_stmt() {
    expect(TokenKind::kw_return);
    auto expr = parse_expr();
    expect_end_of_stmt();
    return make_node<ReturnStmt>(std::move(expr));
}

P<DeclStmt> Parser::parse_decl_stmt() {
    auto decl = parse_decl();
    expect_end_of_stmt();
    return make_node<DeclStmt>(std::move(decl));
}

// Compound statement is a scoped block that consists of multiple statements.
// There is no restriction in order such as variable declarations should come
// first, etc.
//
// CompoundStmt:
//     { Stmt* }
P<CompoundStmt> Parser::parse_compound_stmt() {
    expect(TokenKind::lbrace);
    auto compound = make_node<CompoundStmt>();

    try {
        while (true) {
            skip_newlines();
            if (look().kind == TokenKind::rbrace)
                break;
            auto stmt = parse_stmt();
            compound->stmts.push_back(std::move(stmt));
        }
    } catch (const ParseError &e) {
        std::cerr << "parse exception: " << e.what() << ", looking at " << look() << std::endl;
        exit(EXIT_FAILURE);
    }

    expect(TokenKind::rbrace);
    return compound;
}

// ParamDecls are not trivially lookaheadable with a single token ('a' in 'a:
// int' does not guarantee anything), so this needs to be easily revertable.
P<ParamDecl> Parser::parse_param_decl() {
    auto start_pos = look().pos;

    Name *name = name_table.find_or_insert(look().text);
    next();

    expect(TokenKind::colon);

    auto typeexpr = parse_type_expr();

    return make_node_with_pos<ParamDecl>(start_pos, look().pos, name,
                                              std::move(typeexpr), false);
}

// This doesn't include the enclosing parentheses.
std::vector<P<ParamDecl>> Parser::parse_param_decl_list() {
    std::vector<P<ParamDecl>> decl_list;

    while (look().kind != TokenKind::rparen) {
        decl_list.push_back(parse_param_decl());

        if (look().kind != TokenKind::comma)
            break;
        next();
    }

    return decl_list;
}

P<VarDecl> Parser::parse_var_decl() {
    auto start_pos = look().pos;

    auto mut = (look().kind == TokenKind::kw_var);
    next();

    Name *name = name_table.find_or_insert(look().text);
    next();

    if (look().kind == TokenKind::colon) {
        next();
        // 'let' cannot be used with explicit type specfication
        if (!mut)
            throw ParseError{"initial value required for a 'let' decl"};
        auto typeexpr = parse_type_expr();
        return make_node_with_pos<VarDecl>(start_pos, typeexpr->end_pos, name,
                                           std::move(typeexpr), nullptr, mut);
    } else {
        expect(TokenKind::equals, mut ? "type or initial value required"
                                      : "initial value required");
        auto assignexpr = parse_expr();
        return make_node_with_pos<VarDecl>(start_pos, assignexpr->end_pos, name,
                                           nullptr, std::move(assignexpr), mut);
    }
}

P<FuncDecl> Parser::parse_func_decl() {
    expect(TokenKind::kw_fn);

    Name *name = name_table.find_or_insert(look().text);
    auto func = make_node<FuncDecl>(name);
    func->start_pos = look().pos;
    next();

    // Argument list
    expect(TokenKind::lparen);
    func->param_decl_list = parse_param_decl_list();
    expect(TokenKind::rparen);

    // Return type (-> ...)
    expect(TokenKind::arrow);

    func->return_type_expr = parse_type_expr();

    // Function body
    func->body = node_cast<CompoundStmt>(parse_compound_stmt());
    func->end_pos = look().pos;

    return func;
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

P<Decl> Parser::parse_decl() {
    switch (look().kind) {
    case TokenKind::kw_let:
    case TokenKind::kw_var:
        return parse_var_decl();
    default:
        throw ParseError{"not a start of a declaration"};
    }
}

P<UnaryExpr> Parser::parse_literal_expr() {
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
    expr->start_pos = look().pos;
    expr->end_pos = look().pos + look().text.length();

    next();

    return expr;
}

P<DeclRefExpr> Parser::parse_declref_expr() {
    auto ref_expr = make_node<DeclRefExpr>();

    ref_expr->start_pos = look().pos;
    ref_expr->end_pos = look().pos + look().text.length();

    std::string text = look().text;
    ref_expr->name = name_table.find_or_insert(text);

    next();

    return ref_expr;
}

P<TypeExpr> Parser::parse_type_expr() {
    auto type_expr = make_node<TypeExpr>();

    type_expr->start_pos = look().pos;

    // We encode each type into a unique Name, so that they are easy to find in
    // the type table in the semantic analysis phase.
    std::string text;
    if (look().kind == TokenKind::ampersand) {
        next();
        type_expr->ref = true;
        type_expr->subexpr = node_cast<TypeExpr>(parse_type_expr());
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

    type_expr->name = name_table.find_or_insert(text);
    type_expr->end_pos = look().pos;

    return type_expr;
}

P<Expr> Parser::parse_unary_expr() {
    auto start_pos = look().pos;

    switch (look().kind) {
    case TokenKind::number:
    case TokenKind::string:
        return parse_literal_expr();
    case TokenKind::ident:
        return parse_declref_expr();
    case TokenKind::star: {
        next();
        auto expr = parse_unary_expr();
        return make_node_with_pos<UnaryExpr>(start_pos, look().pos, UnaryExpr::Deref, std::move(expr));
    }
    case TokenKind::ampersand: {
        next();
        auto expr = parse_unary_expr();
        return make_node_with_pos<UnaryExpr>(start_pos, look().pos, UnaryExpr::Address, std::move(expr));
    }
    case TokenKind::lparen: {
        expect(TokenKind::lparen);
        auto expr = parse_expr();
        expect(TokenKind::rparen);
        // TODO: check unwrap
        return make_node_with_pos<UnaryExpr>(start_pos, look().pos, UnaryExpr::Paren, std::move(expr));
    }
    default:
        // Because all expressions start with a unary expression, failing here
        // means no other expression could be matched either, so just do a
        // really generic report.
        throw ParseError{std::to_string(start_pos) + ": expected an expression"};
    }
}

int Parser::get_precedence(const Token &op) const {
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
P<Expr> Parser::parse_binary_expr_rhs(ExprPtr lhs, int precedence) {
    ExprPtr root = std::move(lhs);

    while (true) {
        int this_prec = get_precedence(look());

        // If the upcoming op has lower precedence, finish this subexpression.
        // It will be treated as a single term when this function is re-called
        // with lower precedence.
        if (this_prec < precedence)
            return root;

        Token op = look();
        next();

        // Parse the second term.
        ExprPtr rhs = parse_unary_expr();
        // We do not know if this term should associate to left or right;
        // e.g. "(a * b) + c" or "a + (b * c)".  We should look ahead for the
        // next operator that follows this term.
        int next_prec = get_precedence(look());

        // If the next operator is indeed higher-level ("a + (b * c)"),
        // evaluate the RHS as a single subexpression with elevated minimum
        // precedence. Else ("(a * b) + c"), just treat it as a unary
        // expression.
        if (this_prec < next_prec)
            rhs = parse_binary_expr_rhs(std::move(rhs), precedence + 1);

        // Create a new root with the old root as its LHS, and the recursion
        // result as RHS.  This implements left associativity.
        root = make_node<BinaryExpr>(std::move(root), op, std::move(rhs));
    }

    return root;
}

P<Expr> Parser::parse_expr() {
    auto unary = parse_unary_expr();
    return parse_binary_expr_rhs(std::move(unary));
}

// The language is newline-aware, but newlines are mostly meaningless unless
// they are at the end of a statement or a declaration.  In those cases we use
// this to skip over them.
// @Cleanup: what about comments?
void Parser::skip_newlines() {
    while (look().kind == TokenKind::newline ||
           look().kind == TokenKind::comment) {
        next();
    }
}

P<AstNode> Parser::parse_toplevel() {
    skip_newlines();

    switch (look().kind) {
    case TokenKind::kw_fn:
        return parse_func_decl();
    default:
        throw ParseError{"unreachable"};
    }
}

P<File> Parser::parse_file() {
    auto file = make_node<File>();
    // FIXME
    while (!is_eos()) {
        auto toplevel = parse_toplevel();
        file->toplevels.push_back(std::move(toplevel));
    }
    return file;
}

Ast Parser::parse() {
    return Ast{parse_file(), name_table};
}

} // namespace cmp
