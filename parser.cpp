#include "parser.h"
#include <utility>
#include <sstream>
#include <cassert>

// Notes on error handling
//
// Encapsulating errors in a ParserResult may not be worth it, because this
// mechanism requires the callers to manually aggregate errors from different
// ParserResults into a vector, which otherwise could be done inside callee and
// have the caller not worry about it.  Instead, ParserResult only encapsulates
// the node and the successfulness, and errors are added to a central vector
// owned by Parser.  This is similar to the way Clang handles diagnostics.
//
// ParserResult<T> types are usually not used in its template form; instead,
// they are streamlined into three main cases: StmtResult, DeclResult and
// ExprResult.  This is sufficient because most nodes only specify the type of
// their children at this granularity. It also gives a benefit in that the
// parser functions do not have to manually specify T when returning erroneous
// ParserResult<T>s.

namespace cmp {

void ParserError::report() const {
    std::cerr << location.filename << ":" << location.line << ":" << location.col << ": ";
    std::cerr << "parse error: " << message << std::endl;
}

template <typename T>
P<T> ParserResult<T>::unwrap() {
    if (success()) {
        return std::move(std::get<P<T>>(result));
    }
    std::get<ParserError>(result).report();
    exit(EXIT_FAILURE);
}

static void insert_keywords_in_name_table(NameTable &name_table) {
    for (auto m : keyword_map) {
        name_table.find_or_insert(m.first);
    }
}

Parser::Parser(Lexer &lexer) : lexer(lexer), tok() {
    insert_keywords_in_name_table(name_table);
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

/// Return false if the lookahead token matches the expected one, true
/// otherwise.
bool Parser::expect(TokenKind kind, const std::string &msg = "") {
    if (look().kind != kind) {
        std::stringstream ss;
        if (msg.empty()) {
            ss << "expected '" << tokentype_to_string(kind) << "', got '"
               << tokentype_to_string(look().kind) << "'";
        } else {
            ss << msg;
        }
        errors.push_back(ss.str());
        return true;
    }
    next();
    return false;
}

static bool is_end_of_stmt(Token tok) {
    return tok.kind == TokenKind::newline || tok.kind == TokenKind::comment;
}

// Assignment statements start with an expression, so we cannot easily
// predetermine whether a statement is just an expression or an assignment
// until we see the '='.  We therefore first parse the expression (LHS) and
// then call this to transform that node into an assignment if needed.
/*
static Node *Parser::parse_assignstmt_or_exprstmt(Parser *p, Node *expr)
{
	if (look(p).type == TOK_EQUALS) {
		next(p);
		Node *rhs = parse_expr(p);
		return make_assignstmt(p, expr, rhs);
	} else {
		return make_exprstmt(p, expr);
	}
}
*/

// Parse a statement.
//
// Stmt:
//     Decl ;
//     Expr ;
//     ;
StmtResult Parser::parse_stmt() {
    skip_newlines();

    // Try all possible productions and use the first successful one.
    // We use lookahead (LL(k)) to revert state if a production fails.
    // (See "recursive descent with backtracking":
    // https://en.wikipedia.org/wiki/Recursive_descent_parser)
    if (look().kind == TokenKind::kw_return) {
        ParserResult<Stmt> result{parse_return_stmt()};
        if (!is_end_of_stmt(look())) {
            errors.push_back("unexpected token at end of statement");
            return ParserError{};
        }
        return result;
    }

    // @Cleanup: confusing control flow. Maybe use a loop? RAII?

    if (auto decl = parse_decl(); decl.success()) {
        if (!is_end_of_stmt(look())) {
            errors.push_back("unexpected token at end of declaration");
            return ParserError{};
        }
        return make_node<DeclStmt>(decl.unwrap());
    }

    auto save = get_position();
    if (auto stmt = parse_assign_stmt(); stmt.success())
        return stmt;
    restore_position(save);

    if (auto stmt = parse_expr_stmt(); stmt.success())
        return stmt;

    errors.push_back("expected a statement");
    return ParserError{};
}

StmtResult Parser::parse_expr_stmt() {
    auto r = parse_expr();
    // Only if the lookahead token points to a newline is the whole expression
    // successfully parsed.
    if (r.success() && is_end_of_stmt(look())) {
        next();
        return make_node<ExprStmt>(r.unwrap());
    } else {
        errors.push_back("unexpected token after expression statement");
        return ParserError{};
    }
}

StmtResult Parser::parse_assign_stmt() {
    auto start_pos = look().pos;

    auto lhs_res = parse_expr();
    if (!lhs_res.success())
        return {lhs_res.get_error()};

    if (expect(TokenKind::equals))
        return ParserError{};

    // At this point, it becomes certain that this is an assignment statement,
    // and so we can safely unwrap for RHS.
    auto rhs = parse_expr().unwrap();

    return make_node_with_pos<AssignStmt>(start_pos, look().pos, lhs_res.unwrap(), std::move(rhs));
}

StmtResult Parser::parse_return_stmt() {
    expect(TokenKind::kw_return);
    ExprPtr expr = parse_expr().unwrap();
    return make_node<ReturnStmt>(std::move(expr));
}

// Compound statement is a scoped block that consists of multiple statements.
// There is no restriction in order such as variable declarations should come
// first, etc.
//
// CompoundStmt:
//     { Stmt* }
StmtResult Parser::parse_compound_stmt() {
    expect(TokenKind::lbrace);
    auto compound = make_node<CompoundStmt>();
    ParserResult<Stmt> stmt_res;
    while ((stmt_res = parse_stmt()).success()) {
        compound->stmts.push_back(stmt_res.unwrap());
    }
    // did parse_stmt() fail at the end properly?
    if (look().kind != TokenKind::rbrace) {
        // report the last statement failure
        for (auto s : errors)
            std::cerr << s << std::endl;
        std::cerr << "Parse errors, terminating.\n";
        exit(EXIT_FAILURE);
    }
    if (expect(TokenKind::rbrace))
        return ParserError{};
    return {std::move(compound)};
}

// ParamDecls are not trivially lookaheadable with a single token ('a' in 'a:
// int' does not guarantee anything), so this needs to be easily revertable.
DeclResult Parser::parse_param_decl() {
    if (look().kind != TokenKind::ident) {
        errors.push_back("expected an identifier");
        return ParserError{};
    }

    auto start_pos = look().pos;
    // Insert into the name table
    Name *name = name_table.find_or_insert(look().text);
    next();

    if (look().kind != TokenKind::colon) {
        // This is a greedy error, wherever it happens.
        errors.push_back("expected ':' after name of the variable");
        return ParserError{};
    }
    next();

    auto type_expr = node_cast<TypeExpr>(parse_type_expr().unwrap());

    // TODO: mut?
    auto node = make_node_with_pos<ParamDecl>(start_pos, look().pos, name,
                                              std::move(type_expr), false);
    return make_result(std::move(node));
}

std::vector<P<ParamDecl>> Parser::parse_param_decl_list() {
    std::vector<P<ParamDecl>> decl_list;
    auto before_param = get_position();

    DeclResult res;
    while ((res = parse_param_decl()).success()) {
        decl_list.push_back(node_cast<ParamDecl>(res.unwrap()));

        // No more ',' after 'var: type' means the list is over.
        if (look().kind != TokenKind::comma)
            break;
        next();
    }

    // If the parse position moved, but the parse failed, it should be reported.
    if (get_position() != before_param) {
        res.unwrap();
    }
    return decl_list;
}

DeclResult Parser::parse_var_decl() {
    auto start_pos = look().pos;

    // 'let' or 'var'
    bool mut = look().kind == TokenKind::kw_var;
    next();

    // Try parse_param_decl() first
    auto before_param = get_position();
    auto param_res = parse_param_decl();
    if (param_res.success()) {
        // 'let' cannot be used with explicit type specfication
        if (!mut) {
            error("initial value required");
        }
        auto param_decl = node_cast<ParamDecl>(param_res.unwrap());
        // TODO: if param_decl->mut unmatches mut?
        return make_node_with_pos<VarDecl>(
            start_pos, param_decl->end_pos, param_decl->name,
            std::move(param_decl->type_expr), nullptr, param_decl->mut);
    }
    // If there's no explicit type specification, assignment expression is
    // required for both 'let' and 'var'.  Parse them here by hand.
    else {
        restore_position(before_param);

        Name *name = name_table.find_or_insert(look().text);
        next();

        // Assignment expression should be provided if kind is not
        // specified.
        if (expect(TokenKind::equals, mut ? "type or initial value required"
                                          : "initial value required"))
            return ParserError{};

        auto rhs = parse_expr().unwrap();
        return make_node_with_pos<VarDecl>(start_pos, rhs->end_pos, name,
                                           nullptr, std::move(rhs), mut);
    }
}

DeclResult Parser::parse_func_decl() {
    expect(TokenKind::kw_fn);

    Name *name = name_table.find_or_insert(look().text);
    auto func = make_node<FuncDecl>(name);
    func->start_pos = look().pos;
    next();

    // Argument list
    if (expect(TokenKind::lparen))
        return ParserError{};
    func->param_decl_list = parse_param_decl_list();
    if (expect(TokenKind::rparen))
        return ParserError{};

    // Return type (-> ...)
    if (expect(TokenKind::arrow))
        return ParserError{};

    auto ter = parse_type_expr();
    if (!ter.success())
        return ParserError{};

    func->return_type_expr = node_cast<TypeExpr>(ter.unwrap());

    // Function body
    func->body = node_cast<CompoundStmt>(parse_compound_stmt().unwrap());
    func->end_pos = look().pos;

    return std::move(func);
}

DeclResult Parser::parse_decl() {
    switch (look().kind) {
    case TokenKind::kw_let:
    case TokenKind::kw_var:
        return parse_var_decl();
    default:
        errors.push_back("not a start of a declaration");
        return ParserError{};
    }
}

ExprResult Parser::parse_literal_expr() {
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
        error("non-integer literals not implemented");
        break;
    }
    expr->start_pos = look().pos;
    expr->end_pos = look().pos + look().text.length();

    next();

    return {std::move(expr)};
}

ExprResult Parser::parse_declref_expr() {
    auto ref_expr = make_node<DeclRefExpr>();

    ref_expr->start_pos = look().pos;
    ref_expr->end_pos = look().pos + look().text.length();

    std::string text = look().text;
    ref_expr->name = name_table.find_or_insert(text);

    next();

    return {std::move(ref_expr)};
}

ExprResult Parser::parse_type_expr() {
    auto type_expr = make_node<TypeExpr>();

    type_expr->start_pos = look().pos;

    // We encode each type into a unique Name, so that they are easy to find in
    // the type table in the semantic analysis phase.
    std::string text;
    if (look().kind == TokenKind::ampersand) {
        next();
        type_expr->ref = true;
        type_expr->subexpr = node_cast<TypeExpr>(parse_type_expr().unwrap());
        text = "&" + type_expr->subexpr->name->text;
    }
    else if (look().is_identifier_or_keyword()) {
        type_expr->ref = false;
        type_expr->subexpr = nullptr;
        text = look().text;
        next();
    } else {
        errors.push_back("expected type name");
        return ParserError{};
    }

    type_expr->name = name_table.find_or_insert(text);
    type_expr->end_pos = look().pos;

    return {std::move(type_expr)};
}

ExprResult Parser::parse_unary_expr() {
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
        return make_node_with_pos<UnaryExpr>(start_pos, look().pos, UnaryExpr::Deref, expr.unwrap());
    }
    case TokenKind::ampersand: {
        next();
        auto expr = parse_unary_expr();
        return make_node_with_pos<UnaryExpr>(start_pos, look().pos, UnaryExpr::Address, expr.unwrap());
    }
    case TokenKind::lparen: {
        expect(TokenKind::lparen);
        auto expr = parse_expr();
        if (expect(TokenKind::rparen))
            return ParserError{};
        // TODO: check unwrap
        return make_node_with_pos<UnaryExpr>(start_pos, look().pos, UnaryExpr::Paren, expr.unwrap());
    }
    default:
        // Because all expressions start with a unary expression, failing here
        // means no other expression could be matched either, so just do a
        // really generic report.
        errors.push_back("expected an expression");
        return ParserError{};
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
ParserResult<Expr> Parser::parse_binary_expr_rhs(ExprPtr lhs, int precedence) {
    ExprPtr root = std::move(lhs);

    while (true) {
        int this_prec = get_precedence(look());

        // If the upcoming op has lower precedence, finish this subexpression.
        // It will be treated as a single term when this function is re-called
        // with lower precedence.
        if (this_prec < precedence) {
            return {std::move(root)};
        }

        Token op = look();
        next();

        // Parse the second term.
        ExprPtr rhs = parse_unary_expr().unwrap();
        // We do not know if this term should associate to left or right;
        // e.g. "(a * b) + c" or "a + (b * c)".  We should look ahead for the
        // next operator that follows this term.
        int next_prec = get_precedence(look());

        // If the next operator is indeed higher-level ("a + (b * c)"),
        // evaluate the RHS as a single subexpression with elevated minimum
        // precedence. Else ("(a * b) + c"), just treat it as a unary
        // expression.
        if (this_prec < next_prec) {
            rhs = parse_binary_expr_rhs(std::move(rhs), precedence + 1).unwrap();
        }

        // Create a new root with the old root as its LHS, and the recursion
        // result as RHS.  This implements left associativity.
        root = make_node<BinaryExpr>(std::move(root), op, std::move(rhs));
    }

    return {std::move(root)};
}

ParserResult<Expr> Parser::parse_expr() {
    auto unary_r = parse_unary_expr();
    if (!unary_r.success()) {
        return unary_r;
    }
    // TODO don't unwrap here.
    return parse_binary_expr_rhs(unary_r.unwrap());
}

void Parser::error(const std::string &msg) {
    auto loc = locate();
    std::cerr << loc.filename << ":" << loc.line << ":" << loc.col << ": ";
    std::cerr << "parse error: " << msg << std::endl;
    exit(1);
}

static std::string error_beacon_extract_msg(StringView text) {
    std::string s{text};
    return s;
}

// The language is newline-aware, but newlines are mostly meaningless unless
// they are at the end of a statement or a declaration.  In those cases we use
// this to skip over them.
// @Cleanup: what about comments?
void Parser::skip_newlines() {
    while (look().kind == TokenKind::newline ||
           look().kind == TokenKind::comment) {
        if (look().kind == TokenKind::comment) {
            std::string msg = error_beacon_extract_msg(look().text);
        }
        next();
    }
}

ParserResult<AstNode> Parser::parse_toplevel() {
    skip_newlines();

    switch (look().kind) {
    case TokenKind::eos:
        // TODO how to handle this?
        errors.push_back("end of file");
        return ParserError{};
    case TokenKind::kw_fn:
        return parse_func_decl().unwrap();
    default:
        error("unrecognized toplevel statement");
    }
    error("unreachable");
    return ParserError{};
}

ParserResult<File> Parser::parse_file() {
    auto file = make_node<File>();
    ParserResult<AstNode> top_r;
    while ((top_r = parse_toplevel()).success()) {
        file->toplevels.push_back(top_r.unwrap());
    }
    // TODO no need to unwrap top_r here?
    return {std::move(file)};
}

Ast Parser::parse() {
    return Ast{parse_file().unwrap(), name_table};
}

} // namespace cmp
