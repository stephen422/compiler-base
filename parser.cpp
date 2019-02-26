#include "parser.h"
#include <utility>
#include <sstream>
#include <cassert>

namespace cmp {

void ParseError::report() const {
    std::cerr << location.filename << ":" << location.line << ":" << location.col << ": ";
    std::cerr << "parse error: " << message << std::endl;
}

template <typename T>
NodePtr<T> ParseResult<T>::unwrap() {
    if (success()) {
        return std::move(ptr);
    }
    for (auto const& e : errors) {
        e.report();
    }
    // TODO: exit more gracefully?
    exit(EXIT_FAILURE);
    return nullptr;
}

static void insert_keywords_in_name_table(NameTable &name_table) {
    for (auto m : keyword_map) {
        name_table.insert(m.first);
    }
}

Parser::Parser(Lexer &lexer) : lexer(lexer), tok() {
    insert_keywords_in_name_table(name_table);
    tokens = lexer.lex_all();
}

void Parser::next() {
    if (tokens[lookahead_pos].type != TokenType::eos) {
        lookahead_pos++;
    }
}

const Token Parser::look() const {
    return tokens[lookahead_pos];
}

void Parser::save_state() {
    saved_pos = lookahead_pos;
}

void Parser::revert_state() {
    lookahead_pos = saved_pos;
}

void Parser::expect(TokenType type, const std::string &msg = "") {
    if (look().type != type) {
        std::stringstream ss;
        if (msg.empty()) {
            ss << "expected '" << tokentype_to_string(type) << "', got '"
               << tokentype_to_string(look().type) << "'";
        } else {
	    ss << msg;
	}
        error(ss.str());
    }
    next();
}

// Parse a statement.
//
// Stmt:
//     Decl ;
//     Expr ;
//     ;
ParseResult<Stmt> Parser::parse_stmt() {
    ParseResult<Stmt> result;

    skip_newlines();

    // TODO are we certain that the parsing up to this point is correct?
    save_state();

    // Try all possible productions and use the first successful one.
    // We use lookahead (LL(k)) to revert state if a production fails.
    // (See "recursive descent with backtracking":
    // https://en.wikipedia.org/wiki/Recursive_descent_parser)
    if (look().type == TokenType::comment) {
        next();
        return parse_stmt();
    } else if (look().type == TokenType::kw_return) {
        result = parse_return_stmt();
        expect(TokenType::newline, "unexpected token at end of statement");
        return result;
    }

    // @Cleanup: confusing control flow. Maybe use a loop? RAII?

    if (auto decl = parse_decl()) {
        expect(TokenType::newline, "unexpected token at end of declaration");
        return make_node<DeclStmt>(std::move(decl));
    }
    revert_state();

    if (auto stmt = parse_assign_stmt()) {
        return std::move(stmt);
    }
    revert_state();

    // parse_expr() is pretty much the only one that we can't trivially predict
    // using lookaheads.
    // FIXME: if all of the other productions can be determined by just one
    // lookahead, do we really need save_state() and revert_state()?
    if (auto stmt = parse_expr_stmt()) {
        return std::move(stmt);
    }
    revert_state();

    return ParseError(locate(), "expected a statement");
}

NodePtr<ExprStmt> Parser::parse_expr_stmt() {
    auto r = parse_expr();
    // Only if the lookahead token points to a newline is the whole expression
    // successfully parsed.
    if (r.success() && look().type == TokenType::newline) {
        next();
        return make_node<ExprStmt>(r.unwrap());
    } else {
        return nullptr;
    }
}

NodePtr<AssignStmt> Parser::parse_assign_stmt() {
    if (look().type != TokenType::ident) {
        return nullptr;
    }

    auto lhs = parse_ref_expr();
    expect(TokenType::equals);
    auto rhs_result = parse_expr();
    if (rhs_result.success()) {
        return make_node<AssignStmt>(std::move(lhs), rhs_result.unwrap());
    } else {
        // TODO For now, disregard error message and just hand out nullptr.
        return nullptr;
    }
}

NodePtr<ReturnStmt> Parser::parse_return_stmt() {
    expect(TokenType::kw_return);
    ExprPtr expr = parse_expr().unwrap();
    return make_node<ReturnStmt>(std::move(expr));
}

// Compound statement is a scoped block that consists of multiple statements.
// There is no restriction in order such as variable declarations should come
// first, etc.
//
// CompoundStmt:
//     { Stmt* }
NodePtr<CompoundStmt> Parser::parse_compound_stmt() {
    expect(TokenType::lbrace);
    auto compound = make_node<CompoundStmt>();
    ParseResult<Stmt> stmt_res;
    while ((stmt_res = parse_stmt()).success()) {
        compound->stmts.push_back(stmt_res.unwrap());
    }
    // did parse_stmt() fail at the end properly?
    if (look().type != TokenType::rbrace) {
        // report the last statement failure
        stmt_res.unwrap();
    }
    expect(TokenType::rbrace);
    return compound;
}

NodePtr<VarDecl> Parser::parse_var_decl() {
    auto start_pos = look().pos;
    bool mut = look().type == TokenType::kw_var;
    next();

    auto end_pos = look().pos + look().text.length();
    Token id = look();
    next();

    // Optional assignment expression
    ExprPtr rhs = nullptr;
    if (!mut) {
        expect(TokenType::equals, "initial value should be provided for immutable variables");
        rhs = parse_expr().unwrap();
    } else if (look().type == TokenType::equals) {
        next();
        rhs = parse_expr().unwrap();
    }
    if (rhs) {
        end_pos = rhs->end_pos;
    }

    // Insert to the name table
    Name *name = name_table.find(id.text);
    if (name == nullptr) {
        // New identifier, into the name table
        name = name_table.insert(id.text);
    }

    auto var_decl = make_node<VarDecl>(name, std::move(rhs), mut);
    var_decl->start_pos = start_pos;
    var_decl->end_pos = end_pos;
    return var_decl;
}

NodePtr<Function> Parser::parse_function() {
    expect(TokenType::kw_fn);

    Token name = look();
    auto func = make_node<Function>(name);
    next();

    // TODO: Argument list (foo(...))
    expect(TokenType::lparen);
    expect(TokenType::rparen);

    // Return type (-> ...)
    expect(TokenType::arrow);
    func->return_type = look();
    next();

    // Function body
    func->body = parse_compound_stmt();

    return func;
}

DeclPtr Parser::parse_decl() {
    switch (look().type) {
    case TokenType::kw_let:
    case TokenType::kw_var:
        return parse_var_decl();
    default:
        return nullptr;
    }
}

ExprPtr Parser::parse_literal_expr() {
    LiteralType type;
    // TODO Literals other than integers?
    switch (look().type) {
    case TokenType::number:
        type = LiteralType::integer;
        break;
    default:
        error("non-integer literals not implemented");
        break;
    }
    auto expr = make_node<LiteralExpr>(type, look());
    // TODO takes arbitrary token
    next();
    return expr;
}

NodePtr<RefExpr> Parser::parse_ref_expr() {
    auto ref_expr = make_node<RefExpr>();

    ref_expr->start_pos = look().pos;
    ref_expr->end_pos = look().pos + look().text.length();

    std::string text = look().text;
    ref_expr->name = name_table.find(text);
    if (ref_expr->name == nullptr) {
        // New identifier, into the name table
        ref_expr->name = name_table.insert(text);
    }

    next();

    return ref_expr;
}

ParseResult<Expr> Parser::parse_unary_expr() {
    switch (look().type) {
    case TokenType::number:
    case TokenType::string:
        return parse_literal_expr();
    case TokenType::ident:
        return parse_ref_expr();
    case TokenType::lparen: {
        expect(TokenType::lparen);
        auto expr = parse_expr();
        expect(TokenType::rparen);
        return expr;
    }
    default:
        // Because all expressions start with a unary expression, failing here
        // means no other expression could be matched as well, so just do a
        // really generic report.
        return ParseError(locate(), "expected an expression");
    }
}

int Parser::get_precedence(const Token &op) const {
    switch (op.type) {
    case TokenType::star:
    case TokenType::slash:
        return 1;
    case TokenType::plus:
    case TokenType::minus:
        return 0;
    default:
        // Not an operator
        return -1;
    }
}

ExprPtr Parser::parse_binary_expr_rhs(ExprPtr lhs, int precedence) {
    ExprPtr root = std::move(lhs);

    while (true) {
        int this_prec = get_precedence(look());

        // If the upcoming op has lower precedence, finish this subexpression.
        // It will be treated as a single term when this function is re-called
        // with lower precedence.
        if (this_prec < precedence) {
            return root;
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
            rhs = parse_binary_expr_rhs(std::move(rhs), precedence + 1);
        }

        // Create a new root with the old root as its LHS, and the recursion
        // result as RHS.  This implements left associativity.
        root = make_node<BinaryExpr>(std::move(root), op, std::move(rhs));
    }

    return root;
}

ParseResult<Expr> Parser::parse_expr() {
    auto res = parse_unary_expr();
    if (!res.success()) {
        return res;
    }
    // TODO don't unwrap here.
    return parse_binary_expr_rhs(res.unwrap());
}

void Parser::error(const std::string &msg) {
    auto loc = locate();
    std::cerr << loc.filename << ":" << loc.line << ":" << loc.col << ": ";
    std::cerr << "parse error: " << msg << std::endl;
    exit(1);
}

// The language is newline-aware, but newlines are mostly meaningless unless
// they are at the end of a statement or a declaration.  In those cases we use
// this to skip over them.
// @Cleanup: what about comments?
void Parser::skip_newlines() {
    while (look().type == TokenType::newline) {
        next();
    }
}

ToplevelPtr Parser::parse_toplevel() {
    skip_newlines();

    switch (look().type) {
    case TokenType::eos:
        return nullptr;
    case TokenType::kw_fn:
        return parse_function();
    case TokenType::comment:
    case TokenType::semicolon:
        next();
        return parse_toplevel();
    default:
        error("unrecognized toplevel statement");
    }
    return nullptr;
}

FilePtr Parser::parse_file() {
    auto file = make_node<File>();
    while (auto toplevel = parse_toplevel()) {
        file->toplevels.push_back(std::move(toplevel));
    }
    return file;
}

Ast Parser::parse() {
    return Ast{parse_file(), name_table};
}

} // namespace cmp
