#include "parser.h"
#include <utility>
#include <sstream>

namespace cmp {

void ParseError::report() const {
    std::cerr << location.filename << ":" << location.line << ":" << location.col << ": ";
    std::cerr << "parse error: " << message << std::endl;
}

template <typename T>
NodePtr<T> ParseResult<T>::unwrap() {
    if (success()) {
        return std::move(result);
    }
    for (auto const& e : errors) {
        e.report();
    }
    // TODO: exit more gracefully?
    exit(EXIT_FAILURE);
    return nullptr;
}

void Parser::next() {
    tok = lexer.lex();
}

void Parser::expect(TokenType type) {
    if (tok.type != type) {
        std::stringstream ss;
        ss << "expected " << tokentype_to_string(type)
           << ", got " << tokentype_to_string(tok.type);
        error(ss.str());
    }
    next();
}

void Parser::expect_semi() {
    expect(TokenType::semicolon);
}

// Parse a statement.
//
// Stmt:
//     Decl ;
//     Expr ;
//     ID () ; // TODO: CallExpr
//     ;
StmtPtr Parser::parse_stmt() {
    StmtPtr stmt;

    // Try possible productions one by one, and use what succeeds first.
    // ("recursive descent with backtracking":
    // https://en.wikipedia.org/wiki/Recursive_descent_parser)
    if (tok.type == TokenType::semicolon) {
        // Empty statement (;)
        next();
    } else if (tok.type == TokenType::comment) {
        // FIXME: is it best to filter out comments in parse_stmt()?
        next();
        stmt = parse_stmt();
    } else if (auto decl = parse_decl()) {
        // Decl ;
        expect_semi();
        stmt = make_node<DeclStmt>(std::move(decl));
    } else if (auto res = parse_expr(); res.success()) {
        // Expr ;
        expect_semi();
        stmt = make_node<ExprStmt>(res.unwrap());
    } else if (tok.type == TokenType::kw_return) {
        stmt = parse_return_stmt();
    } else {
        stmt = nullptr;
    }
    return stmt;
}

NodePtr<ReturnStmt> Parser::parse_return_stmt() {
    expect(TokenType::kw_return);
    ExprPtr expr = parse_expr().unwrap();
    // TODO: There are cases where the result pointer of the parse is allowed
    // to be nullptr or not -- here it is not.  It would be good to have a way
    // to express this clearly in code, but without the bulky if (result !=
    // nullptr) blocks.  Maybe ParserResult.unwrap() or similar.
    return make_node<ReturnStmt>(std::move(expr));
}

// Compound statement is a scoped block that consists of multiple statements.
// There is no restriction in order such as variable declarations should come
// first, etc.
//
// CompoundStmt:
//     { Stmt* }
NodePtr<CompoundStmt> Parser::parse_compound_stmt() {
    auto compound = make_node<CompoundStmt>();
    while (auto stmt = parse_stmt()) {
        compound->stmts.push_back(std::move(stmt));
    }
    return compound;
}

NodePtr<VarDecl> Parser::parse_var_decl() {
    bool mut = tok.type == TokenType::kw_var;
    next();

    Token id = tok;
    next();

    ExprPtr rhs = nullptr;
    if (tok.type == TokenType::equals) {
        next();
        // RHS of an assignment should be an expression.
        rhs = parse_expr().unwrap();
    }
    return make_node<VarDecl>(id, std::move(rhs), mut);
}

FunctionPtr Parser::parse_function() {
    expect(TokenType::kw_fn);

    Token name = tok;
    auto func = make_node<Function>(name);
    next();

    // TODO: Argument list (foo(...))
    expect(TokenType::lparen);
    expect(TokenType::rparen);

    // Return type (-> ...)
    expect(TokenType::arrow);
    func->return_type = tok;
    next();

    expect(TokenType::lbrace);
    // List of statements
    func->body = parse_compound_stmt();
    expect(TokenType::rbrace);

    return func;
}

DeclPtr Parser::parse_decl() {
    switch (tok.type) {
    case TokenType::kw_let:
    case TokenType::kw_var:
        return parse_var_decl();
    default:
        return nullptr;
    }
}

ExprPtr Parser::parse_literal_expr() {
    auto expr = make_node<LiteralExpr>(tok);
    // TODO takes arbitrary token
    next();
    return expr;
}

ParseResult<Expr> Parser::parse_unary_expr() {
    switch (tok.type) {
    case TokenType::number:
    case TokenType::ident:
    case TokenType::string: {
        auto expr = parse_literal_expr();
        return ParseResult<Expr>{expr};
        // return parse_literal_expr();
    }
    case TokenType::lparen: {
        expect(TokenType::lparen);
        auto expr = parse_expr();
        expect(TokenType::rparen);
        return expr;
    }
    default:
        // Because all expressions start with a unary expression, failing here
        // means no other expression could be matched as well, so we just
        // report for the most generic type of expression.
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
        int this_prec = get_precedence(tok);

        // If the upcoming op has lower precedence, finish this subexpression.
        // It will be treated as a single term when this function is re-called
        // with lower precedence.
        if (this_prec < precedence) {
            return root;
        }

        Token op = tok;
        next();

        // Parse the second term.
        ExprPtr rhs = parse_unary_expr().unwrap();
        // We do not know if this term should associate to left or right;
        // e.g. "(a * b) + c" or "a + (b * c)".  We should look ahead for the
        // next operator that follows this term.
        int next_prec = get_precedence(tok);

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

ToplevelPtr Parser::parse_toplevel() {
    switch (tok.type) {
    case TokenType::eos:
        return nullptr;
    case TokenType::kw_fn:
        return parse_function();
    case TokenType::comment:
    case TokenType::semicolon:
        next();
        return parse_toplevel();
    default:
        error("unrecognized toplevel");
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

AstNodePtr Parser::parse() {
    return parse_file();
}

} // namespace cmp
