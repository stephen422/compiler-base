#include "parser.h"
#include <utility>
#include <sstream>

namespace comp {

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
        stmt = std::make_unique<DeclStmt>(decl);
    } else if (auto expr = parse_expr()) {
        // Expr ;
        expect_semi();
        stmt = std::make_unique<ExprStmt>(expr);
    } else {
        stmt = nullptr;
    }
    return stmt;
}

DeclPtr Parser::parse_var_decl() {
    bool mut = tok.type == TokenType::kw_var;
    next();

    Token id = tok;
    next();

    ExprPtr rhs = nullptr;
    if (tok.type == TokenType::equals) {
        next();
        // RHS of an assignment should be an expression.
        rhs = parse_expr();
    }
    return std::make_unique<VarDecl>(id, rhs, mut);
}

DeclPtr Parser::parse_func_decl() {
    expect(TokenType::kw_fn);

    Token name = tok;
    auto func = std::make_unique<FuncDecl>(name);
    next();

    // Argument list (foo(...))
    expect(TokenType::lparen);
    expect(TokenType::rparen);

    // Return type (-> ...)
    expect(TokenType::arrow);
    // TODO return type
    next();

    expect(TokenType::lbrace);
    // List of statements
    StmtPtr stmt;
    while ((stmt = parse_stmt())) {
        func->add_stmt(stmt);
    }
    expect(TokenType::rbrace);

    error("implementing return type!");
    return std::make_unique<FuncDecl>(name);
}

DeclPtr Parser::parse_decl() {
    switch (tok.type) {
    case TokenType::kw_let:
    case TokenType::kw_var:
        return parse_var_decl();
    case TokenType::kw_fn:
        return parse_func_decl();
    default:
        error("unknown declaration type");
        return nullptr;
    }
}

ExprPtr Parser::parse_literal_expr() {
    auto expr = std::make_unique<LiteralExpr>(tok);
    // TODO takes arbitrary token
    next();
    return expr;
}

ExprPtr Parser::parse_unary_expr() {
    switch (tok.type) {
    case TokenType::number:
    case TokenType::ident:
        return parse_literal_expr();
    case TokenType::lparen: {
        expect(TokenType::lparen);
        auto expr = parse_expr();
        expect(TokenType::rparen);
        return expr;
    }
    default:
        error("expected a unary expression");
        return nullptr;
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

// @cleanup: Ownership of 'lhs' is not clear.
ExprPtr Parser::parse_binary_expr_rhs(ExprPtr &lhs, int precedence) {
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
        ExprPtr rhs = parse_unary_expr();
        // We do not know if this term should associate to left or right;
        // e.g. "(a * b) + c" or "a + (b * c)".  We should look ahead for the
        // next operator that follows this term.
        int next_prec = get_precedence(tok);

        // If the next operator is indeed higher-level ("a + (b * c)"),
        // evaluate the RHS as a single subexpression with elevated minimum
        // precedence. Else ("(a * b) + c"), just treat it as a unary
        // expression.
        if (this_prec < next_prec) {
            rhs = parse_binary_expr_rhs(rhs, precedence + 1);
        }

        // Create a new root with the old root as its LHS, and the recursion
        // result as RHS.  This implements left associativity.
        root = std::make_unique<BinaryExpr>(root, op, rhs);
    }

    return root;
}

ExprPtr Parser::parse_expr() {
    auto expr = parse_unary_expr();
    expr = parse_binary_expr_rhs(expr);
    return expr;
}

void Parser::error(const std::string &msg) {
    auto loc = locate();
    std::cerr << lexer.src.filename << ":" << loc.first << ":" << loc.second << ": ";
    std::cerr << "parse error: " << msg << std::endl;
    exit(1);
}

AstNodePtr Parser::parse() {
    while (true) {
        switch (tok.type) {
        case TokenType::eos:
            return nullptr;
        case TokenType::kw_let:
        case TokenType::kw_var:
        case TokenType::kw_fn:
            return parse_stmt();
        case TokenType::comment:
        case TokenType::semicolon:
            next();
            continue;
        default:
            return parse_expr();
        }
    }
    return nullptr;
}

} // namespace comp
