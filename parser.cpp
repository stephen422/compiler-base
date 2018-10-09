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
        ss << "expected " << static_cast<int>(type)
           << ", got " << static_cast<int>(tok.type);
        error(ss.str());
    }
    next();
}

void Parser::expect_semi() {
    expect(TokenType::semicolon);
}

DeclPtr Parser::parse_var_decl() {
    bool mut = tok.type == TokenType::kw_var;
    next();

    Token id = tok;
    next();

    expect_semi();
    return std::make_unique<VarDecl>(id, mut);
}

DeclPtr Parser::parse_decl() {
    return parse_var_decl();
}

ExprPtr Parser::parse_literal_expr() {
    auto expr = std::make_unique<LiteralExpr>(tok);
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

ExprPtr Parser::parse_binary_expr_rhs(ExprPtr lhs, int precedence) {
    ExprPtr root = std::move(lhs);

    while (true) {
        int this_prec = get_precedence(tok);

        // If the upcoming op has lower precedence, the subexpression of the
        // precedence level that we are currently parsing in is finished.
        if (this_prec < precedence) {
            return root;
        }

        Token op = tok;
        next();

        // Parse the next term.  We do not know yet if this term should bind to
        // LHS or RHS; e.g. "a * b + c" or "a + b * c".  To know this, we should
        // look ahead for the operator that follows this term.
        ExprPtr rhs = parse_unary_expr();
        int next_prec = get_precedence(tok);

        // If the next operator is indeed higher-level, evaluate the RHS as a
        // whole subexpression with elevated minimum precedence. Else, just
        // treat it as a unary expression.
        if (this_prec < next_prec) {
            rhs = parse_binary_expr_rhs(std::move(rhs), precedence + 1);
        }

        // Submerge root as LHS, and attach the parsed RHS to create a new root.
        // This implies left associativity.
        root = std::make_unique<BinaryExpr>(root, op, rhs);
    }
    return root;
}

ExprPtr Parser::parse_expr() {
    auto expr = parse_unary_expr();
    expr = parse_binary_expr_rhs(std::move(expr));
    return expr;
}

void Parser::error(const std::string &msg) {
    auto loc = lexer.src.locate(tok.pos);
    std::cerr << lexer.src.path << ":" << loc.first << ":" << loc.second << ": ";
    std::cerr << "parse error: " << msg << std::endl;
    exit(1);
}

AstNodePtr Parser::parse() {
    while (true) {
        switch (tok.type) {
        case TokenType::kw_let:
        case TokenType::kw_var:
            return parse_decl();
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
