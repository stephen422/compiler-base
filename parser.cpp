#include "parser.h"
#include <utility>
#include <sstream>

using AstNodePtr = AstNode::OwnPtr;

int AstNode::depth = 0;

AstNodePtr make_ast(NodeType type) {
    return AstNodePtr{new AstNode{type}};
}

AstNodePtr make_ast(NodeType type, const Token &tok) {
    return AstNodePtr{new AstNode{type, tok}};
}

void AstNode::print() {
    switch (type) {
    case NodeType::net_decl:
        std::cout << "netdecl\n";
        break;
    case NodeType::assign:
        std::cout << "assign\n";
        break;
    case NodeType::list:
        std::cout << "list\n";
        break;
    case NodeType::range:
        std::cout << "range\n";
        break;
    default:
        std::cout << tok << std::endl;
        break;
    }
    for (const auto &c : children)
        c->print();
}

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

void AstNode::add(AstNodePtr child) {
    children.push_back(std::move(child));
}

void Expr::print() {
    std::cout << "Expr::print(): " << type << std::endl;
}

void BinaryExpr::print() {
    out() << "[BinaryExpr]\n";

    PrintScope start;

    if (lhs)
        lhs->print();
    else
        out() << "null\n";
    out() << "[Op] '" << op.lit << "'\n";
    if (rhs)
        rhs->print();
    else
        out() << "null\n";
}

ExprPtr Parser::parse_ident() {
    if (tok.type != TokenType::ident)
        expect(TokenType::ident);
    // AstNodePtr ast = make_ast(NodeType::atom, tok);
    ExprPtr expr{new Expr{Expr::literal}};
    next();
    return expr;
}

ExprPtr Parser::parse_literal() {
    ExprPtr expr{new LiteralExpr{tok}};
    next();
    return expr;
}

void LiteralExpr::print() {
    out() << "[LiteralExpr] " << lit.lit << std::endl;
}

#if 0
ExprPtr Parser::parse_list() {
    AstNodePtr node = make_ast(NodeType::list);
    node->add(parse_literal());
    while (tok.type == TokenType::comma) {
        next();
        node->add(parse_literal());
    }
    return nullptr;
}

AstNodePtr Parser::parse_range() {
    AstNodePtr node = make_ast(NodeType::range);
    expect(TokenType::lbracket);
    node->add(parse_literal());
    expect(TokenType::colon);
    node->add(parse_literal());
    expect(TokenType::rbracket);
    return node;
}

AstNodePtr Parser::parse_netdecl() {
    AstNodePtr node = make_ast(NodeType::net_decl);
    next();

    // vectors
    if (tok.type == TokenType::lbracket)
        node->add(parse_range());

    // list of names
    node->add(parse_list());

    expect_semi();
    return node;
}

AstNodePtr Parser::parse_assign() {
    AstNodePtr node = make_ast(NodeType::assign);
    next(); // "assign"
    node->add(parse_ident());
    expect(TokenType::equals);
    node->add(parse_ident());
    expect_semi();
    return node;
}
#endif

ExprPtr Parser::parse_unary_expr() {
    switch (tok.type) {
    case TokenType::number:
    case TokenType::ident:
        return parse_literal();
    case TokenType::lparen: {
        expect(TokenType::lparen);
        auto expr = parse_binary_expr();
        expect(TokenType::rparen);
        return expr;
        // error("ParenExpr not yet implemented");
    }
    default:
        error("not a unary expression");
        return nullptr;
    }
}

ExprPtr Parser::parse_binary_expr() {
    ExprPtr lhs = parse_unary_expr();

    // Check op
    if (!(tok.type == TokenType::star ||
          tok.type == TokenType::plus ||
          tok.type == TokenType::minus)) {
        // next();
        return lhs;
    }

    Token op = tok;
    next();

    ExprPtr rhs = parse_binary_expr();
    ExprPtr expr{new BinaryExpr{lhs, op, rhs}};
    return expr;
}

void Parser::error(const std::string &msg) {
    auto loc = lexer.src.locate(tok.pos);
    std::cerr << lexer.src.path << ":" << loc.first << ":" << loc.second << ": ";
    std::cerr << "parse error: " << msg << std::endl;
    exit(1);
}

AstNodePtr Parser::parse() {
    ExprPtr ast = nullptr;

    while (true) {
        switch (tok.type) {
        // case TokenType::kw_assign:
        //     return parse_assign();
        // case TokenType::kw_wire:
        // case TokenType::kw_reg:
        //     return parse_netdecl();
        case TokenType::comment:
        case TokenType::semicolon:
            next();
            continue;
        default:
            return parse_binary_expr();
        }
    }
    return ast;
}
