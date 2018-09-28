#include "parser.h"
#include <utility>

using NodePtr = AstNode::NodePtr;

NodePtr make_ast(NodeType type) {
    return NodePtr{new AstNode{type}};
}

NodePtr make_ast(NodeType type, const Token &tok) {
    return NodePtr{new AstNode{type, tok}};
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
        std::cerr << "parse error: expected " << static_cast<int>(type)
                  << ", got " << static_cast<int>(tok.type) << std::endl;
        exit(1); // FIXME
    }
    next();
}

void Parser::expect_semi() {
    expect(TokenType::semicolon);
}

void AstNode::add(NodePtr child) {
    children.push_back(std::move(child));
}

void Expr::print() {
    std::cout << "Expr::print(): " << type << std::endl;
}

void BinaryExpr::print() {
    std::cout << "[BinaryExpr]\n";
    if (lhs)
        lhs->print();
    std::cout << op.lit << std::endl;
    if (rhs)
        rhs->print();
    std::cout << std::endl;
}

ExprPtr Parser::parse_ident() {
    if (tok.type != TokenType::ident)
        expect(TokenType::ident);
    // NodePtr ast = make_ast(NodeType::atom, tok);
    ExprPtr expr{new Expr{Expr::literal}};
    next();
    return expr;
}

ExprPtr Parser::parse_literal() {
    ExprPtr expr{new Expr{Expr::literal}};
    next();
    return expr;
}

#if 0
ExprPtr Parser::parse_list() {
    NodePtr node = make_ast(NodeType::list);
    node->add(parse_literal());
    while (tok.type == TokenType::comma) {
        next();
        node->add(parse_literal());
    }
    return nullptr;
}

NodePtr Parser::parse_range() {
    NodePtr node = make_ast(NodeType::range);
    expect(TokenType::lbracket);
    node->add(parse_literal());
    expect(TokenType::colon);
    node->add(parse_literal());
    expect(TokenType::rbracket);
    return node;
}

NodePtr Parser::parse_netdecl() {
    NodePtr node = make_ast(NodeType::net_decl);
    next();

    // vectors
    if (tok.type == TokenType::lbracket)
        node->add(parse_range());

    // list of names
    node->add(parse_list());

    expect_semi();
    return node;
}

NodePtr Parser::parse_assign() {
    NodePtr node = make_ast(NodeType::assign);
    next(); // "assign"
    node->add(parse_ident());
    expect(TokenType::equals);
    node->add(parse_ident());
    expect_semi();
    return node;
}
#endif

ExprPtr Parser::parse_unary_expr() {
    ExprPtr expr{new Expr{Expr::unary}};
    switch (tok.type) {
    case TokenType::number:
        // return parse_literal();
        return expr;
    case TokenType::ident:
        // return parse_ident();
        return expr;
    default:
        return nullptr;
    }
}

ExprPtr Parser::parse_binary_expr() {
    ExprPtr lhs = parse_unary_expr();

    // Check op
    if (!(tok.type == TokenType::star ||
          tok.type == TokenType::plus ||
          tok.type == TokenType::minus)) {
        next();
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

NodePtr Parser::parse() {
    ExprPtr ast = nullptr;

    while (true) {
        switch (tok.type) {
        // case TokenType::kw_assign:
        //     return parse_assign();
        // case TokenType::kw_wire:
        // case TokenType::kw_reg:
        //     return parse_netdecl();
        case TokenType::comment:
            next();
            continue;
        default:
            return parse_binary_expr();
        }
    }
    return ast;
}
