#include "parser.h"
#include <utility>

AST::NodePtr make_ast(NodeType type) {
    return AST::NodePtr{new AST{type}};
}

AST::NodePtr make_ast(NodeType type, const Token &tok) {
    return AST::NodePtr{new AST{type, tok}};
}

void AST::print() {
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

void AST::add(NodePtr child) {
    children.push_back(std::move(child));
}

AST::NodePtr Parser::parse_ident() {
    if (tok.type != TokenType::ident)
        expect(TokenType::ident);
    AST::NodePtr ast = make_ast(NodeType::atom, tok);
    next();
    return ast;
}

AST::NodePtr Parser::parse_literal() {
    AST::NodePtr ast = make_ast(NodeType::atom, tok);
    next();
    return ast;
}

AST::NodePtr Parser::parse_list() {
    AST::NodePtr node = make_ast(NodeType::list);
    node->add(parse_literal());
    while (tok.type == TokenType::comma) {
        next();
        node->add(parse_literal());
    }
    return node;
}

AST::NodePtr Parser::parse_range() {
    AST::NodePtr node = make_ast(NodeType::range);
    expect(TokenType::lbracket);
    node->add(parse_literal());
    expect(TokenType::colon);
    node->add(parse_literal());
    expect(TokenType::rbracket);
    return node;
}

AST::NodePtr Parser::parse_netdecl() {
    AST::NodePtr node = make_ast(NodeType::net_decl);
    next();

    // vectors
    if (tok.type == TokenType::lbracket)
        node->add(parse_range());

    // list of names
    node->add(parse_list());

    expect_semi();
    return node;
}

AST::NodePtr Parser::parse_assign() {
    AST::NodePtr node = make_ast(NodeType::assign);
    next(); // "assign"
    node->add(parse_ident());
    expect(TokenType::equals);
    node->add(parse_ident());
    expect_semi();
    return node;
}

AST::NodePtr Parser::parse_expr() {
    AST::NodePtr lhs;
    switch (tok.type) {
    case TokenType::number:
        lhs = parse_literal();
        break;
    case TokenType::ident:
        lhs = parse_ident();
        break;
    default:
        error("unrecognized LHS of an expression");
    }

    // Check termination
    if (tok.type == TokenType::semicolon) {
        next();
        return lhs;
    }

    Token op = tok;
    next();

    AST::NodePtr rhs = parse_expr();
    return rhs;
}

void Parser::error(const std::string &msg) {
    auto loc = lexer.src.locate(tok.pos);
    std::cerr << lexer.src.path << ":" << loc.first << ":" << loc.second << ": ";
    std::cerr << "parse error: " << msg << std::endl;
    exit(1);
}

AST::NodePtr Parser::parse() {
    AST::NodePtr ast = nullptr;

    while (true) {
        switch (tok.type) {
        case TokenType::kw_assign:
            return parse_assign();
        case TokenType::kw_wire:
        case TokenType::kw_reg:
            return parse_netdecl();
        case TokenType::comment:
            next();
            continue;
        default:
            return parse_expr();
        }
    }
    return ast;
}
