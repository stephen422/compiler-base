#include "parser.h"
#include <utility>

AST::RootPtr make_ast(NodeType type)
{
    return AST::RootPtr{new AST{type}};
}

AST::RootPtr make_ast(NodeType type, const Token &tok)
{
    return AST::RootPtr{new AST{type, tok}};
}

void AST::print()
{
    switch (type) {
    case NodeType::net_decl:
        std::cout << "netdecl\n";
        break;;
    case NodeType::assign:
        std::cout << "assign\n";
        break;;
    case NodeType::range:
        std::cout << "range\n";
        break;;
    default:
        std::cout << tok << std::endl;
        break;
    }
    for (const auto &c : children)
        c->print();
}

Parser::Parser(Lexer &lexer) : lexer(lexer), tok(lexer.lex())
{
}

void Parser::next()
{
    tok = lexer.lex();
}

void Parser::expect(TokenType type)
{
    if (tok.type != type) {
        std::cerr << "parse error: expected " << static_cast<int>(type)
                  << ", got " << static_cast<int>(tok.type) << std::endl;
        exit(1); // FIXME
    }
    next();
}

void Parser::expect_semi()
{
    expect(TokenType::semicolon);
}

void AST::add(RootPtr child)
{
    children.push_back(std::move(child));
}

AST::RootPtr Parser::parse_ident()
{
    if (tok.type != TokenType::ident)
        expect(TokenType::ident);
    AST::RootPtr ast = make_ast(NodeType::atom, tok);
    next();
    return ast;
}

AST::RootPtr Parser::parse_literal()
{
    AST::RootPtr ast = make_ast(NodeType::atom, tok);
    next();
    return ast;
}

AST::RootPtr Parser::parse_range()
{
    AST::RootPtr node = make_ast(NodeType::range);
    expect(TokenType::lbracket);
    node->add(parse_literal());
    expect(TokenType::colon);
    node->add(parse_literal());
    expect(TokenType::rbracket);
    return node;
}

AST::RootPtr Parser::parse_netdecl()
{
    AST::RootPtr node = make_ast(NodeType::net_decl);
    next();

    // vectors
    if (tok.type == TokenType::lbracket)
        node->add(parse_range());

    // name
    // TODO: list
    node->add(parse_ident());

    expect_semi();
    return node;
}

AST::RootPtr Parser::parse_assign()
{
    AST::RootPtr node = make_ast(NodeType::assign);
    next(); // "assign"
    node->add(parse_ident());
    expect(TokenType::equals);
    node->add(parse_ident());
    expect_semi();
    return node;
}

AST::RootPtr Parser::parse()
{
    AST::RootPtr ast = nullptr;

    switch (tok.type) {
    case TokenType::kw_assign:
        ast = parse_assign();
        break;
    case TokenType::kw_wire:
    case TokenType::kw_reg:
        ast = parse_netdecl();
        break;
    default:
        break;
    }
    return ast;
}
