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
    case NodeType::assign:
        std::cout << "assign\n";
        break;;
    default:
        std::cout << tok << std::endl;
        break;
    }
    for (const auto &c : children)
        c->print();
}

Parser::Parser(Lexer &lexer) : lexer(lexer), tok(std::move(lexer.lex()))
{
}

void Parser::next()
{
    tok = std::move(lexer.lex());
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

AST::RootPtr Parser::parse_assign()
{
    AST::RootPtr node {new AST(NodeType::assign)};
    next(); // "assign"
    node->add(parse_ident());
    expect(TokenType::equals);
    node->add(parse_ident());
    return node;
}

void Parser::parse()
{
    AST::RootPtr ast;
    for (; tok.type != TokenType::eos; next()) {
        if (tok.type == TokenType::kw_assign) {
            ast = parse_assign();
        }
    }
    ast->print();
}
