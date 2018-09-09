#include "parser.h"

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

void AST::add_child(RootPtr child)
{
    children.push_back(std::move(child));
}

AST::RootPtr Parser::parse_assign()
{
    AST::RootPtr node {new AST(NodeType::assign)};
    next(); // "assign"
    expect(TokenType::ident);
    node->add_child(AST::RootPtr{new AST(NodeType::atom, tok)});
    expect(TokenType::equals);
    node->add_child(AST::RootPtr{new AST(NodeType::atom, tok)});
    expect(TokenType::ident);
    node->add_child(AST::RootPtr{new AST(NodeType::atom, tok)});
    return node;
}

void Parser::parse()
{
    AST::RootPtr ast;
    for (; tok.type != TokenType::eos; next()) {
        if (tok.type == TokenType::kw_assign) {
            std::cout << "assign!\n";
            ast = parse_assign();
        }
    }
    ast->print();
}
