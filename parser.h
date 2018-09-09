// -*- C++ -*-
#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include <memory>

enum class NodeType {
    net_decl,
    assign,
    atom,
    range,
};

struct AST {
    using RootPtr = std::unique_ptr<AST>;

    AST(NodeType type) : type(type) {}
    AST(NodeType type, const Token &tok) : type(type), tok(std::move(tok)) {}

    void add(RootPtr child);
    void print();

    NodeType type;
    Token tok;
    std::vector<RootPtr> children;
    // Non-owning pointer to the next sibling
    // AST *sibling;
};

AST::RootPtr make_ast(NodeType type);
AST::RootPtr make_ast(NodeType type, const Token &tok);

class Parser {
public:
    Parser(Lexer &lexer);

    AST::RootPtr parse();

private:
    // Parse an identifier atom.
    AST::RootPtr parse_ident();

    // Parse a literal atom.
    AST::RootPtr parse_literal();

    // Parse [msb:lsb].
    AST::RootPtr parse_range();

    // Parse wire declaration.
    AST::RootPtr parse_netdecl();

    // Parse continuous assignments.
    AST::RootPtr parse_assign();

    // Get the next token from the lexer.
    void next();

    void expect(TokenType type);
    void expect_semi();

public:
    Lexer &lexer;
    Token tok; // lookahead token
};

#endif
