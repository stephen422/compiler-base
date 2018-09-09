// -*- C++ -*-
#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include <memory>

enum NodeType {
    assign,
    atom,
};

struct AST {
    using RootPtr = std::unique_ptr<AST>;

    AST(NodeType type) : type(type) {}
    AST(NodeType type, const Token &tok) : type(type), tok(std::move(tok)) {}

    void add_child(RootPtr child);
    void print();

    NodeType type;
    Token tok;
    std::vector<RootPtr> children;
    // Non-owning pointer to the next sibling
    // AST *sibling;
};

class Parser {
public:
    Parser(Lexer &lexer) : lexer(lexer), tok(std::move(lexer.lex())) {}

    void parse();

private:
    AST::RootPtr parse_assign();

    // Get the next token from the lexer.
    void next();

    void expect(TokenType type);

    Lexer &lexer;
    Token tok; // lookahead token
};

#endif
