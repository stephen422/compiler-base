#include "ast.h"

int AstNode::depth = 0;

AstNodePtr make_ast(NodeType type) {
    return AstNodePtr{new AstNode{type}};
}

AstNodePtr make_ast(NodeType type, const Token &tok) {
    return AstNodePtr{new AstNode{type, tok}};
}

void AstNode::add(AstNodePtr child) {
    children.push_back(std::move(child));
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

void Expr::print() {
    std::cout << "Expr::print(): " << type << std::endl;
}

void BinaryExpr::print() {
    out() << "[BinaryExpr]\n";

    PrintScope start;
    lhs->print();
    out() << "[Op] '" << op.lit << "'\n";
    rhs->print();
}
