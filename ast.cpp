#include "ast.h"
#include <sstream>

namespace comp {

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

void VarDecl::print() const {
    out() << "[VarDecl]\n";

    PrintScope start;

    out() << "[Id] " << id.text << "\n";
    out() << "[Mutable:" << (mut ? "Y" : "N") << "]\n";
}

void BinaryExpr::print() const {
    out() << "[BinaryExpr]\n";

    PrintScope start;

    lhs->print();
    out() << "[Op] '" << op.text << "'\n";
    rhs->print();
}

std::string BinaryExpr::flatten() const {
    return "(" + lhs->flatten() + std::string(op.text) + rhs->flatten() + ")";
}

void LiteralExpr::print() const {
    out() << "[LiteralExpr] " << lit.text << std::endl;
}

std::string LiteralExpr::flatten() const {
    return lit.text;
}

} // namespace comp
