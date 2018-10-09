#include "ast.h"

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

    out() << "[Id] " << id.lit << "\n";
    out() << "[Mutable:" << (mut ? "Y" : "N") << "]\n";
}

void BinaryExpr::print() const {
    out() << "[BinaryExpr]\n";

    PrintScope start;

    lhs->print();
    out() << "[Op] '" << op.lit << "'\n";
    rhs->print();
}

void LiteralExpr::print() const {
    out() << "[LiteralExpr] " << lit.lit << std::endl;
}

} // namespace comp
