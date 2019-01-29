#include "ast.h"
#include <sstream>

namespace comp {

int AstNode::depth = 0;

void File::print() const {
    out() << "[File]\n";
    PrintScope start;
    for (const auto &t: toplevels) {
        t->print();
    }
}

void DeclStmt::print() const {
    out() << "[DeclStmt]\n";
    PrintScope start;
    decl->print();
}

void ExprStmt::print() const {
    out() << "[ExprStmt]\n";
    PrintScope start;
    expr->print();
}

void VarDecl::print() const {
    out() << "[VarDecl] " << id << " " << (mut ? "mut" : "") <<"\n";

    if (assign_expr != nullptr) {
        out() << "[AssignExpr]\n";
        PrintScope start;
        assign_expr->print();
    }
}

void Function::print() const {
    out() << "[Function] " << id << "\n";
    PrintScope start;

    out() << "[stmt_list]\n";;
    PrintScope s2;
    for (const auto &s: stmt_list) {
        s->print();
    }
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
