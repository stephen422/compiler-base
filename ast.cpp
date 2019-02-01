#include "ast.h"
#include <sstream>

namespace cmp {

int AstNode::depth = 0;

void File::print() const {
    out() << "[File]\n";
    PrintScope start;
    for (const auto &t: toplevels) {
        t->print();
    }
}

void File::traverse() const {
    for (auto &tl : toplevels) {
        tl->traverse();
    }
}

void DeclStmt::print() const {
    out() << "[DeclStmt]\n";
    PrintScope start;
    decl->print();
}

void DeclStmt::traverse() const {
    std::cout << "traversing DeclStmt\n";
    decl->traverse();
}

void ExprStmt::print() const {
    out() << "[ExprStmt]\n";
    PrintScope start;
    expr->print();
}

void ExprStmt::traverse() const {
    std::cout << "traversing ExprStmt\n";
    expr->traverse();
}

void AssignStmt::print() const {
    out() << "[AssignStmt]\n";
    PrintScope start;
    out() << "[LHS] " << lhs << "\n";
    rhs->print();
}

void AssignStmt::traverse() const {
    rhs->traverse();
}

void ReturnStmt::print() const {
    out() << "[ReturnStmt]\n";
    PrintScope start;
    expr->print();
}

void ReturnStmt::traverse() const {
    expr->traverse();
}

void CompoundStmt::print() const {
    out() << "[CompoudStmt]\n";
    PrintScope start;
    for (auto const& s : stmts) {
        s->print();
    }
}

void CompoundStmt::traverse() const {
    std::cout << "traversing CompoundStmt\n";
    for (auto &stmt : stmts) {
        stmt->traverse();
    }
}

void VarDecl::print() const {
    out() << "[VarDecl] " << id << " " << (mut ? "(mut)" : "") <<"\n";

    if (assign_expr) {
        out() << "[AssignExpr]\n";
        PrintScope start;
        assign_expr->print();
    }
}

void VarDecl::traverse() const {
    std::cout << "traversing VarDecl\n";
    if (assign_expr) {
        assign_expr->traverse();
    }
}

void Function::print() const {
    out() << "[Function] " << id << " " << return_type << "\n";
    PrintScope start;

    body->print();
}

void BinaryExpr::print() const {
    out() << "[BinaryExpr]\n";
    PrintScope start;

    lhs->print();
    out() << "[Op] '" << op.text << "'\n";
    rhs->print();
}

void BinaryExpr::traverse() const {
    lhs->traverse();
    rhs->traverse();
}

std::string BinaryExpr::flatten() const {
    return "(" + lhs->flatten() + std::string(op.text) + rhs->flatten() + ")";
}

void LiteralExpr::print() const {
    out() << "[LiteralExpr] " << lit.text << std::endl;
}

void LiteralExpr::traverse() const {
}

std::string LiteralExpr::flatten() const {
    return lit.text;
}

} // namespace cmp
