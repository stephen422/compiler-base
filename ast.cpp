#include "ast.h"
#include "sema.h"
#include <sstream>

namespace cmp {

int AstNode::depth = 0;

std::pair<size_t, size_t> get_ast_range(std::initializer_list<AstNode *> nodes) {
    size_t min = static_cast<size_t>(-1);
    size_t max = 0;
    for (auto node : nodes) {
        if (!node) {
            continue;
        }
        if (node->start_pos < min) {
            min = node->start_pos;
        }
        if (node->end_pos > max) {
            max = node->end_pos;
        }
    }
    return {min, max};
}

//
// AST Traversal
//

void File::traverse(Semantics &sema) const {
    for (auto &tl : toplevels) {
        tl->traverse(sema);
    }
}

void DeclStmt::traverse(Semantics &sema) const {
    std::cout << "traversing DeclStmt\n";
    decl->traverse(sema);
}

void ExprStmt::traverse(Semantics &sema) const {
    std::cout << "traversing ExprStmt\n";
    expr->traverse(sema);
}

void AssignStmt::traverse(Semantics &sema) const {
    lhs->traverse(sema);
    rhs->traverse(sema);
}

void ReturnStmt::traverse(Semantics &sema) const {
    expr->traverse(sema);
}

void CompoundStmt::traverse(Semantics &sema) const {
    std::cout << "traversing CompoundStmt\n";
    for (auto &stmt : stmts) {
        stmt->traverse(sema);
    }
}

void VarDecl::traverse(Semantics &sema) const {
    auto old_decl = sema.decl_table.find(name);
    if (old_decl != nullptr) { // TODO: check scope
        sema.error(start_pos, "redefinition");
    }
    Declaration decl{*name};
    sema.decl_table.push(Symbol {name, decl});

    if (assign_expr) {
        assign_expr->traverse(sema);
    }
}

void Function::traverse(Semantics &sema) const {
    body->traverse(sema);
}

void LiteralExpr::traverse(Semantics &sema) const {
}

void RefExpr::traverse(Semantics &sema) const {
    if (sema.decl_table.find(name) == nullptr) {
        sema.error(start_pos, "undeclared identifier");
    }
}

void BinaryExpr::traverse(Semantics &sema) const {
    std::cout << "[BinaryExpr] start_pos: " << start_pos << ", end_pos: " << end_pos << std::endl;
    lhs->traverse(sema);
    rhs->traverse(sema);
}

//
// AST Printing
//

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

void AssignStmt::print() const {
    out() << "[AssignStmt]\n";
    PrintScope start;
    lhs->print();
    rhs->print();
}

void ReturnStmt::print() const {
    out() << "[ReturnStmt]\n";
    PrintScope start;
    expr->print();
}

void CompoundStmt::print() const {
    out() << "[CompoudStmt]\n";
    PrintScope start;
    for (auto const& s : stmts) {
        s->print();
    }
}

void VarDecl::print() const {
    out() << "[VarDecl] " << name->text << " " << (mut ? "(mut)" : "") <<"\n";

    if (assign_expr) {
        out() << "[AssignExpr]\n";
        PrintScope start;
        assign_expr->print();
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

std::string BinaryExpr::flatten() const {
    return "(" + lhs->flatten() + std::string(op.text) + rhs->flatten() + ")";
}

void LiteralExpr::print() const {
    out() << "[LiteralExpr] " << lit.text << std::endl;
}

void RefExpr::print() const {
    out() << "[RefExpr] " << "((Name *)";
    printf("0x...%08x", static_cast<uint32_t>(reinterpret_cast<uint64_t>(name)));
    std::cout << ") "<< name->text << std::endl;
}

std::string LiteralExpr::flatten() const {
    return lit.text;
}

std::string RefExpr::flatten() const {
    return name->text;
}

} // namespace cmp
