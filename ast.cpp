#include "ast.h"
#include "sema.h"
#include <cassert>
#include <sstream>

namespace cmp {

int AstNode::depth = 0;

// TODO: max is currently not being used.
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
    }
    return {min, max};
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
    if (expr)
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
    out() << "[VarDecl] " << name->text << "\n";
    PrintScope start;
    if (typeExpr) {
        typeExpr->print();
    }

    if (assignExpr) {
        out() << "[AssignExpr]\n";
        PrintScope start;
        assignExpr->print();
    }
}

void StructDecl::print() const {
    out() << "[StructDecl] " << name->text << "\n";
    PrintScope start;
    for (auto &m : members) {
        m->print();
    }
}

void FuncDecl::print() const {
    out() << "[FuncDecl] " << name->text << "\n";
    PrintScope start;
    for (auto &p : params)
        p->print();
    if (retTypeExpr)
        retTypeExpr->print();
    body->print();
}

void BinaryExpr::print() const {
    out() << "[BinaryExpr]\n";

    PrintScope start;
    lhs->print();
    fmt::print("[Op] '{}'\n", op.text);
    rhs->print();
}

void UnaryExpr::print() const {
    out() << "[UnaryExpr] ";

    switch (unary_kind) {
    case Paren: {
        std::cout << "Paren\n";
        PrintScope start;
        operand->print();
        break;
    }
    case Deref: {
        std::cout << "Deref\n";
        PrintScope start;
        operand->print();
        break;
    }
    case Address: {
        std::cout << "Address\n";
        PrintScope start;
        operand->print();
        break;
    }
    default:
        std::cout << "not implemented\n";
        break;
    }
}

void IntegerLiteral::print() const {
    out() << "[IntegerLiteral] " << value << std::endl;
}

void DeclRefExpr::print() const {
    out() << "[DeclRefExpr] " << "((Name *)";
    printf("0x..%04x", static_cast<uint32_t>(reinterpret_cast<uint64_t>(name)));
    std::cout << ") "<< name->text;
    if (type) {
        std::cout << " '" << type->name->text << "'";
    }
    std::cout << std::endl;
}

void TypeExpr::print() const {
    out() << "[TypeExpr] " << name->text << std::endl;
}

void BadStmt::print() const { out() << "[BadStmt]\n"; }
void BadDecl::print() const { out() << "[BadDecl]\n"; }
void BadExpr::print() const { out() << "[BadExpr]\n"; }

} // namespace cmp
