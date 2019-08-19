#include "ast.h"
#include "sema.h"
#include <cassert>
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
        if (node->startPos < min) {
            min = node->startPos;
        }
        if (node->endPos > max) {
            max = node->endPos;
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
    expr->print();
}

void CompoundStmt::print() const {
    out() << "[CompoudStmt]\n";
    PrintScope start;
    for (auto const& s : stmts) {
        s->print();
    }
}

void ParamDecl::print() const {
    out() << "[ParamDecl] " << name->text << (mut ? " (mut)" : " ") << "\n";
    PrintScope start;
    if (type_expr) {
        type_expr->print();
    }
}

void VarDecl::print() const {
    out() << "[VarDecl] " << name->text << (mut ? " (mut)" : " ") << "\n";
    PrintScope start;
    if (type_expr) {
        type_expr->print();
    }

    if (assign_expr) {
        out() << "[AssignExpr]\n";
        PrintScope start;
        assign_expr->print();
    }
}

void FuncDecl::print() const {
    out() << "[FuncDecl] " << name->text << "\n";
    PrintScope start;
    for (auto &param : paramDeclList) {
        param->print();
    }
    retTypeExpr->print();
    body->print();
}

void BinaryExpr::print() const {
    out() << "[BinaryExpr]\n";
    PrintScope start;

    lhs->print();
    out() << "[Op] '" << op.text << "'\n";
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
    if (inferred_type) {
        std::cout << " '" << inferred_type->name->text << "'";
    }
    std::cout << std::endl;
}

void TypeExpr::print() const {
    out() << "[TypeExpr] " << name->text << std::endl;
}

} // namespace cmp
