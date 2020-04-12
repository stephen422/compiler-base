#include "ast.h"
#include "sema.h"
#include <cassert>
#include <sstream>

namespace cmp {

int AstNode::indent = 0;

// TODO: max is currently not being used.
std::pair<size_t, size_t> get_ast_range(std::initializer_list<AstNode *> nodes) {
    size_t min = static_cast<size_t>(-1);
    size_t max = 0;
    for (auto node : nodes) {
        if (!node) {
            continue;
        }
        if (node->pos < min) {
            min = node->pos;
        }
    }
    return {min, max};
}

std::optional<Decl> Expr::decl() const {
    switch (kind) {
    case ExprKind::decl_ref:
        return as<DeclRefExpr>()->var_decl;
    case ExprKind::func_call:
        return as<FuncCallExpr>()->func_decl;
    case ExprKind::member:
        return as<MemberExpr>()->var_decl;
    case ExprKind::unary:
        if (as<UnaryExpr>()->unary_kind == UnaryExprKind::paren)
            return as<UnaryExpr>()->as<ParenExpr>()->decl();
        break;
    default:
        break;
    }
    return {};
}

//
// AST print functions
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
    if (expr) {
        expr->print();
    }
}

void IfStmt::print() const {
    out() << "[IfStmt]\n";
    PrintScope start;
    cond->print();
    cstmt_true->print();
    if (elseif) {
        elseif->print();
    } else if (cstmt_false) {
        cstmt_false->print();
    }
}

void CompoundStmt::print() const {
    out() << "[CompoudStmt]\n";
    PrintScope start;
    for (auto const& s : stmts) {
        s->print();
    }
}

void VarDeclNode::print() const {
    out() << "[VarDecl] " << name->text << "\n";
    PrintScope start;
    if (type_expr) {
        type_expr->print();
    }

    if (assign_expr) {
        out() << "'='\n";
        PrintScope start;
        assign_expr->print();
    }
}

void StructDeclNode::print() const {
    out() << "[StructDecl] " << name->text << "\n";
    PrintScope start;
    for (auto &m : members)
        m->print();
}

void FuncDeclNode::print() const {
    out() << "[FuncDecl] " << name->text << "\n";
    PrintScope start;
    for (auto &p : args)
        p->print();
    if (ret_type_expr)
        ret_type_expr->print();
    body->print();
}

void BinaryExpr::print() const {
    out() << "[BinaryExpr]\n";

    PrintScope start;
    lhs->print();
    out() << "[Op] " << "'{}'\n";
    rhs->print();
}

void UnaryExpr::print() const {
    out() << "[UnaryExpr] ";

    switch (unary_kind) {
    case UnaryExprKind::deref: {
        std::cout << "Deref\n";
        PrintScope start;
        operand->print();
        break;
    }
    case UnaryExprKind::address: {
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

void StringLiteral::print() const {
    out() << "[StringLiteral] " << value << std::endl;
}

void DeclRefExpr::print() const {
    out() << "[DeclRefExpr] " << "(Name:";
    printf("%04x", static_cast<uint32_t>(reinterpret_cast<uint64_t>(name)));
    std::cout << ") "<< name->text;
    if (type)
        std::cout << " '" << type->name->text << "'";
    std::cout << std::endl;
}

void FuncCallExpr::print() const {
    out() << "[FuncCallExpr] "
          << "(Name:";
    printf("%04x", static_cast<uint32_t>(reinterpret_cast<uint64_t>(func_name)));
    std::cout << ") " << func_name->text;
    if (type)
        std::cout << " '" << type->name->text << "'";
    std::cout << std::endl;

    PrintScope start;
    for (auto &arg : args)
        arg->print();
}

void ParenExpr::print() const {
    out() << "[ParenExpr]" << std::endl;
    PrintScope start;
    operand->print();
}

void MemberExpr::print() const {
    out() << "[MemberExpr]" << std::endl;
    PrintScope start;
    struct_expr->print();
    out() << "'.' " << member_name->text << std::endl;
}

void TypeExpr::print() const {
    out() << "[TypeExpr] " << name->text << std::endl;
}

void BadStmt::print() const { out() << "[BadStmt]\n"; }
void BadDeclNode::print() const { out() << "[BadDecl]\n"; }
void BadExpr::print() const { out() << "[BadExpr]\n"; }

} // namespace cmp
