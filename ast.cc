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

//
// AST visitor functions
//

void AstVisitor::visit_file(const File *f) {
    fmt::print("visiting file\n");
    walk_file(*this, f);
}
void AstVisitor::visit_toplevel(const AstNode *a) {
    switch (a->kind) {
    case AstKind::stmt:
        visit_stmt(static_cast<const Stmt *>(a));
        break;
    case AstKind::decl:
        visit_decl(static_cast<const DeclNode *>(a));
        break;
    default:
        fmt::print("AstKind: {}\n", a->kind);
        assert(false && "not a toplevel node");
    }
}
void AstVisitor::visit_stmt(const Stmt *s) {
    fmt::print("visiting stmt\n");
}
void AstVisitor::visit_compound_stmt(const CompoundStmt *cs) {
    walk_compound_stmt(*this, cs);
}
void AstVisitor::visit_if_stmt(const IfStmt *is) {}
void AstVisitor::visit_decl(const DeclNode *d) {
    switch (d->decl_kind) {
    case DeclNodeKind::var:
        visit_var_decl(static_cast<const VarDeclNode *>(d));
        break;
    case DeclNodeKind::struct_:
        visit_struct_decl(static_cast<const StructDeclNode *>(d));
        break;
    case DeclNodeKind::func:
        visit_func_decl(static_cast<const FuncDeclNode *>(d));
        break;
    case DeclNodeKind::bad:
        break;
    default:
        assert(false);
    }
}
void AstVisitor::visit_var_decl(const VarDeclNode *vd) {
    fmt::print("visitng var_decl\n");
    // nothing to walk
}
void AstVisitor::visit_struct_decl(const StructDeclNode *s) {
    fmt::print("visiting struct decl\n");
    walk_struct_decl(*this, s);
}
void AstVisitor::visit_func_decl(const FuncDeclNode *f) {
    fmt::print("visiting func decl\n");
    walk_func_decl(*this, f);
}
void AstVisitor::visit_expr(const Expr *e) {
    // TODO: switch-case
    switch (e->kind) {
    default:
        break;
    }
}

//
// Walker functions.
//
// These functions exist to separate the logic of walking different AST nodes,
// from the actual work done on each node in the 'visit_...' functions.
//
// These functions combined can be seen as what the 'accept()' function do in
// the visitor pattern. However, whereas the accept() function is declared as
// virtual in the pattern proper, these are not polymorphic. TODO: Document
// why.
//

void walk_file(AstVisitor &v, const File *f) {
    for (auto a : f->toplevels) {
        v.visit_toplevel(a);
    }
}
void walk_var_decl(AstVisitor &v, const VarDeclNode *var) {
    if (var->assign_expr) {
        // walkAST(sema, var->assign_expr, pre_fn, post_fn);
    } else if (var->type_expr) {
        // walkAST(sema, var->type_expr, pre_fn, post_fn);
    } else {
        assert(false && "unreachable");
    }
}
void walk_struct_decl(AstVisitor &v, const StructDeclNode *s) {
    for (auto d : s->members) {
        v.visit_decl(d);
    }
}
void walk_func_decl(AstVisitor &v, const FuncDeclNode *f) {
    v.visit_compound_stmt(f->body);
}
void walk_compound_stmt(AstVisitor &v, const CompoundStmt *cs) {
    for (auto s : cs->stmts) {
        v.visit_stmt(s);
    }
}
void walk_if_stmt(AstVisitor &v, const IfStmt *is) {
    v.visit_expr(is->cond);
}

} // namespace cmp
