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
// AST visitor functions.
//
// These are to be overridden by the specialized visitor classes. They provide
// a default behavior of simply traversing the node, acting nothing upon them.
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
    switch (s->stmt_kind) {
    case StmtKind::decl:
        visit_decl_stmt(static_cast<const DeclStmt *>(s));
        break;
    case StmtKind::expr:
        visit_expr_stmt(static_cast<const ExprStmt *>(s));
        break;
    case StmtKind::assign:
        visit_assign_stmt(static_cast<const AssignStmt *>(s));
        break;
    case StmtKind::return_:
        visit_return_stmt(static_cast<const ReturnStmt *>(s));
        break;
    case StmtKind::compound:
        visit_compound_stmt(static_cast<const CompoundStmt *>(s));
        break;
    case StmtKind::if_:
        visit_if_stmt(static_cast<const IfStmt *>(s));
        break;
    case StmtKind::bad:
        visit_bad_stmt(static_cast<const BadStmt *>(s));
        break;
    default:
        assert(false);
    }
}
void AstVisitor::visit_decl_stmt(const DeclStmt *ds) {
    fmt::print("visiting decl_stmt\n");
    walk_decl_stmt(*this, ds);
}
void AstVisitor::visit_expr_stmt(const ExprStmt *es) {
    fmt::print("visiting expr_stmt\n");
    walk_expr_stmt(*this, es);
}
void AstVisitor::visit_assign_stmt(const AssignStmt *as) {
    fmt::print("visiting assign_stmt\n");
    walk_assign_stmt(*this, as);
}
void AstVisitor::visit_return_stmt(const ReturnStmt *rs) {
    fmt::print("visiting return_stmt\n");
    walk_return_stmt(*this, rs);
}
void AstVisitor::visit_compound_stmt(const CompoundStmt *cs) {
    fmt::print("visiting compound_stmt\n");
    walk_compound_stmt(*this, cs);
}
void AstVisitor::visit_if_stmt(const IfStmt *is) {
    fmt::print("visiting if_stmt\n");
    walk_if_stmt(*this, is);
}
void AstVisitor::visit_bad_stmt(const BadStmt *bs) {
    fmt::print("visiting bad_stmt\n");
    // do nothing
}
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
void AstVisitor::visit_var_decl(const VarDeclNode *v) {
    fmt::print("visiting var_decl\n");
    walk_var_decl(*this, v);
}
void AstVisitor::visit_struct_decl(const StructDeclNode *s) {
    fmt::print("visiting struct decl\n");
    walk_struct_decl(*this, s);
}
void AstVisitor::visit_func_decl(const FuncDeclNode *f) {
    fmt::print("visiting func decl\n");
    walk_func_decl(*this, f);
}
void AstVisitor::visit_bad_decl(const BadDeclNode *b) {
    fmt::print("visiting bad decl\n");
    // do nothing
}
void AstVisitor::visit_expr(const Expr *e) {
    // TODO: switch-case
    switch (e->kind) {
    default:
        break;
    }
}
void AstVisitor::visit_type_expr(const TypeExpr *t) {
    fmt::print("visiting type expr\n");
    // TODO
}

//
// Walker functions.
//
// These functions exist to separate the traversal logic from the actual work
// done on each node in the 'visit_...' functions.  This way, the visitor
// functions only have to worry about whether to do work before or after
// walking the subnodes, by simply putting the walker function in the right
// place.
//
// Note: The functions here combined can be seen as what the 'accept()'
// function do in the visitor pattern. However, whereas the accept() function
// is declared as virtual in the pattern proper, these are not polymorphic.
// TODO: Document why.
//
// TODO: templatize.
//

void walk_file(AstVisitor &v, const File *f) {
    for (auto a : f->toplevels) {
        v.visit_toplevel(a);
    }
}
void walk_var_decl(AstVisitor &v, const VarDeclNode *var) {
    if (var->assign_expr) {
        v.visit_expr(var->assign_expr);
    } else if (var->type_expr) {
        // XXX
        v.visit_type_expr(static_cast<const TypeExpr *>(var->type_expr));
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
    if (f->ret_type_expr) {
        v.visit_expr(f->ret_type_expr);
    }
    for (auto arg : f->args) {
        v.visit_decl(arg);
    }
    v.visit_compound_stmt(f->body);
}
void walk_decl_stmt(AstVisitor &v, const DeclStmt *ds) {
    v.visit_decl(ds->decl);
}
void walk_expr_stmt(AstVisitor &v, const ExprStmt *es) {
    v.visit_expr(es->expr);
}
void walk_assign_stmt(AstVisitor &v, const AssignStmt *as) {
    v.visit_expr(as->rhs);
    v.visit_expr(as->lhs);
}
void walk_return_stmt(AstVisitor &v, const ReturnStmt *rs) {
    v.visit_expr(rs->expr);
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
