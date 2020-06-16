#include "ast.h"
#include "sema.h"
#include <iostream>
#include <sstream>
#include <cassert>

namespace cmp {

int AstNode::indent = 0;

std::ostream &AstNode::out() const {
  if (indent > 0) {
    std::cout << std::string(indent - 2, ' ');
    std::cout << "`-";
  }
  return std::cout;
}

// Gets the union of the source ranges of nodes.
std::pair<size_t, size_t>
get_ast_range(std::initializer_list<AstNode *> nodes) {
  size_t min = static_cast<size_t>(-1);
  size_t max = 0; // FIXME not used
  for (auto node : nodes) {
    if (!node) continue;
    if (node->pos < min) {
      min = node->pos;
    }
  }
  return {min, max};
}

std::optional<Decl *> Expr::declMaybe() const {
  switch (kind) {
  case ExprKind::decl_ref:
    return {as<DeclRefExpr>()->decl};
  case ExprKind::member:
    return as<MemberExpr>()->decl;
  case ExprKind::unary:
    if (as<UnaryExpr>()->kind == UnaryExprKind::paren)
      return as<UnaryExpr>()->as<ParenExpr>()->decl();
    else if (as<UnaryExpr>()->kind == UnaryExprKind::deref)
      return as<UnaryExpr>()->var_decl;
    break;
  default:
    break;
  }
  return {};
}

// Return optional of 'type' member of Decl, or None if this Decl kind doesn't
// have any.
std::optional<Type *> Decl::type() const {
  if (kind == DeclKind::var) {
    return as<VarDecl>()->type;
  } else if (kind == DeclKind::struct_) {
    return as<StructDecl>()->type;
  } else if (kind == DeclKind::enum_) {
    return as<EnumDecl>()->type;
  } else if (kind == DeclKind::enum_variant) {
    return as<EnumVariantDecl>()->type;
  } else if (kind == DeclKind::func) {
    return {};
  }
  assert(false && "not all decl kinds handled");
}

bool FuncDecl::isVoid(Sema &sema) const {
  return ret_type == sema.context.void_type;
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
    if_body->print();
    if (else_if) {
        else_if->print();
    } else if (else_body) {
        else_body->print();
    }
}

void CompoundStmt::print() const {
    out() << "[CompoudStmt]\n";
    PrintScope start;
    for (auto const& s : stmts) {
        s->print();
    }
}

void BuiltinStmt::print() const {
  out() << "[BuiltinStmt]\n";
}

void VarDecl::print() const {
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

void FuncDecl::print() const {
    out() << "[FuncDecl] " << name->text << "\n";
    PrintScope start;
    for (auto &p : args)
        p->print();
    if (ret_type_expr)
        ret_type_expr->print();
    body->print();
}

void StructDecl::print() const {
    out() << "[StructDecl] " << name->text << "\n";
    PrintScope start;
    for (auto &m : members)
        m->print();
}

void EnumVariantDecl::print() const {
    out() << "[EnumVariant] " << name->text << "\n";
}

void EnumDecl::print() const {
    out() << "[EnumDecl] " << name->text << "\n";
    PrintScope start;
    for (auto &m : variants)
      m->print();
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

    switch (kind) {
    case UnaryExprKind::deref: {
        std::cout << "Deref\n";
        PrintScope start;
        operand->print();
        break;
    }
    case UnaryExprKind::ref: {
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

void StructDefExpr::print() const {
    out() << "[StructDefExpr]" << std::endl;
    PrintScope start;
    name_expr->print();

    for (auto desig : desigs) {
        out() << fmt::format(".{}", desig.name->str()) << std::endl;
        PrintScope start;
        desig.expr->print();
    }
}

void MemberExpr::print() const {
    out() << "[MemberExpr]" << std::endl;
    PrintScope start;
    lhs_expr->print();
    out() << "'.' " << member_name->text << std::endl;
}

void TypeExpr::print() const {
    out() << "[TypeExpr] " << name->text << std::endl;
}

void BadStmt::print() const { out() << "[BadStmt]\n"; }
void BadDecl::print() const { out() << "[BadDecl]\n"; }
void BadExpr::print() const { out() << "[BadExpr]\n"; }

} // namespace cmp
