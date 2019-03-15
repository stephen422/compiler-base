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

void File::traverse(Semantics &sema) {
    for (auto &tl : toplevels) {
        tl->traverse(sema);
    }
}

void DeclStmt::traverse(Semantics &sema) {
    decl->traverse(sema);
}

void ExprStmt::traverse(Semantics &sema) {
    expr->traverse(sema);
}

void AssignStmt::traverse(Semantics &sema) {
    lhs->traverse(sema);
    rhs->traverse(sema);
    // Type match.  For now, type of LHS and RHS should match exactly.
    assert(lhs->inferred_type);
    assert(rhs->inferred_type);
    if (lhs->inferred_type != rhs->inferred_type) {
        sema.type_table.print();
        std::cout << "LHS: " << lhs->inferred_type->to_string() << std::endl;
        std::cout << "RHS: " << rhs->inferred_type->to_string() << std::endl;
        sema.error(start_pos, "type mismatch in assignment");
    }
}

void ReturnStmt::traverse(Semantics &sema) {
    expr->traverse(sema);
}

void CompoundStmt::traverse(Semantics &sema) {
    for (auto &stmt : stmts) {
        stmt->traverse(sema);
    }
}

void ParamDecl::traverse(Semantics &sema) {
    type_expr->traverse(sema);
    Declaration decl{name, *type_expr->inferred_type};
    sema.decl_table.insert({name, decl});
}

void VarDecl::traverse(Semantics &sema) {
    Type *inferred_type = nullptr;

    // Check for redefinition
    auto found = sema.decl_table.find(name);
    if (found && found->scope_level == sema.decl_table.get_scope_level()) { // TODO: check scope
        sema.error(start_pos, "redefinition");
    }

    // Infer type from the assignment expression.
    if (assign_expr) {
        assign_expr->traverse(sema);
        inferred_type = assign_expr->inferred_type;
    }
    // If none, try explicit type expression.
    // Assumes assign_expr and type_expr do not coexist.
    else if (type_expr) {
        type_expr->traverse(sema);
        // FIXME: This is kinda hack; inferred_type depicts the type of the
        // _value_ of a Expr, but TypeExpr does not have any value.
        inferred_type = type_expr->inferred_type;
    } else {
        assert(!"unreachable");
    }

    // Inferrence failure!
    if (!inferred_type) {
        sema.error(start_pos, "cannot infer type of variable declaration");
    }

    Declaration decl{name, *inferred_type};
    sema.decl_table.insert({name, decl});
}

void FuncDecl::traverse(Semantics &sema) {
    sema.decl_table.open_scope();
    for (auto &param_decl : param_decl_list) {
        param_decl->traverse(sema);
    }
    body->traverse(sema);
    sema.decl_table.close_scope();
}

void UnaryExpr::traverse(Semantics &sema) {
    // DeclRefs and Literals bypass this function altogether by virtual
    // dispatch, so no need to handle them in this switch.
    switch (unary_kind) {
    case Paren:
        operand->traverse(sema);
        inferred_type = operand->inferred_type;
        break;
    case Deref:
        operand->traverse(sema);
        if (!operand->inferred_type->ref) {
            sema.error(start_pos, "cannot dereference a non-reference");
        }
        inferred_type = operand->inferred_type->value_type;
        break;
    case Address:
        operand->traverse(sema);
        if (node_cast<UnaryExpr>(operand)->unary_kind != DeclRef) {
            // TODO: LValue & RValue
            sema.error(start_pos, "cannot take address of a non-variable (TODO: rvalue)");
        }
        inferred_type = get_reference_type(sema, operand->inferred_type);
        break;
    default:
        assert(!"unreachable");
    }
}

void IntegerLiteral::traverse(Semantics &sema) {
    inferred_type = sema.get_int_type();
}

void DeclRefExpr::traverse(Semantics &sema) {
    Declaration *decl = sema.decl_table.find(name);
    if (decl == nullptr) {
        sema.error(start_pos, "undeclared identifier");
    }
    // Type inferrence
    inferred_type = &decl->type;
}

void TypeExpr::traverse(Semantics &sema) {
    if (subexpr) {
        subexpr->traverse(sema);
    }

    Type *type = sema.type_table.find(name);
    if (type == nullptr) {
        // If this is a value type (canonical type in Clang?), we should check use
        // before declaration.
        if (!ref) {
            sema.error(start_pos, "reference of undeclared type");
        }
        // If not, this is an instantiation of a derivative type, and should be
        // put into the table.
        else {
            assert(subexpr);
            Type ref_type {name, subexpr->inferred_type, true};
            type = sema.type_table.insert({name, ref_type});
        }
    }
    assert(type);
    inferred_type = type;
}

void BinaryExpr::traverse(Semantics &sema) {
    lhs->traverse(sema);
    rhs->traverse(sema);

    // Type inferrence
    if (lhs->inferred_type && rhs->inferred_type &&
        lhs->inferred_type != rhs->inferred_type) {
        sema.error(start_pos, "type mismatch in binary expression");
    }
    // Propagate type of LHS
    inferred_type = lhs->inferred_type;
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
    for (auto &param_decl : param_decl_list) {
        param_decl->print();
    }
    return_type_expr->print();
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
