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
    std::cout << "traversing DeclStmt\n";
    decl->traverse(sema);
}

void ExprStmt::traverse(Semantics &sema) {
    std::cout << "traversing ExprStmt\n";
    expr->traverse(sema);
}

void AssignStmt::traverse(Semantics &sema) {
    lhs->traverse(sema);
    rhs->traverse(sema);
    // Type match.  For now, type of LHS and RHS should match exactly.
    if (lhs->inferred_type != rhs->inferred_type) {
        sema.error(start_pos, "type mismatch in assignment");
    }
}

void ReturnStmt::traverse(Semantics &sema) {
    expr->traverse(sema);
}

void CompoundStmt::traverse(Semantics &sema) {
    std::cout << "traversing CompoundStmt\n";
    for (auto &stmt : stmts) {
        stmt->traverse(sema);
    }
}

void VarDecl::traverse(Semantics &sema) {
    Type *inferred_type = nullptr;

    // Check for redefinition
    if (sema.decl_table.find(name) != nullptr) { // TODO: check scope
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

void Function::traverse(Semantics &sema) {
    body->traverse(sema);
}

void IntegerLiteral::traverse(Semantics &sema) {
    inferred_type = sema.int_type;
}

void RefExpr::traverse(Semantics &sema) {
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
            Type ref_type {subexpr->inferred_type, true};
            type = sema.type_table.insert({name, ref_type});
        }
    }
    inferred_type = type;
    assert(inferred_type);
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

void IntegerLiteral::print() const {
    out() << "[IntegerLiteral] " << value << std::endl;
}

void RefExpr::print() const {
    out() << "[RefExpr] " << "((Name *)";
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
