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
    // For VarDecl, assign_expr has to be traversed first because of the type
    // inference.
    if (assign_expr) {
        assign_expr->traverse(sema);
        // Propagate type from assign_expr
        inferred_type = assign_expr->inferred_type;
    }

    auto old_decl = sema.decl_table.find(name);
    if (old_decl != nullptr) { // TODO: check scope
        sema.error(start_pos, "redefinition");
    }

    // TODO: Proper type inference here.
    if (!inferred_type) {
        if (!type_name) {
            // Neither is the type explicitly specified, this is an inferrence
            // failure
            sema.error(start_pos, "cannot infer type of variable declaration");
        }
        inferred_type = sema.type_table.find(type_name);
        if (!inferred_type) {
            sema.error(start_pos, "undeclared type");
        }
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
    out() << "[VarDecl] " << name->text;
    std::cout << " '" << (type_name ? type_name->text : "(null)") << "'";
    if (mut) {
        std::cout << " (mut)";
    }
    std::cout << std::endl;

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

} // namespace cmp
