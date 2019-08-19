#include "sema.h"
#include "source.h"
#include "ast.h"
#include <iostream>
#include <cassert>

namespace cmp {

std::string Type::to_string() const {
    return name->text;
}

std::string Declaration::to_string() const {
    return name->text + ":" + type.to_string();
}

void Semantics::error(size_t pos, const std::string &msg) {
    auto loc = src.locate(pos);
    std::cerr << loc.filename << ":" << loc.line << ":" << loc.col << ": ";
    std::cerr << "error: " << msg << std::endl;
    std::cout << "==== Declaration table ====\n";
    decl_table.print();
    std::cout << "==== Type table ====\n";
    type_table.print();
    exit(1);
}

static Type make_type_from_text(Semantics &sema, const std::string &text) {
    Name *name = sema.names.getOrAdd(text);
    return Type{name};
}

// @Future: inefficient string operations?
Type *get_reference_type(Semantics &sema, Type *type) {
    Name *name = sema.names.getOrAdd("&" + type->name->text);
    Type ref_type {name, type, true};
    if (auto found = sema.type_table.find(name)) {
        return found;
    }
    return sema.type_table.insert({name, ref_type});
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
        sema.error(startPos, "type mismatch in assignment");
    }
}

void ReturnStmt::traverse(Semantics &sema) {
    expr->traverse(sema);
}

void CompoundStmt::traverse(Semantics &sema) {
    for (auto &stmt : stmts)
        stmt->traverse(sema);
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
        sema.error(startPos, "redefinition");
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
        sema.error(startPos, "cannot infer type of variable declaration");
    }

    Declaration decl{name, *inferred_type};
    sema.decl_table.insert({name, decl});
}

void FuncDecl::traverse(Semantics &sema) {
    sema.decl_table.open_scope();
    for (auto &param : paramDeclList)
        param ->traverse(sema);
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
            sema.error(startPos, "cannot dereference a non-reference");
        }
        inferred_type = operand->inferred_type->value_type;
        break;
    case Address:
        operand->traverse(sema);
        if (static_cast<UnaryExpr *>(operand.get())->unary_kind != DeclRef) {
            // TODO: LValue & RValue
            sema.error(startPos, "cannot take address of a non-variable (TODO: rvalue)");
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
        sema.error(startPos, "undeclared identifier");
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
            sema.error(startPos, "reference of undeclared type");
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
        sema.error(startPos, "type mismatch in binary expression");
    }
    // Propagate type of LHS
    inferred_type = lhs->inferred_type;
}

Semantics::Semantics(Source &src_, NameTable &nt) : src(src_), names(nt) {
    Name *int_name = names.getOrAdd("int");
    Type int_type{int_name};
    this->int_type = type_table.insert({int_name, int_type});
    Name *i64_name = names.getOrAdd("i64");
    Type i64_type{i64_name};
    this->i64_type = type_table.insert({i64_name, i64_type});
}

void semantic_analyze(Semantics &sema, Ast &ast) { ast.root->traverse(sema); }

} // namespace cmp
