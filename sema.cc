#include "sema.h"
#include "ast.h"
#include "fmt/core.h"
#include "source.h"
#include <cassert>

namespace cmp {

std::string Type::to_string() const {
    return name->text;
}

std::string Declaration::to_string() const {
    return name->text + ":" + type.to_string();
}

void Sema::error(size_t pos, const std::string &msg) {
    errors.push_back({source.locate(pos), msg});
}

// @Future: inefficient string operations?
Type *get_reference_type(Sema &sema, Type *type) {
    Name *name = sema.names.get_or_add("&" + type->name->text);
    Type ref_type {name, type, true};
    if (auto found = sema.type_table.find(name)) {
        return found;
    }
    return sema.type_table.insert({name, ref_type});
}

//
// AST Traversal
//

void File::traverse(Sema &sema) {
    for (auto &tl : toplevels) {
        tl->traverse(sema);
    }
}

void DeclStmt::traverse(Sema &sema) { decl->traverse(sema); }

void ExprStmt::traverse(Sema &sema) { expr->traverse(sema); }

void AssignStmt::traverse(Sema &sema) {
    lhs->traverse(sema);
    rhs->traverse(sema);

    // Type check.  For now, type of LHS and RHS should match exactly.
    if (!lhs->type || !rhs->type) {
        // sema error in either hs
        return;
    }
    if (lhs->type != rhs->type) {
        sema.type_table.print();
        std::cout << "LHS: " << lhs->type->to_string() << std::endl;
        std::cout << "RHS: " << rhs->type->to_string() << std::endl;
        sema.error(rhs->pos, "type mismatch: ");
    }
}

void ReturnStmt::traverse(Sema &sema) {
    if (expr) {
        expr->traverse(sema);
        if (!sema.getContext().retType)
            sema.error(expr->pos, "function does not return a value");
        else if (sema.getContext().retType != expr->type)
            sema.error(expr->pos, "return type mismatch");
    } else {
        if (sema.getContext().retType)
            sema.error(pos, "a return a value should be specified for this function");
    }

    sema.getContext().seenReturn = true;
}

void CompoundStmt::traverse(Sema &sema) {
    for (auto &stmt : stmts)
        stmt->traverse(sema);
}

void VarDecl::traverse(Sema &sema) {
    Type *type = nullptr;

    // check for redefinition
    auto found = sema.decl_table.find(name);
    if (found && found->scope_level == sema.decl_table.scope_level)
        sema.error(pos, fmt::format("redefinition of '{}'", *name));

    // type inferrence
    if (assign_expr) {
        assign_expr->traverse(sema);
        type = assign_expr->type;
    } else if (type_expr) {
        type_expr->traverse(sema);
        // FIXME: This is kinda hack; type depicts the type of the _value_ of a
        // Expr, but TypeExpr does not have any value.
        type = type_expr->type;
    } else {
        assert(!"unreachable");
    }

    // 'type' may be null here, e.g. for undeclared type errors.  For those
    // cases, we can't push a new declaration anyway, so just bail.
    if (!type)
        return;

    Declaration decl{name, *type, sema.decl_table.scope_level};
    sema.decl_table.insert({name, decl});
}

void StructDecl::traverse(Sema &sema) {
    for (auto &m : members)
        m->traverse(sema);
}

void FuncDecl::traverse(Sema &sema) {
    sema.scope_open();

    if (ret_type_expr) {
        ret_type_expr->traverse(sema);
        assert(ret_type_expr->type);
        sema.getContext().retType = ret_type_expr->type;
    }

    for (auto &p : params)
        p->traverse(sema);

    body->traverse(sema);

    if (ret_type_expr && !sema.getContext().seenReturn) {
        sema.error(pos, "no return statement found for function");
    }

    sema.scope_close();
}

void UnaryExpr::traverse(Sema &sema) {
    // DeclRefs and Literals have their own traverse(), so no need to handle
    // them in this switch.
    switch (unary_kind) {
    case Paren:
        operand->traverse(sema);
        type = operand->type;
        break;
    case Deref:
        operand->traverse(sema);
        if (!operand->type->ref) {
            sema.error(pos, "cannot dereference a non-reference");
        }
        type = operand->type->value_type;
        break;
    case Address:
        operand->traverse(sema);
        assert(operand->kind == AstKind::unary_expr);
        if (static_cast<UnaryExpr *>(operand)->unary_kind != DeclRef) {
            // TODO: LValue & RValue
            sema.error(pos,
                       "cannot take address of a non-variable (TODO: rvalue)");
        }
        type = get_reference_type(sema, operand->type);
        break;
    default:
        assert(!"unreachable");
    }
}

void IntegerLiteral::traverse(Sema &sema) {
    type = sema.int_type;
}

void DeclRefExpr::traverse(Sema &sema) {
    Declaration *decl = sema.decl_table.find(name);
    if (decl) {
        // Type inferrence
        type = &decl->type;
    } else {
        sema.error(pos, fmt::format("undeclared identifier '{}'", *name));
    }
}

void FuncCallExpr::traverse(Sema &sema) {
    assert(false && "not implemented");
}

void TypeExpr::traverse(Sema &sema) {
    if (subexpr)
        subexpr->traverse(sema);

    Type *type = sema.type_table.find(name);
    if (type == nullptr) {
        // If this is a value type, we should check use before declaration.
        if (!ref) {
            sema.error(pos, fmt::format("unknown type '{}'", *name));
        }
        // If not, this is an instantiation of a derivative type, and should be
        // put into the table.
        else {
            assert(subexpr);
            Type ref_type{name, subexpr->type, true};
            type = sema.type_table.insert({name, ref_type});
        }
    }

    this->type = type;
}

void BinaryExpr::traverse(Sema &sema) {
    lhs->traverse(sema);
    rhs->traverse(sema);

    if (lhs->type && rhs->type && lhs->type != rhs->type)
        sema.error(pos, "type mismatch in binary expression");

    // propagate from left to right
    type = lhs->type;
}

Sema::Sema(const Source &s, NameTable &n) : source(s), names(n) {
    Name *int_name = names.get_or_add("int");
    Type int_type{int_name};
    this->int_type = type_table.insert({int_name, int_type});
    Name *i64_name = names.get_or_add("i64");
    Type i64_type{i64_name};
    this->i64_type = type_table.insert({i64_name, i64_type});
}

// FIXME: lifetime of p.lexer.source() and p.names?
Sema::Sema(Parser &p) : Sema(p.lexer.source(), p.names) {
    errors = p.errors;
    beacons = p.beacons;
}

void Sema::scope_open() {
    decl_table.scope_open();
    type_table.scope_open();
    context_table.push_back(Context{});
}

void Sema::scope_close() {
    decl_table.scope_close();
    type_table.scope_close();
    context_table.pop_back();
}

void Sema::report() const {
    for (auto e : errors)
        fmt::print("{}\n", e);
}

// See cmp::verify().
bool Sema::verify() const {
    return cmp::verify(source.filename, errors, beacons);
}

void sema(Sema &sema, Ast &ast) { ast.root->traverse(sema); }

} // namespace cmp
