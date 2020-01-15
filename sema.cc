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
    auto loc = src.locate(pos);
    std::cout << "==== Declaration table ====\n";
    decl_table.print();
    std::cout << "==== Type table ====\n";
    type_table.print();
    std::cout << std::endl;
    fmt::print(stderr, "{}:{}:{}: error: {}\n", loc.filename, loc.line, loc.col,
               msg);
    exit(1);
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
    assert(lhs->type);
    assert(rhs->type);
    if (lhs->type != rhs->type) {
        sema.type_table.print();
        std::cout << "LHS: " << lhs->type->to_string() << std::endl;
        std::cout << "RHS: " << rhs->type->to_string() << std::endl;
        sema.error(rhs->start_pos, "type mismatch: ");
    }
}

void ReturnStmt::traverse(Sema &sema) {
    if (expr) {
        expr->traverse(sema);
        if (!sema.getContext().retType)
            sema.error(expr->start_pos, "function does not return a value");
        else if (sema.getContext().retType != expr->type)
            sema.error(expr->start_pos, "return type mismatch");
    } else {
        if (sema.getContext().retType)
            sema.error(start_pos, "a return a value should be specified for this function");
    }

    sema.getContext().seenReturn = true;
}

void CompoundStmt::traverse(Sema &sema) {
    for (auto &stmt : stmts)
        stmt->traverse(sema);
}

void VarDecl::traverse(Sema &sema) {
    Type *type = nullptr;

    // Check for redefinition
    auto found = sema.decl_table.find(name);
    if (found && found->scope_level == sema.decl_table.scope_level) { // TODO: check scope
        sema.error(start_pos, "redefinition");
    }

    // Infer type from the assignment expression.
    if (assignExpr) {
        assignExpr->traverse(sema);
        type = assignExpr->type;
    }
    // If none, try explicit type expression.
    // Assumes assignExpr and typeExpr do not coexist.
    else if (typeExpr) {
        typeExpr->traverse(sema);
        // FIXME: This is kinda hack; type depicts the type of the
        // _value_ of a Expr, but TypeExpr does not have any value.
        type = typeExpr->type;
    } else {
        assert(!"unreachable");
    }

    // Inferrence failure!
    if (!type)
        sema.error(start_pos, "cannot infer type of variable declaration");

    Declaration decl{name, *type};
    sema.decl_table.insert({name, decl});
}

void StructDecl::traverse(Sema &sema) {
    for (auto &m : members)
        m->traverse(sema);
}

void FuncDecl::traverse(Sema &sema) {
    sema.scope_open();

    if (retTypeExpr) {
        retTypeExpr->traverse(sema);
        assert(retTypeExpr->type);
        sema.getContext().retType = retTypeExpr->type;
    }

    for (auto &p : params)
        p->traverse(sema);

    body->traverse(sema);

    if (retTypeExpr && !sema.getContext().seenReturn) {
        sema.error(start_pos, "no return statement found for function");
    }

    sema.scope_close();
}

void UnaryExpr::traverse(Sema &sema) {
    // DeclRefs and Literals bypass this function altogether by virtual
    // dispatch, so no need to handle them in this switch.
    switch (unary_kind) {
    case Paren:
        operand->traverse(sema);
        type = operand->type;
        break;
    case Deref:
        operand->traverse(sema);
        if (!operand->type->ref) {
            sema.error(start_pos, "cannot dereference a non-reference");
        }
        type = operand->type->value_type;
        break;
    case Address:
        operand->traverse(sema);
        assert(operand->kind == AstKind::unary_expr);
        if (static_cast<UnaryExpr *>(operand)->unary_kind != DeclRef) {
            // TODO: LValue & RValue
            sema.error(start_pos, "cannot take address of a non-variable (TODO: rvalue)");
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
    if (decl == nullptr) {
        sema.error(start_pos, "undeclared identifier");
    }
    // Type inferrence
    type = &decl->type;
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
            sema.error(start_pos, "reference of undeclared type");
        }
        // If not, this is an instantiation of a derivative type, and should be
        // put into the table.
        else {
            assert(subexpr);
            Type ref_type{name, subexpr->type, true};
            type = sema.type_table.insert({name, ref_type});
        }
    }

    assert(type);
    this->type = type;
}

void BinaryExpr::traverse(Sema &sema) {
    lhs->traverse(sema);
    rhs->traverse(sema);

    if (lhs->type && rhs->type &&
        lhs->type != rhs->type)
        sema.error(start_pos, "type mismatch in binary expression");

    // propagate from left to right
    type = lhs->type;
}

Sema::Sema(Source &s, NameTable &n) : src(s), names(n) {
    Name *int_name = names.get_or_add("int");
    Type int_type{int_name};
    this->int_type = type_table.insert({int_name, int_type});
    Name *i64_name = names.get_or_add("i64");
    Type i64_type{i64_name};
    this->i64_type = type_table.insert({i64_name, i64_type});
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

void semantic_analyze(Sema &sema, Ast &ast) { ast.root->traverse(sema); }

} // namespace cmp