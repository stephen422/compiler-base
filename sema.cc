#include "sema.h"
#include "ast.h"
#include "fmt/core.h"
#include "source.h"
#include <cassert>

namespace cmp {

std::string Type::toString() const { return name->text; }

std::string Declaration::toString() const {
    return name->text + "+" + type->toString();
}

void Sema::error(size_t pos, const std::string &msg) {
    errors.push_back({source.locate(pos), msg});
}

// Get or make a reference type of a given type.
// TODO: inefficient string operations?
Type *getReferenceType(Sema &sema, Type *type) {
    Name *name = sema.names.get_or_add("&" + type->name->text);
    // FIXME: scope_level
    Type ref_type{Type::Kind::ref, name, type};
    if (auto found = sema.type_table.find(name))
        return &found->value;
    return sema.type_table.insert({name, ref_type});
}

//
// AST Traversal
//

void walkAST(Sema &sema, AstNode *ast) {
    // TODO: pre_fn
    printf("pre_fn\n");

    // TODO
    if (!ast) {
        printf("error: ast must not be null!\n");
        return;
    }
    switch (ast->kind) {
    case AstKind::file:
        for (auto tl : static_cast<File *>(ast)->toplevels)
            walkAST(sema, tl);
        break;
    case AstKind::decl_stmt:
        walkAST(sema, static_cast<DeclStmt *>(ast)->decl);
        break;
    case AstKind::expr_stmt:
        walkAST(sema, static_cast<ExprStmt *>(ast)->expr);
        break;
    case AstKind::assign_stmt:
        walkAST(sema, static_cast<AssignStmt *>(ast)->lhs);
        walkAST(sema, static_cast<AssignStmt *>(ast)->rhs);
        break;
    case AstKind::return_stmt: {
        ReturnStmt *ret = static_cast<ReturnStmt *>(ast);
        if (ret)
            // ret->expr might be nullptr if no return statment was ever
            // processed.
            walkAST(sema, ret->expr);
        break;
    }
    case AstKind::compound_stmt:
        for (auto stmt : static_cast<CompoundStmt *>(ast)->stmts)
            walkAST(sema, stmt);
        break;
    case AstKind::var_decl: {
        auto var = static_cast<VarDecl *>(ast);
        if (var->assign_expr)
            walkAST(sema, var->assign_expr);
        else if (var->type_expr)
            walkAST(sema, var->type_expr);
        else
            assert(false && "unreachable");
        break;
    }
    case AstKind::struct_decl:
        for (auto m : static_cast<StructDecl *>(ast)->members)
            walkAST(sema, m);
        break;
    case AstKind::func_decl: {
        // TODO: ret_type insertion between ret_type_expr and body?
        auto func = static_cast<FuncDecl *>(ast);
        if (func->ret_type_expr)
            walkAST(sema, func->ret_type_expr);
        for (auto p : func->params)
            walkAST(sema, p);
        walkAST(sema, func->body);
        break;
    }
    case AstKind::unary_expr: {
        // TODO: proper UnaryKind subtypes
        auto unary = static_cast<UnaryExpr *>(ast);
        if (unary->unary_kind == UnaryExpr::FuncCall)
            for (auto arg : static_cast<FuncCallExpr *>(ast)->args)
                walkAST(sema, arg);
        walkAST(sema, static_cast<UnaryExpr *>(ast)->operand);
    }
    case AstKind::type_expr: {
        auto type_expr = static_cast<TypeExpr *>(ast);
        if (type_expr->subexpr)
            walkAST(sema, type_expr->subexpr);
    }
    case AstKind::binary_expr:
        walkAST(sema, static_cast<BinaryExpr *>(ast)->lhs);
        walkAST(sema, static_cast<BinaryExpr *>(ast)->rhs);
    default:
        // BadExprs, Literals, etc.
        break;
    }

    // TODO: post_fn
    printf("post_fn\n");
}

void File::walk(Sema &sema) {
    for (auto &tl : toplevels)
        tl->walk(sema);
}

void DeclStmt::walk(Sema &sema) { decl->walk(sema); }

void ExprStmt::walk(Sema &sema) { expr->walk(sema); }

void AssignStmt::walk(Sema &sema) {
    lhs->walk(sema);
    rhs->walk(sema);

    // Type check.  For now, type of LHS and RHS should match exactly.
    if (!lhs->type || !rhs->type) {
        // sema error in either hs
        return;
    }
    if (lhs->type != rhs->type) {
        sema.type_table.print();
        fmt::print("LHS: {}\n", lhs->type->toString());
        fmt::print("RHS: {}\n", rhs->type->toString());
        sema.error(rhs->pos, "type mismatch: ");
    }
}

void ReturnStmt::walk(Sema &sema) {
    if (expr) {
        expr->walk(sema);
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

void CompoundStmt::walk(Sema &sema) {
    for (auto &stmt : stmts)
        stmt->walk(sema);
}

void VarDecl::walk(Sema &sema) {
    Type *type = nullptr;

    // check for redefinition
    auto found = sema.decl_table.find(name);
    if (found && found->scope_level == sema.decl_table.scope_level)
        sema.error(pos, fmt::format("redefinition of '{}'", name->toString()));

    // type inferrence
    if (assign_expr) {
        assign_expr->walk(sema);
        type = assign_expr->type;
    } else if (type_expr) {
        type_expr->walk(sema);
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

    Declaration decl{name, type};
    sema.decl_table.insert({name, decl});
}

void StructDecl::walk(Sema &sema) {
    for (auto &m : members)
        m->walk(sema);
}

void FuncDecl::walk(Sema &sema) {
    sema.scope_open();

    if (ret_type_expr) {
        ret_type_expr->walk(sema);
        assert(ret_type_expr->type);
        sema.getContext().retType = ret_type_expr->type;
    }

    for (auto &p : params)
        p->walk(sema);

    body->walk(sema);

    if (ret_type_expr && !sema.getContext().seenReturn)
        sema.error(pos, "no return statement found for function");

    sema.scope_close();
}

void UnaryExpr::walk(Sema &sema) {
    // DeclRefs and Literals have their own walk(), so no need to handle
    // them in this switch.
    switch (unary_kind) {
    case Paren:
        operand->walk(sema);
        type = operand->type;
        break;
    case Deref:
        operand->walk(sema);
        if (operand->type->kind != Type::Kind::ref)
            sema.error(pos, "cannot dereference a non-reference");
        type = operand->type->target_type;
        break;
    case Address:
        operand->walk(sema);
        assert(operand->kind == AstKind::unary_expr);
        if (static_cast<UnaryExpr *>(operand)->unary_kind != DeclRef) {
            // TODO: LValue & RValue
            sema.error(pos,
                       "cannot take address of a non-variable (TODO: rvalue)");
        }
        type = getReferenceType(sema, operand->type);
        break;
    default:
        assert(!"unreachable");
    }
}

void IntegerLiteral::walk(Sema &sema) {
    type = sema.int_type;
}

void StringLiteral::walk(Sema &sema) {
    // TODO: add to the Type table
    type = sema.char_type;
}

void DeclRefExpr::walk(Sema &sema) {
    auto sym = sema.decl_table.find(name);
    if (sym) {
        // Type inferrence
        type = sym->value.type;
    } else {
        sema.error(pos, fmt::format("undeclared identifier '{}'", name->toString()));
    }
}

void FuncCallExpr::walk(Sema &sema) {
    assert(false && "not implemented");
}

void TypeExpr::walk(Sema &sema) {
    if (subexpr)
        subexpr->walk(sema);

    auto sym = sema.type_table.find(name);
    Type *type = nullptr;
    if (!sym) {
        // If this is a value type, we should check use before declaration.
        if (!ref) {
            sema.error(pos, fmt::format("unknown type '{}'", name->toString()));
        }
        // If not, this is an instantiation of a derivative type, and should be
        // put into the table.
        else {
            assert(subexpr);
            // FIXME: scope_level
            Type ref_type{Type::Kind::ref, name, subexpr->type};
            type = sema.type_table.insert({name, ref_type});
        }
    }

    this->type = type;
}

void BinaryExpr::walk(Sema &sema) {
    lhs->walk(sema);
    rhs->walk(sema);

    if (lhs->type && rhs->type && lhs->type != rhs->type)
        sema.error(pos, "type mismatch in binary expression");

    // propagate from left to right
    type = lhs->type;
}

Sema::Sema(const Source &s, NameTable &n) : source(s), names(n) {
    Name *int_name = names.get_or_add("int");
    Type int_type{int_name};
    this->int_type = type_table.insert({int_name, int_type});
    Name *char_name = names.get_or_add("char");
    Type char_type{char_name};
    this->char_type = type_table.insert({char_name, char_type});
}

// FIXME: lifetime of p.lexer.source() and p.names?
Sema::Sema(Parser &p) : Sema(p.lexer.source(), p.names) {
    errors = p.errors;
    beacons = p.beacons;
}

void Sema::scope_open() {
    decl_table.scopeOpen();
    type_table.scopeOpen();
    context_table.push_back(Context{});
}

void Sema::scope_close() {
    decl_table.scopeClose();
    type_table.scopeClose();
    context_table.pop_back();
}

void Sema::report() const {
    for (auto e : errors)
        fmt::print("{}\n", e.toString());
}

// See comments for cmp::verify().
bool Sema::verify() const {
    return cmp::verify(source.filename, errors, beacons);
}

void sema(Sema &sema, Ast &ast) { ast.root->walk(sema); }

} // namespace cmp
