#include "sema.h"
#include "ast.h"
#include "parser.h"
#include "fmt/core.h"
#include "source.h"
#include <cassert>

namespace cmp {

std::string Type::toString() const { return name->text; }

template <typename... Args> Decl *makeDecl(Sema &sema, Args &&... args) {
    Decl *decl = new Decl{std::forward<Args>(args)...};
    sema.decl_pool.push_back(decl);
    return decl;
}

std::string VarDecl::toString() const {
    return name->text;
}

std::string StructDecl::toString() const {
    return name->text;
}

void Sema::error(size_t pos, const std::string &msg) {
    Error e{source.locate(pos), msg};
    fmt::print("{}\n", e.toString());
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

void walkAST(Sema &sema, AstNode *node,
             std::function<void(Sema &sema, AstNode *)> pre_fn,
             std::function<void(Sema &sema, AstNode *)> post_fn) {
    assert(node);

    pre_fn(sema, node);

    switch (node->kind) {
    case AstKind::file:
        for (auto tl : static_cast<File *>(node)->toplevels)
            walkAST(sema, tl, pre_fn, post_fn);
        break;
    case AstKind::decl_stmt:
        walkAST(sema, static_cast<DeclStmt *>(node)->decl, pre_fn, post_fn);
        break;
    case AstKind::expr_stmt:
        walkAST(sema, static_cast<ExprStmt *>(node)->expr, pre_fn, post_fn);
        break;
    case AstKind::assign_stmt:
        walkAST(sema, static_cast<AssignStmt *>(node)->lhs, pre_fn, post_fn);
        walkAST(sema, static_cast<AssignStmt *>(node)->rhs, pre_fn, post_fn);
        break;
    case AstKind::return_stmt: {
        ReturnStmt *ret = static_cast<ReturnStmt *>(node);
        if (ret)
            // ret->expr might be nullptr if no return statment was ever
            // processed.
            walkAST(sema, ret->expr, pre_fn, post_fn);
        break;
    }
    case AstKind::compound_stmt:
        for (auto stmt : static_cast<CompoundStmt *>(node)->stmts)
            walkAST(sema, stmt, pre_fn, post_fn);
        break;
    case AstKind::var_decl: {
        auto var = static_cast<VarDeclNode *>(node);
        if (var->assign_expr)
            walkAST(sema, var->assign_expr, pre_fn, post_fn);
        else if (var->type_expr)
            walkAST(sema, var->type_expr, pre_fn, post_fn);
        else
            assert(false && "unreachable");
        break;
    }
    case AstKind::struct_decl:
        for (auto m : static_cast<StructDeclNode *>(node)->members)
            walkAST(sema, m, pre_fn, post_fn);
        break;
    case AstKind::func_decl: {
        // TODO: ret_type insertion between ret_type_expr and body?
        auto func = static_cast<FuncDeclNode *>(node);
        if (func->ret_type_expr)
            walkAST(sema, func->ret_type_expr, pre_fn, post_fn);
        for (auto p : func->params)
            walkAST(sema, p, pre_fn, post_fn);
        walkAST(sema, func->body, pre_fn, post_fn);
        break;
    }
    case AstKind::unary_expr: {
        // TODO: proper UnaryKind subtypes
        auto unary = static_cast<UnaryExpr *>(node);
        if (unary->unary_kind == UnaryExpr::FuncCall)
            for (auto arg : static_cast<FuncCallExpr *>(node)->args)
                walkAST(sema, arg, pre_fn, post_fn);
        if (static_cast<UnaryExpr *>(node)->operand)
            walkAST(sema, static_cast<UnaryExpr *>(node)->operand, pre_fn,
                    post_fn);
        break;
    }
    case AstKind::type_expr: {
        auto type_expr = static_cast<TypeExpr *>(node);
        if (type_expr->subexpr)
            walkAST(sema, type_expr->subexpr, pre_fn, post_fn);
        break;
    }
    case AstKind::binary_expr:
        walkAST(sema, static_cast<BinaryExpr *>(node)->lhs, pre_fn, post_fn);
        walkAST(sema, static_cast<BinaryExpr *>(node)->rhs, pre_fn, post_fn);
        break;
    default:
        // BadExprs, Literals, etc.
        break;
    }

    post_fn(sema, node);
}

//
// Name binding pass
//

void CompoundStmt::nameBindPre(Sema &sema) { sema.decl_table.scopeOpen(); }

void CompoundStmt::nameBindPost(Sema &sema) { sema.decl_table.scopeClose(); }

void VarDeclNode::nameBindPost(Sema &sema) {
    if (is_member) {
        sema.error(pos, "don't know how to do struct members yet");
        return;
    } else {
        // check for redefinition
        auto found = sema.decl_table.find(name);
        if (found && found->scope_level <= sema.decl_table.scope_level) {
            sema.error(pos,
                       fmt::format("redefinition of '{}'", name->toString()));
        } else {
            // new variable declaration
            // TODO: proper allocator
            Decl *decl = makeDecl(sema, VarDecl{name, nullptr});
            sema.decl_table.insert({name, decl});
        }
    }
}

void StructDeclNode::nameBindPost(Sema &sema) {
    // TODO: repetition with VarDecl::nameBindPost
    // check for redefinition
    auto found = sema.type_table.find(name);
    if (found && found->scope_level <= sema.type_table.scope_level) {
        sema.error(pos, fmt::format("redefinition of '{}'", name->toString()));
    } else {
        Type type{Type::Kind::value, name, nullptr};
        sema.type_table.insert({name, type});
    }
}

void DeclRefExpr::nameBindPost(Sema &sema) {
    auto sym = sema.decl_table.find(name);
    if (sym) {
        decl = sym->value;
    } else {
        sema.error(pos, fmt::format("use of undeclared identifier '{}'", name->toString()));
    }
}

void TypeExpr::nameBindPost(Sema &sema) {
    auto sym = sema.type_table.find(name);
    if (sym) {
        type = &sym->value;
    } else {
        sema.error(pos, fmt::format("use of undeclared type '{}'", name->toString()));
    }
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
        // if (!sema.getContext().ret_type)
        //     sema.error(expr->pos, "function does not return a value");
        // else if (sema.getContext().ret_type != expr->type)
        //     sema.error(expr->pos, "return type mismatch");
    } else {
        // if (sema.getContext().ret_type)
        //     sema.error(pos, "a return a value should be specified for this function");
    }

    // sema.getContext().seen_return = true;
}

void CompoundStmt::walk(Sema &sema) {
    for (auto &stmt : stmts)
        stmt->walk(sema);
}

void VarDeclNode::walk(Sema &sema) {
    Type *type = nullptr;

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
}

void StructDeclNode::walk(Sema &sema) {
    for (auto &m : members)
        m->walk(sema);
}

void FuncDeclNode::walk(Sema &sema) {
    sema.scope_open();

    if (ret_type_expr) {
        ret_type_expr->walk(sema);
        assert(ret_type_expr->type);
        // sema.getContext().ret_type = ret_type_expr->type;
    }

    for (auto &p : params)
        p->walk(sema);

    body->walk(sema);

    // if (ret_type_expr && !sema.getContext().seen_return)
    //     sema.error(pos, "no return statement found for function");

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
        // FIXME
        type = declCast<VarDecl>(*sym->value).type;
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

Sema::~Sema() {
    for (auto d : decl_pool) {
        delete d;
    }
}

// FIXME: lifetime of p.lexer.source() and p.names?
Sema::Sema(Parser &p) : Sema(p.lexer.source(), p.names) {
    errors = p.errors;
    beacons = p.beacons;
}

void Sema::scope_open() {
    decl_table.scopeOpen();
    type_table.scopeOpen();
}

void Sema::scope_close() {
    decl_table.scopeClose();
    type_table.scopeClose();
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
