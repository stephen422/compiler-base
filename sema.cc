#include "sema.h"
#include "ast.h"
#include "parser.h"
#include "fmt/core.h"
#include "source.h"
#include <cassert>

namespace cmp {

std::string Type::str() const { return name->text; }

// TODO: Decl::str()
// std::string VarDecl::str() const {
//     return name->text;
// }
// 
// std::string StructDecl::str() const {
//     return name->text;
// }

void Sema::error(size_t pos, const std::string &msg) {
    Error e(source.locate(pos), msg);
    fmt::print("{}\n", e.str());
}

// Get or make a reference type of a given type.
// TODO: inefficient string operations?
Type *getReferenceType(Sema &sema, Type *type) {
    Name *name = sema.names.get_or_add("&" + type->name->text);
    // FIXME: scope_level
    Type ref_type{TypeKind::ref, name, type};
    if (auto found = sema.type_table.find(name))
        return &found->value;
    return sema.type_table.insert(name, ref_type);
}

//
// AST Traversal
//

void walkAST(Sema &sema, AstNode *node, bool (*pre_fn)(Sema &sema, AstNode *),
              bool (*post_fn)(Sema &sema, AstNode *)) {
    assert(node);

    if (!pre_fn(sema, node)) {
        return;
    }

    switch (node->kind) {
    case AstKind::file:
        for (auto tl : static_cast<File *>(node)->toplevels)
            walkAST(sema, tl, pre_fn, post_fn);
        break;
    // case AstKind::decl_stmt:
    //     walkAST(sema, static_cast<DeclStmt *>(node)->decl, pre_fn, post_fn);
    //     break;
    // case AstKind::expr_stmt:
    //     walkAST(sema, static_cast<ExprStmt *>(node)->expr, pre_fn, post_fn);
    //     break;
    // case AstKind::assign_stmt:
    //     walkAST(sema, static_cast<AssignStmt *>(node)->lhs, pre_fn, post_fn);
    //     walkAST(sema, static_cast<AssignStmt *>(node)->rhs, pre_fn, post_fn);
    //     break;
    // case AstKind::return_stmt: {
    //     ReturnStmt *ret = static_cast<ReturnStmt *>(node);
    //     if (ret) {
    //         // ret->expr might be nullptr if no return statment was ever
    //         // processed.
    //         walkAST(sema, ret->expr, pre_fn, post_fn);
    //     }
    //     break;
    // }
    // case AstKind::compound_stmt:
    //     for (auto stmt : static_cast<CompoundStmt *>(node)->stmts)
    //         walkAST(sema, stmt, pre_fn, post_fn);
    //     break;
    // case AstKind::if_stmt: {
    //     auto *ifstmt = static_cast<IfStmt *>(node);

    //     walkAST(sema, ifstmt->cond, pre_fn, post_fn);
    //     walkAST(sema, ifstmt->cstmt_true, pre_fn, post_fn);

    //     if (ifstmt->elseif) {
    //         walkAST(sema, ifstmt->elseif, pre_fn, post_fn);
    //     } else if (ifstmt->cstmt_false) {
    //         walkAST(sema, ifstmt->cstmt_false, pre_fn, post_fn);
    //     }
    //     break;
    // }
    // case AstKind::var_decl: {
    //     auto var = static_cast<VarDeclNode *>(node);
    //     if (var->assign_expr) {
    //         walkAST(sema, var->assign_expr, pre_fn, post_fn);
    //     } else if (var->type_expr) {
    //         walkAST(sema, var->type_expr, pre_fn, post_fn);
    //     } else {
    //         assert(false && "unreachable");
    //     }
    //     break;
    // }
    // case AstKind::struct_decl:
    //     for (auto m : static_cast<StructDeclNode *>(node)->members) {
    //         walkAST(sema, m, pre_fn, post_fn);
    //     }
    //     break;
    // case AstKind::func_decl: {
    //     // TODO: ret_type insertion between ret_type_expr and body?
    //     auto func = static_cast<FuncDeclNode *>(node);
    //     if (func->ret_type_expr) {
    //         walkAST(sema, func->ret_type_expr, pre_fn, post_fn);
    //     }
    //     for (auto p : func->args) {
    //         walkAST(sema, p, pre_fn, post_fn);
    //     }
    //     walkAST(sema, func->body, pre_fn, post_fn);
    //     break;
    // }
    // case AstKind::unary_expr: {
    //     if (static_cast<UnaryExpr *>(node)->operand) {
    //         walkAST(sema, static_cast<UnaryExpr *>(node)->operand, pre_fn,
    //                  post_fn);
    //     }
    //     break;
    // }
    // case AstKind::func_call_expr:
    //     for (auto arg : static_cast<FuncCallExpr *>(node)->args) {
    //         walkAST(sema, arg, pre_fn, post_fn);
    //     }
    //     break;
    // case AstKind::type_expr: {
    //     auto type_expr = static_cast<TypeExpr *>(node);
    //     if (type_expr->subexpr) {
    //         walkAST(sema, type_expr->subexpr, pre_fn, post_fn);
    //     }
    //     break;
    // }
    // case AstKind::binary_expr:
    //     walkAST(sema, static_cast<BinaryExpr *>(node)->lhs, pre_fn, post_fn);
    //     walkAST(sema, static_cast<BinaryExpr *>(node)->rhs, pre_fn, post_fn);
    //     break;
    default:
        // BadExprs, Literals, etc.
        break;
    }

    if (!post_fn(sema, node)) {
        return; // XXX: pointless
    }
}

//
// Name binding pass
//

// It's hard to namebind MemberExprs at this stage, because we don't know if
// the operand of the dot(.) is actually member-accessible unless we do full
// type checking (e.g. func().mem), except the most trival cases (e.g.
// struct.mem).  So it may be better to defer this to the type checking stage.
// TODO check this in the future.
#if 0
static bool is_member_accessible(Expr *expr) {
    if (expr->kind == AstKind::paren_expr) {
        return is_member_accessible(static_cast<ParenExpr *>(expr)->operand);
    }
    // Note that MemberExpr may or may not be member-accessible, unless it
    // typechecks to be an actual aggregate type.  But we can't filter out
    // those cases in a stage this early.
    return expr->kind == AstKind::decl_ref_expr ||
           expr->kind == AstKind::member_expr;
}

void MemberExpr::name_bind_post(Sema &sema) {
    // Here we check if the operand expression was member-accessible, e.g.
    // error on '(a + b).m'.
    if (!is_member_accessible(struct_expr)) {
        sema.error(pos, fmt::format("cannot member access type 'TODO({})'",
                                    struct_expr->kind));
        return;
    }

    // Now we know that 'struct_expr' is something that contains a Decl*.
    // But how do we get it out? TODO.
}
#endif

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
        fmt::print("LHS: {}\n", lhs->type->str());
        fmt::print("RHS: {}\n", rhs->type->str());
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

    for (auto &p : args)
        p->walk(sema);

    body->walk(sema);

    // if (ret_type_expr && !sema.getContext().seen_return)
    //     sema.error(pos, "no return statement found for function");

    sema.scope_close();
}

void UnaryExpr::walk(Sema &sema) {
    // DeclRefs and Literals have their own walk(), so no need to handle
    // them in this switch.
    switch (kind) {
    // case AstKind::paren_expr:
    //     operand->walk(sema);
    //     type = operand->type;
    //     break;
    // case AstKind::deref_expr:
    //     operand->walk(sema);
    //     if (operand->type->kind != TypeKind::ref)
    //         sema.error(pos, "cannot dereference a non-reference");
    //     type = operand->type->target_type;
    //     break;
    // case AstKind::address_expr:
    //     operand->walk(sema);
    //     assert(operand->kind == AstKind::unary_expr);
    //     if (operand->kind != AstKind::deref_expr) {
    //         // TODO: LValue & RValue
    //         sema.error(pos,
    //                    "cannot take address of a non-variable (TODO: rvalue)");
    //     }
    //     type = getReferenceType(sema, operand->type);
    //     break;
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
        // type = decl_cast<VarDecl>(sym->value)->type;
        type = sym->value->var_decl.type;
    }
}

void FuncCallExpr::walk(Sema &sema) {
    (void)sema;
    assert(!"not implemented");
}

void TypeExpr::walk(Sema &sema) {
    if (subexpr)
        subexpr->walk(sema);

    auto sym = sema.type_table.find(name);
    Type *type = nullptr;
    if (!sym) {
        // If this is a value type, we should check use before declaration.
        if (kind == TypeExprKind::value) {
            sema.error(pos, fmt::format("unknown type '{}'", name->str()));
        }
        // If not, this is an instantiation of a derivative type, and should be
        // put into the table.
        else {
            assert(subexpr);
            // FIXME: scope_level
            Type ref_type{TypeKind::ref, name, subexpr->type};
            type = sema.type_table.insert(name, ref_type);
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

void push_builtin_type_from_name(Sema &s, const std::string &str) {
    Name *name = s.names.get_or_add(str);
    auto decl = s.make_decl(StructDecl{name});
    auto type = s.make_type(name);
    decl->struct_decl.type = type;
    s.decl_table.insert(name, decl);
}

// Push Decls for the builtin types into the global scope of decl_table, so
// that they are visible from any point in the AST.
void setup_builtin_types(Sema &s) {
    push_builtin_type_from_name(s, "int");
    push_builtin_type_from_name(s, "char");
}

Sema::~Sema() {
    for (auto d : decl_pool) {
        delete d;
    }
    for (auto t : type_pool) {
        delete t;
    }
}

// FIXME: lifetime of p.lexer.source() and p.names?
Sema::Sema(Parser &p) : Sema(p.lexer.source(), p.names) {
    errors = p.errors;
    beacons = p.beacons;
}

void Sema::scope_open() {
    decl_table.scope_open();
    type_table.scope_open();
}

void Sema::scope_close() {
    decl_table.scope_close();
    type_table.scope_close();
}

void Sema::report() const {
    for (auto e : errors) {
        fmt::print("{}\n", e.str());
    }
}

// See comments for cmp::verify().
bool Sema::verify() const {
    return cmp::verify(source.filename, errors, beacons);
}

void sema(Sema &sema, Ast &ast) { ast.root->walk(sema); }

void NameBinder::visit_compound_stmt(CompoundStmt *cs) {
    sema.decl_table.scope_open();
    walk_compound_stmt(*this, cs);
    sema.decl_table.scope_close();
}

void NameBinder::visit_decl_ref_expr(DeclRefExpr *d) {
    // XXX: should walk_decl_ref_expr() exist?  It's a chore to look up which
    // one does and which doesn't.

    // TODO: only accept Decls with var type.
    // TODO: functions as variables?
    auto sym = sema.decl_table.find(d->name);
    if (sym && sym->value->kind == DECL_VAR) {
        d->decl = sym->value;
    } else {
        sema.error(d->pos, fmt::format("use of undeclared identifier '{}'",
                                       d->name->str()));
    }
}

void NameBinder::visit_func_call_expr(FuncCallExpr *f) {
    auto sym = sema.decl_table.find(f->func_name);
    if (sym) {
        if (sym->value->kind == DECL_FUNC) {
            f->func_decl = &sym->value->func_decl;
        } else {
            sema.error(f->pos,
                       fmt::format("'{}' is not a function", f->func_name->str()));
            return;
        }
    } else {
        sema.error(f->pos,
                   fmt::format("undeclared function '{}'", f->func_name->str()));
        return;
    }

    walk_func_call_expr(*this, f);

    assert(f->func_decl);

    // check if argument count matches
    if (f->func_decl->args_count() != f->args.size()) {
        sema.error(f->pos, fmt::format("'{}' accepts {} arguments, got {}",
                                    f->func_name->str(), f->func_decl->args_count(),
                                    f->args.size()));
    }
}

void NameBinder::visit_type_expr(TypeExpr *t) {
    walk_type_expr(*this, t);

    // Name binding for TypeExprs only include linking existing Decls to the
    // type names used in the expression, not declaring new ones.  The
    // declaration would be done when visiting VarDecls and StructDecls, etc.

    // XXX: only do this for value types?
    auto sym = sema.decl_table.find(t->name);
    if (sym && sym->value->kind == DECL_TYPE) {
        assert(t->kind == TypeExprKind::value);
        t->decl = sym->value;
    } else {
        sema.error(t->pos,
                   fmt::format("use of undeclared type '{}'", t->name->str()));
        return;
    }
}

void NameBinder::visit_var_decl(VarDeclNode *v) {
    walk_var_decl(*this, v);

    auto found = sema.decl_table.find(v->name);
    if (found && found->value->kind == DECL_VAR &&
        found->scope_level <= sema.decl_table.scope_level) {
        sema.error(v->pos, fmt::format("redefinition of '{}'", v->name->str()));
        return;
    }

    auto decl = sema.make_decl(VarDecl{v->name});
    sema.decl_table.insert(v->name, decl);
    v->var_decl = &decl->var_decl;

    // struct member declarations are also parsed as VarDecls.
    if (v->kind == VarDeclNode::Kind::struct_) {
        assert(!sema.context.struct_decl_stack.empty());
        auto curr_struct = sema.context.struct_decl_stack.back();
        curr_struct->fields.push_back(v->var_decl);
    } else if (v->kind == VarDeclNode::Kind::func) {
        assert(!sema.context.func_decl_stack.empty());
        auto curr_func = sema.context.func_decl_stack.back();
        curr_func->args.push_back(v->var_decl);
    }
}

void NameBinder::visit_struct_decl(StructDeclNode *s) {
    auto found = sema.decl_table.find(s->name);
    if (found && found->value->kind == DECL_TYPE &&
        found->scope_level <= sema.decl_table.scope_level) {
        sema.error(s->pos, fmt::format("redefinition of '{}'", s->name->str()));
        return;
    }

    auto decl = sema.make_decl(StructDecl{s->name});
    sema.decl_table.insert(s->name, decl);
    s->struct_decl = &decl->struct_decl;

    // Decl table is used for checking redefinition when parsing the member
    // list.
    sema.decl_table.scope_open();
    sema.context.struct_decl_stack.push_back(s->struct_decl);

    walk_struct_decl(*this, s);

    sema.context.struct_decl_stack.pop_back();
    sema.decl_table.scope_close();
}

void NameBinder::visit_func_decl(FuncDeclNode *f) {
    auto found = sema.decl_table.find(f->name);
    if (found && found->value->kind == DECL_FUNC &&
        found->scope_level <= sema.decl_table.scope_level) {
        sema.error(f->pos, fmt::format("redefinition of '{}'", f->name->str()));
        return;
    }

    auto decl = sema.make_decl(FuncDecl{f->name});
    sema.decl_table.insert(f->name, decl);
    f->func_decl = &decl->func_decl;

    sema.decl_table.scope_open(); // for argument variables
    sema.context.func_decl_stack.push_back(f->func_decl);

    walk_func_decl(*this, f);

    sema.context.func_decl_stack.pop_back();
    sema.decl_table.scope_close();
}

void TypeChecker::visit_assign_stmt(AssignStmt *as) {
    sema.error(as->pos, "type checking assign stmt");

    assert(as->rhs->type);
    assert(as->lhs->type);
}

void TypeChecker::visit_integer_literal(IntegerLiteral *i) {
    auto int_name = sema.names.get("int");
    auto int_decl = sema.decl_table.find(int_name)->value;
    i->type = int_decl->struct_decl.type;
    assert(i->type);
}

void TypeChecker::visit_string_literal(StringLiteral *s) {
    // TODO
}

void TypeChecker::visit_type_expr(TypeExpr *t) {
    walk_type_expr(*this, t);

    // First, find a type that has this exact name
    if (t->kind == TypeExprKind::value) {
        // t->decl should be non-null after the passing the name binding.
        // And, since we are currently doing single-pass (TODO), its type
        // should also be resolved by now.
        assert(t->decl->struct_decl.type);
        sema.error(t->pos, "lets push these to the table");
    }
}

void TypeChecker::visit_struct_decl(StructDeclNode *s) {
    // Typecheck members first
    walk_struct_decl(*this, s);

    auto type = sema.make_type(TypeKind::value, s->name, nullptr);
    // XXX: no need to insert to type table?
    s->struct_decl->type = type;
}

void TypeChecker::visit_var_decl(VarDeclNode *v) {
    walk_var_decl(*this, v);

    // If a variable declaration specifies the type or an assignment
    // expression, we can just take their inference result.
    if (v->type_expr) {
        v->var_decl->type = v->type_expr->type;
    } else if (v->assign_expr) {
        v->var_decl->type = v->assign_expr->type;
    } else {
        assert(false && "unreachable");
    }
}

} // namespace cmp
