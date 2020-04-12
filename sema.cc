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
    Type ref_type{TypeKind::ref, name, type, nullptr};
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

void push_builtin_type_from_name(Sema &s, const std::string &str) {
    Name *name = s.names.get_or_add(str);
    auto struct_decl = s.make_decl<StructDecl>(name);
    struct_decl->type = s.make_type(name);
    s.decl_table.insert(name, struct_decl);
}

// Push Decls for the builtin types into the global scope of decl_table, so
// that they are visible from any point in the AST.
void setup_builtin_types(Sema &s) {
    push_builtin_type_from_name(s, "int");
    push_builtin_type_from_name(s, "char");
}

Sema::~Sema() {
    for (auto d : decl_pool)
        delete d;
    for (auto t : type_pool)
        delete t;
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
    if (sym && decl_is<VarDecl *>(sym->value)) {
        d->var_decl = get<VarDecl *>(sym->value);
    } else {
        sema.error(d->pos, fmt::format("use of undeclared identifier '{}'",
                                       d->name->str()));
    }
}

// Only binds the function name part of the call, e.g. 'func' of func().
void NameBinder::visit_func_call_expr(FuncCallExpr *f) {
    // resolve function name
    auto sym = sema.decl_table.find(f->func_name);
    if (!sym) {
        sema.error(f->pos, fmt::format("undeclared function '{}'",
                                       f->func_name->str()));
        return;
    }
    if (!decl_is<FuncDecl *>(sym->value)) {
        sema.error(f->pos,
                   fmt::format("'{}' is not a function", f->func_name->str()));
        return;
    }
    f->func_decl = get<FuncDecl *>(sym->value);
    assert(f->func_decl);

    walk_func_call_expr(*this, f);

    // check if argument count matches
    if (f->func_decl->args_count() != f->args.size()) {
        sema.error(f->pos,
                   fmt::format("'{}' accepts {} arguments, got {}",
                               f->func_name->str(), f->func_decl->args_count(),
                               f->args.size()));
    }

    // make decl for the return value
    f->var_decl = sema.make_decl<VarDecl>(nullptr);
}

void NameBinder::visit_type_expr(TypeExpr *t) {
    walk_type_expr(*this, t);

    // Name binding for TypeExprs only include linking existing Decls to the
    // type names used in the expression, not declaring new ones.  The
    // declaration would be done when visiting VarDecls and StructDecls, etc.

    // XXX: only do this for value types?
    auto sym = sema.decl_table.find(t->name);
    if (sym && decl_is<StructDecl *>(sym->value)) {
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
    if (found && decl_is<VarDecl *>(found->value) &&
        found->scope_level <= sema.decl_table.scope_level) {
        sema.error(v->pos, fmt::format("redefinition of '{}'", v->name->str()));
        return;
    }

    v->var_decl = sema.make_decl<VarDecl>(v->name);
    sema.decl_table.insert(v->name, v->var_decl);

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
    if (found && decl_is<StructDecl *>(found->value) &&
        found->scope_level <= sema.decl_table.scope_level) {
        sema.error(s->pos, fmt::format("redefinition of '{}'", s->name->str()));
        return;
    }

    s->struct_decl = sema.make_decl(StructDecl{s->name});
    sema.decl_table.insert(s->name, s->struct_decl);

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
    if (found && decl_is<FuncDecl *>(found->value) &&
        found->scope_level <= sema.decl_table.scope_level) {
        sema.error(f->pos, fmt::format("redefinition of '{}'", f->name->str()));
        return;
    }

    f->func_decl = sema.make_decl<FuncDecl>(f->name);
    sema.decl_table.insert(f->name, f->func_decl);

    sema.decl_table.scope_open(); // for argument variables
    sema.context.func_decl_stack.push_back(f->func_decl);

    walk_func_decl(*this, f);

    sema.context.func_decl_stack.pop_back();
    sema.decl_table.scope_close();
}

void TypeChecker::visit_assign_stmt(AssignStmt *as) {
    walk_assign_stmt(*this, as);

    // XXX: is this the best way to early-exit?
    if (!as->rhs->type || !as->lhs->type)
        return;

    // Only allow exact equality for assignment for now (TODO).
    if (as->rhs->type != as->lhs->type)
        sema.error(as->pos, fmt::format("cannot assign '{}' type to '{}'",
                                        as->rhs->type->name->str(),
                                        as->lhs->type->name->str()));
}

void TypeChecker::visit_integer_literal(IntegerLiteral *i) {
    auto int_name = sema.names.get("int");
    auto int_decl =
        get<StructDecl *>(sema.decl_table.find(int_name)->value);
    i->type = int_decl->type;
    assert(i->type);
}

void TypeChecker::visit_string_literal(StringLiteral *s) {
    // TODO
}

void TypeChecker::visit_decl_ref_expr(DeclRefExpr *d) {
    // Link the type already stored in the Decl object to the Expr's type.
    // @future: currently only handles VarDecls.
    assert(d->var_decl->type);
    d->type = d->var_decl->type;
}

// The VarDecl of a function call's return value is temporary.  Only when it is
// binded to a variable does it become accessible from later positions in the
// code.
//
// How do we model this temporariness?  Let's think in terms of lifetimes
// ('ribs' in Rust). A function return value is a value whose lifetime starts
// and ends in the same statement.
//
// For now, the tool that we can use for starting and ending a Decl's lifetime
// is scopes.  Therefore, if we reshape this problem into something that
// involves a variable that lives in a microscopic scope confined in a single
// statement, we can model the temporary lifetime:
//
//     let v = f()
//  -> let v = { var temp = f() }
//
// Normally, this micro scope thing would only be needed if a statement
// contains a function call (or any other kind of expressions that spawn a
// temporary Decl).  However, we cannot know this when we are visiting the
// encompassing statement node unless we do some look-ahead.  So we just do
// this pushing and popping of micro scopes for every kind of statements.  This
// indicates that the scope_open/scope_close function should be implemented
// reasonably efficiently.
//
// Some interesting cases to think about:
//
// * let v = f()
// * let v = (f())
// * let v = f().mem
//
void TypeChecker::visit_func_call_expr(FuncCallExpr *f) {
    walk_func_call_expr(*this, f);

    assert(f->func_decl->return_type);
    f->var_decl->type = f->func_decl->return_type;
    f->type = f->func_decl->return_type;
}

// MemberExprs cannot be namebinded completely without type checking (e.g.
// func().mem).  So we defer their namebinding to the type checking phase,
// which is done here.
void TypeChecker::visit_member_expr(MemberExpr *m) {
    // Propagate from left to right (struct->mem).
    walk_member_expr(*this, m);

    // If the struct side failed to typecheck, we cannot proceed.
    if (!m->struct_expr->type)
        return;

    if (!m->struct_expr->decl()) {
        sema.error(m->struct_expr->pos,
                   fmt::format("type {} is not a struct",
                               m->struct_expr->type->name->str()));
        return;
    }

    assert(decl_is<VarDecl *>(*m->struct_expr->decl()));
    auto lhs_type = get<VarDecl *>(*m->struct_expr->decl())->type;

    // make sure the LHS is actually a struct
    if (!lhs_type->is_struct()) {
        sema.error(
            m->struct_expr->pos,
            fmt::format("type {} is not a structure", lhs_type->name->str()));
        return;
    }

    // find the member with the same name
    for (auto mem_decl : lhs_type->struct_decl->fields)
        if (m->member_name == mem_decl->name)
            m->var_decl = mem_decl;
    if (!m->var_decl) {
        // TODO: pos for member
        sema.error(m->struct_expr->pos,
                   fmt::format("'{}' is not a member of '{}'",
                               m->member_name->str(), lhs_type->name->str()));
        return;
    }

    // Since the VarDecls for the fields are already typechecked, just
    // copying over the type of the VarDecl completes typecheck for this
    // MemberExpr.
    assert(m->var_decl->type);
    m->type = m->var_decl->type;
}

void TypeChecker::visit_paren_expr(ParenExpr *p) {
    walk_paren_expr(*this, p);

    if (!p->operand->type)
        return;

    p->type = p->operand->type;
}

void TypeChecker::visit_binary_expr(BinaryExpr *b) {
    walk_binary_expr(*this, b);

    if (!b->lhs->type || !b->rhs->type)
        return;

    if (b->lhs->type != b->rhs->type)
        sema.error(
            b->pos,
            fmt::format(
                "incompatible types to binary expression ('{}' and '{}')",
                b->lhs->type->name->str(), b->rhs->type->name->str()));
}

void TypeChecker::visit_type_expr(TypeExpr *t) {
    walk_type_expr(*this, t);

    // first, find a type that has this exact name
    if (t->kind == TypeExprKind::value) {
        // t->decl should be non-null after the passing the name binding.
        // And, since we are currently doing single-pass (TODO), its type
        // should also be resolved by now.
        t->type = get<StructDecl *>(t->decl)->type;
        assert(t->type);
    } else {
        assert(false && "whooops");
    }
}

void TypeChecker::visit_var_decl(VarDeclNode *v) {
    walk_var_decl(*this, v);

    // If a variable declaration specifies the type or an assignment
    // expression, we can just take their inference result.
    if (v->type_expr) {
        v->var_decl->type = v->type_expr->type;
        assert(v->var_decl->type);
    } else if (v->assign_expr) {
        v->var_decl->type = v->assign_expr->type;
    } else {
        assert(false && "unreachable");
    }
}

void TypeChecker::visit_struct_decl(StructDeclNode *s) {
    walk_struct_decl(*this, s);

    // Create a new type for this struct.
    auto type =
        sema.make_type(TypeKind::value, s->name, nullptr, s->struct_decl);
    // XXX: no need to insert to type table?
    s->struct_decl->type = type;
}

void TypeChecker::visit_func_decl(FuncDeclNode *f) {
    walk_func_decl(*this, f);

    if (f->ret_type_expr) {
        if (!f->ret_type_expr->type)
            return;
        f->func_decl->return_type = f->ret_type_expr->type;
    }

    // TODO: return type check from ReturnStmts
}

} // namespace cmp
