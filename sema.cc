#include "sema.h"
#include "ast.h"
#include "parser.h"
#include "fmt/core.h"
#include "source.h"
#include <cassert>

namespace cmp {

std::string Type::str() const { return name->text; }

Decl *make_decl(Sema *sema, const VarDecl &var_decl)
{
    Decl *decl = new Decl{var_decl};
    sema->decl_pool.push_back(decl);
    return decl;
}

Decl *make_decl(Sema *sema, const TypeDecl &type_decl)
{
    Decl *decl = new Decl{type_decl};
    sema->decl_pool.push_back(decl);
    return decl;
}

Decl *make_decl(Sema *sema, const FuncDecl &func_decl)
{
    Decl *decl = new Decl{func_decl};
    sema->decl_pool.push_back(decl);
    return decl;
}

// TODO: Decl::str()
// std::string VarDecl::str() const {
//     return name->text;
// }
// 
// std::string StructDecl::str() const {
//     return name->text;
// }

void Sema::error(size_t pos, const std::string &msg) {
    Error e{source.locate(pos), msg};
    fmt::print("{}\n", e.str());
}

// Get or make a reference type of a given type.
// TODO: inefficient string operations?
Type *getReferenceType(Sema &sema, Type *type) {
    Name *name = sema.names.get_or_add("&" + type->name->text);
    // FIXME: scope_level
    Type ref_type{Type::Kind::ref, name, type};
    if (auto found = sema.type_table.find(name))
        return &found->value;
    return sema.type_table.insert(name, ref_type);
}

//
// AST Traversal
//

void walk_ast(Sema *sema, AstNode *node, bool (*pre_fn)(Sema *sema, AstNode *),
              bool (*post_fn)(Sema *sema, AstNode *)) {
    assert(node);

    if (!pre_fn(sema, node))
        return;

    switch (node->kind) {
    case AstKind::file:
        for (auto tl : static_cast<File *>(node)->toplevels)
            walk_ast(sema, tl, pre_fn, post_fn);
        break;
    case AstKind::decl_stmt:
        walk_ast(sema, static_cast<DeclStmt *>(node)->decl, pre_fn, post_fn);
        break;
    case AstKind::expr_stmt:
        walk_ast(sema, static_cast<ExprStmt *>(node)->expr, pre_fn, post_fn);
        break;
    case AstKind::assign_stmt:
        walk_ast(sema, static_cast<AssignStmt *>(node)->lhs, pre_fn, post_fn);
        walk_ast(sema, static_cast<AssignStmt *>(node)->rhs, pre_fn, post_fn);
        break;
    case AstKind::return_stmt: {
        ReturnStmt *ret = static_cast<ReturnStmt *>(node);
        if (ret)
            // ret->expr might be nullptr if no return statment was ever
            // processed.
            walk_ast(sema, ret->expr, pre_fn, post_fn);
        break;
    }
    case AstKind::compound_stmt:
        for (auto stmt : static_cast<CompoundStmt *>(node)->stmts)
            walk_ast(sema, stmt, pre_fn, post_fn);
        break;
    case AstKind::var_decl: {
        auto var = static_cast<VarDeclNode *>(node);
        if (var->assign_expr)
            walk_ast(sema, var->assign_expr, pre_fn, post_fn);
        else if (var->type_expr)
            walk_ast(sema, var->type_expr, pre_fn, post_fn);
        else
            assert(false && "unreachable");
        break;
    }
    case AstKind::struct_decl:
        for (auto m : static_cast<StructDeclNode *>(node)->members)
            walk_ast(sema, m, pre_fn, post_fn);
        break;
    case AstKind::func_decl: {
        // TODO: ret_type insertion between ret_type_expr and body?
        auto func = static_cast<FuncDeclNode *>(node);
        if (func->ret_type_expr)
            walk_ast(sema, func->ret_type_expr, pre_fn, post_fn);
        for (auto p : func->args)
            walk_ast(sema, p, pre_fn, post_fn);
        walk_ast(sema, func->body, pre_fn, post_fn);
        break;
    }
    case AstKind::unary_expr: {
        // TODO: proper UnaryKind subtypes
        if (static_cast<UnaryExpr *>(node)->operand)
            walk_ast(sema, static_cast<UnaryExpr *>(node)->operand, pre_fn,
                    post_fn);
        break;
    }
    case AstKind::func_call_expr:
        for (auto arg : static_cast<FuncCallExpr *>(node)->args)
            walk_ast(sema, arg, pre_fn, post_fn);
        break;
    case AstKind::type_expr: {
        auto type_expr = static_cast<TypeExpr *>(node);
        if (type_expr->subexpr)
            walk_ast(sema, type_expr->subexpr, pre_fn, post_fn);
        break;
    }
    case AstKind::binary_expr:
        walk_ast(sema, static_cast<BinaryExpr *>(node)->lhs, pre_fn, post_fn);
        walk_ast(sema, static_cast<BinaryExpr *>(node)->rhs, pre_fn, post_fn);
        break;
    default:
        // BadExprs, Literals, etc.
        break;
    }

    if (!post_fn(sema, node))
        return; // XXX: pointless
}

//
// Name binding pass
//

bool CompoundStmt::name_bind_pre(Sema *sema)
{
    sema->decl_table.scope_open();
    return true;
}

bool CompoundStmt::name_bind_post(Sema *sema)
{
    sema->decl_table.scope_close();
    return true;
}

// FIXME: should this be pre or post for VarDecl?
bool VarDeclNode::name_bind_post(Sema *sema)
{
    auto found = sema->decl_table.find(name);
    if (found && found->value->kind == DECL_VAR &&
        found->scope_level <= sema->type_table.scope_level) {
        sema->error(pos, fmt::format("redefinition of '{}'", name->str()));
        return false;
    }

    auto decl = make_decl(sema, VarDecl{name});
    sema->decl_table.insert(name, decl);
    var_decl = &decl->var_decl;

    // Note that member declarations are also parsed as VarDecls.
    if (kind == Kind::struct_) {
        assert(!sema->context.struct_decl_stack.empty());
        auto curr_struct = sema->context.struct_decl_stack.back();
        curr_struct->fields.push_back(var_decl);
    } else if (kind == Kind::func) {
        assert(!sema->context.func_decl_stack.empty());
        auto curr_func = sema->context.func_decl_stack.back();
        curr_func->args.push_back(var_decl);
    }

    return true;
}

// We need to push StructDecl at pre, so that the inner member declarations
// have access and can modify this decl.
bool StructDeclNode::name_bind_pre(Sema *sema)
{
    auto found = sema->decl_table.find(name);
    if (found && found->value->kind == DECL_TYPE &&
        found->scope_level <= sema->decl_table.scope_level) {
        sema->error(pos, fmt::format("redefinition of '{}'", name->str()));
        return false;
    }

    auto decl = make_decl(sema, TypeDecl{name});
    sema->decl_table.insert(name, decl);
    struct_decl = &decl->type_decl;

    // Decl table is used for checking redefinition when parsing the member
    // list.
    sema->decl_table.scope_open();
    sema->context.struct_decl_stack.push_back(struct_decl);

    return true;
}

bool StructDeclNode::name_bind_post(Sema *sema)
{
    sema->context.struct_decl_stack.pop_back();
    sema->decl_table.scope_close();

    return true;
}

bool FuncDeclNode::name_bind_pre(Sema *sema)
{
    auto found = sema->decl_table.find(name);
    if (found && found->value->kind == DECL_FUNC &&
        found->scope_level <= sema->type_table.scope_level) {
        sema->error(pos, fmt::format("redefinition of '{}'", name->str()));
        // TODO: return false so that traversal is early-exited
        return false;
    }

    auto decl = make_decl(sema, FuncDecl{name});
    sema->decl_table.insert(name, decl);
    func_decl = &decl->func_decl;

    sema->decl_table.scope_open(); // for argument variables
    sema->context.func_decl_stack.push_back(func_decl);

    return true;
}

bool FuncDeclNode::name_bind_post(Sema *sema)
{
    sema->context.func_decl_stack.pop_back();
    sema->decl_table.scope_close();

    return true;
}

bool DeclRefExpr::name_bind_post(Sema *sema)
{
    auto sym = sema->decl_table.find(name);
    if (sym) {
        decl = sym->value;
    } else {
        sema->error(
            pos, fmt::format("use of undeclared identifier '{}'", name->str()));
    }

    return true;
}

bool FuncCallExpr::name_bind_pre(Sema *sema)
{
    auto sym = sema->decl_table.find(func_name);
    if (sym) {
        if (sym->value->kind == DECL_FUNC) {
            func_decl = &sym->value->func_decl;
        } else {
            sema->error(
                pos, fmt::format("'{}' is not a function", func_name->str()));
            return false;
        }
    } else {
        sema->error(pos,
                    fmt::format("undeclared function '{}'", func_name->str()));
        return false;
    }

    return true;
}

bool FuncCallExpr::name_bind_post(Sema *sema)
{
    assert(func_decl);

    // check if argument count matches
    if (func_decl->args_count() != args.size()) {
        sema->error(pos,
                    fmt::format("function '{}' accepts {} arguments, got {}",
                                func_name->str(), func_decl->args_count(),
                                args.size()));
    }

    return true;
}

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

bool TypeExpr::name_bind_post(Sema *sema)
{
    auto sym = sema->decl_table.find(name);
    if (sym) {
        // type = &sym->value;
    } else {
        sema->error(pos,
                    fmt::format("use of undeclared type '{}'", name->str()));
        return false;
    }

    return true;
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
    case AstKind::paren_expr:
        operand->walk(sema);
        type = operand->type;
        break;
    case AstKind::deref_expr:
        operand->walk(sema);
        if (operand->type->kind != Type::Kind::ref)
            sema.error(pos, "cannot dereference a non-reference");
        type = operand->type->target_type;
        break;
    case AstKind::address_expr:
        operand->walk(sema);
        assert(operand->kind == AstKind::unary_expr);
        if (operand->kind != AstKind::deref_expr) {
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
        if (!ref) {
            sema.error(pos, fmt::format("unknown type '{}'", name->str()));
        }
        // If not, this is an instantiation of a derivative type, and should be
        // put into the table.
        else {
            assert(subexpr);
            // FIXME: scope_level
            Type ref_type{Type::Kind::ref, name, subexpr->type};
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

Sema::Sema(const Source &s, NameTable &n) : source(s), names(n)
{
    // set up built-in types
    Name *int_name = names.get_or_add("int");
    auto int_type = make_decl(this, TypeDecl{int_name});
    decl_table.insert(int_name, int_type);
    // int_type = type_table.insert(int_name, Type{int_name});

    Name *char_name = names.get_or_add("char");
    auto char_type = make_decl(this, TypeDecl{char_name});
    decl_table.insert(char_name, char_type);
    // char_type = type_table.insert(char_name, Type{char_name});
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
    decl_table.scope_open();
    type_table.scope_open();
}

void Sema::scope_close() {
    decl_table.scope_close();
    type_table.scope_close();
}

void Sema::report() const {
    for (auto e : errors)
        fmt::print("{}\n", e.str());
}

// See comments for cmp::verify().
bool Sema::verify() const {
    return cmp::verify(source.filename, errors, beacons);
}

void sema(Sema &sema, Ast &ast) { ast.root->walk(sema); }

} // namespace cmp
