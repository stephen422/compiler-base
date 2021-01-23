#include "sema.h"
#include "ast.h"
#include "ast_visitor.h"
#include "fmt/core.h"
#include "parser.h"
#include "source.h"
#include "types.h"
#include <cassert>
#include <cstdarg>

#define BUFSIZE 1024

using namespace cmp;

template <typename... Args> void Sema::error(size_t pos, Args &&...args) {
    auto message = fmt::format(std::forward<Args>(args)...);
    auto loc = source.locate(pos);
    fmt::print(stderr, "{}:{}:{}: error: {}\n", loc.filename, loc.line, loc.col,
               message);
    exit(EXIT_FAILURE);
}

Type::Type(Name *n, TypeKind k, Type *rt) : kind(k), name(n), referee_type(rt) {
    copyable = k == TypeKind::ref;
}

bool Type::is_builtin(Sema &sema) const {
    return this == sema.context.int_type || this == sema.context.char_type ||
           this == sema.context.void_type || this == sema.context.string_type;
}

bool Type::isEnum() const {
    // TODO: should base_type be null too?
    return kind == TypeKind::value && type_decl && type_decl->is<EnumDecl>();
}

StructDecl *Type::getStructDecl() { return type_decl->as<StructDecl>(); }
EnumDecl *Type::getEnumDecl() { return type_decl->as<EnumDecl>(); }

Type *make_builtin_type(Sema &sema, Name *n) {
    Type *t = new Type(n);
    sema.type_pool.push_back(t);
    return t;
}

Type *make_value_type(Sema &sema, Name *n, Decl *decl) {
    Type *t = new Type(TypeKind::value, n, decl);
    sema.type_pool.push_back(t);
    return t;
}

Type *make_ref_type(Sema &sema, Name *name, TypeKind ptr_kind,
                    Type *referee_type) {
    Type *t = new Type(name, ptr_kind, referee_type);
    sema.type_pool.push_back(t);
    return t;
}

Type *push_builtin_type_from_name(Sema &s, const std::string &str) {
    Name *name = s.name_table.pushlen(str.data(), str.length());
    auto struct_decl =
        s.make_node<StructDecl>(name, std::vector<VarDecl *>() /* FIXME */);
    struct_decl->type = make_builtin_type(s, name);
    s.decl_table.insert(name, struct_decl);
    return struct_decl->type;
}

// Push Decls for the builtin types into the global scope of decl_table, so
// that they are visible from any point in the AST.
void cmp::setup_builtin_types(Sema &s) {
    s.context.void_type = push_builtin_type_from_name(s, "void");
    s.context.int_type = push_builtin_type_from_name(s, "int");
    s.context.char_type = push_builtin_type_from_name(s, "char");
    s.context.string_type = push_builtin_type_from_name(s, "string");
}

Sema::~Sema() {
    for (auto t : type_pool) {
        delete t;
    }
    for (auto lt : lifetime_pool) {
        delete lt;
    }
    for (auto b : basic_block_pool) {
        delete b;
    }
}

void Sema::scope_open() {
    decl_table.scope_open();
    type_table.scope_open();
    lifetime_table.scope_open();
    borrow_table.scope_open();
}

void Sema::scope_close() {
    decl_table.scope_close();
    type_table.scope_close();
    lifetime_table.scope_close();
    borrow_table.scope_close();
}

void NameBinding::visitCompoundStmt(CompoundStmt *cs) {
    sema.scope_open();
    walk_compound_stmt(*this, cs);
    sema.scope_close();
}

void NameBinding::visitDeclRefExpr(DeclRefExpr *d) {
    auto sym = sema.decl_table.find(d->name);
    if (!sym) {
        sema.error(d->pos, "use of undeclared identifier '{}'", d->name->text);
        return;
    }
    d->decl = sym->value;
}

void NameBinding::visitCallExpr(CallExpr *f) {
    auto sym = sema.decl_table.find(f->func_name);
    if (!sym) {
        sema.error(f->pos, "undeclared function '{}'", f->func_name->text);
        return;
    }

    if (!sym->value->is<FuncDecl>()) {
        sema.error(f->pos, "'{}' is not a function", f->func_name->text);
        return;
    }

    f->callee_decl = sym->value; // FIXME
    assert(f->callee_decl);

    walk_func_call_expr(*this, f);

    // argument count match check
    if (f->kind == CallExprKind::func &&
        f->callee_decl->as<FuncDecl>()->args_count() != f->args.size()) {
        sema.error(
            f->pos, "'{}' accepts {} arguments, got {}", f->func_name->text,
            f->callee_decl->as<FuncDecl>()->args_count(), f->args.size());
        return;
    }
}

void NameBinding::visitTypeExpr(TypeExpr *t) {
    walk_type_expr(*this, t);

    // Namebinding for TypeExprs only include linking existing Decls to the
    // type names used in the expression, not declaring new ones.  The
    // declaration would be done when visiting VarDecls and StructDecls, etc.

    // For pointers and arrays, proper typechecking will be done in the later
    // stages.
    if (t->subexpr)
        return;

    auto sym = sema.decl_table.find(t->name);
    if (sym && sym->value->typemaybe()) {
        assert(t->kind == TypeKind::value);
        t->decl = sym->value;
    } else {
        sema.error(t->pos, "use of undeclared type '{}'", t->name->text);
        return;
    }
}

// Semantically declare a 'name' at 'pos', whose Decl type is T.
// Returns true if success and otherwise does error handling.
template <typename T>
bool declare(Sema &sema, size_t pos, Name *name, T *decl) {
    auto found = sema.decl_table.find(name);
    if (found && found->value->is<T>() &&
        found->scope_level == sema.decl_table.curr_scope_level) {
        sema.error(pos, "redefinition of '{}'", name->text);
        return false;
    }

    // Creates the binding between the decl and the name.
    sema.decl_table.insert(name, decl);

    return true;
}

void NameBinding::visitVarDecl(VarDecl *v) {
    walk_var_decl(*this, v);

    if (!declare<VarDecl>(sema, v->pos, v->name, v))
        return;
}

void NameBinding::visitFuncDecl(FuncDecl *f) {
    if (!declare<FuncDecl>(sema, f->pos, f->name, f))
        return;

    // scope for argument variables
    sema.decl_table.scope_open();
    sema.context.func_decl_stack.push_back(f);

    walk_func_decl(*this, f);

    sema.context.func_decl_stack.pop_back();
    sema.decl_table.scope_close();
}

void NameBinding::visitStructDecl(StructDecl *s) {
    if (!declare<StructDecl>(sema, s->pos, s->name, s))
        return;

    // Decl table is used for checking redefinition when parsing the member
    // list.
    sema.decl_table.scope_open();
    walk_struct_decl(*this, s);
    sema.decl_table.scope_close();
}

#if 0
// Generate a name for the anonymous fields in each enum variant structs.
// For now, these are named "_0", "_1", and so on.
Name *genAnonymousFieldName(Sema &sema, size_t index) {
  char buf[BUFSIZE];
  snprintf(buf, BUFSIZE, "_%lu", index);
  return sema.name_table.get_or_add(std::string{buf});
}
#endif

// Checks if 'expr' is a dereferencing expression, i.e.. '*expr'.
static bool isderefexpr(const Expr *e) {
    return e->kind == ExprKind::unary &&
           e->as<UnaryExpr>()->kind == UnaryExprKind::deref;
}

// Return the Decl object that represents the value of the expression `e` via
// `decl`. It could be a VarDecl for a DerefExpr, FuncDecl for a DeclRefExpr,
// etc.
// Returns false if `e` is not a kind that contains a decl, e.g. a BinaryExpr.
static bool getdecl(const Expr *e, Decl **decl) {
    bool contains = false;
    Decl *d = NULL;

    switch (e->kind) {
    case ExprKind::decl_ref:
        contains = true;
        d = e->as<DeclRefExpr>()->decl;
        break;
    case ExprKind::member:
        contains = e->as<MemberExpr>()->decl.has_value();
        if (contains)
            d = *e->as<MemberExpr>()->decl;
        break;
    case ExprKind::unary:
        if (e->as<UnaryExpr>()->kind == UnaryExprKind::paren) {
            return getdecl(e->as<UnaryExpr>()->as<ParenExpr>()->operand, decl);
        } else if (e->as<UnaryExpr>()->kind == UnaryExprKind::deref) {
            contains = true;
            d = e->as<UnaryExpr>()->var_decl;
        }
        break;
    default:
        break;
    }

    if (decl && contains)
        *decl = d;

    return contains;
}

static bool hasdecl(const Expr *e) { return getdecl(e, NULL); }

static bool islvalue(const Expr *e) {
    Decl *d;
    bool r = getdecl(e, &d);
    return r && d->is<VarDecl>();
}

// Get the VarDecl object that binds to this L-value.
static VarDecl *lvaluedecl(const Expr *e) {
    Decl *d;

    assert(islvalue(e));
    assert(getdecl(e, &d));
    return d->as<VarDecl>();
}

void NameBinding::visitEnumVariantDecl(EnumVariantDecl *v) {
    walk_enum_variant_decl(*this, v);

    // assert(false && "FIXME");
#if 0
  // first, declare a struct of this name
  if (!declare<StructDecl>(sema, v->pos, v->name, nullptr)) return;

  // then, add fields to this struct, whose names are anonymous
  // and only types are specified (e.g. Pos(int, int))
  for (size_t i = 0; i < v->fields.size(); i++) {
    // so that anonymous field names don't clash
    sema.decl_table.scope_open();

    if (auto field_decl = declare<VarDecl>(sema, v->fields[i]->pos,
                                           genAnonymousFieldName(sema, i),
                                           false /* The language doesn't support
                                                    mutability on the struct
                                                    field level. */)) {
      v->struct_decl->fields.push_back(field_decl);
    }

    sema.decl_table.scope_close();
  }

  // now, add this struct into the scope of the enum
  assert(!sema.context.enum_decl_stack.empty());
  auto enclosing_enum = sema.context.enum_decl_stack.back();
  enclosing_enum->variants.push_back(v->struct_decl);
#endif
}

void NameBinding::visitEnumDecl(EnumDecl *e) {
    if (!declare<EnumDecl>(sema, e->pos, e->name, e))
        return;

    sema.decl_table.scope_open();
    sema.context.enum_decl_stack.push_back(e);

    walk_enum_decl(*this, e);

    sema.context.enum_decl_stack.pop_back();
    sema.decl_table.scope_close();
}

// Returns true if success and otherwise (e.g. redeclaration) do error handling.
bool declare(Sema &sema, Name *name, Decl *decl) {
    auto found = sema.decl_table.find(name);
    if (found && found->value->kind == decl->kind &&
        found->scope_level == sema.decl_table.curr_scope_level) {
        sema.error(decl->pos, "redefinition of '{}'", name->text);
        return false;
    }

    sema.decl_table.insert(name, decl);
    return true;
}

static void typecheckDecl(Sema &sema, Decl *d);

static void typecheckExpr(Sema &sema, Expr *e) {
    switch (e->kind) {
    case ExprKind::integer_literal:
        break;
    default:
        assert(!"unknown expr kind");
    }
}

static void typecheckStmt(Sema &sema, Stmt *s) {
    switch (s->kind) {
    case StmtKind::decl:
        typecheckDecl(sema, static_cast<DeclStmt *>(s)->decl);
        break;
    case StmtKind::assign:
        // TODO
        break;
    default:
        assert(!"unknown stmt kind");
    }
}

static void typecheckDecl(Sema &sema, Decl *d) {
    switch (d->kind) {
    case DeclKind::var: {
        auto v = static_cast<VarDecl *>(d);
        if (!declare(sema, v->name, v)) {
            return;
        }
        typecheckExpr(sema, v->assign_expr);
        break;
    }
    case DeclKind::func:
        for (auto body_stmt : static_cast<FuncDecl *>(d)->body->stmts) {
            typecheckStmt(sema, body_stmt);
        }
        break;
    default:
        assert(!"unknown decl kind");
    }
}

void cmp::typecheck(Sema &sema, AstNode *n) {
    switch (n->kind) {
    case AstKind::file:
        for (auto toplevel : static_cast<File *>(n)->toplevels) {
            typecheck(sema, toplevel);
        }
        break;
    case AstKind::stmt:
        typecheckStmt(sema, static_cast<Stmt *>(n));
        break;
    case AstKind::decl:
        typecheckDecl(sema, static_cast<Decl *>(n));
        break;
    default:
        assert(!"unknown ast kind");
    }
}

// Mutability check for assignment statements.
static bool mutcheck_assign(Sema &sema, const Expr *lhs) {
    if (lhs->kind == ExprKind::member) {
        // For MemberExprs, assignability depends on that of its struct side.
        return mutcheck_assign(sema, lhs->as<MemberExpr>()->struct_expr);
    } else if (isderefexpr(lhs)) {
        auto unary = lhs->as<UnaryExpr>();
        if (unary->operand->type->kind != TypeKind::var_ref) {
            sema.error(unary->pos, "'{}' is not a mutable reference",
                       lvaluedecl(unary->operand)->name->text);
            return false;
        }
    } else {
        auto var_decl = lvaluedecl(lhs);
        if (var_decl && !var_decl->mut) {
            sema.error(lhs->pos, "'{}' is not declared as mutable",
                       var_decl->name->text);
            return false;
        }
    }

    return true;
}

// Returns true if this type is a reference type.
static bool isreftype(const Type *ty) {
    return ty->kind == TypeKind::ref || ty->kind == TypeKind::var_ref;
}

// Returns true if this type is a reference type.
static bool isstructtype(const Type *ty) {
    return ty->kind == TypeKind::value && ty->type_decl &&
           ty->type_decl->is<StructDecl>();
}

// Typecheck assignment statement of 'lhs = rhs'.
static bool typecheck_assign(const Type *lhs, const Type *rhs) {
    // TODO: Typecheck assignment rules so far:
    //
    // 1. Reference <- mutable reference.
    // 2. Exact same match.

    // Allow promotion from mutable to immutable reference.
    if (lhs->kind == TypeKind::ref && isreftype(rhs)) {
        // TODO: 'unification'? Ref:
        // http://smallcultfollowing.com/babysteps/blog/2017/03/25/unification-in-chalk-part-1/
        return typecheck_assign(lhs->referee_type, rhs->referee_type);
    }
    return lhs == rhs;
}

// Assignments should check that the LHS is an lvalue.
// This check cannot be done reliably in the parsing stage because it depends
// on the actual type of the expression, not just its kind; e.g. (v) or (3).
//
//                 3 = 4
Type *TypeChecker::visitAssignStmt(AssignStmt *as) {
    walk_assign_stmt(*this, as);

    auto lhs_ty = as->lhs->type;
    auto rhs_ty = as->rhs->type;

    // XXX: is this the best way to early-exit?
    if (!lhs_ty || !rhs_ty)
        return nullptr;

    // Lvalue check.
    if (!islvalue(as->lhs)) {
        sema.error(as->pos, "cannot assign to an rvalue");
        return nullptr;
    }

    // Type compatibility check.
    if (!typecheck_assign(lhs_ty, rhs_ty)) {
        sema.error(as->pos, "cannot assign '{}' type to '{}'",
                   rhs_ty->name->text, lhs_ty->name->text);
        return nullptr;
    }

    // Mutability check.
    //
    // Type compatibility check precedes this, because a type mismatch on an
    // assignment is likely to signify a larger error in the source code than a
    // mutability error (which can mostly be fixed with a single keyword
    // change).
    if (!mutcheck_assign(sema, as->lhs))
        return nullptr;

    // Copyability check.
    //
    // Even if RHS has a non-copyable type, if it is a temporary value, its copy
    // becomes essentially the same as move and thus is allowed.
    // For example, with 'S' being a non-copyable type, the following is legal:
    //
    //     let s1 = S {...};
    //
    // TODO: there's a copy-paste of this code somewhere else.
    if (hasdecl(as->rhs) && !rhs_ty->copyable) {
        sema.error(as->rhs->pos, "cannot copy non-copyable type '{}'",
                   rhs_ty->name->text);
        return nullptr;
    }

    return lhs_ty;
}

Type *TypeChecker::visitReturnStmt(ReturnStmt *rs) {
    visitExpr(rs->expr);
    if (!rs->expr->type)
        return nullptr;

    assert(!sema.context.func_decl_stack.empty());
    auto func_decl = sema.context.func_decl_stack.back();
    if (func_decl->rettype == sema.context.void_type) {
        sema.error(rs->expr->pos, "function '{}' should not return a value",
                   func_decl->name->text);
        return nullptr;
    }

    if (!typecheck_assign(func_decl->rettype, rs->expr->type)) {
        sema.error(rs->expr->pos,
                   "return type mismatch: function returns '{}', but got '{}'",
                   func_decl->rettype->name->text, rs->expr->type->name->text);
        return nullptr;
    }

    return rs->expr->type;
}

Type *TypeChecker::visitIntegerLiteral(IntegerLiteral *i) {
    i->type = sema.context.int_type;
    return i->type;
}

Type *TypeChecker::visitStringLiteral(StringLiteral *s) {
    s->type = sema.context.string_type;
    return s->type;
}

Type *TypeChecker::visitDeclRefExpr(DeclRefExpr *d) {
    // For variables, since there is no type inference now, the type is
    // determined at the same time the variable is declared. So if a variable
    // succeeded namebinding, its type is guaranteed to be determined.
    //
    // For struct and enum names, they are not handled in the namebinding stage
    // and so should be taken care of here.
    auto opt_type = d->decl->typemaybe();
    assert(
        opt_type.has_value() &&
        "tried to typecheck a non-typed DeclRef (first-class functions TODO?)");
    d->type = *opt_type;
    return d->type;
}

Type *TypeChecker::visitCallExpr(CallExpr *f) {
    walk_func_call_expr(*this, f);

    if (f->kind == CallExprKind::func) {
        FuncDecl *callee_func_decl = f->callee_decl->as<FuncDecl>();

        assert(callee_func_decl->rettype);
        f->type = callee_func_decl->rettype;

        // check argument type match
        for (size_t i = 0; i < callee_func_decl->args.size(); i++) {
            if (!f->args[i]->type)
                return nullptr;

            // TODO: proper type comparison
            if (f->args[i]->type != callee_func_decl->args[i]->type) {
                sema.error(f->args[i]->pos,
                           "argument type mismatch: expects '{}', got '{}'",
                           callee_func_decl->args[i]->type->name->text,
                           f->args[i]->type->name->text);
                return nullptr;
            }
        }
    } else {
        assert(!"unreachable");
    }

    return f->type;
}

// Look up the decl of a field of a struct type that has the matching name.
// FIXME: This should return something like a FieldDecl, not a VarDecl.
static VarDecl *findfield(Name *name, Type *struct_ty) {
    for (VarDecl *field : struct_ty->getStructDecl()->fields)
        if (name == field->name)
            return field;
    return nullptr;
}

// Make a new lifetime that is declared by `decl` and starts at the current
// scope.
// The new lifetime will be automatically destroyed on `scope_close()`.
static Lifetime *start_lifetime(Sema &sema, Decl *decl) {
    auto lt = sema.make_lifetime(decl);
    sema.lifetime_table.insert(lt, lt);
    return lt;
}

Type *TypeChecker::visitStructDefExpr(StructDefExpr *s) {
    walk_struct_def_expr(*this, s);

    // check Name is a struct
    Type *ty = s->name_expr->type;
    if (!ty)
        return nullptr;
    if (!isstructtype(ty)) {
        sema.error(s->name_expr->pos, "type '{}' is not a struct",
                   ty->name->text);
        return nullptr;
    }

    for (StructFieldDesignator desig : s->desigs) {
        if (!desig.initexpr->type)
            return nullptr;

        VarDecl *fd;
        if (!(fd = findfield(desig.name, ty))) {
            sema.error(desig.initexpr->pos, // FIXME: wrong pos
                       "'{}' is not a member of '{}'", desig.name->text,
                       ty->getStructDecl()->name->text);
            return nullptr;
        }

        if (!typecheck_assign(fd->type, desig.initexpr->type)) {
            sema.error(desig.initexpr->pos, "cannot assign '{}' type to '{}'",
                       desig.initexpr->type->name->text, fd->type->name->text);
            return nullptr;
        }
    }

    s->type = ty;
    return s->type;
}

Type *TypeChecker::visitCastExpr(CastExpr *c) {
    walk_cast_expr(*this, c);

    c->type = c->type_expr->type;
    return c->type;
}

static VarDecl *addfield(Sema &sema, VarDecl *v, Name *name, Type *type) {
    // mutability is inherited from the parent decl
    VarDecl *fd = sema.make_node<VarDecl>(name, type, v->mut);
    fd->parent = v;
    v->children.push_back({name, fd});
    return fd;
}

// MemberExprs cannot be namebinded completely without type checking (e.g.
// func().mem).  So we defer their namebinding to the type checking phase,
// which is done here.
Type *TypeChecker::visitMemberExpr(MemberExpr *m) {
    // propagate typecheck from left to right (struct -> .mem)
    walk_member_expr(*this, m);

    // if the struct side failed to typecheck, we cannot proceed
    if (!m->struct_expr->type)
        return nullptr;

    Type *lhs_ty = m->struct_expr->type;
    if (!isstructtype(lhs_ty)) {
        sema.error(m->struct_expr->pos, "type '{}' is not a struct",
                   lhs_ty->name->text);
        return nullptr;
    }

    // TODO: isenum
    if (isstructtype(lhs_ty)) {
        VarDecl *fd;
        if (!(fd = findfield(m->member_name, lhs_ty))) {
            // TODO: pos for member
            sema.error(m->struct_expr->pos, "'{}' is not a member of '{}'",
                       m->member_name->text, lhs_ty->name->text);
            return nullptr;
        }

        m->type = fd->type;

        // If struct_expr is an lvalue, this MemberExpr should also be an lvalue
        // and have a Decl object. We do so by inheriting from one of
        // struct_expr's child VarDecls.
        //
        // We need to create a new VarDecl here for each different VarDecl of
        // lhs, because even if with the same struct type and field name,
        // MemberExprs may represent values of different objects in the memory.
        // This happens when the struct_expr is an lvalue.  For example, 'x.a'
        // and 'y.a' in the following occupy two different physical memory
        // space and thus need to be associated to two different Decl objects:
        //
        //    let x = S {.a = ...}
        //    let y = S {.a = ...}
        //    x.a
        //    y.a
        //
        if (islvalue(m->struct_expr)) {
            VarDecl *lhs_decl = lvaluedecl(m->struct_expr);
            for (auto field : lhs_decl->children) {
                if (field.first == m->member_name) {
                    // Field already instantiated into a VarDecl.
                    if (field.second) {
                        m->decl = {field.second};
                        break;
                    }
                }
            }

            // If this field was not yet instantiated, do so.  An example case:
            // '(*p_struct).mem'.
            //
            // As a result of this process, we achieve space savings, because
            // only the members that are actually used in the source code are
            // instantiated.
            if (!m->decl) {
                m->decl = {addfield(sema, lhs_decl, m->member_name, m->type)};
                (*m->decl)->as<VarDecl>()->lifetime =
                    start_lifetime(sema, (*m->decl));
            }
        }
    }

#if 0
    if (struct_type->isEnum()) {
        assert(false && "not yet");
    for (auto mem_decl : struct_type->getEnumDecl()->variants) {
      if (m->member_name == mem_decl->name) {
        m->decl = mem_decl;
        found = true;
        break;
      }
    }
    }
#endif

    if (hasdecl(m))
        assert(*m->decl); // FIXME
    assert(m->type);

    return m->type;
}

// Get or construct a derived type with kind `kind`, from a given type.
//
// Derived types are only present in the type table if they occur in the source
// code.  Trying to push them every time we see one is sufficient to keep this
// invariant.
static Type *derivedtype(Sema &sema, TypeKind kind, Type *type) {
    Name *name = name_of_derived_type(sema.name_table, kind, type->name);
    if (auto found = sema.type_table.find(name))
        return found->value;

    Type *derived = make_ref_type(sema, name, kind, type);
    return *sema.type_table.insert(name, derived);
}

Type *TypeChecker::visitUnaryExpr(UnaryExpr *u) {
    switch (u->kind) {
    case UnaryExprKind::paren: {
        u->type = visitParenExpr(static_cast<ParenExpr *>(u));
        break;
    }
    case UnaryExprKind::deref: {
        if (visitExpr(u->operand)) {
            if (!isreftype(u->operand->type)) {
                sema.error(u->operand->pos,
                           "dereference of a non-reference type '{}'",
                           u->operand->type->name->text);
                return nullptr;
            }
            u->type = u->operand->type->referee_type;

            // Also bind a temporary VarDecl to this expression that respects
            // the mutability of the reference type.  This way we know if this
            // lvalue is assignable or not.
            //
            // For example,
            //
            //     let v: var &int = ...
            //     *v = 3
            //
            // The '*v' here has to have a valid VarDecl with 'mut' as true.
            bool mut = (u->operand->type->kind == TypeKind::var_ref);
            u->var_decl = sema.make_node<VarDecl>(nullptr, u->type, mut);
            // Temporary VarDecls are _not_ pushed to the scoped decl table,
            // because they are not meant to be accessed later from a different
            // position in the source. In the same sense, they don't have a name
            // that can be used to query them.
        }
        break;
    }
    case UnaryExprKind::var_ref:
    case UnaryExprKind::ref: {
        if (visitExpr(u->operand)) {
            // Prohibit taking address of an rvalue.
            if (!islvalue(u->operand)) {
                sema.error(u->pos, "cannot take address of an rvalue");
                return nullptr;
            }

            // Prohibit borrowing an immutable value as mutable.
            if (u->kind == UnaryExprKind::var_ref) {
                auto operand_vardecl = lvaluedecl(u->operand);
                if (!operand_vardecl->mut) {
                    sema.error(
                        u->pos,
                        "cannot borrow '{}' as mutable because it is declared "
                        "immutable",
                        operand_vardecl->name->text);
                    return nullptr;
                }
            }

            auto type_kind = (u->kind == UnaryExprKind::var_ref)
                                 ? TypeKind::var_ref
                                 : TypeKind::ref;
            u->type = derivedtype(sema, type_kind, u->operand->type);
        }
        break;
    }
    default: {
        unreachable();
        break;
    }
    }

    return u->type;
}

Type *TypeChecker::visitParenExpr(ParenExpr *p) {
    walk_paren_expr(*this, p);
    // Passes along nullptr as well.
    return p->type = p->operand->type;
}

Type *TypeChecker::visitBinaryExpr(BinaryExpr *b) {
    walk_binary_expr(*this, b);

    if (!b->lhs->type || !b->rhs->type)
        return nullptr;

    if (b->lhs->type != b->rhs->type) {
        sema.error(b->pos,
                   "incompatible types to binary expression ('{}' and '{}')",
                   b->lhs->type->name->text, b->rhs->type->name->text);
        return nullptr;
    }

    b->type = b->lhs->type;
    return b->type;
}

// Type checking TypeExpr is about tagging the TypeExpr with the Type object
// whose syntactic representation matches the expression.
Type *TypeChecker::visitTypeExpr(TypeExpr *t) {
    walk_type_expr(*this, t);

    if (t->kind == TypeKind::value) {
        // t->decl should be non-null after the name binding stage.
        // And since we are currently doing single-pass, its type should also be
        // resolved by now.
        t->type = *t->decl->typemaybe();
        assert(t->type &&
               "type not resolved after visiting corresponding *Decl");
    } else if (t->kind == TypeKind::ref || t->kind == TypeKind::var_ref ||
               t->kind == TypeKind::ptr) {
        t->type = derivedtype(sema, t->kind, t->subexpr->type);
    } else {
        assert(!"unreachable");
    }

    return t->type;
}

Type *TypeChecker::visitVarDecl(VarDecl *v) {
    walk_var_decl(*this, v);

    // The 'type's on the RHS below _may_ be nullptr, for cases such as RHS
    // being StructDefExpr whose designator failed to typecheck its assignment.
    // The code below passes along the nullptr for those cases.
    if (v->type_expr) {
        v->type = v->type_expr->type;
    } else if (v->assign_expr) {
        // Copyability check.
        // FIXME: copy-paste from visitAssignStmt
        if (hasdecl(v->assign_expr) && v->assign_expr->type &&
            !v->assign_expr->type->copyable) {
            sema.error(v->assign_expr->pos,
                       "cannot copy non-copyable type '{}'",
                       v->assign_expr->type->name->text);
            return nullptr;
        }

        v->type = v->assign_expr->type;
    } else {
        unreachable();
    }

    // Populate children decls for structs.
    if (v->type && isstructtype(v->type)) {
        assert(v->children.empty());
        for (VarDecl *fdesc : v->type->getStructDecl()->fields) {
            addfield(sema, v, fdesc->name, fdesc->type);
        }
    }

    return v->type;
}

Type *TypeChecker::visitFuncDecl(FuncDecl *f) {
    // We need to do return type typecheck before walking the body, so we can't
    // use walk_func_decl() here.

    if (f->rettypeexpr)
        visitExpr(f->rettypeexpr);
    for (auto arg : f->args)
        visitDecl(arg);

    if (f->rettypeexpr) {
        // XXX: confusing flow
        if (!f->rettypeexpr->type)
            return nullptr;
        f->rettype = f->rettypeexpr->type;
    } else {
        f->rettype = sema.context.void_type;
    }

    // FIXME: what about type_table?
    if (f->body) {
        sema.context.func_decl_stack.push_back(f);
        visitCompoundStmt(f->body);
        sema.context.func_decl_stack.pop_back();
    }

    // FIXME: necessary?
    return f->rettype;
}

Type *TypeChecker::visitStructDecl(StructDecl *s) {
    s->type = make_value_type(sema, s->name, s);

    // Do pre-order walk so that recursive struct definitions are legal.
    walk_struct_decl(*this, s);

    for (auto field : s->fields) {
        // Containing one or more non-copyable field makes the whole struct a
        // non-copyable type. For instance, a struct that contains a mutable
        // reference as one of its field will be disallowed to be copy-assigned.
        if (field->type && !field->type->copyable)
            s->type->copyable = false;
    }

    return s->type;
}

Type *TypeChecker::visitEnumVariantDecl(EnumVariantDecl *v) {
    // Create a new type for this struct.
    auto type = make_value_type(sema, v->name, v);
    v->type = type;

    // Do pre-order walk so that recursive struct definitions are legal.
    walk_enum_variant_decl(*this, v);

    return v->type;
}

Type *TypeChecker::visitEnumDecl(EnumDecl *e) {
    auto type = make_value_type(sema, e->name, e);
    e->type = type;

    // Do pre-order walk so that recursive enum definitions are legal.
    walk_enum_decl(*this, e);

    return e->type;
}

BasicBlock *ReturnChecker::visitStmt(Stmt *s, BasicBlock *bb) {
    if (s->kind == StmtKind::if_) {
        return visitIfStmt(s->as<IfStmt>(), bb);
    } else {
        // "Plain" statements that go into a single basic block.
        bb->stmts.push_back(s);
        return bb;
    }
}

BasicBlock *ReturnChecker::visitCompoundStmt(CompoundStmt *cs, BasicBlock *bb) {
    for (auto s : cs->stmts)
        bb = visitStmt(s, bb);
    return bb;
}

// TODO: Currently, all if-else statements create a new empty basic block as
// its exit point.  If we add a new argument to the visitors so that they can
// know which exit point the branches should link to (and create a new one if
// passed a nullptr), we could decrease the number of redundant empty blocks.
BasicBlock *ReturnChecker::visitIfStmt(IfStmt *is, BasicBlock *bb) {
    // An empty basic block that the statements in the if body will be appending
    // themselves on.
    auto if_branch_start = sema.makeBasicBlock();
    bb->succ.push_back(if_branch_start);
    if_branch_start->pred.push_back(bb);
    auto if_branch_end = visitCompoundStmt(is->if_body, if_branch_start);

    auto else_branch_end = bb;
    if (is->else_if) {
        // We could make a new empty basic block here, which would make this CFG
        // a binary graph; or just pass in 'bb', which will make 'bb' have more
        // than two successors.
        else_branch_end = visitIfStmt(is->else_if, bb);
    } else if (is->else_body) {
        auto else_branch_start = sema.makeBasicBlock();
        bb->succ.push_back(else_branch_start);
        else_branch_start->pred.push_back(bb);
        else_branch_end = visitCompoundStmt(is->else_body, else_branch_start);
    }

    auto exit_point = sema.makeBasicBlock();
    if_branch_end->succ.push_back(exit_point);
    else_branch_end->succ.push_back(exit_point);
    exit_point->pred.push_back(if_branch_end);
    exit_point->pred.push_back(else_branch_end);

    return exit_point;
}

// Do the iterative solution for the dataflow analysis.
static void returncheck_solve(const std::vector<BasicBlock *> &walklist) {
    for (auto bb : walklist) {
        bb->returned_so_far = false;
    }

    bool changed = true;
    while (changed) {
        changed = false;

        for (auto bbI = walklist.rbegin(); bbI != walklist.rend(); bbI++) {
            auto bb = *bbI;

            bool all_pred_returns = false;
            if (!bb->pred.empty()) {
                all_pred_returns = true;
                for (auto pbb : bb->pred) {
                    all_pred_returns &= pbb->returned_so_far;
                }
            }

            auto t = bb->returns() || all_pred_returns;
            if (t != bb->returned_so_far) {
                changed = true;
                bb->returned_so_far = t;
            }
        }
    }
}

BasicBlock *ReturnChecker::visitFuncDecl(FuncDecl *f, BasicBlock *bb) {
    if (!f->rettypeexpr)
        return nullptr;
    // For body-less function declarations (e.g. extern).
    if (!f->body)
        return nullptr;

    auto entrypoint = sema.makeBasicBlock();
    auto exitpoint = visitCompoundStmt(f->body, entrypoint);

    std::vector<BasicBlock *> walklist;
    entrypoint->enumerate_postorder(walklist);

    // for (auto bb : walkList)
    //   fmt::print("BasicBlock: {} stmts\n", bb->stmts.size());

    returncheck_solve(walklist);

    if (!exitpoint->returned_so_far)
        sema.error(f->pos, "function not guaranteed to return a value");

    return nullptr;
}

// Checks if contains a return statement.
bool BasicBlock::returns() const {
    for (auto stmt : stmts) {
        if (stmt->kind == StmtKind::return_) {
            return true;
        }
    }
    return false;
}

void BasicBlock::enumerate_postorder(std::vector<BasicBlock *> &walklist) {
    if (walked)
        return;

    for (auto s : succ) {
        s->enumerate_postorder(walklist);
    }

    // post-order traversal
    walked = true;
    walklist.push_back(this);
}

static void codegenDecl(QbeGenerator &q, Decl *d);

static void codegenExpr(QbeGenerator &q, Expr *e) {
    switch (e->kind) {
    case ExprKind::integer_literal:
        q.emitIndent("%_{} = add 0, {}\n", q.valstack.next_id,
                     static_cast<IntegerLiteral *>(e)->value);
        q.valstack.push();
        break;
    default:
        assert(!"unknown expr kind");
    }
}

static void codegenStmt(QbeGenerator &q, Stmt *s) {
    switch (s->kind) {
    case StmtKind::decl:
        codegenDecl(q, static_cast<DeclStmt *>(s)->decl);
        break;
    case StmtKind::assign: {
        auto as = static_cast<AssignStmt *>(s);
        codegenExpr(q, as->rhs);
        // FIXME: hack, handle non-single-token LHS expr
        assert(as->lhs->kind == ExprKind::decl_ref);
        auto lhs_decl = static_cast<DeclRefExpr *>(as->lhs);
        q.emitIndent("%{} = add 0, %_{}\n", lhs_decl->name->text, q.valstack.pop());
        break;
    }
    default:
        assert(!"unknown stmt kind");
    }
}

static void codegenDecl(QbeGenerator &q, Decl *d) {
    switch (d->kind) {
    case DeclKind::var: {
        auto v = static_cast<VarDecl *>(d);
        codegenExpr(q, v->assign_expr);
        q.emitIndent("%{} = add 0, %_{}\n", v->name->text, q.valstack.pop());
        break;
    }
    case DeclKind::func:
        for (auto body_stmt : static_cast<FuncDecl *>(d)->body->stmts) {
            codegenStmt(q, body_stmt);
        }
        break;
    default:
        assert(!"unknown decl kind");
    }
}

void cmp::codegen(QbeGenerator &q, AstNode *n) {
    switch (n->kind) {
    case AstKind::file: {
        q.emit("export function w $main() {{\n");
        q.emit("@start\n");
        QbeGenerator::IndentBlock ib{q};
        for (auto toplevel : static_cast<File *>(n)->toplevels) {
            codegen(q, toplevel);
        }
        q.emit("}}\n");
        break;
    }
    case AstKind::stmt:
        codegenStmt(q, static_cast<Stmt *>(n));
        break;
    case AstKind::decl:
        codegenDecl(q, static_cast<Decl *>(n));
        break;
    default:
        assert(!"unknown ast kind");
    }
}
