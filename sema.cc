#include "sema.h"
#include "ast.h"
#include "ast_visitor.h"
#include "parser.h"
#include "source.h"
#include "types.h"
#include <cassert>
#include <cstdarg>

#define BUFSIZE 1024

using namespace cmp;

void Sema::error(size_t pos, const char *fmt, ...) {
    char buf[BUFSIZE];
    memset(buf, 0, BUFSIZE);

    va_list args;
    va_start(args, fmt);
    int wlen = vsnprintf(buf, BUFSIZE, fmt, args);
    va_end(args);

    if (wlen >= BUFSIZE) assert(false && "snprintf overflow");

    Error e{source.locate(pos), std::string{buf}};
    errors.push_back(e);
}

Type::Type(Name *n, bool mut, Type *bt) : name(n), referee_type(bt) {
  kind = mut ? TypeKind::var_ref : TypeKind::ref;
  copyable = !mut;
}

bool Type::isBuiltinType(Sema &sema) const {
  return this == sema.context.int_type
      || this == sema.context.char_type
      || this == sema.context.void_type
      || this == sema.context.string_type;
}

bool Type::isStruct() const {
  // TODO: should base_type be null too?
  return kind == TypeKind::value && type_decl && type_decl->is<StructDecl>();
}

bool Type::isEnum() const {
  // TODO: should base_type be null too?
  return kind == TypeKind::value && type_decl &&
    type_decl->is<EnumDecl>();
}

StructDecl *Type::getStructDecl() {
  assert(isStruct());
  return type_decl->as<StructDecl>();
}

EnumDecl *Type::getEnumDecl() {
  assert(isEnum());
  return type_decl->as<EnumDecl>();
}

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

Type *makeReferenceType(Sema &sema, Name *n, bool mut, Type *referee_type) {
  Type *t = new Type(n, mut, referee_type);
  sema.type_pool.push_back(t);
  return t;
}

Type *push_builtin_type_from_name(Sema &s, const std::string &str) {
  Name *name = s.name_table.get_or_add(str);
  auto struct_decl = s.make_node<StructDecl>(
      name, std::vector<VarDecl *>() /* FIXME */);
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
  for (auto b : basic_block_pool) {
    delete b;
  }
}

void Sema::scope_open() {
  decl_table.scope_open();
  type_table.scope_open();
  live_list.scope_open();
  borrow_table.scope_open();
}

void Sema::scope_close() {
  decl_table.scope_close();
  type_table.scope_close();
  live_list.scope_close();
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
    sema.error(d->pos, "use of undeclared identifier '%s'", d->name->str());
    return;
  }
  d->decl = sym->value;
}

void NameBinding::visitFuncCallExpr(FuncCallExpr *f) {
  auto sym = sema.decl_table.find(f->func_name);
  if (!sym) {
    sema.error(f->pos, "undeclared function '%s'", f->func_name->str());
    return;
  }
  if (!sym->value->is<FuncDecl>()) {
    sema.error(f->pos, "'%s' is not a function", f->func_name->str());
    return;
  }
  f->func_decl = sym->value->as<FuncDecl>();
  assert(f->func_decl);

  walk_func_call_expr(*this, f);

  // argument count match check
  if (f->func_decl->args_count() != f->args.size()) {
    sema.error(f->pos, "'%s' accepts %lu arguments, got %lu",
               f->func_name->str(), f->func_decl->args_count(), f->args.size());
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
  if (t->subexpr) return;

  auto sym = sema.decl_table.find(t->name);
  if (sym && sym->value->type()) {
    assert(t->kind == TypeExprKind::value);
    t->decl = sym->value;
  } else {
    sema.error(t->pos, "use of undeclared type '%s'", t->name->str());
    return;
  }
}


namespace {

// Semantically declare a 'name' at 'pos', whose Decl type is T.
// Returns true if success and otherwise does error handling.
template <typename T>
bool declare(Sema &sema, size_t pos, Name *name, T *decl) {
  auto found = sema.decl_table.find(name);
  if (found && found->value->is<T>() &&
      found->scope_level == sema.decl_table.curr_scope_level) {
    sema.error(pos, "redefinition of '%s'", name->str());
    return false;
  }

  // Creates the binding between the decl and the name.
  sema.decl_table.insert(name, decl);

  return true;
}

} // namespace

void NameBinding::visitVarDecl(VarDecl *v) {
  walk_var_decl(*this, v);

  if (!declare<VarDecl>(sema, v->pos, v->name, v)) return;
}

void NameBinding::visitFuncDecl(FuncDecl *f) {
  auto err_count = sema.errors.size();

  if (!declare<FuncDecl>(sema, f->pos, f->name, f)) return;

  // scope for argument variables
  sema.decl_table.scope_open();
  sema.context.func_decl_stack.push_back(f);

  walk_func_decl(*this, f);

  sema.context.func_decl_stack.pop_back();
  sema.decl_table.scope_close();

  if (sema.errors.size() != err_count) {
    f->failed = true;
  }
}

void NameBinding::visitStructDecl(StructDecl *s) {
  if (!declare<StructDecl>(sema, s->pos, s->name, s)) return;

  // Decl table is used for checking redefinition when parsing the member
  // list.
  sema.decl_table.scope_open();
  walk_struct_decl(*this, s);
  sema.decl_table.scope_close();
}

namespace {

#if 0
// Generate a name for the anonymous fields in each enum variant structs.
// For now, these are named "_0", "_1", and so on.
Name *genAnonymousFieldName(Sema &sema, size_t index) {
  char buf[BUFSIZE];
  snprintf(buf, BUFSIZE, "_%lu", index);
  return sema.name_table.get_or_add(std::string{buf});
}
#endif

// Checks if 'expr' is a borrowing expression.
bool isReferenceExpr(const Expr *expr) {
  return expr->kind == ExprKind::unary &&
         (expr->as<UnaryExpr>()->kind == UnaryExprKind::ref ||
          expr->as<UnaryExpr>()->kind == UnaryExprKind::var_ref);
}

// Checks if 'expr' is a dereferencing expression, i.e.. '*expr'.
bool isDereferenceExpr(const Expr *expr) {
  return expr->kind == ExprKind::unary &&
         expr->as<UnaryExpr>()->kind == UnaryExprKind::deref;
}

} // namespace

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
  if (!declare<EnumDecl>(sema, e->pos, e->name, e)) return;

  sema.decl_table.scope_open();
  sema.context.enum_decl_stack.push_back(e);

  walk_enum_decl(*this, e);

  sema.context.enum_decl_stack.pop_back();
  sema.decl_table.scope_close();
}

namespace {

VarDecl *makeAnonymousVarDecl(Sema &sema, Type *type, bool mut) {
  auto v = sema.make_node<VarDecl>(nullptr, type, mut);
  return v;
}

bool mutabilityCheckAssignment(Sema &sema, const Expr *lhs) {
  if (lhs->kind == ExprKind::member) {
    // For MemberExprs, assignability depends on that of its struct side.
    return mutabilityCheckAssignment(sema, lhs->as<MemberExpr>()->struct_expr);
  } else if (isDereferenceExpr(lhs)) {
    auto unary = lhs->as<UnaryExpr>();
    if (unary->operand->type->kind != TypeKind::var_ref) {
      sema.error(unary->pos, "'%s' is not a mutable reference",
                 unary->operand->getLValueDecl()->name->str());
      return false;
    }
  } else {
    auto var_decl = lhs->getLValueDecl();
    if (var_decl && !var_decl->mut) {
      sema.error(lhs->pos, "'%s' is not declared as mutable",
                 var_decl->name->str());
      return false;
    }
  }

  return true;
}

// Typecheck assignment statement of 'lhs = rhs'.
bool typeCheckAssignment(const Type *lhs, const Type *rhs) {
  // TODO: Typecheck assignment rules so far:
  //
  // 1. Reference <- mutable reference.
  // 2. Exact same match.

  // Allow promotion from mutable to immutable reference.
  if (lhs->kind == TypeKind::ref && rhs->isReference()) {
    // TODO: 'unification'? Ref:
    // http://smallcultfollowing.com/babysteps/blog/2017/03/25/unification-in-chalk-part-1/
    return typeCheckAssignment(lhs->referee_type, rhs->referee_type);
  }
  return lhs == rhs;
}

} // namespace

// Assignments should check that the LHS is an l-value.
// This check cannot be done reliably in the parsing stage because it depends
// on the actual type of the expression, not just its kind; e.g. (v) or (3).
//
//                 3 = 4
Type *TypeChecker::visitAssignStmt(AssignStmt *as) {
  walk_assign_stmt(*this, as);

  auto lhs_ty = as->lhs->type;
  auto rhs_ty = as->rhs->type;

  // XXX: is this the best way to early-exit?
  if (!lhs_ty || !rhs_ty) return nullptr;

  // L-value check.
  if (!as->lhs->isLValue()) {
    sema.error(as->pos, "cannot assign to an rvalue");
    return nullptr;
  }

  // Type compatibility check.
  if (!typeCheckAssignment(lhs_ty, rhs_ty)) {
    sema.error(as->pos, "cannot assign '%s' type to '%s'", rhs_ty->name->str(),
               lhs_ty->name->str());
    return nullptr;
  }

  // Mutability check.
  //
  // Type compatibility check precedes this, because a type mismatch on an
  // assignment is likely to signify a larger error in the source code than a
  // mutability error (which can mostly be fixed with a single keyword change).
  if (!mutabilityCheckAssignment(sema, as->lhs)) return nullptr;

  // Copyability check.
  //
  // Even if RHS has a non-copyable type, if it is a temporary value, its copy
  // becomes essentially the same as move and thus is allowed.
  // For example, with 'S' being a non-copyable type, the following is legal:
  //
  //     let s1 = S {...};
  //
  // TODO: there's a copy-paste of this code somewhere else.
  if (as->rhs->declMaybe() && !rhs_ty->copyable) {
    sema.error(as->rhs->pos, "cannot copy non-copyable type '%s'",
               rhs_ty->name->str());
    return nullptr;
  }

  return lhs_ty;
}

Type *TypeChecker::visitReturnStmt(ReturnStmt *rs) {
  visitExpr(rs->expr);
  if (!rs->expr->type) return nullptr;

  assert(!sema.context.func_decl_stack.empty());
  auto func_decl = sema.context.func_decl_stack.back();
  if (func_decl->isVoid(sema)) {
    sema.error(rs->expr->pos, "function '%s' should not return a value",
               func_decl->name->str());
    return nullptr;
  }

  if (!typeCheckAssignment(func_decl->ret_type, rs->expr->type)) {
    sema.error(rs->expr->pos,
               "return type mismatch: function returns '%s', but got '%s'",
               func_decl->ret_type->name->str(), rs->expr->type->name->str());
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
  // For variables, since there is no type inference now, the type is determined
  // at the same time the variable is declared. So if a variable succeeded
  // namebinding, its type is guaranteed to be determined.
  //
  // For struct and enum names, they are not handled in the namebinding stage
  // and so should be taken care of here.
  auto opt_type = d->decl->type();
  assert(
      opt_type.has_value() &&
      "tried to typecheck a non-typed DeclRef (first-class functions TODO?)");
  d->type = *opt_type;
  return d->type;
}

Type *TypeChecker::visitFuncCallExpr(FuncCallExpr *f) {
  walk_func_call_expr(*this, f);

  assert(f->func_decl->ret_type);
  f->type = f->func_decl->ret_type;

  // check argument type match
  for (size_t i = 0; i < f->func_decl->args.size(); i++) {
    if (!f->args[i]->type) return nullptr;

    // TODO: proper type comparison
    if (f->args[i]->type != f->func_decl->args[i]->type) {
      sema.error(f->args[i]->pos,
                 "argument type mismatch: expects '%s', got '%s'",
                 f->func_decl->args[i]->type->name->str(),
                 f->args[i]->type->name->str());
      return nullptr;
    }
  }

  return f->type;
}

namespace {

// TODO: This should return something like a FieldDecl, not a VarDecl.
VarDecl *findFieldInStruct(Name *field_name, Type *struct_type) {
  VarDecl *found = nullptr;
  for (auto mem_decl : struct_type->getStructDecl()->fields) {
    if (field_name == mem_decl->name) {
      found = mem_decl;
      break;
    }
  }
  return found;
}

} // namespace

Type *TypeChecker::visitStructDefExpr(StructDefExpr *s) {
  walk_struct_def_expr(*this, s);

  // check Name is a struct
  auto lhs_type = s->name_expr->type;
  if (!lhs_type) return nullptr;
  if (!lhs_type->isStruct()) {
    sema.error(s->name_expr->pos, "type '%s' is not a struct",
               lhs_type->name->str());
    return nullptr;
  }

  for (auto &desig : s->desigs) {
    if (!desig.init_expr->type) return nullptr;

    VarDecl *field_decl = findFieldInStruct(desig.name, lhs_type);
    if (!field_decl) {
      sema.error(desig.init_expr->pos, // FIXME: wrong pos
                 "'%s' is not a member of '%s'", desig.name->str(),
                 lhs_type->getStructDecl()->name->str());
      return nullptr;
    }

    if (!typeCheckAssignment(field_decl->type, desig.init_expr->type)) {
      sema.error(desig.init_expr->pos, "cannot assign '%s' type to '%s'",
                 desig.init_expr->type->name->str(), field_decl->type->name->str());
      return nullptr;
    }
  }

  s->type = lhs_type;
  return s->type;
}

// MemberExprs cannot be namebinded completely without type checking (e.g.
// func().mem).  So we defer their namebinding to the type checking phase,
// which is done here.
Type *TypeChecker::visitMemberExpr(MemberExpr *m) {
  // propagate typecheck from left to right (struct -> .mem)
  walk_member_expr(*this, m);

  // if the struct side failed to typecheck, we cannot proceed
  if (!m->struct_expr->type) return nullptr;

  // make sure the LHS is actually a struct
  auto struct_type = m->struct_expr->type;
  if (!struct_type->isMemberAccessible()) {
    // FIXME: isMemberAccessible <-> struct
    sema.error(m->struct_expr->pos, "type '%s' is not a struct",
               struct_type->name->str());
    return nullptr;
  }

  if (struct_type->isStruct()) {
    // First of all, make sure that this field name indeed exists in the
    // struct's type definition.
    VarDecl *field_decl = findFieldInStruct(m->member_name, struct_type);
    if (!field_decl) {
      // TODO: pos for member
      sema.error(m->struct_expr->pos, "'%s' is not a member of '%s'",
                 m->member_name->str(), struct_type->name->str());
      return nullptr;
    }

    // Type inferrence.
    m->type = field_decl->type;

    // If struct_expr is an lvalue, this MemberExpr should also be an lvalue
    // and have a Decl object. We do so by inheriting from one of struct_expr's
    // child VarDecls.
    //
    // We need to create a new VarDecl here for each different VarDecl of lhs,
    // because even if with the same struct type and field name, MemberExprs may
    // be semantically different objects if their LHS are of different
    // declarations.  For example, 'x.a' and 'y.a' in the following occupy two
    // different physical memory space and thus need to be associated to two
    // different Decl objects:
    //
    //    let x = S {.a = ...}
    //    let y = S {.a = ...}
    //    x.a
    //    y.a
    //
    if (m->struct_expr->isLValue()) {
      auto struct_var_decl = m->struct_expr->getLValueDecl();
      for (auto field : struct_var_decl->children) {
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
      // As a result of this process, we achieve space savings, because only the
      // members that are actually used in the source code are instantiated.
      if (!m->decl) {
        // These VarDecls do not *need* to have a name other than for error
        // reporting purposes, because they are not going to be accessed by
        // their name, but through the VarDecl of their parent struct value.
        // Their mutability is inherited from the parent value.
        auto field_var_decl = sema.make_node<VarDecl>(
            m->member_name, field_decl->type, struct_var_decl->mut);
        m->decl = {field_var_decl};

        assert(sema.live_list.insert(field_var_decl, field_var_decl));
        struct_var_decl->addChild(m->member_name, field_var_decl);
      }
    }
  }

  if (struct_type->isEnum()) {
    assert(false && "not yet");
#if 0
    for (auto mem_decl : struct_type->getEnumDecl()->variants) {
      if (m->member_name == mem_decl->name) {
        m->decl = mem_decl;
        found = true;
        break;
      }
    }
#endif
  }

  if (m->decl.has_value()) assert(*m->decl);
  assert(m->type);
  return m->type;
}

// Get a reference type of a given type, or construct one if it didn't exist in
// the type table.
Type *cmp::getReferenceType(Sema &sema, bool mut, Type *type) {
  auto name = getReferenceTypeName(sema.name_table, mut, type->name);
  if (auto found = sema.type_table.find(name)) return found->value;

  // FIXME: scope_level
  auto ref_type = makeReferenceType(sema, name, mut, type);
  return *sema.type_table.insert(name, ref_type);
}

Type *TypeChecker::visitUnaryExpr(UnaryExpr *u) {
  switch (u->kind) {
  case UnaryExprKind::paren: {
    u->type = visitParenExpr(static_cast<ParenExpr *>(u));
    break;
  }
  case UnaryExprKind::deref: {
    if (visitExpr(u->operand)) {
      if (!u->operand->type->isReference()) {
        sema.error(u->operand->pos, "dereference of a non-reference type '%s'",
                   u->operand->type->name->str());
        return nullptr;
      }
      u->type = u->operand->type->referee_type;

      // Also bind a temporary VarDecl to this expression that respects the
      // mutability of the reference type.  This way we know if this l-value is
      // assignable or not.
      //
      // For example,
      //
      //     let v: var &int = ...
      //     *v = 3
      //
      // The '*v' here has to have a valid VarDecl with 'mut' as true.
      bool mut = (u->operand->type->kind == TypeKind::var_ref);
      u->var_decl = makeAnonymousVarDecl(sema, u->type, mut);
      // Temporary VarDecls are _not_ pushed to the scoped decl table, because
      // they are not meant to be accessed later from a different position in
      // the source. In the same sense, they don't have a name that can be used
      // to query them.
      // TODO is this right?
    }
    break;
  }
  case UnaryExprKind::var_ref:
  case UnaryExprKind::ref: {
    if (visitExpr(u->operand)) {
      bool mut = false;
      if (u->kind == UnaryExprKind::var_ref) mut = true;

      // Prohibit taking address of an rvalue.
      if (!u->operand->isLValue()) {
        sema.error(u->pos, "cannot take address of an rvalue");
        return nullptr;
      }

      // Prohibit borrowing an immutable value as mutable.
      if (u->kind == UnaryExprKind::var_ref) {
        auto operand_vardecl = u->operand->getLValueDecl();
        if (!operand_vardecl->mut) {
          sema.error(u->pos,
                     "cannot borrow '%s' as mutable because it is declared "
                     "immutable",
                     operand_vardecl->name->str());
          return nullptr;
        }
      }

      u->type = getReferenceType(sema, mut, u->operand->type);
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
               "incompatible types to binary expression ('%s' and '%s')",
               b->lhs->type->name->str(), b->rhs->type->name->str());
    return nullptr;
  }

  b->type = b->lhs->type;
  return b->type;
}

// Type checking TypeExpr concerns with finding the Type object whose syntactic
// representation matches the TypeExpr.
Type *TypeChecker::visitTypeExpr(TypeExpr *t) {
  walk_type_expr(*this, t);

  if (t->kind == TypeExprKind::value) {
    // t->decl should be non-null after the name binding stage.
    // And since we are currently doing single-pass, its type should also be
    // resolved by now.
    t->type = *t->decl->type();
    assert(t->type && "type not resolved in corresponding *Decl");
  } else if (t->kind == TypeExprKind::ref || t->kind == TypeExprKind::var_ref) {
    // Derived types are only present in the type table if they occur in the
    // source code.  Trying to push them every time we see one is sufficient
    // to keep this invariant.
    assert(t->subexpr->type); // TODO: check if triggers
    bool mut = t->kind == TypeExprKind::var_ref;
    t->type = ::getReferenceType(sema, mut, t->subexpr->type);
  } else {
    unreachable();
  }

  return t->type;
}

Type *TypeChecker::visitVarDecl(VarDecl *v) {
  walk_var_decl(*this, v);

  // The 'type's on the RHS below _may_ be nullptr, for cases such as RHS being
  // StructDefExpr whose designator failed to typecheck its assignment.  The
  // code below passes along the nullptr for those cases.
  if (v->type_expr) {
    v->type = v->type_expr->type;
  } else if (v->assign_expr) {
    // Copyability check.
    // FIXME: copy-paste from visitAssignStmt
    if (v->assign_expr->declMaybe() && v->assign_expr->type &&
        !v->assign_expr->type->copyable) {
      sema.error(v->assign_expr->pos, "cannot copy non-copyable type '%s'",
                 v->assign_expr->type->name->str());
      return nullptr;
    }

    v->type = v->assign_expr->type;
  } else {
    unreachable();
  }

  // Populate children decls for structs.
  if (v->type && v->type->isStruct()) {
    assert(v->children.empty());
    for (auto field : v->type->getStructDecl()->fields) {
      auto field_var_decl =
          sema.make_node<VarDecl>(field->name, field->type, v->mut);
      v->addChild(field->name, field_var_decl);
    }
  }

  return v->type;
}

Type *TypeChecker::visitFuncDecl(FuncDecl *f) {
  if (f->failed) return nullptr;
  auto err_count = sema.errors.size();

  // We need to do return type typecheck before walking the body, so we can't
  // use walk_func_decl() here.

  if (f->ret_type_expr) visitExpr(f->ret_type_expr);
  for (auto arg : f->args)
    visitDecl(arg);

  if (f->ret_type_expr) {
    // XXX: confusing flow
    if (!f->ret_type_expr->type) return nullptr;
    f->ret_type = f->ret_type_expr->type;
  } else {
    f->ret_type = sema.context.void_type;
  }

  // FIXME: what about type_table?
  sema.context.func_decl_stack.push_back(f);
  visitCompoundStmt(f->body);
  sema.context.func_decl_stack.pop_back();

  if (sema.errors.size() != err_count) {
    f->failed = true;
  }

  // FIXME: necessary?
  return f->ret_type;
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
    // We could make a new empty basic block here, which would make this CFG a
    // binary graph; or just pass in 'bb', which will make 'bb' have more than
    // two successors.
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

namespace {

// Do the iterative solution for the dataflow analysis.
void returnCheckSolve(const std::vector<BasicBlock *> &walklist) {
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

} // namespace

BasicBlock *ReturnChecker::visitFuncDecl(FuncDecl *f, BasicBlock *bb) {
  if (f->failed) return nullptr;
  if (!f->ret_type_expr) return nullptr;

  auto entry_point = sema.makeBasicBlock();
  auto exit_point = visitCompoundStmt(f->body, entry_point);

  std::vector<BasicBlock *> walkList;
  entry_point->enumerate_post_order(walkList);

  // for (auto bb : walkList)
  //   fmt::print("BasicBlock: {} stmts\n", bb->stmts.size());

  returnCheckSolve(walkList);

  if (!exit_point->returned_so_far)
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

void BasicBlock::enumerate_post_order(std::vector<BasicBlock *> &walklist) {
    if (walked) return;

    for (auto s : succ) {
        s->enumerate_post_order(walklist);
    }

    // post-order traversal
    walked = true;
    walklist.push_back(this);
}

void BorrowChecker::visitCompoundStmt(CompoundStmt *cs) {
    sema.scope_open();
    walk_compound_stmt(*this, cs);
    sema.scope_close();
}

// Mark a variable that it is borrowed in the current scope.
//
// Possible borrowing occasions:
// - let x = &a
// - x = &a
// - x = S {.m = &a}
// - f(&a)
void registerBorrow(Sema &sema, VarDecl *borrowee, size_t borrowee_pos) {
  int old_borrow_count = 0;
  auto found = sema.borrow_table.find(borrowee);
  if (found) {
    old_borrow_count = found->value.borrow_count;
  }

  if (old_borrow_count > 0) {
    sema.error(borrowee_pos, "cannot borrow '%s' more than once",
               borrowee->name->str());
    return;
  }

  sema.borrow_table.insert(borrowee, BorrowMap{borrowee, old_borrow_count + 1});
}

void borrowCheckAssignment(Sema &sema, VarDecl *v, const Expr *rhs, bool move) {
  // Pattern-match-like recursion case.
  // This works because we guarantee that every l-value has a VarDecl.
  if (rhs->kind == ExprKind::struct_def) {
    for (auto desig : rhs->as<StructDefExpr>()->desigs) {
      if (!isReferenceExpr(desig.init_expr)) continue;

      // Find child decl by name.
      VarDecl *child = nullptr;
      for (auto c : v->children) {
        if (c.first == desig.name) {
          child = c.second;
          break;
        }
      }

      borrowCheckAssignment(sema, child, desig.init_expr, move);
    }
  } else if (rhs->type->isReference()) {
    if (rhs->isLValue()) {
      // 'Implicit' copying of a borrow, e.g. 'ref1 = ref2'.
      //
      // TODO: What about 'ref1 = returns_ref()'?
      // Rewrite it to 'ref1 = { var temp = returns_ref() }' and do a move?
      if (move) {
        assert(false && "TODO: nullify reference in RHS");
      } else {
        v->borrowee = rhs->getLValueDecl()->borrowee;
      }
    } else if (isReferenceExpr(rhs)) {
      // Explicit borrowing statement, e.g. 'a = &b'.
      //
      // Note that a move assignment with an rvalue RHS is the same as a copy,
      // so both cases are treated in the same code below.
      //
      // TODO: check multiple borrowing rule
      // TODO: mutable and immutable
      auto operand = rhs->as<UnaryExpr>()->operand;
      if (operand->kind == ExprKind::member) {
        // For MemberExprs (e.g. v = &m.a), we are essentially borrowing the
        // whole struct, not just the member.
        //
        // FIXME: We gotta find the root parent, not a parent just one level
        // above.  Add a test case for this.
        v->borrowee = operand->getLValueDecl()->parent;
      } else {
        v->borrowee = operand->getLValueDecl();
      }
    }
    assert(v->borrowee && "borrowee still null");
  } else if (rhs->isLValue() && !rhs->type->isBuiltinType(sema)) {
    // Move of an LValue, e.g. 'a <- b'.
    //
    // TODO: Invalidate RHS here. Program must still run even without this
    // invalidation, because access to the moved out value is forbidden in the
    // semantic phase.

    // TODO: check built-in type
    rhs->getLValueDecl()->moved = true;
  }
}

void BorrowChecker::visitAssignStmt(AssignStmt *as) {
  walk_assign_stmt(*this, as);

  auto lhs_decl = as->lhs->getLValueDecl();
  borrowCheckAssignment(sema, lhs_decl, as->rhs, as->move);
}

void BorrowChecker::visitReturnStmt(ReturnStmt *rs) {
  // For every borrowing expressions in the return statement, we can check if
  // the Decl of the referee is present in the current function scope to find
  // lifetime errors.
  in_return_stmt = true;
  walk_return_stmt(*this, rs);
  in_return_stmt = false;
}

void BorrowChecker::visitExpr(Expr *e) {
  // Use of moved value check.
  // This is a pre-order step so that once a use-after-move error is detected,
  // the traversal stops.
  if (e->isLValue()) {
    if (e->getLValueDecl()->moved) {
      sema.error(e->pos, "use of moved value");
      return;
    }
  }

  AstVisitor<BorrowChecker, void>::visitExpr(e);

  // Return statement borrowck.
  //
  // At this point, other borrowck errors such as use-after-free would have been
  // caught in the calls originated from the above visitExpr().
  if (in_return_stmt && e->type->isReference()) {
    if (e->isLValue()) {
      auto sym = sema.live_list.find(e->getLValueDecl()->borrowee);
      // TODO right now
    } else if (isReferenceExpr(e)) {
    }
  }
}

// Rule: a variable of lifetime 'a should only refer to a variable whose
// lifetime is larger than 'a.
// In other words, at the point of use, the borrowee should be alive.
void BorrowChecker::visitDeclRefExpr(DeclRefExpr *d) {
  if (!d->decl->is<VarDecl>()) return;
  auto var = d->decl->as<VarDecl>();

  // at each use of a reference variable, check if its borrowee is alive
  if (var->borrowee) {
    auto sym = sema.live_list.find(var->borrowee);
    // TODO: refactor into find_exact()
    if (!(sym && sym->value == var->borrowee)) {
      sema.error(d->pos, "'%s' does not live long enough",
                 var->borrowee->name->str());
      return;
    }
  }
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
//
// Move vs Copy
// ============
//
// We are considering adding 'owning pointer' as a language-native type.  This
// type is essentially the same as Rust's Box<T> or C++'s std::unique_ptr<T>.
//
// The language has a stance on assignments which is the opposite to Rust's
// 'move by default'.  In Rust, all assignments are treated as move, unless the
// involving types implement 'Copy' trait. This makes transferring of ownership
// less visible, because it is not easy to see if the type at hand is copyable
// or not without looking up its declaration.
//
// In our language, we essentially distinguish moves from copies syntactically.
// We interpret all assignments as copying, but if the type of the value being
// copied contains a non-copyable type (e.g. an owning pointer or a mutable
// reference), we disallow the copy.  Instead, only move assignments are
// allowed on those types, which have a distinct syntax from the usual copy
// assignment.
//
// This design has several advantages: 1. It makes the transfer of ownership
// more explicitly visible, which is a good thing.  2. It makes the language
// feel less foreign to people coming from C or C++, as those languages engage
// the copy-by-default semantics too.
//
// Function calls
// ==============
//
// The language has a call-by-value semantics.  This means that every
// argument to a function is copied its value to the stack frame of the called
// function.  If we want to pass in a variable of a non-copyable type, we again
// need to use a separate syntax; maybe <-var. (TODO)
//
void BorrowChecker::visitFuncCallExpr(FuncCallExpr *f) {
    walk_func_call_expr(*this, f);
}

void BorrowChecker::visitStructDefExpr(StructDefExpr *s) {
  walk_struct_def_expr(*this, s);

  for (auto desig : s->desigs) {
    if (isReferenceExpr(desig.init_expr)) {
      // auto rhs_deref = desig.init_expr->as<UnaryExpr>()->operand;
      // TODO: desig.decl->borrowee = rhs_deref->getLValueDecl();
    }
  }
}

void BorrowChecker::visitUnaryExpr(UnaryExpr *u) {
  switch (u->kind) {
  case UnaryExprKind::paren:
    visitParenExpr(static_cast<ParenExpr *>(u));
    break;
  case UnaryExprKind::ref:
  case UnaryExprKind::var_ref: // TODO
    visitExpr(u->operand);
    registerBorrow(sema, u->operand->getLValueDecl(), u->pos);
    break;
  case UnaryExprKind::deref:
    visitExpr(u->operand);
    break;
  default:
    assert(false);
    break;
  }
}

void BorrowChecker::visitVarDecl(VarDecl *v) {
  walk_var_decl(*this, v);

  sema.live_list.insert(v, v);
  for (auto child : v->children) {
    sema.live_list.insert(child.second, child.second);
  }

  if (v->assign_expr) {
    borrowCheckAssignment(
        sema, v, v->assign_expr,
        true /* because declarations with an init expr is always a move. */);
  }
}

void CodeGenerator::visitFile(File *f) {
  emit("#include <stdlib.h>\n");
  emit("#include <stdio.h>\n");
  emit("\n");

  walk_file(*this, f);
}

void CodeGenerator::visitIntegerLiteral(IntegerLiteral *i) {
  emitCont("{}", i->value);
}

void CodeGenerator::visitStringLiteral(StringLiteral *s) {
  emitCont("{}", s->value);
}

void CodeGenerator::visitDeclRefExpr(DeclRefExpr *d) {
  emitCont("{}", d->name->str());
}

void CodeGenerator::visitFuncCallExpr(FuncCallExpr *f) {
  emitCont("{}(", f->func_name->str());

  for (size_t i = 0; i < f->args.size(); i++) {
    visitExpr(f->args[i]);
    if (i != f->args.size() - 1)
      emitCont(", ");
  }

  emitCont(")");
}

void CodeGenerator::visitStructDefExpr(StructDefExpr *s) {
  emitCont("(");
  visitExpr(s->name_expr);
  emitCont(")");

  emitCont(" {{");
  for (const auto desig : s->desigs) {
    emitCont(".{} = ", desig.name->str());
    visitExpr(desig.init_expr);
    emitCont(", ");
  }
  emitCont("}}");
}

void CodeGenerator::visitMemberExpr(MemberExpr *m) {
  visitExpr(m->struct_expr);
  emitCont(".");
  emitCont("{}", m->member_name->str());
}

void CodeGenerator::visitUnaryExpr(UnaryExpr *u) {
  switch (u->kind) {
  case UnaryExprKind::paren:
    return visitParenExpr(u->as<ParenExpr>());
    break;
  case UnaryExprKind::ref:
  case UnaryExprKind::var_ref:
    emitCont("&");
    visitExpr(u->operand);
    break;
  case UnaryExprKind::deref:
    emitCont("*");
    visitExpr(u->operand);
    break;
  default:
    assert(false);
    break;
  }
}

void CodeGenerator::visitParenExpr(ParenExpr *p) {
  emitCont("(");
  visitExpr(p->operand);
  emitCont(")");
}

void CodeGenerator::visitBinaryExpr(BinaryExpr *b) {
  visitExpr(b->lhs);
  emitCont(" {} ", b->op.str());
  visitExpr(b->rhs);
}

// Generate C source representation of a Type.
std::string CodeGenerator::cStringify(const Type *t) {
    assert(t);

    if (t == sema.context.string_type) {
        // For now, strings are aliased to char *.  This works as long as
        // strings are immutable and doesn't contain unicode characters.
        return "char*";
    }
    if (t->kind == TypeKind::ref) {
        auto base = cStringify(t->referee_type);
        return base + "*";
    } else {
        return t->name->str();
    }
}

void CodeGenerator::visitExprStmt(ExprStmt *e) {
  emitIndent();
  visitExpr(e->expr);
  emitCont(";\n");
}

void CodeGenerator::visitAssignStmt(AssignStmt *a) {
  emitIndent();
  visitExpr(a->lhs);
  emitCont(" = ");
  visitExpr(a->rhs);
  emitCont(";\n");
}

void CodeGenerator::visitReturnStmt(ReturnStmt *r) {
  emit("return ");
  visitExpr(r->expr);
  emitCont(";\n");
}

void CodeGenerator::visitIfStmt(IfStmt *i) {
  emit("if (");
  visitExpr(i->cond);
  emitCont(") {{\n");
  {
    IndentBlock ib{*this};
    visitCompoundStmt(i->if_body);
  }
  emit("}}");

  if (i->else_body) {
    emitCont(" else {{\n");
    {
      IndentBlock ib{*this};
      visitCompoundStmt(i->else_body);
    }
    emit("}}\n");
  } else if (i->else_if) {
    emitCont(" else ");
    visitIfStmt(i->else_if);
  } else {
    emitCont("\n");
  }
}

void CodeGenerator::visitBuiltinStmt(BuiltinStmt *b) {
  // shed off the #
  b->text.remove_prefix(1);
  emit("{};\n", b->text);
}

void CodeGenerator::visitVarDecl(VarDecl *v) {
  if (v->kind == VarDeclKind::param) {
    emit("{} {}", cStringify(v->type), v->name->str());
  } else {
    emit("{} {};\n", cStringify(v->type), v->name->str());

    if (v->assign_expr) {
      emit("{} = ", v->name->str());
      visitExpr(v->assign_expr);
      emitCont(";\n");
    }
  }
}

void CodeGenerator::visitStructDecl(StructDecl *s) {
  emit("typedef struct {} {{\n", s->name->str());
  {
    IndentBlock ib{*this};
    for (auto memb : s->fields)
      visitDecl(memb);
  }
  emit("}} {};\n", s->name->str());
  emit("\n");
}

void CodeGenerator::visitFuncDecl(FuncDecl *f) {
  if (f->failed) return;

  if (f->ret_type_expr)
    emit("{}", cStringify(f->ret_type));
  else
    emit("void");

  emit(" {}(", f->name->str());
  if (f->args.empty()) {
    emitCont("void");
  } else {
    for (size_t i = 0; i < f->args.size(); i++) {
      visitDecl(f->args[i]);
      if (i != f->args.size() - 1)
        emitCont(", ");
    }
  }
  emitCont(") {{\n");

  {
    IndentBlock ib{*this};
    visitCompoundStmt(f->body);
  }

  emit("}}\n");
  emit("\n");
}
