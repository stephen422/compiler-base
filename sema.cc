#include "sema.h"
#include "ast.h"
#include "ast_visitor.h"
#include "fmt/core.h"
#include "parser.h"
#include "source.h"
#include "types.h"
#include <cassert>
#include <chrono>
#include <cstdarg>

#define BUFSIZE 1024

using namespace cmp;

template <typename... Args> void Sema::error(size_t pos, Args &&... args) {
  auto str = fmt::format(std::forward<Args>(args)...);
  Error e{source.locate(pos), str};
  errors.push_back(e);
}

Type::Type(Name *n, TypeKind k, Type *rt) : kind(k), name(n), referee_type(rt) {
  copyable = k == TypeKind::ref;
}

bool Type::isBuiltin(Sema &sema) const {
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

Type *makeReferenceType(Sema &sema, Name *name, TypeKind ptr_kind,
                        Type *referee_type) {
  Type *t = new Type(name, ptr_kind, referee_type);
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
    sema.error(d->pos, "use of undeclared identifier '{}'", d->name->str());
    return;
  }
  d->decl = sym->value;
}

void NameBinding::visitCallExpr(CallExpr *f) {
  auto sym = sema.decl_table.find(f->func_name);
  if (!sym) {
    sema.error(f->pos, "undeclared function '{}'", f->func_name->str());
    return;
  }

  if (!sym->value->is<FuncDecl>()) {
    sema.error(f->pos, "'{}' is not a function", f->func_name->str());
    return;
  }

  f->callee_decl = sym->value; // FIXME
  assert(f->callee_decl);

  walk_func_call_expr(*this, f);

  // argument count match check
  if (f->kind == CallExprKind::func &&
      f->callee_decl->as<FuncDecl>()->args_count() != f->args.size()) {
    sema.error(f->pos, "'{}' accepts {} arguments, got {}", f->func_name->str(),
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
  if (t->subexpr) return;

  auto sym = sema.decl_table.find(t->name);
  if (sym && sym->value->typeMaybe()) {
    assert(t->kind == TypeKind::value);
    t->decl = sym->value;
  } else {
    sema.error(t->pos, "use of undeclared type '{}'", t->name->str());
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
    sema.error(pos, "redefinition of '{}'", name->str());
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
static bool is_reference_expr(const Expr *expr) {
  return expr->kind == ExprKind::unary &&
         (expr->as<UnaryExpr>()->kind == UnaryExprKind::ref ||
          expr->as<UnaryExpr>()->kind == UnaryExprKind::var_ref);
}

// Checks if 'expr' is a dereferencing expression, i.e.. '*expr'.
static bool isDereferenceExpr(const Expr *expr) {
  return expr->kind == ExprKind::unary &&
         expr->as<UnaryExpr>()->kind == UnaryExprKind::deref;
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
  if (!declare<EnumDecl>(sema, e->pos, e->name, e)) return;

  sema.decl_table.scope_open();
  sema.context.enum_decl_stack.push_back(e);

  walk_enum_decl(*this, e);

  sema.context.enum_decl_stack.pop_back();
  sema.decl_table.scope_close();
}

namespace {

bool mutabilityCheckAssignment(Sema &sema, const Expr *lhs) {
  if (lhs->kind == ExprKind::member) {
    // For MemberExprs, assignability depends on that of its struct side.
    return mutabilityCheckAssignment(sema, lhs->as<MemberExpr>()->struct_expr);
  } else if (isDereferenceExpr(lhs)) {
    auto unary = lhs->as<UnaryExpr>();
    if (unary->operand->type->kind != TypeKind::var_ref) {
      sema.error(unary->pos, "'{}' is not a mutable reference",
                 unary->operand->getLValueDecl()->name->str());
      return false;
    }
  } else {
    auto var_decl = lhs->getLValueDecl();
    if (var_decl && !var_decl->mut) {
      sema.error(lhs->pos, "'{}' is not declared as mutable",
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
  if (!lhs_ty || !rhs_ty) return nullptr;

  // Lvalue check.
  if (!as->lhs->is_lvalue()) {
    sema.error(as->pos, "cannot assign to an rvalue");
    return nullptr;
  }

  // Type compatibility check.
  if (!typeCheckAssignment(lhs_ty, rhs_ty)) {
    sema.error(as->pos, "cannot assign '{}' type to '{}'", rhs_ty->name->str(),
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
    sema.error(as->rhs->pos, "cannot copy non-copyable type '{}'",
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
    sema.error(rs->expr->pos, "function '{}' should not return a value",
               func_decl->name->str());
    return nullptr;
  }

  if (!typeCheckAssignment(func_decl->ret_type, rs->expr->type)) {
    sema.error(rs->expr->pos,
               "return type mismatch: function returns '{}', but got '{}'",
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
  auto opt_type = d->decl->typeMaybe();
  assert(
      opt_type.has_value() &&
      "tried to typecheck a non-typed DeclRef (first-class functions TODO?)");
  d->type = *opt_type;
  return d->type;
}

Type *TypeChecker::visitCallExpr(CallExpr *f) {
  walk_func_call_expr(*this, f);

  if (f->kind == CallExprKind::func) {
    auto callee_func_decl = f->callee_decl->as<FuncDecl>();
    assert(callee_func_decl->ret_type);
    f->type = callee_func_decl->ret_type;

    // check argument type match
    for (size_t i = 0; i < callee_func_decl->args.size(); i++) {
      if (!f->args[i]->type) return nullptr;

      // TODO: proper type comparison
      if (f->args[i]->type != callee_func_decl->args[i]->type) {
        sema.error(f->args[i]->pos,
                   "argument type mismatch: expects '{}', got '{}'",
                   callee_func_decl->args[i]->type->name->str(),
                   f->args[i]->type->name->str());
        return nullptr;
      }
    }
  } else {
    assert(!"unreachable");
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

// Make a new lifetime that is declared by `decl` and starts at the current
// scope.
// The new lifetime will be automatically destroyed on `scope_close()`.
Lifetime *start_lifetime(Sema &sema, Decl *decl) {
  auto lt = sema.make_lifetime(decl);
  sema.lifetime_table.insert(lt, lt);
  return lt;
}

// Make a new lifetime of a reference variable, which is annotated by `name`.
// `annot` is necessary because there is no other way to give information about
// the lifetime of a reference variable unless we can pinpoint the Decl of the
// referee, in which case you can just use `start_lifetime()`.
Lifetime *start_lifetime_of_ref(Sema &sema, Name *annot) {
  auto lt = sema.make_lifetime(static_cast<Name *>(nullptr));
  lt->lifetime_annot = annot;
  sema.lifetime_table.insert(lt, lt);
  return lt;
}

} // namespace

Type *TypeChecker::visitStructDefExpr(StructDefExpr *s) {
  walk_struct_def_expr(*this, s);

  // check Name is a struct
  auto lhs_type = s->name_expr->type;
  if (!lhs_type) return nullptr;
  if (!lhs_type->isStruct()) {
    sema.error(s->name_expr->pos, "type '{}' is not a struct",
               lhs_type->name->str());
    return nullptr;
  }

  for (auto &desig : s->desigs) {
    if (!desig.init_expr->type) return nullptr;

    VarDecl *field_decl = findFieldInStruct(desig.name, lhs_type);
    if (!field_decl) {
      sema.error(desig.init_expr->pos, // FIXME: wrong pos
                 "'{}' is not a member of '{}'", desig.name->str(),
                 lhs_type->getStructDecl()->name->str());
      return nullptr;
    }

    if (!typeCheckAssignment(field_decl->type, desig.init_expr->type)) {
      sema.error(desig.init_expr->pos, "cannot assign '{}' type to '{}'",
                 desig.init_expr->type->name->str(), field_decl->type->name->str());
      return nullptr;
    }
  }

  s->type = lhs_type;
  return s->type;
}

Type *TypeChecker::visitCastExpr(CastExpr *c) {
  walk_cast_expr(*this, c);

  c->type = c->type_expr->type;
  return c->type;
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
    sema.error(m->struct_expr->pos, "type '{}' is not a struct",
               struct_type->name->str());
    return nullptr;
  }

  if (struct_type->isStruct()) {
    // First of all, make sure that this field name indeed exists in the
    // struct's type definition.
    VarDecl *field_decl = findFieldInStruct(m->member_name, struct_type);
    if (!field_decl) {
      // TODO: pos for member
      sema.error(m->struct_expr->pos, "'{}' is not a member of '{}'",
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
    if (m->struct_expr->is_lvalue()) {
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

        // TODO: abstract this and make_node together into a func.
        field_var_decl->lifetime = start_lifetime(sema, field_var_decl);
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
//
// Derived types are only present in the type table if they occur in the source
// code.  Trying to push them every time we see one is sufficient to keep this
// invariant.
Type *get_derived_type(Sema &sema, TypeKind kind, Type *type) {
  auto name = name_of_derived_type(sema.name_table, kind, type->name);
  if (auto found = sema.type_table.find(name)) {
    return found->value;
  }

  auto ref_type = makeReferenceType(sema, name, kind, type);
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
        sema.error(u->operand->pos, "dereference of a non-reference type '{}'",
                   u->operand->type->name->str());
        return nullptr;
      }
      u->type = u->operand->type->referee_type;

      // Also bind a temporary VarDecl to this expression that respects the
      // mutability of the reference type.  This way we know if this lvalue is
      // assignable or not.
      //
      // For example,
      //
      //     let v: var &int = ...
      //     *v = 3
      //
      // The '*v' here has to have a valid VarDecl with 'mut' as true.
      bool mut = (u->operand->type->kind == TypeKind::var_ref);
      u->var_decl = sema.make_node<VarDecl>(nullptr, u->type, mut);
      // Temporary VarDecls are _not_ pushed to the scoped decl table, because
      // they are not meant to be accessed later from a different position in
      // the source. In the same sense, they don't have a name that can be used
      // to query them.
    }
    break;
  }
  case UnaryExprKind::var_ref:
  case UnaryExprKind::ref: {
    if (visitExpr(u->operand)) {
      // Prohibit taking address of an rvalue.
      if (!u->operand->is_lvalue()) {
        sema.error(u->pos, "cannot take address of an rvalue");
        return nullptr;
      }

      // Prohibit borrowing an immutable value as mutable.
      if (u->kind == UnaryExprKind::var_ref) {
        auto operand_vardecl = u->operand->getLValueDecl();
        if (!operand_vardecl->mut) {
          sema.error(u->pos,
                     "cannot borrow '{}' as mutable because it is declared "
                     "immutable",
                     operand_vardecl->name->str());
          return nullptr;
        }
      }

      auto type_kind = (u->kind == UnaryExprKind::var_ref) ? TypeKind::var_ref
                                                           : TypeKind::ref;
      u->type = get_derived_type(sema, type_kind, u->operand->type);
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
               b->lhs->type->name->str(), b->rhs->type->name->str());
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
    t->type = *t->decl->typeMaybe();
    assert(t->type && "type not resolved after visiting corresponding *Decl");
  } else if (t->kind == TypeKind::ref || t->kind == TypeKind::var_ref) {
    t->type = get_derived_type(sema, t->kind, t->subexpr->type);
  } else if (t->kind == TypeKind::ptr) {
    t->type = get_derived_type(sema, t->kind, t->subexpr->type);
  } else {
    assert(!"unreachable");
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
      sema.error(v->assign_expr->pos, "cannot copy non-copyable type '{}'",
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
  // FIXME: broken
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
  if (f->body) {
    sema.context.func_decl_stack.push_back(f);
    visitCompoundStmt(f->body);
    sema.context.func_decl_stack.pop_back();
  }

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
  // For body-less function declarations (e.g. extern).
  if (!f->body) return nullptr;

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

namespace {

// Checks if an expr is 'behind' a reference, i.e. it represents an access
// that goes through the reference.
class BehindRefVisitor : public AstVisitor<BehindRefVisitor, VarDecl *> {
public:
  // 'p' alone does not go though p.
  VarDecl *visitDeclRefExpr(DeclRefExpr *d) {
    return nullptr;
  }
  VarDecl *visitCallExpr(CallExpr *f) {
    assert(false && "TODO");
  }
  // p.m is the same as (*p).m. If 'p' is not a reference, p.m does not go
  // through anything.
  // @Cleanup: maybe rewrite p.m as (*p).m in a unified place?
  VarDecl *visitMemberExpr(MemberExpr *m) {
    if (auto v = visitExpr(m->struct_expr)) {
      return v;
    } else {
      return nullptr;
    }
  }
  VarDecl *visitUnaryExpr(UnaryExpr *u) {
    switch (u->kind) {
    case UnaryExprKind::paren:
      return visitParenExpr(u->as<ParenExpr>());
    case UnaryExprKind::ref:
    case UnaryExprKind::var_ref:
      // &p is not behind p. (???)
      return nullptr;
    case UnaryExprKind::deref:
      if (u->operand->kind == ExprKind::decl_ref) {
        // *p
        return u->operand->getLValueDecl();
      } else {
        // ex) *(*p)
        return visitExpr(u->operand);
      }
    default:
      assert(false && "inexhaustive kind");
      break;
    }
  }
  VarDecl *visitParenExpr(ParenExpr *p) {
    return visitExpr(p->operand);
  }
  // void visitStructDefExpr(StructDefExpr *s);
};

// Mark a variable that it is borrowed in the current scope.
//
// Possible borrowing occasions:
// - let x = &a
// - x = &a
// - x = S {.m = &a}
// - f(&a)
// (What about just '&a')?
void register_borrow_count(Sema &sema, const VarDecl *borrowee, bool mut, size_t borrowee_pos) {
  int immutable_borrow_count_old = 0;
  int mutable_borrow_count_old = 0;

  auto found = sema.borrow_table.find(borrowee);
  if (found) {
    immutable_borrow_count_old = found->value.immutable_borrow_count;
    mutable_borrow_count_old = found->value.mutable_borrow_count;
  }

  if (mutable_borrow_count_old > 0) {
    sema.error(borrowee_pos,
               "cannot borrow '{}' as immutable because it was borrowed as "
               "mutable before",
               borrowee->name->str());
    return;
  }
  if (immutable_borrow_count_old > 0 && mut) {
    sema.error(borrowee_pos,
               "cannot borrow '{}' as mutable because it was borrowed as "
               "immutable before",
               borrowee->name->str());
    return;
  }

  sema.borrow_table.insert(
      borrowee, BorrowMap{borrowee, immutable_borrow_count_old + (mut ? 0 : 1),
                          mutable_borrow_count_old + (mut ? 1 : 0)});
}

// Find the lifetime of the value that this reference is referring to.
// Note that this is not about the lifetime of the reference variable *itself*,
// but about its *referree*.
Lifetime *lifetime_of_reference(Sema &sema, Expr *ref_expr) {
  if (!ref_expr->type->isReference()) return nullptr;

  if (ref_expr->is_lvalue()) {
    // Lvalue reference variable, e.g. 'ptr: &int'.
    return ref_expr->getLValueDecl()->borrowee_lifetime;
  } else if (is_reference_expr(ref_expr)) {
    // Explicit reference expression, e.g. '&a'.
    auto operand = ref_expr->as<UnaryExpr>()->operand;
    if (operand->kind == ExprKind::member) {
      // For MemberExprs (e.g. v = &m.a), we are essentially borrowing from the
      // whole struct, not just the member.
      //
      // FIXME: We gotta find the root parent, not the parent of just one level
      // above. Add a test case for this.
      return operand->getLValueDecl()->parent->lifetime;
    } else {
      return operand->getLValueDecl()->lifetime;
    }
  } else if (ref_expr->is_func_call()) {
    auto func_call_expr = ref_expr->as<CallExpr>();
    auto func_decl = func_call_expr->callee_decl->as<FuncDecl>();

    // Map lifetimes of each args to the annotations, and search for the
    // return value annotation among them.
    //
    // NOTE: lifetime coercion happens here. If multiple lifetimes match to a
    // single annotated name, find the shortest-living one and use that.
    //
    // In the point of view from the inside the function, whether a coercion
    // happened or not on the callee side does not affect the result of the
    // borrowcheck of the function body.
    std::vector<std::pair<Name * /*annotations*/, Lifetime *>> map;
    for (size_t i = 0; i < func_decl->args.size(); i++) {
      if (!func_decl->args[i]->type->isReference()) continue;

      assert(func_decl->args[i]->borrowee_lifetime->lifetime_annot);
      // NOTE that it's `borrowee_lifetime`, *not* `lifetime`!
      map.push_back({func_decl->args[i]->borrowee_lifetime->lifetime_annot,
                     lifetime_of_reference(sema, func_call_expr->args[i])});
    }

    Lifetime *shortest_found = nullptr;
    int shortest_found_scope_level = 0;
    for (auto const &item : map) {
      if (item.first == func_decl->ret_lifetime_annot) {
        if (shortest_found) {
          int item_scope_level = sema.lifetime_table.find(item.second)->scope_level;
          if (item_scope_level >
              shortest_found_scope_level) {
            shortest_found = item.second;
            shortest_found_scope_level = item_scope_level;
          }
        } else {
          shortest_found = item.second;
          shortest_found_scope_level =
              sema.lifetime_table.find(item.second)->scope_level;
        }
      }
    }

    assert(shortest_found);
    return shortest_found;
  } else {
    assert(false && "unimplemented");
  }
}

void borrowcheck_assignment(Sema &sema, VarDecl *v, Expr *rhs, bool move) {
  // We don't want to mess with built-in types.
  if (rhs->type->isBuiltin(sema)) return;

  // Pattern-match-like recursion case.
  // This works because we guarantee that every lvalue has a VarDecl.
  if (rhs->kind == ExprKind::struct_def) {
    for (auto desig : rhs->as<StructDefExpr>()->desigs) {
      if (is_reference_expr(desig.init_expr)) {
        // Find child decl by name.
        VarDecl *child = nullptr;
        for (auto c : v->children) {
          if (c.first == desig.name) {
            child = c.second;
            break;
          }
        }
        borrowcheck_assignment(sema, child, desig.init_expr, move);
      }
    }
    return;
  }

  // Leaf cases of the recursion.

  // FIXME: This code should be good to be substituted with the above
  // one-liner, clear up this whole section.
  if (rhs->type->isReference()) {

    v->borrowee_lifetime = lifetime_of_reference(sema, rhs);

    if (rhs->is_lvalue()) {
      // 'Implicit' copying of a borrow, e.g. 'ref1: &int = ref2: &int'.
      if (move) {
        assert(false && "TODO: nullify reference in RHS");
      }
    } else if (is_reference_expr(rhs)) {
      // Explicit borrowing statement, e.g. 'a = &b'.
      //
      // Note that a move assignment with an rvalue RHS is the same as a copy,
      // so both cases are treated in the same code below.
      auto operand = rhs->as<UnaryExpr>()->operand;
      if (operand->kind == ExprKind::member) {
        operand->getLValueDecl()->parent->borrowed = true;
      } else {
        operand->getLValueDecl()->borrowed = true;
      }
    } else if (rhs->is_func_call()) {
    } else {
      assert(false && "unimplemented");
    }

    // Safety check, remove later.
    if (!v->borrowee_lifetime) {
      sema.error(rhs->pos, "ASSERT: lifetime still null");
      return;
    }
  } else if (move && rhs->is_lvalue()) {
    // Move of a non-reference lvalue, e.g. 'a <- b' or 'a <- *p' (illegal).
    //
    // @Future: Invalidate RHS here. Program must still run even without this
    // invalidation, because access to the moved out value is forbidden in the
    // semantic phase.
    auto ref_behind = BehindRefVisitor{}.visitExpr(rhs);
    if (ref_behind) {
      // E.g. 'a <- *p'.  This is illegal because it invalidates all later
      // accesses through 'p'.
      sema.error(rhs->pos,
                 "cannot move out of '{}' because it will invalidate '{}'",
                 rhs->text(sema.source), ref_behind->name->str());
      return;
    // } else if (rhs->getLValueDecl()->borrowed) {
    } else if (sema.borrow_table.find(rhs->getLValueDecl()) &&
               (sema.borrow_table.find(rhs->getLValueDecl())
                        ->value.mutable_borrow_count > 0 ||
                sema.borrow_table.find(rhs->getLValueDecl())
                        ->value.immutable_borrow_count > 0)) {
      sema.error(rhs->pos, "cannot move out of '{}' because it is borrowed",
                 rhs->text(sema.source));
      return;
    }

    rhs->getLValueDecl()->moved = true;
  }
}

} // namespace

void BorrowChecker::visitAssignStmt(AssignStmt *as) {
  walk_assign_stmt(*this, as);

  auto lhs_decl = as->lhs->getLValueDecl();
  borrowcheck_assignment(sema, lhs_decl, as->rhs, as->move);
}

void BorrowChecker::visitReturnStmt(ReturnStmt *rs) {
  // For every borrowing expressions in the return statement, we can check if
  // the Decl of the referee is present in the current function scope to find
  // lifetime errors.
  in_return_stmt = true;
  walk_return_stmt(*this, rs);

  // Return statement borrowck.
  //
  // At this point, other borrowck errors such as use-after-free would have
  // been caught in the walk_return_stmt() call above.
  if (in_return_stmt && rs->expr->type->isReference()) {
    auto lifetime = lifetime_of_reference(sema, rs->expr);
    if (!lifetime) {
      sema.error(rs->expr->pos, "TODO: null lifetime");
      return;
    }

    assert(!sema.context.func_decl_stack.empty());
    auto current_func = sema.context.func_decl_stack.back();

    if (lifetime->kind == Lifetime::annotated) {
      // Lifetime mismatch check.
      //
      // TODO: Currently we do simple equality comparison (!=) between the
      // lifetimes. This may not be sufficient in the future.
      assert(lifetime->lifetime_annot);
      if (lifetime->lifetime_annot !=
          current_func->ret_type_expr->as<TypeExpr>()->lifetime_annot) {
        sema.error(
            rs->expr->pos, "lifetime mismatch: expected .{}, got .{}",
            current_func->ret_type_expr->as<TypeExpr>()->lifetime_annot->str(),
            lifetime->lifetime_annot->str());
        return;
      }
    } else {
      // References to local variable check.
      // Detect use of a local variable in a reference.
      auto func_scope_level =
          sema.lifetime_table.find(current_func->scope_lifetime)->scope_level;
      auto borrowee_level = sema.lifetime_table.find(lifetime)->scope_level;
      if (borrowee_level > func_scope_level) {
        sema.error(rs->expr->pos,
                   "cannot return value that references local variable '{}'",
                   lifetime->decl->name()->str());
        return;
      }
    }
  }

  in_return_stmt = false;
}

void BorrowChecker::visitExpr(Expr *e) {
  // Use of moved value check.
  // This is a pre-order step so that once a use-after-move error is detected,
  // the traversal stops.
  if (e->is_lvalue()) {
    if (e->getLValueDecl()->moved) {
      sema.error(e->pos, "use of moved value");
      return;
    }
  }

  AstVisitor<BorrowChecker, void>::visitExpr(e);
}

// Rule: a variable of lifetime 'a should only refer to a variable whose
// lifetime is larger than 'a.
// In other words, at the point of use, the borrowee should be alive.
void BorrowChecker::visitDeclRefExpr(DeclRefExpr *d) {
  if (!d->decl->is<VarDecl>()) return;
  auto var = d->decl->as<VarDecl>();

  // at each use of a reference variable, check if its borrowee is alive
  if (var->borrowee_lifetime && var->borrowee_lifetime->kind == Lifetime::exact) {
    auto sym = sema.lifetime_table.find(var->borrowee_lifetime);
    // TODO: refactor into find_exact()
    if (!(sym && sym->value == var->borrowee_lifetime)) {
      sema.error(d->pos, "'{}' does not live long enough",
                 var->borrowee_lifetime->decl->name()->str());
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
// type is essentially the same as Rust's Box<T> or C++'s std::unique_ptr<T>,
// with a fixed 'drop' procedure.
//
// The language has a stance on assignments that is the opposite of Rust's
// 'move by default'.  In Rust, all assignments are treated as move except for
// a small subset of types. This makes the transferring of ownerships less
// obvious, because it is not easy to see if the type at hand is copyable or
// not without looking up its declaration.
//
// In our language, we essentially distinguish moves from copies syntactically.
// We interpret all assignments as copying, but if the type of the value being
// copied contains a non-copyable type (e.g. an owning pointer or a mutable
// reference), we disallow the copy.  Instead, only move assignments are
// allowed on those types, which have a distinct syntax from the usual copy
// assignment.
//
// This design has several advantages:
//
//   1. It makes transfer of ownership explicit and more obvious, which can be
//      a good thing.
//   2. It makes the language feel less foreign to people coming from languages
//      like C or C++ which engage the copy-by-default semantics as well.
//
// Function calls
// ==============
//
// The language has a call-by-value semantics.  This means that every
// argument to a function is copied its value to the stack frame of the called
// function.  If we want to pass in a variable of a non-copyable type, we again
// need to use a separate syntax; maybe <-var. (TODO)
//
void BorrowChecker::visitCallExpr(CallExpr *f) {
    walk_func_call_expr(*this, f);
}

void BorrowChecker::visitStructDefExpr(StructDefExpr *s) {
  walk_struct_def_expr(*this, s);

  for (auto desig : s->desigs) {
    if (is_reference_expr(desig.init_expr)) {
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
    register_borrow_count(sema, u->operand->getLValueDecl(),
                          u->kind == UnaryExprKind::var_ref, u->pos);
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

  v->lifetime = start_lifetime(sema, v);
  for (auto child : v->children) {
    // FIXME: but... shouldn't have these already been pushed at the time of
    // their declaration?
    child.second->lifetime = start_lifetime(sema, child.second);
  }

  if (v->assign_expr) {
    borrowcheck_assignment(
        sema, v, v->assign_expr,
        true /* because declarations with an init expr is always a move. */);
  } else if (v->type_expr) {
    if (v->type_expr->as<TypeExpr>()->lifetime_annot) {
      if (v->kind == VarDeclKind::param) {
        // gotta set the annotated lifetimes
        v->borrowee_lifetime = start_lifetime_of_ref(
            sema, v->type_expr->as<TypeExpr>()->lifetime_annot);
      } else {
        assert(false && "TODO: annotations in local VarDecl");
      }
    }
  }
}

void BorrowChecker::visitFuncDecl(FuncDecl *f) {
  // Necessary because of the early returns.
  class BorrowCheckFuncRAII {
    BorrowChecker &bc;
    bool save;

  public:
    BorrowCheckFuncRAII(BorrowChecker &bc) : bc(bc) {
      save = bc.in_annotated_func;
    }
    ~BorrowCheckFuncRAII() { bc.in_annotated_func = save; }

    void set(bool b) { bc.in_annotated_func = b; }
  };

  BorrowCheckFuncRAII raii(*this);

  for (auto arg : f->args) {
    if (arg->type_expr->as<TypeExpr>()->lifetime_annot) {
      raii.set(true);
      break;
    }
  }

  if (in_annotated_func) {
    std::vector<Name *> declared_lifetimes;

    // Require that every argument is annotated.
    for (auto arg : f->args) {
      if (arg->type->isReference() &&
          !arg->type_expr->as<TypeExpr>()->lifetime_annot) {
        sema.error(arg->pos, "missing lifetime annotation");
        return;
      }
      declared_lifetimes.push_back(
          arg->type_expr->as<TypeExpr>()->lifetime_annot);
    }

    // Require that return value is annotated.
    if (f->ret_type && f->ret_type->isReference() &&
        !f->ret_type_expr->as<TypeExpr>()->lifetime_annot) {
      sema.error(f->ret_type_expr->pos, "missing lifetime annotation");
      return;
    }

    // Check if the annotation of the return value was already seen in the args
    // list.
    bool seen = false;
    for (auto lt : declared_lifetimes) {
      if (f->ret_type_expr->as<TypeExpr>()->lifetime_annot == lt) {
        seen = true;
        break;
      }
    }
    if (!seen) {
      sema.error(f->ret_type_expr->pos, "unknown lifetime annotation '{}'",
                 f->ret_type_expr->as<TypeExpr>()->lifetime_annot->str());
      return;
    }

    f->ret_lifetime_annot = f->ret_type_expr->as<TypeExpr>()->lifetime_annot;
  }

  // This is used for local variable detection.
  f->scope_lifetime = start_lifetime(sema, f);

  sema.context.func_decl_stack.push_back(f);

  walk_func_decl(*this, f);

  sema.context.func_decl_stack.pop_back();
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

void CodeGenerator::visitCallExpr(CallExpr *f) {
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
std::string c_stringify_type(Sema &sema, const Type *t) {
  if (t == sema.context.string_type) {
    // For now, strings are aliased to char *.  This works as long as
    // strings are immutable and doesn't contain unicode characters.
    return "char*";
  } else if (t->isReference()) {
    auto base = c_stringify_type(sema, t->referee_type);
    return base + "*";
  } else if (t->kind == TypeKind::ptr) {
    auto base = c_stringify_type(sema, t->referee_type);
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
    emit("{} {}", c_stringify_type(sema, v->type), v->name->str());
  } else {
    emit("{} {};\n", c_stringify_type(sema, v->type), v->name->str());

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

  if (f->ret_type_expr) {
    emit("{}", c_stringify_type(sema, f->ret_type));
  } else {
    emit("void");
  }

  emitCont(" {}(", f->name->str());

  if (f->args.empty()) {
    emitCont("void");
  } else {
    for (size_t i = 0; i < f->args.size(); i++) {
      visitDecl(f->args[i]);
      if (i != f->args.size() - 1) {
        emitCont(", ");
      }
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

void CodeGenerator::visitExternDecl(ExternDecl *e) {
  // Do nothing.
}
