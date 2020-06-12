#include "ast.h"
#include "sema.h"
#include "parser.h"
#include "source.h"
#include "types.h"
#include <cassert>

#define BUFSIZE 1024

namespace cmp {

// TODO: Decl::str()
// std::string VarDecl::str() const {
//     return name->str();
// }
// 
// std::string StructDecl::str() const {
//     return name->str();
// }

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

Type *make_builtin_type(Sema &sema, Name *n) {
    Type *t = new Type(n);
    sema.type_pool.push_back(t);
    return t;
}

Type *make_value_type(Sema &sema, Name *n, Decl decl) {
    Type *t = new Type(TypeKind::value, n, decl);
    sema.type_pool.push_back(t);
    return t;
}

Type *make_ref_type(Sema &sema, Name *n, Type *base_type) {
    Type *t = new Type(TypeKind::ref, n, base_type);
    sema.type_pool.push_back(t);
    return t;
}

Type *push_builtin_type_from_name(Sema &s, const std::string &str) {
    Name *name = s.name_table.get_or_add(str);
    auto struct_decl = s.make_decl<StructDecl>(name);
    struct_decl->type = make_builtin_type(s, name);
    s.decl_table.insert(name, struct_decl);
    return struct_decl->type;
}

// Push Decls for the builtin types into the global scope of decl_table, so
// that they are visible from any point in the AST.
void setup_builtin_types(Sema &s) {
  s.context.void_type = push_builtin_type_from_name(s, "void");
  s.context.int_type = push_builtin_type_from_name(s, "int");
  s.context.char_type = push_builtin_type_from_name(s, "char");
  s.context.string_type = push_builtin_type_from_name(s, "string");
}

Sema::Sema(Parser &p) : Sema(p.lexer.source(), p.names, p.errors, p.beacons) {}

Sema::~Sema() {
    for (auto d : decl_pool) {
        delete d;
    }
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

// Return optional of 'type' member of Decl, or None if this Decl kind doesn't
// have any.
std::optional<Type *> decl_get_type(const Decl decl) {
    if (auto d = declCast<VarDecl>(decl)) {
        return d->type;
    } else if (auto d = declCast<StructDecl>(decl)) {
        return d->type;
    } else if (auto d = declCast<EnumDecl>(decl)) {
        return d->type;
    } else if (auto d = declCast<FuncDecl>(decl)) {
        return {};
    }
    assert(false && "not all decl kinds handled");
}

void NameBinder::visitCompoundStmt(CompoundStmt *cs) {
    sema.scope_open();
    walk_compound_stmt(*this, cs);
    sema.scope_close();
}

void NameBinder::visitAssignStmt(AssignStmt *as) {
    walk_assign_stmt(*this, as);

    // Checking if an AST node has an associated Decl object is a purely
    // syntactical check, and therefore can be done in the name binding phase.
    //
    // XXX: It's not obvious (and not even correct?) that r-values correspond to
    // expressions that don't have an associated Decl object. Maybe make an
    // is_rvalue() function.
    auto decl_opt = as->lhs->decl();
    if (!decl_opt) {
        sema.error(as->pos, "LHS is not assignable");
        return;
    }

    auto var_decl = declCast<VarDecl>(*decl_opt);

    // XXX: What about MemberExprs?
    // Name binding on LHS may have been failed.
    // if (!var_decl) {
    //     assert(false);
    //     return;
    // }

    if (var_decl && !var_decl->mut) {
        sema.error(as->pos, "'%s' is immutable", var_decl->name->str());
        return;
    }
}

void NameBinder::visitDeclRefExpr(DeclRefExpr *d) {
  auto sym = sema.decl_table.find(d->name);
  if (!sym) {
    sema.error(d->pos, "use of undeclared identifier '%s'", d->name->str());
    return;
  }
  d->decl = sym->value;
}

// Only binds the function name part of the call, e.g. 'func' of func().
void NameBinder::visitFuncCallExpr(FuncCallExpr *f) {
    // resolve function name
    auto sym = sema.decl_table.find(f->func_name);
    if (!sym) {
        sema.error(f->pos, "undeclared function '%s'", f->func_name->str());
        return;
    }
    if (!declCast<FuncDecl>(sym->value)) {
        sema.error(f->pos, "'%s' is not a function", f->func_name->str());
        return;
    }
    f->func_decl = declCast<FuncDecl>(sym->value);
    assert(f->func_decl);

    walk_func_call_expr(*this, f);

    // check if argument count matches
    if (f->func_decl->args_count() != f->args.size()) {
        sema.error(f->pos, "'%s' accepts %lu arguments, got %lu",
                   f->func_name->str(), f->func_decl->args_count(),
                   f->args.size());
        return;
    }
}

void NameBinder::visitTypeExpr(TypeExpr *t) {
  walk_type_expr(*this, t);

  // Namebinding for TypeExprs only include linking existing Decls to the
  // type names used in the expression, not declaring new ones.  The
  // declaration would be done when visiting VarDecls and StructDecls, etc.

  // For pointers and arrays, proper typechecking will be done in the later
  // stages.
  if (t->subexpr) return;

  auto sym = sema.decl_table.find(t->name);
  if (sym && decl_get_type(sym->value)) {
    assert(t->kind == TypeExprKind::value);
    t->decl = sym->value;
  } else {
    sema.error(t->pos, "use of undeclared type '%s'", t->name->str());
    return;
  }
}

// Semantically declare a 'name' at a 'pos', whose Decl type is T.
// Returns nullptr if declaration failed due to e.g. redeclaration.
template <typename T, typename... Args>
T *declare(Sema &sema, size_t pos, Name *name, Args &&... args) {
    auto found = sema.decl_table.find(name);
    if (found && decl_is<T>(found->value) &&
        found->scope_level == sema.decl_table.curr_scope_level) {
        sema.error(pos, "redefinition of '%s'", name->str());
        return nullptr;
    }

    T *decl = sema.make_decl<T>(name, std::forward<Args>(args)...);
    // bind decl to name
    sema.decl_table.insert(name, decl);

    return decl;
}

void NameBinder::visitVarDecl(VarDeclNode *v) {
    walk_var_decl(*this, v);

    if (!(v->var_decl = declare<VarDecl>(sema, v->pos, v->name, v->mut)))
        return;

    // struct member declarations are also parsed as VarDecls.
    if (v->kind == VarDeclNodeKind::struct_) {
        assert(!sema.context.struct_decl_stack.empty());
        auto enclosing_struct = sema.context.struct_decl_stack.back();
        enclosing_struct->fields.push_back(v->var_decl);
    } else if (v->kind == VarDeclNodeKind::param) {
        assert(!sema.context.func_decl_stack.empty());
        auto enclosing_func = sema.context.func_decl_stack.back();
        enclosing_func->args.push_back(v->var_decl);
    }
}

void NameBinder::visitFuncDecl(FuncDeclNode *f) {
    auto err_count = sema.errors.size();

    if (!(f->func_decl = declare<FuncDecl>(sema, f->pos, f->name))) return;

    // scope for argument variables
    sema.decl_table.scope_open();
    sema.context.func_decl_stack.push_back(f->func_decl);

    walk_func_decl(*this, f);

    sema.context.func_decl_stack.pop_back();
    sema.decl_table.scope_close();

    if (sema.errors.size() != err_count) {
        f->failed = true;
    }
}

void NameBinder::visitStructDecl(StructDeclNode *s) {
    if (!(s->struct_decl = declare<StructDecl>(sema, s->pos, s->name))) return;

    // Decl table is used for checking redefinition when parsing the member
    // list.
    sema.decl_table.scope_open();
    sema.context.struct_decl_stack.push_back(s->struct_decl);

    walk_struct_decl(*this, s);

    sema.context.struct_decl_stack.pop_back();
    sema.decl_table.scope_close();
}

namespace {

// Generate a name for the anonymous fields in each enum variant structs.
// For now, these are named "_0", "_1", and so on.
Name *gen_anonymous_field_name(Sema &sema, size_t index) {
    char buf[BUFSIZE];
    snprintf(buf, BUFSIZE, "_%lu", index);
    return sema.name_table.get_or_add(std::string{buf});
}

} // namespace

void NameBinder::visitEnumVariantDecl(EnumVariantDeclNode *v) {
  walk_enum_variant_decl(*this, v);

  // first, declare a struct of this name
  if (!(v->struct_decl = declare<StructDecl>(sema, v->pos, v->name))) return;

  // then, add fields to this struct, whose names are anonymous
  // and only types are specified (e.g. Pos(int, int))
  for (size_t i = 0; i < v->fields.size(); i++) {
    // so that anonymous field names don't clash
    sema.decl_table.scope_open();

    if (auto field_decl = declare<VarDecl>(sema, v->fields[i]->pos,
                                           gen_anonymous_field_name(sema, i),
                                           false /*TODO: mut*/)) {
        v->struct_decl->fields.push_back(field_decl);
    }

    sema.decl_table.scope_close();
  }

  // now, add this struct into the scope of the enum
  assert(!sema.context.enum_decl_stack.empty());
  auto enclosing_enum = sema.context.enum_decl_stack.back();
  enclosing_enum->variants.push_back(v->struct_decl);
}

void NameBinder::visitEnumDecl(EnumDeclNode *e) {
  if (!(e->enum_decl = declare<EnumDecl>(sema, e->pos, e->name))) return;

  sema.decl_table.scope_open();
  sema.context.enum_decl_stack.push_back(e->enum_decl);

  walk_enum_decl(*this, e);

  sema.context.enum_decl_stack.pop_back();
  sema.decl_table.scope_close();
}

// Typecheck assignment statement of 'lhs = rhs'.
bool typecheck_assignment(const Type *lhs, const Type *rhs) {
    // only allow exact equality for assignment for now (TODO)
    return lhs == rhs;
}

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

    // Mutability check.
    // TODO

    // Copyability check.
    if (!rhs_ty->copyable) {
        sema.error(as->rhs->pos, "cannot copy '%s'", rhs_ty->name->str());
        return nullptr;
    }

    if (!typecheck_assignment(lhs_ty, rhs_ty)) {
        sema.error(as->pos, "cannot assign '%s' type to '%s'",
                   rhs_ty->name->str(), lhs_ty->name->str());
        return nullptr;
    }

    return lhs_ty;
}

bool FuncDecl::isVoid(Sema &sema) const {
  return ret_ty == sema.context.void_type;
}

Type *TypeChecker::visitReturnStmt(ReturnStmt *rs) {
  visitExpr(rs->expr);
  if (!rs->expr->type) return nullptr; // TODO

  assert(!sema.context.func_decl_stack.empty());
  auto func_decl = sema.context.func_decl_stack.back();
  if (func_decl->isVoid(sema)) {
    sema.error(rs->expr->pos, "function '%s' should not return a value",
               func_decl->name->str());
    return nullptr;
  }

  if (!typecheck_assignment(func_decl->ret_ty, rs->expr->type)) {
    sema.error(rs->expr->pos,
               "return type mismatch: function returns '%s', but got '%s'",
               func_decl->ret_ty->name->str(), rs->expr->type->name->str());
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
  auto opt_type = decl_get_type(d->decl);
  assert(
      opt_type.has_value() &&
      "tried to typecheck a non-typed DeclRef (first-class functions TODO?)");
  d->type = *opt_type;
  return d->type;
}

Type *TypeChecker::visitFuncCallExpr(FuncCallExpr *f) {
    walk_func_call_expr(*this, f);

    assert(f->func_decl->ret_ty);
    f->type = f->func_decl->ret_ty;

    // check argument type match
    for (size_t i = 0; i < f->func_decl->args.size(); i++) {
        assert(f->args[i]->type);
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

Type *TypeChecker::visitStructDefExpr(StructDefExpr *s) {
    walk_struct_def_expr(*this, s);

    // check Name is a struct
    auto lhs_type = s->name_expr->type;
    if (!lhs_type) return nullptr;
    if (!lhs_type->is_struct()) {
        sema.error(s->name_expr->pos, "type '%s' is not a struct",
                   lhs_type->name->str());
        return nullptr;
    }

    // typecheck each field
    // XXX: copy-paste from visitMemberExpr
    for (auto &desig : s->desigs) {
        for (auto field : lhs_type->get_struct_decl()->fields) {
            if (desig.name == field->name) {
                desig.decl = field;
                break;
            }
        }

        if (!desig.decl) {
            const char *fmt = "no field named '%s' in struct '%s'";
            sema.error(desig.expr->pos, // FIXME: wrong pos
                       fmt, desig.name->str(),
                       lhs_type->get_struct_decl()->name->str());
            return nullptr;
        }

        // skip if earlier typecheck error
        if (!desig.expr->type) return nullptr;

        if (!typecheck_assignment(desig.decl->type, desig.expr->type)) {
            sema.error(desig.expr->pos, "cannot assign '%s' type to '%s'",
                       desig.expr->type->name->str(),
                       desig.decl->type->name->str());
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
  if (!m->lhs_expr->type)
    return nullptr;

  // make sure the LHS is actually a struct
  auto lhs_type = m->lhs_expr->type;
  if (!lhs_type->is_member_accessible()) {
    sema.error(m->lhs_expr->pos, "type '%s' is not a struct",
               lhs_type->name->str());
    return nullptr;
  }

  // find a member with the same name
  // TODO: can this be abstracted?
  bool found = false;
  if (lhs_type->is_struct()) {
    for (auto mem_decl : lhs_type->get_struct_decl()->fields) {
      if (m->member_name == mem_decl->name) {
        m->decl = mem_decl;
        found = true;
        break;
      }
    }
  }
  if (lhs_type->is_enum()) {
    for (auto mem_decl : lhs_type->get_enum_decl()->variants) {
      if (m->member_name == mem_decl->name) {
        m->decl = mem_decl;
        found = true;
        break;
      }
    }
  }
  if (!found) {
    // TODO: pos for member
    sema.error(m->lhs_expr->pos, "'%s' is not a member of '%s'",
               m->member_name->str(), lhs_type->name->str());
    return nullptr;
  }

  // the fields are already typechecked
  assert(decl_get_type(m->decl));
  m->type = *decl_get_type(m->decl);

  return m->type;
}

// Get or make a reference type of a given type.
Type *get_reference_type(Sema &sema, Type *type) {
    Name *name = sema.name_table.get_or_add("*" + type->name->text);
    if (auto found = sema.type_table.find(name)) return found->value;

    // FIXME: scope_level
    auto ref_type = make_ref_type(sema, name, type);
    return *sema.type_table.insert(name, ref_type);
}

Type *TypeChecker::visitUnaryExpr(UnaryExpr *u) {
  switch (u->kind) {
  case UnaryExprKind::paren:
    visitParenExpr(static_cast<ParenExpr *>(u));
    break;
  case UnaryExprKind::deref:
    visitExpr(u->operand);
    // XXX: arbitrary
    if (!u->operand->type)
      return nullptr;

    if (u->operand->type->kind != TypeKind::ref) {
      sema.error(u->operand->pos, "dereference of a non-reference type '%s'",
                 u->operand->type->name->str());
      return nullptr;
    }
    u->type = u->operand->type->base_type;
    break;
  case UnaryExprKind::ref:
    visitExpr(u->operand);
    // XXX: arbitrary
    if (!u->operand->type)
      return nullptr;

    // taking address of an rvalue is prohibited
    // TODO: proper l-value check
    if (!u->operand->decl()) {
      sema.error(u->operand->pos, "cannot take address of an rvalue");
      return nullptr;
    }
    u->type = get_reference_type(sema, u->operand->type);
    break;
  default:
    assert(false);
    break;
  }

  return u->type;
}

Type *TypeChecker::visitParenExpr(ParenExpr *p) {
  walk_paren_expr(*this, p);

  if (!p->operand->type)
    return nullptr;

  p->type = p->operand->type;
  return p->type;
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
        // And since we are currently doing single-pass (TODO), its type should
        // also be resolved by now.
        t->type = *decl_get_type(t->decl);
        assert(t->type && "type not resolved in corresponding *Decl");
    } else if (t->kind == TypeExprKind::ref) {
        // Derived types are only present in the type table if they occur in the
        // source code.  Trying to push them every time we see one is sufficient
        // to keep this invariant.
        assert(t->subexpr->type);
        if (auto found = sema.type_table.find(t->name)) {
            t->type = found->value;
        } else {
            t->type = get_reference_type(sema, t->subexpr->type);
        }
    } else {
        assert(false && "whooops");
    }

    return t->type;
}

Type *TypeChecker::visitVarDecl(VarDeclNode *v) {
    walk_var_decl(*this, v);

    // assert(!!v->mut && "TODO");

    if (v->type_expr) {
        // XXX: maybe we can use just assertions, rather than if-not-returns,
        // because typecheck of type_expr always succeeds because namebinding
        // handles all of the type errors for them?
        assert(v->type_expr->type);
        v->var_decl->type = v->type_expr->type;
    } else if (v->assign_expr) {
        assert(v->assign_expr->type);
        v->var_decl->type = v->assign_expr->type;
    } else {
        assert(false && "unreachable");
    }

    return v->var_decl->type;
}

Type *TypeChecker::visitFuncDecl(FuncDeclNode *f) {
  if (f->failed) return nullptr;
  auto err_count = sema.errors.size();

  // We need to do return type typecheck before walking the body, so we can't
  // use the generic walk_func_decl function here.

  if (f->ret_type_expr) visitExpr(f->ret_type_expr);
  for (auto arg : f->args)
    visitDecl(arg);

  if (f->ret_type_expr) {
    // XXX: confusing flow
    if (!f->ret_type_expr->type) return nullptr;
    f->func_decl->ret_ty = f->ret_type_expr->type;
  } else {
    f->func_decl->ret_ty = sema.context.void_type;
  }

  // FIXME: what about type_table?
  sema.context.func_decl_stack.push_back(f->func_decl);
  visitCompoundStmt(f->body);
  sema.context.func_decl_stack.pop_back();

  if (sema.errors.size() != err_count) {
    f->failed = true;
  }

  // FIXME: necessary?
  return f->func_decl->ret_ty;
}

Type *TypeChecker::visitStructDecl(StructDeclNode *s) {
    s->struct_decl->type = make_value_type(sema, s->name, s->struct_decl);

    // Do pre-order walk so that recursive struct definitions are possible.
    walk_struct_decl(*this, s);

    for (auto field : s->struct_decl->fields) {
        if (!field->type) return nullptr;

        // Containing one or more non-copyable field makes the struct
        // non-copyable as well.  For example, a struct that contains a mutable
        // reference as its field cannot be trivially copied in the same
        // lexical scope.
        if (!field->type->copyable) {
            s->struct_decl->type->copyable = false;
        }
    }
    return s->struct_decl->type;
}

Type *TypeChecker::visitEnumVariantDecl(EnumVariantDeclNode *v) {
  // Create a new type for this struct.
  auto type = make_value_type(sema, v->name, v->struct_decl);
  v->struct_decl->type = type;

  // This is a pre-order walk so that recursive struct definitions are made
  // possible.
  walk_enum_variant_decl(*this, v);

  return v->struct_decl->type;
}

Type *TypeChecker::visitEnumDecl(EnumDeclNode *e) {
  auto type = make_value_type(sema, e->name, e->enum_decl);
  e->enum_decl->type = type;

  // This is a pre-order walk so that recursive enum definitions are made
  // possible.
  walk_enum_decl(*this, e);

  return e->enum_decl->type;
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
  auto if_branch_start = sema.make_basic_block();
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
    auto else_branch_start = sema.make_basic_block();
    bb->succ.push_back(else_branch_start);
    else_branch_start->pred.push_back(bb);
    else_branch_end = visitCompoundStmt(is->else_body, else_branch_start);
  }

  auto exit_point = sema.make_basic_block();
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

BasicBlock *ReturnChecker::visitFuncDecl(FuncDeclNode *f, BasicBlock *bb) {
  if (f->failed) return nullptr;
  if (!f->ret_type_expr) return nullptr;

  auto entry_point = sema.make_basic_block();
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
void register_borrow(Sema &sema, VarDecl *borrowee, size_t borrowee_pos) {
    int old_borrow_count = 0;
    auto found = sema.borrow_table.find(borrowee->name);
    if (found) {
        old_borrow_count = found->value.borrow_count;
    }

    if (old_borrow_count > 0) {
        sema.error(borrowee_pos, "cannot borrow '%s' more than once",
                   borrowee->name->str());
        return;
    }

    sema.borrow_table.insert(borrowee->name,
                             BorrowMap{borrowee, old_borrow_count + 1});
}

// Checks if 'expr' starts with '&'.
bool is_ref_expr(const Expr *expr) {
    return expr->kind == ExprKind::unary &&
           expr->as<UnaryExpr>()->kind == UnaryExprKind::ref;
}

void BorrowChecker::visitAssignStmt(AssignStmt *as) {
    walk_assign_stmt(*this, as);

    if (is_ref_expr(as->rhs)) {
        // check multiple borrowing rule
        // TODO: mutable and immutable
        auto rhs_deref = as->rhs->as<UnaryExpr>()->operand;
        assert(as->lhs->decl().has_value());
        assert(rhs_deref->decl().has_value());
        auto lhs_decl = declCast<VarDecl>(*as->lhs->decl());
        auto rhs_deref_decl = declCast<VarDecl>(*rhs_deref->decl());
        lhs_decl->borrowee = rhs_deref_decl;
    }
}

// Rule: a variable of lifetime 'a should only refer to a variable whose
// lifetime is larger than 'a.
// In other words, at the point of use, the borrowee should be alive.
void BorrowChecker::visitDeclRefExpr(DeclRefExpr *d) {
    if (!decl_is<VarDecl>(d->decl)) return;
    auto var = declCast<VarDecl>(d->decl);

    // at each use of a reference variable, check if its borrowee is alive
    if (var->borrowee) {
        // TODO: refactor into find_exact()
        auto sym = sema.live_list.find(var->borrowee->name);
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
        if (!is_ref_expr(desig.expr)) continue;
        
        // FIXME: touching desig.decl might be pointless
        auto rhs_deref = desig.expr->as<UnaryExpr>()->operand;
        desig.decl->borrowee = declCast<VarDecl>(*rhs_deref->decl());
    }
}

void BorrowChecker::visitUnaryExpr(UnaryExpr *u) {
    switch (u->kind) {
    case UnaryExprKind::paren:
        visitParenExpr(static_cast<ParenExpr *>(u));
        break;
    case UnaryExprKind::ref:
        visitExpr(u->operand);
        register_borrow(sema, declCast<VarDecl>(*u->operand->decl()), u->pos);
        break;
    case UnaryExprKind::deref:
        visitExpr(u->operand);
        break;
    default:
        assert(false);
        break;
    }
}

void BorrowChecker::visitVarDecl(VarDeclNode *v) {
    walk_var_decl(*this, v);

    sema.live_list.insert(v->name, v->var_decl);

    if (v->assign_expr && is_ref_expr(v->assign_expr)) {
        // store borrowee relationship
        auto rhs_deref = v->assign_expr->as<UnaryExpr>()->operand;
        v->var_decl->borrowee = declCast<VarDecl>(*rhs_deref->decl());
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
    visitExpr(desig.expr);
    emitCont(", ");
  }
  emitCont("}}");
}

void CodeGenerator::visitMemberExpr(MemberExpr *m) {
  visitExpr(m->lhs_expr);
  emitCont(".");
  emitCont("{}", m->member_name->str());
}

void CodeGenerator::visitUnaryExpr(UnaryExpr *u) {
  switch (u->kind) {
  case UnaryExprKind::paren:
    return visitParenExpr(u->as<ParenExpr>());
    break;
  case UnaryExprKind::ref:
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
        auto base = cStringify(t->base_type);
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

void CodeGenerator::visitVarDecl(VarDeclNode *v) {
  if (v->kind == VarDeclNodeKind::param) {
    emit("{} {}", cStringify(v->var_decl->type), v->name->str());
  } else {
    emit("{} {};\n", cStringify(v->var_decl->type), v->name->str());

    if (v->assign_expr) {
      emit("{} = ", v->name->str());
      visitExpr(v->assign_expr);
      emitCont(";\n");
    }
  }
}

void CodeGenerator::visitStructDecl(StructDeclNode *s) {
  emit("typedef struct {} {{\n", s->name->str());
  {
    IndentBlock ib{*this};
    for (auto memb : s->members)
      visitDecl(memb);
  }
  emit("}} {};\n", s->name->str());
  emit("\n");
}

void CodeGenerator::visitFuncDecl(FuncDeclNode *f) {
  if (f->failed) return;

  if (f->ret_type_expr)
    emit("{}", cStringify(f->func_decl->ret_ty));
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

} // namespace cmp
