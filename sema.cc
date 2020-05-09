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
    errors.push_back(e);
}

Type *make_value_type(Sema &sema, Name *n, Type::TypeDecl decl) {
    Type *t = new Type(TypeKind::value, n, nullptr, decl);
    sema.type_pool.push_back(t);
    return t;
}

Type *make_ref_type(Sema &sema, Name *n, Type *base_type) {
    Type *t = new Type(TypeKind::ref, n, base_type, std::monostate{});
    sema.type_pool.push_back(t);
    return t;
}

Type *push_builtin_type_from_name(Sema &s, const std::string &str) {
  Name *name = s.name_table.get_or_add(str);
  auto struct_decl = s.make_decl<StructDecl>(name);
  struct_decl->type = make_value_type(s, name, std::monostate{});
  s.decl_table.insert(name, struct_decl);
  return struct_decl->type;
}

// Push Decls for the builtin types into the global scope of decl_table, so
// that they are visible from any point in the AST.
void setup_builtin_types(Sema &s) {
    s.context.voidTy = push_builtin_type_from_name(s, "void");
    s.context.intTy = push_builtin_type_from_name(s, "int");
    s.context.charTy = push_builtin_type_from_name(s, "char");
    s.context.stringTy = push_builtin_type_from_name(s, "string");
}

Sema::Sema(Parser &p) : Sema(p.lexer.source(), p.names, p.errors, p.beacons) {}

Sema::~Sema() {
    for (auto d : decl_pool)
        delete d;
    for (auto t : type_pool)
        delete t;
    for (auto b : bb_pool)
        delete b;
}

void Sema::scope_open() {
    decl_table.scope_open();
    type_table.scope_open();
}

void Sema::scope_close() {
    decl_table.scope_close();
    type_table.scope_close();
}

// Return 'type' member of Decl, or nullptr if this Decl kind doesn't have any.
std::optional<Type *> decl_get_type(const Decl decl) {
    if (auto d = decl_as<VarDecl>(decl)) {
        return d->type;
    } else if (auto d = decl_as<StructDecl>(decl)) {
        return d->type;
    } else if (auto d = decl_as<EnumDecl>(decl)) {
        return d->type;
    } else if (auto d = decl_as<FuncDecl>(decl)) {
        return {};
    }
    assert(false && "not all decl kinds handled");
}

void NameBinder::visitCompoundStmt(CompoundStmt *cs) {
    sema.decl_table.scope_open();
    walk_compound_stmt(*this, cs);
    sema.decl_table.scope_close();
}

void NameBinder::visitDeclRefExpr(DeclRefExpr *d) {
    auto sym = sema.decl_table.find(d->name);
    if (!sym) {
        sema.error(d->pos, fmt::format("use of undeclared identifier '{}'",
                                       d->name->str()));
        return;
    }
    if (auto vd = decl_as<VarDecl>(sym->value)) {
        d->kind = DeclRefKind::var;
        d->var_decl = vd;
    } else if (auto ed = decl_as<EnumDecl>(sym->value)) {
        d->kind = DeclRefKind::enum_;
        d->enum_decl = ed;
    } else {
        assert(false && "not implemented");
    }
}

// Only binds the function name part of the call, e.g. 'func' of func().
void NameBinder::visitFuncCallExpr(FuncCallExpr *f) {
    // resolve function name
    auto sym = sema.decl_table.find(f->funcName);
    if (!sym) {
        sema.error(f->pos, fmt::format("undeclared function '{}'",
                                       f->funcName->str()));
        return;
    }
    if (!decl_as<FuncDecl>(sym->value)) {
        sema.error(f->pos,
                   fmt::format("'{}' is not a function", f->funcName->str()));
        return;
    }
    f->funcDecl = decl_as<FuncDecl>(sym->value);
    assert(f->funcDecl);

    walk_func_call_expr(*this, f);

    // check if argument count matches
    if (f->funcDecl->argsCount() != f->args.size()) {
        sema.error(f->pos,
                   fmt::format("'{}' accepts {} arguments, got {}",
                               f->funcName->str(), f->funcDecl->argsCount(),
                               f->args.size()));
    }
}

void NameBinder::visitTypeExpr(TypeExpr *t) {
    walk_type_expr(*this, t);

    // Namebinding for TypeExprs only include linking existing Decls to the
    // type names used in the expression, not declaring new ones.  The
    // declaration would be done when visiting VarDecls and StructDecls, etc.

    // For pointers and arrays, proper typechecking will be done in the later
    // stages.
    if (t->subexpr)
        return;

    auto sym = sema.decl_table.find(t->name);
    if (sym && decl_get_type(sym->value)) {
        assert(t->kind == TypeExprKind::value);
        t->decl = sym->value;
    } else {
        sema.error(t->pos,
                   fmt::format("use of undeclared type '{}'", t->name->str()));
        return;
    }
}

// Semantically declare a 'name' at a 'pos', whose Decl type is T.
// Returns nullptr if declaration failed due to e.g. redeclaration.
template <typename T> T *declare(Sema &sema, Name *name, size_t pos) {
  auto found = sema.decl_table.find(name);
  if (found && decl_as<T>(found->value) &&
      found->scope_level <= sema.decl_table.scope_level) {
    sema.error(pos, fmt::format("redefinition of '{}'", name->str()));
    return nullptr;
  }

  T *decl = sema.make_decl<T>(name);
  // binding of name and decl
  sema.decl_table.insert(name, decl);

  return decl;
}

void NameBinder::visitVarDecl(VarDeclNode *v) {
  walk_var_decl(*this, v);

  v->var_decl = declare<VarDecl>(sema, v->name, v->pos);
  if (!v->var_decl)
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
  f->func_decl = declare<FuncDecl>(sema, f->name, f->pos);
  if (!f->func_decl)
    return;

  sema.decl_table.scope_open(); // for argument variables
  sema.context.func_decl_stack.push_back(f->func_decl);

  walk_func_decl(*this, f);

  sema.context.func_decl_stack.pop_back();
  sema.decl_table.scope_close();
}

void NameBinder::visitStructDecl(StructDeclNode *s) {
  s->struct_decl = declare<StructDecl>(sema, s->name, s->pos);
  if (!s->struct_decl)
    return;

  // Decl table is used for checking redefinition when parsing the member
  // list.
  sema.decl_table.scope_open();
  sema.context.struct_decl_stack.push_back(s->struct_decl);

  walk_struct_decl(*this, s);

  sema.context.struct_decl_stack.pop_back();
  sema.decl_table.scope_close();
}

// Generate a name for the anonymous fields in each enum variant structs.
// For now, these are named as "_0", "_1", etc.
static Name *gen_anonymous_field_name(Sema &sema, size_t index) {
  auto text = fmt::format("_{}", index);
  return sema.name_table.get_or_add(text);
}

void NameBinder::visitEnumVariantDecl(EnumVariantDeclNode *v) {
  walk_enum_variant_decl(*this, v);

  // first, declare a struct of this name
  v->struct_decl = declare<StructDecl>(sema, v->name, v->pos);
  if (!v->struct_decl)
    return;

  // then, add fields to this struct, whose names are anonymous
  // and only types are specified (e.g. Pos(int, int))
  for (size_t i = 0; i < v->fields.size(); i++) {
    // so that anonymous field names don't clash
    sema.decl_table.scope_open();

    // TODO cast should be removed in the future
    auto field_decl = declare<VarDecl>(sema, gen_anonymous_field_name(sema, i),
                                       v->fields[i]->pos);
    if (!field_decl)
      return;

    v->struct_decl->fields.push_back(field_decl);

    sema.decl_table.scope_close();
  }

  // now, add this struct into the scope of the enum
  assert(!sema.context.enum_decl_stack.empty());
  auto enclosing_enum = sema.context.enum_decl_stack.back();
  enclosing_enum->variants.push_back(v->struct_decl);
}

void NameBinder::visitEnumDecl(EnumDeclNode *e) {
  e->enum_decl = declare<EnumDecl>(sema, e->name, e->pos);
  if (!e->enum_decl)
    return;

  sema.decl_table.scope_open();
  sema.context.enum_decl_stack.push_back(e->enum_decl);

  walk_enum_decl(*this, e);

  sema.context.enum_decl_stack.pop_back();
  sema.decl_table.scope_close();
}

// Assignments should check that the LHS is an l-value.
// This check cannot be done reliably in the parsing stage because it depends
// on the actual type of the expression, not just its kind; e.g. (v) or (3).
//
//                 3 = 4
void TypeChecker::visitAssignStmt(AssignStmt *as) {
  walk_assign_stmt(*this, as);

  auto lhs_ty = as->lhs->type;
  auto rhs_ty = as->rhs->type;

  // XXX: is this the best way to early-exit?
  if (!lhs_ty || !rhs_ty)
    return;

  // L-value check.
  // XXX: It's not obvious that r-values correspond to expressions that don't
  // have an associated Decl object. Maybe make an isRValue() function.
  if (!as->lhs->decl()) {
    sema.error(as->pos, fmt::format("LHS is not assignable"));
    return;
  }

  // Only allow exact equality for assignment for now (TODO).
  if (lhs_ty != rhs_ty)
    sema.error(as->pos, fmt::format("cannot assign '{}' type to '{}'",
                                    rhs_ty->name->str(), lhs_ty->name->str()));
}

bool FuncDecl::isVoid(Sema &sema) const {
  return retTy == sema.context.voidTy;
}

void TypeChecker::visitReturnStmt(ReturnStmt *rs) {
  walk_return_stmt(*this, rs);
  if (!rs->expr->type)
    return;

  assert(!sema.context.func_decl_stack.empty());
  auto func_decl = sema.context.func_decl_stack.back();
  if (func_decl->isVoid(sema)) {
    sema.error(rs->expr->pos,
               fmt::format("function '{}' should not return a value",
                           func_decl->name->str()));
    return;
  }

  if (rs->expr->type != func_decl->retTy) {
    sema.error(
        rs->expr->pos,
        fmt::format("return type mismatch: function returns '{}', but got '{}'",
                    func_decl->retTy->name->str(),
                    rs->expr->type->name->str()));
    return;
  }
}

void TypeChecker::visitIntegerLiteral(IntegerLiteral *i) {
  i->type = sema.context.intTy;
}

void TypeChecker::visitStringLiteral(StringLiteral *s) {
  s->type = sema.context.stringTy;
}

void TypeChecker::visitDeclRefExpr(DeclRefExpr *d) {
  // For varibles, since there is no type inference now, the type is determined
  // at the same time the variable is declared. So if a variable succeeded
  // namebinding, its type is guaranteed to be determined.
  //
  // For struct and enum names, they are not handled in the namebinding stage
  // and so should be taken care of here.
  if (d->kind == DeclRefKind::var) {
    assert(d->var_decl->type);
    d->type = d->var_decl->type;
  } else if (d->kind == DeclRefKind::enum_) {
    assert(d->enum_decl->type);
    d->type = d->enum_decl->type;
  } else {
    assert(false);
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
void TypeChecker::visitFuncCallExpr(FuncCallExpr *f) {
  walk_func_call_expr(*this, f);

  assert(f->funcDecl->retTy);
  f->type = f->funcDecl->retTy;

  // check argument type match
  for (size_t i = 0; i < f->funcDecl->args.size(); i++) {
    assert(f->args[i]->type);
    // TODO: proper type comparison
    if (f->args[i]->type != f->funcDecl->args[i]->type) {
      sema.error(f->args[i]->pos,
                 fmt::format("argument type mismatch: expects '{}', got '{}'",
                             f->funcDecl->args[i]->type->name->str(),
                             f->args[i]->type->name->str()));
    }
  }
}

// MemberExprs cannot be namebinded completely without type checking (e.g.
// func().mem).  So we defer their namebinding to the type checking phase,
// which is done here.
void TypeChecker::visitMemberExpr(MemberExpr *m) {
  // propagate typecheck from left to right (struct -> .mem)
  walk_member_expr(*this, m);

  // if the struct side failed to typecheck, we cannot proceed
  if (!m->lhs_expr->type)
    return;

  // make sure the LHS is actually a struct
  auto lhs_type = m->lhs_expr->type;
  if (!lhs_type->is_member_accessible()) {
    sema.error(m->lhs_expr->pos,
               fmt::format("type '{}' is not a struct", lhs_type->name->str()));
    return;
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
    sema.error(m->lhs_expr->pos,
               fmt::format("'{}' is not a member of '{}'",
                           m->member_name->str(), lhs_type->name->str()));
    return;
  }

  // the fields are already typechecked
  assert(decl_get_type(m->decl));
  m->type = *decl_get_type(m->decl);
}

// Get or make a reference type of a given type.
Type *getReferenceType(Sema &sema, Type *type) {
  Name *name = sema.name_table.get_or_add("*" + type->name->text);
  if (auto found = sema.type_table.find(name))
    return found->value;

  // FIXME: scope_level
  auto refTy = make_ref_type(sema, name, type);
  return *sema.type_table.insert(name, refTy);
}

void TypeChecker::visitUnaryExpr(UnaryExpr *u) {
    switch (u->unaryKind) {
    case UnaryExprKind::paren:
        visitParenExpr(static_cast<ParenExpr *>(u));
        break;
    case UnaryExprKind::deref:
        visitExpr(u->operand);
        // XXX: arbitrary
        if (!u->operand->type)
            return;

        if (u->operand->type->kind != TypeKind::ref) {
            sema.error(u->operand->pos,
                       fmt::format("dereference of a non-reference type '{}'",
                                   u->operand->type->name->str()));
            return;
        }
        u->type = u->operand->type->base_type;
        break;
    case UnaryExprKind::address:
        visitExpr(u->operand);
        // XXX: arbitrary
        if (!u->operand->type)
            return;

        // taking address of an rvalue is prohibited
        // TODO: proper l-value check
        if (!u->operand->decl()) {
            sema.error(u->operand->pos, "cannot take address of an rvalue");
            return;
        }
        u->type = getReferenceType(sema, u->operand->type);
        break;
    default:
        assert(false);
        break;
    }
}

void TypeChecker::visitParenExpr(ParenExpr *p) {
    walk_paren_expr(*this, p);

    if (!p->operand->type)
        return;

    p->type = p->operand->type;
}

void TypeChecker::visitBinaryExpr(BinaryExpr *b) {
  walk_binary_expr(*this, b);

  if (!b->lhs->type || !b->rhs->type)
    return;

  if (b->lhs->type != b->rhs->type) {
    sema.error(
        b->pos,
        fmt::format("incompatible types to binary expression ('{}' and '{}')",
                    b->lhs->type->name->str(), b->rhs->type->name->str()));
    return;
  }

  b->type = b->lhs->type;
}

// Type checking TypeExpr concerns with finding the Type object whose syntactic
// representation matches the TypeExpr.
void TypeChecker::visitTypeExpr(TypeExpr *t) {
  walk_type_expr(*this, t);

  if (t->kind == TypeExprKind::value) {
    // t->decl should be non-null after the name binding stage.
    // And since we are currently doing single-pass (TODO), its type should
    // also be resolved by now.
    t->type = *decl_get_type(t->decl);
    assert(t->type && "type not resolved in corresponding *Decl");
  } else if (t->kind == TypeExprKind::ref) {
    // Derived types are only present in the type table if they occur in the
    // source code.  Trying to push them every time we see one is sufficient to
    // keep this invariant.
    assert(t->subexpr->type);
    if (auto found = sema.type_table.find(t->name)) {
      t->type = found->value;
    } else {
      t->type = make_ref_type(sema, t->name, t->subexpr->type);
      sema.type_table.insert(t->name, t->type);
    }
  } else {
    assert(false && "whooops");
  }
}

void TypeChecker::visitVarDecl(VarDeclNode *v) {
    walk_var_decl(*this, v);

    if (v->type_expr) {
        v->var_decl->type = v->type_expr->type;
        assert(v->var_decl->type);
    } else if (v->assign_expr) {
        v->var_decl->type = v->assign_expr->type;
    } else {
        assert(false && "unreachable");
    }
}

void TypeChecker::visitFuncDecl(FuncDeclNode *f) {
  // We need to do return type typecheck before walking the body, so we can't
  // use the generic walk_func_decl function here.

  if (f->retTypeExpr)
    visitExpr(f->retTypeExpr);
  for (auto arg : f->args)
    visitDecl(arg);

  if (f->retTypeExpr) {
    // XXX: confusing flow
    if (!f->retTypeExpr->type)
      return;
    f->func_decl->retTy = f->retTypeExpr->type;
  } else {
    f->func_decl->retTy = sema.context.voidTy;
  }

  // FIXME: what about type_table?
  sema.context.func_decl_stack.push_back(f->func_decl);
  visitCompoundStmt(f->body);
  sema.context.func_decl_stack.pop_back();
}

void TypeChecker::visitStructDecl(StructDeclNode *s) {
  // Create a new type for this struct.
  auto type = make_value_type(sema, s->name, s->struct_decl);
  s->struct_decl->type = type;

  // This is a pre-order walk so that recursive struct definitions are made
  // possible.
  walk_struct_decl(*this, s);
}

void TypeChecker::visitEnumVariantDecl(EnumVariantDeclNode *v) {
  // Create a new type for this struct.
  auto type = make_value_type(sema, v->name, v->struct_decl);
  v->struct_decl->type = type;

  // This is a pre-order walk so that recursive struct definitions are made
  // possible.
  walk_enum_variant_decl(*this, v);
}

void TypeChecker::visitEnumDecl(EnumDeclNode *e) {
  auto type = make_value_type(sema, e->name, e->enum_decl);
  e->enum_decl->type = type;

  // This is a pre-order walk so that recursive enum definitions are made
  // possible.
  walk_enum_decl(*this, e);
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
  auto ifBranchStart = sema.make_basic_block();
  bb->succ.push_back(ifBranchStart);
  ifBranchStart->pred.push_back(bb);
  auto ifBranchEnd = visitCompoundStmt(is->if_body, ifBranchStart);

  auto elseBranchEnd = bb;
  if (is->else_if) {
    // We could make a new empty basic block here, which would make this CFG a
    // binary graph; or just pass in 'bb', which will make 'bb' have more than
    // two successors.
    elseBranchEnd = visitIfStmt(is->else_if, bb);
  } else if (is->else_body) {
    auto elseBranchStart = sema.make_basic_block();
    bb->succ.push_back(elseBranchStart);
    elseBranchStart->pred.push_back(bb);
    elseBranchEnd = visitCompoundStmt(is->else_body, elseBranchStart);
  }

  auto exitPoint = sema.make_basic_block();
  ifBranchEnd->succ.push_back(exitPoint);
  elseBranchEnd->succ.push_back(exitPoint);
  exitPoint->pred.push_back(ifBranchEnd);
  exitPoint->pred.push_back(elseBranchEnd);

  return exitPoint;
}

// Do the iterative solution for the dataflow analysis.
static void returnCheckSolve(const std::vector<BasicBlock *> &walkList) {
  for (auto bb : walkList)
    bb->returnedSoFar = false;

  bool changed = true;
  while (changed) {
    changed = false;

    for (auto bbI = walkList.rbegin(); bbI != walkList.rend(); bbI++) {
      auto bb = *bbI;

      bool allPredReturns = false;
      if (!bb->pred.empty()) {
        allPredReturns = true;
        for (auto pbb : bb->pred)
          allPredReturns &= pbb->returnedSoFar;
      }

      auto t = bb->returns() || allPredReturns;
      if (t != bb->returnedSoFar) {
        changed = true;
        bb->returnedSoFar = t;
      }
    }
  }
}

BasicBlock *ReturnChecker::visitFuncDecl(FuncDeclNode *f, BasicBlock *bb) {
  if (!f->retTypeExpr)
    return nullptr;

  auto entryPoint = sema.make_basic_block();
  auto exitPoint = visitCompoundStmt(f->body, entryPoint);

  std::vector<BasicBlock *> walkList;
  entryPoint->enumeratePostOrder(walkList);

  // for (auto bb : walkList)
  //   fmt::print("BasicBlock: {} stmts\n", bb->stmts.size());

  returnCheckSolve(walkList);

  if (!exitPoint->returnedSoFar)
    sema.error(f->pos, "function not guaranteed to return a value");

  return nullptr;
}

bool BasicBlock::returns() const {
  for (auto stmt : stmts)
    if (stmt->kind == StmtKind::return_)
      return true;
  return false;
}

void BasicBlock::enumeratePostOrder(std::vector<BasicBlock *> &walkList) {
  if (walked)
    return;

  for (auto s : succ)
    s->enumeratePostOrder(walkList);

  // post-order traversal
  walked = true;
  walkList.push_back(this);
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
  emitCont("{}(", f->funcName->str());

  for (size_t i = 0; i < f->args.size(); i++) {
    visitExpr(f->args[i]);
    if (i != f->args.size() - 1)
      emitCont(", ");
  }

  emitCont(")");
}

void CodeGenerator::visitMemberExpr(MemberExpr *m) {
  visitExpr(m->lhs_expr);
  emitCont(".");
  emitCont("{}", m->member_name->str());
}

void CodeGenerator::visitUnaryExpr(UnaryExpr *u) {
  switch (u->unaryKind) {
  case UnaryExprKind::paren:
    return visitParenExpr(u->as<ParenExpr>());
    break;
  case UnaryExprKind::address:
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
  if (t == sema.context.stringTy) {
    // For now, strings are aliased to char *.  This works as long as strings
    // are immutable and doesn't contain unicode characters.
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
  if (f->retTypeExpr)
    emit("{}", cStringify(f->func_decl->retTy));
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
