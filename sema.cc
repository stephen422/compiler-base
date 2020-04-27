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

void push_builtin_type_from_name(Sema &s, const std::string &str) {
    Name *name = s.names.getOrAdd(str);
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

void Sema::report() const {
    for (auto e : errors) {
        fmt::print("{}\n", e.str());
    }
}

// See comments for cmp::verify().
bool Sema::verify() const {
    return cmp::verify(source.filename, errors, beacons);
}

void NameBinder::visitCompoundStmt(CompoundStmt *cs) {
    sema.decl_table.scope_open();
    walk_compound_stmt(*this, cs);
    sema.decl_table.scope_close();
}

void NameBinder::visitDeclRefExpr(DeclRefExpr *d) {
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
void NameBinder::visitFuncCallExpr(FuncCallExpr *f) {
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
    if (sym && decl_is<StructDecl *>(sym->value)) {
        assert(t->kind == TypeExprKind::value);
        t->decl = sym->value;
    } else {
        sema.error(t->pos,
                   fmt::format("use of undeclared type '{}'", t->name->str()));
        return;
    }
}

void NameBinder::visitVarDecl(VarDeclNode *v) {
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
    } else if (v->kind == VarDeclNode::Kind::param) {
        assert(!sema.context.func_decl_stack.empty());
        auto curr_func = sema.context.func_decl_stack.back();
        curr_func->args.push_back(v->var_decl);
    }
}

void NameBinder::visitStructDecl(StructDeclNode *s) {
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

void NameBinder::visitFuncDecl(FuncDeclNode *f) {
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

// Assignments should check that the LHS is an l-value.
// This check cannot be done reliably in the parsing stage because it depends
// on the actual type of the expression, not just its kind; e.g. (v) or (3).
//
//                 3 = 4
void TypeChecker::visitAssignStmt(AssignStmt *as) {
    walk_assign_stmt(*this, as);

    auto lhsTy = as->lhs->type;
    auto rhsTy = as->rhs->type;

    // XXX: is this the best way to early-exit?
    if (!lhsTy || !rhsTy)
        return;

    // L-value check.
    // XXX: It's not obvious that r-values correspond to expressions that don't
    // have an associated Decl object. Maybe make an isRValue() function.
    if (!as->lhs->decl()) {
        sema.error(as->pos, fmt::format("LHS is not assignable"));
        return;
    }

    // Only allow exact equality for assignment for now (TODO).
    if (lhsTy != rhsTy)
        sema.error(as->pos,
                   fmt::format("cannot assign '{}' type to '{}'",
                               rhsTy->name->str(), lhsTy->name->str()));
}

void TypeChecker::visitReturnStmt(ReturnStmt *rs) {
  walk_return_stmt(*this, rs);
  if (!rs->expr->type)
    return;

  assert(!sema.context.func_decl_stack.empty());
  auto func_decl = sema.context.func_decl_stack.back();
  if (func_decl->is_void()) {
    sema.error(rs->expr->pos,
               fmt::format("function '{}' should not return a value",
                           func_decl->name->str()));
    return;
  }

  if (rs->expr->type != func_decl->return_type) {
    sema.error(
        rs->expr->pos,
        fmt::format("return type mismatch: function returns '{}', but got '{}'",
                    func_decl->return_type->name->str(),
                    rs->expr->type->name->str()));
    return;
  }
}

void TypeChecker::visitIntegerLiteral(IntegerLiteral *i) {
    auto int_name = sema.names.get("int");
    auto int_decl =
        get<StructDecl *>(sema.decl_table.find(int_name)->value);
    i->type = int_decl->type;
    assert(i->type);
}

void TypeChecker::visitStringLiteral(StringLiteral *s) {
    // TODO
}

void TypeChecker::visitDeclRefExpr(DeclRefExpr *d) {
    // Since there is no type inference now, the type is determined at the same
    // time the variable is declared. So if a variable succeeded namebinding,
    // its type is guaranteed to be determined.
    //
    // @future: Currently only handles VarDecls.
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
void TypeChecker::visitFuncCallExpr(FuncCallExpr *f) {
    walk_func_call_expr(*this, f);

    assert(f->func_decl->return_type);
    f->type = f->func_decl->return_type;

    // check argument type match
    for (size_t i = 0; i < f->func_decl->args.size(); i++) {
        assert(f->args[i]->type);
        // TODO: proper type comparison
        if (f->args[i]->type != f->func_decl->args[i]->type) {
            sema.error(
                f->args[i]->pos,
                fmt::format("argument type mismatch: expects '{}', got '{}'",
                            f->func_decl->args[i]->type->name->str(),
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
    if (!m->struct_expr->type)
        return;

    // make sure the LHS is actually a struct
    auto lhs_type = m->struct_expr->type;
    if (!lhs_type->is_struct()) {
        sema.error(m->struct_expr->pos, fmt::format("type '{}' is not a struct",
                                                    lhs_type->name->str()));
        return;
    }

    // find a member with the same name
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

// Get or make a reference type of a given type.
Type *getReferenceType(Sema &sema, Type *type) {
    Name *name = sema.names.getOrAdd("*" + type->name->text);
    if (auto found = sema.type_table.find(name))
        return found->value;

    // FIXME: scope_level
    auto refTy =
        sema.make_type(TypeKind::ref, name, type, nullptr, true /*FIXME*/);
    return *sema.type_table.insert(name, refTy);
}

void TypeChecker::visitUnaryExpr(UnaryExpr *u) {
    switch (u->unary_kind) {
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

    if (b->lhs->type != b->rhs->type)
        sema.error(
            b->pos,
            fmt::format(
                "incompatible types to binary expression ('{}' and '{}')",
                b->lhs->type->name->str(), b->rhs->type->name->str()));
}

// Type checking TypeExpr concerns with finding the Type object whose syntactic
// representation matches the TypeExpr.
void TypeChecker::visitTypeExpr(TypeExpr *t) {
    walk_type_expr(*this, t);

    // first, find a type that has this exact name
    if (t->kind == TypeExprKind::value) {
        // t->decl should be non-null after the name binding stage.
        // And since we are currently doing single-pass (TODO), its type should
        // also be resolved by now.
        assert(decl_is<StructDecl *>(t->decl));
        t->type = get<StructDecl *>(t->decl)->type;
        assert(t->type);
    } else if (t->kind == TypeExprKind::ref) {
        // Derived types are only present in the type table if they occur in
        // the source code.  Trying to push them every time we see one is
        // sufficient to keep this invariant.
        assert(t->subexpr->type);
        if (auto found = sema.type_table.find(t->name)) {
            t->type = found->value;
        } else {
            t->type = sema.make_type(TypeKind::ref, t->name, t->subexpr->type,
                                     nullptr, true /*FIXME*/);
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

void TypeChecker::visitStructDecl(StructDeclNode *s) {
    walk_struct_decl(*this, s);

    // Create a new type for this struct.
    auto type =
        sema.make_type(TypeKind::value, s->name, nullptr, s->struct_decl, true);
    // XXX: no need to insert to type table?
    s->struct_decl->type = type;
}

void TypeChecker::visitFuncDecl(FuncDeclNode *f) {
    // We need to do return type typecheck before walking the body, so we can't
    // use the generic walk_func_decl function here.

    if (f->retTypeExpr)
        visitExpr(f->retTypeExpr);
    for (auto arg : f->args)
        visitDecl(arg);

    if (f->retTypeExpr) {
        if (!f->retTypeExpr->type)
            return;
        f->func_decl->return_type = f->retTypeExpr->type;
    }

    // FIXME: what about type_table?
    sema.context.func_decl_stack.push_back(f->func_decl);
    visitCompoundStmt(f->body);
    sema.context.func_decl_stack.pop_back();
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

void CodeGenerator::visitIntegerLiteral(IntegerLiteral *i) {
  emitCont("{}", i->value);
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

void CodeGenerator::visitBinaryExpr(BinaryExpr *b) {
  visitExpr(b->lhs);
  emitCont(" {} ", b->op.str());
  visitExpr(b->rhs);
}

void CodeGenerator::visitTypeExpr(TypeExpr *t) {
  emitCont("{}", t->type->name->str());
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

void CodeGenerator::visitReturnStmt(ReturnStmt *r) {
  emit("return ");
  visitExpr(r->expr);
  emitCont(";\n");
}

void CodeGenerator::visitVarDecl(VarDeclNode *v) {
  emit("{} {};\n", v->var_decl->type->name->str(), v->name->str());
  if (v->assign_expr) {
    emit("{} = ", v->name->str());
    visitExpr(v->assign_expr);
    emitCont(";\n");
  }
}

void CodeGenerator::visitFuncDecl(FuncDeclNode *f) {
  if (f->retTypeExpr)
    visitExpr(f->retTypeExpr);
  else
    emit("void");

  emit(" {}(", f->name->str());
  for (auto arg : f->args)
    visitDecl(arg);
  emitCont(") {{\n");

  {
    IndentBlock ib{*this};
    visitCompoundStmt(f->body);
  }

  emit("}}\n");
  emit("\n");
}

} // namespace cmp
