#ifndef AST_VISITOR_H
#define AST_VISITOR_H

#include "ast.h"

namespace cmp {

// AST visitor.
//
// Custom visitors, e.g. name binders or type checkers, are expected to inherit
// from this class and override the node visitor functions as necessary.
//
// Curiously-recurring template pattern (CRTP) is used to enable calling the
// visitor function of the derived class from the that of the parent class
// defined here.  See commit log b7e6113.
template <typename Derived, typename RetTy = void, typename... Args>
class AstVisitor {
  // 'Derived this'. By calling visitors and walkers through the return
  // pointer of this function, the base class can access the derived visitor
  // implementations.
  constexpr Derived *dis() { return static_cast<Derived *>(this); }

public:
  //
  // AST visitor functions.
  //
  // These are to be overridden by the specialized visitor classes. They
  // provide a default behavior of simply traversing the node, acting nothing
  // upon them.
  //

  RetTy visitFile(File *f, Args... args) {
    walk_file(*dis(), f, args...);
    return RetTy();
  }
  RetTy visitToplevel(AstNode *a, Args... args) {
    switch (a->kind) {
    case AstKind::stmt:
      return dis()->visitStmt(static_cast<Stmt *>(a), args...);
      break;
    case AstKind::decl:
      return dis()->visitDecl(static_cast<Decl *>(a), args...);
      break;
    default:
      assert(false && "not a toplevel node");
    }
    return RetTy();
  }
  RetTy visitStmt(Stmt *s, Args... args) {
    switch (s->kind) {
    case StmtKind::decl:
      return dis()->visitDeclStmt(static_cast<DeclStmt *>(s), args...);
      break;
    case StmtKind::expr:
      return dis()->visitExprStmt(static_cast<ExprStmt *>(s), args...);
      break;
    case StmtKind::assign:
      return dis()->visitAssignStmt(static_cast<AssignStmt *>(s), args...);
      break;
    case StmtKind::return_:
      return dis()->visitReturnStmt(static_cast<ReturnStmt *>(s), args...);
      break;
    case StmtKind::compound:
      return dis()->visitCompoundStmt(static_cast<CompoundStmt *>(s), args...);
      break;
    case StmtKind::if_:
      return dis()->visitIfStmt(static_cast<IfStmt *>(s), args...);
      break;
    case StmtKind::builtin:
      return dis()->visitBuiltinStmt(static_cast<BuiltinStmt *>(s), args...);
      break;
    case StmtKind::bad:
      // do nothing
      break;
    default:
      assert(false);
    }
    return RetTy();
  }
  RetTy visitDeclStmt(DeclStmt *ds, Args... args) {
    walk_decl_stmt(*dis(), ds, args...);
    return RetTy();
  }
  RetTy visitExprStmt(ExprStmt *es, Args... args) {
    walk_expr_stmt(*dis(), es, args...);
    return RetTy();
  }
  RetTy visitAssignStmt(AssignStmt *as, Args... args) {
    walk_assign_stmt(*dis(), as, args...);
    return RetTy();
  }
  RetTy visitReturnStmt(ReturnStmt *rs, Args... args) {
    walk_return_stmt(*dis(), rs, args...);
    return RetTy();
  }
  RetTy visitCompoundStmt(CompoundStmt *cs, Args... args) {
    walk_compound_stmt(*dis(), cs, args...);
    return RetTy();
  }
  RetTy visitIfStmt(IfStmt *is, Args... args) {
    walk_if_stmt(*dis(), is, args...);
    return RetTy();
  }
  RetTy visitBuiltinStmt(BuiltinStmt *bs, Args... args) {
    // nothing to walk
    return RetTy();
  }
  RetTy visitDecl(Decl *d, Args... args) {
    switch (d->kind) {
    case DeclKind::var:
      return dis()->visitVarDecl(static_cast<VarDecl *>(d), args...);
      break;
    case DeclKind::func:
      return dis()->visitFuncDecl(static_cast<FuncDecl *>(d), args...);
      break;
    case DeclKind::struct_:
      return dis()->visitStructDecl(static_cast<StructDecl *>(d), args...);
      break;
    case DeclKind::enum_variant:
      return dis()->visitEnumVariantDecl(static_cast<EnumVariantDecl *>(d),
                                         args...);
      break;
    case DeclKind::enum_:
      return dis()->visitEnumDecl(static_cast<EnumDecl *>(d), args...);
      break;
    case DeclKind::bad:
      // do nothing
      break;
    default:
      assert(false);
    }
    return RetTy();
  }
  RetTy visitVarDecl(VarDecl *v, Args... args) {
    walk_var_decl(*dis(), v, args...);
    return RetTy();
  }
  RetTy visitFuncDecl(FuncDecl *f, Args... args) {
    walk_func_decl(*dis(), f, args...);
    return RetTy();
  }
  RetTy visitStructDecl(StructDecl *s, Args... args) {
    walk_struct_decl(*dis(), s, args...);
    return RetTy();
  }
  RetTy visitEnumVariantDecl(EnumVariantDecl *e, Args... args) {
    walk_enum_variant_decl(*dis(), e, args...);
    return RetTy();
  }
  RetTy visitEnumDecl(EnumDecl *e, Args... args) {
    walk_enum_decl(*dis(), e, args...);
    return RetTy();
  }
  RetTy visitExpr(Expr *e, Args... args) {
    switch (e->kind) {
    case ExprKind::integer_literal:
      return dis()->visitIntegerLiteral(static_cast<IntegerLiteral *>(e),
                                        args...);
      break;
    case ExprKind::string_literal:
      return dis()->visitStringLiteral(static_cast<StringLiteral *>(e),
                                       args...);
      // do nothing
      break;
    case ExprKind::decl_ref:
      return dis()->visitDeclRefExpr(static_cast<DeclRefExpr *>(e), args...);
      break;
    case ExprKind::func_call:
      return dis()->visitFuncCallExpr(static_cast<FuncCallExpr *>(e), args...);
      break;
    case ExprKind::struct_def:
      return dis()->visitStructDefExpr(static_cast<StructDefExpr *>(e),
                                       args...);
      break;
    case ExprKind::member:
      return dis()->visitMemberExpr(static_cast<MemberExpr *>(e), args...);
      break;
    case ExprKind::unary:
      return dis()->visitUnaryExpr(static_cast<UnaryExpr *>(e), args...);
      break;
    case ExprKind::binary:
      return dis()->visitBinaryExpr(static_cast<BinaryExpr *>(e), args...);
      break;
    case ExprKind::type:
      return dis()->visitTypeExpr(static_cast<TypeExpr *>(e), args...);
      break;
    case ExprKind::bad:
      // do nothing
      break;
    default:
      assert(false);
      break;
    }
    return RetTy();
  }
  RetTy visitIntegerLiteral(IntegerLiteral *i, Args... args) {
    // nothing to walk
    return RetTy();
  }
  RetTy visitStringLiteral(StringLiteral *s, Args... args) {
    // nothing to walk
    return RetTy();
  }
  RetTy visitDeclRefExpr(DeclRefExpr *d, Args... args) {
    // nothing to walk
    return RetTy();
  }
  RetTy visitFuncCallExpr(FuncCallExpr *f, Args... args) {
    walk_func_call_expr(*dis(), f, args...);
    return RetTy();
  }
  RetTy visitStructDefExpr(StructDefExpr *s, Args... args) {
    walk_struct_def_expr(*dis(), s, args...);
    return RetTy();
  }
  RetTy visitMemberExpr(MemberExpr *m, Args... args) {
    walk_member_expr(*dis(), m, args...);
    return RetTy();
  }
  RetTy visitUnaryExpr(UnaryExpr *u, Args... args) {
    switch (u->kind) {
    case UnaryExprKind::paren:
      return dis()->visitParenExpr(static_cast<ParenExpr *>(u), args...);
      break;
    case UnaryExprKind::ref:
    case UnaryExprKind::var_ref:
    case UnaryExprKind::deref:
      return dis()->visitExpr(u->operand, args...);
      break;
    default:
      assert(false && "inexhaustive kind");
      break;
    }
    return RetTy();
  }
  RetTy visitParenExpr(ParenExpr *p, Args... args) {
    walk_paren_expr(*dis(), p, args...);
    return RetTy();
  }
  RetTy visitBinaryExpr(BinaryExpr *b, Args... args) {
    walk_binary_expr(*dis(), b, args...);
    return RetTy();
  }
  RetTy visitTypeExpr(TypeExpr *t, Args... args) {
    walk_type_expr(*dis(), t, args...);
    return RetTy();
  }
};

//
// AST walker functions.
//
// These functions exist to separate the traversal logic from the actual work
// done on each node in the 'visit_...' functions.  This way, the visitor
// functions only have to worry about whether to do work before or after
// walking the subnodes, by simply positioning the walker function in the right
// place.
//
// Polymorphism is required because they should be able to call different
// derived visitor methods.
//
// Note: The functions here combined can be regarded as what the 'accept()'
// function do in the visitor pattern. However, whereas the accept() function
// is declared as virtual in the pattern proper, these need not be polymorphic.
// TODO: Document why.
//

template <typename Visitor, typename... Args>
void walk_file(Visitor &v, File *f, Args... args) {
    for (auto a : f->toplevels) {
        v.visitToplevel(a, args...);
    }
}
template <typename Visitor, typename... Args>
void walk_var_decl(Visitor &v, VarDecl *var, Args... args) {
    if (var->assign_expr) {
        v.visitExpr(var->assign_expr, args...);
    } else if (var->type_expr) {
        v.visitTypeExpr(static_cast<TypeExpr *>(var->type_expr), args...);
    } else {
        assert(false && "unreachable");
    }
}
template <typename Visitor, typename... Args>
void walk_func_decl(Visitor &v, FuncDecl *f, Args... args) {
    if (f->ret_type_expr) v.visitExpr(f->ret_type_expr, args...);
    for (auto arg : f->args)
        v.visitDecl(arg, args...);
    v.visitCompoundStmt(f->body, args...);
}
template <typename Visitor, typename... Args>
void walk_struct_decl(Visitor &v, StructDecl *s, Args... args) {
    for (auto d : s->fields) {
        v.visitDecl(d, args...);
    }
}
template <typename Visitor, typename... Args>
void walk_enum_variant_decl(Visitor &v, EnumVariantDecl *e, Args... args) {
    for (auto f : e->fields)
        v.visitExpr(f, args...);
}
template <typename Visitor, typename... Args>
void walk_enum_decl(Visitor &v, EnumDecl *e, Args... args) {
    for (auto var : e->variants)
        v.visitEnumVariantDecl(var, args...);
}
template <typename Visitor, typename... Args>
void walk_decl_stmt(Visitor &v, DeclStmt *ds, Args... args) {
    v.visitDecl(ds->decl, args...);
}
template <typename Visitor, typename... Args>
void walk_expr_stmt(Visitor &v, ExprStmt *es, Args... args) {
    v.visitExpr(es->expr, args...);
}
template <typename Visitor, typename... Args>
void walk_assign_stmt(Visitor &v, AssignStmt *as, Args... args) {
    v.visitExpr(as->rhs, args...);
    v.visitExpr(as->lhs, args...);
}
template <typename Visitor, typename... Args>
void walk_return_stmt(Visitor &v, ReturnStmt *rs, Args... args) {
    v.visitExpr(rs->expr, args..., args...);
}
template <typename Visitor, typename... Args>
void walk_compound_stmt(Visitor &v, CompoundStmt *cs, Args... args) {
    for (auto s : cs->stmts) {
        v.visitStmt(s, args...);
    }
}
template <typename Visitor, typename... Args>
void walk_if_stmt(Visitor &v, IfStmt *is, Args... args) {
    v.visitExpr(is->cond, args...);
    v.visitCompoundStmt(is->if_body, args...);
    if (is->else_if)
        v.visitIfStmt(is->else_if, args...);
    else if (is->else_body)
        v.visitCompoundStmt(is->else_body, args...);
}
template <typename Visitor, typename... Args>
void walk_func_call_expr(Visitor &v, FuncCallExpr *f, Args... args) {
    for (auto arg : f->args) {
        v.visitExpr(arg, args...);
    }
}
template <typename Visitor, typename... Args>
void walk_struct_def_expr(Visitor &v, StructDefExpr *s, Args... args) {
    v.visitExpr(s->name_expr, args...);
    for (auto d : s->desigs) {
        v.visitExpr(d.init_expr, args...);
    }
}
template <typename Visitor, typename... Args>
void walk_member_expr(Visitor &v, MemberExpr *m, Args... args) {
    v.visitExpr(m->struct_expr, args...);
}
template <typename Visitor, typename... Args>
void walk_paren_expr(Visitor &v, ParenExpr *p, Args... args) {
    v.visitExpr(p->operand, args...);
}
template <typename Visitor, typename... Args>
void walk_binary_expr(Visitor &v, BinaryExpr *b, Args... args) {
    v.visitExpr(b->lhs, args...);
    v.visitExpr(b->rhs, args...);
}
template <typename Visitor, typename... Args>
void walk_type_expr(Visitor &v, TypeExpr *t, Args... args) {
    if (t->subexpr) v.visitExpr(t->subexpr, args...);
}

}

#endif
