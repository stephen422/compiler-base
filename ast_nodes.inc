// -*- C++ -*-
#ifndef AST_NODES_INC
#define AST_NODES_INC

namespace cmp {

struct AstNode;
struct File;

struct Stmt;
struct DeclStmt;
struct ExprStmt;
struct AssignStmt;
struct ReturnStmt;
struct IfStmt;
struct BuiltinStmt;
struct CompoundStmt;

struct Expr;
struct IntegerLiteral;
struct StringLiteral;
struct DeclRefExpr;
struct FuncCallExpr;
struct StructDefExpr;
struct MemberExpr;
struct UnaryExpr;
struct ParenExpr;
struct BinaryExpr;
struct TypeExpr;

struct DeclNode;
struct VarDeclNode;
struct FuncDeclNode;
struct StructDeclNode;
struct EnumVariantDeclNode;
struct EnumDeclNode;

}

#endif
