#ifndef AST_H
#define AST_H

#include "lexer.h"
#include <iostream>
#include <memory>
#include <unordered_map>

namespace cmp {

class Semantics;

enum class AstType {
    none, // FIXME necessary?
    file,
    toplevel,
    decl_stmt,
    expr_stmt,
    assign_stmt,
    return_stmt,
    compound_stmt,
    var_decl,
    literal_expr,
    integer_literal,
    ref_expr,
    binary_expr,
    function,
};

class AstNode;
class File;
class Toplevel;
class Stmt;
class Expr;
class Decl;
class Function;

// Owning pointers to each AST node.
using AstNodePtr = std::unique_ptr<AstNode>;
using FilePtr = std::unique_ptr<File>;
using ToplevelPtr = std::unique_ptr<Toplevel>;
using StmtPtr = std::unique_ptr<Stmt>;
using ExprPtr = std::unique_ptr<Expr>;
using DeclPtr = std::unique_ptr<Decl>;
using FunctionPtr = std::unique_ptr<Function>;

template <typename T>
using NodePtr = std::unique_ptr<T>;

template<typename T, typename... Args>
NodePtr<T> make_node(Args&&... args) {
    return std::make_unique<T>(std::forward<Args>(args)...);
}

std::pair<size_t, size_t> get_ast_range(std::initializer_list<AstNode *> nodes);

// A Name corresponds to a single unique identifier string in the source text.
// There may be multiple occurrences of a string in the source text, but only
// one instance of the matching Name can reside in the name table.
class Name {
public:
    Name(const std::string &s) : text(s) {}
    std::string text;
};

// A NameTable is a hash table of Names queried by their string value.  It
// serves to reduce the number of string hashing operation, since we can look
// up the symbol table using Name instead of raw char * throughout the semantic
// analysis.
class NameTable {
public:
    Name *insert(const std::string &s) {
        auto pair = map.insert({s, {s}});
        return &pair.first->second;
    }
    Name *find(const std::string &s) {
        auto found = map.find(s);
        if (found == map.end()) {
            return nullptr;
        } else {
            return &found->second;
        }
    }
    std::unordered_map<std::string, Name> map;
};

// Ast is aggregate type that contains all information necessary for semantic
// analysis of an AST: namely, the root node and the name table.
class Ast {
public:
    Ast(AstNodePtr r, NameTable &nt) : root(std::move(r)), name_table(nt) {}
    AstNodePtr root;
    NameTable &name_table;
};

class AstNode {
public:
    AstNode() {}
    AstNode(AstType type) : type(type) {}
    virtual ~AstNode() = default;

    // AST printing.
    virtual void print() const = 0;
    // AST traversal.
    // TODO: AST is traversed at least twice, i.e. once for semantic analysis
    // and once for IR generation.  So there should be a generic way to
    // traverse it; maybe pass in a lambda that does work for a single node?
    virtual void traverse(Semantics &sema) = 0;
    // Convenience method for downcasting.
    template <typename T> constexpr T *as() {
        return static_cast<T *>(this);
    }
    // Convenience ostream for AST printing.
    // Handles indentation, tree drawing, etc.
    std::ostream &out() const {
        if (depth > 0) {
            std::cout << std::string(depth - 2, ' ');
            std::cout << "`-";
        }
        return std::cout;
    }

    // RAII trick to handle indentation.
    class PrintScope {
    public:
        PrintScope() { depth += 2; }
        ~PrintScope() { depth -= 2; }
    };

    AstType type = AstType::none; // node type
    size_t start_pos = 0;         // start pos of this AST in the source text
    size_t end_pos = 0;           // end pos of this AST in the source text
    // Indentation of the current node when dumping AST.
    // static because all nodes share this.
    static int depth;
};

// ========
//   File
// ========

// File is simply a group of Toplevels.
class File : public AstNode {
public:
    File() : AstNode(AstType::file) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    std::vector<ToplevelPtr> toplevels;
};

// =============
//   Statement
// =============

class Stmt : public AstNode {
public:
    Stmt(AstType type) : AstNode(type) {}
};

class DeclStmt : public Stmt {
public:
    DeclStmt(DeclPtr decl) : Stmt(AstType::decl_stmt), decl(std::move(decl)) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    DeclPtr decl;
};

class ExprStmt : public Stmt {
public:
    ExprStmt(ExprPtr expr) : Stmt(AstType::expr_stmt), expr(std::move(expr)) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    ExprPtr expr;
};

class RefExpr;
class AssignStmt : public Stmt {
public:
    AssignStmt(NodePtr<RefExpr> lhs, ExprPtr rhs) : Stmt(AstType::assign_stmt), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    NodePtr<RefExpr> lhs;
    ExprPtr rhs;
};

class ReturnStmt : public Stmt {
public:
    ReturnStmt(ExprPtr expr) : Stmt(AstType::return_stmt), expr(std::move(expr)) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    ExprPtr expr;
};

class CompoundStmt : public Stmt {
public:
    CompoundStmt() : Stmt(AstType::compound_stmt) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    std::vector<StmtPtr> stmts;
};

// ===============
//   Expressions
// ===============

class Type;

class Expr : public AstNode {
public:
    Expr(AstType type) : AstNode(type) {}

    Type *inferred_type = nullptr;
};

class IntegerLiteral : public Expr {
public:
    IntegerLiteral(int64_t v) : Expr(AstType::integer_literal), value(v) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    int64_t value; // TODO: big integers?
};

class RefExpr : public Expr {
public:
    RefExpr() : Expr(AstType::ref_expr) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    // The value of this pointer serves as a unique integer ID to be used for
    // indexing the symbol table.
    Name *name = nullptr;
};

class BinaryExpr : public Expr {
public:
    BinaryExpr(ExprPtr lhs_, Token op_, ExprPtr rhs_)
        : Expr(AstType::binary_expr), lhs(std::move(lhs_)), op(op_),
          rhs(std::move(rhs_)) {
        auto pair = get_ast_range({lhs.get(), rhs.get()});
        start_pos = pair.first;
        end_pos = pair.second;
    }
    void print() const override;
    void traverse(Semantics &sema) override;

    ExprPtr lhs;
    Token op;
    ExprPtr rhs;
};

// ================
//   Declarations
// ================

// A declaration.
class Decl : public AstNode {
public:
    Decl(AstType type) : AstNode(type) {}
};

// Variable declaration.
class VarDecl : public Decl {
public:
    VarDecl(Name *name, ExprPtr expr, bool mut)
        : Decl(AstType::var_decl), name(name), assign_expr(std::move(expr)), mut(mut) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    // The value of this pointer serves as a unique integer ID to be used for
    // indexing the symbol table.
    Name *name = nullptr;
    ExprPtr assign_expr;
    bool mut; // is this a "var" declaration?
};

// ============
//   Toplevel
// ============

class Toplevel : public AstNode {
public:
    Toplevel(AstType type) : AstNode(type) {}
};

// Function definition.  There is no separate 'function declaration': functions
// should always be defined whenever they are declared.
class Function : public Toplevel {
public:
    Function(const Token &id) : Toplevel(AstType::function), id(id) {}
    void print() const override;
    void traverse(Semantics &sema) override;

    Token id;
    // Compound statement body
    NodePtr<CompoundStmt> body;
    Token return_type;
};

void test(Semantics &sema);

} // namespace cmp

#endif
