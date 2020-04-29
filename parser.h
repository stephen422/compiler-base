#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"
#include "error.h"
#include <variant>

namespace cmp {

// ParserResult wraps the result of a single parse operation, i.e. the
// resulting AST node in the successful case, or an error object in the failing
// case. It also marks the position where the parse operation started (TODO).
// With these information, it enables several helpful features:
//
//   1. it allows the caller to easily recover from the parse failure, using the
//   marked position info;
//
//   2. it enables the parser to proceed and try alternative productions
//   without being interrupted by an error generated in the failed production;
//
//   3. it allows the caller to overwrite the error message with a more
//   descriptive, context-aware one.
template <typename T> class ParserResult {
public:
    // Uninitialized
    ParserResult() {}

    // Successful result
    // U is the more-specific type.
    template <typename U>
    ParserResult(U *ptr) : result(ptr) {}

    // Erroneous result
    ParserResult(const Error &error) : result(error) {}

    // Upcasting
    template <typename U>
    ParserResult(ParserResult<U> &&res) {
        if (res.success()) {
            result = static_cast<T *>(res.ptr());
        } else {
            result = res.error();
        }
    }

    // Returns 'res', provided there were no errors; if there were, report them
    // and cause the compiler to exit.
    T *unwrap();

    // Get the stored node pointer.
    T *ptr() const { return std::get<T *>(result); }

    // Get the stored Error object.
    const Error &error() const { return std::get<Error>(result); }

    // Is this result successful?
    bool success() { return std::holds_alternative<T *>(result); }

private:
    std::variant<T *, Error> result;
};

using StmtResult = ParserResult<Stmt>;
using DeclResult = ParserResult<DeclNode>;
using ExprResult = ParserResult<Expr>;

class Parser {
public:
  Lexer &lexer;                                // owned lexer
  Token tok;                                   // lookahead token
  std::vector<std::unique_ptr<AstNode>> nodes; // node pointer pool
  std::vector<Error> &errors;                  // error list
  std::vector<Error> &beacons;                 // error beacon list
  AstNode *ast = nullptr;                      // resulting AST
  NameTable names;                             // name table

  Parser(Lexer &lexer, std::vector<Error> &errors, std::vector<Error> &beacons);
  Ast parse();

private:
  // Parse the whole file.
  File *parseFile();

  // Parse a toplevel statement.
  AstNode *parseToplevel();

  // Statement parsers.
  Stmt *parseStmt();
  Stmt *parse_expr_or_assign_stmt();
  Stmt *parse_return_stmt();
  IfStmt *parse_if_stmt();
  DeclStmt *parse_decl_stmt();
  CompoundStmt *parseCompoundStmt();
  BuiltinStmt *parseBuiltinStmt();
  bool is_end_of_stmt() const;
  bool is_eos();

  // Declaration parsers
  DeclNode *parseDecl();
  DeclNode *parseVarDecl(VarDeclNode::Kind kind);
  std::vector<DeclNode *> parseVarDeclList(VarDeclNode::Kind kind);
  StructDeclNode *parseStructDecl();
  FuncDeclNode *parseFuncDecl();
  bool isStartOfDecl() const;

  // Expression parsers
  Expr *parse_expr();
  Expr *parseUnaryExpr();
  Expr *parseLiteralExpr();
  Expr *parseFuncCallOrDeclRefExpr();
  Expr *parse_type_expr();
  Expr *parseBinaryExprRhs(Expr *lhs, int precedence);
  Expr *parseMemberExprMaybe(Expr *expr);
  bool isStartOfTypeExpr() const;

  std::vector<Error> parse_error_beacon();

  // Error handling
  void error(const std::string &msg);
  void errorExpected(const std::string &msg);

  // Advance the lookahead token.
  void next();

  // Expect and consume functions.
  bool expect(Tok kind, const std::string &msg);

  // Skip until a specific token(s) show up.
  void skip_until(Tok kind);
  void skip_until_any(const std::vector<Tok> &kinds);
  void skip_until_end_of_line();
  void skip_to_next_line();
  void skip_newlines();

  template <typename T, typename... Args> T *make_node(Args &&... args) {
    nodes.emplace_back(new T{std::forward<Args>(args)...});
    return static_cast<T *>(nodes.back().get());
  }

  template <typename T, typename... Args>
  T *make_node_pos(size_t pos, Args &&... args) {
    auto node = make_node<T>(std::forward<Args>(args)...);
    node->pos = pos;
    return node;
  }

  // Figure out the current location (line, col) in the source.
  SourceLoc locate() const { return lexer.source().locate(tok.pos); }
};

} // namespace cmp

#endif
