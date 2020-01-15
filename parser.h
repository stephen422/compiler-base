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
using DeclResult = ParserResult<Decl>;
using ExprResult = ParserResult<Expr>;

class Parser {
public:
    Lexer &lexer;                                // associated lexer
    Token tok;                                   // lookahead token
    std::vector<std::unique_ptr<AstNode>> nodes; // node pointer pool
    std::vector<Error> errors;                   // error list
    std::vector<Error> beacons;                  // error beacon list
    AstNode *ast = nullptr;                      // resulting AST
    NameTable names;                             // name table

    Parser(Lexer &lexer);
    Ast parse();
    void report() const;
    bool verify() const;

private:
    // Parse the whole file.
    File *parse_file();

    // Parse a toplevel statement.
    AstNode *parse_toplevel();

    // Statement parsers.
    Stmt *parse_stmt();
    Stmt *parse_expr_or_assign_stmt();
    Stmt *parse_return_stmt();
    DeclStmt *parse_decl_stmt();
    CompoundStmt *parse_compound_stmt();
    bool is_end_of_stmt() const;
    bool is_eos();

    // Declaration parsers
    Decl *parse_decl();
    std::vector<Decl *> parse_var_decl_list();
    Decl *parse_var_decl();
    StructDecl *parse_struct_decl();
    FuncDecl *parse_func_decl();
    bool is_start_of_decl() const;

    // Expression parsers
    Expr *parse_expr();
    Expr *parse_unary_expr();
    UnaryExpr *parse_literal_expr();
    Expr *parse_funccall_or_declref_expr();
    Expr *parse_typeexpr();
    Expr *parse_binary_expr_rhs(Expr *lhs, int precedence = 0);
    bool is_start_of_typeexpr() const;

    std::vector<Error> parse_error_beacon();

    // Error handling
    void error(const std::string &msg);
    void error_expected(const std::string &msg);

    // Advance the lookahead token.
    void next();

    // Expect and consume functions.
    bool expect(TokenKind kind, const std::string &msg);

    // Skip until a specific token(s) show up.
    void skip_until(TokenKind kind);
    void skip_until(const std::vector<TokenKind> &kinds);
    void skip_until_end_of_line();
    void skip_newlines();

    template <typename T, typename... Args> T *make_node(Args &&... args)
    {
        nodes.emplace_back(new T{std::forward<Args>(args)...});
        return static_cast<T *>(nodes.back().get());
    }

    template <typename T, typename... Args>
    T *make_node_with_pos(size_t pos, Args &&... args)
    {
        auto node = make_node<T>(std::forward<Args>(args)...);
        node->pos = pos;
        return node;
    }

    // Figure out the current location (line, col) in the source.
    SourceLoc locate() const { return lexer.source().locate(tok.pos); }
};

} // namespace cmp

#endif
