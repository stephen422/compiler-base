#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"
#include <variant>

namespace cmp {

class ParseError : public std::runtime_error {
public:
    ParseError(const std::string &msg) : std::runtime_error(msg) {}
};

class ParserError {
public:
    ParserError() {}
    ParserError(SourceLoc loc, const std::string &msg): loc(loc), message(msg) {}

    void report() const;

    SourceLoc loc;
    std::string message;
};

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
    ParserResult(const ParserError &error) : result(error) {}

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

    // Get the stored ParserError object.
    const ParserError &error() const { return std::get<ParserError>(result); }

    // Is this result successful?
    bool success() { return std::holds_alternative<T *>(result); }

private:
    std::variant<T *, ParserError> result;
};

using StmtResult = ParserResult<Stmt>;
using DeclResult = ParserResult<Decl>;
using ExprResult = ParserResult<Expr>;

class Parser {
public:
    Parser(Lexer &lexer);
    ~Parser();

    Ast parse();

private:
    template <typename T> using Res = ParserResult<T>;

    // Parse the whole file.
    File *parse_file();

    // Parse a toplevel statement.
    AstNode *parse_toplevel();

    // Statement parsers.
    StmtResult parse_stmt();
    StmtResult parse_expr_or_assign_stmt();
    ReturnStmt *parse_return_stmt();
    DeclStmt *parse_decl_stmt();
    CompoundStmt *parse_compound_stmt();
    bool is_end_of_stmt() const;
    bool is_eos();

    // Declaration parsers.
    Decl *parse_decl();
    std::vector<VarDecl *> parse_var_decl_list();
    VarDecl *parse_var_decl();
    Res<StructDecl> parse_struct_decl();
    FuncDecl *parse_func_decl();
    bool is_start_of_decl() const;

    // Expression parsers.
    Expr *parse_expr();
    UnaryExpr *parse_literal_expr();
    DeclRefExpr *parse_declref_expr();
    TypeExpr *parse_type_expr();
    Expr *parse_unary_expr();
    Expr *parse_binary_expr_rhs(Expr *lhs, int precedence = 0);

    // Error nodes.
    ParserError error(const std::string &msg) { return {locate(), msg}; }
    BadStmt *stmt_error(const std::string &msg);
    BadDecl *decl_error(const std::string &msg);
    BadExpr *expr_error(const std::string &msg);

    // Advance the lookahead token.
    void next();

    // Get the current lookahead token.
    const Token look() const;

    // Expect and consume functions.
    Res<AstNode> expect(TokenKind kind, const std::string &msg);
    bool expect_end_of_stmt();

    void skip_newlines();

    template <typename T, typename... Args> T *make_node(Args &&... args)
    {
        T *ptr = new T{std::forward<Args>(args)...};
        nodes.push_back(ptr);
        return ptr;
    }

    template <typename T, typename... Args>
    T *make_node_with_pos(size_t startPos, size_t endPos, Args &&... args)
    {
        auto node = make_node<T>(std::forward<Args>(args)...);
        node->startPos = startPos;
        node->endPos = endPos;
        return node;
    }

    // Figure out the current location (line, col) in the source.
    SourceLoc locate() const { return lexer.source().locate(look().pos); }

    Lexer &lexer;                 // associated lexer
    Token tok;                    // lookahead token
    std::vector<AstNode *> nodes; // node pointer pool
    NameTable names;              // name table (TODO: document)
    std::vector<Token> tokens;    // lexed tokens
    size_t look_index = 0;        // lookahead position in the cache
};

} // namespace cmp

#endif
