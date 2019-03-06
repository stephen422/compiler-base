#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"

namespace cmp {

class ParseError {
public:
    ParseError(SourceLoc loc, const std::string &msg): location(loc), message(msg) {}
    // Report this error to stderr.
    void report() const;

    SourceLoc location;
    std::string message;
};

// ParseResult wraps the result of a single parse operation, along with
// possible errors generated in the process.  It serves two main purposes:
//   1. it enables the parser to proceed and try alternative productions
//   without being interrupted by an error generated in a failed production;
//   2. it allows higher, more context-aware parse operations to override the
//   error message with a more descriptive one.
//
// TODO: isn't this an overkill?
template <typename T> class ParseResult {
public:
    ParseResult() {}
    // Successful result
    template <typename U>
    ParseResult(NodePtr<U> ptr): ptr(std::move(ptr)), errors() {}
    // Erroneous result
    ParseResult(const ParseError &error): errors({error}) {}
    // Upcasting
    template <typename U>
    ParseResult(ParseResult<U> &res): ptr(std::move(res.ptr)), errors(std::move(res.errors)) {}

    // Returns 'res', provided there were no errors; if there were, report them
    // and cause the compiler to exit.
    NodePtr<T> unwrap();
    bool success() { return errors.empty(); }

    NodePtr<T> ptr = nullptr;
    std::vector<ParseError> errors;
};

class Parser {
public:
    Parser(Lexer &lexer);

    Ast parse();

private:
    // Parse a whole file.
    FilePtr parse_file();

    // Parse a toplevel statement.
    ToplevelPtr parse_toplevel();

    // Statement parsers
    ParseResult<Stmt> parse_stmt();
    NodePtr<ExprStmt> parse_expr_stmt();
    NodePtr<AssignStmt> parse_assign_stmt();
    NodePtr<ReturnStmt> parse_return_stmt();
    NodePtr<CompoundStmt> parse_compound_stmt();

    // Declaration parsers
    DeclPtr parse_decl();
    NodePtr<VarDecl> parse_var_decl();
    NodePtr<Function> parse_function();

    // Expression parsers
    ParseResult<Expr> parse_expr();
    NodePtr<UnaryExpr> parse_literal_expr();
    NodePtr<IntegerLiteral> parse_integer_literal();
    NodePtr<DeclRefExpr> parse_declref_expr();
    NodePtr<TypeExpr> parse_type_expr();
    ParseResult<UnaryExpr> parse_unary_expr();
    // Extend a unary expression into binary if possible, by parsing any
    // attached RHS.  Returns the owning pointer to the newly constructed binary
    // expression.
    //
    // After the call, 'lhs' is invalidated by being moved away.  Subsequent
    // code should use the returned pointer instead.
    ExprPtr parse_binary_expr_rhs(ExprPtr lhs, int precedence = 0);

    // Get the next token from the lexer.
    void next();

    // Get the current lookahead token.
    const Token look() const;

    // Save the current parsing state so that the parser could be reverted back
    // to it later.
    // This disposes any state saved earlier, and makes it impossible to revert
    // the parser to any earlier state.
    void save_state();
    // Revert parsing state back to the last saved state by save_state().
    void revert_state();

    // Get the precedence of an operator.
    int get_precedence(const Token &op) const;

    void expect(TokenKind kind, const std::string &msg);
    void error(const std::string &msg);
    void skip_newlines();

    Source &get_source() const { return lexer.src; }

    // Figure out the current location (line, col) in the source.
    SourceLoc locate() const {
        return get_source().locate(look().pos);
    }

    Lexer &lexer;                           // associated lexer
    Token tok;                              // lookahead token
    NameTable name_table; // name table (TODO: document)
    std::vector<Token> tokens;              // lexed tokens
    std::vector<Token> lookahead_cache;     // lookahead tokens cache
    size_t lookahead_pos = 0;               // lookahead position in the cache
    size_t saved_pos = 0; // saved lookahead position (for backtracking)
};

} // namespace cmp

#endif
