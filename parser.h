// -*- C++ -*-
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

// TODO Document
template <typename T>
class ParseResult {
public:
    ParseResult() {}
    // Successful result
    template <typename U>
    ParseResult(NodePtr<U> ptr): ptr(std::move(ptr)), errors() {}
    // Erroneous result
    ParseResult(const ParseError &error): errors({error}) {}
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

    AstNodePtr parse();

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
    ExprPtr parse_literal_expr();
    // TODO: There's no UnaryExpr, so we can't change this to
    // NodePtr<UnaryExpr>. Better make one?
    ParseResult<Expr> parse_unary_expr();
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
    const Token &look() const;

    // Save the current parsing state so that the parser could be reverted back
    // to it later.
    // This disposes any state saved earlier, and makes it impossible to revert
    // the parser to any earlier state.
    void save_state();
    // Revert parsing state back to the last saved state by save_state().
    void revert_state();

    // Get the precedence of an operator.
    int get_precedence(const Token &op) const;

    void expect(TokenType type, const std::string &msg);
    void error(const std::string &msg);

    void skip_newlines();

    // Figure out the current location (line, col) in the source.
    SourceLoc locate() const {
        return lexer.src.locate(look().pos);
    }

    Lexer &lexer;
    Token tok; // lookahead token
    std::vector<Token> lookahead_cache; // lookahead tokens cache
    size_t lookahead_pos = 0; // lookahead position in the cache

    // Current operator precedence when parsing an expression.
    // int precedence = 0;
};

} // namespace cmp

#endif
