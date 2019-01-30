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

// TODO
template <typename T>
class ParseResult {
public:
    ParseResult() {}
    // Successful result
    template <typename U>
    ParseResult(NodePtr<U> res): result(std::move(res)), errors() {}
    // Erroneous result
    ParseResult(const ParseError &error): errors({error}) {}
    // Returns 'res', provided there were no errors; if there were, report them
    // and cause the compiler to exit.
    NodePtr<T> unwrap();
    bool success() { return errors.empty(); }

    NodePtr<T> result;
    std::vector<ParseError> errors;
};

class Parser {
public:
    Parser(Lexer &lexer) : lexer(lexer), tok(lexer.lex()) {}

    AstNodePtr parse();

private:
    // Parse a whole file.
    FilePtr parse_file();

    // Parse a toplevel statement.
    ToplevelPtr parse_toplevel();

    // Parse a statement.
    ParseResult<Stmt> parse_stmt();

    NodePtr<ReturnStmt> parse_return_stmt();
    NodePtr<CompoundStmt> parse_compound_stmt();

    // Parse a declaration.
    DeclPtr parse_decl();

    // Parse a variable declaration.
    NodePtr<VarDecl> parse_var_decl();

    // Parse a function declaration.
    FunctionPtr parse_function();

    // Parse an expression.
    ParseResult<Expr> parse_expr();

    // Parse a unary expression.
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

    // Parse a literal expression.
    ExprPtr parse_literal_expr();

    // Get the next token from the lexer.
    void next();

    // Get the precedence of an operator.
    int get_precedence(const Token &op) const;

    void expect(TokenType type, const std::string &msg);
    void error(const std::string &msg);

    // Figure out the current location (line, col) in the source.
    SourceLoc locate() const {
        return lexer.src.locate(tok.pos);
    }

    Lexer &lexer;
    Token tok; // lookahead token

    // Current operator precedence when parsing an expression.
    // int precedence = 0;
};

} // namespace cmp

#endif
