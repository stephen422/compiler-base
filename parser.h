#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"
#include <variant>

namespace cmp {

class ParseError {
public:
    ParseError(SourceLoc loc, const std::string &msg): location(loc), message(msg) {}
    // Report this error to stderr.
    void report() const;

    SourceLoc location;
    std::string message;
};

// ParseResult wraps the result of a single parse operation, i.e. the resulting
// AST node in the successful case, or an error object in the failing case. It
// also marks the position where the parse operation started.  With these
// information, it enables several helpful features:
//
//   1. it allows the caller to easily recover from the parse failure, using the
//   marked position info;
//
//   2. it enables the parser to proceed and try alternative productions
//   without being interrupted by an error generated in the failed production;
//
//   3. it allows the caller to overwrite the error with a more descriptive,
//   context-aware one.
template <typename T> class ParseResult {
public:
    // Uninitialized
    ParseResult() {}

    // Successful result
    template <typename U>
    ParseResult(NodePtr<U> ptr) : result(std::move(ptr)) {}

    // Erroneous result
    ParseResult(const ParseError &error) : result(error) {}

    // Upcasting
    template <typename U>
    ParseResult(ParseResult<U> &res) {
        if (res.success()) {
            result = res.get_ptr();
        } else {
            result = res.get_error();
        }
    }

    // Returns 'res', provided there were no errors; if there were, report them
    // and cause the compiler to exit.
    NodePtr<T> unwrap();
    // TODO
    NodePtr<T> get_ptr() { return std::move(std::get<NodePtr<T>>(result)); }
    ParseError &get_error() { return std::get<ParseError>(result); }
    bool success() { return std::holds_alternative<NodePtr<T>>(result); }

    std::variant<NodePtr<T>, ParseError> result;
};

class Parser {
public:
    Parser(Lexer &lexer);

    Ast parse();

private:
    // Parse a whole file.
    FilePtr parse_file();

    // Parse a toplevel statement.
    AstNodePtr parse_toplevel();

    // Statement parsers
    ParseResult<Stmt> parse_stmt();
    NodePtr<ExprStmt> parse_expr_stmt();
    NodePtr<AssignStmt> parse_assign_stmt();
    NodePtr<ReturnStmt> parse_return_stmt();
    NodePtr<CompoundStmt> parse_compound_stmt();

    // Declaration parsers
    DeclPtr parse_decl();
    ParseResult<ParamDecl> parse_param_decl();
    std::vector<NodePtr<ParamDecl>> parse_param_decl_list();
    NodePtr<VarDecl> parse_var_decl();
    NodePtr<FuncDecl> parse_func_decl();

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

    class ParserPosition {
    public:
        ParserPosition(size_t pos) : pos(pos) {}

        size_t pos;
    };

    auto get_position() const { return lookahead_pos; }

    // Restore the parsing position back to the given one.
    void restore_position(size_t pos) {
        lookahead_pos = pos;
    }

    // Get the precedence of an operator.
    int get_precedence(const Token &op) const;

    void expect(TokenKind kind, const std::string &msg);
    // Report an error and terminate.
    void error(const std::string &msg);

    // Construct a successful ParseResult, annotating it with the start
    // position.
    template <typename T> auto ok(NodePtr<T> ptr) {
        // Every time a parse operation succeeds, we update prev_pos to point
        // the position where the operation finished.
        return ptr;
    }

    // Construct a failed ParseResult, annotating it with an error message and
    // the start position.
    ParseError fail(const std::string &msg) {
        return ParseError{locate(), msg};
    }

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
};

} // namespace cmp

#endif
