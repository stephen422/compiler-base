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

class Parser {
public:
    Parser(Lexer &lexer);

    Ast parse();

private:
    // Parse a whole file.
    P<File> parse_file();

    // Parse a toplevel statement.
    P<AstNode> parse_toplevel();

    // Statement parsers.
    P<Stmt> parse_stmt();
    P<Stmt> parse_expr_or_assign_stmt();
    P<ReturnStmt> parse_return_stmt();
    P<DeclStmt> parse_decl_stmt();
    P<CompoundStmt> parse_compound_stmt();
    bool is_end_of_stmt() const;
    bool is_eos();

    // Declaration parsers.
    P<Decl> parse_decl();
    P<ParamDecl> parse_param_decl();
    std::vector<P<ParamDecl>> parse_param_decl_list();
    P<VarDecl> parse_var_decl();
    P<FuncDecl> parse_func_decl();
    bool is_start_of_decl() const;

    // Expression parsers.
    P<Expr> parse_expr();
    P<UnaryExpr> parse_literal_expr();
    P<DeclRefExpr> parse_declref_expr();
    P<TypeExpr> parse_type_expr();
    P<Expr> parse_unary_expr();
    P<Expr> parse_binary_expr_rhs(ExprPtr lhs, int precedence = 0);

    // Get the next token from the lexer.
    void next();

    // Get the current lookahead token.
    const Token look() const;

    // Get the current parsing position.
    auto getProgress() const { return lookahead_pos; }

    // Restore the parsing position back to the given one.
    void restoreProgress(size_t pos) {
        lookahead_pos = pos;
    }

    // Get the precedence of an operator.
    int get_precedence(const Token &op) const;

    void expect(TokenKind kind, const std::string &msg);
    void expect_end_of_stmt();

    void skip_newlines();

    // Figure out the current location (line, col) in the source.
    SourceLoc locate() const {
        return lexer.get_source().locate(look().pos);
    }

    Lexer &lexer;                           // associated lexer
    Token tok;                              // lookahead token
    NameTable name_table; // name table (TODO: document)
    std::vector<Token> tokens;              // lexed tokens
    std::vector<Token> lookahead_cache;     // lookahead tokens cache
    size_t lookahead_pos = 0;               // lookahead position in the cache
    std::vector<std::string> errors;
};

} // namespace cmp

#endif
