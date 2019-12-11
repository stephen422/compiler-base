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
    ~Parser();

    Ast parse();

private:
    // Parse the whole file.
    File *parseFile();

    // Parse a toplevel statement.
    AstNode *parseToplevel();

    // Statement parsers.
    Stmt *parse_stmt();
    Stmt *parse_expr_or_assign_stmt();
    ReturnStmt *parse_return_stmt();
    DeclStmt *parse_decl_stmt();
    CompoundStmt *parse_compound_stmt();
    bool is_end_of_stmt() const;
    bool is_eos();

    // Declaration parsers.
    Decl *parse_decl();
    std::vector<VarDecl *> parse_var_decl_list();
    VarDecl *parse_var_decl();
    StructDecl *parse_struct_decl();
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
    BadStmt *stmt_error(const std::string &msg);
    BadDecl *decl_error(const std::string &msg);
    BadExpr *expr_error(const std::string &msg);

    // Advance the lookahead token.
    void next();

    // Get the current lookahead token.
    const Token look() const;

    // Expect and consume functions.
    bool expect(TokenKind kind, const std::string &msg);
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
