#ifndef CMP_PARSER_H
#define CMP_PARSER_H

#include "ast.h"
#include "lexer.h"
#include "sema.h"
#include <variant>

namespace cmp {

Name *name_of_derived_type(NameTable &names, TypeKind kind, Name *referee_name);

class Parser {
public:
    Lexer &lexer;
    Sema &sema;

    Token tok;                                   // lookahead token
    std::vector<std::unique_ptr<AstNode>> nodes; // node pointer pool
    AstNode *ast = nullptr;                      // resulting AST

    // End position of the last consumed token.
    // Used for tracking the range of the current token.
    size_t last_tok_endpos = 0;

    // Token cache.
    // These are data structures that enable flexible roll-back of the parsing
    // state.
    // Cache of the lookahead tokens.
    std::vector<Token> token_cache;
    // Index of the token to be read in the next next() call.
    size_t next_read_pos = 0;

    struct State {
        Token tok;
        size_t last_tok_endpos;
        size_t next_read_pos;
        size_t error_count;
    };
    State save_state();
    void restore_state(State state);

    Parser(Lexer &lexer, Sema &sema);
    AstNode *parse();

private:
    // Parse the whole file.
    File *parse_file();

    // Parse a toplevel statement.
    AstNode *parse_toplevel();

    // Statement parsers.
    Stmt *parse_stmt();
    Stmt *parse_expr_or_assign_stmt();
    Stmt *parse_return_stmt();
    IfStmt *parse_if_stmt();
    DeclStmt *parse_decl_stmt();
    CompoundStmt *parse_compound_stmt();
    BuiltinStmt *parse_builtin_stmt();
    bool is_end_of_stmt() const;
    bool is_eos() const;

    // Declaration parsers
    Decl *parseDecl();
    VarDecl *parse_var_decl(VarDeclKind kind);
    template <typename T, typename F>
    std::vector<T> parse_comma_separated_list(F &&parseFn);
    FuncDecl *parse_func_header();
    FuncDecl *parse_func_decl();
    StructDecl *parse_struct_decl();
    EnumVariantDecl *parse_enum_variant();
    std::vector<EnumVariantDecl *> parse_enum_variant_decl_list();
    EnumDecl *parse_enum_decl();
    ExternDecl *parse_extern_decl();
    bool is_start_of_decl();

    // Expression parsers
    Expr *parse_expr();
    Expr *parse_unary_expr();
    Expr *parse_literal_expr();
    Expr *parse_funccall_or_declref_expr();
    Expr *parse_cast_expr();
    Expr *parse_type_expr();
    Expr *parse_binary_expr_rhs(Expr *lhs, int precedence);
    Expr *parse_member_expr_maybe(Expr *expr);
    std::optional<StructDefTerm> parse_structdef_field();
    bool lookahead_structdef();
    Expr *parse_structdef_maybe(Expr *expr);

    // Error handling
    void error(const std::string &msg);
    void error_expected(const std::string &msg);

    // Advance the lookahead token.
    void next();

    // Expect and consume functions.
    bool expect(Tok kind, const std::string &msg);

    // Skip until a specific token(s) show up.
    void skip_until(Tok kind);
    void skip_until_any(std::initializer_list<Tok> &kinds);
    void skip_until_end_of_line();
    void skip_to_next_line();
    void skip_newlines();

    // Figure out the current location (line, col) in the source.
    SourceLoc locate() const { return lexer.source().locate(tok.pos); }

    // Convenience function for make_node_range.
    template <typename T, typename... Args>
    T *make_node_range(size_t pos, Args &&...args) {
        return sema.make_node_range<T>({pos, last_tok_endpos},
                                       std::forward<Args>(args)...);
    }
};

} // namespace cmp

#endif
