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
    // Parse the whole file.
    P<File> parseFile();

    // Parse a toplevel statement.
    P<AstNode> parseToplevel();

    // Statement parsers.
    P<Stmt> parseStmt();
    P<Stmt> parseExprOrAssignStmt();
    P<ReturnStmt> parseReturnStmt();
    P<DeclStmt> parseDeclStmt();
    P<CompoundStmt> parseCompoundStmt();
    bool isEndOfStmt() const;
    bool isEos();

    // Declaration parsers.
    P<Decl> parseDecl();
    std::vector<P<VarDecl>> parseVarDeclList();
    P<VarDecl> parseVarDecl();
    P<StructDecl> parseStructDecl();
    P<FuncDecl> parseFuncDecl();
    bool isStartOfDecl() const;

    // Expression parsers.
    P<Expr> parseExpr();
    P<UnaryExpr> parseLiteralExpr();
    P<DeclRefExpr> parseDeclRefExpr();
    P<TypeExpr> parseTypeExpr();
    P<Expr> parseUnaryExpr();
    P<Expr> parseBinaryExprRhs(ExprPtr lhs, int precedence = 0);

    // Error nodes.
    P<BadStmt> stmt_error(const std::string &msg);
    P<BadDecl> decl_error(const std::string &msg);
    P<BadExpr> expr_error(const std::string &msg);

    // Advance the lookahead token.
    void next();

    // Get the current lookahead token.
    const Token look() const;

    // Get and restore the current parsing position.
    auto getProgress() const { return look_index; }
    void restoreProgress(size_t pos) { look_index = pos; }

    // Get the precedence of an operator.
    int getPrecedence(const Token &op) const;

    bool expect(TokenKind kind, const std::string &msg);
    bool expectEndOfStmt();

    void skipNewlines();

    // Figure out the current location (line, col) in the source.
    SourceLoc locate() const { return lexer.getSource().locate(look().pos); }

    Lexer &lexer;                       // associated lexer
    Token tok;                          // lookahead token
    NameTable names;                    // name table (TODO: document)
    std::vector<Token> tokens;          // lexed tokens
    size_t look_index = 0;               // lookahead position in the cache
    std::vector<std::string> errors;
};

} // namespace cmp

#endif
