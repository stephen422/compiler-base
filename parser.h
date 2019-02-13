#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

typedef enum NodeType {
    ND_TOKEN,
    ND_VARDECL,
    ND_TYPE,
    ND_LITEXPR,
    ND_BINEXPR,
    ND_EXPRSTMT,
    ND_DECLSTMT,
    ND_RETURNSTMT,
    ND_COMPOUNDSTMT,
    ND_FUNCTION,
} NodeType ;

typedef struct AstNode AstNode;
typedef struct AstNode {
    NodeType type;
    // Literal/token-only expression
    Token token;
    // Binary expression
    AstNode *lhs;
    AstNode *op;
    AstNode *rhs;
    // Variable declaration
    AstNode *decltype;
    AstNode *expr;
    int mutable;
    // Expression statement
    AstNode *stmt_expr;
    // Declaration statement
    AstNode *decl;
    // Compound statement
    AstNode **stmt_buf;
    AstNode *body;
    // Function name (FIXME: unnecessary)
    Token name;
    // Function return type (TODO: type node)
    Token return_type;
} AstNode;

// A Name corresponds to any single unique identifier string in the source
// text.  There may be multiple occurrences of the string in the source text,
// but one instance of the matching Name exists in the course of compilation.
// A NameTable is a hash table of Names queried by their raw string.  It serves
// to reduce the number of string hashing operation, since we can look up the
// symbol table using Name instead of raw char * throughout the semantic
// analysis.
typedef struct Name Name;
typedef struct Name {
    char *text;
    struct Name *next;
} Name;

#define NAMETABLE_SIZE 512

typedef struct {
    Name *keys[NAMETABLE_SIZE];
    Name *name_buf;              // memory buffer that stores Names
} NameTable;

typedef struct {
    Lexer lexer;            // lexer driven by this parser
    Token *lookahead_cache; // lookahead tokens cache
    int cache_index;        // current lookahead position in lookahead_cache
    AstNode **nodep_buf;    // pointers to the allocated nodes
    NameTable name_table;   // name table
} Parser;

void parser_push_name(Parser *p, Token tok);
Name *parser_get_name(Parser *p, Token tok);
void parser_init(Parser *p, const char *filename);
void parser_cleanup(Parser *p);
AstNode *parse(Parser *p);

#endif
