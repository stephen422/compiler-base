#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

// A Name corresponds to any single unique identifier string in the source
// text.  There may be multiple occurrences of the string in the source text,
// but only one matching Name object is created in the course of compilation.
typedef struct Name Name;
typedef struct Name {
    char *text;
    struct Name *next;
} Name;

// A NameTable is a hash table of Names queried by their raw string.  It serves
// to reduce the number of string hashing operation, since we can lookup the
// symbol table using Name instead of raw char * throughout the semantic
// analysis.
#define NAMETABLE_SIZE 512

typedef struct {
    Name *keys[NAMETABLE_SIZE];
    Name *name_buf; // memory buffer that stores Names
} NameTable;

typedef enum NodeType {
    ND_FILE,
    ND_TOKEN,
    ND_VARDECL,
    ND_TYPE,
    ND_REFEXPR,
    ND_LITEXPR,
    ND_BINEXPR,
    ND_EXPRSTMT,
    ND_DECLSTMT,
    ND_RETURNSTMT,
    ND_COMPOUNDSTMT,
    ND_FUNCDECL,
} NodeType;

typedef struct Node Node;
typedef struct Node {
    NodeType type;
    Token token;

    Name *name;
    Node *lhs;
    Node *op;
    Node *rhs;
    Node **nodes; // compoundstmt, file

    // vardecl
    Node *decltype;
    Node *expr;
    int mutable;

    Node *stmt_expr; // exprstmt
    Node *decl;      // declstmt

    Node *body;        // funcdecl
    Token return_type; // funcdecl
} Node;

typedef struct {
    Lexer lexer;            // lexer driven by this parser
    Token *lookahead_cache; // lookahead tokens cache
    int cache_index;        // current lookahead position in lookahead_cache
    Node **nodep_buf;       // pointers to the allocated nodes
    NameTable name_table;   // name table
} Parser;

void parser_push_name(Parser *p, Token tok);
Name *parser_get_name(Parser *p, Token tok);
void parser_init(Parser *p, const char *filename);
void parser_cleanup(Parser *p);
Node *parse(Parser *p);
void print_ast(Parser *p, const Node *node);

#endif
