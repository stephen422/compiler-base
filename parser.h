/* vim: set ft=c: */
#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"
#include "sema.h"

// These are meant to be passed around by value.
typedef struct {
	SrcLoc loc;
	char *msg;
} Error;

// Parser state.
typedef struct {
    Lexer lexer;         // lexer driven by this parser
    Token tok;           // current token
    Error *errors;       // list of possible parse errors
    Error *beacons;      // list of possible parse errors
    Node **nodep_buf;    // pointers to the allocated nodes
    NameTable nametable; // name table
} Parser;

void parser_push_name(Parser *p, Token tok);
void parserInit(Parser *p, const char *filename);
void parserInitText(Parser *p, const char *text, size_t len);
void parserCleanup(Parser *p);
Node *parse(Parser *p);
Error parseErrorBeacon(Parser *p);
void parserVerify(Parser *p);
void printAst(Parser *p, const Node *node);

#endif
