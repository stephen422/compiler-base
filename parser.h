/* vim: set ft=c: */
#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"
#include "sema.h"

typedef struct {
	SrcLoc loc;
	char *msg;
} Error;

typedef struct {
	Lexer lexer;	      // lexer driven by this parser
	Token *token_cache;   // lookahead tokens cache
	int cache_pos;        // current lookahead position in token_cache
	Error *errors;	      // list of possible parse errors
	Node **nodep_buf;     // pointers to the allocated nodes
	NameTable nametable;  // name table
} Parser;

void parser_push_name(Parser *p, Token tok);
Name *parser_get_name(Parser *p, Token tok);
void parser_init(Parser *p, const char *filename);
void parser_cleanup(Parser *p);
Node *parse(Parser *p);
void print_ast(Parser *p, const Node *node);

#endif
