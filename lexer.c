#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "lexer.h"
#include "stretchy_buffer.h"

char *token_names[NUM_TOKENTYPES] = {
	[TOK_IDENT] = "identifier",
	[TOK_NUM] = "number",
	[TOK_NEWLINE] = "\\n",
	[TOK_ARROW] = "->",
	[TOK_LPAREN] = "(",
	[TOK_RPAREN] = ")",
	[TOK_LBRACE] = "{",
	[TOK_RBRACE] = "}",
	[TOK_DOT] = ".",
	[TOK_COMMA] = ",",
	[TOK_COLON] = ":",
	[TOK_SEMICOLON] = ";",
	[TOK_EQUALS] = "=",
	[TOK_PLUS] = "+",
	[TOK_STAR] = "*",
	[TOK_AMPERSAND] = "&",
	[TOK_SLASH] = "/",
	[TOK_BANG] = "!",
	[TOK_FN] = "fn",
	[TOK_LET] = "let",
	[TOK_VAR] = "var",
	[TOK_RETURN] = "return",
	[TOK_INT] = "int",
	[TOK_ERR] = "unknown",
};

static struct token_map symbols[] = {
	{"->", TOK_ARROW},
	{"\n", TOK_NEWLINE},
	{"/", TOK_SLASH},
	{"(", TOK_LPAREN},
	{")", TOK_RPAREN},
	{"{", TOK_LBRACE},
	{"}", TOK_RBRACE},
	{"[", TOK_LBRACKET},
	{"]", TOK_RBRACKET},
	{"<", TOK_LT},
	{">", TOK_GT},
	{".", TOK_DOT},
	{",", TOK_COMMA},
	{":", TOK_COLON},
	{";", TOK_SEMICOLON},
	{"'", TOK_QUOTE},
	{"\"", TOK_DOUBLEQUOTE},
	{"`", TOK_BACKTICK},
	{"=", TOK_EQUALS},
	{"+", TOK_PLUS},
	{"-", TOK_MINUS},
	{"*", TOK_STAR},
	{"&", TOK_AMPERSAND},
	{"^", TOK_CARET},
	{"~", TOK_TILDE},
	{"\\", TOK_BACKSLASH},
	{"|", TOK_PIPE},
	{"!", TOK_BANG},
	{"?", TOK_QUESTION},
	{"@", TOK_AT},
	{"#", TOK_HASH},
	{"$", TOK_DOLLAR},
	{"%", TOK_PERCENT},
	{"-", TOK_DASH},
	{NULL, 0}
};

struct token_map keywords[] = {
	{"fn", TOK_FN},
	{"let", TOK_LET},
	{"var", TOK_VAR},
	{"return", TOK_RETURN},
	{"int", TOK_INT},
	{"if", TOK_IF},
	{"else", TOK_ELSE},
	{"for", TOK_FOR},
	{"error", TOK_ERROR},
	{NULL, 0}
};

int is_keyword(Token tok)
{
	return TOK_KEYWORDS < tok.type && tok.type < TOK_ERR;
}

static char *readfile(const char *filename, long *filesize)
{
	FILE *f = fopen(filename, "r");
	char *s;
	if (!f) {
		fprintf(stderr, "error: %s: %s\n", filename, strerror(errno));
		exit(EXIT_FAILURE);
	}
	fseek(f, 0, SEEK_END);
	*filesize = ftell(f);
	rewind(f);

	s = malloc(*filesize + 1);
	if (!s) {
		fprintf(stderr, "out of memory\n");
		exit(EXIT_FAILURE);
	}
	fread(s, *filesize, 1, f);
	s[*filesize] = '\0';
	fclose(f);
	return s;
}

static void step(Lexer *l)
{
	if (l->rd_off < l->srclen) {
		l->off = l->rd_off;
		l->ch = l->src[l->off];
		if (l->ch == '\n') {
			sb_push(l->line_offs, l->off);
		}
		l->rd_off++;
	} else {
		l->off = l->srclen;
		l->ch = 0; // EOF
	}
}

int lexerInit(Lexer *l, const char *filename)
{
	memset(l, 0, sizeof(Lexer));
	l->filename = calloc(256, 1);
	l->filename = memcpy(l->filename, filename, 255);
	l->src = readfile(filename, &l->srclen);
	step(l);
	return 1;
}

int lexerInitText(Lexer *l, const char *text, size_t len)
{
	memset(l, 0, sizeof(Lexer));
	l->filename = calloc(256, 1);
	l->filename = strncpy(l->filename, "(null)", 255);
        l->srclen = len;
        l->src = calloc(len + 1, 1);
        strncpy(l->src, text, len);
        step(l);
	return 1;
}

void lexerCleanup(Lexer *l)
{
	sb_free(l->line_offs);
	free(l->filename);
	free(l->src);
}

// NOTE: more strict?
static void consume(Lexer *l, long n)
{
	for (long i = 0; i < n; i++)
		step(l);
}

static char lookn(Lexer *l, long n)
{
	if (l->off + n < l->srclen)
		return l->src[l->off + n];
	return '\0';
}

static void makeToken(Lexer *l, TokenType type)
{
	memset(&l->tok, 0, sizeof(Token));
	l->tok.type = type;
	l->tok.range = (SrcRange) {l->start, l->off};
}

void token_free(Token *t)
{
	free(t);
}

static void lexIdentOrKeyword(Lexer *l)
{
	while (isalnum(l->ch) || l->ch == '_')
		step(l);

	for (struct token_map *m = &keywords[0]; m->text != NULL; m++) {
		const char *c = m->text;
		const char *s = l->src + l->start;

		// Skip over characters common for the source text and the candidate.
		for (; *c != '\0' && s < l->src + l->off && *c == *s; c++, s++)
			/* nothing */;

		if (*c == '\0' && s == l->src + l->off) {
			// If both are terminated, a keyword successfully matched.
			makeToken(l, m->type);
			return;
		}
	}

	// Otherwise, this is an identifier.
	makeToken(l, TOK_IDENT);
}

static void skip_numbers(Lexer *l)
{
	while (isdigit(l->ch))
		step(l);
}

static void lexNumber(Lexer *l)
{
	skip_numbers(l);
	if (l->ch == '.') {
		step(l);
		skip_numbers(l);
	}
	makeToken(l, TOK_NUM);
}

// NOTE: taken from ponyc
static void lexSymbol(Lexer *l) {
    for (const struct token_map *m = &symbols[0]; m->text != NULL; m++) {
        for (int i = 0; m->text[i] == '\0' || m->text[i] == lookn(l, i); i++) {
            if (m->text[i] == '\0') {
                consume(l, i);
                makeToken(l, m->type);
                return;
            }
        }
    }
    fprintf(stderr, "lex error: unknown symbol '%c'\n", l->ch);
    step(l);
    makeToken(l, TOK_ERR);
}

// Lex the next token and place it into l->token.
// Return EOF if reached source EOF.
int lexerNext(Lexer *l) {
    for (;;) {
        l->start = l->off;

        switch (l->ch) {
        case 0: {
            makeToken(l, TOK_EOF);
            return EOF;
        }
        case ' ':
        case '\t': {
            step(l);
            break;
        }
        case '/': {
            step(l);
            if (l->ch == '/') {
                while (l->ch != '\n')
                    step(l);
                makeToken(l, TOK_COMMENT);
                return 0;
            } else {
                makeToken(l, TOK_SLASH);
                return 0;
            }
        }
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9': {
            lexNumber(l);
            return 0;
        }
        default: {
            if (isalpha(l->ch) || l->ch == '_') {
                lexIdentOrKeyword(l);
            } else {
                lexSymbol(l);
            }
            return 0;
        }
        }
    }

    return 0;
}

SrcLoc locate(Lexer *l, size_t pos)
{
	// Search linearly for line that contains this position.
	if (sb_count(l->line_offs) == 0) {
		// First line
		return (SrcLoc) {1, pos + 1};
	}
	int line;
	for (line = 0; line < sb_count(l->line_offs); line++) {
		if (pos < l->line_offs[line]) {
			break;
		}
	}
	int col = pos - l->line_offs[line - 1];
	return (SrcLoc) {line + 1, col};
}

void tokenPrint(Lexer *lex, const Token tok)
{
	switch (tok.type) {
	case TOK_IDENT:
        case TOK_COMMENT:
        case TOK_NUM :
		printf("'%.*s'\n", (int)(tok.range.end - tok.range.start), lex->src + tok.range.start);
		break;
	case TOK_ERR:
		printf("error\n");
		break;
	default:
		if (tok.type >= TOK_KEYWORDS)
			printf("'%s'\n", token_names[tok.type]);
		else
			printf("'%s'\n", token_names[tok.type]);
		break;
	}
}
