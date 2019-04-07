#include "sema.h"
#include "ast.h"
#include "stretchy_buffer.h"
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>

static Map declmap;
static Map typemap;

static void fatal(const char *msg) {
	fprintf(stderr, "%s\n", msg);
	exit(EXIT_FAILURE);
}

static void error(const char *fmt, ...)
{
	va_list args;

	fprintf(stderr, "error: ");
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");

	exit(EXIT_FAILURE);
}

static Decl *make_decl(Name *name, Type *type)
{
	Decl *decl = calloc(1, sizeof(Decl));

	decl->name = name;
	decl->type = type;

	return decl;
}

static void free_decl(Decl *decl)
{
	free(decl);
}

static uint64_t hash(const void *p) {
	uint64_t x = (uint64_t)p;

	x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
	x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
	x = x ^ (x >> 31);

	return x;
}

static Symbol *make_symbol(Name *name, void *value) {
	Symbol *s;

	s = calloc(1, sizeof(Symbol));
	if (!s)
		fatal("out of memory");
	s->name = name;
	s->value = value;

	return s;
}

static void free_symbol(Symbol *s) {
	free(s->value);
	free(s);
}

static void map_init(Map *map)
{
	*map = calloc(HASHTABLE_SIZE, sizeof(Symbol *));
}

static Symbol *map_find(Map tab, Name *name) {
	int index = hash(name) % HASHTABLE_SIZE;

	for (Symbol *s = tab[index]; s; s = s->next)
		if (s->name == name)
			return s;

	return NULL;
}

static void map_push(Map map, Name *name, void *value) {
	if (map_find(map, name) != NULL)
		return;

	int i = hash(name) % HASHTABLE_SIZE;
	Symbol **p = &map[i];
	Symbol *s = make_symbol(name, value);

	s->next = *p;
	*p = s;
}

static void map_free(Map *map)
{
	for (int i = 0; i < HASHTABLE_SIZE; i++)
		if ((*map)[i])
			free_symbol((*map)[i]);
	free(*map);
}

void traverse(Node *node) {
	Decl *decl;

	switch (node->kind) {
	case ND_FILE:
		for (int i = 0; i < sb_count(node->nodes); i++)
			traverse(node->nodes[i]);
		break;
	case ND_FUNCDECL:
		traverse(node->body);
		break;
	case ND_EXPRSTMT:
		break;
	case ND_VARDECL:
		if (map_find(declmap, node->name) != NULL)
			error("redefinition of '%s'", node->name->text);
		decl = make_decl(node->name, NULL);
		map_push(declmap, node->name, decl);
		break;
	case ND_DECLSTMT:
		traverse(node->decl);
		break;
	case ND_RETURNSTMT:
		break;
	case ND_COMPOUNDSTMT:
		for (int i = 0; i < sb_count(node->nodes); i++) {
			traverse(node->nodes[i]);
		}
		break;
	default:
		fprintf(stderr, "%s: don't know how to traverse node kind %d\n",
			__func__, node->kind);
		break;
	}
}

void sema(Node *node)
{
	map_init(&declmap);
	map_init(&typemap);

	traverse(node);

	map_free(&declmap);
	map_free(&typemap);
}
