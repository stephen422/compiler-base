#include "sema.h"
#include "ast.h"
#include "stretchy_buffer.h"
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>

static Map declmap;
static Map typemap;

extern struct token_map keywords[];

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

///
/// Name table API
///

static uint64_t strhash(const char *text, int len)
{
	uint64_t hash = 5381;
	for (int i = 0; i < len; i++) {
		int c = text[i];
		hash = ((hash << 5) + hash) + c; // hash * 33 + c
		len--;
	}
	return hash;
}

Name *push_name(NameTable *nt, char *s, size_t len)
{
	int i = strhash(s, len) % NAMETABLE_SIZE;
	Name *name = calloc(1, sizeof(Name));
	Name **p;

	name->text = calloc(len + 1, sizeof(char));
	strncpy(name->text, s, len);

	p = &nt->keys[i];
	name->next = *p;
	*p = name;
	return *p;
}

Name *get_name(NameTable *nt, char *s, size_t len)
{
	int index = strhash(s, len) % NAMETABLE_SIZE;

	for (Name *n = nt->keys[index]; n; n = n->next)
		if (strlen(n->text) == len && !strncmp(n->text, s, len))
			return n;

	return NULL;
}

Name *get_or_push_name(NameTable *nt, char *s, size_t len)
{
	Name *name = get_name(nt, s, len);

	if (!name)
		name = push_name(nt, s, len);
	return name;
}

static Type *make_type(Name *name, Type *canon_type)
{
	Type *type = calloc(1, sizeof(Type));

	type->name = name;
	type->canon_type = canon_type;

	return type;
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

static uint64_t ptrhash(const void *p) {
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

static Symbol *map_find(Map map, Name *name) {
	int index = ptrhash(name) % HASHTABLE_SIZE;

	for (Symbol *s = map[index]; s; s = s->next)
		if (s->name == name)
			return s;

	return NULL;
}

static void map_push(Map map, Name *name, void *value) {
	int i;

	if (map_find(map, name) != NULL)
		return;

	i = ptrhash(name) % HASHTABLE_SIZE;
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

static void setup_builtin_types(void)
{
}

void sema(Node *node)
{
	map_init(&declmap);
	map_init(&typemap);

	// Start by pushing built-in types into the type map.
	setup_builtin_types();

	traverse(node);

	map_free(&declmap);
	map_free(&typemap);
}
