#include "sema.h"
#include "stretchy_buffer.h"
#include <stdio.h>
#include <stdint.h>
#include <string.h>

static uint64_t hash(const char *text)
{
    uint64_t hash = 5381;
    int c;
    while ((c = *text++)) {
        hash = ((hash << 5) + hash) + c; // hash * 33 + c
    }
    return hash;
}

static Symbol *make_symbol(const char *name, Decl decl)
{
    Symbol *s = calloc(1, sizeof(Symbol));
    s->name = calloc(strlen(name) + 1, sizeof(char));
    memcpy(s->name, name, strlen(name));
    s->decl = decl;
    return s;
}

static void free_symbol(Symbol *s)
{
    free(s->name);
    free(s);
}

static void symbol_push(SymbolTable tab, const char *name, Decl decl)
{
    int index = hash(name) % HASHTABLE_SIZE;
    Symbol **p = &tab[index];
    Symbol *new = make_symbol(name, decl);
    new->next = *p;
    *p = new;
}

static Symbol *symbol_find(SymbolTable tab, const char *name)
{
    int index = hash(name) % HASHTABLE_SIZE;
    for (Symbol *s = tab[index]; s; s = s->next) {
        if (!strcmp(s->name, name)) {
            return s;
        }
    }
    return NULL;
}

void traverse(Node *node)
{
    switch (node->type) {
    case ND_FILE:
        for (int i = 0; i < sb_count(node->nodes); i++) {
            traverse(node->nodes[i]);
        }
        break;
    case ND_FUNCDECL:
        printf("traversing ND_FUNCTION\n");
        traverse(node->body);
        break;
    case ND_EXPRSTMT:
        printf("traversing ND_EXPRSTMT\n");
        break;
    case ND_DECLSTMT:
        printf("traversing ND_DECLSTMT\n");
        break;
    case ND_RETURNSTMT:
        printf("traversing ND_RETURNSTMT\n");
        break;
    case ND_COMPOUNDSTMT:
        printf("traversing ND_COMPOUNDSTMT\n");
        for (int i = 0; i < sb_count(node->nodes); i++) {
            traverse(node->nodes[i]);
        }
        break;
    default:
        fprintf(stderr, "%s: don't know how to traverse node type %d\n", __func__, node->type);
        break;
    }
}
