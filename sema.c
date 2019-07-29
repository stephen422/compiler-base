#include "sema.h"
#include "ast.h"
#include "stretchy_buffer.h"
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>

static Map declmap;
static Map typemap;
static Type *i32_type;
static Type *i64_type;

extern struct token_map keywords[];
static void map_push_scope(Map *m);
static void map_pop_scope(Map *m);
static void map_print(const Map *m);

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

    printf("==== declmap ====\n");
    map_print(&declmap);
    printf("==== typemap ====\n");
    map_print(&typemap);

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
    Name *n, **p;
    int i;

    n = get_name(nt, s, len);
    if (n)
        return n;

    i = strhash(s, len) % NAMETABLE_SIZE;
    n = calloc(1, sizeof(Name));
    n->text = calloc(len + 1, sizeof(char));
    strncpy(n->text, s, len);

    p = &nt->keys[i];
    n->next = *p;
    *p = n;

    return *p;
}

Name *push_refname(NameTable *nt, const Name *name)
{
    int len = strlen(name->text);
    char *s = calloc(len + 2, 1);

    s[0] = '&';
    strncpy(s + 1, name->text, len);
    Name *n = push_name(nt, s, strlen(s));
    free(s);

    return n;
}

Name *get_name(NameTable *nt, char *s, size_t len)
{
    int index = strhash(s, len) % NAMETABLE_SIZE;

    for (Name *n = nt->keys[index]; n; n = n->next)
        if (strlen(n->text) == len && !strncmp(n->text, s, len))
            return n;

    return NULL;
}

static Type *make_type(Name *name, enum TypeKind kind, Type *canon_type)
{
    Type *type = calloc(1, sizeof(Type));

    type->kind = kind;
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

///
/// Symbol map API
///

static uint64_t ptrhash(const void *p) {
    uint64_t x = (uint64_t)p;

    x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
    x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
    x = x ^ (x >> 31);

    return x;
}

static Symbol *make_symbol(Name *name, int scope, void *value) {
    Symbol *s;

    s = calloc(1, sizeof(Symbol));
    if (!s) {
        fatal("out of memory");
    }
    s->name = name;
    s->scope = scope;
    s->value = value;

    return s;
}

static void free_symbol(Symbol *s) {
    free(s->value);
    free(s);
}

static void map_init(Map *m)
{
    memset(m, 0, sizeof(Map));
    map_push_scope(m);
}

static void map_destroy(Map *m)
{
    for (int i = 0; i < m->n_scope; i++)
        map_pop_scope(m);
    sb_free(m->scopes);
}

static Symbol *map_find(Map *m, Name *name) {
    assert(name);

    int i = ptrhash(name) % HASHTABLE_SIZE;

    for (Symbol *s = m->buckets[i]; s; s = s->next)
        if (s->name == name)
            return s;
    return NULL;
}

static Symbol *map_push_at_scope(Map *m, Name *name, void *value, int scope)
{
    Symbol *found, *s, **p;
    int i;

    found = map_find(m, name);
    if (found && found->scope == scope) {
        /* same one in current scope */
        return found;
    }

    i = ptrhash(name) % HASHTABLE_SIZE;
    s = make_symbol(name, scope, value);

    /* insert into the bucket */
    p = &m->buckets[i];
    s->next = *p;
    *p = s;

    /* swap scope head */
    p = &m->scopes[scope];
    s->cross = *p;
    *p = s;

    return s;
}

static Symbol *map_push(Map *m, Name *name, void *value)
{
    return map_push_at_scope(m, name, value, m->n_scope - 1);
}

static void free_bucket(Symbol *s)
{
    while (s) {
        Symbol *t = s->next;
        free_symbol(s);
        s = t;
    }
}

static void map_push_scope(Map *m)
{
    m->n_scope++;
    sb_push(m->scopes, NULL);
}

static void map_pop_scope(Map *m)
{
    assert(m->n_scope > 0 && "attempt to pop from empty map!");

    /* free the cross link */
    Symbol *s = m->scopes[m->n_scope-1];
    while (s) {
        int i = ptrhash(s->name) % HASHTABLE_SIZE;
        m->buckets[i] = s->next;

        Symbol *t = s->cross;
        free_symbol(s);
        s = t;
    }

    m->n_scope--;
    sb_len(m->scopes) = m->n_scope;
}

static void push_scope(void)
{
    map_push_scope(&declmap);
    map_push_scope(&typemap);
}

static void pop_scope(void)
{
    map_pop_scope(&declmap);
    map_pop_scope(&typemap);
}

static void map_print(const Map *m)
{
    for (int i = 0; i < m->n_scope; i++) {
        printf("[Scope %d]:", i);
        for (Symbol *s = m->scopes[i]; s; s = s->cross)
            printf(" {%s}", s->name->text);
        printf("\n");
    }

    for (int i = 0; i < HASHTABLE_SIZE; i++) {
        if (!m->buckets[i])
            continue;

        printf("[%2d]:", i);
        for (Symbol *s = m->buckets[i]; s; s = s->next)
            printf(" {%s}", s->name->text);
        printf("\n");
    }
}

static int is_redefinition(Map *m, Name *name)
{
    Symbol *s = map_find(m, name);

    return s && (s->scope == m->n_scope - 1);
}

// Declare or find the reference type of an existent type.  Reference types are
// declared at the same scope as their dereferenced types.
static Type *reftype(NameTable *nt, Type *type) {
    Symbol *s = map_find(&typemap, type->name);
    assert(s);
    int scope = s->scope;

    Name *refname = push_refname(nt, type->name);
    Symbol *try = map_find(&typemap, refname);
    if (try) {
        return try->value;
    }

    Type *reftype = make_type(refname, T_REF, type);
    return map_push_at_scope(&typemap, refname, reftype, scope)->value;
}

// Walk the AST starting from 'node' as the root node.
static void walk(NameTable *nt, Node *node)
{
    Decl *decl;
    Type *type;
    Symbol *s;

    if (!node) {
        return;
    }

    switch (node->kind) {
    case ND_FILE:
        for (int i = 0; i < sb_count(node->nodes); i++) {
            walk(nt, node->nodes[i]);
        }
        break;
    case ND_FUNCDECL: {
        push_scope();

        for (int i = 0; i < sb_count(node->paramdecls); i++) {
            walk(nt, node->paramdecls[i]);
        }
        walk(nt, node->rettypeexpr);
        walk(nt, node->body);

        int has_return = 0;

        for (int i = 0; i < sb_count(node->body->nodes); i++) {
            Node *child = node->body->nodes[i];
            if (child->kind != ND_RETURNSTMT) {
                continue;
            }

            has_return = 1;
            if (!node->rettypeexpr) {
                error("function does not return value\n");
            }
            if (child->expr->type != node->rettypeexpr->type) {
                error("wrong return type %s, expected %s\n",
                      child->expr->type->name->text,
                      node->rettypeexpr->type->name->text);
            }
        }

        if (node->rettypeexpr && !has_return) {
            error("function should return a value\n");
        }

        pop_scope();
        break;
    }
    case ND_TYPEEXPR:
        if (node->typeexpr) {
            walk(nt, node->typeexpr);
            if (node->ref) {
                node->type = reftype(nt, node->typeexpr->type);
            } else {
                error("don't know what to do with this subtype: %s\n", node->name);
            }
        } else {
            // canonical type
            s = map_find(&typemap, node->name);
            if (!s) {
                error("undeclared type %s\n", node->name);
            }
            node->type = (Type *)s->value;
        }
        break;
    case ND_IDEXPR:
        s = map_find(&declmap, node->name);
        if (!s) {
            error("'%s' is not declared", node->name->text);
        }
        decl = (Decl *)s->value;
        node->type = decl->type;
        break;
    case ND_LITEXPR:
        // for now, only supports i32 literals
        node->type = i32_type;
        break;
    case ND_REFEXPR:
        walk(nt, node->expr);

        node->type = reftype(nt, node->expr->type);
        break;
    case ND_DEREFEXPR: {
        walk(nt, node->expr);

        Type *operand_type = node->expr->type;
        if (operand_type->kind != T_REF) {
            error("dereference of a non-reference type %s\n", operand_type->name->text);
        }
        node->type = operand_type->canon_type;
        break;
    }
    case ND_BINEXPR:
        walk(nt, node->lhs);
        walk(nt, node->rhs);

        assert(node->lhs->type);
        assert(node->rhs->type);

        if (node->lhs->type != node->rhs->type) {
            printf("%s vs %s\n", node->lhs->type->name->text, node->rhs->type->name->text);
            error("LHS and RHS of binary expression differs in type");
        }
        node->type = node->lhs->type;
        break;
    case ND_EXPRSTMT:
        walk(nt, node->expr);
        break;
    case ND_PARAMDECL:
        walk(nt, node->typeexpr);

        if (is_redefinition(&declmap, node->name)) {
            error("redefinition of '%s'", node->name->text);
        }

        decl = make_decl(node->name, node->typeexpr->type);
        map_push(&declmap, node->name, decl);
        break;
    case ND_VARDECL:
        if (is_redefinition(&declmap, node->name))
            error("redefinition of '%s'", node->name->text);

        // infer type from expression
        if (node->expr) {
            walk(nt, node->expr);
            type = node->expr->type;
        }
        // else, try explicit type spec
        else if (node->typeexpr) {
            walk(nt, node->typeexpr);
            type = node->typeexpr->type;
        } else {
            assert(0 && "unreachable");
        }

        if (!type) {
            // inference failure
            error("cannot infer type of '%s'", node->name->text);
        }

        decl = make_decl(node->name, type);
        map_push(&declmap, node->name, decl);
        break;
    case ND_DECLSTMT:
        walk(nt, node->decl);
        break;
    case ND_ASSIGNSTMT:
        /* RHS first */
        walk(nt, node->rhs);
        walk(nt, node->lhs);
        break;
    case ND_COMPOUNDSTMT:
        push_scope();
        for (int i = 0; i < sb_count(node->nodes); i++) {
            walk(nt, node->nodes[i]);
        }
        pop_scope();
        break;
    case ND_RETURNSTMT:
        walk(nt, node->expr);
        break;
    default:
        fprintf(stderr, "%s: don't know how to walk node kind %d\n",
                __func__, node->kind);
        break;
    }
}

static Type *setup_type_from_str(NameTable *nt, char *s)
{
    Name *n = push_name(nt, s, strlen(s));
    Type *t = make_type(n, T_CANON, NULL);
    return map_push(&typemap, n, t)->value;
}

static void setup_builtin_types(NameTable *nt)
{
    i32_type = setup_type_from_str(nt, "i32");
    i64_type = setup_type_from_str(nt, "i64");
}

void sema(ASTContext ast)
{
    map_init(&declmap);
    map_init(&typemap);

    setup_builtin_types(ast.nametable);

    walk(ast.nametable, ast.root);

    printf("==== declmap ====\n");
    map_print(&declmap);
    printf("==== typemap ====\n");
    map_print(&typemap);

    map_destroy(&declmap);
    map_destroy(&typemap);
}
