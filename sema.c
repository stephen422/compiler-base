#include "sema.h"
#include "ast.h"
#include "stretchy_buffer.h"
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>

extern struct token_map keywords[];
static void map_push_scope(Map *m);
static void map_pop_scope(Map *m);
static void map_print(const Map *m);

void fatal(const char *fmt, ...) {
    va_list args;

    fprintf(stderr, "fatal: ");
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");

    exit(EXIT_FAILURE);
}

static const char *retid = "*ret";

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

Name *push_name(NameTable *nt, const char *s, size_t len)
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

Name *get_name(NameTable *nt, const char *s, size_t len)
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

static Symbol *make_sym(Name *name, int scope, void *value) {
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
    for (int i = 0; i < m->n_scope; i++) {
        map_pop_scope(m);
    }
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
        // There is already one in current scope
        return found;
    }

    i = ptrhash(name) % HASHTABLE_SIZE;
    s = make_sym(name, scope, value);

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

static void push_scope(Context *c)
{
    map_push_scope(&c->declmap);
    map_push_scope(&c->typemap);
}

static void pop_scope(Context *c)
{
    map_pop_scope(&c->declmap);
    map_pop_scope(&c->typemap);
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
static Type *reftype(Context *c, Type *type) {
    Symbol *s = map_find(&c->typemap, type->name);
    assert(s);
    int scope = s->scope;

    Name *refname = push_refname(c->nt, type->name);
    Symbol *try = map_find(&c->typemap, refname);
    if (try) {
        return try->value;
    }

    Type *reftype = make_type(refname, T_REF, type);
    return map_push_at_scope(&c->typemap, refname, reftype, scope)->value;
}

// Walk the AST starting from 'node' as the root node.
static void walk(Context *c, Node *node)
{
    Decl *decl;
    Type *type;
    Symbol *s;
    Expr *e;
    DeclNode *d;
    TypeExpr *t;

    if (!node) {
        return;
    }

    switch (node->base.kind) {
    case ND_FILE:
        for (int i = 0; i < sb_count(node->nodes); i++) {
            walk(c, node->nodes[i]);
        }
        break;
    case ND_FUNCDECL: {
        // Function declaration
        // TODO: type is NULL for now
        Decl *funcdecl = make_decl(node->d.name, NULL);
        map_push(&c->declmap, node->d.name, funcdecl);

        push_scope(c);

        // Function header
        for (int i = 0; i < sb_count(node->paramdecls); i++) {
            walk(c, node->paramdecls[i]);
        }
        walk(c, (Node *)node->rettypeexpr);

        // Push an alias type with a user-undefinable name ("*ret") to the type
        // map.  This alias is used to identify the return type of the current
        // function when visiting return statements at the lower recursion
        // levels.
        Name *retname = push_name(c->nt, retid, strlen(retid));
        if (node->rettypeexpr) {
            assert(node->rettypeexpr->type);
            Type *rettype = make_type(retname, T_ALIAS, node->rettypeexpr->type);
            map_push(&c->typemap, retname, rettype);
        }

        // Function body
        walk(c, node->body);

        if (node->rettypeexpr) {
            Symbol *s = map_find(&c->declmap, retname);
            if (!s) {
                error("function should return a value!!\n");
            }
        }

        pop_scope(c);
        break;
    }
    case ND_TYPEEXPR:
        t = &node->t;
        // XXX
        // if (t->typeexpr) {
        //     walk(c, (Node *)t->typeexpr);

        //     // XXX
        //     // if (t.ref) {
        //     //     t.type = reftype(c, t.typeexpr->type);
        //     // } else {
        //     //     error("don't know what to do with this subtype: %s\n", node->t.name);
        //     // }
        // } else {
        //     // canonical type
        //     s = map_find(&c->typemap, t->name);
        //     if (!s)
        //         error("undeclared type %s\n", t->name);
        //     t->type = (Type *)s->value;
        // }
        break;
    case ND_IDEXPR:
        s = map_find(&c->declmap, node->e.name);
        if (!s)
            error("'%s' is not declared", node->e.name->text);

        decl = (Decl *)s->value;
        node->e.type = decl->type;
        break;
    case ND_LITEXPR:
        // for now, only supports i32 literals
        node->e.type = c->i32_type;
        break;
    case ND_REFEXPR:
        walk(c, (Node *)node->e.target);

        node->e.type = reftype(c, node->e.target->type);
        break;
    case ND_DEREFEXPR: {
        walk(c, (Node *)node->e.target);

        Type *operand_type = node->e.target->type;
        if (operand_type->kind != T_REF) {
            error("dereference of a non-reference type %s\n", operand_type->name->text);
        }
        node->e.type = operand_type->canon_type;
        break;
    }
    case ND_BINEXPR:
        walk(c, (Node *)node->e.lhs);
        walk(c, (Node *)node->e.rhs);

        assert(node->e.lhs->type);
        assert(node->e.rhs->type);

        if (node->e.lhs->type != node->e.rhs->type) {
            printf("%s vs %s\n", node->e.lhs->type->name->text, node->e.rhs->type->name->text);
            error("LHS and RHS of binary expression differs in type");
        }
        node->e.type = node->e.lhs->type;
        break;
    case ND_EXPRSTMT:
        walk(c, (Node *)node->s.stmt_expr);
        break;
    case ND_PARAMDECL:
        walk(c, (Node *)node->d.typeexpr);

        if (is_redefinition(&c->declmap, node->d.name))
            error("redefinition of '%s'", node->d.name->text);

        decl = make_decl(node->d.name, node->d.typeexpr->type);
        map_push(&c->declmap, node->d.name, decl);
        break;
    case ND_VARDECL:
        d = &node->d;

        if (is_redefinition(&c->declmap, d->name))
            error("redefinition of '%s'", d->name->text);

        // infer type from expression
        if (d->expr) {
            walk(c, (Node *)d->expr);
            type = d->expr->type;
        }
        // else, try explicit type spec
        else if (d->typeexpr) {
            walk(c, (Node *)d->typeexpr);
            type = d->typeexpr->type;
        } else {
            assert(0 && "unreachable");
        }

        if (!type) {
            // inference failure
            error("cannot infer type of '%s'", d->name->text);
        }

        decl = make_decl(d->name, type);
        map_push(&c->declmap, d->name, decl);
        break;
    case ND_DECLSTMT:
        walk(c, node->s.decl);
        break;
    case ND_ASSIGNSTMT:
        /* RHS first */
        walk(c, (Node *)node->e.rhs);
        walk(c, (Node *)node->e.lhs);
        break;
    case ND_COMPOUNDSTMT:
        push_scope(c);
        for (int i = 0; i < sb_count(node->nodes); i++) {
            walk(c, node->nodes[i]);
        }
        pop_scope(c);
        break;
    case ND_RETURNSTMT: {
        walk(c, (Node *)node->s.stmt_expr);

        Name *retname = get_name(c->nt, retid, strlen(retid));
        if (retname) {
            Type *rettype = map_find(&c->typemap, retname)->value;
            assert(rettype->kind == T_ALIAS);

            // empty return statements ('return')
            if (!node->s.stmt_expr) {
                error("function should return a value\n");
            } else if (node->s.stmt_expr->type != rettype->canon_type) {
                error("wrong return type %s, expected %s\n",
                      node->s.stmt_expr->type->name->text,
                      rettype->canon_type->name->text);
            }

            // To indicate the upper recursions that at least one return
            // statement has been seen, push a bogus declaration with a
            // user-undefinable name to declmap.  This declaration will be
            // removed when the function scope ends.
            Decl *bogus = make_decl(retname, rettype->canon_type);
            map_push(&c->declmap, retname, bogus);
        } else {
            error("function does not return a value\n");
        }
        break;
    }
    default:
        fprintf(stderr, "%s: don't know how to walk node kind %d\n",
                __func__, node->base.kind);
        break;
    }
}

static Type *setup_type_from_str(Context *c, char *s)
{
    Name *n = push_name(c->nt, s, strlen(s));
    Type *t = make_type(n, T_CANON, NULL);
    return map_push(&c->typemap, n, t)->value;
}

static void init_context(Context *c)
{
    map_init(&c->declmap);
    map_init(&c->typemap);

    c->i32_type = setup_type_from_str(c, "i32");
    c->i64_type = setup_type_from_str(c, "i64");
}

static void destroy_context(Context *c)
{
    map_destroy(&c->declmap);
    map_destroy(&c->typemap);
}

void sema(NameTable *nt, Node *root)
{
    Context c;
    c.nt = nt;

    init_context(&c);

    walk(&c, root);

    printf("==== declmap ====\n");
    map_print(&c.declmap);
    printf("==== typemap ====\n");
    map_print(&c.typemap);

    destroy_context(&c);
}
