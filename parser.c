typedef struct ast_t ast_t;

struct ast_t {
    int type;
    ast_t *child;
};
