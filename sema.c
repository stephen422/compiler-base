#include "sema.h"
#include "stretchy_buffer.h"
#include <stdio.h>

// Traverse the AST starting from 'node' as the root node.
void traverse(AstNode *node)
{
    switch (node->type) {
    case ND_FUNCTION:
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
        for (int i = 0; i < sb_count(node->stmt_buf); i++) {
            traverse(node->stmt_buf[i]);
        }
        break;
    default:
        fprintf(stderr, "%s: don't know how to traverse node type %d\n", __func__, node->type);
        break;
    }
}
