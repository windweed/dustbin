#include "binary_tree_node.h"
#include <stdlib.h>
/**
 * Note: The returned array must be malloced, assume caller calls free().
 */
int* inorderTraversal(struct TreeNode* root, int* returnSize){
    int* result = (int *) malloc(sizeof(int) * 1000);
    
    struct TreeNode** stack = (struct TreeNode *)
                              malloc(sizeof(struct TreeNode) * 1000);
    int top = -1;

    *returnSize = 0;
    struct TreeNode* node = root;
    while (node != NULL || top > -1) {
        while (node != NULL) {
            stack[++top] = node;
            node = node->left;
        }
        node = stack[top--];
        result[(*returnSize)++] = node->val;
        node = node->right;
    }
    free(stack);
    return result;
}
