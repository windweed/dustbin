#include "treenode.h"

int dfs(struct TreeNode *node, int low, int high) {
    if (!node) {
        return 0;
    }
    if (node->val < low) {
        return dfs(node->right, low, high);
    } else if (node->val > high) {
        return dfs(node->left, low, high);
    } else {
        return node->val + dfs(node->left, low, high) + dfs(node->right, low, high);
    }
}

int rangeSumBST(struct TreeNode* root, int low, int high) {
    return dfs(root, low, high);
}
