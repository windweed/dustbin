/**
 * 23，合并k个升序链表
 * https://leetcode.cn/problems/merge-k-sorted-lists/
 *
 * brief：
 *     给定一个链表数组，每个链表都已按升序排列。
 *     请将所有链表合并到一个升序链表中，返回合并后的链表。
 *
 * tags：链表，堆 lists, heap
 *
 * exmaples：
 *     1. [[1,4,5],[1,3,4],[2,6]] => [1,1,2,3,4,4,5,6]
 *
 * tips：
 *     1. k == lists.length
 *     2. 0 <= k <= 10^4
 *     3, 0 <= list[i].length <= 500
 *     4, -10^4 <= list[i][j] <= 10^4
 *     5, lists[i] 按升序排列
 *     6，lists[i].length 总和不超过10^4
 *
 * solution:
 *     没有使用递归，而是使用堆
*/

#include "stl.h"
#include "lc_list_node.h"

class Solution {
public:
    ListNode* mergeKLists(vector<ListNode*>& lists) {
        auto cmp = [](ListNode*& n1, ListNode*& n2) {
            return n1->val > n2->val;
        };
        priority_queue<ListNode*, vector<ListNode*>, decltype(cmp)> pq(cmp);
        for (ListNode* head : lists) {
            if (head != nullptr) {
                pq.push(head);
            }
        }

        ListNode dummy, *p = &dummy;

        while (!pq.empty()) {
            ListNode *node = pq.top();
            pq.pop();
            p->next = node;
            if (node->next != nullptr) {
                pq.push(node->next);
            }
            p = p->next;
        }
        p->next = nullptr;
        return dummy.next;
    }
};
