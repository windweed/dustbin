/**
 * 21, 合并两个有序链表，简单
 * https://leetcode.cn/problems/merge-two-sorted-lists/
 *
 * brief：
 *     将两个升序链表合并为一个新的升序链表并返回。
 *     新链表是通过拼接给定的两个链表的所有结点组成的
 *
 * tags：链表
 *
 * exmaples：
 *     1. [1,2,4], [1,3,4] => [1,1,2,3,4,4]
 *
 * tips：
 *     1. 节点数目范围 [0,50]
 *     2. -100 <= node.val <= 100
 *     3. l1 和 l2 均按非递减顺序排列
 *
 * solution:
 *     没有使用递归，而是使用简单遍历
*/

#include "lc_list_node.h"

class Solution {
public:
    ListNode* mergeTwoLists(ListNode* list1, ListNode* list2) {
        ListNode dummy, * p = &dummy;
        ListNode* l1 = list1, * l2 = list2;
        while (l1 && l2) {
            if (l1->val < l2->val) {
                p->next = l1;
                l1 = l1->next;
            } else {
                p->next = l2;
                l2 = l2->next;
            }
            p = p->next;
        }
        if (l1 == nullptr) {
            p->next = l2;
        } else if (l2 == nullptr) {
            p->next = l1;
        }
        return dummy.next;
    }
};