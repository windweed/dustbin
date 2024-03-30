/**
 * 19，删除链表的倒数第N个节点，中等
 * https://leetcode.cn/problems/remove-nth-node-from-end-of-list
 *
 * brief:
 *     给你一个链表，删除链表的倒数第N个节点，返回链表的头节点
 *
 * tags:
 *     链表，双指针
 *
 * examples:
 *     [1,2,3,4,5],n=2 => [1,2,3,5]
 *
 * tips:
 *     1. 1 <= list.length <= 30
 *     2. 0 <= node.val <= 100
 *     3. 1 <= n <= list.length
*/

#include "lc_list_node.h"

class Solution {
public:
    ListNode* removeNthFromEnd(ListNode* head, int n) {
        ListNode dummy(-1, head);
        ListNode* slow = &dummy;
        ListNode* fast = head;
        while (n--) {
            fast = fast->next;
        }

        while (fast != nullptr) {
            slow = slow->next;
            fast = fast->next;
        }
        slow->next = slow->next->next;

        return dummy.next;
    }
};
