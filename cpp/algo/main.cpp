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

using namespace std;

int main() {
    cout << "Hi" << endl;
    return EXIT_SUCCESS;
}
