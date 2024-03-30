#include "stl.h"
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

int main() {
    cout << "Hi" << endl;
    return EXIT_SUCCESS;
}
