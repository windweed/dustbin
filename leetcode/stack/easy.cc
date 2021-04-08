#include <string>
#include <stack>
#include <unordered_map>

using namespace std;

/**
 * @brief 20 有效的括号 valid-parentheses
 *        给定一个只包括'(', ')', '{', '}', '[', ']' 的字符串s, 判断字符串是否有效。
 *        有效字符串满足：1. 左括号必须和相同类型的右括号闭合。
 *                      2. 左括号必须以正确的顺序闭合
 * @example 1: Input: s = "()"; Output: true
 *          2: Input: s = "()[]{}"; Output: true
 *          3: Input: s = "(]"; Output: false
 *          4: Input: s = "{[]}"; Output: true
*/
class ValidParentheses_20 {
public:
    bool isValid(string s) {
        int len = s.length();

        if (len % 2 == 1) { return false; }

        auto paires = unordered_map<char, char> {
            {')', '('},
            {']', '['},
            {'}', '{'}
        };
        auto workstack = stack<char>();
        for (int i = 0; i < len; i++) {
            char ch = s.at(i);
            if (paires.find(ch) != paires.end()) { // 左括号入栈，右括号搜索
                if (workstack.empty() || workstack.top() != paires.at(ch)) {
                    return false;
                }
                workstack.pop();
            } else {
                workstack.push(ch);
            }
        }
        return workstack.empty();
    }
};