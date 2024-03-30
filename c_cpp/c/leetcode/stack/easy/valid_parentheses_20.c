#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#define ARR_SIZE(a) (sizeof(a) / sizeof(a[0]))

/**
 * @brief 20.有效的括号 valid_parentheses
 *        给定一个只包括"(){}[]"的字符串s，判断字符串是否有效,即：
 *        1. 左括号必须用相同类型的有括号闭合
 *        2. 左括号必须以正确的顺序闭合
 * @example 1⃣️: Input: s = "()"; Output: true
 *          2⃣️: Input: s = "(]"; Output: false
*/
static char pairs(char a);
bool isValid_20(const char* s) {
    int n = strlen(s);
    if (n % 2 == 1) {   // 奇数个一定不成立
        return false;
    }

    int* stack = (int*) malloc(sizeof(int) * (n + 1));
    int top = -1;

    for (int i = 0; i < n; i++) {
        // 当右括号到达，此时栈不空且栈顶是对应的左括号才正确，此时出栈；否则进栈
        char ch = pairs(s[i]);
        if (ch) {
            if (top == -1 || ch != stack[top]) {
                free(stack);
                return false;
            }
            --top;
        } else {
            ++top;
            stack[top] = s[i];
        }
    }

    free(stack);
    return top == -1;
}

static char pairs(char a) {
    if (a == '}') return '{';
    if (a == ']') return '[';
    if (a == ')') return '(';
    return 0;
}

int main() { // for compile
    const char* const test_20[] = {
        "{}[", "", "()[{}]", "(", "}", "({}[()])"
    };
    for (int i = 0; i < ARR_SIZE(test_20); i++) {
        printf("\"%s\"    -> %d\n", test_20[i], isValid_20(test_20[i]));
    }
    return 0;
}