// simplify-path 71 medium
/**
 * @file 71. simplify-path 简化路径
 * 给定一个字符串`path`，表示某个Unix风格的绝对路径(以'/'开头)，请将其简化。
 * 
 * 即简化掉 . .. // 结尾/
 * @example Input: path = "/a/./b/../../c/"; Output: "/c".
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

char *
simplifyPath(const char *path)
{
    size_t n = strlen(path);
    
    char* result = (char *) malloc(sizeof(char) * (n + 1));
    memset(result, '\0', n + 1);

    char** stack = (char **) malloc(sizeof(char *) * (n + 1));
    int top = -1;

    char* s = strdup(path);
    for (char* p = strtok(s, "/"); p != NULL; p = strtok(NULL, "/")) {
        if (strcmp(p, "..") == 0 && top > -1) {
            --top;
        } else {
            stack[++top] = p;
        }
    }

    if (top == -1) {
        result[0] = '/';
    } else {
        while (top > -1) {
            strcat(result, "/");
            char* q = stack[top--];
            strcat(result, q);
        }
    }

    free(stack);
    return result;

}

int
main()
{
    printf("%s\n", simplifyPath("/a/b/../c/"));
    printf("%s\n", simplifyPath("/../"));
    return EXIT_SUCCESS;
}
