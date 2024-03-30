# ColoredPrintf

from google/google-test

```c
#include <cstdlib>
#include <cstdarg>
#include <cstdio>

enum GTestColor
{
    COLOR_DEFAULT,
    COLOR_RED,
    COLOR_GREEN,
    COLOR_YELLOW
};

const char* GetAnsiColorCode(GTestColor color)
{
    switch (color) {
    case COLOR_RED:     return "1";
    case COLOR_GREEN:   return "2";
    case COLOR_YELLOW:  return "3";
    default:            return NULL;
    };
}

void ColoredPrintf(GTestColor color, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    printf("\033[0;3%sm", GetAnsiColorCode(color));
    vprintf(fmt, args);
    printf("\033[m");
    va_end(args);
}

int main()
{
    ColoredPrintf(COLOR_GREEN, "hello\n");
    ColoredPrintf(COLOR_RED, "warning: %s\n", "hi");
    ColoredPrintf(COLOR_RED, "warning: %s\n", "6");
    ColoredPrintf(COLOR_RED, "warning: %s\n", "4");
    ColoredPrintf(COLOR_RED, "warning: %s\n", "2");
    ColoredPrintf(COLOR_YELLOW, "hi\n");
    return 0;
}

```