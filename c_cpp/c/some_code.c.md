
/**
 * 偶校验
*/
int
even_parity(int value, int n_bits)
{
    int parity = 0;
    while (n_bits > 0)
    {
        parity += value & 1;
        value >>= 1;
        n_bits--;
    }
    return parity % 2 == 0;
}


/**
 * 递归。
 * 许多教科书都把计算阶乘和斐波那契数列用来说明递归，这是非常不幸的。
 * 在第一个例子里，递归并没有提供任何优越之处，
 * 在第二个例子里，它的效率之低非常恐怖。
*/

/**
 * 把一个整数从二进制形式转换为可打印字符。
 * @example 4276 -> '4' '2' '7' '6'
 * @param value - 无符号整型，前导0被删除
*/
void
binary_to_ascii(uint value)
{
    uint quotient;
    quotient = value / 10;
    if (quotient != 0)
        binary_to_ascii(quotient);
    putchar(value % 10 + '0');
}

/*
 * 当你使用递归方式实现一个函数之前，先问问自己，使用递归带来的好处是否抵得上它的代价。
 * 这个代价可能比当初看上去要大得多。
*/

// 递归实现的阶乘
long
factorial(int n) {
    return n <= 0 ? 1 : n * factorial(n - 1);
}
// 非递归方式实现的阶乘
long
factorial(int n) {
    int result = 1;
    while (n > 1) {
        return *= n;
        n--;
    }
    return result;
}

// 递归实现的斐波那契数
long
fibonacci(int n) {
    return n <= 2 ? 1 : fibonacci(n - 1) + fibonacci(n - 2);
}
// 非递归实现的斐波那契数
long
fibonacci(int n) {
    long result, previous_result, next_older_result;
    result = previous_result = 1;
    while (n > 2) {
        n--;
        next_older_result = previous_result;
        previous_result = result;
        result = previous_result + next_older_result;
    }
    return result;
}

// 手撕strcpy
void
strcpy(char* buffer, char const* string) {
    while ((*buffer++ = *string++) != '\0');
}

// 接受一个无符号整型值，把它转换为字符，并打印，前导0被去除。
void binary_to_ascii(unsigned int value)
{
    unsigned int quotient;
    quotient = value / 10;
    if (quotient != 0)
    {
        binary_to_ascii(quotient);
    }
    putchar(value % 10 + '0');
}



