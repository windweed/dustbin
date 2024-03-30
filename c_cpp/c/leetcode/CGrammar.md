# C Grammar

[C99扩展](https://gcc.gnu.org/onlinedocs/gcc/C-Extensions.html#C-Extensions)

## 1 变量

**字面量**

*   整型字面量后缀 L/l -> long, U/u -> unsigned。
*   浮点字面量默认double。L/l -> long double, F/f -> float。
*   `3.14; 1E10; 25.; .5; 6.2e4;`

**转义**

*   `\200 \x111`

**有符号和无符号**\
`~x = -(x + 1)`\
`-8 -> 7(0111)的反码 -> 1000`\
`-5 -> 4(0100)的反码 -> 1011`

1.  永远不要为了多出一个二进制位的精度而用无符号数表示数值.
2.  循环中, 如果是无符号类型, 由于++运算会进行模运算, 因此循环变量达到最大值后会变为0,\
    从而循环终止; 如果是有符号类型, 达到最大值后会变为最小负数然后逐渐增大为0, 循环终止.
3.  如果发生整数溢出, 一般的策略是丢弃最高有效位(就是最高位)

### 作用域

略

### 链接属性 linkage

标识符的链接属性(linkage)决定如何处理在不同文件中出现的标识符。\
标识符的作用域与它的链接属性有关，但这两个属性并不相同。

链接属性一共有三种：external(外部), internal(内部), none(无)。

*   *none* 总是被当作单独的个体。也就是说该标识符的多个声明被当作独立不同的实体。
*   *internal* 在同一个源文件内的所有声明中都执行同一个实体，但位于不同源文件中的多个声明则分属不同的实体。
*   *external* 不论声明多少次，位于几个源文件，都表示同一实体。

```c
typedef char* a;
int b;
int c(int d) {
    int e;
    int f(int g);
}
```

缺省情况下，b,c,f的链接属性为external，其余为none。\
因此，如果另一个源文件也包含了标识符b的类似声明并调用函数c，它们实际访问的是\
这个源文件所定义的实体。\
f的链接属性是external是因为它是个函数。

关键字`extern`和`static`用于在声明中修改标识符的链接属性。

如果某个声明在正常情况下具有`external`链接属性，加上static可以使它的链接属性\
变为internal.\
如将上述代码第二行改为：`static int b;`，那么b就将为这个源文件所私有。\
在其他源文件中如果也连接到一个叫做b的变量，那么它们引用的是另一个不同的变量。

类似地，把函数声明为static，可以防止它被其他源文件调用。

static只对缺省链接属性为`external`的声明才有改变链接属性的效果。\
如果第四行改为`static int e;`，效果也完全不同，因为e的缺省链接属性并不是external。

external为一个标识符指定属性后，就可以访问在其他任何位置定义的这个实体。\
`external int k;`表示`k`在别处定义，这里访问的是在其他源文件中声明的外部变量。

用于具有文件作用域的声明时，该关键字可选。然而，如果你在一个地方定义变量，并在使用这个\
变量的其他源文件的声明中添加external关键字，可以使读者更容易理解你的意图。

```c
static int i;
int f() {
    extern int k;
    extern int i; // extern第二次出现，不改变第一次指定的链接属性。还是static
}
```

当extern关键字用于源文件中一个标识符的第一次声明时，它指定该标识符具有external
链接属性。但是，如果它用于该标识符的第二次或以后的声明时，它并不会更改由第一次
声明所指定的链接属性。如，上述代码中的extern int i; 并不修改由第一行指定的i的链接属性。

### 存储类型 storage class

指存储变量值的内存类型。变量的存储类型决定变量何时创建、销毁、它的值将保存多久。\
有三个地方可以用于存储变量：普通内存，运行时堆栈，寄存器。

变量的缺省存储类型取决于它的声明位置。 凡是在任何代码块之外声明的变量总是存储\
于静态内存中，也就是不属于堆栈的内存，这类变量称为静态(static)变量。对于这类\
变量，你无法为它们指定其它存储类型。

静态变量在程序运行之前创建，在程序的整个执行期间始终存在。\
它始终保持原先的值，除非给它一个赋一个新值，或程序结束。

在代码块内部声明的变量的缺省存储类型是自动的(automatic)，也就是说它存储于堆栈中，
称为自动(auto)变量。
在程序执行到声明自动变量的代码块时自动变量才被创建，当程序的执行流离开该代码块时，
这些自动变量便自行销毁。

对于在代码块内部声明的变量，如果给它加上关键字static，可以使它的存储类型从自动
变为静态。注意，修改变量的存储类型在并不表示修改该变量的作用域。

rigister可以用于自动变量的声明，提示它们应该存储与寄存器而不是内存中。
但是，编译器并不一定理睬register关键字。

如果不显式地指定初值，静态变量将初始化为0。

***static总结***\
当用于不同的上下文环境时，static关键字具有不同的意思。
当用于函数定义时，或用于代码块之外的变量声明时，static关键字用于修改标识符的
链接属性，从external改为internal，但标识符的存储类型和作用域不受影响。用这种方式
声明的函数或变量只能在声明它们的源文件中访问。
当用于代码块内部的变量声明时，static关键字用于修改变量的存储类型，从自动变为静态，
当变量的链接属性和作用域不受影响。用这种方式声明的变量在程序执行之前创建，并
在程序的整个执行期间一直存在。

函数总是存储于静态内存中。

## 2 语句

*   for循环中，`continue`直接跳到调整部分。
*   `/` 两个操作数都是整数，执行整除；其他情况执行浮点除法。如果任一操作数为负，结果取决于编译器。
*   `%` 接受两个整数。
*   `<<` 左边丢弃，右边补`0`。
*   `>>` 逻辑移位：左边补0; 算术移位：填充的值与符号位相同。
*   无符号执行的所有移位都是逻辑移位(全部补0), 有符号取决于编译器。
*   `^` 异或。

```c
value |= 1 << bit_num; // 指定位置1
value &= ~(1 << bit_num); // 指定位清0
value ^= 1 << bit_num  //指定位取反

value & 1 << bit_num;   // 测试指定位
```

**类型转换**

1.  整型提升 integral promotion

```c
char a, b, c;
a = b + c;    // a和b被提升成整型，加完后再截短。
```

1.  寻常算术转换 usual arithmetic conversion

```c
// 类型不同时，下面的向上转换
long double
double
float
ulong
long
uint
int
```

1.  精度损失

```c
int -> float // float仅要求6位精度
float -> int // 小数被舍弃
```

**运算符优先级**

```txt
// 结合性： 先算哪边。
()       括号         无
f()      函数调用     左
[] . ->  取成员       左
i++ i--  后缀自增减   左
!        逻辑反       右
~        位取反       右
+ -      正负号       右
++i --i  前缀自增减   右
* &      地址         右
sizeof               右
(int)    类型转换     右
* / % + -  算术符    左
<< >>    移位        左
> >= < <= == !=  比较 左
& ^ |     位运算     左
&& ||     逻辑       左
?:                   无
= += -= ... 赋值     右
,           逗号     左
```

## 3 指针

每一个最小寻址单位被称为一个字节(byte)， 每个字节都包含了存储一个字符所需要的位数，一般为8。\
为了存储更大的值，我们把多个字节合在一起作为一个更大的内存单位，如使用4个字节来保存整数。\
尽管一次使用4个字节，但仍然只有一个地址。至于这个地址是最左还是最右的字节的位置，取决于编译器。

`*p++;` 先执行`*p`, 再执行`p++`。

```c
// 给定一个指向以NULL结尾的指针列表的指针，在列表中的字符串中查找一个特定字符
bool
find_char(char** strarr, char val)
{
    char* str;
    while ((str = *strarr++) != NULL)
    {
        while (*str != '\0')
            if (*str++ == val)
                return true;
    }
    return false;
}
```

## 4 函数

一个没有参数的函数的参数表应该写为(void)，而不是()。

### 可变参数列表

可变参数列表通过宏来实现，这些宏定义于`stdarg.h`文件：

*   类型 `va_list`
*   宏 `va_start`, `va_arg`, `va_end`

<!---->

*   `va_start`接受2个参数，`va_list`变量的名字，和省略号前最后一个有名字的参数。\
    初始化过程把`va_list`变量设置为指向可变参数部分的第一个参数。
*   `va_arg`接受两个参数，`va_list`变量的名字，和参数列表中下一个参数的类型。\
    `va_arg`返回这个参数的值， 并使`va_list`变量指向下一个可变参数。

```c
// 计算指定数量的值的平均值
#include <stdarg.h>

float
average(int n, ...) {
    va_list params;
    int count;
    float sum = 0;

    va_start(params, n);
    for (count = 0; count < n; count ++) {
        sum += va_arg(params, int);
    }
    va_end(params);
    return sum / n;
}

```

### 函数指针

```c
int func(int);
int (*pf)(int)= &func;  // “&”可不加
int ans = (*pf)(25);    // “*”可不加
```

声明  `int (*f[])()`  分析：\
首先，最后面的括号是个函数调用，因此 `(*f[])`是个函数名。返回值为int，参数表为空。\
然后，`[]`的优先级比`*`高，因此`f[]`先执行，所以`f`是个数组名。取出元素再`*`结果为函数，\
因此，`f`数组的成员是函数指针，指向的函数返回值为int，参数表为空。\
因此， 上述表达式声明了一个数组。成员是函数指针。

## 5 数组

在C中，在几乎所有使用数组名的表达式中，数组名的值都是一个指针常量，类型取决于元素的类型。\
与指针不同的是，只有当数组名在表达式中使用时，编译器才会为它产生一个指针常量。\
只有在两种场合下，数组名并不用指针常量来表示

*   `sizeof` 返回整个数组的长度，而不是指向数组的指针的长度。
*   `&` 返回一个指向数组的指针，而不是指向某个常量指针的指针。类型(eg)为`int(*)[N]`.
    而不是 `int*`。如例中的类型，指针+1会走 (4\*N) 个字节。
    以下两句完全等价：

```c
int b[10];
int* c = b;
int* d = &b[0]; // 注意，&b是不对的。
```

除优先级外，取数组元素`[]`和`*`完全相同。\
即：`array[subscript]` <=> `*(array + subscript)`

作为参数时，以下两种方式是等价的：

```c
int f(char* str);
int g(char str[]);
```

因为此处使用数组名并不符合“sizeof或&”，因此实际上传递的是指针常量。\
此时在函数中sizeof(str)，只能得到指针的长度(一般为4)。

**字符数组的初始化**

```c
char msg[] = {'H', 'e', 'l', 'l', 'o', '\0'};
```

这种方式太过于笨拙，因此C语言标准提供了以下方法：

```c
char msg[] = "Hello";
```

注意，这里的`"Hello"`并不是字符串常量，而是初始化列表。\
字面量字符串除了用在初始化一个字符数组时，都是一个字符串常量。

### 多维数组

一维数组的数组名是一个指针常量，它的类型是"指向元素类型的指针"，它指向数组的第一个元素。\
多维数组的第一维的元素实际上是另一个数组。\
以 `int matrix[6][10];` 为例，matrix这个名字的值是一个指向它第一个元素的指针，所以\
matrix是一个指向一个包含10个整型元素的数组的指针。

*   `matrix + 1` 指向下一层。
*   `*(matrix + 1)` 指向第二层第一个元素(就是真正的第一个，不是下标为1)
*   `*(matrix + 1) + 5` 指向第二层第六个元素。
*   `*(*(matrix + 1) + 5)` 等价于 `matrix[1][5]`。

### 数组指针

`int (*p)[10] = matrix;`  p是一个指向拥有10个整型元素的数组的指针。\
当你把p与1个整数相加时，该整数值首先根据10个整型值的长度进行调整，然后再执行加法。\
也因此，声明数组指针一定要给出第二维的长度。\
用这个指针，可以在matrix中一行一行地移动。

```c
// 指向第一个元素：
int* p = &matrix[0][0];
int* q = matrix[0];     // 相当于数组名matrix[0]直接给指针。数组名退化了
```

```c
// 做形参
void func(int (*mat)[10]);
void func(int mat[][10]);
```

来段代码感受下数组指针：

```c
#include <stdio.h>

void f(int a[])      { printf("0x%p, 0x%p\n", a, a + 1); }
void g(int (*a)[10]) { printf("0x%p, 0x%p\n", a, a + 1); }

int main(void) {
    int b[10] = {1, 2, 5, 7, 3, 1};
    f(b);
    g(&b);

    int (*c)[5] = (int(*)[5])b;
    printf("0x%p, 0x%p\n", c, c + 1);

    return 0;
}
// 输出：
$ ./test 
0x0x7ffebb2a55c0, 0x0x7ffebb2a55c4
0x0x7ffebb2a55c0, 0x0x7ffebb2a55e8
0x0x7ffebb2a55c0, 0x0x7ffebb2a55d4

```

### 指针数组

```c
/**
* 判断参数是否与一个关键字列表中的任何单词匹配，并返回匹配的索引值。
* 如果未找到匹配，返回 -1.
*/
int lookup_keywork(char const* const desired_word, char const* keyword_table[],
    int const size)
{
    char const** kwp;
    for (kwp = keyword_table; kwp < keyword_table + size; kwp ++)
    {
        if (strcmp(desired_word, *kwp) == 0)
        {
            return kwp - keyword_table;
        }
    }
    return -1;
}
```

关键字存储于数组中：

```c
char const keyword[][9] = {
    "do",
    "for",
    "if",
    "rigister",
};
```

## 6 结构体

### 结构体的存储分配

结构体其实就是一种内存布局，访问结构体成员其实就是访问偏移量。

编译器按照成员列表的顺序一个接一个给每个成员分配内存。只有在当存储成员时需要满足\
正确的边界对齐要求时，成员之间才可能用于填充的额外内存空间。

```c
struct ALIGN {
    char a;
    int b;
    char c;
};
```

如果某个机器的整型值长度为4个字节，并且起始存储位置必须能够被4整除，那么就会浪费\
6个字节的内存。每个结构体会占据12字节的空间，但实际只是用6个。如下图：

```matrix
1 0 0 0
1 1 1 1
1 0 0 0
```

可以在声明中对结构的成员列表重新排列，让对边界要求最严格的成员首先出现，这种做法可以\
最大限度减少因边界对齐而带来的空间损失。如

```c
struct ALIGN2 {
    int b;
    char a;
    char c;
};
```

```matrix
1 1 1 1
1 1 0 0
```

sizeof 可以得到一个结构的整体长度，包括因边界对齐而跳过的那些字节。\
如果必须确定结构某个成员的实际位置，可以使用`offsetof`宏。`offsetof(type, member);`

### 位段

位段成员只能是 `int`, `signed int`, `unsigned int`中的一种。

以下实现取决于编译器：

*   默认的int是有符号的还是无符号的。
*   整型的位的最大数目。如32位的位段在16位机器上可能有问题。
*   成员在内存中的分配是从左向右还是从右向左。
*   声明2个位段时，若第二个比较大，编译器有可能把第二个位段放在内存的下一个字(WORD)，也\
    可能直接放在第一个位段后面，从而在两个内存位置的边界上形成重叠。

```c
struct CHAR {
    unsigned ch    : 7;
    unsigned font  : 6;
    usingned       : 3;    // 不指定名称，不使用。
    unsigned size  : 16;
};
```

位段提供的唯一优点是简化了源代码。任何可以用位段实现的任务都可以使用移位和掩码来实现。

*   位域在字中是从左到右排列的。
*   不允许为位域指定绝对位置。
*   如果总位宽超过了一个字的容纳能力，则超出部分被置于下一个字中。

实际上，与用一个char或int描述一个二进制位相比，使用位域的方式常常会浪费内存空间。\
原因在于，从一个字中提取一个二进制位，以及修改字中的一个二进制位而不影响其他位，\
需要花费好几个机器指令(这些指令也是存储在内存中的)。\
不要试图通过使用位域来节省内存空间，除非你需要大量数据对象，并且每个对象都包含很小的数据域。

### 指定初始化 (C99)

```c
#include <stdio.h>
struct Person {
    int age;
    char name[20];
};

struct Test {
    struct Person p1, p2;
};

int main() {
    struct Test t = {
        .p1 = {
            .age = 20,
            .name = "xiaoming",
        },
        .p2 = {
            .name = {'a', 'b', 'c'}, // C99标准
            age: 10, // GCC扩展， 有的编译器不支持。
        },
    };

    struct Test tarr[] = {
        [0].p1 = {
            .age = 10,
            name: "hello",
        },
        [1].p1 = {
            .name = "ooooo",
        }
    };

    printf("p1: %d, %s\np2: %d, %s\n", t.p1.age, t.p1.name, t.p2.age, t.p2.name);
    printf("arr: %d, %s\n", tarr[0].p1.age, tarr[0].p1.name);
    return 0;
}
```

```bash
$ gcc -o test test.c --std=c99
$ ./test 
p1: 20, xiaoming
p2: 10, abc
arr: 10, hello
```

### Union

```c
union {float f; int i;} fi = {0.5F};
```

*   如果联合的各个成员具有不同的长度，联合的长度就是它最长成员的长度。
*   联合可以被初始化，但必须是联合第一个成员的类型，而且必须使用花括号(struct初始化方法)。

### Flexible Array Member (C99)

一种语法糖。

结构体中的最后一个成员是一个数组，但长度未给出，或为0。这种数组被称为柔性数组。

前边必须有至少一个其他成员。sizeof返回的结构体大小不包括此数组的内存。

这种结构的产生于对动态结构体的需求。有时，需要在结构体中存放一个长度动态的字符串。\
那，一般做法就是放个char\*:

```c
struct Fam {
    int a;
    double b;
    char* p;
};
```

但这样不利于操作。如果可以把字符串紧接在结构体后面，可以只用一次malloc就解决问题:

```c
char a[] = "Hello World";
struct Fam* pfam = (struct Fam*) malloc(sizeof(struct Fam) + strlen(a) + 1);
strcpy(ps + 1, a);
```

即，`(char*)(ps + 1)` 就是字符串"Hello World"的地址。可以通过结构体指针访问，且\
不占用结构体的空间。

为了简化语法，gcc直接提供了支持：
[GCC Zero-Length-Array](https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html)

```c
#include <stdlib.h>
#include <string.h>

struct line {
    int length;
    char contents[0];   // C99 `char contents[];`
};

int main() {
    int len = 10;
    struct line* myline = (struct line*) malloc(sizeof(struct line) + len);
    myline->length = len;
    memset(myline->contents, 'a', len);
    return 0;
}
```

可变长数组初始化只有在最上层才有效，非最上层只能给空值。VSCode可以识别这个错误。

```c
struct foo {
    int x;
    int y[];
};

struct bar { struct foo z;};

struct foo a = {1, {2, 3, 4}};      // valid
struct bar b = {{1, {2, 3, 4}}};    // invalid
struct bar c = {{1, {}}};           // valid
struct foo d[1] = {{1, {2, 3, 4}}}; // invalid
```

### 7 预处理

C预处理器(preprocessor)在源代码编译前对其进行一些文本性质的操作，\
主要包括删除注释，插入#include文件，定义、替换#define, 确定条件编译部分。

宏不可以递归。

宏展开的顺序：

1.  检查参数，如果包含#define定义的符号, 它们首先被替换。
2.  替换文本，插入到程序中原本的文本的位置。
3.  再次扫描，如果包含了#define定义的符号，则重复上述过程。

**#**  宏参数转字符串

```c
#include <stdio.h>
#define PRINT(FORMAT, VALUE)         \
    printf("The value of " #VALUE    \
        " is " FORMAT "\n", VALUE)

int main () {
    int x = 60;
    PRINT("%d", x + 3);
    return 0;
}
// 输出：
// The value of x + 3 is 63
```

\## 把两边的符号连接起来

```c
#define ADD_TO_SUM(sum_number, value)   \
    sum ## sum_number += value

ADD_TO_SUM(5, 25);
// 扩展为
// sum5 += 25;
```

有些任务只能用宏实现：

```c
#define MALLOC(n, type)    \
    ((type*)malloc((n) * sizeof(type)))
```

`#undef name` 移除宏定义

### 命令行

如，代码中： `int array[ARR_SIZE];`\
那么，命令行可以：  `-DARR_SIZE=100`\
`-Uname` 使程序中符号name的初始定义被忽略。与条件编译结合使用时，很有用。

**#include**\
预处理器删除这条指令，并用包含文件的内容取代之。\
头文件保护虽然有用，但预处理器仍将读入整个头文件。即使这个文件的所有内容都将被忽略。
