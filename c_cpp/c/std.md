# 标准库

## 1 环境库

**库函数失败时**，会在一个外部整型变量errno(errno.h)中保存错误代码。  
`void perror(char const* message);`(stdio.h)函数简化了向用户报告错误的过程。  
如果message不空，perror打印这个字符串，后面跟一个分号，一个空格，然后打印一条用于解释errno  
当前错误代码的信息。  

`void exit(int status);`(stdlib.h)用于终止程序的执行。status返回给操作系统

`void assert(int expression);` 主要用来调试。  
如果测试为假，就向标准错误打印一条诊断信息并**终止程序**。  
这条信息格式由编译器定义，但将包含表达式，源文件名称，行号。  
如果为真，无任何效果，程序继续执行。  
*point*  可以在编译时通过定义`NDEBUG`消除所有断言. `-DNDEBUG`

## 2 IO库

### 2.1 IO概念

操作系统负责与不同硬件通信的细节，并对外提供一个更为简单和统一的I/O接口。C提供了进一步抽象，    
就C程序而言，所有的I/O操作只是简单地从程序移进或移出字节的事情。  
这种字节流便被称为**流**(stream)。  

绝大多数流是**完全缓冲**(fully buffered)，这意味着“读取”和“写入”实际上是从一块被称为  
**缓冲区**(buffer)的内存区域来回复制数据。 用于输出流的缓冲区只有在它写满时  
才会被刷新(flush, 物理写入)到设备或文件中。  

使用stdio时，缓冲状态将因编译器而异。

调试时最好调用printf()后立即调用fflush：
```c
printf("something");
fflush(stdout);
```

流分为两种类型，文本(text)流和二进制(binary)流。

文本流的有些特性在不同的系统中可能不同。  
如文本行的最大长度，文本行的结束方式。  
标准把文本行定义为零个或多个字符，后面跟一个表示结束的换行符。  
库函数负责内外的翻译，如在Windows系统上，输出时，文本中的换行符被写成`\r\n`,  
输入时，文本中的`\r`被丢弃。

二进制流的字节将完全根据程序编写它们的形式写入到文件或设备中，  
完全根据他们从文件或读取的形式读入到程序中。  
如果不希望I/O函数修改文本文件的行末字符，也可以把二进制流用于文本文件。

**struct FILE**  
`FILE`是一个数据结构，用于访问一个流。  
对于每一个ANSI C程序，运行时系统必须提供至少三个流：标准输入(standard input)、  
标准输出(standard output)和标准错误(standard error)。  
这些流的名字分别为`stdin`, `stdout`, `stderr`, 它们都是指向FILE结构的指针。  

修改默认的标准输入与输出设备：  `$ program < data > answer`    

### 2.2 流IO总览

流必须提供`FILE*`。 流必须使用`fopen`打开。 流必须使用`fclose`关闭。

IO函数以三种基本形式处理数据： 单字符，文本行，二进制数据。  
只用于stdin或stdout的IO函数  
|数据类型|输入|输出|描述|
|:-:|:-:|:-:|:-:|
|字符|getchar|putchar|读写单个字符|
|文本行|gets scanf|puts printf|文本行未格式化的输入/输出 // 格式化输入/输出|
|二进制数据|fread|fwrite|读写二进制数据|

可用于所有流的IO函数
|目的|函数|
|:-:|:-:|
|字符输入|fgetc, getc|
|字符输出|fputc, putc|
|文本行输入|fgets|
|文本行输出|fputs|
|格式化输入|fscanf  (内存字符串 sscanf)|
|格式化输出|fprintf (内存字符串 sprintf)|

`FILE* fopen(char const *name, char const *mode);`  
打开一个特定的文件，并把一个流和这个文件相关联。  
`int fclose(FILE *f);`  
关闭前会刷新缓冲区。  成功则返回0，失败返回EOF。

### 2.3 字符流

```c
// 从流中读入
int fgetc(FILE *stream);
int getc(FILE *stream);
int getchar(void); // 从stdin读取一个字符

// 向流中写入
int fputc(int character, FILE *stream);
int putc(int character, FILE *stream);
int putchar(int character);
```
注意，fgetc和fputc是真正的函数，而 getc, putc, getchar, putchar 都是宏

`int ungetc(int character, FILE *stream);` 用于将一个先前读入的字符返回到流中。  
```c
// 把一串从标准输入读取的数字转换为整数
#include <stdio.h>
#include <ctype.h>
int read_int()
{
    int value;
    int ch;
    value = 0;
    while( (ch = getchar()) != EOF && isdigit( ch ) )
    {
        value *= 10;
        value += ch - '0';
    }
    ungetc(ch, stdin);
    return value;
}
```

### 2.4 未格式化行IO

```c
char* fgets(char* buffer, int buffer_size, FILE* stream);
char* gets(char* buffer); // 不要使用。只是为了向后兼容。

int fputs(char const* buffer, FILE* stream);
int puts(char const* buffer);
```

`fgets`从指定的stream读取字符并把它们复制到buffer中。  
当它读到一个换行符并存储到缓冲区，就不再读取。注意换行符也会被读入。  
如果缓冲区内存储的字符数达到 buffer_size - 1 个，也停止读取。  此时并不会丢失数据，因为  
    下一次调用fgets将从流的下一个字符开始读取。  
在任何一种情况下，一个NUL将被添加到缓冲区所存储的数据的末尾，使其成为一个字符串。

`fputs`的buffer参数必须包含一个字符串，预期以NUL结尾。  
逐字写入，因此可能没有换行符，也可能有很多个换行符。  
    也因此fputs可能一次写入一行的一部分，也可能一次写入好几行。

`gets`不会保存换行符。  

`puts`自动添加换行符。

### 2.5 格式化行IO

int fprintf(FILE* stream, const char* format, ...);

[见文档](https://en.cppreference.com/w/c/io)

**printf**  
`printf("%*.*s")`问题：  
第一个 \* 代表位宽，即全部输出的长度，右对齐，第二个 \* 代表真正输出的长度。  

```c
const char* str = "this is test example";
int a = 15, b = 10;
printf("%.*s\n", 10, str);
printf("%*.*s\n", 20, 10, str);
printf("%*.*s\n", a, b, str);

// 输出：
this is te
          this is te
     this is te
```
printf一般格式  
`%[标志][输出最小宽度][.精度][长度]类型`  
1. 类型
    + d 十进制，有符号
    + u 十进制，无符号
    + o 八进制，无符号
    + x,X 十六进制，无符号
    + 
    + f 小数
    + e,E 指数形式
    + g,G 自动选择 %f 还是 %e
    + 
    + c 字符
    + s 字符串
2. 标志
    * \- 左对齐,右边补空格
    * \+ 输出正负符号
    * 空格 输出为正加空格，输出为负加符号
    * \# 自动添加八进制前缀0，十六进制前缀0x，小数有小数部分才给小数点
3. 输出最小宽度
    实际位数多于定义宽度则按实际位数输出，否则补空格或0
4. 精度
    如果输出数字，表示小数的位数，如果输出字符，表示字符个数；若实际位数大于所定义的精度数，则截去超过的部分
5. 长度
    + h 短整型
    + l 长整型

`%-5o\n` 左对齐按八进制输出，最少5位。

printf输出size_t的正确姿势是使用 `%zu`
```
// scanf
int main()
{
    char* szline = "1336878|云南省楚雄市|530000";
    char fNum[32];
    char fCity[128];
    char szHomeId[64];

    sscanf(szline, "%[^|]|%[^|]|%s", &fNum, &fCity, &szHomeId);

    printf("%s  %s  %s\n", fNum, fCity, szHomeId);

    return 0;
}
```

不要使用scanf("%s")和gets(char*)  
一般使用getchar代替scanf:  
int x;  
while ((x = getchar()) != EOF) //...  

## 3 时间库

**处理器时间**  
`clock_t clock(void);` 返回从程序开始执行起处理器所消耗的时间。  
clock()返回一个数字，由编译器定义，通常是处理器时钟滴答值。  
为了将这个时间转换为秒，需要除以`CLOCKS_PER_SEC`  
失败返回-1

**当天时间**  
`time_t time(time_t* returned_value);` 返回当前的日期和时间。  一般存入epoch  
如果参数是一个非NULL的指针，时间值将通过这个指针进行存储。  
标准并未规定时间的编码方式，因此调用两次time再相减得出时间差是不靠谱的。  
失败返回-1  

有了time_t，就可以：
`char* ctime(time_t const* time_value);` 返回一个指向字符串的指针，格式如下：  
`Sun Ju1 4 00:03:00 2008\n\0`  许多编译器使用静态数组实现这个字符串

`double difftime(time_t time1, time_t time2);`  计算时间差
结果转换为秒。  

`struct tm* gmtime(time_t const* time_value);`  把时间转换为世界协调时间(UTC)。  
`struct tm* localtime(time_t const* time_value);` 把时间转换为当地时间。  
`struct tm`可以访问日期时间的各个部分，如年，月，日。

有了tm结构体，之后就可以：
```c
char* asctime(struct tm const* tm_ptr);  // 转换为字符串， 与ctime一样
time_t mktime(struct tm* tm_ptr);
```

## 4 算术库

**随机数**
```c
srand( (unsigned int)time(NULL) );
int i = rand();
```

## 5 字符串库

`strncpy` 最多复制n个，如果strlen(src)小于len，则后面用0填充到len个字节。  
否则不会以0结尾。  
`strncat` 最多复制n个到目标数组后面，但strncat总是在结果后面添加一个NUL，且不会对目标  
数组用NUL字节进行填充。注意，目标数组原先的字符并没有算在strncat的长度中。  
注意，实际拷贝的长度为len + 1(因为NUL)。  

```c
// strrstr
// 在s1中查找s2最右出现的位置，并返回一个指向该位置的指针。
#include <string.h>
char* my_strrstr(char const* s1, char const* s2)
{
    char *last, *current;
    last = NULL;
    if (*s2 != NULL)
    {
        current = strstr(s1, s2);
        while(current != NULL)
        {
            last = current;
            current = strstr(last + 1, s2);
        }
    }
    return last;
}
```

当一个字符串常量出现于表达式中时，它的值是个指针常量。编译器把这些指定字符的一份拷贝  
存储于内存的某个位置，并存储一个指向第一个字符的指针。  

```c
// 二进制转字符，十六进制显示
remainder = value % 10;
if (remainder < 10)
    putchar(remainder + '0');
else
    putchar(remainder - 10 + 'A');

// 偷懒方式
putchar("0123456789ABCDEF"[value % 16]);
```

**strpbrk**  
返回一个指向str中第一个匹配group中任何一个字符的字符位置。未找到返回NULL。

**strspn & strcspn**  
`size_t strspn(char const* str, char const* group);`  
gourp 指定一个或多个字符。strspn返回str起始部分匹配group中任意字符的字符数。  
strcspn对不匹配的字符进行计数。  

```c
// 计算一个指向字符串中第一个非空白字符的指针
ptr = buffer + strspn(buffer, "\n\r\f\t\v");
```

**strtok**  
它从字符串中隔离各个单独的称为*标记(token)*的部分并丢弃分隔符。  
`char* strtok(char* str, char const* sep);`  
sep 定义用作分隔符的字符集合。strtok找到str的下一个标记，并将其用NUL结尾，然后  
返回一个指向这个标记的指针。  

strtok会修改它处理的字符串，因此如果源字符串不能修改，请复制一份。

* 如果strtok的第一个参数不是NULL，函数将找到字符串的第一个标记。同时将保存它在字符串中  
    的位置。  
* 如果strtok的第一个参数是NULL，函数就会在同一个字符串中从这个被保存的位置开始，  
    像前边一样，查找下一个标记。如果字符串中不存在更多的标记，返回NULL。

```c
void print_tokens(char* line)
{
    static char whitespace[] = " \t\f\r\v\n";
    char* token;
    for (token = strtok(line, whitespace); token != NULL;
        token = strtok(NULL, whitespace))
    {
        printf("Next token is '%s'\n", token);
    }
}
```
strtok不能同时解析2个字符串。因此，如果上述for中调用了一个使用strtok的函数，代码将失败。  

**strerror**  
`char* strerror(int error_number);`  
把其中一个错误代码作为参数并返回一个指向用于描述错误的字符串的指针。

### 字符

以下函数，非０为真。
ctype.h
```c
iscntrl
isspace
isdigit
isxdigit
islower
isupper
isalpha
isalnum
ispunct
isgraph
isprint
tolower
toupper
```

**mem系列**  
如果源和目标参数内存可能重叠，用memmove代替memcpy。  
如，将数组x位置开始的元素左移一位。
```c
memmove(x, x + 1, (count - 1) * sizeof(x[0]));
```
memcmp()按照无符号字节进行比较。

