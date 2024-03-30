# extern "C" 深度解析

```c
#ifndef __MY_HANDLE_H__
#define __MY_HANDLE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <errno.h>

typedef void* my_handle_t;

my_handle_t create_handle(const char* name);
int operate_on_handle(my_handle_t handle);
void close_handle(my_handle_t handle);

#ifdef __cplusplus
}
#endif

#endif
```

一般来说没有什么问题。如果这个头文件从来没有被任何C++程序引用的话。  

`__cplusplus` 是一个cpp规范规定的预定义宏。你可以信任的是：所有现代cpp编译器都预先定义了它，  
而C语言编译器则不会。  
所以，如果上述代码被C语言程序引用的话，内容就等价于直接去掉`__cplusplus`部分的文件。  
在这种情况下，`extern "C" {}` 经过预处理后根本就不存在，那么，它和`#include`指令之间的关系  
问题自然也就不存在。

## 1 extern "C"的前世今生

在Cpp编译器中，有一位*暗黑破坏神*，专门从事一门叫做"名字粉碎"(name mangling)的工作。  
当把一个Cpp的源文件投入编译器的时候，它就开始工作，把每一个它在源文件里看到的外部可见的名字  
粉碎得面目全非，然后存储到二进制目标文件的符号表里。

之所以在Cpp的世界里存在这样一个怪物，是因为cpp允许对一个名字给予不同的定义，只要在语义上  
没有二义性就好。比如，你可以让两个函数同名而参数表不同(函数重载)，甚至原型完全一样，只要  
名字空间不同即可。

另外，C++程序的构造方式仍然继承了C语言的传统： 编译器把每一个通过命令行指定的源代码文件看作  
一个独立的编译单元，生成目标文件；然后，链接器通过查找这些目标文件的符号表将它们链接在一起  
生成可执行程序。

编译和链接是两个阶段的事情；事实上，编译器和链接器是两个完全独立的工具。  
编译器可以通过语义分析知道那些同名的符号之间的差别；而链接器却只能通过目标文件符号表中保存的  
名字来识别对象。

所以，编译器进行名字粉碎的目的是为了让链接器在工作的时候不陷入困惑，将所有名字重新编码，  
生成全局唯一，不重复的新名字，让链接器能够准确识别每个名字所对应的对象。

但，C却是一门单一名字空间的语言，也不允许函数重载，即，在一个编译和链接范围之内，C语言不允许  
存在同名对象。比如，在一个编译单元内部，不允许存在同名函数，无论是否用`static`修饰；在一个  
可执行程序对应的所有目标文件里，不允许存在同名对象，无论它代表一个全局变量，还是一个函数。  
所以，C语言编译器不需要对任何名字进行复杂的处理(或仅仅对名字进行简单一致的修饰(decoration),  
比如统一加一个下划线)。

Cpp的缔造者Bjarne Stroustrup在最初就把“能够兼容C，能够复用大量已经存在的C库”列为Cpp的重要  
目标。但，两种语言的编译器对待名字的处理方式是不一致的，这就给链接过程带来了麻烦。

eg，有一个名为`my_handle.h`的头文件，内容如下：
```c
// 省略了头文件保护宏
typedef unsigned int result_t;
typedef void* my_handle_t;
my_handle_t create_handle(const char* name);
result_t operate_on_handle(my_handle_t handle);
void close_handle(my_handle_t handle);
```
函数的实现放在一个叫做`my_handle.c`的C语言代码文件里，内容如下：
```c
#include "my_handle.h"
my_handle_t create_handle(const char* name) {return (my_handle_t)0;}
resutl_t operate_on_handle(my_handle_t handle) {return 0;}
void close_handle(my_handle_t handle) {}
```
然后使用C语言编译器编译`my_handle.c`，生成目标文件`my_handle.o`。由于C编译器不粉碎名字，  
所以在`my_handle.o`的符号表里，这三个函数的名字和源代码文件中的声明是一致的。
```
0000001a T _close_handle
00000000 T _create_handle
0000000d T _operate_on_handle
```
随后，我们想让一个C++程序调用这些函数，所以，它也包含了头文件`my_handle.h`。假设这个Cpp  
源代码文件叫做`my_handle_client.cpp`，内容如下：
```cxx
#include "my_handle.h"
#include "my_handle_client.h"
void my_handle_client::do_something(const char* name) {
    my_handle_t handle = create_handle(name);
    (void) operate_on_handle(handle);
    close_handle(handle);
}
```
然后对这个文件用Cpp编译器编译，生成目标文件`my_handle_client.o`。由于Cpp编译器会对名字进行  
粉碎，所以生成的目标文件中符号表会有如下内容：
```
0000002a s EH_frame1
         U __Z12close_handlePv
         U __Z13create_handlePKc
         U __Z17operate_on_handlePv
00000000 T __ZN16my_handle_client12do_somethingEPKc
00000048 S __ZN16my_handle_client12do_somethingEPKc.eh
```
其中，第2-4行就是那三个函数的名字被粉碎后的样子。  

然后，为了让程序可以工作，你必须将`my_handle.o`和`my_handle_client.o`放在一起链接。  
由于在两个目标文件中对于同一个对象的命名不一样，链接器将报告相关的“符号未定义”错误。  
```
Undefined symbols:
    "close_handle(void*)", referenced from:
        my_handle_client::do_something(char const*) in
my_handle_client.o
..........
```
为了解决这一问题，Cpp引入了链接规范(linkage specification)的概念，表示法为  
extern "language string", Cpp编译器支持"C"和"C++"。

## 2 链接规范的用法

```cxx
extern "C" void foo();
extern "C" {
    void foo();
    int bar();
}
```
对于之前的例子而言，如果把头文件`my_handle.h`的内容改成：
```cxx
#ifndef _MY_HANDLE_H__
#define _MY_HANDLE_H__

extern "C" {

typedef unsigned int result_t;
typedef void* my_handle_t;

my_handle_t create_handle(const char* name);
result_t operate_on_handle(my_handle_t handle);
void close_handle(my_handle_t handle);

}
#endif
```
然后使用C++编译器重新编译`my_handle_client.cpp`，则生成的目标文件`my_handle_client.o`  
中的符号表就变为：
```
00000000 T __ZN16my_handle_client12do_somethingEPKc
00000048 S __ZN16my_handle_client12do_somethingEPKc.eh
         U _close_handle
         U _create_handle
         U _operate_on_handle
```
从中我们可以看出，用`extern "C"`修饰了的声明，其生成的符号和C编译器生成的符号保持了一致。  
再把`my_handle.o`和`my_handle_client.o`链接在一起的时候，就不会再有"符号未定义"错误了。  

但，此时如果重新编译`my_handle.c`，C编译器将报错，因为`extern "C"`是C++的语法，C不支持。  
所以，应该使用`__cplusplus`宏。(代码略去)

以下章节为扩展内容
- - -

## 3 小心门后的未知世界

接下来探讨一个问题，为什么不能把`#include`放在`extern "C"`里面？  
其实还是宏展开的问题。

先看一个例子：`a.h`, `b.h`, `c.h`, `foo.cpp`。  
`foo.cpp`包含`c.h`, `c.h`包含`b.h`, `b.h`包含`a.h`：
a.h:
```c
// a.h __A_H_略
#ifdef __cplusplus
extern "C" {
#endif

void a();

#ifdef __cplusplus
}
#endif
```
b.h:
```c
// b.h __B_H_
#ifdef __cplusplus
extern "C" {
#endif

#include "a.h"

void b();

#ifdef __cplusplus
}
#endif
```
c.h:
```c
#ifdef __cplusplus
extern "C" {
#endif

#include "b.h"

void c();

#ifdef __cplusplus
}
#endif
```
foo.cpp:
```c
#include "c.h"
```
接下来使用C++编译器编译`foo.cpp`：
```cxx
extern "C" {
    extern "C" {
        extern "C" {
            void a();
        }
        void b();
    }
    void c();
}
```
即，`#include`指令放在`extern "C"`里的时候，会造成`extern "C"`的嵌套。这种嵌套是被C++  
允许的。嵌套时以最内层为准。比如在下面的代码中，`foo`会使用C++链接规范，`bar`会使用C链接规范。  
```cxx
extern "C" {
    extern "C++" {
        void foo();
    }
    void bar();
}
```
如果能够保证一个C语言头文件直接或间接依赖的所有头文件也都是C的，那么按照C++语言规范，这种嵌套  
应该不会有问题，但有些编译器会报错(如MSVC2005)。  
不过，应该把`#include`放在`extern "C"`外面来避免嵌套。  比如之前的例子，如果我们把各个  
头文件的`#include`都移到`extern "C"`外面，然后再编译，会得到：
```cxx
extern "C" {
    void a();
}
extern "C" {
    void b();
}
extern "C" {
    void c();
}
```
这样肯定就OK了

把`#include`指令放置在`extern "C"`里的另一个重大风险是可能会无意中改变一个函数声明的链接  
规范。比如，`b.h`包含`a.h`：  
a.h:
```cxx
#ifdef __cplusplus
    void foo(int);  // 本意为C++函数
#else
    void a(int);
#endif
```
b.h:
```cxx
#ifdef __cplusplus
extern "C" {
#endif

#include "a.h"
void b();

#ifdef __cplusplus
}
#endif
```
此时，如果用C++预处理器展开`b.h`将会得到：
```cxx
extern "C" {
    void foo(int); // foo变成了C链接规范
    void b();
}
```

## 4 总结

严格地讲，链接规范只应该修饰函数，变量，函数类型。

如果你可以判断一个头文件永远不可能让C++代码来使用，那就不需要加`extern "C"`。
