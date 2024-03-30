# 浅拷贝引起的double free问题

```bash
*** Error in `bin/ident_demo': double free or corruption (!prev): 0x000000000247b010 ***
```

```cpp
#include <iostream>
#include <cstring>

using namespace std;

class A
{
public:
    A(const char* uri)
    {
        p = (char*)calloc(6, 1);
        strncpy(p, uri, 6);
    }
    ~A()
    {
        free(p);
        p = NULL;
    }

    char* p;
};

int main()
{
    A testarr[] {"hello", "world"};
    for (auto a : testarr)
    {
        cout << a.p << endl;
    }
    return 0;
}
```

分析：  由于 `for (auto a : testarr)`这里  
执行了复制(浅拷贝)，临时的a被析构时，free掉了对应地址的数据，然后真正的testarr[0]析构时，就doublefree了。  

实质上是由于浅拷贝带来的重复引用同一地址问题。  
一块地址被free掉之后，其他引用同一块地址的指针都失效了。  
当这个重复引用是由临时变量带来的时候，很隐蔽。  

解决： 只要加上引用就ok了。防止其复制。`for (auto& a : testarr)`


