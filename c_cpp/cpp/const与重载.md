# 函数重载与const

函数重载时若只以const为区别，则分情况：
```cpp
void f(int a) { cout << a << "int" << endl;}
void f(const int a) { cout << a << "const int" << endl;}
```
编译报错：(其实IDE里已经显示redefinition了)
```bash
Starting build...
/usr/bin/g++ -g /home/czy/tmp/candy.cc -o candy -std=c++11 -ggdb
/home/czy/tmp/candy.cc: In function ‘void f(int)’:
/home/czy/tmp/candy.cc:9:6: error: redefinition of ‘void f(int)’
 void f(const int a) { cout << a << "const int" << endl;}
      ^
/home/czy/tmp/candy.cc:8:6: error: ‘void f(int)’ previously defined here
 void f(int a) { cout << a << "int" << endl;}
      ^

Build finished with error(s).
```
如上图，显示"redefinition"。  
因为这样的情况下，**参数的const与否都影响不到实参的值**。因此权限是一样的。  
同理，下面的函数也无法构成重载：
```cpp
void f(int* a) { cout << a << "int" << endl;}
void f(int* const a) { cout << *a << "const int" << endl;}
```
编译时报错与上面类似。

再看下面的情况，这个可以构成重载
```cpp
void f(int* a) { cout << *a << "int" << endl;}
void f(const int* a) { cout << *a << "const int" << endl;}
```
编译输出信息略，就成功。因为对于第二个函数，解引用a再使用明显是非法的。因此构成重载。  
使用下面的代码测试：
```cpp
int main() {
    int a = 1;
    f(&a);
    const int b = 2;
    f(&b);
    return 0;
}
```
从ide就可以看出调用了不同的函数。

同样，只有返回值不同的话也无法构成重载：
```cpp
int f(int a) { return a + 1;}
const int f(int a) { return a + 1;}
```
出错:
> ambiguates old declaration 'int (int)'
> cannot overload functions distinguished by retrun type alone C/C++(311)

但作为成员函数时，可以有以下效果：
```cpp
class B {
public:
    int b;
    const int g() const {return b;}
    int g() {return b;}
};
```
说明：这里的B::g()函数，若只有返回值不同，依旧无法重载，但，  
第一个g(),即 `const int g() const {return b;}`，后面加上const即可重载。  
这个原因就涉及到const类对象，已经超出了我们的讨论范围。

### 进阶

const函数的重载问题。  

首先运行以下代码，发现 p 和 q 都是6。(gcc4)
```cpp
class B {
    int b;
public:
    B() : b{5}{}
    const int g() const {return b;}
    int g() {return b + 1;}
};

int main() {
    B bb = B();
    int p = bb.g();
    const int q = bb.g();
    cout << p << " " << q << endl;

    return 0;
}
```
这是怎么回事呢？  

运行以下代码，就全明白了：
```cpp
class B {
    int b;
public:
    B() : b{5}{}
    const int g() const {return b;}
    int g() {return b + 1;}
};

int main() {
    B bb = B();
    int p = bb.g();
    const int q = bb.g();
    cout << p << " " << q << endl;

    const B cb = B();
    int cp = cb.g();
    const int cq = cb.g();
    cout << cp << " " << cq << endl;
    return 0;
}
```
附上输出：
```bash
11:35:28 czy ~/tmp 
$ ./candy 
6 6
5 5
```
真相：  

const成员函数(即在函数后面跟const)的作用并不是声明"不会修改成员"，  
而是const对象会调用的函数。  

而当没有重载的非const版本时，const函数会被当作正常函数调用，  
const对象和非const对象都可以使用：
```cpp
class B {
    int b;
public:
    B() : b{5}{}
    int g() const {return b;}
    // int g() {return b + 1;}
};

int main() {
    B bb = B();
    int p = bb.g();
    const int q = bb.g();
    cout << p << " " << q << endl;

    const B cb = B();
    int cp = cb.g();
    const int cq = cb.g();
    cout << cp << " " << cq << endl;
    return 0;
}
```
```bash
$ ./candy
5 5
5 5
```

