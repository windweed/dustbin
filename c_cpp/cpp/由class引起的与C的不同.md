# 由class引起的与C的不同

## 1. 函数指针

```cpp
// .h
class A
{
    int add1(int, int);
    int add2(int, int);
    typedef int (A::*addFunc)(int, int);
    addFunc myadd_arr_[2];
};

//.cpp
A::A()
{
    myadd_arr[0] = &add1;
    (this->*myadd_arr[0])(1, 2);
}
```

## 2. pthread入口

首先 `pthread_create` 需要的线程函数类型为 `void* (*)(void*)`，  
普通的成员函数作为`pthread_create`的线程函数会出现问题。  
因为当线程函数被封装在类中，this指针会被作为参数隐式传入函数中，  
即类的成员函数在经过编译器处理后，会变成带有this指针参数的全局函数。  
从而和线程函数参数(void*)不匹配，无法通过编译。

转成static函数可以使成员变量转为不带this指针的全局函数，但又无法在该函数中访问其他非静态方法/变量。  

因此都是传递this指针多套一层。

```cpp
class MyClass {
private:
    pthread_t tid;
public:
    // 线程入口
    void startThread() {
        pthread_create(&tid, NULL, router, (void*)this);
    }
private:
    // 实际工作内容
    void doSth() {cout << endl;}
    // 中介router
    static void* router(void* p) {
        MyClass* q = (MyClass*) p;
        q->doSth();
    }
};
```




