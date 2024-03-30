# std::atomic

总是应该将atomic object初始化。  
* `store()`(赋予一个新值)  
* `load()`(取当前值)  
* `exchange(val)` 赋值val并返回旧值a的拷贝。

```cpp
#include <iostream>
#include <atomic>
#include <thread>

using std::atomic;
using std::thread;

atomic<long long> data{10};

void do_work()
{
    // +3，返回新值的拷贝
    data.fetch_add(3, std::memory_order_relaxed);
}

void sub_work()
{
    data.fetch_sub(1, std::memory_order_relaxed);
}

int main()
{
    thread th1{do_work};
    thread th2{sub_work};
    thread th3{do_work};
    thread th4{sub_work};
    thread th5{do_work};

    th1.join();
    th2.join();
    th3.join();
    th4.join();
    th5.join();

    std::cout << "Result: " << data << std::endl;

    return EXIT_SUCCESS;
}

// $ g++ main.cc -lpthread -std=c++11
// 17

```