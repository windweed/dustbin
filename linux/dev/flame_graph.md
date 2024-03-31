# 火焰图

火焰图(Flame Graph)是由Linux性能优化大师Brendan Gregg发明的，  
和所有其他的trace和profiling方法不同的是，Flame Graph以一个全局的视野来看待时间分布，它从  
底部往顶部，列出所有可能的调用栈。

以典型的分析CPU时间花费到哪个函数的on-cpu火焰图为例来展开。CPU火焰图中的每一个方框是一个函数，  
方框的长度，代表了它的执行时间，所以越宽的函数，执行越久。火焰图的楼层每高一层，  
就是更深一级的函数被调用，最顶层的函数是叶子函数。

火焰图的生成过程：
1. 先trace系统，获取系统的profiling数据。
2. 用脚本来绘制。

[脚本获取](https://github.com/brendangregg/FlameGraph)

下面通过实例来体验下火焰图的生成过程。
```c
#include <pthread.h>

void func_d(void) { for(int i=0;i<50000;i++); }
void func_a(void) { for(int i=0;i<100000;i++); func_d(); }
void func_b(void) { for(int i=0;i<200000;i++); }
void func_c(void) { for(int i=0;i<200000;i++); }

void* thread_func(void* param) {
    while (1) {
        for (int i=0;i<300000;i++);
        func_a();
        func_b();
        func_c();
    }
}

int main(void) {
    pthread_t tid1, tid2;
    pthread_create(&tid1, NULL, thread_func, NULL);
    pthread_create(&tid2, NULL, thread_func, NULL);
    pthread_join(tid1, NULL);
    pthread_join(tid2, NULL);
    return 0;
}
```
先用类似perf top分析CPU时间主要花费在哪里
```bash
$ gcc test.c -pthread -std=c99
$ ./a.out &
$ sudo perf top
```
接下来生成火焰图(root)
```bash
$ perf record -F 99 -a -g -- sleep 60
[ perf record: Woken up 5 times to write data ]
[ perf record: Captured and wrote 1.338 MB perf.data (11882 samples) ]
$ perf script | ./stackcollapse-perf.pl > out.perf-folded
$ ./flamegraph.pl out.perf-folded > perf-kernel.svg
```
上述程序捕系统的行为60秒，最后调用flamegraph.pl生成一个火焰图。


除了on-cpu的火焰图以外，off-cpu的火焰图，对于分析系统堵在IO、SWAP、取得锁方面的帮助很大。  
有利于分析系统在运行时究竟在等待什么，系统资源之间的彼此伊伴。

关于火焰图的更多细节和功能，可以访问：  
[更多火焰图细节和功能](http://www.brendangregg.com/flamegraphs.html)
