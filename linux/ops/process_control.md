# 进程管理

## 1-进程的观察

### ps

显示某个时间点的进程情况

```bash
$ ps aux # 观察所有进程
$ ps -lA # 同上
$ ps axjf # 连同部分进程树状态
```
**ps参数**
+ -A或-e - 所有进程
+ -a - 与terminal无关的进程
+ -u - 有效使用者
+ x - 完整信息
+ l - 详细信息
+ j - 工作格式
+ -f - 完整输出

ps只建议记住两个命令：  
* `ps aux`
* `ps -l` - 只列出自己的bash相关的进程
```bash
$ ps -l
F S   UID    PID   PPID  C PRI  NI ADDR SZ WCHAN  TTY          TIME CMD
0 S  1000  15440  15439  0  80   0 - 29110 do_wai pts/2    00:00:00 bash
0 R  1000  18702  15440  0  80   0 - 38333 -      pts/2    00:00:00 ps
```
`ps -l` 输出格式详解：  
* F : process flag 这个进程的总结权限
    + 4 : 此进程权限为root
    + 1 : 此子进程仅进行了fork而没有实际执行
* S : STAT 进程状态
    + R : Running
    + S : Sleep, 可唤醒
    + D : Sleep, 不可唤醒。通常是在等待I/O。
    + T : Stop
    + Z : Zombie, 已终止，但无法移出内存。
* UID/PID/PPID : 用户ID/进程ID/父进程ID
* C : 百分比的CPU使用率
* PRI/NI : Priority/Nice的缩写 进程被CPU执行的优先级。越小越高。
    PRI与NI详见[3-进程执行顺序](#3-进程执行顺序)
* ADDR/SZ/WCHAN :
    + ADDR : kernel function, 指出该进程在内存的哪个部分，running进程一般显示 '-'。
    + SZ : 此进程用掉多少内存。
    + WCHAN : 目前进程是否运作中. '-'表示正在运作。
* TTY : 登入者的终端机位置。远程登录则使用动态终端接口(pts/n)
* TIME : 使用掉的CPU时间。
* CMD : 触发指令

```bash
$ ps aux | head -5
USER        PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND
root          1  0.0  0.0 125472  3832 ?        Ss   1月04   0:03 /usr/lib/systemd/systemd --switched-root --system --deserialize 22
root          2  0.0  0.0      0     0 ?        S    1月04   0:00 [kthreadd]
root          3  0.0  0.0      0     0 ?        S    1月04   0:03 [ksoftirqd/0]
root          7  0.0  0.0      0     0 ?        S    1月04   0:01 [migration/0]
```
`ps aux` 输出格式详解
* USER : 使用者账号
* PID : 进程ID
* %CPU : 使用掉的CPU资源百分比
* %MEM : 占用的物理内存百分比
* VSZ : 使用掉的虚拟内存量(KB)
* RSS : 占用的固定的内存量(KB)
* TTY : 所在终端机。若与终端机无关则显示'?'。
* STAT : 与 `ps -l` 的 S 相同。
* START : 启动时间。
* TIME : 实际使用的CPU时间。
* COMMAND : 实际指令。

`$ ps -lA` 显示全部进程。  
`$ ps axjf` 以进程树显示。

#### 僵尸进程

通常，造成僵尸(zombie)进程的成因是因为该进程应该已经执行完毕，或者因故应该要终止了，但是，  
该进程的父进程却无法完整地将该进程结束掉，而造成那个进程一直存在内存当中。  
通常僵尸进程会直接交给`systemd`这支程序来负责。

### top

持续观测

top 基本用法

```shell

```

**top的参数**
* -d : 后接画面更新的秒数。默认3s
* -b : 以批次方式执行top，通常用来输出成文件。
* -n : 与`-b`搭配，输出次数。
* -p : (重要) 指定PID，查看特定进程
    - `shift + H`  查看此进程的线程信息

**top状态下指令**
* ? : 显示帮助
* P : CPU使用资源排序(默认)
* M : 内存使用排序
* N : PID排序
* T : CPU使用时间累计排序
* k : (重要) 给某PID特定信号
* r : 给某PID指定nice值
* H : 显示线程信息

**top输出格式详解**
```bash
top - 18:20:05 up 2 days,  8:40,  2 users,  load average: 0.00, 0.01, 0.05
Tasks: 177 total,   1 running, 176 sleeping,   0 stopped,   0 zombie
%Cpu(s):  0.2 us,  0.0 sy,  0.0 ni, 99.8 id,  0.0 wa,  0.0 hi,  0.0 si,  0.0 st
KiB Mem :  4030416 total,  1821544 free,   214700 used,  1994172 buff/cache
KiB Swap:  4063228 total,  4063228 free,        0 used.  3549384 avail Mem 

   PID USER      PR  NI    VIRT    RES    SHR S  %CPU %MEM     TIME+ COMMAND
     1 root      20   0  125472   3832   2500 S   0.0  0.1   0:03.27 systemd
     2 root      20   0       0      0      0 S   0.0  0.0   0:00.24 kthreadd
     3 root      20   0       0      0      0 S   0.0  0.0   0:03.39 ksoftirqd/0 
     7 root      rt   0       0      0      0 S   0.0  0.0   0:01.94 migration/0
```
* 第一行 : top...
    + 目前时间 : 18:20:05
    + 已开机时间 : up 2 days
    + 登入系统的用户数 : 2 users
    + 1,5,15分钟平均负载 : load average: 0.00, 0.01, 0.05(平均运作几个进程)
* 第二行 : Tasks... 略
* 第三行 : %Cpus... 注意，多核CPU时，按1/2/3切换不同CPU
    + us : user 用户态CPU时间比例
    + sy : system 内核态CPU时间比例
    + ni : nice 运行低优先级进程的CPU时间比例
    + id : idle 空闲CPU时间比例
    + wa : iowait (重要) 处于IO等待的CPU时间比例
    + hi : hard interrupt 处理硬中断的CPU时间比例
    + si : soft interrupt 处理软中断的CPU时间比例
    + st : steal 当前系统运行在虚拟机中时，被其他虚拟机占用的CPU时间比例
* 第四行，第五行: 物理内存/虚拟内存使用情况。
* 第六行 :
    + PID : 略
    + USER : 略
    + PR/NI : Priority/Nice 优先执行顺序。越小越高
    + VIRT : 使用的虚拟内存
    + RES : 使用的物理内存(不包括共享内存)
    + SHR : 使用的共享内存
    + %CPU : CPU使用率
    + %MEM : 使用的内存占比
    + TIME+ : 启动以来所用的全部CPU时间
    + COMMAND : 启动命令。 `top -c`可看到命令行参数。

### pstree

```bash
$ sudo yum install -y psmisc
```

**pstree选项与参数**
* -A : 各进程树以ASCII字符连接
* -U : 各进程树以Unicode字符连接
* -p : 同时列出PID
* -u : 同时列出user

## 2-进程管理

## 3-进程执行顺序






# perf

perf是一款Linux性能分析工具。

Linux性能计数器是一个新的基于内核的子系统，它提供一个性能分析框架，  
比如硬件(CPU, PMU(Performance Monitoring Unit)功能  
和软件功能(软件计数器，tracepoint)功能。

通过perf，应用程序可以利用PMU、tracepoint和内核中的计数器来进行性能统计。  
它不但可以分析制定应用程序的性能问题(per thread)，也可以用来分析内核的性能问题。

## 调优方向

可以以以下三种事件为调优方向：
* Hardware Event - 由PMU部件产生，在特定的条件下探测性能事件是否发生以及发生的次数。比如cache命中。
* Software Event - 内核产生的事件，分布在各个功能模块中，统计和操作系统相关性能事件。比如进程切换，tick数等。
* Tracepoint Event - 内核中静态tracepoint所触发的事件，这些tracepoint用来判断程序运行期间内核的行为细节(这些tracepoint的对应sysfs节点在/sys/kernel/debug/tracing/events目录下)比如slab分配器的分配次数等。

## perf使用

1. annotate - 解析perf record生成的perf.data文件，显示被注释的代码。
2. archive - 根据数据文件记录的build-id,将所有被采样到的elf文件打包。
3. bench - perf中内置的benchmark，目前包括两套针对调度器和内存管理子系统的benchmark。
4. buildid-cache 管理perf的buildid缓存，每个elf文件都有一个buildid, 被用来关联性能数据与elf文件。
5. buildid-list 列出数据文件中记录的所有buildid。
6. diff 对比两个数据文件的差异，能够给出每个符号(函数)在热点分析上的具体差异。
7. evlist 列出数据文件perf.data中所有性能事件。
8. inject 该工具读取perf record工具记录的事件流，并将其定向到标准输出。在被分析代码中的任何一点，都可以向事件流中注入其他事件。
9. kmem 针对内核内存(slab)子系统进行追踪测量的工具。
10. kvm 用来追踪测试运行在KVM虚拟机上的Guest OS。
11. list 列出当前系统支持的所有性能事件。包括硬件性能事件、软件性能事件以及检查点。
12. lock 分析内核中的锁信息，包括锁的争用情况，等待延迟等。
13. mem 内存存取情况。
14. record 收集采样信息，并将其记录在数据文件中。随后可通过其他工具对数据文件进行分析。
15. report 读取perf record创建的数据文件，并给出热点分析结果。
16. sched 针对调度器子系统的分析工具。
17. script 执行perl或python写的功能扩展脚本、生成脚本框架、读取数据文件中的数据信息等。
18. stat 执行某个命令、收集特定进程的性能概况，包括CPI、Cache丢失率等。
19. test 对当前软硬件平台进行健全性测试，可用此工具测试当前的软硬件平台是否支持perf的所有功能。
20. timechart 针对测试期间系统行为进行可视化的工具。
21. top 对系统性能进行实时分析。
22. trace 关于syscall的工具。
23. probe 用于定义动态检查点。

**全局性概况**  
* perf list - 查看当前系统支持的性能事件。
* perf bench - 对系统性能进行摸底。
* perf test - 对系统进行健全性测试。
* perf stat - 对全局性能进行统计。

**全局细节**  
* perf top - 实时查看当前系统进程函数占用率情况。
* perf probe - 自定义动态事件。

**特定功能分析**  
* perf kmem - 针对slab子系统性能分析。
* perf kvm - 针对kvm虚拟化分析。
* perf lock - 分析锁性能。
* perf mem - 分析内核调度器性能。
* perf sched - 分析内核调度器性能。
* perf trace - 记录系统调用轨迹。

**record**
* perf record - 记录信息到perf.data。
* perf report - 生成报告。
* perf diff - 对两个记录进行diff。
* perf evlist - 列出记录的性能事件。
* perf annotate - 显示perf.data函数代码。
* perf archive - 将相关符号打包，方便在其他机器进行分析。
* perf script - 将perf.data输出可读性文本。

**timechart**
* perf timechart record - 记录事件。
* perf timechart - 生成output.svg文档。

```shell
$ perf top -p 12345   # 进程
$ perf top -t 67890   # 线程
```
