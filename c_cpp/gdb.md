# GDB

## 启动
1. `$ gdb hello`      # hello为二进制可执行文件  
2. `$ gdb -p 18204`   # 调试 pid 为18204的进程（对于已开启的进程）  
3. `$ gdb hello core.1234`  # 调试coredump

## 基本操作

**attach**  
attach 的效果与 `$ gdb -p 1234` 一样
```bash
$ sudo gdb    # root启动gdb
(gdb) attach 1234  # pid
......
(gdb) detach
```

**run**  
`(gdb) r`    # run 执行  
or  
```bash
(gdb) set args /home/ming/ --sentinel
(gdb) r
```  
or  
`$ gdb --args ./a.out aabb 123`  # 调试带参数的程序(a.out 为程序， aabb 123为参数)   
`(gdb) show args` # 查看argv  

`(gdb) q`    # quit 退出  

**break**  
```bash
(gdb) b f     # 在f函数入口打断点
(gdb) b A::f  # 在A类的f函数入口打断点  
(gdb) b hello.cpp:g      # 在hello.cpp的g函数打断点
(gdb) b hello.cpp:123    # breakpoint 在hello.cpp的123行打断点
(gdb) b 13 if i == 8   # 条件断点
(gdb) b 7 if cnt > 3
(gdb) i b  # infomation 查看所有断点信息  
(gdb) dis 1   #  disable第一个断点  
(gdb) ena 1   #  enable第一个断点  
(gdb) delete 1 # 删除第一个断点
(gdb) d   # 删除全部断点 or
(gdb) d breakpoints # 删除所有断点
```

`(gdb) l`  # 显示源代码  
`(gdb) start`  # 开始调试  
  

**process**  
```bash
(gdb) c    # continue 继续  用于恢复被break终端的流程
(gdb) n    # next 单步跳过  
(gdb) s    # step into单步进入  
(gdb) s 5  # 执行5步
(gdb) set setp-mode on/off # 不跳过/跳过没有调试信息的函数、语句。默认off
(gdb) show setp-mode
(gdb) fin  # finish . step out单步跳出  将当前函数执行完毕
(gdb) return #直接在函数的当前为止返回，不管执行到什么位置
(gdb) until # 简写u，把当前的循环执行完毕后退出循环。
(gdb) until location # 执行到行号或函数名。
```

**display**
```bash
(gdb) p vec  # print 打印myvar的值
$1 = std::vector of length 3, capacity 3 = {1, 2, 3}
(gdb) set print array on # 开启数组显示
(gdb) p vec
$2 = std::vector of length 3, capacity 3 = {
    1,
    2,
    3
}
(gdb) print/d sum 十进制
# o八进制， t二进制，c字符，f浮点，s字符串
# x十六进制，u十六进制显示无符号整型，a十六进制，z同x但打印前导0
# r raw。
(gdb) p/x pinfo   # 打印pinfo的16进制格式
(gdb) p/x $pc     # 当前指令地址

# 比如循环中查看某变量，每次print实在太烦，可以display，每次自动显示
(gdb) display sum
(gdb) display /i $pc 查看当前指令的汇编代码
```

**栈帧**
```bash
# backtrace
(gdb) bt  # backtrace 打印堆栈状态  
```
```bash
# 跳转栈帧
(gdb) f   # 查看当前栈帧信息
(gdb) f 5 # frame 打印第5个栈帧(bt之后)  
(gdb) info frame # 简写info f 显示当前栈帧详细信息
(gdb) info args   # 获取当前栈帧函数参数名及其值
(gdb) i locals  # 查看全部局部变量及其值
```

**shell**
```bash
(gdb) shell clear
```

```bash
(gdb) x /24xb tmpbuffer
0x7fffc37fd060: 0xd4    0xc3    0xb2    0xa1    0x02    0x00    0x04    0x00
0x7fffc37fd068: 0x00    0x00    0x00    0x00    0x00    0x00    0x00    0x00
0x7fffc37fd070: 0xff    0xff    0x04    0x00    0x01    0x00    0x00    0x00
(gdb) 


`(gdb)` info thread   # 查看线程信息
`(gdb)` t 5           # 切换到第五号线程

```

`(gdb) tui` or  
ctrl + x + a  进入图形调试界面

## 问题
问题1  在特定文件中加入断点时提示：  
No source file named file.c. Make breakpoint pending on future shared library load? (y or [n]）

原因： 编译时缺少-g参数。  
解决方法：在CMakeLists.txt 中添加一行  :  
`add_definitions("-g")`

问题2  Missing separate debuginfos, use: debuginfo-install glibc-2.17-196.el7_4.2.x86_64 libgcc-4.8.5-16.el7_4.2.x86_64 libstdc++-4.8.5-16.el7_4.2.x86_64 

解决方法：
```bash
$ sudo yum install nss-softokn-debuginfo --nogpgcheck
$ yum install yum-utils
$ uname -rsp
```
然后到[http://debuginfo.centos.org/7/x86_64/](http://debuginfo.centos.org/7/x86_64/)找对应的rpm包：  
`kernel-debuginfo-common-x86_64-3.10.0-514.el7.x86_64.rpm`  
`kernel-debug-debuginfo-3.10.0-514.el7.x86_64.rpm`  
`kernel-debuginfo-3.10.0-514.el7.x86_64.rpm`

然后`$ rpm -ivh`安装。  
然后`$ debuginfo-install ...`


## watch

```bash
(gdb) p exprs
$2 = std::vector of length 2, capacity 2 = {0x1690e08, "hello", 0x1690e48}
(gdb) watch *(int*)0x1690e48
(gdb) c
......
Hardware watchpoint 2: *(int*)0x1690e48

Old value = 1684614748
New value = 1953722216
0x00007fffff66df634 in __memcpy_ssse3_back () from /lib64/libc.so.6

```
