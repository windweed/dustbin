# expect

expect 是建立在TCL基础上的一个自动化交互套件。  
在一些需要交互输入指令的场景下，可通过脚本设置自动进行交互通信。  
其交互流程为：  
`spawn`启动指定进程 -> `expect`获取指定关键字 -> `send`向指定进程发送指定指令 -> 退出。

## 安装

```bash
# 检查是否安装TCL
$ whereis tcl
tcl: /usr/lib64/tcl8.5 /usr/share/tcl8.5
# 若无tcl，则yum安装
$ sudo yum install -y tcl
$ sudo yum install -y expect
# 查看安装路径
$ command -v expect
/usr/bin/expect
$ expect -v
expect version 5.45
```

## 流程

先来一段实例
```bash
expect<<-END
    spawn ssh root@${host}
    expect {
    "yes/no" { send "yes\r";exp_continue }
    "password:" { send "$passwd\r"; }
    }
    expect "*#"

    send "cd /home/mdsoss/bin/\r"
    expect "*"
    send "$rename_string\r"
    expect "*"

    send "rm -rf $local_compress_file\r"
    expect "*"
    send "scp root@${local_ip}:${local_program_path}/${local_compress_file} .\r"
    expect {
    "yes/no" { send "yes\r";exp_continue }
    "password:" { send "$passwd\r"; }
    }
    expect "*#"

    send "tar zxvf ${local_compress_file}\r"
    expect "*"
    send "$chown_string\r"
    expect "*"

    send "$pkill_string\r"
    expect "*"

    send "exit\r"

expect eof
END
```
**解析：**  

* spawn  启动新的交互进程，后面跟命令或指定程序
* expect  从进程中接收信息，如果匹配成功，就往下执行
* send  向进程发送字符串
* send exp_send  用于发送指定的字符串信息
* exp_continue  在expect中多次匹配时会用到
* send_user  用来打印输出。相当于shell中的echo
* interact  允许用户交互
* exit  退出脚本。
* eof  expect执行结束，退出
* set  定义变量
* puts  输出变量
* set timeout  设置超时时间

## 示例2

```bash
#! /usr/bin/expect

set timeout 30

spawn ssh -l root 172.16.22.131
expect "password*"
send "123456\r"
interact

```

ssh
```
[root@YN-DH-CJ-4G-06 ~]# ssh -l root 172.169.1.48
root@172.169.1.48's password: 
Permission denied, please try again.
root@172.169.1.48's password: 
Last failed login: Fri Aug 21 15:29:17 CST 2020 from 172.169.1.47 on ssh:notty
There was 1 failed login attempt since the last successful login.
Last login: Fri Aug 21 15:28:42 2020 from 172.169.1.47
[root@YN-DH-CJ-4G-07 ~]# 


[root@YN-DH-CJ-4G-06 ~]# ssh -l root 172.169.1.48
root@172.169.1.48's password: 
Permission denied, please try again.
root@172.169.1.48's password: 
Last failed login: Fri Aug 21 15:29:17 CST 2020 from 172.169.1.47 on ssh:notty
There was 1 failed login attempt since the last successful login.
Last login: Fri Aug 21 15:28:42 2020 from 172.169.1.47
[root@YN-DH-CJ-4G-07 ~]# 


```



