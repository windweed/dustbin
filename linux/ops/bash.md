# bash

## 概念

```bash
# 查看当前系统可用shell
$ cat /etc/shells

# 查看当前用户默认shell
$ echo $SHELL

# 查看当前桌面系统
$ echo $DESKTOP_SESSION

# 查看登入取得的shell:
$ cat /etc/passwd | cut -d ':' -f 7
```

**指令运作顺序**  
相对/绝对路径  >  alias  >  built-in  >  $PATH

**命令执行的判断依据**  
```bash
$ command1, command2    ## 按序执行，无论前一个命令是否成功  
$ command1 && command2  ## 第一个命令返回真，则执行第二个命令 
$ command1 || command2  ## 第一个命令返回假，则执行第二个命令 
```

**bash通配符 wildcard**  
+ \*      0 ~ 无穷个任意字符  
+ ?       一定有一个任意字符  
+ [abcd]  一定有一个字符，可能是abcd中的任一个  
+ [0-9]   数字  
+ [a-z]   小写字母  
+ [^abc]  一定有一个字符，且不是a不是b不是c

## 重定向 Redirection

文件描述符是一个整数, 用来跟踪已打开的文件或数据流  
stdin : 0，    stdout : 1，   stderr : 2  

```bash
# 重定向stdout stderr
$ echo "this is a sample text 1" > temp.txt   # 覆盖文件内容. 不存在时会先新建.  
$ echo "this is a sample text 2" >> temp.txt  # 追加到文件末尾
$ command 2> file
$ command 2>> file

# 分开重定向
$ command < input.txt 1> out.txt 2> err.txt

# stdout stderr合并：
$ find /home -name .bashrc 2>&1 list  # 或  
$ find /home -name .bashrc &> list

# 垃圾桶黑洞装置 /dev/null . 写入/dev/null的全部数据都会被丢弃  
$ command > /dev/null
$ command > /dev/null 2>&1  
# 可以用来关闭日志，或屏蔽烦人的刷屏  
```

```bash
# 双向重定向 tee
# 将数据同时送到文件与stdout  
# -a 追加方式(append)  
$ last | tee last.out | cut -d " " -f1
$ ls -l /home | tee -a ~/homefile | more
$ cat a* | tee out.txt | cat -n
```

```bash
# 输入重定向
# 用cat来建立一个档案
$ cat > catfile
testing
testing2
# <==  这里按下ctrl+d离开

# or

$ cat > catfile << "EOF"
> this is a test
> ok now stop
> EOF  # <== 输入这个关键词, 回车, 立刻就结束,不需要ctrl+d

# or 直接使用文件

$ cat > catfile < ~/.bashrc
$ ll catfile ~/.bashrc
```
