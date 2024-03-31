# other things

## PS1 & 彩色输出

```bash
export PS1='\[\e[32m\]\u@\H \[\e[0m\]'

echo -e "\e[1;31m This is red text \e[0m"
echo -e "\e[1;42m Green Background \e[0m"

# 文字： 30黑色， 31红色， 32绿色， 33黄色， 34蓝色， 35洋红， 36青，37白
# 背景： 40黑色, 41红色, 42绿色, 43黄色, 44蓝色, 45洋红, 46青色, 47白色
```

## bash进站欢迎信息

    /etc/issue, /etc/motd
    issue与PS1一样也有变量
    \d 本地端的日期
    \l 显示第几个终端机接口
    \m 硬件等级
    \n 主机的网络名称
    \o domain name
    \r uname -r
    \t 本地端时间
    \s 操作系统名称
    \v 操作系统版本

    /etc/issue.net 提供给telnet远程登录程序用
    /etc/motd 用户登录后的提示信息

## bash与文件系统及程序的限制关系: ulimit

假如十个人同时登陆，同时开启100个大小为10MB的档案，内存不就爆炸了吗?\
bash可以限制“限制用户的某些系统资源”，包括可以开启的档案数量，可以使用的CPU时间，可以使用的内存总量等。

    $ ulimit [-SHacdfltu] [配额]
    -H :    hard limit, 严格设定。不能超过
    -S :    soft limit警告设定。超限警告
    -a :    后面不接东西。列出所有限制额度
    -c :    某些程序发生错误时，系统可能会将该程序在内存中的信息写成档案(除错用)
        #这种档案被称为核心档案(core file).此为限制每个核心档案的最大容量
    -f :    此shell可以建立的最大档案容量(一般2G)，单位Kbytes
    -d :    程序可使用的最大断裂内存(segment)容量
    -l :    可用于锁定(lock)的内存量
    -t :    可使用的最大CPU时间，单位秒
    -u :    单一用户可使用的最大程序(process)数量

    取消coredump大小限制:
    ulimit -c unlimited

## 中文乱码问题

语言显示与以下有关:

1.  linux默认支持的语系 /etc/sysconfig/i18n
2.  终端(bash)语系  LANG
3.  档案原本编码
4.  开启终端的软件  如gnome

一般来说第三点和第四点一致即可正确显示\
假设某文件档案编码为big5,环境为gnome,启动的终端接口为GNOME-terminal,\
则: `# LANG=zh_TW.big5`\
再在终端接口工具栏-终端机-设定字符编码-中文big5

## 换行符

DOS为`^M$`(CR+LF),Linux为`$$`(LF)\
linux指令执行的判断依据是\[Enter]\
可用命令转换\
`# dos2unix [-kn] file [newfile]`\
`# unix2dos [-kn] file [newfile]`\
\-k:保留档案原本mtime时间格式(不更新档案上次内容经过修订的时间)\
\-n:保留原本旧档,将转换后的内容输出到新档案

## 编码语系转换 iconv

    # iconv --list
    # iconv -f 原本编码 -t 新编码 filename [-o newfile]
    --list: 列出iconv支持的语系数据
    -f: from
    -t: to
    -o file : 如要保留原本的档案,使用-o newname可以建立新编码档案
    eg:
    # iconv -f big5 -t utf8 vi.big5 -o vi.utf8
    eg:
    # 将繁体中文的utf8转成简体中文的utf8
    # iconv -f utf8 -t big5 vi.utf8 | \
    > iconv -f big5 -t gb2312 | iconv -f gb2312 -t utf8 -o vi.gb.utf8

## shift

...
shift #进行一次“一个变量的shift”\
...
shift 3 #进行第二次“三个变量的shift”\
...
shift会移动变量，可以接数字，代表拿掉最前面几个参数的意思\
例，6个参数，one, two, three, four, five, six\
第一次shift后：two, three, four, five, six\
第二次直接拿掉三个： five, six

## shell script的追踪与调试

sh or bash. both work\
`sh [-nvx] scripts.sh`\
\-x : 将使用到的script内容显示到屏幕上。(很有用)(执行过程)\
\-n : 不要执行，仅查询语法上的问题。若无问题，不会显示任何信息\
\-v : 执行script之前，先将script的内容输出到屏幕上\
eg:\
`$ bash -x script.sh`

可以用set -x和set +x对脚本进行部分调试

```bash
#! /bin/bash  
for i in {1..6};
do
    set -x    #set -x 在执行时显示参数和命令
    echo $i
    set +x    #set +x 禁止调试
done
echo "Script executed"
```

只会打印出echo \$i的调试信息.因为使用了-x和+x对调试区域进行了限制\
`set -v` 当命令进行读取时显示输入\
`set +v` 禁止打印输入

自定义调试风格:\
通过传递\_DEBUG环境变量来建立

```bash
#! /bin/bash
function DEBUG()
{
    [ "$_DEBUG"  == "on" ] && $@ || :    #命令:告诉shell不要进行任何操作
}
for i in {1..10}
do
    DEBUG echo $i
done
```

可以将调试功能置"on"来运行上面的脚本\
`$ _DEBUG=on ./script.sh`

利用shebang进行调试:
将`#! /bin/bash` 改为 `#! /bin/bash -xv`

## shell环境变量

set观察所有变量，含环境变量和自定义变量\
OSTYPE, HOSTTYPE, MACHTYPE(主机硬件与核心的等级)\
locale影响显示结果的语系变量\
可以查询支持的语系\
语系档案位于/usr/lib/locale\
LANG    主语言的环境\
LC\_ALL  整体语系的环境

## 获取文件最后修改时间

`$ stat test.tar.gz -c %Y`
