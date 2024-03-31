# pipe

无法直接处理stderr;  
管道后面接的命令必须要能够接受stdin的数据;  

## cut

将同一行里面的数据进行分解, 将某一段信息切出来
* -d 指定分隔符
* -f 切割后的数据用-f取出第几段。从1开始数
* -c 以字符(characters)的单位取出固定字符区间。像下标一样。从1开始数。
```bash
# 找出PATH的第五个路径
$ echo $PATH
/bin:/usr/bin:/sbin:/usr/sbin:/usr/local/bin:/usr/X11R6/bin:/usr/games:
$ echo  $PATH | cut -d ':' -f 5
/usr/local/bin

# 将export输出的信息取得第12字符以后的所有字符串
$ export | cut -c 12-

# 用last在显示的登陆者中的信息中仅留下用户名
$ last | cut -d ' ' -f 1
```

## grep

提取包含某些字符的特定行  
`grep [-acinv] '查找字符串' myfile  `
* -n 行号
* -v 反选
* -a 将binary文件以text文件的方式查找数据
* -c 计算找到目标的次数
* -i  忽略大小写
* -A 5 显示结果行的后5行
* -B 6 显示结果行的前6行

```bash
#在last的输出信息中,只要有root就取出,并且仅取第一列
$ last | grep 'root' | cut -d ' ' -f 1
```

## sort uniq wc

*sort* 可以排序,并可以依据不同的数据类型来排序. 建议先使用LANG=C来让语系统一  
*uniq* 将重复的数据仅列出一个显示  
*wc* 统计信息的整体数据

* sort [-fbMnrtuk] [file or stdin]  
    + -u 就是uniq. 相同的数据中, 仅出现一行
    * -t 指定分隔符, 默认tab
    * -k 以那个字段(field)来进行排序    # 常用
    * -n 使用"纯数字"进行排序(默认以文字类型排序,比如10会排在1前面)
    * -f 忽略大小写
    * -b 忽略最前面的空格
    * -M 按月份排序
    * -r 反向排序

* uniq
    + -i 忽略大小写
    + -c 计数    

* wc
    + -l 行数
    + -w 字数
    + -m 字符数

```bash
# eg:个人账号都记录在/etc/passwd下,将账号进行排序
$ cat /etc/passwd | sort

# eg:/etc/passwd以:分隔,以第三列来排序
$ cat /etc/passwd | sort  -t ':' -k 3 或
$ cat /etc/passwd | sort -t ':' -k 3 -n

# eg: 用last将账号取出,仅取出账号列,进行排序后仅取出一位，并要知道每个人登录次数
$ last  | cut -d ' ' -f 1 | sort | uniq -c

# eg: /etc/man.config里有多少相关字,行,字符数
$ cat /etc/man.config | wc
# 输出的三个数字分别代表行数(-l),字数(-w),字符数(-m)

# eg: last最后2行并非账号内容,以一行命令取得本月登录系统的总人次
$ last | grep [a-zA-Z] | grep -v 'wtmp' | wc -l
```

## tr

用来删除一段信息当中的文字,或进行文字信息的转换  
+ -d : 删除信息中的SET1这个字符串
+ -s : 替换掉重复的字符
```bash
# eg:将last输出的信息中所有小写字符变成大写字符
$ last | tr '[a-z]' '[A-Z]'

# eg: 将/etc/passwd输出的信息中的冒号删除
$ cat /etc/passwd | tr -d ':'

# eg: 将/etc/passwd转存成dos断行到/root/passwd,再将^M删除
# 以下为root权限操作
$ cp /etc/passwd /root/passwd && unix2dos /root/passwd # yum install -y unix2dos
unix2dos: converting file /etc/passwd to DOS format ...
$ file /etc/passwd /root/passwd
/etc/passwd: ASCII text
/root/passwd: ASCII text, with CRLF line terminators
$ cat /root/passwd | tr -d '\r' > /root/passwd.linux
$ ll /etc/passwd /root/passwd*
```

## xargs

产生某个命令的参数  
xargs 可以读入stdin的数据,并且以空格符或断行字符进行分辨,将stdin的数据分割成参数  
因为是以空格符作为分隔,所以如果有一些名词含有空格符的时候, xargs可能会误判  
`$ xargs [-0epn] command`  
* -0 : 如果输入的stdin含有特殊字符,例如` \ 空格等,这个参数可以将它还原成一般字符.
    这个参数可以用于特殊状态
* -e : EOF. 后面可以接一个字符串,当xargs分析到这个字符串,就会停止继续工作
* -p : 在执行每个命令的参数时,都会询问用户
* -n : 后面接次数,每次command命令执行时, 要使用几个参数
当xargs后面没有接任何的命令时, 默认时以echo来进行输出

```bash
# 将/etc/passwd内的第一列取出,仅取三行,使用finger这个命令将每个账号内容显示出来
$ cut -d ':' -f1 /etc/passwd | head -n 3 | xargs finger
Login: root
...
# finger account可以取得该账号的相关说明内容,例如上面的输出就是finger root后的结果
# 在本例中,我们使用cut取出账号名称,用head取出三个账号,  
最后由xargs将三个账号的名称变成finger后面需要的参数

# eg: 同上例,但是每次执行finger时,都询问用户是否操作
$ cut -d ':' -f1 /etc/passwd | head -n 3 | xargs -p finger
finger root bin daemon ?...y


# eg: 将所有的/etcpasswd内的账号都以finger查阅,但一次仅查阅5个账号
$ cut -d ':' -f1 /etc/passwd | xargs -p -n 5 finger
finger root bin daemon adm lp ?...y
...
finger uucp operator games gohper ftp ?...y
...

# eg: 同上,但分析到lp就结束这串命令
$ cut -d ':' -f1 /etc/passwd | xargs -p -e'lp' figner
finger root bin daemon adm ?...
# -e'lp'中间没有空格.分析到lp这个字符串时,后面的其他stdin的内容就会被xargs舍弃掉了
```
其实使用xargs的原因是,很多命令其实并不是管道命令,因此通过xargs来提供该命令需要的stdin
```bash
# eg: 找出/sbin下面具有特殊权限的文件名,并使用ls -l 列出详细属性
$ find /sbin -perm +7000 | ls -l
    # 结果仅列出了root所在目录下的文件.因为ls并不是管道命令

$ find /sbin -perm +7000 | xargs ls -l
-rwsr-xr-x 1 root root 10429 May 25 2019sbin/mount.nfs...
```

## 无名管道
在管道命令中,经常会使用前一个命令的stdout来作为这次的stdin,  
某些需要用到文件名(例如tar)来进行处理时,该stdin与stdout可以利用减号`-`来替代  
`$ tar -cvf - /home | tar -xvf -`  
将/home里的文件打包,但打包的数据不是记录到文件,而是传送到stdout;经过管道后,  
将`tar -cvf - /home` 传送给后面的`tar -xvf -` 后面的这个-则是取用前一个命令的stdout,  
因此就不需要再使用文件了  

## 其他管道命令

### col

* -x : tab(^I)转换为space  
* -b : 在文字内有反斜杠时,仅保留反斜杠最后接的那个字符  
```bash
# eg: cat -A 显示出所有特殊按键,最后以col将tab转换为space  
$ cat -A /etc/man.config    <==很多^I.那就是tab  
$ cat /etc/man.config | col -x | cat -A | more  

# eg:将col的man page转存成/root/col.man的纯文本文件
$ man col > /root/col.man
$ vi /root/col.man
    注:输出很多怪异符号
$ man col | col -b > /root/col.man
```
col经常被利用于将man page转存为纯文本文件以方便查阅


### join

处理两个文件之间的数据,且主要是将两个文件当中有相同数据的那一行加在一起
* -t : join默认以空格符分隔数据,并且比对"第一个字段"的数据,如果2个文件相同,
    则将2条数据连成一行,且第一个字段放在第一个
* -i : 忽略大小写差异
* -1 : 第一个文件要用哪个字段来分析
* -2 : 第二个文件要用哪个字段来分析

```bash
# eg: 将/etc/passwd与/etc/shadow相关数据整合成一列
$ head -n 3 /etc/passwd /etc/shadow
$ join -t ':' /etc/passwd /etc/shadow
    #将两个文件的第一字段相同者整合成一行
    #第二个文件的相同字段并不会显示(因为已经在第一行了)

# eg: /etc/passwd第四个字段为GID,此GID记录在/etc/group当中的第三个字段,整合它们
$ head -n 3 /etc/passwd /etc/group
$ join -t ':' -1 4 /etc/passwd -2 3 /etc/group
    #相同的字段部分被移动到最前面了
```
可以根据文件之间的相关性将有关系的数据放在一起  
使用join之前,所需要处理的文件应该要事先经过排序(sort).否则有些对比的项目会被略过


### paste

paste直接将两行贴在一起, 中间以tab键隔开
* -d : 后面可以接分隔字符,默认tab
* \- : 如果file部分写成-, 表示来自stdin的数据

```bash
# eg: 将/etc/passwd 与 /etc/shadow 同一行粘贴在一起
$ paste /etc/passwd /etc/shadow
bin:x:1:1:bin:/bin:/sbin/nologin    bin:*:14126:0:99999:7:::
daemon:x:2:2:daemon:/sbin:/sbin/nologin daemon:*:14216:0:99999:7:::
adm:x:3:4:adm:/var/adm:/sbin/nologin    adm:*:14126:0:999999:7:::

# eg: 先将/etc/group 读出(cat),然后与上例粘贴在一起,且仅取出前三行
$ cat /etc/group | paste /etc/passwd /etc/shadow -|head -n 3
上面的-代表stdin
```

### expand tab转空格，unexpand 空白转tab

`# expand [-t] file`  
* -t : 后可接数字.也可自定义一个tab等于多少个字符.一般8个

```bash
# eg: 将/etc/man.config内行首为MANPATH的字样取出,仅取前三行
$ grep '^MANPATH' /etc/man.config | head -n 3
MANPATH /usr/man
MANPATH /usr/share/man
MANPATH /usr/local/man

# eg: 承上例,将所有符号都列出来(cat)
$ grep '^MANPATH' /etc/man.config | head -n 3 | cat -A
MANPATH^I/usr/man$
MANPATH^I/usr/share/man$
MANPATH^I/usr/local/man$

# eg: 承上例,将tab设置成6个空格
$ grep '^MANPATH' /etc/man.config | head -n 3 | expand -t 6 - | cat -A
MANPATH      /usr/man$
...
```

### split

如果有文件太大,导致一些携带式设备无法复制的问题.这时就可以split了  
split可以将一个大文件根据文件大小或行数来切割成为小文件  
`# split [-bl] file PREFIX`  
* -b : 后面可接欲切割成的文件大小,可加单位,例如b, k, m等
* -l : 以行数来进行切割
* PREFIX : 代表前导符, 可作为切割文件的前导文字
```bash
# eg: 我的 /etc/termcap 有七百多KB, 若想要分成300KB 一个文件时怎么办
$ cd /tmp; split -b 300k /etc/termcap termcap
tmp$ ll -k termcap*
-rw-r--r-- 1 root root 300 Feb 7 10:00 termcapaa
-rw-r--r-- 1 root root 300 Feb 7 10:00 termcapab
-rw-r--r-- 1 root root 189 Feb 7 10:00 termcapac
    #文件名可以随便取,只要写上前导文字,小文件就会以xxxaa, xxxab, xxxac等方式来新建小文件

# eg: 将上面三个小文件合成一个文件,文件名为termcapback
$ cat termcap* >> termcapback

# eg:将使用`ls -al /`输出的信息中,每10行记录成一个文件

tmp$ ls -al / | split -l 10 - lsroot
tmp$ wc -l lsroot*
10 lsrootaa
10 lsrootab
6  lsrootac
26 total
```
