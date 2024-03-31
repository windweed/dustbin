# shell script

## 1 简单交互

```bash
$ echo -n 'hello'   # -n 不换行，
$ echo -e "hello\c" # 也可不换行  -e 开启转义  
$ printf "%-5s %4.2f" a b  # 负号左对齐， 宽度4，小数保留2位  
$ read -p "input your number: " usernumber
```

## 2 变量 variable

### 2.1 变量定义和使用

```bash
# 在bash shell中，无论赋值时有没有使用引号，值都会以字符串的形式存储
# 等号前后不能有空格
# 以双引号包围变量的值时输出时会先解析里面的变量和命令.  以单引号包围变量的值时原样输出.  
$ variable='hello'
$ variable="world"
$ variable=hello

# 命令的执行结果赋给变量
$ variable=`ls -al`
$ variable=$( ls -al )

# 使用一个定义过的变量
$ echo $variable
$ echo ${variable}

# readonly设置变量只读
$ myurl="http://abc.com"
$ readonly myurl

# unset删除变量(无法删除只读变量)(没什么用)
$ unset log
```

### 2.2 内置变量

* $$  表示当前shell进程的id，即pid。对于shell脚本，就是这些脚本所在的进程id
* $0  当前脚本的文件名
* $n  (n=1,2...)表示传递给脚本或函数的第几个参数. 当n>=10时，需要使用 ${n}
* $#  参数个数
* $*  所有参数
* $@  也是所有参数。被""包围时，与$*稍有不同.
* $?  上个命令的退出状态或函数的返回值

**$\* 与 $@**  
* 不被双引号包含时，都以"$1" "$2"..."$n"的形式输出所有参数   每个变量是独立的
* 被双引号包含时，$* 会将参数作为一个整体，以"$1c$2c...c$n"的形式输出所有参数,
    c为分隔字符，默认空格；  
    $@ 依旧以"$1" "$2"..."$n"的形式输出所有参数  
*一般直接使用$@*

### 2.3 字符串

```bash
# 字符串基础

# 拼接
$ your_name="lilei"
$ greeting="hello, "$your_name" !"
$ greeting_1="hello, ${your_name} !"
$ PATH="$PATH":/home/bin
$ PATH=$PATH:/home/bin
$ PATH=${PATH}:/home/bin

# 获取字符串长度
$ string="abcd"
$ echo ${#string}   #输出4

# 字符串反转
$ str="helloworld"
$ echo ${str} | rev
dlrowolleh

# 查找子字符串
$ string="alibaba is a great company"
$ echo $(expr index "$string" xa)  # 输出1 # 返回的a的位置，从1开始数。若找不到返回0

```
```bash
# 字符串下标切割。 总是从左往右截取

$ string="helloworld"
$ echo ${string:1:4} # 4 为长度
ello
$ echo ${string:1:-5} # -5 是下标
ello
# 从右边开始截取
$ echo ${string:0-5:4} # 4为长度
worl
$ echo ${string:0-5:-3} # -3是下标
wo
```
```bash
# 按指定子字符串切割

$ url="http://c.biancheng.net/index.html"

# #*substr 截取右边字符。往右删。#右边是要删除的内容。
$ echo ${url#*:}
//c.biancheng.net/index.html
$ echo ${url#*ttp:}
//c.biancheng.net/index.html

$ echo ${url#*/}
/c.biancheng.net/index.html
# ##*substr 匹配到最后一个
$ echo ${url##*/}
index.html

# %substr* 截取左边字符
$ echo ${url%/*}
http://c.biancheng.net
# %%substr* 匹配到最左边
$ echo ${url%%/*}
http:

# 实用小技巧
${test##*/}, ${test%/*} 分别是得到文件名，或者目录地址最简单方法。
```
```bash
# 字符串替换

# ${string/substr/replace} 替换第一个
$ s="helloworld"
$ echo ${s/ll/b}
heboworld
# // 全部替换
$ echo ${s//l/b}
hebboworbd

# #, % 前后缀替换(不匹配则不会替换)
$ echo ${s/#ll/b}  # 不匹配则无效果
helloworld
$ echo ${s/#h/b}
belloworld
$ echo ${s/#hel/b}
bloworld
$ echo ${s/%orld/bb}
hellowbb
```

```bash
# 测试与替换

# 测试username这个变量，若不存在则给内容为root
$ username=${username-root}

# 不过有点问题，如果username为空，就会修改原变量
$ username=""
$ username=${username-root}
$ echo $username
 
$ 加上冒号可以解决
$ username=${username:-root}


```


### 2.4 数组

bash只支持一维数组  
```bash
# 普通下标数组

# 数组创建
$ array_name1=(value1 value2 value3)

$ array_name2=(
    value0
    value1
    value2
)

# 读
$ ${array_name[index]}
$ ${array_name}       #输出第一个元素

# 写
$ array_name[0]=value0
$ array_name[1]=value1

# 数组元素个数
$ length=${#array_name[*]}

# 以清单形式打印出数组中的所有值
$ echo ${array_name[*]} 或
$ echo ${array_name[@]}
```

### 2.5 关联数组

从bash 4.0版本开始引入。关联数组可以使用字符串作为数组索引

```bash
# 创建
$ declare -A fruits_value
$ fruits_value=([apple]='100 dollars' [orange]='150 dollars')
$ fruits_value[apple]='102 dollars'

# 访问
$ echo "apple costs ${fruits_value[apple]}"
apple costs 100 dollars

# 列出数组索引:
$ echo ${!array_var[*]}
$ echo ${!array_var[@]}
orange apple
```

### 2.6 数字

```bash
$ no1=4
$ no2=5

# let
$ let res=no1+no2    # =，+前后都不能有空格, $随意
$ let no1++
$ let no1+=$no2   # $可不加

# $(()) and $[]
# 等号前后不能有空格。 内部空格和$都随意
$ result=$(($no1+no2 + 50 ))
$ result=$[ no1 + $no2+5]

# $(expr)
# 等号前后不能有空格。必须有$, 有空格
$ result=`expr 3 + 4`
$ result=$(expr $no1 + 5)
```
推荐使用`let`和`$[]`。  
自增自减和复合赋值最好用let，否则就得 `no1=$[no1+1]`

`$RANDOM`  生成0~32767之间的数值

## 3 判断

`test` 与 `[]`  
-a与， -o或， !非。 优先级： ! > -a > -o

### 3.1 整数判断

-eq相等， -ne不等， -gt大于， -lt小于， -ge大于等于， -le小于等于
```bash
$ a=1; b=2; c=3; d=d

$ [ $a -eq $b ] && echo "equal" || echo "not equal"
not equal

$ test $a -lt $c && echo "a < c" || echo "a >= c"
a < c

# 注意
$ [ $a -lt $d ] && echo "a < d" || echo "a >= d"
-bash: [: d: integer expression expected
a >= d
```

### 3.2 字符串判断

比整数判断更加常用。 `==`, `!=`, `>`, `-z`空, `-n`非空
```bash
$ a=1; b=2; c=3; d=d
$ [ $a == $b ] && echo 1 || echo 0
0
$ [ $a != $b ] && echo 1 || echo 0
1
# 注意。小于最好用双[[]]
# 字符串比较大小，而不是整数比较。数字之间的比较推荐 -eq, -ne等等
$ [ $a < $b ] && echo 1 || echo 0
-bash: 2: No such file or directory
0
$ [[ $a < $b ]] && echo 1 || echo 0
1

# 注意下面的这个f变量并没有定义。因此字符串判空一定要加双引号
$ [ -n $f ] && echo 1 || echo 2
1
$ [ -n "$f" ] && echo 1 || echo 2
2

# 注意，对于空变量/未定义变量，双操作数的情况要加""或变为[[]]保证安全。
# f未定义
$ [ $a == $f ] && echo "eq" || echo "ne"
-bash: [: 1: unary operator expected
ne
$ [ $a == "$f" ] && echo "eq" || echo "ne"
ne
$ [[ $a == $f ]] && echo "eq" || echo "ne"
ne
```

### 3.3 文件判断

* -f  包含正常的路径或文件名 (常用)
* -e  文件存在               (常用)
* -d  文件存在且为目录       (常用)
* -s  文件存在且至少有一个字符(不空)
* 
* -r  文件存在且可读
* -w  文件存在且可写
* -x  文件存在且可执行
* -c  文件存在且为字符型特殊文件
* -b  文件存在且为块特殊文件
* -L  如果给定的变量包含的是一个符号链接,返回真
* -g  是否设置了SGID位
* -k  是否设置了粘着位(Sticky Bit)
* -p  是否是具名管道
* -u  是否设置了SUID位

```bash
$ test -r file -o -x file      # file具有x或r权限时，返回true
$ test ! -x file               # file不具有x权限时，返回true
```

## 4 流程控制

### 4.1 if

```bash
read -p "input a age: " age

if [ $age -le 2 ]; then
    echo "baby"
elif (($age >=9 && $age<=17)); then  # (())
    echo "teenager"
else
    echo "other seq"
fi
```
```bash
# 从文件中按行读取，输出非空行
cat test.txt | while read line
do
    if [ -n "$line" ]; then   #注意必须加引号
        echo $line
    fi
done
# $line 不加引号的话，会把空行也输出。因为有个回车符。加了引号变普通字符串
```

### 4.2 while

```bash
# 计算1+2+...+100
sum=0
i=0
while [ $i -le 100 ]
do
    let sum+=i
    let i++
done
echo $sum
# 5050
```
```bash
# while死循环
while :
while [ 1 ]
```

### 4.3 for

```bash
for str in this is a string
do
    echo $str
done

for i in {1..10}
do
    echo $i
done
```
```bash
# 用ping判断网络状态
network="192.168.1"
for sitenu in $(seq 1 100)
do
    host=${network}.${sitenu}
    #取得ping的回传值
    ping -c 1 -w 1 $host &> /dev/null && result=0 || result=1

    #显示结果是正确的(UP)还是错误的没有连通(DOWN)
    if [ "$result" == 0 ]; then
        echo "Server $host is UP."
    else
        echo "Server $host is DOWN."
    fi
done
```
```bash
# 让用户输入某个目录文件名，然后程序找出某目录内的文件名的权限
read -p "input a directory: " dir

if [ "$dir" == "" -o ! -d "$dir" ]; then
    echo "the $dir is NOT exist in you system."
    exit 1
fi

for filename in $( ls $dir )
do
    perm=""
    absfile=$dir/$filename
    test -r "$absfile" && perm="$perm readable"
    test -w "$absfile" && perm="$perm writable"
    test -x "$absfile" && perm="$perm executable"
    echo "the file $absfile's permission is$perm"
done
```
```bash
# 累加1+2+3+....+n
read -p "input a number: " nu

sum=0
for (( i=1; i<= $nu; i=i +1))
do
    let sum+=i
done

echo $sum
```

### 4.4 switch

```bash
while :
do
    read -p "input a mumber between 1 to 5 " aNum

    case $aNum in
    1|2|3|4|5)
        echo "your number is $aNum"
        ;;
    *)
        echo "you don't select a number between 1 to 5"
        continue
        ;;
    esac
done
```

### 4.5 break 和 continue

`break n` / `continue n` (n为整数)，表示跳出/跳过第几层循环。  
注意，case结构中;;就是break，如果出现break那不是跳出case，而是跳出循环。  

## 5 函数

```bash
# 定义
function fname() {
    echo $1" "$2
    return 1       # 不要想着使用shell的函数的返回值。根本不靠谱。
}

# 调用
fname hello world  # 不要加括号

# 删除
unset .f fname

# 导出
export -f fname
```
