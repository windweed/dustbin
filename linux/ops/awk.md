# awk

## 参数

*   \-F 指定输入分隔符，可以是string或REG。默认空格
*   \-v 传递外部变量
*   \-f 从脚本文件中读取awk.  脚本里直接写单引号里面的部分就可以

## 内置变量

*   \$0       当前整行。包含分隔符。
*   \$n       当前行的第n个字段。 从1开始数
*   FS       字段分隔符(默认是任何空格) 一般在BEGIN块或者用-F指定
*   NF       字段数量    最后一个字段为`$NF`, 倒数第二个为 `$(NF-1)`
*   NR       记录数，在执行过程中对应于当前行号。多文件记录递增   # END{print NR} 统计行数
*   FNR      类似于NR，不过多文件记录不递增，都从1开始
*
*   FILENAME 文件名
*   OFMT     数字的输出格式(默认 %.6g)
*   OFS      输出字段分隔符(默认一个空格)
*   ORS      输出记录分隔符(默认一个换行符)
*   RS       记录分隔符(默认一个换行符)

## 语法

`$ awk 'BEGIN{FS="|"; i=0; print "Start";} {/^root/ print; i++} END{print i, "End";}' hello.txt`
整个脚本必须置于单/双引号中\
语句之间由换行符或分号隔开

BEGIN pattern END三部分都是可选的

BEGIN块在从输入流中读取之前执行。 END块在读到EOF后执行。\
pattern块逐行扫描文件，读入一行，执行一次，直至文件被读取完毕。没有提供pattern块则默认执行print

**print**\
print 参数以逗号分隔，打印以空格作为定界符\
可双引号拼接变量\
`$ echo | awk '{ var1="v1"; var2="v2"; var3="v3"; print var1"="var2"="var3; }'`\
v1=v2=v3

**printf**\
`$ awk '{printf("filename:%10s,linenumber:%s,column:%s", FILENAME, NR, NF)}' myfile`

## 模式

REG用 // 包围\
关系运算符\
模式匹配    ~ 匹配   ~! 不匹配\
eg 搜索/etc/passwd有root关键字的所有行,并显示对应的shell\
`$ awk -F ":" '/root/{print $7}' /etc/passwd`\
/bin/bash

eg 打印第四列以100开头的行\
`$ awk '{if($4 ~ /^100*/) print $0;}'`

## 外部值传递 -v

eg

```bash
$ VAR=10000
$ echo | awk -v VARIABLE=$VAR '{ print VARIABLE }'
10000
```

## 算术

操作数自动转为数值， 所有非数值变为0\
eg：\
统计某个文件夹下的文件占用的字节数, 以MB显示\
`$ ll | awk 'BEGIN{size=0;} {size=size+$5;} END{print "size is ", size/102/1024, "MB"}'`

## 数组

下标可以是数字和字母\
eg： 显示/etc/passwd的账户

```bash
$ awk -F ':' 'BEGIN{count=0;} {name[count] = $1; count++;} \
END{for (i=0; i<count; i++) print(i, name[i])}' /etc/passwd`  
```

默认初始化为0或者说空字符串\
delete可以删除数组元素\
`delete array_name[idx]`

## 流程

`next`\
跳过当前行\
一般用于多行合并

## 函数

`substr($2, 1, 10)`\
取第二列，从第一个字母开始，取10个。（从1开始数）

`index($1, "9999")`  搜索

## 重定向

```bash
{
   print (adfsdfsf) >> "hello.txt";
}
```



# &#x20;实例



```txt

# file: teams.txt 

Bucks Milwaukee    60 22 0.732
Raptors Toronto    58 24 0.707
76ers Philadelphia 51 31 0.622
Celtics Boston     49 33 0.598
Pacers Indiana     48 34 0.585
```

1,显示包含“0.5”的所有记录：

```bash
% awk '/0.5/ {print $0}' teams.txt 
Celtics Boston     49 33 0.598
Pacers Indiana     48 34 0.585
```



2, 显示包含“0.5”的所有记录中的第一和第二个字段：

```bash
% awk '/0.5/ {print $1, $2}' teams.txt
Celtics Boston
Pacers Indiana
```



3, 显示数字开头的记录，并打印第一个字段：

```bash
% awk '/^[0-9]/ {print $1}' teams.txt 
76ers
```

4.1, 显示第二个字段中包含“ia”的所有记录：

```bash
% awk '$2 ~ /ia/' teams.txt 
76ers Philadelphia 51 31 0.622
Pacers Indiana     48 34 0.585
```

4.2, 显示第二个字段中不包含“ia”的所有记录：

```bash
% awk '$2 !~ /ia/' teams.txt
Bucks Milwaukee    60 22 0.732
Raptors Toronto    58 24 0.707
Celtics Boston     49 33 0.598
```

5, 显示第四个字段大于等于30的记录：

```bash
% awk '$4 >= 30' teams.txt 
76ers Philadelphia 51 31 0.622
Celtics Boston     49 33 0.598
Pacers Indiana     48 34 0.585
```

6, 显示从"Raptors" 到 "Celtics" 的所有记录：(范围模式)

```bash
% awk '/Raptors/, /Celtics/' teams.txt 
Raptors Toronto    58 24 0.707
76ers Philadelphia 51 31 0.622
Celtics Boston     49 33 0.598
```

7, 显示第四个字段等于31到第四个字段等于34的记录：(范围模式2)

```bash
% awk '$4 == 31, $4 == 34' teams.txt 
76ers Philadelphia 51 31 0.622
Celtics Boston     49 33 0.598
Pacers Indiana     48 34 0.585
```

