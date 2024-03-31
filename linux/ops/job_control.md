# Job Control 

工作管理

## 1_&_直接将指令放到后台

```bash
$ tar -zpcf /tmp/etc.tar.gz /etc &
[1] 54211
## a few seconds later
[1]+ Done       tar -zpcf /tmp/etc.tar.gz /etc
```
中括号里面是工作号码(job number), 与bash有关。  
后面是PID。  
完成后会出现"Done"的提示。

## 2_C_z_将工作丢到后台暂停

```bash
$ vim ~/.bashrc
# 一般模式下按下 Ctrl + Z
[1]+ Stopped       vim ~/.bashrc
```

## 3_jobs_查看目前的后台工作状态

```bash
$ jobs
[1]+ Stopped       vim ~/.bashrc
```
加号(+)代表预设的取用工作。即如果直接输入fg，则此工作被拿到前台。

其实， '+'代表最近被放到后台的工作号码，'-'代表最近最后第二个被放到后台的工作号码。超过第三个，就没有+-号了。

**jobs参数**
+ -l - 列出PID
+ -r - 只列出run的工作
+ -s - 只列出stop的工作

## 4_fg_将后台工作拿到前台

```bash
$ fg %2
$ fg 3
```

## 5_bg_让后台工作开始运行

执行此命令后，工作对应的命令会变为&状态，即后台状态
```bash
$ bg %1
```

## 6_kill_管理后台的工作

```bash
$ kill -15 %1
```
信号值可以用`kill -l`查看，或`man 7 signal`。

## 7_nohup_脱机管理

不挂断地运行命令  

如果工作需要进行一大段时间，又不能放到后台，那么，可以使用at，或者nohup。

nohup可以在脱机或注销系统后，让工作继续进行，忽略所有挂断(SIGHUP)信号。用 & 放到后台后，退出ssh界面命令也会继续执行。

用法：  
`$ nohup ./GnXDR &`  

由于即使退出ssh连接nohop也会不停打印日志，因此处理不当会造成磁盘爆满，定位文件位置后(ls -lh)，  
可执行

```bash
# 保留一万条数据，其他删掉
log = `tail -n 10000 nohup.out`
echo "$log" > nohup.out
```

直接 `rm -fr nohup.out` 是没有用的，要先杀掉程序。  
不能杀掉的话也可以选择清空它：  
```bash
$ echo "" > nohup.out

or

$ cat /dev/null > nohup.out
```

```bash
# 直接选择不生成
$ nohup ./GnXDR > /dev/null 2> log &

or

$ nohup ./GnXDR &> /dev/null &

```


## 8_crontab_定时任务

```bash
$ sudo yum install cronie
$ sudo systemctl start crond
```

```bash
SHELL=/bin/bash
PATH=/sbin:/bin:/usr/sbin:/usr/bin
MAILTO=root

# For details see man 4 crontabs

# Example of job definition:
# .---------------- minute (0 - 59)
# |  .------------- hour (0 - 23)
# |  |  .---------- day of month (1 - 31)
# |  |  |  .------- month (1 - 12) OR jan,feb,mar,apr ...
# |  |  |  |  .---- day of week (0 - 6) (Sunday=0 or 7) OR sun,mon,tue,wed,thu,fri,sat
# |  |  |  |  |
# *  *  *  *  * user-name  command to be executed

# 每五分钟执行一次
*/5 * * * * czy /home/czy/F/GnXDR/trunk/src/gtest/middle/src/notify/notify_new.sh > /home/czy/tmp/fg.log
```


