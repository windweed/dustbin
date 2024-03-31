# netstat

* -a 列出所有连接
* -t tcp
* -u udp
* -n 禁用域名解析。只显示ip。否则显示经典名称(如ssh,localhost等)
* -l listening
* -p 进程信息
* -e 更多信息，如用户名  UID会以root显示
* -s 打印网络统计数据，包括某个协议下的收发包数量
* -r 打印内核路由信息，与route相同
* -i 打印网络接口信息(网卡)
    -ie 打印出来的就和ifconfig一样了
* -c 持续输出
* -g 输出IPv4和IPv6的多播组信息

`$ watch -d -n0 "netstat -atnp | grep ESTA"`  
    # 监视active状态的连接 # active即ESTABLISHED状态


重新启动系统注册表文件  
`/etc/init.d/syslogd restart`
