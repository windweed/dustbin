# systemctl

## 旧版本

`$ service iptables stop`  
`$ /etc/init.d/iptables stop`

## RHEL7

改用了systemctl, 代替service和chkconfig
```bash
# 查看启动了的服务
$ systemctl list-unit-files | grep enabled

# 状态
$ systemctl status firewalld.service

# 开启
$ systemctl start firewalld.service

# 关闭
$ systemctl stop firewalld.service

# 重启
$ systemctl restart firewalld.service

# 开机启用
enable
# 开机禁用
disable
# 查看服务是否开机启动
# systemctl is-enabled firewalld.service; echo $?

```
