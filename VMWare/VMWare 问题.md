# 大坑虚拟机

## VM 软件设置

虚拟机设置，网络适配器，右上角勾选 >已连接， 启动时连接

菜单 -> 编辑 -> 虚拟网络编辑器\
右下角更改设置\
VMnet8 NAT模式\
左下，勾选 >使用本地DHCP服务将IP地址分配给虚拟机。\
填写子网，掩码

## VNet8网卡设置

固定ip和掩码。注意网卡ip不是网关ip

## 虚拟机设置

```bash
/etc/sysconfig/network-scripts/ifcfg-ens33

TYPE="Ethernet"
BOOTPROTO="static"
DEFROUTE="yes"
PEERDNS="yes"
PEERROUTES="yes"
IPV4_FAILURE_FATAL="no"
#IPV6INIT="yes"
#IPV6_AUTOCONF="yes"
#IPV6_DEFROUTE="yes"
#IPV6_PEERDNS="yes"
#IPV6_PEERROUTES="yes"
#IPV6_FAILURE_FATAL="no"
#IPV6_ADDR_GEN_MODE="stable-privacy"
NAME="ens33"
UUID="14420076-c596-4bfc-af80-739f8e86af1b"
DEVICE="ens33"
ONBOOT="yes"
DNS1=114.114.114.114
IPADDR=192.168.8.96
NETMASK=255.255.255.0
```

这里从始至终未处理网关

## 虚拟机操作

`$ service network restart`

### 远程终端问题

```bash
# 防火墙状态
$ systemctl status firewalld

# 开启端口 （一般开80和3306）
$ firewall-cmd --zone=public --add-port=80/tcp --permanent
$ firewall-cmd --zone=public --add-port=3306/tcp --permanent

# 重启防火墙
$ firewall-cmd --reload
```

## 处理网关

虚拟机设置NAT修改网关， 虚拟机中配置文件加入`GATEWAY=192.168.1.1`(必须与设置中相同)

## win10防火墙

入站规则 “文件和打印共享(回显请求-ICMPv4-In)”， 启用规则

## 其他

VMWare服务\
W + r， `service.msc`\
打开DHCP, NAT

### 桥接

虚拟网络编辑器， “桥接到”， “自动” 改为 “个人区域网”，应用，再改回“自动”， 应用。

## 不兼容问题

> vmware workstation与device credential guard不兼容。在
> 禁用Device/Credential Gurad后，可以运行VMware Workstation。

点击确定后弹出：

> 传输(VMDB)错误-14: Pipe connection has been broken。

解决：

1.  Win + R, gpedit.msc

本地计算机策略-> 计算机配置->管理模板->系统->Device Gurad。\
打开基于虚拟化的安全。\
禁用。

1.  Win + R, services.msc

本地服务-> HV主机服务->禁用。

1.  控制面板

卸载程序->启用或关闭windows功能-> 去掉勾选Hyper-V

重启

1.  管理员powershell

`bcdedit /set hypervisorlaunchtype off`\
打开：\
`off`变成`auto`\
重启。
