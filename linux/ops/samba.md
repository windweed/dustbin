# SAMBA共享文件(windows 访问 linux)

[原文地址](https://www.cnblogs.com/lyrichu/p/6867573.html)  

全程root

安装samba
```bash
$ yum install -y samba
```
关闭防火墙
```bash
$ systemctl stop firewalld.service

$ systemctl disable firewalld

```
关闭selinux
```bash
$ sestatus
...
Current mode:   enforcing
...
$ setenforce 0

$ sestatus
...
Current mode:  permissive
...

# 想要永久生效：
$ vim /etc/selinux/conf
# 将
SELINUX=enforcing
# 修改为
SELINUX=permissive

$ reboot
```


开启samba
```bash
$ service smb status
$ systemctl start smb

$ vim /etc/samba/smb.conf  # 默认即可

$ pdbedit -a xiaoming
   # or
$ smbpasswd -a xiaoming

# 查看用户
$ pdbedit -L

```
windows上

win + R   
`\\192.168.1.10`  
输入账号密码  

或

右键创建快捷方式


注：
`etc/samba/smb.conf`
```bash
[global]
        workgroup = SAMBA
        security = user

        passdb backend = tdbsam

        printing = cups
        printcap name = cups
        load printers = yes
        cups options = raw

[homes]
        comment = Home Directories
        valid users = %S, %D%w%S
        browseable = No
        read only = No
        inherit acls = Yes

[printers]
        comment = All Printers
        path = /var/tmp
        printable = Yes
        create mask = 0600
        browseable = No

[print$]
        comment = Printer Drivers
        path = /var/lib/samba/drivers
        write list = @printadmin root
        force group = @printadmin
        create mask = 0664
        directory mask = 0775

[GnXDR]
    path = /home/czy/F/GnXDR/
    available = yes
    valid users = czy
    read only = no
    browsable = yes
    public = yes
    writable = yes
```

设置samba开机启动
```bash
$ chkconfig smb on
Note: Forwarding request to 'systemctl enable smb.service'.
Created symlink from /etc/systemd/system/multi-user.target.wants/smb.service to /usr/lib/systemd/system/smb.service.
$
```