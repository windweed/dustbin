# SSH

ssh-keygen、ssh-agent、scp、sftp都是OpenSSH提供的一些辅助工具软件。

```bash
$ sudo yum install openssh
$ ssh -V
```

# 基本使用方式

```bash
$ ssh hostname # 域名或主机名。将使用客户端的当前用户名。
$ ssh user@hostname  # 指定用户名
$ ssh -l user hostname # 用 `-l` 指定用户名
$ ssh -p 8888 baidu.com  # `-p` 指定端口
$ ssh -F /usr/local/ssh/other_config # 指定配置文件
```


## 文件权限

linux下 \~/.ssh 目录权限要为700,

authorized\_keys和私钥和config文件的权限要为600，\
公钥权限为644，
known\_hosts权限为644。

## 生成

```bash
$ ssh-keygen -t rsa -C "abc@xxx.com"
Enter file in which to save the key (/Users/caichenghan/.ssh/id_rsa): 
```

输入自定义名字

## 禁止root远程登录

```bash
$ vim /etc/ssh/sshd_config

# 修改
PermitRootLogin yes
# 为
PermitRootLogin no

# 重启sshd
$ systemctl restart sshd.service
# 注意修改后一定要检查以下能不能上去再退出
$ ssh xiaoming@localhost -p newport
```

## CRT防止超时掉线

Options -> Global Options -> General -> Default Session ->\
Edit Default Settings... -> Terminal -> Anti-idle -> Send protocol NO-OP

# 多个KEY

首先，key是按邮箱生成的。\
即生成key时要指定邮箱。\
生成key时，可以指定不同的名称。如`id_rsa_1`, `id_rsa_2`

## 查看系统ssh-key代理

```bash
$ ssh-add -l
```

如果输出

    The agent has no identities.

则说明没有代理，有则可以通过`ssh-add -D`删除。

然后依次将不同的ssh添加代理：

```bash
$ ssh-add ~/.ssh/id_rsa1
Identity added: .....
$ ssh-add ~/.ssh/id_rsa2
Identity added: .....
```

如果提示以下信息:\
`Could not open a connection to your authentication agent.`

那么运行：

```bash
$ eval `ssh-agent -s`
Agent pid 5123
```

OR

```bash
$ ssh-agent bash
```

再运行`ssh-add`

## 创建\~/.ssh/config

```bash
Host github.com
HostName github.com
User czy
IdentityFile /home/czy/.ssh/id_rsa


Host github-zii
HostName github.com
User czy
IdentityFile /home/czy/.ssh/id_rsa_163czy

```

未加入配置文件的网站会自动使用id\_rsa

后续配置单独项目时这样添加：

```bash
$ git remote add test git@github-zii:windweed/ForD.git 
```

而不能直接使用`git@github.com`

测试SSH

```bash
$ ssh -T git@github-zii.com
Hi xxx! You've successfully...
```

proj/.git/config

```
[core]
    repositoryformatversion = 0
    filemode = true
    bare = false
    logallrefupdates = true
[user]
    name = czy
    email = ziijchen@163.com
[remote "github"]
    url = git@github-zii:windweed/HSUTILS.git
    fetch = +refs/heads/*:refs/remotes/github/*
[branch "main"]
    remote = github
    merge = refs/heads/main

```


