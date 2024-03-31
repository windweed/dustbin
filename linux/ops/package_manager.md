# dpkg与apt

## apt

* apt install
    + `apt install firefox=1.0.0`
    + -f  # 修复依赖关系
* apt remove
* apt purge   # 移除软件包及配置文件
* apt update  # 存储库索引
* apt upgrade # 升级所有可以升级的软件包
* apt autoremove # 自动删除不需要的包
* apt full-upgrade # 升级软件包时自动处理依赖关系
* apt search  # 搜索包
* apt list  # 列出包含条件的包
    + --upgradable  # 可以升级的包
    + --installed
* apt show    # 查询指定软件有多少个版本
    `apt show firefox-esr`
* apt-cache madison vim
* apt-cache policy vim   # 列出所有来源的版本

## dpkg

* /etc/dpkg/dpkg.cfg    # 配置文件
* /var/log/dpkg.log
* /var/lib/dpkg/available   # 安装过的软件包信息
* /var/lib/dpkg/status   # 存放系统现在所有安装软件的状态信息
* /var/lib/dpkg/info     # 记录安装软件包控制目录的控制信息文件

### dpkg数据库

* $ dpkg -l   # 显式所有已安装的deb包


# yum

## 切换yum源为163源。
RHEL 7.3 Server
全程root

# rpm -qa | grep yum
记住输出
eg:
yum-langpacks-0.4.2-7.e17.noarch
yum-3.4.3-150.e17.noarch
yum-rhn-plugin-2.0.1-6.e17.noarch
yum-utils-1.1.31-40.e17.noarch
yum-metadata-parser-1.1.4-10.e17.x86_64

卸载原yum源
# rpm -qa | grep yum | xargs rpm -e --nodeps


到镜像站下载rpm包
http://mirrors.163.com/centos/7/os/x86_64/Packages
eg:
python-urlgrabber-.310-8.e17.noarch.rpm
rpm-4.11.3-25.e17.x86_64.rpm
yum-4.11.3-40.e17.x86_64.rpm
yum-metadata-parser-1.1.4-10.e17.x86_64.rpm
yum-rhn-plugin-2.0.1-10.e17.noarch.rpm
yum-utils-1.1.31-52.e17.noarch.rpm
yum-langpacks-0.4.2-7.e17.noarch.rpm
与第一个命令输出相同. 后面的数字可以不同。要进去镜像站自己找

一次性安装这些rpm包
# rpm -ivh --force rpm..... yum.... yum....

修改配置文件
# wget -O http://mirrors.163.com/.help/CentOS7-Base-163.repo
将该文件放在/etc/yum.repos.d/文件夹下。
# mv CentOS7-Base-163.repo /etc/yum.repos.d/
修改此文件
:1,$s/$releasever/7/g

刷新
# yum clean all
# yum makecache
# yum update

测试
# yum repolist all
status列如果有disable，修改CentOS7-Base-163.repo文件
根据上条命令的repo name列找到对应源，将其enabled参数值修改为1

