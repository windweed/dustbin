# ldconfig

查看某个库是否安装：

```bash
$ ldconfig -p | grep "xxx"
```

`ldconfi`主要是在默认搜索目录`/lib`和`/usr/lib`以及动态库配置文件\
`/etc/ld.so.conf`内所列目录下，搜索可共享的动态链接库，  进而创建出动态装入程序`ld.so`所需的连接和缓存文件。\
缓存文件默认为`/etc/ld.so.cache`，此文件保存已排好序的动态链接库名字列表，为了让动态链接库为系统\
所共享，需运行动态链接库的管理命令`ldconfig`。
