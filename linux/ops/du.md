# du

```bash
# 只显示总和大小
$ du -sh 
34M

# 显示全部文件，包括所有子目录里的文件
$ du -ah

# 显示几个文件或文件夹，并统计总和
$ du /etc/dhcp/ default/ -csh
8.0K        dhcp/
12K        default/
20K        total

# 输出当前目录下各子目录使用的空间
$ du -h --max-depth=1
```
