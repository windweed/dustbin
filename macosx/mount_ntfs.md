# mount\_ntfs

利用mac自带的mount\_ntfs工具解决mac无法写入NTFS硬盘的问题

## 查询移动硬盘的硬盘名

首先在不插入硬盘的情况下使用`mount`查看现有的硬盘挂载，
然后插入硬盘再执行一次，查看自己的硬盘名。也可以使用`grep ntfs`过滤。
一般名称为`/dev/disk2s1 on /Volumes/XXX ......`
其中，`/dev/disk2s1`为硬盘名，`/Volumes/XXX` 是该磁盘显示的目录名。

## 解除挂载

`umount /dev/disk2s1`

如果权限不够，就用sudo

## 以可读写模式重新挂载硬盘到指定目录

    $ mkdir ~/Desktop/mnt
    $ cd ~/Desktop/mnt
    $ mount_ntfs -o rw,nobrowse /dev/disk2s1 ~/Desktop/mnt

