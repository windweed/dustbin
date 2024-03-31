# rz,sz

## 安装

```bash
$ sudo yum install lrzsz
```
```bash
$ tar zxvf lrzsz-0.12.20.tar.gz
$ cd lrzsz-0.12.20
$ ./configure
$ make
$ sudo make install

$ ln -s /usr/local/bin/lrz /usr/bin/rz
$ ln -s /usr/local/bin/lsz /usr/bin/sz
```

## 使用

CRT -> Options -> Session options -> X/Y/Zmodem 设置上传下载目录

```bash
$ rz -ybe
$ sz
```
