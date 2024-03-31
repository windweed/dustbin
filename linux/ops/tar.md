# tar

## tar本身参数

* -c 产生新的压缩包
* -f 指定包的名字
* -x 解压
* -v 显示所有过程
    `$ tar -cf all.tar *.jpg`  # 将所有.jpg文件打包成一个名为all.tar 的包

* -r 增加文件
    `$ tar -rf all.tar *.gif`  # 将所有.git文件增加到all.tar的包里面去

* -u 更新文件
    `$ tar -uf all.tar logo.gif`

* -t 列出文件
    `$ tar -tf all.tar`


## tar调用其它程序

### gzip gunzip

GNU组织的。 后缀为 .gz 。参数为 -z
```bash
$ tar -czf all.tar.gz *.jpg    # 压缩
$ tar -xzf all.tar.gz          # 解压
$
$ gzip -d all.gz
$ gunzip all.gz
```

### bzip2 bunzip2

后缀为 .bz2 。参数为 -j
```bash
$ tar -cjf all.tar.bz2 *.jpg   # 压缩
$ tar -xjf all.tar.bz2         # 解压
$
$ bzip2 -d all.bz2
$ bunzip all.bz2
```

### xz

后缀为 .xz  参数为 -J
```bash
-z, --compress    # 压缩
-d, --decomprss   # 解压
-k, --keep        # 不要删除输入文件
```

### compress uncompress

后缀 .Z     参数 -Z
```bash
$ tar -cZf all.tar.Z *.jpg     # 压缩
$ tar -xZf all.tar.Z
$
$ uncompress all.Z
```


## zip

```bash
$ zip all.zip *.jpg
$ unzip all.zip
```
