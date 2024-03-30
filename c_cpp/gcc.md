# GCC4.8.5

## gcc -v 默认config输出

```bash
Configured with: ../configure --enable-bootstrap
--enable-languages=c,c++,fortran,objc,obj-c++,ada,go,d,lto 
--prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info 
--with-bugurl=http://bugzilla.redhat.com/bugzilla --enable-shared 
--enable-threads=posix --enable-checking=release --enable-multilib 
--with-system-zlib --enable-__cxa_atexit 
--disable-libunwind-exceptions --enable-gnu-unique-object 
--enable-linker-build-id --with-gcc-major-version-only 
--with-linker-hash-style=gnu --enable-plugin 
--enable-initfini-array --with-isl 
--enable-offload-targets=nvptx-none --without-cuda-driver 
--enable-gnu-indirect-function --enable-cet --with-tune=generic 
--with-arch_32=i686 --build=x86_64-redhat-linux
```


## 安装（RHEL7）

```bash
# 准备工作

$ yum -y install gcc-c++
$ yum install glibc-headers gcc-c++
$ export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:/usr/local/lib64
 #： LD_LIBRARY_PATH用于在程序加载运行期间查找动态链接库时指定除系统默认路径外的其他路径

# 下载
$ wget ftp://ftp.gnu.org/gnu/gcc/gcc-4.8.5.tar.gz
$ tar -zxvf gcc-4.8.5.tar.gz
$ mv gcc-4.8.5 /usr/local
$ cd /usr/local/gcc-4.8.5

# 安装依赖  mpfr， gmp,  mpc
$ ./contrib/download_prerequisites

# 创建编译目录
$ mkdir build
$ cd build

# 安装
$ ../configure  --enable-languages=c,c++ --build=x86_64-linux --disable-multilib
    # 可选 --prefix=/usr/local/gcc4.8.5
$ gmake -j$(nproc)
$ sudo gmake install
```

### 错误
```bash
make[2]: Leaving directory '/home/chenzhiyi/source_code/gcc-4.8.5-build'
make[1]: *** [stage1-bubble] Error 2
make: *** [all] Error 2
# 解决： 
$ yum -y install gcc-c++
# 若make成功则只有  Leaving direcory ...... 提示信息
make[2]: *** [all-stage1-gcc] Error 2
```
```bash
# gcc9编译gcc4.8.5
cfns.gperf:101:1: error: ‘const char* libc_name_p(const char*, unsigned int)’ redeclared inline with ‘gnu_inline’ attribute
```
[gcc9编译gcc4.8.5make错误](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=ec1cc0263f156f70693a62cf17b254a0029f4852)

### 收尾工作
```bash
$ vim /etc/profile
# 添加
export PATH=$PATH:/usr/local/gcc/bin
$ source /etc/profile

# 检查版本
$ /usr/local/gcc/bin/gcc -v

# 卸载：
$ cd gcc-4.8.5/build/gcc
$ make uninstall
```

## 使用

`gcc -o functions.o -c functions.cpp -I./include`

`gcc -o main *.o -I./include -L./lib -lgtest -lpthread`

`$ g++ main.cc -o main`步骤拆解：
1. `-E` 生成预处理文件:  `$ g++ -E main.cc -o main.i`
2. `-S` 将`main.i`翻译成汇编文件:  `$ g++ -S main.i -o main.s`
3. `-c` 生成二进制可重定位目标文件:  `$ g++ -c main.s -o main.o`
4. 链接器:  `$ g++ main.o -o main`



