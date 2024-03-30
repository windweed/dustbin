# Lua语言入门

作者：Roberto Ierusalimschy, Waldemar, Luiz. 1993年。

LuaRocks: 一个Lua模块的部署和管理系统。

Lua语言用户社区<http://lua-users.org>。

本笔记基于Lua5.3。

首先，介绍Lua作为独立语言的语言特性（使用Lua独立解释器执行独立的Lua文件，或Lua终端），
然后介绍C语言API。

## Linux

### 安装

    apt install lua5.3
    apt liblua5.3-0
    apt liblua5.3-dev

### 开发

include位置：\
`/usr/include/lua5.3/`

lib位置：

    $ ls /usr/lib/x86_64-linux-gnu/ | grep lua
    liblua5.3.a
    liblua5.3-c++.a
    liblua5.3-c++.so
    liblua5.3-c++.so.0
    liblua5.3-c++.so.0.0.0
    liblua5.3.so
    liblua5.3.so.0
    liblua5.3.so.0.0.0

cmake:
`target_link_libraries(main lua5.3)`
