# 1 linux

```bash
$ sudo apt install erlang
# erlang 会被安装到 /usr/lib/erlang
# 注意，如果是apt安装而非源码安装，那么erlang是不安装文档的：
$ swing /usr/lib/erlang -> ls
bin  erts-12.2.1  lib  man  releases  usr
$ sudo apt install erlang-doc
# 目录下多了 /doc 目录：
$ ls
bin  doc  erts-12.2.1  lib  man  releases  usr
$ sudo apt install rebar3
$ rebar3 new app hello
```

注意，这个erlang-doc只是PDF/HTML版，如果想用man来查看，需要安装另一个版本：

```bash
$ apt install erlang-manpages
```

然后就可以用 `$ erl -man maps` 查看文档了(maps是举例)

# 2 mac

```bash
$ brew install erlang
```

# 3 windows

erlang用安装包安装即可

rebar3:，[先下载rebar3](http://rebar3.org/docs/getting-started/), 然后同目录创建`rebar3.cmd`文件：
```
@echo off
setlocal
set rebarscript=%~f0
escript.exe "%rebarscript:.cmd=%" %*
```

# 4 dialyzer 初始化

```bash
$ dialyzer --build_plt --apps erts kernel stdlib
  Creating PLT /Users/janetjiang/Library/Caches/erlang/.dialyzer_plt ...
Unknown functions:
  compile:file/2 (c.erl:385:10)
  compile:forms/2 (escript.erl:204:10)
  compile:noenv_forms/2 (erl_abstract_code.erl:10:9)
  compile:noenv_forms/2 (qlc_pt.erl:444:14)
  compile:output_generated/1 (c.erl:444:10)
  crypto:crypto_one_time/5 (beam_lib.erl:987:11)
  crypto:start/0 (beam_lib.erl:1023:10)
  crypto:strong_rand_bytes/1 (net_kernel.erl:2056:37)
Unknown types:
  compile:option/0 (c.erl:92:19)
  compile:option/0 (erl_expand_records.erl:42:26)
  compile:option/0 (erl_lint.erl:51:47)
  compile:option/0 (qlc.erl:541:32)
  compile:option/0 (qlc_pt.erl:73:32)
 done in 0m9.95s
done (passed successfully)

$ dialyzer ./kvs.erl
```

# 5 启动 epmd

Erlang Port Mapper Daemon (EPMD)

```bash
# start a random Erlang node on your machine
$ erl -sname foo -s init stop -noshell
```

# 6 vscode debug

用vscode打开上面步骤创建的 `/hello`目录，

首先安装插件 `pgourlain.erlang`  
注意，坑点！！不能安装 `erlang-ls.erlang-ls`! 否则可能debugger起不来。

在 `/src/swing_erl_app.erl` 中新建一个 `start/0`并导出, 然后在 `start/0`中填写代码

```
-module(swing_erl_app).
-behaviour(application).

-export([start/2, stop/1, start/0]).

start(_StartType, _StartArgs) ->
    swing_erl_sup:start_link().
stop(_State) ->
    ok.


start() ->
    io:format("hello world~n"),
    start(startType, startArgs).
```

然后新建 `tasks.json` 与 `launch.json`:

```json
// file: tasks.json
{
	"version": "2.0.0",
	"tasks": [
		{
			"label": "rebar3 compile",
			"type": "shell",
			"command": "rebar3 compile",
			"problemMatcher": "$erlang",
			"group": {
				"kind": "build"
			}
		}
	]
}
```

```json
// file: launch.json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Erlang Launch",
            "type": "erlang",
            "request": "launch",
            "cwd":"${workspaceRoot}/",
            "arguments": "-s swing_erl_app start", // 或删除 start
            "preLaunchTask": "rebar3 compile",
        },

    ]
}
```
