# 1

## 1.1 Lua独立解释器与Lua基本类型

Lua独立解释器(stand-alone interpreter), 通常被命名为`lua`或者`lua53`，`lua5.3`等,\
可以用来执行包含Lua代码的文本文件。源文件为`lua.c`。

比如，创建一个名为`hello.lua`的文件，并输入以下代码：

```lua
print("helo lua")
```

使用lua解释器运行：

```shell
% lua ./hello.lua
```

不带参数调用lua，可进入交互模式(interactive mode)：

```shell
% lua
Lua 5.3.6  Copyright (C) 1994-2020 Lua.org, PUC-Rio
> 
```

可通过EOF(`Ctrl+D`或`Ctrl-Z`)或`os.exit()`退出交互模式。

我们将Lua语言执行的每一段代码(一个文件，或交互模式下的一行)称为一个程序段(chunk)，即一组命令\
或表达式组成的序列。

Lua5.3起，交互模式下的表达式的结果会直接输出；\
Lua5.3以前的版本，想要获得交互模式下表达式的值，需要在表达式前边加一个等号`=`。\
如果不想输出结果，可以在行尾加一个分号`;`

```shell
% lua
> math.pi / 4
0.78539816339745
> a = 15
> a
15
> io.flush()
true
> io.flush();
>
```

可以使用`-i`参数让Lua语言解释器在执行完指定的程序段后进入交互模式：

```shell
% lua -i my_prog
```

上述命令会在执行完`my_prog`文件中的程序段后进入交互模式，这对调试和手工测试很有用。

另一种运行程序段的方式是使用函数`dofile()`，该函数会立即执行一个文件。\
比如，有一个内容如下的文件`lib1.lua`

```lua
function norm(x, y)
    return math.sqrt(x^2 + y^2)
end

function twice(x)
    return 2.0 * x
end
```

然后在交互模式下运行：

```shell
> dofile("lib1.lua")
> n = norm(3.4, 1.0)
> twice(n)
7.0880180586677
>
```

Lua语言中的标识符由字母、数字、下划线组成，不能以数字开头。

下划线+大写字母(比如`_VERSION`) 通常被Lua用作特俗用途，应避免将其用作其他用途。

Lua注释：

```lua
-- 单行注释
print(1)
--[[
    多行注释
    多行注释1
]]
```

技巧，用 `--[[` + `--]]` 包裹代码，放开注释时，第一行加一个`-`即可。

Lua的连续语句间，不需要分隔符。也可以使用分号。

Lua中，全局变量无需声明即可使用。使用未经初始化的全局变量时，得到的结果是`nil`。\
Lua不区分未初始化的变量和被赋值为`nil`的变量。

Lua是一门动态类型语言，共8个基本类型：

*   `nil` 空
*   `boolean` 布尔，`true`和`false`
*   `number` 数值
*   `string` 字符串
*   `userdata` 用户数据
*   `function` 函数
*   `thread` 线程
*   `table` 表

可以使用`type()`函数获取变量类型，返回字符串。

`userdata`可以把任意的C语言数据保存在Lua语言变量中。比如 `io.stdin`。

条件测试中，只有 `false` 和 `nil` 被视为假。

Lua独立解释器，`-e`参数允许在命令行中直接输入代码：

```shell
% lua53 -e "print(math.sin(2))"
0.90929742682568
%
```

`-l` 用于加载库

```ps
PS C:\Users\swing\Desktop\luaNote> lua53 -llib1
Lua 5.3.6  Copyright (C) 1994-2020 Lua.org, PUC-Rio
> twice(123)
246.0
>
```

`lua`命令的完整参数形式：

```shell
% lua [option] [script [args]]
```

解释器在处理参数前，会查找名为`LUA_INIT_5_3`的环境变量，如果找不到，就会再找`LUA_INIT`。\
如果任一个存在，并且其内容为`@filename`，那么解释器就会运行相应的文件。如果不以`@`开头，\
那么就会执行其中的代码。

可以通过预定义的全局变量 `arg` 来获取解释器传入的参数。\
例如，当执行以下命令时，

```shell
% lua script a b c
```

`arg`的索引0中保存的内容为脚本名，之后的索引保存参数，脚本之前的选线位于负数索引上：

```ps
PS C:\Users\swing\Desktop\luaNote> lua53 -i -e "sin=math.sin" lib1.lua 12 "abc" a
Lua 5.3.6  Copyright (C) 1994-2020 Lua.org, PUC-Rio
> arg[-4]
D:\swing\coding\lua\lua-5.3.6_Win64_bin\lua53.exe
> arg[-3]
-i
> arg[-2]
-e
> arg[-1]
sin=math.sin
> arg[0]
lib1.lua
> arg[1]
12
> arg[2]
abc
> arg[3]
a 
> 
```

```lua
function norm(x, y)
    return math.sqrt(x^2 + y^2)
end

function twice(x)
    return 2.0 * x
end
```




