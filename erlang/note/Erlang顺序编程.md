# Erlang shell

Unix从命令行启动(`$ erl`), windows从开始菜单启动。
退出按`Ctrl + C`(windows按`Ctrl + Break`)，后接`a`。
`a`会立即停止系统，可能导致数据损坏。受控关闭应该用`q().` 即quit. `q()`是`init:stop()`在eshell里的别名。
要立即停止系统，执行`erlang:halt()`。

打开erlang shell:

```shell
$ erl
Erlang R16B ...
1> 1 + 2.
3
```

Erlang用百分号注释。

不能把所有东西都输入eshell。Erlang模块里的语法形式不是表达式，不能被shell理解。具体来说，不能输入附注。就是以连字符开头的事物(比如`-module`, `-export`)。

可以按`Tab`尝试扩展当前模块或函数的名称。

eshell的一些内建命令：

*   `pwd()` 打印当前工作目录
*   `ls()` 列出当前工作目录中所有文件名
*   `cd(Dir)` 修改当前目录到`Dir`

# 整数

Erlang可以用任意长度的整数执行整数运算。

```erlang
2> 123456789 * 987654321 * 112233445566778899 * 998877665544332211.
13669560260321809985966198898925761696613427909935341
3>
```

## 多进制

```erlang
4> 16#cafe * 32#sugar. % 16进制，32进制
1577682511434
5>
```

## \$记法

`$a` 就是代表a的整数(97)。 与C语言的char型相似，`'a'` 代表97。

```erlang
3> $a.
97
4> $0.
48
```

# 浮点数

Erlang内部使用64位的IEEE 754-1985浮点数。因此使用浮点数的程序会存在和C一样的浮点数取整与精度问题。

```erlang
2> -2.3e+6.
-2.3e6
5> 5/3.
1.6666666666666667
6> 4/2.
2.0
7> 5 div 3.
1
8> 5 rem 3.
2
9> 5 div 2.
2
10>
```

# 变量

Erlang的变量必须以大写字母开头。只能赋值一次。
已被指派一个值的变量称为*绑定变量*，否则为*未绑定变量*。

变量的作用域是它定义时所处的语汇单元。函数子句之间的变量是不相关的。

在Erlang中，变量获得值是一次成功模式匹配操作的结果。
`Lhs = Rhs`的真正意思是：计算右侧(Rhs)的值，然后将结果与左侧(Lhs)的模式进行匹配。
变量只是模式的一种简单形式。

如果要忽略某个变量，可以用下划线替代。

另外，`_Var` 这种变量是普通变量，但它不会在变量未使用时编译报错。

可以用 `=` 操作符给变量绑定一个值。

```eshell
2> X = 123.
123
3> X * 2.
246
```

如果试图改变变量的值，就会报错：

```eshell
5> X = 1.
** exception error: no match of right hand side value 1
6> 
```

`=` 实际上是一个模式匹配操作符。

`f()`会让erlang shell忘记现有的所有绑定。

# 原子

Erlang中，原子被用于表示常量值，是全局性的，不需要宏定义或包含文件。
原子以小写字母开头，后接数字、字母、下划线、at(`@`)。还可以放在单引号里，这样的原子可以使用特殊符号，也可以使用转义字符。如 `'Monday'`, `'+'`, `'*'`, `'spac e'`，
甚至还可以给无需引号的原子加上引号，因此`'a'`和`a`的意思完全一致。
一个原子的值就是它本身。

```eshell
10> hello.
hello
11>
```

# 元组

如果想把一些数量固定的项目组成单一的实体，就需要使用元组(tuple)。
使用大括号声明，逗号分隔元素。

```erlang
P = {10, 45}.
```

为了更容易记住元组的用途，一种常用的做法是将原子作为元组的第一个元素，用它来表示元组的用途，比如上面的点，可以表示为：

```erlang
P = {point, 10, 45}.
```

元组可以嵌套：

```erlang
Person = {person, {name, joe}, {height, 1.82}, {eyecolour, brown}}.
```

如果构建元组时用到变量，那么新元组会共享该变量所引用数据结构的值。

```erlang
1> F = {firstname, joe}.
{firstname,joe}
2> L = {lastName, armstrong}.
{lastName,armstrong}
3> P = {person, F, L}.
{person,{firstname,joe},{lastName,armstrong}}
```

提取元组的值：

```erlang
1> Point = {point, 10, 45}.
{point,10,45}
2> {point, X, Y} = Point.
{point,10,45}
3> X.
10
4> Y.
45
5> Point1 = {point, 25, 25}.
{point,25,25}
6> {point, C, C} = Point1.
{point,25,25}
7> C.
25
8> Person = {person, {name, joe, armstrong}, {footsize, 42}}.
{person,{name,joe,armstrong},{footsize,42}}
9> {_, {_, Who, _}, _} = Person.
{person,{name,joe,armstrong},{footsize,42}}
10> Who.
joe
11> 
```

# 列表

列表(list)被用来存放任意数量的事物。
使用中括号声明，逗号分隔元素。元素可以是任何类型。

```erlang
12> [1+7, hello, 202, {cost, apple, 10}, 3].
[8,hello,202,{cost,apple,10},3]
```

列表的第一个元素被称为列表头(head),去掉列表头剩下的称为列表尾(tail)。\
它们分别可以使用`hd/1`, `tl/1`得到。

访问列表头是一种非常高效的操作。

```erlang
14> L = [1,2,3,4,5].
[1,2,3,4,5]
15> hd(L).
1
16> tl(L).
[2,3,4,5]
17> 
```

如果`T`是一个列表，那么`[H|T]` 也是一个列表，它的头是`H`, 尾是`T`。
竖线`|`把列表的头与尾分开。`[]`是个空列表。
无论何时，只要用`[...|T]`的语法构建一个列表，就应该确保`T`是列表。

可以给`T`的开头添加不止一个元素:`[E1, E2, ... En|T]`。

如果有一个非空列表`L`, 那么 `[X|Y] = L` 会提取列表头作为`X`, 列表尾作为`Y`。

## ++和--

这是两个中缀操作符，其中，`A ++ B` 将B附加到A后面：

```erlang
1> [1,2,3] ++ [2,1].
[1,2,3,2,1]
```

`A -- B` 从A中移除B。从列表头开始移除。

```erlang
2> [1,2,3,1,2,3,2] -- [1,2,2].
[3,1,3,2]
3> [a,b,c,d] -- [a,a,a,c].
[b,d]
```

## 列表推导 list comprehension

列表推导是用来创建列表的表达式。

e.g. 1 让列表中的元素数值翻倍：

```erlang
1> L = [1,2,3,4,5].
[1,2,3,4,5]
2> [2*X || X <- L].
[2,4,6,8,10]
```

上面的`[2*X || X <- L]`意思为：“由2\*X组成的列表，X从L中提取”。

e.g. 2 把原始列表中的数字加倍：

```erlang
3> Buy = [{oranges, 4}, {newspaper, 1}, {apples, 10}, {pears, 6}, {milk, 3}].
[{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}]
4> [{Name, 2*Number} || {Name, Number} <- Buy].
[{oranges,8},{newspaper,2},{apples,20},{pears,12},{milk,6}]
```

上面的表达式中，`{Name, Number}` 是一个**模式**， `{Name, 2*Number}`是一个**构造器**。

e.g. 3 简化的`lists:map/2`

```erlang
map(F, L) -> [F(X) || X <- L].
```

现在总结列表推导表达式的一般形式：

`[X || Qualifier1, Qualifier2, ...]`

其中，`X` 为任意表达式, `Qualifier` 可以是生成器，位串生成器或过滤器。

*   生成器(generator): `Pattern <- ListExpr`
*   位串(bitstring): `BitStringPattern <= BitStringExpr`
*   过滤器(filter): 判断函数，布尔表达式等。

另外，生成器的`Pattern`部分也可以充当过滤器：

```erlang
5> [X || {a, X} <- [{a, 1}, {b, 2}, {c, 3}, {a, 4}, hello, "wow"]].
[1,4]
```

e.g. 4 quickSort

```erlang
qsort([]) -> [];
qsort([Pivot | T]) ->
  qsort([X || X <- T, X < Pivot])
  ++ [Pivot] ++
  qsort([X || X <- T, X >= Pivot]).
```

e.g. 5 毕达哥拉斯三元组(A^2 + B^2 == C^2)

```erlang
pythag(N) ->
  [{A, B, C} ||
    A <- lists:seq(1, N),  %% lists:seq(1, N): 返回闭区间[1, N]的所有整数
    B <- lists:seq(1, N),
    C <- lists:seq(1, N),
    A + B + C =< N,
    A*A + B*B =:= C*C
  ].
```

e.g. 6 单词排列

```erlang
%% mylib.erl
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L -- [H])].


2> mylib:perms("abc").
["abc","acb","bac","bca","cab","cba"]
```

## 构建列表的原则

1.  总是向列表头添加元素
2.  从输入列表的头部提取元素，然后把它们添加到输出列表头部
3.  如果顺序很重要，就调用`list:reverse/1`

注意，`L ++ [H]` 是非常低效的。

## 字符串

严格来说，Erlang里没有字符串。要表示字符串，可以选择：

1.  整数列表
2.  二进制型

使用整数列表时，列表里的每个元素都代表了一个Unicode代码点(codepoint)。

可以用字面量来创建这样的列表：

```erlang
1> Name = "Hello".
"Hello"
```

"Hello" 其实只是一个列表的简写，这个列表包含了代表字符串里各个字符的整数字符代码。

Erlang字符串必须使用双引号。

当shell打印某个列表的值时，如果所有列表内所有整数都代表可打印字符，就会将其打印成字符串字面量。\
否则打印成列表记法, 如果想使用整数格式打印，必须使用格式化语句, 如下

```erlang
8> A = [83,117,114,112,114,105,115,101].
"Surprise"
9> A.
"Surprise"
10> io:format("~w~n", [A]).
[83,117,114,112,114,105,115,101]
ok
```

## 8/16进制/反斜杠转义

注意，这种只能用在字符串或带引号的原子中。

```erlang
3> '\100'.
'@'
5> X = "a\x{221e}b\\".  %% 中间的221e是无穷大
[97,8734,98,92]
6> io:format("~ts~n", [X]).
a∞b\
ok
```

# 模块

模块是Erlang的基本代码单元。模块保存在扩展名为`.erl`的文件里，需要先编译才能运行，编译后的模块扩展名为`.beam`。\
其中第一行为模块名，必须以小写字母开头(atom)。

## eshell内编译

创建如下文件`hello.erl`

```erlang
-module(hello).
-export([start/0]). % 导出声明

start() ->
    io:format("Hello world~n").
```

从`hello.erl`的目录里启动Erlang shell，编译运行：

```eshell
$ erl
1> c(hello).
{ok,hello}
2> hello:start().
Hello world
ok
3> halt().
$
```

`c(hello).`执行编译, `{ok,hello}`即编译成功。

## eshell外编译

```shell
$ erlc hello.erl
$ erl -noshell -s hello start -s init stop
Hello world
```

`erlc`从命令行启动了Erlang编译器。编译器编译了`hello.erl`并生成一个`hello.beam`的目标代码文件。

`erl -noshell ...` 加载了hello模块并执行`hello:start`函数，随后执行了`init:stop`函数，终止了Erlang会话。

# 函数

函数里的模式匹配与eshell一样，下面创建一个`area`函数，计算矩形面积。模块为`geometry`:

```erlang
%% geometry.erl
-module(geometry).  %% 模块声明，必须与存放该模块的主文件名相同
-export([area/1]).  %% 导出声明。导出的函数才能在模块外使用。area/1 表示函数名为area,带有一个参数。Fun/N记法，N被称为函数的元数(arity)。

%% area函数有两个**子句**，由分号隔开，最后一个子句以句号结束。每个子句都有一个头部和主体，由->隔开：
area({rectangle, Width, Height}) -> Width * Height;
area({square, Side})             -> Side * Side.
%% 头部为一个函数名+若干个模式，主体包含一系列表达式。
%% 函数的返回值就是子句主体里最后一条表达式的值。
```

接下来编译并运行，展示函数的调用方法：

```erlang
1> c(geometry).  %% c(), compile, 编译。
{ok,geometry}   %% 编译成功，且 geometry已被加载。当前目录多了一个 geometry.beam：
2> geometry:area({rectangle, 10, 6}).
60
3> geometry:area({square, 5}).
25
4> ls().
geometry.beam     geometry.erl
ok
```

## 简单测试

```erlang
-module(geometry).
-export([area/1, test/0]).

test() ->
    12 = area({rectangle, 3, 4}),
    144 = area({square, 12}),
    test_ok.

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side})             -> Side * Side.
```

```erlang
1> c(geometry).
{ok,geometry}
2> geometry:test().
test_ok
```

函数执行时，按文件中声明的顺序进行模式匹配。

## 实例：购物

假设有这样一个购物列表：

`Buy = [{oranges, 4}, {newspaper, 1}, {apples, 10}, {pears, 6}, {milk, 3}].`

现在想知道购物花了多少，需要知道每一项的价格，假设此信息在一个名为shop的模块中计算：

```erlang
-module(shop).
-export([cost/1]).
-import(lists, [map/2, sum/1]).

cost(oranges)   -> 5;
cost(newspaper) -> 8;
cost(apples)    -> 2;
cost(pears)     -> 9;
cost(milk)      -> 7.
```

测试此模块：

```erlang
2> c(shop).
{ok,shop}
3> shop:cost(orange).
** exception error: no function clause matching shop:cost(orange) (shop.erl, line 4)
4> shop:cost(oranges).
5
```

计算总价值：

```erlang
%% shop.erl

total([{What, N} | T]) -> cost(What) * N + total(T);
total([])              -> 0.
```

```erlang
6> shop:total(Buy).
123
```

## fun 匿名函数

```erlang
8> Double = fun(X) -> X * 2 end.
#Fun<erl_eval.42.3316493>
9> Double(10).
20

%% 多子句匿名函数：
10> TempConvert = fun({c, C}) -> {f, 32 + C * 9 / 5};
10>                  ({f, F}) -> {c, (F - 32) * 5 / 9}
10>               end.
#Fun<erl_eval.42.3316493>
```

### fun做入参

```erlang
11> L = [1,2,3,4].
[1,2,3,4]
12> lists:map(fun(X) -> 2 * X end, L).
[2,4,6,8]
```

```erlang
13> Even = fun(X) -> (X rem 2) =:= 0 end.
#Fun<erl_eval.42.3316493>
14> lists:map(Even, [1,2,3,4,5,6,7,8]).
[false,true,false,true,false,true,false,true]
15> lists:filter(Even, [1,2,3,4,5,6,7,8]).
[2,4,6,8]
```

### fun做返回值

我们有一个水果列表：

```erlang
18> Fruit = [apple, pear, orange].
[apple,pear,orange]
```

定义一个函数`MakeTest(L)`, 将L转变成一个测试函数，用来检查它的参数是否在列表L中。

```erlang
19> MakeTest = fun(L) -> fun(M) -> lists:member(M, L) end end.
#Fun<erl_eval.42.3316493>
20> IsSomeFruit = MakeTest(Fruit).
#Fun<erl_eval.42.3316493>
22> IsSomeFruit(apple).
true
23> IsSomeFruit(dog).
false
24> lists:filter(IsSomeFruit, [dog, orange, apple, bear]).
[orange,apple]
```

### 函数引用

```erlang
% file x1

square(X) -> X * X.

double(L) -> lists:map(fun square/1, L). %% fun Mod:Func/Arity 格式


%% 另一个文件， module x2

double(L) -> lists:map(fun x1:square/1, L). %% fun Mode:Func/Arity 格式
```

## apply

函数也可以由内置函数`apply`启动。(应当尽量避免使用这个函数)

`apply(Mod, Func, [Arg1, Arg2 ... ArgN])`会将模块Mod里的Func函数应用到Arg1...ArgN上。\
它等价于以下调用：\
`Mod:Func(Arg1, Arg2 ... ArgN)`\
它与直接调用函数的区别在于模块名和函数名是可以通过动态计算得出的。

# 关卡 grard

关卡是一种结构，是模式匹配的一种扩展。用在模式匹配/函数头部/表达式中，进行更多操作。\
关卡用作表达式时，必须返回`true`或`false`。true称为“执行成功”， false称为“执行失败”。\
关卡可以并列使用，或关系用分号隔开，与关系用逗号隔开。\
关卡不能调用用户定义的函数。

`f(X, Y) when is_integer(X), X > Y, Y < 6 ->...`\
当X是整数，X大于Y并且Y<6

`is_tuple(T), tuple_size(T) =:= 6, abs(element(3, T)) > 5`

`element(4, X) =:= hd(L)`

`X =:= dog; X =:= cat`\
X是dog或cat

短路布尔：

`A >= -1.0 andalso A + 1 > B`

`is_atom(L) orelse (is_list(L) andalso length(L) > 2)`

注意，`and` 和 `or` 在Erlang中不是短路的，两边都要求值。

# 控制流

## case

有时候，为所有问题都单独定义函数子句并不方便，这时可以使用case或if表达式。

    case Expression of
      Pattern1 [when Guard1] -> Expr_seq1;
      Pattern2 [when Guard2] -> Expr_seq2;
      ...
    end

`Expression` 的值与`PatternN`依次匹配，直到匹配成功，后面的`Expr_seqN`就会被执行，执行结果\
也就是整个case表达式的值。如果匹配全失败，就会发生异常。

e.g. 自己实现 lists\:filter

```erlang
filter(P, [H|T]) ->
  case P(H) of
    true  -> [H|filter(P, T)];
    false -> filter(P, T)
  end;
filter(P, []) ->
  [].
```

## if

```erlang
if
  Guard1 ->
    Expr_seq1;
  Guard2 ->
    Expr_seq2;
  ...
end
```

顺序执行，遇到第一个true就执行，`Expr_seqN`的结果也就是整个if表达式的值。必须有一个能成功，否则就会异常。

## begin...end

把一系列的表达式封在一起，整个`begin...end`表达式的值就是最后一个表达式的值。

# record记录

记录其实就是元组的另一种形式。

通过给元组的元素命名，可以通过名称来访问，而不必记住名字(`element(N, T)`)。

语法：

```erlang
-record(Name,
  {
    key1 = Default1, %% 默认值
    key2, %% 相当于 key2 = undefined
  }
).
```

注意， `record`不能用与eshell。可以用`rr`读入record定义。一般放在`.hrl`的文件中，然后包含(`-include`).

实例：

首先定义

```erlang
-record(todo, {status=reminder, who=joe, text}).
%% 创建
2> rr("records.hrl").
[todo]
3> MYTODO = #todo{status = urgent, text = "Fix errata in book"}.
#todo{status = urgent,who = joe,text = "Fix errata in book"}
%% 更新
4> MYTODO1 = MYTODO#todo{status = done}.
#todo{status = done,who = joe,text = "Fix errata in book"}
%% 提取字段
5> #todo{who = W, text = Text} = MYTODO1.
#todo{status = doen,who = joe,text = "Fix errata in book"}
6> W.
joe
7> Text.
"Fix errata in book"
%% 记录的本质
8> rf(todo).
ok
9> MYTODO1.
{todo,doen,joe,"Fix errata in book"}
```

# Map

`#{Key1 Op Val1, Key2 Op Val2, ... KeyN Op ValN}`\
`Op` 是 `=>`(新增或更新) 或 `:=`(更新)

内部是有序的。

```erlang
%% 创建
1> F1 = #{a => 1, b => 2}.
#{a => 1,b => 2}
%% 更新
2> F2 = F1#{a := 3}.
#{a => 3,b => 2}
%% 提取字段
12> #{a := A, b := 2} = F2.
#{a => 3,b => 2}
13> A.
3
%% to_list函数
14> maps:to_list(F2).
[{a,3},{b,2}]
%% from_list类似。如果键出现不止一次，只使用第一次的值
%% is_key
16> maps:is_key(a, F2).
true

%% find 与 get
17> maps:get(c, F2).
** exception error: bad key: c
     in function  maps:get/2
        called as maps:get(c,#{a => 3,b => 2})
        *** argument 1: not present in map
18> maps:get(c, F2, key_not_exist).
key_not_exist
19> maps:find(c, F2).
error
20> maps:find(a, F2).
{ok,3}
```

# 顺序编程的错误处理

## 错误的产生

先看一个代码错误：

```erlang
% file: shop.erl
cost(oranges) -> 5.

%% eshell调用：
1> c(shop).
{ok,shop}
2> shop:cost(socks).
** exception error: no function clause matching shop:cost(socks) (shop.erl, line 4)
3>
```

作为`cost`函数，我们无法修复错误，因为确实不知道socks的价格。应该由调用者来决定函数崩溃后怎么处理。\
这个就是 *let it crash*.

异常错误发生于系统遇到内部错误时、或显式调用`throw(Exception)`、`exit(Why)`、`error(E)`触发。

*   `exit(Why)`: 终止进程。如果这个异常没有被捕捉，信号`{'EXIT', Pid, Why}`就会被广播给当前进程链接的所有进程。
*   `throw(Why)`: 抛出一个调用者可能想要捕获的异常。可以使用`try...catch`块捕获。
*   `error(Why)`: 标识"崩溃性错误"，即调用者也没准备好的，与系统内部异常差不多。

## 错误的捕获

Erlang有两种捕获异常的方式，一是`try...catch`表达式, 二是 `catch`表达式。

### try...catch

```erlang
try FuncOrExpSeq of %% 这个of 和后面的PatternN 都可以省略
  Pattern1 [when Guard1] -> Exp1;
  Pattern2 [when Guard2] -> Exp2;
  ...
catch
  ExcpetionType1: ExPattern1 [when ExGuard1] -> ExExpressions1;
  ExcpetionType2: ExPattern2 [when ExGuard2] -> ExExpressions2;
  ...
after %% 可选
  AfterExpressions
end
```

执行`FuncOrExpSeq`, 如果没有抛出异常，那么正常执行try后面的PatternN；\
如果有异常，catch块执行，其中，`ExcpetionTypeN`是原子`throw`, `exit`, `error`之一。\
如果省略了，就使用`throw`。  省略了的格式会变成`catch _ -> ...`
`after`块一定会被执行。

最简形式为

```erlang
try F
catch
  ...
end
```

实例：

```erlang
% file: try_demo.erl

-module(try_demo).
-export([demo/0]).

test(1) -> ok_atom;
test(2) -> throw(throw_atom);
test(3) -> exit(exit_atom);
test(4) -> {'EXIT', boardcastEXIT};
test(5) -> error(err_atom).


demo() ->
  [catcher(I) || I <- lists:seq(1, 5)].

catcher(N) ->
  try test(N) of
    Val -> {N, normal, Val}
  catch
    throw:X -> {N, caught, throw, X};
    exit:X  -> {N, caught, exited, X};
    error:X -> {N, caught, error, X}
end.

% 测试：
1> c(try_demo).
{ok,try_demo}
2> try_demo:demo().
[{1,normal,ok_atom},
 {2,caught,throw,throw_atom},
 {3,caught,exited,exit_atom},
 {4,normal,{'EXIT',boardcastEXIT}},
 {5,caught,error,err_atom}]
```

### catch

catch表达式的出现要早于try...catch。\
异常如果发生在catch语句里，就会被转换成一个描述此错误的`{'EXIT', ...}`元组。

```erlang
%% file: try_demo.erl
demo2() ->
  [{I, (catch test(I))} || I <- lists:seq(1, 5)].

%%
3> try_demo:demo2().
[{1,ok_atom},
 {2,throw_atom},
 {3,{'EXIT',exit_atom}},
 {4,{'EXIT',boardcastEXIT}},
 {5,
  {'EXIT',{err_atom,[{try_demo,test,1,
                               [{file,"try_demo.erl"},{line,8}]},
                     {try_demo,'-demo2/0-lc$^0/1-0-',1,
                               [{file,"try_demo.erl"},{line,23}]},
                     {try_demo,'-demo2/0-lc$^0/1-0-',1,
                               [{file,"try_demo.erl"},{line,23}]},
                     {erl_eval,do_apply,7,[{file,"erl_eval.erl"},{line,748}]},
                     {shell,exprs,7,[{file,"shell.erl"},{line,691}]},
                     {shell,eval_exprs,7,[{file,"shell.erl"},{line,647}]},
                     {shell,eval_loop,3,[{file,"shell.erl"},{line,632}]}]}}}]
```

可以看出，catch提供了更详细的报错信息

### 捕捉全部错误

```erlang
try Expr
catch
  _:_ -> ... %
end
```

捕捉到错误后，可以使用`erlang:get_stacktrace()`来找到最近的栈跟踪信息。\
注意，如果被调用的函数是表达式序列的最后一个函数，那么此函数的调用位置信息不会保留在栈上(尾调用优化)。

# 二进制型与位语法

二进制型(binary)是一种数据结构，它被设计成用一种节省空间的方式来保存大批量的原始数据。\
Erlang虚拟机对二进制的输入、输出和消息传递都做了优化，十分高效。\
如果二进制型里的位数不是8的整数倍，那么称为*位串*(bitstring)。

二进制型的编写和打印形式是`<<>>`之间的一列整数或字符串。e.g.:

```erlang
1> <<5,10,20>>.
<<5,10,20>>
2> <<"hello">>.
<<"hello">>
3> <<65, 66, 67>>.
<<"ABC">>
```

整数必须位于`[0-255]`范围。

实例：

把三个变量X,Y,Z打包进一个16位的内存区域。X占3位，Y占7位，Z占6位。

```erlang
1> M = <<2:3, 30:7, 20:6>>.
<<71,148>>
```

为什么是71,148:

2(3) -> 010;\
30(7)-> 0011110;\
20(6) -> 010100;

\=> 0100011110010100\
\=> 0100\_0111\_1001\_0100\
\=> 71, 148

```erlang
2> M1 = <<2:3, 30:7, 20:5>>.
<<71,84:7>>
```

\=> 010; 0011110; 10100;\
\=> 01000111, 1010100;  第二个不够8，前头补0是 01010100, 即84

解包：

```erlang
9> <<X:3, Y:7, Z:6>> = M.
<<71,148>>
10> {X, Y, Z}.
{2,30,20}
```

位语法的一般形式为以下的两种之一

    <<>>
    <<E1, E2, ... En>>

每个`Ei`都是一个片段。有以下四种形式：

1.  Value
2.  Value\:Size
3.  Value/TypeSpecifierList
4.  Value\:Size/TypeSpecifierList

如果表达式的总位数是8的整数倍，就会构建一个二进制型，否则构建一个位串。\
构建时，Value必须是已绑定变量、字符串、整数、浮点数、二进制中的一种。\
Size必须是一个能够得出整数的表达式。用来指明片段大小。默认值取决于不同的数据类型，整数是8，\
浮点数是64，二进制是二进制本身大小。
TypeSpecifierList是一个用连字符分隔的列表，形式为`End-Sign-Type-Unit`。任何一个都可以省略，\
也可以按任意顺序排列。

*   End: `big | little | native`, 默认`big`
*   Sign: `signed | unsigned`, 默认`unsigned`。只用于模式匹配。
*   Type: `integer | float | binary | bytes | bitstring | bits | utf8 | utf16 | utf32`, 默认 `integer`
*   Unit: `unit:1|2...256`。integer, float, bitstring的Unit默认值是1，binary是8。片段总长度为`Size * Unit`字节。

位串无法写入文件或套接字。

位推导(bit comprehension)：

```erlang
11> B = <<16#5f>>.
<<"_">>
12> [X || <<X:1>> <= B].
[0,1,0,1,1,1,1,1]
13> << <<X>> || <<X:1>> <= B>>.
<<0,1,0,1,1,1,1,1>>
```

# 预处理器与宏

Erlang模块在编译前会自动由Erlang预处理器(epp)处理。预处理器展开所有宏，并插入包含文件。\
要查看预处理器的处理结果，可以使用如下命令：

```bash
$ erlc -P some_module.erl
```

这会生成一个名为`some_module.erl.P`的文件

## 文件包含

`-include("myfile.hrl")` 包含普通文件。\
`-include_lib("kernel/include/file.hrl")` 包含库文件。

## 宏的定义与展开

```erlang
-define(A, 1).
-define(F(A, B), {A + B, A - B}).

test() ->
    io:format("~w~n", [?A]),
    io:format("~w~n", [?F(1, 2)]).
```

预定义宏

*   `?FILE` 当前文件名
*   `?MODULE` 当前模块名
*   `?LINE` 当前行号

## 宏控制流

*   `-undef(Macro).`
*   `-ifdef(Macro).`
*   `-ifndef(Macro).`
*   `-else.`
*   `-endif.`

编译时增加宏定义：

`1> c(m1, {d, debug_flag}).` 这个语句会给m1模块定义一个 `debug_flag` 的宏。

# 进程字典

每个Erlang进程都有一个被称为\*进程字典(process dictionary)\*的私有数据存储区域。\
进程字典是一个关联数组。可以用以下函数操作

*   `put(K, V) -> OldV` 给字典添加一个`K, V`组合。返回`K`以前关联的值。如果没有，就返回`undefined`
*   `get(K) -> V` 查找。不存在`K`的话返回 `undefined`
*   `get() -> [{K, V}]` 返回整个字典
*   `get_keys(V) -> [K]` 返回一个列表，内含字典里所有值为V的键
*   `erase(K) -> V` 返回K关联的值，不存在K则返回undefined, 然后删除K
*   `erase() -> [{K, V}]` 删除整个字典

