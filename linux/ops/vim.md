# vim

## vi

**移动**
```txt
h←   j↓   k↑   l→    # 30j 向下移动30行
ctrl+f/d             # 向下滚动一页/半页
ctrl+b/u             # 向上滚动一页/半页
nG       # 移动到第n行
G        # 移动到最后一行
    gg   # 移动到第一行(1G)
0        # 移动到行首
$        # 移动到行尾
```

**删除/复制/粘贴**
```txt
x     delete  
X     backspace  
    nx    连续删除n个字符  
dd    删除整行  
ndd   向下删除n行  
    d1G   删除光标到第一行的所有数据  
    dG    删除光标到最后一行的所有数据  
    d$    删除光标到行尾  
    d0    删除光标到行首  

yy     复制整行  
nyy    向下复制n行  
    y1G   复制光标到第一行的所有数据  
    yG    复制光标到最后一行的所有数据  
    y$    复制光标到行尾  
    y0    复制光标到行首  

p    在光标下一行粘贴  
P    在光标上一行粘贴  
```

**Undo/Redo**
```txt
u    撤销  
.    重复前一个动作
```

**搜索/替换**
```
# 先进入一般模式

/hello    向下搜索hello  
?hello    向上搜索hello 
    n    下一个  
    N    上一个  
:100,200s/vbird/VBIRD/g # 在第100~200行之间搜索vbird并取代为VBIRD  
:1,$s/word1/word2/gc	# 全部范围 且需要用户确认  
```

**进入编辑模式**
```txt
i    从目前光标所在处插入  
o    在下一行插入新行  
O    在上一行插入新行  
R    连续replace 按esc退出  
```

**一般模式操作**
```txt
:e              # 更新内容
:w [filename]	# 另存为filename  
:r [filename]	# 在编辑的数据中,读入filename的数据到游标所在行后面  
:n1,n2 w [filename]	 # 将n1到n2的内容存储成filename这个档案  
```

## vim高级功能

```txt
v    字符选择  
V    行选择  
ctrl + v    区块选择  
y    将反白区域复制  
d    将反白区域删除  
p    粘贴  
```

### 多档案编辑

比如把一个文件里的部分字粘到另一个文件里  
```txt
$ vim hello.txt world.txt  
:n    编辑下一个档案  
:N    编辑上一个档案  
:files    列出目前vim开启的所有档案
```

### 多窗口

```vim
:sp    # 上下切分, 新窗口打开同一个档案  
:vsp hello.txt # 左右切分, 新窗口打开hello.txt  
窗口之间移动 ctrl + w + h/j/k/l  
```

### 补齐功能

```txt
ctrl+x ctrl+n    根据当前文件的内容文字  
ctrl+x ctrl+o    根据扩展名，以vim内置关键词补全
```

## vimrc

```bash
~/.vimrc
/etc/vimrc
/etc/vim/vimrc
```

```vim
set nocompatible    "关闭vi兼容模式
set nu  /  nonu    "行号
set hlsearch  /  nohlsearch    "高亮搜索结果
set incsearch    "输入搜索内容就显示搜索结果
set ignorecase  /  noignorecase   "搜索忽略大小写
set cursorline    "高亮当前行
syntax on  /  off

set expandtab    "tab转空格
set tabstop=4    "tab转换为空格的数目
set softtabstop=4    "一次backspace删掉4个空格

set showmatch    "高亮匹配括号
set autoindent  /  noautoindent    "自动缩排

set backup / nobackup "是否存储备份档。 编辑时，原文件会被另存成名为 filename~ 的文件
set ruler    "右下角的状态栏说明
set showmode    "是否要显示--INSERT--之类的字眼在左下角
set laststatus=2  "显示文件名

set backspace=2  "可删除任意内容
set bg=dark   /  light     "设置色调
```

## APPEND

显式当前打开的文件的文件名： `Ctrl + G`

