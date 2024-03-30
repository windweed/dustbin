
* 修改字体  
    alt + Y, 修改后要 View->Mono Font Size


* 去除多余空格和Tab  
    Options -> Perferences -> Files -> Remove extra white space when saving。

si的主要配置都在Options -> Perferences中  

# 1 General

Project File Synchronization ->Remove missing file from project选上可以避免因文件找不到而弹出错误对话框。  

# 2 Typing

Source Editing->Indent commands affect #-preprocessor statements。去掉后（默认值），进行多行缩进时不会影响预处理语句（如#if...#endif）。  

Auto Completion->Use detailed completion window，选上后，联想时可以出现该函数的详细信息。  

Auto Completion->Insert paremeters for functions，去掉后，自动联想不会把整个参数都输出到当前行。  

Browsing in Lists->Match syllables while typing(slower)  关闭  

# 3 Files

Opening Files->Sharing: Let other programs modify files，以共享方式打开文件，这个很重要，保证可以在其它编辑中同时编辑该文件。典型的场景就是用ide环境去动态编译调试，而用si静态阅读。  

Customize 'Open' Command...，用于设置Ctrl+O打开的页面，默认选项是 Project File list view in Project Window，建议保持默认。

Saving Files->Preserve Undo data and revision marks after saving，如果发现保存后就不能undo了，请检查该选项是否选中。

Remove extra white space when saving。保存时自动去除每行尾部的空格和tab。建议选中。

# 4 Languages

Conditional Parsing。我们的代码中一般都会有一些开关宏，通过在Conditions中配置这些宏的默认值，可以让si把配置为不开启的宏视为无效代码，从而不进行符号检索。

如果源代码中的开关宏太多，还可以使用Condition Parsing中的Scan Files来自动找出所有开关宏。

# 6 Display

Options->Horizontal scroll bars for each new window。很多大师都教导我们说一行不要写太多代码。在这个指导思想下，我们不需要这个东东。

Show exact case of file names。如果看不惯si把所有的文件名首字母都大写就勾上这个选项吧。

Tile source and destination windows for Source Link commands。Source Link很多时候用于外部命令输出结果的解析（如Make, lint），这个功能会把解析结果与目标窗口自动tile，很实用。

Trim long path names with ellipses。这个建议不要选中。事实上这个主要影响标题栏，但一般来说标题栏上的空间是充裕的，选上之后往往会令我们不知道所编辑文件的具体位置。

# 8 Syntax Formatting

Basics->Use only color formatting。只启用style中关于颜色的设置。其它如粗体、斜体、阴影等都不启用。

Apply Styles for Lanugage Elements。把分类启用style，都选上吧。

Symbol Reference Lookups->Qualify references to members。检测成员的有效性，如果不是类/结构体中的一部分，则不启用style。虽然可能导致性能降低，但还是建议打开。同样Qualify references to functions也是。

这里有个按钮可以进入Doc Types页面（Options菜单也可以进入），里面有很多重要选项：

Editing Options中，

Expand tabs, Visible tabs可以帮助我们发现并转换tab。
Show right margint和Margin width 可以提醒我们是否把一行写得太长。
Symbol Window选项建议关闭（因为有快捷键）。

# 9 Syntax Decorations

Show arrows at goto statements。 在goto时显示一个向上或向下的箭头

Annotate closing braces with end-statement。在"}"后显示标识，表示该"}"与哪个if/switch配对。  
Annotate closing braces only for long blocks。 只在较长的语句块时才显示标识。
# 10 快捷键

Alt + ,   后退  
Alt + .   前进

Ctrl + u  剪切一行
Ctrl + p  粘贴一行
Ctrl + k  复制一行

F9        向左缩进
F10       向右缩进

