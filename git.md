# 配置

## 配置文件位置说明

Git配置变量存储于三个不同的位置：  
1. /etc/gitconfig 包含系统上每一个用户及他们仓库的通用配置。  
    如果在执行`git config`时带上`--system`选项，就会读写该文件中配置变量。
2. ~/.gitconfig 或 ~/.config/git/config 文件：只针对当前用户。可以传递  
    `--global`选项让Git读写此文件，这会对你系统上所有仓库生效。
3. .git/config : 针对该仓库。可以传递`--local`选项让Git读写此文件(默认情况)。  

每一个级别会覆盖上一级别的配置  
在Windows系统中，Git会查找`$HOME/.gitconfig`文件。  
Git同样会查找MSys根目录, 即Git安装的根目录下的`/etc/gitconfig`。  
还有`C:\ProgramData\Git\config`。此文件只能通过管理员使用`git config -f `修改。

可以通过以下命令查看所有的配置以及它们所在的文件：  
`$ git config --list --show-origin`

## 基本配置

### 配置名字/邮箱

安装Git之后，要做的第一件事就是设置用户名和邮件。因为每个提交都会使用这些信息。  

```
$ git config --global user.name "John Doe"
$ git config --global user.email johndoe@example.com
```

当你想针对特定项目使用不同的用户名和邮件时，在项目目录下运行没有`--global`选项
的命令就可以了。  

### 配置提交时使用的编辑器

`$ git config --global core.editor vim`

### 配置SSH Key

如果想协作的话，需要有远端仓库，需要配置SSH Key。以github为例

`$ ssh-keygen -t rsa -C "xiaoming@163.com"`
选择目录的时候直接回车使用默认目录, 也可指定目录. 密码也是可选的.直接回车.  
把`~/.ssh/id_rsa.pub` (windows是`c:/users/xiaoming/.ssh/id_rsa.pub`)里的内容粘进github主页→settings→ssh and GPG keys→ new SSH keys

### CRLF问题

```shell
$ git config --global core.autocrlf input
	# push时把CRLF转换成LF， pull时不转换。
$ git config --global core.autocrlf true
	# push时自动把CRFL转换成LF，pull时LF转换为CRLF
$ git config --global core.autocrlf false
	# pull和push都不变
```

## 查看配置

可以通过 `$ git config <key>` 来检查Git的某一项配置
```
$ git config user.name
John Doe
```
由于Git会从多个文件中读取统一配置变量的不同值，因此可能会看到意料之外的值。
此时，可以查询Git中该变量的原始值，显示哪一个配置文件最后设置了该值。
```
$ git config --show-origin rerere.autoUpdate
file:/home/johndoe/.gitconfig false
```

也可以直接查看原始文件：  
```
$ cat .gitconfig
[user]
        name = swing
        email = ziijchen@outlook.com
[core]
        editor = vim
```



# 获取帮助

```
$ git help <verb>
$ git <verb> --help   # -h显示简单帮助
$ man git-<verb>
```


# Git操作相关（核心）

## 基操

```
$ git status
$ git add .    # 这里一般. 就可以
$ git commit -m "my commit"    # 提交到本地库
```

丢弃工作区(workspace)的修改  
`$ git checkout -- readme.txt`  
把readme.txt文件在工作区的修改全部撤销

- 若修改后还没有add, 就回到和版本库一模一样的状态
- 若已add, 又修改了, 就回到add之后的状态

`$ git reset HEAD readme.txt`  # 放弃add  
#(add之后)撤销暂存区的修改, 重新放回工作区    
`$ git reset`既可回退版本, 也可以把stage的修改回退到working directory

本地删除后, 要从版本库中删除:  (慎用。可以无视词条)
`$ git rm test.txt`  
#就是放弃这个文件在.git文件夹中的跟踪，之后也无法恢复

恢复误删的文件:  
`$ git checkout -- test.txt`   # 其实本质上是放弃修改。删除也算一种修改
#注: git checkout其实是用版本库里的版本替换工作区的版本
#这样恢复只能恢复到最新版本, 修改会丢失

### git reset

`$ git reset --hard HEAD^`     # 回退到上一个版本(commit)
`$ git reset --hard HEAD^^`    # 回退到上上个版本
`$ git reset --hard 123456`    # 版本号不用写全

### 查看

查看某次提交修改的文件  
`$ git log --name-only`

查看工作区和版本库里面最新版本的区别  
`$ git diff HEAD -- readme.txt`

## 标签

发布版本时, 通常先在版本库中打一个标签. 这样, 就唯一确定了打标签时刻的版本.
所以标签也是版本库的一个快照.  标签其实就是指向某个commit的指针

打标签

首先, 切换到需要打标签的分支:  `$ git checkout master`  

然后打标签，默认打在最新提交的commit上：  
`$ git tag v1.0`  

打到特定commit上：  
```
$ git log --pretty=oneline --abbrev-commit
123456 asdfgh
67aces qwert
...

$ git tag v0.9 67aces
```

详细标签

`$ git tag -a v0.1 -m "version 0.1 released" 67aces`  
-a 指定标签名, -m 指定说明文字

查看标签

`$ git tag`             # 查看所有标签
`$ git show v0.9`       # 查看具体标签信息

推送标签到远程

`$ git push origin v1.0`    # 推送特定标签
`$ git push origin --tags`  # 推送全部本地未推送的标签

删除标签

本地删除：  
`$ git tag -d v0.1`
远程删除: 本地删除之后，  
`$ git push origin :refs/tags/v0.9`

私钥签名

`$ git tag -s v0.2 -m "signed version 0.2 released" 67aces`  
签名采用PGP签名


## Git分支

分支简介
为了真正理解Git的分支，我们需要回顾一下Git保存数据的方式。

Git保存的不是文件的变化或差异，而是一系列不同时刻的快照。

进行提交操作时，Git会保存一个提交对象(commit object)。  
该提交对象会包含一个指向暂存内容快照的指针，还包含作者姓名，邮箱，  
提交信息和指向它父对象的指针。  
首次提交产生的提交对象没有父对象，普通提交操作产生的提交对象有一个  
父对象，而由多个分支合并产生的提交对象有多个父对象。  

现在假设有一个工作目录，里面包含了三个将要被暂存和提交的文件。  
暂存操作会为每一个文件计算校验和，然后把当前版本的文件快照保存到  
Git仓库中(Git使用blob对象来保存它们)，最终将校验和加入到暂存区域  
等待提交。  
```
$ git add README test.rb LICENSE
$ git commit -m 'The initial commit of my project'
```
提交时，Git会先计算每一个子目录(本例中只有根目录)的校验和，然后在  
Git仓库中这些校验和保存为树对象。随后，Git便会创建一个提交对象，  
它除了包含上面提到的那些信息外，还包含指向这个树对象(项目根目录)  
的指针。如此一来，Git就可以在需要的时候重现此次保存的快照。  

现在，Git仓库中有5个对象：3个blob对象(保存文件快照)，1个树对象(记录  
目录结构和blob对象索引)，以及1个提交对象(包含着指向前述树对象的指针  
和所有提交信息)  

可以简单地使用`git log --decorate`查看各个分支当前所指的对象。  

`$ git log --oneline --decorate --graph --all`


1.2 将本地仓库与远程仓库关联 (重要)  
	# 用本地文件创建(初始化)一个远程repository。非常实用和常用  
	$ git remote add origin git@github.com:xiaoming/hello.git  
		# 注:很容易写错成...origin master ...  
		# 到这一步的时候还没有指定关联的远程分支  
		# origin 可以换名字。这只是对远程仓库的一个引用  
	$ git add .  
	$ git commit -m "hello"  
		# 若为空文件夹则这两步可省略，若不空一定不能省略	  
	$ git push -u origin master    # 把本地库的所有内容推送到远程库上 #推荐	  
	或 $ git pull --rebase origin master    
		# 若空文件夹则可不加--rebase  #一般来说用这一条的时候本地不可能是空的  
		# 拉取哪个分支是随意的，这里以master为例  
	# 本地分支默认只有master, 关联其他分支见1.3  
	# `git remote add origin ...`这一步如果错误,提示"远程库已存在",则  
		$ git remote rm origin  
  
	# 关联默认分支  
		$ git branch --set-upstream-to=origin/master  
  
1.3 已有远程分支master和dev，新建本地分支与远程分支关联 (重要)  
	$ git checkout -b dev  #新建并切换  
	$ git pull origin dev      #拉取远程dev到本地dev  
  
1.4 在本地新建分支并推送到远程 (管理员)  
	$ git checkout -b test  
	$ git push origin test //会在远程仓库创建一个test分支.  或者  
	$ git push origin dev:dev。这样远程仓库也会有一个dev分支  
  
1.5 删除本地分支 (重要)  
	# 先切到别的分支上  
	$ git branch -d dev    # 不提交无法删除，可用 -D 参数强行删除  
  
1.6 删除远程分支 (管理员)  
	$ git push origin  :dev    # push一个空分支  
	$ git push origin --delete dev  
	删除远程分支后`git branch -a`依然可以看到远程分支  
	$ git remote show orging      #查看删除分支情况  
	$ git remote prune origin  
  
1.7 本地合并 (重要)  
	$ git checkout master  # 目标分支，别的分支要合过来的分支  
	$ git merge dev           # 源分支，被合的  
	$ git merge dev --squash  # 合并多个commit。合完要commit  
  
1.8 获取远程库中本地没有的部分  
	$ git fetch  
	$ git fetch origin main  


### 远程分支

$ git push origin master    # 推到远程
$ git pull origin master    # 拉取

### rebase

将提交到某一分支上的所有修改都移至另一分支上，就像“重新播放”一样。  
现在，experiment分支和master分支有一个分叉。  
```
$ git checkout experiment
$ git rebase master
```
它的原理是首先找到这两个分支的最近共同祖先，然后对比当前分支  
相对于该祖先的历次提交，提取相应的修改并存为临时文件，然后将当前  
分支指向目标基底C3(master),最后以此将之前另存为临时文件的修改依序应用。

### 添加fork库

```sh
$ git remote add myfork ssh://...
$ git fetch myfork my_remote_br:localbr
$ git checkout localbr
$ git push myfork HEAD:my_remote_br
```

### 删除远程分支的本地引用

本地的分支中，由 `git branch -a` 列出的所有分支，有一些远程分支的引用，\
这些引用很可能已经失效(远程分支已经删除)，需要清理。\
方法如下：

```bash
$ git remote prune origin --dry-run # 列出可以删除的分支
$ git remote prune origin # 执行删除
```


# Show Git Info and Auto-Completion in Bash

open ~/.bashrc

Add the following text to your ~/.bashrc
```
# PS1
# specify(change) the path to your own.
source ~/git-2.22.0/contrib/completion/git-completion.bash
source ~/git-2.22.0/contrib/completion/git-prompt.sh

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="verbose git svn"

# Example
PS1='\[\e[32m\]\u \[\e[33m\]\w\[\e[36m\]$(__git_ps1 " <%s>")\[\e[31m\]\n\$ \[\e[0m\]'
```

than,
```bash
$ source ~/.bashrc
```

My PS1  
`\[\e[35m\]\t \[\e[32m\]\u \[\e[33m\]\w \[\e[36m\]$(__git_ps1 "<%s>")\[\e[31m\]\n\$ \[\e[0m\]`



