# declare

`$ declare -i total=$firstnu*$secnu`    #无空格  #还不如直接let

declare/typeset
若declare不接任何参数，bash就会将所有变量名与内容列出。就和set一样
$ declare [-aixr] variable
* -a 将后面变量定义为数组(array)
* -A 关联数组
* -i ...整数(integer)
* -x 与export一样，定义为环境变量
* -r readonly

+x 将-变成+可以进行“取消”动作
```bash
$ declare -p sum  #-p可以单独列出变量的类型，会显示变量declare时的参数
用法:
$ declare -ix sum="450"
$ declare -r sum
$ declare +x sum
$ declare -p sum  #自己试一下
```
