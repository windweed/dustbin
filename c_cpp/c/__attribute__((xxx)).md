# \_\_attribute\_\_

`__attribute__`机制是GNU C的一大特色，可用来设置函数、变量、类型的属性。  
放于声明的尾部，分号之前。


# 初始化

`__attribute__((constructor))`

`void __attribute__((constructor)) f();`,  
f()会在`main`之前调用。

## 函数属性

可以使编译器在错误检查方面的功能更强(当然了先得打开`-Wall`)。  

### format

给被声明的函数加上类似`printf/scanf`的特征，用来使编译器检查函数声明和  
函数实际调用参数之间的格式化字符串是否匹配。  
格式： `format(archetype, string-index, first-to-check)`


