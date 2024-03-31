# bc

```bash
# 可浮点数:
$ echo "4 * 0.56" | bc
2.24
$ echo "scale=2; 3/8" | bc    #scale=2 设小数位个数为2
0.37

# 可进制转换:
$ no=100;
$ echo "obase=2; $no" | bc
1100100
$ no=1100100
$ echo "obase=10; ibase=2; $no" | bc
100

# 可平方/平方根
$ echo "sqrt(100)" | bc
$ echo "10^10" | bc
```
