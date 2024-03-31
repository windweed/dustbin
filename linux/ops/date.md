# date

## epoch与正常时间转换

```bash
$ date '+%Y-%m-%d %H:%M:%S' -d @1598004677
2020-08-21 18:11:17

$ date -d "2015-08-04 15:03:55" +%s
1438671835
```
```bash
$ date '+%Y-%m-%d %H:%M:%S'
2020-08-24 19:02:46

$ date +%s
1598266998

```