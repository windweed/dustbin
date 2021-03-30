#! python3

import os

os.chdir(os.getcwd())

# major_num, minor_num, pindex, sType, chRes, intRes
# 01,0111,256,0111010,,
# m_abc[0] = {"111", "222", 1, 1, "999", 1};

with open("my.cfg", mode='r', encoding="utf-8") as f,\
    open("res.txt", 'w', encoding="utf-8", newline="\n") as w:
    i :int = 0
    for line in f:
        if line.startswith("#") or line.startswith(","):
            continue
        sp :list = line.split(",")
        rawline = '[{i}] = {{"{maj}", "{min}", {id}, {typ:d}, "{cr}", {ir}}};'\
                  .format(i = i, maj = sp[0], min = sp[1], id = sp[2], \
                          typ = int(sp[3]), cr="", ir=0)
        w.write("    m_abc" + rawline + '\n')
        i += 1



