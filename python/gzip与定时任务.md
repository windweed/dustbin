# gzip和定时任务

```py3
#! python3

import threading
import time
import datetime
import os, sys
import re
import gzip
from typing import Dict, List, Tuple


DATA_TO_WRITE : Dict[str, Tuple[str, List[str]]] = {} # host -> (ua, [url])


def put_into_data(host :str, ua :str, url: str) -> None:
    # print("parse_line")
    url = getPath(url)
    if len(host) > 0:
        if host not in DATA_TO_WRITE:
            tmp_url_list = [url]
            DATA_TO_WRITE[host] = (ua, tmp_url_list)
        else:
            DATA_TO_WRITE[host][1].append(url)
            old_ua = DATA_TO_WRITE[host][0]
            if len(old_ua) == 0:
                tup = DATA_TO_WRITE[host]
                l = list(tup)
                l[0] = ua
                tup = l
                # DATA_TO_WRITE[host][0] = ua


def get_ua(host :str) -> str:
    return DATA_TO_WRITE[host][0]


def get_cated_url(host :str) -> str:
    tmpstr = "@".join(DATA_TO_WRITE[host][1])
    return tmpstr if len(tmpstr) < 4096 else tmpstr[0:4095]


def parse_file(raw_files :List[str]):
    print("parse_file:", raw_files)
    start = time.process_time()
    for raw_file in raw_files:
        print("raw_file: ", raw_file)
        with gzip.open(raw_file, mode='rt', errors="ignore") as gin:
            print("open gzip OK")
            for line in gin:
                line = line.strip()
                sp = line.split('|')
                ua, host, url = sp[15], sp[36], sp[37]
                put_into_data(host, ua, url)
    end :float = time.process_time()
    print("Used: %s Secs To process file %s" %(end - start, raw_file))


def getPath(url :str) -> str:
    # print("getPath")
    if len(url) == 0:
        return ""

    url = url.lstrip("http://").lstrip("https://")
    if '/' not in url:
        return ""

    url = url[url.index('/'):]

    if len(url) < 20:
        return url
    else:
        tmp :str = url if '?' not in url else url[0:url.index('?')]
        return tmp


def write_csv_file(strtime :str):
    print("write_csv_file")
    wfile = strtime + ".csv"
    with open(wfile, mode='w', encoding='utf-8', newline='\n') as w:
        for host in DATA_TO_WRITE.keys():
            # tup = DATA_TO_WRITE[host]
            tmp_result = "|!".join([host, get_ua(host), get_cated_url(host)])
            w.write(tmp_result)
            w.write('\n')


def time_handler():
    for file in getSomeFiles(5):
        print("yield result:", file)
        parse_file(file)

    now = datetime.datetime.now()
    write_csv_file(now.strftime("%Y%m%d%H%M%S"))

    print("hello")
    global TIMER
    TIMER = threading.Timer(5, time_handler)
    TIMER.start()


def getAllFiles(base_dir :str) -> List[str]:
    filenames : List[str] = [] # str
    for root, ds, fs in os.walk(base_dir):
        for f in fs:
            re_name = "gz$"
            if re.search(re_name, f):
                fullname :str = os.path.join(root, f)
                filenames.append(fullname)
                print("got gz file OK")
    return filenames


def getSomeFiles(n :int) -> List[str]:
    if len(files) == 0:
        print("all done")
        exit(0)

    if len(files) >= n:
        yield files[:n]
        del files[:n]
    else:
        yield files
        del files[0:]



os.chdir(os.getcwd() + "/../")
# target_dir = sys.argv[1]
# files = []
files = getAllFiles('.')
# print(files)
time_handler()




```



