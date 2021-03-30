#! /usr/bin/env python2
# _*_ coding:utf-8 _*_

import threading
import time
import datetime
import os, sys
import re
import gzip

# usage
if len(sys.argv) != 3:
    print "USAGE:"
    print "Chinese Ver:"
    print "需要两个参数。"
    print "第一个参数为.gz文件所在目录，即要处理的gz文件的目录。"
    print "第二个参数是.txt文件要输出的目录。"
    print "请使用绝对路径,结尾不要加'/'"
    print "\nEnglish Ver:"
    print "Two parameters required."
    print "The first param is the directory where the .gz files exist."
    print "The second param is the dir where .txt files will be generated."
    print "Please use the absolute path, And don't use '/' in the end.\n"
    exit(1)


def getNowTime(): # -> str
    return datetime.datetime.now().strftime("%Y%m%d%H%M%S")


DATA_DIR = sys.argv[1] + "/" + getNowTime()[:8]# str
DST_DIR  = sys.argv[2]
STARTTIME = int(getNowTime())
DATA_TO_WRITE = {} # Dict[str, Tuple[str, List[str]]] # {host, (ua, [urls])}


def put_into_data(host, ua, url):
    url = getPath(url)
    if len(host) > 0:
        if host not in DATA_TO_WRITE:
            DATA_TO_WRITE[host] = (ua, [url]) # maybe ua url both empty
        else:
            urls = DATA_TO_WRITE[host][1]
            if url not in urls: # prevent duplicate url
                urls.append(url)
            old_ua = DATA_TO_WRITE[host][0]
            if len(old_ua) == 0:
                tup = DATA_TO_WRITE[host]
                l = list(tup)
                l[0] = ua
                tup = l


def get_ua(host): # -> str
    return DATA_TO_WRITE[host][0]


def get_cated_url(host): # -> str
    urls = DATA_TO_WRITE[host][1]
    # rm empty string
    while '' in urls:
        urls.remove('')

    tmpstr = "@!".join(urls)
    return tmpstr if len(tmpstr) < 4096 else tmpstr[:4095]


def parse_file(raw_file):
    print "parsing  : ", raw_file
    with gzip.open(str(raw_file), mode='rt') as gin:
        for line in gin:
            sp = line.strip().split('|')
            if not len(sp) < 38: # TODO bug
                ua, host, url = sp[15], sp[36], sp[37]
                put_into_data(host, ua, url)


def getPath(url):
    '''
        https://baidu.com/www/123?i=123?ui=2 -> /www/123
        # less than 20 : all
        # equals or larger than 20: if has a '?', before '?', or, all
        # print("getPath")
    '''
    if len(url) == 0:
        return ""

    url = url.lstrip().lstrip("http://").lstrip("https://")
    if '/' not in url:
        return ""

    url = url[url.index('/'):]

    if len(url) < 20:
        return url
    else:
        return url if '?' not in url else url[0:url.index('?')] # before '?'


# write all data in dict DATA_TO_WRITE
def write_txt_file(strtime):
    wfile = DST_DIR + "/" + "xhttp_" + strtime + "000.txt"
    print "writing  :", wfile
    with open(wfile, mode='w') as w:
        for host in DATA_TO_WRITE.keys():
            ua, url, x_req, appid = get_ua(host), get_cated_url(host), "", ""
            tmp_result = "|!".join([host, ua, url, x_req, appid])
            w.write(tmp_result)
            w.write('\n')
    DATA_TO_WRITE.clear()


# every 60s, scan all .gz files in WATCH_DIR, then, write to txt file after 
# processing, then delete all .gz files
def start_watch():
    # start = time.process_time()
    CURR_FILES = getAllFiles(DATA_DIR)
    for f in CURR_FILES:
        parse_file(f)
        nowtime = getNowTime()
        if int(nowtime) - STARTTIME >= 50:
            STARTTIME = int(nowtime)
            write_txt_file(nowtime)

            # end = time.process_time()
            # print "Used: %s Secs to Run TimeHandler" %(end - start)
    rmAllGz(DATA_DIR)

    global TIMER
    TIMER = threading.Timer(60, start_watch)
    TIMER.start()


def getAllFiles(base_dir):
    print "Getting Files in: ", base_dir
    filenames = [] # str
    for root, ds, fs in os.walk(base_dir):
        for f in fs:
            re_name = "gz$"
            if re.search(re_name, f):
                fullname = os.path.join(root, f)
                filenames.append(fullname)
    return filenames


# UNUSED
def rmFiles(files):
    for f in files:
        os.remove(f)


def rmAllGz(base_dir):
    for root, ds, fs in os.walk(base_dir):
        for f in fs:
            re_name = "gz$"
            if re.search(re_name, f):
                fullname = os.path.join(root, f)
                print "removing :", fullname
                os.remove(fullname)


# like `unshift files 1` in perl
# no need to judge empty
def getOneFile(files):
    file = files[0]
    del files[0]
    return file


############    main    ################
FTIME_FILES = getAllFiles(DATA_DIR) # first time's files
# first process all AND delete all
print "Start Process Old Files"
while len(FTIME_FILES) > 0:
    # start = time.process_time()
    curr_file = getOneFile(FTIME_FILES)
    parse_file(curr_file)

    strtime = getNowTime()
    if int(strtime) - STARTTIME >= 50:
        STARTTIME = int(strtime)
        write_txt_file(strtime)
print "Old Files All done."
rmAllGz(DATA_DIR)
print "Now Starting Watching", DATA_DIR
start_watch()
