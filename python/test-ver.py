#! /usr/bin/env python2
# _*_ coding:utf-8 _*_
# __author__:czy
# Input: 1|22222|https://helloworld.com|Mozilla|helloworld.com|123456789||
# Output: host => (ua, [urls])

import threading
import time
import datetime
import os, sys
import re
import multiprocessing
import gzip


def getNowTime():
    return datetime.datetime.now().strftime("%Y%m%d%H%M%S")


def parseGzFile(gzFile, D, L):
    print "Parsing : ", gzFile
    with gzip.open(str(gzFile), mode='rt') as gin: # str() to prevent the yield problem
        for line in gin:
            sp = line.strip().split('|')
            if len(sp) >= 38: # bug. this `if` block can be removed if fixed.
                ua, host, url = sp[15], sp[36], getPath(sp[37])
                if len(host) > 0:
                    with L:
                        if host not in D:
                            D[host] = (ua, [url]) # maybe both empty
                        else:
                            try:
                                urls = D[host][1]
                                if url not in urls:
                                    urls.append(url)
                                old_ua = D[host][0]
                                if len(old_ua) == 0:
                                    tup = D[host]  # D[host][0] = ua
                                    l = list(tup)
                                    l[0] = ua
                                    tup = l
                            except KeyError:
                                pass


def getAllFiles(base_dir):
    # print "Getting Files in: ", base_dir # debug
    filenames = []
    for root, ds, fs in os.walk(base_dir):
        for f in fs:
            re_name = "gz$"
            if re.search(re_name, f):
                fullname = os.path.join(root, f)
                # yield fullname
                filenames.append(fullname)
    return filenames


def producer(q):
    while True:
        files = getAllFiles(DATA_DIR)
        while len(files) > 0:
            q.put(unshiftOneFile(files))
        time.sleep(60) # 每一分钟扫描一次目录


def customer(q):
    while True:
        file = q.get()
        parseGzFile(file, DATA, LOCK)
        print "Removing : " file
        os.remove(file)


def writer(q):
    while True:
        time.sleep(55)
        nowtime = getNowTime()
        if int(nowtime) - CLOCK_BEGIN >= 50 and len(DATA) > 0:
            wfile = "".join([DST_DIR, '/', "xhttp_", nowtime, "000.txt"])
            writeToFile(wfile, DATA, LOCK)


def unshiftOneFile(files):
    file = files[0]
    del files[0]
    return file


def getPath(url):
    if len(url) == 0:
        return ""
    url = url.lstrip().lstrip("http://").lstrip("https://")
    if '/' not in url:
        return ""
    url = url[url.index('/'):]
    if len(url) < 20:
        return url
    else:
        return url if '?' not in url else url[:url.index('?')]


def getUa(D, host):
    return D[host][0]


def getCatedUrl(D, host):
    urls = D[host][1]
    while '' in urls:
        urls.remove('')
    return "@!".join(urls)[:4095]


def writeToFile(filename, D, L):
    print "Writing :", filename
    with open(filename, mode='w') as w:
        for host in D.keys():
            with L:
                ua, url, xreq, appid = getUa(D, host), getCatedUrl(D, host), "", ""
                line = "|!".join([host, ua, url, xreq, appid])
                w.write(line)
                w.write('\n')
    D.clear()


# main
if len(sys.argv) != 4:
    print "USAGE:"
    print "Chinese Ver:"
    print "需要三个参数。"
    print "第一个参数为cdr文件所在目录，不需要写日期文件夹。"
    print "第二个参数是.txt文件要输出的目录。"
    print "第三个参数是处理gz文件的进程数量"
    print "请使用绝对路径"
    print "\nEnglish Ver:"
    print "Three parameters required."
    print "The first param is the directory where the cdr files lay."
    print "The second param is the dir where .txt files will be generated."
    print "The third param is the amount of the process to deal with gz file"
    print "Please use the absolute path.\n"
    exit(1)


DATA_DIR = sys.argv[1].rstrip('/') + '/' + getNowTime()[:8] # only date
DST_DIR  = sys.argv[2].rstrip('/')
READER_NUM = int(sys.argv[3])
CLOCK_BEGIN = int(getNowTime())

QUEUE = multiprocessing.Queue()

MGR  = multiprocessing.Manager()
LOCK = MGR.Lock()
DATA = MGR.dict()

PRODUCER = multiprocessing.Process(target=producer, args=(QUEUE,))
READERS = []
for i in xrange(READER_NUM):
    reader = multiprocessing.Process(target=customer, args=(QUEUE,))
    READERS.append(reader)
WRITER   = multiprocessing.Process(target=writer, args=(QUEUE,))

PRODUCER.start()
for r in READERS:
    r.start()
WRITER.start()

PRODUCER.join()
for r in READERS:
    r.join()
WRITER.join()
