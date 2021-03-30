#! /usr/bin/python3

# get specific line in sorts of tar files, whose filename is "2020201-HTTP-"...

import os, sys
import re
import fcntl
import tarfile
import multiprocessing

def pickLinesInOneTar(tar_file :str, daytime :str):
    key_info = re.split("[_-]", tar_file)[3]
    spam = tarfile.open(tar_file)
    for file in spam.getnames():
        lines = spam.extractfile(file)
        for line in lines:
            key = line.split("|")[5] if key_info in ["HTTP", "HTTPS"] else "un"

        if key in contents:
            filename = ".".join([key, key_info, daytime])
            with open(filename, "a+") as w:
                fcntl.flock(w.fileno(), fcntl.LOCK_EX)
                w.write(line)

def getAllTarFile(base_dir :str, daytime :str):
    for root, ds, fs in os.walk(base_dir):
        for f in fs:
            re_name = "_" + daytime + "\d+HTTPS?-" # \d for daytime
            if re.search(re_name, f):
                fullname = os.path.join(root, f)
                yield fullname

if __name__ == '__main__':
    target_dir = sys.argv[1] # dir, e.g. /home/xxx/data
    day_str = sys.argv[2] # date, e.g. 20200304
    contents = set(sys.argv[3:])  # filter, the line you want to extract

    mypool = multiprocessing.Pool(processes = 5)
    for file in getAllTarFile(target_dir, day_str):
        print(file)
        mypool.apply_async(pickLinesInOneTar, (file, day_str,))
    mypool.close()
    mypool.join()
    print("OK")
