# find

```bash
czy@redhat 15:25 ~/E/yum-sources 
$ ll
total 2880
-rw-r--r--. 1 root root    1572 Dec  1  2016 CentOS7-Base-163.repo
-rw-rw-r--. 1 czy  czy   110944 Mar 19 15:26 python-urlgrabber-3.10-9.el7.noarch.rpm
-rw-rw-r--. 1 czy  czy  1219344 Mar 19 15:27 rpm-4.11.3-40.el7.x86_64.rpm
-rw-rw-r--. 1 czy  czy  1297332 Mar 19 15:31 yum-3.4.3-163.el7.centos.noarch.rpm
-rw-rw-r--. 1 czy  czy    31312 Mar 19 15:31 yum-langpacks-0.4.2-7.el7.noarch.rpm
-rw-rw-r--. 1 czy  czy    28348 Mar 19 15:30 yum-metadata-parser-1.1.4-10.el7.x86_64.rpm
-rw-rw-r--. 1 czy  czy    34744 Mar 19 15:44 yum-plugin-fastestmirror-1.1.31-52.el7.noarch.rpm
-rw-rw-r--. 1 czy  czy    83040 Mar 19 15:30 yum-rhn-plugin-2.0.1-10.el7.noarch.rpm
-rw-rw-r--. 1 czy  czy   124364 Mar 19 15:30 yum-utils-1.1.31-52.el7.noarch.rpm
czy@redhat 15:25 ~/E/yum-sources 
$ find . -name "*.rpm" -type f -newermt '2020-03-19 15:31' | xargs ls -l
-rw-rw-r--. 1 czy czy 1297332 Mar 19 15:31 ./yum-3.4.3-163.el7.centos.noarch.rpm
-rw-rw-r--. 1 czy czy   31312 Mar 19 15:31 ./yum-langpacks-0.4.2-7.el7.noarch.rpm
-rw-rw-r--. 1 czy czy   34744 Mar 19 15:44 ./yum-plugin-fastestmirror-1.1.31-52.el7.noarch.rpm
czy@redhat 15:26 ~/E/yum-sources 
$ find . -name "*.rpm" -type f ! -newermt '2020-03-19 15:31' | xargs ls -l
-rw-rw-r--. 1 czy czy  110944 Mar 19 15:26 ./python-urlgrabber-3.10-9.el7.noarch.rpm
-rw-rw-r--. 1 czy czy 1219344 Mar 19 15:27 ./rpm-4.11.3-40.el7.x86_64.rpm
-rw-rw-r--. 1 czy czy   28348 Mar 19 15:30 ./yum-metadata-parser-1.1.4-10.el7.x86_64.rpm
-rw-rw-r--. 1 czy czy   83040 Mar 19 15:30 ./yum-rhn-plugin-2.0.1-10.el7.noarch.rpm
-rw-rw-r--. 1 czy czy  124364 Mar 19 15:30 ./yum-utils-1.1.31-52.el7.noarch.rpm
czy@redhat 15:26 ~/E/yum-sources 
$ 
```