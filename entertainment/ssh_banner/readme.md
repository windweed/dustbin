# SSH Banner

Show Specified Word When You Connect via SSH.

## How to use

mv `xxx_banner` to `/etc/ssh/`

**root**  
`$ vim /etc/ssh/sshd_config`

change the line  
`Banner None`  
to  
`Banner /etc/ssh/ubuntu_banner` (or `centos_banner`, etc)

**redhat**  
`$ systemctl restart sshd`

## Other

you can create any banner by `../screenfetch.sh`

