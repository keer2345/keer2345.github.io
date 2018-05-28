---
title: Docker Notebook
date: 2018-05-28 13:01:17
tags: docker
---
# Install 
## Install on Ubuntu

### 必须使用sudo操作的解决方案:

1. 创建docker组：`sudo groupadd docker`
1. 将当前用户加入docker组：`sudo gpasswd -a ${USER} docker`
1. 重启服务：`sudo service docker restart`
1. 刷新docker成员：`newgrp - dockerocker`
