---
title: Build Shadowsocks in Debian
date: 2017-04-12 13:28:21
tags: system
---

# 服务端
## 准备环境
* debian8
* python2.7

## 安装
```
sudo apt-get install shadowsocks
```
<!-- more -->
## 配置
sudo vi  /etc/shadowsocks/config.json
```
{
    "server":"my_server_ip",
    "server_port":****,
    "local_address": "127.0.0.1",
    "local_port":****,
    "password":"******",
    "timeout":300,
    "method":"aes-256-cfb",
    "fast_open": false,
    "workers": 1
}
```
## 启动与停止
```
ssserver -c /etc/shadowsocks/config.json -d start
ssserver -c /etc/shadowsocks/config.json -d start
```

## 设置开机自动启动
 sudo vi /etc/rcsudo vi /etc/rc.local

将下面的语句添加到文件的最后，如果有exit 0，加在这一行的上边。
```
/usr/local/bin/ssserver -c /etc/shadowsocks.json -d start
```

# 客户端
参见https://github.com/shadowsocks/shadowsocks-qt5/wiki
