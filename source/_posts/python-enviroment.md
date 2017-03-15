---
title: Python Enviroment
date: 2016-06-04 09:44:39
categories: python
tags: python
---

# 虚拟环境
## virtualenv
```
pip install virtualenv

virtualenv venv
virtualenv -p python2.7 venv27

source venv/bin/activate
deactivate
```

# 项目依赖
## pip
### pip镜像
经常在使用Python的时候需要安装各种模块，而pip是很强大的模块安装工具，但是由于国外官方pypi经常被墙，导致不可用，所以我们最好是将自己使用的pip源更换一下，这样就能解决被墙导致的装不上库的烦恼。
网上有很多可用的源，例如：
* 豆瓣: `http://pypi.douban.com/simple/`
* 清华: `https://pypi.tuna.tsinghua.edu.cn/simple`

#### 临时使用
可以在使用pip的时候加参数`-i https://pypi.tuna.tsinghua.edu.cn/simple`
例如：`pip install -i https://pypi.tuna.tsinghua.edu.cn/simple gevent`，这样就会从清华这边的镜像去安装gevent库。

#### 永久修改
* Linux下
修改 `~/.pip/pip.conf` (没有就创建一个)， 修改 index-url至tuna，内容如下：
```
[global]
index-url = https://pypi.tuna.tsinghua.edu.cn/simple
```
 
* windows下
直接在user目录中创建一个pip目录，如：C:\Users\xx\pip，新建文件pip.ini，内容如下
```
[global]
index-url = https://pypi.tuna.tsinghua.edu.cn/simple
```

### 导出与安装
```
pip freeze > requirements.txt
pip install -r requirements.txt
```
