---
title: Python Enviroment
date: 2016-06-04 09:44:39
categories: python
tags: python
---

# Python版本设置
## 添加版本
```shell
$sudo update-alternatives --install /usr/bin/python python /usr/bin/python2.7 1
update-alternatives: using /usr/bin/python2.7 to provide /usr/bin/python (python) in auto mod
$sudo update-alternatives --install /usr/bin/python python /usr/bin/python3.5 2
update-alternatives: using /usr/bin/python3.4 to provide /usr/bin/python (python) in auto mod
```
## 查看版本
```shell
update-alternatives --list python
```
## 配置版本
```shell
$sudo update-alternatives --config python
有 2 个候选项可用于替换 python (提供 /usr/bin/python)。

  选择       路径              优先级  状态
------------------------------------------------------------
* 0            /usr/bin/python3.5   2         自动模式
  1            /usr/bin/python2.7   1         手动模式
  2            /usr/bin/python3.5   2         手动模式

要维持当前值[*]请按<回车键>，或者键入选择的编号：
```

## 移除替代版本
我们的系统中不再存在某个 Python 的替代版本时，我们可以将其从`update-alternatives`列表中删除掉。例如，我们可以将列表中的 python2.7 版本移除掉。
```shell
$sudo update-alternatives --remove python /usr/bin/python2.7
update-alternatives: removing manually selected alternative - switching python to auto mode
update-alternatives: using /usr/bin/python3.5 to provide /usr/bin/python (python) in auto mode
```

<!--more-->

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
* 阿里: `http://mirrors.aliyun.com/pypi/simple/`
* 豆瓣: `http://pypi.douban.com/simple/`
* 清华: `https://pypi.tuna.tsinghua.edu.cn/simple`

#### 临时使用
可以在使用pip的时候加参数`-i https://pypi.tuna.tsinghua.edu.cn/simple`
例如：`pip install -i https://pypi.tuna.tsinghua.edu.cn/simple gevent`，这样就会从清华这边的镜像去安装gevent库。

#### 永久修改
* Linux下
修改 `~/.pip/pip.conf` (没有就创建一个)，添加或修改，内容如下【参考
[阿里help](http://mirrors.aliyun.com/help/pypi)】：
```
[global]
index-url = http://mirrors.aliyun.com/pypi/simple/

[install]
trusted-host=mirrors.aliyun.com
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
