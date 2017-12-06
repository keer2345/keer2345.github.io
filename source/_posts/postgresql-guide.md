---
title: PostgreSQL学习笔记
date: 2017-08-10 22:54:06
categories: database
tags: postgresql
---

参考资料
>http://blog.sina.com.cn/s/blog_6af33caa0100ypck.html
>https://www.cnblogs.com/z-sm/archive/2016/07/05/5644165.html
>http://www.cnblogs.com/sparkdev/p/5678874.html
>http://www.cnblogs.com/zhangpengshou/p/5464610.html

# 在Ubuntu安装PostgreSQL


## 安装PostgreSQL
```
sudo apt-get install postgresql-9.6 postgresql-contrib-9.6
```
检查安装：
```
> $ dpkg -l | grep postgresql
```

相应的，可以使用`dpkg --purge`来卸载软件。

<!-- more -->

## 修改postgres数据库用户的密码为123456
```
sudo -u postgres psql
postgres=# ALTER USER postgres WITH PASSWORD '123456';
```
* 其中，sudo -u postgres 是使用postgres 用户登录的意思
* PostgreSQL数据默认会创建一个postgres的数据库用户作为数据库的管理员，密码是随机的

## 启动与停止服务
```
sudo service postgresql start | stop | restart | status
```

## 安装GUI Admin
### pgAdmin4
>https://askubuntu.com/questions/831262/how-to-install-pgadmin-4-in-desktop-mode-on-ubuntu

```
pip install -p python3 pyadmin4
cd pyadmin4
source bin/activate

(pgadmin4) > $ pip install https://ftp.postgresql.org/pub/pgadmin/pgadmin4/v2.0/pip/pgadmin4-2.0-py2.py3-none-any.whl
```

```
(pgadmin4) > $ vim lib/python2.7/site-packages/pgadmin4/config_local.py
```

```python
import os

DATA_DIR        = os.path.realpath(os.path.expanduser(u'~/.pgadmini4/'))
LOG_FILE        = os.path.join(DATA_DIR, 'pgadmin4.log')
SQLITE_PATH     = os.path.join(DATA_DIR, 'pgadmin4.db')
SESSION_DB_PATH = os.path.join(DATA_DIR, 'sessions')
STORAGE_DIR     = os.path.join(DATA_DIR, 'storage')
SERVER_MODE     = False
```
```
(pgadmin4) > $ python lib/python3.5/site-packages/pgadmin4/pgAdmin4.py
```
快捷方式：
```
touch pgadmin4
chmod +x pgadmin4
vim pgadmin4
```
将以下内容写入文件*pgadmin4*
```
#!/bin/bash
source bin/activate
python lib/python3.5/site-packages/pgadmin4/pgAdmin4.py
```
以后就可以以从方式启动
```
./pgadmin4
```

Access at http://localhost:5050

# 使用

## 基本的命令
```
\password：设置密码
\q：退出
\h：查看SQL命令的解释，比如\h select。
\?：查看psql命令列表。
\l：列出所有数据库。
\c [database_name]：连接其他数据库。
\d：列出当前数据库的所有表格。
\d [table_name]：列出某一张表格的结构。
\du：列出所有用户。
\e：打开文本编辑器。
\conninfo：列出当前数据库和连接的信息。
```

## 一般的使用

登录与退出
```shell
> $ su - postgres
Password: 
postgres@qinjh ~ $ psql
psql (9.6.6)
输入 "help" 来获取帮助信息.

postgres=# \q
postgres@qinjh ~ $
```



连接指定数据库
```
psql -U 用户名 -d 数据库名称 [-h 127.0.0.1 -p 5432]
```

