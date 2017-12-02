---
title: PostgreSQL学习笔记
date: 2017-08-10 22:54:06
categories: database
tags: postgresql
---

# 在Ubuntu安装PostgreSQL
>https://www.cnblogs.com/z-sm/archive/2016/07/05/5644165.html
>http://www.cnblogs.com/sparkdev/p/5678874.html
>http://www.cnblogs.com/zhangpengshou/p/5464610.html

<!-- more -->

## 安装前检查是否有旧版本
```
dpkg -l |grep postgresql
```
## 安装PostgreSQL
* 添加postgresql源：
	```
	sudo touch /etc/apt/sources.list.d/pgdb.list
	sudo vim /etc/apt/sources.list.d/pgdb.list
	```
* 把下面这行数据添加到pgdb.list文件中：
		```
		deb https://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main
		```
* 执行下面的命令添加postgresql安装包的秘钥：
		```
		sudo wget --quiet -O - https://postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
		```
* 接下来就可以安装了：
		```
		sudo apt-get update
	    sudo apt-get install postgresql-9.4
		```
* 检查安装：
		```
		> $ dpkg -l | grep postgresql
		ii  pgdg-keyring                          2017.1                                       all          keyring for apt.postgresql.org
		ii  postgresql-9.6                        9.6.4-1.pgdg14.04+1                          amd64        object-relational SQL database, version 9.6 server
		ii  postgresql-client-9.6                 9.6.4-1.pgdg14.04+1                          amd64        front-end programs for PostgreSQL 9.6
		ii  postgresql-client-common              184.pgdg14.04+1                              all          manager for multiple PostgreSQL client versions
		ii  postgresql-common                     184.pgdg14.04+1                              all          PostgreSQL database-cluster manager
		```

## 修改postgres数据库用户的密码为123456
```
sudo -u postgres psql
postgres=# ALTER USER postgres WITH PASSWORD '123456';
```
* 其中，sudo -u postgres 是使用postgres 用户登录的意思
* PostgreSQL数据默认会创建一个postgres的数据库用户作为数据库的管理员，密码是随机的

## 安装pgAdmin4
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
Access at http://localhost:5050

# 使用
## 一般的使用
```shell
> $ su - postgres
Password: 
postgres@qinjh ~ $ psql
psql (9.6.6)
输入 "help" 来获取帮助信息.

postgres=# \q
postgres@qinjh ~ $
```

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
