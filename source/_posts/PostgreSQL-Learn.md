---
title: PostgreSQL学习笔记
date: 2017-08-10 22:54:06
tags: postgresql
---

# 在Ubuntu安装PostgreSQL
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

# 创建数据库和角色
## 修改postgres数据库用户的密码为123456
```
sudo -u postgres psql
postgres=# ALTER USER postgres WITH PASSWORD '123456';
```
* 其中，sudo -u postgres 是使用postgres 用户登录的意思
* PostgreSQL数据默认会创建一个postgres的数据库用户作为数据库的管理员，密码是随机的
