---
title: Flask React Docker in Testdriven - Part I - 12
date: 2018-09-23 20:49:22
tags: [testdriven, flask, react, docker]
---

# Workflow
## Aliases
我们创建两个命令的别名：
`~/.bashrc`:
```
alias dc='docker-compose'
alias dm='docker-machine'
```

```
$ source ~/.bashrc
```

<!-- more -->

## "Saved" State
使用Docker Machine进行本地开发？VM是否处于“已保存”状态？
```
$ docker-machine ls

NAME               ACTIVE   DRIVER       STATE     URL                         SWARM   DOCKER        ERRORS
testdriven-prod    *        amazonec2    Running   tcp://34.207.173.181:2376           v18.03.1-ce
testdriven-dev     -        virtualbox   Saved                                         Unknown
```

尝试一下：
```
$ docker-machine start testdriven-dev
```

如果这不起作用，你需要关闭VM：
1. 启动virtualbox
1. 选择VM并单击“开始”
1. 退出VM并选择“关闭机器电源”
1. 退出virtualbox
1. VM现在应该处于“已停止”状态：


VM现在应该处于“已停止”状态：
```
$ docker-machine ls

NAME               ACTIVE   DRIVER       STATE     URL                         SWARM   DOCKER        ERRORS
testdriven-prod    *        amazonec2    Running   tcp://34.207.173.181:2376           v18.03.1-ce
testdriven-dev     -        virtualbox   Stopped
```

现在可以启动机器了：
```
$ docker-machine start dev
```

应该是“Running”:
```
$ docker-machine ls

NAME               ACTIVE   DRIVER       STATE     URL                         SWARM   DOCKER        ERRORS
testdriven-prod    *        amazonec2    Running   tcp://34.207.173.181:2376           v18.03.1-ce
testdriven-dev     -        virtualbox   Running   tcp://192.168.99.100:2376           v18.03.1-ce
```

## 无法下载Python包？
再次，在本地使用Docker Machine？尝试 `pip install` 在 Docker 机器内部时遇到此错误吗？
```
Retrying (Retry(total=4, connect=None, read=None, redirect=None))
after connection broken by 'NewConnectionError(
  '<pip._vendor.requests.packages.urllib3.connection.VerifiedHTTPSConnection object at 0x7f0f88deec18>:
Failed to establish a new connection: [Errno -2] Name or service not known',)':
/simple/flask/
```

重新启动计算机，然后重新开始：
```
$ docker-machine restart testdriven-dev
$ docker-machine env testdriven-dev
$ eval $(docker-machine env testdriven-dev)
$ docker-compose -f docker-compose-dev.yml up -d --build
```

## 常用命令
构建镜像
```
$ docker-compose -f docker-compose-dev.yml build
```
运行容器
```
$ docker-compose -f docker-compose-dev.yml up -d
```
创建数据库
```
$ docker-compose -f docker-compose-dev.yml run users python manage.py recreate_db
```
种子数据库
```
$ docker-compose -f docker-compose-dev.yml run users python manage.py seed_db
```
运行测试
```
$ docker-compose -f docker-compose-dev.yml run users python manage.py test
```


## 其他命令
停止容器
```
$ docker-compose -f docker-compose-dev.yml stop
```
击毁容器
```
$ docker-compose -f docker-compose-dev.yml down
```
强制构建
```
$ docker-compose -f docker-compose-dev.yml build --no-cache
```
删除镜像
```
$ docker rmi $(docker images -q)
```

## Postgres
想通过psql访问数据库？
```
$ docker-compose -f docker-compose-dev.yml exec users-db psql -U postgres
```


然后，您可以连接到数据库并运行SQL查询。例如：
```
# \c users_dev
# select * from users;
```
