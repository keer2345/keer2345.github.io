---
title: Flask React Docker in Testdriven - Part I - 05
date: 2018-09-23 20:49:15
tags: [testdriven, flask, react, docker]
---

# Docker Config

让我们将 Flask 应用容器化（containerize）。

确保已经安装了 Docker ：
```
$ docker -v
Docker version 18.06.1-ce, build e68fc7a

$ docker-compose -v
docker-compose version 1.22.0, build f46880fe

$ docker-machine -v  # 目前 docker-machine 不是必需的
docker-machine version 0.14.0, build 89b8332
```

<!-- more -->

添加文件 `services/users/Dockerfile-dev`
```
# base image
FROM python:3.6.5-alpine

# new
# install dependencies
RUN apk update && \
    apk add --virtual build-deps gcc python-dev musl-dev

# set working directory
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

# add and install requirements
COPY ./requirements.txt /usr/src/app/requirements.txt
RUN pip install --upgrade pip
RUN pip install -r requirements.txt

# add app
COPY . /usr/src/app

# run server
CMD python manage.py run -h 0.0.0.0
```

这里，我们使用 `Alpine` 版本，优点在于：
1. 使用更少的磁盘空间得以减少主机成本
1. 更快地构建、加载和运行
1. 更安全（因为包含很少的库文件和包）
1. 快速部署


添加文件 `services/users/.dockerignore` ，类似 `.gitignore` :
```
.dockerignore
Dockerfile-dev
Dockerfile-prod
```

在项目的根目录添加 `docker-compose-dev.yml` :
```
version: "3.6"

services:
  users:
    build:
      context: ./services/users
      dockerfile: Dockerfile-dev
    volumes:
      - "./services/users:/usr/src/app"
    ports:
      - 5001:5000
    environment:
      - FLASK_APP=project/__init__.py
      - FLASK_ENV=development
      - APP_SETTINGS=project.config.DevelopmentConfig 
```
这个配置将创建 `users` 的服务。

`volumes` 将当前工作目录挂载到容器中，对于开发环境而言，这是必须的，以便在对源代码进行更改时更新容器。如果没有这个，每次更改代码时都必须重新构建映像。

在根目录构建该项目的镜像：
```
$ docker-compose -f docker-compose-dev.yml build
```


这将是第一次需要几分钟。由于Docker缓存了第一次构建的结果，后续构建将会快得多。构建完成后，启动容器：
```
$ docker-compose -f docker-compose-dev.yml up -d
```

> 该 `-d` 标志用于在后台运行容器。

在浏览器访问 http://127.0.0.1:5001/users/ping ，就可以看到和之前相同的 JSON 响应。
```json
{
  "message": "pong!",
  "status": "success"
}
```

接下来，添加环境变量到配置文件 `docker-compose-dev.yml` 中：
```
# ...

environment:
      - FLASK_APP=project/__init__.py
      - FLASK_ENV=development
      - APP_SETTINGS=project.config.DevelopmentConfig  # new
```

更新`services/users/project/__init__.py` 文件：
```python
import os

from flask import Flask, jsonify

app = Flask(__name__)

# set config
app_settings = os.getenv('APP_SETTINGS')
app.config.from_object(app_settings)


@app.route('/users/ping', methods=['GET'])
def ping_pong():
    return jsonify({'status': 'success', 'message': 'pong'})
```

更新容器：
```
$ docker-compose -f docker-compose-dev.yml up -d --build
```
> 或者，我们可以使用以下命令来启停容器：
> ```
$ docker-compose -f docker-compose-dev.yml up -d
$ docker-compose -f docker-compose-dev.yml down -v
> ```

想要测试，以确保加载正确的配置？在路由处理程序之前的 `__init__.py` 中添加一条 `print` 语句，以查看应用程序配置以确保它正常工作：
```python
import sys
print(app.config, file=sys.stderr)
```


然后只需查看日志：
```
$ docker-compose -f docker-compose-dev.yml logs
```


你应该看到类似的东西：
```
<Config {
  'ENV': 'development', 'DEBUG': True, 'TESTING': False,
  'PROPAGATE_EXCEPTIONS': None, 'PRESERVE_CONTEXT_ON_EXCEPTION': None,
  'SECRET_KEY': None, 'PERMANENT_SESSION_LIFETIME': datetime.timedelta(31),
  'USE_X_SENDFILE': False, 'SERVER_NAME': None, 'APPLICATION_ROOT': '/',
  'SESSION_COOKIE_NAME': 'session', 'SESSION_COOKIE_DOMAIN': None,
  'SESSION_COOKIE_PATH': None, 'SESSION_COOKIE_HTTPONLY': True,
  'SESSION_COOKIE_SECURE': False, 'SESSION_COOKIE_SAMESITE': None,
  'SESSION_REFRESH_EACH_REQUEST': True, 'MAX_CONTENT_LENGTH': None,
  'SEND_FILE_MAX_AGE_DEFAULT': datetime.timedelta(0, 43200),
  'TRAP_BAD_REQUEST_ERRORS': None, 'TRAP_HTTP_EXCEPTIONS': False,
  'EXPLAIN_TEMPLATE_LOADING': False, 'PREFERRED_URL_SCHEME': 'http',
  'JSON_AS_ASCII': True, 'JSON_SORT_KEYS': True, 'JSONIFY_PRETTYPRINT_REGULAR':
  False, 'JSONIFY_MIMETYPE': 'application/json', 'TEMPLATES_AUTO_RELOAD': None,
  'MAX_COOKIE_SIZE': 4093}
>
```

> 确保测试完成后删除该条 `print` 语句。
