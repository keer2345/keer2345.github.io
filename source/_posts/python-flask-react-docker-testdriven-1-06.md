---
title: Flask React Docker in Testdriven - Part I - 06
date: 2018-09-23 20:49:16
tags: [testdriven, flask, react, docker]
---
# Postgres Setup

本节配置 PostgreSQL，并运行在另一个容器，连接到 `users` 服务。

添加相关包：
```
$ pip install flask-sqlalchemy
$ pip install psycopg2-binary
```

<!-- more -->

更新配置文件 `services/users/project/config.py`:
```python
import os


class BaseConfig:
    """Base configuration"""
    TESTING = False
    SQLALCHEMY_TRACK_MODIFICATIONS = False  # new


class DevelopmentConfig(BaseConfig):
    """Development configuration"""
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_URL')  # new


class TestingConfig(BaseConfig):
    """Testing configuration"""
    TESTING = True
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_TEST_URL')  # new


class ProductionConfig(BaseConfig):
    """Production configuration"""
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_URL')  # new
```

添加数据库实例 `services/users/project/__init__.py`:
```python
import os

from flask import Flask, jsonify
from flask_sqlalchemy import SQLAlchemy

app = Flask(__name__)

# set config
app_settings = os.getenv('APP_SETTINGS')
app.config.from_object(app_settings)

# instantiate the db
db = SQLAlchemy(app)


# model
class User(db.Model):
    __tablename__ = 'users'
    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    username = db.Column(db.String(128), nullable=False)
    email = db.Column(db.String(128), nullable=False)
    active = db.Column(db.Boolean(), default=True, nullable=False)

    def __init__(self, username, email):
        self.username = username
        self.email = email


@app.route('/users/ping', methods=['GET'])
def ping_pong():
    return jsonify({'status': 'success', 'message': 'pong'})

```

添加数据库的相关脚本：
```
mkdir services/users/project/db
```

`services/users/project/db/create.sql`:
```sql
CREATE DATABASE users_prod;
CREATE DATABASE users_dev;
CREATE DATABASE users_test;
```

`services/users/project/db/Dockerfile`:
```dockerfile
# base image
FROM postgres:10.4-alpine

# run create.sql on init
ADD create.sql /docker-entrypoint-initdb.d
```


更新根目录下的 `docker-compose-dev.yml`:
```
version: "3.6"

services:
  users:
    build:
      context: ./services/users
      dockerfile: Dockerfile-dev
    volumes:
      - './services/users:/usr/src/app'
    ports:
      - 5001:5000
    environment:
      - FLASK_APP=project/__init__.py
      - FLASK_ENV=development
      - APP_SETTINGS=project.config.DevelopmentConfig 
      - DATABASE_URL=postgres://postgres:postgres@users-db:5432/users_dev
      - DATABASE_TEST_URL=postgres://postgres:postgres@users-db:5432/users_test
    depends_on:
      - users-db

  users-db:
    build:
      context: ./services/users/project/db
      dockerfile: Dockerfile
    ports:
      - 5435:5432
    environment:
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
```


一旦启动，Postgres 将映射 `5435` 端口到宿主机，而在其他容器中然然以 `5432` 提供服务。由于 `users` 服务不仅依赖于容器正在运行，而且还依赖于实际的 Postgres 实例正常运行，我们将一个 `entrypoint.sh` 文件添加到 *users* ：
```
#!/bin/sh

echo "Waiting for postgres..."

while ! nc -z users-db 5432; do
  sleep 0.1
done

echo "PostgreSQL started"

python manage.py run -h 0.0.0.0
```

因此，我们引用 Postgres 容器 `users-db` ，不断循环，知道返回类似 `Connection to users-db port 5432 [tcp/postgresql] succeeded!` 的内容。

修改 `Dockerfile-dev` :
```dockerfile
# base image
FROM python:3.6.5-alpine

# install dependencies
RUN apk update && \
    apk add --virtual build-deps gcc python-dev musl-dev && \
    apk add postgresql-dev && \
    apk add netcat-openbsd

# set working directory
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

# add and install requirements
COPY ./requirements.txt /usr/src/app/requirements.txt
RUN pip install --upgrade pip
RUN pip install -r requirements.txt

# add entrypoint.sh
COPY ./entrypoint.sh /usr/src/app/entrypoint.sh
RUN chmod +x /usr/src/app/entrypoint.sh

# add app
COPY . /usr/src/app

# run server
# CMD python manage.py run -h 0.0.0.0
CMD ["/usr/src/app/entrypoint.sh"]
```

> 您可能会遇到 `permission denied` 这样的保错，尝试修改本地文件的权限：`chmod +x services/users/entrypoint.sh`。如果都不起作用，试试 [docker entrypoint running bash script gets “permission denied”](https://stackoverflow.com/questions/38882654/docker-entrypoint-running-bash-script-gets-permission-denied) .

重构:
```
$ docker-compose -f docker-compose-dev.yml up -d --build
```

现在访问 http://127.0.0.1:5001/users/ping 可以看到结果返回：
```json
{
  "message": "pong!",
  "status": "success"
}
```

修改 `manage.py`:
```python
from flask.cli import FlaskGroup

from project import app, db

cli = FlaskGroup(app)


@cli.command()
def recreate_db():
    db.drop_all()
    db.create_all()
    db.session.commit()


if __name__ == '__main__':
    cli()
```

`recreate_db` 会将一个新命令注册到 CLI，以便我们可以从命令行运行它，我们将很快使用它将模型应用于数据库。
