---
title: Flask react docker in Testdriven.io
date: 2018-06-04 20:47:13
tags: [Testdriven.io, flask, react, docker]
---

> https://testdriven.io

> https://github.com/testdrivenio/testdriven-app-2.2/tree/part1

<!-- more -->

# Get Started
## 准备工作
```
$ mkdir testdriven-app && cd testdriven-app
$ mkdir services && cd services
$ mkdir users && cd users
$ mkdir project
$ virtualenv env
$ source env/bin/activate
(env)$ pip install flask
```

## 编写文件

```python
# services/users/project/__init__.py

from flask import Flask, jsonify


# instantiate the app
app = Flask(__name__)


@app.route('/users/ping', methods=['GET'])
def ping_pong():
    return jsonify({
        'status': 'success',
        'message': 'pong!'
    })
```

## Flask CLI
接下来，我们配置 [Flask CLI](http://flask.pocoo.org/docs/1.0/cli/) 工具来通过命令行运行和管理应用。
> 如果习惯于使用 [Flask Script](https://flask-script.readthedocs.io/en/latest/) ，请随时替换为 CLI 工具，但是需要注意的是 Flask Script 已经弃用了。

首先，在 *users* 路径添加 `manage.py` 文件：
```python
# services/users/manage.py


from flask.cli import FlaskGroup

from project import app


cli = FlaskGroup(app)


if __name__ == '__main__':
    cli()
```

这里，我们创建了`FlaskGroup`实例扩展 `CLI` 通过命令行来与 Flask 应用相关联。

## 运行服务
```
(env)$ export FLASK_APP=project/__init__.py
(env)$ python manage.py run
```

在浏览器打开 http://127.0.0.1:5000/users/ping ，就可以看到结果：
```
{
  "message": "pong!",
  "status": "success"
}
```

## 配置文件
```python
# services/users/project/config.py


class BaseConfig:
    """Base configuration"""
    TESTING = False


class DevelopmentConfig(BaseConfig):
    """Development configuration"""
    pass


class TestingConfig(BaseConfig):
    """Testing configuration"""
    TESTING = True


class ProductionConfig(BaseConfig):
    """Production configuration"""
    pass
```

更新`__init__.py`为开发环境配置：
```python
# services/users/project/__init__.py


from flask import Flask, jsonify

# instantiate the app
app = Flask(__name__)

# set config
app.config.from_object('project.config.DevelopmentConfig')


@app.route('/users/ping', methods=['GET'])
def ping_pong():
    return jsonify({
        'status': 'success',
        'message': 'pong'
    })
```
## 调试模式
再次运行，这次我们开始 debug 模式：
```
(env) > $ export FLASK_DEBUG=1
(env) > $ python manage.py run
* Serving Flask app "project/__init__.py" (lazy loading)
 * Environment: production
   WARNING: Do not use the development server in a production environment.
   Use a production WSGI server instead.
 * Debug mode: on
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
 * Restarting with stat
 * Debugger is active!
 * Debugger PIN: 172-223-302
```

现在我们改变代码后，应用将会自动重载，不需要重新启动应用了。

最后，我们在 *users* 目录下生成`requirements.txt`文件，并使用 *git* 管理代码。
```
# services/users
pip freeze > requirements.txt
```

# Docker Container
这节我们来将应用容器化。

确保我们安装了 *Docker*, *Docker Compose*, *Docker Machine*。
```
$ docker -v
Docker version 18.03.1-ce, build 9ee9f40
$ docker-compose -v
docker-compose version 1.21.2, build a133471
```

> 事实上，Docker Machine 并不是必要的，我们将跳过 Docker Machine 部分

## 编写相关Docker文件
`users/.dockerignore`，类似`.gitignore`:
```
env
.dockerignore
Dockerfile-dev
Dockerfile-prod
```

`Dockerfile-dev`:
```
# FROM python:3.6.4
FROM python

# set working directory
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

# add requirements
COPY ./requirements.txt /usr/src/app/requirements.txt

# install requirements
RUN pip install -r requirements.txt

# add app
COPY . /usr/src/app

# run server
CMD python manage.py run -h 0.0.0.0
```

在根目录添加`docker-compose-dev.yml`文件：
```
version: '3'

services:

  users:
    container_name: users
    build:
      context: ./services/users
      dockerfile: Dockerfile-dev
    volumes:
      - './services/users:/usr/src/app'
    ports:
      - 5001:5000
    environment:
      - FLASK_APP=project/__init__.py
      - FLASK_DEBUG=1
```

这个配置将会创建`users`容器。

## 构建与运行
```
docker-compose -f docker-compose-dev.yml build
docker-compose -f docker-compose-dev.yml up -d
```
> `-d`是指容器在后台运行。

这时候，访问 http://127.0.0.1:5001/users/ping 也能访问到页面。

## 配置开发环境

接下来，在`docker-compose-dev.yml`添加环境变量来加载开发环境的应用配置：
```
version: '3'

services:

  users:
    container_name: users
    build:
      context: ./services/users
      dockerfile: Dockerfile-dev
    volumes:
      - './services/users:/usr/src/app'
    ports:
      - 5001:5000
    environment:
      - FLASK_APP=project/__init__.py
      - FLASK_DEBUG=1
      - APP_SETTINGS=project.config.DevelopmentConfig
```
然后，完善`project/__init__.py`：
```python
import os
from flask import Flask, jsonify

# instantiate the app
app = Flask(__name__)

# set config
app_settings = os.getenv('APP_SETTINGS')
app.config.from_object(app_settings)


@app.route('/users/ping', methods=['GET'])
def ping_pong():
    return jsonify({
        'status': 'success',
        'message': 'pong'
    })
```

如果想要测试配置是否被加载，可以添加`print`到`__init__.py`文件的路由处理代码之前，查看应用的配置确保其起作用：
```python
import sys
print(app.config, file=sys.stderr)
```

# Posgres Setup
本节，我们将配置 Postgres，将其运行在另一个容器中，将其链接到`users`容器。

```
(env) > $ pip install flask-sqlalchemy
(env) > $ pip install psycopg2
```

## 修改配置文件
`config.py`:
```python
class BaseConfig:
    """Base configuration"""
    TESTING = False
    SQLALCHEMY_TRACK_MODIFICATIONS = False 

class DevelopmentConfig:
    """Development configuration"""
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_URL')


class TestingConfig:
    """Testing configuration"""
    TESTING = True
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_TEST_URL')


class ProductionConfig:
    """Production configuration"""
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_URL')
```

修改`__init__.py`，在文件中创建 SQLAlchemy 并定义数据模型：
```python
import os
from flask import Flask, jsonify
from flask_sqlalchemy import SQLAlchemy

# instantiate the app
app = Flask(__name__)

# set config
app_settings = os.getenv('APP_SETTINGS')
app.config.from_object(app_settings)

# instantiate the db
db = SQLAlchemy(app)


# model
class User(db.Model):
    __tablename__ = "users"
    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    username = db.Column(db.String(128), nullable=False)
    email = db.Column(db.String(128), nullable=False)
    active = db.Column(db.Boolean(), default=True, nullable=True)

    def __init__(self, username, email):
        self.username = username
        self.email - email


# routes
@app.route('/users/ping', methods=['GET'])
def ping_pong():
    return jsonify({
        'status': 'success',
        'message': 'pong'
    })
```

## 创建数据库
在 *project* 路径下建立 *db* 文件夹，并在其中创建`create.sql`文件：
```sql
CREATE DATABASE users_prod;
CREATE DATABASE users_dev;
CREATE DATABASE users_test;
```

接下来，我们在同级目录创建`db/Dockerfile`文件：
```
FROM postgres

# run create.sql on init
ADD create.sql /docker-entrypoint-initdb.d
```

完善 *docker-compose-dev.yml*:
```
version: '3'

services:

  users:
    container_name: users
    build:
      context: ./services/users
      dockerfile: Dockerfile-dev
    volumes:
      - './services/users:/usr/src/app'
    ports:
      - 5001:5000
    environment:
      - FLASK_APP=project/__init__.py
      - FLASK_DEBUG=1
      - APP_SETTINGS=project.config.DevelopmentConfig
      - DATABASE_URL=postgres://postgres:postgres@users-db:5432/users_dev
      - DATABASE_TEST_URL=postgres://postgres:postgres@users-db:5432/users_test
    depends_on:
      - users-db
    links:
      - users-db

  users-db:
    container_name: users-db
    build:
      context: ./services/users/project/db
      dockerfile: Dockerfile
    ports:
      - 5435:5432
    environment:
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
```

在 *users* 文件夹下添加`entrypoint.sh`:
```
#!/bin/sh

echo "Waiting for postgres..."

while ! nc -z users-db 5432; do
  sleep 0.1
done

echo "PostgreSQL started"

python manage.py run -h 0.0.0.0
```

完善`Dockerfile-dev`:
```
# FROM python:3.6.4
FROM python

# install environment dependencies
RUN apt-get update -yqq \
  && apt-get install -yqq --no-install-recommends \
    netcat \
  && apt-get -q clean

# set working directory
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

# add requirements
COPY ./requirements.txt /usr/src/app/requirements.txt

# install requirements
RUN pip install -r requirements.txt

# add entrypoint.sh
COPY ./entrypoint.sh /usr/src/app/entrypoint.sh

# add app
COPY . /usr/src/app

# run server
# CMD python manage.py run -h 0.0.0.0
CMD ["./entrypoint.sh"]
```

赋予权限：
```
$ chmod +x services/users/entrypoint.sh
```

合理性检查：
```
$ docker-compose -f docker-compose-dev.yml up -d --build
```

访问 http://127.0.0.1:5001/users/ping 就能看到结果了。


## 重建数据库
修改`manage.py`:
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

可以看到，我们注册了一个新的命令`recreate_db`，CLI 能从命令行运行它：
```
$ docker-compose -f docker-compose-dev.yml \
  run users python manage.py recreate_db
```

想知道起不起作用？让我们看一下数据库：
```
$ docker exec -ti $(docker ps -aqf "name=users-db") psql -U postgres

users_dev=# \c users_dev
You are now connected to database "users_dev" as user "postgres".

users_dev=# \dt
         List of relations
 Schema | Name  | Type  |  Owner
--------+-------+-------+----------
 public | users | table | postgres
(1 row)

users_dev=# \d users
                                     Table "public.users"
  Column  |          Type          | Collation | Nullable |              Default
              
----------+------------------------+-----------+----------+---------------------
--------------
 id       | integer                |           | not null | nextval('users_id_se
q'::regclass)
 username | character varying(128) |           | not null | 
 email    | character varying(128) |           | not null | 
 active   | boolean                |           |          | 
Indexes:
    "users_pkey" PRIMARY KEY, btree (id)

users_dev=# \q
```

> 如果我们运行了上面的重建数据库的命令，则之前的数据都会丢失。

# Test Setup
在*project*路径下添加*tests*目录，并 在该目录中添加如下文件：`__init__.py`, `base.py`, `test_config.py`,`test_users.py`

```
pip install Flask-Testing
```

```python
# services/users/project/tests/base.py


from flask_testing import TestCase

from project import app, db


class BaseTestCase(TestCase):
    def create_app(self):
        app.config.from_object('project.config.TestingConfig')
        return app

    def setUp(self):
        db.create_all()
        db.session.commit()

    def tearDown(self):
        db.session.remove()
        db.drop_all()
```

```python
# services/users/project/tests/test_config.py


import os
import unittest

from flask import current_app
from flask_testing import TestCase

from project import app


class TestDevelopmentConfig(TestCase):
    def create_app(self):
        app.config.from_object('project.config.DevelopmentConfig')
        return app

    def test_app_is_development(self):
        self.assertTrue(app.config['SECRET_KEY'] == 'my_precious')
        self.assertFalse(current_app is None)
        self.assertTrue(
            app.config['SQLALCHEMY_DATABASE_URI'] ==
            os.environ.get('DATABASE_URL')
        )


class TestTestingConfig(TestCase):
    def create_app(self):
        app.config.from_object('project.config.TestingConfig')
        return app

    def test_app_is_testing(self):
        self.assertTrue(app.config['SECRET_KEY'] == 'my_precious')
        self.assertTrue(app.config['TESTING'])
        self.assertFalse(app.config['PRESERVE_CONTEXT_ON_EXCEPTION'])
        self.assertTrue(
            app.config['SQLALCHEMY_DATABASE_URI'] ==
            os.environ.get('DATABASE_TEST_URL')
        )


class TestProductionConfig(TestCase):
    def create_app(self):
        app.config.from_object('project.config.ProductionConfig')
        return app

    def test_app_is_production(self):
        self.assertTrue(app.config['SECRET_KEY'] == 'my_precious')
        self.assertFalse(app.config['TESTING'])


if __name__ == '__main__':
    unittest.main()
```
```python
# services/users/project/tests/test_users.py


import json
import unittest

from project.tests.base import BaseTestCase


class TestUserService(BaseTestCase):
    """Tests for the Users Service."""

    def test_users(self):
        """Ensure the /ping route behaves correctly."""
        response = self.client.get('/users/ping')
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 200)
        self.assertIn('pong!', data['message'])
        self.assertIn('success', data['status'])


if __name__ == '__main__':
    unittest.main()
```

```python
@cli.command()
def test():
    """ Runs the tests without code coverage"""
    tests = unittest.TestLoader().discover('project/tests', pattern='test*.py')
    result = unittest.TextTestRunner(verbosity=2).run(tests)
    if result.wasSuccessful():
        return 0
    return 1
```

重构和运行：
```
$ docker-compose -f docker-compose-dev.yml up -d --build

$ docker-compose -f docker-compose-dev.yml \
  run users python manage.py test
```

运行完后，会发现报错：`self.assertTrue(app.config['SECRET_KEY'] == 'my_precious')`

升级 *base.py*:
```python
class BaseConfig:
    """Base configuration"""
    TESTING = False
    SQLALCHEMY_TRACK_MODIFICATIONS = False
    SECRET_KEY = 'my_precious'
```
测试结果：
```
----------------------------------------------------------------------
Ran 4 tests in 0.081s

```


# Flask Blueprints
> 如果不了解 Blueprints，可以查看这篇官方文档 [Blueprints](http://flask.pocoo.org/docs/1.0/blueprints/)。本质上，它有着自己的组件，使用内部的代码、模板和静态文件。

```
mkdir services/users/project/api
cd services/users/project/api
touch __init__.py users.py models.py
```
```python
# services/users/project/api/users.py


from flask import Blueprint, jsonify


users_blueprint = Blueprint('users', __name__)


@users_blueprint.route('/users/ping', methods=['GET'])
def ping_pong():
    return jsonify({
        'status': 'success',
        'message': 'pong!'
    })

```

```python
# services/users/project/api/models.py


from project import db


class User(db.Model):
    __tablename__ = "users"
    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    username = db.Column(db.String(128), nullable=False)
    email = db.Column(db.String(128), nullable=False)
    active = db.Column(db.Boolean(), default=True, nullable=False)

    def __init__(self, username, email):
        self.username = username
        self.email = email
```

移除 `project/__init__.py` 里的 `class User()` 并添加 [Application Factory](http://flask.pocoo.org/docs/1.0/patterns/appfactories/)。

```python
import os
from flask import Flask
from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()


def create_app():
    app = Flask(__name__)

    # set config
    app_settings = os.getenv('APP_SETTINGS')
    app.config.from_object(app_settings)

    # set up extensions
    db.init_app(app)

    # register blueprints
    from api.users import users_blueprint
    app.register_blueprint(users_blueprint)

    # shell context for flask cli
    app.shell_context_processor({
        'app': app,
        'db': db
    })
    return app
```

注意 [shell_context_processor](https://testdriven.io/part-one-flask-blueprints)，其用于注册`app`和`db`到 shell 命令行。现在我们的应用中不需要 *import* 而可以直接地使用它们。

```python
# services/users/manage.py

from project import create_app, db

app = create_app()
cli = FlaskGroup(create_app=create_app)

...
```
```python
# project/tests/base.py

from project import create_app, db

app = create_app()
```

最后，移除`docker-compose-dev.yml`里面的`FALSK_APP`环境设置:
```
# - FLASK_APP=project/__init__.py
```
让我们来测试一下：
```
$ docker-compose -f docker-compose-dev.yml up -d

$ docker-compose -f docker-compose-dev.yml \
  run users python manage.py recreate_db

$ docker-compose -f docker-compose-dev.yml \
  run users python manage.py test
```

# RESTful Routes
接下来，我们建立 3 个路由：

|Endpoint|HTTP Method|CRUD Method|Result|
|----|----|----|----|
|/user|GET|READ|get all users|
|/users/:id|GET|READ|get single user|
|/users|POST|CREATE|add a user|

## POST
在`project/tests/test_users.py`中的`TestUserService()`类添加：
```python
def test_add_user(self):
    """Ensure a new user can be added to the database."""
    with self.client:
        response = self.client.post(
            '/users',
            data=json.dumps({
                'username': 'michael',
                'email': 'michael@mherman.org'
            }),
            content_type='application/json',
        )
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 201)
        self.assertIn('michael@mherman.org was added!', data['message'])
        self.assertIn('success', data['status'])
```


运行测试命令：
```
$ docker-compose -f docker-compose-dev.yml \
  run users python manage.py test
```
我们会看到报错，是因为没有在`project/api/users.py`添加响应的路由处理方法：
```python
from flask import Blueprint, jsonify, request

from project.api.models import User
from project import db

# ...

@users_blueprint.route('/users', methods=['POST'])
def add_user():
    post_data = request.get_json()
    username = post_data.get('username')
    email = post_data.get('email')
    db.session.add(User(username=username, email=email))
    db.session.commit()
    response_object = {
        'status': 'success',
        'message': f'{email} was added!'
    }
    return jsonify(response_object), 201
```    

`test_users.py`:
```python
def test_add_user_invalid_json(self):
    """Ensure error is thrown if the JSON object is empty."""
    with self.client:
        response = self.client.post(
            '/users',
            data=json.dumps({}),
            content_type='application/json',
        )
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 400)
        self.assertIn('Invalid payload.', data['message'])
        self.assertIn('fail', data['status'])

def test_add_user_invalid_json_keys(self):
    """
    Ensure error is thrown if the JSON object does not have a username key.
    """
    with self.client:
        response = self.client.post(
            '/users',
            data=json.dumps({'email': 'michael@mherman.org'}),
            content_type='application/json',
        )
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 400)
        self.assertIn('Invalid payload.', data['message'])
        self.assertIn('fail', data['status'])

def test_add_user_duplicate_email(self):
    """Ensure error is thrown if the email already exists."""
    with self.client:
        self.client.post(
            '/users',
            data=json.dumps({
                'username': 'michael',
                'email': 'michael@mherman.org'
            }),
            content_type='application/json',
        )
        response = self.client.post(
            '/users',
            data=json.dumps({
                'username': 'michael',
                'email': 'michael@mherman.org'
            }),
            content_type='application/json',
        )
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 400)
        self.assertIn(
            'Sorry. That email already exists.', data['message'])
        self.assertIn('fail', data['status'])
```        

更新路由处理：
```python
from sqlalchemy import exc


@users_blueprint.route('/users', methods=['POST'])
def add_user():
    post_data = request.get_json()
    response_object = {
        'status': 'fail',
        'message': 'Invalid payload.'
    }
    if not post_data:
        return jsonify(response_object), 400
    username = post_data.get('username')
    email = post_data.get('email')
    try:
        user = User.query.filter_by(email=email).first()
        if not user:
            db.session.add(User(username=username, email=email))
            db.session.commit()
            response_object['status'] = 'success'
            response_object['message'] = f'{email} was added!'
            return jsonify(response_object), 201
        else:
            response_object['message'] = 'Sorry. That email already exists.'
            return jsonify(response_object), 400
    except exc.IntegrityError as e:
        db.session.rollback()
        return jsonify(response_object), 400
```

运行测试：
```
> $ docker-compose -f docker-compose-dev.yml \ 
  run users python manage.py test

Starting users-db ... done
test_app_is_development (test_config.TestDevelopmentConfig) ... ok
test_app_is_production (test_config.TestProductionConfig) ... ok
test_app_is_testing (test_config.TestTestingConfig) ... ok
test_add_invalid_json (test_users.TestUserService)
Ensure error is thrown if the JSON object is empty. ... ok
test_add_user (test_users.TestUserService)
Ensure a new user can be added to the database. ... ok
test_add_user_duplicate_email (test_users.TestUserService)
Ensure error is thrown if the email already exists. ... ok
test_add_user_invalid_json_keys (test_users.TestUserService) ... ok
test_users (test_users.TestUserService)
Ensure the /ping route behaves correctly. ... ok

----------------------------------------------------------------------
Ran 8 tests in 0.184s

OK
```


## GET single user
```python
from project import db
from project.api.models import User

def add_user(username, email):
    user = User(username=username, email=email)
    db.session.add(user)
    db.session.commit()
    return user

# ...

def test_single_user(self):
    """Ensure get single user behaves correctly."""
    user = User(username='michael', email='michael@mherman.org')
    db.session.add(user)
    db.session.commit()
    with self.client:
        response = self.client.get(f'/users/{user.id}')
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 200)
        self.assertIn('michael', data['data']['username'])
        self.assertIn('michael@mherman.org', data['data']['email'])
        self.assertIn('success', data['status'])


def test_single_user_no_id(self):
    """Ensure error is thrown if an id is not provided."""
    with self.client:
        response = self.client.get('/users/blah')
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 404)
        self.assertIn('User does not exist', data['message'])
        self.assertIn('fail', data['status'])

def test_single_user_incorrect_id(self):
    """Ensure error is thrown if the id does not exist."""
    with self.client:
        response = self.client.get('/users/999')
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 404)
        self.assertIn('User does not exist', data['message'])
        self.assertIn('fail', data['status'])
```

```python
@users_blueprint.route('/users/<user_id>', methods=['GET'])
def get_single_user(user_id):
    """Get single user details"""
    response_object = {
        'status': 'fail',
        'message': 'User does not exist'
    }
    try:
        user = User.query.filter_by(id=int(user_id)).first()
        if not user:
            return jsonify(response_object), 404
        else:
            response_object = {
                'status': 'success',
                'data': {
                    'id': user.id,
                    'username': user.username,
                    'email': user.email,
                    'active': user.active
                }
            }
            return jsonify(response_object), 200
    except ValueError:
        return jsonify(response_object), 404
```

## GET all users

```python
def test_all_users(self):
    """Ensure get all users behaves correctly."""
    add_user('michael', 'michael@mherman.org')
    add_user('fletcher', 'fletcher@notreal.com')
    with self.client:
        response = self.client.get('/users')
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 200)
        self.assertEqual(len(data['data']['users']), 2)
        self.assertIn('michael', data['data']['users'][0]['username'])
        self.assertIn(
            'michael@mherman.org', data['data']['users'][0]['email'])
        self.assertIn('fletcher', data['data']['users'][1]['username'])
        self.assertIn(
            'fletcher@notreal.com', data['data']['users'][1]['email'])
        self.assertIn('success', data['status'])
```
```python
@users_blueprint.route('/users', methods=['GET'])
def get_all_users():
    """Get all users"""
    response_object = {
        'status': 'success',
        'data': {
            'users': [user.to_json() for user in User.query.all()]
        }
    }
    return jsonify(response_object), 200
```                    

## 前段页面展示
运行命令
```
docker-compose -f docker-compose-dev.yml up -d 
```

浏览器访问  http://DOCKER_MACHINE_IP:5001/users ，可以看到返回结果：
```js
{
  "data": {
    "users": [ ]
  },
  "status": "success"
}
```

在`manage.py`添加种子数据：
```python
@cli.command()
def seed_db():
    """Seeds the database."""
    db.session.add(User(username='michael', email="hermanmu@gmail.com"))
    db.session.add(User(username='michaelherman', email="michael@mherman.org"))
    db.session.commit()
```
运行命令再次访问页面
```
$ docker-compose -f docker-compose-dev.yml \
  run users python manage.py seed_db
```      

可以看到如下返回结果：
```js
{
    "data": {
        "users": [
        {
            "active": true,
                "email": "hermanmu@gmail.com",
                "id": 1,
                "username": "michael"
        },
        {
            "active": true,
            "email": "michael@mherman.org",
            "id": 2,
            "username": "michaelherman"
        }
        ]
    },
        "status": "success"
}
```
