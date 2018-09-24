---
title: Flask React Docker in Testdriven - Part I - 08
date: 2018-09-23 20:49:18
tags: [testdriven, flask, react, docker]
---

# Flask Blueprints

测试之后，我们来重构应用，添加 *Bluepoint* 。

```
mkdir services/users/project/api
touch services/users/project/api/__init__.py
```

`services/users/project/api/users.py`


```python
from flask import Blueprint, jsonify

users_bluepoint = Blueprint('users', __name__)


@users_bluepoint.route('/users/ping', methods=['GET'])
def ping_pong():
    return jsonify({'status': 'success', 'message': 'pong!'})
```

<!-- more -->

在这里，我们创建了一个 `Blueprint` 类的新实例，并将 `ping_pong()` 视图函数绑定到它。

`services/users/project/api/models.py`:
```python
from sqlalchemy.sql import func

from project import db


class User(db.Model):
    __tablename__ = 'users'

    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    username = db.Column(db.String(128), nullable=False)
    email = db.Column(db.String(128), nullable=False)
    active = db.Column(db.Boolean(), default=True, nullable=False)
    created_date = db.Column(db.DateTime, default=func.now(), nullable=False)

    def __init__(self, username, email):
        self.username = username
        self.email = email
```


`services/users/project/__init__.py`:
```python
import os

from flask import Flask
from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()


def create_app(script_info=None):
    app = Flask(__name__)

    # set config
    app_settings = os.getenv('APP_SETTINGS')
    app.config.from_object(app_settings)

    # set up extensions
    db.init_app(app)

    # register bluepoint
    from project.api.users import users_bluepoint
    app.register_blueprint(users_bluepoint)

    # shell context for flask cli
    @app.shell_context_processor
    def ctx():
        return {'app': app, 'db': db}

    return app
```


注意到 `shell_context_processor` 。这用于注册 `app` 和 `db` shell。现在我们可以使用应用程序上下文和数据库，而无需将它们直接导入 *shell* ，您很快就会看到它。

`services/users/manage.py`:
```python
import unittest

from flask.cli import FlaskGroup

from project import create_app, db
from project.api.models import User

app = create_app()
cli = FlaskGroup(create_app=create_app)


@cli.command()
def recreate_db():
    db.drop_all()
    db.create_all()
    db.session.commit()


@cli.command()
def test():
    """ Runs the tests without code coverage"""
    tests = unittest.TestLoader().discover('project/tests', pattern='test*.py')
    result = unittest.TextTestRunner(verbosity=2).run(tests)
    if result.wasSuccessful():
        return 0
    return 1


if __name__ == '__main__':
    cli()
```


现在，您可以直接使用 `app` 和 `db` 上下文：
```
$ docker-compose -f docker-compose-dev.yml run users flask shell

Python 3.6.5 (default, Jun  6 2018, 23:08:29)
[GCC 6.4.0] on linux
App: project [development]
Instance: /usr/src/app/instance

>>> app
<Flask 'project'>

>>> db
<SQLAlchemy engine=postgres://postgres:***@users-db:5432/users_dev>

>>> exit()
```

更新 `project/tests/base.py` 和 `project/tests/test_config.py` 顶部的导入：
```python
from project import create_app

app = create_app()
```

最后，`FLASK_APP` 从 `docker-compose-dev.yml` 中删除环境变量：
```
environment:
    - FLASK_ENV=development
    - APP_SETTINGS=project.config.DevelopmentConfig
    - DATABASE_URL=postgres://postgres:postgres@users-db:5432/users_dev
    - DATABASE_TEST_URL=postgres://postgres:postgres@users-db:5432/users_test
```

开始测试！
```
$ docker-compose -f docker-compose-dev.yml up -d

$ docker-compose -f docker-compose-dev.yml run users python manage.py recreate_db

$ docker-compose -f docker-compose-dev.yml run users python manage.py test
```

将模型应用于开发环境的数据库中：
```
$ docker-compose -f docker-compose-dev.yml run users python manage.py recreate_db
```

工作了吗？让我们进入数据库看看：
```
$ docker-compose -f docker-compose-dev.yml exec users-db psql -U postgres

psql (10.4)
Type "help" for help.

postgres=# \c users_dev
You are now connected to database "users_dev" as user "postgres".
users_dev=# \dt
         List of relations
 Schema | Name  | Type  |  Owner
--------+-------+-------+----------
 public | users | table | postgres
(1 row)

users_dev=# select * from users;
 id | username | email | active | created_date 
----+----------+-------+--------+--------------
(0 rows)

users_dev=# \q
```
