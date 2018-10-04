---
title: Flask React Docker in Testdriven - Part II - 4
date: 2018-09-25 20:49:27
tags: [testdriven, flask, react, docker]
---

# Flask Debug Toolbar
本节讲解 [Flask调试工具栏](https://flask-debugtoolbar.readthedocs.io/en/latest/) 。

Flask Debug Toolbar 是一个 Flask 扩展，可以帮助您调试应用程序。它在视图中添加了一个调试工具栏，它提供有关 HTTP 标头，请求变量，配置设置以及呈现特定视图所需的 SQLAlchemy 查询数的信息。您可以使用此信息来查找视图呈现中的瓶颈。

<!-- more -->

```
pip install flask-debugtoolbar
```
```
pip freeze > services/users/requirements.txt
```


要启用，创建工具栏的实例，然后将它添加到应用程序 `create_app()` 中的 `services/users/project/__init__.py`:
```python
import os

from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from flask_debugtoolbar import DebugToolbarExtension  # new


# instantiate the extensions
db = SQLAlchemy()
toolbar = DebugToolbarExtension()  # new


def create_app(script_info=None):

    # instantiate the app
    app = Flask(__name__)

    # set config
    app_settings = os.getenv('APP_SETTINGS')
    app.config.from_object(app_settings)

    # set up extensions
    db.init_app(app)
    toolbar.init_app(app)  # new

    # register blueprints
    from project.api.users import users_blueprint
    app.register_blueprint(users_blueprint)

    # shell context for flask cli
    @app.shell_context_processor
    def ctx():
        return {'app': app, 'db': db}

    return app
```

`services/users/project/config.py`:
```python
import os  # new


class BaseConfig:
    """Base configuration"""
    TESTING = False
    SQLALCHEMY_TRACK_MODIFICATIONS = False
    SECRET_KEY = 'my_precious'
    DEBUG_TB_ENABLED = False              # new
    DEBUG_TB_INTERCEPT_REDIRECTS = False  # new


class DevelopmentConfig(BaseConfig):
    """Development configuration"""
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_URL')
    DEBUG_TB_ENABLED = True  # new


class TestingConfig(BaseConfig):
    """Testing configuration"""
    TESTING = True
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_TEST_URL')


class ProductionConfig(BaseConfig):
    """Production configuration"""
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_URL')
```



> 更多的配置选项可以查看[文档](https://flask-debugtoolbar.readthedocs.io/#configuration) 。


`services/users/project/tests/test_config.py`:
```python
import os
import unittest

from flask import current_app
from flask_testing import TestCase

from project import create_app

app = create_app()


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
        self.assertTrue(app.config['DEBUG_TB_ENABLED'])  # new


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
        self.assertFalse(app.config['DEBUG_TB_ENABLED'])  # new


class TestProductionConfig(TestCase):
    def create_app(self):
        app.config.from_object('project.config.ProductionConfig')
        return app

    def test_app_is_production(self):
        self.assertTrue(app.config['SECRET_KEY'] == 'my_precious')
        self.assertFalse(app.config['TESTING'])
        self.assertFalse(app.config['DEBUG_TB_ENABLED'])  # new


if __name__ == '__main__':
    unittest.main()
```


更新容器并运行测试：
```
$ docker-compose -f docker-compose-dev.yml up -d --build
$ docker-compose -f docker-compose-dev.yml run users python manage.py test
```

浏览器导航到 http://localhost 将看到工具栏：

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/0923/005.png)
</center>
