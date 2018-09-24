---
title: Flask React Docker in Testdriven - Part I - 07
date: 2018-09-23 20:49:17
tags: [testdriven, flask, react, docker]
---
# Test Setup

本节讲解测试。

```
$ pip install flask-testing

$ mkdir services/users/project/tests && cd services/users/project/tests
$ touch __init__.py base.py test_config.py test_users.py
```

<!-- more -->

`services/users/project/tests/base.py`:
```python
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

`services/users/project/tests/test_config.py`:
```python
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
        self.assertTrue(app.config['SQLALCHEMY_DATABASE_URI'] == os.environ.
                        get('DATABASE_URL'))


class TestTestingConfig(TestCase):
    def create_app(self):
        app.config.from_object('project.config.TestingConfig')
        return app

    def test_app_is_testing(self):
        self.assertTrue(app.config['SECRET_KEY'] == 'my_precious')
        self.assertTrue(app.config['TESTING'])
        self.assertFalse(app.config['PRESERVE_CONTEXT_ON_EXCEPTION'])
        self.assertTrue(app.config['SQLALCHEMY_DATABASE_URI'] == os.environ.
                        get('DATABASE_TEST_URL'))


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

`services/users/project/tests/test_users.py`:
```python
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


在 `manage.py` 中添加新的命令，以运行测试：
```python
import unittest

# ...

@cli.command()
def test():
    """ Runs the tests without code coverage"""
    tests = unittest.TestLoader().discover('project/tests', pattern='test*.py')
    result = unittest.TextTestRunner(verbosity=2).run(tests)
    if result.wasSuccessful():
        return 0
    return 1

# ...
```

由于添加了 `flask-testing` 包，我们需要重构镜像：
```
$ pip freeze > services/users/requirements.txt

$ docker-compose -f docker-compose-dev.yml up -d --build
```

容器启动后，运行测试：
```
$ docker-compose -f docker-compose-dev.yml run users python manage.py test
```

我们会看到如下报错：
```
self.assertTrue(app.config['SECRET_KEY'] == 'my_precious')
AssertionError: False is not true
```

修改配置文件：
```
class BaseConfig:
    """Base configuration"""
    TESTING = False
    SQLALCHEMY_TRACK_MODIFICATIONS = False
    SECRET_KEY = 'my_precious'
```

再次测试就成功了：
```
----------------------------------------------------------------------
Ran 4 tests in 0.063s

OK
```
