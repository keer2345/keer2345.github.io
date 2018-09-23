---
title: Flask React Docker in Testdriven - Part I - 04
date: 2018-09-23 20:48:15
tags: [testdriven, flask, react, docker]
---
# Geting Started

本节，我们设置最基本的项目结构，以及定义第一个服务。

创建全新的项目，并安装 Flask ：
```
$ mkdir testdriven-app && cd testdriven-app
$ virtualenv .venv
$ source .venv/bin/activate
(.venv)$ pip install flask

$ mkdir services && cd services
$ mkdir users && cd users
$ mkdir project
```

<!-- more -->

在 *project* 目录添加 `__init__.py` 文件。

`services/users/project/__init__.py` :
```python
from flask import Flask, jsonify

app = Flask(__name__)


@app.route('/users/ping', methods=['GET'])
def ping_pong():
    return jsonify({'status': 'success', 'message': 'pong'})
```

接下来，配置 `Flask CLI` 工具以在终端运行应用。
> 如果您习惯了 [Flask Script](https://flask-script.readthedocs.io/en/latest/)，也可以用其替换 `Flask CLI`，但是请注意，它已经被[弃用](https://github.com/smurfix/flask-script/issues/172)了。

`services/users/manage.py` :
```python
from flask.cli import FlaskGroup

from project import app

cli = FlaskGroup(app)

if __name__ == '__main__':
    cli()
```

在这里，我们创建了一个新 `FlaskGroup` 实例，以使用与 Flask 应用程序相关的命令扩展常规 CLI 。

从“users”目录运行服务器：
```
(.venv)$ export FLASK_APP=project/__init__.py
(.venv)$ python manage.py run
```

在浏览器中访问 http://127.0.0.1:5000/users/ping 应该可以看到：
```json
{
    "message": "pong",
    "status": "success"
}
```

终止服务，并在 `project` 路径下创建 `config.py`。

`services/users/project/config.py`:

```python
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

更新 `__init__.py` 文件并引入 `dev` 配置：

`services/users/project/__init__.py`

```python
from flask import Flask, jsonify

app = Flask(__name__)

# set config
app.config.from_object('project.config.DevelopmentConfig')


@app.route('/users/ping', methods=['GET'])
def ping_pong():
    return jsonify({'status': 'success', 'message': 'pong'})
```

再次运行。这一次，我们通过设置 `FLASK_ENV` 的环境变量为 `development` 来启用调试模式。
```
$ export FLASK_ENV=development
$ python manage.py run
 * Serving Flask app "project/__init__.py" (lazy loading)
 * Environment: development
 * Debug mode: on
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
 * Restarting with stat
 * Debugger is active!
 * Debugger PIN: 181-409-129
```

现在，当您对代码进行更改时，应用程序将自动重新加载。完成后，终止服务器并从虚拟环境中停用。然后，将 `requirements.txt` 文件添加到 `users` 目录：
```
cd ~/testdriven-app
pip freeze > requirements.txt
```

最好，将项目使用 `Git` 管理。
