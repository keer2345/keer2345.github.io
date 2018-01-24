---
title: Flask投票应用 01 （介绍与安装）
date: 2018-01-23 22:51:02
categories: python
tags: [python, flask]
---

> https://danidee10.github.io/2016/09/18/flask-by-example-1.html

Welcome everyone! 我们来开始一个Flask系列，学习如何构建一个在线投票应用。

这一系列分为几个部分，我们最后构建的应用包含以下特性：
- 基本的认证系统
- 各种主题的投票
- 交互的进度条展示投票的百分比

我们使用[Flask](http://flask.pocoo.org/)做为后端，[Bootstrap3](http://getbootstrap.com/)做为前段样式，[ReactJS](https://facebook.github.io/react/)增强UI并有更多的交互。

<!-- more -->


<!-- vim-markdown-toc GFM -->

* [为什么使用Flask?](#为什么使用flask)
* [教程目标](#教程目标)
* [准备工作](#准备工作)
    * [安装 Virtualenv](#安装-virtualenv)
    * [安装Flask](#安装flask)
* [从Hello world开始](#从hello-world开始)

<!-- vim-markdown-toc -->

我们的应用命名为**Flask_Vote**

# 为什么使用Flask?
> Flask is a microframework for Python based on Werkzeug, Jinja 2 and good intentions

如果我们做为一个新手并对Django有一些迷惑和沮丧，那么不妨尝试一下Flask，使用Flask能帮助我们对Python Web有更深的了解。

# 教程目标
- Built a REST API with flask
- Understanding ORM SQLAlchemy
- Flask extension like [Flask SQLAlchemy](http://flask-sqlalchemy.pocoo.org/2.3/),[Flask Migrate](https://flask-migrate.readthedocs.io/en/latest/) and [Flask-Admin](http://flask-admin.readthedocs.io/en/latest/).
- Learned how to Build interactive and dynamic UI's with *ReactJS*.
- How to structure your flask applications and use *Flask Blueprints*.
- Learnt how to run background jobs with [Celery](http://www.celeryproject.org).

先看一下效果图：
<center>
![效果图](https://danidee10.github.io/images/screenshot.png)
</center>

进度条效果图：
<center>
![进度条](https://danidee10.github.io/images/votr.gif)
</center>

# 准备工作
- Python 3.x
- Virtualenv
- Flask

## 安装 Virtualenv
通过pip安装:
```
sudo pip3 install virtualenv
```

创建基于python3的虚拟环境
```
virtualenv -p python3 .venv
```

激活虚拟环境
```
source .venv/bin/activate
```

退出虚拟环境
```
deactivate
```

## 安装Flask
```
(.venv) $ pip3 install flask
```

# 从Hello world开始
创建*manage.py*文件:
```
mkdir Flask_Vote
cd Flask_Vote
touch manage.py
```

编辑*manage.py*:
```python
from flask import Flask
vote = Flask(__name__)


@vote.route('/')
def home():
    return "Hello world!"


if __name__ == '__main__':
    vote.run()
```

在终端运行：
```
(.venv) > $ python manage.py
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```
访问http://127.0.0.1:5000/就可以看到效果。

> 本文的源代码可以在[Github](https://github.com/keer2345/Flask_Vote/tree/v.01)中找到。
