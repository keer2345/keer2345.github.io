---
title: Flask Web投票应用02：使用SQLAlchemy设计和操作数据库
date: 2018-01-24 20:21:44
categories: python
tags: [python, flask]
---

本节主要演示Flask中多对多的关系。
<center>
![表关系](https://danidee10.github.io/images/votr.png)
</center>

<!-- more -->



# Flask SQLAlchemy
[SQLAlchemy](http://www.sqlalchemy.org/)在Python社区是成熟且流行的ORM，Python有许多ORM，但是我们在这里仍选择SQLAlchemy，因为它的流行度以及出色的文档。

[Flask SQLAlchemy](http://flask-sqlalchemy.pocoo.org/2.3/)是SQLAlchemy的Flask扩展（当然我们也可以使用纯粹的SQLAlchemy，如果您想这样做的话）。


## 安装Flask SQLAlchemy
```
pip install flask-sqlalchemy
```
## 创建模型*models.py*
```python
from manage import db


#  Base model that for other models to inherit from
class Base(db.Model):
    __abstract__ = True
    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    date_created = db.Column(db.DateTime, default=db.func.current_timestamp())
    date_modified = db.Column(db.DateTime,
                              default=db.func.current_timestamp(),
                              onupdate=db.func.current_timestamp())


class Topics(Base):
    title = db.Column(db.String(500))

    #  user friendly way to display the object
    def __repr__(self):
        return self.title


class Options(Base):
    name = db.Column(db.String(200))


class Polls(Base):
    topic_id = db.Column(db.Integer, db.ForeignKey('topics.id'))
    option_id = db.Column(db.Integer, db.ForeignKey('options.id'))
    vote_count = db.Column(db.Integer, default=0)
    status = db.Column(db.Boolean)

    #  Many to Many
    topic = db.relationship('Topics',
                            foreign_keys=[topic_id],
                            backref=db.backref('options', lazy='dynamic'))
    option = db.relationship('Options', foreign_keys=[option_id])

    def __repr__(self):
        return self.option.name
```

## 创建配置文件*config.py*
```python
import os

basedir = os.path.abspath(os.path.dirname(__file__))


class Config(object):
    SECRET_KEY = 'development key'  # keep this key secret during production
    SQLALCHEMY_DATABASE_URI = os.environ.get(
        'DATABASE_URL') or 'sqlite:///' + os.path.join(basedir, 'vote.db')
    SQLALCHEMY_TRACK_MODIFICATIONS = False
    DEBUG = True
```

## 编辑文件*manage.py*
```python
from flask import Flask
from flask_sqlalchemy import SQLAlchemy

from config import Config

vote = Flask(__name__)
# load config from the config file we created earlier
vote.config.from_object(Config)

db = SQLAlchemy(vote)

import models

# initialize and create the database
#  db.init_app(vote)
#  db.create_all(app=vote)


@vote.route('/')
def home():
    return "Hello world!"


if __name__ == '__main__':
    vote.run()
```

然后在Python shell中执行如下脚本生成数据库：
```shell
(.venv) > $ python
Python 3.6.4 (default, Dec 19 2017, 17:29:45) 
[GCC 5.4.0 20160609] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> from manage import vote, db
>>> db.init_app(vote)
>>> db.create_all(app=vote)
```

在Sqlite3中可以查看到*models.py*中对应的三个表：
```sqlite
(.venv) > $ sqlite3
SQLite version 3.11.0 2016-02-15 17:29:24
Enter ".help" for usage hints.
Connected to a transient in-memory database.
Use ".open FILENAME" to reopen on a persistent database.
sqlite> .open vote.db
sqlite> .tables
options  polls    topics 
```

# 在Shell中演示SQLAlchemy
```shell
(.venv) > $ python
Python 3.6.4 (default, Dec 19 2017, 17:29:45) 
[GCC 5.4.0 20160609] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> from manage import vote, db
>>> from models import Topics, Options, Polls
>>> topic = Topics(title='Which side is going to win the EPL this season')
>>> arsenal = Options(name='Arsenal')
>>> spurs = Options(name='Spurs')

>>> poll_1 = Polls(topic=topic, option=arsenal)
>>> poll_2 = Polls(topic=topic, option=spurs)
>>> topic.options.all()
[Arsenal, Spurs]
>>> poll_1.vote_count = 3
>>> poll_2.vote_count = 2

>>> poll_1.topic
Which side is going to win the EPL this season
>>> poll_1.topic.title
'Which side is going to win the EPL this season'

>>> for option in topic.options.all():
...     print(option, option.vote_count)
... 
Arsenal 3
Spurs 2

>>> db.session.add(arsenal)
>>> db.session.add(spurs)
>>> db.session.add(topic)
>>> db.session.add(poll_1)
>>> db.session.add(poll_2)
>>> db.session.commit()
```

再来演示一个示例：
```shell
>>> city = Options(name='Manchester city')
>>> liverpool = Options(name='Liverpool FC')
>>> liverpool = Polls(option=liverpool)
>>> city = Polls(option=city)
>>> new_topic = Topics(title='Whos better liverpool or city', options=[liverpool, city])
>>> new_topic.options.all()
[Liverpool FC, Manchester city]
>>> liverpool.topic
Whos better liverpool or city
>>> liverpool.topic.title
'Whos better liverpool or city'
```

> 本文的源代码可以在[Github](https://github.com/keer2345/Flask_Vote/tree/v.02)中找到。
