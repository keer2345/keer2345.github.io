---
title: Python Django Tutorial
date: 2018-05-26 09:12:52
categories: python
tags: [django]
---
# 准备
## 安装
```shell
mkdir django_tutorial
cd django_tutorial

virtualenv .venv
source .venv/bin/activate

pip install django
```

## 查看Django版本
```shell
(.venv) > $ django-admin --version
(.venv) > $ python -m django --version

2.0.5
```

<!-- more -->

# 创建项目
```shell
django-admin startproject django_tutorial .
```
用如下命令运行项目：
```
python manage.py runserver
```

## 创建一个APP
```shell
python manage.py startapp musics 
```

**切记**，在*setting.py*中的`INSTALLED_APPS`中加入该APP的名称：
```python
INSTALLED_APPS = [
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'musics',
]
```

## 视图Views
```shell
mkdir templates
touch templates/hello_django.html
```


*musics/templates/index.html**:
```html
<!DOCTYPE html>
<html lang="en" dir="ltr">
  <head>
    <meta charset="utf-8">
    <title>Title</title>
  </head>
  <body>
  <h1>Hello Django</h1>
  {{ data }}
  </body>
</html>
```

*musics/views.py*:
```python
from django.shortcuts import render


def hello_view(request):
    hello = "Nice to meet you!"
    context = {'data': hello}
    return render(request, 'index.html', context)
```

*musics/urls.py*:
```python
from django.urls import path

from . import views

urlpatterns = [
    path('', views.hello_view, name='hello'),
]
```

*django_tutorial/urls.py*:
```python
from django.contrib import admin
from django.urls import include, path

urlpatterns = [
    path('musics/', include('musics.urls')),
    path('admin/', admin.site.urls),
]
```


访问http://127.0.0.1:8000/musics/

# 数据库
## 构建模型
*musics/models.py*:
```python
from django.db import models


class Music(models.Model):
    song = models.TextField(default="song")
    singer = models.TextField(default='AKB48')
    last_modify_date = models.DateTimeField(auto_now=True)
    created = models.DateTimeField(auto_now_add=True)

    class Mate:
        db_table = "music"
```

## 构建数据库
```
python manage.py makemigrations
python manage.py migrate

```
```shell
(.venv) > $ python manage.py makemigrations
Migrations for 'musics':
  musics/migrations/0001_initial.py
    - Create model Music
```

```python
(.venv) > $ python manage.py migrate
Operations to perform:
  Apply all migrations: admin, auth, contenttypes, musics, sessions
Running migrations:
  Applying contenttypes.0001_initial... OK
  Applying auth.0001_initial... OK
  Applying admin.0001_initial... OK
  Applying admin.0002_logentry_remove_auto_add... OK
  Applying contenttypes.0002_remove_content_type_name... OK
  Applying auth.0002_alter_permission_name_max_length... OK
  Applying auth.0003_alter_user_email_max_length... OK
  Applying auth.0004_alter_user_username_opts... OK
  Applying auth.0005_alter_user_last_login_null... OK
  Applying auth.0006_require_contenttypes_0002... OK
  Applying auth.0007_alter_validators_add_error_messages... OK
  Applying auth.0008_alter_user_username_max_length... OK
  Applying auth.0009_alter_user_last_name_max_length... OK
  Applying musics.0001_initial... OK
  Applying sessions.0001_initial... OK
```

- `makemigrations`： 会创建一个文件，记录更新了哪些东西，比如上面的例子就会创建文件*musics/migrations/0001_initial.py*
- `migrate`： 根据`makemigrations`创建的文件，更新我们都的数据库

## Django ORM
在shell中使用ORM

### create
```shell
python manage.py shell
>>> Music.objects.create(song = 'song1',singer = 'SKE148')
<Music: Music object (1)>
>>> Music.objects.create()
<Music: Music object (2)>
```

### read
```shell
>>> Music.objects.all()
<QuerySet [<Music: Music object (1)>, <Music: Music object (2)>]>
>>> Music.objects.get(pk=1)
<Music: Music object (1)>
>>> Music.objects.filter(id=1)
<QuerySet [<Music: Music object (1)>]>
```

### update
```shell
>>> data = Music.objects.filter(id=1)
>>> data.update(song='song_update')
1
```

### delete
```shell
>>> data = Music.objects.filter(id=2)
>>> data.delete()
(1, {'musics.Music': 1})
```

# 管理台
```python
INSTALLED_APPS = [
    'django.contrib.admin',
    # ...
]
```

## 创建管理员
```
python manage.py createsuperuser
```

```shell
(.venv) > $ python manage.py createsuperuse
Username (leave blank to use 'qinjh'): admin
Email address:******
Password: 
Password (again): 
This password is too short. It must contain at least 8 characters.
This password is too common.
Password: 
Password (again): 
This password is too common.
This password is entirely numeric.
Password: 
Password (again): 
Superuser created successfully.
```

## 注册models
*musics/admin.py*:
```python
from django.contrib import admin

from musics.models import Music

admin.site.register(Music)
```

## Model Field.choices
**Model Field.choices**很实用，具体可以参考https://docs.djangoproject.com/en/2.0/ref/models/fields/#choices
*musics/models.py*:
```python
TYPE_CHOICES = (
    ('T1', 'type 1'),
    ('T2', 'type 2'),
    ('T3', 'type 3'),
    ('T4', 'type 4'),
)

class Music(models.Model):
    ......
    type = models.CharField(
        max_length=2,
        choices=TYPE_CHOICES,
        default="T1"
    )

    class Meta:
        db_table = "music"

    def display_type_name(self):
        return self.get_type_display()
```

*musics/views.py*:
```python
from django.shortcuts import render

from musics.models import Music


def hello_view(request):
    hello = "Nice to meet you!"
    musics = Music.objects.all()
    context = {'data': hello, 'musics': musics}
    return render(request, 'index.html', context)
```

*musics/templates/index.html*:
```python
 {% for music in musics %}
     <p>id : {{ music.id }}</p>
     <p>song : {{ music.song }}</p>
     <p>singer : {{ music.singer }}</p>
     <p>type : {{ music.type }}</p>
     <p>display_type_name : {{ music.display_type_name }}</p>
 {%endfor%}
```

## Integrating Django with a legacy database
如果说我们已经有了一个数据库，想要根据数据库表来生成models，这时候部可能一个一个的来手工转换，Django提供了一个现成的命令：
```
python manage.py inspectdb > models.py
```
这样打开models.py就可以看到结果了。具体可以参考https://docs.djangoproject.com/en/2.0/howto/legacy-databases/#auto-generate-the-models


具体的代码可以参考https://github.com/keer2345/django_tutorial/tree/v_1.0_basic
