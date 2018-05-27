---
title: Django Tutorial 02 -- Restful
date: 2018-05-26 19:16:13
categories: python
tags: [django, restful]
---

> [Django REST framework](http://www.django-rest-framework.org/)

# 准备

## 环境

```shell
pip install djangorestframework
```

```shell
(.venv) > $ pip freeze
Django==2.0.5
djangorestframework==3.8.2
pytz==2018.4
```
<!-- more -->
## 安装
在*setting.py*中的**INSTALLED_APPS**中加入`rest_framework`设置：
```python
INSTALLED_APPS = (
    # ...
    'rest_framework',
    # ...
)
```

# Serializers 序列化
*Serializers*序列化是Django Restful framework很重要的一個地方，主要功能是将Python结构序列化为其他格式，例如我们常用的JSON。

在[上一节](http://keer2345.github.io/2018/05/26/python-django-tutorial-01/)中，我们建立了*musics*模型，这里我们来将其进行序列化。

在musics中新增*serializers.py*：
```python
from rest_framework import serializers

from musics.models import Music


class ToUpperCaseCharField(serializers.CharField):
    def to_representation(self, value):
        return value.upper()


class MusicSerializers(serializers.ModelSerializer):
    days_since_created = serializers.SerializerMethodField()
    singer = ToUpperCaseCharField()

    class Mate:
        model = Music
        # fields = '__all__'
        fields = ('id', 'song', 'singer', 'last_modify_date',
                  'created', 'days_since_created')

    def get_days_since_created(self, obj):
        return (now() - obj.created).days
```

## serializers.SerializerMethodField()
http://www.django-rest-framework.org/api-guide/fields/#serializermethodfield

## 自定义序列化
http://www.django-rest-framework.org/api-guide/relations/#custom-relational-fields

# Views
在[上一节](http://keer2345.github.io/2018/05/26/python-django-tutorial-01/)中，我们使用views，而在Django Restful framework中，我们使用viewsets，具体如下：

*musics/views.py*:
```python
# from django.shortcuts import render
from rest_framework import viewsets

from musics.models import Music
from musics.serializers import MusicSerializer

# def hello_view(request):
#     hello = "Nice to meet you!"
#     musics = Music.objects.all()
#     context = {'data': hello, 'musics': musics}
#     return render(request, 'index.html', context)


class MusicViewSet(viewsets.ModelViewSet):
    queryset = Music.objects.all()
    serializer_class = MusicSerializer
```

只需要这样，就拥有了CRUD的全部功能，是不是很神奇呢～～

之所以这样，是因为Django Restful framework的`viewsets.ModelViewSet`帮我们定义好了这些功能，具体可以参考http://www.django-rest-framework.org/api-guide/viewsets/#modelviewset

## 配置路由
*urls.py*:
```python
from django.contrib import admin
from django.urls import include, path
from rest_framework import routers

from musics.views import MusicViewSet

router = routers.DefaultRouter()
router.register(r'music', MusicViewSet)

urlpatterns = [
    # path('musics/', include('musics.urls')),
    path('admin/', admin.site.urls),
    path('api/', include(router.urls)),
]
```

然后访问http://127.0.0.1:8000/api/，就可以看到结果了。类似这样：
```html
HTTP 200 OK
Allow: GET, HEAD, OPTIONS
Content-Type: application/json
Vary: Accept

{
    "music": "http://127.0.0.1:8000/api/music/"
}
```

# 测试API
## 什么是Restful API
- `GET`获取资源
- `PUT`更新资源
- `DELETE`删除资源
- `POST`新增资源
- `PATCH`更新资源的部分内容

## 测试工具
测试API的工具很多，这里我们选择[Postman](https://www.getpostman.com/)，使用方法很简单，在Chrome中安装Postman应用。

输入http://127.0.0.1:8000/api/music/，点击*send*就可以看到返回结果：
```
[
    {
        "id": 1,
        "song": "song_update",
        "singer": "SHE148",
        "last_modify_date": "2018-02-26T07:05:56.910436Z",
        "created": "2018-01-26T07:05:56.910501Z",
        "days_since_created": 120
    }
]
```

## POST
添加数据也很方便，输入地址http://127.0.0.1:8000/api/music/，选择`POST`类型，然后在*body*的地方填入`song`和`singer`的值，点击*send*，就是提交了。

接着会返回响应刚才我们添加的数据。


## GET
在Postman中选择`GET`，然后输入http://127.0.0.1:8000/api/music/就能看到返回的列表结果。

## PUT
## DELETE

# Performing raw SQL queries
参考https://docs.djangoproject.com/en/2.0/topics/db/sql/#performing-raw-queries

## Performing raw queries
*models.py*:
```python
# ...

def fun_raw_sql_query(**kwargs):
    song = kwargs.get('song')
    if song:
        result = Music.objects.raw(
            'SELECT * FROM musics_music WHERE song = %s', [song])
    else:
        result = Music.objects.raw('SELECT * FROM musics_music')
    return result
```

*views.py*:
```python
from rest_framework import status, viewsets
from rest_framework.decorators import list_route
from rest_framework.response import Response

from musics.models import Music, fun_raw_sql_query
from musics.serializers import MusicSerializer


class MusicViewSet(viewsets.ModelViewSet):
    queryset = Music.objects.all()
    serializer_class = MusicSerializer

    # /api/music/raw_sql_query/
    @list_route(methods=['get'])
    def raw_sql_query(self, request):
        song = request.query_params.get('song', None)
        music = fun_raw_sql_query(song=song)
        serializer = MusicSerializer(music, many=True)
        return Response(serializer.data, status=status.HTTP_200_OK)
```
更详细的可以参考https://docs.djangoproject.com/en/2.0/topics/db/sql/#performing-raw-queries

## Executing custom SQL directly
使用这个方法，可以绕过models，直接访问数据库。
*models.py*:
```python
def fun_raw_sql_query(**kwargs):
    song = kwargs.get('song')
    if song:
        result = Music.objects.raw(
            'SELECT * FROM musics_music WHERE song = %s', [song])
    else:
        result = Music.objects.raw('SELECT * FROM musics_music')
    return result


def namedtuplefetchall(cursor):
    # Return all rows from a cursor as a namedtuple
    desc = cursor.description
    nt_result = namedtuple('Result', [col[0] for col in desc])
    return [nt_result(*row) for row in cursor.fetchall()]


def fun_sql_cursor_update(**kwargs):
    song = kwargs.get('song')
    pk = kwargs.get('pk')

    '''
    Note that if you want to include literal percent signs in the query,
    you have to double them in the case you are passing parameters:
    '''
    with connection.cursor() as cursor:
        cursor.execute(
            "UPDATE musics_music SET song = %s WHERE id = %s", [song, pk])
        cursor.execute("SELECT * FROM musics_music WHERE id = %s", [pk])
        # result = cursor.fetchone()
        result = namedtuplefetchall(cursor)
    result = [
        {
            'id': r.id,
            'song': r.song,
            'singer': r.singer,
            'last_modify_date': r.last_modify_date,
            'created': r.created,
        }
        for r in result
    ]

    return result
```
*views.py*:
```python
    # /api/music/{pk}/sql_cursor_update/
    @detail_route(methods=['put'])
    def sql_cursor_update(self, request, pk=None):
        song = request.data.get('song', None)
        if song:
            music = fun_sql_cursor_update(song=song, pk=pk)
            return Response(music, status=status.HTTP_200_OK)
```

更多的可以参考https://docs.djangoproject.com/en/2.0/topics/db/sql/#executing-custom-sql-directly

# 授權 (Authentications )

# Test

# Versioning


具体的代码可以参考https://github.com/keer2345/django_tutorial/tree/v_2.0_restful
