---
title: Debugging a Python Flask Application in a Container with Docker Compose
date: 2018-07-22 20:45:58
tags: [flask, docker]
---



自己编写和调试 Python 应用本身并非难事，只需要启动 [pdb](https://github.com/trstringer/cli-debugging-cheatsheets/blob/master/python.md#python-command-line-debugging-cheatsheet)
 并处于[调试状态](https://github.com/trstringer/cli-debugging-cheatsheets/blob/master/python.md#python-command-line-debugging-cheatsheet)。

但是当你开始在它上面添加层，比如 Flask、Gunicorn、Docker容器某种形式的容器编排时，调试不再是一项微不足道的任务。

# 容器编排
上面提到的“最外层”组件需要一点解释。是的，有时您的Web API和应用程序是独立的，不需要其他外部组件。但通常需要您的Web应用程序与其他服务进行通信（在RDBMS中保留数据，进行其他服务调用，使用缓存层等）。

这个实例应用就是这样做的：这是一个依赖于 Redis 缓存并与之通信的 Flask 项目。

<center>
![example](https://upload-images.jianshu.io/upload_images/1702157-cffa91ae20104204.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)
</center>

任何在多个容器中工作了很长时间的人都知道这不是一件容易的事情，更不用说调试了。

值得注意的是，即使我非常喜欢 *Kubernetes* ，我觉得Docker Compose仍然是本地主机上用于容器编排的最佳工具，用于我的内部循环开发（开发，运行，调试......全部在本地）。

<!-- more -->

# 示例应用
这个例子包括 Flask 应用，docker-compose 配置, 以及一个 Dockerfile 。整个库的源码可以在这里找到： [GitHub](https://github.com/trstringer/python-flask-docker-compose-debugging)。为了简单期间，目录下放置了 app.py，Dockerfile，docker-compose.yml。

`app.py`
```python
from flask import Flask, request, jsonify
import redis

app = Flask(__name__)


@app.route("/")
def default_route():
    """Default route to return a simple message"""
    return jsonify("hello world ..")


@app.route('/message', methods=['GET'])
@app.route('/message/<new_message>', methods=['POST'])
def message_handle(new_message=None):
    """handle the getting and setting of the message"""
    redis_client = redis.StrictRedis(host="redis")

    if request.method == 'GET':
        output = redis_client.get('message')
        # import pdb; pdb.set_trace()
        if output:
            return jsonify(dict(message=output.decode('utf-8')))

        return jsonify(dict(message='no output found for new_message'))

    redis_client.set('message', new_message)
    return jsonify(dict(message='set new_message'))


if __name__ == '__main__':
    app.run('0.0.0.0', 8000, debug=True)
```

接下来，我们用简单的方法将这个 Flask 应用容器化（Containerize）：

`Dockerfile`
```
FROM python
WORKDIR /urs/src/app
EXPOSE 8000

COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

CMD ["gunicorn", "--workers=2", "--bind=0.0.0.0:8000", "app:app"]
```
这里要注意的重要部分是图像 `CMD` 设置为在我的Flask应用程序上调用 `Gunicorn`（一种生产质量的WSGI）。

最后，我们配置 Docker Compose 来处理多个容器，让应用使用 Redis 缓存：

`docker-compose.yml`
```
version: '3'
services:
  svc1:
    build: .
    links:
      - redis
    ports:
      - "8000:8000"
  redis:
    image: redis
```
我们的Docker Compose配置只定义了两个服务：*Flask* 应用程序和 *Redis* 缓存。重要的是要注意我们必须“链接”  `redis` 服务 `svc1`（更多关于下面的链接）。

# 运行（不调试）应用程序
在我们讨论进入调试器之前，让我们先看看这个应用程序运行。在存储库/项目的根目录中，运行以下命令...
```
docker-compose up --build -d
```
```
curl -X POST localhost:8000/message/newmessage
curl localhost:8000/message
```
<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/docker/2018072201.gif)
</center>

# 调试应用
没有调试器的编程就像在没有卷尺的情况下建造房屋。你将需要经常使用它。但是对于所有这些移动部件（Gunicorn，容器，Docker Compose），打入调试器并不是一个简单的操作。

以下是如何在进入Flak应用程序的pdb时中断...
```
docker-compose run -p 8000:8000 svc1 python3 -m pdb app.py
```
注意：如果您更改了代码（例如添加断点等），请确保运行 `docker-compose build` 。

首先我 `docker-compose run` 用来运行单个服务 *svc1* 。请注意我的 docker-compose 配置，我将 *redis* 服务“链接”到 *svc1* 。由于该链接，该容器也将被提起。

另外一个重要的组成部分对于一个Python开发人员来说无疑是很普通的： `python3 -m pdb app.py` 。这就是我们如何使用pdb进行交互式调试（在这种情况下，我想调试我的Flask应用程序）。我们并没有通过 Gunicorn 这个明确的命令（我们不想通过 WSGI ，只想直接调用 Flask 程序）。

我们回到 `App.py` ，我们注释掉了一行 `# import pdb; pdb.set_trace()` ，在运行Docker Compose之前取消注释它会给你一个断点。在对消息路由发出GET请求时，在初始中断输入后继续执行交互式调试的断点。这是体验的样子：

https://cdn-images-1.medium.com/max/800/1*0w-63bkYG-wXCTXtkNsm8Q.gif


<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/docker/2018072202.gif)
</center>

# 总结
在使用微服务和Python Flask编写生产级软件时，必须能够进入调试器。我们采用了Python，Flask，Gunicorn，Docker和Docker Compose这个相当普遍但强大的架构，并进入了交互式调试器pdb。我希望你喜欢这个插图！

> https://medium.com/@trstringer/debugging-a-python-flask-application-in-a-container-with-docker-compose-fa5be981ec9a
