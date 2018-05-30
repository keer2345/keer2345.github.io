---
title: Docker Curriculum for Beginners
date: 2018-05-30 08:23:52
tags: docker
---

> https://docker-curriculum.com

Github: [prakhar1989/docker-curriculum](https://github.com/prakhar1989/docker-curriculum)

# 先决条件
- [Amazon Web Services](http://aws.amazon.com/)
- [Docker Hub](https://hub.docker.com/)

**Hub国内镜像** [网易云](https://c.163.com/hub#/m/home/)

# 环境
在官网很容易找到在 *MacOS*, *Linux* 以及 *Windows* 的安装方法。

这里记录以下在 *Ubuntu* 安装后任然需要`sudo`的解决办法：
1. 创建docker组：`sudo groupadd docker`
1. 将当前用户加入docker组：`sudo gpasswd -a ${USER} docker`
1. 重启服务：`sudo service docker restart`
1. 刷新docker成员：`newgrp - docker` 或者重启图形界面`pkill X`

测试是否安装成功：
```
$ docker run hello-world

Hello from Docker.
This message shows that your installation appears to be working correctly.
...
```

# 尝试 Busybox
*BusyBox* 是一个集成了三百多个最常用Linux命令和工具的软件。*BusyBox* 包含了一些简单的工具，例如`ls`、`cat`和`echo`等等，还包含了一些更大、更复杂的工具，例`grep`、`find`、`mount`以及`telnet`。有些人将 BusyBox 称为 Linux 工具里的瑞士军刀。简单的说BusyBox就好像是个大工具箱，它集成压缩了 Linux 的许多工具和命令，也包含了 Android 系统的自带的shell。

## 获取镜像
```
$ docker pull busybox

Using default tag: latest
latest: Pulling from library/busybox
07a152489297: Pull complete 
Digest: sha256:141c253bc4c3fd0a201d32dc1f493bcf3fff003b6df416dea4f41046e0f37d47
Status: Downloaded newer image for busybox:latest
```

```
$ docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
busybox             latest              8c811b4aec35        6 days ago          1.15MB
```

## 运行
```
docker run busybox
```
等待，我们什么结果都没有看到，难道出现了bug？并不是。我们允许的`docker run busybox`，并没有提供命令，所以容器（*container*）启动了，运行了空命令并退出。好吧，让我们试试输出一些内容：
```
docker run busybox echo "hello from busybox"
```
```
docker ps
docker ps -a
```


我们可能想在容器中只运行一个命令：
```
$ docker run -it busybox sh
/ # ls
bin   dev   etc   home  proc  root  sys   tmp   usr   var
/ # uptime 
 01:13:57 up 26 min,  0 users,  load average: 0.08, 0.09, 0.08
```

参数`-it`表示我们在容器中使用交互式终端。

现在，只要我们想要，就可以在容器中运行更多的命令，可以花一些时间来运行自己喜欢的命令。

> **注意：** 如果您喜欢冒险，可以在容器中运行`rm -rf bin`，确保这个命令是在容器中运行，而不是在实体机上。这样做将是其他的诸如`ls`，`echo`等命令无法工作。一旦停止，你可以使用命令`exit`退出容器。Docker每次创建新容器，一切都会重新开始。

总结一下`docker run`命令，它将是我们最常用到的命令，得花些时间来适应它。可以通过`docker run --help`来获得更多的帮助。当我们进一步了解后，将看到更多的`docker run`使用形式。

在我们开始下一步之前，先快速的了解一下*删除容器*。我们注意到，在我们退出容器后，使用`docker ps -a`任然看到有残留，之前我们运行了多次`docker run`留下的无用容器会占用我们的磁盘空间。因此，经验之谈，一旦完成fff 后就清理容器。我们可以使用`docker rm`命令，并复制容器的 ID 在命令后面。
```
$ docker ps -a
CONTAINER ID        IMAGE               COMMAND                  CREATED             STATUS                        PORTS               NAMES
c5a178a926b3        busybox             "sh"                     10 minutes ago      Exited (0) 8 minutes ago                          reverent_mahavira
25246e36a8e0        busybox             "sh"                     11 minutes ago      Exited (0) 10 minutes ago                         focused_meninsky
ab8bbfc341dc        busybox             "sh"                     17 minutes ago      Exited (127) 11 minutes ago                       keen_hermann
ba6cdd2b4efe        busybox             "echo 'hello from bu…"   19 minutes ago      Exited (0) 19 minutes ago                         sad_euclid
6db1a9361128        busybox             "sh"                     27 minutes ago      Exited (0) 27 minutes ago                         admiring_shtern

$ docker rm 6db1a9361128 ba ab 25
6db1a9361128
ba
ab
25

$ docker ps -a
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS                     PORTS               NAMES
c5a178a926b3        busybox             "sh"                11 minutes ago      Exited (0) 8 minutes ago                       reverent_mahavira
```
**提示：**
1. 为了方便，我们可以只输入ID的前几位，只要能识别到这是唯一的ID既可。
1. 如果我们有一大堆的容器需要清理，可以运行
```
docker rm $(docker ps -a -q -f status=exited)
```
该命令将删除所有状态为`Exited`的容器。

最后，我们可以使用`docker rmi`删除不再使用的镜像。


## 术语
在本节的最后，我们使用了大量的`docker`专用术语，可能有一些会混淆不清。因此，我们先来理清 Docker 生态系统中的常用术语。
- *Images* 构建容器的基本应用。前面例子中我们使用`docker pull`下载了 **Busybox** 镜像。
- *Contianer* 从 Docker 镜像创建并运行的实际应用，我们使用`docker run`创建 Busybox 镜像的容器，使用`docker ps`列出容器。
- *Docker Damemon* 管理构建、运行和分发 Docker 容器的主机上的后台服务。守护进程是客户端与之通讯的操作系统中运行的进程。
- *Docker Client* 允许用户与守护进程交互的命令行工具。通常也可以有其他形式的客户端，例如为用户提供 GUI 的 [Kitematic](https://kitematic.com/)
- *Docker Hub* Docker 镜像的注册表（[registry](https://hub.docker.com/explore/)）。我们可以将 registry 视为大量 Docker 镜像的目录，如果需要，可以通过 registry 来获取镜像。

# Webapps with Docker

我们已经了解了`docker run`、玩转 Docker 容器以及一些 Docker 术语。掌握这些知识后，我们准备进入实战，例如用 Docker 部署网站应用。

## 静态站点

让我们从头开始，第一件事是我们如何运行一个简单的静态网站，我们准备从 Docker Hub 获取一个 Docker 镜像，运行容器并看一下运行 webserver 是多么的容易。

让我开始吧。我们要使用的镜像是一个按页面网站，我已经建立好了 demo 并注册在主机上——`prakhar1989/static-site`。我们可以用一个命令直接下载并运行这个镜像`docker run`：
```
docker run prakhar1989/static-site
```
由于这个镜像并不存在本地，客户端先从注册表获取镜像然后运行。如果一切顺利，将在终端看到`Nginx is running...`。好了，服务器正在运行了。如何查看网站呢？运行端口是什么呢？更重要的是，如何从我们的机器直接访问容器呢？

这种情况下，客户端并不会公开任何端口，所以我们需要重新运行`docker run`命令来发布端口。我们该找到一种方法让我们的终端不会连接着正在运行的容器。这样，我们就可以愉快的关闭终端而保持容器的运行。这称之为**分离模式（detached）**。
```
$ docker run -d -P --name static-site prakhar1989/static-site
60b528502d3aaff8748746212eaaa045ae7fcafb28133a252f1ba7cff34d0b37
```

上面的命中，`-d`会分离终端，`-P`公开所有暴露的端口到随机端口，`--name`对应我们想要赋予的名称。现在我们使用`docker port [CONTAINER]`命令来查看端口：
```
$ docker port static-site
443/tcp -> 0.0.0.0:32768
80/tcp -> 0.0.0.0:32769
```
现在我们在浏览器打开http://localhost:32769/就可以看到页面了。

> 注意：如果使用的是 docker-toolbox，可能需要使用`docker-machine ip default`来获取 IP 地址。

我们也可以自定义客户端连接指向容器的端口。
```
$ docker run -p 8888:80 prakhar1989/static-site
Nginx is running...
```
<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/docker/2018053001.png)
</center>


运行`docker stop [CONTAINER ID]`可以停止分离容器。

我们相信这些都非常简单。在真实的服务器部署它，我们只需要安装 Docker，以及运行上面这些 Docker 命令。现在，我们已经知道如何在 Docker 镜像中运行网站服务器，您可能想知道——我们是如何创建自己的 Docker 镜像？这是我们将在下一节探讨的问题。

## Docker Images
前面我们看到了镜像，这一节我们将深入了解镜像，并构建我们自己的镜像。最后，我们将使用该镜像在本地运行我们的应用程序并部署到 [AWS](http://aws.amazon.com/) 上，以便与我们的朋友分享！是不是有点儿激动？让我们开始吧。

Docker images 是 Contianer 的基础.之前的例子，我们 **pulled** 到 Busybox 镜像，并要求 Docker 客户端运行**基于（based）**该镜像的程序。

使用`docker images`可以查看本地的一系列镜像。
```
$ docker images
REPOSITORY                      TAG                 IMAGE ID            CREATED             VIRTUAL SIZE
prakhar1989/catnip              latest              c7ffb5626a50        2 hours ago         697.9 MB
prakhar1989/static-site         latest              b270625a1631        21 hours ago        133.9 MB
python                          3-onbuild           cf4002b2c383        5 days ago          688.8 MB
martin/docker-cleanup-volumes   latest              b42990daaca2        7 weeks ago         22.14 MB
ubuntu                          latest              e9ae3c220b23        7 weeks ago         187.9 MB
busybox                         latest              c51f86c28340        9 weeks ago         1.109 MB
hello-world                     latest              0a6ba66e537a        11 weeks ago        960 B
```

上面给出的一系列镜像是从注册表中获取的，以及我自己创建的镜像列表（我们很快就可以看到是如何实现的）。`TAG`是指特定的镜像快照，`IMAGE ID`对应唯一镜像。

为简单起见，我们可以将镜像看成类似 git 仓库，可以通过修改来 [committed](https://docs.docker.com/engine/reference/commandline/commit/) 镜像并拥有多个版本。如果不指定版本号，客户端默认为 `latest`。例如，我们可以获取制定版本的 ubuntu 镜像：
```
$ docker pull ubuntu:12.04
```

我们可以从注册表中（例如 Docker Hub）获取镜像，或者自行创建。[Docker Hub](https://hub.docker.com/explore/)  上有成千上万的镜像，我们也可以直接使用命令`docker search`来查找镜像。

要注意的是，我们要意识到基本镜像（base）和子镜像（child）间的差异。
- **Base iamges** 基本镜像是没有父镜像的，通常是操作系统之类的镜像，例如 ubuntu，busybox 或 debian
- **Child images** 子镜像是基于基本镜像构建并添加了额外的功能

接下来是一些官方镜像和用户镜像，既可以是基本镜像，也可以是子镜像
- **Official images** 提供了官方维护和支持，它们的特点是一个词长，比如 ubuntu, python, busybox, hello-world 都是官方镜像
- **User images** 创建和共享镜像，比如你或者我。它们基于基本镜像创建并附加了额外的功能。通常，它们被格式化成`user/image-name`

## 第一个镜像 Image
现在我们对镜像有了更好的理解，是时候自己创建了。我们本节的目标是创建一个简单的沙盒（sandboxes）[Flask](http://flask.pocoo.org/) 应用的镜像。为了这次演示，我们已经创建了一个小小的 [Flask程序](https://github.com/prakhar1989/docker-curriculum/tree/master/flask-app)，每次`.gif`被加载是随机显示猫——因为我们知道，谁不喜欢猫呢？如果您还没有准备好，请克隆仓库到本地：
```
$ git clone https://github.com/prakhar1989/docker-curriculum
$ cd docker-curriculum/flask-app
```
> 注意：是克隆到您工作的主机上，而不是克隆在 Docker 容器里

下一步是使用这个应用程序创建一个镜像。就像上面所说的，所有的用户镜像都基于基本镜像。我们的应用是用 Python 写的，基本镜像我们就使用 [Python 3](https://hub.docker.com/_/python/) ，更具体地说，我们的镜像使用`python:3-onbuild`版本。

您可能会问，什么是`onbuild`版本呢？
> 这些图像包含多个 ONBUILD 触发器，应该是启动大多数应用程序所需的全部。构建将复制 *requirements.txt* 文件，`pip install`在所述文件上运行，然后复制当前目录 */usr/src/app*。

换句话说，镜像的`onbuild`版本包含帮助程序，这些镜像是为您工作的。我们该怎么做的？答案是使用文件`Dockerfile`:

## Dockerfile
[Dockerfile](https://docs.docker.com/engine/reference/builder/) 是一个简单的文本文件，是包含创建镜像时一系列 Docker 客户端调用的命令。以简单的方式自动化镜像创建，最佳的是在 Dockerfile 中编写的 [commands](https://docs.docker.com/engine/reference/builder/#from) *几乎*等价于 Linux 命令。意味着我们并不需要学习新的语法来创建自己的 Dockerfile 文件。


