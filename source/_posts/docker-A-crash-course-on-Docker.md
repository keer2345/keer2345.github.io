---
title: A crash course on Docker
date: 2018-07-21 21:00:21
tags: [docker]
---


如果您在过去一年中关注软件趋势，Docker 一定是你每隔一段时间就听到厌倦术语。大量开发者谈论容器，隔离虚拟机，你可能会感到不知所措。今天为您打破这一切，最终理解为什么需要容器 (*Container*) 来作为我们的服务 (*Service*) 。

1. 为什么需要Docker
     - 关键术语概述
     - 为什么需要 Caas 和 Docker
1. 快速上手
     - 安装Docker
     - 创建容器
1. 真实场景
     - 创建 Nginx 容器来托管静态站点
     - 学习使用构建工具来自动化Docker命令

<!-- more -->

# 为什么需要Docker
我也不断地问自己这个问题，在成为一名坚定的开发者之后，我终于坐下来接受了使用容器的可怕性。以下是我为什么要尝试一下的看法。

## Docker？
Docker是一个用于创建容器化应用程序的软件。容器的概念是用于运行一个软件的小型无状态环境。

> A container image is a lightweight, stand-alone, executable package of a piece of software that includes everything needed to run it: code, runtime, system tools, system libraries, settings.

> — [Official Docker website](https://www.docker.com/what-container)

## Virtual machine?
虚拟机（VM）就像名字所说的那样。真实机器的虚拟版本。它模拟大型机器内部机器的硬件。这意味着，您可以在一台较大的服务器上运行多个虚拟机。你见过电影 *Inception* 吗？是的，有点像那样。使VM运行的是一个很酷的软件，称为 *Hypervisor* 。

## Hypervisor?
虚拟机仅在Hypervisor下工作。它是一种特殊的软件，可以使物理机器托管多个不同的虚拟机。所有这些VM都可以运行自己的程序，并且似乎正在使用主机的硬件。但是，实际上是Hypervisor正在为VM分配资源。

注意：如果您曾经尝试过安装VirtualBox等软件，只是为了让它失败，很可能是因为没有在计算机的BIOS中启用Hyper-V。这可能发生在我身上的时间比我记忆中的多。*紧张的笑*

这里有一篇介绍 Hypervisor 的文章：[**Virtualization 101: What is a Hypervisor?**](https://www.pluralsight.com/blog/it-ops/what-is-hypervisor "https://www.pluralsight.com/blog/it-ops/what-is-hypervisor")

## 回答我的问题……
为什么我们需要 CaaS? 我们一直在使用虚拟机这么长时间，容器怎么会突然变得如此好？好吧，没有人说虚拟机很糟糕，他们很难管理。

DevOps通常很难，你需要一个专门的人来做这项工作。虚拟机占用大量存储空间和RAM，并且可以及时设置。更不用说你需要公平的经验来以正确的方式管理它们。

## 防止做第二次，将它自动化
使用Docker，您可以抽象出所有及时的配置和环境设置，而是专注于编码。使用[Docker Hub](https://hub.docker.com/)，您可以获取预先构建的映像，并在常规VM所需的时间内启动和运行。

然而，最大的优势是创造一个同质化的环境。现在你只需要安装一个软件——Docker，而不必安装不同的依赖项列表来运行你的应用程序。由于它是跨平台的，因此团队中的每个开发人员都将在完全相同的环境中工作。这同样适用于您的开发，登台和生产服务器。现在，这很酷。不再“它在我的机器上运行。”

# 快速上手
让我们通过安装来解决问题。很棒的是你可以只在开发机器上安装一个软件，并且仍然确保一切都能正常工作。从字面上看，Docker就是你所需要的。
## 安装Docker
幸运的是，安装过程非常简单。让我告诉你如何在[Ubuntu](https://docs.docker.com/engine/installation/linux/docker-ce/ubuntu/)上做到这一点。
```
$ sudo apt-get update 
$ sudo apt-get install -y docker.io
```
这就是你所需要的一切。为了确保它正在运行，您可以运行另一个命令。
```
$ sudo systemctl status docker
```
如果系统服务已停止，您可以运行两个命令的组合来将其执行，并确保它在开机时启动。
```
$ sudo systemctl start docker && sudo systemctl enable docker
```
就是这样，你准备好了。

在Docker的基本安装中，您需要运行`docker`命令`sudo`。但是，您可以将您的用户添加到`docker group`，并且您将能够在没有的情况下运行该命令`sudo`。
```
$ sudo usermod -aG docker $ {USER} 
$ su  -  $ {USER}
```

## 玩转容器Container
Docker安装和运行后，接着了解以下四个命令：
- `create` 从镜像创建容器
-  `ps`   列出正在运行的容器，`-a` 列出所有容器
- `start` 启动已创建的容器
- `attch` 将终端的标准输入和输出连接到正在运行的容器，就像您对任何虚拟机一样将您连接到容器。

让我们从小做起。我们将从Docker Hub获取一个Ubuntu映像并从中创建一个容器。
```
$ docker create -it ubuntu:16.04 bash
```
我们正在添加 `-it` 一个选项，为容器提供一个集成终端，因此我们可以连接到它，同时告诉它运行bash命令，这样我们就可以得到一个合适的终端接口。通过指定 `ubuntu:16.04` 我们从 `Docker Hub` 中提取版本标签为 16.04 的 Ubuntu 映像。

运行create命令后，继续验证容器是否已创建。
```
$ docker ps -a
```
看起来像这样：
```
CONTAINER ID  IMAGE        COMMAND  CREATED    STATUS   PORTS  NAMES
7643dba89904  ubuntu:16.04 "bash"   X min ago  Created         name
```
很棒，容器已创建并准备启动。运行容器就像给 `start` 命令输入容器的ID一样简单。
```
$ docker start 7643dba89904
```
再次检查容器是否正在运行，但现在没有`-a`标志。
```
$ docker ps
```
如果是，请继续并附加到它。
```
$ docker attach 7643dba89904
```
你看到了吗？光标变化。为什么？因为你刚进入容器。多么酷啊。您现在可以在Ubuntu中运行您习惯使用的任何`bash`命令，就好像它是在云中运行的实例一样。来吧，试试吧。  
```
$ ls
```
它会工作得很好，并列出所有目录。哎呀，甚至$ ll会工作。这个简单的小Docker容器就是您所需要的。这是你自己的小虚拟游乐场，你可以在那里进行开发，测试或任何你想要的！无需使用虚拟机或重型软件。为了证明我的观点，请继续在这个小容器中安装你喜欢的任何东西。前进。安装Node将正常工作，成为我的客人并尝试一下。或者，如果要退出容器，您只需要输入即可`exit`。容器将停止，您可以通过键入再次列出它`$ docker ps -a` 。

# 真实场景
是时候来一些实战了，将用于今后的项目和生产应用中。

## 无状态容器？
是指每个容器都是孤立的，无状态的。意味着一旦删除容器，内容将被永久删除。
```
$ docker rm 7643dba89904
```
好吧，问题来了，这种情况下该如何持久化数据呢？

您听说过卷（Volumes）吗？Volumes 让我们映射宿主机的目录到容器的内部目录，就是这样：
```
$ docker create -it -v $(pwd):/var/www ubuntu:latest bash
```
容器创建时，添加 `-v` 来制定卷以创建持久化，上面的命令将绑定当前目录到容器的内部目录 `/var/www` 。

> 其实，我们可以通过 `run` 命令来同时运行 `create` 和 `start` 。

```
$ docker run -it -d ubuntu:16.04 bash
```

## 为什么如此重视卷？
让我花点时间来告诉你为什么？我们通过以下几个步骤创建一个简单的 Nginx 服务器来托管静态站点。

创建一个目录，命名为你喜欢的名称，为了方便起见，我命名为 `myapp` 。在目录下创建 `index.html` ：
```html
<html>
  <head>
    <link 
 href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" 
 rel="stylesheet" 
 integrity="sha256-MfvZlkHCEqatNoGiOXveE8FIwMzZg4W85qfrfIFBfYc= sha512-dTfge/zgoMYpP7QbHy4gWMEGsbsdZeCXz7irItjcC3sPUFtf0kuFbDz/ixG7ArTxmDjLXDmezHubeNikyKGVyQ==" 
 crossorigin="anonymous">
    <title>Docker Quick Start</title>
  </head>
  <body>
    <div class="container">
      <h1>Hello Docker</h1>
      <p>This means the nginx server is working.</p>
    </div>
  </body>
</html>
```
```
$ docker run --name webserver -v $(pwd):/usr/share/nginx/html -d -p 8080:80 nginx
```
在这里你可以看到我们从Docker Hub抓取一个nginx图像，这样我们就可以获得nginx的即时配置。卷配置类似于我们上面所做的，我们只指向nginx托管HTML文件的默认目录。`--name` 我们设置 `webserver` 的 `-p 8080:80` 选项和选项有什么新功能。我们将容器的端口 `80` 映射到主机上的端口 `8080` 。当然，不要忘记在 *myapp* 目录中运行该命令。

就这么简单。我们只需要几个命令即可启动并运行nginx Web服务器。在index.html中随意编辑一些内容。重新加载页面，您将看到内容已更改。该死的，我多么喜欢Docker。

## 如何让生活变得更轻松
有一个说法，如果我需要做两次，我宁愿自动化它。幸运的是，Docker让我受到了保护。在 `index.html` 文件旁边添加一个 `Dockerfile` 。它的名字实际上只是Dockerfile，没有任何扩展名。
```
#Dockerfile
FROM nginx 
VOLUME /usr/share/nginx/html 
EXPOSE 80
```
使用 `build` 构建镜像：
```
$ docker build . -t webserver:v1
```
该 `.` 被指定其中Dockerfile位于将被用于构建镜像，而 `-t` 标记该图像的标记。这个镜像将被称为 `webserver:v1` 。
```
docker run -v $(pwd):/usr/share/nginx/html -d -p 80:80 webserver
```

Dockerfile的强大之处在于您可以为容器提供自定义。您可以根据自己的喜好预先构建图像。但是，如果你真的不喜欢重复的任务，你可以更进一步，安装[**docker-compose**](https://docs.docker.com/compose/)。

## Docker-compose?
它将让你在一个命令中构建和运行容器。但是，更重要的是，您可以构建整个容器集群并使用docker-compose配置它们。

在Dockerfile旁边添加另一个名为 `docker-compose.yml` 的文件并粘贴此代码段：
```
version: '3'

services:
    webserver:
       container_name: webserver
       build:
           context: .
           dockerfile: Dockerfile
       ports:
           - "80:80"
       volumes:
           - .:/usr/share/nginx/html
```
第一次运行需要构建：
```
docker-compose up --build -d
```
以后的启动和停止只需要：
```
docker-compose up -d
docker-compose stop
```
停止和移除容器：
```
docker-compose down
```
如果您想要删除容器，可以运行 `rm` 我上面提到的命令，否则使用该 `rmi` 命令删除镜像。
```
docker rmi <image_id>
```
尽量不要留下残留的容器，如果不需要，请务必将其删除。

## 扩展
为了确保Docker并非唯一的容器技术，我们留意到一个小工具，Docker仅仅是当今容器化的其中一个选择。但是， [rkt](https://github.com/rkt/rkt) 似乎也做得很好。

深入挖掘，我不得不提到容器编排。我们只讨论过冰山一角。*Docker-compose* 是一种用于创建容器网络的工具。但是，管理所有这些并确保最大化时间是编排发挥作用的地方。

这根本不是一项微不足道的任务。随着容器数量的增长，我们需要一种自动化我们通常所做的各种 DevOps 任务的方法。Orchestration 可以帮助我们完成配置主机，在需要扩展或缩小时创建或删除容器，重新创建失败的容器，网络容器等等。所有大枪都使用谷歌的 [Kubernetes](https://kubernetes.io/) 或 Docker 自己的 [Swarm模式](https://docs.docker.com/engine/swarm/swarm-tutorial/) 解决方案。

# 打包
哇，这是很多东西......  
如果我没有让你相信使用 CaaS 的巨大好处和 Docker 的简单性，我建议你重新考虑并将你现有的一个应用程序包装在 Docker 容器中！

> Docker容器实际上只是一个小型VM，您可以在其中执行任何操作，从开发，登台，测试到托管生产应用程序。

Docker的同质性对于生产环境来说就像魔术一样。它将减轻部署应用程序和管理服务器的压力。因为现在你肯定知道本地的任何工作都可以在云端工作。这就是我所说的安心。不再听到臭名昭着的句子我们都听过太多次了。




----
> https://blog.sourcerer.io/a-crash-course-on-docker-learn-to-swim-with-the-big-fish-6ff25e8958b0
