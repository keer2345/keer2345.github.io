---
title: Docker 官方文档学习笔记 -- Get started
date: 2018-06-02 06:00:44
tags: docker
---

> https://docs.docker.com/get-started/

# Orientation and setup
## Docker 概念
Docker 是开发人员和系统管理人员使用的通过容器来**开发**、**部署**和**运行**。其实这并不是一个新颖的概念，但对于部署应用来说是相当容易的。

容器化（Containerization，集装箱化）的流行得益于容器是：
- 灵活：再复杂的应用也能够容器化
- 轻量：容器利用并共享主机内核
- 可互换：可以即使（on-the-fly）部署和升级
- 便携：在本地构建，部署到云端并可在任何地方运行
- 可扩展：可以增加和自动分发容器副本
- 可堆叠：可以垂直并即使堆叠服务

### Images and Containers
通过运行中的镜像启动容器。**镜像Image** 是可执行的包，包括运行应用所需的一切——代码、运行时、库、环境以及配置文件。

**容器Container** 是一个镜像的运行实例——当内存中的镜像执行时（也就是说，带有状态、进程的镜像）。在`Linux`中可以通过`docker ps`查看运行中的容器。

### 容器与虚拟机
容器共享主机系统内核，运行进程独立，不占用其他可执行程序的内存，轻量化。

虚拟机（Virtual Machine）是一个独立的操作系统，通常虚拟机所需要的资源要比应用本身大得多。

## 环境准备
### 查看版本
1. 确保是最新版本
```
docker --version
Docker version 18.03.1-ce, build 9ee9f40
```
1. 查看更多的信息
```
docker info
docker version
```

### 测试安装是否成功
1. 运行 [hello-world](https://hub.docker.com/_/hello-world/) 镜像
```
docker run hello-world
## ...
Hello from Docker!
This message shows that your installation appears to be working correctly.
## ...
```
1. 列出镜像
```
docker image ls
```
1. 列出容器（由镜像衍生）
```
docker container ls
docker container ls --all
docker container ls -aq
```

## 小结
```
## List Docker CLI commands
docker
docker container --help

## Display Docker version and info
docker --version
docker version
docker info

## Execute Docker image
docker run hello-world

## List Docker images
docker image ls

## List Docker containers (running, all, all in quiet mode)
docker container ls
docker container ls --all
docker container ls -aq
```

# Containers
是时候以 Docker 的方法构建应用了。我们从应用的底层结构开始（也就是本节要说的容器）。容器的上一层是服务层（第3节），最后是堆栈，定义了所有服务的交互（第5节）。
- Stack
- Services
- **Container**

## 新的开发环境
过去开发一个 Python APP，需要先配置一堆的开发环境。使用 Docker 则只需获取到 Python 的运行镜像，无需安装，这些都由文件`Dockerfile`来定义。

## Dockerfile定义容器
`Dockerfile`定义了容器的环境，访问网络接口和磁盘驱动在环境中是虚拟化的，与真实系统隔离，所以需要映射端口到外部系统，并明确哪些文件需要“复制”到环境中。

创建一个空目录并进入该目录，创建文件`Dockerfile`：
```
# Use an official Python runtime as a parent image
FROM python:2.7-slim

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# Install any needed packages specified in requirements.txt
RUN pip install --trusted-host pypi.python.org -r requirements.txt

# Make port 80 available to the world outside this container
EXPOSE 80

# Define environment variable
ENV NAME World

# Run app.py when the container launches
CMD ["python", "app.py"]
```
