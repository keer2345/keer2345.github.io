---
title: Docker for Data Science
date: 2018-07-23 20:19:29
tags: [docker, analysis]
---

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/docker/2018072301.png)
</center>

如果我们关注软件发展趋势，会感到 Docker 就是上帝送给我们的礼物。它的基本理念就是如果能在我的机器上工作，那么在您的机器上也能同样地工作。

<!-- more -->

# What’s in it for Data Scientists?
1. **时间** 节省了安装软件包的时间
1. **可重复研究** 我认为Docker类似于在报告中设置随机数种子。您机器中使用的相同依赖项和库版本将在另一个人的计算机上使用。这可以确保您生成的分析将在任何其他分析器计算机上运行。
1. **分发** 不仅分发代码，还分发代码运行的环境

# How Does it Work?
Docker采用（可重用）层的概念。所以你在里面写的任何一行 `Dockerfile` 被视为一层（layer）。例如，您通常会从以下开始：
```
FROM ubuntu
RUN apt-get install python3
```
这个 Dockerfile 将在 `Ubuntu` 层上安装一个层 `python3` 。

本质上你会为每个项目都写一些 `apt-get install` ，`pip install` 等命令到你的 Dockerfile 中，而不是子本地来执行。

我建议您阅读 [https://docs.docker.com/get-started/](https://docs.docker.com/get-started/) 上的教程以开始使用 Docker 。该**学习曲线是最小的**（2天最多工作），收益也是巨大的。

# Dockerhub
最后，值得一提的是 Dockerhub ，它是Docker得以强大的原因。就像 Github 相对于 Git ，拥有一个开放的 Docker 镜像分享平台。你总是可以使用 `docker build ...` 在本地构建镜像，但 `push` 这个镜像到 Dockerhub 上也很好以至于可以很容易的让其他人 `pull` 下来供个人使用。

[此处](https://hub.docker.com/r/sachinruk/ml_class/) 提供了我的 Docker 图像，用于机器学习和数据科学，以及[源文件](https://github.com/sachinruk/Dockerfiles/blob/master/ML_class/Dockerfile)。


# Concluding Thoughts
就个人而言，我已经开始在大多数 Github Repo 中包含一个Dockerfile。特别是考虑到这意味着我永远不必处理安装问题。

Docker是软件工程师（现在数据科学家/分析师）应该具备的工具之一（与git几乎相同和尊重）。很长一段时间，统计学家和数据科学家忽略了数据分析的软件方面。考虑到使用Docker已经变得如此简单和直观，没有理由不将它作为您的软件开发管道的一部分。
