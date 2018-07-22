---
title: docker How Docker Can Help You Become A More Effective Data Scientist
date: 2018-07-22 23:12:45
tags: [docker,analysis]
---




<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/docker/2018072203.png)
</center>

过去 5 年，我听说了很多的 Docker 容器。好像很多的软件工程的朋友都是用它来开发应用，我想要弄清楚它是如何使我们更富有效率，但是我发现一些在线教程要么说明不会用于数据科学，要么没有给我足够的信息来帮助我理解如何有效快速使用 Docker 。

我写了这本快速入门书，因此您无需解析所有信息，而是可以学习快速入门所需的知识。

<!-- more -->

# 什么是 Docker
您可以将Docker视为轻量级虚拟机 - 它包含运行应用程序所需的一切。docker容器可以捕获系统状态的快照，以便其他人可以快速重新创建您的计算环境。这就是本教程所需要知道的全部内容，但有关更多详细信息，请[访问此处](https://medium.freecodecamp.org/a-beginner-friendly-introduction-to-containers-vms-and-docker-79a9e3e119b)。

## 为什么需要使用Docker
1. **再生性** 作为一个专业的数据科学家，工作的再生性非常重要。再生性不仅有助于同行评审，还可确保您构建的模型，应用程序或分析能够在没有摩擦的情况下运行，从而使您的可交付成果更加强大并经得起时间的考验。例如，如果你在 python 中构建了一个模型，那么通常仅仅运行 `pip  freeze` 并将生成的 `requirements.txt` 文件发送给你的同事是不够的，因为它只会封装特定于 python 的依赖项 - 而通常存在于外部的依赖项python，例如操作系统，编译器，驱动程序，配置文件或代码成功运行所需的其他数据。即使你只是分享 python 依赖关系。
1. **计算环境的可移植性** 作为数据科学家，特别是在机器学习中，能够快速改变您的计算环境会极大地影响您的工作效率。数据科学工作通常从原型设计，探索和研究开始 - 这些工作不一定需要专门的计算资源。这项工作经常发生在笔记本电脑或个人电脑上。但是，经常出现这样的情况：不同的计算资源会大大加快您的工作流程 - 例如，具有更多CPU的机器或更强大的GPU用于深度学习等。我看到许多数据科学家将自己局限于他们的本地计算环境，因为他们认为在远程机器上重新创建本地环境会产生摩擦。Docker使得移植环境（所有库，文件等）的过程变得非常简单。快速移植您的计算环境也是Kaggle竞赛中的巨大竞争优势，因为您可以以经济高效的方式利用AWS上宝贵的计算资源。最后，创建一个docker文件允许您移植许多您喜欢的本地环境 - 例如bash别名或vim插件。
1. **加强您的工程设计** 熟悉Docker可以让您将模型或分析部署为应用程序（例如，作为可以提供预测的 REST API 后台），使你的工作可以供其他人访问。此外，作为数据科学工作流程的一部分，您可能需要与其进行交互的其他应用程序可能存在于Docker容器（如数据库或其他应用程序）中。

# Docker术语
在我们深入研究之前，熟悉Docker术语是有帮助的：
- **镜像 Image**：是您想要构建的蓝图。例如：带有Nvidia驱动程序的Ubuntu + TensorFlow和正在运行的Jupyter Server。
- **容器 Container**：是您实现的镜像实例化。您可以运行同一镜像的多个副本。掌握镜像和容器之间的区别非常重要，因为这是新来者混淆的常见原因。如果镜像和容器之间的差异不明确，请停止并再次阅读。
- **Dockerfile**：用于创建镜像的配方。Dockerfiles包含特殊的Docker语法。从官方文档：Dockerfile是一个文本文档，其中包含用户可以在命令行上调用以组合图像的所有命令。
- **Commit**：像git一样，Docker容器提供版本控制。您可以通过提交更改随时将docker容器的状态保存为**新映像**。
- **DockerHub / Image Registry**：人们可以发布公共（或私人）镜像的位置，以促进协作和共享。
- **Layer**：对现有镜像的修改，由Dockerfile中的指令表示。图层按顺序应用于基本镜像以创建最终镜像。

我会在帖子的其余部分使用这个术语，所以如果你迷路了，请回到这个列表！这些术语很容易混淆，特别是在图像和容器之间 - 所以在阅读时要保持警惕！

# 安装Docker

您可以免费下载和安装Docker Community Edition。您可以按照[此处](https://www.docker.com/community-edition#/download)的说明操作。

## 创建您的第一个Docker镜像

在创建docker容器之前，创建一个定义图像的Dockerfile非常有用。让我们慢慢浏览下面的Dockerfile。*您可以*[*在随附的Github仓库中*](https://github.com/hamelsmu/Docker_Tutorial/blob/master/basic_tutorial/Dockerfile)*找到此文件**。

`Dockerfile`
```
# reference: https://hub.docker.com/_/ubuntu/
FROM ubuntu:16.04

# Adds metadata to the image as a key value pair example LABEL version="1.0"
LABEL maintainer="Hamel Husain <youremail@gmail.com>"

##Set environment variables
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN apt-get update --fix-missing && apt-get install -y wget bzip2 ca-certificates \
    build-essential \
    byobu \
    curl \
    git-core \
    htop \
    pkg-config \
    python3-dev \
    python-pip \
    python-setuptools \
    python-virtualenv \
    unzip \
    && \
apt-get clean && \
rm -rf /var/lib/apt/lists/*

RUN echo 'export PATH=/opt/conda/bin:$PATH' > /etc/profile.d/conda.sh && \
    wget --quiet https://repo.continuum.io/archive/Anaconda3-5.0.0.1-Linux-x86_64.sh -O ~/anaconda.sh && \
    /bin/bash ~/anaconda.sh -b -p /opt/conda && \
    rm ~/anaconda.sh

ENV PATH /opt/conda/bin:$PATH

RUN pip --no-cache-dir install --upgrade \
        altair \
        sklearn-pandas

# Open Ports for Jupyter
EXPOSE 7745

#Setup File System
RUN mkdir ds
ENV HOME=/ds
ENV SHELL=/bin/bash
VOLUME /ds
WORKDIR /ds
ADD run_jupyter.sh /ds/run_jupyter.sh
RUN chmod +x /ds/run_jupyter.sh

# Run a shell script
CMD  ["./run_jupyter.sh"]
```
## FROM
在**FROM**语句封装Docker的最神奇的部分。此语句指定要在其上构建的基本映像。在使用**FROM**指定基本映像后， Docker将在您的本地环境中查找名为 **ubuntu：16.04** 的映像 ——如果它无法在本地找到它，它将搜索您指定的Docker Registry，默认情况下是[DockerHub](https://hub.docker.com/explore/)。这种分层机制很方便，因为您经常希望在诸如Ubuntu之类的操作系统之上安装程序。而不是担心如何从头开始安装Ubuntu，您可以简单地构建在官方Ubuntu映像之上！Dockerhub上托管了各种各样的Docker镜像，包括那些提供多个操作系统的镜像，例如，如果你想要一个已经安装了Anaconda的容器，你可以在[官方的anaconda docker镜像](https://hub.docker.com/r/continuumio/anaconda3/)之上构建一个容器。最重要的是，您也可以随时发布您构建的图像，即使该图像是通过在另一个图像上分层创建的！可能性是无止境。


## LABEL
此语句将元数据添加到您的图像，并且完全是可选的。我添加这个，以便其他人知道谁与图像联系，所以我可以搜索我的docker容器，特别是当它们中的许多同时在服务器上运行时。

## ENV 
```
ENV LANG = C.UTF-8 LC_ALL = C.UTF-8
```
这允许您更改环境变量，并且非常简单。你可以[在这里](https://docs.docker.com/engine/reference/builder/#environment-replacement)阅读更多相关信息。

## RUN
这通常是在构建Docker镜像时完成您想要的工作的主力。您可以运行 *apt-get* 和 *pip install* 之类的任意 *shell* 命令来安装所需的包和依赖项。
```
RUN apt-get update --fix-missing && apt-get install -y wget bzip2    
    build-essential \
    ca-certificates \
    git-core \  
...
```
RUN语句之后的命令与Docker无关，但是如果您自己安装这些软件包，则会运行正常的linux命令，因此如果您不熟悉某些软件包或linux命令，请不要担心。另外，作为另一条建议 - 当我第一次开始学习docker时，我查看了Github或DockerHub上的其他Dockerfiles，并在Dockerfile中复制并粘贴了我想要的相关部分。

关于 *RUN* 语句，您可能注意到的一件事是格式化。每个库或包都整齐地缩进并按字母顺序排列以便于阅读。这是Dockerfiles的一个普遍惯例，所以我建议您采用它，因为它将简化协作。

## EXPOSE
如果您尝试公开端口，则此语句很有用 - 例如，如果您从容器内部或某种Web服务中提供jupyter笔记本。Docker的文档非常适合解释EXPOSE语句：

> 该  `EXPOSE` 指令实际上没有发布端口。它作为构建映像的人和运行容器的人之间的一种文档，用于发布要发布的端口。要在运行容器时实际发布端口，请使用 `-p` 标志 `docker run` 来发布和映射一个或多个端口，或使用 `-P` 标志发布所有公开的端口并将它们映射到高阶端口。

## VOLUME
```
VOLUME /ds
```
此语句允许您在docker容器和主机之间共享数据。VOLUME语句允许您安装外部安装的卷。只有在运行容器时才会声明主机目录（因为您可能在不同的计算机上运行此容器），而不是在定义映像时*。目前，您只需在docker容器中指定要与主机容器共享的文件夹的名称。
> 主机目录在容器运行时声明：主机目录（mountpoint）本质上是依赖于主机的。这是为了保持图像的可移植性。因为无法保证给定的主机目录在所有主机上都可用。因此，您无法从Dockerfile中安装主机目录。该VOLUME指令不支持指定host-dir参数。您必须在创建或运行容器时指定安装点。

此外，这些卷旨在将数据保留在容器的文件系统之外，如果您处理大量不希望使用docker镜像的数据，这通常很有用。保存Docker映像时，此VOLUME目录中的任何数据都不会保存为映像的一部分，但是将保存容器中此目录之外的数据。

##　WORKDIR
```
WORKDIR /ds
```
此语句设置工作目录，以便在另一个命令中引用没有绝对路径的特定文件。例如，Dockerfile中的最后一个语句是
```
CMD [“./run_jupyter.sh”]
```
假设工作目录为 `/ds`

## ADD
```
ADD run_jupyter.sh /ds/run_jupyter.sh
```
此命令允许您在运行docker容器时将文件从主机复制到docker容器中。我用它来执行bash脚本并将有用的东西导入到容器中，例如 `.bashrc` 文件。

请注意这里未完全指定主机容器的路径，因为主机路径是相对于运行容器时指定的上下文目录（稍后讨论）。

当我运行这个容器时，我将在上下文目录的根目录中运行 `run_jupyter.sh` 文件，这就是为什么源文件前面没有路径的原因。

## CMD
Docker容器的设计理念是它们是短暂的，并且只能保持足够长的时间来完成运行您想要运行的应用程序。但是，对于数据科学，我们经常希望这些容器保持运行，即使它们没有正在运行。许多人通过简单地运行bash shell（除非你杀掉它之后不会终止）来实现这一目标的一种方式。
```
CMD [“./run_jupyter.sh”]
```
在上面的命令中，我正在运行一个实例化Jupyter笔记本服务器的shell脚本。但是，如果您没有要运行的任何特定应用程序但希望容器在不退出的情况下运行 - 您只需使用以下命令运行bash shell：
```
CMD ["/bin/bash"]
```

# 构建Docker镜像
这是关于Dockerfiles的大量信息。别担心，其他一切从这里开始都相当简单。现在我们已经以DockerFile的形式创建了我们的配方，是时候构建一个图像了。您可以通过以下命令完成此操作：
![image.png](https://upload-images.jianshu.io/upload_images/1702157-0ddca36f87e0620c.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

这将构建一个docker镜像（不是容器，如果你不记得差异是什么，请阅读本文开头的术语！），然后你可以在以后运行它。

# 从Docker镜像创建并运行容器
现在你已准备好将所有这些魔力付诸实践。我们可以通过执行以下命令来启动此环境：

![image.png](https://upload-images.jianshu.io/upload_images/1702157-a78c9a90637ca8f9.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

运行之后，您的容器将启动并运行！因为这个，jupyter服务器将运行
```
CMD [“./run_jupyter.sh”]
```
命令在Dockerfile的末尾。现在你应该可以在它所服务的端口上访问你的jupyter笔记本了 - 在这个例子中，它应该可以从[http://localhost:7745/](http://localhost:7654/)访问密码教程。如果远程运行此docker容器，则必须设置 [local端口转发](https://help.ubuntu.com/community/SSH/OpenSSH/PortForwarding)以便可以从浏览器访问jupyter服务器。

# 与您的容器交互
一旦容器启动并运行，这些命令就会派上用场：
## 将新的终端会话附加到容器
如果您需要安装某些软件或使用shell，这将非常有用。
![image.png](https://upload-images.jianshu.io/upload_images/1702157-4b68358576bb89c0.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)
## 将容器的状态保存为新图像
即使您开始使用包含所有要安装的库的Dockerfile，但随着时间的推移，您可以通过交互式添加更多库和包来显着更改容器的状态。将容器的状态保存为稍后可以共享或分层的图像非常有用。您可以使用docker commit CLI命令完成此操作：
```
docker commit <container_name> new_image_name:tag_name(optional)
```
例如，如果我想将名为container1的容器的状态保存为名为hamelsmu / tutorial:v2的图像，我只需运行此命令：
```
docker commit container_1 hamelsmu/tutorial:v2
```
您可能想知道为什么*hamelsmu /*位于图像名称前面 - 这使得以后[更容易将此容器推送到DockerHub](http://n%20order%20to%20push%20a%20repository%20to%20the%20docker%20hub%2C%20you%20need%20to%20name%20your%20local%20image%20using%20your%20docker%20hub%20username%2C%20and%20the%20repository%20name%20that%20you%20created/)，因为hamelsmu是我的DockerHub用户名（稍后会详细介绍）。如果您在工作中使用Docker，可能有一个内部私有Docker仓库，您可以将Docker镜像推送到其中。
## 列表运行容器
当我忘记当前正在运行的容器的名称时，我经常使用它
```
docker ps -a -f status=running
```
如果在没有status = running标志的情况下运行上述命令，那么您将在系统上看到所有容器的列表（即使它们不再运行）。这对于跟踪旧容器非常有用。

## 将镜像推送到DockerHub（或其他注册表）
如果您想与他人分享您的工作，或者方便地将镜像保存在云中，这将非常有用。请注意，在执行此操作时不要共享任何私人信息（DockerHub上也有私有存储库）。

首先创建一个DockerHub存储库并相应地命名您的图像，[如此处所述](https://docs.docker.com/docker-hub/repos/)。这将涉及运行命令docker **login**以首先连接到DockerHub或其他注册表上的帐户。例如，要将图像推送[到此容器](https://hub.docker.com/r/hamelsmu/tutorial/)，我首先必须将我的本地图像命名为hamelsmu / tutorial（我可以选择任何标记名称）例如，CLI命令：
```
docker push hamelsmu/tutorial:v2
```
使用标记**v2**将上述docker镜像推送[到此存储库](https://hub.docker.com/r/hamelsmu/tutorial/tags/)。应该注意的是，如果您将镜像公之于众，**其他人可以简单地在镜像上进行**分层，就像我们在本教程中向**ubuntu**图像添加图层一样。这对于寻求复制或扩展您的研究的其他人非常有用。

# 现在你拥有了超能力
现在您已了解如何操作Docker，您可以执行以下任务：

*   与同事和朋友分享可重复的研究。
*   通过根据需要临时将代码移动到更大的计算环境，赢得Kaggle比赛而不会破坏。
*   在笔记本电脑上的docker容器内本地原型，然后将相同的计算无缝地移动到服务器上，而不会让您流连忘返，同时随身携带许多您喜爱的本地环境（您的别名，vim插件，bash脚本，自定义）提示等）。
*   使用[Nvidia-Docker](https://github.com/NVIDIA/nvidia-docker)快速实例化在GPU计算机上运行Tensorflow，Pytorch或其他深度学习库所需的所有依赖项（如果从头开始这样做可能会很痛苦）。有关详细信息，请参阅下面的奖励部分。
* 将模型发布为应用程序，例如作为从docker容器提供预测的rest api。当您的应用程序是Dockerized时，可以根据需要轻松复制它。

# 进一步阅读
我们只是抓住了Docker的表面，你可以做更多的事情。我专注于Docker的领域，我认为你最常遇到的是数据科学家，并希望你有足够的信心开始使用它。以下是帮助我完成Docker之旅的一些资源：
*   [有用的Docker命令](https://zaiste.net/posts/removing_docker_containers/)
*   [更有用的Docker命令](https://www.digitalocean.com/community/tutorials/how-to-remove-docker-images-containers-and-volumes)
*   [Dockerfile引用](https://docs.docker.com/engine/reference/builder/#run)
*   [如何在DockerHub上创建和推送到存储库](https://docs.docker.com/docker-hub/repos/#viewing-repository-tags)

# 额外奖励：Nvidia-Docker
我首先学习Docker的最初动机是在单个GPU上对深度学习模型进行原型设计，并在我需要更多马力后将计算移动到AWS。我还参加了[Jeremy Howard](http://www.fast.ai/)的优秀课程[Fast.AI，](http://www.fast.ai/)并希望与其他人分享原型。

但是，要正确封装所有依赖项（如Nvidia GPU的驱动程序），您需要使用[Nvidia-Docker](https://github.com/NVIDIA/nvidia-docker)而不是Docker。这需要比使用vanilla Docker更多的工作，但是一旦你理解了Docker就可以直接进行。

我已经将我的Nvidia-Docker设置放在这个[Github](https://github.com/hamelsmu/Docker_Tutorial/tree/master/gpu_tutorial)，并将此作为练习给读者留下。

> https://towardsdatascience.com/how-docker-can-help-you-become-a-more-effective-data-scientist-7fc048ef91d5
