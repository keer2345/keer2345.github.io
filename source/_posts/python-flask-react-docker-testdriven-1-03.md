---
title: Flask React Docker in Testdriven - Part I - 03
date: 2018-09-23 20:47:15
tags: [testdriven, flask, react, docker]
---

# App概述
我们在建什么？

在本课程结束时，您将构建一个代码评估工具，用于对代码练习进行评级，类似于Codecademy，使用Python，Flask，JavaScript和ReactJS。该应用程序本身将允许用户登录并提交解决方案以解决编码问题。他们还可以获得有关特定解决方案是否正确的反馈。

最终应用：

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/0923/001.gif)
</center>


<!-- more -->

在开发和设计每个微服务时，我们将使用[十二因素应用程序模式](https://12factor.net/)。

除了十二因素外，我们还将练习测试驱动开发（TDD），在有意义的情况下首先编写测试。重点将放在服务器端单元，功能和集成测试，客户端单元测试和端到端测试上，以确保整个系统按预期工作。

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/0923/002.png)
</center>

最后，我们将深入探讨 Docker 和容器编排，以帮助管理，扩展和部署我们的微服务。
