---
title: Flask React Docker in Testdriven - Part II - 1
date: 2018-09-25 20:49:24
tags: [testdriven, flask, react, docker]
---
# Introduction
在第2部分中，我们将添加代码覆盖率和持续集成测试，以确保每个服务都可以独立运行和测试。最后，我们将把 React 和 Jest（一个 JavaScript 测试运行器）和 Enzyme（一个专门为 React 设计的测试库）添加到客户端。

<!-- more -->

## 结构
请注意我们如何使用单个 git 仓库管理单个项目中的每个微服务。重要的是要注意，您还可以将每个服务分解为一个单独的项目，每个项目都有自己的 git  repo 。每种方法都有利弊 —— [mono repo](https://danluu.com/monorepo/) vs multiple repo。做你的研究。

> 对多重回购方式感兴趣？查看本课程第 1 版的代码：
1. [flask-microservices-main](https://github.com/testdrivenio/flask-microservices-main) - Docker Compose文件，Nginx，管理脚本
1. [flask-microservices-users](https://github.com/testdrivenio/flask-microservices-users) - 用于管理用户和身份验证的Flask应用程序
1. [flask-microservices-client](https://github.com/testdrivenio/flask-microservices-client) - 客户端，React app
1. [flask-microservices-swagger](https://github.com/testdrivenio/flask-microservices-swagger) - Swagger API文档
1. [flask-microservices-eval](https://github.com/testdrivenio/flask-microservices-eval) - 用于管理用户分数和练习的Flask应用程序


## 目标
到这部分结束时，你将能够......
1. 使用 Docker Container 中的代码覆盖率运行单元和集成测试
1. 通过 linter 检查代码是否存在任何代码质量问题
1. 配置 Travis CI 以进行持续集成测试
1. 解释 React 是什么以及它与 Angular 和 Vue 的比较
1. 使用在 Docker 容器内运行的 React
1. 单元测试使用 Jest 和 Enact 反应组件
1. 使用 React 组件创建单页应用程序（ SPA ）
1. 使用 React 道具并适当地说明
1. 通过组件生命周期方法管理 React 组件的状态
1. 在构建时将环境变量传递给 Docker 镜像
1. 使用 React 受控组件来处理表单提交
1. 创建一个使用多级 Docker 构建的生产 Dockerfile

## 应用

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/0923/004.png)
</center>

查看官方教程在 EC2 上运行的实时应用：
- http://testdriven-production-alb-1112328201.us-east-1.elb.amazonaws.com

我们还可以测试以下接口：

Endpoint|HTTP Method|CRUD Method|Result
----|----|----|----
/|GET|READ|Load React app
/users|GET|READ|get all users
/users/:id|GET|READ|get single user
/users|POST|CREATE|add a user
/users/ping|GET|READ|sanity check

- 第二部分的官方代码：https://github.com/testdrivenio/testdriven-app-2.3/releases/tag/part2
- 我的代码：https://github.com/keer2345/testdriven-app/tree/part2

## 依赖
第二部分，我们将使用以下依赖：
1. Coverage.py v4.5.1
1. flake8 v3.5.0
1. Flask Debug Toolbar v0.10.1
1. Node v10.4.1
1. npm v6.1.0
1. Create React App v1.5.2
1. React v16.4.1
1. React Scripts v1.1.4
1. React Dom 16.4.1
1. Axios v0.17.1
1. Flask-CORS v3.0.6
1. Enzyme v3.3.0
1. enzyme-adapter-react-16 v1.1.1
