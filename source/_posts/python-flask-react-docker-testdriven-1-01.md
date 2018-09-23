---
title: Flask React Docker in Testdriven - Part I - 01
date: 2018-09-23 20:47:13
tags: [testdriven, flask, react, docker]
---

# 介绍

> https://testdriven.io



本章，我们学习如何使用 Docker 构建可复用的开发环境，以创建 Flask-restful, Postgres 网页应用，并部署到云服务器。

<!-- more -->
## 准备
- Docker: [Get started with Docker](https://docs.docker.com/engine/getstarted/)
- Docker Compose: [Get started with Docker Compose](https://docs.docker.com/compose/gettingstarted/)
- Docker Machine: [Docker Machine Overview](https://docs.docker.com/machine/overview/)
- Flask: [Flaskr TDD](https://github.com/mjhea0/flaskr-tdd)

## 目标
学完本章，我们可以：
1. 开发 Flask + Restful API
1. 实践测试驱动（test-driven）开发
1. 使用 Docker 在本地配置及运行服务
1. 利用 volumes 并加载代码到 container
1. 在 container 中运行单元和集成测试
1. 启动不同容器中运行的服务以相互通信
1. 在 Docker 容器内运行 Flask
1. 在 Amazon EC2 上安装 Flask, Nginx 和 Gunicorn
1. 使用 Docker Machine 部署到 EC2

# App
应用的最后效果： http://testdriven-production-alb-1112328201.us-east-1.elb.amazonaws.com/

这个应用运行在三个容器：Flask, Postgres, Nginx 。在随后的章节中我们将添加权限和其他服务。

# 依赖
1. Python v3.6.5
1. Flask v1.0.2
1. Docker v18.03.1-ce
1. Docker Compose v1.21.1
1. Docker Machine v0.14.0
1. Docker Compose file v3.6
1. Flask-SQLAlchemy v2.3.2
1. psycopg2 v2.7.4
1. Flask-Testing v0.6.2
1. Gunicorn v19.8.1
1. Nginx v1.15.0
1. Bulma 0.7.1
