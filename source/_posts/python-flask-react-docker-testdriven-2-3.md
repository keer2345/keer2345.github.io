---
title: Flask React Docker in Testdriven - Part II - 3
date: 2018-09-25 20:49:26
tags: [testdriven, flask, react, docker]
---
# Continuous Integration
本节，我们通过 [Travis CI](https://travis-ci.com/) 将持续集成添加到项目中。

<!-- more -->

安装 [入门指南](https://docs.travis-ci.com/user/getting-started/#To-get-started-with-Travis-CI) 的步骤一、步骤二来启用 Travis （登陆其官网，并通过 Github 账户登录）。


然后构建，将 `.travis.yml` 文件添加到项目根目录：
```yml
sudo: required

services:
  - docker

env:
  DOCKER_COMPOSE_VERSION: 1.21.1

before_install:
  - sudo rm /usr/local/bin/docker-compose
  - curl -L https://github.com/docker/compose/releases/download/${DOCKER_COMPOSE_VERSION}/docker-compose-`uname -s`-`uname -m` > docker-compose
  - chmod +x docker-compose
  - sudo mv docker-compose /usr/local/bin

before_script:
  - docker-compose -f docker-compose-dev.yml up --build -d

script:
  - docker-compose -f docker-compose-dev.yml run users python manage.py test
  - docker-compose -f docker-compose-dev.yml run users flake8 project

after_script:
  - docker-compose -f docker-compose-dev.yml down
```

完成后，将 `README.md` 文件添加到项目根目录，添加 `Travis` 状态标记：
```
# Microservices with Docker, Flask, and React

[![Build Status](https://travis-ci.com/YOUR_GITHUB_USERNAME/testdriven-app.svg?branch=master)](https://travis-ci.com/YOUR_GITHUB_USERNAME/testdriven-app)
```

> 确保 `YOUR_GITHUB_USERNAME` 是您实际的 Github 用户名。

在 Travis 网站上添加该项目，并在设置页点击 “More options -> Trigger build” 。

就工作流而言，目前，虽然项目结构仍然有些简单，但我们将：
1. 在本地编写新功能
1. 提交并推送代码
1. 确保测试通过 Travis
