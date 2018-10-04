---
title: Flask React Docker in Testdriven - Part I - 13
date: 2018-09-23 20:49:23
tags: [testdriven, flask, react, docker]
---
# Structure

- 官方的源代码 Part 1: https://github.com/testdrivenio/testdriven-app-2.3/releases/tag/part1

- 我的源代码 Part 1：https://github.com/keer2345/testdriven-app/tree/part1

<!-- more -->

完成了第一部分，项目的结构看起来像这样：
```
├── docker-compose-dev.yml
├── docker-compose-prod.yml
└── services
    ├── nginx
    │   ├── Dockerfile-dev
    │   ├── Dockerfile-prod
    │   ├── dev.conf
    │   └── prod.conf
    └── users
        ├── Dockerfile-dev
        ├── Dockerfile-prod
        ├── entrypoint-prod.sh
        ├── entrypoint.sh
        ├── manage.py
        ├── project
        │   ├── __init__.py
        │   ├── api
        │   │   ├── __init__.py
        │   │   ├── models.py
        │   │   ├── templates
        │   │   │   └── index.html
        │   │   └── users.py
        │   ├── config.py
        │   ├── db
        │   │   ├── Dockerfile
        │   │   └── create.sql
        │   └── tests
        │       ├── __init__.py
        │       ├── base.py
        │       ├── test_config.py
        │       └── test_users.py
        └── requirements.txt
```

