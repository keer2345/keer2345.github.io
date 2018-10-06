---
title: Flask React Docker in Testdriven - Part II - 9
date: 2018-09-25 20:49:32
tags: [testdriven, flask, react, docker]
---

# Structure

第二部分的源代码及其结构。

<!-- more -->

- 官方源代码 part 2: https://github.com/testdrivenio/testdriven-app-2.3/releases/tag/part2
- 我的源代码 part 2: https://github.com/keer2345/testdriven-app/tree/part2

项目结构：
```
├── README.md
├── docker-compose-dev.yml
├── docker-compose-prod.yml
└── services
    ├── client
    │   ├── Dockerfile-dev
    │   ├── Dockerfile-prod
    │   ├── README.md
    │   ├── build
    │   ├── coverage
    │   ├── package.json
    │   ├── public
    │   │   ├── favicon.ico
    │   │   ├── index.html
    │   │   └── manifest.json
    │   ├── src
    │   │   ├── components
    │   │   │   ├── AddUser.jsx
    │   │   │   ├── UsersList.jsx
    │   │   │   └── __tests__
    │   │   │       ├── AddUser.test.jsx
    │   │   │       ├── UsersList.test.jsx
    │   │   │       └── __snapshots__
    │   │   │           ├── AddUser.test.jsx.snap
    │   │   │           └── UsersList.test.jsx.snap
    │   │   ├── index.js
    │   │   ├── logo.svg
    │   │   ├── registerServiceWorker.js
    │   │   └── setupTests.js
    │   └── yarn.lock
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
        ├── htmlcov
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
