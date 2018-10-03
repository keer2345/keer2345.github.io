---
title: Flask React Docker in Testdriven - Part I - 10
date: 2018-09-23 20:49:20
tags: [testdriven, flask, react, docker]
---

# Deployment
编写完了路由和测试，让我们部署这个应用吧。

按此处说明[注册](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/get-set-up-for-amazon-ec2.html)并创建 [IAM](https://aws.amazon.com/iam/) 用户。

> IAM 参考文章：[Controlling Access to Amazon EC2 Resources](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UsingIAM.html)


<!-- more -->


## 前期准备
1. 添加生产环境下的文件
```
touch docker-compose-prod.yml
touch services/users/Dockerfile-prod
touch services/users/entrypoint-prod.sh
```
1. 添加 IAM 用户，添加的时候选择”AWS 管理控制台访问“。
1. 记录下”AWS Access Key ID" 和 "AWS Secret Access Key"。

然后，使用 [Docker Machine](https://docs.docker.com/machine/) 来[创建](https://docs.docker.com/machine/reference/create/) Docker host 。

```
$ docker-machine create --driver amazonec2 testdriven-prod
```

> 更多参考：[Amazon Web Services (AWS) EC2 example](https://docs.docker.com/machine/examples/aws/)

```
$ aws configure
AWS Access Key ID : AKIAJB4S634V********
AWS Secret Access Key : DW0AqEHW6Xo4CuOZ03ekH1D1A9hEdB24********
Default region name [None]: 
Default output format [None]: 

$ docker-machine create --driver amazonec2 testdriven-prod
Creating CA: /home/****/.docker/machine/certs/ca.pem
Creating client certificate: /home/****/.docker/machine/certs/cert.pem
Running pre-create checks...
Creating machine...
(testdriven-prod) Launching instance...
Waiting for machine to be running, this may take a few minutes...
Detecting operating system of created instance...
Waiting for SSH to be available...
Detecting the provisioner...
Provisioning with ubuntu(systemd)...
Installing Docker...
Copying certs to the local machine directory...
Copying certs to the remote machine...
Setting Docker configuration on the remote daemon...
Checking connection to Docker...
Docker is up and running!
To see how to connect your Docker Client to the Docker Engine running on this virtual machine, run: docker-machine env testdriven-prod
```

接着激活主机并指向 Docker 客户端：
```
$ docker-machine env testdriven-prod
$ eval $(docker-machine env testdriven-prod)
```

> 更多的 `eval` 命令可以参考[这里](https://stackoverflow.com/questions/40038572/eval-docker-machine-env-default/40040077#40040077)

查看当前正在运行的机器：
```
$ docker-machine ls
```


注册容器，创建数据库并添加种子数据，运行测试：
```
$ docker-compose -f docker-compose-prod.yml up -d --build

$ docker-compose -f docker-compose-prod.yml run users python manage.py recreate_db

$ docker-compose -f docker-compose-prod.yml run users python manage.py seed_db

$ docker-compose -f docker-compose-prod.yml run users python manage.py test
```

接着，在**安全组**添加一条`自定义TCP规则`、端口 `5000`、来源`自定义 0.0.0.0/0`，并通过一下命令获取该机器的 IP：
```
docker-machine ip testdriven-prod
```

测试，确保 `DOCKER_MACHINE_IP` 为刚才获取到的实际 IP。
1. http://DOCKER_MACHINE_IP:5001/users/ping
1. http://DOCKER_MACHINE_IP:5001/users

## 配置
应用配置和环境变量都是些什么？这些配置对吗？我们使用的是生产环境的配置吗？运行来检查看看：
```
$ docker-compose -f docker-compose-prod.yml run users env
```

将看到 `APP_SETTINGS` 的值为 `project.config.DevelopmentConfig` 。为了修改它，我们在 `docker-compose-prod.yml` 中改变它的值：
```
environment:
  - APP_SETTINGS=project.config.ProductionConfig
  - DATABASE_URL=postgres://postgres:postgres@users-db:5432/users_prod
  - DATABASE_TEST_URL=postgres://postgres:postgres@users-db:5432/users_test
```

更新：
```
$ docker-compose -f docker-compose-prod.yml up -d
```

重新创建数据库以及生成数据种子：
```
$ docker-compose -f docker-compose-prod.yml run users python manage.py recreate_db

$ docker-compose -f docker-compose-prod.yml run users python manage.py seed_db
```

确保应用保持运行并再次检查环境变量。

## Gunicorn
要使用 Gunicorn ，首先得安装它：
```
pip install gunicorn
```

编写文件 `services/users/entrypoint-prod.sh` :
```
#!/bin/sh

echo "Waiting for postgres..."

while ! nc -z users-db 5432; do
  sleep 0.1
done

echo "PostgreSQL started"

gunicorn -b 0.0.0.0:5000 manage:app
```

文件 `touch services/users/Dockerfile-prod`:
```
# base image
FROM python:3.6.5-alpine

# install dependencies
RUN apk update && \
    apk add --virtual build-deps gcc python-dev musl-dev && \
    apk add postgresql-dev && \
    apk add netcat-openbsd

# set working directory
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

# add and install requirements
COPY ./requirements.txt /usr/src/app/requirements.txt
RUN pip install --upgrade pip
RUN pip install -r requirements.txt

# add entrypoint.sh
COPY ./entrypoint-prod.sh /usr/src/app/entrypoint-prod.sh
RUN chmod +x /usr/src/app/entrypoint-prod.sh

# add app
COPY . /usr/src/app

# run server
# CMD python manage.py run -h 0.0.0.0
CMD ["/usr/src/app/entrypoint-prod.sh"]
```

`touch services/users/entrypoint-prod.sh`:
```
version: "3.6"

services:
  users:
    build:
      context: ./services/users
      dockerfile: Dockerfile-prod
    # volumes:
    #   - './services/users:/usr/src/app'
    ports:
      - 5000:5000
    environment:
      - FLASK_ENV=production
      - APP_SETTINGS=project.config.ProductionConfig 
      - DATABASE_URL=postgres://postgres:postgres@users-db:5432/users_prod
      - DATABASE_TEST_URL=postgres://postgres:postgres@users-db:5432/users_test
    depends_on:
      - users-db

  users-db:
    build:
      context: ./services/users/project/db
      dockerfile: Dockerfile
    ports:
      - 5435:5432
    environment:
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
```

更新：
```
$ docker-compose -f docker-compose-prod.yml up -d --build
```

> 当我们添加了新的依赖后，`--build` 是必须的。

## Nginx
接下来，让我们启动 Nginx 并运行 Web 服务器的反向代理。创建 `services/nginx` 文件夹，并添加 `Dockerfile-prod`、`prod.conf` 文件。

`services/nginx/Dockerfile-prod`:

```
FROM nginx:1.15.0-alpine

RUN rm /etc/nginx/conf.d/default.conf
COPY /prod.conf /etc/nginx/conf.d
```


`services/nginx/prod.conf`:
```
server {

  listen 80;

  location / {
    proxy_pass        http://users:5000;
    proxy_redirect    default;
    proxy_set_header  Host $host;
    proxy_set_header  X-Real-IP $remote_addr;
    proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header   X-Forwarded-Host $server_name;
  }

}
```

添加 `nginx` 服务到 `docker-compose-prod.yml`:
```
  nginx:
    build:
      context: ./services/nginx
      dockerfile: Dockerfile-prod
    restart: always
    ports:
      - 80:80
    depends_on:
      - users
```

然后，移除 `users` 服务暴露的端口，只给其他容器保留 `5000` 端口：
```
expose:
  - '5000'
```

> 重要的是要注意，如果存在共享网络，默认情况下会暴露端口，因此 `EXPOSE` 不需要明确使用以使端口可用于其他容器。但是，使用它仍然是一个很好的做法，`EXPOSE` 以便其他开发人员可以看到正在暴露的端口。换句话说，这是一种文档形式。

构建镜像并运行容器：
```
$ docker-compose -f docker-compose-prod.yml up -d --build nginx
```

在 AWS 添加端口 `80` 到安全组，再次在浏览器测试，这次使用 http://DOCKER_MACHINE_IP/users 。


让我们在本地也做更新吧。首先，添加 `nginx` 到 `docker-compose-dev.yml`:
```
nginx:
  build:
    context: ./services/nginx
    dockerfile: Dockerfile-dev
  restart: always
  ports:
    - 80:80
  depends_on:
    - users
```

`services/nginx/Dockerfile-dev`:
```
FROM nginx:1.15.0-alpine

RUN rm /etc/nginx/conf.d/default.conf
COPY /dev.conf /etc/nginx/conf.d
```


`services/nginx/dev.conf`:
```
server {

  listen 80;

  location / {
    proxy_pass        http://users:5000;
    proxy_redirect    default;
    proxy_set_header  Host $host;
    proxy_set_header  X-Real-IP $remote_addr;
    proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header  X-Forwarded-Host $server_name;
  }

}
```

接下来，我们使 Docker 指向本地：
```
eval $(docker-machine env -u)
```

运行 nginx 容器：
```
$ docker-compose -f docker-compose-dev.yml up -d --build nginx
```

测试 http://localhost/users 。
> 注意到了吗？您可以在本机使用 `80` 端口或者 `5001` 端口访问应用。为什么呢？在生产环境上，只能使用 `80` 端口访问，又是为什么呢？
