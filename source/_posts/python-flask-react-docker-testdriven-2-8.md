---
title: Flask React Docker in Testdriven - Part II - 8
date: 2018-09-25 20:49:31
tags: [testdriven, flask, react, docker]
---
# React and Docker
让我们将 React App 容器化。

<!-- more -->

## 本地部署
在 *client* 目录下添加 `Dockerfile-dev`，添加如下代码：
```
# base image
FROM node:10.4.1-alpine

# set working directory
WORKDIR /usr/src/app

# add `/usr/src/app/node_modules/.bin` to $PATH
ENV PATH /usr/src/app/node_modules/.bin:$PATH

# install and cache app dependencies
COPY package.json /usr/src/app/package.json
RUN npm install --silent
RUN npm install react-scripts@1.1.4 -g --silent

# start app
CMD ["npm", "start"]
```

> 通过静默 NPM 输出 `--silent` 是个人选择。但是，它经常不赞成，因为它可以吞没错误。请记住这一点，以免浪费时间进行调试。

添加 `.dockerignore`：
```
node_modules
coverage
build
env
htmlcov
.dockerignore
Dockerfile-dev
Dockerfile-prod
```

然后，将新服务添加到 `docker-compose-dev.yml` 文件中，如下所示：
```
client:
  build:
    context: ./services/client
    dockerfile: Dockerfile-dev
  volumes:
    - './services/client:/usr/src/app'
    - '/usr/src/app/node_modules'
  ports:
    - 3007:3000
  environment:
    - NODE_ENV=development
    - REACT_APP_USERS_SERVICE_URL=${REACT_APP_USERS_SERVICE_URL}
  depends_on:
    - users
```

在终端中，导航到项目根目录，然后设置 `REACT_APP_USERS_SERVICE_URL` 环境变量：
```
$ export REACT_APP_USERS_SERVICE_URL=http://localhost
```


构建映像并启动新容器：
```
$ docker-compose -f docker-compose-dev.yml up --build -d client
```

运行客户端测试：
```
$ docker-compose -f docker-compose-dev.yml run client npm test
```
在浏览器中导航到 http://localhost:3007 以测试应用程序。

如果您导航到主要路由会发生什么？由于我们仍然将流量路由到 Flask 应用程序（通过 Nginx ），您将看到旧的应用程序，提供服务器端模板。我们需要更新 Nginx 配置以将流量路由到React应用程序的主路由。

`services / nginx / dev.conf`:
```
server {

  listen 80;

  location / {
    proxy_pass http://client:3000;
    proxy_redirect    default;
    proxy_set_header  Host $host;
    proxy_set_header  X-Real-IP $remote_addr;
    proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header  X-Forwarded-Host $server_name;
  }

  location /users {
    proxy_pass        http://users:5000;
    proxy_redirect    default;
    proxy_set_header  Host $host;
    proxy_set_header  X-Real-IP $remote_addr;
    proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header  X-Forwarded-Host $server_name;
  }

}
```

发生了什么？
1. 这些 `location` 块定义了特定位置的设置。
1. 当请求的URI与位置块中的 URI 匹配时，Nginx会相应地传递请求 — 例如，传递给 React 或 Flask 开发服务器。

此外，该 `client` 服务需要在 nginx 之前启动，因此更新 docker-compose-dev.yml ：
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
    - client
```
更新容器：
```
docker-compose -f docker-compose-dev.yml up -d --build
```
然后访问浏览器：
1. http://localhost
1. http://localhost/users

设置了卷之后，我们可以利用自动重新加载。要测试，请打开[日志](https://docs.docker.com/compose/reference/logs/)：
```
$ docker-compose -f docker-compose-dev.yml logs -f
```

清楚终端屏幕，然后更改 `App` 组件中的状态对象：
```javascript
this.state = {
  users: [],
  username: 'justatest',
  email: '',
};
```

保存后，您应该看到应用程序重新编译，浏览器应该自行刷新：

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/0923/012.png)
</center>

确保在继续之前将状态更改回来。

> 在本地使用 Docker Machine？使用 Docker Machine 和 VirtualBox 进行自动重装是否有问题？

尝试通过 [chokidar](https://github.com/paulmillr/chokidar) [启用](https://github.com/facebookincubator/create-react-app/blob/master/packages/react-scripts/template/README.md#troubleshooting) 轮询机制，方法是将以下环境变量键/对添加到 *docker-compose-dev.yml* 文件中 `CHOKIDAR_USEPOLLING=true` 。有关详细信息，请查看 [Dockerizing a React App](http://mherman.org/blog/2017/12/07/dockerizing-a-react-app)。

## React构建
在更新生产环境之前，让我们脱离 Docker 创建一个使用 Create React App 的构建（例如，在新的终端窗口中），这将生成静态文件。

确保 `REACT_APP_USERS_SERVICE_URL` 设置了环境变量：
```
$ export REACT_APP_USERS_SERVICE_URL=http://localhost
```

然后从“services/client”目录运行命令 `build` :
```
$ npm run build
```


您应该在 “services/client” 中看到一个带有静态文件的 “build” 目录。我们需要使用基本的 Web 服务器来提供服务。让我们使用 Python 标准库中的 HTTP 服务器。导航到 “build” 目录，然后运行服务器：
```
$ python3 -m http.server
```

这将在 http://localhost:8000 上提供应用程序。在浏览器中测试它以确保它的工作原理。完成后，终止服务器并导航回项目根目录。

## 生产
将 *Dockerfile-prod* 添加到 “client” 目录的根目录：
```
###########
# BUILDER #
###########

# base image
FROM node:10.4.1-alpine as builder

# set working directory
WORKDIR /usr/src/app

# install app dependencies
ENV PATH /usr/src/app/node_modules/.bin:$PATH
COPY package.json /usr/src/app/package.json
RUN npm install --silent
RUN npm install react-scripts@1.1.4 -g --silent

# set environment variables
ARG REACT_APP_USERS_SERVICE_URL
ENV REACT_APP_USERS_SERVICE_URL $REACT_APP_USERS_SERVICE_URL
ARG NODE_ENV
ENV NODE_ENV $NODE_ENV

# create build
COPY . /usr/src/app
RUN npm run build


#########
# FINAL #
#########

# base image
FROM nginx:1.15.0-alpine

# copy static files
COPY --from=builder /usr/src/app/build /usr/share/nginx/html

# expose port
EXPOSE 80

# run nginx
CMD ["nginx", "-g", "daemon off;"]
```

在这里，我们使用[多级构建](https://docs.docker.com/engine/userguide/eng-image/multistage-build/)来创建用于生成静态文件（通过 `npm run build` ）的临时镜像，然后将其复制到生产镜像。临时构建镜像与原始文件和与镜像关联的文件夹一起被丢弃。这样可以生成精简的准生产镜像。

让我们在没有 Docker Compose 的情况下测试它。

首先，从 “services/client” 构建映像，确保使用该 `--build-arg` 标志传入适当的参数：
```
$ docker build -f Dockerfile-prod -t "test" ./ \
  --build-arg NODE_ENV=development \
  --build-arg REACT_APP_USERS_SERVICE_URL=http://DOCKER_MACHINE_IP
```
> 确保 `DOCKER_MACHINE_IP` 为您的 Docker Machine 相关联的 IP

使用 “services/clinet” 中的 `Dockerfile-prod` 构建名称为 `test` 的新镜像所需的参数，这些参数都可以通过在 Dockerfile 访问 [ARG](https://docs.docker.com/engine/reference/builder/#arg) 指令，然后将其用作值 `NODE_ENV` 和 `REACT_APP_USERS_SERVICE_URL` 环境变量。

> 可以运行 `docker images` 查看所有镜像。

从 `test` 镜像注册容器，将容器中的 `80` 端口映射到容器外部的 `9000` 端口：
```
docker run -d -p 9000:80 test
```

浏览器访问 http://localhost:9000 。

完成后停止并移除容器：
```
docker stop CONTAINER_ID
docker rm CONTAINER_ID
```

最后，移除镜像：
```
docker rmi test
```

随着 `Dockerfile-prod` 文件的创建和测试，添加该服务到 `docker-compose-prod.yml`：
```
client:
  container_name: client
  build:
    context: ./services/client
    dockerfile: Dockerfile-prod
    args:
      - NODE_ENV=production
      - REACT_APP_USERS_SERVICE_URL=${REACT_APP_USERS_SERVICE_URL}
  ports:
    - '3007:80'
  depends_on:
    - users
```

相对在运行时才传递 `NODE_ENV` 和 `REACT_APP_USERS_SERVICE_URL` 环境变量，我们将它们定义为构建时的参数。

同样，`clent` 需要在 `nginx` 之前启动，因此需要修改 `docker-compose-prod.yml`：
```
nginx:
  container_name: nginx
  build: ./services/nginx
  restart: always
  ports:
    - 80:80
  depends_on:
    - users
    - client  # new
```

还需要修改 `services/nginx/prod.conf`：
```
server {

  listen 80;

  location / {
    proxy_pass http://client:80;
    proxy_redirect    default;
    proxy_set_header  Host $host;
    proxy_set_header  X-Real-IP $remote_addr;
    proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header  X-Forwarded-Host $server_name;
  }

  location /users {
    proxy_pass        http://users:5000;
    proxy_redirect    default;
    proxy_set_header  Host $host;
    proxy_set_header  X-Real-IP $remote_addr;
    proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header  X-Forwarded-Host $server_name;
  }

}
```

要在生产上更新，将 `testdriven-prod` 置为活动（active）状态，将 `REACT_APP_USERS_SERVICE_URL` 环境变量置为 `testdriven-prod` 机器相对应的 IP 。然后更新容器：
```
docker-machine env testdriven-prod
eval $(docker-machine env testdriven-prod)
export REACT_APP_USERS_SERVICE_URL=http://DOCKER_MACHINE_IP
docker-compose -f docker-compose-prod.yml up -d --build
```

> 请记住：由于环境变量是在构建时添加的，因此如果更改变量，则必须重新构建Docker镜像。

确保浏览器中的一切正常。

## Travis
还有一件事：将 `REACT_APP_USERS_SERVICE_URL` 环境变量添加到 `.travis.yml` 文件中，位于 `before_script`：
```
before_script:
  - export REACT_APP_USERS_SERVICE_URL=http://127.0.0.1 # new
  - docker-compose -f docker-compose-dev.yml up --build -d
```

提交并将代码推送到GitHub。在继续之前确保Travis构建通过。
