---
title: Install Latest Nodejs
date: 2018-02-07 10:58:22
catetories: system
tags: [system,nodejs]
---
# Install
https://tecadmin.net/install-latest-nodejs-npm-on-ubuntu/

```shell
curl -sL https://deb.nodesource.com/setup_9.x | sudo -E bash -
sudo apt-get update
sudo apt-get install nodejs
```

<!-- more -->

## Configuration
使用`nrm`切换`npm`源
```
npm install -g nrm
nrm ls
nrm use taobao
```

## Update

`npm install package_name@version -g`

```
npm install npm@latest -g
npm install node@latest -g
```
# 基本使用
## 初始化和安装包

>具体的包名和版本号我们可以到`www.npmjs.com`查找。

```shell
mkdir project_dir
cd project_dir

# 初始化
npm init -y

# i = install
npm install jquery
npm i vue

# 只要有package.json存在，node_modules可以删除重新安装
rm -rf node_modules
npm i
```

## 删除包
```shell
npm uninstall vue
```

## 升级包
```
npm update jquery
```
