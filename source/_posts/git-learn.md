---
title: Git Learn
date: 2016-05-02 00:46:08
categories: git
tags: git
---
`Git`是一个神奇的分布式版本控制系统。

## 安装
```
apt-get install git tig
```

## 配置
```shell ~/.gitconfig
[user]
  name = keer2345
  email = keer2345@gmail.com
[color]
  ui = true
[core]
  editor = vim
[alias]
  st = status
  ci = commit -a -v
  throw = reset --hard HEAD
  throwh = reset --hard HEAD^
  co = checkout
  br = branch
[push]
  default = simple
```

## 基本命令
```
git init
git status
git log
git add .
git commit -a -v
git diff #要查看尚未暂存的文件更新了哪些部分
git diff --staged #要看已经暂存起来的文件和上次提交时的快照之间的差异
```

## 分支与标签
### 新建、合并、删除分支
```
git checkout -b iss5d3

git branch iss53
git checkout iss53

git checkout master
git merge iss53

git branch -d iss53
git branch -D iss53   # -D 表示强制删除未合并的分支
```

### 分支管理
```shell
git branch
git branch -v
git branch --merged  #查看哪些分支已被并入当前分支
git branch --no-merged #查看尚未合并的分支
```

### 标签
```
#1b2e1d63ff 是你想要标记的提交 ID 的前 10 位字符。
#也可以使用少一点的提交 ID 前几位或全部，只要它的指向具有唯一性。
git log
git tag 1.0.0 1b2e1d63ff
git tag -d 1.0.0 #删除标签

#默认情况下，git push并不会把tag标签传送到远端服务器上
#只有通过显式命令才能分享标签到远端仓库。
git push origin 1.0.0 #将本地 1.0.0 的tag推送到远端服务器

git push --tags  #push所有tag，命令格式为：git push [origin] --tags
```