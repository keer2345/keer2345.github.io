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

<!--more-->

## 配置`~/.gitconfig`
添加配置：
```
git config --global user.name "keer2345"
```
`~/.gitconfig` 文件:

```
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
[http]
  postBuffer = 524288000
[credential]
  helper = store
```

## 基本命令
```
git init
git status
git log
git add .
git commit -a -v
```
### 查看差异
```
git diff #要查看尚未暂存的文件更新了哪些部分
git diff --staged #要看已经暂存起来的文件和上次提交时的快照之间的差异
```
### 查看与远端库的差异
```
# 获取远端库最新信息
$ git fetch origin [branch]
# 做diff
$ git diff local_branch origin/branch
$ git diff -- local_branch origin/branch
```

## 日志查看
```
git log
git log --pretty=oneline
git reflog
```

## 版本回退
在Git中，用`HEAD`表示当前版本，上一个版本是`HEAD^`，上上个版本就是`HEAD^^`，往上10个版本写成`HEAD~10`。
```
git reset --hard HEAD
git reset --hard HEAD^

git reflog
git reset --hard 0050777

git reset HEAD file
git checkout -- file
```
### 场景
1. 当你改乱了工作区某个文件的内容，想直接丢弃工作区的修改时，用命令`git checkout -- file`。
1. 当你不但改乱了工作区某个文件的内容，还添加到了暂存区时，想丢弃修改，分两步，第一步用命令`git reset HEAD file`，就回到了场景1，第二步按场景1操作。

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
下载其他分支
```
git checkout -b other_branch origin/other_branch
```

### fork后的远程分支合并
> http://blog.csdn.net/qq1332479771/article/details/56087333

```
git remote -v
git remote add new_branch_name git@github.com:***/***.git  # fork前的仓库
git remote -v # 再次查看仓库
git remote remove new_branch_name # 删除远程仓库

git fetch new_branch_name
git merge new_branch_name/master
git pull origin master # 更新并合并自己远程仓库的代码
git push # 向自己的远程仓库推送代码
```
### git删除已经push到远程服务器上的某个提交

1. 找到想要回退到的commit-id
2. 本地代码回退到1中的commit-id

```
git reset --hard <commit-id>
```
3. 推送到远程服务器

```
git push origin HEAD:master --force
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
## 设置代理
Git代理设置，加速clone

1. 确保你的ss客户端正常连接，打开命令行输入以下代码
1. 1080是shadowsocks的默认端口，如果你修改过，
只需要把上面代码里的1080替换掉。


```
git config --global http.proxy 'socks5://127.0.0.1:1080'
git config --global https.proxy 'socks5://127.0.0.1:1080'
```


## 参考资料
[十五分钟了解Git](https://blog.angularindepth.com/become-a-git-pro-by-learning-git-architecture-in-15-minutes-9c995db6faeb)
[Git最详细篇](http://www.jianshu.com/p/e8a6c4e40b58)
