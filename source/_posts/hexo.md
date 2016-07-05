---
title: Hexo
date: 2016-06-04 09:25:14
categories: blog
tags: hexo
---
# Hexo安装与部署
```
npm install hexo-cli -g
hexo init blog
cd blog
npm install
hexo server
```

<!--more-->

# 基本使用
```
hexo new [layout]<title>
hexo clean
hexo generate
hexo deploy
```

# 分类与标签
## 设置分类
```
categories: a
```
## 设置标签
1. 单个标签
```
tags: a
```
2. 多个标签
```
tags: [a,b,c]
```
