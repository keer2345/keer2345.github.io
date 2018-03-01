---
title: Learn Elisp The Hard Way -- 04.ElispScripts
date: 2018-03-01 22:42:50
categories: editor
tags: [emacs, lisp]
---
# Writing Functions
## 脚本模式的Elisp
建立一个脚本文件，然后通过命令行运行该脚本，比如：
*first_programme.el*
```lisp
(message "Bonjour Tout Le Monde")
```
运行：
```shell
emacs --no-site-file --script message.el
```

## 定义简单函数
我们开始定义一个函数，创建一个文件并添加以下代码：
```lisp
(defun bonjour () (message "Bonjour Tout Le Monde"))
(bonjour)
```
