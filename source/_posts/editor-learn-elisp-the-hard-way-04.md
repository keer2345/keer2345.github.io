---
title: Learn Elisp The Hard Way -- 04.ElispScripts
date: 2018-03-01 22:42:50
categories: editor
tags: [emacs, lisp]
---
# 编写函数
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

<!-- more -->

## 定义简单函数
我们开始定义一个函数，创建一个文件并添加以下代码：
```lisp
(defun bonjour () (message "Bonjour Tout Le Monde"))
(bonjour)
```

## 定义复杂点的函数
```lisp
(defun tough_maths (i j k)
  "jeepers maths is tough!"
  (message (number-to-string (+ i j k))))
(tough_maths 3 4 5)
```
执行后将输出：
```
12
```
上面这种情况，`defun`带有4个参数，而不是3个。第二个参数`(i j k)`是函数的列表参数，第三个参数是文档——它可以占据多行，第四个参数是函数的主要部分。

## 可变参数数量的函数
```lisp
(defun variable_arity (a &optional b &rest c)
   "This is a function which has variable arity"
   (message (concat "variable a is " a))
   (message (concat "variable b is " b))
   (if c (message "c is not an empty list") (message "c is an empty list")))
(message "run the fn with 1 variable")
(variable_arity "eh")
(message "run the fn with 2 variables")
(variable_arity "eh" "bee")
(message "run the fn with 3 variables")
(variable_arity "eh" "bee" "see")
(message "run the fn with 4 variables")
(variable_arity "eh" "bee" "see" "dee")
(message "run the fn with 5 variables")
(variable_arity "eh" "bee" "see" "dee" "eee")
```
结果为：
```
run the fn with 1 variable
variable a is eh
variable b is
c is an empty list
run the fn with 2 variables
variable a is eh
variable b is bee
c is an empty list
run the fn with 3 variables
variable a is eh
variable b is bee
c is not an empty list
run the fn with 4 variables
variable a is eh
variable b is bee
c is not an empty list
run the fn with 5 variables
variable a is eh
variable b is bee
c is not an empty list
```

`&optional b`表示`&optional`后面的为可选参数，`&rest c`表示`c`为一个*list*。

# 更多的函数
## 介绍
本节介绍：
- 使用本地变量
- 递归函数

## 使用本地变量
我们已经学过如何使用`set`和`setq`来设置全局变量。全局变量使用起来让人非常的困惑，它能随时随地改变值，让我们很难查找原因来安全地使用和调试。因此，定义本地变量就显得相当重要了。

常常用函数`let`来定义，有点类似`set`。

在文件中输入以下表达式：
```lisp
(let ((first "hey")
(second "ho"))
(message first)
(message second))
(message first)
```
输出：
```
hey
ho
Symbol's value as variable is void: first
```

```lisp
(defun myfun (a b c d)
"This is a nonce function designed to show how to
use local variables safely"
(let ((e (+ a b))
(f (* c d)))
(- e f)))
(message (number-to-string (myfun 7 5 3 1)))
```
输出：
```
9
```

## 递归函数
创建文件：
```lisp
(defun print_int (n)
"This function prints a list of integers in reverse order"
(message (number-to-string n))
(if (= n 0) (message "That's all folks!") (print_int (- n 1))))
(print_int 5)
```
输出结果：
```
5
4
3
2
1
0
That's all folks!
```
