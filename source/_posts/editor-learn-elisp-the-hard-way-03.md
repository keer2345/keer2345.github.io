---
title: Learn Elisp The Hard Way -- 03.FirstSteps
date: 2018-02-25 09:33:32
categories: editor
tags: [emacs, lisp]
---

# First Elisp Programme
## 开始
本课将展示如何执行第一个Elisp程序
- 开启Emacs
- 进入*scratch*缓存（在菜单`Buffer -> *scratch*`)

输入下面的内容：
```lisp
(+ 1 2)
```
然后将光标置于表达式的后面，按下快捷键`C-j`执行：
```
3
```
如果按下组合键`C-x C-e`，结果则会出现在*minibuffer*栏。

<!-- more -->

## 已经学到了什么
已经了解如何执行基本的Elisp表达式。

# 原始的数据类型
## 数据类型
数据类型分为原始数据类型(primitive data types)和非原始数据类型(non-primitive data types)。

原始数据类型：
- integer
- float
- string
- character
- bool-vector
- symbol
- sequence
- cons
- array
- vector
- char-table
- hash-table
- function
- primitive function (or subr)
- macro
- byte-code
- auto-load

另外，Elisp还有一系列特定的数据类型，因为Elisp是关于编辑器的脚本语言，这些数据类型是：
- buffer
- marker
- window
- frame
- terminal
- window configuration
- frame configuration
- process
- stream
- keymap
- overlay
- font

> **提醒** 我们不需要死记硬背这些数据类型

## 本节将涵盖的知识点
- 表达尔值
- 整数、浮点数
- 字符串
- 测试数据类型
- 值之间的转换

## Booleans
't'表示*true*，`nil`表示*false*：
```lisp
(= 1 1)
t
(= 1 2)
nil
```

使用`if`判断，如果为`true`返回第一个值，反之返回第二个值：
```lisp
(if  t "haha" "hehe")
"haha"
(if nil "haha" "hehe")
"hehe"
```
然而，*booleans*并不是一种类型，而是常量变量。

处理*true/false*并非如此简单，空列表`()`表示`false`：
```lisp
(if () "haha" "hehe")
"hehe"
(if 101 "haha" "hehe")
"haha"
```

## Strings
```lisp
(concat "abc" "def")
"abcdef"
(concat "a'b" "c\"d")
"a'bc\"d"
(concat "ab" "cd" "ef" "12" "34")
"abcdef1234"
```

## Predicate Functions -- Test The Types Of Values
```lisp
(integerp 11)
t
(integerp (+ 1 2.0))
nil
(+ 1 "two")
```

## 数据类型间的转换
```lisp
(+ 1 2.5)
3.5
(float 1)
1.0
(number-to-string 1234)
"1234"
(string-to-number "1234")
1234
```

# Lists
## 介绍
你已经注意到目前为止所有的表达式是这样的`(something somethingelse anotherthing)`。基本的样式为`list`——用括号`()`定义。目前为止我们看到的所有的表达式为最简单的`list`，第一个元素为操作符，接下来的元素是数据。但是，`list`也可以是简单的数据。
## Data Lists
用单引号表示数据列表：
```lisp
'(1 2 3)
(1 2 3)

(quote (1 2 3))
(1 2 3)

(list 1 2 3)
(1 2 3)

`(1 2 3)
(1 2 3)
```

## 嵌套表达式
```lisp
(+ 1 (* 2 3))
7
```

## 扩展
- 添加一个新的值到列表的头部
```lisp
(cons 1 '(2 3))
(1 2 3)
```
- 获取列表的第一个元素
```lisp
(car '(1 2 3))
1
```
- 获取列表的尾部元素
```lisp
(cdr '(1 2 3))
(2 3)
```

# 符号和变量
## 符号概述
构建函数之前，我们需要了解什么是符号*symbols*，我们已经知道了类似`(concat "abc" "def")`的表达式，表达式中的`concat`就是一个符号——它是一个函数*function*或者操作符*operator*。

*Symbols*可以包含下列东东：
- 操作符或者函数
- 数据值
- 属性列表
- 打印符号的名称

一些符号也是所谓的常熟变量——它们自我评价（它们的值就是它们的名称），并且不能将数据值赋予其它任何东西。

常量值用于定义属性列表。

## 符号作为函数名
符号能命名为函数或者操作符，我们将在以后的课程中介绍。

## 符号作为变量
在Elisp中数据类型的实例既是它自身：
```lisp
9
9

fill-column
70
```
这里第一次出现Elisp全局范围的变量`fill-column`。
```lisp
(set 'do_tell '11)
11

do_tell
11
```

请注意，当用单引号来设置`do_tell`时，单引号告诉Elisp不要将`do_tell`作为一个变量，而是通过`set`设置成一个值。操作符`setq`让我们不用对第一个参数设置单引号。
```lisp
(setq do_tell '22)
22

(+ do_tell '33)
55
```

`set`和`setq`有一种变种称为`defvar`。它只应用于未初始化的变量，如果一个变量已经被定义，则不会被覆盖。
```lisp
(setq do_tell 33)
33

(defvar farmer_dell 123)
farmer_dell


farmer_dell
123

(defvar do_tell 123)
do_tell


do_tell
33
```

（只有先使用了`set`或`setq`定义了`do_tell`后，上面的演示才会起到效果）

## 常数变量和关键词符号
一些符号不能被重新定义。我们之前看到的`nil`和`t`，以及设计成冒号`:`开始的也是常数变量。
```lisp
(setq :hotdog 3)

hotdog
3
```

## Property Lists
```lisp
(:quality "great" :achievement "impressive")
```

# Arrays (And Sequences)
## Introduction
- strings
- vectors
- bool-vector
- char-table

## Why Arrays And Not Lists?
数组和列表的不同在于，数组有固定的长度，而列表长度可变。
## 字符串和矢量的区别
## 为什么只有一维数组
## 创建数组
```lisp
[1 2 3]
[1 2 3]

(vector 1 2 3)
[1 2 3]

(string 97 98 99)
"abc"
```
