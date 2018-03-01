---
title: Learn Elisp The Hard Way -- 01.Perface
date: 2018-02-24 22:17:04
categories: editor
tags: [emacs, lisp]
---
# 学习Elisp

> https://github.com/hypernumbers/learn_elisp_the_hard_way/blob/master/contents/learning-elisp.rst

## 适合的读者
本书为那些不是很熟悉函数式编程的人们编写，适合大部分有主流编程语言经验的程序员，比如Ruby, Java, Python, Perl, C, C++等等。

假定读者在一个干净的Ubuntu GUN/Linux上工作。


## 本书是如何讲授Elisp的
本书通过中介的Emacs来讲授Elisp。Emacs有一个*scratch*窗口可以练习Elisp。开始的课程我们在*scratch*窗口下进行，后面的课程将创建独立的lisp文件并加载到Emacs中来使用它们。

<!-- more -->

# 为什么写这本书

> https://github.com/hypernumbers/learn_elisp_the_hard_way/blob/master/contents/why-did-I-write-this-book.rst

## 为什么是Elisp?
我写这本书来自学Elisp。我每天使用Emacs，我是一个Erlang程序员，选择Emacs来作为Erlang的IDE。

Emacs是一款优秀的软件，通过写这本书，我希望写Elisp配置并在我的工作环境中扩展Emacs。

## 为什么选择这样的格式?
我的目标人群是没有函数式编程经验的程序员。写这本书能有一个很好的方式来学习Elisp，也希望对读者有益。

列举一些我们还不知道的知识点：
1. Emacs和XEmacs的区别
1. Elisp和Lisp的区别
1. Common Lisp与Clojure或者其他Lisp有着怎样的关系
1. Lisp如何进行包管理

## 学习函数式编程
如果想学习函数式编程，真的应该学习Erlang。

# 本书为谁而写

> https://github.com/hypernumbers/learn_elisp_the_hard_way/blob/master/contents/who-is-this-book-for.rst

## 目标读者

目标读者是拥有编程校验而又缺乏函数式编程的程序员：
- 过程化语言 Fortran, Algol, C etc, etc
- 面向对象语言 C++, Java, Ruby etc, etc
- 脚本语言 PHP, Python, Perl etc, etc

函数式语言包含：
- Lisp 和它的方言 Scheme, clojure, Common Lisp (CL) 等等
- Erlang
- Haskell

## 需要多少经验
有一些非函数式编程的经验就足够了！

# 本书涵盖的方面
## 包含了哪些
本书将涉及足够多的Elisp使得你可以：
- 理解`.emacs`文件
- 在`.emacs`文件编写自己的函数
- 创建自己的Emacs菜单
- 编写函数来执行类似打开、关闭文件的事件
- 附加功能键，可以自定义Emacs行为

## 未包含哪些
本书没有教给你足够多的Lisp知识来成为一个Lisp开发者。
