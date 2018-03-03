---
title: Learn Elisp The Hard Way -- 05.Elisp in Emacs
date: 2018-03-03 09:21:21
categories: editor
tags: [emacs, lisp]
---

# 在Emacs中编写Elisp
## 介绍
Emacs是一个提供一些列强大工具的支持多种语言程序开发的编辑器，它当然在Elisp方面的支持也是非常的优秀。

这一节我们来了解一下在Emacs下编写Elisp。

当打开`*.el`结尾的文件后，将看到Emacs菜单栏多了*Emacs-Lisp*菜单。
<center>
![](https://github.com/hypernumbers/learn_elisp_the_hard_way/raw/master/images/emacs-lisp.png)
</center>

<!-- more -->

## 调试模式

# 添加函数到Emacs
## 简单的自定义函数
在`.emacs.d/init.el`文件中添加如下函数：
```lisp
(defun doodlebug ()
 "Nonce function"
 (interactive)
 (message "Howdie-doodie fella"))
```
打开Emacs后，通过*Alt-x*运行命令`doodlebug`，在minibuffer上会显示:
```
Howdie-doodie fella
```

## 带有用户输入的自定义函数
```lisp
(defun doodlebug (a b c)
  "Nonce function"
  (interactive "sAdjective: \nsNoun: \nsExclamation: \n")
  (message "Its a %s thing, this %s, so it is, %s" a b c))
```
输入命令后，会依次要求输入变量值，然后输出`message`。

## 给自定义函数绑定快捷键
比如：
```
(global-set-key [f5] 'doodlebug)
```

我们可以通过命令来确定哪个函数绑定了哪个键：
```lisp
(lookup-key (current-global-map) [f5])
```

通过`kbd`操作符绑定：
```lisp
(global-set-key (kbd "C-c a") 'doodlebug)
(lookup-key (current-global-map) (kbd "C-c a"))
```

# Emacs菜单
## 介绍
本节将创建Emacs菜单来绑定自定义的函数。

在`.emacs.d/init.el`添加如下函数：
```lisp
(defun omar-hip ()
  "a nonce menu function"
  (interactive)
  (message "hip, hop, don't stop"))

(defun omar-hotel ()
 "another nonce menu function"
 (interactive)
 (message "hotel, motel, holiday inn"))
```

## 菜单的基础知识
我们将创建`Omar`菜单来调用上面两个函数。添加代码：
```lisp
(define-key global-map [menu-bar omar]
  (cons "Omar's Menu" (make-sparse-keymap "Omar")))
```

重新打开Emacs，我们将看到名称为`Omar's Menu`的菜单，但是下面还没有项目。现在让我们来添加项目：
```lisp
(define-key-after global-map [menu-bar omar]
  (cons "Omar's Menu" (make-sparse-keymap "Omar")))
(define-key global-map [menu-bar omar omar-hip]
  '(menu-item "Hip" omar-hip
             :help "Hip, yeah!"))
(define-key global-map [menu-bar omar separator-replace-tags]
  '(menu-item "--"))
(define-key global-map [menu-bar omar omar-hotel]
  '(menu-item "Hotel" omar-hotel
             :help "Hotel, yeah!"))
```

## 在现有菜单添加新的项目
```lisp
(defvar menu-bar-omar-menu (make-sparse-keymap "Omar's Menu"))
(define-key menu-bar-omar-menu [omar-hip]
  '(menu-item "Hip" omar-hip
             :help "Hip, yeah!"))
(define-key menu-bar-omar-menu [separator-omar-1]
  '(menu-item "--"))
(define-key menu-bar-omar-menu [omar-hotel]
  '(menu-item "Hotel" omar-hotel
             :help "Hotel, yeah!"))
(define-key menu-bar-edit-menu [omar]
       (list 'menu-item "Omar Menu" menu-bar-omar-menu))
```

## 快捷键
```lisp
(global-set-key [f5] 'omar-hip)
```

