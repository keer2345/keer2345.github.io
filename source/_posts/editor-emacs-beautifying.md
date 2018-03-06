---
title: Beautifying Emacs
date: 2018-03-05 13:38:11
categories: editor
tags: [emacs]
---
<center>
<h2>Customizing Emacs!</h2>
<h1>Beautifying Emacs!</h1>
</center>

> https://muto.ca/beautifying-emacs.html

## 概述

Emacs是GNU下很牛的文本编辑器！由于它有众多良好的特性，受众多程序员喜欢（当然还有其他行业的牛人）。它始于70年代，其实看到它的初始化界面就可以设想是那个年代的了。我们第一个次启动Emacs时看到的界面类似这样：

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/emacs/20180305-1.gif)
</center>

事实上，这样的界面并不优美，尽管它是优秀的代码编辑器之一。

我们可以将Emacs的界面可以更加漂亮和优雅。比如这样：
<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/emacs/20180305-2.gif)
</center>

信不信由你，我只使用了两个外部GUI扩展（黑色主题和绿色的modeline)。

这篇简单的文章一步步的带您配置（当然，您可以按您的想法配置）。

## 从基本设置开始
设置*Source Code Pro*字体：
```lisp
(set-face-attribute
  'default nil :font "Source Code Pro-13")
```
设置路径
```lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
```
启动时的配置文件：
```lisp
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
```
相对应的，在`init.el`的最后要指明自定义配置文件的交互接口：
```lisp
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
```

## 包管理
*lisp/init-elpa.el*
```lisp
(require 'package)

;; Add package sources
; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; define require-package function
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
  If NO-REFRESH is non-nil, the available package lists will not be
  re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
    t
    (if (or (assoc package package-archive-contents) no-refresh)
      (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(setq package-enable-at-startup nil)
(package-initialize)

(provide 'init-elpa)
```

## 让界面更简洁
*lisp/init-gui-frames.el*

设置界面：
```lisp
;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
```

控制透明度：
```lisp
;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))
```

禁止使用鼠标：
```lisp
(require-package 'disable-mouse)
(global-disable-mouse-mode)
```

## 设置主题
```lisp
(require-package 'zenburn-theme)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(zenburn))
(provide 'init-themes)
```
