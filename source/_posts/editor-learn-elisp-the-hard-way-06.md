---
title: Learn Elisp The Hard Way -- 06.Elisp Files
date: 2018-03-03 21:02:06
categories: editor
tags: [emacs, lisp]
---

# Elisp in Files
## 介绍
在`~/.emacs.d/.omars-dir/`下编辑文件`myomar.el`：
```elisp
(defun omar-hip ()
(interactive)
(message "hip, hop, don't stop"))
(defun omar-hotel ()
(interactive)
(message "hotel, motel, holiday inn"))
```
为了让文件生效，我们需要在`init.el`添加路径：
```lisp
(add-to-list 'load-path "~/.emacs.d/omars-dir")
```

<!-- more -->

## 使用文件
在`init.el`文件的`load-path`后面添加：
```lisp
(require 'myomar)
```

在文件`myomar.el`文件尾部添加：
```lisp
(provide 'myomar)

```

## 执行模式

# Working with Buffers
## 准备我们的Elisp文件`omarmenu.el`:
```lisp
(defun omar-count ()
  (interactive)
  (message "When we have finished this will count the number of words in the current buffer"))

(defvar menu-bar-omar-menu (make-sparse-keymap "Omar"))

(define-key-after global-map [menu-bar omar]
  (cons "Omar's Menu" (make-sparse-keymap "Omar")))

(define-key global-map [menu-bar omar omar-count]
  '(menu-item "Count" omar-count
             :help "Will eventually count words in the current buffer!"))

(global-set-key (kbd "C-c a") 'omar-count)
```

在`init.el`添加:
```lisp
(require 'omarmenu)
```

# Scope Of Variables
`set`, `setq`, `let`, `let*`, etc
## 介绍
Elisp有四中范围
- global scope
- local scope
- buffer-local scope
- terminal-local scope
