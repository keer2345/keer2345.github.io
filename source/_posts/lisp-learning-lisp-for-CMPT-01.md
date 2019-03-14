---
title: Learning Lisp for CMPT 01-Basic LISP Programming
date: 2019-03-13 13:01:39
tags: lisp
---

> http://www2.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/

> http://www2.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/1/tutorial1.html

<!--more-->

# Lisp表达式
Common Lisp 环境与用户交互时遵循下面的算法：
```
loop
	read in an expression from the console;
	evaluate the expression;
	print the result of evaluation to the console;
end loop.
```

在 Emacs + SLIME 的工作模式下：
``` lisp
USER(1):(* 2 (cos 0) (+ 4 6))
20.0
```

# 定义函数
``` lisp
(defun my-double (x) (* x 2))
```

```
(my-double 3)
6
```


# 编辑、加载和编译LISP程序
有这么一个文件 `testing.lisp`：

``` lisp
(defun triple (x)
  "Compute three times X." ;Inline comments can
  (* 3 X))  ; be placed here.

(defun negate (x)
  (- x))
```

在 LISP (SLIME) 环境加载该文件：

```
USER(5): (load "testing.lisp")
; Loading ./testing.lisp
T
```
运行函数：
```
USER(6): (triple 2)
6
USER(7): (negate 3)
-3
```

# 控制结构：递归和条件
例子，计算 `n` 的阶乘 `n!`：

``` lisp
(defun factorial (N)
  (if (= N 1)
	  1
	  (* N (factorial (- N 1)))))
```

运行：

```
(factorial 5)
120
```

例子，1+2+...+N ：

``` lisp
(defun my-count (N)
  (if (=  N 1)
	  1
	  (+ N (my-count (- N 1)))))
```
运行 `(my-count 9)` 查看结果。


例子，计算次方（A 的 B 次方）：

``` lisp
(defun my-power (A B)
  (if (zerop B)
	  1
	  (* A (my-power A (- B 1)))))
```

# 多重递归

``` lisp
;; 1 1 2 3 5 8 13 ...
(defun my-fibonacci (N)
  (if (or (zerop N) (= N 1))
	  1
	  (+ (my-fibonacci (- N 1)) (my-fibonacci (- N 2)))))
```

# 列表的递归
计算列表长度：

``` common-lisp
(defun my-list-length (L)
  (if (null L)
	  0
	  (+ 1 (my-list-length (cdr L)))))
```

寻找第 *n* 个元素：

``` common-lisp
(defun my-nth (N L)
  (if (null L)
	  nil
	  (if (zerop N)
	  (car L)
	  (my-nth (- N 1) (cdr L)))))
```

判断一个元素是否在列表中：
``` common-lisp
(defun my-member (E L)
  (cond
	((null L) nil)
	((eq E (car L)) t)
	(t (my-member E (cdr L)))))
```

追加列表：

``` common-lisp
(defun my-append (L1 L2)
  (if (null L1)
	  L2
	  (cons (car L1) (my-append (cdr L1) L2)))))
```

像集合（Sets）一样使用列表（Lists）：

``` common-lisp
(defun my-intersection (L1 L2)
  (cond
	((null L1) nil)
	((member (car L1) L2)
	 (cons (car L1) (my-intersection (cdr L1) L2)))
	(t (my-intersection (cdr L1) L2))))
```
