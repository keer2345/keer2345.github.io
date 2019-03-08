---
title: Learning Lisp for CMPT 01 - Basic LISP Programming
date: 2019-03-07 22:13:52
tags: lisp
---

> https://www.cs.sfu.ca/CourseCentral/310/pwfong/Lisp

> https://www.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/1/tutorial1.html

# LISP表达式
Common LISP 环境与用户交互遵循下面的算法：
```
loop
	read in an expression from the console;
	evaluate the expression;
	print the result of evaluation to the console;
end loop.
```

Common LISP 读取表达式，评估它，然后打印结果。例如想要计算 `(2 * cos(0) * (4 + 6))`：
```
~> sbcl
This is SBCL 1.4.16, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (* 2 (cos 0) (+ 4 6))
20.0
*
```

在我们继续下一步之前，需要了解：
- LISP 大部分但是 *function* 应用，函数 *f(x)* 表示为 `(f x)`，比如 *cos(0)* 写做 `(cos 0)`。
- 表达式不区分大小写。
- "+" 是一个加法函数，返回参数的和。
- 一些函数，类似 "+","*" 可以有多个参数，比如 `(+ 1 2 3 4)`。

一些内建的算法函数：
- (+ x1 x2 ... xN)
- (* x1 x2 ... xN)
- (- x y)
- (/ x y)
- (rem x y)
- (abs x)
- (max x1 x2 ... xN)
- (min x1 x2 ... xN)

# 定义函数functions
```
[1]> (defun double(x) (* x 2))
DOUBLE
[2]> (double 3)
6
[3]> (double 7)
14
```

# 编辑，加载和编译
