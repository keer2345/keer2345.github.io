---
title: Learning Lisp for CMPT 02-Advanced Functional Programming in LISP
date: 2019-03-15 21:09:09
tags: lisp
---

> http://www2.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/2/tutorial2.html

# 辅助函数和累加器变量
`reverse` 是 Common Lisp 的内建函数，作用是将一个列表（List）反转：

``` common-lisp
USER(1): (reverse '(1 2 3 4))
(4 3 2 1)
USER(2): (reverse '(1 (a b) (c d) 4))
(4 (c d) (a b) 1)
USER(3): (reverse nil)
NIL
```
<!-- more -->

我们也可以很容易地实现自己的 `reverse` 函数，但是要提高效率并非易事，参考上一节的范例，我们可以实现一个简单的 `reverse` 函数：

``` common-lisp
(defun my-reverse (L)
  (if (null L)
	  nil
	  (my-append (my-reverse (cdr L)) (cons (car L) nil))))
```

注意到上面的递归调用到了上一节的 `my-append` 函数来实现。但是这样调用效率是低下并且很耗费内存的。

首先我们来追踪外层的 `my-reverse` 函数：
```
CL-USER> (trace my-reverse)
(MY-REVERSE)
CL-USER> (my-reverse '(1 2 3 4))
  0: (MY-REVERSE (1 2 3 4))
	1: (MY-REVERSE (2 3 4))
	  2: (MY-REVERSE (3 4))
		3: (MY-REVERSE (4))
		  4: (MY-REVERSE NIL)
		  4: MY-REVERSE returned NIL
		3: MY-REVERSE returned (4)
2: MY-REVERSE returned (4 3)
1: MY-REVERSE returned (4 3 2)
0: MY-REVERSE returned (4 3 2 1)
(4 3 2 1)
```

然后追踪里层的 `my-append` 函数：
```
CL-USER> (trace my-append)
(MY-APPEND)
CL-USER> (my-reverse '(1 2 3 4))
  0: (MY-REVERSE (1 2 3 4))
	1: (MY-REVERSE (2 3 4))
	  2: (MY-REVERSE (3 4))
		3: (MY-REVERSE (4))
		  4: (MY-REVERSE NIL)
		  4: MY-REVERSE returned NIL
		  4: (MY-APPEND NIL (4))
		  4: MY-APPEND returned (4)
		3: MY-REVERSE returned (4)
		3: (MY-APPEND (4) (3))
		  4: (MY-APPEND NIL (3))
		  4: MY-APPEND returned (3)
		3: MY-APPEND returned (4 3)
	  2: MY-REVERSE returned (4 3)
	  2: (MY-APPEND (4 3) (2))
		3: (MY-APPEND (3) (2))
		  4: (MY-APPEND NIL (2))
		  4: MY-APPEND returned (2)
		3: MY-APPEND returned (3 2)
	  2: MY-APPEND returned (4 3 2)
	1: MY-REVERSE returned (4 3 2)
	1: (MY-APPEND (4 3 2) (1))
	  2: (MY-APPEND (3 2) (1))
		3: (MY-APPEND (2) (1))
		  4: (MY-APPEND NIL (1))
		  4: MY-APPEND returned (1)
		3: MY-APPEND returned (2 1)
2: MY-APPEND returned (3 2 1)
1: MY-APPEND returned (4 3 2 1)
0: MY-REVERSE returned (4 3 2 1)
(4 3 2 1)
```

可以看到相当的繁琐，我们要通过辅助函数和累加器变量构建效率更高的版本：

``` common-lisp
(defun my-reverse2-aux (L A)
  (if (null L)
	  A
	  (my-reverse2-aux (cdr L) (cons (car L) A))))

(defun my-reverse2 (L)
  (my-reverse2-aux L nil))
```

# 阶乘
我们用辅助函数的形式重写阶乘函数：

``` common-lisp
(defun my-factorial2-aux (N A)
  (if (= N 1)
	  A
	  (my-factorial2-aux (- N 1) (* N A))))

(defun my-factorial2 (N)
  (my-factorial2-aux N 1))
```

# 尾部递归
递归函数通常易于思考它是如何实现的，可是缺点是运行缓慢。下面我们来用尾部递归的方式实现三个函数：

1. 计算累加， 1+2+...+N ：

``` common-lisp
(defun fast-triangular-aux (N A)
  (if (= N 1)
	  A
	  (fast-triangular-aux (- N 1) (+ N A))))
(defun fast-triangular (N)
  (fast-triangular-aux N 1))
```

2. 计算次方 B^E ：

``` common-lisp
(defun fast-power-aux (B E A)
  (if (zerop E)
	  A
	  (fast-power-aux B (- E 1) (* B A))))
(defun fast-power (B E)
  (fast-power-aux B E 1))
```

3. 计算列表长度：

``` common-lisp
(defun fast-list-length-aux (L A)
  (if (null L)
	  A
	  (fast-list-length-aux (cdr L) (+ 1 A))))
(defun fast-list-length (L)
  (fast-list-length-aux L 0))
```

# 函数作为对象
有时候，我们需要传递多次相同的对象。例如，我们定义一个函数：

``` common-lisp
(defun my-double (x)
  (* 2 x))
```
```
USER(11): (my-double (my-double (my-double (my-double 5))))

80
```

然而，这样做很笨拙，还不能只管的表示出转换了多少次。我们来构建一个递归函数：

``` common-lisp
(defun repeat-transformation (F N X)
  (if (zerop N)
	  X
	  (repeat-transformation F (- N 1) (funcall F X))))
```
我们来测试以下，重复4次 `9 * 2`：
```
(repeat-transformation (function my-double) 4 9)

144
```

# 高阶函数
刚才的函数不仅能进行数学运行，还可以处理列表，比如：

``` common-lisp
(defun prepend-blah (L) (cons 'blah L))
```
```
(repeat-transformation (function prepend-blah) 4 nil)

(BLAH BLAH BLAH BLAH)
```

又或者，获取列表中的第七个元素：

``` common-lisp
(car (repeat-transformation (function cdr) 6 '(a b c d e f g h i j)))

G
```

# Lambda表达式

``` common-lisp
(repeat-transformation #'(lambda (x) (* 3 x)) 5 2)

486
```

下面我们来构建一个函数 `apply-func-list`，这个函数有两个入参，第一个入参是带有一系列函数的列表，第二个入参为一个对象。例如：

``` common-lisp
(apply-func-list (list #'my-double #'list-length #'rest) '(1 2 3))
```
等价于

``` common-lisp
(my-double (list-length (rest '(1 2 3))))
```

具体的函数如下：

``` common-lisp
(defun apply-func-list (L X)
  (if (null L)
	  X
	  (funcall (car L) (apply-func-list (cdr L) X))))
```
用几个例子测试以下：

```
(apply-func-list (list #'(lambda (N) (* 10 N)) #'fourth) '(10 20 30 40 50))

400
```
```
(apply-func-list (list #'third #'second) '((1 2) (3 4 5) (6)))

5
```
```
(apply-func-list (list #'(lambda (N) (- 10 N)) #'list-length) '(a b c d e f))

4
```
```
(apply-func-list (list #'list #'list) 'blah)

((BLAH))
```

# 迭代列表

``` common-lisp
(defun double-list-elements (L)
  (if (null L)
	  nil
	  (cons (my-double (car L)) (double-list-elements (cdr L)))))

(defun reverse-list-elements (L)
  (if (null L)
	  nil
	  (cons (reverse (car L)) (reverse-list-elements (cdr L)))))
```
可以将上面两个函数整合成一个：
``` common-list
(defun mapfirst (F L)
  (if (null L)
	  nil
	  (cons (funcall F (car L)) (mapfirst F (cdr L)))))
```
运行：
```
(mapfirst #'my-double '(1 2 3 4 5))

(2 4 6 8 10)

(mapfirst #'reverse '((1 2 3) (a b c) (4 5 6) (d e f)))

((3 2 1) (C B A) (6 5 4) (F E D))
```

当然，也可以通过 `lambda` 传入函数：
```
(mapfirst #'(lambda (x) (* x x)) '(1 2 3 4 5))
(1 4 9 16 25)

(mapfirst #'butlast '((1 2 3) (a b c) (4 5 6) (d e f)))
((1 2) (A B) (4 5) (D E))
```

事实上，由于高阶函数太常用了，Common Lisp 已经内置了这样的函数，称为 `mapcar`：
```
(mapcar #'(lambda (x) (* x x)) '(1 2 3 4 5))
(1 4 9 16 25)

(mapcar #'butlast '((1 2 3) (a b c) (4 5 6) (d e f)))
((1 2) (A B) (4 5) (D E))
```

# 寻找迭代 find-if

``` common-lisp
(find-if #'evenp '(1 2 3 4 5))
2

(find-if #'(lambda (X) (not (null X))) '(nil nil (1 2 3) (4 5)))
(1 2 3)

(find-if #'(lambda (X) (>= (list-length X) 3)) '((1 2) (3 4) (5 6 7) (a b c d)))
(5 6 7)

(find-if #'(lambda (X) (evenp (list-length X))) '((1) (2 3 4) (5 6)))
(5 6)

(find-if #'(lambda (X) (zerop (rem X 3))) '(1 2 3 4 5 6 7))
3
```

# 过滤迭代 filter
