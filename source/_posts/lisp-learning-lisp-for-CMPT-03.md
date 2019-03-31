---
title: Learning Lisp for CMPT 03-Data Abstraction
date: 2019-03-27 21:26:38
tags: lisp
---

> http://www2.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/3/tutorial3.html

# 二叉树

假设我们想创建一种新的递归数据类型（*recursive data type*），即我们熟悉的二叉树。首先得定义数据类型的构造函数（*contructors*）、选择器（*selectors*）和识别器（*recognizers*）。

<!-- more -->

在二叉树下，我们需要做：

1. *Constructors*: 我们有两种二叉树：*leaves* 和 *nodes*。因此，我们需要为每个类构造函数：
   - `(make-bin-tree-leaf E)` 一片叶子是一个组件的符合对象，元素为 `E`。
   - `(make-bin-tree-node E B1 B2)` 一个包含三个组件的节点，元素 `E`，左子树 `B1`，右子树 `B2`。每一个 `B1`, `B2` 是一个二叉树。
   > 注意到二叉树的定义本质上是递归的（例如节点）。大的二叉树可由小的二叉树组成。

1. *Selectors*：我们需要为各种二叉树的每个组件定义选择器。
   - `(bin-tree-leaf-element L)`：一片叶子 `L` 的检索元素。
   - `(bin-tree-node-element N)`：一个节点 `N` 的检索元素。
   - `(bin-tree-node-left N)`：一个左子树 `N` 的检索元素。
   - `(bin-tree-node-right N)`：一个右子树 `N` 的检索元素。

1. *Recognizers*：我们为每一类二叉树定义识别器。
   - `(bin-tree-leaf-p B)`：测试给定二叉树是否B是一片叶子。
   - `(bin-tree-node-p B)`：测试给定二叉树是否B是一个节点。


注意到我们还没有写一行代码，但我们仍能够写下这些构造函数、选择器和识别器的函数名，这一过程或多或少是机械的：
1. 为每个递归数据类型的变体定义构造函数。构造函数的参数定义了复合对象的组件。
1. 每个构造函数的参数，定义一个选择器来检索类似的组件。
1. 每个构造函数，定义一个类似的识别器。

下一个问题是我们如何表示一个二叉树作为 LISP 对象。当然，我们首先想到的是列表：
- 用一个包含元素 `E` 的列表表示一片叶子，例如 `(list E)`。
- 包含三个元素的列表表示节点，分别是元素 `E`、左子树 `B1`、右子树 `B2`。例如 `(list E B1 B2)`。

基于上面的陈述，我们可以实现递归数据类型的函数：

``` common-lisp
;;
;; Binary Trees
;;

;;
;; Constructors for binary trees
;;

(defun make-bin-tree-leaf (E)
  "Create a leaf."
  (list E))

(defun make-bin-tree-node (E B1 B2)
  "Create a node with element K, left subtree B1 and right subtree B2."
  (list E B1 B2))

;;
;; Selectors for binary trees
;;

(defun bin-tree-leaf-element (L)
  "Retrieve the element of a leaf L."
  (first L))

(defun bin-tree-node-element (N)
  "Retrieve the element of a node N."
  (first N))

(defun bin-tree-node-left (N)
  "Retrieve the left subtree of a node N."
  (second N))

(defun bin-tree-node-right (N)
  "Retrieve the right subtree of a node N."
  (third N))

;;
;; Recognizers for binary trees
;;

(defun bin-tree-leaf-p (B)
  "Test if binary tree B is a leaf."
  (and (listp B) (= (list-length B) 1)))

(defun bin-tree-node-p (B)
  "Test if binary tree B is a node."
  (and (listp B) (= (list-length B) 3)))
```

表述如下：
```
CL-USER> (make-bin-tree-node '*
							 (make-bin-tree-node '+
												 (make-bin-tree-leaf 2)
												 (make-bin-tree-leaf 3))
							 (make-bin-tree-node '-
												 (make-bin-tree-leaf 7)
												 (make-bin-tree-leaf 8)))

(* (+ (2) (3)) (- (7) (8)))
```

呈现出来的二叉树如下：

			*
		   / \
		  /   \
		 /     \
		+       -
	   / \     / \
	  2   3   7   8

# 查找二叉树
假设我们把二叉树作为容器，一个表达式 `E` 是二叉树 `B` 的一员：
1. `B` 是一片叶子，元素是 `E`。
1. `B` 是一个节点，`E` 即是元素也是一个子树的成员。

例如，`(* (+ (2) (3)) (- (7) (8)))`，*, +, 2, 3, -, 7, 8 都是成员，能直接实现我们的递归数据类型函数，判断某个元素是否为二叉树的成员：

``` common-lisp
(defun bin-tree-member-p (B E)
  "Test if E is an element in binary tree B."
  (if (bin-tree-leaf-p B)
	  (equal E (bin-tree-leaf-element B))
	(or (equal E (bin-tree-node-element B))
		(bin-tree-member-p (bin-tree-node-left B) E)
	(bin-tree-member-p (bin-tree-node-right B) E))))
```
跟踪执行如下：

```
CL-USER> (trace bin-tree-member-p)
(BIN-TREE-MEMBER-P)
USER(15): (bin-tree-member-p '(+ (* (2) (3)) (- (7) (8))) 7)
 0: (BIN-TREE-MEMBER-P (+ (* (2) (3)) (- (7) (8))) 7)
   1: (BIN-TREE-MEMBER-P (* (2) (3)) 7)
	 2: (BIN-TREE-MEMBER-P (2) 7)
	 2: returned NIL
	 2: (BIN-TREE-MEMBER-P (3) 7)
	 2: returned NIL
   1: returned NIL
   1: (BIN-TREE-MEMBER-P (- (7) (8)) 7)
	 2: (BIN-TREE-MEMBER-P (7) 7)
	 2: returned T
   1: returned T
 0: returned T
T
```

再来联系一个例子，计算二叉树成员的个数：

``` common-lisp
(defun bin-tree-size (B)
  "Return the number of members in binary tree B."
  (if (bin-tree-leaf-p B)
	  1
	(+ 1
	   (bin-tree-size (bin-tree-node-left B))
	   (bin-tree-size (bin-tree-node-right B)))))
```
```
CL-USER> (trace bin-tree-size)
(BIN-TREE-SIZE)
CL-USER> (bin-tree-size '(* (+ (2) (3)) (- (7) (8))) )
  0: (BIN-TREE-SIZE (* (+ (2) (3)) (- (7) (8))))
	1: (BIN-TREE-SIZE (+ (2) (3)))
	  2: (BIN-TREE-SIZE (2))
	  2: BIN-TREE-SIZE returned 1
	  2: (BIN-TREE-SIZE (3))
	  2: BIN-TREE-SIZE returned 1
	1: BIN-TREE-SIZE returned 3
	1: (BIN-TREE-SIZE (- (7) (8)))
	  2: (BIN-TREE-SIZE (7))
	  2: BIN-TREE-SIZE returned 1
	  2: (BIN-TREE-SIZE (8))
	  2: BIN-TREE-SIZE returned 1
	  1: BIN-TREE-SIZE returned 3
	  0: BIN-TREE-SIZE returned 7
	  7
CL-USER>
```

# 贯穿二叉树(Traversing Binary Trees)
我们来写一个反转二叉树的函数，让左子树与右子树交换。

``` common-lisp
(defun binary-tree-reverse (B)
  "Reverse binary tree B."
  (if (bin-tree-leaf-p B)
	  B
	  (let ((elmt (bin-tree-node-element B))
		(left (bin-tree-node-left B))
		(right (bin-tree-node-right B)))
	(make-bin-tree-node
	 elmt
	 (binary-tree-reverse right)
	 (binary-tree-reverse left)))))
```

- 情况1：`B` 是一片叶子，返回 B 本身。
- 情况2：`B` 是一个节点，将重新构建一个新的节点，将左子树和右子树交换。

```
CL-USER> (binary-tree-reverse '(* (+ (2) (3)) (- (7) (8))) )

	 (* (- (8) (7)) (+ (3) (2)))
```

用图形表示如下：

			*
		   / \
		  /   \
		 /     \
		-       +
	   / \     / \
	  8   7   3   2


我们再来实现一个函数，将二叉树的成员提取出来，放入一个列表中。

``` common-lisp
(defun bin-tree-preorder (B)
  "Create a list containing keys of B in preorder."
  (if (bin-tree-leaf-p B)
	  (list (bin-tree-leaf-element B))
	(let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
	  (cons elmt
		(append (bin-tree-preorder left)
			(bin-tree-preorder right))))))
```
```
USER(13): (trace bin-tree-preorder)
(BIN-TREE-PREORDER)
USER(14): (bin-tree-preorder '(* (+ (2) (3)) (- (7) (8))))
 0: (BIN-TREE-PREORDER (* (+ (2) (3)) (- (7) (8))))
   1: (BIN-TREE-PREORDER (+ (2) (3)))
	 2: (BIN-TREE-PREORDER (2))
	 2: returned (2)
	 2: (BIN-TREE-PREORDER (3))
	 2: returned (3)
   1: returned (+ 2 3)
   1: (BIN-TREE-PREORDER (- (7) (8)))
	 2: (BIN-TREE-PREORDER (7))
	 2: returned (7)
	 2: (BIN-TREE-PREORDER (8))
	 2: returned (8)
   1: returned (- 7 8)
 0: returned (* + 2 3 - 7 8)
(* + 2 3 - 7 8)
```

正如之前讨论过的，`append` 调用上面的代码效率并不高，可以优化一下：

``` common-lisp
(defun fast-bin-tree-preorder (B)
  "A tail-recursive version of bin-tree-preorder."
  (preorder-aux B nil))

(defun preorder-aux (B A)
  "Append A to the end of the list containing elements of B in preorder."
  (if (bin-tree-leaf-p B)
	  (cons (bin-tree-leaf-element B) A)
	(let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
	  (cons elmt
		(preorder-aux left
			  (preorder-aux right A))))))
```
```
USER(15): (trace fast-bin-tree-preorder preorder-aux)
(PREORDER-AUX FAST-BIN-TREE-PREORDER)
USER(16): (fast-bin-tree-preorder '(* (+ (2) (3)) (- (7) (8))))
 0: (FAST-BIN-TREE-PREORDER (* (+ (2) (3)) (- (7) (8))))
   1: (PREORDER-AUX (* (+ (2) (3)) (- (7) (8))) NIL)
	 2: (PREORDER-AUX (- (7) (8)) NIL)
	   3: (PREORDER-AUX (8) NIL)
	   3: returned (8)
	   3: (PREORDER-AUX (7) (8))
	   3: returned (7 8)
	 2: returned (- 7 8)
	 2: (PREORDER-AUX (+ (2) (3)) (- 7 8))
	   3: (PREORDER-AUX (3) (- 7 8))
	   3: returned (3 - 7 8)
	   3: (PREORDER-AUX (2) (3 - 7 8))
	   3: returned (2 3 - 7 8)
	 2: returned (+ 2 3 - 7 8)
   1: returned (* + 2 3 - 7 8)
 0: returned (* + 2 3 - 7 8)
(* + 2 3 - 7 8)
```

**练习1：**
实现一个函数，将二叉树的成员提取出来组成列表，并将节点置后。常规写法和尾部递归写法分别为：

``` common-lisp
(defun bin-tree-postorder (B)
  "Create a list containing elements of B in postorder."
  (if (bin-tree-leaf-p B)
	  (list (bin-tree-leaf-element B))
	(let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
	  (append (bin-tree-postorder left)
		  (append (bin-tree-postorder right)
			  (cons elmt nil))))))

(defun fast-bin-tree-postorder (B)
  "A tail-recursive version of bin-tree-postorder."
  (postorder-aux B nil))

(defun postorder-aux (B A)
  "Append A to the end of the list containing elements of B in postorder."
  (if (bin-tree-leaf-p B)
	  (cons (bin-tree-leaf-element B) A)
	(let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
	  (postorder-aux left
			 (postorder-aux right
					(cons elmt A))))))
```
```
(CL-USER>
bin-tree-postorder '(* (+ (2) (3)) (- (7) (8))))
(2 3 + 7 8 - *)
CL-USER>
(fast-bin-tree-postorder '(* (+ (2) (3)) (- (7) (8))))
(2 3 + 7 8 - *)
CL-USER>
```

**练习2：**
将节点放在中间：

``` common-lisp
(defun bin-tree-inorder (B)
  "Create a list containing elements of B in inorder."
  (if (bin-tree-leaf-p B)
	  (list (bin-tree-leaf-element B))
	(let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
	  (append (bin-tree-inorder left)
		  (cons elmt
			(bin-tree-inorder right))))))

(defun fast-bin-tree-inorder (B)
  "A tail-recursive version of bin-tree-inorder."
  (inorder-aux B nil))

(defun inorder-aux (B A)
  "Append A to the end of the list containing elements of B in inorder."
  (if (bin-tree-leaf-p B)
	  (cons (bin-tree-leaf-element B) A)
	(let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
	  (inorder-aux left
		   (cons elmt
			 (inorder-aux right A))))))
```
```
CL-USER>
(bin-tree-inorder '(* (+ (2) (3)) (- (7) (8))))
(2 + 3 * 7 - 8)
CL-USER>
(fast-bin-tree-inorder '(* (+ (2) (3)) (- (7) (8))))
(2 + 3 * 7 - 8)
CL-USER>
```

# 抽象数据类型
抽象数据类型是个黑匣子，他们定义了外部接口，而不是实现。例如，一个抽象的集合（`set`）提供了以下操作：
- `(make-empty-set)` 创建空的集合。
- `(set-insert S E)` 返回一个集合，它在集合 `S` 的基础上添加了元素 `E`。
- `(set-remove S E)` 返回一个集合，原有集合 `S` 中移除了元素 `E`。
- `(set-member-p S E)` 判断元素 `E` 是否属为集合 `S` 的成员。
- `(set-empty-p S)` 判断集合 `S` 是否为空。

为了实现这抽象数据类型，我们用没有重复元素的列表（list)来表示集合。

``` common-lisp
(defun make-empty-set ()
  "Creates an empty set."
  nil)

(defun set-insert (S E)
  "Return a set containing all the members of set S plus the element E."
  (adjoin E S :test #'equal))

(defun set-remove (S E)
  "Return a set containing all the members of set S except for element E."
  (remove E S :test #'equal))

(defun set-member-p (S E)
  "Return non-NIL if set S contains element E."
  (member E S :test #'equal))

(defun set-empty-p (S)
  "Return true if set S is empty."
  (null S))
```

**练习：**
在 CLTL2 查找 `adjoin`, `remove` 和 `member` 的定义。特别是知道关键字 `:test` 是如何被用于指定等式测试函数的。如果我们省略 `:test` 和 `#'equal` 会发生什么？

> 注意，我们已经实现了一个抽象的数据类型（集合 sets），并通过接口函数实现更多的递归数据列表和额外的计算约束（不重复）。

# 二叉查找树
实现同样的抽象集合的另一种方式是使用更有效的二叉查找树（*binary search tree* **BST**），二叉查找树是在二叉树的基础上添加了额外的计算约束：
- 所有成员在左子树上没有大于节点的元素。
- 所有成员在右子树上的元素大于节点。
- 所有的叶子成员都是不同的。

而且，我们通过更多基本的递归数据结构（二叉树）的附加计算约束实现了抽象数据类型（sets）。特别是，我们使用二叉树的叶子来存储一组成员（set），并且树节点提供了索引，得以提高搜索效率，有代表性的 `{1 2 3 4}` 集合，看起来像这样：

			2
		   / \
		  /   \
		 /     \
		1       3
	   / \     / \
	  1   2   3   4

一个空的 BST 用 `NIL` 表示，非空的 BST 用二叉树表示。我们来构建和识别空的 BST。

``` common-lisp
(defun make-empty-BST ()
  "Create an empty BST."
  nil)

(defun BST-empty-p (B)
  "Check if BST B is empty."
  (null B))
```

考虑到额外的计算约束，相关测试实现如下：

``` common-lisp
(defun BST-member-p (B E)
  "Check if E is a member of BST B."
  (if (BST-empty-p B)
	  nil
	(BST-nonempty-member-p B E)))

(defun BST-nonempty-member-p (B E)
  "Check if E is a member of nonempty BST B."
  (if (bin-tree-leaf-p B)
	  (= E (bin-tree-leaf-element B))
	(if (<= E (bin-tree-node-element B))
	(BST-nonempty-member-p (bin-tree-node-left B) E)
	  (BST-nonempty-member-p (bin-tree-node-right B) E))))
```
```
USER(16): (trace BST-member-p BST-nonempty-member-p)
(BST-NONEMPTY-MEMBER-P BST-MEMBER-P)
USER(17): (BST-member-p '(2 (1 (1) (2)) (3 (3) (4))) 3)
 0: (BST-MEMBER-P (2 (1 (1) (2)) (3 (3) (4))) 3)
   1: (BST-NONEMPTY-MEMBER-P (2 (1 (1) (2)) (3 (3) (4))) 3)
	 2: (BST-NONEMPTY-MEMBER-P (3 (3) (4)) 3)
	   3: (BST-NONEMPTY-MEMBER-P (3) 3)
	   3: returned T
	 2: returned T
   1: returned T
 0: returned T
T
```

以下函数实现插入功能：

``` common-lisp
(defun BST-insert (B E)
  "Insert E into BST B."
  (if (BST-empty-p B)
	  (make-bin-tree-leaf E)
	(BST-nonempty-insert B E)))

(defun BST-nonempty-insert (B E)
  "Insert E into nonempty BST B."
  (if (bin-tree-leaf-p B)
	  (BST-leaf-insert B E)
	(let ((elmt  (bin-tree-node-element B))
	  (left  (bin-tree-node-left    B))
	  (right (bin-tree-node-right   B)))
	  (if (<= E (bin-tree-node-element B))
	  (make-bin-tree-node elmt
				  (BST-nonempty-insert (bin-tree-node-left B) E)
				  right)
	(make-bin-tree-node elmt
				left
				(BST-nonempty-insert (bin-tree-node-right B) E))))))

(defun BST-leaf-insert (L E)
  "Insert element E to a BST with only one leaf."
  (let ((elmt (bin-tree-leaf-element L)))
	(if (= E elmt)
	L
	  (if (< E elmt)
	  (make-bin-tree-node E
				  (make-bin-tree-leaf E)
				  (make-bin-tree-leaf elmt))
	(make-bin-tree-node elmt
				(make-bin-tree-leaf elmt)
				(make-bin-tree-leaf E))))))
```


			2                      2
		   / \                    / \
		  /   \                  /   \
		 /     \       ==>      /     \
		1       3              1       3
	   / \     / \            / \     / \
	  1   2   3   4          1   2  2.5  4
									/ \
								  2.5  3

代码的具体执行步骤如下：
```
USER(22): (trace BST-insert BST-nonempty-insert BST-leaf-insert)
(BST-LEAF-INSERT BST-NONEMPTY-INSERT BST-INSERT)
USER(23): (BST-insert '(2 (1 (1) (2)) (3 (3) (4))) 2.5)
 0: (BST-INSERT (2 (1 (1) (2)) (3 (3) (4))) 2.5)
   1: (BST-NONEMPTY-INSERT (2 (1 (1) (2)) (3 (3) (4))) 2.5)
	 2: (BST-NONEMPTY-INSERT (3 (3) (4)) 2.5)
	   3: (BST-NONEMPTY-INSERT (3) 2.5)
		 4: (BST-LEAF-INSERT (3) 2.5)
		 4: returned (2.5 (2.5) (3))
	   3: returned (2.5 (2.5) (3))
	 2: returned (3 (2.5 (2.5) (3)) (4))
   1: returned (2 (1 (1) (2)) (3 (2.5 (2.5) (3)) (4)))
 0: returned (2 (1 (1) (2)) (3 (2.5 (2.5) (3)) (4)))
(2 (1 (1) (2)) (3 (2.5 (2.5) (3)) (4)))
```

移除元素的实现如下：

``` common-lisp
(defun BST-remove (B E)
  "Remove E from BST B."
  (if (BST-empty-p B)
	  B
	(if (bin-tree-leaf-p B)
	(BST-leaf-remove B E)
	  (BST-node-remove B E))))

(defun BST-leaf-remove (L E)
  "Remove E from BST leaf L."
  (if (= E (bin-tree-leaf-element L))
	  (make-empty-BST)
	L))

(defun BST-node-remove (N E)
  "Remove E from BST node N."
  (let
	  ((elmt  (bin-tree-node-element N))
	   (left  (bin-tree-node-left    N))
	   (right (bin-tree-node-right   N)))
	(if (<= E elmt)
	(if (bin-tree-leaf-p left)
		(if (= E (bin-tree-leaf-element left))
		right
		  N)
	  (make-bin-tree-node elmt (BST-node-remove left E) right))
	  (if (bin-tree-leaf-p right)
	  (if (= E (bin-tree-leaf-element right))
		  left
		N)
	(make-bin-tree-node elmt left (BST-node-remove right E))))))
```

			2                      2
		   / \                    / \
		  /   \                  /   \
		 /     \       ==>      /     \
		1       3              1       4
	   / \     / \            / \
	  1   2   3   4          1   2

函数的具体执行步骤如下：

```
USER(4): (trace BST-remove BST-node-remove)
(BST-NODE-REMOVE BST-REMOVE)
USER(5): (BST-remove '(2 (1 (1) (2)) (3 (3) (4))) 3)
 0: (BST-REMOVE (2 (1 (1) (2)) (3 (3) (4))) 3)
   1: (BST-NODE-REMOVE (2 (1 (1) (2)) (3 (3) (4))) 3)
	 2: (BST-NODE-REMOVE (3 (3) (4)) 3)
	 2: returned (4)
   1: returned (2 (1 (1) (2)) (4))
 0: returned (2 (1 (1) (2)) (4))
(2 (1 (1) (2)) (4))
```

实现排序：

``` common-lisp
(defun make-empty-sorted-list ()
  "Create empty sorted list."
  nil)

(defun sorted-list-empty-p (L)
  "Test if a sorted list L is empty."
  (null L))

(defun sorted-list-member-p (L E)
  "Test if element E is a member of a sorted list L."
  (if (null L)
	  nil
	(if (> E (first L))
	(sorted-list-member-p (rest L) E)
	  (= E (first L)))))

(defun sorted-list-insert (L E)
  "Insert element E into a sorted list L to produce a new sorted list."
  (if (null L)
	  (list E)
	(if (> E (first L))
	(cons (first L) (sorted-list-insert (rest L) E))
	  (if (= E (first L))
	  L
	(cons E L)))))

(defun sorted-list-remove (L E)
  "Remove element E from sorted list L to produce a new sorted list."
  (if (null L)
	  nil
	(if (> E (first L))
	(cons (first L) (sorted-list-remove (rest L) E))
	  (if (= E (first L))
	  (rest L)
	L))))
```

# 多项式 Polynomials
