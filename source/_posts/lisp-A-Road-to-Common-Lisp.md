---
title: A Road to Common Lisp
date: 2019-03-10 21:12:37
tags: [lisp]
---

>  http://stevelosh.com/blog/2018/08/a-road-to-common-lisp/

本篇文章讲述如何学习 Common Lisp 语言，希望能够给同样对 Common Lisp 语言有兴趣的朋友提供一些有用的建议。

# 背景
我们先了解一下 Common Lisp 的来历以及它是一门怎么样的计算机语言是很有必要的。如果以现代编程语言的角度来看 Common Lisp，它确实有一些奇怪。
##　历史
Common Lisp 历史悠久，我们大概的说一下，不在这里阐述它的前世今生——如果有兴趣可以参考以下资料：
- Wikipedia 的 [History of Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)#History) 和 [History of Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp#History)。
- [Where it Began section in Practical Common Lisp](http://www.gigamonkeys.com/book/introduction-why-lisp.html#where-it-began)。
- [History: Where did Lisp come from?](https://www.cs.cmu.edu/Groups//AI/lang/lisp/faq/lisp_2.faq)
- [Common Lisp: the Untold Story](http://www.nhplace.com/kent/Papers/cl-untold-story.html)
- [The Evolution of Lisp](https://www.dreamsongs.com/Files/HOPL2-Uncut.pdf)

如果我们不想详细的阅读上面的资料的话，就来快速了解一下 Lisp 六十多年来的历史吧。

Lisp 起始于上个世纪 50 年代，创始人是麻省理工大学（MIT）的 John McCarthy。之后的二十多年里，有许多版本的 Lisp 方言产生并繁荣发展，其中一些比较有名的有 Maclisp, BBN Lisp/Interlisp, Franz Lisp, Spice Lisp, Lisp Machine Lisp。它们有各自不同的实现，并在不同的领域不断成长、变化。

到了八十年代，人们认为有太多互不相容的 Lisp 方言并非好事，希望有一个共通的语言能适应每一个人，在 1984 年，Guy Steele 发布了一个版本的 [Common Lisp: the Language](https://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html)。

我们可以看到这本书围绕着 Lisp 25 年来的实际应用、实验、经验以及历史借鉴来讲述。即便如此，这本书也没有满足每一个人的需要，所以在 1986 年，国际信息技术标准委员会为 Common Lisp 指定了 ANSI 规范。

委员会致力于将其标准化，并与 1990 年发布了 Common Lisp 第二版，更为全面。此时，Lisp 语言家族已经有超过30年的经验和历史借鉴。而一门现代语言（Python）的第一版才刚刚发布不久。

## 影响
我想告诉大家，Common Lisp 是怎样一门稳定、强大、使用、可扩展而又奇怪的 语言。

**兼容性**

在其他语言，如果升级之后可能一些库会被破坏，比如用最新版本的 Ruby 运行一年前的代码可能需要完善一下。我当前的工作语言是 Scala，如果在　Github 上找到一个两三年前的库，我很怀疑它是否能适应当前的需要。兼容性是现代语言不得不每天都要面对的事情。

对于 Common Lisp 来说这都不是事，后面我会推荐一本 1990 年代写的书，我们可以不做修改的用最新版本的 Common Lisp 运行这本书里的代码。

**实用性**

Common Lisp 是一门强大而使用的语言。在我们学习 Common Lisp 并查找一些库的时候，内心常常会想 “这个项目最后一次更新是在几年前吗？是不是本放弃了？” 而 Common Lisp 的稳定性告诉我们这些库会一直持续，可不要小看了它们。

**扩展性**

Common Lisp 之所以如此实用得益于它的可扩展性。它有着强大的宏（Macros），宏允许我们编写库来实现在其他语言中的核心特性：
- Common Lisp 并没包含字符串插入，想要实现这个功能？没问题，你不需要等候 Scala 或者 Python，只需要[一个库](https://edicl.github.io/cl-interpol/)。
- 想要尝试一些没有引用的不确定的变成吗？看[这里](https://nikodemus.github.io/screamer/)。
- 语法匹配可以使代码变漂亮，提高可读性。而 Common Lisp 自身并没有这个功能，我们可以看[这里](https://github.com/guicho271828/trivia/wiki/What-is-pattern-matching%3F-Benefits%3F)。
- 享受 Haskell 或 Scala 里的代数数据类型？看[这里](https://github.com/tarballs-are-good/cl-algebraic-data-type)。

所有这些库通过宏可以无缝实现。当然，没有宏也可以实现这些功能，但是不得不添加层来管理，像这样：
```lisp
(match foo
  '(list x y z) (lambda (x y z) (+ x y z))
  '(vector x y) (lambda (x y) (- x y)))
```

只是不会像这样：
```lisp
(match foo
  ((list x y z) (+ x y z))
  ((vector x y) (- x y)))
```

没有人在 Common Lisp 标准版添加模式匹配，因为您可以将其写成库来获取 90% 以上的内容，语言本身给予您足够的能力来扩展。

**功率**

宏使得 Lisp 得以扩展，因为它能让您代码转换成其他代码。类似 C 语言里的宏，但 Common Lisp 的宏又有不同，因为它是语言的一部分。

在 Common Lisp 中，宏在语言自身当中，我们可以使用宏来写函数，并使用这些函数编写更多的宏。它不是分层的，而是抽象的反馈循环。

但是，宏并不是让 Common Lisp 变得可扩展的唯一原因，有的人并没有意识到虽然 Common Lisp 是一门高阶语言，但它也有丰富的低阶工具。它从不会像 C, Rust, 或 Forth 那样低级，而我们会惊讶于 ANSI 包含的内容。

并非所有Common Lisp实现都实际执行所有这些优化，但Common Lisp的设计者有远见地包含支持它们所需的语言功能。这种支持极高级编程与宏的组合以及合理数量的低级优化意味着即使规范已有二十多年的历史，它仍然是今天构建的良好基础。设计师所汲取的三十年经验和历史使他们能够创造出一种能够存活数十年的非常实用的语言。

**奇怪**

理解 Common Lisp 的实用性当然是很重要的，但也要适应其现有方言存在的很奇怪（甚至是丑陋）的部分。可以看看第二版的 *Common Lisp: the Language and Look up* 的索引中看到 `kludges` 一词的奇怪。

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2019/0311-1.jpeg)
</center>

Common Lisp 是一门设计得不够优雅的语言，这一历史包袱也算是它的一个特性吧。如果设计者尝试让其更完美或者代码更漂亮，可能导致其语言本身被忽略，而不是被采用和接受。

## 学习Common Lisp的方式

如果没有被上面的特性吓倒，那就开始我们的学习吧。

在查找 Common Lisp 的学习指南时，并不会找到让自己满意的结果，这是因为很多的 Common Lisp 材料都处于初级阶段。这里列出一些相关的书籍，我会推荐出一些比较好的书籍，但也要吸取其他资料的精华。

**获取Lisp**

在开始学习之前，我们得先安装 Common Lisp，它符合 ANSI 规范。
- 如果使用的是 MacOS，可以下载 GUI 界面的应用，在 App Store 找到 [ClozureCL](https://ccl.clozure.com/) （通常简称为CCL）。
- 其他系统请选择 [SBCL](http://www.sbcl.org/)。

> - Clozure 和 Clojure 是完全不同的，请别混搅了。
> - 您可能听说过 CLISP，它可能是你所需要的。但并非如此，CLISP 虽然是另一种 Lisp 实现，但它出现的时间并不长，没有 CCL 或 SBCL 普及，如果我们遇到问题的时候可能很难找到解决方案。

**选择一款编辑器**

可能我们第一个想到的便是 Emacs，但并不一定非 Emacs 不可，我们可以使用适合自己的任何一款编辑器。MacOS 中的 CCL 绑定了一款文本编辑器。

Emacs, Vim, Sublime Text, Atom, 不管怎样，哪一款都可以，它们都可以支持括号匹配、高亮代码以及自动缩排代码。

**第一个Lisp**

我们来编写文件 `hello.lisp`:
```lisp
(defun hello()
  (write-line "What is your name?")
  (let ((name (read-line)))
	(format t "Hello, ~A.~%" name)))
```

别担心看不懂代码，我们在这里只需要确人环境是能正常工作的。

打开 SBCL 或 CCL，加载上面的文件并运行其中的函数：

```
$ sbcl
* (load "hello.lisp")

T
* (hello)
What is your name?
Steve
Hello, Steve.
NIL
*
```

**资料介绍**

我所知道的 Common Lisp 入门书籍是 *[Common Lisp: A Gentle Introduction to Symbolic Computation](https://www.cs.cmu.edu/~dst/LispBook/)*。全书贯穿习题，让我们很容易理解：
- 如何记住众多的函数名？
- 为什么很少使用字符串？
- 什么时候应该使用这烦人的引号？

**实践**

接下来要介绍的一本书是 *[Practical Common Lisp](http://www.gigamonkeys.com/book/)*。我们也应该适应于在 *[the Common Lisp language specification](http://www.lispworks.com/documentation/lw70/CLHS/Front/Contents.htm) 查找规范，它是 Common Lisp 的终极指南。

**该有所行动了**

阅读了上面两本书，是时候该做些什么了，我们不需要进行一些大型的项目，可以尝试写一些 Lisp 程序。如果愿意：
- 解决 [Project Euler](https://projecteuler.net/) 问题。
- 做一些 [Advent of Code](https://adventofcode.com/) 练习。
- 弄一个[简单的推特机器人](https://twitter.com/git_commands)。
- 写一款私人日历来记录日程，查看天气预报等。
- 使用 [Sketch](https://github.com/vydd/sketch) 在 [Coding Math videos](https://www.youtube.com/user/codingmath/videos) 实现一些东西。

不管您实现了什么，关键在于是您自己实现的。

**把Lisp当做一个系统**

现在是时候把您的 Common Lisp 技能提升一个等级了，前面说了可以使用任意编辑器来写代码是因为那样能积累很多经验，但现在是时候跳入“深渊”了。

大多数语言的开发过程看起来像这样：
1. 用编辑器编写项目代码。
1. 编辑项目（一些语言跳过这一步）。
1. 运行项目（或者测试）。
1. 观察输出（在终端，或浏览器，等等）。
1. 继续第一步。

但这不是大多数 Common Lisp 用户的交互方式，在 Common Lisp 中是这样循环的：
1. 开始一个 Lisp 过程。
1. 加载项目。
1. 在编辑器中编写代码。
1. 告诉运行的进程只编译您编辑的代码。
1. 在进程中与变化的代码交互，通过 REPL, HTTP请求等等。
1. 观察输出（在终端，或浏览器，等等）。
1. 继续第三步。

当你拥抱 Lisp 的工作方式时，很少会重新编译和重新加载整个项目。通常你会写一个函数(或一个宏,或者参数,等等),编译的仅仅是函数,也许有点 REPL，然后继续下一个函数。相比传统的 compile-everything-then-run 是有优势的。

首先，提高了编译速度，你不必等待编译器,所以你的注意力/思维过程从来没有时间游荡。

其次，当你回到你的编译(或运行)的结果时,任何错误或警告几乎可以让你确定是哪几行代码编译。

编写 Common Lisp 感觉是与生活交互，共同呼吸，又像一个[很好的教学助理](https://www.reddit.com/r/lisp/comments/4oo1cp/common_lisp_for_clojure_programmer/d4eec68/)。

这种哲学的 Lisp 不仅仅是编程语言，而是生活，呼吸编程[系统](https://www.dreamsongs.com/Files/Incommensurability.pdf)也不仅仅是简单的反馈和交互式 REPL。

举个例子：设想在玩一款视频游戏，偶尔会导致除零的错误伤害计算。假设你在这个特定的任务上，即将开始游戏并加载保存的文件，并按步骤开始，在追求杀死最后一个怪物,你打损伤缺陷!在传统的语言,会发生两件事情：
1. 游戏崩溃，你发现一个堆栈跟踪，也许是核心的。
1. 你包装在一个 `try` 块主要游戏循环日志和忽略错误堆栈跟踪,让比赛继续。

第一种情况相当糟，你必须试着追踪bug的快照时什么东西看起来像(堆栈跟踪和核心转储)。即使你能修复它,现在你必须重做所有打回到测试最初致力于追求代码。

第二种情况也很糟，如果你只是忽略错误,游戏现在可能处于一种奇怪的状态。你也可能会失去一些重要的上下文需要调试的问题，除非你还保存核心转储（但我不知道许多人节省的核心转储每个例外）。
