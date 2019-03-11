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
