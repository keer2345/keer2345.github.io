---
title: Strategic Scala Style: Principle of Least Power
date: 2020-01-22 21:54:47
tags: Scala
---

> http://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html

Scala 语言庞大而复杂，提供了大量的工具而使得开发者在同样的事情上有多重处理方式，尽可能的解决所有问题。开发者如何选择使用什么方式呢？这篇文章首先 提供了战略性（Strategic）的方式。

<!-- more -->

# 关于Strageic Scala Style
本篇文章基于我的学习经验，假设你已经知道了 Scala 语言的大多数特性并且能使用它们，并将注意力集中在如何选择更好的解决方案。它纯粹的使用“Vanilla Scala”以及它们的标准库特性和 API 。你将不会学到任何其他的东西，例如 Akka, Scalaz 等。

毫无疑问，我们来自 "Monadic" 或 "Reactive" 或 "Scala.js"。然而，期望综合文档仍然是足够广泛适用的。

# Philosophy: Principle of Least Power
本节的原则是：

> 选择解决方案，选择最强的解决方案来解决问题

开发者们尝试创建强大、灵活的解决方案。然而，强大、灵活的解决方案是很难分析的。

> 选择一种有元，强大而又能解决问题的语言。

# Complexity is your Enemy
最常见的抱怨是适用 Scala 的开发者因其难以阅读和复杂而困扰。我不会快速的将编译，但是考虑到抱怨，使代码“易于阅读”和“减少抱怨”应当优先于开发者所适用的语言。编程语言之间不会有“更好”或者“更糟”的说法。

回到 Scala，开发者们应当让代码“易于阅读”和“减少困扰”。幸运的是，Scala 提供了这样的辅助工具。

# Don't Fear Refactoring

