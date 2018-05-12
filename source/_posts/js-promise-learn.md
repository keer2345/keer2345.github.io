---
title: js promise learn
date: 2018-05-12 09:28:09
categories: javascript
tags: [javascript, promise, es6]
---

> https://medium.com/javascript-scene/master-the-javascript-interview-what-is-a-promise-27fc71e77261

# 什么是Promise

```javascript
typeof new Promise((resolve, reject) => {}) === 'object' // true
```

*Promise*是一个Object，其可能在未来某个时间产生一个值：既可能是已解决（`resolved`）的值，也可能是未解决的（例如，出现了网络错误）。

*Promise*有三种状态：
- *fulfilled*
- *rejected*
- *pending*

确保用户可以捕捉毁掉处理已完成的值（`fulfilled`）或者被拒绝的原因（`rejection`）。

一旦*promise*构造函数被调用，Promise忠诚的处理任何任务。如果您需要懒惰式的话，可以了解以下[observables](https://github.com/Reactive-Extensions/RxJS)或[tasks](https://github.com/rpominov/fun-task)。

<!-- more -->

# 一份不完整的Promise历史
早起的Promise实现以及特性（或者类似想法）起始于诸如早在1980现代的*MultiLisp*和*Concurrent Prolog*语言。*promise*一词于1988年被Barbara Liskov 和 Liuba Shrira引用。

第一次听说promise时，是全新的*Node*社区在讨论一种最好的方式来实现异步行为。社区尝试使用promise，但是最终选定*Node*标准的*error-first*回调。

大约在同一时间，*Dojo*在其延时API中添加了promise。日益增长的兴趣和活动最终有了新的*Promises/A*规范设计以实现各种promise的彼此协作。

*jQuery*的异步行为围绕Promise重构，它对promise的支持与*Dojo*延时很相似，并迅速变成了一度最普遍的promise实现。然而，它不支持两个通道（fulfilled / rejected）的链接行为和异常管理，这是我们期望在promise上构建的工具。

尽管有这些弱点，*jQuery*官方让promise成为主流，并有更好的独立于promise的库，类似 *Q*，而且 *Bluebird* 变得很受欢迎。*jQuery*的实现与promise规范的一些重要分类不兼容，后者重写了*[Promises/A+](https://promisesaplus.com/)*规范。

ES6让这一规范顺从Promise，并且一些重要的APIs建立在新标准的Promise支持：尤其是[WHATWG Fetch](https://fetch.spec.whatwg.org/)规范和[ Async Functions](https://tc39.github.io/ecmascript-asyncawait/)标准。

这里描述的Promise是兼容于*Promises/A+*规范，并关注于ES6标准的Promise实现。

# Promises如何工作

Promise是一个对象，可以从异步函数中返回同步结果，重申一遍，其有3种状态：
- **Fulfilled:** `onFulfilled()`将被调用（例如`resolve()`被调用）。
- **Rejected:** `onRejected()`将被调用（例如`reject()`被调用）。
- **Pending:** 还未履行或者被拒绝。


如果一个promise不是在等待中的话，其被固化下来（状态要么是*履行*或*拒绝*）。有时我们使用*resolved*和*settled*表示同样的意思：*非等待*。

Promise一旦固化，将不能再重新固化，再次调用`resolve()`或`rejecte()`将毫无影响，已固化的Promise的不可变是其很重要的一个特性。

传统的JavaScript Promise没有暴露promise状态。反而，我们期望把promise当做黑盒子，只有函数负责创建promise并熟知promise状态，或访问解决，或拒绝。

这是一个返回promise的函数，其将在特定延时后解决（resolve）：
```javascript
const wait = time => new Promise((resolve) => setTimeout(resolve, time));

wait(3000).then() => console.log('Hello Promise!'); // 'Hello Promise!'
```
上面的代码等价于：
```javascript
const wait = function(time) {
    return new Promise(function(resolve) {
        setTimeout(resolve, time);
    })
}

wait(3000).then(function() {
    console.log('Hello Promise!');
})
```

这段代码将等待3秒钟，然后打印`Hello Promise!`。所有的Promise规范定义了`.then()`方法，我们可以用以处理resolved或者rejected的值。

ES6 Promise的构造函数接受一个函数，其带有两个参数：`resolve()`和`reject()`。在上面的例子中，我们只是用了`resolve()`，因此我们在参数列表中避开了`reject()`。接着我们调用`setTimeout()`来创建延迟，并在延迟结束时调用`resolve()`。

我们可以选择`resolve()`或`reject()`使用值，这些值将传递给附带的回调函数`.then()`。

当`reject()`有值时，我们总是传递`Error`对象，一般来说，我们需要两种可能的解析状态：正常愉快的途径，或者异常——阻止正常愉快途径发生的任何事情。传递`Error`对象来明确的表明。

# 重要的Promise规则
标准的Promise遵循[Promises/A+ specification](https://promisesaplus.com/implementations)规范。基于此标准有许多promise实现，包括JavaScript标准的ECMAScript promises。

Promise必须遵循一组特定的规则：
- promise或者是thenable是一个提供了标准的`then()`方法的对象。
- Pending Promise能转换成已履行或者被拒绝的状态。
- fulfilled或者rejected promise一旦固化，将不能转换成任何其他状态。
- 一旦promise被固化，它必须有值（也可能是`undefined`），该值不能再做改变。

在上下文中的改变是指一致性比较（`===`），一个对象可能被用于已履行的值，并且对象属性可能变异。

每一个promise必须提供一个具有以下样式的`.then()`方法：
```javascript
promise.then(
  onFulfilled?: Function,
  onRejected?: Function
) => Promise
```
