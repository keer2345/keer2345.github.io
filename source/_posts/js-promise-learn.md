---
title: js promise learn
date: 2018-05-12 09:28:09
categories: javascript
tags: [javascript, promise, es6]
---

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
