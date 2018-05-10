---
title:  6 Reasons Why JavaScript’s Async/Await Blows Promises Away
date: 2018-05-07 17:58:48
tags: [javascript, es6, asynchronous]
---
> https://hackernoon.com/6-reasons-why-javascripts-async-await-blows-promises-away-tutorial-c7ec10518dd9

# Async/await 101
- Async/await is a new way to write asynchronous code. Previous options for asynchronous code are callbacks and promises.
- Async/await is actually built on top of promises. It cannot be used with plain callbacks or node callbacks.
- Async/await is, like promises, non blocking.
- Async/await makes asynchronous code look and behave a little more like synchronous code. This is where all its power lies.

# Syntax

This is how you would implement it using promises
```javascript
const makeRequest = () =>
  getJSON()
    .then(data => {
      console.log(data)
      return "done"
    })

makeRequest()
```

And this is how it looks with async/await
```javascript
const makeRequest = async () => {
  console.log(await getJSON())
  return "done"
}

makeRequest()
```

