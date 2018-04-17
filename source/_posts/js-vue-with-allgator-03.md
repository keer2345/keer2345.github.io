---
title: Vue with Allgator 03
date: 2018-04-17 21:12:50
categories: vue
tags: vue
---

1. Events
1. 动态样式
1. `v-model`的两种方式

<!-- more -->

# Events

> https://alligator.io/vuejs/events/

## v-on
用户在视图中的交互可以触发DOM中的事件，比如`click`，`keyup`。我们使用`v-on`指令处理这些事件。


一个简单的例子，我们点击按钮实现递增的计数：
```javascript
data() {
  return {
    count: 0
  }
}
```
```html
<label>Count is: {{count}}</label>
<button v-on:click="count++">Increment</button>
```

### 绑定方法到 v-on
使用方法名将其绑定到事件：
```html
<input v-model="addValue">
<button v-on:click="addToCount">Add</button>
```
```javascript
methods: {
  addToCount: function() {
    this.count = this.count + parseInt(this.addValue);
  }
}
```

### v-on的简写方式
`v-on:`可以用`@`来代替。比如：
```html
<button @click="addToCount">Add</button>
```

## 修改事件
**event.preventDefault()**经常被调用，用于阻止浏览器的默认行为。例如：
```html
<a href="test" @click.prevent="addToCount">Add</a>
```

在Vue中以下修改是适用的：
- **stop** 防止事件冒泡的DOM树
- **prevent** 防止默认行为
- **capture** 捕获模式用于事件处理
- **self** 只触发事件本身
- **once** 仅运行函数一次

## 修改按键
我们使用`keyup`来处理按键事件：
```html
<input v-on:keyup.13="addToCount" v-model="addValue">
```
**13**表示回车键，抬起回车键后会触发*addToCount*事件。

Vue提供了预定义的一些按键，比如`enter`,`tab`,`delete`,`esc`,`space`和`left`。当然，也可以为按键设置别名：
```javascript
Vue.config.keyCodes.a = 65
```

# 动态样式

> https://alligator.io/vuejs/dynamic-styles/


# `v-model`的两种方式

> https://alligator.io/vuejs/v-model-two-way-binding/

