---
title: Vue with Allgator 01
date: 2018-04-15 21:02:18
categories: vue
tags: vue
---

1. Start
1. v-if
1. v-for

<!-- more -->

# 开始

> https://alligator.io/vuejs/hello-world-vuejs/

> Vue.js作为一个响应组件快速地流行起来，它是轻量级的，易于掌握的，并且是类似于React和Angular的又一个很好的选择。


让我们从一个最简单的例子开始：
```html
<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width" />
    <title>Hello World in Vue.js</title>
</head>

<body>
    <div id="app">
        <h1>{{ msg }}</h>
    </div>
</body>

<script src="https://unpkg.com/vue"></script>
<script>
    var vue = new Vue({
        el: "#app",
        data() {
            return {
                msg: "hello world!!!"
            }
        }
    });
</script>

</html>
```

# 条件指令

> https://alligator.io/vuejs/conditional-directives/

- `v-if`
- `v-else`
- `v-else-if`
- `v-show`

## v-if
继续上节的例子，我们添加如下代码：
```javascript
data() {
  return {
    msg: "Hello World!",
    isLoggedIn: false
  }
}
```
```html
<button v-if="isLoggedIn">Logout</button>
```

## template元素

`v-if`指令只能显示或隐藏元素及其子元素，但它可以通过一个指令控制多个元素:
```html
<template v-if="isLoggedIn">
  <label> Logout </button>
  <button> Logout </button>
</template>
```

## v-else
```html
<button v-if="isLoggedIn"> Logout </button>
<button v-else> Log In </button>
```

## v-else-if
```html
<button v-if="isLoggedIn"> Logout </button>
<label v-else-if="isLoginDisabled"> Register disabled </label>
<button v-else> Log In </button>
```

## v-show
`v-show`与`v-if`很相似，主要区别在于：
- `v-if` 表达式为真时才渲染元素到DOM
- `v-show` 渲染所有元素到DOM，当条件为*false*时用CSS属性来隐藏元素
- `v-show`不支持`v-else`, `v-else-if`

# 使用v-for迭代项目

> https://alligator.io/vuejs/iterating-v-for/

## v-for

### 基本使用

```javascript
data() {
  return {
    messages:  ['hello', 'vue', 'js'],
    shoppingItems: [
      {name: 'apple', price: '10'},
      {name: 'orange', price: '12'}
    ]
  }
}
```
```html
<ul>
  <li v-for="msg in messages">{{ msg }}</li>
</ul>

<ul>
  <li v-for="item in shoppingItems">
    {{ item.name }} - {{ item.price }}
  </li>
</ul>
```

### template元素
```html
<template v-for="item in shoppingItems">
  <label> {{ item.name }} </label>
  <label> {{ item.price }} </label>
  <button>Buy</button>
</template>
```

### 在对象中使用
```javascript
data() {
  return {
    objectItems: {
      key1: 'item1',
      key2: 'item 2',
      key3: 'item3'
    }
  }
}
```


```html
<ul>
  <li v-for="item in objectItems">{{ item }}</li>
</ul>

<ul>
  <li v-for="(item, key, index) in objectItems">
    {{ item }} - {{ key }} - {{ index }}
  </li>
</ul>
```
### 通过Range使用v-for
```html
<ul>
  <li v-for="item in 15">{{ item }}</li>
</ul>
```

## Key
改变数组的顺序时,默认情况下Vue会改变每个现有元素中的数据,而不是将DOM元素移动到更新的位置。

我们可以设置Vue跟踪每个元素使用一个*key*，这将使它移动元素而不是替换值。这些值对于被迭代的数据来说是独一无二的。
```html
<ul>
  <li v-for="item in shoppingItems" :key="item.name">
    {{ item.name }} - {{ item.price }}
  </li>
</ul>
```

## 管理变化
开箱即用的*v-for*支持数组突变的方法，它们是`push`,`pop`,`shift`,`unshift`,`splice`,`sort`,`reverse`。如果在数组上执行这些操作，*v-for*将展现处理后的新数据。

## 过滤列表
### 使用计算值来过滤
```html
<ul>
  <li v-for="item in itemsLessThanTen" :key="item.name">
    {{ item.name }} - {{ item.price }}
  </li>
</ul>
```
```javascript
data() {
  return {
    shoppingItems: [
      {name: 'apple', price: '7'},
      {name: 'orange', price: '12'}
    ]
  }
},
computed:{
  itemsLessThanTen: function() {
    return this.shoppingItems.filter(function(item) {
      return item.price > 10;
    })
  }
}
```

### 使用方法过滤
```html
<ul>
  <li v-for="item in filterItems(shoppingItems)" :key="item.name">
    {{ item.name }} - {{ item.price }}
  </li>
</ul>
``````

```html
data() {
  return {
    shoppingItems: [
      {name: 'apple', price: '7'},
      {name: 'orange', price: '12'}
    ]
  }
},
methods:{
  filterItems: function(items) {
    return items.filter(function(item) {
      return item.price > 10;
    })
  }
}
```
