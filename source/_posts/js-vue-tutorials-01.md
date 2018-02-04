---
title: Vue Tutorials 01
date: 2018-02-02 21:04:03
categories: javascript
tags: [javascript,vue]
---

# 初步
## 插入文本
- `{{ message }}`
- `<span v-text='message'></span>`
- `<span v-html='message'` HTML转义

```html
<script src="https://unpkg.com/vue"></script>

<div id = 'app'>
  <h2><span v-text='product'></span>, Vue.js</h2>
</div>

<script>
var app = new Vue({
  el: '#app',
  data: {
    product: 'Cherry'
  }
})
</script>
```

我们可以在Google Chrome浏览器的终端改变`product`的值：
```
Console:   app.product = 'Oxheart'
```

<!-- more -->

## 条件控制`v-if`
```html
<div id='app'>
  <span v-if='seen'>Now we can seen it</span>
</div>
```
对应的vue:
```javascript
var app = new Vue({
  el: '#app',
  data: {
    seen:true
  }
})
```
## 循环列表`v-for`
- `<li v-for='list in lists'> ... `

```html
<script src="https://unpkg.com/vue"></script>

<div id = 'app'>
  <ul>
    <li v-for='product in products'>
      <span v-text='product'/>
    </li>
  </ul>
</div>

<script>
var app = new Vue({
  el: '#app',
  data: {
    products: [
      'Blueberry','Spanish Cherry','Mangosteen'
    ]
  }
})
</script>
```

## 处理输入
### `v-on`
使用`v-on`触发Vue方法, 可以将其简化成`@`，比如`@click`。
```html
<div id = 'app'>
  <p>{{ message }}</p>
  <button v-on:click='reverseMessage'>Reverse Message</button>
</div>
```
```javascript
var app = new Vue({
  el: "#app",
  data: {
    message: "Hello Vue!"
  },
  methods: {
    reverseMessage: function() {
      this.message = this.message.split('').reverse().join('');
    }
  }
});
```
### `v-model`
使用`v-model`可以实现表单输入和应用状态之间的双向绑定。
```html
<div id = 'app'>
  <p>{{ message }}</p>
  <input v-model='message'></input>
</div>
```
```javascript
var app = new Vue({
  el: "#app",
  data: {
    message: "Hello Vue!"
  }
});
```
### `v-bind`
`v-bind`通常可以简化，比如`v-bind:href`可以简化成`:href`。
```html
<div id='app2'>
  <a v-bind:href='href'>hello</a>
  <a :href='href'>hello</a>
</div>
```
```javascript
var app2 = new Vue({
  el:'#app2',
  data:{
    href:'https://www.baidu.com'
  }
});
```
