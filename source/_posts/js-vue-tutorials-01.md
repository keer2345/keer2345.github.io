---
title: Vue Tutorials 01 -- 介绍
date: 2018-02-07 21:04:03
categories: javascript
tags: [vue]
---
# 引用Vue
```html
<script src="https://unpkg.com/vue"></script>
```
或者通过其他CDN引用，比如`http://www.bootcdn.cn/`。

<!-- more -->

# 介绍
## 插入文本
- `{{ message }}`
- `<span v-text='message'/>`
- `<div v-html='message'/>`

## 双向绑定`v-model`
使用`v-model`可以实现表单输入和应用状态之间的双向绑定。
```html
<input type='text' v-model='message'>
<span>Your message is : {{ message }}</span>
```
<script async src="//jsfiddle.net/keer2345/tpkon0es/1/embed/result,js,html,css/light/"></script>

## 循环`v-for`
```html
<ul>
  <li v-for='list in lists'>{{ list }}</li>
</ul>
```

<script async src="//jsfiddle.net/keer2345/tpkon0es/6/embed/result,js,html,css/light/"></script>

## 属性绑定`v-bind`

```html
<div id='app'>
  <a v-bind:class='{active: isActive}' v-bind:href='url'>
    Home
  </a>
  <img v-bind:src='img'/>
</div>
```
> `v-bind`可以忽略，用冒号`:`代替，比如`<a :href='url'>Home</a>`。

## `v-on`

<script async src="//jsfiddle.net/keer2345/tpkon0es/10/embed/result,js,html,css/light/"></script>

> `v-on`可以简写成`@`，比如`<button @click='onClick'>Click Me</button>`。
