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
使用`v-model`可以实现表单输入和应用状态之间的双向绑定。一般是在`input`,`textarea`,`select`标签下使用。
```html
<input type='text' v-model='message'>
<span>Your message is : {{ message }}</span>

<script async src="//jsfiddle.net/keer2345/tpkon0es/1/embed/result,html,js,css/light/"></script>

## 循环`v-for`
```html
<ul>
  <li v-for='list in lists'>{{ list }}</li>
</ul>
```

<script async src="//jsfiddle.net/keer2345/tpkon0es/6/embed/result,html,js,css/light/"></script>

### 更多的`v-model`特性
- `v-model.lazy` 惰性更新状态
- `v-model.term` 将前后空格去掉。

### 更多的`v-model`使用
<script async src="//jsfiddle.net/keer2345/tpkon0es/12/embed/result,html,js,css/light/"></script>

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

<script async src="//jsfiddle.net/keer2345/tpkon0es/10/embed/result,html,js,css/light/"></script>

> `v-on`可以简写成`@`，比如`<button @click='onClick'>Click Me</button>`。

## 控制流指令`v-if`
```html
<div v-if="role='admin' || role=='super_admin'">
  I am a Administrator.
</div>
<div v-else-if="role=='hr'">
  ...
</div>
<div v-else>
  ...
</div>
```

## 计算属性`computed`
比如计算各科成绩的总分以及平均分：
```html
Math:
<input type='text' v-model.number='math'>
English:
<input type='text' v-model.number='english'>
Physics:
<input type='text' v-model.number='physics'>
Sum: {{ sum }}, Average: {{avg}}.
```

```javascript
var app = new Vue({
  el:'#app',
  data:{
    math:90,
    english:80,
    physics:70,
  },
  computed:{
      sum: function(){
        return parseFloat(this.math) + this.english + this.physics;
      },
      avg: function(){
        return Math.round(this.sum / 3);
      },
  }
})
```

## 组件`component`
### 全局组件与局部组件

<script async src="//jsfiddle.net/keer2345/tpkon0es/14/embed/result,html,js,css/light/"></script>

### 组件的传参(父子通信)：
<script async src="//jsfiddle.net/keer2345/tpkon0es/15/embed/result,html,js,css/light/"></script>

### 组件的传参(子父通信)：
<script async src="//jsfiddle.net/keer2345/tpkon0es/18/embed/result,html,js,css/light/"></script>
