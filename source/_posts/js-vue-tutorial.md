---
title: Vue.js Tutorial
date: 2018-02-11 12:31:48
categories: javascript
tags: [vue]
---
> http://vegibit.com/vue-js-tutorial/#twowaydatabinding

Vue.js类似其他流行的Javascript框架，例如React.js，Rivets.js，Reactive.js，Angular.js以及Knockout.js。这些框架提供了构造交互式网页应用的轮廓。Raw HTML扮演模板语言，这些框架扩展其语法以提供数据绑定和用户接口数据模型。Vue.js力求通过框架使工作变得简单化和函数化。让我们通过一篇简单的教程来了解它是如何工作的，并为我们提供了些什么。

# 创建Vue实例
我们使用Vue工作之前，需要初始化一个Vue实例，或者视图模型，对应于包含特殊元素的节点。我们创建一个`div`，并对齐赋予`id`属性，在Vue实例中使用*css*选择器将两者绑定：
```html
<div id='myVueInstance' class='container'>
  <!--  build out all kinds of things here -->
</div>
```
```javascript
var viewModel = new Vue({
  el:'#myVueInstance'
});
```

# 绑定数据的两种方式
选择我们拥有一个魔法强大经过Vue绑定的的`div`，我们可以随意的在该div里面添加元素。我们演示两种绑定方式的特性，我们在文本输入框中使用`v-model`指令，可以认为这些简单的指令是html的属性。事实上，我们使用这些指令扩展了html。具体可以参看[VueJS关键部分——指令](http://vegibit.com/vue-js-directives/)


