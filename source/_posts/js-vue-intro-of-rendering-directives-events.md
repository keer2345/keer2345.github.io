---
title: Intro of Vue - Rendering, Directives, Events
date: 2018-10-17 20:47:15
tags: vue
---

Article Series:
1. **Rendering, Directives, Events**
1. Components, Props, and Slots
1. Vue-cli
1. Vuex
1. Animations


<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/10/01.png)
</center>

<!-- more -->

# Rendering, Directives, Events

> https://css-tricks.com/intro-to-vue-1-rendering-directives-events/

## 从零开始
```html
<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>Document</title>
</head>

<body>

    <div id="app">
        {{message}}
    </div>

    <script src="https://cdn.jsdelivr.net/npm/vue@2.5.17/dist/vue.min.js"></script>
    <script>
        var app = new Vue({
            el: '#app',
            data: {
                message: 'Hello Vue!'
            }
        })
    </script>

    <style>
        body {
            font-family: 'Bitter', serif;
        }

        #app {
            text-align: center;
            padding: 70px;
            font-size: 22px;
            max-width: 360px;
            margin: 0 auto;
            display: table;
        }
    </style>
</body>

</html>
```

## 条件渲染
### v-for
```html
<div id="app">
    <ul>
        <li v-for="item in items">
            {{ item }}
        </li>
    </ul>
</div>

<script src="https://cdn.jsdelivr.net/npm/vue@2.5.17/dist/vue.min.js"></script>
<script>
    var app = new Vue({
        el: '#app',
        data: {
            items: [
                'thingie',
                'another thingie',
                'lots of stuff',
                'yadda yadda'
            ]
        }
    })
</script>
```

是不是相当地简洁明了？

### v-model
```html
<div id="app">
    <h3>Type here:</h3>
    <textarea v-model="message" class="message" row='5' maxlength="72"></textarea><br />
    <p class="booktext">{{message}}</p>
</div>
<script src="https://cdn.jsdelivr.net/npm/vue@2.5.17/dist/vue.min.js"></script>
<script>
    var app = new Vue({
        el: '#app',
        data: {
            message: 'This is a good place to type things'
        }
    })
</script>
```
### 其它指令

- `v-if, v-else-if, v-else`
- `v-bind`
- `v-on`
- `v-model`
- `v-pre`
- `v-once`
- `v-show`


也有一些很棒的时间修饰符（event modifiers）和加速开发的 API：
- `@mousemove.stop` 堪比 `e.stopPropogation()`
- `@mousemove.prevent` 类似 `e.preventDefault()`
- `@submit.prevent` 将在提交时不再重载页面
- `@click.once` 不能与 `v-once` 混淆，这里单击事件（click event）将被触发一次
- `v-model.lazy` 不会自动填充内容,它将等待绑定到事件发生


参看 [configure your own keycodes](https://vuejs.org/v2/api/#keyCodes)

接下来我们讨论更多的例子。

## 事件处理

### v-on

`v-on` 常常简写为`@`，比如 `v-on:click` 等价于 `@click`。

```html
<div id="app">
    <p><button @click="increment">Add</button></p>
    <p>{{count}}</p>
</div>
<script src="https://cdn.jsdelivr.net/npm/vue@2.5.17/dist/vue.min.js"></script>
<script>
    var app = new Vue({
        el: '#app',
        data: {
            count: 0
        },
        methods: {
            increment() {
                this.count++;
            }
        }
    })
</script>
```

`methods` 并非创建自定义函数的唯一方式，也可以使用 `watch`。主要区别在于 `methods` 常用于小的同步场景，而 `watch` 则在多任务、异步或响应中的高消耗中改变数据。我常在动画中使用 `watch`。

### v-bind
`v-bind` 可以简写为 `:`
```html
<div id="app" :style="{background:`hsl(${x},80%,50%)`}" @mousemove="xCoordinate">
    <p>
        <button @click="increment">+</button>
        {{counter}}
        <button @click="decrement">-</button>
    </p>
    <p>Pixels across: {{x}}</p>
</div>

<script src="https://cdn.jsdelivr.net/npm/vue@2.5.17/dist/vue.min.js"></script>

<script>
    var app = new Vue({
        el: '#app',
        data: {
            counter: 0,
            x: 0
        },
        methods: {
            increment() {
                this.counter++;
            },
            decrement() {
                this.counter--;
            },
            xCoordinate(e) {
                this.x = e.clientX;
            }
        }
    })
</script>
```
