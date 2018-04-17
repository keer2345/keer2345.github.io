---
title: Vue with Allgator 02
date: 2018-04-15 23:27:13
categories: vue
tags: vue
---

1. Template
1. Computed properties

<!-- more -->

# Template

> https://alligator.io/vuejs/templating/

> 模板允许从模型中绑定值到视图。在Vue中，可以使用简单的HTML绑定。

从第二版起，Vue.js允许JavaScript模板添加到HTML，这一节将介绍HTML模板。

我们使用以下数据模型：
```javascript
data() {
  return {
    text: 'Hello world',
    htmlContent: 'Hello bold new world!',
    isDisabledAttr: true
  }
}
```

## 插值Interpolation
插值是指一个或多个变量的行为，计算值并在模板中展现结果。它可以简单得像`hello World`的字符串，也可以处理复杂表达式的结果。

### 文本
最简单的是使用两个花括号，创建模型到模板的单向绑定`one-way`，模板中显示原始数据。
```html
<span>Text binding: {{ text }}</span><br>
```

### 一次绑定
使用`v-once`指令来绑定和停止跟踪项目。使用`v-once`意味着以后改变模型中的值不会影响模板中显示的值，如果`text`的值在后期被改变，`span`标签仍然显示初始值。
```html
<span v-once>Text binding: {{ text }}</span><br>
```

### 属性
```html
<button v-bind:disabled="isDisabledAttr">
  Disabled
</button>

<button :disabled="isDisabledAttr">
  Disabled
</button>
```

### JS表达式
```html
<span>{{ text + ' ' + isDisabledAttr }}</span>
```

## 过滤器filter
```javascript
filters: {
  allCaps: function (value) {
    if (!value) return '';
    value = value.toString();
    return value.toUpperCase();
  }
}
```
```html
<span>{{ text | allCaps }}</span>
```


# 介绍计算属性

> https://alligator.io/vuejs/computed-properties/

> **Computed properties**能用于快速计算属性以便展示在视图上。

其实在Vue.js中有多种方式可以为视图设置值。比如直接绑定，使用JS表达式，或者过滤器转换内容。另外，我们还可以使用计算属性`computed properties`来计算基于数据模型中一个值或者一组值显示的值。

## 计算属性
计算属性允许模型相关，复制的视图值计算。这些值将被绑定到依赖项的值,只在需要时更新。

例如，我们有这样一个数组模型：
```javascript
data() {
  return {
    results: [
       {
          name: 'English',
          marks: 70
        },
        {
          name: 'Math',
          marks: 80
        },
        {
          name: 'History',
          marks: 90
        }
      ]
  }
}
```

假设我们需要查看分数合计，我们可以使用`filter`或者`expression`来完成。
- `filter` 用于简单的数据格式和所需应用程序中的多个位置。
- `expression` 不允许使用流操作或其他复杂的逻辑。他们应该保持简单。

```javascript
computed: {
  totalMarks: function() {
    let total = 0;
    for(let i = 0; i < this.results.length; i++){
      total += parseInt(this.results[i].marks);
    }
    return total;
  }
}
```

```html
<div id="app">
  <div v-for="subject in results">
    <input v-model="subject.marks">
    <span>
      Marks for {{ subject.title }}: {{ subject.marks }}
    </span>
  </div>

  <span>
    Total marks are: {{ totalMarks }}
  </span>
</div>
```

> 更详细的源代码可以在这里查看https://jsfiddle.net/Alligatorio/zugwb7mf/
