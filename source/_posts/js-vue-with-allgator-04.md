---
title: Vue with Allgator 03
date: 2018-04-17 21:14:50
categories: vue
tags: vue
---

1. 自定义Vue插件
1. 集成RxJS和Vue.js
1. 添加`v-model`来支持Vue.js组件
1. 理解Vue.js组件实例化

<!-- more -->

# 自定义Vue插件
> https://alligator.io/vuejs/creating-custom-plugins/

Vue.js插件强大而又很容易添加到我们应用的全局特性中。其拥有各种各样的用途，从分发应用的组件到诸如路由和不可变数据存储的外围功能。

Vue.js插件是相当简单的，通过`install`方法安装。该方法带有两个参数：全局的`Vue`对象，和包含用户自定义选项的对象。然而，一个插件系统这么简单还可以产生相当大的影响。

## 我们的第一个插件

作为介绍的第一个插件，我们将编写一个基本的插件来实现当一个组件被每次加载到DOM时写入控制台。

*my-vue-plugin.js*:
```javascript
// This is your plugin object. It can be exported to be used anywhere.
const MyPlugin = {
  // The install method is all that needs to exist on the plugin object.
  // It takes the global Vue object as well as user-defined options.
  install(Vue, options) {
    // We call Vue.mixin() here to inject functionality into all components.
  	Vue.mixin({
      // Anything added to a mixin will be injected into all components.
      // In this case, the mounted() method runs when the component is added to the DOM.
      mounted() {
        console.log('Mounted!');
      }
    });
  }
};

export default MyPlugin;
```
这个组件现在可以用于Vue应用，通过导入并调用`Vue.use(MyPlugin)`。
*main.js*:
```javascript
import Vue from 'vue'
import MyPlugin from './my-vue-plugin.js'
import App from './App.vue'

// The plugin is loaded here.
Vue.use(MyPlugin)

new Vue({
  el: '#app',
  render: h => h(App)
});
```
您可能会想，为什么不直接在*main.js*调用`Vue.mixin()`呢？原因在于我们在Vue添加了全局的函数而没有直接修改APP，这是几乎总是最好的分裂成一个单独的模块,可以添加或删除。这也使得插件非常容易分配。

## 添加功能
### 安装APP组件和指令
如果希望将打包的组件和指令作为插件发布，可以这样写：
```javascript
import MyComponent from './components/MyComponent.vue';
import MyDirective from './directives/MyDirective.js';

const MyPlugin {
  install(Vue, options) {
    Vue.component(MyComponent.name, MyComponent)
    Vue.directive(MyDirective.name, MyDirective)
  }
};

export default MyPlugin;
```

### 修改全局的Vue对象
可以从插件将Vue的全局对象修改成期望的：
```javascript
const MyPlugin {
  install(Vue, options) {
    Vue.myAddedProperty = 'Example Property'
    Vue.myAddedMethod = function() {
   	  return Vue.myAddedProperty
    }
  }
};

export default MyPlugin;
```
### 修改Vue实例
直接向组件实例添加一个属性或方法没有任何注入机制,您可以修改Vue原型如下所示:
```javascript
const MyPlugin {
  install(Vue, options) {
    Vue.prototype.$myAddedProperty = 'Example Property'
    Vue.prototype.$myAddedMethod = function() {
   	  return Vue.myAddedProperty
    }
  }
};

export default MyPlugin;
```
这些属性将被添加到所有组件和Vue实例。

### 添加组件周期勾子或属性
如上所示的第一个插件例子，您可以添加生命周期钩子,通过使用Mixin修改Vue组件。
```javascript
const MyPlugin = {
  install(Vue, options) {

  	Vue.mixin({
      mounted() {
        console.log('Mounted!');
      }
    });

  }
};

export default MyPlugin;
```
## 自动安装
想让其他人使用您的插件，通常期望您的插件包含Vue标签，能自动安装而不需要调用`Vue.use()`，我们可以在最后添加以下几行来实现：
```javascript
// Automatic installation if Vue has been added to the global scope.
if (typeof window !== 'undefined' && window.Vue) {
  window.Vue.use(MyPlugin)
}
```
## 发布插件
一旦完成插件并准备发布到社区，以下步骤可以让人们发现您的插件：
- 开源并分发文件到NPM和GITHUB。
- 推送到官方的awesome-vue，很多人从这儿寻找插件。
- 提交到Vue Forum, Vue Gitter Channel和Twitter。

# 集成RxJS和Vue.js
> https://alligator.io/vuejs/using-rxjs/

## 了解RxJS
- [The introduction to Reactive Programming you've been missing](https://gist.github.com/staltz/868e7e9bc2a7b8c1f754)
- [知乎](https://www.zhihu.com/question/53307578)

# 添加`v-model`来支持Vue.js组件
> https://alligator.io/vuejs/add-v-model-support/

# 理解Vue.js组件实例化
> https://alligator.io/vuejs/component-instancing/
