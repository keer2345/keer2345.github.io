---
title: Vue Cli 3 Introduction
date: 2018-10-19 23:32:49
tags: [vue]
---

# Vue Cli 3 Introduction

**Vue CLI 3** 介绍，我们将使用其来生成前端应用。

我们将了解：
- 安装最新的 *Vue CLI 3*
- 使用新特性
- 创建前端应用
- 为开发和生产设置环境变量
- 添加代理指向 API 调用
- 手工安装及添加 *Vue CLI* 插件

[Vue CLI v3](https://github.com/vuejs/vue-cli) 为开发者提供了新的体验，允许我们很容易地生成零配置的 Vue 项目。一旦项目需要更多扩展的时候可以使用插件添加更多的配置选项，不像 **Create React App**，我们可以不通过 ejecting 而是通过 Vue CLI 插件来配置项目。

Vue CLI v3 提供了很多新特性，例如：
- 交互式的脚手架 `@vue/cli`
- 零配置快速原型 `@vue/cli` 和 `@vue/cli-service-global`
- 运行时依赖（`@vue/cli-service`）提供的众多特性：可升级、构建在 webpack 之上、拥有预设配置、可修改的配置文件、可扩展的插件、等等
- 官方插件支持使用强大的前端生态系统工具，所以我们不需要关注 webpack，只需要关注 Vue CLI 插件。

<!-- more -->

## 安装 Vue CLI v3
```
npm install -g @vue/cli
# or
yarn global add @vue/cli
```
## 插件应用
```
vue create frontend
```

将会看到许多特性的询问，例如 TypeScript 支持，添加 Vue Router，添加 Vuex 等等。

```
Vue CLI v3.0.0-beta.15
? Please pick a preset: Manually select features
? Check the features needed for your project: (Press <space> to select, <a> to t
oggle all, <i> to invert selection)
❯◉ Babel
 ◯ TypeScript
 ◯ Progressive Web App (PWA) Support
 ◯ Router
 ◯ Vuex
 ◯ CSS Pre-processors
 ◉ Linter / Formatter
 ◯ Unit Testing
 ◯ E2E Testing
```

选择我们的应用所需的特性，需要等待一段时间来生产和安装包。

## 运行服务
```
cd frontend

yarn serve
# or
npm run serve
```
会看到如下输出：
```
DONE  Compiled successfully in 2729ms                                  01:50:56


  App running at:
  - Local:   http://localhost:8080/ 
  - Network: http://192.168.1.11:8080/

  Note that the development build is not optimized.
  To create a production build, run npm run build.
```

通过浏览器访问 http://localhost:8080 ，将会看到我们运行中的应用。

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/10/06.png)
</center>

## 项目剖析
```
tree -I "node_modules"
```
```
.
├── babel.config.js
├── package.json
├── package-lock.json
├── public
│   ├── favicon.ico
│   └── index.html
└── src
    ├── App.vue
    ├── assets
    │   └── logo.png
    ├── components
    │   └── HelloWorld.vue
    └── main.js
```

- `babel.config.js`：包含 Babel （ES Transpiler 允许我们在那些尚未支持现代 JavaScript 的浏览器上使用这些新特性）配置。
- `package.json`：包含所需的 JavaScript 模块、项目信息和依赖。
- `public`：包含公开的访问文件，例如 `index.html` 和图标。
- `src`：我们将把大多数时间都花在这个目录上，包含了我们项目的 Vue 源文件。
- `src/main.js`：应用初始化的启动文件。
- `src/App.vue`：应用的主要组件。
- `src/assets`：包含静态资源。
- `src/components`：包含应用的组件。


## Vue CLI v3 脚本
Vue CLI v3 为开发和生产环境提供了很多零配置的 npm 脚本，以及诸如热代码、模块重载等大量特性。

打开 `package.json` 会看到很多不一样的脚本：
```javascript
# ...
  "scripts": {
    "serve": "vue-cli-service serve",
    "build": "vue-cli-service build",
    "lint": "vue-cli-service lint"
  },
```

比如：
```
npm run build
npm run lint
```

## 环境变量
我们可以添加 `.env` 文件并采取以下结构使用环境变量：
```
VUE_APP_DEBUG=true
...
```

> 环境变量应该以 `VUE_APP_` 开头。

Vue CLI 将通过 `process.env` 来加载我们定义的环境变量。例如，通过 `process.env.VUE_APP_DEBUG` 访问 `VUE_APP_DEBUG` 变量。

我们可以通过给环境变量文件的名称添加恰当的后缀来定义环境变量来指定诸如*开发环境*或者*生产环境*，例如：
- `.env.development`：开发环境
- `.env.production`：生产环境

> `.env` 中的环境变量会覆盖 `.env.development` 或 `.env.production` 等文件中相同的变量。

## 使用代理
当从前端应用调用 API 时，我们需要使用代理来调用以避免跨域资源共享（CORS）相关的问题。Vue CLI v3 提供了内建特性来使用代理。

我们可以在 `package.json` 中添加一个 **proxy** 对象来很方便的配置代理。例如：
```javascript
{
  "proxy": {
    "/api": "http://localhost:8000"
  }
}
```

因此，如果我们需要让 API 调用运行在 `http://localhost:8000` 上的 Django 服务，只需简单的调用 `http://localhost:8000/api/*` ，并且 Vue CLI 将注意转发给 `http://localhost:8080/*`。

## 添加插件
Vue CLI v3 利用插件来提供不同的功能。当我们在新建项目初始化是选择的特性并调用所需的插件，我们还可以手工的安装任何插件并调用它们。例如，我们可以采用以下命令来添加 **Progressive Web App (PWA)** 功能：
```
npm install @vue/cli-plugin-pwa
vue invoke pwa
```

## 检查我们的 Webpack 没有 Ejecting
在很多情况下,你会想要访问 Webpack 配置文件提供先进的东西不是通过 CLI 插件。

除了 ejecting 以外, Vue CLI 允许我们检查 Webpack 配置来查看什么是 Vue CLI 生成的。这将有助于证实生成的配置是否为我们所期望的。在我们的项目中生成的 Vue CLI，运行以下命令：
```
vue inspect
```

这将在终端展示 Webpack 配置，但我们可以通过以下命令输出到文件中：
```
vue inspect > webpack.config.js
```

也可以使用路径节点检查部分配置：
```
vue inspect resolveLoader.modules
```

## 总结
通过本篇文章，我们了解如何安装 Vue CLI v3 和大量诸如添加插件、环境变量和 API 调用代理的特性。

> 扩展阅读 https://www.techiediaries.com/
