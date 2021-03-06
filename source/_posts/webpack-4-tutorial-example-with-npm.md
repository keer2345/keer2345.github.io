---
title: Webpack 4  Tutorial Example with NPM
date: 2018-04-13 21:53:33
categories: webpack
tags: [webpack]
---
本文介绍通过`npm`来演示`webpack`入门的示例。

*webpack*为JavaScript应用提供了静态绑定，当*webpack*处理我们的应用时，其递归地处理应用中每一个模块的依赖，然后打包这些模块到一个或者多个*bundles*，我尽可能地时本篇教程易于理解。

# Webpack4 的改进之处
- 性能改进和更快的构建时间
- 零配置小应用程序（我们也可以看看*ParcelJS*教程）
- 针对纯模块更好的*tree shaking*，没有副作用
- `<script async>`支持
- 默认的，我们可以导出导入网页集合（Rust, C++, C, etc）
- 介绍可以在**developement**或**production**下选择的***mode***属性，默认为**production**。

# Webpack4 的开始
刚开始，我们只需要理解以下四个**核心概念**：
- Entry
- Output
- Loaders
- Plugins

<!-- more -->

## Entry
**Entry**表明哪一个模块是webpack构建的最开始的依赖图，即入口。

下面是最简单的`entry`配置：
```javascript
// webpack.config.js

module.exports = {
  entry: './path/to/my/entry/file.js'
};
```
## Output
**Output**告知webpack的出口，例如下面配置：
```javascript
// webpack.config.js

const path = require('path');

module.exports = {
  entry: './path/to/my/entry/file.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'my-first-webpack.bundle.js'
  }
};
```

## Loaders
**Loaders**是webpack用来处理非JavaScript的文件。例如**vue-loader**,**babel-loader**,**css-loader**, etc
```javascript
// webpack.config.js

const path = require('path');

const config = {
  entry: './path/to/my/entry/file.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'my-first-webpack.bundle.js'
  },
  module: {
    rules: [
      { test: /\.txt$/, use: 'raw-loader' }
    ]
  }
};

module.exports = config;
```
## Plugins
Loaders用于制定特定的模块，**Plugins**能够执行更广泛的任务，插件从包优化以及小到定义环境变量。比如：
```javascript
// webpack.config.js

const HtmlWebpackPlugin = require('html-webpack-plugin'); //installed via npm
const webpack = require('webpack'); //to access built-in plugins
const path = require('path');

const config = {
  entry: './path/to/my/entry/file.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'my-first-webpack.bundle.js'
  },
  module: {
    rules: [
      { test: /\.txt$/, use: 'raw-loader' }
    ]
  },
  plugins: [
    new webpack.optimize.UglifyJsPlugin(),
    new HtmlWebpackPlugin({template: './src/index.html'})
  ]
};

module.exports = config;
```

# 开始webpack项目
好了，让我们来开始一个具体的示例。
## 环境
```shell
mkdir demo
cd demo

npm init -y
npm add webpack webpack-cli -D
```
## 零配置的 webpack 4
如果我们使用过以前的webpack版本，那么得创建*webpack.config.js*文件。 webpack4这一版本默认不用创建配置文件，我们只需要创建文件夹*src*，并在其中创建*index.js*文件。当然，更详细的配置的话，还是需要创建配置文件的。

1. 创建文件
```shell
mkdir src
touch src/index.js
```
1. 编辑*src/index.js*:
```javascript
console.log('webpack 4 is running without configuration file');
```
1. 运行命令`webpack`进行打包，顺利的话会出现一下结果:
```
Hash: 024a0c82b3c29457486d
Version: webpack 4.5.0
Time: 281ms
Built at: 2018-4-13 23:01:10
  Asset       Size  Chunks             Chunk Names
main.js  607 bytes       0  [emitted]  main
Entrypoint main = main.js
   [0] ./src/index.js 65 bytes {0} [built]
WARNING in configuration
The 'mode' option has not been set, webpack will fallback to 'production' for this value.
Set 'mode' option to 'development' or 'production' to enable defaults for each environment.
You can also set it to 'none' to disable any default behavior.
Learn more: https://webpack.js.org/concepts/mode/
```
并会生成**dist**文件夹和其下的**main.js**文件。注意，默认构建*production*。
如果出现异常，可能要全局安装`webpack`,`webpack-cli`:
```shell
npm install webpack webpack-cli -g
```


默认是**production mode**。但是我们也可以生成开发模式，在*package.json*的*scripts*下配置如下两行：
```javascript
"scripts": {
    "dev": "webpack --mode development",
    "prod": "webpack --mode production"
},
```
这样，就可以用以下命令来打包开发模式和生产模式了：
```shell
npm run dev
npm run prod
```

# 使用Babel转换JavaScript ES6
现在很多JavaScript都是用ES6编写，但是一些浏览器并不支持ES6，所以我们需要转换成ES5。

> 相关阅读：[Beginner’s Guide To Setup  ES6 Development Environment](https://appdividend.com/2017/03/28/beginners-guide-to-setup-es6-development-environment/)

webpack并不知道如何转换，我们需要加载器（Loaders）来做这项工作。**babel-loader**是webpack用来转到ES6到ES5的加载器，让我们来安装它吧：
```shell
npm install babel-core babel-loader -D
```
好了，现在我们需要配置**babel-loader**。首先，在根目录创建文件*webpack.config.js*。其它的配置都是默认的，我们只需要定义**label-loader**配置：
```javascript
// webpack.config.js

module.exports = {
   module: {
      rules: [
        {
          test: /\.js$/,
          exclude: /node_modules/,
          use: {
            loader: "babel-loader"
          }
        }
      ]
   }
}
```

现在，我们需要为开发模式安装更多的依赖，那就是**webpack-dev-server**:
```shell
npm install webpack-dev-server -D
```
它为我们提供了开发环境下本地应用的开发服务器，我们需要编辑*package.json*里面的*scripts*内容：
```javascript
"scripts": {
    "dev": "webpack-dev-server --mode development",
    "prod": "webpack --mode production"
},
```
现在，我们在*src/index.js*编写ES6的代码：
```javascript
// index.js

const app = (a, b) => {
   return a + b;
}

alert(app(4,5));
```
我们使用了**ES6**特性中的箭头函数。

现在，我们在根目录创建文件*index.html*:
```html
<!DOCTYPE html>
<html lang="en">
<head>
   <meta charset="UTF-8">
   <meta name="viewport" content="width=device-width, initial-scale=1.0">
   <meta http-equiv="X-UA-Compatible" content="ie=edge">
   <title>Webpack 4 Tutorial</title>
</head>
<body>
   <div id="app"></div>
   <script src="main.js"></script>
</body>
</html>
```
用下面的命令启动服务：
```shell
npm run dev
```
打开 http://localhost:8080 ， 我们可以看到效果。

# 在webpack4中配置React.js
现在，我们来配置**React.js**，需要添加以下依赖：
```shell
npm install babel-preset-react babel-preset-stage-0 -D
```
在根目录建立文件`.babelrc`：
```
{
   "presets": ["react", "stage-0"]
}
```

## 安装React.js
```shell
npm install react react-dom --save
```

在*src*文件夹中创建文件*AppComponent.js*:
```javascript
// AppComponent.js

import React, { Component } from 'react';

export default class AppComponent extends Component {
   render() {
      return (
         <div>
            App Component is Rendered!!
         </div>
      )
   }
}
```

好了，**React**组件已经创建了。接下来，在*index.js*中引入*AppComponent.js*：
```javascript
// index.js

import React from 'react';
import { render } from 'react-dom';
import AppComponent from './AppComponent';

render(<AppComponent />, document.getElementById('app'));
```

现在，运行如下命令可以在浏览器中看到效果了：
```shell
npm run dev
```
