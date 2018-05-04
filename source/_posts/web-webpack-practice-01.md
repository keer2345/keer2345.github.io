---
title: Webpack Practice 01 - 起步
date: 2018-05-04 13:33:18
tags: webpack
---

> 参考： http://webpack.wuhaolin.cn/

# 安装和使用
## 安装Webpack
```
sudo npm i webpack -g
```

```
mkdir demo
cd demo

npm init -y
npm i webpack webpack-cli -D
```

## 使用
在Webpack 4中，可以使用默认的配置，即**零配置**，不需要创建`webpack.config.js`文件。

创建`/index.html`:
```html
<!DOCTYPE html>
<html>

<head>
    <meta charset="utf-8" />
    <title>Home Page</title>
</head>

<body>
    <div id='app'></div>
    <!-- input the output file of webpack -->
    <script src="dist/main.js"></script>
</body>

</html>
```

创建`/src/show.js`
```javascript
function show(content) {
    window.document.getElementById('app').innerText = 'Hello , ' + content;
}

module.exports = show
```

创建`/src/index.js`
```javascript
const show = require('./show.js');

show('webpack !!!');
```

运行命令打包：
```
webpack
```

然后在浏览器打开`path/to/index.html`就可以看到效果了。

## 搭建开发服务
现在，我们需要为开发模式安装更多的依赖，那就是**webpack-dev-server**:
```
npm install webpack-dev-server -D
```
它为我们提供了开发环境下本地应用的开发服务器，我们需要编辑`package.json`里面的*scripts*内容：
```javascript
// ...
 "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1",
    "dev": "webpack-dev-server --mode development",
    "prod": "webpack --mode production"
  },
// ...
```
用下面的命令启动服务：
```
npm run dev
```
打开 http://localhost:8080 ， 我们可以看到效果。

## 零配置
上面的例子使用了零配置，也就是我们没有创建`webpack.config.js`文件，因为我们使用了默认的文件名和路径：
- 根目录的默认文件为`index.html`
- 默认入口文件为`src/index.js`
- 默认输出文件为`dist/main.js`

## webpack.config.js
在webpack 4中，可以无须任何配置使用，然而大多数项目会需要很复杂的设置，这就是为什么webpack仍然要支持配置文件。这比在终端(terminal)中手动输入大量命令要高效的多，所以让我们创建一个取代以上使用 CLI 选项方式的配置文件`webpack.config.js`:
```javascript
const path = require('path')

module.exports = {
    entry: './src/index.js',
    output: {
        filename: 'main.js',
        path: path.resolve(__dirname, 'dist')
    }
};
```
现在，让我们通过新配置文件再次执行构建：
```
webpack --config webpack.config.js
```

# 使用Loader
安装Loader
```
npm i -D style-loader css-loader
```

`webpack.config.js`
```javascript
const path = require('path')

module.exports = {
    entry: './src/index.js',
    output: {
        filename: 'main.js',
        path: path.resolve(__dirname, 'dist')
    },
    module: {
        rules: [{
            test: /\.css$/,
            use: ['style-loader', 'css-loader']
        }]
    }
};
```

`src/index.css`:
```css
#app {
    text-align: center;
}
```

`src/index.js`:
```javascript
require('./index.css');

const show = require('./show.js');

show('webpack !!!');
```

# 使用Plugin
```
npm i -D extract-text-webpack-plugin@next
```

```javascript
// const path = require('path');
const ExtractTextPlugin = require("extract-text-webpack-plugin");


module.exports = {
    // entry: './src/index.js',
    // output: {
    //     filename: 'main.js',
    //     path: path.resolve(__dirname, 'dist')
    // },

    module: {
        rules: [{
            test: /\.css$/,
            use: ExtractTextPlugin.extract({
                fallback: "style-loader",
                use: "css-loader"
            })
        }]
    },
    plugins: [
        new ExtractTextPlugin("main.css"),

    ]
};
```

# 使用DevServer
前面提到过了，我们这里再强调一次：
```
npm i -D webpack-dev-server
```
