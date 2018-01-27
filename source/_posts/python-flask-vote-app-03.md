---
title: Flask Web投票应用03：模板，认证和静态页面
date: 2018-01-25 22:42:46
categories: python
tags: [python, flask]
---

欢迎回来，这一节我们将为投票应用设计登录页面和认证系统，允许用户创建主题并投票。

# 登录页面
我们使用[bootstrap](https://getbootstrap.com/)来自定义符合我们需求的页面。在此之前，我们创建一些目录来存储模板和一些静态文件（css, images 和 javascript）。

## 创建必须的目录
```shell
mkdir -p templates static/css static/images static/js
```
## 下载bootstrap
```shell
wget -pk hhttps://getbootstrap.com/docs/3.3/examples/jumbotron-narrow
```
## 复制相关文件到指定的目录
```shell
mv getbootstrap.com/docs/3.3/examples/jumbotron-narrow/index.html templates 
mv getbootstrap.com/docs/3.3/examples/jumbotron-narrow/jumbotron-narrow.css static/css
mv getbootstrap.com/docs/3.3/dist/css/bootstrap.min.css static/css
mv getbootstrap.com/docs/3.3/dist/fonts static
mv getbootstrap.com/docs/3.3/assets/css/ie10-viewport-bug-workaround.css static/css
mv getbootstrap.com/docs/3.3/assets/js/* static/js
rm -rf getbootstrap.com
```

我们也从[bootstrap](https://getbootstrap.com/docs/3.3/examples/signin/)下载登录页面的示例，并重命名登录页面模板为*signup.html*。重复之前的步骤，而且不要忘记将这里的*index.html*重命名为*signup.html*。
```shell
wget -pk https://getbootstrap.com/docs/3.3/examples/signin/
```

```shell
mv getbootstrap.com/docs/3.3/examples/signin/index.html templates/signup.html
mv getbootstrap.com/docs/3.3/examples/signin/signin.css static/css
rm -rf getbootstrap.com
```

整理完成之后，我们看到的效果大概是这样的：
```shell
(.venv) > $ tree templates static
templates
├── index.html
└── signup.html
static
├── css
│   ├── bootstrap.min.css
│   ├── ie10-viewport-bug-workaround.css
│   ├── jumbotron-narrow.css
│   └── signin.css
├── fonts
│   ├── glyphicons-halflings-regular.eot
│   ├── glyphicons-halflings-regular.eot?
│   ├── glyphicons-halflings-regular.svg
│   ├── glyphicons-halflings-regular.ttf
│   ├── glyphicons-halflings-regular.woff
│   └── glyphicons-halflings-regular.woff2
├── images
└── js
    ├── ie10-viewport-bug-workaround.js
    └── ie-emulation-modes-warning.js
```


> 本文的源代码可以在[Github](https://github.com/keer2345/Flask_Vote/tree/v.03)中找到。
