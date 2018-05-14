---
title: 微信小程序开发——wepy的简单使用
date: 2018-05-13 16:01:41
categories: wechat
tags: [wechat, wepy]
---

# 环境配置
## Linux微信web开发者工具
- [Github](https://github.com/cytle/wechat_web_devtools)
- 运行命令：`./bin/wxdt`

# Wepy
- [Github](https://github.com/Tencent/wepy)
## Wepy的安装
```
npm install wepy-cli -g
```
## Wepy的项目搭建
```
wepy init standard wepy-project
```
## 与微信开发者工具的结合
打开微信开发者工具，选择项目的时候，选择`项目目录`为刚才建立的目录*wepy-project*，填写`AppID`与`项目名称`就可以了，接下来就可以用自己喜欢的编辑器来开发微信小程序了～

微信开发者工具预览的就是我们源文件`src`编译成`dist`后的效果。因此，我们如果修改了源文件，则要重新编译项目`wepy build`，可以开启实时编译`wepy build --watch`。
