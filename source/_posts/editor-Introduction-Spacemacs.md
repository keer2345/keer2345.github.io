---
title: 因为刚好遇见你：Spacemacs
date: 2017-11-11 11:05:34
categories: editor
tags: [emacs, spacemacs]
---


>A community-driven Emacs distribution - The best editor is neither Emacs nor Vim, it's Emacs *and* Vim!

[*Spacemacs*](https://github.com/syl20bnr/spacemacs) - 一个基于社区驱动的*Emacs*衍生版本，这个优秀的编辑器既不是*Emacs*，也不是*Vim*，它是*Emacs*和*Vim*的结合体。

>因为刚好遇见你
留下足迹才美丽
风吹花落泪如雨

<!-- more -->

# 介绍
几个月以前，我决定尝试Spacemacs —— 这个靠大量配置来管理的Emacs版本。在这篇文章中，我想分享一些使用经验以及回顾一些必需的基础知识。如果您也想试一试的话，那就一起开始吧！

![Spacemacs](http://upload-images.jianshu.io/upload_images/1702157-baf9610693a3f7d4.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

用了几年的vim，在我看来，vim的键位安排是非常好的，我喜欢vim那优雅的快捷键，也为此投入了很多时间来学习和掌握它们。我也将vim的工作模式应用于IDE（Intellij IDE）、浏览器（Firefox、Chrome）等。与此同时，我也尝试着了解Emacs，是优雅而惊艳的，渐渐地，我从vim转移到了Spacemacs。

Emacs是最流行的文本编辑器之一，它始于上个世纪80年代并在具体仍非常流行，因为Emacs与现代的软件开发需求齐头并进。一直有这么一种说法：
>*Emacs, "a great operating system, lacking only a decent editor"*

可以从[这里](https://en.wikipedia.org/wiki/Editor_war)了解更多Vim与Emacs的“战争”。茁壮成长的Spacemacs社区似乎要结束编辑器之间的“战争”，从此一劳永逸～～

Spacemacs的学习曲线并不陡峭，可以不用深入了解Emacs，但是强烈建议深入了解一下vim，因为Spacemacs默认使用的是Vim快捷键。

# 安装
Spacemacs几天前（20171102）在develop分支推送了最新的版本，要求Emacs的最低版本为`25.2`。如果我们的Emacs版本不够高，则先升级Emacs。
## 安装Emacs 25
```
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs25
```
## 检查版本号：
```
> $ emacs --version                                                                      
GNU Emacs 25.3.2
Copyright (C) 2017 Free Software Foundation, Inc.
GNU Emacs comes with ABSOLUTELY NO WARRANTY.
You may redistribute copies of GNU Emacs
under the terms of the GNU General Public License.
For more information about these matters, see the file named COPYING.
```
## 安装Spacemacs
由于master分支已经很久不维护了，强烈建议安装develop分支。

### 备份
```
cd ~
mv .emacs.d .emacs.d.bak
mv .emacs .emacs.bak
```
### 安装
- Spacemacs默认字体为*Source Code Pro*，该字体在编程中有非常好的体验，也为最多程序员所推崇。如果系统缺少该字体，请先添加该字体。具体请参考（https://github.com/adobe-fonts/source-code-pro）。

- 接下来开始安装Spacemacs：
```
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d -b develop
```
随后启动emacs，就会弹出Spacemacs界面，并开始连接网络下载很多插件。
首次启动emacs，在`~/`目录会生成一个`.spacemacs`文件。
- 添加镜像。由于我们网络的原因，有时无法访问到官方网站下载插件，或者网速非常慢。这时我们可以先把Spacemacs终止掉，添加国内[镜像源](https://elpa.emacs-china.org/)之后再重新启动Spacemacs。具体方法是添加下面的代码到 `.spacemacs` 的 `dotspacemacs/user-init()`
```
(setq configuration-layer--elpa-archives
    '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
      ("org-cn"   . "http://elpa.emacs-china.org/org/")
      ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))
```
- 为了方便管理自定义的spacemacs，我们新建一个`.spacemacs.d`文件夹，把`.spacemacs`文件移到该文件夹下，并改名为`init.el`。这样就可以方便用*Git*仓库来管理版本了。

# Spacemacs都有些什么
不像纯粹的Emacs，Spacemacs预配置有大量的包和定义好的使用模式。在Spacemacs中操作围绕这空格键`space`，按下空格键触发会触发快捷菜单列表并且带有描述。
![space key](http://upload-images.jianshu.io/upload_images/1702157-07209380be5830cf.gif?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

第一次看见这些菜单会觉得很陌生，但是我们很快会习惯的，按下其中一个快捷键将引导我们到下一层菜单，使用了一段时间我们就能记住常用的快捷键。例如， **w**代表窗口window，**b**代表缓存buffer，**f**代表文件file，**p**代表项目菜单，等等。还有一些快捷键便于在窗口间切换。

# 配置
像Emacs，Spacemacs引以为傲的是高配置性。大部分的选项存储在`~/.spacemacs`（我们之前想配置转移到了`~/.spacemacs.d/init.el`）文件，可以使用`SPC f e d`（即先按空格键、接着按f、e、d键）键在Spacemacs中快速打开。

在该配置文件的头部，可以看到`dotspacemacs-configuration-layers`部分，Spacemacs层是重要插件，如果我们有使用Emacs的背景，一定是用过MELPA包，Spacemacs层围绕MELPA提供了常用的包。

许多层不包含该在配置中，需要激活它们来使用。为了激活层，简单的方式是在配置文件中添加层。比如我们想要使用python语言，则在层里面添加python，类似于这样：

<center>
![layers](http://upload-images.jianshu.io/upload_images/1702157-86c69d588b54f0b4.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)
</center>

添加完层后，我们通过`SPC f e R`来重载配置，或者重启Emacs。

# 基本操作
想要使用Spacemacs，我们就得熟悉一些基本操作：
- 如何管理窗口/缓存
- 如何管理文件
- 如何查找
等等。

掌握这些操作足够你用Spacemacs来完成基本的工作。

## 窗口管理
窗口管理相对简单。当启动Spacemacs时，就打开了一个窗口，只要我们喜欢，可以垂直或者水平的随意分割任意多的窗口。
- `SPC w /` 垂直分割窗口
- `SPC w -` 水平分割窗口

分割窗口之后，可以使用`SPC N`来切换窗口，`N`代表窗口的数值（通常为0~9），可以在窗口左下角查看到其数值。

## 缓存管理
Spacemacs的缓存和其他编辑器非常类似，我们可以在缓存中打开文件，选择一个窗口浏览缓存，通过`SPC b b`查看缓存列表。

我们可以通过`SPC f f`来打开文件，如果问价不存在，Spacemacs将会为我们创建该文件。

## 项目
大多数的开发是在一个项目中，Spacemacs使用了Emacs中的一个叫做*Projectile*的包来自动探测我们是否工作在一个项目当中，允许我们在该项目中查找文件或者文本。*Projectile*通常使用版本控制器来确定我们是否工作在一个项目当中（例如，一个*Git*目录）。

当我们工作在项目当中时，可以用`SPC p t`触发文件管理器来浏览项目。

![tree](http://upload-images.jianshu.io/upload_images/1702157-81c898d80c21407f.gif?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

## 在项目中查找文件
我们可以使用`SPC p f`在项目中模糊查找文件，将*p*理解成*project*，*f*理解成*find-file*将有助于我们记住快捷键。

![find file](http://upload-images.jianshu.io/upload_images/1702157-e737f7c3e3629e13.gif?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

## 在项目中查找文本
我们可以通过`SPC /`查找项目中的任意文本。

![search text](http://upload-images.jianshu.io/upload_images/1702157-6d21c7a0737a38f5.gif?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

# 社区
Spacemacs拥有活跃的社区，在其Github上，或者在国内比较活跃的Emacs（https://emacs-china.org/） 论坛上，我们都能受益匪浅。

在使用了一段时间的Spacemacs后，逐渐感觉到自己喜欢上了它的工作模式，用Spacemacs十分富有成效，并拥有无穷无尽的包供我们安装和使用。如果您也在寻找一个可高度配置的文本编辑器，强烈建议您试用Spacemacs，希望它能给您带来惊喜！
