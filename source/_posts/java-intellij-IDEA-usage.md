---
title: Jntellij IDEA Usage
date: 2020-01-08 23:07:07
tags: [java, editor]
---

# 获取神器码
- http://idea.medeming.com/

# Plugins
## IdeaVIM 插件的使用
- [JetBrains/ideavim](https://github.com/JetBrains/ideavim)
- [IdeaVim键盘流](https://www.jianshu.com/p/fd82bbed2c25)

`./.ideavimrc`:
```
inoremap jk <ESC>

" leader: ,
let mapleader = ","

" 映射idea常用快捷键
" 重命名
nnoremap <leader>r :action RenameElement<CR>
" 格式化当前文件
nnoremap <leader>f :action ReformatCode<CR><esc>
nnoremap <leader>q :action Close<CR><esc>
nnoremap <leader>i :action Generate<CR><esc>

" 插件模拟
" surround
set surround
" multiple-cursors
set multiple-cursors
" easyMotion 模拟，额外依赖插件：AceJump,IdeaVim-EasyMotion
set easymotion
" 注释插件模拟
set commentary
```
