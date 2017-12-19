---
title: Work on Spacemacs
date: 2017-12-17 21:55:23
categories: editor
tags: [editor, emacs, spacemacs]
---
# Python
## Edit  Python
Edit python with virtual environment by Spacemacs.
1. Create a virtual environment:
```shell
virtualenv -p python3 venv
```
1. Install pre-requisite package in the virtual environment:
```shell
source venv/bin/activate
pip install anaconda-mode
```
1. When we edit python with virtual environment by Spacemacs, we muste activate the virtual environment in Spacemacs, run following shortcut key could choose the virtual environment directory:
    - `M-x` - `pyvenv activate`
    - `, V a`
    - `SPC m V a`
1. Yapf for Emacs
[Yapfify](https://github.com/JorisE/yapfify) uses yapf to format a Python buffer. It can be called explicitly on a certain buffer, but more conveniently, a minor-mode ‘yapf-mode’ is provided that turns on automatically running YAPF on a buffer before saving. Run following shortcut key:
    - `M-x` - `yapfift-buffer`
    - `, =`
    - `SPC m =`
