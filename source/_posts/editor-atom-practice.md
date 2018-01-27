---
title: Atom配置与使用
date: 2018-01-26 13:17:45
categories: editor
tags: [editor, atom]
---
# Install Atom on Ubuntu
```
sudo add-apt-repository ppa:webupd8team/atom
sudo apt-get update
sudo apt-get install atom
```

# Setting Proxy
`vim ~/.atom/.apm/.apmrc`
```shell
http-proxy=http://127.0.0.1:1080
https-proxy=http://127.0.0.1:1080
strict-ssl=false
```

# Theme
- Dracula UI
- Monokai Syntax

# Package
- Color Picker
- Emmet
- Minimap
- Beautify
- Atom Linter
- File Icons
- JavaScript Snippets
- CSS Comb
- Git plus
- Remote Edit
- Autocomplete-python
- Linter-flake8
- Python-tools
- vim-mode-plus
    - ex-mode
    - relative-numbers
- terminal-plus
