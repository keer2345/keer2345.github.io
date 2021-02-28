---
title: Lisp of Slime and Quicklisp
date: 2021-02-27 21:00:14
tags: lisp
---

# Install
**[SBCL](http://www.sbcl.org/)**
```
brew install sbcl
```

**[Quicklisp](https://www.quicklisp.org/beta/)**
```
curl -O https://beta.quicklisp.org/quicklisp.lisp
```
```
sbcl --no-sysinit --no-userinit --load ~/quicklisp.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit
```

**Quicklisp Libraries**

- https://common-lisp.net/libraries
 
**[Slime](https://common-lisp.net/project/slime/)**

Install *slime* package in Emacs：*M-x package-install RET slime RET*，then add configuration：
```
(setq inferior-lisp-program "sbcl")
```

Enjoy your Lisp trip!!!

<!-- more -->

# Book
- Common lisp the language 2nd
- On lisp
- Practical common lisp
- Let over lambda
- Draft proposed American National Standard forInformation Systems—Programming Language—Common Lisp X3J13/94-101R
