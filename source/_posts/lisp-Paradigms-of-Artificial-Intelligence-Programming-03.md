---
title: Paradigms of Artificial Intelligence Programming 03
date: 2018-09-01 09:59:09
tags: lisp
---

> https://github.com/norvig/paip-lisp/blob/master/docs/chapter3.md

# Chapter 03 Overview of Lisp

> No doubt about it. Common Lisp is a big language.
> 
> -Guy L. Steele, Jr.
> 
> Foreword to Koschman 1990

## A Guide to Lisp Style
In general, there are six maxims that every programmer should follow:
1. Be specific.
1. Use abstractions.
1. Be concise.
1. Use the provided tools.
1. Don't be obscure.
1. Be consistent.

## Special Forms
|definitions|conditional|variables|iteration|other|
|----|----|----|----|----|
|defun|and|let|do|declare|
|defstruct|case|let*|do*|function|
|defvar|cond|pop|dolist|progn|
|defparameter|if|push|dotimes|quote|
|defconstant|or|setf|loop|return|
|defmacro|unless|incf||trace|
|labels|when|decf||untrace|

To be precise, only `declare, function, if, labels, let, let*, progn` and `quote` are true special forms. The others are actually defined as macros that expand into calls to more primitive special forms and functions. There is no real difference to the programmer, and Common Lisp implementations are free to implement macros as special forms and vice versa, so for simplicity we will continue to use "special form" as a blanket term for both true special forms and built-in macros.

### Special Forms for Definitions
```
(defun function-name (parameter...) "optional documentation" body...)
(defmacro macro-name (parameter...) "optional documentation" body...)

(defvar variable-name initial-value "optional documentation" )
(defparameter variable-name value "optional documentation")
(defconstant variable-name value "optional documentation")



```

### Special Forms for Conditionals

### Special Forms for Dealing with Variables and Places

### Functions and Special Forms for Repetition

### Repetition through Recursion

### Other Special Forms

### Macros

### Backquote Notation

## Functions on Lists
