---
title: Paradigms of Artificial Intelligence Programming 01
date: 2018-08-28 22:38:25
tags: [lisp]
---

> https://github.com/norvig/paip-lisp/blob/master/docs/chapter1.md

# Chapter 01 Introduction to Lisp

> You think you know when you learn, are more sure when you can write, even more when you can teach, but certain when you can program.

> -Alan Perlis
Yale University computer scientist

<!-- more -->

This book covers a number of typical AI problems, showing how each problem can be broken down into manageable pieces, and also how each piece can be described in the programming language Common Lisp. Ideally, readers will learn enough through studying these examples to attack new AI problems with style, grace, and success.

## Install
```
sudo apt install clisp
```

## Basic
Lisp types `>` to indicate it is ready to accept the next computation. So we are faced with a screen that looks like this:
```lisp
>
```
```
> (+ 2 2)
4
>
```

## Symbolic Computation
Here's an example of a computation on lists:
```lisp
> (append '(Pat Kim) '(Robin Sandy)) => (PAT KIM ROBIN SANDY)
```


```lisp
> 'John => JOHN

> '(John Q Public) => (JOHN Q PUBLIC)

> '2 => 2

> 2 => 2

> '(+  2 2) => (+  2 2)

> (+  2 2) 4

> John => *Error: JOHN is not a bound variable*

> (John Q Public) => *Error: JOHN is not a function*

```
```lisp
> (append '(Pat Kim) (list '(John Q Public) 'Sandy))
(PAT KIM (JOHN Q PUBLIC) SANDY)

> (length (append '(Pat Kim) (list '(John Q Public) 'Sandy)))
4
```

Above, there are four important points to make about symbols:
1. Lisp does not attach any external significance to the objects it manipulates.
1. Common Lisp provides over 700 built-in functions, like `append`, `length`, `+` ...
1. Common Lisp are not case sensitive. 
1. A wide variety of characters are allowed in symbols: numbers, letters, and other punctuation marks like `+` or `!`

## Variable
One way to give a value to a variable is with `setf`:
```lisp
> (setf p '(John Q Public)) => (JOHN Q PUBLIC)
> p => (JOHN Q PUBLIC)
> (setf x 10) => 10
> (+ x x) => 20
> (+ x (length p)) => 13
```

## Special Forms
`setf` is called a special form because it does something special: if it did not exist, it would be impossible to write a function that assigns a value to a variable. The philosophy of Lisp is to provide a small number of special forms to do the things that could not otherwise be done, and then to expect the user to write everthing else as functions.

|||
|----|----|
|defun|define function|
|defparameter|define special variable|
|setf|set variable or field to new value|
|let|bind local variable(s)|
|case|choose one of several alternatives|
|if|do one thing or another, depending on a test|
|function (#')|refer to a function|
|quote (')|introduce constant data|

## List
```lisp
> p => (JOHN Q PUBLIC)

> (first p) JOHN

> (rest p) => (Q PUBLIC)

> (second p) => Q

> (third p) => PUBLIC

> (fourth p) => NIL

> (length p) => 3
```


```lisp
> (setf x '((1st element) 2 (element 3) ((4)) 5))
((1ST ELEMENT) 2 (ELEMENT 3) ((4)) 5)

> (length x) => 5

> (first x) => (1ST ELEMENT)

> (second x) => 2

> (third x) => (ELEMENT 3)

> (fourth x) => ((4))

> (first (fourth x)) => (4)

> (first (first (fourth x))) => 4

> (fifth x) => 5

> (first x) => (1ST ELEMENT)

> (second (first x)) => ELEMENT

> (last p) => (PUBLIC)

> (first (last p)) => PUBLIC
```

```lisp
> p => (JOHN Q PUBLIC)

> (cons 'Mr p) => (MR JOHN Q PUBLIC)

> (cons (first p) (rest p)) => (JOHN Q PUBLIC)

> (setf town (list 'Anytown 'USA)) => (ANYTOWN USA)

> (list p 'of town 'may 'have 'already 'won!) =>
((JOHN Q PUBLIC) OF (ANYTOWN USA) MAY HAVE ALREADY WON!)

> (append p '(of) town '(may have already won!)) =>
(JOHN Q PUBLIC OF ANYTOWN USA MAY HAVE ALREADY WON!)

> p => (JOHN Q PUBLIC)
```

## Defining New Functions
In general, a function definition takes the following form (where the documentation string is optional, and all other parts are required):
```lisp
(defun *function-name* (*parameter...*)
      "*documentation string*"
      *function-body...*)
```
The special form `defun` stands for *define function.* It is used here to define a new function called `last-name`:
```lisp
(defun last-name (name)
  "Select the last name from a name represented as a list."
  (first (last name)))
```

```lisp
> (last-name p) => PUBLIC

> (last-name '(Rear Admiral Grace Murray Hopper)) => HOPPER

> (last-name '(Rex Morgan MD)) => MD

> (last-name '(Spot)) => SPOT

> (last-name '(Aristotle)) => ARISTOTLE``
```

```lisp
(defun first-name (name)
  "Select the first name from a name represented as a list."
  (first name))

> p => (JOHN Q PUBLIC)`

> (first-name p) => JOHN`

> (first-name '(Wilma Flintstone)) => WILMA`

> (setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot) 
              (Aristotle) (A A Milne) (Z Z Top)
              (Sir Larry Olivier) (Miss Scarlet))) => 

((JOHN Q PUBLIC) (MALCOLM X) (ADMIRAL GRACE MURRAY HOPPER)
 (SPOT) (ARISTOTLE) (A A MILNE) (Z Z TOP) (SIR LARRY OLIVIER)
 (MISS SCARLET))

> (first-name (first names)) => JOHN
```

## Using Functions
**mapcar** :

```lisp
> (mapcar #'last-name names)
(PUBLIC X HOPPER SPOT ARISTOTLE MILNE TOP OLIVIER SCARLET)

> (mapcar #'first-name names)
(JOHN MALCOLM ADMIRAL SPOT ARISTOTLE A Z SIR MISS)

> (mapcar #'- '(1 2 3 4)) => (-1 -2 -3 -4)

> (mapcar #'+ '(1 2 3 4) '(10 20 30 40)) => (11 22 33 44)
```

**member** :
```lisp
(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")


(defun first-name (name)
  "Select the first name from a name represented as a list."
  (if (member (first name) *titles*)
  (first-name (rest name))
  (first name)))

> (mapcar #'first-name names)
(JOHN MALCOLM GRACE SPOT ARISTOTLE A Z LARRY SCARLET)

> (first-name '(Madam Major General Paula Jones))
PAULA


```

We can see how this works by *tracing* the execution of `first-name`, and seeing the values passed to and returned from the function. The special forms `trace` and `untrace` are used for this purpose.
```lisp
> (trace first-name)
(FIRST-NAME)

> (first-name '(John Q Public))
(1 ENTER FIRST-NAME: (JOHN Q PUBLIC))
(1 EXIT FIRST-NAME: JOHN)
JOHN

> (first-name '(Madam Major General Paula Jones)) =>
(1 ENTER FIRST-NAME: (MADAM MAJOR GENERAL PAULA JONES))
  (2 ENTER FIRST-NAME: (MAJOR GENERAL PAULA JONES))
    (3 ENTER FIRST-NAME: (GENERAL PAULA JONES))
      (4 ENTER FIRST-NAME: (PAULA JONES))
      (4 EXIT FIRST-NAME: PAULA)
    (3 EXIT FIRST-NAME: PAULA)
  (2 EXIT FIRST-NAME: PAULA)
(1 EXIT FIRST-NAME: PAULA)
PAULA

> (untrace first-name) => (FIRST-NAME)

> (first-name '(Mr Blue Jeans)) => BLUE
```

## Higher-Order Functions
A function that takes another function as an argument is called a higher-order function. `mapcar` is an example.

We will define a new function called mappend:
```lisp
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

> (apply #'+ '(1 2 3 4)) => 10

> (apply #'append '((1 2 3) (a b c))) => (1 2 3 A B C)
```


Now we define a new function, self-and-double, and apply it to a variety of arguments:
```lisp
> (defun self-and-double (x) (list x (+ x x)))

> (self-and-double 3) => (3 6)

> (apply #'self-and-double '(3)) => (3 6)

> (mapcar #'self-and-double '(1 10 300)) => ((1 2) (10 20) (300 600))

> (mappend #'self-and-double '(1 10 300)) => (1 2 10 20 300 600)
```


```lisp
(defun numbers-and-negations (input)
  "Given a list, return only the numbers and their negations."
  (mappend #' number-and-negation input))

(defun number-and-negation (x)
  "If x is a number, return a list of x and -x."
  (if (numberp x)
      (list x (- x))
      nil))

> (numbers-and-negations '(testing 1 2 3 test)) => (1 -1 2 -2 3 -3)

```

`funcall` is similar to `apply`; it too takes a function as its first argument and applies the function to a list of arguments, but in the case of `funcall`, the arguments are listed separately:

```lisp
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))


> (funcall #'+ 2 3) => 5

> (apply #' + '(2 3)) => 5

> (funcall #' + '(2 3)) => *Error: (2 3) is not a number.*
```

**lambda** :

In general, the form of a *lambda* expression is:
```
(lambda (*parameters...*) *body...*)
```


```lisp
> ((lambda (x) (+ x 2)) 4) => 6

> (funcall #'(lambda (x) (+ x 2)) 4) => 6
```

```lisp
> append => *Error: APPEND is not a bound variable*

> (lambda (x) (+ x 2)) => *Error: LAMBDA is not a function*

>(mapcar #'(lambda (x) (+ x x))
         '(1 2 3 4 5)) =>
(2 4 6 8 10)

> (mappend #'(lambda (l) (list l (reverse l)))
           ((1 2 3) (a b c))) =>
((1 2 3) (3 2 1) (A B C) (C B A))
```



## Other Data Types
So far we have seen just four kinds of Lisp objects: numbers, symbols, lists, and functions. Lisp actually defines about 25 different types of objects: vectors, arrays, structures, characters, streams, hash tables, and others. At this point we will introduce one more, the string. As you can see in the following, strings, like numbers, evaluate to themselves. Strings are used mainly for printing out messages, while symbols are used for their relationships to other objects, and to name variables. The printed representation of a string has a double quote mark `""` at each end.

```lisp
> "a string" => "a string"

> (length "a string") => 8

> (length "") => 0
```

## Summary: The Lisp Evaluation Rule
We can now summarize the evaluation rule for Lisp.
- Every expression is either a list or an atom.
- Every list to be evaluated is either a special form expression or a function application.
- A special form expression is defined to be a list whose first element is a special form operator. 
- A function application is evaluated by first evaluating the arguments (the rest of the list) and then finding the function named by the first element of the list and applying it to the list of evaluated arguments.
- Every atom is either a symbol or a nonsymbol.
- A symbol evaluates to the most recent value that has been assigned to the variable named by that symbol. 
- A nonsymbol atom evaluates to itself.

## What Makes Lisp Different?
What is it that sets Lisp apart from other languages? Why is it a good language for AI applications? There are at least eight important factors:
- Built-in Support for Lists
- Automatic Storage Management
- Dynamic Typing
- First-Class Functions
- Uniform Syntax
- Interactive Environment
- Extensibility
- History


[☝ Back to Catalog](../../../2018/08/28/lisp-Paradigms-of-Artificial-Intelligence-Programming)

[☜ Preface](../../../2018/08/28/lisp-Paradigms-of-Artificial-Intelligence-Programming-00/)

[☞ A Simple Lisp Program](../../../2018/08/31/lisp-Paradigms-of-Artificial-Intelligence-Programming-02/)
