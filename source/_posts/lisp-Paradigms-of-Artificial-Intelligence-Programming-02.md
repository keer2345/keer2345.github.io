---
title: Paradigms of Artificial Intelligence Programming 02
date: 2018-08-31 21:26:58
tags: lisp
---

> https://github.com/norvig/paip-lisp/blob/master/docs/chapter2.md

# Chapter 02 A Simple Lisp Program

> Certum quod factum.
> 
> (One is certain of only what one builds.)
> 
> -Giovanni Battista Vico (1668-1744)
> 
> Italian royal historiographer

## A Grammar for a Subset of English
The program we will develop in this chapter generates random English sentences. Here is a simple grammar for a tiny portion of English:
```lisp
Sentence => Noun-Phrase + Verb-Phrase
Noun-Phrase => Article + Noun
Verb-Phrase => Verb + Noun-Phrase
Article => the, a,...
Noun => man, ball, woman, table...
Verb => hit, took, saw, liked...
```

## A Straightforward Solution
We will develop a program that generates random sentences from a phrase-structure grammar. The most straightforward approach is to represent each grammar rule by a separate Lisp function:
```lisp
(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))
```
Now we can test the program by generating a few random sentences, along with a noun phrase and a verb phrase:
```lisp

> (sentence) => (THE WOMAN HIT THE BALL)

> (sentence) => (THE WOMAN HIT THE MAN)

> (sentence) =>(THE BALL SAW THE WOMAN)

> (sentence) => (THE BALL SAW THE TABLE)

> (noun-phrase) => (THE MAN)

> (verb-phrase) => (LIKED THE WOMAN)

> (trace sentence noun-phrase verb-phrase article noun verb) =>
(SENTENCE NOUN-PHRASE VERB-PHRASE ARTICLE NOUN VERB)

> (sentence) =>
(1 ENTER SENTENCE)
  (1 ENTER NOUN-PHRASE)
    (1 ENTER ARTICLE)
    (1 EXIT ARTICLE: (THE))
    (1 ENTER NOUN)
    (1 EXIT NOUN: (MAN))
  (1 EXIT NOUN-PHRASE: (THE MAN))
  (1 ENTER VERB-PHRASE)
    (1 ENTER VERB)
    (1 EXIT VERB: (HIT))
    (1 ENTER NOUN-PHRASE)
      (1 ENTER ARTICLE)
      (1 EXIT ARTICLE: (THE))
      (1 ENTER NOUN)
      (1 EXIT NOUN: (BALL))
    (1 EXIT NOUN-PHRASE: (THE BALL))
  (1 EXIT VERB-PHRASE: (HIT THE BALL))
(1 EXIT SENTENCE: (THE MAN HIT THE BALL))
(THE MAN HIT THE BALL)
```

The program works fine, and the trace looks just like the sample derivation above, but the Lisp definitions are a bit harder to read than the original grammar rules. This problem will be compounded as we consider more complex rules. Suppose we wanted to allow noun phrases to be modified by an indefinite number of adjectives and an indefinite number of prepositional phrases. In grammatical notation, we might have the following rules:

```lisp
Noun-Phrase => Article + Adj* + Noun + PP*
Adj* => 0̸, Adj + Adj*
PP* => 0̸, PP + PP*
PP => Prep + Noun-Phrase
Adj => big, little, blue, green, ...
Prep => to, in, by, with, ...
```

```lisp
(defun Adj* ()
  (if (= (random 2) 0)
  nil
  (append (Adj) (Adj*))))
(defun PP* ()
  (if (random-elt '(t nil))
  (append (PP) (PP*))
  nil))
(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))
```

I've chosen two different implementations for Adj* and PP*; either approach would work in either function. We have to be careful, though; here are two approaches that would not work:

```lisp
(defun Adj* ()
  "Warning - incorrect definition of Adjectives."
  (one-of '(nil (append (Adj) (Adj*)))))
(defun Adj* ()
  "Warning - incorrect definition of Adjectives."
  (one-of (list nil (append (Adj) (Adj*)))))
```

## Rule-Based Solution
An alternative implementation of this program would concentrate on making it easy to write grammar rules and would worry later about how they will be processed. Let's look again at the original grammar rules:
```
Sentence => Noun-Phrase + Verb-Phrase
Noun-Phrase => Article + Noun
Verb-Phrase => Verb + Noun-Phrase
Article => the, a, ...
Noun => man, ball, woman, table...
Verb => hit, took, saw, liked...
```


## Two Paths to Follow
## Changing the Grammar without Changing the Program
## Using the Same Data for Several Programs



[☝ Back to Catalog](../../../2018/08/28/lisp-Paradigms-of-Artificial-Intelligence-Programming)

[☜ Introduction to Lisp](../../../2018/08/28/lisp-Paradigms-of-Artificial-Intelligence-Programming-01/)

[☞ Overview of Lisp](../../../2018/09/01/lisp-Paradigms-of-Artificial-Intelligence-Programming-03/)
