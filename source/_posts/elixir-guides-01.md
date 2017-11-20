---
title: Elixir Guides 01
date: 2017-11-20 21:53:30
categories: erlang
tags: [elixir]
---

# Introduction


<!-- vim-markdown-toc GFM -->

* [Install](#install)
* [Interactive Mode](#interactive-mode)
* [Running scripts](#running-scripts)

<!-- vim-markdown-toc -->

## Install
We can install the latest version of **Erlang** and **Elixir** from [asdf-vm/asdf](https://github.com/asdf-vm/asdf), it extendable version manager with support for Ruby, Node.js, Elixir, Erlang & more.

<!--more-->

## Interactive Mode
After install elixir, we have three new executables:`iex`,`elixir` and `elixirc`.

Open up `iex` and type the following expressions:
```elixir
Erlang/OTP 20 [erts-9.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false]

Interactive Elixir (1.5.2) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> 40 + 2
42
iex(2)> "hello" <> " world"
"hello world"
```

To exit `iex` press `Ctrl+C` twice.

## Running scripts
We can edit a file *simples.exs* and execute it with `elixir`:
```elixir
IO.puts "Hello world from Elixir"
```
```shell
$ elixir simple.exs
Hello world from Elixir
```

