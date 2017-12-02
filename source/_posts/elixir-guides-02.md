---
title: Elixir Guides 02
date: 2017-11-20 22:31:43
categories: erlang
tags: elixir
---


# Basic Types

<!-- vim-markdown-toc GFM -->

* [Basic arithmetic](#basic-arithmetic)
* [Booleans](#booleans)
* [Atom](#atom)
* [String](#string)
* [Anonymous functions](#anonymous-functions)
* [(Linked)Lists](#linkedlists)
* [Tuples](#tuples)
* [Basic Operators](#basic-operators)

<!-- vim-markdown-toc -->
<!-- more -->

## Basic arithmetic
```elixir
iex> 1 + 2
3
iex> 5 * 5
25
iex> 10 / 2
5.0

iex> div(10, 2)
5
iex> div 10, 2
5
iex> rem 10, 3
1
```

Elixir also supports shortcut notations for entering binary, octal, and hexadecimal numbers:
```elixir
iex> 0b1010
10
iex> 0o777
511
iex> 0x1F
31
```
Scientific notation:
```elixir
iex> 1.0
1.0
iex> 1.0e-10
1.0e-10
```

```elixir
iex> round(3.58)
4
iex> trunc(3.58)
3
```

## Booleans
Elixir supports `true` and `false` as booleans:
```elixir
iex> true
true
iex> true == false
false
```

Elixir provides a bunch of predicate functions to check for a value type:
- `is_boolean/1`
- `is_integer/1`
- `is_float/1`
- `is_number/1`

> The `h` helper can also be used to access documentation for any function. For example `h is_integer/1` or `h ==/1`.

## Atom
An atom is a constant whose name is its own value. Some other languages call these symbols:
```elixir
iex> :hello
:hello
iex> :hello == :world
false
```
The booleans `true` and `false` are, in fact, atoms:
```elixir
iex> true == :true
true
iex> is_atom(false)
true
iex> is_boolean(:false)
true
```
Finally, Elixir has a construct called aliases which we will explore later. Aliases start in upper case are also atoms:
```elixir
iex> is_atom(Hello)
true
```

## String
Strings in Elixir are delimited by double quotes, and they are encoded in UTF-8:
```elixir
iex> "hellö"
"hellö"
```
We can print a string using the `IO.puts/1`:
```elixir
iex> IO.puts "hello\nworld"
hello
world
:ok
```
## Anonymous functions
```elixir
iex> add = fn a, b -> a + b end
#Function<12.71889879/2 in :erl_eval.expr/5>
iex> add.(1, 2)
3
iex> is_function(add)
true
iex> is_function(add, 2) # check if add is a function that expects exactly 2 arguments
true
iex> is_function(add, 1) # check if add is a function that expects exactly 1 argument
false

iex> double = fn a -> add.(a, a) end
#Function<6.71889879/1 in :erl_eval.expr/5>
iex> double.(2)
4

iex> x = 42
42
iex> (fn -> x = 0 end).()
0
iex> x
42
```

## (Linked)Lists
List operators never modify the existing list:
```elixir
iex> [1, 2, true, 3]
[1, 2, true, 3]
iex> length [1, 2, 3]
3

iex> [1, 2, 3] ++ [4, 5, 6]
[1, 2, 3, 4, 5, 6]
iex> [1, true, 2, false, 3, true] -- [true, false]
[1, 2, 3, true]
```

The functions `hd/1` and `tl/1`:
```elixir
iex> list = [1, 2, 3]
iex> hd(list)
1
iex> tl(list)
[2, 3]
```

## Tuples
```elixir
iex> {:ok, "hello"}
{:ok, "hello"}
iex> tuple_size {:ok, "hello"}
2

iex> tuple = {:ok, "hello"}
{:ok, "hello"}
iex> elem(tuple, 1)
"hello"
iex> tuple_size(tuple)
2

iex> tuple = {:ok, "hello"}
{:ok, "hello"}
iex> put_elem(tuple, 1, "world")
{:ok, "world"}
iex> tuple
{:ok, "hello"}
```

## Basic Operators

`++`, `--`, `<>`:
```elixir
iex> [1, 2, 3] ++ [4, 5, 6]
[1, 2, 3, 4, 5, 6]
iex> [1, 2, 3] -- [2]
[1, 3]

iex> "foo" <> "bar"
"foobar"
```

`or`,`and`,`not`
`||`,`&&`,`!`
`==`,`!=`,`===`,`!==`,`<=`,`>=`,`<`,`>`

>Note: If you are an Erlang developer, and and or in Elixir actually map to the andalso and orelse operators in Erlang.

```elixir
iex> true and true
true
iex> false or is_atom(:example)
true

iex> 1 and true
** (BadBooleanError) expected a boolean on left-side of "and", got: 1

iex> false and raise("This error will never be raised")
false
iex> true or raise("This error will never be raised")
true

# or
iex> 1 || true
1
iex> false || 11
11

# and
iex> nil && 13
nil
iex> true && 17
17

# !
iex> !true
false
iex> !1
false
iex> !nil
true


iex> 1 == 1
true
iex> 1 != 2
true
iex> 1 < 2
true


iex> 1 == 1.0
true
iex> 1 === 1.0
false

iex> 1 < :atom
true
```

>_number < atom < reference < function < port < pid < tuple < map < list < bitstring_
