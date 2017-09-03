---
title: Erlang Learn
date: 2017-09-02 23:24:03
tags: erlang
---
# 基本概念
数据类型

# 模块与函数
## 模块
### 创建一个模块
`geometry.erl`
```
-module(geometry).
-export([area/1]).

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side}) -> Side * Side.
```
在终端编译运行
```
> $ erl
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

Eshell V7.3  (abort with ^G)
1> c(geometry).
{ok,geometry}
2> geometry:area({rectangle,10,6}).
60
3> geometry:area({square,14}).
196
```
添加测试
```
-module(geometry).
-export([test/0, area/1]).

test() ->
	12 = area({rectangle, 3, 4}),
	25 = area({square, 5}),
	tests_worked.

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side}) -> Side * Side.
```
编译运行
```
1> c(geometry).
{ok,geometry}
2> geometry:test().
tests_worked
```
### 购物车模块
shop.erl
```
-module(shop).
-export([cost/1]).

cost(oranges)   -> 5;
cost(newspaper) -> 8;
cost(apples)    -> 2;
cost(pears)     -> 9;
cost(milk)      -> 7.
```
shop1.erl
```
-module(shop1).
-export([total/1]).

total([])              -> 0;
total([{What, N} | T]) -> shop:cost(What) * N + total(T).
```
编译运行
```
5> c(shop)
5> .
{ok,shop}
6> c(shop1).
{ok,shop1}
7> shop1:total([]).
0
8> shop1:total([{milk,3}]).
21
9> shop1:total([{milk,3},{pears,7}]).
84
```

## *fun*: 基本的抽象单元
```
10> Double = fun(X) -> 2* X end.
#Fun<erl_eval.6.50752066>
11> Double(3).
6
12> A = fun(X,Y) -> math:sqrt( X*X + Y*Y) end.
#Fun<erl_eval.12.50752066>
13> A(3,4).
5.0
14> TempConvert = fun({c,C}) -> {f, 32 + C*9/5};
14>                  ({f,F}) -> {c, (F-32)*5/9}
14>               end.
#Fun<erl_eval.6.50752066>
15> TempConvert({c,100}).
{f,212.0}
16> TempConvert({f,212}).
{c,100.0}
```

### 以*fun*作为参数的函数
```
18> L = [1,2,3,4].
[1,2,3,4]
19> lists:map(fun(X) -> X*2 end, L).
[2,4,6,8]
20> Even = fun(X) -> (X rem 2) =:=0 end.
#Fun<erl_eval.6.50752066>
21> Even(7).
false
22> Even(8).
true
23> lists:map(Even,L).
[false,true,false,true]
24> lists:filter(Even,L).
[2,4]
```
