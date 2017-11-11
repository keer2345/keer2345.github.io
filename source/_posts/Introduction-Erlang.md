---
title: 介绍Erlang
date: 2017-11-11 10:57:00
categories: erlang
tags: erlang
---


<center>
![Erlang](https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1510379432890&di=ac28d6b749b3c532954d7ded5413c82b&imgtype=0&src=http%3A%2F%2Fi.dimg.cc%2Fef%2Fba%2F32%2Fd8%2F3c%2Fa1%2F59%2F67%2F4c%2F26%2F33%2F97%2Fb2%2F94%2F32%2Fb7.jpg)
</center>

一直以来，都想尝试点不同的编程语言。这篇文章将带您快速了解以下方面的内容：

- Erlang的基本信息
- 基本数据类型
- 函数和递归
- 模块
- 模式匹配

<!-- more -->

## 什么是Erlang
Erlang(['ə:læŋ])是一种通用的面向并发的编程语言，它由瑞典电信设备制造商爱立信所辖的CS-Lab开发，目的是创造一种可以应对大规模并发活动的编程语言和运行环境。Erlang问世于1987年，经过十年的发展，于1998年发布开源版本。Erlang是运行于虚拟机的解释性语言，但是现在也包含有乌普萨拉大学高性能Erlang计划（HiPE）开发的本地代码编译器，自R11B-4版本开始，Erlang也开始支持脚本式解释器。在编程范型上，Erlang属于多重范型编程语言，涵盖函数式、并发式及分布式。顺序执行的Erlang是一个及早求值, 单次赋值和动态类型的函数式编程语言。

Erlang是一个结构化，动态类型编程语言，内建并行计算支持。最初是由爱立信专门为通信应用设计的，比如控制交换机或者变换协议等，因此非常适 合于构建分布式，实时软并行计算系统。使用Erlang编写出的应用运行时通常由成千上万个轻量级进程组成，并通过消息传递相互通讯。进程间上下文切换对于Erlang来说仅仅 只是一两个环节，比起C程序的线程切换要高效得多得多了。

使用Erlang来编写分布式应用要简单的多，因为它的分布式机制是透明的：对于程序来说并不知道自己是在分布式运行。Erlang运行时环境是一个虚拟机，有点像Java虚拟机，这样代码一经编译，同样可以随处运行。它的运行时系统甚至允许代码在不被中断 的情况下更新。另外如果需要更高效的话，字节代码也可以编译成本地代码运行。


<center>
![Erlang](https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1510379340010&di=2702b698950bf61db122e2c1f7665dae&imgtype=0&src=http%3A%2F%2Fimg1.cache.netease.com%2Fcatchpic%2FC%2FC8%2FC815F5F0E7886F666527F658BF980670.png)
</center>

## 安装
安装相当容易，可以从官网 [the official website](https://www.erlang.org/downloads) 下载二进制包或者使用包管理器进行安装。比如：
- Mac OS：`brew install erlang`
- Ubuntu：`sudo apt-get install erlang`

## 第一步
让我们开启Erlang终端做一些简单的操作。在Terminal终端输入`erl`（对于Windows用户可能要用`werl`）。
```erlang
> $ erl                                                                                  
Erlang/OTP 18 [erts-7.3] ... ...

Eshell V7.3  (abort with ^G)
1> 2 + 4.
6
2> 5 * 8.4.
42.0
```
有意思的是Erlang所有表达式以句点（`.`）结束，意味着我们可以方便的在书写多行表达式，Erlang将认为这是个单一表达式，直到遇到句点终止符（`.`）：
```erlang
3> "Hello"   
3> ++
3> " world!"
3> .
"Hello world!"
```
退出终端使用`q().`或者`init:stop().`。

## 基本数据类型
### *Numbers*
类似其他语言，Erlang拥有数值型：*integers* 和 *floats* 。除了标准计数制和操作，它还提供了两个附加的计数制：
- *base#value* - Erlang允许特别的数值用于不同的进制（从二进制到36进制）：
```erlang
1> 2#1001.
9
2> 16#1E.
30
3> 36#20.
72
```
- *$char* - ASCII码或者Unicode码表达字符：
```erlang
1> $a.
97
2> $A.
65
```
此外，Erlang还允许使用科学计数法（例如`2.3e-1`等于`0.23`）。

### *Atom*
原子是用于名称的常量，它表示它自身。类似于Scala的symbols，但是也不像Scala，它使用广泛。原子如果不是以小写字符开头，则需加上单引号（`'`）：
```erlang
1> foo.
foo
2> 'foo'.
foo
3> 'Foo'.
'Foo'
```
### *Booleans*
在Erlang中没有布尔型，原子`true`和`false`用于表示布尔的值。
同样的，Erlang提供了一下布尔表达式：`not`、`and`、`andalso`、`or`、`orelse`、`xor`。

### *Tuples*
元组是由固定数量的多种数据类型元素构成，由大括号表示：
```erlang
1> {1, "two", true}.
{1,"two",true}
2> {}.
{}
```
我们通常使用第一个元素来表示该元组的含义。例如：`{person, "John", "Doe", 36 }` ，第一个元素告诉我们该元组包含了一个人的信息，当元组用于匹配模式时这是非常有用的。
>注意：在编译时解释的元组已经被Erlang记录了，例如第一个元素表示记录类型为原子*Atom*，后面的元素为字段值。

### *Lists*
列表表示一系列的元素，Erlang中的列表是循环的数据构造：
- 可以是空的`[]`
- 有头部和尾部组成`[Head | Tail]`。Head是第一个元素，Tail为剩余的元素。

一个列表用于表示枚举计数*enumeration notation*（例如`[1,2,3,4]`），递归计数（例如`[1 | [2 | [3 | [4 | [] ] ] ] ]`），或者合成为（`[1,2 | [3 | [4 | [] ] ] ] `）：
```erlang
1> [1,2,3,4].
[1,2,3,4]
2> [1|[2|[3|[4|[]]]]].
[1,2,3,4]
3>  [1,2|[3|[4|[]]]].  
[1,2,3,4]
4>  
4> [1, two, "three", [4]].
[1,two,"three",[4]]
```
### *String*
字符串使用双引号表示，它是列表的缩写：
`"hello"`等价于` [$h,$e,$l,$l,$o]`和`[104,101,108,108,111]`。

## 比较操作
比较操作和主流语言是一样的，有一些在Erlang的表示有点不同：

| 操作符        | 描述           |
| :-------------: |:-------------|
| ==| 等于 |
| /=| 不等于 |
| =<| 小于等于 |
| <| 小于 |
| >=| 大于等于 |
| >| 大于 |
| =:=|   精确等于 |
| =/=| 精确不等于 |


当要确认是否是相同类型的时候，精确等于/不等于（`=:=` 和 `=/=`）是非常有用的。

```erlang
1> 42 =:= 42.  
true  
2> 42 == 42.0.  
true  
3> 42 =:= 42.0.  
false  
4> 42 =:= 30.  
false  
```

## 变量
Erlang在变量上是很有趣的，变量一经赋值，它的值将不能再做改变。让我们看看下面的变量定义：
```erlang
1> Hello = "Hello World!".  
"Hello World!"
```
关于变量需要注意以下两点：
- 变量名以大写字母开头
- 通过匹配模式**pattern matching**绑定变量（`=` 是匹配操作符）。如果变量未绑定，匹配成功的话它将赋予一个值，否则，该变量的值能用于任何表达式（函数、匹配模式、`case`表达式等等）

如果我们视图第二次绑定值将会报错：
```erlang
2> Hello = 3.  
** exception error: no match of right hand side value 3
```

使用匹配操作符我们还可以使用元组或列表预先给多个变量复制：
```erlang
1> {A, B} = {3, 4}.  
{3,4}
2> A.  
3  
3> B.  
4  
4> [Head| Tail] = [1, 2, 3, 4].  
[1,2,3,4]
5> Head.  
1  
6> Tail.  
[2,3,4]
7> [X, Y, Z | Tail1] = [true, "Hello", 42, {2, 4}, 7.4].  
[true,"Hello",42,{2,4},7.4]
```

当使用匹配操作符，我们不可能绑定每一个值。匿名变量将发挥作用，使用`_`定义。

当然，通常情况下一般为变量起个有意义的名字，哪怕该变量没有使用。所幸的是，Erlang使用下划线`_`前缀的变量（例如`_Foo`）可以避免编译时生成警告信息。

## 函数和递归
在函数式编程语言中，函数扮演着重要的角色，Erlang也不例外。Erlang中函数自语句以分号`;`分离，最后一个子语句以句点`.`结束。
```erlang
take(0, _Xs) -> [];  
take(_N, []) -> [];  
take(N, [X| Xs]) when N > 0 -> [X | take(N-1, Xs)]. 
```
这里定义了递归函数`take`，返回列表的的前`N`个元素。让我们来分析一下这个函数的子语句：
- 如果第一个入参为*0*，返回空列表；
- 如果第二个入参*list*为空，返回空列表；
- 如果第一个入参大于*0*，第二个入参*list*不为空，添加头部元素到返回的结果列表，并且递归下一个值（第一个入参为*N-1*，第二个入参为*list*的尾部元素，知道*N*等于*0*。

Erlang也有匿名函数**anonymous functions**，与命名函数的定义类似，我们使用`fun`关键词并且以`end`结束：
```erlang
1> Double = fun(X) -> 2*X end.
2> Double(23).
46
```

使用复合的子语句定义匿名函数也很简单：
```erlang
3> Abs = fun(X) when X >= 0 -> X;  
3> (X) when X < 0 -> -X end.  
#Fun<erl_eval.6.52032458>
4> Abs(3).  
3  
5> Abs(-4).  
4  
6> Abs(0).  
0  
```

*高位函数（Higher-order functions）*返回一个函数并且（或者）使用一个或者多个函数作为参数。这使得函数式编程非常的强大。

一个常用的函数是`map`，用来遍历列表的每一个元素：
```erlang
7> lists:map(Double, [1, 2, 3, 4, 5]).  
[2,4,6,8,10]
```
也可以使函数成为一个绑定的变量：
```erlang
lists:map(fun (X) -> X * 2 end, Xs).  
```

命名函数可以作为参数，Erlang将其当做原子，像下面这样：
```erlang
inc(X) -> X + 1.  
lists:map(fun inc/1, Xs). 
```

## 模块
函数组织在模块中，模块有一系列的属性和函数组成。让我们定义一个模块并命名为文件**foo.er**：
```erlang
-module(foo).
-export([square/1]).

square(A) ->  
    A * A.
```

让我们分析上面代码的结构：
- 第一行定义模块名称*foo*：`-module(foo).`
- 使用`export`属性来从模块中选择函数。并且函数名后紧跟着表明改函数的入参个数。
- 最后我们定义该函数。

我们使用`c(module).`命令来编译和加载模块：
```erlang
1> c(foo).  
{ok,foo}
```
现在模块被加载了，我们可以调用`square`函数了：
```erlang
2> foo:square(4).  
16
```
>注意：当定义一个模块，我们可以使用`-import(Module, Functions)`来从从其他模块导入函数。

## 模式匹配
和其他函数式编程语言类似，模式匹配可能是Erlang中很特别的结构了。
我们在之前已经看到两种方式的模式匹配：使用等号（`=`）和子语句函数。

这里，我们使用`case`表达式，通常的`case`表达式格式为：
```erlang
case Expression of ->  
  Pattern1 [when Guard1] -> Body1;
  ...;
  PatternN [when GuardN] -> BodyN
end  
```

使用模式匹配的阶乘函数看起来像这样：
```erlang
fact(N) ->  
  case N of
    1 ->
      1;
    N when N > 0 ->
      N * fact(N - 1)
  end.
```

## 尾声
在这篇简短的文章中，我们通过Erlang基本的语法和语义，了解了一些函数编程的概貌。为了更进一步了解Erlang的强大，建议查看更多的Erlang和Open Telecom Platform方面的书籍。

