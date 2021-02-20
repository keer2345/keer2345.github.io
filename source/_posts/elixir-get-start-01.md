---
title: Elixir Get Start 01
date: 2020-08-16 23:26:49
tags: elixir
---

# 概述
```elixir
$ elixir -v
Erlang/OTP 23 [erts-11.0.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe] [dtrace]

Elixir 1.10.4 (compiled with Erlang/OTP 23)
```
<!-- more -->

# 使用 mix 管理 elixir 项目
`mix help` 可以查看 mix 支持的诸多命令。

## 创建项目
```
mix new sample
```
## 编译和运行
`lib/sample.ex`:
```elixir
defmodule Sample do
  def hello do
    IO.puts "hello Elixir"
  end
end
```
```elixir
$ mix compile

$ iex -S mix
iex(1)> Sample.hello
hello Elixir
:ok
```
## 单元测试
```
touch test/sample_test.exs
```
```elixir
defmodule SampleTest do
  use ExUnit.Case
  doctest Sample

  test "case01" do
    assert :ok == Sample.hello
  end

  test "case02" do
    assert :ng == Sample.hello
  end
end
```
```elixir
$ mix test
hello Elixir
hello Elixir


  1) test case02 (SampleTest)
     test/sample_test.exs:9
     Assertion with == failed
     code:  assert :ng == Sample.hello
     left:  :ng
     right: :ok
     stacktrace:
       test/sample_test.exs:10: (test)

.

Finished in 0.1 seconds
2 tests, 1 failure

Randomized with seed 947314
```
## 编译二进制文件
`lib/sample.ex/`:
```elixir
defmodule Sample do
  def main(args \\ []) do
    IO.puts "hello "
    IO.puts args
  end
end
```
为了编译二进制，设置 `mix.exs` 如下：
```elixir
def project do
  [app: :sample,
   version: "0.1.0",
   elixir: "~> 1.10",
   escript: escript,                            # 追加 escript 配置
   start_permanent: Mix.env == :prod,
   deps: deps]
end

def escript do                                  # 追加 escript 函数，其中设置 main_module
  [main_module: Sample]
end
```
编异成二进制：
```
$ mix escript.build

$ ls -l sample # 此为二进制文件

$ ./sample
hello

$ ./sample "Elixir"
hello
Elixir
```

> https://www.cnblogs.com/wang_yb/p/5196884.html
