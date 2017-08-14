---
title: Spring Boot 初尝试之一：基础应用开发
date: 2017-08-13 21:09:03
categories: java
tags: [java, spring, spring boot]
---
## 创建Web应用
在官网`https://start.spring.io/`或者用IDEA集成的`Spring Initializr`生成Web项目

<!-- more -->

## Hello world
```java
package com.example.demo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@SpringBootApplication
@RestController
public class DemoApplication {

    @RequestMapping("/")
    String index() {
        return "Hello world!";
    }

    public static void main(String[] args) {
        SpringApplication.run(DemoApplication.class, args);
    }
}
```

## 运行
* 通过IDEA集成的Maven里面的`spring-boot:run`运行
* 执行`mvn package`，然后执行`java -jar demo-0.0.1-SNAPSHOT.jar`运行

可以通过配置resources里面的`application.properties`文件来修改参数的默认值，比如
```
server.port=8000
```
