---
title: Spring Boot 初尝试01：最基本的Web应用
date: 2017-08-13 21:09:03
categories: java
tags: [java, spring, spring boot]
---
## 创建Web应用
在官网`https://start.spring.io/`或者用IDEA集成的`Spring Initializr`生成Web项目

<!-- more -->

## 控制器`HelloController.java`
```java
package com.keer.controller;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Created by qinjh on 17-8-15.
 */
@RestController
public class HelloController {
    @RequestMapping("/hello")
    public String index() {
        return "Hello world!!!";
    }
}
```

## Spring Boot 主程序
```java
package com.keer;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Chapter01Application {

	public static void main(String[] args) {
		SpringApplication.run(Chapter01Application.class, args);
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

## 测试`Chapter01ApplicationTests.java`
```java
package com.keer;

import com.keer.controller.HelloController;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.Matchers.equalTo;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


@RunWith(SpringRunner.class)
@SpringBootTest
public class Chapter01ApplicationTests {

    private MockMvc mvc;

    @Before
    public void setUp() throws Exception {
        mvc = MockMvcBuilders.standaloneSetup(new HelloController()).build();
    }

    @Test
    public void getHello() throws Exception {
        mvc.perform(MockMvcRequestBuilders.get("/hello").accept(MediaType
                .APPLICATION_JSON)).andExpect(status().isOk()).andExpect
                (content().string(equalTo("Hello world!!!")));
    }

}
```
