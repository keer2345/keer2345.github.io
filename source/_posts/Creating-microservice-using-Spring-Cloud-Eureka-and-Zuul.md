---
title: Creating microservice using Spring Cloud, Eureka and Zuul
date: 2021-07-01 13:17:12
tags: spring-cloud
---

# 概述

Spring 框架提供了一系列的库来创建微服务，它们是 Spring Cloud 项目的一部分。这篇文章展示如何使用 Spring Boot 和下列技术来创建简单的微服务：

- **Zuul** 网关服务，提供动态路由，监听，弹性，安全性以及其他
- **Ribbon** 客户端负载均衡
- **Feign** 声明 REST 客户端
- **Eureka** 服务注册和发现
- **Sleuth** 通过日志分布式跟踪
- **Zipkin** 具有请求可视化的分布式跟踪系统

[piomin/sample-spring-microservices](https://github.com/piomin/sample-spring-microservices) 是一个简单的应用，下图展示了这个应用的结构。客户端由 _customer-service_ 提供，通过 _Zuul_ 网关存储基本客户数据。此端点与 _account-service_ 互动，以收集 _account-service_ 端点所服务的客户账户信息。每个服务在 _Eureka_ 的 _discovery-serice_ 上自行注册，并使用 _spring-cloud-sleuth_ 的 _zipkin-service_ 发送日志。

![](https://i0.wp.com/piotrminkowski.com/wp-content/uploads/2017/02/san1s57hfsas5v53ms53.png?w=710&ssl=1)

# account-service

## Create Project

Dependencies of Spring Boot project:

- Eureka Server
- Spring Boot Actuator
- Sleuth
- Zipkin Client

`pom.xml`:

```xml
<dependencies>
	<dependency>
		<groupId>org.springframework.boot</groupId>
		<artifactId>spring-boot-starter-actuator</artifactId>
	</dependency>
	<dependency>
		<groupId>org.springframework.cloud</groupId>
		<artifactId>spring-cloud-sleuth-zipkin</artifactId>
	</dependency>
	<dependency>
		<groupId>org.springframework.cloud</groupId>
		<artifactId>spring-cloud-starter-netflix-eureka-server</artifactId>
	</dependency>
	<dependency>
		<groupId>org.springframework.cloud</groupId>
		<artifactId>spring-cloud-starter-sleuth</artifactId>
	</dependency>

  <!-- https://mvnrepository.com/artifact/org.projectlombok/lombok -->
  <dependency>
      <groupId>org.projectlombok</groupId>
      <artifactId>lombok</artifactId>
      <version>1.18.20</version>
      <scope>provided</scope>
  </dependency>

	<dependency>
		<groupId>org.springframework.boot</groupId>
		<artifactId>spring-boot-starter-test</artifactId>
		<scope>test</scope>
	</dependency>
</dependencies>
<dependencyManagement>
	<dependencies>
		<dependency>
			<groupId>org.springframework.cloud</groupId>
			<artifactId>spring-cloud-dependencies</artifactId>
			<version>${spring-cloud.version}</version>
			<type>pom</type>
			<scope>import</scope>
		</dependency>
	</dependencies>
</dependencyManagement>
```

在 _sccount-service_ 中，使用 _findByCustomer_ 方法通过 _id_ 获取客户账户。
