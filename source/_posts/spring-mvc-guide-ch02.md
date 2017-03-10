---
title: Spring MVC 学习指南 02
date: 2017-03-08 19:36:56
categories: java
tags: [java, spring, SpringMvcGuide]
---
# Spring MVC 初步了解
## 构建一个Java Web项目
```
$ mvn archetype:generate \           
-DarchetypeCatalog=internal \
-DgroupId=com.keer \
-DartifactId=ch02 \
-DarchetypeArtifactId=maven-archetype-webapp
```

<!--more-->

## 添加依赖包和Tomcat支持
pom.xml
```xml
<dependency>
	<groupId>javax.servlet</groupId>
	<artifactId>javax.servlet-api</artifactId>
	<version>3.1.0</version>
	<scope>provided</scope>
</dependency>
<dependency>
	<groupId>org.springframework</groupId>
	<artifactId>spring-webmvc</artifactId>
	<version>4.2.4.RELEASE</version>
</dependency>
<dependency>
	<groupId>javax.servlet</groupId>
	<artifactId>jstl</artifactId>
	<version>1.2</version>
	<scope>runtime</scope>
</dependency>

...

<build>
    <finalName>ch02</finalName>
    <plugins>
        <plugin>
            <groupId>org.apache.tomcat.maven</groupId>
            <artifactId>tomcat7-maven-plugin</artifactId>
            <version>2.1</version>
            <configuration>
                <port>8080</port>
                <path>/demo</path>
                <uriEncoding>UTF-8</uriEncoding>
                <finalName>demo</finalName>
                <server>tomcat7</server>
            </configuration>
        </plugin>
    </plugins>
</build>
```

## 添加Java文件
Product.java
```java
package com.keer.domain;

import java.math.BigDecimal;

/**
 * Created by keer on 2017/3/9.
 */
public class Product {
    private String name;
    private String description;
    private Float price;

    //Getter and Setter
}

```
ProductForm.java
```java
package com.keer.form;

/**
 * Created by keer on 2017/3/9.
 */
public class ProductForm {
    private String name;
    private String description;
    private String price;

 	//Getter and Setter
}

```
InputProductController.java

```java
package com.keer.controller;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.Controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Created by keer on 2017/3/9.
 */
public class InputProductController implements Controller {

    private static final Log logger =
            LogFactory.getLog(InputProductController.class);

    public ModelAndView handleRequest(HttpServletRequest request,
                                      HttpServletResponse response)
            throws Exception {
        logger.info("InputProductController called");
        return new ModelAndView("/WEB-INF/jsp/ProductForm.jsp");
    }
}

```
SaveProductController.java

```java
package com.keer.controller;

import com.keer.domain.Product;
import com.keer.form.ProductForm;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.Controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Created by keer on 2017/3/9.
 */
public class SaveProductController implements Controller {

    private static final Log logger =
            LogFactory.getLog(InputProductController.class);


    public ModelAndView handleRequest
            (HttpServletRequest request, HttpServletResponse response)
            throws Exception {

        logger.info("SaveProductController called");
        ProductForm productForm = new ProductForm();
        // populate action properties
        productForm.setName(request.getParameter("name"));
        productForm.setDescription(request.getParameter("description"));
        productForm.setPrice(request.getParameter("price"));

        // create model
        Product product = new Product();
        product.setName(productForm.getName());
        product.setDescription(productForm.getDescription());
        try {
            product.setPrice(Float.parseFloat(productForm.getPrice()));
        } catch (NumberFormatException e) {
        }
        // insert code to save Product
        System.out.println("Name--:" + product.getName());
        System.out.println("Desc--:" + product.getDescription());
        System.out.println("Price--:" + product.getPrice());

        return new ModelAndView("/WEB-INF/jsp/ProductDetails.jsp",
                "product", product);

    }

}

```

## 添加Spring MVC支持
web.xml
```xml
<servlet>
    <servlet-name>springmvc</servlet-name>
    <servlet-class>
        org.springframework.web.servlet.DispatcherServlet
    </servlet-class>
    <init-param>
        <param-name>contextConfigLocation</param-name>
        <param-value>classpath:springmvc-servlet.xml</param-value>
    </init-param>
    <load-on-startup>1</load-on-startup>
</servlet>

<servlet-mapping>
    <servlet-name>springmvc</servlet-name>
    <url-pattern>*.action</url-pattern>
</servlet-mapping>
```

resources/springmvc-servlet.xml
```xml
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xsi:schemaLocation="http://www.springframework.org/schema/beans
       http://www.springframework.org/schema/beans/spring-beans.xsd">

    <bean name="/input-product.action"
          class="com.keer.controller.InputProductController"/>
    <bean name="/save-product.action"
          class="com.keer.controller.SaveProductController"/>

</beans>
```
## 添加JSP和CSS文件
/WEB-INF/jsp/ProductForm.jsp
```
<!DOCTYPE html>
<html>
<head>
    <title>Save Product</title>
    <style type="text/css">@import url(css/main.css);</style>
    <%@ page isELIgnored="false" %>
</head>
<body>
<div id="global">
    <h4>The product has been saved.</h4>
    <p>
    <h5>Details:</h5>
    Product Name : ${product.name}<br/>
    Description: ${product.description}<br/>
    Price: $${product.price}
    </p>
</div>
</body>
</html>
```
/WEB-INF/jsp/ProductDetails.jsp
```
<!DOCTYPE html>
<html>
<head>
    <title>Save Product</title>
    <style type="text/css">@import url(css/main.css);</style>
    <%@ page isELIgnored="false" %>
</head>
<body>
<div id="global">
    <h4>The product has been saved.</h4>
    <p>
    <h5>Details:</h5>
    Product Name : ${product.name}<br/>
    Description: ${product.description}<br/>
    Price: $${product.price}
    </p>
</div>
</body>
</html>
```

webapp/css/main.css
```css
#global {
    text-align: left;
    border: 1px solid #dedede;
    background: #efefef;
    width: 560px;
    padding: 20px;
    margin: 100px auto;
}

form {
    font: 100% verdana;
    min-width: 500px;
    max-width: 600px;
    width: 560px;
}

form fieldset {
    border-color: #bdbebf;
    border-width: 3px;
    margin: 0;
}

legend {
    font-size: 1.3em;
}

form label {
    width: 250px;
    display: block;
    float: left;
    text-align: right;
    padding: 2px;
}

#buttons {
    text-align: right;
}

#errors, li {
    color: red;
}
```

## Spring 的 View Resolver
springmvc-servlet.xml
```xml
 <bean id="viewResolver"
      class="org.springframework.web.servlet.view.InternalResourceViewResolver">
    <property name="prefix" value="/WEB-INF/jsp/"/>
    <property name="suffix" value=".jsp"/>
</bean>
```
相应的，Controller的`ModelAndView`改成如下：
InputProductController.java
```
return new ModelAndView("ProductForm");
```
SaveProductController.java
```
return new ModelAndView("ProductDetails",
                "product", product);
```