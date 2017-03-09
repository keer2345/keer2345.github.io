---
title: Spring MVC 学习指南 01
date: 2017-03-08 19:36:55
categories: java
tags: [java, spring, SpringMvcGuide]
---

# Spring 框架

[Spring](http://spring.io/)框架是一个开源的企业应用开发框架。[martinfowler](https://martinfowler.com/)的这篇文章介绍了依赖注入和控制反转二者的不同：[Inversion of Control Containers and the Dependency Injection pattern](http://martinfowler.com/articles/injection.html)。

<!--more-->
## XML配置文件
### spring-config.xml
```xml
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xsi:schemaLocation="http://www.springframework.org/schema/beans
       http://www.springframework.org/schema/beans/spring-beans.xsd">

       ...

       <!--导入其他配置文件：-->
       <import resource="/resources/config1.xml"/>

</beans>
```
## 向构造器传递参数
### Product.java
```java
package com.keer;

public class Product {
    private String name;
    private String description;
    private float price;

    public Product() {
    }

    public Product(String name, String description, float price) {
        this.name = name;
        this.description = description;
        this.price = price;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    // description, price's getter & setter

}

```
### Address.java
```java
package com.keer;

public class Address {
    private String line1;
    private String line2;
    private String city;
    private String state;
    private String zipCode;
    private String country;

    public Address(String line1, String line2, String city, String state, String zipCode, String country) {
        this.line1 = line1;
        this.line2 = line2;
        this.city = city;
        this.state = state;
        this.zipCode = zipCode;
        this.country = country;
    }

	// getter & setter

    @Override
    public String toString() {
        return "Address{" +
                "line1='" + line1 + '\'' +
                ", line2='" + line2 + '\'' +
                ", city='" + city + '\'' +
                ", state='" + state + '\'' +
                ", zipCode='" + zipCode + '\'' +
                ", country='" + country + '\'' +
                '}';
    }
}
```
### Employee.java
```java
package com.keer;

public class Employee {
    private String firstName;
    private String lastName;
    private Address homeAddress;

    public Employee() {
    }

    public Employee(String firstName, String lastName, Address homeAddress) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.homeAddress = homeAddress;
    }

    // getter & setter

    @Override
    public String toString() {
        return "Employee{" +
                "firstName='" + firstName + '\'' +
                ", lastName='" + lastName + '\'' +
                ", homeAddress=" + homeAddress +
                '}';
    }
}
```
### spring-config.xml
```xml
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xsi:schemaLocation="http://www.springframework.org/schema/beans
       http://www.springframework.org/schema/beans/spring-beans.xsd">

    <bean id="localDate" class="java.time.LocalDate"
          factory-method="now"/>

    <bean name="product" class="com.keer.Product"/>

    <bean name="featuredProduct" class="com.keer.Product">
        <constructor-arg name="name" value="Olive Oil"/>
        <constructor-arg name="description" value="this is a product's description"/>
        <constructor-arg name="price" value="9.95"/>
    </bean>

    <bean name="featuredProduct2" class="com.keer.Product">
        <constructor-arg index="0" value="Ultimate Olive Oil"/>
        <constructor-arg index="1" value="The purest olive oil on the market"/>
        <constructor-arg index="2" value="9.95"/>
    </bean>

    <bean name="employee1" class="com.keer.Employee">
        <property name="homeAddress" ref="simpleAddress"/>
        <property name="firstName" value="Junior"/>
        <property name="lastName" value="Moore"/>
    </bean>

    <bean name="employee2" class="com.keer.Employee">
        <constructor-arg name="firstName" value="Senior"/>
        <constructor-arg name="lastName" value="Moore"/>
        <constructor-arg name="homeAddress" ref="simpleAddress"/>
    </bean>

    <bean name="simpleAddress" class="com.keer.Address">
        <constructor-arg name="line1" value="151 Corner Street"/>
        <constructor-arg name="line2" value=""/>
        <constructor-arg name="city" value="Albany"/>
        <constructor-arg name="state" value="NY"/>
        <constructor-arg name="zipCode" value="99999"/>
        <constructor-arg name="country" value="US"/>
    </bean>
</beans>
```
### App.java
```java
package com.keer;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.time.LocalDate;

public class App {
    public static void main(String[] args) {
        ApplicationContext context = new ClassPathXmlApplicationContext(new String[]{"spring-config.xml"});

        Product product1 = context.getBean("product", Product.class);
        product1.setName("New Product");
        System.out.println("product1: " + product1.getName());

        Product product2 = context.getBean("product", Product.class);
        System.out.println("product2: " + product2.getName());

        Product featuredProduct = context.getBean("featuredProduct", Product.class);
        System.out.println(featuredProduct.getName() + ", " + featuredProduct.getDescription()
                + ", " + featuredProduct.getPrice());

        LocalDate localDate = context.getBean("localDate", java.time.LocalDate.class);
        System.out.println("today:" + localDate);


        Employee employee1 = context.getBean("employee1", Employee.class);
        System.out.println(employee1.getFirstName() + " " + employee1.getLastName());
        System.out.println(employee1.getHomeAddress());

        Employee employee2 = context.getBean("employee2", Employee.class);
        System.out.println(employee2.getFirstName() + " " + employee2.getLastName());
        System.out.println(employee2.getHomeAddress());

    }
}
```
