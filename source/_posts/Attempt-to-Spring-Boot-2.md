---
title: Spring Boot 初尝试——基础应用开发（2）
date: 2017-08-13 21:14:22
categories: java
tags: [java,spring,spring boot]
---

## 添加MySQL依赖配置
```xml
<dependency>
		<groupId>org.springframework.boot</groupId>
		<artifactId>spring-boot-starter-data-jpa</artifactId>
</dependency>

<dependency>
		<groupId>mysql</groupId>
		<artifactId>mysql-connector-java</artifactId>
</dependency>
```

<!-- more -->

## 添加实体
Department.java
```java
package com.example.demo.entity;

import javax.persistence.*;

/**
 * Created by qinjh on 17-8-13.
 */
@Entity
@Table(name = "department")
public class Department {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;

    public Department() {
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}

```

Role.java
```java
package com.example.demo.entity;

import javax.persistence.*;
import java.io.Serializable;

/**
 * Created by qinjh on 17-8-13.
 */
@Entity
@Table
public class Role implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    private String name;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}

```

User.java
```java
package com.example.demo.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * Created by qinjh on 17-8-13.
 */
@Entity
@Table(name = "usr")
public class User implements Serializable {
    @Id
    @GeneratedValue
    private Long id;
    private String name;
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createdTime;
    @ManyToOne
    @JoinColumn(name = "dept_id")
    @JsonBackReference
    private Department department;
    @ManyToMany(cascade = {}, fetch = FetchType.EAGER)
    @JoinTable(name = "usr_role",
            joinColumns = {@JoinColumn(name = "usr_id")},
            inverseJoinColumns = {@JoinColumn(name = "role_id")})
    private List<Role> roles;

   //getter and setter
	 ...

}
```

## 实体持久化

DepartmentRepository.java
```java
package com.example.demo.repo;

import com.example.demo.entity.Department;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * Created by qinjh on 17-8-13.
 */
@Repository
public interface DepartmentRepository extends JpaRepository<Department, Long> {
}
```

RoleRepository.java
```java
@Repository
public interface RoleRepository extends JpaRepository<Role, Long> {
}
```

UserRepository.java
```
@Repository
public interface UserRepository extends JpaRepository<User, Long> {
}

```

## MySQL测试
配置数据库链接：application.properties，启动`spring-boot:run`
``properties`
spring.jpa.database=mysql
spring.jpa.show-sql=true
spring.jpa.hibernate.ddl-auto=update
spring.jpa.hibernate.naming.strategy=org.hibernate.cfg.ImprovedNamingStrategy

spring.datasource.url=jdbc:mysql://localhost:3306/ssmKeer?characterEncoding=utf8
spring.datasource.username=root
spring.datasource.password=123456
```
添加JPA配置类
```java
package com.example.demo;

import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.Database;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import javax.sql.DataSource;
import java.util.Properties;

/**
 * Created by qinjh on 17-8-13.
 */
@Configuration
@EnableJpaRepositories(basePackages = "com.example.demo.repo")
public class JpaConfiguration {
    @Bean
    PersistenceExceptionTranslationPostProcessor persistenceExceptionTranslationPostProcessor() {
        return new PersistenceExceptionTranslationPostProcessor();
    }

    @Bean
    public DataSource dataSource() {

        DriverManagerDataSource dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName("com.mysql.jdbc.Driver");
        dataSource.setUrl("jdbc:mysql://localhost:3306/ssmkeer?characterEncoding=utf8");
        dataSource.setUsername("root");
        dataSource.setPassword("123456");

        return dataSource;
    }

    @Bean
    public LocalContainerEntityManagerFactoryBean entityManagerFactory() {
        LocalContainerEntityManagerFactoryBean entityManagerFactoryBean = new LocalContainerEntityManagerFactoryBean();
        entityManagerFactoryBean.setDataSource(dataSource());
        entityManagerFactoryBean.setPackagesToScan("com.example.demo");
        entityManagerFactoryBean.setJpaProperties(buildHibernateProperties());
        entityManagerFactoryBean.setJpaVendorAdapter(new HibernateJpaVendorAdapter() {{
            setDatabase(Database.MYSQL);
        }});
        return entityManagerFactoryBean;
    }

    protected Properties buildHibernateProperties() {
        Properties hibernateProperties = new Properties();

        hibernateProperties.setProperty("hibernate.dialect", "org.hibernate.dialect.MySQL5Dialect");
        hibernateProperties.setProperty("hibernate.show_sql", "true");
        hibernateProperties.setProperty("hibernate.use_sql_comments", "false");
        hibernateProperties.setProperty("hibernate.format_sql", "true");
        hibernateProperties.setProperty("hibernate.hbm2ddl.auto", "update");
        hibernateProperties.setProperty("hibernate.generate_statistics", "false");
        hibernateProperties.setProperty("javax.persistence.validation.mode", "none");

        //Audit History flags
        hibernateProperties.setProperty("org.hibernate.envers.store_data_at_delete", "true");
        hibernateProperties.setProperty("org.hibernate.envers.global_with_modified_flag", "true");

        return hibernateProperties;
    }

    @Bean
    public PlatformTransactionManager transactionManager() {
        return new JpaTransactionManager();
    }

    @Bean
    public TransactionTemplate transactionTemplate() {
        return new TransactionTemplate(transactionManager());
    }
}
```

测试MySQL
```java
package com.example.demo;

import com.example.demo.entity.Department;
import com.example.demo.entity.Role;
import com.example.demo.entity.User;
import com.example.demo.repo.DepartmentRepository;
import com.example.demo.repo.RoleRepository;
import com.example.demo.repo.UserRepository;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.Assert;

import java.util.Date;
import java.util.List;


/**
 * Created by qinjh on 17-8-13.
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = {JpaConfiguration.class})
public class MySQLTest {
    private static Logger logger = LoggerFactory.getLogger(MySQLTest.class);

    @Autowired
    UserRepository userRepository;
    @Autowired
    DepartmentRepository departmentRepository;
    @Autowired
    RoleRepository roleRepository;

    @Before
    public void initData() {
        userRepository.deleteAll();
        departmentRepository.deleteAll();
        roleRepository.deleteAll();

        Department department = new Department();
        department.setName("开发部");
        departmentRepository.save(department);
        Assert.notNull(department.getId());
        System.out.println(department.getName());

        Role roleAdmin = new Role();
        roleAdmin.setName("Admin");
        roleRepository.save(roleAdmin);
        Assert.notNull(roleAdmin.getId());

        Role roleUser = new Role();
        roleUser.setName("User");
        roleRepository.save(roleUser);

        User user = new User();
        user.setName("keer");
        user.setCreatedTime(new Date());
        user.setDepartment(department);
        List<Role> roleList = roleRepository.findAll();
        Assert.notNull(roleList);
        user.setRoles(roleList);
        userRepository.save(user);
        Assert.notNull(user.getId());
    }

    @Test
    public void findPage() {
        Pageable pageable = new PageRequest
                (0, 10, new Sort(Sort.Direction.ASC, "id"));
        Page<User> page = userRepository.findAll(pageable);
        Assert.notNull(page);

        for (User user : page.getContent()) {
            logger.info("====user==== user name:{}, department name:{}, role name:{} ",
                    user.getName(), user.getDepartment().getName(), user.getRoles().get(0).getName());
        }
    }
}
```
