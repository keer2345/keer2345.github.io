---
title: Jeesite企业信息化开发基础平台
date: 2017-04-20 21:20:28
categories: java
tags: java
---

[Jeesite](https://github.com/thinkgem/jeesite)

JeeSite 是一个企业信息化开发基础平台，Java企业应用开源框架，Java EE（J2EE）快速开发框架，使用经典技术组合（Spring、Spring MVC、Apache Shiro、MyBatis、Bootstrap UI），包括核心模块如：组织机构、角色用户、权限授权、数据权限、内容管理、工作流等。 http://jeesite.com

<!-- more -->

# Jeesite搭建
## 搭建数据库
1. 创建MySQL数据库jeesite，字符集选择utf8_general_ci，如果是linux，需要设置MySQL的配置文件，忽略大小写`lower_case_table_names=1`
1. 通过`maven-antrun-plugin`插件，执行`mvn antrun:run -Pinit-db`，创建表以及导入数据。
```
				<plugin>
					<groupId>org.apache.maven.plugins</groupId>
					<artifactId>maven-antrun-plugin</artifactId>
					<version>1.7</version>
					<configuration>
						<target>

							<!-- mysql -->
							<property name="dbunit.datatype" value="org.dbunit.ext.mysql.MySqlDataTypeFactory" />

							<!-- mssql
							<property name="dbunit.datatype" value="org.dbunit.ext.mssql.MsSqlDataTypeFactory" /> -->

							<!-- oracle
							<property name="dbunit.datatype" value="org.dbunit.ext.oracle.Oracle10DataTypeFactory" /> -->

							<property file="src/main/resources/jeesite.properties" />
							<sql driver="${jdbc.driver}" url="${jdbc.url}" userid="${jdbc.username}" password="${jdbc.password}"
								onerror="continue" encoding="${project.build.sourceEncoding}">
								<classpath refid="maven.test.classpath" />
								<transaction src="db/sys/jeesite_${jdbc.type}.sql"/>
								<transaction src="db/cms/jeesite_${jdbc.type}.sql"/>
								<transaction src="db/oa/jeesite_${jdbc.type}.sql"/>
								<transaction src="db/gen/jeesite_${jdbc.type}.sql"/>
								<transaction src="db/gen/example_${jdbc.type}.sql"/>
								<transaction src="db/test/jeesite_${jdbc.type}.sql"/>
								<transaction src="db/act/drop/activiti.${jdbc.type}.drop.engine.sql"/>
								<transaction src="db/act/drop/activiti.${jdbc.type}.drop.history.sql"/>
								<transaction src="db/act/drop/activiti.${jdbc.type}.drop.identity.sql"/>
								<transaction src="db/act/create/activiti.${jdbc.type}.create.engine.sql"/>
								<transaction src="db/act/create/activiti.${jdbc.type}.create.history.sql"/>
								<transaction src="db/act/create/activiti.${jdbc.type}.create.identity.sql"/>
							</sql>

							<taskdef name="dbunit" classname="org.dbunit.ant.DbUnitTask" classpathref="maven.test.classpath" />

							<!-- mysql、mssql -->
							<dbunit driver="${jdbc.driver}" url="${jdbc.url}" userid="${jdbc.username}"
								 password="${jdbc.password}">

							<!-- oracle
							<dbunit driver="${jdbc.driver}" url="${jdbc.url}" userid="${jdbc.username}"
								password="${jdbc.password}" schema="${jdbc.username}" > -->

								<dbconfig>
									<property name="datatypeFactory" value="${dbunit.datatype}" />
								</dbconfig>
								<classpath refid="maven.test.classpath" />
								<operation type="INSERT" src="db/sys/jeesite_data.xls" format="xls" transaction="true"/>
								<operation type="INSERT" src="db/cms/jeesite_data.xls" format="xls" transaction="true"/>

							</dbunit>

						</target>
					</configuration>
				</plugin>
```
1. 运行`mvn tomcat7:run`，访问`http://127.0.0.1:8181/jeesite/`，用户名：thinkgem  密码：admin
