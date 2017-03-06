---
title: Maven应用
date: 2017-03-06 22:59:39
categories: java
tags: java maven
---

## 创建项目
```
mvn archetype:generate 
  -DgroupId={project-packaging} 
  -DartifactId={project-name} 
  -DarchetypeArtifactId=maven-archetype-quickstart 
  -DinteractiveMode=false
```

### 普通项目
```
mvn archetype:generate 
  -DgroupId=com.mycompany.app 
  -DartifactId=my-app 
  -DarchetypeArtifactId=maven-archetype-quickstart 
  -DinteractiveMode=false
```
`mvn archetype:generate`:固定格式

`-DgroupId`:组织标识（包名）

`-DartifactId`:项目名称

`-DarchetypeArtifactId`:指定ArchetypeId，maven-archetype-quickstart，创建一个JavaProject；maven-archetype-webapp，创建一个Web Project

`-DinteractiveMode`:是否使用交互模式
### Web项目
```
mvn archetype:generate 
-DarchetypeCatalog=internal 
-DgroupId=com.rv.buildnew 
-DartifactId=rbuildnew 
-DarchetypeArtifactId=maven-archetype-webapp  
```

创建工程的时候还卡在Generating project in Interactive mode不动了，解决方法是加个参数 `-DarchetypeCatalog=internal` 让它不要从远程服务器上取catalog。

## 参考
[maven3常用命令、java项目搭建、web项目搭建详细图解](http://blog.csdn.net/edward0830ly/article/details/8748986)