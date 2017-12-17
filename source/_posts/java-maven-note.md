---
title: Maven应用
date: 2017-03-06 22:59:39
categories: java
tags: [java,maven]
---
[Maven](https://maven.apache.org)是优秀的Java构建工具,能够帮我们自动化构建过程，从清理、编译、测试到生成报告，再到打包和部署。

<!--more-->

## 安装Maven
### 安装
* 下载[Maven](https://maven.apache.org/download.cgi)并解压`tar xzvf apache-maven-3.3.9-bin.tar.gz`
* 配置环境变量`~/.bash_profile`
```
export M2_HOME=/Users/.../apache-maven-3.3.9
export PATH=$PATH:$M2_HOME/bin
```
* 使环境变量生效`source ~/.bash_profile`

### 配置镜像
采用Maven阿里云中央仓库，修改maven根目录下的`conf/setting.xml`文件，内容如下：
```
<mirrors>
    <mirror>
      <id>alimaven</id>
      <name>aliyun maven</name>
      <url>http://maven.aliyun.com/nexus/content/groups/public/</url>
      <mirrorOf>central</mirrorOf>        
    </mirror>
</mirrors>	
```

## 创建项目
```
mvn archetype:generate \
  -DgroupId={project-packaging} \
  -DartifactId={project-name} \
  -DarchetypeArtifactId=maven-archetype-quickstart \
  -DinteractiveMode=false
```

创建工程的时候还卡在Generating project in Interactive mode不动了，解决方法是加个参数 `-DarchetypeCatalog=internal` 让它不要从远程服务器上取catalog。

### 普通项目
```
mvn archetype:generate \
  -DgroupId=com.mycompany.app \
  -DartifactId=my-app \
  -DarchetypeArtifactId=maven-archetype-quickstart \
  -DinteractiveMode=false
```
`mvn archetype:generate`:固定格式

`-DgroupId`:组织标识（包名）

`-DartifactId`:项目名称

`-DarchetypeArtifactId`:指定ArchetypeId，maven-archetype-quickstart，创建一个JavaProject；maven-archetype-webapp，创建一个Web Project

`-DinteractiveMode`:是否使用交互模式
### Web项目
```
mvn archetype:generate \
-DarchetypeCatalog=internal \
-DgroupId=com.keer \
-DartifactId=keerProject \
-DarchetypeArtifactId=maven-archetype-webapp  
```
给Web项目添加Tomcat插件：
```
<build>
    <finalName>jspdemo03</finalName>
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


## 参考
[maven3常用命令、java项目搭建、web项目搭建详细图解](http://blog.csdn.net/edward0830ly/article/details/8748986)