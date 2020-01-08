---
title: Emacs + Java + Maven Environment Develop
date: 2020-01-06 21:45:03
tags: [emacs, java, maven]
---

# Maven
## 新建项目
参考：https://maven.apache.org/guides/getting-started/maven-in-five-minutes.html


```
mvn -B archetype:generate -DgroupId=firstJava -DartifactId=firstJava -DarchetypeArtifactId=maven-archetype-quickstart
```
<!-- more -->

## 添加maven依赖并进行下载

```
cd firstJava
```
`pom.xml`:
```xml
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
    <modelVersion>4.0.0</modelVersion>
    <groupId>firstJava</groupId>
    <artifactId>firstJava</artifactId>
    <packaging>jar</packaging>
    <version>1.0-SNAPSHOT</version>
    <name>firstJava</name>
    <url>http://maven.apache.org</url>

    <dependencies>
        <dependency>
            <groupId>junit</groupId>
            <artifactId>junit</artifactId>
            <version>3.8.1</version>
            <scope>test</scope>
        </dependency>
        <dependency>
            <groupId>commons-codec</groupId>
            <artifactId>commons-codec</artifactId>
            <version>1.11</version>
        </dependency>
    </dependencies>

</project>
```

```
mvn dependency:resolve
```

## 编译并运行代码
`src/main/java/firstJava/App.java`:

```java
package firstJava;

/**
 * Hello world!
 *
 */

import org.apache.commons.codec.binary.Base64;

public class App
{
    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );
        System.out.println(Base64.encodeBase64String(args[0].getBytes()));
    }
}
```

```
mvn -B archetype:generate -DgroupId=firstJava -DartifactId=firstJava -DarchetypeArtifactId=maven-archetype-quickstart
```

```
[INFO] Scanning for projects...
[INFO] 
[INFO] ------------------------< firstJava:firstJava >-------------------------
[INFO] Building firstJava 1.0-SNAPSHOT
[INFO] --------------------------------[ jar ]---------------------------------
[INFO] 
[INFO] --- maven-resources-plugin:2.6:resources (default-resources) @ firstJava ---
[WARNING] Using platform encoding (UTF-8 actually) to copy filtered resources, i.e. build is platform dependent!
[INFO] skip non existing resourceDirectory /home/qinjh/workspace/java/tmp/firstJava/src/main/resources
[INFO] 
[INFO] --- maven-compiler-plugin:3.1:compile (default-compile) @ firstJava ---
[INFO] Nothing to compile - all classes are up to date
[INFO] 
[INFO] --- exec-maven-plugin:1.6.0:java (default-cli) @ firstJava ---
Hello World!
bGFybHVv
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time: 1.362 s
[INFO] Finished at: 2020-01-06T21:16:15+08:00
[INFO] ------------------------------------------------------------------------
```

## 打包并运行jar包
`pom.xml`:

```xml
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">

    <!-- ... -->

<dependencies>

    <!-- ... -->

</dependencies>

<build>
    <plugins>
        <plugin>
            <!-- <groupId>org.apache.maven.plugins</groupId> -->
            <artifactId>maven-assembly-plugin</artifactId>
                <!-- <version>2.4</version> -->
                <configuration>
                    <descriptorRefs>
                        <descriptorRef>jar-with-dependencies</descriptorRef>
                    </descriptorRefs>
                </configuration>
                <executions>
                    <execution>
                        <id>make-assembly</id>
                        <phase>package</phase>
                        <goals>
                            <goal>single</goal>
                        </goals>
                    </execution>
                </executions>
            </plugin>
        </plugins>
    </build>

</project>
```

```
mvn package -DskipTests
```


```
[INFO] Scanning for projects...
[INFO] 
[INFO] ------------------------< firstJava:firstJava >-------------------------
[INFO] Building firstJava 1.0-SNAPSHOT
[INFO] --------------------------------[ jar ]---------------------------------
[INFO] 
[INFO] --- maven-resources-plugin:2.6:resources (default-resources) @ firstJava ---
[WARNING] Using platform encoding (UTF-8 actually) to copy filtered resources, i.e. build is platform dependent!
[INFO] skip non existing resourceDirectory /home/qinjh/workspace/java/tmp/firstJava/src/main/resources
[INFO] 
[INFO] --- maven-compiler-plugin:3.1:compile (default-compile) @ firstJava ---
[INFO] Nothing to compile - all classes are up to date
[INFO] 
[INFO] --- maven-resources-plugin:2.6:testResources (default-testResources) @ firstJava ---
[WARNING] Using platform encoding (UTF-8 actually) to copy filtered resources, i.e. build is platform dependent!
[INFO] skip non existing resourceDirectory /home/qinjh/workspace/java/tmp/firstJava/src/test/resources
[INFO] 
[INFO] --- maven-compiler-plugin:3.1:testCompile (default-testCompile) @ firstJava ---
[INFO] Nothing to compile - all classes are up to date
[INFO] 
[INFO] --- maven-surefire-plugin:2.12.4:test (default-test) @ firstJava ---
[INFO] Tests are skipped.
[INFO] 
[INFO] --- maven-jar-plugin:2.4:jar (default-jar) @ firstJava ---
[INFO] 
[INFO] --- maven-assembly-plugin:2.2-beta-5:single (make-assembly) @ firstJava ---
[INFO] Building jar: /home/qinjh/workspace/java/tmp/firstJava/target/firstJava-1.0-SNAPSHOT-jar-with-dependencies.jar
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  3.539 s
[INFO] Finished at: 2020-01-06T21:53:38+08:00
[INFO] ------------------------------------------------------------------------
```

# Emacs
## Emacs配置参考
- https://github.com/keer2345/keer-emacs
- https://github.com/keer2345/lisp-learning

## Emacs集成Java
- 安装java开发插件meghanada: https://github.com/mopemope/meghanada-emacs



> https://www.jianshu.com/p/db16f65bde79?from=timeline&isappinstalled=0
