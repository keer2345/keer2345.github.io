---
title: Gradle Configuration
date: 2021-02-20 21:23:02
tags: gradle
---
# 解决gradle下载慢的问题
## 新建项目下载gradle慢的问题

1. 打开用户主目录:
- linux平台 `/Home/用户名/.gradle`
- windows平台 `c:\Users\用户名\.gradle`
- macos平台 `/Users/用户名/.gradle`

找到 `./gradle/wrapper/dist/gradle-版本号-bin目录`

2. （可选）官方网站下载这个版本号对应的安装包 http://services.gradle.org/distributions/ 把下载下来的文件存放到这个目录下面的子目录 (目录名由字母和数字组成) 下面, 大功告成了。

<!-- more -->

## 问题：下载依赖jar包慢

1. 解决：在 `build.gradle` 文件中添加国内镜像
```
repositories { 
  jcenter()
  mavenCentral()
  maven{ url "http://maven.aliyun.com/nexus/content/groups/public/" }
  maven { url "https://kotlin.bintray.com/kotlinx" }
}
```

2. 或者，新建 `~.gradle/init.gradle`:
```
allprojects {
    repositories {
        def ALIYUN_REPOSITORY_URL = 'https://maven.aliyun.com/repository/public'
        all { ArtifactRepository repo ->
            if(repo instanceof MavenArtifactRepository){
                def url = repo.url.toString()
                if (url.startsWith('https://repo1.maven.org/maven2')) {
                    project.logger.lifecycle "Repository ${repo.url} replaced by $ALIYUN_REPOSITORY_URL."
                    remove repo
                }
            }
        }
        maven { url ALIYUN_REPOSITORY_URL }
    }
}
```
