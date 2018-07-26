---
title: Create SFTP with Docker
date: 2018-07-26 23:19:06
tags: [docker, ftp]
---

详细文档参见 [atmoz/sftp](https://store.docker.com/community/images/atmoz/sftp)


# 最简单的例子
```
docker run -p 22:22 -d atmoz/sftp foo:pass:::upload
```

<!-- more -->

# 映射本机路径
```
docker run \
    -v /host/upload:/home/foo/upload \
    -p 2222:22 -d atmoz/sftp \
    foo:pass:1000
```

> `foo:pass:1000` 中的 `uid` 对应宿主机 `uid`，可以通过 `cat /etc/passwd` 查看。

# 使用docker-compose
```
mkdir -p ~/app/sftp
cd ~/app/sftp
mkdir docker-compose.yml
```

`docker-compose.yml`:
```
sftp:
    image: atmoz/sftp
    volumes:
        - /host/upload:/home/foo/upload
    ports:
        - "2222:22"
    command: foo:pass:1000
```

运行：
```
docker-compose up -d
```

# 客户端
客户端我们可以选择 `FileZilla`:
```
sudo apt-get install filezilla
```

注意：在“文件→站点管理器→新站点→协议“中选择 `SFTP`。

然后就可以下载和上传文件了。

