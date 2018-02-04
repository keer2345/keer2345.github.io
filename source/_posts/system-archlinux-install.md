---
title: Archlinux Install
date: 2018-02-02 22:45:36
categories: system
tags: archlinux
---
# Install
- https://www.cnblogs.com/vachester/p/5635819.html
- http://www.viseator.com/categories/Linux/Arch/

<!-- more -->


# Install Software
## Pacman
```
pacman -Syy  同步远程软件仓库
pacman -Syu  更新系统
pacman -Sc   Pacman下载的包文件位于 /var/cache/pacman/pkg/ 目录。该命令将清理未安装的包文件。
pacman -Scc  清理缓存

pacman -R  package_name
pacman -Rs package_name
```
## Yaourt
> https://www.cnblogs.com/reddusty/p/5098005.html
```
sudo pacman -S yaourt
```

```
yaourt -S package_name – 从AUR安装软件包
yaourt -Ss password – 使用关键字搜索软件包
yaourt -Syu –aur – 从AUR升级本地软件数据库并安装更新
yaourt -Si package_name – 列出软件包信息
yaourt -Sc – 从缓存中清楚旧的软件包
yaourt -Su – 安装AUR中的更新软件包
yaourt -Sy – 获取最新的AUR软件包数据库
yaourt -Cd – 清楚AUR软件包数据库
yaourt -R package_name – 删除软件包
```
## Software
- adobe-source-code-pro-fonts
