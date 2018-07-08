---
title: wechat miniapp backend guide
date: 2018-06-25 17:30:44
tags: [wechat, backend]
---

小程序需要申请一个ssl安全证书，推荐大家使用 [Let's Encrypt - Free SSL/TLS Certificates](https://link.zhihu.com/?target=https%3A//letsencrypt.org/) 的免费证书，然后进入 [Certbot](https://link.zhihu.com/?target=https%3A//certbot.eff.org/)，选择 [Certbot](https://link.zhihu.com/?target=https%3A//certbot.eff.org/%23centos6-nginx) 证书类型，并按照文档在本机生成证书，证书有效期是三个月，但是可以使用 *crontab* 任务去更新证书。Certbot 的官方文档描述截图如下：
<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/wechat/2018062501.jpg)
</center>
